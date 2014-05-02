#include <linux/module.h>
#include <linux/string.h>
#include <linux/fs.h>
#include <linux/time.h>
#include <linux/slab.h>
#include <linux/init.h>
#include <linux/blkdev.h>
#include <linux/parser.h>
#include <linux/mount.h>
#include <linux/namei.h>
#include <linux/statfs.h>
#include <linux/mtd/super.h>
#include <linux/ctype.h>
#include <linux/highmem.h>
#include <linux/pagemap.h>
#include <linux/uaccess.h>
#include <linux/buffer_head.h>
#include <linux/printk.h>
#include "ch10fs.h"
#include "internal.h"

static struct kmem_cache *ch10fs_inode_cachep;

static const umode_t ch10fs_modemap[8] = {
	0,			/* hard link */
	S_IFDIR  | 0644,	/* directory */
	S_IFREG  | 0644,	/* regular file */
	S_IFLNK  | 0777,	/* symlink */
	S_IFBLK  | 0600,	/* blockdev */
	S_IFCHR  | 0600,	/* chardev */
	S_IFSOCK | 0644,	/* socket */
	S_IFIFO  | 0644		/* FIFO */
};

static const unsigned char ch10fs_dtype_table[] = {
	DT_UNKNOWN, DT_DIR, DT_REG, DT_LNK, DT_BLK, DT_CHR, DT_SOCK, DT_FIFO
};

static struct inode *ch10fs_iget(struct super_block *sb, unsigned long pos);



/*
 * read data from an ch10fs image on a block device
 */
static int ch10fs_blk_read(struct super_block *sb, unsigned long pos,
			  void *buf, size_t buflen)
{
	struct buffer_head *bh;
	unsigned long offset;
	size_t segment;

	/* copy the string up to blocksize bytes at a time */
	while (buflen > 0) {
		offset = pos & (CH10BSIZE - 1);
		segment = min_t(size_t, buflen, CH10BSIZE - offset);
		bh = sb_bread(sb, pos >> CH10BSBITS);

		if (!bh) {
			return -EIO;
		}
		//		print_hex_dump_bytes("", DUMP_PREFIX_OFFSET, bh->b_data, 1024);
		memcpy(buf, bh->b_data + offset, segment);
		brelse(bh);
		buf += segment;
		buflen -= segment;
		pos += segment;
	}

	return 0;
}

/*
 * read data from the ch10fs image
 */
int ch10fs_dev_read(struct super_block *sb, unsigned long pos,
		   void *buf, size_t buflen)
{
	//	printk(KERN_INFO "ch10fs: reading %u at %lu on %s\n", buflen, pos, sb->s_id);
	size_t limit;

	limit = ch10fs_maxsize(sb);
	if (pos >= limit) {
		return -EIO;
	}
	if (buflen > limit - pos)
		buflen = limit - pos;

	if (sb->s_bdev)
		return ch10fs_blk_read(sb, pos, buf, buflen);

	return -EIO;
}

/*
 * read a page worth of data from the image
 */
static int ch10fs_readpage(struct file *file, struct page *page)
{
	struct inode *inode = page->mapping->host;
	loff_t offset, size;
	unsigned long fillsize, pos;
	void *buf;
	int ret;

	buf = kmap(page);
	if (!buf)
		return -ENOMEM;

	/* 32 bit warning -- but not for us :) */
	offset = page_offset(page);
	size = i_size_read(inode);
	fillsize = 0;
	ret = 0;
	if (offset < size) {
		size -= offset;
		fillsize = size > PAGE_SIZE ? PAGE_SIZE : size;

		pos = CH10FS_I(inode)->i_dataoffset + offset;

		ret = ch10fs_dev_read(inode->i_sb, pos, buf, fillsize);
		if (ret < 0) {
			SetPageError(page);
			fillsize = 0;
			ret = -EIO;
		}
	}

	if (fillsize < PAGE_SIZE)
		memset(buf + fillsize, 0, PAGE_SIZE - fillsize);
	if (ret == 0)
		SetPageUptodate(page);

	flush_dcache_page(page);
	kunmap(page);
	unlock_page(page);
	return ret;
}

static const struct address_space_operations ch10fs_aops = {
	.readpage	= ch10fs_readpage
};


static int ch10fs_dirblock_count(struct super_block *sb) {
	int ret;
	struct ch10fs_dir_block db;
	ret = ch10fs_dev_read(sb, 512, &db, sizeof(db));
	if(ret < 0) return ret;
	int count = 1;
	__be64 cblock = 1;

	while(be64_to_cpu(db.next) != cblock && count < CH10FS_MAX_DIRBLOCKS) {
		ret = ch10fs_dev_read(sb, 512, &db, sizeof(db));
		if(ret < 0) return ret;
		count++;
		cblock = be64_to_cpu(db.next);
	}

	return count;
}

static int ch10fs_get_volume_count(struct super_block *sb) {
	int ret;
	struct ch10fs_dir_block db;	
	ret = ch10fs_dev_read(sb, 512, &db, sizeof(db));
	if(ret < 0) return ret;
	int count = 1;
	__be64 cblock = 1;

	char volname[CH10FS_MAXVOLN];
	strncpy(volname, db.volume, CH10FS_MAXVOLN);
	while(be64_to_cpu(db.next) != cblock && count < CH10FS_MAX_DIRBLOCKS) {
		ret = ch10fs_dev_read(sb, 512, &db, sizeof(db));
		if(ret < 0) return ret;
		if(strncmp(volname, db.volume, CH10FS_MAXVOLN) != 0) {
			strncpy(volname, db.volume, CH10FS_MAXVOLN);
			count++;
		}

		cblock = be64_to_cpu(db.next);
	}

	return count;
}

static int ch10fs_fill_volumes(struct file *filp, void *dirent, filldir_t filldir) {
	int ret;
	struct inode *i = filp->f_dentry->d_inode;
	struct dentry *de = filp->f_dentry;
	struct ch10fs_dir_block db;	
	ret = ch10fs_dev_read(i->i_sb, 512, &db, sizeof(db));
	if(ret < 0) return ret;
	int count = 1;
	__be64 cblock = 1;

	char volname[CH10FS_MAXVOLN];
	strncpy(volname, db.volume, CH10FS_MAXVOLN);

	int ino = 1;
	if(strncmp(volname, "", CH10FS_MAXVOLN) == 0) {
		filldir(dirent, "unnamed", 7, filp->f_pos++, ino, DT_DIR);
	} else {
		filldir(dirent, volname, strnlen(volname, CH10FS_MAXVOLN), filp->f_pos++, ino, DT_DIR);
	}

	while(be64_to_cpu(db.next) != cblock && count < CH10FS_MAX_DIRBLOCKS) {
		ret = ch10fs_dev_read(i->i_sb, 512, &db, sizeof(db));
		if(ret < 0) return ret;
		ino += (CH10_FILES_PER_DIR_ENTRY + 1);
		if(strncmp(volname, db.volume, CH10FS_MAXVOLN) != 0) {
			strncpy(volname, db.volume, CH10FS_MAXVOLN);
			filldir(dirent, volname, strnlen(volname, CH10FS_MAXVOLN), filp->f_pos++, ino, DT_DIR);
		}
		count++;

		cblock = be64_to_cpu(db.next);
	}
}

/*
 * read the entries from a directory
 */
static int ch10fs_readdir(struct file *filp, void *dirent, filldir_t filldir)
{
	struct inode *i = filp->f_dentry->d_inode;
	struct dentry *de = filp->f_dentry;
	struct ch10fs_inode ri;
	struct ch10fs_dir_block db;
	unsigned long offset, maxoff;
	int j, ino, nextfh;
	int stored = 0;
	char fsname[CH10FS_MAXFN]; 
	int ret;

	//	printk(KERN_NOTICE "CH10FS: reading volume count %d\n", ch10fs_get_volume_count(i->i_sb));

	//	ret = ch10fs_dev_read(i->i_sb, 512, &db, sizeof(db));

	printk(KERN_NOTICE "Ch10fs: readddir on inode %d\n", i->i_ino);

	//	printk(KERN_NOTICE "CH10FS: dir block size: %u, file cnt: %hu\n", be32_to_cpu(db.block_size), be16_to_cpu(db.file_cnt));

	if(filp->f_pos > 0 )
		return 1;
	if(filldir(dirent, ".", 1, filp->f_pos++, de->d_inode->i_ino, DT_DIR)||
	   (filldir(dirent, "..", 2, filp->f_pos++, de->d_parent->d_inode->i_ino, DT_DIR)))
		return 0;

	if(i->ino == 0) {	
		ch10fs_fill_volumes(filp, dirent, filldir);
	}

	return 1;
}

/*
 * look up an entry in a directory
 */
static struct dentry *ch10fs_lookup(struct inode *dir, struct dentry *dentry,
				   unsigned int flags)
{
	unsigned long offset, maxoff;
	struct inode *inode;
	struct ch10fs_inode ri;
	const char *name;		/* got from dentry */
	int len, ret;

	printk(KERN_NOTICE "CH10FS: looking up inode %d\n", dir->i_ino);
#if 0
	offset = dir->i_ino;
	ret = ch10fs_dev_read(dir->i_sb, offset, &ri, ROMFH_SIZE);
	if (ret < 0)
		goto error;

	/* search all the file entries in the list starting from the one
	 * pointed to by the directory's special data */
	maxoff = ch10fs_maxsize(dir->i_sb);
	offset = be32_to_cpu(ri.spec) & ROMFH_MASK;

	name = dentry->d_name.name;
	len = dentry->d_name.len;

	for (;;) {
		if (!offset || offset >= maxoff)
			goto out0;

		ret = ch10fs_dev_read(dir->i_sb, offset, &ri, sizeof(ri));
		if (ret < 0)
			goto error;

		/* try to match the first 16 bytes of name */
		ret = ch10fs_dev_strcmp(dir->i_sb, offset + ROMFH_SIZE, name,
				       len);
		if (ret < 0)
			goto error;
		if (ret == 1)
			break;

		/* next entry */
		offset = be32_to_cpu(ri.next) & ROMFH_MASK;
	}

#endif
	inode = ch10fs_iget(dir->i_sb, offset);
	if (IS_ERR(inode)) {
		ret = PTR_ERR(inode);
		goto error;
	}
	goto outi;

	/*
	 * it's a bit funky, _lookup needs to return an error code
	 * (negative) or a NULL, both as a dentry.  ENOENT should not
	 * be returned, instead we need to create a negative dentry by
	 * d_add(dentry, NULL); and return 0 as no error.
	 * (Although as I see, it only matters on writable file
	 * systems).
	 */
out0:
	inode = NULL;
outi:
	d_add(dentry, inode);
	ret = 0;
error:
	return ERR_PTR(ret);
}

static const struct file_operations ch10fs_dir_operations = {
	.read		= generic_read_dir,
	.readdir	= ch10fs_readdir,
	.llseek		= default_llseek,
};

static const struct inode_operations ch10fs_dir_inode_operations = {
	.lookup		= ch10fs_lookup,
};

static struct inode *ch10fs_root_iget(struct super_block *sb)
{
	struct ch10fs_inode_info *inode;
	struct ch10fs_dir_block db;
	struct inode *i;
	unsigned nextfh;
	int ret;
	umode_t mode;

	ret = ch10fs_dev_read(sb, 512, &db, sizeof(db));
	if (ret < 0)
		goto error;

	/* get an inode for this image position */
	i = iget_locked(sb, 0);
	if (!i)
		return ERR_PTR(-ENOMEM);

	if (!(i->i_state & I_NEW))
		return i;

	/* precalculate the data offset */
	//	inode = CH10FS_I(i);
	//inode->i_metasize = sizeof(db);
	//	inode->i_dataoffset = pos + inode->i_metasize;

	set_nlink(i, 1);		/* Hard to decide.. */
	//	i->i_size = be32_to_cpu(ri.size);
	i->i_mtime.tv_sec = i->i_atime.tv_sec = i->i_ctime.tv_sec = 0;
	i->i_mtime.tv_nsec = i->i_atime.tv_nsec = i->i_ctime.tv_nsec = 0;

	/* set up mode and ops */
	mode = S_IFDIR | 0644;

	i->i_size = CH10FS_I(i)->i_metasize;
	i->i_op = &ch10fs_dir_inode_operations;
	i->i_fop = &ch10fs_dir_operations;
	i->i_mode = mode;

	unlock_new_inode(i);
	return i;
	
eio:
	ret = -EIO;
error:
	printk(KERN_ERR "CH10FS: read error for root inode\n");
	return ERR_PTR(ret);
}


/*
 * get a ch10fs inode based on its position in the image (which doubles as the
 * inode number)
 */
static struct inode *ch10fs_iget(struct super_block *sb, unsigned long pos)
{
	struct ch10fs_inode_info *inode;
	struct ch10fs_inode ri;
	struct inode *i;
	unsigned long nlen;
	unsigned nextfh;
	int ret;
	umode_t mode;
#if 0
	ret = ch10fs_dev_read(sb, pos, &ri, sizeof(ri));
	if (ret < 0)
		goto error;

	/* determine the length of the filename */
	nlen = ch10fs_dev_strnlen(sb, pos + ROMFH_SIZE, CH10FS_MAXFN);
	if (IS_ERR_VALUE(nlen))
		goto eio;

	/* get an inode for this image position */
	i = iget_locked(sb, pos);
	if (!i)
		return ERR_PTR(-ENOMEM);

	if (!(i->i_state & I_NEW))
		return i;

	/* precalculate the data offset */
	inode = CH10FS_I(i);
	inode->i_metasize = (ROMFH_SIZE + nlen + 1 + ROMFH_PAD) & ROMFH_MASK;
	inode->i_dataoffset = pos + inode->i_metasize;

	set_nlink(i, 1);		/* Hard to decide.. */
	i->i_size = be32_to_cpu(ri.size);
	i->i_mtime.tv_sec = i->i_atime.tv_sec = i->i_ctime.tv_sec = 0;
	i->i_mtime.tv_nsec = i->i_atime.tv_nsec = i->i_ctime.tv_nsec = 0;

	/* set up mode and ops */
	mode = ch10fs_modemap[nextfh & ROMFH_TYPE];

	switch (nextfh & ROMFH_TYPE) {
	case ROMFH_DIR:
		i->i_size = CH10FS_I(i)->i_metasize;
		i->i_op = &ch10fs_dir_inode_operations;
		i->i_fop = &ch10fs_dir_operations;
		if (nextfh & ROMFH_EXEC)
			mode |= S_IXUGO;
		break;
	case ROMFH_REG:
		i->i_fop = &ch10fs_ro_fops;
		i->i_data.a_ops = &ch10fs_aops;
		if (i->i_sb->s_mtd)
			i->i_data.backing_dev_info =
				i->i_sb->s_mtd->backing_dev_info;
		if (nextfh & ROMFH_EXEC)
			mode |= S_IXUGO;
		break;
		break;
	default:
		/* depending on MBZ for sock/fifos */
		nextfh = be32_to_cpu(ri.spec);
		init_special_inode(i, mode, MKDEV(nextfh >> 16,
						  nextfh & 0xffff));
		break;
	}

	i->i_mode = mode;

	unlock_new_inode(i);
	return i;
#endif
eio:
	ret = -EIO;
error:
	printk(KERN_ERR "CH10FS: read error for inode 0x%lx\n", pos);
	return ERR_PTR(ret);
}

/*
 * allocate a new inode
 */
static struct inode *ch10fs_alloc_inode(struct super_block *sb)
{
	struct ch10fs_inode_info *inode;
	inode = kmem_cache_alloc(ch10fs_inode_cachep, GFP_KERNEL);
	return inode ? &inode->vfs_inode : NULL;
}

/*
 * return a spent inode to the slab cache
 */
static void ch10fs_i_callback(struct rcu_head *head)
{
	struct inode *inode = container_of(head, struct inode, i_rcu);
	kmem_cache_free(ch10fs_inode_cachep, CH10FS_I(inode));
}

static void ch10fs_destroy_inode(struct inode *inode)
{
	call_rcu(&inode->i_rcu, ch10fs_i_callback);
}

/*
 * get filesystem statistics
 */
static int ch10fs_statfs(struct dentry *dentry, struct kstatfs *buf)
{
	struct super_block *sb = dentry->d_sb;
	u64 id = huge_encode_dev(sb->s_bdev->bd_dev);

	buf->f_type = CH10FS_MAGIC;
	buf->f_namelen = CH10FS_MAXFN;
	buf->f_bsize = CH10BSIZE;
	buf->f_bfree = buf->f_bavail = buf->f_ffree;
	buf->f_blocks = 0;//(ch10fs_maxsize(dentry->d_sb) + ROMBSIZE - 1) >> ROMBSBITS;
	buf->f_fsid.val[0] = (u32)id;
	buf->f_fsid.val[1] = (u32)(id >> 32);
	return 0;
}

/*
 * remounting must involve read-only
 */
static int ch10fs_remount(struct super_block *sb, int *flags, char *data)
{
	*flags |= MS_RDONLY;
	return 0;
}

static const struct super_operations ch10fs_super_ops = {
	.alloc_inode	= ch10fs_alloc_inode,
	.destroy_inode	= ch10fs_destroy_inode,
	.statfs		= ch10fs_statfs,
	.remount_fs	= ch10fs_remount,
};

/*
 * fill in the superblock
 */
static int ch10fs_fill_super(struct super_block *sb, void *data, int silent)
{
	struct ch10fs_dir_block *csb;
	struct inode *root;
	unsigned long img_size;
	const char *storage;
	size_t len;
	int ret;

	sb_set_blocksize(sb, CH10BSIZE);
	sb->s_maxbytes = 0xFFFFFFFF;
	sb->s_magic = CH10FS_MAGIC;
	sb->s_flags |= MS_RDONLY | MS_NOATIME;
	sb->s_op = &ch10fs_super_ops;

	/* read the image superblock and check it */
	csb = kmalloc(512, GFP_KERNEL);
	if (!csb)
		return -ENOMEM;

	sb->s_fs_info = (void *) 1024;
	ret = ch10fs_dev_read(sb, 512, csb, 512);
	if (ret < 0)
		goto error_csb;

	//img_size = be32_to_cpu(csb->size);

	sb->s_fs_info = (void *)  0x20000000;//img_size;

	if (csb->word0 != CH10FS_MAGIC_WORD0 || csb->word1 != CH10FS_MAGIC_WORD1) {
		if (!silent)
			printk(KERN_WARNING "VFS:"
			       " Can't find a ch10fs filesystem on dev %s.\n",
			       sb->s_id);
		goto error_csb_inval;
	}

	storage = "the block layer";
	len = strnlen(csb->volume, CH10FS_MAXFN);
	//	if (!silent)
	printk(KERN_NOTICE "CH10FS: Mounting image '%*.*s' through %s\n",
	       (unsigned) len, (unsigned) len, csb->volume, storage);

	kfree(csb);
	csb = NULL;

	/* find the root directory */
	root = ch10fs_root_iget(sb);
	if (IS_ERR(root))
		goto error;

	sb->s_root = d_make_root(root);
	if (!sb->s_root)
		goto error;

	return 0;

error:
	return -EINVAL;
error_csb_inval:
	ret = -EINVAL;
error_csb:
	kfree(csb);
	return ret;
}

/*
 * get a superblock for mounting
 */
static struct dentry *ch10fs_mount(struct file_system_type *fs_type,
			int flags, const char *dev_name,
			void *data)
{
	struct dentry *ret = ERR_PTR(-EINVAL);

	ret = mount_bdev(fs_type, flags, dev_name, data,
			 ch10fs_fill_super);

	return ret;
}

/*
 * destroy a ch10fs superblock in the appropriate manner
 */
static void ch10fs_kill_sb(struct super_block *sb)
{
	if (sb->s_bdev) {
		kill_block_super(sb);
		return;
	}
}

static struct file_system_type ch10fs_fs_type = {
	.owner		= THIS_MODULE,
	.name		= "ch10fs",
	.mount		= ch10fs_mount,
	.kill_sb	= ch10fs_kill_sb,
	.fs_flags	= FS_REQUIRES_DEV,
};

/*
 * inode storage initialiser
 */
static void ch10fs_i_init_once(void *_inode)
{
	struct ch10fs_inode_info *inode = _inode;

	inode_init_once(&inode->vfs_inode);
}

/*
 * ch10fs module initialisation
 */
static int __init init_ch10fs_fs(void)
{
	int ret;

	printk(KERN_INFO "IRIG 106 Ch. 10 Filesystem.\n");

	ch10fs_inode_cachep =
		kmem_cache_create("ch10fs_i",
				  sizeof(struct ch10fs_inode_info), 0,
				  SLAB_RECLAIM_ACCOUNT | SLAB_MEM_SPREAD,
				  ch10fs_i_init_once);

	if (!ch10fs_inode_cachep) {
		printk(KERN_ERR
		       "Ch10fs error: Failed to initialise inode cache\n");
		return -ENOMEM;
	}
	ret = register_filesystem(&ch10fs_fs_type);
	if (ret) {
		printk(KERN_ERR "Ch10fs error: Failed to register filesystem\n");
		goto error_register;
	}
	return 0;

error_register:
	kmem_cache_destroy(ch10fs_inode_cachep);
	return ret;
}

/*
 * ch10fs module removal
 */
static void __exit exit_ch10fs_fs(void)
{
	unregister_filesystem(&ch10fs_fs_type);
	/*
	 * Make sure all delayed rcu free inodes are flushed before we
	 * destroy cache.
	 */
	rcu_barrier();
	kmem_cache_destroy(ch10fs_inode_cachep);
}

module_init(init_ch10fs_fs);
module_exit(exit_ch10fs_fs);

MODULE_DESCRIPTION("IRIG 106 Ch10 Filesystem");
MODULE_AUTHOR("Arthur Walton");
MODULE_LICENSE("GPL");
