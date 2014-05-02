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

	printk(KERN_INFO "ch10fs: readpage\n");

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
	int ret, count;
	struct ch10fs_dir_block db;
	__be64 cblock;

	ret = ch10fs_dev_read(sb, 512, &db, sizeof(db));
	if(ret < 0) return ret;
	
	cblock = 1;
	count = 1;
	while(be64_to_cpu(db.next) != cblock && count < CH10FS_MAX_DIRBLOCKS) {
		ret = ch10fs_dev_read(sb, 512, &db, sizeof(db));
		if(ret < 0) return ret;
		count++;
		cblock = be64_to_cpu(db.next);
	}

	return count;
}

static int ch10fs_get_volume_count(struct super_block *sb) {
	int ret, count;
	u64 cblock;
	struct ch10fs_dir_block db;	
	char volname[CH10FS_MAXVOLN];

	ret = ch10fs_dev_read(sb, 512, &db, sizeof(db));
	if(ret < 0) return ret;
	count = 1;
	cblock = 1;
	
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
/*
 * read the entries from a directory
 */
static int ch10fs_readdir(struct file *filp, void *dirent, filldir_t filldir)
{
	int ret, count = 1;
	struct ch10fs_dir_block db;	
	u64 cblock = 1;
	char volname[CH10FS_MAXVOLN];
	unsigned int ino = 1;
	int stored = 0;
	struct inode *i = filp->f_dentry->d_inode;
	struct dentry *de = filp->f_dentry;

	printk(KERN_NOTICE "Ch10fs: readddir on inode %lu, position %lu\n", i->i_ino, filp->f_pos);

       	if(filp->f_pos > 0 )
	       	return 1;
	if(filldir(dirent, ".", 1, 1, de->d_inode->i_ino, DT_DIR)||
	   (filldir(dirent, "..", 2, 2, de->d_parent->d_inode->i_ino, DT_DIR)))
	  return 0;

	//	ret = filldir(dirent, ".", 1, filp->f_pos, de->d_inode->i_ino, DT_DIR);
	//	printk(KERN_INFO "ch10fs: filling in '.' dir entry with ino %lu\n", de->d_inode->i_ino);
	//	if(ret) return 0;

	//	filp->f_pos++;

	//	ret = filldir(dirent, "..", 2, filp->f_pos++, de->d_parent->d_inode->i_ino, DT_DIR);
	//	printk(KERN_INFO "ch10fs: filling in '..' dir entry with ino %lu\n", de->d_parent->d_inode->i_ino);
	//	if(ret) return 0;

	//	filp->f_pos++;

	ret = ch10fs_dev_read(i->i_sb, 512, &db, sizeof(db));
	if(ret < 0) return 0;

	strncpy(volname, db.volume, CH10FS_MAXVOLN);
	if(strncmp(volname, "", CH10FS_MAXVOLN) == 0) {
		if(filldir(dirent, "unnamed", 7, ino, ino, DT_DIR) < 0)
			goto out;
		printk(KERN_INFO "ch10fs: filling in '%s' dir entry with ino %lu\n", "unnamed", ino);
		filp->f_pos = ino;
		stored++;
	} else {
		if(filldir(dirent, volname, strnlen(volname, CH10FS_MAXVOLN), ino, ino, DT_DIR) < 0)
			goto out;
		printk(KERN_INFO "ch10fs: filling in '%s' dir entry with ino %lu\n", volname, ino);

		filp->f_pos = ino;
		stored++;
	}

	while(be64_to_cpu(db.next) != cblock && count < CH10FS_MAX_DIRBLOCKS) {
		ret = ch10fs_dev_read(i->i_sb, 512, &db, sizeof(db));
		if(ret < 0) return ret;

		ino += (CH10_FILES_PER_DIR_ENTRY + 1);
		if(strncmp(volname, db.volume, CH10FS_MAXVOLN) != 0) {
			strncpy(volname, db.volume, CH10FS_MAXVOLN);
			if(filldir(dirent, volname, strnlen(volname, CH10FS_MAXVOLN), ino, ino, DT_DIR) < 0) 
				goto out;
			  
			printk(KERN_INFO "ch10fs: filling in '%s' dir entry with ino %lu\n", volname, ino);
			filp->f_pos = ino;

			stored++;
		}
		count++;

		cblock = be64_to_cpu(db.next);
	}

 out:
	return stored;
}

/*
 * returns the inode number of the directory with the specified name
 */
static int ch10fs_lookup_dir_ino(struct super_block *sb, const char *name) {
	struct ch10fs_dir_block db;
	u64 cblock = 1;	
	int ino = 1, ret;
	printk(KERN_INFO "ch10fs: looking up ino for dir '%s' unnamed? %d\n", name, strncmp(name, "unnamed", CH10FS_MAXVOLN));

	ret = ch10fs_dev_read(sb, 512, &db, sizeof(db));
	if(ret < 0) return ret;

	while(be64_to_cpu(db.next) != cblock && ino < CH10FS_MAX_DIRBLOCKS) {
		ret = ch10fs_dev_read(sb, 512, &db, sizeof(db));
		if(ret < 0) return ret;

		printk(KERN_INFO "ch10fs: examining volume '%s'\n", db.volume);
		
		if(strncmp(name, db.volume, CH10FS_MAXVOLN) == 0 ||
		   (strnlen(db.volume, CH10FS_MAXVOLN) == 0 
		    && strncmp(name, "unnamed", CH10FS_MAXVOLN) == 0)) {
			printk(KERN_INFO "ch10fs: found ino %d for dir %s\n", ino, name);
			return ino;
		}		
		
		cblock = be64_to_cpu(db.next);
		ino += (CH10_FILES_PER_DIR_ENTRY + 1);
	}

	return -EIO;
}

/*
 * returns the inode number of the file with the specified name in the volume
 */
static int ch10fs_lookup_file_ino(struct super_block *sb, const char* volume, const char *name) {
	struct ch10fs_dir_block db;
	u64 cblock = 1;
	int ino = 1, ret, i;

	printk(KERN_INFO "ch10fs: lookup_file_ino\n");

	ret = ch10fs_dev_read(sb, 512, &db, sizeof(db));
	if(ret < 0) return ret;

	while(be64_to_cpu(db.next) != cblock && ino < CH10FS_MAX_DIRBLOCKS) {
		ret = ch10fs_dev_read(sb, 512, &db, sizeof(db));
		if(ret < 0) return ret;
		
		for(i = 0; i < db.file_cnt; i++) {
			if(strncmp(name, db.entries[i].name, CH10FS_MAXFN) == 0) {
				return ino + i + 1;
			}   
		}
		
		cblock = be64_to_cpu(db.next);
		ino += (CH10_FILES_PER_DIR_ENTRY + 1);
	}

	return -EIO;
}

/*
 * look up an entry in a directory
 */
static struct dentry *ch10fs_lookup(struct inode *dir, struct dentry *dentry,
				   unsigned int flags)
{
	unsigned long index;
	struct inode *inode;
	const char *name;		/* got from dentry */
	int len, ret;

	printk(KERN_NOTICE "CH10FS: looking up '%s' in inode %lu\n", name = dentry->d_name.name, dir->i_ino);

	index = dir->i_ino;
	/* Walk the directory entry list, if we are at the root then we look for a volume,
	   otherwise we look for a file */

	name = dentry->d_name.name;
	len = dentry->d_name.len;

	if(index == 0) {
		index = ch10fs_lookup_dir_ino(dir->i_sb, name);
		if(index < 0)
			goto error;
	} else {
		index = ch10fs_lookup_file_ino(dir->i_sb, "", name);
	}

	inode = ch10fs_iget(dir->i_sb, index);
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
	struct ch10fs_dir_block db;
	struct inode *i;
	int ret;
	umode_t mode;

	printk(KERN_INFO "ch10fs: root_iget\n");

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
	
error:
	printk(KERN_ERR "CH10FS: read error for root inode\n");
	return ERR_PTR(ret);
}

static struct inode *ch10fs_dir_iget(struct super_block *sb, unsigned long ino) {
	struct ch10fs_dir_block db;
	struct inode *i;
	unsigned long cblock = 1;
	int index = 1, ret;

	printk(KERN_INFO "ch10fs: dir iget for ino %lu\n", ino);

	ret = ch10fs_dev_read(sb, 512, &db, sizeof(db));
	if(ret < 0) return ERR_PTR(ret);

	while(be64_to_cpu(db.next) != cblock && index < CH10FS_MAX_DIRBLOCKS) {
		ret = ch10fs_dev_read(sb, 512, &db, sizeof(db));
		if(ret < 0) return ERR_PTR(ret);

		if(index == ino) {
			/* get an inode for this image position */
			i = iget_locked(sb, ino);
			if (!i)
				return ERR_PTR(-ENOMEM);

			if (!(i->i_state & I_NEW))
				return i;

			/* precalculate the data offset */

			set_nlink(i, 1);		/* Hard to decide.. */
			i->i_size = 0;
			i->i_mtime.tv_sec = i->i_atime.tv_sec = i->i_ctime.tv_sec = 0;
			i->i_mtime.tv_nsec = i->i_atime.tv_nsec = i->i_ctime.tv_nsec = 0;

			/* set up mode and ops */
			i->i_size = 0;
			i->i_op = &ch10fs_dir_inode_operations;
			i->i_fop = &ch10fs_dir_operations;
			i->i_mode = S_IFDIR  | 0644;

			unlock_new_inode(i);
			return i;
		}
		
		cblock = be64_to_cpu(db.next);
		ino += (CH10_FILES_PER_DIR_ENTRY + 1);
	}

	return ERR_PTR(-EIO);
}


static struct inode *ch10fs_file_iget(struct super_block *sb, unsigned long ino) {
	struct ch10fs_dir_block db;
	struct inode *i;
	unsigned long cblock = 1;
	int index = 1, ret, j;

	printk(KERN_INFO "ch10fs: file_iget on ino %lu\n", ino);

	ret = ch10fs_dev_read(sb, 512, &db, sizeof(db));
	if(ret < 0) return ERR_PTR(ret);

	while(be64_to_cpu(db.next) != cblock && index < CH10FS_MAX_DIRBLOCKS) {
		ret = ch10fs_dev_read(sb, 512, &db, sizeof(db));
		if(ret < 0) return ERR_PTR(ret);

		for(j = 0; j < db.file_cnt; j++) {
			if((index + j + 1) != ino) continue;
				
			/* get an inode for this image position */
			i = iget_locked(sb, ino);
			if (!i)
				return ERR_PTR(-ENOMEM);
			
			if (!(i->i_state & I_NEW))
				return i;
			
			set_nlink(i, 1);		/* Hard to decide.. */
			i->i_size = be32_to_cpu(db.entries[j].size);
			i->i_mtime.tv_sec = i->i_atime.tv_sec = i->i_ctime.tv_sec = 0;
			i->i_mtime.tv_nsec = i->i_atime.tv_nsec = i->i_ctime.tv_nsec = 0;
			
			/* set up mode and ops */
			i->i_fop = &ch10fs_ro_fops;
			i->i_data.a_ops = &ch10fs_aops;
			i->i_mode = S_IFREG  | 0644;
			
			unlock_new_inode(i);
			return i;
		}
		
		cblock = be64_to_cpu(db.next);
		ino += (CH10_FILES_PER_DIR_ENTRY + 1);
	}

	return ERR_PTR(-EIO);
}


/*
 * get a ch10fs inode based on its inode number
 */
static struct inode *ch10fs_iget(struct super_block *sb, unsigned long ino)
{
  int is_a_dir = ino == 0 || ino == 1 || ino % (CH10_FILES_PER_DIR_ENTRY + 1) == 0;
  if(is_a_dir) {
		return ch10fs_dir_iget(sb, ino);
	} else {
		return ch10fs_file_iget(sb, ino);
	}
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
	buf->f_blocks = 1000;//(ch10fs_maxsize(dentry->d_sb) + ROMBSIZE - 1) >> ROMBSBITS;
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
