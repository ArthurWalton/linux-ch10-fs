/* IRIG 106 Chapter 10 filesystem driver
 *
 * Copyright © 2007 Arthur Walton (arthur.walton2@gmail.com)
 * All Rights Reserved.
 *
 * Derived from: ROMFS file system
 * Copyright © 2007 Red Hat, Inc. All Rights Reserved.
 * written by David Howells (dhowells@redhat.com)
 *
 * using parts of the ROMFS file system, Linux implementation
 * Copyright © 1997-1999  Janos Farkas <chexum@shadow.banki.hu>
 *
 * and parts of the minix filesystem
 * Copyright © 1991, 1992  Linus Torvalds
 *
 * and parts of the affs filesystem additionally
 * Copyright © 1993  Ray Burr
 * Copyright © 1996  Hans-Joachim Widmaier
 *
 * Changes
 *	May 2014			Initial release
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public Licence
 * as published by the Free Software Foundation; either version
 * 2 of the Licence, or (at your option) any later version.
 */

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

static struct inode *ch10fs_iget(struct super_block *sb, int pos);

/*
 * read data from an ch10fs image on a block device
 */
static int ch10fs_blk_read(struct super_block *sb, loff_t pos,
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
int ch10fs_dev_read(struct super_block *sb, loff_t pos,
		   void *buf, size_t buflen)
{
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
	loff_t offset, size, pos;
	size_t fillsize;
	void *buf;
	int ret;

	buf = kmap(page);
	if (!buf) {
		printk(KERN_INFO "ch10fs: no memory to allocate page\n");
		return -ENOMEM;
	}

	offset = page_offset(page);
	size = i_size_read(inode);
	fillsize = 0;
	ret = 0;

	if (offset < size) {
		size -= offset;
		fillsize = size > PAGE_SIZE ? PAGE_SIZE : size;
		pos = (CH10FS_I(inode)->i_datablock * 512) + offset;
		ret = ch10fs_dev_read(inode->i_sb, pos, buf, fillsize);
		if (ret < 0) {
			printk(KERN_INFO "ch10fs: error reading page from disk\n");
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

/*
 * length limited verson of kstrtoint
 */
int __must_check kstrntoint(const char *s, unsigned int base, unsigned int *res, size_t size) {
	char tmp[size + 1];
	memcpy(tmp, s, size);
	tmp[size] = '\0';
	return kstrtoint(tmp, base, res);
}

/*
 * convert a ch10 date and time entry to a timespec
 */
static struct timespec ch10fs_fs_datetime_to_timespec(u8 *createDate, u8 *createTime) {
	int day, mon, year, hour, min, sec, hsec;
	struct timespec t;
	kstrntoint(createDate, 10, &day, 2);
	kstrntoint(createDate + 2, 10, &mon, 2);
	kstrntoint(createDate + 4, 10, &year, 4);

	kstrntoint(createTime, 10, &hour, 2);
	kstrntoint(createTime + 2, 10, &min, 2);
	kstrntoint(createTime + 4, 10, &sec, 2);
	kstrntoint(createTime + 8, 10, &hsec, 2);

	t.tv_sec = mktime(year, mon, day, hour, min, sec);
	t.tv_nsec = hsec * 10000000;
	
	printk(KERN_INFO "ch10fs: day-%d, month-%d, year-%d, "
	       "hour-%d, minute-%d, second-%d, hs-%d\n",
	       day, mon, year, hour, min, sec, hsec);

	return t;
}

/*
 * fills the context with list of files from the directory dir_ino
 */
static bool ch10fs_fill_volume_files(struct file *file, struct dir_context *ctx, int dir_ino) {
	int ret, len, ino, f;
	struct ch10fs_dir_block db;
	struct ch10fs_file_entry fe;
	u64 cblock;
	u64 offset;
	char volname[CH10FS_MAXVOLN];
	struct inode *i = file_inode(file);

	printk(KERN_INFO "ch10fs: listing files in ino %d\n", dir_ino);

	for(cblock = 0, db.next = cpu_to_be64(1), ino = 1;
	    be64_to_cpu(db.next) != cblock;
	    ino += (CH10_FILES_PER_DIR_ENTRY + 1)) {
		cblock = be64_to_cpu(db.next);
		ret = ch10fs_dev_read(i->i_sb, cblock * 512, &db, sizeof(db));
		if(ret < 0) 
			goto out;

		if (db.word0 != CH10FS_MAGIC_WORD0 || db.word1 != CH10FS_MAGIC_WORD1) {
			printk(KERN_WARNING "ch10fs:"
			       " tried to read block %llu with invalid magic.\n",
			       cblock);
			goto out;
		}

		/* found the dir, copy its name so we can find the rest of it */
		if(ino == dir_ino) {
			strncpy(volname, db.volume, CH10FS_MAXVOLN);
			printk(KERN_INFO "ch10fs: found volume '%s' at ino %d\n", volname, ino);
		}

		/* not at the right dir block yet */
		if(ctx->pos >= ino + CH10_FILES_PER_DIR_ENTRY) {
			printk(KERN_INFO "ch10fs: ino of %d less than target of %lu\n"
			       , ino + CH10_FILES_PER_DIR_ENTRY, (unsigned long)ctx->pos);
			continue;
		}

		if(strncmp(volname, db.volume, CH10FS_MAXVOLN) != 0) {
			printk(KERN_INFO "ch10fs: current volume '%s' doesnt match expected '%s'\n"
			       , db.volume, volname);
			continue;
		}

		offset = cblock * 512 + sizeof(db);
		for(f = 0; f < be16_to_cpu(db.file_cnt); f++) {
			ret = ch10fs_dev_read(i->i_sb, offset, &fe, sizeof(fe));
			if(ret < 0) 
				goto out;
			offset += sizeof(fe);

			/* not at the right file entry yet */
			if(ino + f + 1 <= ctx->pos) continue; 

			len = strnlen(fe.name, CH10FS_MAXFN);
			if (!dir_emit(ctx, fe.name, len, ino + f + 1, DT_REG))
				goto out;
			
			printk(KERN_INFO "ch10fs: filling in '%s' file entry with ino %u\n", 
			       fe.name, ino + f + 1);
			ctx->pos = ino + f + 1;
		}
	}

 out:
	return 0;
}

/*
 * fills the context with a list of all directories, 
 * ch10 does not support nested directories
 */
static bool ch10fs_fill_directories(struct file *file, struct dir_context *ctx) {
	int ret, len, ino, offset;
	struct ch10fs_dir_block db;
	char volname[CH10FS_MAXVOLN];
	u64 cblock;
	struct inode *i = file_inode(file);
	
	dir_emit_dots(file, ctx);

	offset = 2;
	for(cblock = 0, db.next = cpu_to_be64(1), ino = 1;
	    be64_to_cpu(db.next) != cblock;
	    ino += (CH10_FILES_PER_DIR_ENTRY + 1)) {
		cblock = be64_to_cpu(db.next);

		ret = ch10fs_dev_read(i->i_sb, cblock * 512, &db, sizeof(db));
		if(ret < 0) 
			goto out;

		if (db.word0 != CH10FS_MAGIC_WORD0 || db.word1 != CH10FS_MAGIC_WORD1) {
			printk(KERN_WARNING "ch10fs:"
			       " tried to read block %llu with invalid magic.\n",
			       cblock);
			goto out;
		}

		/* if the dir name is blank we will give it a name of 'unnamed' */
		if(strncmp("", db.volume, CH10FS_MAXVOLN) == 0) {
			strncpy(db.volume, "unnamed", CH10FS_MAXVOLN);
		}

		if(strncmp(volname, db.volume, CH10FS_MAXVOLN) == 0) continue;
		strncpy(volname, db.volume, CH10FS_MAXVOLN);
		offset++;

		if(ctx->pos >= offset) continue;

		len = strnlen(volname, CH10FS_MAXVOLN);
		if (!dir_emit(ctx, volname, len, ino, DT_DIR))
			goto out;
		
		printk(KERN_INFO "ch10fs: filling in '%s' dir entry with ino %u\n", volname, ino);
		ctx->pos++;

	}

 out:
	return 0;
}

/*
 * read the entries from a directory
 */
static int ch10fs_iterate(struct file *file, struct dir_context *ctx)
{
	struct inode *i = file_inode(file);
	printk(KERN_NOTICE "ch10fs: iterate on inode %lu, position %lu\n", 
	       i->i_ino, (unsigned long)ctx->pos);

	/* only the root (ino 0) has directories */
	if(i->i_ino == 0) {
		ch10fs_fill_directories(file, ctx);		
	} else {
		ch10fs_fill_volume_files(file, ctx, i->i_ino);
	}

	return 0;
}

/*
 * returns the inode number of the directory with the specified name
 */
static long ch10fs_lookup_dir_ino(struct super_block *sb, const char *volname) {
	int ret, ino;
	struct ch10fs_dir_block db;
	u64 cblock;

	for(cblock = 0, db.next = cpu_to_be64(1), ino = 1;
	    be64_to_cpu(db.next) != cblock;
	     ino += (CH10_FILES_PER_DIR_ENTRY + 1)) {
		cblock = be64_to_cpu(db.next);

		ret = ch10fs_dev_read(sb, cblock * 512, &db, sizeof(db));
		if(ret < 0) 
			goto out;

		if (db.word0 != CH10FS_MAGIC_WORD0 || db.word1 != CH10FS_MAGIC_WORD1) {
			printk(KERN_WARNING "ch10fs:"
			       " tried to read block %llu with invalid magic.\n",
			       cblock);
			goto out;
		}

		if(strncmp(volname, db.volume, CH10FS_MAXVOLN) == 0 ||
		   (strnlen(db.volume, CH10FS_MAXVOLN) == 0 
		    && strncmp(volname, "unnamed", CH10FS_MAXVOLN) == 0)) {
			printk(KERN_INFO "ch10fs: found ino %d for dir %s\n", ino, volname);
			return ino;
		}		
	}

 out:
	return -EIO;
}

/*
 * returns the inode number of the file with the specified name in the volume
 */
static int ch10fs_lookup_file_ino(struct super_block *sb, int dir_ino, const char *filename) 
{
	int ret, ino, f;
	struct ch10fs_dir_block db;
	struct ch10fs_file_entry fe;
	u64 cblock;
	u64 offset;
	char volname[CH10FS_MAXVOLN];

	printk(KERN_INFO "ch10fs: looking for file '%s' on volume '%d'\n", filename, dir_ino);

	for(cblock = 0, db.next = cpu_to_be64(1), ino = 1;
	    be64_to_cpu(db.next) != cblock;
	     ino += (CH10_FILES_PER_DIR_ENTRY + 1)) {
		cblock = be64_to_cpu(db.next);
		ret = ch10fs_dev_read(sb, cblock * 512, &db, sizeof(db));
		if(ret < 0) 
			goto out;

		if (db.word0 != CH10FS_MAGIC_WORD0 || db.word1 != CH10FS_MAGIC_WORD1) {
			printk(KERN_WARNING "ch10fs:"
			       " tried to read block %llu with invalid magic.\n",
			       cblock);
			goto out;
		}

		if(ino == dir_ino) {
			strncpy(volname, db.volume, CH10FS_MAXVOLN);
			printk(KERN_INFO "ch10fs: found volume '%s' for file '%s'\n", volname, filename);
		}

		if(strncmp(volname, db.volume, CH10FS_MAXVOLN) != 0) continue;

		offset = cblock * 512 + sizeof(db);
		for(f = 0; f < be16_to_cpu(db.file_cnt); f++) {
			ret = ch10fs_dev_read(sb, offset, &fe, sizeof(fe));
			if(ret < 0) 
				goto out;
			offset += sizeof(fe);
			
			if(strncmp(filename, fe.name, CH10FS_MAXVOLN) != 0) continue;

			printk(KERN_INFO "ch10fs: found ino %u for file '%s'\n", 
			       ino + f + 1, filename);

			return ino + f + 1;
		}
	}
out:
	return -EIO;
}

/*
 * look up an entry in a directory
 */
static struct dentry *ch10fs_lookup(struct inode *dir, struct dentry *dentry,
				   unsigned int flags)
{
	long index;
	struct inode *inode;
	const char *name;		/* got from dentry */
	int len, ret;

	index = dir->i_ino;
	/* Walk the directory entry list, if we are at the root then we look for a volume,
	   otherwise we look for a file */

	name = dentry->d_name.name;
	len = dentry->d_name.len;

	/* if this belongs to the root (ino 0) then it is a directory */
	if(index == 0) {
		index = ch10fs_lookup_dir_ino(dir->i_sb, name);
		if(index < 0) {
			printk(KERN_INFO "ch10fs: failed to find ino\n");
			return ERR_PTR(index);
		}
	/* if it doesn't belong to root then it is a file */
	} else {
		index = ch10fs_lookup_file_ino(dir->i_sb, dir->i_ino, name);
		if(index < 0)
			return ERR_PTR(index);
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
	.iterate	= ch10fs_iterate,
	.llseek		= default_llseek,
};

static const struct inode_operations ch10fs_dir_inode_operations = {
	.lookup		= ch10fs_lookup,
};

/*
 * creates the root inode (ino 0)
 */
static struct inode *ch10fs_root_iget(struct super_block *sb)
{
	struct ch10fs_dir_block db;
	struct inode *i;
	int ret;

	ret = ch10fs_dev_read(sb, 512, &db, sizeof(db));
	if (ret < 0)
		goto error;

	/* get an inode for this image position */
	i = iget_locked(sb, 0);
	if (!i)
		return ERR_PTR(-ENOMEM);

	if (!(i->i_state & I_NEW))
		return i;

	set_nlink(i, 1);		/* Hard to decide.. */
	i->i_mtime.tv_sec = i->i_atime.tv_sec = i->i_ctime.tv_sec = 0;
	i->i_mtime.tv_nsec = i->i_atime.tv_nsec = i->i_ctime.tv_nsec = 0;
	// TODO: give the root directory a creation time of the first file in it

	i->i_size = 0;
	i->i_op = &ch10fs_dir_inode_operations;
	i->i_fop = &ch10fs_dir_operations;
	i->i_mode = S_IFDIR | 0555;

	unlock_new_inode(i);
	return i;
	
error:
	printk(KERN_ERR "CH10FS: read error for root inode\n");
	return ERR_PTR(ret);
}

/*
 * creates a directory inode for the given inode number
 */
static struct inode *ch10fs_dir_iget(struct super_block *sb, unsigned long target_ino) {
	int ret, ino;
	struct ch10fs_dir_block db;
	struct inode *i;
	u64 cblock;

	printk(KERN_INFO "ch10fs: file_iget on ino %lu\n", target_ino);

	for(cblock = 0, db.next = cpu_to_be64(1), ino = 1;
	    be64_to_cpu(db.next) != cblock;
	     ino += (CH10_FILES_PER_DIR_ENTRY + 1)) {
		cblock = be64_to_cpu(db.next);
		ret = ch10fs_dev_read(sb, cblock * 512, &db, sizeof(db));
		if(ret < 0) 
			goto error;

		if (db.word0 != CH10FS_MAGIC_WORD0 || db.word1 != CH10FS_MAGIC_WORD1) {
			printk(KERN_WARNING "ch10fs:"
			       " tried to read block %llu with invalid magic.\n",
			       cblock);
			goto error;
		}

		if(ino == target_ino) {
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
			i->i_mode = S_IFDIR  | 0555;

			unlock_new_inode(i);
			return i;
		}
	}

error:
	return ERR_PTR(-EIO);
}

/*
 * creates a file inode for the given inode number
 */
static struct inode *ch10fs_file_iget(struct super_block *sb, unsigned long target_ino) {
	int ret, ino, f;
	struct ch10fs_inode_info *inode;
	struct ch10fs_dir_block db;
	struct ch10fs_file_entry fe;
	struct inode *i;
	struct timespec ctime;
	u64 cblock;
	u64 offset;

	printk(KERN_INFO "ch10fs: file_iget on ino %lu\n", target_ino);

	for(cblock = 0, db.next = cpu_to_be64(1), ino = 1;
	    be64_to_cpu(db.next) != cblock;
	     ino += (CH10_FILES_PER_DIR_ENTRY + 1)) {
		cblock = be64_to_cpu(db.next);
		ret = ch10fs_dev_read(sb, cblock * 512, &db, sizeof(db));
		if(ret < 0) 
			goto error;

		if (db.word0 != CH10FS_MAGIC_WORD0 || db.word1 != CH10FS_MAGIC_WORD1) {
			printk(KERN_WARNING "ch10fs:"
			       " tried to read block %llu with invalid magic.\n",
			       cblock);
			goto error;
		}

		offset = (cblock * 512) + sizeof(db);
		printk(KERN_WARNING "ch10fs:"
		       " looking through %hu files on block %llu.\n",
		       be16_to_cpu(db.file_cnt), cblock);
		for(f = 0; f < be16_to_cpu(db.file_cnt); f++, offset += sizeof(fe)) {
			printk(KERN_WARNING "ch10fs:"
			       " at file %d, looking for %lu.\n",
			       (ino + f + 1), target_ino);
			
			if((ino + f + 1) != target_ino) continue;

			printk(KERN_WARNING "ch10fs:"
			       " found file %lu.\n", target_ino);

			ret = ch10fs_dev_read(sb, offset, &fe, sizeof(fe));
			if(ret < 0) 
				goto error;
				
			/* get an inode for this image position */
			i = iget_locked(sb, target_ino);
			if (!i)
				return ERR_PTR(-ENOMEM);
			
			if (!(i->i_state & I_NEW))
				return i;

			printk(KERN_WARNING "ch10fs:"
			       " found file information for ino %lu, size %llu at block %llu.\n",
			       target_ino, be64_to_cpu(fe.size), be64_to_cpu(fe.start_block));

			/* precalculate the data offset */
			inode = CH10FS_I(i);
//			inode->i_metasize = (ROMFH_SIZE + nlen + 1 + ROMFH_PAD) & ROMFH_MASK;
			inode->i_datablock = be64_to_cpu(fe.start_block);
			
			set_nlink(i, 1);		/* Hard to decide.. */
			i->i_size = min(be64_to_cpu(fe.size), be64_to_cpu(fe.block_cnt) * 512);

			ctime =	ch10fs_fs_datetime_to_timespec(fe.cdate, fe.ctime);

			i->i_mtime = i->i_atime = i->i_ctime = ctime;
			
			/* set up mode and ops */
			i->i_fop = &ch10fs_ro_fops;
			i->i_data.a_ops = &ch10fs_aops;
			i->i_mode = S_IFREG  | 0644;
			
			unlock_new_inode(i);
			return i;
		}
	}

error:
	return ERR_PTR(-EIO);
}


/*
 * get a ch10fs inode based on its inode number
 */
static struct inode *ch10fs_iget(struct super_block *sb, int ino)
{
	int is_a_dir = ino == 0 || ino == 1 
		|| ino % (1 + (CH10_FILES_PER_DIR_ENTRY + 1)) == 0;
	printk(KERN_INFO "ch10fs: ch10fs_iget for ino %d\n", ino);
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
	if (!silent)
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
