/* Ch10Fs internal definitions
 *
 * Copyright © 2007 Arthur Walton (arthur.walton2@gmail.com)
 * All Rights Reserved.
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License
 * as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 */

#include "ch10fs.h"

struct ch10fs_inode_info {
	struct inode	vfs_inode;
	unsigned long	i_metasize;	/* size of non-data area */
	unsigned long	i_datablock;	/* from the start of fs */
};

static inline size_t ch10fs_maxsize(struct super_block *sb)
{
	return (size_t) (unsigned long) sb->s_fs_info;
}

static inline struct ch10fs_inode_info *CH10FS_I(struct inode *inode)
{
	return container_of(inode, struct ch10fs_inode_info, vfs_inode);
}

#define ch10fs_ro_fops	generic_ro_fops

