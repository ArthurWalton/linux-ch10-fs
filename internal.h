#include "ch10fs.h"

struct ch10fs_inode_info {
	struct inode	vfs_inode;
	unsigned long	i_metasize;	/* size of non-data area */
	unsigned long	i_dataoffset;	/* from the start of fs */
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

/*
 * storage.c
 */
extern int ch10fs_dev_read(struct super_block *sb, unsigned long pos,
			  void *buf, size_t buflen);
extern ssize_t ch10fs_dev_strnlen(struct super_block *sb,
				 unsigned long pos, size_t maxlen);
extern int ch10fs_dev_strcmp(struct super_block *sb, unsigned long pos,
			    const char *str, size_t size);
