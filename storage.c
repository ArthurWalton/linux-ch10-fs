#include <linux/fs.h>
#include <linux/mtd/super.h>
#include <linux/buffer_head.h>
#include "internal.h"

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
		if (!bh)
			return -EIO;
		memcpy(buf, bh->b_data + offset, segment);
		brelse(bh);
		buf += segment;
		buflen -= segment;
		pos += segment;
	}

	return 0;
}

/*
 * determine the length of a string in ch10fs on a block device
 */
static ssize_t ch10fs_blk_strnlen(struct super_block *sb,
				 unsigned long pos, size_t limit)
{
	struct buffer_head *bh;
	unsigned long offset;
	ssize_t n = 0;
	size_t segment;
	u_char *buf, *p;

	/* scan the string up to blocksize bytes at a time */
	while (limit > 0) {
		offset = pos & (CH10BSIZE - 1);
		segment = min_t(size_t, limit, CH10BSIZE - offset);
		bh = sb_bread(sb, pos >> CH10BSBITS);
		if (!bh)
			return -EIO;
		buf = bh->b_data + offset;
		p = memchr(buf, 0, segment);
		brelse(bh);
		if (p)
			return n + (p - buf);
		limit -= segment;
		pos += segment;
		n += segment;
	}

	return n;
}

/*
 * compare a string to one in a ch10fs image on a block device
 * - return 1 if matched, 0 if differ, -ve if error
 */
static int ch10fs_blk_strcmp(struct super_block *sb, unsigned long pos,
			    const char *str, size_t size)
{
	struct buffer_head *bh;
	unsigned long offset;
	size_t segment;
	bool matched, terminated = false;

	/* compare string up to a block at a time */
	while (size > 0) {
		offset = pos & (CH10BSIZE - 1);
		segment = min_t(size_t, size, CH10BSIZE - offset);
		bh = sb_bread(sb, pos >> CH10BSBITS);
		if (!bh)
			return -EIO;
		matched = (memcmp(bh->b_data + offset, str, segment) == 0);

		size -= segment;
		pos += segment;
		str += segment;
		if (matched && size == 0 && offset + segment < CH10BSIZE) {
			if (!bh->b_data[offset + segment])
				terminated = true;
			else
				matched = false;
		}
		brelse(bh);
		if (!matched)
			return 0;
	}

	if (!terminated) {
		/* the terminating NUL must be on the first byte of the next
		 * block */
		BUG_ON((pos & (CH10BSIZE - 1)) != 0);
		bh = sb_bread(sb, pos >> CH10BSBITS);
		if (!bh)
			return -EIO;
		matched = !bh->b_data[0];
		brelse(bh);
		if (!matched)
			return 0;
	}

	return 1;
}


/*
 * read data from the ch10fs image
 */
int ch10fs_dev_read(struct super_block *sb, unsigned long pos,
		   void *buf, size_t buflen)
{
	size_t limit;

	limit = ch10fs_maxsize(sb);
	if (pos >= limit)
		return -EIO;
	if (buflen > limit - pos)
		buflen = limit - pos;

	if (sb->s_bdev)
		return ch10fs_blk_read(sb, pos, buf, buflen);

	return -EIO;
}

/*
 * determine the length of a string in ch10fs
 */
ssize_t ch10fs_dev_strnlen(struct super_block *sb,
			  unsigned long pos, size_t maxlen)
{
	size_t limit;

	limit = ch10fs_maxsize(sb);
	if (pos >= limit)
		return -EIO;
	if (maxlen > limit - pos)
		maxlen = limit - pos;

	if (sb->s_bdev)
		return ch10fs_blk_strnlen(sb, pos, maxlen);

	return -EIO;
}

/*
 * compare a string to one in ch10fs
 * - the string to be compared to, str, may not be NUL-terminated; instead the
 *   string is of the specified size
 * - return 1 if matched, 0 if differ, -ve if error
 */
int ch10fs_dev_strcmp(struct super_block *sb, unsigned long pos,
		     const char *str, size_t size)
{
	size_t limit;

	limit = ch10fs_maxsize(sb);
	if (pos >= limit)
		return -EIO;
	if (size > CH10FS_MAXFN)
		return -ENAMETOOLONG;
	if (size + 1 > limit - pos)
		return -EIO;

	if (sb->s_bdev)
		return ch10fs_blk_strcmp(sb, pos, str, size);

	return -EIO;
}
