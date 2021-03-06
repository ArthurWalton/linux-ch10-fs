/* Ch10Fs header
 *
 * Copyright © 2007 Arthur Walton (arthur.walton2@gmail.com)
 * All Rights Reserved.
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License
 * as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 */
#ifndef __LINUX_CH10FS_FS_H
#define __LINUX_CH10FS_FS_H

#include <linux/types.h>
#include <linux/fs.h>

/* The basic structures of the ch10fs filesystem */

#define CH10BSIZE BLOCK_SIZE
#define CH10BSBITS BLOCK_SIZE_BITS
#define CH10BMASK (CH10BSIZE-1)
#define CH10FS_MAGIC 0x42424242
#define CH10_FILES_PER_DIR_ENTRY 4

#define CH10FS_MAXVOLN 32
#define CH10FS_MAXFN 56
#define CH10FS_MAX_DIRBLOCKS 10000
#define __mkw(h,l) (((h)&0x00ff)<< 8|((l)&0x00ff))
#define __mkl(h,l) (((h)&0xffff)<<16|((l)&0xffff))
#define __mk4(a,b,c,d) cpu_to_be32(__mkl(__mkw(a,b),__mkw(c,d)))

#define CH10FS_MAGIC_W0 __mk4('F','O','R','T')
#define CH10FS_MAGIC_W1 __mk4('Y','t','w','o')

struct ch10fs_file_entry {
	u8 name[56];
	__be64 start_block;
	__be64 block_cnt;
	__be64 size;
	u8 cdate[8];
	u8 ctime[8];
	u8 time_type;  
	u8 reserved[7];
	u8 close_time[8];
};

struct ch10fs_dir_block {
	__be32 mag0;
	__be32 mag1;
	u8 rev;
	u8 shutdown;
	__be16 file_cnt;
	__be32 block_size;
	u8 volume[CH10FS_MAXVOLN];
	__be64 next;
	__be64 prev;
};

#endif
