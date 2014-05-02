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

#define CH10FS_MAGIC_WORD0 __mk4('F','O','R','T')
#define CH10FS_MAGIC_WORD1 __mk4('Y','t','w','o')

struct ch10fs_file_entry {
	u8 name[56];       
	__be64 blockNum;      
	__be64 numBlocks;     
	__be64 size;          
	u8 createDate[8];  
	u8 createTime[8];  
	u8 timeType;       
	u8 reserved[7];    
	u8 closeTime[8];   
};

struct ch10fs_dir_block {
	__be32 word0;
	__be32 word1;
	u8 rev;
	u8 shutdown;
	__be16 file_cnt;
	__be32 block_size;
	char volume[CH10FS_MAXVOLN];
	__be64 next;
	__be64 prev;
	struct ch10fs_file_entry entries[];
};

#endif
