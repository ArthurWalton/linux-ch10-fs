#ifndef __LINUX_CH10FS_FS_H
#define __LINUX_CH10FS_FS_H

#include <linux/types.h>
#include <linux/fs.h>

/* The basic structures of the ch10fs filesystem */

#define CH10BSIZE 512
#define CH10BSBITS BLOCK_SIZE_BITS
#define CH10BMASK (CH10BSIZE-1)
#define CH10FS_MAGIC 0x42424242

#define CH10FS_MAXVOLN 32
#define CH10FS_MAXFN 56

#define __mkw(h,l) (((h)&0x00ff)<< 8|((l)&0x00ff))
#define __mkl(h,l) (((h)&0xffff)<<16|((l)&0xffff))
#define __mk4(a,b,c,d) cpu_to_be32(__mkl(__mkw(a,b),__mkw(c,d)))

#define CH10FS_MAGIC_WORD0 __mk4('F','O','R','T')
#define CH10FS_MAGIC_WORD1 __mk4('Y','t','w','o')

struct ch10fs_inode {
	__be32 next;
	__be32 spec;
	__be32 size;
	__be32 checksum;
	char name[0];
};


struct ch10fs_super_block {
	__be32 word0;
	__be32 word1;
	__be32 size;
	__be32 checksum;
	char name[0];		/* volume name */
};



/*
 * Ch10 Directory Entry
 */
typedef struct {
  //! name of the directory entry
  u8 name[56];       
  //! block number that the entry starts at
  u64 blockNum;      
  //! length of the entry in blocks
  u64 numBlocks;     
  //! length of the entry in bytes
  u64 size;          
  //! date entry was created
  u8 createDate[8];  
  //! time entry was created
  u8 createTime[8];  
  //! time system the previous date and time were stored in
  u8 timeType;       
  //! currently unused, reserved for future use
  u8 reserved[7];    
  //! time this entry was finished being written
  u8 closeTime[8];   
} ch10_dir_entry;

/*
 * Ch10 Directory Block   
 */
typedef struct {
  //! Identifies this as being a directory block, always set to FORTYtwo
  u8 magicNumAscii[8];  
  //! revision number of the data recording standard in use
  u8 revNum;            
  //! flag to indicate filesystem was not properly shutdown while writing to this directory
  u8 shutdown;          
  //! number of directory entries/files that are in this block
  u16 numEntries;       
  //! number of bytes per block
  u32 bytesPerBlock;    
  //! name of this directory block
  u8 volName[32];       
  //! block address of next directory block
  u64 forwardLink:64;   
  //! block address of previous directory block
  u64 reverseLink:64;   
  //! all entries/files in the block
  //  ch10_dir_entry dirEntries[MAX_FILES_PER_DIR]; 
} ch10_dir_block;

#endif
