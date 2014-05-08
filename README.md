IRIG 106 Chapter 10 Filesystem Driver for Linux
=============

Linux filesystem driver for the IRIG 106 Chapter 10 Filesystem. see http://www.irig106.org/ for details of the format.

For kernel version 3.11 and above

Limitations:
- Only supports block size of 512
- Most Ch10 devices have an empty block 0 with no standard partition table, 
  as a result you currently have to manually mount the drive.
- Hasn't yet been tested on > 2GB files
	

