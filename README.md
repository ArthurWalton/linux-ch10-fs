IRIG 106 Chapter 10 Filesystem Driver for Linux
=============
IRIG 106 Chapter 10 is a filesystem used by flight recorders and is designed to work on a large variety of media (SSD, Tape, disks, etc.). see http://www.irig106.org/ for details of the format.

For kernel version 3.11 and above

Limitations:
- Read only
- Only supports block size of 512
- Most Ch10 devices have an empty block 0 with no standard partition table, 
  as a result most drives will not auto-mount.	

