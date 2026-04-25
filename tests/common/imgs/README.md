# .img file content documentation

## Common parameters (FAT12, FAT16, FAT32)

- Volume name: "SIMPLEFATFS" (apart from FAT16, which has "SMTHELSE")
- Directory structure: same as that under the `structure/` folder
- `/rootdir/example.txt` is read-only
- `/hidden/` and `/hidden/hidden.txt` are marked as hidden

### Volume labels

|       | BPB         | Root directory |
|-------|-------------|----------------|
| FAT12 | N/A         | HELLOWORLD     |
| FAT16 | SOMETHINGEL | SOMETHINGEL    |
| FAT32 | SIMPLEFATFS | N/A            |
