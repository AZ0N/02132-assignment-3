# Assignment 3
**Course:** 02132 Computer Systems
## Testing
```cmd
sbt "test:runMain SystemTopTester"
```
# Cycle counts
Cycle counts for processing `cellImage`:
| Version             | Cycle count |
| ------------------- | ----------- |
| Initial             | 1658        |
| Merge loop + border | 1277        |
| Merge loop + pixel  | 953         |