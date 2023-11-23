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
| Write border sep..  | 878         |
| Black to Black 1D   | 742         |
| Next is white 1D    | 713         |
| B2B from white 1D   | 702         |
| B2B from white 2D   | 684         |

# Cycle counts per part
Cycle counts for processing `cellImage`:
| part                | Cycle count |
| ------------------- | ----------- |
| border              | 75          |
| Merge loop + border | 1277        |
| Merge loop + pixel  | 953         |
| Write border sep..  | 878         |
| Black to Black 1D   | 742         |
| Next is white 1D    | 713         |
| B2B from white 1D   | 702         |
| B2B from white 2D   | 684         |