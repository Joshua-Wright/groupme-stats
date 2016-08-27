# groupme-stats
program to download messages from a GroupMe group and make interesting graphs

## Current Outputs:
* `likesReceivedByUser.dat`
* `likesGivenByUser.dat`
* `usagePerHour.dat`
* `usagePerTimePerUser.dat`
* `allRawText.dat`
* `wordFrequency.dat`
* `allLikesGivenByUserToUser.dat`
* `allMessageLengths.dat`

```
Usage: groupme-stuff COMMAND [ARGS...]
Commands:
    stats FILE                             runs statistics on the file FILE
    downloadAll API_KEY GROUP_NAME FILE    dumps the group GROUP_NAME dat to FILE
    update API_KEY FILE                    updates the group data for FILE
```
