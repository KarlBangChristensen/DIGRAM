# DIGRAM

DIGRAM is a stand alone software package running under windows (*.exe file). This repository contains SAS macros that can be used to export data to DIGRAM, create batch files, submit batch files to DIGRAM, and read DIGRAM output files.


## Example

To do item analysis using DIGRAM you need to download the file scd.exe and save it somewhere on you computer. We will assume that the file has been saved in the folder `c:\dropbox\GRM`. We consider the data set `SASUSER.example` containing 8 polytomous items (scored 0,1,2) and two binary exogenous variables (person factors):

```
ID it22 it23 it24 it25 it26 it27 it28 it29 woman over40 
1 1 1 2 2 1 1 1 1 1 0 
2 1 1 1 0 1 2 2 2 1 0 
3 1 1 0 1 1 1 1 1 0 1 
4 1 1 1 2 2 2 1 1 0 1 
5 1 1 1 2 1 1 2 0 1 1 
:
```

to analyze this data set using DIGRAM we need to create a number of files:

- A command/ini script
- An imv file
- a csv file






### The ini file

Saved as `C:\Dropbox\GRM\DIGRAM.ini` the ini file looks like the same as the command file



"C:\Dropbox\COHQ\GRM\DIGRAM.ini"
