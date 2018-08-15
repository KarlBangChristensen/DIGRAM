# DIGRAM

DIGRAM is a stand alone software package running under windows (*.exe file). This repository contains SAS macros that can be used to export data to DIGRAM, create batch files, submit batch files to DIGRAM, and read DIGRAM output files.


## Example

To do item analysis using DIGRAM you need to download the file scd.exe and save it somewhere on you computer. We will assume that the file has been saved in the folder `c:\dropbox\GRM`. We consider the data set `SASUSER.example` containing 8 polytomous items (scored 0,1,2) and two binary exogenous variables (person factors):

```
it22 it23 it24 it25 it26 it27 it28 it29 woman over40 
1 1 2 2 1 1 1 1 1 0 
1 1 1 0 1 2 2 2 1 0 
1 1 0 1 1 1 1 1 0 1 
1 1 1 2 2 2 1 1 0 1 
1 1 1 2 1 1 2 0 1 1 
:
```

to analyze this data set using DIGRAM we need to create a number of files

### The command script

Saved as `C:\Dropbox\GRM\DIGRAM.cmd` the command file looks like this

```
ITEMS abcdefgh
EXO ij
LLR
ITA 0 3 5 9 100 999
```

### The imv file

Saved as `C:\Dropbox\GRM\DIGRAM.imv` the imv file looks like this

```
a,it22,0,zero,1,one,2,two
b,it23,0,zero,1,one,2,two
c,it24,0,zero,1,one,2,two
d,it25,0,zero,1,one,2,two
e,it26,0,zero,1,one,2,two
f,it27,0,zero,1,one,2,two
g,it28,0,zero,1,one,2,two
h,it29,0,zero,1,one,2,two
i,woman,0,zero,1,one
j,over40,0,zero,1,one
```

note that variables used in the item analysis are labeled using the letters `a`, `b`, ...

### The ini file



"C:\Dropbox\COHQ\GRM\DIGRAM.ini"
