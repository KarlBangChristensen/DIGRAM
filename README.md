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

- A data file with the data saved as comma separated values in a [csv file](https://github.com/KarlBangChristensen/DIGRAM/blob/master/DIGRAM.csv)
- A file with information about the variables in the data set saved as a [imv file](https://github.com/KarlBangChristensen/DIGRAM/wiki/The-imv-file)
- A command script. This can be saved as configuration options in an [ini file](https://github.com/KarlBangChristensen/DIGRAM/wiki/The-ini-file) or as a command script in an [cmd file](https://github.com/KarlBangChristensen/DIGRAM/wiki/The-command-script)

Having created these files DIGRAM can be used to do the analyses specified in the file `C:\Dropbox\GRM\DIGRAM.ini` or `C:\Dropbox\GRM\DIGRAM.cmd`. 

The files are created like this: first the SAS macro `DIGR.sas` is downloaded from GitHub and included

```
%let url=https://raw.githubusercontent.com/KarlBangChristensen/DIGRAM/master;
filename DIGR url "&url/DIGR.sas";
%include DIGR;
```

next, the SAS macro is used. We need to specify the dataset, the list of items, the list of exogenous variables, and the location of the file scd.exe `c:\dropbox\GRM`. 

```
%DIGR();
```

