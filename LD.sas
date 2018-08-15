%macro LD(path, outdata=WORK.LD, ndec=2);
	proc import out=&outdata
	            datafile="&path\CheckLIandDIF.txt" 
	            dbms=dlm replace;
	     delimiter='20'x; 
	     getnames=YES;
		 guessingrows=20000;
	     datarow=2; 
	run;
	data &outdata;
		set &outdata;
		where Exovar='';
		drop Exovar;
		format p PVALUE6.4;
	run;
%mend LD;
