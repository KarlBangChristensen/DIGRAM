%macro DIF(path, outdata=WORK.DIF, ndec=2);
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
		where Exovar ne '';
		drop Item2;
		format p PVALUE6.4;
	run;
%mend DIF;
