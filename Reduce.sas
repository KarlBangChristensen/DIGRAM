%macro Reduce(path, outdata=WORK.Reduce);
	proc import out=&outdata
	            datafile="&path\Reduce.txt" 
	            dbms=dlm replace;
	     delimiter='20'x; 
	     getnames=YES;
	     datarow=2; 
	run;
	data &outdata;
		set &outdata;
		format p PVALUE6.4;
	run;
%mend Reduce;
