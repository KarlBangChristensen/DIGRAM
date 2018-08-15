%macro FittedModel(path, outdata=WORK.FittedModel);
	proc import out=&outdata
	            datafile="&path\FittedModel.txt" 
	            dbms=dlm replace;
	     delimiter='20'x; 
	     getnames=NO;
	     datarow=1; 
	run;
	/*
	data &outdata;
		set &outdata;
		format p PVALUE6.4;
	run;
	*/
%mend FittedModel;
