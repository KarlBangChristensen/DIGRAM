%macro Thresholds(path, outdata=WORK.Thresholds);
	options nonotes;
	proc import out=&outdata
	            datafile="&path\Thresholds.txt" 
	            dbms=dlm replace;
	     delimiter='20'x; 
	     getnames=NO;
	     datarow=1; 
	run;
	* number of thresholds;
	proc datasets noprint;
		contents data=WORK.Thresholds out=__var;
	quit;
	proc sql noprint;
		select count(unique(name)) into :_nthres from __var;
	quit;
	%let _nthres=&_nthres.;
	%let k=%eval(&_nthres.-2);
	data &outdata;
		set &outdata;
		item=VAR1;
		array _thres[*] VAR3-VAR&_nthres.;
		array thres[*] thres1-thres&k.;
		do l=1 to &k;
			thres[l]=_thres[l];
		end;
		drop VAR1-var&_nthres. l;
	run;
	proc datasets noprint;
		delete __var;
	quit;
	options notes;
%mend Thresholds;


