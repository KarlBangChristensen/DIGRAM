%macro PersonLocations(path, outdata=WORK.PersonLocations);
	proc import out=&outdata
	            datafile="&path\PersonLocations.txt" 
	            dbms=dlm replace;
	     delimiter='20'x; 
	     getnames=YES;
	     datarow=2; 
	run;
	data &outdata;
		set &outdata;
	run;
%mend PersonLocations;
