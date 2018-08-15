%macro ItemFits(path, outdata=WORK.ItemFits, ndec=2);
	proc import out=&outdata
	            datafile="&path\ItemFits.txt" 
	            dbms=dlm replace;
	     delimiter='20'x; 
	     getnames=YES;
	     datarow=2; 
	run;
	data &outdata;
		set &outdata;
		format outfit infit ObsGamma ExpGamma 8.&ndec; 
		format p_outfit p_infit p_gamma PVALUE6.4;
	run;
%mend ItemFits;
