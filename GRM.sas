%macro GRM(path, outdata=WORK.GRM);
	data &outdata;
		infile "&path\GRM.txt";
	run;
%mend GRM;
