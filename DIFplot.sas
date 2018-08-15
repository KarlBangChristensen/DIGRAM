%macro DIFplot(	path, 
				DIFvar,
				DIFformat=,
				outdata=WORK.DIFplot);
	/**********************************************************************

				requires personparameter-output: 
					&path\DIGRAM-persons.txt;

				* works with a single DIF-variable coded 1,2,3, ..;

	**********************************************************************/
	options nonotes;
	%let DIFvar=&DIFvar.;
	proc import out=_dat
	            datafile="&path\PersonLocations.txt" 
	            dbms=dlm replace;
	     delimiter='20'x; 
	     getnames=YES;
	     datarow=2; 
	run;
	* number of DIF-strata;
	proc sql;
		select count(unique(&DIFvar.)), min(&DIFvar.), max(&DIFvar.) 
		into :nexo, :min, :max 
		from _dat;
	quit;
	%let nexo=&nexo.;
	%let min=&min.;
	%let max=&max.;
	%put *********************************************;
	%put DIF-variable &DIFvar. takes &nexo values ranging from &min to &max;
	%put *********************************************;
	proc sgplot data=_dat;
		series x=score y=Escore / lineattrs=(thickness=2 color=grey) group=&DIFvar;
		label score='Score in reference group';
		label Escore='Equated score';
		format &DIFvar &DIFformat..;
	run;
	data &outdata;
		merge 
		%do s=&min %to &max;
			_dat(where=(&difvar=&s) rename=(Escore=Escore&s))
		%end;
		;
		label score="Score";
		%do s=&min %to &max;
			label Escore&s="Equated score group &s";
		%end;
		keep score Escore&min-Escore&max;
	run;
	* clean-up;
	proc datasets noprint;
*		delete _dat;
	quit;
	options notes;
%mend DIFplot;	
