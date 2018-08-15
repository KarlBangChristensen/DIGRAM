%macro ARIplot(	path, 
				rows=3, 
				columns=3, 
				CLASS_SIZE=40, 
				deletetemp=YES);
	/**********************************************************************

	path=c:\dropbox\FF\DIGRAM

	path=&path\DIGR

	**********************************************************************/
	options nonotes;

	proc import datafile = "&path\Ari_dot.csv"
				out = _ari
				dbms = dlm
				replace; 
				delimiter = ";";
				GETNAMES = YES;
				guessingrows=32767;
	run;
	* collaps score groups;
	proc sql;
		create table _margdist as
		select Score, n as frequency
		from _ari
		where ItemNo=1;
	quit;
	data _collaps;
		set _margdist end=last;
		retain _sum1(0);
		retain interval(1);
		if _sum1 ge &CLASS_SIZE then do; 
			interval=interval+1; 
			_sum1=0;  
		end;
		_sum1=_sum1+frequency;
		if last then do;
			call symput('_maxint',interval);
			call symput('_maxsum',_sum1);
		end;
	run;
	* check last score group is big enough;
	data _collaps; 
		set _collaps; 
		_maxsum=&_maxsum; 
		CLASS_SIZE=&CLASS_SIZE; 
		if (interval=&_maxint) and (_maxsum<CLASS_SIZE) then do;
			interval=&_maxint-1; 
		end;
	run;
	proc sql;
		create table _ari2 as
		select *
		from _ari as A, _collaps as C
		where A.Score=C.Score;
	quit;
	proc sql;
		create table pl as
		select 	ItemNo, 
				Item, 
				Interval, 
				sum(n*ObsMean)/sum(n) as O,
				sum(n) as _N, 
				sum(n*ExpMean)/sum(n) as E, 
				sum(n*ExpVar)/sum(n) as V
		from _ari2
		group by Item, interval
		order by Item, interval
		;
	quit;
	proc sort data=pl nodupkey;
		by item interval;
	run;
	* maximum value of 'interval';
	proc sql;
		select max(interval) into :__maxint from pl;
	quit;
	data pl;
		set pl;
		lower=E-1.96*sqrt(V/_N);
		upper=E+1.96*sqrt(V/_N);
	run;
	proc sgpanel data=pl;
		panelby ItemNo / rows=&rows columns=&columns noheader spacing=10;
		inset Item / position=topleft;
		series x=interval y=O / lineattrs=(thickness=2 color=grey);
		band x=interval lower=lower upper=upper / 	legendlabel='95% CI'
													transparency=0.5;
		colaxis values=(1 to &__maxint);
		label O='Mean item score';
		label interval='class interval';
	run;
	* clean-up;
	%if (&deletetemp='YES') %then %do;
		proc datasets noprint;
			delete _ari _ari2;
		quit;
	%end;
	options notes;
%mend ARIplot;	

