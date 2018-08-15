%macro ICCplot(	Thresholds=WORK.Thresholds, 
				rows=3, 
				columns=3,
				outdata=WORK.ICCplot);

	**********************************************************************
	reads data set with thresholds and plots ICCs (item means and item 
	category probability curves). The x-axis (latent variable) is computed
	as (lowest threshold)-1 to (highest threshold+1). 

	**********************************************************************
	;


	OPTIONS MPRINT;


	* number of items;
	proc sql noprint;
		select count(item) into :_nitems from &Thresholds;
	quit;
	%let _nitems=&_nitems.;
	* number of thresholds;
	proc datasets noprint;
		contents data=WORK.Thresholds out=__var;
	quit;
	proc sql noprint;
		select count(unique(name)) into :_nthres from __var where name NE 'item';
	quit;
	%let _nthres=&_nthres.;
	* item names;
	proc sql noprint;
	  select item
	  into :_item1-:_item&_nitems
	  from &Thresholds;
	quit;
	* range;
	proc sql noprint;
		select 
			floor(min(min(thres1 %do j=2 %to &_nthres; ,thres&j %end;)))-2,
			ceil(max(max(thres1 %do j=2 %to &_nthres; ,thres&j %end;)))+2
			into :xmin, :xmax
		from &Thresholds;
	quit;
	%let xmin=&xmin.;
	%let xmax=&xmax.;
	%put Threshold range is &xmin to &xmax;
	* compute probabilities;
	proc sql noprint;
		create table _ as select *, 0 as eta0
		%do l=1 %to &_nthres; 
			, (0-thres1 %do ll=2 %to &l; -thres&ll %end;) as eta&l 
		%end;
		from &Thresholds;
	quit;
	data __;
		set _;
		%do l=0 %to &_nthres;
			if not(missing(eta&l)) then do;
				score=&l;
				eta=eta&l;
				output;
			end;
		%end;
	run;
	data ___;
		set __;
			do theta=%eval(&xmin.) to %eval(&xmax.) by 0.01; 
			output; 
		end;
		keep item score eta theta;
	run;
	proc sql;
		create table &outdata as 
		select * , exp(score*theta+eta)/(sum(exp(score*theta+eta))) as prob
		from ___
		group by item, theta
		order by item, score, theta;
	quit;
	*means; 
	proc sql;
		create table means as 
		select item, theta, sum(score*prob) as mean
		from &outdata 
		group by item, theta
		order by item, theta;
	quit;
	* plots;
	proc sgpanel data=&outdata;
		panelby item / rows=&rows columns=&columns NOVARNAME;
		series x=theta y=prob / group=score;
		inset item;
	run;
	proc sgplot data=&outdata;
		series x=theta y=prob / group=score;
		by item;
	run;
	proc sgpanel data=means;
		panelby item / rows=&rows columns=&columns NOVARNAME;
		series x=theta y=mean;
		inset item;
	run;
	* remove temporary data set;
	proc datasets noprint;
		delete _ __ ___;
	quit;
%mend ICCplot;
