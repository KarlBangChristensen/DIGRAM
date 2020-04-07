%macro DIGR(data, 
			idvar, 
			items, 
			exo, 
			folder, 
			model,
			personpar=NO,
			reduce=NO,
			LD=NO,
			DIF=NO,
			GlobalTests=NO,
			ItemFits=NO,
			ARI=NO,
			name=DIGRAM, 
			deletetemp=YES,
			commandfile=YES);
	
	/**********************************************************************

	generating &name.ini file in stead of the file &name.cmd makes DIGRAM
	run analyses automatically (do not have to press enter)

	Options:

    0 : Reduce model
    1 : tests of local independence
    2 : tests of no DIF 
    3 : global test of homogeneity
    4 : analysis of local homogeneity
    5 : global tests of no DIF
    6 : analysis of local DIF
    7 : tests of unidimensionality
    8 : analysis of item correlations
    9 : item fit statistics
   10 : estimate person parameters
   11 : show sufficient margins of the model
   12 : show sufficient margins of all models that have been fitted in the current step
   13 : show estimates of all models that have been fitted in the current step
   14 : generate output for latent regression
   15 : generate output for ICC curves
   16 : Include incomplete item responses
   17 : Analysis of targeting

  100 : Output on text files
  101 : Extended output
  102 : Save person estimates 
			equated total scores in case of DIF, 
			imputed total scores in case of missing item responses 

  999 : Saves the output on ItemAnalysis.txt and exits from DIGRAM

	data: the data set	
	idvar: person identification variable
	items: list of items
	exo: list of exogenous variables
	folder: the folder where the file scd.exe is located
	commandfile: commandfile=NO for interactive analysis using the DIGRAM GUI 
	name: default name=DIGRAM
	ARI: if YES - compute only the ARI-file (ARI=NO is default)

	**********************************************************************/

	options nonotes;

	********************************************
	* save variable names as macro variables
	********************************************;
	data _null_; 
		set &data;
		array _it (*) &items; 
		array _ex (*) &exo; 
		length ___name $30; 
		if _n_=1 then do;
			do _i=1 to dim(_it);
				call vname(_it{_i},___name); 
				call symput('_item'||trim(left(put(_i,4.))),trim(left(___name)));
			end;
			_p=dim(_it); 
			call symput('_nitems',trim(left(put(_p,4.))));
			do _i=1 to dim(_ex);
				call vname(_ex{_i},___name); 
				call symput('_exo'||trim(left(put(_i,4.))),trim(left(___name)));
			end;
			_p=dim(_ex); 
			call symput('_nexo',trim(left(put(_p,4.))));
		end;
	run;
	%do _i=1 %to &_nitems;
		proc sql noprint;
			select min(&&_item&_i), max(&&_item&_i) 
			into :_min&_i, :_max&_i 
			from &data;
		quit;
	%end;

	data temp;
		gmin=%sysfunc(
		min(&_min1 %do _i=2 %to &_nitems; , &&_min&_i %end;)
		);
		gmax=%sysfunc(
		max(&_min1 %do _i=2 %to &_nitems; , &&_max&_i %end;)
		);
	run; 

	proc sql noprint;
		select gmin, gmax 
		into :_gmin, :_gmax 
		from temp;
	run;

	%let _gmin=&_gmin;
	%let _gmax=&_gmax;

	%do _e=1 %to &_nexo;
		proc sql noprint;
			select min(&&_exo&_e), max(&&_exo&_e) 
			into :_exomin&_e, :_exomax&_e 
			from &data;
		quit;
	%end;

	****************************
	* item labels
	****************************;
	%let _1=a;	%let _2=b;	%let _3=c;	%let _4=d;	%let _5=e;
	%let _6=f;	%let _7=g;	%let _8=h;	%let _9=i;	%let _10=j;
	%let _11=k;	%let _12=l;	%let _13=m;	%let _14=n;	%let _15=o;
	%let _16=p;	%let _17=q;	%let _18=r;	%let _19=s;	%let _20=t;
	%let _21=u;	%let _22=v;	%let _23=w;	%let _24=x;	%let _25=y;
	%let _26=z;	%let _27=A;	%let _28=B;	%let _29=C;	%let _30=D;
	%let _31=E;	%let _32=F;	%let _33=G;	%let _34=H;	%let _35=I;
	%let _36=J;	%let _37=K;	%let _38=L;	%let _39=M;	%let _40=N;
	%let _41=O;	%let _42=P;	%let _43=Q;	%let _44=R;	%let _45=S;
	%let _46=T;	%let _47=U;	%let _48=V;	%let _49=W;	%let _50=X;

	****************************
	* category labels        
	****************************;
	%let _lab0=zero; 		%let _lab1=one; 		%let _lab2=two; 	%let _lab3=three; 		%let _lab4=four; 	
	%let _lab5=five;		%let _lab6=six; 		%let _lab7=seven; 	%let _lab8=eight; 		%let _lab9=nine;
	%let _lab9=nine;		%let _lab10=ten; 		%let _lab11=eleven;	%let _lab12=twelve;		%let _lab13=thirteen;
	%let _lab14=fourteen;	%let _lab15=fifteen;	%let _lab16=sixteen;%let _lab17=seventeen;	%let _lab18=eighteen;

	*****************************
	* item list
	*****************************;
	%let itlist=;
	%do _i=1 %to &_nitems;
		%let itlist=&itlist&&_&_i;
	%end;

	*****************************
	* exo list
	*****************************;
	%let exolist=;
	%do _e=1 %to &_nexo;
		%let _y=%eval(&_nitems+&_e);
		%put _y is &_y;
		%let exolist=&exolist&&_&_y;
	%end;

	*******************************
	* write something in log
	*******************************;
	%put ----------------------------------;
	%put data set &data, &_nitems items, &_nexo exogenous variables;
	%put ----------------------------------;
	%put itlist is &itlist, exolist is &exolist;

	***********************************
	* csv file
	***********************************;
	data _out;
		set &data;
		id=&idvar;
		keep 
		id 
		%do _i=1 %to &_nitems; &&_item&_i %end;
		%do _e=1 %to &_nexo; &&_exo&_e %end;
		;
	run;
	proc export data=_out
				outfile="&folder\&name..csv" 
				dbms=csv
				replace;
	run;

	***********************************
	* imp file
	***********************************;
	data _imp;
		var="&folder.\               ";	output;
		var="&name"; 				output;
		var="-"; output;
		var="&name..cmd"; 			output;
	run;
	data _null_;
		set _imp;
		file "&folder\&name..imp";
		put VAR;
	run;

	***********************************
	* imv file
	***********************************;
	data _imv;
		VAR='                                                 ';
		%do _i=1 %to &_nitems;
			%let line=&&_&_i..,&&_item&_i..;
			%do _x=&_gmin %to &_gmax;
				%let line=&line.,&_x.,&&_lab&_x..;
			%end;
			var="&line"; 
			output;
		%end;
		%do _e=1 %to &_nexo;
			%let _j=%eval(&_e+&_nitems);
			%let line=&&_&_j..,&&_exo&_e..;
			%do _y=&&_exomin&_e %to &&_exomax&_e;
				%let line=&line.,&_y.,&&_lab&_y..;
			%end;
			var="&line";
			output;
		%end;
	run;
	data _null_;
		set _imv;
		file "&folder\&name..imv" notitles;
		put @1 VAR;
	run;
	***********************************
	* cmd file
	***********************************;
	%let line= ;
	%if %upcase(&reduce.)^=NO %then %do;
		%let line=&line 0;
	%end;
	%if %upcase(&LD.)^=NO %then %do;
		%let line=&line 1;
	%end;
	%if %upcase(&DIF.)^=NO %then %do;
		%let line=&line 2;
	%end;
	%if %upcase(&GlobalTests.)^=NO %then %do;
		%let line=&line 3 5;
	%end;
	%if %upcase(&ItemFits.)^=NO %then %do;
		%let line=&line 9;
	%end;
	%if %upcase(&personpar.)^=NO %then %do;
		%let line=&line 10;
	%end;
	%if %upcase(&ARI.)^=NO %then %do;
		%let line=&line 18;
	%end;
	%if %upcase(&commandfile.)^=NO %then %do;
		%let line=&line 100 999;
		%if %upcase(&model.)^=SCREEN %then %do;
			data _cmd;
				var="ITEMS &itlist                          "; output;
				var="EXO &exolist                           "; output;
				var="LLR &model                             "; output;
				var="ITA 0 &line                            "; output;
			run;
		%end;
		%else %do;
			data _cmd;
				var="ITEMS &itlist                         "; output;
				var="EXO &exolist                          "; output;
				var="SCR I                                 "; output;
				var="ITA 0 &line                           "; output;
			run;
		%end;
		data _null_;
			set _cmd;
			* saving this file as name.ini is probably better;
			file "&folder\&name..cmd";
			put VAR;
		run;
		data _null_;
			set _cmd;
			* saving this file as name.ini is probably better;
			file "&folder\&name..ini";
			put VAR;
		run;
	%end;

	********************************************
	* clean-up
	********************************************;
	%if (&deletetemp='YES') %then %do;
		proc datasets noprint;
			delete _cmd _collaps _imp _imv _out;
		quit;
	%end;
	options notes;

%mend DIGR;

