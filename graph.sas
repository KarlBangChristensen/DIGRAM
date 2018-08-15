%macro graph(path);
	options nonotes;
	ods escapechar='~';
	* reads "path\Variables.txt" and saves it as WORK.variables;
	proc import out=variables
	            datafile="&path\Variables.txt" 
	            dbms=dlm replace;
	     delimiter='20'x; 
	     getnames=NO;
	     datarow=1; 
	run;
	data _items _exo;
		set variables;
		if VAR1='ITE' then output _items;
		if VAR1='EXO' then output _exo;
	run;
	* reads "Path\FittedModel.txt" and saves it as WORK._model;
	proc import out=_model
	            datafile="&path\FittedModel.txt" 
	            dbms=dlm replace;
	     delimiter='20'x; 
	     getnames=NO;
	     datarow=1; 
	run;
	* items and exogenous variables;
	proc sql noprint;
		select count(unique(VAR2)) into :_nitems from _items;
		select count(unique(VAR2)) into :_nexo from _exo;
	quit;
	%let _nitems=&_nitems.;
	%let _nexo=&_nexo.;
	%put &_nitems. items and &_nexo. exogenous variable(s);
	proc sql noprint;
		select VAR2 into :_item1-:_item&_nitems from _items;
		select VAR3 into :_it_m1-:_it_m&_nitems from _items;
		select VAR2 into :_exo1-:_exo&_nexo from _exo;
		select VAR3 into :_ex_1-:_ex_&_nexo from _exo;
	quit;
   	* interaction terms;
	proc datasets noprint;
		contents data=WORK._model out=_inter;
	quit;
	proc sql noprint;
		select count(unique(name)) into :_ninter from _inter where LENGTH=2;
	quit;
	%let _ninter=&_ninter.;
	data _inter;
		set _model;
		%do _inter=1 %to &_ninter;
			%let _inter=&_inter.;
			if (VAR&_inter='') then do;
			end;
			else do;
				inter=VAR&_inter;
				output;
			end;
		%end;
	run;
	data _inter;
		set _inter;
		inter1=substr(inter,1,1);
		inter2=substr(inter,2,1);
		keep inter inter1 inter2;
	run;
	proc sql noprint;
		select inter into :_inter1-:_inter&_ninter from _inter;
		select inter1 into :_i1_1-:_i1_&_ninter from _inter;
		select inter2 into :_i2_1-:_i2_&_ninter from _inter;
	quit;
	%put &_ninter interactions;
	%do _inter=1 %to &_ninter;
		%put &&_inter&_inter (&&_i1_&_inter,&&_i2_&_inter);
	%end;
	* graph template;
	proc template;
	define statgraph GRM;
		begingraph / DesignHeight=500 DesignWidth=700;
			entrytitle "Graphical Rasch Model - latent variable ~{unicode theta}";
			layout lattice;
	   			layout region;
					pathdiagram fromid=from toid=to /
					arrangement=GRIP
					nodeid=id
					nodetitle=id
					nodeshape=shape
					nodepriority=np
					linkdirection=direct
					nodeColorGroup=col
					textSizemin=1
					DIAGRAMSCALE=0.5;
				endlayout;
				sidebar / align=right;
        		layout overlay / pad=(left=2px);
          			layout gridded / columns=1 border=true;
						entry halign=left "Items";
						%do _i=1 %to &_nitems;
							entry halign=left "&&_item&_i" halign=right "&&_it_m&_i";
						%end;
						entry halign=left "Exo";
						%do _e=1 %to &_nexo;
							entry halign=left "&&_exo&_e" halign=right "&&_ex_&_e";
						%end;
				endlayout;
	        endlayout;
      endsidebar;
    endlayout;
		endgraph;
	end;
	run;
	* latent variable;
	data __lv;
		from="theta"; _nodeType_='INPUT'; col=2; id="theta"; np=2; shape='OVAL'; output;
	run;
	* items;
	data __it;
		%do _i=1 %to &_nitems;
			from="&&_item&_i"; _nodeType_='INPUT'; col=1; id="&&_item&_i"; np=1; shape='OVAL'; output;
		%end;
	run;
	data __arrows1;
		%do _i=1 %to &_nitems;
			from="theta"; to="&&_item&_i"; direct='TO               '; output;
		%end;
	run;
	* exogenous variables;
	data __exo;
		%do _e=1 %to &_nexo;
			from="&&_exo&_e"; _nodeType_='INPUT'; col=3; id="&&_exo&_e"; np=3; shape='OVAL'; output;
		%end;
	run;
	data __arrows2;
		%do _e=1 %to &_nexo;
			from="theta"; to="&&_exo&_e"; direct='FROMTO           '; output;
		%end;
	run;
	* inetractions;
	data __arrows3;
		%do _inter=1 %to &_ninter;
			from="&&_i1_&_inter"; to="&&_i2_&_inter"; direct='FROMTO           '; output;
		%end;
	run;
	* code graph;
	data GRM;
		set __lv __it __arrows1 __arrows2 __arrows3 __exo;
	run;
	* delete temporary data sets;
	proc datasets noprint;
		delete __lv __it __arrows1 __arrows2 __arrows3 __exo;
	quit;
	* print graph;
	ods graphics;
	proc sgrender data=GRM template=GRM;
	run;
	options notes;
%mend graph;





