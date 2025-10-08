%macro descriptive_stats(in, by, by_order=, where=, weight=1, var_list=, types_list=, var_cat_order=, only_mean=0, debug=0, out=stats, print=1);
	*Prepare dataset and macro variables for further calculations;
	*Check by subgroups, save N of each subgroup and their names;
	data temp_in;
		set &in.;
		%if %length(&where.)>0 %then %do;where &where.;%end;
	run;
	proc contents data=temp_in out=temp_contents noprint;
	run;
	proc format;
		value $stat
		'N'='N'
		'NMISS'='Missing'
		'MEAN_SD'='Mean (SD)'
		'MEDIAN'='Median'
		'MIN_MAX'='Range'
		'Q1_Q3'='IQR'
		;
	run;
	proc sort data=temp_in;
		by &by.;
	run;

	proc freq data=temp_in noprint;
		tables &by./ list out=temp_by;
		%if &weight.^=1 %then %do;weight &weight.;%end;
	run;
	data temp_by;
		set temp_by;
		%if %length(&by_order.)>0 %then %do;
			%do ds_i=1 %to %sysfunc(countw(&by_order.));
				if _N_=&ds_i. then by_order=%scan(&by_order., &ds_i.);
			%end;
		%end;
		%else %do;
			by_order=_N_;
		%end;
	run;
	data _null_;
		set temp_by;
		call symputx('name_'||put(by_order,1.), &by.);
		call symputx('n_'||put(by_order,1.), put(count,comma12.));
	run;
	proc sql noprint;
		select count(distinct &by.) as by_groups
		into :by_groups
		from temp_in
	quit;

	*Core calculations;
	*Process variables - loop over variable list;
	%do ds_i=1 %to %sysfunc(countw(&var_list.,|));
		%let ds_var=%scan(&var_list.,&ds_i.,|);
		data temp_label_&ds_i.;
			set temp_contents;
			where upcase(name)=upcase("&ds_var.");
			var=upcase("&ds_var.");
			row_desc=label;
			%if %scan(&types_list.,&ds_i.,|)=2 %then %do;
				row_desc=strip(row_desc)||", n (%)";
			%end;
			ord=&ds_i.;
			ord2=0;
			keep var row_desc ord ord2;
		run;
		proc sql noprint;
			select case when format not in('$') then format else '' end as var_format, type
			into :ds_var&ds_i._format trimmed, :ds_var&ds_i._type trimmed
			from temp_contents
			where upcase(name)=upcase("&ds_var.")
			;
		quit;
		proc summary data=temp_in nway completetypes;
			class &ds_var. / preloadfmt order=data missing;
			output out=temp_all_levels_var&ds_i.(keep=&ds_var.);
		run;
		proc sql noprint; 
			create table temp_cat_levels_&ds_i. as 
/*					select distinct &var.*/
				select distinct
					%if %length(&&ds_var&ds_i._format.)>0 %then %do;
						put(b.&ds_var., &&ds_var&ds_i._format..) as &ds_var.
					%end;
					%else %if &&ds_var&ds_i._type.=1 %then %do;
						strip(put(b.&ds_var., best.)) as &ds_var.
					%end;
					%else %do;
						b.&ds_var. as &ds_var.
					%end;
				from temp_in as a
					right join temp_all_levels_var&ds_i. as b on a.&ds_var.=b.&ds_var.
				where b.&ds_var. is not missing
				order by 1
			; 
		quit; 
		*For numerical variables normal mean(type=1);
		%if %scan(&types_list.,&ds_i.,|)=1 %then %do;
			ods select none;
			proc means data=temp_in n %if &weight.^=1 %then %do;sumwgt;%end; nmiss mean std median min max q1 q3 STACKODSOUTPUT;
				class &by.;
				var &ds_var.;
				%if &weight.^=1 %then %do;weight &weight.;%end;
				ods output summary=temp_num_1_&ds_i. ;
			run;
			ods select all;
			data temp_num_2_&ds_i.;
				set temp_num_1_&ds_i.;
				n2=compress(put(n,comma12.));
				%if &weight.^=1 %then %do;
					n2=compress(put(sumwgt,comma12.));
					nmiss2=compress(put(nmiss,comma12.));
				%end;
				%else %do;
					n2=compress(put(n,comma12.));
					nmiss2=compress(put(nmiss,comma12.));
				%end;
				if mean^=. and stddev^=. then 
					mean_sd=compress(put(mean, comma12.1))||' ('||compress(put(stddev,comma12.2))||')';
				else if mean^=. and stddev=. then 
					mean_sd=compress(put(mean, comma12.1))||' (N/A)';
				else mean_sd='N/A';
				if median^=. then 
					median2=compress(put(median,comma12.1));
				else median2='N/A';
				if min^=. then 
					min_max=compress(put(min, comma12.1))||', '||compress(put(max,comma12.1));
				else min_max='N/A';
				if q1^=. then 
					q1_q3=compress(put(q1, comma12.1))||', '||compress(put(q3,comma12.1));
				else q1_q3='N/A';
				drop _control_ n nmiss mean stddev median min max q1 q3;
				rename n2=n nmiss2=nmiss median2=median;
			run;
			proc sql;
				create table temp_num_3_&ds_i. as
					select a.*
						,b.by_order
					from temp_num_2_&ds_i. as a
						left join temp_by as b on a.&by.=b.&by.
				;
			quit;
			proc transpose data=temp_num_3_&ds_i. out=temp_num_4_&ds_i. prefix=stat_;
				id by_order;
				var n nmiss mean_sd median min_max q1_q3;
			run;
			data temp_var_&ds_i.;
				length row_desc $128;
				set temp_num_4_&ds_i.(rename=(_name_=row_desc));
				ord=&ds_i.;
				ord2=_N_;
				row_desc=put(upcase(row_desc),$stat.);
				var=upcase("&ds_var.");
				%if &only_mean.=1 %then %do;
					if row_desc^='Mean (SD)' and ord2>0 then delete;
					ord2=1;
				%end;
			run;
		%end;

		*For categorical variables (type=2);
		%if %scan(&types_list.,&ds_i.,|)=2 %then %do;
			ods select none;
			proc freq data=temp_in noprint;
				by &by.;
				table &ds_var. / list out=temp_cat_0_&ds_i.;
				%if &weight.^=1 %then %do;weight &weight.;%end;
			run;
			proc sql noprint;
				create table temp_levels_&ds_i. as
					select &by., &ds_var.
					from temp_cat_levels_&ds_i.,temp_by
				;
			quit;			
			proc sql noprint;
				create table temp_cat_1_&ds_i. as
					select &by.
						%if %length(&&ds_var&ds_i._format.)>0 %then %do;
							,put(a.&ds_var., &&ds_var&ds_i._format..) as &ds_var.
						%end;
						%else %if &&ds_var&ds_i._type.=1 %then %do;
							,strip(put(a.&ds_var., best.)) as &ds_var.
						%end;
						%else %do;
							,a.&ds_var. as &ds_var.
						%end;
						,count
						,percent
				from temp_cat_0_&ds_i. as a
				;
			quit;
			proc sql noprint;
				create table temp_cat_2_&ds_i. as
					select b.*, coalesce(count,0) as count, coalesce(percent,0) as percent
					from temp_cat_1_&ds_i. as a
						right join temp_levels_&ds_i. as b on a.&ds_var.=b.&ds_var. and a.&by.=b.&by.
				;
			quit;
			ods select all;
			proc sql;
				create table temp_cat_3_&ds_i. as
					select a.*
						,compress(put(a.count, comma12.))||' ('||compress(put(a.percent,comma12.1))||'%)' as n_perc
						,b.by_order
					from temp_cat_2_&ds_i. as a
						left join temp_by as b on a.&by.=b.&by.
					order by &ds_var.
				;
			quit;
			proc transpose data=temp_cat_3_&ds_i. out=temp_cat_4_&ds_i. prefix=stat_;
				by &ds_var.;
				id by_order;
				var n_perc;
			run;
			data temp_var_&ds_i.;
				length var $32 row_desc $128;
				set temp_cat_4_&ds_i.(rename=(_name_=var));
				ord=&ds_i.;
				ord2=_N_;
				var=upcase("&ds_var.");
/*				%if %length(&&ds_var&ds_i._format.)>0 %then %do;*/
/*					row_desc=put(&ds_var., &&ds_var&ds_i._format..);*/
/*				%end;*/
/*				%else %if &&ds_var&ds_i._type.=1 %then %do;*/
/*					row_desc=strip(put(&ds_var., best.));*/
/*				%end;*/
/*				%else %do;*/
/*					row_desc=strip(put(&ds_var., $128.));*/
/*				%end;*/
				row_desc=&ds_var.;
				keep var row_desc stat: ord ord2;
			run;
		%end;
		*End of processing categorical variable;

		*For unique levels count(type=0);
		%if %scan(&types_list.,&ds_i.,|)=0 %then %do;
			proc sql;
				create table temp_uniq_1_&ds_i. as
					select &by.
						,count(distinct &ds_var.) as count
					from temp_in
					group by 1
				;
			quit;
			proc sql;
				create table temp_uniq_2_&ds_i. as
					select a.*
						,compress(put(a.count, comma12.)) as n
						,b.by_order
					from temp_uniq_1_&ds_i. as a
						left join temp_by as b on a.&by.=b.&by.
				;
			quit;
			proc transpose data=temp_uniq_2_&ds_i. out=temp_uniq_3_&ds_i. prefix=stat_;
				id by_order;
				var n;
			run;
			data temp_var_&ds_i.;
				length var $32 row_desc $128;
				set temp_uniq_3_&ds_i.(rename=(_name_=var));
				ord=&ds_i.;
				ord2=0;
				var=upcase("&ds_var.");
				%if %length(&&ds_var&ds_i._format.)>0 %then %do;
					row_desc=put(&ds_var., &&ds_var&ds_i._format..);
				%end;
				%else %do;
					row_desc=strip(put(&ds_var., $128.));
				%end;
				keep var row_desc stat: ord ord2;
			run;
		%end;
		*End of processing categorical variable;
	%end; 



	*Create final output dataset;
	data temp_combined;
		length var $32 row_desc $128;
		set
		%do ds_i=1 %to %sysfunc(countw(&var_list.,|));
			temp_label_&ds_i.
			temp_var_&ds_i.
		%end;
		;
		by ord ord2;
	run;

	%let cat_var_num=0;
	data temp_combined2;
		set temp_combined;
		%do ds_i=1 %to %sysfunc(countw(&var_list.,|));
			%if %scan(&types_list.,&ds_i.,|)=2 %then %do;
				%let cat_var_num=%eval(&cat_var_num.+1);
				%let new_order=%scan(&var_cat_order.,&cat_var_num.,|);
				%if %length(&new_order.)>0 %then %do;
					%do ds_j=1 %to %sysfunc(countw(&new_order.,%str( )));
						if ord=&ds_i. and ord2=%scan(&new_order.,&ds_j.,%str( )) then reord=&ds_j.;
					%end;
					if ord=&ds_i. and ord2=0 then reord=ord2;
				%end;
				%else %do;
					if ord=&ds_i. then reord=ord2;
				%end;
			%end;
			%else %do;
				if ord=&ds_i. then reord=ord2;
			%end;
		%end;
		if reord=. then delete;
	run;
	proc sort data=temp_combined2;
		by ord reord;
	run;
	data &out.;
		set temp_combined2;
		by ord reord;
		if last.ord then last=1;
	run;

	*Print output table;
	%if &print.=1 %then %do;
		proc report data=&out. split='^'
			STYLE(report) = [fontfamily=Arial borderleftstyle=none borderrightstyle=none borderbottomstyle=none bordertopstyle=none] 
			STYLE(header) = [fontfamily=Arial fontsize=10pt backgroundcolor=white fontweight=light borderbottomwidth=1 borderbottomstyle=solid bordertopstyle=solid] 
			STYLE(column) = [fontfamily=Arial paddingleft=5 paddingright=5] 
			STYLE(lines) = [fontfamily=Arial  borderbottomstyle=none bordertopstyle=solid] 
		;
			column ord ord2 reord row_desc 
				%do ds_i=1 %to &by_groups.;
					stat_&ds_i.
				%end;
				last
			;

			define ord / order order=data noprint "";
			define ord2 / order order=data noprint "";
			define reord / order order=data noprint "";
			define row_desc / display "" STYLE=[asis=on just=left];
			%do ds_i=1 %to &by_groups.;
				define stat_&ds_i. / display "&&name_&ds_i..^(N=&&n_&ds_i..)" style=[textalign=center];
			%end;
			define last / display noprint "";

			compute last; 
				count+1; 
				if (mod(count,2))=0 then do; 
					CALL DEFINE(_ROW_, "STYLE", "STYLE=[BACKGROUND=greyef]"); 
				end; 
				if ord2=0 then do;
					CALL DEFINE("row_desc", "STYLE", "STYLE=[fontweight=bold]"); 
				end;
				if ord2>0 then do;
					CALL DEFINE("row_desc", "STYLE", "STYLE=[paddingleft=20]"); 
				end;
				if last=1 then do;
					CALL DEFINE(_ROW_, "style", "STYLE=[borderbottomstyle=solid]"); 
					if (mod(count,2))=0 then do; 
						CALL DEFINE(_ROW_, "STYLE", "STYLE=[BACKGROUND=greyef borderbottomstyle=solid]"); 
					end; 
				end;
			endcomp; 
		run;
	%end;

	*Delete temporary interim datasets;
	%if &debug=0 %then %do;
		proc datasets lib=work noprint;
			delete temp_:;
		run;
		quit;
	%end;

%mend;
%macro pval(
	in
	,by
	,by_order=
	,compare_list=1_2
	,where=
	,weight=1
	,var_list=
	,types_list=
	,post_hoc_list=
	,test_lists=
	,debug=0
	,out=pval
);

	*Prepare dataset and macro variables for further calculations;
	*Check by subgroups, save N of each subgroup and their names;
	data temp_in;
		set &in.;
		%if %length(&where.)>0 %then %do;where &where.;%end;
	run;
	proc contents data=temp_in out=temp_contents noprint;
	run;
	proc freq data=temp_in noprint;
		tables &by./ list out=temp_by;
		%if &weight.^=1 %then %do;weight &weight.;%end;
	run;
	data temp_by;
		set temp_by;
			by_var=&by.;
		%if %length(&by_order.)>0 %then %do;
			%do pv_i=1 %to %sysfunc(countw(%nrbquote(&by_order.)));
				if _N_=&pv_i. then by_order=%scan(&by_order., &pv_i.);
			%end;
		%end;
		%else %do;
			by_order=_N_;
		%end;
	run;
	data _null_;
		set temp_by;
		call symputx('name_'||strip(put(by_order,1.)), strip(&by.));
		call symputx('n_'||strip(put(by_order,1.)), strip(put(count,comma12.)));
	run;
	proc sql noprint;
		select count(distinct &by.) as by_groups
		into :by_groups
		from temp_in
	quit;

	proc format;
		value mypval
			low -< 0.001 = '<0.001'
			.='N/A'
			other =[pvalue6.3]
		;
	run;

	*Core calculations;
	*Process variables - loop over variable list;
	%do pv_i=1 %to %sysfunc(countw(%nrbquote(&var_list.),|));
		%let var=%scan(%nrbquote(&var_list.),&pv_i.,|);
		proc sql noprint;
			select case when format not in('$') then format else '' end as var_format, type
			into :var&pv_i._format trimmed, :var&pv_i._type trimmed
			from temp_contents
			where upcase(name)=upcase("&var.")
			;
		quit;

		*For numerical variables normal mean(type=1);
		%if %scan(&types_list.,&pv_i.,|)=1 %then %do;
			%do pv_k=1 %to %sysfunc(countw(%nrbquote(&compare_list.),|));
				%let compare_pair=%scan(%nrbquote(&compare_list.),&pv_k.,|);
				%let treated=%substr(&compare_pair.,1,1);
				%let compare=%substr(&compare_pair.,3,1);

				ods select none;
				proc npar1way data=temp_in wilcoxon;
					where &by.="&&name_&treated." or &by.="&&name_&compare.";
				    class &by.;
				    var &var.;
					%if &weight.^=1 %then %do;weight &weight.;%end;
					ods output WilcoxonTest=temp_num_&pv_i._&pv_k.;
				run;
				ods select all;
	/*			proc sort data=temp_num_&pv_i.;*/
	/*				by &by.;*/
	/*			run;*/
				proc sql; 
				    create table temp_var_&pv_i._&pv_k. as  
				    	select variable as var
	/*						%if &post_hoc.=1 %then %do; */
	/*							,'Mean (SD)' as row_desc*/
	/*						%end;*/
							,&pv_i. as ord
	/*						%if &post_hoc.=0 %then %do; */
								,0 as ord2
	/*						%end;*/
	/*						%if &post_hoc.=1 %then %do; */
	/*							,1 as ord2*/
	/*						%end;*/
							,prob2 as pval_num
							,compress(put(pval_num, mypval.)) as pval
							,"Wilcoxon" as pval_test
				    	from temp_num_&pv_i._&pv_k.
					; 
				quit; 
			%end;
		%end;

		*For categorical variables (type=2);
		%if %scan(&types_list.,&pv_i.,|)=2 %then %do;
/*			proc sql noprint; */
/*			   select coalesce(count(distinct &var.),0) into :_mylevel_ */
/*				from temp_in*/
/*				where &var. is not missing*/
/*				; */
/*			quit; */

			proc summary data=temp_in nway completetypes;
				class &var. / preloadfmt order=data missing;
				output out=temp_all_levels_var&pv_i.(keep=&var.);
			run;

			proc sql noprint; 
				create table temp_cat_levels_&pv_i. as 
/*					select distinct &var.*/
					select distinct
						%if %length(&&var&pv_i._format.)>0 %then %do;
							put(b.&var., &&var&pv_i._format..) as row_desc
						%end;
						%else %if &&var&pv_i._type.=1 %then %do;
							strip(put(b.&var., best.)) as row_desc
						%end;
						%else %do;
							b.&var. as row_desc
						%end;
					from temp_in as a
						right join temp_all_levels_var&pv_i. as b on a.&var.=b.&var.
					where b.&var. is not missing
					order by 1
				; 
			quit; 
/*			proc sql noprint; */
/*				select distinct &var. */
/*				into :var_levels separated by '|'*/
/*				from temp_in*/
/*				order by 1*/
/*				; */
/*			quit; */
			proc sql noprint; 
				select *
				into :var_levels separated by '|'
				from temp_cat_levels_&pv_i.
				order by 1
				; 
			quit; 
			%put &var_levels.;



			%do pv_k=1 %to %sysfunc(countw(%nrbquote(&compare_list.),|));
				%let compare_pair=%scan(%nrbquote(&compare_list.),&pv_k.,|);
				%let treated=%substr(&compare_pair.,1,1);
				%let compare=%substr(&compare_pair.,3,1);

	/*			%if &post_hoc.=0 %then %do; */
				ods select none;
				proc freq data = temp_in; 
					where &by.="&&name_&treated." or &by.="&&name_&compare.";
					table &var. * &by. / chisq warn=output; 
					%if &weight.^=1 %then %do;weight &weight.;%end;
					ods output ChiSq=temp_cat_chisq_&pv_i._&pv_k.;
				run; 
				ods select all;
				%let fisher=;
				%if %sysfunc(exist(temp_cat_chisq_&pv_i._&pv_k.)) %then %do;
					proc sql noprint;
						select warning
						into :fisher
						from temp_cat_chisq_&pv_i._&pv_k.
						where statistic='Chi-Square'
						;
					quit;
					%if &fisher.=0 %then %do;
						data temp_var_&pv_i._&pv_k.;
							length var $32 pval $20 pval_test $20;
							set temp_cat_chisq_&pv_i._&pv_k.;
							where statistic='Chi-Square';
							pval=compress(put(prob, mypval.));
							pval_num=prob;
							pval_test=statistic;
							var=upcase("&var.");
							ord=&pv_i.;
							ord2=0;
							keep var ord: pval:;
						run;
					%end;
					%if &fisher.=1 %then %do;
						ods select none;
						proc freq data = temp_in; 
							where &by.="&&name_&treated." or &by.="&&name_&compare.";
							table &var. * &by. / fisher; 
							%if &weight.^=1 %then %do;weight &weight.;%end;
							ods output FishersExact=temp_cat_fish_&pv_i._&pv_k.;
						run; 
						ods select all;
						data temp_var_&pv_i._&pv_k.;
							length var $32 pval $20 pval_test $20;
							set temp_cat_fish_&pv_i._&pv_k.;
							where name1='XP2_FISH';
							pval=compress(put(Nvalue1, mypval.));
							pval_num=Nvalue1;
							pval_test="Fisher";
							var=upcase("&var.");
							ord=&pv_i.;
							ord2=0;
							keep var ord: pval:;
						run;
					%end;
				%end;
				%else %do;
					data temp_var_&pv_i;
						length var $32 pval $20 pval_test $20;
						pval='N/A';
						pval_num=.;
						pval_test="";
						var=upcase("&var.");
						ord=&pv_i.;
						ord2=0;
						keep var ord: pval:;
					run;
				%end;

/*			ods select all;*/
/*			data temp_cat_1_&pv_i.;*/
/*				set temp_cat_chisq_&pv_i.;*/
/*				where statistic='Chi-Square';*/
/*				var=upcase("&var.");*/
/*				ord=&pv_i.;*/
/*				ord2=0;*/
/*					prob_char=compress(put(prob, mypval.))||'^{super 1}';*/
/*				prob_char=compress(put(prob, mypval.));*/
/*				keep var ord ord2 prob prob_char;*/
/*			run;*/

				%if %scan(%nrbquote(&post_hoc_list.),&pv_i.,|)=1 %then %do; 
					%do pv_j=1 %to %sysfunc(countw(%nrbquote(&var_levels.),|));
						data temp_in_pval_&pv_i._&pv_j._&pv_k.;
							length temp_var $128;
							set temp_in;
								where &by.="&&name_&treated." or &by.="&&name_&compare.";
								%if %length(&&var&pv_i._format.)>0 %then %do;
									if put(&var.,&&var&pv_i._format..)^="%scan(%nrbquote(&var_levels.),&pv_j.,|)" then temp_var="Other level than currently processed";
									else temp_var="%scan(%nrbquote(&var_levels.),&pv_j.,|)";
								%end;
								%if %length(&&var&pv_i._format.)=0 %then %do;
									if &var.^="%scan(%nrbquote(&var_levels.),&pv_j.,|)" then temp_var="Other level than currently processed";
									else temp_var="%scan(%nrbquote(&var_levels.),&pv_j.,|)";
								%end;
						run;
						proc sql noprint;
							select count(*) as cnt
							into :var_&pv_i._&pv_j._&pv_k._level_cnt
							from temp_in_pval_&pv_i._&pv_j._&pv_k.
							where temp_var^='Other level than currently processed'
							;
						quit;
						proc sql noprint;
							select count(*) as cnt
							into :var_&pv_i._&pv_j._&pv_k._other_cnt
							from temp_in_pval_&pv_i._&pv_j._&pv_k.
							where temp_var='Other level than currently processed'
							;
						quit;


						%if &&var_&pv_i._&pv_j._&pv_k._level_cnt.>0 and &&var_&pv_i._&pv_j._&pv_k._other_cnt.>0 %then %do;
							ods select none;
							proc freq data = temp_in_pval_&pv_i._&pv_j._&pv_k.; 
								table temp_var * &by. / chisq warn=output; 
								%if &weight.^=1 %then %do;weight &weight.;%end;
								ods output ChiSq=temp_cat_chisq_&pv_i._&pv_j._&pv_k.;
							run; 
							ods select all;
							%let fisher=;
							proc sql noprint;
								select warning
								into :fisher
								from temp_cat_chisq_&pv_i._&pv_j._&pv_k.
								where statistic='Chi-Square'
								;
							quit;
							%if &fisher.=0 %then %do;
								data temp_var_&pv_i._&pv_j._&pv_k.;
									length pval $20;
									set temp_cat_chisq_&pv_i._&pv_j._&pv_k.;
									where statistic='Chi-Square';
									pval=compress(put(prob, mypval.));
									pval_num=prob;
									pval_test=statistic;
									keep pval:;
								run;
							%end;
							%if &fisher.=1 %then %do;
								ods select none;
								proc freq data = temp_in_pval_&pv_i._&pv_j._&pv_k.; 
									table temp_var * &by. / fisher; 
									%if &weight.^=1 %then %do;weight &weight.;%end;
									ods output FishersExact=temp_cat_fish_&pv_i._&pv_j._&pv_k.;
								run; 
								ods select all;
								data temp_var_&pv_i._&pv_j._&pv_k.;
									length pval $20;
									set temp_cat_fish_&pv_i._&pv_j._&pv_k.;
									where name1='XP2_FISH';
									pval_num=Nvalue1;
									pval=compress(put(Nvalue1, mypval.));
									pval_test="Fisher";
									keep pval:;
								run;
							%end;
						%end;
						%else %do;
							data temp_var_&pv_i._&pv_j._&pv_k.;
								length pval $20;
								pval_num=.;
								pval='N/A';
								pval_test="";
							run;
						%end;
					%end;

					data temp_var_ph_&pv_i._&pv_k.;
						length row_desc $128  pval $20 pval_num 8  pval_test $100;
						set
							%do pv_j=1 %to %sysfunc(countw(%nrbquote(&var_levels.),|));
								temp_var_&pv_i._&pv_j._&pv_k.(in=level_&pv_j.)
							%end;
						;
						var=upcase("&var.");
						%do pv_j=1 %to %sysfunc(countw(%nrbquote(&var_levels.),|));
							if level_&pv_j. then row_desc=strip("%scan(%nrbquote(&var_levels.),&pv_j.,|)");
							if level_&pv_j. then ord2=&pv_j.;
						%end;
						ord=&pv_i.;
						keep var ord: pval: row_desc;
					run;
				%end;
/*				proc sql; */
/*					create table temp_cat_1_&pv_i. as */
/*						select b.var, a.row_desc*/
/*							,b.ord*/
/*							,b.ord2*/
/*							,b.prob */
/*							,b.prob_char */
/*						from temp_cat_levels_&pv_i. as a */
/*							left join temp_cat_chisq_&pv_i. as b on a.row_desc = b.row_desc*/
/*					; */
/*				quit; */
			%end;

/*  			proc sql; */
/*   				create table temp_var_&pv_i. as  */
/*	   				select var*/
/*						%if &post_hoc.=1 %then %do; */
/*							,row_desc as row_desc*/
/*						%end;*/
/*						,ord*/
/*						,ord2*/
/*						,prob as pval_num*/
/*						,prob_char as pval*/
/*	   				from temp_cat_1_&pv_i.*/
/*				; */
/* 		 	quit; */
		%end;
	%end;
		*End of processing categorical variable;

/*	%end;*/

	
	%do pv_k=1 %to %sysfunc(countw(%nrbquote(&compare_list.),|));
		data temp_combined_&pv_k.;
			length var $32 row_desc $128;
			set 
			%do pv_i=1 %to %sysfunc(countw(%nrbquote(&var_list.),|));
				temp_var_&pv_i._&pv_k.
				%if %scan(&types_list.,&pv_i.,|)=2 and %scan(%nrbquote(&post_hoc_list.),&pv_i.,|)=1 %then %do;
					temp_var_ph_&pv_i._&pv_k.
				%end;
			%end;
			;
			format row_desc $128.;
		run;
	%end;

	proc sql noprint;
/*		select distinct pval_test*/
/*		into :tests_applied separated by '|'*/
/*		from temp_combined*/
/*		where pval_test^=""*/
/*		order by 1*/
/*		;*/

		select distinct pval_test
		into :tests_applied separated by '|'
		from(
			%do pv_k=1 %to %sysfunc(countw(%nrbquote(&compare_list.),|));
				%if &pv_k.>1 %then %do;UNION%end;
				select pval_test
				from temp_combined_&pv_k.
			%end;
		)
		where pval_test^=""
		order by 1
		;
	quit;


	%do pv_k=1 %to %sysfunc(countw(%nrbquote(&compare_list.),|));
		data temp_combined_&pv_k.;
			set temp_combined_&pv_k.;
			%if %length(&tests_applied.)>0 %then %do;
				%do pv_i=1 %to %sysfunc(countw(%nrbquote(&tests_applied.),|));
					if pval_test="%scan(&tests_applied.,&pv_i.,|)" then pval=strip(pval)||"^{super &pv_i.}";
				%end;
			%end;
		run;
	%end;

	data &out.;
		merge 
		%do pv_k=1 %to %sysfunc(countw(%nrbquote(&compare_list.),|));
			temp_combined_&pv_k.(rename=(pval=pval&pv_k. pval_num=pval&pv_k._num pval_test=pval&pv_k._test))
		%end;
		;
	run;



/*	data &out.;*/
/*		set temp_combined;*/
/*		%if %length(&tests_applied.)>0 %then %do;*/
/*			%do pv_i=1 %to %sysfunc(countw(%nrbquote(&tests_applied.),|));*/
/*				if pval_test="%scan(&tests_applied.,&pv_i.,|)" then pval=strip(pval)||"^{super &pv_i.}";*/
/*			%end;*/
/*		%end;*/
/*	run;*/

	%if &debug=0 %then %do;
		proc datasets lib=work noprint;
			delete temp_:;
		run;
		quit;
	%end;

%mend;
%macro asd(in, by, treated, by_order=, where=, weight=1, var_list=, types_list=, compare_list=1_2, post_hoc_list=, debug=0, out=asd);
	*Prepare dataset and macro variables for further calculations;
	*Check by subgroups, save N of each subgroup and their names;
	data temp_in;
		set &in.;
		%if %length(&where.)>0 %then %do;where &where.;%end;
	run;
	proc contents data=temp_in out=temp_contents noprint;
	run;
	proc freq data=temp_in noprint;
		tables &by./ list out=temp_by;
		%if &weight.^=1 %then %do;weight &weight.;%end;
	run;
	data temp_by;
		set temp_by;
			by_var=&by.;
		%if %length(&by_order.)>0 %then %do;
			%do as_i=1 %to %sysfunc(countw(%nrbquote(&by_order.)));
				if _N_=&as_i. then by_order=%scan(&by_order., &as_i.);
			%end;
		%end;
		%else %do;
			by_order=_N_;
		%end;
	run;
	data _null_;
		set temp_by;
		call symputx('name_'||strip(put(by_order,1.)), strip(&by.));
		call symputx('n_'||strip(put(by_order,1.)), strip(put(count,comma12.)));
	run;
	proc sql noprint;
		select count(distinct &by.) as by_groups
		into :by_groups
		from temp_in
	quit;

	%do as_j=1 %to %sysfunc(countw(%nrbquote(&compare_list.),|));
		%let compare_pair=%scan(%nrbquote(&compare_list.),&as_j.,|);
		%let treated=%substr(&compare_pair.,1,1);
		%let compare=%substr(&compare_pair.,3,1);


/*	proc sql noprint;*/
/*		create table temp_by as*/
/*			select &by. as by_var*/
/*				,case when &by.=&treated. then 0 else 1 end as by_order*/
/*				,sum(1*&weight.) as patient_cnt*/
/*			from temp_in*/
/*			group by 1,2*/
/*			order by 2*/
/*		;*/
/*	quit;*/
/*	data _null_;*/
/*		set temp_by;*/
/*		call symputx('name_'||put(by_order,1.), by_var);*/
/*		call symputx('n_'||put(by_order,1.), put(patient_cnt,comma12.));*/
/*	run;*/


	*Core calculations;
	*Process variables - loop over variable list;
	%do i=1 %to %sysfunc(countw(&var_list.,|));
		%let var=%scan(&var_list.,&i.,|);
		proc sql noprint;
			select case when format not in('$') then format else '' end as var_format
			into :var&i._format trimmed
			from temp_contents
			where upcase(name)=upcase("&var.")
			;
		quit;

		*For numerical variables normal mean(type=1);
		%if %scan(&types_list.,&i.,|)=1 %then %do;
			proc means data=temp_in noprint;
				class &by.;
				var &var.;
				%if &weight.^=1 %then %do;weight &weight.;%end;
				output out=temp_num_&i._&as_j. mean=_mean_  std=_std_;
			run;
			data temp_num_&i._&as_j.;
				set temp_num_&i._&as_j.;
				where _type_=1;
			run;
			proc sort data=temp_num_&i._&as_j.;
				by &by.;
			run;
			proc sql; 
			    create table temp_var_&i._&as_j. as  
			    	select upcase("&var.") as var
						,&i. as ord
						,0 as ord2
						,abs((compare._mean_ - treated._mean_)/sqrt((compare._std_**2 + treated._std_**2)/2)) as asd_num
						,compress(put(calculated asd_num, comma9.4)) as asd
			    	from temp_num_&i._&as_j.(where = (&by.="&&name_&compare.")) as compare, 
			      		 temp_num_&i._&as_j.(where = (&by.="&&name_&treated.")) as treated
				; 
			quit; 
		%end;

		*For categorical variables (type=2);
		%if %scan(&types_list.,&i.,|)=2 %then %do;
			proc sql noprint; 
			   select coalesce(count(distinct &var.),0) into :_mylevel_ 
				from temp_in
				where &var. is not missing
				; 
			quit; 
			%put &_mylevel_.;	

			proc sql; 
				create table temp_cat_levels_&i._&as_j. as 
					select distinct &var., by_var, by_order
					from temp_in, temp_by
					where &var. is not missing
				; 
			quit; 

			ods select none;
			proc freq data = temp_in; 
				table &var. * &by.; 
				%if &weight.^=1 %then %do;weight &weight.;%end;
				ods output CrossTabFreqs = temp_cat_freq_&i._&as_j.; 
			run; 
			ods select all;

			proc sql; 
				create table temp_cat_1_&i._&as_j. as 
					select a.*
						,b.ColPercent 
					from temp_cat_levels_&i._&as_j. as a 
						left join temp_cat_freq_&i._&as_j. as b on	a.by_var = b.&by. 
							and a.&var. = b.&var.
				; 
			quit; 

			data temp_cat_2_&i._&as_j.; 
				set temp_cat_1_&i._&as_j.; 
				if ColPercent = . then ColPercent = 0; 
			run; 

			proc sort data = temp_cat_2_&i._&as_j. out = temp_cat_3_&i._&as_j.; 
				by by_var &var.; 
			run; 

  			data temp_cat_4_&i._&as_j.; 
				set temp_cat_3_&i._&as_j.; 
				by by_var; 
				if last.by_var then delete; 
				ColPercent = ColPercent/100; 
  			run; 

			*For 1 category input 0;
			%if &_mylevel_. = 1 %then %do; 
	  			proc sql noprint; 
				    create table temp_var_&i._&as_j. as  
				    	select distinct upcase("&var.") as var
							,&i. as ord
							,0 as ord2
							,0 as asd_num
							,compress(put(calculated asd_num, comma9.4)) as asd
				    	from temp_cat_levels_&i._&as_j.
					; 	  			
				quit; 
				%if %scan(%nrbquote(&post_hoc_list.),&i.,|)=1 %then %do;
		  			proc sql noprint; 
					    create table temp_var_row_&i._&as_j. as  
					    	select distinct upcase("&var.") as var
								%if %length(&&var&i._format.)>0 %then %do;
									,put(&var., &&var&i._format..) as row_desc
								%end;
								%else %do;
									,strip(put(&var., $128.)) as row_desc
								%end;
								,&i. as ord
								,1 as ord2
								,0 as asd_num
								,compress(put(calculated asd_num, comma9.4)) as asd
					    	from temp_cat_levels_&i._&as_j.
						; 	  			
					quit; 
		   		%end; 
	   		%end; 

			*For 2 categories calculate it in standard way;
			%if &_mylevel_. = 2 %then %do; 
	  			data temp_cat_4_&i._&as_j.; 
				   set temp_cat_3_&i._&as_j.; 
				   by by_var; 
				   if last.by_var then delete; 
				   ColPercent = ColPercent/100; 
	  			run; 

	  			proc sql; 
	   				create table temp_var_&i._&as_j. as  
		   				select upcase("&var.") as var
							,&i. as ord
							,0 as ord2
							,abs((a.ColPercent - b.ColPercent)/(sqrt((a.ColPercent*(1- a.ColPercent)+b.ColPercent*(1-b.ColPercent))/2))) as asd_num
							,compress(put(calculated asd_num, comma9.4)) as asd
		   				from temp_cat_4_&i._&as_j.(where = (by_var = "&&name_&compare.")) as a, 
		     		    	 temp_cat_4_&i._&as_j.(where = (by_var = "&&name_&treated.")) as b
					; 
	 		 	quit; 
				%if %scan(%nrbquote(&post_hoc_list.),&i.,|)=1 %then %do;
					data temp_cat_row_4_&i._&as_j.; 
					   set temp_cat_3_&i._&as_j.; 
					   by by_var; 
					   retain ord2;
	/*				   if last.by_var then delete; */
					   ColPercent = ColPercent/100; 
					   if first.by_var then ord2=1;
					   else ord2=ord2+1;
		  			run; 

		  			proc sql; 
		   				create table temp_var_row_&i._&as_j. as  
			   				select upcase("&var.") as var
								%if %length(&&var&i._format.)>0 %then %do;
									,put(b.&var., &&var&i._format..) as row_desc
								%end;
								%else %do;
									,strip(put(b.&var., $128.)) as row_desc
								%end;
								,&i. as ord
								,b.ord2 as ord2
								,abs((a.ColPercent - b.ColPercent)/(sqrt((a.ColPercent*(1- a.ColPercent)+b.ColPercent*(1-b.ColPercent))/2))) as asd_num
								,compress(put(calculated asd_num, comma9.4)) as asd
			   				from temp_cat_row_4_&i._&as_j.(where = (by_var = "&&name_&compare.")) as a
			     		    	left join temp_cat_row_4_&i._&as_j.(where = (by_var = "&&name_&treated.")) as b on a.&var.=b.&var.
						; 
		 		 	quit; 
		   		%end; 
			%end;
			

			*For 3 or more categories use Mahalanobis distance;
			%if &_mylevel_. >= 3 %then %do; 
					proc sql noprint; 
						select ColPercent 
						into :tlist separated by ' '  
						from temp_cat_4_&i._&as_j.
						where by_var = "&&name_&treated."
						; 

					    select ColPercent 
						into :clist separated by ' '  
						from temp_cat_4_&i._&as_j.
						where by_var = "&&name_&compare."
						; 
					quit; 
		   			%let _k_ = %sysfunc(strip(%eval(&_mylevel_.-1))); 

					data t_1_&i._&as_j.; 
						array t{*}  t1- t&_k_.   (&tlist.); 
						array c{*}  c1- c&_k_.   (&clist.); 
						array tc{*} tc1 - tc&_k_. ; 
						do i = 1 to dim(t); 
							tc{i} = t{i} - c{i}; 
						end; 
						drop i; 
					run; 

					%let _dm = ; 
					%let _dm = %eval(&_k_.*&_k_.); 

		  			data covdata_&i._&as_j.; 
		   				array t{*}  t1- t&_k_.  (&tlist.); 
		   				array c{*}  c1- c&_k_.   (&clist.); 
		   				array cv{&_k_.,&_k_.} x1 -x&_dm.; 
		   				do i = 1 to &_k_.; 
		    				do j = 1 to &_k_.; 
		     					if i = j then do; 
		      						cv{i,j} = 0.5*(t{i}*(1-t{i}) + c{i}*(1-c{i})); 
		      						end; 
		     					else do; 
		      						cv{i,j} = -0.5 * (t[i] * t[j] + c[i] * c[j]); 
		      						end; 
		    					if cv{&_k_.,&_k_.] ne . then output; 
		    				end; 
		  				end; 
		  			run; 

		  			proc transpose data = covdata_&i._&as_j.(keep = x1 -x&_dm.) out = covdata_1_&i._&as_j.; 
		  			run; 

		  			data covdata_2_&i._&as_j.; 
		   				set covdata_1_&i._&as_j.; 
		   				retain id gp 1; 
		   				if mod(_n_ - 1,&_k_.) = 0 then gp = gp + 1; 
		  			run; 

				  	proc sort data = covdata_2_&i._&as_j. ; 
				   		by gp id; 
				  	run;   

					data covdata_3_&i._&as_j.; 
				   		set covdata_2_&i._&as_j.; 
				   		by gp id; 
				   		retain lp; 
				   		if first.gp then lp = 0; 
				   		lp = lp+1; 
				  	run; 

					*transpose to a S variance-covariance matrix format;
				  	data covdata_4_&i._&as_j.; 
				   		set covdata_3_&i._&as_j.; 
				   		retain y1-y&_k_.; 
				   		array cy{1:&_k_.} y1-y&_k_.; 
				   		by gp id; 
				   		if first.gp then do; 
				    		do k = 1 to &_k_.; 
				     			cy{k} = .; 
						    end; 
				   		end; 
				   		cy{lp} = col1; 
				   		if last.gp then output; 
				   		keep y:; 
				  	run; 

					*get inverse of S matrix;
					data A_1_&i._&as_j.; 
						set covdata_4_&i._&as_j.; 
						array _I{*} I1-I&_k_.; 
						do j=1 to &_k_.; 
							if j=_n_ then _I[j]=1;  
							else _I[j]=0; 
						end; 
						drop j; 
					run; 

				*inverse;
		    	%do j=1 %to &_k_.; 
		    		proc orthoreg data=A_1_&i._&as_j. outest=A_inv_&j._&i._&as_j.(keep=y1-y&_k_.) 
		     			noprint singular=1E-16; 
		     			model I&j=y1-y&_k_. /noint; 
		    		run; 
		    		quit; 
		    	%end; 

		   		data A_inverse_&i._&as_j.; 
		    		set 
						%do j=1 %to &_k_.; 
		     				A_inv_&j._&i._&as_j.
		     			%end;
					; 
		   		run; 

		    	proc transpose data=A_inverse_&i._&as_j. out=A_inverse_t_&i._&as_j.; 
		  		run; 

		   		*calculate the mahalanobis distance;
		  		data t_2_&i._&as_j.; 
		   			set A_inverse_t_&i._&as_j.; 
		   			array t{*}  t1- t&_k_.  (&tlist.); 
		   			array c{*}  c1- c&_k_.  (&clist.); 
		   			i = _n_; 
		   			trt = t{i}; 
		   			ctl = c{i}; 
		   			tc = t{i} - c{i}; 
		  		run; 
		 
				data t_3_&i._&as_j.; 
		   			set t_2_&i._&as_j.; 
		   			array aa{&_k_.} col1 - col&_k_.; 
		   			array bb{&_k_.} bb1- bb&_k_.; 
		   			do i = 1 to &_k_.; 
		    			bb{i} = aa{i}*tc; 
		   			end; 
		  		run; 

		  		proc summary data = t_3_&i._&as_j. ; 
		   			var bb1-bb&_k_.; 
		   			output out = t_4_&i._&as_j. sum =; 
		  		run; 

		  		data temp_var_&i._&as_j.; 
		   			merge t_1_&i._&as_j. t_4_&i._&as_j.; 
					var=upcase("&var.");
					ord=&i.;  
					ord2=0;  
		   			array d1{*} tc1- tc&_k_. ; 
		   			array d2{*} bb1-bb&_k_.; 
		   			array d3{*} y1-y&_k_.; 
		   			do i = 1 to &_k_.; 
		   				d3{i} = d1{i}*d2{i}; 
		   			end; 
		   			asd_num = sqrt(sum(of y1-y&_k_.)); 
		   			asd = compress(put(asd_num,comma9.4));  
		   			keep var ord ord2 asd_num asd; 
		  		run; 

			%if %scan(%nrbquote(&post_hoc_list.),&i.,|)=1 %then %do;
				data temp_cat_row_4_&i._&as_j.; 
				   set temp_cat_3_&i._&as_j.; 
				   by by_var; 
				   retain ord2;
/*				   if last.by_var then delete; */
				   ColPercent = ColPercent/100; 
				   if first.by_var then ord2=1;
				   else ord2=ord2+1;
	  			run; 

	  			proc sql; 
	   				create table temp_var_row_&i._&as_j. as  
		   				select upcase("&var.") as var
							%if %length(&&var&i._format.)>0 %then %do;
								,put(b.&var., &&var&i._format..) as row_desc
							%end;
							%else %do;
								,strip(put(b.&var., $128.)) as row_desc
							%end;
							,&i. as ord
							,b.ord2 as ord2
							,abs((a.ColPercent - b.ColPercent)/(sqrt((a.ColPercent*(1- a.ColPercent)+b.ColPercent*(1-b.ColPercent))/2))) as asd_num
							,compress(put(calculated asd_num, comma9.4)) as asd
		   				from temp_cat_row_4_&i._&as_j.(where = (by_var = "&&name_&compare.")) as a
		     		    	left join temp_cat_row_4_&i._&as_j.(where = (by_var = "&&name_&treated.")) as b on a.&var.=b.&var.
					; 
	 		 	quit; 
			%end;
		%end;
/*		%end;*/
	%end;
		*End of processing categorical variable;
	%end;
	*End of comparisons loop;

	%end;
	
	%do as_j=1 %to %sysfunc(countw(%nrbquote(&compare_list.),|));
		data temp_out_&as_j.;
			length var $32 row_desc $128;
			set 
				%do i=1 %to %sysfunc(countw(&var_list.,|));
					temp_var_&i._&as_j.
					%if %scan(&types_list.,&i.,|)=2 and %scan(%nrbquote(&post_hoc_list.),&i.,|)=1 %then %do;
						temp_var_row_&i._&as_j.
					%end;
				%end;
			;
			format row_desc $128.;
		run;
	%end;

	data &out.;
		merge 
			%do as_j=1 %to %sysfunc(countw(%nrbquote(&compare_list.),|));
				temp_out_&as_j.(rename=(asd=asd&as_j. asd_num=asd&as_j._num))
			%end;
		;
		by ord ord2;
	run;

	%if &debug=0 %then %do;
		proc datasets lib=work noprint;
			delete t_: A_: covdata: temp_:;
		run;
		quit;
	%end;

%mend;
%macro output_table(in
	,var_list
	,types_list
	,only_mean=0
	,by=
	,by_order=
	,var_cat_order=
	,compare_list=1_2
	,post_hoc_list=
	,test_lists=
	,treated=
	,where=
	,weight=1
	,asd=0
	,pval=0
	,out=
	,print=1
	,debug=0
);


	%descriptive_stats(in=&in.
		,by=&by.
		,by_order=&by_order.
		,where=&where.
		,weight=&weight.
		,var_list=&var_list.
		,types_list=&types_list.
		,var_cat_order=&var_cat_order.
		,only_mean=&only_mean.
		,debug=&debug.
		,print=0
	);
	%if &asd.=1 %then %do;
		%asd(in=&in.
			,by=&by.
			,compare_list=&compare_list.
			,where=&where.
			,weight=&weight.
			,var_list=&var_list.
			,types_list=&types_list.
			,post_hoc_list=&post_hoc_list.
			,debug=&debug.
		);
	%end;
	%if &pval.=1 %then %do;
		%pval(in=&in.
			,by=&by.
			,by_order=&by_order.
			,compare_list=&compare_list.
			,where=&where.
			,weight=&weight.
			,var_list=&var_list.
			,types_list=&types_list.
			,post_hoc_list=&post_hoc_list.
			,test_lists=&test_lists.
			,debug=&debug.
		);

		proc sql noprint;
			select distinct pval_test
			into :tests_applied separated by '|'
			from(
				%do ot_k=1 %to %sysfunc(countw(%nrbquote(&compare_list.),|));
					%if &ot_k.>1 %then %do;UNION%end;
					select pval&ot_k._test as pval_test
					from pval
				%end;
			)
			where pval_test^=""
			order by 1
			;
		quit;
		data test;
			footer=%do ot_i=1 %to %sysfunc(countw(%nrbquote(&tests_applied.),|));%if &ot_i.>1 %then %do;||%end;"^{super &ot_i.}%scan(&tests_applied.,&ot_i.,|) test p-value; " %end;;
			call symputx('footer', footer);
		run;
	%end;
	
	%if %length(&out.)=0 %then %do; 
		%let output_table=output_table;
	%end;
	%if %length(&out.)>0 %then %do; 
		%let output_table=&out.;
	%end;

/*	%if &method.=2 %then %do;*/
/*	proc sql;*/
/*		create table &output_table. as*/
/*			select stats.**/
/*				%if &asd.=1 %then %do;*/
/*					,asd_num, asd*/
/*				%end;*/
/*				%if &pval.=1 %then %do;*/
/*					,pval_num, pval*/
/*				%end;*/
/*			from stats*/
/*				%if &asd.=1 %then %do;*/
/*					left join asd on asd.ord=stats.ord and asd.row_desc=stats.row_desc*/
/*				%end;*/
/*				%if &pval.=1 %then %do;*/
/*					left join pval on pval.ord=stats.ord and pval.row_desc=stats.row_desc*/
/*				%end;*/
/*			order by ord, reord*/
/*		;*/
/*	quit;*/
/*	data &output_table.;*/
/*		set &output_table.;*/
/*		by ord reord;*/
/*		if last.ord then last=1;*/
/*	run;*/
/*	%end;*/

	proc sort data=stats;
		by ord ord2;
	run;
	data temp_out;
		merge stats(in=stats) 
			%if &asd.=1 %then %do;
				asd(in=asd_table drop=row_desc) 
			%end;
			%if &pval.=1 %then %do;
				pval(in=pval_table drop=row_desc)
			%end;
		;
		by ord ord2;
		if pval_table and not stats then delete;
	run;
	proc sort data=temp_out;
		by ord reord;
	run;
	data &output_table.;
		set temp_out;
		by ord reord;
		if last.ord then last=1;
	run;

	data temp_in;
		set &in.;
		%if %length(&where.)>0 %then %do;where &where.;%end;
	run;
	proc sort data=temp_in;
		by &by.;
	run;
	proc freq data=temp_in noprint;
		tables &by./ list out=temp_by;
		%if &weight.^=1 %then %do;weight &weight.;%end;
	run;
	data temp_by;
		set temp_by;
		%if %length(&by_order.)>0 %then %do;
			%do ot_i=1 %to %sysfunc(countw(&by_order.));
				if _N_=&ot_i. then by_order=%scan(&by_order., &ot_i.);
			%end;
		%end;
		%else %do;
			by_order=_N_;
		%end;
	run;
	data _null_;
		set temp_by;
		call symputx('name_'||put(by_order,1.), &by.);
		call symputx('n_'||put(by_order,1.), put(count,comma12.));
	run;
	proc sql noprint;
		select count(distinct &by.) as by_groups
		into :by_groups
		from temp_in
	quit;

	%if &print.=1 %then %do;
		proc report data=&output_table. split='^'
			STYLE(report) = [fontfamily=Arial borderleftstyle=none borderrightstyle=none borderbottomstyle=none bordertopstyle=none] 
			STYLE(header) = [fontfamily=Arial fontsize=10pt backgroundcolor=white fontweight=light borderbottomwidth=1 borderbottomstyle=solid bordertopstyle=solid] 
			STYLE(column) = [fontfamily=Arial paddingleft=5 paddingright=5 cellpadding=0 cellspacing=0 borderspacing=0 cellheight = 30] 
			STYLE(lines) = [fontfamily=Arial  borderbottomstyle=none bordertopstyle=solid ] 
		;
			column ord ord2 reord row_desc 
				%do ot_i=1 %to &by_groups.;
					stat_&ot_i.
				%end;
				%if &asd.=1 %then %do;
					%do ot_i=1 %to %sysfunc(countw(%nrbquote(&compare_list.),|));
						asd&ot_i._num asd&ot_i.
					%end;
				%end;
				%if &pval.=1 %then %do;
					%do ot_i=1 %to %sysfunc(countw(%nrbquote(&compare_list.),|));
						pval&ot_i._num pval&ot_i.
					%end;
				%end;
				
				last
			;

			define ord / order order=data noprint "";
			define ord2 / order order=data noprint "";
			define reord / order order=data noprint "";
			define row_desc / display "" STYLE=[asis=on just=left];
			%do ot_i=1 %to &by_groups.;
				define stat_&ot_i. / display "&&name_&ot_i..^(N=&&n_&ot_i..)" style=[textalign=center];
			%end;

			%if &asd.=1 %then %do;
				%do ot_i=1 %to %sysfunc(countw(%nrbquote(&compare_list.),|));
					%let compare_pair=%scan(%nrbquote(&compare_list.),&ot_i.,|);
					%let treated=%substr(&compare_pair.,1,1);
					%let compare=%substr(&compare_pair.,3,1);
					define asd&ot_i._num / display noprint "";
					%if &by_groups.>2 %then %do;
						define asd&ot_i. / display "&&name_&treated.^vs.^&&name_&compare.^ASD";
					%end;
					%if &by_groups.<=2 %then %do;
						define asd&ot_i. / display "ASD";
					%end;
				%end;
			%end;

			%if &pval.=1 %then %do;
				%do ot_i=1 %to %sysfunc(countw(%nrbquote(&compare_list.),|));
					%let compare_pair=%scan(%nrbquote(&compare_list.),&ot_i.,|);
					%let treated=%substr(&compare_pair.,1,1);
					%let compare=%substr(&compare_pair.,3,1);
					define pval&ot_i._num / display noprint "";
					%if &by_groups.>2 %then %do;
						define pval&ot_i. / display "&&name_&treated.^vs.^&&name_&compare.^P-value";
					%end;
					%if &by_groups.<=2 %then %do;
						define pval&ot_i. / display "P-value";
					%end;
				%end;
			%end;
			define last / display noprint "";

			compute last; 
				count+1; 
				if (mod(count,2))=0 then do; 
					CALL DEFINE(_ROW_, "STYLE", "STYLE=[BACKGROUND=greyef]"); 
				end; 
				if ord2=0 then do;
					CALL DEFINE("row_desc", "STYLE", "STYLE=[fontweight=bold]"); 
				end;
				if ord2>0 then do;
					CALL DEFINE("row_desc", "STYLE", "STYLE=[paddingleft=20]"); 
				end;
				if last=1 then do;
					CALL DEFINE(_ROW_, "style", "STYLE=[borderbottomstyle=solid]"); 
					if (mod(count,2))=0 then do; 
						CALL DEFINE(_ROW_, "STYLE", "STYLE=[BACKGROUND=greyef borderbottomstyle=solid]"); 
					end; 
				end;
				%if &asd.=1 %then %do;
					%do ot_i=1 %to %sysfunc(countw(%nrbquote(&compare_list.),|));
						if asd&ot_i._num>0.1 and asd&ot_i._num^=. then CALL DEFINE("asd&ot_i.", "style", "STYLE=[fontweight=bold]"); 
					%end;
				%end;
				%if &pval.=1 %then %do;
					%do ot_i=1 %to %sysfunc(countw(%nrbquote(&compare_list.),|));
						if pval&ot_i._num<0.05 and pval&ot_i._num^=. then CALL DEFINE("pval&ot_i.", "style", "STYLE=[fontweight=bold]"); 
					%end;
				%end;
			endcomp; 

			%if &pval.=1 %then %do;
				compute after;
					line @1 "&footer.";
				endcomp;
			%end;
		run;
	%end;

	*Delete temporary interim datasets;
	%if &debug=0 %then %do;
		proc datasets lib=work noprint;
			delete temp_: asd pval stats
			;
		run;
		quit;
	%end;
	%if %length(&out.)=0 %then %do; 
		proc datasets lib=work noprint;
			delete output_table
			;
		run;
		quit;
		 
	%end;
%mend;
/*
proc format;
  value yesno
    1='Yes'
	0='No'
  ;
run;
data demo_patients;
  do patient_id = 1 to 120;
    cohort = ifc(rand("uniform") < 0.5, "Treatment", "Control");
    gender = ifc(rand("uniform") < 0.5, "Male", "Female");
    age = round(18 + rand("uniform")*70, 1);   
    age_group = catx("-", put(floor(age/10)*10, 2.), put(floor(age/10)*10+9, 2.));
    r = ceil(rand("uniform")*3);
    select (r);
        when (1) race = "White";
        when (2) race = "Black";
        when (3) race = "Other";
    end;
    diabetes = (rand("uniform") < 0.25);
    obesity  = (rand("uniform") < 0.15);
    osteoporosis = (rand("uniform") < 0.10);
    output;
  end;

  label 
      patient_id    = 'Patient ID'
      cohort        = 'Cohort Assignment'
      gender        = 'Gender'
      age           = 'Age (years)'
      age_group     = 'Age Group (10-year intervals)'
      race          = 'Race / Ethnicity'
      diabetes      = 'Diabetes Status'
      obesity       = 'Obesity Status'
      osteoporosis  = 'Osteoporosis Status'
  ;
  format 
	diabetes yesno.
	obesity yesno.
	osteoporosis yesno.
  ;
  drop r;
run;

options nodate nonumber;
ods escapechar='^';
ods rtf file='C:\table_output.rtf';
%output_table(
  in=demo_patients,
  by=cohort,
  var_list=gender|age|age_group|race|diabetes|obesity|osteoporosis,
  types_list=2|1|2|2|2|2|2,
  pval=1
);
ods rtf close;


*/
