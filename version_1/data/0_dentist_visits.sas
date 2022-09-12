
/*
### ### ### ### ### ### ### ### ### ### ### ### ### #
### ### ### ### ### ### ### ### ### ### ### ### ### #
###        SAS script 0_dentist_visits.sas        ###
###                Replication file               ###        
###  Cost Sharing and Accessibility in Publicly   ###
###         Funded Primary Care: Helsinki         ###
###               2022 by T. Haaga                ###
### ### ### ### ### ### ### ### ### ### ### ### ### #
### ### ### ### ### ### ### ### ### ### ### ### ### #

Content: Read and mutate primary care dentist visits from Avohilmo for 2011-2014.

Inputs: tutkpalv_u1418_ammatti tutkpalv_u1418_ammattioikeudet
		tutkpalv_u1418_avohilmo201X_s where x in (1:4)
Output: Output: dentist_visits_201x where x in (1:4)

Libnames: */

libname pc "D:\d66\external";
libname raw "W:\ASMA2\data\raw";
libname interm "W:\ASMA2\data\interim";

/*###
###*/


/*### ### ### ### ### ### ### ### ### ### ### ### ### 
#### 1) Load classifications for doctors and nurses. ####
### ### ### ### ### ### ### ### ### ### ### ### ###*/

* TK ammattiluokitus 2001;
data tk_codes(drop=avo_raportointiryhma_nimi);
	set pc.tutkpalv_u1418_ammatti;
	where avo_raportointiryhma_koodi in ('10','11','30'); * Doctors, dentists, and nurses;
	rename tarkin_taso_koodi = ammatti;
run;

proc sort data=tk_codes;
	by avo_raportointiryhma_koodi;
run;

* Valvira ammattioikeudet 2008;
data valv_codes(drop=avo_raportointiryhma_nimi);
	set pc.tutkpalv_u1418_ammattioikeudet;
	where avo_raportointiryhma_koodi in ('10','11','30'); * Doctors, dentists, and nurses;
	rename ammattioikeus_koodi = kaynti_ammattioikeus;
run;

proc sort data=valv_codes;
	by avo_raportointiryhma_koodi;
run;

*  ;

/*
TK ammattiluokitus 2001:
Doctors: '222','2221', '2222' (dentists), '22211','22212','22213'
Nurses: '323','3231','32311','32312','32313','32314','32315','3232'

Valvira ammattioikeudet 2008
Doctors: '000','001','002','031','032','034','701','702','717','718','720','810','811','900','901','724','910','812'
Dentists: '003','004','033','035','333','703','704','713','904','911'
Nurses: '100','300','400','503','508','509','710','730','740','780','790','800','820','727','728','803'
*/


/*### ### ### ### ### ### ### ### ### ### ### ### ### 
#### 2) Read and mutate primary care visit data on uncancelled visits. ####
### ### ### ### ### ### ### ### ### ### ### ### ###*/

* Now, read the data;

options nolabel;

%MACRO read_data;
%DO i = 1 %TO 4;

data visits_1&i;
	set pc.tutkpalv_u1418_avohilmo201&i._s;


	/* Not all data are read:
		1) IDs must be observed.
		2) Time stamps must be observed.
		3) We only take dental care (T60).
		4) Of the contacts, we take those visits where the client physically
			visited professional.
		5) The contact was not cancelled. */

	where not missing(shnro) 
		and not missing(kaynti_alkoi) 
		and kaynti_palvelumuoto = 'T60'
		and kaynti_yhteystapa = 'R10'
		and missing(peruutus_ajankohta) and missing(peruutus_syy);
	

	/* Create variable profession such that:
		1 = dentists
		-1 = the rest */

	ammatti = put(kaynti_ammatti, 6. -L); 

	if ammatti in ('222','2222') or 
		kaynti_ammattioikeus in ('003','004','033','035','333','703','704','713','904','911') 
		then profession = 1;
	else profession = -1;

	
	* Create a variable for weekday, day, month and year of the visit;
	visits_date = datepart(kaynti_alkoi);
	month = month(visits_date);
	year = year(visits_date);
	day = day(visits_date);
	wday = weekday(visits_date);
	

	* Keep only relevant variables;
	keep shnro profession month year day wday;

	* Rename columns;
	rename shnro=id;

	length profession month year day wday 3;

run;

%END;
%MEND read_data;

%read_data;


/*### ### ### ### ### ### ### ### ### ### ### ### ### 
#### 3) Create data for analyses. ####
### ### ### ### ### ### ### ### ### ### ### ### ###*/

%MACRO visits_main;
%DO i = 1 %TO 4;

* Subset the data;

data visits_main_1&i;
	set visits_1&i;

	* Take only dentist visits (not on weekends);
	where profession = 1
		and wday in (2:6);
	
	drop profession wday;

run;

proc sort data=visits_main_1&i NODUPRECS;
	by _all_;
run;

* Save to cvs;

proc export data=visits_main_1&i
	outfile= "W:\ASMA2\data\interim\dentist_visits_201&i..csv"
	dbms=csv;
run;

%END;
%MEND visits_main;

%visits_main;


* End;
