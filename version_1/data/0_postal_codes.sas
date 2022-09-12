
/*
### ### ### ### ### ### ### ### ### ### ### ### ### #
### ### ### ### ### ### ### ### ### ### ### ### ### #
###          SAS script 0_postal_codes.sas        ###
###                Replication file               ###        
###  Cost Sharing and Accessibility in Publicly   ###
###         Funded Primary Care: Helsinki         ###
###               2022 by T. Haaga                ###
### ### ### ### ### ### ### ### ### ### ### ### ### #
### ### ### ### ### ### ### ### ### ### ### ### ### #

Content: Read data on postal code areas which will be used as 
			a clustering variable.

Inputs: tutkpalv_u1418_avohilmo201X_s where x in (1:4)
Output: Output: postal_codes_201x where x in (1:4)

Libnames: */

libname pc "D:\d66\external";
libname interm "W:\ASMA2\data\interim";

/*###
###*/


* Read all primary care contacts from 2011-2014;

options nolabel;

%MACRO read_data;
%DO i = 1 %TO 4;

data visits_1&i;
	set pc.tutkpalv_u1418_avohilmo201&i._s;


	/* Not all data are read:
		1) IDs must be observed.
		2) Postal code area must be observed. */
	where not missing(shnro) 
		and not missing(asiakas_postinumero);
	
	* Keep only relevant variables;
	keep shnro asiakas_postinumero;

	* Rename columns;
	rename shnro=id asiakas_postinumero=postal_code;

run;

%END;
%MEND read_data;

%read_data;


* Concatenate tables;

data visits;
	set visits_11 visits_12 visits_13 visits_14;
run;

* Sort;

proc sort Data=visits;
	by id postal_code;
run;

* Take unique ID-postal code rows;

proc sort data=visits NODUPRECS;
	by _all_;
run;


* Save;

proc export data=visits
	outfile= "W:\ASMA2\data\interim\postal_codes.csv"
	dbms=csv;
run;

* End;
