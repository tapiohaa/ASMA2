
/*
### ### ### ### ### ### ### ### ### ### ### ### ### #
### ### ### ### ### ### ### ### ### ### ### ### ### #
###   SAS script 0_specialist_consultations.sas   ###
###                Replication file               ###        
###  Cost Sharing and Accessibility in Publicly   ###
###            Funded Primary Care: Helsinki.     ###
###               2022 by T. Haaga                ###
### ### ### ### ### ### ### ### ### ### ### ### ### #
### ### ### ### ### ### ### ### ### ### ### ### ### #

Content: Read and mutate specialized health care contacts from Hilmo.

Inputs: tutkpalv_1418_thl_hilmoX_s where X in (1:2)
Output: Output: specialist_visits_YY where YY in (11:14)

Libnames: */

libname hilmo "D:\d66\external\THL_aineisto_2019_2020";
libname interm "W:\ASMA2\data\interim";

/*###
###*/


/*### ### ### ### ### ### ### ### ### ### ### ### ### 
#### 1) Read and mutate data on contacts in specialized healthcare. ####
### ### ### ### ### ### ### ### ### ### ### ### ###*/

* Now, read the data;

options nolabel;

%MACRO read_data;
%DO i = 1 %TO 2;

data contacts_&i;
	set hilmo.tutkpalv_1418_thl_hilmo&i._s;

	* IDs must be observed;
	where not missing(shnro);
	
	* Create a variable for day, month and year of the contact start date;
	date = input(substr(strip(tupva), 1, 10), DDMMYY10.);
	year = year(date);
	month = month(date);
	day = day(date);

	* Keep only relevant variables;
	keep shnro year month day pala ea kavijary satap;

	* Rename columns;
	rename shnro=id pala=service_area ea=specialty 
		kavijary=visitor_type satap=arrival_type;

	length month year day 3;

run;

%END;
%MEND read_data;

%read_data;


* Concatenate;
data contacts;
	set contacts_1 contacts_2;
run;


/*### ### ### ### ### ### ### ### ### ### ### ### ### 
#### 2) Save.  ####
### ### ### ### ### ### ### ### ### ### ### ### ###*/

%MACRO save_data;
%DO i = 2011 %TO 2014;

proc export data=contacts(where= (year=&i.))
	outfile= "W:\ASMA2\data\interim\specialist_visits_&i..csv"
	dbms=csv;
run;

%END;
%MEND save_data;

%save_data;

* End;
