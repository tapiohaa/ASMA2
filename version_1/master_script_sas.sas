
/*
### ### ### ### ### ### ### ### ### ### ### ### ### #
### ### ### ### ### ### ### ### ### ### ### ### ### #
###         SAS script master_script_sas.sas      ###
###                Replication file               ###        
###  Cost Sharing and Accessibility in Publicly   ###
###          Funded Primary Care: Helsinki        ###
###               2022 by T. Haaga                ###
### ### ### ### ### ### ### ### ### ### ### ### ### #
### ### ### ### ### ### ### ### ### ### ### ### ### #

# Content: This proposes an order with which to run SAS scripts. 

Note: We do not recommend running all the scripts at once as the SAS work
	library should be (manually) made empty between runs. However, if the scripts
	are run at ones, the running time is approximately 170 minutes.

Libnames: */

* Folders for processed datasets;
libname raw "W:\ASMA2\data\raw";
libname interm "W:\ASMA2\data\interim";

* Avohilmo and Hilmo visit data;
libname pc "D:\d66\external";
libname hilmo "D:\d66\external\THL_aineisto_2019_2020";

* Social assistance data;
libname toimtu "D:\d66\external";

* FOLK modules basic, income, and family;
libname fbasic "D:\ready-made\FOLK_perus_11a";
libname fincome "D:\ready-made\FOLK_tulo_11a";
libname ffamily "D:\ready-made\FOLK_perh_11a";


/*###
###*/

* Remove comment symbols before running the scipts one at a time;

/* 

filename storage "W:\ASMA2\data";

* Extract socioeconomic data and data on social assistance;
%inc storage("0_folk_data.sas");					* Approximately 38 minutes;
* Inputs: folk_20112020_tua_perus21tot_1 + folk_20112019_tua_tulo21tot_1 +;
*			folk_20112020_tua_perh21tot_1 (.sas7bdat);
* Output: folk_data_201X + social_assistance_201X where X in (1:4) (.csv);

* Extract postal codes from primary care data;
%inc storage("0_postal_codes.sas");					* Approximately 46 minutes;
* Inputs: tutkpalv_u1418_avohilmo201X_s where x in (1:4) (.sas7bdat);
* Output: postal_codes (.csv);

* Read primary care data to construct the visit data;
%inc storage("0_visits.sas");						* Approximately 34 minutes;
* Inputs: tutkpalv_u1418_ammatti + tutkpalv_u1418_ammattioikeudet + ;
*			tutkpalv_u1418_avohilmo201X_s where x in (1:4) (.sas7bdat);
* Outputs: visits_201x where x in (1:4) (.csv);

* Read dental care data to construct the dentist visit data;
%inc storage("0_dentist_visits.sas");				* Approximately 34 minutes;
* Inputs: tutkpalv_u1418_ammatti + tutkpalv_u1418_ammattioikeudet + ;
*			tutkpalv_u1418_avohilmo201X_s where x in (1:4) (.sas7bdat);
* Outputs: dentist_visits_201x where x in (1:4) (.csv);

* Read data on specialized healthcare contacts;
%inc storage("0_specialist_consultations.sas");		* Approximately 14 minutes;
* Inputs: tutkpalv_1418_thl_hilmoX_s where X in (1:2) (.sas7bdat);
* Output: referrals_YY where YY in (11:19) (.sas7bdat);

*/


* End;


