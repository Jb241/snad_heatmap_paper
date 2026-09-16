****Project: social environments and cognitive health
****Data:  Construct baseline SNACK Wave 1 and SNAD Wave 1
****Author:  Tianyao Qu
****Date:    05/07/2025
****Updated: 05/07/2025


**** load data****
clear all
set more off 

cd "C:\Users\windy\OneDrive - Indiana University\1_IU_Project\SNAD\4_Cog_Bri"
*use "snack_snad_merged_c_0502_TQ.dta", clear

**** inspect data****
// total baseline respondents = 314+154 = 468
unique SUBID           // n=314
unique SNACK_ID        // n=154

fre datasource
drop if datasource==.

/*
//generate global ids
encode SNACK_ID, gen (surveyid)
order SUBID SNACK_ID surveyid datasource
replace surveyid = SUBID if datasource==1|datasource==2|datasource==3
sum surveyid
recode datasource (1 2 3=1 "snad123") (4 5=2 "snack12"), gen(dummysource)
*/
//locate all variables
sum parenthood working grphouse anyvolunteer church_f married smcount famfriends_day livealone ///
    assisting_adult widowed anybrevement phassles  ///
	friends family marriage frndsupp  pkin famsupp msupport3 communityscale pclose padvice pprox mfreq ploan pchores ///
	occcomplex_people efctsize b1density bridging density_rev diverse netsize diff_race weakest avg_euc_age numphonecontact diff_college pfriend sole if snadwave==1

sum  language epmem moca_raw MCI_rev if snadwave==1
sum female agesnad white college gds15 gds30 faq  if snadwave==1

sort SUBID datasource

***************************************
**1. for snad
****************************************
keep if datasource==1|datasource==2|datasource==3
fre snadwave

//Identify variables to impute (exclude ID and wave vars)
sort SUBID snadwave
bys SUBID: egen numwave =  count(snadwave)
tab numwave
order  SUBID datasource numwave

local dv_varlist `"parenthood working grphouse anyvolunteer church_f married smcount famfriends_day livealone assisting_adult widowed anybrevement phassles friends family marriage frndsupp  pkin famsupp msupport3 communityscale pclose padvice pprox mfreq ploan pchores occcomplex_people efctsize b1density bridging density_rev diverse netsize diff_race weakest avg_euc_age numphonecontact diff_college pfriend sole language epmem moca_raw MCI_rev female agesnad white college gds15 gds30 faq"'

//Forward fill (impute from future waves to earlier waves)
foreach x in `dv_varlist'  {
    bysort SUBID (snadwave): replace `x' = `x'[_n+1] if missing(`x')
	bysort SUBID (snadwave): replace `x' = `x'[_n+2] if missing(`x'[_n+1]) & numwave==3
}

keep if snadwave==1

//check number of obs
sum parenthood working grphouse anyvolunteer church_f married smcount famfriends_day livealone ///
    assisting_adult widowed anybrevement phassles  ///
	friends family marriage frndsupp  pkin famsupp msupport3 communityscale pclose padvice pprox mfreq ploan pchores ///
	occcomplex_people efctsize b1density bridging density_rev diverse netsize diff_race weakest avg_euc_age numphonecontact diff_college pfriend sole

sum  language epmem moca_raw MCI_rev
sum female agesnad white college gds15 faq 

*save "snad_baseline_imputed_0508_TQ.dta", replace


***************************************
**2. for snack
****************************************
clear all
set more off 

*use "snack_snad_merged_c_0502_TQ.dta", clear

**** inspect data****
// total baseline respondents = 314+154 = 468
unique SNACK_ID        // n=154

fre datasource
drop if datasource==.

//locate all variables
sum parenthood working grphouse anyvolunteer church_f married smcount famfriends_day livealone ///
    assisting_adult widowed anybrevement phassles  ///
	friends family marriage frndsupp  pkin famsupp msupport3 communityscale pclose padvice pprox mfreq ploan pchores ///
	occcomplex_people efctsize b1density bridging density_rev diverse netsize diff_race weakest avg_euc_age numphonecontact diff_college pfriend sole if snackwave==1

sum  language epmem moca_raw MCI_rev if snackwave==1
sum female agesnack white college gds15 gds30 faq  if snackwave==1

*** keep snack data
sort SNACK_ID datasource
keep if datasource==4|datasource==5
fre snackwave

//Identify variables to impute (exclude ID and wave vars)
sort SNACK_ID snackwave
bys SNACK_ID: egen numwave =  count(snackwave)
tab numwave
order  SNACK_ID datasource numwave

local dv_varlist `"parenthood working grphouse anyvolunteer church_f married smcount famfriends_day livealone assisting_adult widowed anybrevement phassles friends family marriage frndsupp  pkin famsupp msupport3 communityscale pclose padvice pprox mfreq ploan pchores occcomplex_people efctsize b1density bridging density_rev diverse netsize diff_race weakest avg_euc_age numphonecontact diff_college pfriend sole language epmem moca_raw MCI_rev female agesnack white college gds15 gds30 faq"'

//Forward fill (impute from future waves to earlier waves)
foreach x in `dv_varlist'  {
    bysort SUBID (snackwave): replace `x' = `x'[_n+1] if missing(`x')
}

keep if snackwave==1

// fixing some variables
merge 1:1 SNACK_ID using "snackwave1_supp_short_0430_TQ.dta", keepusing(pfriend)

//check number of obs
sum parenthood working grphouse anyvolunteer church_f married smcount famfriends_day livealone ///
    assisting_adult widowed anybrevement phassles  ///
	friends family marriage frndsupp  pkin famsupp msupport3 communityscale pclose padvice pprox mfreq ploan pchores ///
	occcomplex_people efctsize b1density bridging density_rev diverse netsize diff_race weakest avg_euc_age numphonecontact diff_college pfriend sole

sum  language epmem moca_raw MCI_rev
sum female agesnack white college gds15 faq 

**note:SNACK W1 or W2 does not have grphouse assisting_adult communityscale numphonecontact 

*save "snack_baseline_imputed_0508_TQ.dta"



***************************************
**3. append baseline datasets 
****************************************
clear all
set more off 

use "snack_baseline_imputed_0508_TQ.dta", clear
append using "snad_baseline_imputed_0508_TQ.dta"

// logistics
unique SUBID
unique SNACK_ID

tab datasource
tab snadwave
tab snackwave

/*
// saving datasets
save "snack_snad_baseline_imputed_0508_TQ.dta"
erase "snack_baseline_imputed_0508_TQ.dta"
erase "snad_baseline_imputed_0508_TQ.dta"
*/


**** loading baseline datatset ****
use "snack_snad_baseline_imputed_0508_TQ.dta", clear

// outcomes
sum moca_raw execfxn epmem CRAFTDRESD language
sum language epmem moca_raw 
local dv_varlist `"language epmem moca_raw "'
foreach var in `dv_varlist'  {
	egen std_`var' = std(`var')
} 

** Controls
// note: gds in snack data while dgs15 in snad data
sum female ageall white college gds30 gds15 faq
*binary: female white college
local dv_varlist `"ageall faq gds30  gds15"'
foreach var in `dv_varlist'  {
	egen std_`var' = std(`var')
} 

/*
gen std_gds = std_gds15 if datasource==1
replace std_gds = std_gds30 if datasource==2
sum std_gds

not use bc too much missingnesss
*/

// social participation
sum parenthood working grphouse anyvolunteer church_f married smcount famfriends_day livealone
local dv_varlist `"parenthood working grphouse anyvolunteer church_f married smcount famfriends_day livealone"'
foreach var in `dv_varlist'  {
	egen std_`var' = std(`var')
} 

// social stressors
sum assisting_adult widowed anybrevement phassles  
local dv_varlist `"assisting_adult widowed anybrevement phassles  "'
foreach var in `dv_varlist'  {
	egen std_`var' = std(`var')
} 

// social bonding
sum friends family marriage frndsupp  pkin famsupp msupport3 communityscale pclose padvice pprox mfreq ploan pchores
local dv_varlist `"friends family marriage frndsupp  pkin famsupp msupport3 communityscale pclose padvice pprox mfreq ploan pchores"'
foreach var in `dv_varlist'  {
	egen std_`var' = std(`var')
} 

//social bridging
sum occcomplex_people efctsize b1density bridging density_rev diverse netsize diff_race weakest avg_euc_age numphonecontact diff_college pfriend sole sdage pcollege pwhite

local dv_varlist `"occcomplex_people efctsize b1density bridging density_rev diverse netsize diff_race weakest avg_euc_age numphonecontact diff_college pfriend sole sdage pcollege pwhite"'
foreach var in `dv_varlist'  {
	egen std_`var' = std(`var')
} 


*****************************************
*********** models and visuals **********
*****************************************
**** logistics  ****
tab snackwave snadwave      //wave indictor
tab datasource

sort SUBID SNACK_ID datasource

**** each dependent variable for cognitive health  ****
sum  std_language std_epmem std_moca_raw MCI_rev
**** descriptives for each group of social factors ****
*** 1. Controls ***
fre female
fre agesnack
fre white
fre college
*fre std_gds                 //uds geriatric depression; std gds15 retrived from snad, gds 30 retrieved from snack; more=worse
fre std_gds15
fre std_faq                 //functional disbaility: higher= more dependent on other people on daily tasks

***************************************************
**** stacking estimates all in one spreadsheet ****
***************************************************
sum parenthood working grphouse anyvolunteer std_church_f married std_smcount std_famfriends_day livealone ///
    assisting_adult widowed anybrevement std_phassles   ///
	std_friends std_family std_marriage std_frndsupp std_pkin std_famsupp std_msupport3 std_communityscale std_pclose std_padvice std_pprox std_mfreq std_ploan std_pchores      ///
	std_occcomplex_people std_efctsize std_b1density std_bridging std_density_rev std_diverse std_netsize std_diff_race std_weakest avg_euc_age std_numphonecontact std_diff_college std_pfriend sole sdage pcollege pwhite

//1. language
set matsize 11000
eststo clear
local dv_varlist `"parenthood working grphouse anyvolunteer std_church_f married std_smcount std_famfriends_day livealone assisting_adult widowed anybrevement std_phassles std_friends std_family std_marriage std_frndsupp std_pkin std_famsupp std_msupport3 std_communityscale std_pclose std_padvice std_pprox std_mfreq std_ploan std_pchores	std_occcomplex_people std_efctsize std_b1density std_bridging std_density_rev std_diverse std_netsize std_diff_race std_weakest avg_euc_age std_numphonecontact std_diff_college std_pfriend sole sdage pcollege pwhite"'

foreach x in `dv_varlist'  {
	qui eststo `x': reg std_language c.std_ageall female white college std_faq std_gds15 i.datasource `x' 
    qui mat N1 = e(N) * J(1, colsof(e(b)), 1)  // Ensure it's a row vector
    qui estadd mat N1 = N1

}

eststo bivar: appendmodels $eststo
* Note: appendmodels is a stata program created in "stack_in_one_column.do" to stack all bivariate regression coefs in one column
esttab bivar using snack_snad_imputed_coef_0513.csv, replace ///
	  b(a2) se nonumbers nogap  ///
	 drop(std_ageall female white college std_faq std_gds15 *.datasource)  ///
	 ///bic aic scalars("ll Log lik." "chi2 Chi-squared") ///
	 star(+ 0.10 * 0.05 ** 0.01 *** 0.001) ///
	 	 title(1. predicting std_language (veral fluence))

//2. epmem
eststo clear
local dv_varlist `"parenthood working grphouse anyvolunteer std_church_f married std_smcount std_famfriends_day livealone assisting_adult widowed anybrevement std_phassles std_friends std_family std_marriage std_frndsupp std_pkin std_famsupp std_msupport3 std_communityscale std_pclose std_padvice std_pprox std_mfreq std_ploan std_pchores	std_occcomplex_people std_efctsize std_b1density std_bridging std_density_rev std_diverse std_netsize std_diff_race std_weakest avg_euc_age std_numphonecontact std_diff_college std_pfriend sole sdage pcollege pwhite "'

foreach x in `dv_varlist'  {
	qui eststo `x': reg std_epmem c.std_ageall female white college std_faq std_gds15 i.datasource `x' 
    qui mat N1 = e(N) * J(1, colsof(e(b)), 1)  // Ensure it's a row vector
    qui estadd mat N1 = N1

}

eststo bivar: appendmodels $eststo
* Note: appendmodels is a stata program created in "stack_in_one_column.do" to stack all bivariate regression coefs in one column
esttab bivar using snack_snad_imputed_coef_0513.csv, append ///
	  b(a2) se nonumbers nogap  ///
	 drop(std_ageall female white college std_faq std_gds15 *.datasource)  ///
	 ///bic aic scalars("ll Log lik." "chi2 Chi-squared") ///
	 star(+ 0.10 * 0.05 ** 0.01 *** 0.001) ///
	 	 title(1. predicting std_epmem score)		

//3. moca score 
eststo clear
local dv_varlist `"parenthood working grphouse anyvolunteer std_church_f married std_smcount std_famfriends_day livealone assisting_adult widowed anybrevement std_phassles std_friends std_family std_marriage std_frndsupp std_pkin std_famsupp std_msupport3 std_communityscale std_pclose std_padvice std_pprox std_mfreq std_ploan std_pchores	std_occcomplex_people std_efctsize std_b1density std_bridging std_density_rev std_diverse std_netsize std_diff_race std_weakest avg_euc_age std_numphonecontact std_diff_college std_pfriend sole sdage pcollege pwhite"'

foreach x in `dv_varlist'  {
	qui eststo `x': reg std_moca_raw c.std_ageall female white college std_faq std_gds15 i.datasource `x'
    qui mat N1 = e(N) * J(1, colsof(e(b)), 1)  // Ensure it's a row vector
    qui estadd mat N1 = N1

}

eststo bivar: appendmodels $eststo
* Note: appendmodels is a stata program created in "stack_in_one_column.do" to stack all bivariate regression coefs in one column
esttab bivar using snack_snad_imputed_coef_0513.csv, append ///
	  b(a2) se nonumbers nogap  ///
	 drop(std_ageall female white college std_faq std_gds15 *.datasource)  ///
	 ///bic aic scalars("ll Log lik." "chi2 Chi-squared") ///
	 star(+ 0.10 * 0.05 ** 0.01 *** 0.001) ///
	 	 title(1. predicting MoCA score)	 

		 
//4. diagnosis based on moca score
eststo clear
local dv_varlist `"parenthood working grphouse anyvolunteer std_church_f married std_smcount std_famfriends_day livealone assisting_adult widowed anybrevement std_phassles std_friends std_family std_marriage std_frndsupp std_pkin std_famsupp std_msupport3 std_communityscale std_pclose std_padvice std_pprox std_mfreq std_ploan std_pchores	std_occcomplex_people std_efctsize std_b1density std_bridging std_density_rev std_diverse std_netsize std_diff_race std_weakest avg_euc_age std_numphonecontact std_diff_college std_pfriend sole sdage pcollege pwhite  "'

foreach x in `dv_varlist'  {
	qui eststo `x': reg MCI_rev c.std_ageall female white college std_faq std_gds15 i.datasource `x'
    qui mat N1 = e(N) * J(1, colsof(e(b)), 1)  // Ensure it's a row vector
    qui estadd mat N1 = N1

}

eststo bivar: appendmodels $eststo
* Note: appendmodels is a stata program created in "stack_in_one_column.do" to stack all bivariate regression coefs in one column
esttab bivar using snack_snad_imputed_coef_0513.csv, append ///
	  b(a2) se nonumbers nogap  ///
	 drop(std_ageall female white college std_faq std_gds15 *.datasource)  ///
	 ///bic aic scalars("ll Log lik." "chi2 Chi-squared") ///
	 star(+ 0.10 * 0.05 ** 0.01 *** 0.001) ///
	 	 title(1. predicting MCI_rev)	
	 







