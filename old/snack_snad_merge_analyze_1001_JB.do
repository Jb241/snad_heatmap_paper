****Project: social environments and cognitive health
****Data: Clean and Analyze SNACK Wave 1 &2 and SNAD Wave 1-3
****Author:  Tianyao Qu
****Date:    03/05/2025
****Updated: 0807/2025


** analyses start at line 153

clear all
*cd "C:\Users\windy\OneDrive - Indiana University\1_IU_Project\SNAD\4_Cog_Bri"

***Note: Analyses begin in line 84
*use "snad_social_cog_0124_TQ.dta", clear

/*
*use "forvisuals_0130_JB.dta", clear
drop if time == 4 | time == 5
unique SUBID 

*** housekeeping
gen ageall= agesnad
gen snadwave= time
recode time (1=1 "snad w1") (2=2 "snad w2") (3=3 "snad w3"), gen(datasource)
fre datasource
fre snadwave
/*
replace datasource = "snad w1" if time==1
replace datasource = "snad w2" if time==2
replace datasource = "snad w3" if time==3
*/

sort SUBID snadwave

*** Merging SNACK dataset ***
drop source
append using "snack_w1w2_merged_c_0305_TQ.dta", generate(source) force //check the date for this file before executing
***check for errors
label define source 0 "SNAD" 1 "SNACK"
label values source source
tab source
fre datasource 

tab snadwave 
tab snackwave

replace datasource=4 if snackwave==1
replace datasource=5 if snackwave==2

recode datasource (1=1 "snad w1") (2=2 "snad w2") (3=3 "snad w3") (4=4 "snack w1") (5=5 "snack w2"), gen(datasource2)
drop datasource
rename datasource2 datasource 
sort SUBID datasource
order SUBID snadwave snackwave datasource source

fre datasource
unique SUBID  // SNAD
unique SNACK_ID  //SNACK


*** Data inspection ***
** all cognitive function measures
sum CRAFTDRESD moca_raw execfxn epmem MCIdementia language
fre MCIdementia            //diagnosed mild cognitive decline or dementia  (BINARY)
fre CRAFTDRESD             //"Craft Story Delayed Recalled, higher=better" (CONTINUOUS)
fre moca_raw               //moca score (CONTINUOUS)
fre execfxn                //executive memory (CONTINUOUS)
fre epmem                  //episodic memory (CONTINUOUS)
fre language
 
** control variables
sum gds15 gds30
egen gds = concat(gds15 gds30)
sum gds
sum moca_raw agesnack female white college faq

** predictors
drop widowed
recode life1 (1=1 "widowed") (0 2 =0), gen(widowed)
sum married parenthood working livealone anyvolunteer  famfriends_day widowed  anybrevement  mhassles phassles uclaloneliness famsupp frndsupp marriage family friends pclose msupport3 mstrength mprox mfreq pprox ploan padvice pchores occcomplex_people sdage diverse pkin  netsize b1density efctsize sole weakest sdstrength sdfreq bridging

*save "snack_snad_merged_c_0312_TQ.dta", replace

*/
*****************************************
********** standardization/merging for the final stacked dataset for all variables in use 
*****************************************
clear all
*cd "C:\Users\windy\OneDrive - Indiana University\1_IU_Project\SNAD\4_Cog_Bri"

use "Old/snack_snad_merged_c_0312_TQ.dta", clear
*drop std_*

tab datasource
destring datasource, replace

order SUBID SNACK_ID datasource
sort  SNACK_ID SUBID datasource
sum diff_college diff_race diff_white
  
** merge with suplementary network data **
merge m:m SUBID time using "r01net_supp_short_0331_TQ.dta"
drop _merge
sum diff_college diff_race diff_white avg_euc_age pfriend

*keep SUBID SNACK_ID datasource diff_college diff_race avg_euc_age pfriend
merge m:1 SNACK_ID using "snackwave1_supp_short_0430_TQ.dta"
drop _merge

merge m:1 SNACK_ID  using "snackwave2_supp_short_0430_TQ.dta", force
drop _merge

sum diff_college diff_race diff_white avg_euc_age pfriend

** cleaning & truncating **
order SUBID SNACK_ID datasource diff_college* diff_race* diff_white* avg_euc_age* pfriend*
sum diff_college* diff_race* diff_white* avg_euc_age* pfriend*

//combine 
replace diff_college_snackw1=. if datasource==5
replace diff_college_snackw2=. if datasource==4
egen diff_college_all = rowmean(diff_college diff_college_snackw1 diff_college_snackw2)
drop diff_college
rename diff_college_all diff_college

replace diff_race_snackw2=. if datasource==4
egen diff_race_all = rowmean(diff_race diff_race_snackw2)
drop diff_race
rename diff_race_all diff_race

replace avg_euc_age_snackw1=. if datasource==5
replace avg_euc_age_snackw2=. if datasource==4
egen avg_euc_age_all = rowmean(avg_euc_age avg_euc_age_snackw1 avg_euc_age_snackw2)
drop avg_euc_age
rename avg_euc_age_all avg_euc_age

replace pfriend_snackw1=. if datasource==5
replace pfriend_snackw2=. if datasource==4
egen pfriend_all = rowmean(pfriend pfriend_snackw1 pfriend_snackw2)
drop pfriend_snackw2
rename pfriend_all pfriend_snackw2

replace diff_white_snackw1=. if datasource==5
replace diff_college_snackw2=. if datasource==4
egen diff_college_all = rowmean(diff_college diff_college_snackw1 diff_college_snackw2)
drop diff_college
rename diff_college_all diff_college

sum diff_college diff_race avg_euc_age pfriend

//rescale some ivs
fre occcomplex_people 
recode occcomplex_people (0=8) (1=7) (2=6) (3=5) (4=4) (5=3) (6=2) (7=1) (8=0)  

fre weakest
replace weakest = 11 - weakest
label var weakest "Minimum tie strength reversed"

gen density_rev = abs(3 - density)
label var density_rev "Mean tie strength btwn alters/valued density"

recode MCIdementia (0=1 "Normal") (1=0 "MCI/dementia"), gen(MCI_rev)

*** saving dataset ***
// updated 0507, 2025
*save "snack_snad_merged_c_0502_TQ.dta", replace

// updated 0807, 2025
//rescale depression variable, harmonize dgs30 and gds15
/**SNAD Wave 1-3 + SNACK W1 have gds15; SNACK W2 has gds30
tab datasource if gds15~=.
tab datasource if GDS_TOT~=.

** formula to transform gds30 to gds15 (Almeida, 1999)
gen gds15_snackw2equiv = 0.45 * GDS_TOT + 0.2
replace gds15_snackw2equiv = 15 if gds15_snackw2equiv > 15&gds15_snackw2equiv!=.  // max=15 for gds15

gen gds15_h= .
replace gds15_h = gds15_snackw2equiv if !missing(GDS_TOT)
replace gds15_h= gds15 if missing(GDS_TOT) 

sum gds15_h gds15
*/

** harmonizing Z-score (gds15 from SNAD and gds30 from SNACK)
fre datasource if gds15~=.
egen gds15_std = std(gds15)
fre datasource if gds30~=.
gen gds_h = .
replace gds_h = gds15_std if datasource==1|datasource==2|datasource==3
replace gds_h = gds30 if datasource==4|datasource==5

fre gds_h

drop famfriends_day  
recode famfriends_f (0=.) (1=1) (2=6) (3=36) (4=156) (5=365), gen(famfriends_day)

*********************************************
*** analysis begins here, updated 0507 TQ ***
*********************************************
clear all
cd "/Users/jessicalogin/Library/CloudStorage/OneDrive-SharedLibraries-IndianaUniversity/Qu, Tianyao - 4_Cog_Bri"
use "snack_snad_merged_c_0502_TQ.dta", clear

drop diff_white
gen diff_white = 1-pwhite if white==1
replace diff_white = pwhite if white==0
lab var diff_white "Race heterophily (White vs. nonWhite)"

// outcomes
*sum moca_raw execfxn epmem CRAFTDRESD language
sum language epmem moca_raw 
local dv_varlist `"language epmem moca_raw "'
foreach var in `dv_varlist'  {
	egen std_`var' = std(`var')
} 

** Controls
// note: gds in snack data while dgs15 in snad data
sum female ageall white college  faq
*binary: female white college
local dv_varlist `"ageall faq  "'
foreach var in `dv_varlist'  {
	egen std_`var' = std(`var')
} 


// social participation
sum married parenthood working livealone grphouse anyvolunteer church_day famfriends_day smcount 
local dv_varlist `"married parenthood working livealone grphouse anyvolunteer church_day famfriends_day smcount"'
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
sum friends family marriage frndsupp famsupp msupport3 communityscale pkin pclose padvice pprox mfreq ploan pchores
local dv_varlist `"friends family marriage frndsupp famsupp msupport3 communityscale pkin pclose padvice pprox mfreq ploan pchores"'
foreach var in `dv_varlist'  {
	egen std_`var' = std(`var')
} 

//social bridging
sum bridging efctsize diverse density_rev density netsize avg_euc_age weakest occcomplex_people  diff_college diff_white numphonecontact pfriend sole diff_race sdage pcollege pwhite

local dv_varlist `"bridging efctsize diverse density_rev density netsize avg_euc_age weakest occcomplex_people  diff_college   diff_white numphonecontact pfriend sole diff_race sdage pcollege pwhite"'
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
fre datasource

encode SNACK_ID, gen (surveyid)
order SUBID SNACK_ID surveyid datasource
replace surveyid = SUBID if datasource==1|datasource==2|datasource==3
sum surveyid
recode datasource (1 2 3=1 "snad123") (4 5=2 "snack12"), gen(dummysource)
tab dummysource


**** each dependent variable for cognitive health  ****
sum  std_language std_epmem std_moca_raw MCI_rev
**** descriptives for each group of social factors ****
*** 1. Controls ***
fre female
fre ageall
fre white
fre college
fre gds_h                 //uds geriatric depression; std gds15 retrived from snad, gds 30 retrieved from snack; more=worse
fre std_faq                 //functional disbaility: higher= more dependent on other people on daily tasks

***************************************************
**** stacking estimates all in one spreadsheet ****
***************************************************
** ==44 vars in total
sum married parenthood working livealone grphouse anyvolunteer std_church_day std_famfriends_day std_smcount   ///
    assisting_adult widowed anybrevement phassles    ///
	std_friends std_family std_marriage std_frndsupp std_famsupp std_msupport3 std_communityscale std_pkin std_pclose std_padvice std_pprox std_mfreq std_ploan std_pchores   ///
	std_bridging std_efctsize std_diverse std_density_rev std_density std_netsize std_avg_euc_age std_weakest std_occcomplex_people std_diff_college diff_white std_numphonecontact std_pfriend sole std_diff_race std_sdage std_pcollege std_pwhite 

// null model
** 686 obs; ICC= .716968
*meglm std_language c.std_ageall female white college std_faq gds_h i.dummysource || surveyid:
	
//1. language
set matsize 11000
eststo clear
local dv_varlist `"married parenthood working livealone grphouse anyvolunteer std_church_day std_famfriends_day std_smcount  assisting_adult widowed anybrevement phassles std_friends std_family std_marriage std_frndsupp std_famsupp std_msupport3 std_communityscale std_pkin std_pclose std_padvice std_pprox std_mfreq std_ploan std_pchores std_bridging std_efctsize std_diverse std_density_rev std_density std_netsize std_avg_euc_age std_weakest std_occcomplex_people std_diff_college std_diff_white std_numphonecontact std_pfriend sole std_diff_race std_sdage std_pcollege std_pwhite "'


foreach x in `dv_varlist'  {
	qui eststo `x': meglm std_language c.std_ageall female white college std_faq gds_h i.dummysource `x' || surveyid:
    qui mat N1 = e(N) * J(1, colsof(e(b)), 1)  // Ensure it's a row vector
    qui estadd mat N1 = N1

}

eststo bivar: appendmodels $eststo
* Note: appendmodels is a stata program created in "stack_in_one_column.do" to stack all bivariate regression coefs in one column
esttab bivar using coefs_0720.csv, replace ///
	  b(a2) se nonumbers nogap  ///
	 drop(std_ageall female white college std_faq gds_h *.dummysource)  ///
	 ///bic aic scalars("ll Log lik." "chi2 Chi-squared") ///
	 star(+ 0.10 * 0.05 ** 0.01 *** 0.001) ///
	 	 title(1. language (verbal fluence))


//2. epmem
eststo clear
local dv_varlist `"married parenthood working livealone grphouse anyvolunteer std_church_day std_famfriends_day std_smcount  assisting_adult widowed anybrevement phassles std_friends std_family std_marriage std_frndsupp std_famsupp std_msupport3 std_communityscale std_pkin std_pclose std_padvice std_pprox std_mfreq std_ploan std_pchores std_bridging std_efctsize std_diverse std_density_rev std_density std_netsize std_avg_euc_age std_weakest std_occcomplex_people std_diff_college std_diff_white std_numphonecontact std_pfriend sole std_diff_race std_sdage std_pcollege std_pwhite"'

foreach x in `dv_varlist'  {
	qui eststo `x': meglm std_epmem c.std_ageall female white college std_faq gds_h i.dummysource `x' || surveyid:
    qui mat N1 = e(N) * J(1, colsof(e(b)), 1)  // Ensure it's a row vector
    qui estadd mat N1 = N1

}

eststo bivar: appendmodels $eststo
* Note: appendmodels is a stata program created in "stack_in_one_column.do" to stack all bivariate regression coefs in one column
esttab bivar using coefs_0720.csv, append ///
	  b(a2) se nonumbers nogap  ///
	 drop(std_ageall female white college std_faq gds_h *.dummysource)  ///
	 ///bic aic scalars("ll Log lik." "chi2 Chi-squared") ///
	 star(+ 0.10 * 0.05 ** 0.01 *** 0.001) ///
	 	 title(2. episodic memory)		


//3. moca score 
eststo clear
local dv_varlist `"married parenthood working livealone grphouse anyvolunteer std_church_day std_famfriends_day std_smcount  assisting_adult widowed anybrevement phassles std_friends std_family std_marriage std_frndsupp std_famsupp std_msupport3 std_communityscale std_pkin std_pclose std_padvice std_pprox std_mfreq std_ploan std_pchores std_bridging std_efctsize std_diverse std_density_rev std_density std_netsize std_avg_euc_age std_weakest std_occcomplex_people std_diff_college std_diff_white std_numphonecontact std_pfriend sole std_diff_race std_sdage std_pcollege std_pwhite"'

foreach x in `dv_varlist'  {
	qui eststo `x': meglm std_moca_raw c.std_ageall female white college std_faq gds_h i.dummysource `x' || surveyid:
    qui mat N1 = e(N) * J(1, colsof(e(b)), 1)  // Ensure it's a row vector
    qui estadd mat N1 = N1

}

eststo bivar: appendmodels $eststo
* Note: appendmodels is a stata program created in "stack_in_one_column.do" to stack all bivariate regression coefs in one column
esttab bivar using coefs_0720.csv, append ///
	  b(a2) se nonumbers nogap  ///
	 drop(std_ageall female white college std_faq gds_h *.dummysource)  ///
	 ///bic aic scalars("ll Log lik." "chi2 Chi-squared") ///
	 star(+ 0.10 * 0.05 ** 0.01 *** 0.001) ///
	 	 title(3. MoCA score)	 

		 
//4. diagnosis based on moca score
eststo clear
local dv_varlist `"married parenthood working livealone grphouse anyvolunteer std_church_day std_famfriends_day std_smcount  assisting_adult widowed anybrevement phassles std_friends std_family std_marriage std_frndsupp std_famsupp std_msupport3 std_communityscale std_pkin std_pclose std_padvice std_pprox std_mfreq std_ploan std_pchores std_bridging std_efctsize std_diverse std_density_rev std_density std_netsize std_avg_euc_age std_weakest std_occcomplex_people std_diff_college std_diff_white std_numphonecontact std_pfriend sole std_diff_race std_sdage std_pcollege std_pwhite"'

foreach x in `dv_varlist'  {
	qui eststo `x': meglm MCI_rev c.std_ageall female white college std_faq gds_h i.dummysource `x' || surveyid:
    qui mat N1 = e(N) * J(1, colsof(e(b)), 1)  // Ensure it's a row vector
    qui estadd mat N1 = N1

}

eststo bivar: appendmodels $eststo
* Note: appendmodels is a stata program created in "stack_in_one_column.do" to stack all bivariate regression coefs in one column
esttab bivar using coefs_0720.csv, append ///
	  b(a2) se nonumbers nogap  ///
	 drop(std_ageall female white college std_faq gds_h *.dummysource)  ///
	 ///bic aic scalars("ll Log lik." "chi2 Chi-squared") ///
	 star(+ 0.10 * 0.05 ** 0.01 *** 0.001) ///
	 	 title(4. Normal vs. MCI)	
	 






*********************************
******* Change score ************
*********************************

// recodes
* Create overall wave variable
gen snadsnackwave = .
replace snadsnackwave = snadwave if dummysource == 1
replace snadsnackwave = snackwave if dummysource == 2

* Create baseline variables
local varlist `"married parenthood working livealone grphouse anyvolunteer std_church_day std_famfriends_day std_smcount  assisting_adult widowed anybrevement phassles std_friends std_family std_marriage std_frndsupp std_famsupp std_msupport3 std_communityscale std_pkin std_pclose std_padvice std_pprox std_mfreq std_ploan std_pchores std_bridging std_efctsize std_diverse std_density_rev std_density std_netsize std_avg_euc_age std_weakest std_occcomplex_people std_diff_college std_diff_white std_numphonecontact std_pfriend sole std_diff_race std_sdage std_pcollege std_pwhite std_ageall female white college std_faq gds_h"'

foreach var in `varlist' {
	bysort surveyid (snadsnackwave): gen baseline_`var' = `var'[1]
}
tab baseline_white white, m // some changes
tab baseline_female female, m // no changes
tab baseline_college college, m // no changes
compare baseline_std_ageall std_ageall // some changes


* Create change score variables
local dv_varlist `"std_language std_epmem std_moca_raw MCI_rev"'

foreach var in `dv_varlist' {
	bysort surveyid (snadsnackwave): gen `var'_chg = `var' - `var'[_n-1]
}

// analysis

* Descriptives
local dv_varlist `"std_language std_epmem std_moca_raw MCI_rev"'

foreach var in `dv_varlist' {
	sum `var'
}
foreach var in `dv_varlist' { // cuts the N in half
	sum `var'_chg
}
 
* Example models

meglm std_language c.std_ageall female white college std_faq gds_h i.dummysource married || surveyid: // N=654, groups=373, not sig -
meglm std_language_chg c.baseline_std_ageall female baseline_white college baseline_std_faq baseline_gds_h i.dummysource baseline_married || surveyid: // N=291, groups=231, not sig +

meglm std_language c.std_ageall female white college std_faq gds_h i.dummysource std_bridging || surveyid: // N=683, groups=376, sig +
meglm std_language_chg c.baseline_std_ageall female baseline_white college baseline_std_faq baseline_gds_h i.dummysource baseline_std_bridging || surveyid: // N=308, groups=242, not sig +

meglm std_language c.std_ageall female white college std_faq gds_h i.dummysource std_efctsize || surveyid: // N=619, groups=363, sig +
meglm std_language_chg c.baseline_std_ageall female baseline_white college baseline_std_faq baseline_gds_h i.dummysource baseline_std_efctsize || surveyid: // N=236, groups=199, not sig -


* All models stacked 
set matsize 11000
eststo clear
local dv_varlist `"married parenthood working livealone grphouse anyvolunteer std_church_day std_famfriends_day std_smcount  assisting_adult widowed anybrevement phassles std_friends std_family std_marriage std_frndsupp std_famsupp std_msupport3 std_communityscale std_pkin std_pclose std_padvice std_pprox std_mfreq std_ploan std_pchores std_bridging std_efctsize std_diverse std_density_rev std_density std_netsize std_avg_euc_age std_weakest std_occcomplex_people std_diff_college std_diff_white std_numphonecontact std_pfriend sole std_diff_race std_sdage std_pcollege std_pwhite "'


foreach x in `dv_varlist'  {
	qui eststo `x': meglm std_language_chg c.baseline_std_ageall female baseline_white college baseline_std_faq baseline_gds_h i.dummysource baseline_`x' || surveyid:, iterate(100) tolerance(1e-6)
    qui mat N1 = e(N) * J(1, colsof(e(b)), 1)  // Ensure it's a row vector
    qui estadd mat N1 = N1

	qui estadd scalar N_obs = e(N)
    qui estadd scalar N_grp = e(N_g)[1,1] 
}

eststo bivar: appendmodels $eststo
* Note: appendmodels is a stata program created in "stack_in_one_column.do" to stack all bivariate regression coefs in one column
esttab bivar using chg_coefs_1001.csv, replace ///
	  b(a2) se nonumbers nogap  ///
	 drop(baseline_std_ageall female baseline_white college baseline_std_faq baseline_gds_h *.dummysource)  ///
	 ///bic aic scalars("ll Log lik." "chi2 Chi-squared") ///
	 star(+ 0.10 * 0.05 ** 0.01 *** 0.001)
	 
*esttab using sample_sizes.csv, replace ///
    cells(none) ///
    scalars("N_obs Observations" "N_grp Groups") ///
    noobs nomtitles	 
/*
preserve
clear
set obs `: word count `dv_varlist''
gen N_obs = .
gen N_grp = .

local i = 1
qui foreach x in `dv_varlist' {
    est restore `x'
    replace N_obs = e(N_obs) in `i'
    replace N_grp = e(N_grp) in `i'
    local i = `i' + 1
}

// Now you can use sum():
sum N_obs
sum N_grp

/*
. sum N_obs

    Variable |        Obs        Mean    Std. dev.       Min        Max
-------------+---------------------------------------------------------
       N_obs |         45    266.7111    54.59713         47        308

. sum N_grp

    Variable |        Obs        Mean    Std. dev.       Min        Max
-------------+---------------------------------------------------------
       N_grp |         45    210.9111    44.97819         41        242
*/
restore
*/

//2. epmem
eststo clear
local dv_varlist `"married parenthood working livealone grphouse anyvolunteer std_church_day std_famfriends_day std_smcount  assisting_adult widowed anybrevement phassles std_friends std_family std_marriage std_frndsupp std_famsupp std_msupport3 std_communityscale std_pkin std_pclose std_padvice std_pprox std_mfreq std_ploan std_pchores std_bridging std_efctsize std_diverse std_density_rev std_density std_netsize std_avg_euc_age std_weakest std_occcomplex_people std_diff_college std_diff_white std_numphonecontact std_pfriend sole std_diff_race std_sdage std_pcollege std_pwhite"'

foreach x in `dv_varlist'  {
	qui eststo `x': meglm std_epmem_chg c.baseline_std_ageall female baseline_white college baseline_std_faq baseline_gds_h i.dummysource baseline_`x' || surveyid:, iterate(100) tolerance(1e-6)
    qui mat N1 = e(N) * J(1, colsof(e(b)), 1)  // Ensure it's a row vector
    qui estadd mat N1 = N1

}

eststo bivar: appendmodels $eststo
* Note: appendmodels is a stata program created in "stack_in_one_column.do" to stack all bivariate regression coefs in one column
esttab bivar using chg_coefs_1001.csv, append ///
	  b(a2) se nonumbers nogap  ///
	 drop(baseline_std_ageall female baseline_white college baseline_std_faq baseline_gds_h *.dummysource)  ///
	 ///bic aic scalars("ll Log lik." "chi2 Chi-squared") ///
	 star(+ 0.10 * 0.05 ** 0.01 *** 0.001)	

//3. moca score 
eststo clear
local dv_varlist `"married parenthood working livealone grphouse anyvolunteer std_church_day std_famfriends_day std_smcount  assisting_adult widowed anybrevement phassles std_friends std_family std_marriage std_frndsupp std_famsupp std_msupport3 std_communityscale std_pkin std_pclose std_padvice std_pprox std_mfreq std_ploan std_pchores std_bridging std_efctsize std_diverse std_density_rev std_density std_netsize std_avg_euc_age std_weakest std_occcomplex_people std_diff_college std_diff_white std_numphonecontact std_pfriend sole std_diff_race std_sdage std_pcollege std_pwhite"'

foreach x in `dv_varlist'  {
	qui eststo `x': meglm std_moca_raw_chg c.baseline_std_ageall female baseline_white college baseline_std_faq baseline_gds_h i.dummysource baseline_`x' || surveyid:, iterate(100) tolerance(1e-6)
    qui mat N1 = e(N) * J(1, colsof(e(b)), 1)  // Ensure it's a row vector
    qui estadd mat N1 = N1

}

eststo bivar: appendmodels $eststo
* Note: appendmodels is a stata program created in "stack_in_one_column.do" to stack all bivariate regression coefs in one column
esttab bivar using chg_coefs_1001.csv, append ///
	  b(a2) se nonumbers nogap  ///
	 drop(baseline_std_ageall female baseline_white college baseline_std_faq baseline_gds_h *.dummysource)  ///
	 ///bic aic scalars("ll Log lik." "chi2 Chi-squared") ///
	 star(+ 0.10 * 0.05 ** 0.01 *** 0.001)	 

preserve
clear
set obs `: word count `dv_varlist''
gen N_obs = .
gen N_grp = .

local i = 1
qui foreach x in `dv_varlist' {
    est restore `x'
    replace N_obs = e(N_obs) in `i'
    replace N_grp = e(N_grp) in `i'
    local i = `i' + 1
}

// Now you can use sum():
sum N_obs
sum N_grp

/*
. sum N_obs

    Variable |        Obs        Mean    Std. dev.       Min        Max
-------------+---------------------------------------------------------
       N_obs |         45    266.7111    54.59713         47        308

. sum N_grp

    Variable |        Obs        Mean    Std. dev.       Min        Max
-------------+---------------------------------------------------------
       N_grp |         45    210.9111    44.97819         41        242
*/
restore
		 
//4. diagnosis based on moca score
eststo clear
local dv_varlist `"married parenthood working livealone grphouse anyvolunteer std_church_day std_famfriends_day std_smcount  assisting_adult widowed anybrevement phassles std_friends std_family std_marriage std_frndsupp std_famsupp std_msupport3 std_communityscale std_pkin std_pclose std_padvice std_pprox std_mfreq std_ploan std_pchores std_bridging std_efctsize std_diverse std_density_rev std_density std_netsize std_avg_euc_age std_weakest std_occcomplex_people std_diff_college std_diff_white std_numphonecontact std_pfriend sole std_diff_race std_sdage std_pcollege std_pwhite"'

foreach x in `dv_varlist'  {
	qui eststo `x': meglm MCI_rev_chg c.baseline_std_ageall female baseline_white college baseline_std_faq baseline_gds_h i.dummysource baseline_`x' || surveyid:, iterate(100) tolerance(1e-6)
    qui mat N1 = e(N) * J(1, colsof(e(b)), 1)  // Ensure it's a row vector
    qui estadd mat N1 = N1

}

eststo bivar: appendmodels $eststo
* Note: appendmodels is a stata program created in "stack_in_one_column.do" to stack all bivariate regression coefs in one column
esttab bivar using chg_coefs_1001.csv, append ///
	  b(a2) se nonumbers nogap  ///
	 drop(baseline_std_ageall female baseline_white college baseline_std_faq baseline_gds_h *.dummysource)  ///
	 ///bic aic scalars("ll Log lik." "chi2 Chi-squared") ///
	 star(+ 0.10 * 0.05 ** 0.01 *** 0.001)	

preserve
clear
set obs `: word count `dv_varlist''
gen N_obs = .
gen N_grp = .

local i = 1
qui foreach x in `dv_varlist' {
    est restore `x'
    replace N_obs = e(N_obs) in `i'
    replace N_grp = e(N_grp) in `i'
    local i = `i' + 1
}

// Now you can use sum():
sum N_obs
sum N_grp

/*
. sum N_obs

    Variable |        Obs        Mean    Std. dev.       Min        Max
-------------+---------------------------------------------------------
       N_obs |         45    266.7111    54.59713         47        308

. sum N_grp

    Variable |        Obs        Mean    Std. dev.       Min        Max
-------------+---------------------------------------------------------
       N_grp |         45    210.9111    44.97819         41        242
*/
restore











