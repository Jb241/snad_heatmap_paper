****Project: social environments and cognitive health
****Data: Clean and Analyze SNACK Wave 1 &2 in 2025
****Author:  Tianyao Qu
****Date:    03/05/2025
****Updated: 03/05/2025

clear all
cd "C:\Users\windy\OneDrive - Indiana University\1_IU_Project\SNAD\4_Cog_Bri"

*** Import SNACK wave 1 dataset ***
use "snack_wave1_combined_c_0305_TQ", clear
sort SNACK_ID
*** Merging SNACK dataset ***
merge 1:1 SNACK_ID snackwave using "snack_wave2_combined_c_0305_TQ.dta", force
sort SNACK_ID snackwave
order SNACK_ID snackwave datasource
tab snackwave 
tab datasource
unique SNACK_ID 
drop _merge SUBID
gen SUBID = SNACK_ID

encode datasource, gen(datasource2)
fre datasource2 
drop datasource
recode datasource2 (1=91 "snack w1") (2=92 "snack w2"), gen(datasource)
fre datasource

*** Data inspection ***
** all cognitive function measures
sum CRAFTDRESD moca_raw execfxn epmem MCIdementia
fre MCIdementia            //diagnosed mild cognitive decline or dementia  (BINARY)
fre CRAFTDRESD             //"Craft Story Delayed Recalled, higher=better" (CONTINUOUS)
fre moca_raw               //moca score (CONTINUOUS)
fre execfxn                //executive memory (CONTINUOUS)
fre epmem                  //episodic memory (CONTINUOUS)

** control variables
sum moca_raw agesnack female white college gds30 faq

** predictors
sum married parenthood working livealone anyvolunteer  famfriends_day widowed  anybrevement  mhassles phassles uclaloneliness famsupp frndsupp marriage family friends pclose msupport3 mstrength mprox mfreq pprox ploan padvice pchores sdage diverse pkin  netsize b1density efctsize sole weakest sdstrength sdfreq bridging

*save "snack_w1w2_merged_c_0305_TQ.dta", replace


*****************************************
********** standardization for all variables in use 
*****************************************
use "snack_w1w2_merged_c_0305_TQ.dta", clear

// outcomes
sum moca_raw execfxn epmem CRAFTDRESD 
local dv_varlist `"moca_raw execfxn epmem CRAFTDRESD"'
foreach var in `dv_varlist'  {
	egen std_`var' = std(`var')
} 

** Controls
sum female agesnack white college gds30 faq
*binary: female white college
local dv_varlist `"agesnack gds30 faq"'
foreach var in `dv_varlist'  {
	egen std_`var' = std(`var')
} 

// social integration
sum  married kids parenthood working livealone anyvolunteer numvolunteer famfriends_f   famfriends_day
local dv_varlist `"married kids parenthood working livealone anyvolunteer numvolunteer famfriends_f    famfriends_day "'
foreach var in `dv_varlist'  {
	egen std_`var' = std(`var')
} 

// social stressors
sum widowed ladder anybrevement   mhassles phassles uclaloneliness   
local dv_varlist `"widowed ladder anybrevement  mhassles phassles uclaloneliness"'
foreach var in `dv_varlist'  {
	egen std_`var' = std(`var')
} 

// social bonding
sum famsupp frndsupp marriage family friends pclose msupport3 mstrength mprox pprox mfreq ploan padvice pchores
local dv_varlist `"famsupp frndsupp marriage family friends pclose msupport3 mstrength mprox pprox mfreq ploan padvice pchores"'
foreach var in `dv_varlist'  {
	egen std_`var' = std(`var')
} 

//social bridging
sum pwhite  pcollege sdage diverse pkin pfem netsize b1density efctsize sole weakest sdstrength sdfreq bridging
local dv_varlist `"pwhite  pcollege sdage diverse pkin pfem netsize b1density efctsize sole weakest sdstrength sdfreq bridging "'
foreach var in `dv_varlist'  {
	egen std_`var' = std(`var')
} 

*****************************************
*********** models and visuals **********
*****************************************
**** logistics  ****
unique SNACK_ID    //153 obs
tab snackwave        //wave indictor
tab datasource

**** each dependent variable for cognitive health  ****
sum std_moca_raw std_execfxn std_epmem std_CRAFTDRESD MCIdementia
recode MCIdementia (0=1 "Normal") (1=0 "MCI/dementia"), gen(MCI_rev)

**** descriptives for each group of social factors ****
*** 1. Controls ***
fre female
fre agesnack
fre white
fre college
fre gds30               //uds geriatric depression
fre faq                 //functional disbaility: higher= more dependent on other people on daily tasks

***************************************************
**** stacking estimates all in one spreadsheet ****
***************************************************
sum married parenthood working livealone anyvolunteer  std_famfriends_day  widowed  anybrevement  std_mhassles std_phassles std_uclaloneliness std_famsupp std_frndsupp std_marriage std_family std_friends  std_pclose std_msupport3 std_mstrength std_mprox std_mfreq std_pprox std_ploan std_padvice std_pchores   std_sdage std_diverse std_pkin  std_netsize std_b1density std_efctsize sole std_weakest std_sdstrength std_sdfreq 

///sum std_diff_college

//1. moca score 
eststo clear
local dv_varlist `"married parenthood working livealone anyvolunteer  std_famfriends_day  widowed  anybrevement  std_mhassles std_phassles std_uclaloneliness std_famsupp std_frndsupp std_marriage std_family std_friends  std_pclose std_msupport3 std_mstrength std_mprox std_mfreq std_pprox std_ploan std_padvice std_pchores   std_sdage std_diverse std_pkin  std_netsize std_b1density std_efctsize sole std_weakest std_sdstrength std_sdfreq bridging"'

foreach x in `dv_varlist'  {
	qui eststo `x': reg std_moca_raw c.std_agesnack female white college std_gds30 std_faq i.snackwave `x'
    qui mat N1 = e(N) * J(1, colsof(e(b)), 1)  // Ensure it's a row vector
    qui estadd mat N1 = N1

}

/*
foreach x in `dv_varlist'  {
	qui eststo `x': meglm std_moca_raw c.std_agesnack female white college std_gds30 std_faq `x'  ||SNACK_ID:
    mat N1 = e(N) * J(1, colsof(e(b)), 1)  // Ensure it's a row vector
    qui estadd mat N1 = N1
}
*/

eststo bivar: appendmodels $eststo
* Note: appendmodels is a stata program created in "stack_in_one_column.do" to stack all bivariate regression coefs in one column
esttab bivar using snack_coef_w1w2_0305.csv, replace ///
	  b(a2) se nonumbers nogap  ///
	 drop(std_agesnack female white college std_gds30 std_faq *.snackwave)  ///
	 ///bic aic scalars("ll Log lik." "chi2 Chi-squared") ///
	 star(+ 0.10 * 0.05 ** 0.01 *** 0.001) ///
	 	 title(1. predicting MoCA score)	 

//2. execfxn
eststo clear
local dv_varlist `"married parenthood working livealone anyvolunteer  std_famfriends_day  widowed  anybrevement  std_mhassles std_phassles std_uclaloneliness std_famsupp std_frndsupp std_marriage std_family std_friends  std_pclose std_msupport3 std_mstrength std_mprox std_mfreq std_pprox std_ploan std_padvice std_pchores   std_sdage std_diverse std_pkin  std_netsize std_b1density std_efctsize sole std_weakest std_sdstrength std_sdfreq bridging"'


foreach x in `dv_varlist'  {
	qui eststo `x': reg std_execfxn c.std_agesnack female white college std_gds30 std_faq i.snackwave  `x'
    qui mat N1 = e(N) * J(1, colsof(e(b)), 1)  // Ensure it's a row vector
    qui estadd mat N1 = N1

}

eststo bivar: appendmodels $eststo
* Note: appendmodels is a stata program created in "stack_in_one_column.do" to stack all bivariate regression coefs in one column
esttab bivar using snack_coef_w1w2_0305.csv, append ///
	  b(a2) se nonumbers nogap  ///
	 drop(std_agesnack female white college std_gds30 std_faq *.snackwave)  ///
	 ///bic aic scalars("ll Log lik." "chi2 Chi-squared") ///
	 star(+ 0.10 * 0.05 ** 0.01 *** 0.001) ///
	 	 title(1. predicting execfxn )	 
 


//3. epmem
eststo clear
local dv_varlist `"married parenthood working livealone anyvolunteer  std_famfriends_day  widowed  anybrevement  std_mhassles std_phassles std_uclaloneliness std_famsupp std_frndsupp std_marriage std_family std_friends  std_pclose std_msupport3 std_mstrength std_mprox std_mfreq std_pprox std_ploan std_padvice std_pchores   std_sdage std_diverse std_pkin  std_netsize std_b1density std_efctsize sole std_weakest std_sdstrength std_sdfreq bridging"'

foreach x in `dv_varlist'  {
	qui eststo `x': reg std_epmem c.std_agesnack female white college std_gds30 std_faq i.snackwave `x'
    qui mat N1 = e(N) * J(1, colsof(e(b)), 1)  // Ensure it's a row vector
    qui estadd mat N1 = N1

}

eststo bivar: appendmodels $eststo
* Note: appendmodels is a stata program created in "stack_in_one_column.do" to stack all bivariate regression coefs in one column
esttab bivar using snack_coef_w1w2_0305.csv, append ///
	  b(a2) se nonumbers nogap  ///
	 drop(std_agesnack female white college std_gds30 std_faq *.snackwave)  ///
	 ///bic aic scalars("ll Log lik." "chi2 Chi-squared") ///
	 star(+ 0.10 * 0.05 ** 0.01 *** 0.001) ///
	 	 title(1. predicting epmem )	

//4. CRAFTDRESD
eststo clear
local dv_varlist `"married parenthood working livealone anyvolunteer  std_famfriends_day  widowed  anybrevement  std_mhassles std_phassles std_uclaloneliness std_famsupp std_frndsupp std_marriage std_family std_friends  std_pclose std_msupport3 std_mstrength std_mprox std_mfreq std_pprox std_ploan std_padvice std_pchores   std_sdage std_diverse std_pkin  std_netsize std_b1density std_efctsize sole std_weakest std_sdstrength std_sdfreq bridging"'

foreach x in `dv_varlist'  {
	qui eststo `x': reg std_CRAFTDRESD c.std_agesnack female white college std_gds30 std_faq i.snackwave `x'
    qui mat N1 = e(N) * J(1, colsof(e(b)), 1)  // Ensure it's a row vector
    qui estadd mat N1 = N1

}

eststo bivar: appendmodels $eststo
* Note: appendmodels is a stata program created in "stack_in_one_column.do" to stack all bivariate regression coefs in one column
esttab bivar using snack_coef_w1w2_0305.csv, append ///
	  b(a2) se nonumbers nogap  ///
	 drop(std_agesnack female white college std_gds30 std_faq *.snackwave)  ///
	 ///bic aic scalars("ll Log lik." "chi2 Chi-squared") ///
	 star(+ 0.10 * 0.05 ** 0.01 *** 0.001) ///
	 	 title(1. predicting std_CRAFTDRESD )	
	 

//5. diagnosis based on moca score
eststo clear
local dv_varlist `"married parenthood working livealone anyvolunteer  std_famfriends_day  widowed  anybrevement  std_mhassles std_phassles std_uclaloneliness std_famsupp std_frndsupp std_marriage std_family std_friends  std_pclose std_msupport3 std_mstrength std_mprox std_mfreq std_pprox std_ploan std_padvice std_pchores   std_sdage std_diverse std_pkin  std_netsize std_b1density std_efctsize sole std_weakest std_sdstrength std_sdfreq bridging"'

foreach x in `dv_varlist'  {
	qui eststo `x': logit MCI_rev c.std_agesnack female white college std_gds30 std_faq i.snackwave `x'
    qui mat N1 = e(N) * J(1, colsof(e(b)), 1)  // Ensure it's a row vector
    qui estadd mat N1 = N1

}

eststo bivar: appendmodels $eststo
* Note: appendmodels is a stata program created in "stack_in_one_column.do" to stack all bivariate regression coefs in one column
esttab bivar using snack_coef_w1w2_0305.csv, append ///
	  b(a2) se nonumbers nogap  ///
	 drop(std_agesnack female white college std_gds30 std_faq *.snackwave)  ///
	 ///bic aic scalars("ll Log lik." "chi2 Chi-squared") ///
	 star(+ 0.10 * 0.05 ** 0.01 *** 0.001) ///
	 	 title(1. predicting MCI_rev )	





*** generate general variable for merging datasets
use "snack_w1w2_merged_c_0305_TQ.dta", clear
tab datasource
gen ageall = agesnack 












