// Descriptives for Paper

use "snack_snad_merged_c_0502_TQ.dta", clear

************************************************
// Data Prep
************************************************

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

************************************************
// Outcomes
************************************************

sum MCI_rev
sum std_moca_raw // what is wrong with this one?
sum epmem
sum language

************************************************
// Bridging
************************************************

sum netsize
sum pfriend
sum diverse
sum weakest
sum density
sum b1density
sum sole
sum efctsize
sum bridging
sum avg_euc_age
sum diff_white 
sum diff_college
sum occcomplex_people
sum numphonecontact

************************************************
// Bridging
************************************************

sum marriage
sum family
sum friends
sum famsupp
sum frndsupp
sum padvice
sum pchores
sum ploan
sum msupport3
sum communityscale
sum pkin
sum pclose
sum mfreq
sum pprox

************************************************
// Stressor
************************************************

sum widowed
sum anybrevement
sum assisting_adult
sum phassles

************************************************
// Participation
************************************************

sum married
sum parenthood
sum working
sum grphouse
sum livealone
sum anyvolunteer
sum church_day
sum famfriends_day
sum smcount

************************************************
// Demographics
************************************************

* Age
sum ageall if datasource == 1 | datasource == 2 | datasource == 3
sum ageall if datasource == 4 | datasource == 5
sum ageall

* Gender
sum female if datasource == 1 | datasource == 2 | datasource == 3
sum female if datasource == 4 | datasource == 5
sum female
		
* Education
sum college if datasource == 1 | datasource == 2 | datasource == 3
sum college if datasource == 4 | datasource == 5
sum college

* Race
sum white if datasource == 1 | datasource == 2 | datasource == 3
sum white if datasource == 4 | datasource == 5
sum white



* Depression
sum gds if datasource == 1 | datasource == 2 | datasource == 3
sum gds if datasource == 4 | datasource == 5
sum gds

* FAQ

* different ranges, one standardized the other not
 * has problem because it is already standardized in SNAD
 
egen faq_new = rowtotal(func_checks func_tax func_shopping func_hobby func_coffee func_meal func_events func_tv func_appts func_travel)
compare faq_new faq if datasource == 1
compare faq_new faq if datasource == 2
compare faq_new faq if datasource == 3
compare faq_new faq if datasource == 4
compare faq_new faq if datasource == 5

* replace faq_new = . if datasource == 4 | datasource == 5
* egen std_faq_new = std(faq_new)
* compare std_faq_new faq if datasource == 1
* not identical. I don't know how faq was made originally...

replace faq = faq_new if datasource == 1 | datasource == 2 | datasource == 3


sum faq if datasource == 1 | datasource == 2 | datasource == 3
sum faq if datasource == 4 | datasource == 5
sum faq

* still different ranges even though constructed the same way, but I think that just means that people are less disabled in SNACK



/*
* rescale depression variable, harmonize dgs30 and gds15
**SNAD Wave 1-3 + SNACK W1 have gds15; SNACK W2 has gds30
tab datasource if gds15~=.
tab datasource if GDS_TOT~=.

** formula to transform gds30 to gds15 (Almeida, 1999)

gen gds15_snackequiv = 0.45 * GDS_TOT + 0.2
replace gds15_snackw2equiv = 15 if gds15_snackw2equiv > 15&gds15_snackw2equiv!=. 
 // max=15 for gds15

gen gds15_h= .
replace gds15_h = gds15_snackw2equiv if !missing(GDS_TOT)
replace gds15_h= gds15 if missing(GDS_TOT) 

sum gds15_h gds15

tab gds15_h datasource
tab gds30 datasource

gen snadequiv_gds30 = 1.57 + 1.95 * gds15
sum snadequiv_gds30
sum gds15
sum gds30
sum GDS_TOT

tab GDS_TOT datasource
tab snadequiv_gds30 datasource
tab gds30 datasource
tab gds15 datasource
tab gds15_snackw2equiv datasource
* the only one with snack w2 data is gds30, but it appears to be standardized
* problem with GDS_TOT is that not in w2 
