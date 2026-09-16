****Project: social environments and cognitive health
****Purpose: related to network variable construction
****Data: SNACK WAVE 1 & 2 Long matched data
****Author:  Tianyao Qu
****Date:    4/30/2025
****Updated: 4/30/2025

**************************
**************************
*** SNAD network long form*
***************************
**************************

clear all
cd "C:\Users\windy\OneDrive - Indiana University\1_IU_Project\SNAD\4_Cog_Bri"

******************************************************
*** Import SNACK wave 2 network long form dataset ***
******************************************************
import excel "C:\Users\windy\OneDrive - Indiana University\1_IU_Project\SNAD\Focal\SNACK W2 data\250205_SNACK_W2_LONG-clean.xlsx", sheet("Sheet1") firstrow

*** merge with SNACK Wave 1 demograpics ***
destring SUBID, replace
destring *, replace

drop  SUBID
gen SUBID = SNACK_ID

merge m:1 SNACK_ID using "C:\Users\windy\OneDrive - Indiana University\1_IU_Project\SNAD\Focal\SNACK W2 data\snack_w1_demographics_02262025.dta", keepusing(female racesnack white college)
keep if _merge==3
drop _merge 

merge m:1 SUBID using "C:\Users\windy\OneDrive - Indiana University\1_IU_Project\SNAD\Focal\SNACK W2 data\SNACK_W2-no-net-vars.dta", keepusing(age)
keep if _merge==3
drop _merge 

**** logistics  ****
unique SUBID    //122 obs
tab alterid

sort SUBID alterid

******************************************
**** 1. Network Variable Construction ****
******************************************
** Proprotion of friends
tab relfriend
bysort SUBID: egen pfriend=mean(relfriend)
tab pfriend
lab var pfriend "Prop of friends"


** age heterophily **
//this is the age similarity using Euclidean distance of alter age from ego’s age, 
// as written in Brea's egocentric network analysis book, section 7.2.2.
// for converting it to difference, we need to ask Brea
gen snackage=age
sum snackage                // this is ego's age

gen age_abshet = abs(alterage - snackage)
lab var age_abshet "absolute difference between alters'age and ego age"

* 1. Compute squared differences for each ego–alter dyad
gen double agesqdiff = (alterage-snackage)^2

* 2. By ego, take the MEAN of those squared differences (i.e. sum ÷ n)
bysort SUBID: egen double mean_agesq = mean(agesqdiff)

* 3. Back to “years” by taking the square root
gen double avg_euc_age = sqrt(mean_agesq)

* 4. Label and inspect
label var avg_euc_age "Average Euclidean age‐distance per ego"
summarize avg_euc_age, detail
histogram avg_euc_age, discrete title("Avg. Age Euclidean Distance")

** education heterophily **
fre college
gen diff_college = 1-pcollege if college==1
replace diff_college = pcollege if college==0
lab var diff_college "Education heterophily" 

** race heterophily **
fre racesnack
recode racesnack (2=1 "asian") (3=2 "black") (6=3 "white") (else=4 "other"), gen(race4) //1Asian 2Black 3White 4Other, 4 categories in alter race
fre race4

label define alterrace 1 "Asian" 2 "African American" 3 "White" 4 "Other"
label values alterrace alterrace
fre alterrace

gen alterasian = alterrace==1
replace alterasian=. if alterrace==.
gen alterblack = alterrace==2
replace alterblack=. if alterrace==.
gen alterwhite = alterrace==3
replace alterwhite=. if alterrace==.
gen alterother = alterrace==4
replace alterother=. if alterrace==.

drop pwhite

bysort SUBID: egen pwhite=mean(alterwhite)
bysort SUBID: egen pblack=mean(alterblack)
bysort SUBID: egen pasian=mean(alterasian)
bysort SUBID: egen pother=mean(alterother)


gen     diff_race = 1-pwhite if race4==3
replace diff_race = 1-pblack if race4==2
replace diff_race = 1-pasian if race4==1
replace diff_race = 1-pother if race4==4
lab var diff_race "Race heterophily"

hist diff_race
graph close

gen diff_white = 1-pwhite if race4==3
replace diff_white = pwhite if race4==1|race4==2|race4==4
lab var diff_white "Race heterophily (White vs. nonWhite)"

/**** Summary ****/
fre pfriend        
fre avg_euc_age    
fre age_abshet
fre diff_race
fre diff_white
fre diff_college

/**** Covert Long (Network Alters) format into Wide ****/
*** The Exported data format will have one unique SUBID per year ***
order SUBID SNACK_ID
sum SNACK_ID alterid
keep SNACK_ID  alterid pfriend  avg_euc_age age_abshet  diff_race diff_white diff_college

sort SNACK_ID alterid

// covert dataset to Long time by time (longitutidal data)
collapse pfriend avg_euc_age age_abshet  diff_race diff_white diff_college , by(SNACK_ID)

// check logistics
// can use the SUBID and time to merge current dataset to other dataset in use
unique SNACK_ID

gen snackwave=2
gen datasource = "snack w2"
encode datasource, gen(datasource1)
recode datasource1 (1=5)
fre datasource1
drop datasource
rename datasource1 datasource

//labeling for later merging 
gen pfriend_snackw2=pfriend
gen diff_college_snackw2=diff_college 
gen avg_euc_age_snackw2=avg_euc_age 
gen diff_race_snackw2=diff_race 
gen diff_white_snackw2=diff_white 
gen age_abshet_snackw2=age_abshet 

// save dataset
*save "snackwave2_supp_short_0430_TQ.dta", replace


******************************************************
*** Import SNACK wave 1 network long form dataset ***
******************************************************
clear all
use "C:\Users\windy\OneDrive - Indiana University\1_IU_Project\SNAD\Focal\SNACK_Long_merged_05-05-2022.dta" , clear

merge m:1 SNACK_ID using "C:\Users\windy\OneDrive - Indiana University\1_IU_Project\SNAD\Focal\SNACK W2 data\snack_w1_demographics_02262025.dta", keepusing(college)
drop _merge 

** Proprotion of friends
tab relfriend
bysort SUBID: egen pfriend=mean(relfriend)
tab pfriend
lab var pfriend "Prop of friends"

** education heterophily **
fre college
gen diff_college = 1-pcollege if college==1
replace diff_college = pcollege if college==0
lab var diff_college "Education heterophily" 

** race heterophily
gen diff_white = 1-pwhite if white==1
replace diff_white = pwhite if white==0
lab var diff_white "Race heterophily (White vs. nonWhite)"


** age heterophily **
//this is the age similarity using Euclidean distance of alter age from ego’s age, 
// as written in Brea's egocentric network analysis book, section 7.2.2.
// for converting it to difference, we need to ask Brea
gen agesnack = age
sum agesnack                // this is ego's age

gen age_abshet = abs(alterage - agesnack)
lab var age_abshet "absolute difference between alters'age and ego age"

* 1. Compute squared differences for each ego–alter dyad
gen double agesqdiff = (alterage-agesnack)^2

* 2. By ego, take the MEAN of those squared differences (i.e. sum ÷ n)
bysort SUBID: egen double mean_agesq = mean(agesqdiff)

* 3. Back to “years” by taking the square root
gen double avg_euc_age = sqrt(mean_agesq)

* 4. Label and inspect
label var avg_euc_age "Average Euclidean age‐distance per ego"
summarize avg_euc_age, detail
histogram avg_euc_age, discrete title("Avg. Age Euclidean Distance")
graph close



/**** Summary ****/
fre pfriend        
fre diff_college
fre diff_white
fre avg_euc_age
fre age_abshet
/**** Covert Long (Network Alters) format into Wide ****/
*** The Exported data format will have one unique SUBID per year ***
sum SUBID  SNACK_ID alterid
keep SUBID SNACK_ID alterid pfriend diff_college diff_white avg_euc_age age_abshet

sort SUBID alterid
drop if SNACK_ID==""
// covert dataset to Long time by time (longitutidal data)
collapse pfriend diff_college diff_white avg_euc_age age_abshet, by(SNACK_ID)

// check logistics
// can use the SUBID and time to merge current dataset to other dataset in use
unique SNACK_ID
sort SNACK_ID  

gen snackwave=1
gen datasource = "snack w1"
encode datasource, gen(datasource1)
recode datasource1 (1=4)
fre datasource1
drop datasource
rename datasource1 datasource

// labeling for later merging
gen pfriend_snackw1=pfriend
gen diff_college_snackw1=diff_college
gen diff_white_snackw1=diff_white 
gen avg_euc_age_snackw1=avg_euc_age 
gen age_abshet_snackw1=age_abshet 


// save dataset
*save "snackwave1_supp_short_0430_TQ.dta", replace



