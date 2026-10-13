
// Recoding raw data for SNAD/SNACK heatmap paper
// Created: 09/16/26
// Data used:
//	snad-analysis-focal-r01match-20260701.dta
//	SNAD-Analysis-focal-LONG-R01match-20260120.dta
//  SNACK_W1_W2_W3.xlsx
//  SNACK-wave1-long.dta
//  250818_SNACK_W2_LONG-clean.dta
//  NC-LONG-clean-250825.dta
//  manual_fix_june10.dta

* Working directory (set to "Code"):
cd "/Users/jessicalogin/Desktop/Fall 2026 Working Folder/Code"

****************************************************************
****************************************************************
* SNAD
****************************************************************
****************************************************************
use "../Data/raw and intermediate/manual_fix_june10.dta", clear
merge 1:1 SUBID date_snad using "../Data/raw and intermediate/snad-analysis-focal-r01match-20260701.dta"
drop if _merge==1

replace rey_sum = rey_sum_fixed if _merge==3
replace delayed_rey_sum = delayed_rey_sum_fixed if _merge==3
replace trail_a_time = trail_a_time_fixed if _merge==3
replace trail_b_time = trail_b_time_fixed if _merge==3
replace moca_raw = moca_raw_fixed if _merge==3

******
// Data info vars
codebook SUBID
codebook time

******
// Covariates
codebook agesnad
clonevar age = agesnad

codebook female
label define sex_lbl 0 "Male" 1 "Female"
label values female sex_lbl

codebook white
label define race_lbl 0 "Not White" 1 "White"
label val white race_lbl

codebook edu
recode edu (1 2 3 4 = 0 "Less than BA") (5 6 = 1 "BA or more"), gen(college)
tab edu college, m

local faq_vars func_appts func_checks func_coffee func_events func_hobby func_meal func_shopping func_tax func_travel func_tv
codebook `faq_vars'
foreach var in `faq_vars' {
	replace `var' = . if `var' == 5 | `var' == 6
}
**# Bookmark #1 - double check that this is the right way to make faq:
alpha `faq_vars', gen(faq) asis

******
// Cognitive dependent variables
codebook moca_raw
codebook craftdre
codebook language // average of three standardized variables
codebook execfxn // standardized residual for regression of traila time on trailb time
codebook trail_b_time
codebook epmem // average of three standardized variables (including craftdre and delayed reyavlt)

codebook diagnosis_iadc
codebook diagnosis_moca
gen MCI_rev = .
replace MCI_rev = 0 if diagnosis_iadc == 2 | diagnosis_iadc == 3
replace MCI_rev = 1 if diagnosis_iadc == 1
replace MCI_rev = 0 if diagnosis_iadc == . & (diagnosis_moca == 2 | diagnosis_moca == 3)
replace MCI_rev = 1 if diagnosis_iadc == . & (diagnosis_moca == 1)
label define MCI_lbl 0 "MCI/dementia" 1 "No MCI/dementia"
label val MCI_rev MCI_lbl

******
// Mental health dependent variables
codebook gds15
gen gds15_rev = 15 - gds15

*codebook gad7_1 gad7_2 gad7_3 gad7_4 gad7_5 gad7_6 gad7_7 // lots missing
codebook anxiety1 anxiety2 anxiety3 anxiety4 // ranges from 0 (least anx) to 3 (most)
replace anxiety1 = . if anxiety1 == 4
replace anxiety2 = . if anxiety1 == 4
replace anxiety3 = . if anxiety1 == 4
replace anxiety4 = . if anxiety1 == 4
replace anxiety1 = anxiety1 + 1
replace anxiety2 = anxiety2 + 1
replace anxiety3 = anxiety3 + 1
replace anxiety4 = anxiety4 + 1

alpha anxiety1 anxiety2 anxiety3 anxiety4, gen(anxiety)
gen anxiety_rev = 4 - anxiety

codebook happiness // least 1 to most 10 happy
tab happiness, m nolab

codebook whole  // how would you describe your life as a whole? 1 (poor) to 4 (excellent)

******
// Social participation
codebook married

codebook kids
gen parenthood = .
replace parenthood = 1 if kids > 0
replace parenthood = 0 if kids == 0
label define parent_lbl 0 "Not parent" 1 "Parent"
label val parenthood parent_lbl

codebook employment
recode employment (1 2 = 1 "Part or full-time") (3/9 = 0 "Not currently working"), gen(working)
tab employment working, m

codebook living_number
gen livealone = .
replace livealone = 1 if living_number == 0
replace livealone = 0 if living_number > 0
label define live_lbl 0 "Live with people" 1 "Live alone"
label val livealone live_lbl
gen livealone_rev = 1 - livealone

codebook group_housing1
clonevar grphouse = group_housing1

codebook volunteer volunteer_help // volunteer for orgs or individuals
gen anyvolunteer = 0
replace anyvolunteer = 1 if volunteer == 1 | volunteer_help == 1
replace anyvolunteer = . if volunteer == . & ///
	(volunteer_help == 3 | volunteer_help == 4 | volunteer_help == .)
label define vol_lbl 0 "No volunteering" 1 "Volunteered"
label val anyvolunteer vol_lbl

codebook religious_attendance1
recode religious_attendance1 (1 = 52) (2 = 18) (3 = 4) (4 = 2) (5 = 0) , gen(church_day)
tab religious_attendance1 church_day, m

codebook famfriends_f
recode famfriends_f (0 = .) (1 = 1) (2 = 4) (3 = 48) (4 = 208) (5 = 360), gen(famfriends_day)
tab famfriends_f famfriends_day, m

local smvars social_media1___1 social_media1___2 social_media1___3 social_media1___4 social_media1___5 social_media1___6 social_media1___7 social_media1___8 social_media1___9 social_media1___10 social_media1___13 social_media1___11
codebook `smvars'
egen smcount = rowtotal(`smvars'), missing

******
// Social stressors
codebook assisting_adult
label define assist_lbl 0 "Not assisting" 1 "Yes assisting"
label val assisting_adult assist_lbl

codebook life1  		// only 17 yes
gen widowed = life1
label define wid_lbl 0 "Not widowed" 1 "Widowed"
label val widowed wid_lbl

codebook life1 life2 life3 life4 // only life1 and life3 are bereavement
gen anybrevement = 0
replace anybrevement = 1 if life1 == 1 | life3 == 1
replace anybrevement = . if life1 == . & life3 == .
label define breve_lbl 0 "No loss" 1 "Bereaved"
label val anybrevement breve_lbl

gen socialloss = .
replace socialloss = 0 if life1 == 0 & life3 == 0 & life4 == 0
replace socialloss = 1 if life1 == 1 | life3 == 1 | life4 == 1
label define loss_lbl 0 "No loss" 1 "Loss"
label val socialloss loss_lbl

clonevar illspouse = life2

codebook phassles

******
// Social bonding
codebook friends
codebook family
codebook marriage

* items are standardized and then scale is created:
codebook friends_help1 go_wrong1 joys_sorrows1 problems_friends1
alpha friends_help1 go_wrong1 joys_sorrows1 problems_friends1, std item gen(frndsupp)

* items are standardized and then scale is created:
codebook family_help1 help_support1 problems_family1 make_decisions1
alpha family_help1 help_support1 problems_family1 make_decisions1, std item gen(famsupp)

codebook msupport3


gen integration1_rev = 7 - integration1 // i don't belong (agree 1 - disagree 7)
codebook integration2 // i feel close to others (agree 1 - disagree 7)
codebook integration3 // source of comfort (agree 1 - disagree 7)
alpha integration1_rev integration2 integration3, gen(communityscale)
gen communityscale_rev = -communityscale

codebook pkin
codebook pclose
codebook padvice
codebook pprox
codebook mfreq
codebook ploan
codebook pchores

******
// Social bridging
codebook bridging
codebook efctsize
codebook diverse

codebook density
gen density_rev = abs(3 - density)

codebook b1density
gen b1density_rev = abs(1 - b1density)

codebook netsize

codebook weakest
replace weakest = 11 - weakest

codebook occcomplex_people
recode occcomplex_people (0=8) (1=7) (2=6) (3=5) (4=4) (5=3) (6=2) (7=1) (8=0)  

codebook sole
codebook sdage
codebook pcollege
codebook pwhite

tab contacts_phone
clonevar numphonecontact = contacts_phone 
replace numphonecontact = "1000" if contacts_phone == "1,000"
replace numphonecontact = "100" if contacts_phone == "100  "
replace numphonecontact = "1000" if contacts_phone == "1000 or more "
replace numphonecontact = "2426" if contacts_phone == "2,426"
replace numphonecontact = "328" if contacts_phone == "328  "
replace numphonecontact = "105" if contacts_phone == "a little over 100"
replace numphonecontact = "" if contacts_phone == "no cell phone and no contacts on home phone"
replace numphonecontact = "" if contacts_phone == "unknown"
replace numphonecontact = "" if contacts_phone == "unsure/did not have phone"
replace numphonecontact = "" if contacts_phone == "unsure"
replace numphonecontact = "" if contacts_phone == "unsure  "
replace numphonecontact = "250" if contacts_phone == "200 plus"
replace numphonecontact = "2000" if contacts_phone == "2,000"
replace numphonecontact = "1252" if contacts_phone == "1,252"
replace numphonecontact = "" if contacts_phone == "unsure due to not having smartphone "
replace numphonecontact = "18" if contacts_phone == "15-20"
replace numphonecontact = "" if contacts_phone == "does not have a cell phone"
replace numphonecontact = "565" if contacts_phone == "565  "
replace numphonecontact = "62" if contacts_phone == "62  "
destring numphonecontact, replace force
tab contacts_phone numphonecontact if numphonecontact == ., m

drop diff_college // drop heterogeneity measure to replace with heterophily
codebook college pcollege
gen diff_college = 1 - pcollege if college==1
replace diff_college = pcollege if college==0

* Save and then add in network variables
save "../Data/raw and intermediate/snad_heatmap_paper_intermediate.dta", replace

use "../Data/raw and intermediate/SNAD-Analysis-focal-LONG-R01match-20260120.dta", clear

codebook relfriend
bysort SUBID time: egen pfriend=mean(relfriend)

codebook alterage agesnad
replace alterage = . if alterage > 104
gen double agesqdiff = (alterage-agesnad)^2
bysort SUBID time: egen double mean_agesq = mean(agesqdiff)
gen double avg_euc_age = sqrt(mean_agesq)

codebook race alterrace
recode race (2 = 1 "Asian") (3 = 2 "African American") (5 = 3 "White") (1 4 6 = 4 "Other"), gen(race4)
gen alterasian = alterrace==1
replace alterasian=. if alterrace==.
gen alterblack = alterrace==2
replace alterblack=. if alterrace==.
gen alterwhite = alterrace==3
replace alterwhite=. if alterrace==.
gen alterother = alterrace==4
replace alterother=. if alterrace==.
drop pwhite pblack pasian pother
bysort SUBID time: egen pwhite=mean(alterwhite)
bysort SUBID time: egen pblack=mean(alterblack)
bysort SUBID time: egen pasian=mean(alterasian)
bysort SUBID time: egen pother=mean(alterother)

gen diff_race = 1 - pwhite if race4 == 3
replace diff_race = 1 - pblack if race4 == 2
replace diff_race = 1 - pasian if race4 == 1
replace diff_race = 1 - pother if race4 == 4

gen diff_white = 1 - pwhite if race4 == 3
replace diff_white = pwhite if race4 == 1 | race4 == 2 | race4 == 4

* Merge back with cleaned ego data
bysort SUBID time: keep if _n == 1
keep SUBID time pfriend avg_euc_age diff_race diff_white
merge 1:1 SUBID time using "../Data/raw and intermediate/snad_heatmap_paper_intermediate.dta"
drop if _merge == 1
drop _merge

******
// Labels
label var time "Wave"
label var age "Age"
label var female "Sex (ref=Male)"
label var white "Race (ref=Not white)"
label var college "Education (ref=Less than BA)"
label var faq "Functional ability scale"
label var moca_raw "MoCA score"
label var craftdre "Craft story 21 delayed recall paraphrase score"
label var language "Language (naming tasks average score)"
label var execfxn "Executive function (residual of trails a & b time)"
label var trail_b_time "Trails B time"
label var epmem "Episodic memory (tasks average)"
label var MCI_rev "Diagnosis (ref=MCI or dementia)"
label var gds15 "GDS-15 score"
label var anxiety "Anxiety scale"
label var happiness "Happiness"
label var whole "Life satisfaction"
label var married "Married (ref=Not married)"
label var parenthood "Parenthood (ref=Not a parent)"
label var working "Employed (ref=Not employed)"
label var livealone "Live alone (ref=Live with others)"
label var grphouse "Live in group housing (ref=Do not)"
label var anyvolunteer "Volunteered in past year (ref=No volunteering)"
label var church_day "Number of days attending religious service in past year"
label var smcount "Number of social media sites used"
label var assisting_adult "Assisting an adult (ref=Not assisting)"
label var anybrevement "Spouse, close relative, or close friend died in past year"
label var socialloss "Lost spouse, relative, or friend in past year"
label var illspouse "Seriously ill spouse in past year"
label var phassles "Proportion of alters who hassle"
label var friends "Friend relationship quality"
label var family "Family relationship quality"
label var marriage "Marriage relationship quality"
label var frndsupp "Percieved friends social support scale"
label var famsupp "Percieved family social support scale"
label var msupport3 "Average number of support functions provided by each alter"
label var communityscale "Community integration scale"
label var pkin "Proportion of alters who are kin"
label var pclose "Proportion of alters who are very close"
label var padvice "Proportion of alters who give suggestions for problems"
label var pprox "Proportion of alters who live within 30 minute drive"
label var mfreq "Average contact frequency by alters"
label var ploan "Proportion of alters who loan money"
label var pchores "Proportion of alters who help with daily chores"
label var bridging "Latent bridging scale"
label var efctsize "Effective network size"
label var diverse "Network relationship diversity"
label var density_rev "Alter-level tie strength valued density (reversed)"
label var b1density_rev "Network density (reversed)"
label var netsize "Number of alters in network"
label var weakest "Strength of weakest network tie (reversed)"
label var occcomplex_people "Occupational complexity with people (reversed)"
label var diff_college "Education heterophily" 
label var diff_white "Race heterophily (by white/nonwhite)"
label var diff_race "Race heterophily"
label var sole "Sole bridger in network"
label var sdage "Standard deviation of alters' ages"
label var pcollege "Proportion of alters with college degree"
label var pwhite "Proportion of alters who are White"
label var numphonecontact "Number of contacts in phone"
label var avg_euc_age "Average Euclidean age distance from ego"
label var pfriend "Proportion of alters who are friends"
label var famfriends_day "Days in a year visiting family/friends"
label var widowed "Spouse died in past year"
label var gds15_rev "GDS-15 reversed"
label var anxiety_rev "Anxiety scale reversed"
label var communityscale_rev "Community scale - reversed"

****
* Save cleaned SNAD data
keep SUBID time age female white college faq moca_raw craftdre language execfxn ///
    trail_b_time epmem MCI_rev gds15 anxiety happiness whole married parenthood ///
    working livealone grphouse anyvolunteer church_day smcount anybrevement socialloss ///
    illspouse phassles friends family marriage frndsupp famsupp msupport3 communityscale pkin ///
	pclose padvice pprox mfreq ploan pchores bridging efctsize diverse density_rev b1density_rev ///
    netsize weakest occcomplex_people diff_college diff_white diff_race sole sdage pcollege ///
	pwhite numphonecontact avg_euc_age pfriend famfriends_day widowed assisting_adult whole ///
	anxiety_rev gds15_rev communityscale_rev livealone_rev
	
gen source = 1
label define source_lbl 1 "SNAD"
label val source source_lbl

tostring SUBID, replace
save "../Data/snad_heatmap_paper_clean.dta", replace

****************************************************************
****************************************************************
* SNACK
****************************************************************
****************************************************************

local staticvars gender race edu kids
local gdsvars GDS_1 GDS_2 GDS_3 GDS_4 GDS_5 GDS_6 GDS_7 GDS_8 GDS_9 GDS_10 GDS_11 GDS_12 GDS_13 GDS_14 GDS_15 GDS_16 GDS_17 GDS_18 GDS_19 GDS_20 GDS_21 GDS_22 GDS_23 GDS_24 GDS_25 GDS_26 GDS_27 GDS_28 GDS_29 GDS_30
local gds15vars GDS_1 GDS_2 GDS_3 GDS_4 GDS_7 GDS_8 GDS_9 GDS_10 GDS_12 GDS_14 GDS_15 GDS_17 GDS_21 GDS_22 GDS_23
local anxvars anxiety1 anxiety2 anxiety3 anxiety4
local gadvars GAD_1 GAD_2 GAD_3 GAD_4 GAD_5 GAD_6 GAD_7
local smvars social_media1___1 social_media1___2 social_media1___3 social_media1___4 social_media1___5 social_media1___6 social_media1___7 social_media1___8 social_media1___9 social_media1___10 social_media1___11

foreach w in 1 2 3 {

    display as result _n "======== WAVE `w' ========"
cd "/Users/jessicalogin/Desktop/Fall 2026 Working Folder/Code"

import excel "../Data/raw and intermediate/SNACK_W1_W2_W3.xlsx", firstrow sheet("W`w'") clear

****** 
// Deal with wave inconsistent naming/contents
if `w' == 3 {
    rename gds_* GDS_*
    rename (family_help help_support friends_help) (family_help1 help_support1 friends_help1)
}

foreach v in `gdsvars' `gadvars' `smvars' religious_attendance1 {
    capture confirm variable `v'
    if _rc gen byte `v' = .
}

if `w' == 1 {
    preserve
    keep SUBID `staticvars'
    save "../Data/raw and intermediate/snack_static.dta", replace
    restore
}
else {
    merge 1:1 SUBID using "../Data/raw and intermediate/snack_static.dta", ///
        keepusing(`staticvars') update keep(master match match_update match_conflict) nogen
}

******
// Data info vars
codebook SUBID
gen time = `w'

******
// Covariates
codebook age

codebook gender // 2 is female, 1 is male
recode gender (2 = 1 "Female") (1 = 0 "Male"), gen(female)

codebook race // 1 is aian, 2 is asian, 3 is pac island, 4 is black, 5 is white, 6 is multi, 7 is unknown
recode race (5 = 1 "White") (1 2 3 4 6 = 0 "Not White"), gen(white)

codebook edu
gen college = .
replace college = 0 if edu == "1" | edu == "2" | edu == "3"
replace college = 1 if edu == "4" | edu == "5"
label define college_lbl 0 "Less than BA" 1 "BA or more", replace
label val college college_lbl
tab edu college, m

local faq_vars func_appts func_checks func_coffee func_events func_hobby func_meal func_shopping func_tax func_travel func_tv
codebook `faq_vars' // 0 normal, 1 some difficulty, 2 needs assist, 3 dependent, 0 NA
foreach var in `faq_vars' {
	destring `var', replace
	rename `var' `var'_low 
	gen `var' = `var'_low + 1
}
**# Bookmark #1 - double check that this is the right way to make faq:
alpha `faq_vars', gen(faq) asis

******
// Cognitive dependent variables
codebook moca_raw

codebook CRAFTDRE
rename CRAFTDRE craftdre

codebook animals veg MINTTOTS
egen animalsSD = std(animals)
egen vegSD = std(veg)
egen minttotsSD = std(MINTTOTS)
egen language=rowmean(animalsSD vegSD minttotsSD)

destring trail_a_time trail_b_time, replace
gen negTraila= -trail_a_time 
gen negTrailb= -trail_b_time 
regress negTrailb negTraila
predict Trailb_resid, residuals
egen execfxn = std(Trailb_resid)

codebook craftdre delayed_rey_sum UDSBENTD
destring delayed_rey_sum, replace
egen craftdreSD = std(craftdre)
egen reydelaySD = std(delayed_rey_sum)
egen udsbentdSD = std(UDSBENTD)
egen epmem=rowmean(craftdreSD reydelaySD udsbentdSD)

gen MCI_rev = .
replace MCI_rev = 0 if moca_raw <= 25
replace MCI_rev = 1 if moca_raw > 25
label define MCI_lbl 0 "MCI/dementia" 1 "No MCI/dementia", replace
label val MCI_rev MCI_lbl

******
// Mental health dependent variables

codebook `gdsvars'
foreach var in `gdsvars' {
	destring `var', replace
}
* note: 1 = Yes, 2 = No
recode GDS_1 (1 = 0) (2 = 1) // satisfied?
recode GDS_2 (2 = 0) (1 = 1) // dropped activities?
recode GDS_3 (2 = 0) (1 = 1) // empty?
recode GDS_4 (2 = 0) (1 = 1) // bored?
recode GDS_5 (1 = 0) (2 = 1) // hopeful?
recode GDS_6 (2 = 0) (1 = 1) // bothered?
recode GDS_7 (1 = 0) (2 = 1) // good spirits?
recode GDS_8 (2 = 0) (1 = 1) // afraid?
recode GDS_9 (1 = 0) (2 = 1) // happy?
recode GDS_10 (2 = 0) (1 = 1) // helpless?
recode GDS_11 (2 = 0) (1 = 1) // restless?
recode GDS_12 (2 = 0) (1 = 1) // stay at home?
recode GDS_13 (2 = 0) (1 = 1) // worry?
recode GDS_14 (2 = 0) (1 = 1) // memory problems?
recode GDS_15 (1 = 0) (2 = 1) // wonderful?
recode GDS_16 (2 = 0) (1 = 1) // downhearted?
recode GDS_17 (2 = 0) (1 = 1) // worthless?
recode GDS_18 (2 = 0) (1 = 1) // worry?
recode GDS_19 (1 = 0) (2 = 1) // exciting?
recode GDS_20 (2 = 0) (1 = 1) // hard?
recode GDS_21 (1 = 0) (2 = 1) // energy?
recode GDS_22 (2 = 0) (1 = 1) // hopeless?
recode GDS_23 (2 = 0) (1 = 1) // worse off?
recode GDS_24 (2 = 0) (1 = 1) // upset?
recode GDS_25 (2 = 0) (1 = 1) // cry?
recode GDS_26 (2 = 0) (1 = 1) // trouble concentrate?
recode GDS_27 (1 = 0) (2 = 1) // enjoy?
recode GDS_28 (2 = 0) (1 = 1) // avoid social?
recode GDS_29 (1 = 0) (2 = 1) // easy?
recode GDS_30 (1 = 0) (2 = 1) // clear mind?

egen gds30 = rowtotal(`gdsvars'), missing
egen gds15 = rowtotal(`gds15vars')
gen gds15_rev = 15 - gds15

codebook `gadvars'
foreach var in `gadvars' {
	destring `var', replace
}
egen gad = rowtotal(`gadvars'), missing

codebook `anxvars' // ranges 1-4
destring `anxvars', replace
alpha `anxvars', gen(anxiety)
gen anxiety_rev = 4 - anxiety

destring happiness, replace
recode happiness (1 = 0) (2 = 1) (3 = 2) (4 = 3) ///
	(5 = 4) (6 = 5) (7 = 6) (8 = 7) (9 = 8) (10 = 9) (12 = 10) ///
	(11 = .)
tab happiness, m nolab

destring whole, replace  // how would you describe your life as a whole?

******
// Social participation

destring marital, replace
gen married = .
replace married = 1 if marital == 2
replace married = 0 if marital == 1 | marital == 3 | marital == 4 | marital == 5 | marital == 6
label define mar_lbl 0 "Not married" 1 "Currently married", replace
label val married mar_lbl

destring kids, replace
gen parenthood = .
replace parenthood = 1 if kids > 0
replace parenthood = 0 if kids == 0
label define parent_lbl 0 "Not parent" 1 "Parent", replace
label val parenthood parent_lbl

destring employment, replace
recode employment (1 2 = 1 "Part or full-time") (3/9 = 0 "Not currently working"), gen(working)
tab employment working, m

if `w' == 2 {
	replace living_number = "1" if living_number == "One" | living_number == "one"
	replace living_number = "0" if living_number == "Just me" | ///
		living_number == "none" | living_number == "varies, I have an Airbnb in the space" ///
		| living_number == "zero" | living_number == "0ne"
}
if `w' == 3 {
	replace living_number = "1" if living_number == "one"
	replace living_number = "0" if living_number == "none" | living_number == "self" | living_number == "varies, no one else permanent but I run an Airbnb"
	replace living_number = "." if living_number == "200"
}
destring living_number, replace
gen livealone = .
replace livealone = 1 if living_number == 0
replace livealone = 0 if living_number > 0
label define live_lbl 0 "Live with people" 1 "Live alone", replace
label val livealone live_lbl
gen livealone_rev = 1 - livealone

destring living_own1, replace
gen grphouse = .
replace grphouse = 1 if living_own1 == 3
replace grphouse = 0 if living_own1 == 1 | living_own1 == 2 | living_own1 == 4
label define grp_lbl 0 "Not in group housing" 1 "Live in group housing", replace
label val grphouse grp_lbl

destring volunteer, replace
destring volunteer_help, replace
gen anyvolunteer = 0
replace anyvolunteer = 1 if volunteer == 1 | volunteer_help == 1
replace anyvolunteer = . if volunteer == . & ///
	(volunteer_help == 3 | volunteer_help == 4 | volunteer_help == .)
label define vol_lbl 0 "No volunteering" 1 "Volunteered", replace
label val anyvolunteer vol_lbl

destring religious_attendance1, replace
recode religious_attendance1 (9 = 52) (10 = 18) (11 = 4) (12 = 2) (13 = 0) , gen(church_day)

destring famfriends_f, replace
recode famfriends_f (9 = .) (8 = 0) (7 = 1) (6 = 4) (5 = 48) (4 = 208) (1 = 360), gen(famfriends_day)

codebook `smvars'
foreach var in `smvars' {
	destring `var', replace
}
egen smcount = rowtotal(`smvars'), missing

******
// Social stressors
*codebook assisting_adult (does not exist in SNACK)
*label define assist_lbl 0 "Not assisting" 1 "Yes assisting"
*label val assisting_adult assist_lbl
gen assisting_adult = .

destring life1 life2 life3 life4, replace		// only 4 yes
gen widowed = .
replace widowed = 1 if life1 == 1
replace widowed = 0 if life1 == 2
label define wid_lbl 0 "Not widowed" 1 "Widowed", replace
label val widowed wid_lbl

codebook life1 life2 life3 life4 // only life1 and life3 are bereavement
gen anybrevement = 0
replace anybrevement = 1 if life1 == 1 | life3 == 1
replace anybrevement = . if life1 == . & life3 == .
label define breve_lbl 0 "No loss" 1 "Bereaved", replace
label val anybrevement breve_lbl

gen socialloss = .
replace socialloss = 0 if life1 == 2 & life3 == 2 & life4 == 2
replace socialloss = 1 if life1 == 1 | life3 == 1 | life4 == 1
label define loss_lbl 0 "No loss" 1 "Loss", replace
label val socialloss loss_lbl

gen illspouse = .
replace illspouse = 1 if life2 == 1
replace illspouse = 0 if life2 == 2
label define ill_lbl 0 "Not ill" 1 "Ill spouse", replace
label val illspouse ill_lbl

save "../Data/raw and intermediate/snack_w`w'_intermed1.dta", replace
import excel "../Data/raw and intermediate/SNACK_W1_W2_W3.xlsx", firstrow sheet("W`w'_SNI") clear
capture confirm variable SUBID
	if _rc rename SNACK_ID SUBID
	else drop SNACK_ID
replace SUBID = ustrregexra(SUBID, "[^A-Za-z0-9_]", "")
keep SUBID phassles
merge 1:1 SUBID using "../Data/raw and intermediate/snack_w`w'_intermed1.dta"
drop _merge

******
// Social bonding
destring friends family marriage, replace

* items are standardized and then scale is created:
local frndvars friends_help1 go_wrong1 joys_sorrows1 problems_friends1
codebook `frndvars'
foreach var in `frndvars' {
	destring `var', replace
}
alpha `frndvars', std item gen(frndsupp)

* items are standardized and then scale is created:
local famvars family_help1 help_support1 problems_family1 make_decisions1
codebook `famvars'
foreach var in `famvars' {
	destring `var', replace
}
alpha `famvars', std item gen(famsupp)

save "../Data/raw and intermediate/snack_w`w'_intermed2.dta", replace
import excel "../Data/raw and intermediate/SNACK_W1_W2_W3.xlsx", firstrow sheet("W`w'_SNI") clear
capture confirm variable SUBID
	if _rc rename SNACK_ID SUBID
	else drop SNACK_ID
replace SUBID = ustrregexra(SUBID, "[^A-Za-z0-9_]", "")
keep SUBID msupport3 pkin pclose padvice pprox mfreq ploan pchores
merge 1:1 SUBID using "../Data/raw and intermediate/snack_w`w'_intermed2.dta"
drop _merge

codebook msupport3

*codebook integration1 integration2 integration3 (does not exist in SNACK)
gen communityscale_rev = .

codebook pkin
codebook pclose
codebook padvice
codebook pprox
codebook mfreq
codebook ploan
codebook pchores

******
// Social bridging

save "../Data/raw and intermediate/snack_w`w'_intermed3.dta", replace
import excel "../Data/raw and intermediate/SNACK_W1_W2_W3.xlsx", firstrow sheet("W`w'_SNI") clear
capture confirm variable SUBID
	if _rc rename SNACK_ID SUBID
	else drop SNACK_ID
replace SUBID = ustrregexra(SUBID, "[^A-Za-z0-9_]", "")
keep SUBID netsize diverse weakest density efctsize sole density b1density sdage pcollege pwhite
merge 1:1 SUBID using "../Data/raw and intermediate/snack_w`w'_intermed3.dta"
drop _merge

sem (Bridging -> netsize diverse weakest density efctsize sole) if netsize>0 & !missing(netsize) ///
    , var(Bridging@1) method(mlmv) vce(cluster SUBID) cov(e.netsize*e.efctsize) cov(e.netsize*e.density) cov(e.diverse*e.density) cov(e.density*e.sole) cov(e.diverse*e.weakest) cov(e.netsize*e.diverse) cov(e.weakest*e.sole) difficult
estat gof, stats(all)
estat mindices
predict bridging,latent(Bridging) 
label var bridging "Bridging capital"
*adjust bridging when netsize=0 or missing
replace bridging=. if missing(netsize) 
sum bridging
local min=r(min)
replace bridging=`min' if netsize==0 //set bridging to minimum value when netsize=0

codebook bridging
codebook efctsize
codebook diverse

codebook density
gen density_rev = abs(3 - density)

codebook b1density
gen b1density_rev = abs(1 - b1density)

codebook netsize

codebook weakest
replace weakest = 11 - weakest

codebook sole
codebook sdage
codebook pcollege
codebook pwhite

codebook college pcollege
gen diff_college = 1 - pcollege if college==1
replace diff_college = pcollege if college==0

*codebook occcomplex_people (does not exist in SNACK data)
*recode occcomplex_people (0=8) (1=7) (2=6) (3=5) (4=4) (5=3) (6=2) (7=1) (8=0)  
gen occcomplex_people = .

/* tab contacts_phone (does not exist in SNACK)
clonevar numphonecontact = contacts_phone 
replace numphonecontact = "1000" if contacts_phone == "1,000"
replace numphonecontact = "100" if contacts_phone == "100  "
replace numphonecontact = "1000" if contacts_phone == "1000 or more "
replace numphonecontact = "2426" if contacts_phone == "2,426"
replace numphonecontact = "328" if contacts_phone == "328  "
replace numphonecontact = "105" if contacts_phone == "a little over 100"
replace numphonecontact = "" if contacts_phone == "no cell phone and no contacts on home phone"
replace numphonecontact = "" if contacts_phone == "unknown"
replace numphonecontact = "" if contacts_phone == "unsure/did not have phone"
replace numphonecontact = "" if contacts_phone == "unsure"
replace numphonecontact = "" if contacts_phone == "unsure  "
replace numphonecontact = "250" if contacts_phone == "200 plus"
replace numphonecontact = "2000" if contacts_phone == "2,000"
replace numphonecontact = "1252" if contacts_phone == "1,252"
replace numphonecontact = "" if contacts_phone == "unsure due to not having smartphone "
replace numphonecontact = "18" if contacts_phone == "15-20"
replace numphonecontact = "" if contacts_phone == "does not have a cell phone"
replace numphonecontact = "565" if contacts_phone == "565  "
replace numphonecontact = "62" if contacts_phone == "62  "
destring numphonecontact, replace force
tab contacts_phone numphonecontact if numphonecontact == ., m
*/
gen numphonecontact = .

* Save and then add in alter-level network variables
save "../Data/raw and intermediate/snack_w`w'_intermed4.dta", replace
keep SUBID age race
save "../Data/raw and intermediate/snack_w`w'_intermed5.dta", replace

if `w' == 1 {
    use "/Users/jessicalogin/Library/CloudStorage/OneDrive-IndianaUniversity/Indiana/Data/SNACK-wave1-long.dta", clear
}
else if `w' == 2 {
    use "/Users/jessicalogin/Library/CloudStorage/OneDrive-IndianaUniversity/Indiana/Data/250818_SNACK_W2_LONG-clean.dta", clear
}
else {
    use "/Users/jessicalogin/Library/CloudStorage/OneDrive-IndianaUniversity/Indiana/Data/NC-LONG-clean-250825.dta", clear
}

capture confirm string variable SUBID
if _rc {
    rename SUBID SUBID_long
    decode SUBID_long, gen(SUBID)
}
replace SUBID = ustrregexra(SUBID, "[^A-Za-z0-9_]", "")
merge m:1 SUBID using "../Data/raw and intermediate/snack_w`w'_intermed5.dta"

codebook relfriend
bysort SUBID: egen pfriend=mean(relfriend)

codebook alterage
replace alterage = . if alterage > 104
gen double agesqdiff = (alterage-age)^2
bysort SUBID: egen double mean_agesq = mean(agesqdiff)
gen double avg_euc_age = sqrt(mean_agesq)

codebook race alterrace
recode race (2 = 1 "Asian") (4 = 2 "African American") (5 = 3 "White") (1 3 6 = 4 "Other") (7 = .), gen(race4)
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

gen diff_race = 1 - pwhite if race4 == 3
replace diff_race = 1 - pblack if race4 == 2
replace diff_race = 1 - pasian if race4 == 1
replace diff_race = 1 - pother if race4 == 4

gen diff_white = 1 - pwhite if race4 == 3
replace diff_white = pwhite if race4 == 1 | race4 == 2 | race4 == 4

bysort SUBID: keep if _n == 1
keep SUBID pfriend avg_euc_age diff_race diff_white

merge 1:1 SUBID using "../Data/raw and intermediate/snack_w`w'_intermed4.dta"
drop if _merge == 1
drop _merge

******
// Labels
label var time "Wave"
label var age "Age"
label var female "Sex (ref=Male)"
label var white "Race (ref=Not white)"
label var college "Education (ref=Less than BA)"
label var faq "Functional ability scale"
label var moca_raw "MoCA score"
label var craftdre "Craft story 21 delayed recall paraphrase score"
label var language "Language (naming tasks average score)"
label var execfxn "Executive function (residual of trails a & b time)"
label var trail_b_time "Trails B time"
label var epmem "Episodic memory (tasks average)"
label var MCI_rev "Diagnosis (ref=MCI or dementia)"
label var gds15 "GDS-15 score"
label var anxiety "Anxiety scale"
label var gad "GAD-7 score"
label var happiness "Happiness"
label var whole "Life satisfaction"
label var married "Married (ref=Not married)"
label var parenthood "Parenthood (ref=Not a parent)"
label var working "Employed (ref=Not employed)"
label var livealone "Live alone (ref=Live with others)"
label var grphouse "Live in group housing (ref=Do not)"
label var anyvolunteer "Volunteered in past year (ref=No volunteering)"
label var church_day "Number of days attending religious service in past year"
label var smcount "Number of social media sites used"
label var assisting_adult "Assisting an adult (ref=Not assisting)"
label var anybrevement "Spouse, close relative, or close friend died in past year"
label var socialloss "Lost spouse, relative, or friend in past year"
label var illspouse "Seriously ill spouse in past year"
label var phassles "Proportion of alters who hassle"
label var friends "Friend relationship quality"
label var family "Family relationship quality"
label var marriage "Marriage relationship quality"
label var frndsupp "Percieved friends social support scale"
label var famsupp "Percieved family social support scale"
label var msupport3 "Average number of support functions provided by each alter"
label var communityscale "Community integration scale"
label var pkin "Proportion of alters who are kin"
label var pclose "Proportion of alters who are very close"
label var padvice "Proportion of alters who give suggestions for problems"
label var pprox "Proportion of alters who live within 30 minute drive"
label var mfreq "Average contact frequency by alters"
label var ploan "Proportion of alters who loan money"
label var pchores "Proportion of alters who help with daily chores"
label var bridging "Latent bridging scale"
label var efctsize "Effective network size"
label var diverse "Network relationship diversity"
label var density_rev "Alter-level tie strength valued density (reversed)"
label var b1density_rev "Network density (reversed)"
label var netsize "Number of alters in network"
label var weakest "Strength of weakest network tie (reversed)"
label var occcomplex_people "Occupational complexity with people (reversed)"
label var diff_college "Education heterophily" 
label var diff_white "Race heterophily (by white/nonwhite)"
label var diff_race "Race heterophily"
label var sole "Sole bridger in network"
label var sdage "Standard deviation of alters' ages"
label var pcollege "Proportion of alters with college degree"
label var pwhite "Proportion of alters who are White"
label var numphonecontact "Number of contacts in phone"
label var avg_euc_age "Average Euclidean age distance from ego"
label var pfriend "Proportion of alters who are friends"
label var famfriends_day "Days in a year visiting family/friends"
label var widowed "Spouse died in past year"
label var gds15_rev "GDS-15 reversed"
label var anxiety_rev "Anxiety scale reversed"
label var communityscale_rev "Community scale - reversed"

keep SUBID time age female white college faq moca_raw craftdre language execfxn ///
    trail_b_time epmem MCI_rev gds15 anxiety happiness whole married parenthood ///
    working livealone grphouse anyvolunteer church_day smcount anybrevement socialloss ///
    illspouse phassles friends family marriage frndsupp famsupp msupport3 pkin pclose ///
    padvice pprox mfreq ploan pchores bridging efctsize diverse density_rev b1density_rev ///
    netsize weakest diff_college diff_white diff_race sole sdage pcollege pwhite ///
    avg_euc_age pfriend famfriends_day assisting_adult communityscale numphonecontact ///
	occcomplex_people widowed anxiety_rev gds15_rev communityscale_rev livealone_rev
	
save "../Data/raw and intermediate/snack_w`w'.dta", replace

}

******
** Merge three snack waves
use "../Data/raw and intermediate/snack_w1.dta", clear
append using "../Data/raw and intermediate/snack_w2.dta" ///
             "../Data/raw and intermediate/snack_w3.dta"
gen source = 2
label define source_lbl 2 "SNACK"
label val source source_lbl

sort SUBID time
save "../Data/snack_heatmap_paper_clean.dta", replace

*****************************************
** Merge SNAD & SNACK **
use "../Data/snad_heatmap_paper_clean.dta", clear
append using "../Data/snack_heatmap_paper_clean.dta"

local compvars time age faq moca_raw craftdre language execfxn ///
    trail_b_time epmem MCI_rev gds15 anxiety happiness whole married parenthood ///
    working livealone grphouse anyvolunteer church_day smcount anybrevement socialloss ///
    illspouse phassles friends family marriage frndsupp famsupp msupport3 pkin pclose ///
    padvice pprox mfreq ploan pchores bridging efctsize diverse density_rev b1density_rev ///
    netsize weakest diff_college diff_white diff_race sole sdage pcollege pwhite ///
    avg_euc_age pfriend famfriends_day assisting_adult communityscale numphonecontact ///
	occcomplex_people widowed anxiety_rev gds15_rev communityscale_rev livealone_rev

table (var) (source) if time == 1, ///
    statistic(count `compvars') statistic(mean `compvars') statistic(sd `compvars') ///
    statistic(min `compvars') statistic(max `compvars') nformat(%9.2f)

* something is off with SNAD effective network size because its minimum is 0 but should be 1

* Standardize

foreach var in `compvars'  {
	egen std_`var' = std(`var')
} 

save "../Data/combined_heatmap_paper.dta", replace




