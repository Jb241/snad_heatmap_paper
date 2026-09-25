
// Recoding raw data for SNAD/SNACK heatmap paper
// Created: 09/16/26
// Data used:
//	snad-analysis-focal-r01match-20260701.dta
//	SNAD-Analysis-focal-LONG-R01match-20260120.dta
//  

* Working directory (set to "Code"):
cd "/Users/jessicalogin/Library/CloudStorage/OneDrive-SharedLibraries-IndianaUniversity/Qu, Tianyao - 4_Cog_Bri/Fall 2026 Working Folder/Code"

****************************************************************
****************************************************************
* SNAD
****************************************************************
****************************************************************

use "../Data/raw and intermediate/snad-analysis-focal-r01match-20260701.dta", clear

******
// Data info vars
codebook SUBID
codebook time

******
// Covariates
codebook agesnad

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
alpha `faq_vars', gen(faq)

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

*codebook gad7_1 gad7_2 gad7_3 gad7_4 gad7_5 gad7_6 gad7_7 // lots missing
codebook anxiety1 anxiety2 anxiety3 anxiety4
alpha anxiety1 anxiety2 anxiety3 anxiety4, gen(anxiety)

codebook happiness
tab happiness, m nolab

codebook whole  // how would you describe your life as a whole?

******
// Social participation
codebook married

codebook kids
gen parenthood = .
replace parenthood = 1 if kids > 0
replace parenthood = 0 if kids == 0
label define parent_lbl 0 "Not parent" 1 "Parent"

codebook employment
recode employment (1 2 = 1 "Part or full-time") (3/9 = 0 "Not currently working"), gen(working)
tab employment working, m

codebook living_number
gen livealone = .
replace livealone = 1 if living_number == 0
replace livealone = 0 if living_number > 0
label define live_lbl 0 "Live with people" 1 "Live alone"
label val livealone libe_lbl

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

codebook life1 life2 life3 // life2 is not death
gen anybrevement = 0
replace anybrevement = 1 if life1 == 1 | life3 == 1
replace anybrevement = . if life1 == . & life3 == .
label define breve_lbl 0 "No loss" 1 "Bereaved"

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

codebook integration
clonevar communityscale = integration 

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
bysort SUBID: egen double mean_agesq = mean(agesqdiff)
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

* Merge back with cleaned ego data
keep SUBID time pfriend avg_euc_age diff_race diff_white
merge m:1 SUBID time using "../Data/raw and intermediate/snad_heatmap_paper_intermediate.dta"

******
// Labels
label var time "Wave"
label var agesnad "Age"
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

****
* Save cleaned SNAD data
save "../Data/snad_heatmap_paper_clean.dta", replace

****************************************************************
****************************************************************
* SNACK
****************************************************************
****************************************************************

import excel "../Data/raw and intermediate/SNACK_W1_W2_W3.xlsx", firstrow sheet() clear


/* Recode Variable Goals
SUBID time
ageall female white college faq 
moca_raw execfxn epmem CRAFTDRESD language MCI_rev
gds
female ageall white college faq
married parenthood working livealone grphouse anyvolunteer church_day famfriends_day smcount
assisting_adult widowed anybrevement phassles
friends family marriage frndsupp famsupp msupport3 communityscale pkin pclose padvice pprox mfreq ploan pchores
bridging efctsize diverse density_rev density netsize avg_euc_age weakest occcomplex_people  diff_college diff_white numphonecontact pfriend sole diff_race sdage pcollege pwhite

*/

