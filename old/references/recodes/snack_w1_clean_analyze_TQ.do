****Project: social environments and cognitive health
****Data: Clean and Analyze SNACK Wave 1 in 2022
****Author:  Tianyao Qu
****Date:    2/4/2025
****Updated: 2/25/2025

clear all

***************************************************************
**#1 Data cleaning 
***************************************************************
clear all
cd "C:\Users\windy\OneDrive - Indiana University\1_IU_Project\SNAD\4_Cog_Bri"

** import R01 raw dataset from SNAD dataset
use "C:\Users\windy\OneDrive - Indiana University\1_IU_Project\SNAD\Focal\SNACK_Long_merged_05-05-2022.dta", clear

unique SUBID    //350 unique obs at time 1
unique SNACK_ID  //n=154 for SNACK data wave 1 in 2022
describe SUBID SNACK_ID

** Extract SNACK data in a seperate dataset **
sort SUBID SNACK_ID
order SUBID SNACK_ID time
drop if SNACK_ID==""

order SNACK_ID alterid alterrace altercollege
*** making some network variables
** different race
unique alterrace, gen(diff_race) by(SNACK_ID)
order SNACK_ID alterid diff_race 
tab diff_race

sort SNACK_ID diff_race
tab wave
duplicates drop SNACK_ID, force //n=153 after dropping

** save dataset **
*save "C:\Users\windy\OneDrive - Indiana University\1_IU_Project\SNAD\4_Cog_Bri\snack_social_cog_0225_TQ.dta", replace

***************************************************************
**#2 Import SNACK dataset in use
***************************************************************
use "snack_wave1_combined_c_0305_TQ", clear

**# 1.1 Demographics and ses variables# **
*  demographics 
// age
tab age
rename age agesnack

//gender
tab female           //n=145?

//race
drop white white1
fre race_string   //90% are white; 8 missing
encode race_string, gen(racesnack)  
recode racesnack (1=.) (2 3 4 5 =0) (6=1), gen(white)
tab white

//education
fre edu 
recode edu (1/3=0 "less than college") (4/5=1 "college and more"), gen(college)
tab college 

//Marriage
drop married married_iadc_demo married_oldred_demo dem_marital
fre marital
recode marital (2 3=1 "married/partnered") (1 4 5=0 "nv/widowed/divorced"), gen(married)
tab married

// functional disability
rename FAQ_TOT faq

//depression 1
drop gds15
rename gds15_snack gds15

//depression 2 0.84
drop gds30
alpha GDS_8 GDS_1- GDS_30, item std gen(gds30)


//Kids
fre kids
recode kids (1/max=1),gen(parenthood)
tab parenthood
lab var parenthood "if had any child"

//hhinc
fre annual_household
gen hhinc=annual_household
recode hhinc (1=5000)(2=12500)(3=20000)(4=30000)(5=42500)(6=62500)(7=87500)(8=12500)(9=.)
lab var hhinc "Household income in $"
tab hhinc

//Employment
tab employment
gen working=employment
recode working (1/2=1)(3/9=0)
tab working
lab var working "Currently working full or part-time"

//Living alone
fre living_number
recode living_number (0=1) (1/max=0), gen(livealone)
tab livealone
lab var livealone "Currently living alone"

//social ladder
fre ladder

//occupation complexity
fre occcomplex_data occcomplex_people occcomplex_things

// occupational prestige score
clonevar occuprestige = NTPrestige
tab occuprestige

//summary
sum agesnad female white college faq gds15 married parenthood hhinc working livealone ladder occcomplex_data occcomplex_people occcomplex_things 

**# 1.3 Health and other well-being **
/* functional disability
sum  BILLS- TRAVEL
recode BILLS- TRAVEL (8 9 =.)
alpha BILLS- TRAVEL, std item gen(faq)
lab var faq "10-items UDS FAQ Functional disability"
*/

**# 1.4 Social engagment **
//quality of relationships (Spouse, family, friends) 0.68
fre family marriage friends

* UCLA loneliness 0.8901
fre lack_companionship1 left_out1 feel_isolated1
alpha lack_companionship1 left_out1 feel_isolated1, std item gen(uclaloneliness)
lab var uclaloneliness "UCLA loneliness scale"

* if currently widowed 
fre marital
recode marital (4=1 "widowed") (1 2 3 5=0), gen(widowed)

**brevement in the last year
fre life1 life3  
egen numbrevement = rowtotal(life1 life3), miss
lab var numbrevement "Number of family members or friends who died last year"
tab numbrevement

gen anybrevement=numbrevement
recode anybrevement (1/max=1)
fre anybrevement
lab var anybrevement "Any family members or friends died last year"

drop numbrevement

* Lifestyle Cognitive Engagement
fre tv_f radio_f musicinstr_f rnews_f rbooks_f cwpuzzles_f cardgame_f puzzlegame_f write_f museum_f memthink_f sew_f computer_f 

//defining "several times=4"
recode tv_f radio_f musicinstr_f rnews_f ///
       rbooks_f cwpuzzles_f cardgame_f   ///
	   puzzlegame_f write_f museum_f memthink_f sew_f computer_f  ///
	   (0=.) (1=1) (2=4)(3=48)(4=208)(5=365)(6=0)

fre tv_d radio_d musicinstr_d rnews_d ///
       rbooks_d cwpuzzles_d cardgame_d   ///
	   puzzlegame_d write_d museum_d memthink_d sew_d computer_d 	
	   
recode tv_d radio_d musicinstr_d rnews_d ///
       rbooks_d cwpuzzles_d cardgame_d   ///
	   puzzlegame_d write_d museum_d memthink_d sew_d computer_d  ///
	   (0=.) (1=0.5)(2=.75)(3=1.5)(4=2.5)(5=4)	
	   	   
gen tvhrs= tv_f*tv_d
gen radiohrs = radio_f*radio_d
gen musichrs = musicinstr_d*musicinstr_d
gen rnewshrs = rnews_f*rnews_d
gen rbookshrs= rbooks_f*rbooks_d
gen cwpuzzlehrs = cwpuzzles_f*cwpuzzles_d
gen cardgamehrs =cardgame_f*cardgame_d
gen writehrs = write_f*write_d
gen museumhrs = museum_f*museum_d
gen memthinkhrs =memthink_f*memthink_d
gen sewhrs = sew_f*sew_d
gen computerhrs = computer_f*computer_d

egen lifecog = rowtotal(*hrs), mi
tab lifecog
gen lifecogday = lifecog/365		   
lab var lifecog "Lifestyle cognitive engagement (hours per year)"
lab var lifecogday "Avg hours of lifestyle cognitive engagement a day"
tab lifecogday

drop tvhrs-computerhrs

* Multidimensional scale of perceived social support
// overall 0.8516
fre family_help1 help_support1 friends_help1 go_wrong1 problems_family1 joys_sorrows1 make_decisions1 problems_friends1
alpha family_help1 help_support1 friends_help1 go_wrong1 problems_family1 joys_sorrows1 make_decisions1 problems_friends1, std item gen(overallsup)
lab var overallsup "Percieved overall social support scale"

// family support 0.8503
alpha family_help1 help_support1 problems_family1 make_decisions1, std item gen(famsupp)
lab var famsupp "Percieved family social support scale"

// friends support 0.8738
alpha friends_help1 go_wrong1 joys_sorrows1 problems_friends1, std item gen(frndsupp)
lab var frndsupp "Percieved friends social support scale"

/* Phone Contact
fre contacts_phone
clonevar phone=contacts_phone 
replace phone="105" if contacts_phone=="a little over 100"
replace phone="" if contacts_phone=="no cell phone and no contacts on home phone"
replace phone="" if contacts_phone=="unsure/did not have phone"
replace phone="" if contacts_phone=="unsure"
replace phone="250" if contacts_phone=="200 plus"
replace phone="2000" if contacts_phone=="2,000"
replace phone="1252" if contacts_phone=="1,252"
replace phone="" if contacts_phone=="unsure due to not having smartphone "
replace phone="18" if contacts_phone=="15-20"
replace phone="18" if contacts_phone=="15-20"
replace phone="9999999" if contacts_phone=="does not have a cell phone"

destring phone, replace force
replace phone=. if phone==9999999
fre contacts_phone if missing(phone)

gen numphonecontact= phone
lab var numphonecontact "Number of contacts in phone"

//log transformation to correct right skewness
gen numphonecontact_log=log(numphonecontact) 
lab var numphonecontact_log "Logged number of contacts in phone"
*/
* social media
fre  social_media1___1- social_media1___12
egen smcount = rowtotal( social_media1___1- social_media1___12), mi
tab smcount
lab var smcount "Number of social media sites used"

/* community sociality/safety
* community integration 0.8150
fre integration1 integration2 integration3
recode integration1 integration2 integration3 (1=7)(2=6)(3=5)(4=4)(5=3)(6=2)(7=1)
alpha integration1 integration2 integration3, std item gen(communityscale)
lab var communityscale "Community integration scale"
*/
* volunteering 
// any volunteering, including work for organizations and individual others
fre volunteer volunteer_help
recode volunteer_help (3 4 =.)
gen anyvolunteer = 0
replace anyvolunteer=1 if volunteer==1 | volunteer_help==1
replace anyvolunteer=. if volunteer==.& volunteer_help==.
lab var anyvolunteer "Any volunteering in past year including orgs and individuals"
tab anyvolunteer

//volunteering complexity 
fre volunteer_act___coach volunteer_act___teach volunteer_act___mentor volunteer_act___usher volunteer_act___food volunteer_act___cloth volunteer_act___fund volunteer_act___med volunteer_act___office volunteer_act___manage volunteer_act___art volunteer_act___drive volunteer_act___other volunteer_other

egen numvolunteer = rowtotal(volunteer_act___coach volunteer_act___teach volunteer_act___mentor volunteer_act___usher volunteer_act___food volunteer_act___cloth volunteer_act___fund volunteer_act___med volunteer_act___office volunteer_act___manage volunteer_act___art volunteer_act___drive volunteer_act___other), miss
lab var numvolunteer "Number of types of volunteering activities"
tab numvolunteer

/* caregiving
fre assisting_adult
*/

*Social participation
// freq religious services attendance 
fre religious_attendance1
recode religious_attendance1 (1=5 "at least once in a year")(2=4)(3=3)(4=2)(5=1 "never"), gen(church_f)
lab var church_f "freq Religious attendance in a year"
fre church_f

//freq visiting families and friends
fre famfriends_f
recode famfriends_f (0=.) (6=0)
lab var famfriends_f "Frequency of visiting family and friends: higher=more"
tab famfriends_f

** Integration/Participation
*church_f 
recode religious_attendance1 (1=52) (2=24) (3=10) (4=5) (5=0), gen(church_day)
tab church_day
*famfriends_f
recode famfriends_f (1=1) (2=10) (3=36) (4=156) (5=360), gen(famfriends_day)
tab famfriends_day


// summary measures
sum widowed anybrevement lifecog lifecogday overallsup famsupp frndsupp smcount  anyvolunteer numvolunteer church_day famfriends_day family marriage friends uclaloneliness

**# 1.2 Cognitive outcomes **
** cognitive change index self report 0.9399 (in Adam's paper: subjective vs objective)
//subjective; objective 1. moca_raw 2. craft story recalled
sum ccis01- ccis20 
alpha ccis01- ccis20, std item gen(ccisscale)
lab var ccisscale "Cognitive change index scale. higher=exacerbate"

** moca score
tab moca_raw

**1 Create standardized scores to sample average **
destring veg, replace
gen negTraila= -trail_a_time
gen negTrailb= -trail_b_time
regress negTrailb negTraila
predict Trailb_resid, residuals

egen reydelaySD=std(delayed_rey_sum)
foreach var in CRAFTDRE negTraila digif digib animals veg waisdsymbol UDSBENTC UDSBENTD MINTTOTS  {
egen `var'SD=std(`var')
}
lab var CRAFTVRS "Craft Story Delayed Recalled, higher=better"

** 2 Create sample-specific composites**

* Attention: Digit Span Forward/Backward (2-item)
egen attention=rowmean(digifSD digibSD)  // note: higher is better

* Speed of Processing: Trails A; WAIS-R Digit Symbol  (2-item)
egen speed=rowmean(negTrailaSD waisdsymbolSD) // note: higher is better

* Executive Function: : Trails B [task-switching] (1-item) // formerly also Verbal Fluency (F & L words);
egen execfxn = std(Trailb_resid) // note: higher is better
// formerly, egen exec=rowmean(UDSVERFCSD UDSVERLCSD negTrailbSD), but the alpha was too low

* Episodic Memory: Craft Story 21 (Recall Paraphrase); Rey AVLT Delayed Recall; Benson Complex Figure Recall (3-item)
egen epmem=rowmean(CRAFTDRESD reydelaySD UDSBENTDSD) // note: higher is better

* Language: Animal and Vegetable Naming, Multilingual Naming Test (3-item)
egen language=rowmean(animalsSD vegSD MINTTOTSSD)

* Visual/Spatial: Benson Complex Figure Copy (1 item only)
gen visual=UDSBENTCSD

* diagnosis using moca cut points 
drop diagnosis_moca 
recode moca_raw (26/30=1) (18/25=2) (0/17=3),gen(diagnosis_moca)
label var diagnosis_moca "diagnosis based on MoCA scores"


/** 3 clinical cognitive decline
fre diagnosis_iadc
gen diag2=diagnosis_iadc
replace diag2=diagnosis_moca if diagnosis_iadc==.
lab var diag2 "Diagnosis (consensus if available, MOCA derived if missing)"
tab diag2
tab diag2 if time==1

// MCI and dementia
recode diag2 (1=0 "Normal") (2/3 =1 "MCI/dementia"), gen(MCIdementia)
tab MCIdementia
*/

*MCI and dementia
recode diagnosis_moca (1=0 "Normal") (2/3 =1 "MCI/dementia"), gen(MCIdementia)
tab MCIdementia


** all cognitive function measures
sum CRAFTDRESD moca_raw execfxn epmem   
///sum MCIdementia
//fre MCIdementia            //diagnosed mild cognitive decline or dementia  (BINARY)
fre CRAFTDRESD             //"Craft Story Delayed Recalled, higher=better" (CONTINUOUS)
fre moca_raw               //moca score (CONTINUOUS)
fre execfxn                //executive memory (CONTINUOUS)
fre epmem                  //episodic memory (CONTINUOUS)

gen snackwave=1
gen datasource = "snack w1"


save "snack_wave1_combined_c_0305_TQ.dta", replace

***** Save demographic information in a sperate dataset to be merged with SNACK W2 *****
use "snack_wave1_combined_c_0305_TQ.dta", clear

keep SNACK_ID SUBID female white college racesnack kids
save "C:\Users\windy\OneDrive - Indiana University\1_IU_Project\SNAD\Focal\SNACK W2 data\snack_w1_demographics_02262025.dta", replace


*****************************************
********** standardization for all variables in use 
*****************************************
use "snack_wave1_combined_c_0305_TQ.dta", clear

// outcomes
sum moca_raw execfxn epmem CRAFTDRESD 
local dv_varlist `"moca_raw execfxn epmem CRAFTDRESD"'
foreach var in `dv_varlist'  {
	egen std_`var' = std(`var')
} 

** Controls
sum female agesnack white college gds15 faq
*binary: female white college
local dv_varlist `"agesnack gds15 faq"'
foreach var in `dv_varlist'  {
	egen std_`var' = std(`var')
} 

// social integration
sum  married kids parenthood working livealone anyvolunteer numvolunteer church_f famfriends_f smcount lifecogday church_day famfriends_day
local dv_varlist `"married kids parenthood working livealone anyvolunteer numvolunteer church_f famfriends_f smcount lifecogday church_day famfriends_day "'
foreach var in `dv_varlist'  {
	egen std_`var' = std(`var')
} 

// social stressors
sum widowed ladder occuprestige anybrevement occcomplex_data occcomplex_people occcomplex_things mhassles phassles uclaloneliness   
local dv_varlist `"widowed ladder occuprestige anybrevement occcomplex_data occcomplex_people occcomplex_things mhassles phassles uclaloneliness"'
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
sum pwhite diff_race pcollege sdage diverse pkin pfem netsize b1density efctsize sole weakest sdstrength sdfreq bridging
local dv_varlist `"pwhite diff_race pcollege sdage diverse pkin pfem netsize b1density efctsize sole weakest sdstrength sdfreq bridging"'
foreach var in `dv_varlist'  {
	egen std_`var' = std(`var')
} 



*****************************************
*********** models and visuals **********
*****************************************
**** logistics  ****
unique SNACK_ID    //153 obs
tab wave        //wave indictor

**** each dependent variable for cognitive health  ****
sum std_moca_raw std_execfxn std_epmem std_CRAFTDRESD

**** descriptives for each group of social factors ****
*** 1. Controls ***
fre female
fre agesnack
fre white
fre college
fre gds15               //uds geriatric depression
fre faq                 //functional disbaility: higher= more dependent on other people on daily tasks

***************************************************
**** stacking estimates all in one spreadsheet ****
***************************************************
sum married parenthood working livealone anyvolunteer std_church_day std_famfriends_day std_smcount std_lifecogday widowed std_occuprestige anybrevement std_occcomplex_people std_mhassles std_phassles std_uclaloneliness std_famsupp std_frndsupp std_marriage std_family std_friends  std_pclose std_msupport3 std_mstrength std_mprox std_mfreq std_pprox std_ploan std_padvice std_pchores std_diff_race  std_sdage std_diverse std_pkin  std_netsize std_b1density std_efctsize sole std_weakest std_sdstrength std_sdfreq std_bridging

///sum std_diff_college

//1. moca score 
eststo clear
local dv_varlist `"married parenthood working livealone anyvolunteer std_church_day std_famfriends_day std_smcount std_lifecogday widowed std_occuprestige anybrevement std_occcomplex_people std_mhassles std_phassles std_uclaloneliness std_famsupp std_frndsupp std_marriage std_family std_friends  std_pclose std_msupport3 std_mstrength std_mprox std_mfreq std_pprox std_ploan std_padvice std_pchores std_diff_race  std_sdage std_diverse std_pkin  std_netsize std_b1density std_efctsize sole std_weakest std_sdstrength std_sdfreq std_bridging"'

foreach x in `dv_varlist'  {
	eststo `x': reg std_moca_raw c.std_agesnack female white college std_faq std_gds15 `x'
    mat N1 = e(N) * J(1, colsof(e(b)), 1)  // Ensure it's a row vector
    qui estadd mat N1 = N1

}

eststo bivar: appendmodels $eststo
* Note: appendmodels is a stata program created in "stack_in_one_column.do" to stack all bivariate regression coefs in one column
esttab bivar using snack_coef_0225.csv, replace ///
	  b(a2) se nonumbers nogap  ///
	 drop(std_agesnack female white college std_faq std_gds15)  ///
	 ///bic aic scalars("ll Log lik." "chi2 Chi-squared") ///
	 star(+ 0.10 * 0.05 ** 0.01 *** 0.001) ///
	 	 title(1. predicting MoCA score)	 

//2. execfxn
eststo clear
local dv_varlist `"married parenthood working livealone anyvolunteer std_church_day std_famfriends_day std_smcount std_lifecogday widowed std_occuprestige anybrevement std_occcomplex_people std_mhassles std_phassles std_uclaloneliness std_famsupp std_frndsupp std_marriage std_family std_friends  std_pclose std_msupport3 std_mstrength std_mprox std_mfreq std_pprox std_ploan std_padvice std_pchores std_diff_race  std_sdage std_diverse std_pkin  std_netsize std_b1density std_efctsize sole std_weakest std_sdstrength std_sdfreq std_bridging"'

foreach x in `dv_varlist'  {
	eststo `x': reg std_execfxn c.std_agesnack female white college std_faq std_gds15 `x'
    mat N1 = e(N) * J(1, colsof(e(b)), 1)  // Ensure it's a row vector
    qui estadd mat N1 = N1

}

eststo bivar: appendmodels $eststo
* Note: appendmodels is a stata program created in "stack_in_one_column.do" to stack all bivariate regression coefs in one column
esttab bivar using snack_coef_0225.csv, append ///
	  b(a2) se nonumbers nogap  ///
	 drop(std_agesnack female white college std_faq std_gds15)  ///
	 ///bic aic scalars("ll Log lik." "chi2 Chi-squared") ///
	 star(+ 0.10 * 0.05 ** 0.01 *** 0.001) ///
	 	 title(1. predicting std_execfxn)	 


//3. epmem
eststo clear
local dv_varlist `"married parenthood working livealone anyvolunteer std_church_day std_famfriends_day std_smcount std_lifecogday widowed std_occuprestige anybrevement std_occcomplex_people std_mhassles std_phassles std_uclaloneliness std_famsupp std_frndsupp std_marriage std_family std_friends  std_pclose std_msupport3 std_mstrength std_mprox std_mfreq std_pprox std_ploan std_padvice std_pchores std_diff_race  std_sdage std_diverse std_pkin  std_netsize std_b1density std_efctsize sole std_weakest std_sdstrength std_sdfreq std_bridging"'

foreach x in `dv_varlist'  {
	eststo `x': reg std_epmem c.std_agesnack female white college std_faq std_gds15 `x'
    mat N1 = e(N) * J(1, colsof(e(b)), 1)  // Ensure it's a row vector
    qui estadd mat N1 = N1

}

eststo bivar: appendmodels $eststo
* Note: appendmodels is a stata program created in "stack_in_one_column.do" to stack all bivariate regression coefs in one column
esttab bivar using snack_coef_0225.csv, append ///
	  b(a2) se nonumbers nogap  ///
	 drop(std_agesnack female white college std_faq std_gds15)  ///
	 ///bic aic scalars("ll Log lik." "chi2 Chi-squared") ///
	 star(+ 0.10 * 0.05 ** 0.01 *** 0.001) ///
	 	 title(1. predicting epmem)	 

//4. CRAFTDRESD
eststo clear
local dv_varlist `"married parenthood working livealone anyvolunteer std_church_day std_famfriends_day std_smcount std_lifecogday widowed std_occuprestige anybrevement std_occcomplex_people std_mhassles std_phassles std_uclaloneliness std_famsupp std_frndsupp std_marriage std_family std_friends  std_pclose std_msupport3 std_mstrength std_mprox std_mfreq std_pprox std_ploan std_padvice std_pchores std_diff_race  std_sdage std_diverse std_pkin  std_netsize std_b1density std_efctsize sole std_weakest std_sdstrength std_sdfreq std_bridging"'

foreach x in `dv_varlist'  {
	eststo `x': reg std_CRAFTDRESD c.std_agesnack female white college std_faq std_gds15 `x'
    mat N1 = e(N) * J(1, colsof(e(b)), 1)  // Ensure it's a row vector
    qui estadd mat N1 = N1

}

eststo bivar: appendmodels $eststo
* Note: appendmodels is a stata program created in "stack_in_one_column.do" to stack all bivariate regression coefs in one column
esttab bivar using snack_coef_0225.csv, append ///
	  b(a2) se nonumbers nogap  ///
	 drop(std_agesnack female white college std_faq std_gds15)  ///
	 ///bic aic scalars("ll Log lik." "chi2 Chi-squared") ///
	 star(+ 0.10 * 0.05 ** 0.01 *** 0.001) ///
	 	 title(1. predicting std_CRAFTDRESD)	 





