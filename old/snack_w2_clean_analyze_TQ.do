****Project: social environments and cognitive health
****Data: Clean and Analyze SNACK Wave 2 in 2025
****Author:  Tianyao Qu
****Date:    2/26/2025
****Updated: 03/05/2025

clear all
cd "C:\Users\windy\OneDrive - Indiana University\1_IU_Project\SNAD\4_Cog_Bri"

** analyses start at line 390 **

***************************************************************
**#1 Import Datasets
***************************************************************
** Import psycho/social/behavioral data and harmonize with social network data

import excel "C:\Users\windy\OneDrive - Indiana University\1_IU_Project\SNAD\Focal\SNACK W2 data\250205_SNACK_W2_BEH.xlsx", sheet("data") firstrow

***change variable types
destring  marital living_own1-moved living_number employment buy_food-ladder mastery1-feel_isolated1 volunteer volunteer_act* volunteer_help volunteer_often volunteer_company volunteer_change1, replace
recast float volunteer_act* volunteer_help volunteer_often volunteer_company 
destring *, replace

save "C:\Users\windy\OneDrive - Indiana University\1_IU_Project\SNAD\Focal\SNACK W2 data\SNACK_W2-no-net-vars.dta"

*********************************************
** Import social network data (short form)
import excel "C:\Users\windy\OneDrive - Indiana University\1_IU_Project\SNAD\Focal\SNACK W2 data\250205_SNACK_W2_SHORT-clean.xlsx", sheet("Sheet1") firstrow
destring *, replace

save "C:\Users\windy\OneDrive - Indiana University\1_IU_Project\SNAD\Focal\SNACK W2 data\SNACK_W2_netvars_short.dta"


**********************************************************************
**# 2  Merge with social network data and SNACK wave demographic information
**********************************************************************
use "C:\Users\windy\OneDrive - Indiana University\1_IU_Project\SNAD\Focal\SNACK W2 data\SNACK_W2-no-net-vars.dta", clear

merge 1:1 SUBID using "C:\Users\windy\OneDrive - Indiana University\1_IU_Project\SNAD\Focal\SNACK W2 data\SNACK_W2_netvars_short.dta"
tab _merge
keep if _merge==3
drop _merge

destring SUBID, replace force
gen snackwave=2
gen datasource = "snack w2"

**************************************************
**Merge with SNACK w1 demographic information
merge m:1 SNACK_ID using "C:\Users\windy\OneDrive - Indiana University\1_IU_Project\SNAD\Focal\SNACK W2 data\snack_w1_demographics_02262025.dta"
keep if _merge==3
drop _merge

*save "C:\Users\windy\OneDrive - Indiana University\1_IU_Project\SNAD\Focal\SNACK W2 data\snack_w2_combined_02262025.dta", replace


***************************************************************
**#4 Data Cleaning for SNACK W2 data
***************************************************************
unique SUBID    //119 unique obs at time 1
unique SNACK_ID  //n=119 for SNACK data wave 1 in 2023
describe SUBID SNACK_ID

** Extract SNACK data in a seperate dataset **
sort SUBID SNACK_ID
order SUBID SNACK_ID snackwave
*drop if SNACK_ID==""

**# 1.1 Demographics and ses variables# **
*  demographics 
// age
tab age
rename age agesnack

//gender
tab female           //n=145?

//race
tab white

//education
tab college 

//Marriage
fre marital
recode marital (1 2=1 "married/partnered") (3 4 5 6=0 "nv/widowed/divorced"), gen(married)
tab married

// functional disability
drop faq
egen faq= rowtotal(func_checks func_tax func_shopping func_hobby func_coffee func_meal func_events func_tv func_appts func_travel)
tab faq

//depression 0.8513
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
replace living_number = "0" in 7
replace living_number = "1" in 2
replace living_number = "1" in 3
replace living_number = "0" in 10
replace living_number = "0" in 43
replace living_number = "0" in 54
replace living_number = "0" in 56
replace living_number = "0" in 53

destring living_number, replace force
recode living_number (0=1) (1/max=0), gen(livealone)
tab livealone
lab var livealone "Currently living alone"

//social ladder
fre ladder

/*occupation complexity
fre occcomplex_data occcomplex_people occcomplex_things
*/

/* occupational prestige score
clonevar occuprestige = NTPrestige
tab occuprestige
*/
//summary
sum agesnack female white college faq gds30 married parenthood hhinc working livealone ladder    

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
des family marriage friends

* UCLA loneliness 0.8901
fre lack_companionship1 left_out1 feel_isolated1
alpha lack_companionship1 left_out1 feel_isolated1, std item gen(uclaloneliness)
lab var uclaloneliness "UCLA loneliness scale"

* if currently widowed 
fre marital
recode marital (3=1 "widowed") (1 2 4 5 6=0), gen(widowed)

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

* social media
fre  social_media1___1- social_media1___12
egen smcount = rowtotal( social_media1___1- social_media1___12), mi
tab smcount
lab var smcount "Number of social media sites used"
*/
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
/* freq religious services attendance 
fre religious_attendance1
recode religious_attendance1 (1=5 "at least once in a year")(2=4)(3=3)(4=2)(5=1 "never"), gen(church_f)
lab var church_f "freq Religious attendance in a year"
fre church_f
*/
//freq visiting families and friends
fre famfriends_f
recode famfriends_f (0=.) (6=0)
lab var famfriends_f "Frequency of visiting family and friends: higher=more"
tab famfriends_f

** Integration/Participation
/*church_f 
recode religious_attendance1 (1=52) (2=24) (3=10) (4=5) (5=0), gen(church_day)
tab church_day
*/
*famfriends_f
recode famfriends_f (1=1) (2=10) (3=36) (4=156) (5=360), gen(famfriends_day)
tab famfriends_day


// summary measures
sum widowed anybrevement lifecog lifecogday overallsup famsupp frndsupp  anyvolunteer numvolunteer famfriends_day family marriage friends uclaloneliness

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
recode moca_raw (26/30=1) (18/25=2) (0/17=3),gen(diagnosis_moca)
label var diagnosis_moca "diagnosis based on MoCA scores"

**** Latent bridging measure ****
	sem (Bridging -> netsize diverse weakest density efctsize sole) if netsize>0 & !missing(netsize) ///
    , var(Bridging@1) method(adf) cov(e.netsize*e.efctsize) cov(e.netsize*e.density) cov(e.diverse*e.density) cov(e.density*e.sole) cov(e.diverse*e.weakest) cov(e.netsize*e.diverse) cov(e.weakest*e.sole)

estat gof, stats(all)
estat mindices

predict bridging,latent(Bridging) 
label var bridging "Bridging capital"

*adjust bridging when netsize=0 or missing
replace bridging=. if missing(netsize) 
sum bridging
local min=r(min)
replace bridging=`min' if netsize==0 //set bridging to minimum value when netsize=0

kdensity bridging




/** 3 clinical cognitive decline
fre diagnosis_iadc
gen diag2=diagnosis_iadc
replace diag2=diagnosis_moca if diagnosis_iadc==.
lab var diag2 "Diagnosis (consensus if available, MOCA derived if missing)"
tab diag2
tab diag2 if time==1

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

save "snack_wave2_combined_c_0305_TQ.dta", replace


*****************************************
********** standardization for all variables in use 
*****************************************
use "snack_wave2_combined_c_0305_TQ.dta", clear

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
sum  married kids parenthood working livealone anyvolunteer numvolunteer famfriends_f  lifecogday  famfriends_day
local dv_varlist `"married kids parenthood working livealone anyvolunteer numvolunteer famfriends_f  lifecogday  famfriends_day "'
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
tab wave        //wave indictor

**** each dependent variable for cognitive health  ****
sum std_moca_raw std_execfxn std_epmem std_CRAFTDRESD

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
sum married parenthood working livealone anyvolunteer  std_famfriends_day  std_lifecogday widowed  anybrevement  std_mhassles std_phassles std_uclaloneliness std_famsupp std_frndsupp std_marriage std_family std_friends  std_pclose std_msupport3 std_mstrength std_mprox std_mfreq std_pprox std_ploan std_padvice std_pchores   std_sdage std_diverse std_pkin  std_netsize std_b1density std_efctsize sole std_weakest std_sdstrength std_sdfreq 

///sum std_diff_college

//1. moca score 
eststo clear
local dv_varlist `"married parenthood working livealone anyvolunteer  std_famfriends_day  std_lifecogday widowed  anybrevement  std_mhassles std_phassles std_uclaloneliness std_famsupp std_frndsupp std_marriage std_family std_friends  std_pclose std_msupport3 std_mstrength std_mprox std_mfreq std_pprox std_ploan std_padvice std_pchores   std_sdage std_diverse std_pkin  std_netsize std_b1density std_efctsize sole std_weakest std_sdstrength std_sdfreq bridging"'

foreach x in `dv_varlist'  {
	eststo `x': reg std_moca_raw c.std_agesnack female white college  std_gds30 `x'
    mat N1 = e(N) * J(1, colsof(e(b)), 1)  // Ensure it's a row vector
    qui estadd mat N1 = N1

}

eststo bivar: appendmodels $eststo
* Note: appendmodels is a stata program created in "stack_in_one_column.do" to stack all bivariate regression coefs in one column
esttab bivar using snack_coef_w2_0226.csv, replace ///
	  b(a2) se nonumbers nogap  ///
	 drop(std_agesnack female white college std_gds30)  ///
	 ///bic aic scalars("ll Log lik." "chi2 Chi-squared") ///
	 star(+ 0.10 * 0.05 ** 0.01 *** 0.001) ///
	 	 title(1. predicting MoCA score)	 

//2. execfxn
eststo clear
local dv_varlist `"married parenthood working livealone anyvolunteer  std_famfriends_day  std_lifecogday widowed  anybrevement  std_mhassles std_phassles std_uclaloneliness std_famsupp std_frndsupp std_marriage std_family std_friends  std_pclose std_msupport3 std_mstrength std_mprox std_mfreq std_pprox std_ploan std_padvice std_pchores   std_sdage std_diverse std_pkin  std_netsize std_b1density std_efctsize sole std_weakest std_sdstrength std_sdfreq bridging"'

foreach x in `dv_varlist'  {
	eststo `x': reg std_execfxn c.std_agesnack female white college  std_gds30 `x'
    mat N1 = e(N) * J(1, colsof(e(b)), 1)  // Ensure it's a row vector
    qui estadd mat N1 = N1

}

eststo bivar: appendmodels $eststo
* Note: appendmodels is a stata program created in "stack_in_one_column.do" to stack all bivariate regression coefs in one column
esttab bivar using snack_coef_w2_0226.csv, append ///
	  b(a2) se nonumbers nogap  ///
	 drop(std_agesnack female white college  std_gds30)  ///
	 ///bic aic scalars("ll Log lik." "chi2 Chi-squared") ///
	 star(+ 0.10 * 0.05 ** 0.01 *** 0.001) ///
	 	 title(1. predicting std_execfxn)	 


//3. epmem
eststo clear
local dv_varlist `"married parenthood working livealone anyvolunteer  std_famfriends_day  std_lifecogday widowed  anybrevement  std_mhassles std_phassles std_uclaloneliness std_famsupp std_frndsupp std_marriage std_family std_friends  std_pclose std_msupport3 std_mstrength std_mprox std_mfreq std_pprox std_ploan std_padvice std_pchores   std_sdage std_diverse std_pkin  std_netsize std_b1density std_efctsize sole std_weakest std_sdstrength std_sdfreq bridging"'

foreach x in `dv_varlist'  {
	eststo `x': reg std_epmem c.std_agesnack female white college std_gds30 `x'
    mat N1 = e(N) * J(1, colsof(e(b)), 1)  // Ensure it's a row vector
    qui estadd mat N1 = N1

}

eststo bivar: appendmodels $eststo
* Note: appendmodels is a stata program created in "stack_in_one_column.do" to stack all bivariate regression coefs in one column
esttab bivar using snack_coef_w2_0226.csv, append ///
	  b(a2) se nonumbers nogap  ///
	 drop(std_agesnack female white college  std_gds30)  ///
	 ///bic aic scalars("ll Log lik." "chi2 Chi-squared") ///
	 star(+ 0.10 * 0.05 ** 0.01 *** 0.001) ///
	 	 title(1. predicting std_epmem)	 

//4. CRAFTDRESD
eststo clear
local dv_varlist `"married parenthood working livealone anyvolunteer  std_famfriends_day  std_lifecogday widowed  anybrevement  std_mhassles std_phassles std_uclaloneliness std_famsupp std_frndsupp std_marriage std_family std_friends  std_pclose std_msupport3 std_mstrength std_mprox std_mfreq std_pprox std_ploan std_padvice std_pchores   std_sdage std_diverse std_pkin  std_netsize std_b1density std_efctsize sole std_weakest std_sdstrength std_sdfreq bridging"'

foreach x in `dv_varlist'  {
	eststo `x': reg std_CRAFTDRESD c.std_agesnack female white college  std_gds30 `x'
    mat N1 = e(N) * J(1, colsof(e(b)), 1)  // Ensure it's a row vector
    qui estadd mat N1 = N1

}

eststo bivar: appendmodels $eststo
* Note: appendmodels is a stata program created in "stack_in_one_column.do" to stack all bivariate regression coefs in one column
esttab bivar using snack_coef_w2_0226.csv, append ///
	  b(a2) se nonumbers nogap  ///
	 drop(std_agesnack female white college  std_gds30)  ///
	 ///bic aic scalars("ll Log lik." "chi2 Chi-squared") ///
	 star(+ 0.10 * 0.05 ** 0.01 *** 0.001) ///
	 	 title(1. predicting std_CRAFTDRESD)	 





























