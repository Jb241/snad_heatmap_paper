****Project: Check out correlations with social bridging
****Data: SNAD R01
****Author:  Tianyao Qu
****Date:    10/21/2024
****Updated: 12/19/2024

clear all

cd "C:\Users\windy\OneDrive - Indiana University\1_IU_Project\SNAD\2_SNAD Measures"
use "C:\Users\windy\OneDrive - Indiana University\1_IU_Project\SNAD\Focal\snad-analysis-focal-r01match-20240603.dta", clear



***************************************************************
**#1 Data cleaning 
***************************************************************
**# 1.1 Demographics and ses variables# **
*  demographics 
//gender
tab female

//race
fre white

//education
fre edu

//Marriage
clonevar Fmarried= married

//Kids
fre kids
recode kids (1/max=1),gen(Fparenthood)
tab Fparenthood
lab var Fparenthood "if having any children"

//hhinc
fre annual_household
gen Fhhinc=annual_household
recode Fhhinc (1=5000)(2=12500)(3=20000)(4=30000)(5=42500)(6=62500)(7=87500)(8=12500)(9=.)
lab var Fhhinc "Household income in $"

//Employment
gen Fworking=employment
recode Fworking (1/2=1)(3/9=0)
tab Fworking
lab var Fworking "Currently working full or part-time"

//Living alone
fre living_number
recode living_number (0=1) (1/max=0), gen(Flivealone)
tab Flivealone
lab var Flivealone "Currently living alone"

//group housing
clonevar grphouse = group_housing1
lab var grphouse "crrently living in group housing for older adults"


//social ladder
fre ladder
clonevar Fladder = ladder

//occupation complexity
fre occcomplex_data occcomplex_people occcomplex_things
egen Focccomplex = rowmean(occcomplex_data occcomplex_people occcomplex_things)
tab Focccomplex
lab var Focccomplex "occupational complexity. higher = less complex"

// occupational prestige score
clonevar Foccuprestige = NTPrestige

//summary
sum edu Fmarried Fparenthood Fhhinc Fworking Flivealone grphouse Fladder Focccomplex Foccuprestige



**# 1.2 Cognitive functions **
// cognitive change index self report 0.9399 (in Adam's paper: subjective vs objective)
//subjective; objective 1. moca_raw 2. craft story recalled 
alpha ccis01- ccis20, std item gen(Fccisscale)
lab var Fccisscale "Cognitive change index scale. higher=exacerbate"

//39-item list self-reported ECOG cognitive reliability 0.9746
sum concern list recent convo objects datesweek told appts objnames instruct words thoughts story point meanings tvprog spoken mapfollow mapread car meet neighborhood store house shop weather sched conversate act organize checkbook finance priority mail strategy multtask interrupt distract cook

recode list-cook (9=.)
alpha list-cook, std item gen(FECOGSelf)
lab var FECOGSelf " 39-item ECOG Self cognitive reliability, high=worse"

//36-item list informant-reported ECOG cognitive reliability 0.9873
sum concern list_i recent_i convo_i objects_i repeat_i dates_i told_i appts_i objnames_i instruct_i words_i thoughts_i story_i point_i meanings_i tvprog_i spoken_i mapfollow_i mapread_i car_i meet_i neighborhood_i store_i house_i shop_i weather_i sched_i thinkthrough_i thinkahead_i organize_i checkbook_i finance_i priority_i mail_i meds_i multtask_i interrupt_i distract_i cook_i

recode list_i-cook_i (9=.)
alpha list_i-cook_i, std item gen(FECOGInformant)
lab var FECOGInformant " 39-item ECOG Informant cognitive reliability, high=worse"

// difference in self-reported and informant-reported
foreach x of varlist list recent convo objects told appts objnames instruct words thoughts story point meanings tvprog spoken mapfollow mapread car meet neighborhood store house shop weather sched organize checkbook finance priority mail multtask interrupt distract cook {
	gen ecogdiff_`x' =  `x' - `x'_i 

}

egen Fdiff = rowmean(ecogdiff_*)
alpha ecogdiff_*, std item gen(Fdiffscale)   //0.96
drop ecogdiff_*
pwcorr Fdiff FECOGSelf FECOGInformant Fdiffscale, sig
lab var Fdiff "ECOG Self-Informant cognitive reliability, high diff"


** clinical cognitive decline
gen Fdiag=diagnosis_iadc
replace Fdiag=diagnosis_moca if diagnosis_iadc==.
lab var Fdiag "Diagnosis (consensus if available, MOCA derived if missing)"
tab Fdiag

// all cognitive function measures
lab var CRAFTDRESD "Craft Story Delayed Recalled, higher=better"
sum attention speed execfxn epmem language visual moca_raw CRAFTDRESD Fccisscale FECOGSelf FECOGInformant Fdiff


// imaging data
gen Famygovl = totalamygvol/icv
lab var Famygovl "totalamygvol divided by icv"

sum reserve frolobe parlobe temlobe occlobe medtemp wmh delayed_rey_sum totalhippvol totalamygvol Famygovl  

**# 1.3 Health and other well-being **
* sense of control
fre mastery1 mastery2
egen Favgmastery = rowmean(mastery1 mastery2)
lab var Favgmastery "Average sense of mastery"

* happiness
fre happiness
gen Fhappy = happiness
lab var Fhappy "happiness"

* UCLA loneliness 0.8901
fre lack_companionship1 left_out1 feel_isolated1
alpha lack_companionship1 left_out1 feel_isolated1, std item gen(Fuclaloneliness)
lab var Fuclaloneliness "UCLA loneliness scale"

* perceived stress 0.8596
fre stress1 stress2 stress3 stress4 stress5 stress6 stress7 stress8 stress9 stress10
alpha stress1 stress2 stress3 stress4 stress5 stress6 stress7 stress8 stress9 stress10, std item gen(Fpsstress)
lab var Fpsstress "Percieved stress scale"

* anxiety symptoms
fre anxiety1 anxiety2 anxiety3 anxiety4
alpha anxiety1 anxiety2 anxiety3 anxiety4, std item gen(Favganxiety)
lab var Favganxiety "4-items anxiety symptoms scale"

* depression 
fre gds15 DEPD
clonevar Fgds15 = gds15

* other psychological symptoms
fre DEL HALL AGIT ANX DEPD ELAT APA DISN IRR MOT NITE APP 
recode DEL HALL AGIT ANX DEPD ELAT APA DISN IRR MOT NITE APP (9=.)

sum DEL HALL AGIT ANX DEPD ELAT APA DISN IRR MOT NITE APP

* physical activity 
fre vigor vigorhr vigormn vigortothr vigortotmn
fre moderate modhr modmn modtothr modtotmn
fre walk walkhr walkmn walktothr walktotmin
fre  sitmn sitwedhr sitwedmn

egen Favgphysical = rowmean(vigor moderate walk) 
tab Favgphysical
lab var Favgphysical "Avg days of physical activities a week"

* Quality of life
//with relationships included 0.8711
fre health energy spirits living_situation memory self do_chores do_fun money whole family marriage friends

alpha health energy spirits living_situation memory self do_chores do_fun money whole family marriage friends, std item gen(Fqolwithrela)
lab var Fqolwithrela "Quality of life scale with relationships included"

//excluding relationships 0.8598
alpha health energy spirits living_situation memory self do_chores do_fun money whole, std item gen(Fqolnorela)
lab var Fqolnorela "Quality of life scale with no relationship items included"

/*
//quality of relationships (Spouse, family, friends) 0.68
alpha family marriage friends, std item gen(Fqolonlyrela)
lab var Fqolonlyrela "Quality of relationship scale (Spouse, family, and friends)"
*/

* sleep quality
fre sleep
clonevar Fsleep =  sleep
replace Fsleep = -Fsleep
lab var Fsleep "self-rated sleep quality, higher means better"

// perceived sleep quality scale 0.6583
fre psq5a- psq5i 
alpha psq5a- psq5i, std item gen(Fpsleepq)
replace Fpsleepq = -Fpsleepq
lab var Fpsleepq "Percieved sleep quality scale, higher means better"

* hearing loss 
fre hearing
recode hearing (1=1) (2=0), gen(Fhearingloss)
tab Fhearingloss

* vision loss
fre vision
recode vision (1=1) (2=0), gen(Fvisionloss)
tab Fvisionloss

//summary measures
sum Favgmastery Fhappy Fuclaloneliness Fpsstress Favganxiety Fgds15 DEL HALL AGIT ANX DEPD ELAT APA DISN IRR MOT NITE APP Favgphysical Fqolwithrela Fqolnorela Fsleep Fpsleepq Fhearingloss Fvisionloss

**# 1.4 Social engagment **
* life events
fre life1 life2 life3 life4 life5 life6 life7
egen Fnumlifeevents = rowtotal(life1 life2 life3 life4 life5 life6 life7), miss
lab var Fnumlifeevents "Number of stressful life events"

fre life1 life2 life3  
egen Fnumbrevement = rowtotal(life1 life2 life3), miss
lab var Fnumbrevement "Number of family members or friends who died last year"
tab Fnumbrevement

gen Fanybrevement=Fnumbrevement
recode Fanybrevement (1/max=1)
fre Fanybrevement
lab var Fanybrevement "Any family members or friends died last year"


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
rename lifecogday Flifecogday


* Multidimensional scale of perceived social support
// overall 0.8516
fre family_help1 help_support1 friends_help1 go_wrong1 problems_family1 joys_sorrows1 make_decisions1 problems_friends1
alpha family_help1 help_support1 friends_help1 go_wrong1 problems_family1 joys_sorrows1 make_decisions1 problems_friends1, std item gen(Foverallsup)
lab var Foverallsup "Percieved overall social support scale"

// family support 0.8503
alpha family_help1 help_support1 problems_family1 make_decisions1, std item gen(Ffamsupp)
lab var Ffamsupp "Percieved family social support scale"

// friends support 0.8738
alpha friends_help1 go_wrong1 joys_sorrows1 problems_friends1, std item gen(Ffrndsupp)
lab var Ffrndsupp "Percieved friends social support scale"


* Phone Contact
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

gen Fnumphonecontact= phone
lab var Fnumphonecontact "Number of contacts in phone"

//log transformation to correct right skewness
gen Fnumphonecontact_log=log(Fnumphonecontact) 
lab var Fnumphonecontact_log "Logged number of contacts in phone"

* social media
fre social_media1___1 social_media1___2 social_media1___3 social_media1___4 social_media1___5 social_media1___6 social_media1___7 social_media1___8 social_media1___9 social_media1___10 social_media1___13 social_media1___11

egen Fsmcount = rowtotal(social_media1___1 social_media1___2 social_media1___3 social_media1___4 social_media1___5 social_media1___6 social_media1___7 social_media1___8 social_media1___9 social_media1___10 social_media1___13 social_media1___11), mi
tab Fsmcount
lab var Fsmcount "Number of social media sites used"

* community sociality/safety
* community integration 0.8150
fre integration1 integration2 integration3
recode integration1 integration2 integration3 (1=7)(2=6)(3=5)(4=4)(5=3)(6=2)(7=1)
alpha integration1 integration2 integration3, std item gen(Fcommunity)
lab var Fcommunity "Community integration scale"

* volunteering 
// any volunteering, including work for organizations and individual others
fre volunteer volunteer_help
recode volunteer_help (3 4 =.)
gen Fanyvolunteer = 0
replace Fanyvolunteer=1 if volunteer==1 | volunteer_help==1
replace Fanyvolunteer=. if volunteer==.& volunteer_help==.
lab var Fanyvolunteer "Any volunteering in past year including orgs and individuals"
tab Fanyvolunteer

//volunteering complexity 
fre volunteer_act___coach volunteer_act___teach volunteer_act___mentor volunteer_act___usher volunteer_act___food volunteer_act___cloth volunteer_act___fund volunteer_act___med volunteer_act___office volunteer_act___manage volunteer_act___art volunteer_act___drive volunteer_act___other volunteer_other

egen Fnumvolunteer = rowtotal(volunteer_act___coach volunteer_act___teach volunteer_act___mentor volunteer_act___usher volunteer_act___food volunteer_act___cloth volunteer_act___fund volunteer_act___med volunteer_act___office volunteer_act___manage volunteer_act___art volunteer_act___drive volunteer_act___other), miss
tab Fnumvolunteer
lab var Fnumvolunteer "Number of types of volunteering activities"

* caregiving
fre assisting_adult
clonevar Frcaregiver=assisting_adult
lab var Frcaregiver "if R is providing daily care to other"


*Social participation
// freq religious services attendance 
fre religious_attendance1
recode religious_attendance1 (5=1 "at least once a week") (4=2 "once or twice a month") (3=3 "a few times a year") (2=4 "seldom") (5=1 "never"), gen(Freligiousattendance)
lab var Freligiousattendance "Frequency of attending religious services: higher=more"

*recode religious_attendance1 (1=52)(2=18)(3=4)(4=2)(5=0), gen(church_f)
*lab var church_f "number of days of Religious attendance in a year"

//freq visiting families and friends
fre famfriends_f
recode famfriends_f (0=.) (6=0), gen(Ffamfriends)
lab var famfriends_f "Frequency of visiting family and friends: higher=more"

*visitors_f
*recode visitors_f (0=.) (1=1) (2=4) (3=48) (4=208) (5=360) (6=0) 

//freq of volunteer
*fre volunteer_often
*recode volunteer_often (6=.) (1=52) (2=20) (3=3) (4=1) 
/*
egen Fsocpartic=rowtotal(church_f famfriends_f  visitors_f volunteer_often ),mi
tab Fsocpartic
lab var Fsocpartic "# days social participation (religious, vol, frd&fam) in a year"
*/
// summary measures
sum Fnumlifeevents Fnumbrevement Fanybrevement Flifecogday Foverallsup Ffamsupp Ffrndsupp Fnumphonecontact Fnumphonecontact_log Fsmcount Fcommunity Fanyvolunteer Fnumvolunteer Frcaregiver Freligiousattendance famfriends_f

***************************************************************
**#3. Dependent variables
***************************************************************
** bridging
sum bridging

** bonding 
alpha family marriage friends, gen(rq)
lab var rq "Relationship quality"
alpha family_help1-problems_friends1,gen(pss)
lab var pss "Perceived social support"
sem (Bonding -> pkin plisten pcare padvice mstrength netsize rq	pss), var(Bonding@1) method(mlmv) vce(cluster SUBID) cov(e.pkin*e.netsize) cov(e.rq*e.pss) cov(e.pkin*e.mstrength) cov(e.mstrength*e.rq) cov(e.mstrength*e.pss) cov(e.netsize*e.pss) cov(e.netsize*e.rq) cov(e.pcare*e.pss)
estat gof, stats(all)
estat mindices

predict bonding, latent(Bonding) 
label var bonding "Bonding capital"

** cognition
egen Fstdmoca =  std(moca_raw)
sum moca_raw Fstdmoca
kdensity moca_raw 
kdensity Fstdmoca
graph close 

** controls
sum agesnad female white 



***************************************************************
**#2 Data analysis
***************************************************************
** controls
sum agesnad female white 

*############################################*
*#########A. Predicting Bridging ############*
*############################################*
**1. demographics and ses predicting bridging
sum edu Fmarried Fparenthood Fhhinc Fworking Flivealone grphouse Fladder Focccomplex Foccuprestige

xtset SUBID time
eststo clear

foreach x of varlist edu Fmarried Fparenthood Fhhinc Fworking Flivealone grphouse Fladder Focccomplex Foccuprestige {
	eststo `x': meglm bridging c.agesnad female white `x' ||SUBID:
	mat N1= e(N)*J(`=rowsof(e(b))', `=colsof(e(b))', 1)
    estadd mat N1= N1
}

coefplot *, legend(off) drop(agesnad female white _cons) xline(0)  ///
            xtitle(predicting bridging from demographics and ses) ///
			aux(N1)  ///
            mlabel("{it: N}=" + string(@aux1)) mlabcolor(none) ///
			saving(r01_1.1bridging_demographics_ses, replace)  ///
            addplot(scatter @at @ul, ms(i) mlabel(@mlbl) mlabcolor(black) mlabpos(3) mlabsize(small)) coeflabels(,labsize(small))
			
graph export "C:\Users\windy\OneDrive - Indiana University\1_IU_Project\SNAD\2_SNAD Measures\r01_1.1bridging_demographics_ses.png", as(png) name("Graph") replace
graph close

**2. cognitive health
eststo clear
xtset SUBID time
sum attention speed execfxn epmem language visual moca_raw CRAFTDRESD Fccisscale FECOGSelf FECOGInformant Fdiff reserve frolobe parlobe temlobe occlobe medtemp wmh delayed_rey_sum totalhippvol totalamygvol 

foreach x of varlist attention speed execfxn epmem language visual moca_raw CRAFTDRESD Fccisscale FECOGSelf FECOGInformant Fdiff reserve frolobe parlobe temlobe occlobe medtemp wmh delayed_rey_sum totalhippvol totalamygvol     {
	eststo `x': meglm bridging c.agesnad female white `x' ||SUBID:
	mat N1= e(N)*J(`=rowsof(e(b))', `=colsof(e(b))', 1)
    estadd mat N1= N1
}

coefplot *, legend(off) drop(agesnad female white _cons) xline(0)  ///
            xtitle(predicting bridging from cognitive functioning) ///
			aux(N1)  ///
			saving(r01_1.2bridging_cognition.gph, replace)  ///
            mlabel("{it: N}=" + string(@aux1)) mlabcolor(none) ///
            addplot(scatter @at @ul, ms(i) mlabel(@mlbl) mlabcolor(black) mlabpos(3) mlabsize(small)) coeflabels(,labsize(small))
			
graph export "C:\Users\windy\OneDrive - Indiana University\1_IU_Project\SNAD\2_SNAD Measures\r01_1.2bridging_cognition.png", as(png) name("Graph") replace
graph close

**3. other health and well-being
label var Fhearingloss "hearing loss last year"
label var Fvisionloss "vision loss last year"

sum Favgmastery Fhappy Fuclaloneliness Fpsstress Favganxiety Fgds15 DEL HALL AGIT ANX DEPD ELAT APA DISN IRR MOT NITE APP Favgphysical Fqolwithrela Fqolnorela Fsleep Fpsleepq Fhearingloss Fvisionloss

eststo clear
xtset SUBID time

foreach x of varlist Favgmastery Fhappy Fuclaloneliness Fpsstress Favganxiety Fgds15 DEL HALL AGIT ANX DEPD ELAT APA DISN IRR MOT NITE APP Favgphysical Fqolwithrela Fqolnorela Fsleep Fpsleepq Fhearingloss Fvisionloss  {
	eststo `x': meglm bridging c.agesnad female white `x' ||SUBID:
	mat N1= e(N)*J(`=rowsof(e(b))', `=colsof(e(b))', 1)
    estadd mat N1= N1
}

coefplot *, legend(off) drop(agesnad female white _cons) xline(0)  ///
            xtitle(predicting bridging from other health and wellbeing) ///
			aux(N1)  ///
			saving(r01_1.3bridging_othhealth, replace)  ///
            mlabel("{it: N}=" + string(@aux1)) mlabcolor(none) ///
            addplot(scatter @at @ul, ms(i) mlabel(@mlbl) mlabcolor(black) mlabpos(3) mlabsize(small)) coeflabels(,labsize(small))
			
graph export "C:\Users\windy\OneDrive - Indiana University\1_IU_Project\SNAD\2_SNAD Measures\r01_1.3bridging_othhealth.png", as(png) name("Graph") replace
graph close


**4. social engagement
sum Flifecogday Fnumlifeevents Fnumbrevement Fanybrevement Foverallsup Ffamsupp Ffrndsupp Fnumphonecontact Fnumphonecontact_log Fsmcount Fcommunity Fanyvolunteer Fnumvolunteer Frcaregiver Freligiousattendance famfriends_f

eststo clear
xtset SUBID time

foreach x of varlist Flifecogday Fnumlifeevents Fnumbrevement Fanybrevement Foverallsup Ffamsupp Ffrndsupp Fnumphonecontact Fnumphonecontact_log Fsmcount Fcommunity Fanyvolunteer Fnumvolunteer Frcaregiver Freligiousattendance famfriends_f  {
	eststo `x': meglm bridging c.agesnad female white `x' ||SUBID:
	mat N1= e(N)*J(`=rowsof(e(b))', `=colsof(e(b))', 1)
    estadd mat N1= N1
}

coefplot *, legend(off) drop(agesnad female white _cons) xline(0)  ///
            xtitle(predicting bridging from social engagement) ///
			aux(N1)  ///
			saving(r01_1.4bridging_socialengagement, replace)  ///
            mlabel("{it: N}=" + string(@aux1)) mlabcolor(none) ///
            addplot(scatter @at @ul, ms(i) mlabel(@mlbl) mlabcolor(black) mlabpos(3) mlabsize(small)) coeflabels(,labsize(small))
			
graph export "C:\Users\windy\OneDrive - Indiana University\1_IU_Project\SNAD\2_SNAD Measures\r01_1.4bridging_socialengagement.png", as(png) name("Graph") replace
graph close


*############################################*
*#########B. Predicting Bonding ############*
*############################################*
kdensity bonding
graph close

**1. demographics and ses predicting bridging
sum edu Fmarried Fparenthood Fhhinc Fworking Flivealone grphouse Fladder Focccomplex Foccuprestige

xtset SUBID time
eststo clear

foreach x of varlist edu Fmarried Fparenthood Fhhinc Fworking Flivealone grphouse Fladder Focccomplex Foccuprestige {
	eststo `x': meglm bonding c.agesnad female white `x' ||SUBID:
	mat N1= e(N)*J(`=rowsof(e(b))', `=colsof(e(b))', 1)
    estadd mat N1= N1
}

coefplot *, legend(off) drop(agesnad female white _cons) xline(0)  ///
            xtitle(predicting bonding from demographics and ses) ///
			aux(N1)  ///
			saving(r01_2.1bonding_demographics_ses, replace)  ///
            mlabel("{it: N}=" + string(@aux1)) mlabcolor(none) ///
            addplot(scatter @at @ul, ms(i) mlabel(@mlbl) mlabcolor(black) mlabpos(3) mlabsize(small)) coeflabels(,labsize(small))
			
graph export "C:\Users\windy\OneDrive - Indiana University\1_IU_Project\SNAD\2_SNAD Measures\r01_2.1bonding_demographics_ses.png", as(png) name("Graph") replace
graph close

**2. cognitive health
eststo clear
xtset SUBID time
sum attention speed execfxn epmem language visual moca_raw CRAFTDRESD Fccisscale FECOGSelf FECOGInformant Fdiff reserve frolobe parlobe temlobe occlobe medtemp wmh delayed_rey_sum totalhippvol totalamygvol 

foreach x of varlist attention speed execfxn epmem language visual moca_raw CRAFTDRESD Fccisscale FECOGSelf FECOGInformant Fdiff reserve frolobe parlobe temlobe occlobe medtemp wmh delayed_rey_sum totalhippvol totalamygvol     {
	eststo `x': meglm bonding c.agesnad female white `x' ||SUBID:
	mat N1= e(N)*J(`=rowsof(e(b))', `=colsof(e(b))', 1)
    estadd mat N1= N1
}

coefplot *, legend(off) drop(agesnad female white _cons) xline(0)  ///
            xtitle(predicting bonding from cognitive functioning) ///
			aux(N1)  ///
			saving(r01_2.2bonding_cognition, replace)  ///
            mlabel("{it: N}=" + string(@aux1)) mlabcolor(none) ///
            addplot(scatter @at @ul, ms(i) mlabel(@mlbl) mlabcolor(black) mlabpos(3) mlabsize(small)) coeflabels(,labsize(small))
			
graph export "C:\Users\windy\OneDrive - Indiana University\1_IU_Project\SNAD\2_SNAD Measures\r01_2.2bonding_cognition.png", as(png) name("Graph") replace
graph close

**3. other health and well-being
label var Fhearingloss "hearing loss last year"
label var Fvisionloss "vision loss last year"

sum Favgmastery Fhappy Fuclaloneliness Fpsstress Favganxiety Fgds15 DEL HALL AGIT ANX DEPD ELAT APA DISN IRR MOT NITE APP Favgphysical Fqolwithrela Fqolnorela Fsleep Fpsleepq Fhearingloss Fvisionloss

eststo clear
xtset SUBID time

foreach x of varlist Favgmastery Fhappy Fuclaloneliness Fpsstress Favganxiety Fgds15 DEL HALL AGIT ANX DEPD ELAT APA DISN IRR MOT NITE APP Favgphysical Fqolwithrela Fqolnorela Fsleep Fpsleepq Fhearingloss Fvisionloss  {
	eststo `x': meglm bonding c.agesnad female white `x' ||SUBID:
	mat N1= e(N)*J(`=rowsof(e(b))', `=colsof(e(b))', 1)
    estadd mat N1= N1
}

coefplot *, legend(off) drop(agesnad female white _cons) xline(0)  ///
            xtitle(predicting bonding from other health and wellbeing) ///
			aux(N1)  ///
			saving(r01_2.3bonding_othhealth, replace)  ///
            mlabel("{it: N}=" + string(@aux1)) mlabcolor(none) ///
            addplot(scatter @at @ul, ms(i) mlabel(@mlbl) mlabcolor(black) mlabpos(3) mlabsize(small)) coeflabels(,labsize(small))
			
graph export "C:\Users\windy\OneDrive - Indiana University\1_IU_Project\SNAD\2_SNAD Measures\r01_2.3bonding_othhealth.png", as(png) name("Graph") replace
graph close


**4. social engagement
sum Flifecogday Fnumlifeevents Fnumbrevement Fanybrevement Foverallsup Ffamsupp Ffrndsupp Fnumphonecontact Fnumphonecontact_log Fsmcount Fcommunity Fanyvolunteer Fnumvolunteer Frcaregiver Freligiousattendance famfriends_f

eststo clear
xtset SUBID time

foreach x of varlist Flifecogday Fnumlifeevents Fnumbrevement Fanybrevement Foverallsup Ffamsupp Ffrndsupp Fnumphonecontact Fnumphonecontact_log Fsmcount Fcommunity Fanyvolunteer Fnumvolunteer Frcaregiver Freligiousattendance famfriends_f  {
	eststo `x': meglm bonding c.agesnad female white `x' ||SUBID:
	mat N1= e(N)*J(`=rowsof(e(b))', `=colsof(e(b))', 1)
    estadd mat N1= N1
}

coefplot *, legend(off) drop(agesnad female white _cons) xline(0)  ///
            xtitle(predicting bonding from social engagement) ///
			aux(N1)  ///
			saving(r01_2.4bonding_socialengagement, replace)  ///
            mlabel("{it: N}=" + string(@aux1)) mlabcolor(none) ///
            addplot(scatter @at @ul, ms(i) mlabel(@mlbl) mlabcolor(black) mlabpos(3) mlabsize(small)) coeflabels(,labsize(small))
			
graph export "C:\Users\windy\OneDrive - Indiana University\1_IU_Project\SNAD\2_SNAD Measures\r01_2.4bonding_socialengagement.png", as(png) name("Graph") replace
graph close

*############################################*
*#########C. Predicting cognition ############*
*############################################*
egen Fstdmoca =  std(moca_raw)
sum moca_raw Fstdmoca
kdensity moca_raw 
kdensity Fstdmoca
graph close 

**1. demographics and ses predicting bridging
sum edu Fmarried Fparenthood Fhhinc Fworking Flivealone grphouse Fladder Focccomplex Foccuprestige

xtset SUBID time
eststo clear

foreach x of varlist edu Fmarried Fparenthood Fhhinc Fworking Flivealone grphouse Fladder Focccomplex Foccuprestige {
	eststo `x': meglm Fstdmoca c.agesnad female white `x' ||SUBID:
	mat N1= e(N)*J(`=rowsof(e(b))', `=colsof(e(b))', 1)
    estadd mat N1= N1
}

coefplot *, legend(off) drop(agesnad female white _cons) xline(0)  ///
            xtitle(predicting stdmoca from demographics and ses) ///
			aux(N1)  ///
			saving(r01_3.1stdmoca_demographics_ses, replace)  ///
            mlabel("{it: N}=" + string(@aux1)) mlabcolor(none) ///
            addplot(scatter @at @ul, ms(i) mlabel(@mlbl) mlabcolor(black) mlabpos(3) mlabsize(small)) coeflabels(,labsize(small))
			
graph export "C:\Users\windy\OneDrive - Indiana University\1_IU_Project\SNAD\2_SNAD Measures\r01_3.1stdmoca_demographics_ses.png", as(png) name("Graph") replace
graph close

**2. cognitive health
eststo clear
xtset SUBID time
sum attention speed execfxn epmem language visual CRAFTDRESD Fccisscale FECOGSelf FECOGInformant Fdiff reserve frolobe parlobe temlobe occlobe medtemp wmh delayed_rey_sum totalhippvol totalamygvol 

foreach x of varlist attention speed execfxn epmem language visual CRAFTDRESD Fccisscale FECOGSelf FECOGInformant Fdiff reserve frolobe parlobe temlobe occlobe medtemp wmh delayed_rey_sum totalhippvol totalamygvol     {
	eststo `x': meglm Fstdmoca c.agesnad female white `x' ||SUBID:
	mat N1= e(N)*J(`=rowsof(e(b))', `=colsof(e(b))', 1)
    estadd mat N1= N1
}

coefplot *, legend(off) drop(agesnad female white _cons) xline(0)  ///
            xtitle(predicting stdmoca from cognitive functioning) ///
			aux(N1)  ///
			saving(r01_3.2stdmoca_cognition, replace)  ///
            mlabel("{it: N}=" + string(@aux1)) mlabcolor(none) ///
            addplot(scatter @at @ul, ms(i) mlabel(@mlbl) mlabcolor(black) mlabpos(3) mlabsize(small)) coeflabels(,labsize(small))
			
graph export "C:\Users\windy\OneDrive - Indiana University\1_IU_Project\SNAD\2_SNAD Measures\r01_3.2stdmoca_cognition.png", as(png) name("Graph") replace
graph close

**3. other health and well-being
sum Favgmastery Fhappy Fuclaloneliness Fpsstress Favganxiety Fgds15 DEL HALL AGIT ANX DEPD ELAT APA DISN IRR MOT NITE APP Favgphysical Fqolwithrela Fqolnorela Fsleep Fpsleepq Fhearingloss Fvisionloss

eststo clear
xtset SUBID time

foreach x of varlist Favgmastery Fhappy Fuclaloneliness Fpsstress Favganxiety Fgds15 DEL HALL AGIT ANX DEPD ELAT APA DISN IRR MOT NITE APP Favgphysical Fqolwithrela Fqolnorela Fsleep Fpsleepq Fhearingloss Fvisionloss  {
	eststo `x': meglm Fstdmoca c.agesnad female white `x' ||SUBID:
	mat N1= e(N)*J(`=rowsof(e(b))', `=colsof(e(b))', 1)
    estadd mat N1= N1
}

coefplot *, legend(off) drop(agesnad female white _cons) xline(0)  ///
            xtitle(predicting stdmoca from other health and wellbeing) ///
			aux(N1)  ///
			saving(r01_3.3stdmoca_othhealth, replace)  ///
            mlabel("{it: N}=" + string(@aux1)) mlabcolor(none) ///
            addplot(scatter @at @ul, ms(i) mlabel(@mlbl) mlabcolor(black) mlabpos(3) mlabsize(small)) coeflabels(,labsize(small))
			
graph export "C:\Users\windy\OneDrive - Indiana University\1_IU_Project\SNAD\2_SNAD Measures\r01_3.3stdmoca_othhealth.png", as(png) name("Graph") replace
graph close


**4. social engagement
sum Flifecogday Fnumlifeevents Fnumbrevement Fanybrevement Foverallsup Ffamsupp Ffrndsupp Fnumphonecontact Fnumphonecontact_log Fsmcount Fcommunity Fanyvolunteer Fnumvolunteer Frcaregiver Freligiousattendance famfriends_f

eststo clear
xtset SUBID time

foreach x of varlist Flifecogday Fnumlifeevents Fnumbrevement Fanybrevement Foverallsup Ffamsupp Ffrndsupp Fnumphonecontact Fnumphonecontact_log Fsmcount Fcommunity Fanyvolunteer Fnumvolunteer Frcaregiver Freligiousattendance famfriends_f  {
	eststo `x': meglm Fstdmoca c.agesnad female white `x' ||SUBID:
	mat N1= e(N)*J(`=rowsof(e(b))', `=colsof(e(b))', 1)
    estadd mat N1= N1
}

coefplot *, legend(off) drop(agesnad female white _cons) xline(0)  ///
            xtitle(predicting stdmoca from social engagement) ///
			aux(N1)  ///
			saving(r01_3.4stdmoca_socialengagement, replace)  ///
            mlabel("{it: N}=" + string(@aux1)) mlabcolor(none) ///
            addplot(scatter @at @ul, ms(i) mlabel(@mlbl) mlabcolor(black) mlabpos(3) mlabsize(small)) coeflabels(,labsize(small))
			
graph export "C:\Users\windy\OneDrive - Indiana University\1_IU_Project\SNAD\2_SNAD Measures\r01_3.4stdmoca_socialengagement.png", as(png) name("Graph") replace
graph close


*############################################*
*#########D. Predicting Overall support ############*
*############################################*
hist Foverallsup, normal
egen stdoverallsup = std(Foverallsup)
hist stdoverallsup, normal
graph close

**1. demographics and ses predicting bridging
sum edu Fmarried Fparenthood Fhhinc Fworking Flivealone grphouse Fladder Focccomplex Foccuprestige

xtset SUBID time
eststo clear

foreach x of varlist edu Fmarried Fparenthood Fhhinc Fworking Flivealone grphouse Fladder Focccomplex Foccuprestige {
	eststo `x': meglm stdoverallsup c.agesnad female white `x' ||SUBID:
	mat N1= e(N)*J(`=rowsof(e(b))', `=colsof(e(b))', 1)
    estadd mat N1= N1
}

coefplot *, legend(off) drop(agesnad female white _cons) xline(0)  ///
            xtitle(predicting overall support from demographics and ses) ///
			aux(N1)  ///
            mlabel("{it: N}=" + string(@aux1)) mlabcolor(none) ///
			saving(r01_1.1support_demographics_ses, replace)  ///
            addplot(scatter @at @ul, ms(i) mlabel(@mlbl) mlabcolor(black) mlabpos(3) mlabsize(small)) coeflabels(,labsize(small))
			
graph export "C:\Users\windy\OneDrive - Indiana University\1_IU_Project\SNAD\2_SNAD Measures\r01_1.1supp_demographics_ses.png", as(png) name("Graph") replace
graph close

**2. cognitive health
eststo clear
xtset SUBID time
sum attention speed execfxn epmem language visual moca_raw CRAFTDRESD Fccisscale FECOGSelf FECOGInformant Fdiff reserve frolobe parlobe temlobe occlobe medtemp wmh delayed_rey_sum totalhippvol totalamygvol 

foreach x of varlist attention speed execfxn epmem language visual moca_raw CRAFTDRESD Fccisscale FECOGSelf FECOGInformant Fdiff reserve frolobe parlobe temlobe occlobe medtemp wmh delayed_rey_sum totalhippvol totalamygvol     {
	eststo `x': meglm stdoverallsup c.agesnad female white `x' ||SUBID:
	mat N1= e(N)*J(`=rowsof(e(b))', `=colsof(e(b))', 1)
    estadd mat N1= N1
}

coefplot *, legend(off) drop(agesnad female white _cons) xline(0)  ///
            xtitle(predicting overall support from cognitive functioning) ///
			aux(N1)  ///
			saving(r01_1.2support_cognition.gph, replace)  ///
            mlabel("{it: N}=" + string(@aux1)) mlabcolor(none) ///
            addplot(scatter @at @ul, ms(i) mlabel(@mlbl) mlabcolor(black) mlabpos(3) mlabsize(small)) coeflabels(,labsize(small))
			
graph export "C:\Users\windy\OneDrive - Indiana University\1_IU_Project\SNAD\2_SNAD Measures\r01_1.2supp_cognition.png", as(png) name("Graph") replace
graph close

**3. other health and well-being
label var Fhearingloss "hearing loss last year"
label var Fvisionloss "vision loss last year"

sum Favgmastery Fhappy Fuclaloneliness Fpsstress Favganxiety Fgds15 DEL HALL AGIT ANX DEPD ELAT APA DISN IRR MOT NITE APP Favgphysical Fqolwithrela Fqolnorela Fsleep Fpsleepq Fhearingloss Fvisionloss

eststo clear
xtset SUBID time

foreach x of varlist Favgmastery Fhappy Fuclaloneliness Fpsstress Favganxiety Fgds15 DEL HALL AGIT ANX DEPD ELAT APA DISN IRR MOT NITE APP Favgphysical Fqolwithrela Fqolnorela Fsleep Fpsleepq Fhearingloss Fvisionloss  {
	eststo `x': meglm stdoverallsup c.agesnad female white `x' ||SUBID:
	mat N1= e(N)*J(`=rowsof(e(b))', `=colsof(e(b))', 1)
    estadd mat N1= N1
}

coefplot *, legend(off) drop(agesnad female white _cons) xline(0)  ///
            xtitle(predicting overall support from other health and wellbeing) ///
			aux(N1)  ///
			saving(r01_1.3support_othhealth, replace)  ///
            mlabel("{it: N}=" + string(@aux1)) mlabcolor(none) ///
            addplot(scatter @at @ul, ms(i) mlabel(@mlbl) mlabcolor(black) mlabpos(3) mlabsize(small)) coeflabels(,labsize(small))
			
graph export "C:\Users\windy\OneDrive - Indiana University\1_IU_Project\SNAD\2_SNAD Measures\r01_1.3supp_othhealth.png", as(png) name("Graph") replace
graph close


**4. social engagement
sum Flifecogday Fnumlifeevents Fnumbrevement Fanybrevement Foverallsup Ffamsupp Ffrndsupp Fnumphonecontact Fnumphonecontact_log Fsmcount Fcommunity Fanyvolunteer Fnumvolunteer Frcaregiver Freligiousattendance famfriends_f

eststo clear
xtset SUBID time

foreach x of varlist Flifecogday Fnumlifeevents Fnumbrevement Fanybrevement  Ffamsupp Ffrndsupp Fnumphonecontact Fnumphonecontact_log Fsmcount Fcommunity Fanyvolunteer Fnumvolunteer Frcaregiver Freligiousattendance famfriends_f  {
	eststo `x': meglm stdoverallsup c.agesnad female white `x' ||SUBID:
	mat N1= e(N)*J(`=rowsof(e(b))', `=colsof(e(b))', 1)
    estadd mat N1= N1
}

coefplot *, legend(off) drop(agesnad female white _cons) xline(0)  ///
            xtitle(predicting overall support from social engagement) ///
			aux(N1)  ///
			saving(r01_1.4support_socialengagement, replace)  ///
            mlabel("{it: N}=" + string(@aux1)) mlabcolor(none) ///
            addplot(scatter @at @ul, ms(i) mlabel(@mlbl) mlabcolor(black) mlabpos(3) mlabsize(small)) coeflabels(,labsize(small))
			
graph export "C:\Users\windy\OneDrive - Indiana University\1_IU_Project\SNAD\2_SNAD Measures\r01_1.4supp_socialengagement.png", as(png) name("Graph") replace
graph close



**************************************************************************************************
**************************************************************************************************
*********** Create a list of dropping variables ***********
**************************************************************************************************
**************************************************************************************************
clear all

cd "C:\Users\windy\OneDrive - Indiana University\1_IU_Project\SNAD\2_SNAD Measures"
use "C:\Users\windy\OneDrive - Indiana University\1_IU_Project\SNAD\Focal\snad-analysis-focal-r01match-20240603.dta", clear

*********************
** inspect the data
*********************
*codebook
*describe

***  wave indicator 
fre time
unique SUBID  //n=313
keep if time==1

** destring all string variable
findname, type(string) local(strvars)
destring `strvars', replace force

** summary to check if all variables have valid stats
ds
local keep_vars SUBID

* Create a temporary dataset to track observation counts n<100
foreach var of varlist _all {
    count if !missing(`var')
    * If the number of non-missing observations is less than 100, mark the variable
    if `r(N)' < 50 {
        local keep_vars `keep_vars' `var'
    }
}
keep `keep_vars'

quietly {
    log using r01dropplist.txt, text replace
    noisily codebook, compact header 
    log close
}

quietly {
    log using r01dropplist2.txt, text replace
    noisily codebook, header 
    log close
}










