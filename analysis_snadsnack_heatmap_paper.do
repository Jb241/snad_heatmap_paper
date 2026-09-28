
// Analysis for SNAD/SNACK heatmap paper
// Created: 09/16/26

* Working directory (set to "Code"):
cd "/Users/jessicalogin/Library/CloudStorage/OneDrive-SharedLibraries-IndianaUniversity/Qu, Tianyao - 4_Cog_Bri/Fall 2026 Working Folder/Code"

use "../Data/combined_heatmap_paper.dta", clear

sum married parenthood working livealone grphouse anyvolunteer ///
	std_church_day std_famfriends_day std_smcount assisting_adult widowed ///
	anybrevement phassles std_friends std_family std_marriage std_frndsupp ///
	std_famsupp std_msupport3 std_communityscale std_pkin std_pclose ///
	std_padvice std_pprox std_mfreq std_ploan std_pchores ///
	std_bridging std_efctsize std_diverse std_density_rev ///
	std_netsize std_avg_euc_age std_weakest std_occcomplex_people ///
	std_diff_college diff_white std_numphonecontact std_pfriend ///
	sole std_diff_race std_sdage std_pcollege std_pwhite 
	
****************************************
** All Models (feeding heatmap)

* Create function to output coefs
run "stack_in_one_column.do" 
	
//1. language
set matsize 11000
eststo clear
local dv_varlist `"married parenthood working livealone grphouse anyvolunteer std_church_day std_famfriends_day std_smcount  assisting_adult widowed anybrevement phassles std_friends std_family std_marriage std_frndsupp std_famsupp std_msupport3 std_communityscale std_pkin std_pclose std_padvice std_pprox std_mfreq std_ploan std_pchores std_bridging std_efctsize std_diverse std_density_rev std_b1density_rev std_netsize std_avg_euc_age std_weakest std_occcomplex_people std_diff_college std_diff_white std_numphonecontact std_pfriend sole std_diff_race std_sdage std_pcollege std_pwhite "'


foreach x in `dv_varlist'  {
	qui eststo `x': meglm std_language c.std_age female white college std_faq gds_h i.source `x' || surveyid:
    qui mat N1 = e(N) * J(1, colsof(e(b)), 1)  // Ensure it's a row vector
    qui estadd mat N1 = N1

}

eststo mods: appendmodels $eststo
esttab mods using "../Output/Coefficients/heatmap_coefs.csv", replace ///
	  b(a2) se nonumbers nogap  ///
	 drop(std_age female white college std_faq gds_h *.source)  ///
	 star(+ 0.10 * 0.05 ** 0.01 *** 0.001) ///
	 	 title(1. language (verbal fluence))


//2. epmem
eststo clear
local dv_varlist `"married parenthood working livealone grphouse anyvolunteer std_church_day std_famfriends_day std_smcount  assisting_adult widowed anybrevement phassles std_friends std_family std_marriage std_frndsupp std_famsupp std_msupport3 std_communityscale std_pkin std_pclose std_padvice std_pprox std_mfreq std_ploan std_pchores std_bridging std_efctsize std_diverse std_density_rev std_b1density_rev std_netsize std_avg_euc_age std_weakest std_occcomplex_people std_diff_college std_diff_white std_numphonecontact std_pfriend sole std_diff_race std_sdage std_pcollege std_pwhite"'

foreach x in `dv_varlist'  {
	qui eststo `x': meglm std_epmem c.std_age female white college std_faq gds_h i.source `x' || surveyid:
    qui mat N1 = e(N) * J(1, colsof(e(b)), 1)  // Ensure it's a row vector
    qui estadd mat N1 = N1

}

eststo mods: appendmodels $eststo
esttab mods using "../Output/Coefficients/heatmap_coefs.csv", append ///
	  b(a2) se nonumbers nogap  ///
	 drop(std_age female white college std_faq gds_h *.dummysource)  ///
	 star(+ 0.10 * 0.05 ** 0.01 *** 0.001) ///
	 	 title(2. episodic memory)		


//3. moca score 
eststo clear
local dv_varlist `"married parenthood working livealone grphouse anyvolunteer std_church_day std_famfriends_day std_smcount  assisting_adult widowed anybrevement phassles std_friends std_family std_marriage std_frndsupp std_famsupp std_msupport3 std_communityscale std_pkin std_pclose std_padvice std_pprox std_mfreq std_ploan std_pchores std_bridging std_efctsize std_diverse std_density_rev std_b1density_rev std_netsize std_avg_euc_age std_weakest std_occcomplex_people std_diff_college std_diff_white std_numphonecontact std_pfriend sole std_diff_race std_sdage std_pcollege std_pwhite"'

foreach x in `dv_varlist'  {
	qui eststo `x': meglm std_moca_raw c.std_age female white college std_faq gds_h i.source `x' || surveyid:
    qui mat N1 = e(N) * J(1, colsof(e(b)), 1)  // Ensure it's a row vector
    qui estadd mat N1 = N1

}

eststo mods: appendmodels $eststo
esttab mods using "../Output/Coefficients/heatmap_coefs.csv", append ///
	  b(a2) se nonumbers nogap  ///
	 drop(std_age female white college std_faq gds_h *.source)  ///
	 star(+ 0.10 * 0.05 ** 0.01 *** 0.001) ///
	 	 title(3. MoCA score)	 

		 
//4. diagnosis
eststo clear
local dv_varlist `"married parenthood working livealone grphouse anyvolunteer std_church_day std_famfriends_day std_smcount  assisting_adult widowed anybrevement phassles std_friends std_family std_marriage std_frndsupp std_famsupp std_msupport3 std_communityscale std_pkin std_pclose std_padvice std_pprox std_mfreq std_ploan std_pchores std_bridging std_efctsize std_diverse std_density_rev std_b1density_rev std_netsize std_avg_euc_age std_weakest std_occcomplex_people std_diff_college std_diff_white std_numphonecontact std_pfriend sole std_diff_race std_sdage std_pcollege std_pwhite"'

foreach x in `dv_varlist'  {
	qui eststo `x': meglm MCI_rev c.std_age female white college std_faq gds_h i.source `x' || surveyid:
    qui mat N1 = e(N) * J(1, colsof(e(b)), 1)  // Ensure it's a row vector
    qui estadd mat N1 = N1

}

eststo mods: appendmodels $eststo
esttab mods using "../Output/Coefficients/heatmap_coefs.csv", append ///
	  b(a2) se nonumbers nogap  ///
	 drop(std_age female white college std_faq gds_h *.source)  ///
	 star(+ 0.10 * 0.05 ** 0.01 *** 0.001) ///
	 	 title(4. Normal vs. MCI)	
