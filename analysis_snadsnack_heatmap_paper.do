
// Analysis for SNAD/SNACK heatmap paper
// Created: 09/16/26
// Data used:
//	combined_heatmap_paper.dta
//	from recodes_snadsnack_heatmap_paper.do

* Working directory (set to "Code"):
cd "/Users/jessicalogin/Desktop/Fall 2026 Working Folder/Code"

use "../Data/combined_heatmap_paper.dta", clear

sum married parenthood working livealone_rev grphouse anyvolunteer ///
	std_church_day std_famfriends_day std_smcount assisting_adult widowed ///
	anybrevement std_phassles std_friends std_family std_marriage std_frndsupp ///
	std_famsupp std_msupport3 std_communityscale_rev std_pkin std_pclose ///
	std_padvice std_pprox std_mfreq std_ploan std_pchores ///
	std_bridging std_efctsize std_diverse std_density_rev ///
	std_netsize std_avg_euc_age std_weakest std_occcomplex_people ///
	std_diff_college diff_white std_numphonecontact std_pfriend ///
	std_sole std_diff_race std_sdage std_pcollege std_pwhite 
	
****************************************
** All Models (feeding heatmap)

* Create function to output coefs
run "output_coefficients_program.do" 

* Set up IVs
local varlist `"std_married std_parenthood std_working std_livealone_rev std_grphouse std_anyvolunteer std_church_day std_famfriends_day std_smcount  std_assisting_adult std_widowed std_anybrevement std_phassles std_friends std_family std_marriage std_frndsupp std_famsupp std_msupport3 std_communityscale_rev std_pkin std_pclose std_padvice std_pprox std_mfreq std_ploan std_pchores std_bridging std_efctsize std_diverse std_density_rev std_b1density_rev std_netsize std_avg_euc_age std_weakest std_occcomplex_people std_diff_college std_diff_white std_numphonecontact std_pfriend std_sole std_diff_race std_sdage std_pcollege std_pwhite "'

* Cognitive outcomes
run_heatmap, dvs(std_language std_epmem std_moca_raw MCI_rev) ///
	covars(c.std_age female white college std_faq std_gds15 i.source) ///
	ivs(`varlist') ///
	drop(std_age female white college std_faq std_gds15 *.source) ///
	file("../Output/Coefficients/cog_heatmap_coefs.csv")

* Mental health outcomes
run_heatmap, dvs(std_gds15_rev std_anxiety_rev std_happiness std_whole) ///
	covars(c.std_age female white college std_faq i.source) ///
	ivs(`varlist') ///
	drop(std_age female white college std_faq *.source) ///
	file("../Output/Coefficients/mh_heatmap_coefs.csv")

****************************************
** Multiple test correction

****************************************
** Pairwise correlation matrix

pwcorr `varlist' std_language std_epmem std_moca_raw MCI_rev std_gds15_rev std_anxiety_rev std_happiness std_whole, sig obs

matrix C = r(C)      // correlations
matrix P = r(sig)    // p-values
matrix N = r(Nobs)   // pairwise N

local out "../Output/Coefficients/corr_matrix.xlsx"
putexcel set "`out'", replace sheet("r")
putexcel A1 = matrix(C), names nformat("0.00")
putexcel set "`out'", modify sheet("p")
putexcel A1 = matrix(P), names nformat("0.000")
putexcel set "`out'", modify sheet("N")
putexcel A1 = matrix(N), names
putexcel save

****************************************
** Coefficient size comparison


