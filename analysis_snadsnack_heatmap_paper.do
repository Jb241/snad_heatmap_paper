
// Analysis for SNAD/SNACK heatmap paper
// Created: by JB 09/16/26
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
// All Models (feeding heatmap)

* Load function to output coefs
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
// Multiple test correction

* Load functions for correction
run "multiple_test_correction_program.do"

* Predictor groups (for plot colors/symbols)
local g_part "std_married std_parenthood std_working std_livealone_rev std_grphouse std_anyvolunteer std_church_day std_famfriends_day std_smcount"
local g_stress "std_assisting_adult std_widowed std_anybrevement std_phassles"
local g_bond "std_friends std_family std_marriage std_frndsupp std_famsupp std_msupport3 std_communityscale_rev std_pkin std_pclose std_padvice std_pprox std_mfreq std_ploan std_pchores"
local g_bridge "std_bridging std_efctsize std_diverse std_density_rev std_b1density_rev std_netsize std_avg_euc_age std_weakest std_occcomplex_people std_diff_college std_diff_white std_numphonecontact std_pfriend std_sole std_diff_race std_sdage std_pcollege std_pwhite"
local grpopts part(`g_part') stress(`g_stress') bond(`g_bond') bridge(`g_bridge')

* Outcomes and plot titles
local cog_dvs std_language std_epmem std_moca_raw MCI_rev
local cog_titles `" "Verbal Fluency" "Episodic Memory" "MoCA" "MCI vs. Normal" "'
local mh_dvs std_gds15_rev std_anxiety_rev std_happiness std_whole
local mh_titles `" "Depressive Symptoms (GDS-15, reversed)" "Anxiety (reversed)" "Happiness" "Life Satisfaction" "'

* Cognitive outcomes
mtc_batch, dvs(`cog_dvs') titles(`"`cog_titles'"') ///
	covars(c.std_age female white college std_faq std_gds15 i.source) ///
	ivs(`varlist') grpopts(`grpopts') ///
	stub(cog) maintitle("P-values Across Cognitive Outcomes")

* Mental health outcomes
mtc_batch, dvs(`mh_dvs') titles(`"`mh_titles'"') ///
	covars(c.std_age female white college std_faq i.source) ///
	ivs(`varlist') grpopts(`grpopts') ///
	stub(mh) maintitle("P-values Across Mental Health Outcomes")

* Stack everything into one results file + summary table
mtc_collect, dvs(`cog_dvs' `mh_dvs')

* Save local for these
preserve
use "../Output/Coefficients/mtc_all_results.dta", clear
keep if sig_fdr == 1
levelsof predictor, local(keyivs)
if "`keyivs'" == "" {
    display as error "No predictors passed FDR correction for the selected outcomes."
    exit 2000
}
restore


****************************************
// Pairwise correlation matrix

* All variables
pwcorr `varlist' std_language std_epmem std_moca_raw MCI_rev std_gds15_rev std_anxiety_rev std_happiness std_whole, sig obs

matrix C = r(C) // correlations
matrix P = r(sig) // p-values
matrix N = r(Nobs) // pairwise N

local out "../Output/Coefficients/corr_matrix.xlsx"
putexcel set "`out'", replace sheet("r")
putexcel A1 = matrix(C), names nformat("0.00")
putexcel set "`out'", modify sheet("p")
putexcel A1 = matrix(P), names nformat("0.000")
putexcel set "`out'", modify sheet("N")
putexcel A1 = matrix(N), names
putexcel save

* Bonding/bridging
local bb_vars `g_bond' `g_bridge'
pwcorr `bb_vars', sig obs

matrix Cbb = r(C)
matrix Pbb = r(sig)
matrix Nbb = r(Nobs)

local out "../Output/Coefficients/corr_matrix_bonding_bridging.xlsx"
putexcel set "`out'", replace sheet("r")
putexcel A1 = matrix(Cbb), names nformat("0.00")
putexcel set "`out'", modify sheet("p")
putexcel A1 = matrix(Pbb), names nformat("0.000")
putexcel set "`out'", modify sheet("N")
putexcel A1 = matrix(Nbb), names
putexcel save

****************************************
// Coefficient size comparison

* Helper function refitting models to get same-sign coefs
capture program drop cs_coefs
program define cs_coefs, eclass
	syntax, DV(string) COVars(string) IVs(string) SIgns(string)
	// signs = direction of each predictor in the OG sample

	* Person id for resampled clusters (duplicates need new ids)
	local cid _bsid
	capture confirm variable _bsid
	if _rc { 	// if no existing _bsid variable
		tempvar cid
		qui egen `cid' = group(SUBID)
	}

	* One model per predictor; store sign * coef in a row vector
	local k : word count `ivs' 	// number of predictors
	tempname b
	matrix `b' = J(1, `k', .)
	local i = 0
	foreach x of local ivs { 	// loop over predictors
		local ++i
		local s : word `i' of `signs' 	// get OG sign for this predictor
		qui meglm `dv' `covars' `x' || `cid':
		matrix `b'[1, `i'] = `s' * _b[`x']  // multiply coef by OG sign & store
	}
	matrix colnames `b' = `ivs'		// predictor as column name
	ereturn post `b', obs(`=_N')
end

* Set up options for program ^
local cog_covars "c.std_age female white college std_faq std_gds15 i.source"
local mh_covars "c.std_age female white college std_faq i.source"
local reps 200
local seed 4553

* Set up results file with one row per comparison
tempname hold
tempfile res
postfile `hold' str32 outcome str40 top str40 predictor double diff se p lb ub using `res', replace

* Loop over outcomes
foreach dv in `cog_dvs' `mh_dvs' {
	display as result _n "=== `dv' ==="

	* Pick the covariates for this outcome
	local covars `cog_covars'
	if strpos(" `mh_dvs' ", " `dv' ") > 0 local covars `mh_covars' // change covars to mh if in

	******
	// Get the FDR-significant predictors (strongest first) and their directions
	preserve
	use "../Output/Coefficients/mtc_all_results.dta", clear
	keep if outcome == "`dv'" & sig_fdr == 1	// keep fdr sig predictors for this outcome
	gen abscoef = abs(coef)
	gsort -abscoef		// sort strong to weak
	local sigivs ""		
	local signs ""
	forvalues i = 1/`=_N' {
		local sigivs `sigivs' `=predictor[`i']' // local for significant vars
		local signs `signs' `=sign(coef[`i'])'	// local for sign of those
	}
	restore

	* Need at least 2 significant predictors to compare
	local k : word count `sigivs'
	if `k' < 2 {
		display as text "fewer than 2 FDR-significant predictors"
		continue
	}
	local top : word 1 of `sigivs'

	******
	// Bootstrap the sign-aligned coefficients using helper
	capture drop _bsid
	bootstrap _b, reps(`reps') seed(`seed') cluster(SUBID) idcluster(_bsid) nodots: ///
		cs_coefs, dv(`dv') covars(`covars') ivs(`sigivs') signs(`signs')

	******
	// Test strongest predictor vs. each of the others
	foreach x of local sigivs {
		if "`x'" == "`top'" continue 	// skip if this run is for strongest iv
		qui lincom `top' - `x'
		post `hold' ("`dv'") ("`top'") ("`x'") (r(estimate)) (r(se)) (r(p)) (r(lb)) (r(ub))
	}
}
postclose `hold'

* Export
preserve
use `res', clear
format diff se lb ub %9.3f
format p %9.4f
list outcome top predictor diff p, sepby(outcome) noobs abbreviate(20)
export delimited using "../Output/Coefficients/coef_size_comparison.csv", replace
restore

****************************************
** Cross-outcome tests: does predictor relate differently to cognition vs. mental health?

* Domain composites
capture drop cog_raw mh_raw std_cog_comp std_mh_comp
gen cog_raw = (std_language + std_epmem + std_moca_raw) / 3 // not including MCI bc logit
gen mh_raw  = (std_gds15_rev + std_anxiety_rev + std_happiness + std_whole) / 4
egen std_cog_comp = std(cog_raw)
egen std_mh_comp  = std(mh_raw)
drop cog_raw mh_raw

local cog_covars "c.std_age female white college std_faq std_gds15 i.source"
local mh_covars  "c.std_age female white college std_faq i.source"

* Similar program to above
capture program drop cs_cross
program define cs_cross, eclass
	syntax, DVA(string) COVA(string) DVB(string) COVB(string) IVs(string)
	local cid _bsid
	capture confirm variable _bsid
	if _rc {
		tempvar cid
		qui egen `cid' = group(SUBID)
	}
	local k : word count `ivs'
	tempname b
	matrix `b' = J(1, `k', .)
	local i = 0
	foreach x of local ivs {
		local ++i
		qui meglm `dva' `cova' `x' || `cid':
		local b1 = _b[`x']
		qui meglm `dvb' `covb' `x' || `cid':
		matrix `b'[1, `i'] = `b1' - _b[`x']   // subtract the two coefs
	}
	matrix colnames `b' = `ivs'
	ereturn post `b', obs(`=_N')
end

capture drop _bsid
bootstrap _b, reps(200) seed(4553) cluster(SUBID) idcluster(_bsid) nodots: ///
	cs_cross, dva(std_cog_comp) cova(`cog_covars') ///
	          dvb(std_mh_comp) covb(`mh_covars') ivs(`keyivs')

estimates store boot_cross

* Table
estimates restore boot_cross
tempname hold
tempfile res
postfile `hold' str40 predictor double diff se pval lb ub using `res', replace
foreach x of local keyivs {
	qui lincom `x'
	post `hold' ("`x'") (r(estimate)) (r(se)) (r(p)) (r(lb)) (r(ub))
}
postclose `hold'
preserve
use `res', clear
format diff se lb ub %9.3f
format pval %9.4f
list predictor diff se pval, noobs abbreviate(20)
export delimited using "../Output/Coefficients/cross_outcome_differences.csv", replace
restore

****************************************
// Gini coefficients

run "gini_program.do"

gini_table, part(`g_part') stress(`g_stress') bond(`g_bond') bridge(`g_bridge') ///
	file("../Output/Coefficients/gini_by_variable.csv")
