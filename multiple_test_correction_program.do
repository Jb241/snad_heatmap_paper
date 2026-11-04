
// Programs for multiple test correction (Benjamini-Hochberg FDR)
// Created: 11/03/26
// Used by:
//	analysis_snadsnack_heatmap_paper.do

* Drop old versions so the file can be re-run without "already defined" errors
capture program drop mtc_fit
capture program drop mtc_fdr
capture program drop mtc_plot
capture program drop mtc_run
capture program drop mtc_batch
capture program drop mtc_collect

****************************************
** 1. Helper function to fit one model per predictor and store important ests.
* mtc_fit

program define mtc_fit
	syntax, DV(string) COVars(string) IVs(string) [LABMax(integer 40)]
	// dv = outcome; covars = covariates; ivs = predictors; labmax = max label length

	******
	// Set up the results file
	tempname hold
	tempfile res
	postfile `hold' str40 predictor str80 plabel double coef se pval using `res', replace
		* one row per predictor: name, plot label, coefficient, SE, p-value

	******
	// Loop over predictors
	foreach x of local ivs {
		* Status message
		display as text "  `dv' ~ `x'"
		
		******
		// Run each model and grab coef/se
		* Error if model does not converge
		capture quietly meglm `dv' `covars' `x' || SUBID:
		if _rc != 0 {
			display as error "    model failed for `x' (rc = " _rc "); skipped"
			continue
		}
		* Otherwise reset and then capture coef and se
		scalar bb = .
		scalar ss = .
		capture scalar bb = _b[`x']
		capture scalar ss = _se[`x']
		* Error if coef or se if missing (likely collinear)
		if missing(bb) | missing(ss) | ss <= 0 {	// error if coef/se dropped
			display as error "    `x' omitted or has no SE (collinear/empty); skipped"
			continue
		}

		******
		// Store the predictor's label for plotting
		local lab ""
		capture local lab : variable label `x'
		if `"`lab'"' == "" local lab "`x'"	// if no label, use the variable name
		}
		
		******
		* Write this predictor's row (incl p-value from a two-sided z-test)
		post `hold' ("`x'") (`"`lab'"') (bb) (ss) (2*normal(-abs(bb/ss)))
	}
	postclose `hold' // close the results file
	use `res', clear // load the results into memory
	drop if missing(pval) // keep only predictors that estimated
end

****************************************
** 2. Helper function to perform Benjamini-Hochberg correction.
* mtc_fdr

program define mtc_fdr, rclass
	syntax, [Q(real 0.05)] // q = FDR level (default to 5%)
	local m = _N // number of tests is number of rows

	******
	// BH q-values
	* Order rows from smallest to largest p
	egen rank = rank(pval), unique
	sort rank
	* Perform BH adjustment (p * m divided by rank with 1 at most)
	gen double qval = min(1, pval * `m' / rank)
	* Ensure q never exceeds the q of a larger p
	gsort -rank	// order from largest to smallest p
	replace qval = min(qval, qval[_n-1]) if _n > 1	// 
	sort rank

	******
	// Significance flags
	gen byte sig_raw = pval <= 0.05 // significant without correction
	gen byte sig_fdr = qval <= `q' // significant after correction

	******
	// Summary numbers
	* Get the largest p-value of FDR-significant results as threshold
	qui summarize pval if sig_fdr == 1, meanonly
	local thr = r(max)
	* Number of originally significant tests
	qui count if sig_raw == 1
	local n_raw = r(N)
	* Number of corrected tests that are significant
	qui count if sig_fdr == 1
	local n_fdr = r(N)
	* Output
	return scalar thr = `thr'
	return scalar m = `m'
	return scalar n_raw = `n_raw'
	return scalar n_fdr = `n_fdr'
end

****************************************
** 3. Helper function to create the plot.
* mtc_plot

program define mtc_plot
	syntax, NAme(string) TItle(string) [THR(string) LEGEND]
	// name = outcome; title = panel title; thr = FDR p-threshold; legend = show the group legend
	
	* Define text position, just to the right of the data
	qui summarize coef
	local xtxt = r(max) + 0.2

	******
	// FDR threshold line (or a note if nothing survives the correction)
	local thrline ""
	* If there was 1+ significant fdr for threshold, create line/text label
	if "`thr'" != "." & "`thr'" != "" {
		local thrfmt : display %7.5f `thr'
		local thrline `"yline(`thr', lcolor(gs10) lpattern(shortdash))"'
		local thrtext `"text(`thr' `xtxt' "FDR p = `thrfmt'", size(small) color(gs3) placement(east))"'
	}
	* If there were 0 significant fdr tests, then note instead
	else {
		local thrtext `"text(0.009 `xtxt' "No FDR-significant results", size(small) color(gs3) placement(east))"'
	}

	******
	// One layer per predictor group
	* Set appearance by group
	local colors  blue magenta stc3 orange // colors
	local symbols X triangle square circle	// shapes
	local sizes large small small small	// sizes
	* Create list of formulated layers by group
	forvalues g = 1/4 {
		local c : word `g' of `colors'
		local s : word `g' of `symbols'
		local z : word `g' of `sizes'
		local plots `plots' (scatter pval coef if group == `g', ///
								mcolor(`c') msymbol(`s') msize(`z') ///
								mlabel(plabel) mlabcolor(`c') ///
								mlabsize(vsmall))
	}

	******
	// Only show legend when requested (usually for just one of the facets)
	if "`legend'" == "" local leg "legend(off)"
	else local leg `"legend(order(1 "Participation" 2 "Stressors" 3 "Bonding" 4 "Bridging") row(1) size(small))"'

	******
	// Create the plot
	twoway `plots', ///
		xtitle("Standardized coefficient", size(small)) ytitle("") ///
		yscale(log reverse) ///
		ylabel(1 0.1 0.01 0.001 0.0001, angle(0) nogrid notick) ///
		yline(0.05, lcolor(gs10) lpattern(shortdash)) `thrline' ///
		xline(0, lcolor(gs10) lpattern(shortdash)) ///
		xlabel(, angle(0) nogrid notick) ///
		text(0.06 `xtxt' "p = 0.05", size(small) color(gs3) placement(east)) `thrtext' ///
		xsize(5.5) plotregion(margin(0 30 2 8)) ///
		graphregion(color(white) lstyle(none)) plotregion(color(white) lstyle(none)) ///
		title("`title'", size(small)) `leg' ///
		name(mtc_`name', replace) scheme(s1color)

	graph export "../Output/Figures/mtc_`name'.png", as(png) ///
		name(mtc_`name') replace width(2400)
end

****************************************
** 4. Performs correction pipeline for one outcome from start to finish.
		* fit -> correct -> save -> plot
* mtc_run

program define mtc_run
	syntax, DV(string) COVars(string) IVs(string) TItle(string) ///
		PARt(string) STRess(string) BONd(string) BRIdge(string) ///
		[Q(real 0.05) LABMax(integer 40) LEGEND]
	// part/stress/bond/bridge = lists of predictors in each group
	preserve
	display as result _n "=== `dv' ==="

	******
	// Use helpers 1/2 to create models and apply correction
	mtc_fit, dv(`dv') covars(`covars') ivs(`ivs') labmax(`labmax')
	mtc_fdr, q(`q')
	local thr = r(thr)
	display as result "`dv': m = " r(m) "; p<.05 = " r(n_raw) "; BH-FDR (q=`q') = " r(n_fdr) "; p threshold = `thr'"

	******
	// Define predictor groups
	gen group = .
	local i = 0
	foreach g in part stress bond bridge {
		local ++i
		foreach v of local `g' {
			replace group = `i' if predictor == "`v'"
		}
	}
	label define grp 1 "Participation" 2 "Stressors" 3 "Bonding" 4 "Bridging", replace
	label values group grp

	******
	// Identify and save this outcome's results
	gen str32 outcome = "`dv'"
	gen str60 outcome_title = `"`title'"'
	order outcome outcome_title group predictor plabel coef se pval qval sig_raw sig_fdr
	save "../Output/Coefficients/mtc_`dv'.dta", replace

	******
	// Plot
	mtc_plot, name(`dv') title(`"`title'"') thr(`thr') `legend'

	restore
end

****************************************
** 5. Performs correction pipeline for multiple outcome from start to finish.
		* uses mtc_run
* mtc_batch


program define mtc_batch
	syntax, DVs(string) TItles(string asis) COVars(string) IVs(string) ///
		GRPopts(string) STUb(string) MAINtitle(string) [Q(real 0.05)]
		// stub() = used in the combined file name
	local n : word count `dvs' // number of outcomes
	local i = 0
	local gnames // holds the list of plot names

	******
	// Loop over outcomes
	foreach dv of local dvs {
		local ++i
		local ttl : word `i' of `titles'
		local leg ""
		if `i' == `n' local leg "legend" // legend only on the last panel
		mtc_run, dv(`dv') covars(`covars') ivs(`ivs') title(`"`ttl'"') ///
			`grpopts' q(`q') `leg'
		local gnames `gnames' mtc_`dv'
	}

	******
	// Combined figure
	graph combine `gnames', ///
		ycommon xcommon commonscheme col(2) imargin(2 2 2 2) iscale(0.7) ///
		graphregion(color(white) lstyle(none)) plotregion(color(white) lstyle(none)) ///
		title("`maintitle'", size(small)) ///
		note("FDR = Benjamini-Hochberg false discovery rate, q = `q', applied within outcome", size(tiny))
	graph export "../Output/Figures/mtc_`stub'_combined.png", as(png) replace width(3200)
end

****************************************
** 6. Stack every result into a file, builds a summary table, and deletes temporary files.
* mtc_collect

program define mtc_collect
	syntax, DVs(string)
	preserve
	clear

	******
	// Stack all outcomes
	foreach dv of local dvs {
		append using "../Output/Coefficients/mtc_`dv'.dta"	// add this outcome's rows
		erase "../Output/Coefficients/mtc_`dv'.dta"	// remove the temporary file
	}
	save "../Output/Coefficients/mtc_all_results.dta", replace
	export delimited using "../Output/Coefficients/mtc_all_results.csv", replace

	******
	// Summary table
	gen seq = _n
	gen pthr = pval if sig_fdr == 1	// p-values of FDR-significant results only
	* Get summaries (# tests, # p sig, # fdr sig, p threshold)
	collapse (min) seq (first) outcome_title (count) n_tests = pval ///
		(sum) n_p05 = sig_raw n_fdr = sig_fdr (max) p_threshold = pthr, by(outcome)
	sort seq
	drop seq
	format p_threshold %8.5f
	* Show the table then export
	list, noobs sep(0) abbreviate(20)
	export delimited using "../Output/Coefficients/mtc_summary.csv", replace

	restore
end
