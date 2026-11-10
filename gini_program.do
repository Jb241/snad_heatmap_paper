// Programs for Gini coefficients (distribution of predictors)
// Created: 11/10/26
// Used by:
//	analysis_snadsnack_heatmap_paper.do

* Drop old versions so the file can be re-run without "already defined" errors
capture program drop gini_calc
capture program drop gini_table

****************************************
** 1. Helper function: Gini, top-10% share, and bottom-50% share for one variable.
* gini_calc

program define gini_calc, rclass
	syntax varname [if] [, SHIFT]
	// shift = if the variable has negative values, shift so its minimum is 0
	// (Gini needs non-negative values; flagged in r(shifted))
	marksample touse
	tempvar x r tx
	qui gen double `x' = `varlist' if `touse'
	qui summarize `x'
	local n    = r(N)
	local mn   = r(min)
	local mean = r(mean)
	local gini .
	local top10 .
	local bot50 .
	local shifted 0
	local binary 0

	if `n' >= 2 {
		* 0/1 variables: Gini just reflects prevalence
		qui count if !inlist(`x', 0, 1) & !missing(`x')
		local binary = (r(N) == 0)

		local ok 1
		if `mn' < 0 {
			if "`shift'" == "" local ok 0
			else {
				qui replace `x' = `x' - `mn'
				local shifted 1
			}
		}
		if `ok' {
			qui summarize `x'
			local tot = r(sum)
			if `tot' > 0 {
				qui egen double `r' = rank(`x') if !missing(`x'), unique
				qui gen double `tx' = (2*`r' - `n' - 1) * `x'
				qui summarize `tx'
				local gini = r(sum) / (`n' * `tot')      // population Gini
				qui summarize `x' if `r' > 0.9 * `n'
				local top10 = r(sum) / `tot'             // share held by top 10%
				qui summarize `x' if `r' <= 0.5 * `n'
				local bot50 = r(sum) / `tot'             // share held by bottom 50%
			}
		}
	}
	return scalar n       = `n'
	return scalar mean    = `mean'
	return scalar gini    = `gini'
	return scalar top10   = `top10'
	return scalar bot50   = `bot50'
	return scalar shifted = `shifted'
	return scalar binary  = `binary'
end

****************************************
** 2. Runs gini_calc on every predictor in each group, for pooled/SNAD/SNACK,
**    and exports one results file.
* gini_table

program define gini_table
	syntax, PARt(string) STRess(string) BONd(string) BRIdge(string) FILE(string) ///
		[WAVE(integer 1)]
	// part/stress/bond/bridge = predictor lists (std_ names) for each group
	// wave = which wave to use so each person counts once (default 1)
	// file = csv path for results

	local grps part stress bond bridge
	local glabs Participation Stressors Bonding Bridging

	tempname hold
	tempfile res
	postfile `hold' str8 subsample str14 grp str32 variable str80 vlabel ///
		double n mean gini top10 bot50 shifted binary using `res', replace

	foreach s in 1 2 3 {
		if `s' == 1 {
			local sname "Pooled"
			local cond "time == `wave'"
		}
		if `s' == 2 {
			local sname "SNAD"
			local cond "time == `wave' & source == 1"
		}
		if `s' == 3 {
			local sname "SNACK"
			local cond "time == `wave' & source == 2"
		}

		local gi = 0
		foreach g of local grps {
			local ++gi
			local glab : word `gi' of `glabs'
			foreach x of local `g' {
				local v = subinstr("`x'", "std_", "", 1)   // raw (unstandardized) name
				local lab : variable label `v'
				qui gini_calc `v' if `cond', shift
				post `hold' ("`sname'") ("`glab'") ("`v'") (`"`lab'"') ///
					(r(n)) (r(mean)) (r(gini)) (r(top10)) (r(bot50)) ///
					(r(shifted)) (r(binary))
			}
		}
	}
	postclose `hold'

	preserve
	use `res', clear
	format mean gini top10 bot50 %9.3f
	format n %9.0fc
	export delimited using "`file'", replace
	list grp variable n gini top10 bot50 shifted binary if subsample == "Pooled", ///
		sepby(grp) noobs abbreviate(20)
	restore
end
