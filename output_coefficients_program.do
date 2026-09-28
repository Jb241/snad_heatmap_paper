** Stata program for stacking a series bivarites results in one column for each DV **
** Create a program called appendmodels
** Update: Tianyao, 1/29/2025 
** Source: http://fmwww.bc.edu/RePEc/bocode/e/estout.old/advanced.html#advanced901
** Note: Run below codes all at once by clicking the excute button up right; don't select any lines
** ----------------------------------------------------------
** Update: Jess, 9/28/26
** Note: Added run_heatmap program to aid efficient matrix exportation into R

capt prog drop _all

*------------------------------------------------------------
* appendmodels: stacks the first equation of several models
*------------------------------------------------------------
program appendmodels, eclass
    // using first equation of model
    syntax namelist
    tempname b V tmp
    foreach name of local namelist {
        qui est restore `name'
        mat `tmp' = e(b)
        local eq1: coleq `tmp'
        gettoken eq1 : eq1
        mat `tmp' = `tmp'[1,"`eq1':"]
        local cons = colnumb(`tmp',"_cons")
        if `cons'<. & `cons'>1 {
            mat `tmp' = `tmp'[1,1..`cons'-1]
        }
        mat `b' = nullmat(`b') , `tmp'
        mat `tmp' = e(V)
        mat `tmp' = `tmp'["`eq1':","`eq1':"]
        if `cons'<. & `cons'>1 {
            mat `tmp' = `tmp'[1..`cons'-1,1..`cons'-1]
        }
        capt confirm matrix `V'
        if _rc {
            mat `V' = `tmp'
        }
        else {
            mat `V' = ( ///
                `V' , J(rowsof(`V'), colsof(`tmp'), 0) \ ///
                J(rowsof(`tmp'), colsof(`V'), 0) , `tmp' ///
            )
        }
    }
    local names: colfullnames `b'
    mat coln `V' = `names'
    mat rown `V' = `names'
    eret post `b' `V'
    eret local cmd "whatever"
end

*------------------------------------------------------------
* run_heatmap: runs one model per IV for each DV, stacks each
* DV into a column, and exports DVs side by side to a CSV
* (coefficients with stars only, no SEs or t-stats)
*------------------------------------------------------------
capture program drop run_heatmap
program run_heatmap
	syntax, dvs(string) covars(string) ivs(string) drop(string) file(string)
	local cols
	local i = 0
	foreach dv of local dvs {
		local ++i
		display as text "Running models for `dv'..."
		local mods
		foreach x of local ivs {
			qui meglm `dv' `covars' `x' || SUBID:
			qui est store m_`x'
			local mods `mods' m_`x'
		}
		appendmodels `mods'
		qui est store col`i'
		qui est drop m_*
		local cols `cols' col`i'
	}
	esttab `cols' using "`file'", replace ///
		cells(b(star fmt(%9.2f))) star(+ 0.10 * 0.05 ** 0.01 *** 0.001) ///
		drop(`drop') mtitles(`dvs') ///
		collabels(none) nonumbers noobs nonotes
	est drop col*
end
