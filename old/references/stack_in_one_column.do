** Stata program for stacking a series bivarites results in one column for each DV **
** Create a program called appendmodels
** Update: Tianyao, 1/29/2025 
** Source: http://fmwww.bc.edu/RePEc/bocode/e/estout.old/advanced.html#advanced901
** Note: Run below codes all at once by clicking the excute button up right; don't select any lines


capt prog drop _all
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
