* File info ********************************************************************

* File:    Restricted LA-EASI (based on Lewbel & Pendakur, 2009)
* Authors: Charlotte Plinke & Michael Sureth
* Paper:   Assessing the Potential of Tax Policies in Reducing Environmental 
*          Impacts from European Food Consumption


************************ Preparation and settings ******************************

* load settings
cd path_placeholder
do "config.do"
set more off

* set required matrix size
global matsize_value=500+${nprice}*(1+${npowers}+${ndem}+${nprice}*(1+${ndem}+1)+${ndem})
di "$matsize_value"
set matsize $matsize_value

* load data 
insheet using "data_temp.csv", clear

* extract country
split hh_id, parse("_") limit(1) gen(country)
global country = country[1]

* Household characteristics should be numeric (normalized) or dummies
destring z* , ignore("NA") replace


* (1) N-1 equations, parameters are recovered from theoretical restrictions ----
if ("$equations" == "nmin1"){

* define which categories to include vs. residual category
numlist "1/$nprice"
local all `r(numlist)'
numlist $p_to_norm
local residcat `r(numlist)'
global include: list all - residcat
di "$include"

* normalised prices (normalized using config$pnorm) enter the demand system
destring nlogp* , ignore("NA") replace
}

* (2) N equations, adding-up restriction ignored ------
else if ("$equations" == "alln"){

* normalised prices (normalized using total food expenditures) enter the demand system
* destring nfoodexplogp* , ignore("NA") replace
* rename normalized prices such that they can be used in setting up equations
drop nlogp*
rename nfoodexplogp* nlogp*
destring nlogp* , ignore("NA") replace

* define which categories to include 
numlist "1/$nprice"
local all `r(numlist)'
global include: list uniq all 

* set p_to_norm to 0
global p_to_norm_KEEP $p_to_norm
global p_to_norm 0
}

* define which demographic variables to include
numlist "1/$ndem"
local all `r(numlist)'
if("$country" == "IT"){
	local noinc 4
	global include_z: list all - noinc
} 
else {
	global include_z: list uniq all 
}

************************ SET UP EQUATIONS **************************************

* Price of goods and services in the same category as well as on the prices of other categories --> $nplist
global nplist ""
foreach j of numlist $include {
	global nplist "$nplist nlogp`j'"
}

foreach i of numlist $include {

	* Dummy variables controlling for household characteristics --> $zlist
	global zlist`i' ""
	foreach k of numlist $include_z {
		global zlist`i' "${zlist`i'} z`k'`i'"
	}

	* Interaction terms between prices and household characteristics --> $npzlist
	global npzlist`i' ""
	if("$pzint" == "pzintyes"){
		foreach j of numlist $include {
			foreach k of numlist $include_z {

				g int_nlogp`j'z`k'`i'=nlogp`j'*z`k'`i'
				global npzlist`i' "${npzlist`i'} int_nlogp`j'z`k'`i'"	
			}
		}
	}

	* Real total expenditures (and polynomials)  --> $ylist
	global ylist`i' ""
	forvalues j=1(1)$npowers {
		global ylist`i' "${ylist`i'} log_y`j'`i'"
	}
	di "${ylist`i'}"

	* Interaction terms between total expenditures and household characteristics --> $yzlist
	global yzlist`i' ""
	foreach k of numlist $include_z {
		g int_logy1`i'z`k'`i'=log_y1`i'*z`k'`i'
		global yzlist`i' "${yzlist`i'} int_logy1`i'z`k'`i'"
	}
	di "${yzlist`i'}"
	
	* Interaction terms between total expenditures and prices  --> $ynplist
	global ynplist`i' ""
	foreach j of numlist $include {
		g int_logy1`i'nlogp`j'=log_y1`i'*nlogp`j'
		global ynplist`i' "${ynplist`i'} int_logy1`i'nlogp`j'"
	}
	di "${ynplist`i'}"
}
	
	
* Set up equations and put into a list
* "uncensored" - without adjustment for censored distribution
global eqlist ""
foreach i of numlist $include {
	global eq`i' "(s`i' ${ylist`i'} ${zlist`i'} ${yzlist`i'} ${nplist} ${npzlist`i'} ${ynplist`i'})"
	macro list eq`i'
	global eqlist "$eqlist \$eq`i'"
}

* "censored" - with adjustment for censored distribution –-> 
global eqlist_censored ""
foreach i of numlist $include {
	global eqc`i' "(s`i' ${ylist`i'} ${zlist`i'} ${yzlist`i'} ${nplist} ${npzlist`i'} ${ynplist`i'} pdf`i')"
	macro list eqc`i'
	global eqlist_censored "${eqlist_censored} \$eqc`i'"
}
di "$eqlist_censored"



************************ SET UP CONSTRAINTS ************************************

* Create linear constraints (p) and put them into a list called conlist
constraint drop _all
global conlist ""
local i = 0
foreach j of numlist $include {
	local jplus1=`j'+1
	forvalues h=`jplus1'(1)$nprice {
		if `h' != $p_to_norm {
			local i = `i'+1
			constraint `i' [s`j']nlogp`h'=[s`h']nlogp`j'
			global conlist "$conlist `i'"
		}
	}
}
di "$conlist" 

* add constraints for yp interactions
foreach j of numlist $include {
	local jplus1=`j'+1
	forvalues h=`jplus1'(1)$nprice {
		if `h' != $p_to_norm {
			local i = `i'+1
			constraint `i' [s`j']int_logy1`j'nlogp`h'=[s`h']int_logy1`h'nlogp`j'	
			global conlist "$conlist `i'"
		}
	}
}
di "$conlist"

* add constraints for pz interactions
if("$pzint" == "pzintyes"){
	foreach k of numlist $include_z {
		foreach j of numlist $include {
			local jplus1=`j'+1
			forvalues h=`jplus1'(1)$nprice {
				if `h' != $p_to_norm {
					local i = `i'+1
					constraint `i' [s`j']int_nlogp`h'z`k'`j'=[s`h']int_nlogp`j'z`k'`h'
					global conlist "$conlist `i'"
				}
			}
		}
	}
}
di "$conlist" 

constraint list



***************************** LA - EASI ****************************************

************** restricted
if ("$weight_yesno" == "unweighted") {
	if "$cens_yesno" == "uncensored" {
		sureg $eqlist, constr($conlist)
	}
	else if "$cens_yesno" == "censored" {
		sureg $eqlist_censored, constr($conlist)
	}
	else error
}
else if ("$weight_yesno" == "weighted") {
	if "$cens_yesno" == "uncensored" {
		reg3 $eqlist [aweight=hh_wgt], constr($conlist)
	}
	else if "$cens_yesno" == "censored" {
		reg3 ${eqlist_censored} [aweight=hh_wgt], constr($conlist) 
	}
	else error
} 
else error


* store estimated coefficients in variables
foreach i of numlist $include {
	gen cons_restr_`i' = _b[s`i':_cons]
	
	if("$cens_yesno" == "censored"){
	gen f_restr_`i' = _b[s`i':pdf`i']
	}
	else if("$cens_yesno" == "uncensored"){
	gen f_restr_`i' = 0
	}

	foreach r of numlist 1(1)$npowers{
		gen br`r'_restr_`i' = _b[s`i':log_y`r'`i']
	}
	
	foreach j of numlist $include {
		gen a_restr_i`i'j`j' = _b[s`i':nlogp`j']
		gen b_restr_i`i'j`j'  = _b[s`i':int_logy1`i'nlogp`j']
	}

	foreach k of numlist $include_z {
		gen c_restr_`i'_z`k' = _b[s`i':z`k'`i']
		gen d_restr_`i'_z`k' = _b[s`i':int_logy1`i'z`k'`i']
		if("$pzint" == "pzintyes"){
			foreach j of numlist $include {
				gen e_restr_i`i'j`j'_z`k' = _b[s`i':int_nlogp`j'z`k'`i']
			}
		}
	}
}

if ("$equations" == "nmin1"){

* Recover parameters from restrictions (i.e., adding up and homogeneity restriction)

* br parameters must sum to zero for all r from 1 to R
forvalues r=1(1)$npowers{
	gen br`r'_help = 0
	foreach i of numlist $include {
		replace br`r'_help=br`r'_help+ br`r'_restr_`i'
	}
	gen br`r'_restr_$p_to_norm = br`r'_help*(-1)
	drop br`r'_help
}

* a parameters (p-coefficients) must sum to zero
foreach i of numlist $include {
	gen a_help_`i' = 0
	foreach j of numlist $include {
		replace a_help_`i'=a_help_`i' + a_restr_i`i'j`j'
	}
	gen a_restr_i`i'j$p_to_norm = a_help_`i'*(-1)
	
	* symmetry
	gen a_restr_i${p_to_norm}j`i' = a_restr_i`i'j$p_to_norm
	drop a_help_`i'
}

gen a_help_${p_to_norm} = 0
foreach j of numlist $include {
	replace a_help_${p_to_norm}=a_help_${p_to_norm} + a_restr_i${p_to_norm}j`j'
}
gen a_restr_i${p_to_norm}j$p_to_norm = a_help_${p_to_norm}*(-1)
drop a_help_${p_to_norm}


* b parameters (py-coefficients) must sum to zero
foreach i of numlist $include {
	gen b_help_`i' = 0
	foreach j of numlist $include {
		replace b_help_`i'=b_help_`i' + b_restr_i`i'j`j'
	}
	gen b_restr_i`i'j$p_to_norm = b_help_`i'*(-1)
	gen b_restr_i${p_to_norm}j`i' = b_restr_i`i'j$p_to_norm
	drop b_help_`i'
}

gen b_help_${p_to_norm} = 0
foreach j of numlist $include {
	replace b_help_${p_to_norm}=b_help_${p_to_norm} + b_restr_i${p_to_norm}j`j'
}
gen b_restr_i${p_to_norm}j$p_to_norm = b_help_${p_to_norm}*(-1)
drop b_help_${p_to_norm}


* c parameters (z-coefficients) must sum to zero
foreach k of numlist $include_z {
	gen c_help_z`k'=0
	foreach i of numlist $include {
		replace c_help_z`k'=c_help_z`k' + c_restr_`i'_z`k'
	}
	gen c_restr_${p_to_norm}_z`k' = c_help_z`k'*(-1)
	drop c_help_z`k'
}

* d parameters (zy-coefficients) must sum to zero
foreach k of numlist $include_z {
	gen d_help_z`k'=0
	foreach i of numlist $include {
		replace d_help_z`k'=d_help_z`k' + d_restr_`i'_z`k'
	}
	gen d_restr_${p_to_norm}_z`k' = d_help_z`k'*(-1)
	drop d_help_z`k'
}

* e parameters (pz-coefficients) must sum to zero 
if("$pzint" == "pzintyes"){
foreach k of numlist $include_z {
	foreach i of numlist $include {
		gen e_help_`i'_`k' = 0
		foreach j of numlist $include {
			replace e_help_`i'_`k'=e_help_`i'_`k' + e_restr_i`i'j`j'_z`k'
		}
		gen e_restr_i`i'j${p_to_norm}_z`k' = e_help_`i'_`k'*(-1)
		
		* symmetry
		gen e_restr_i${p_to_norm}j`i'_z`k' = e_restr_i`i'j${p_to_norm}_z`k'
		drop e_help_`i'_`k'
	}
	
	gen e_help_${p_to_norm}_`k' = 0
	foreach j of numlist $include {
		replace e_help_${p_to_norm}_`k'=e_help_${p_to_norm}_`k' + e_restr_i${p_to_norm}j`j'_z`k'
	}
	gen e_restr_i${p_to_norm}j${p_to_norm}_z`k' = e_help_${p_to_norm}_`k'*(-1)
	drop e_help_${p_to_norm}_`k'
}
}

* f parameter for residual category
gen f_restr_${p_to_norm} = 0

* cons for residual category
gen cons_restr_${p_to_norm} = 0
}

if("$pzint" != "pzintyes"){
gen e_restr_NA = 0
}


* reload settings for p_to_norm in case of alln
else if ("$equations" == "alln"){
global p_to_norm $p_to_norm_KEEP
}

* export estimated and recovered parameter values
qui estpost summarize br* a_restr* b_restr* c_restr* d_restr* e_restr* f_restr* cons_restr*
esttab using "coefs.csv", cells("mean") replace plain

sleep 100

******************************** End of file ***********************************