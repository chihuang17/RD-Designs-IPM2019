********************************************************************************
** The Regression Discontinuity Design -- Re-analysis of Klasnja Titiunik (2017)
** Authors: Matias D. Cattaneo, Rocio Titiunik and Gonzalo Vazquez-Bare
** Last update: 10-JUN-2019
********************************************************************************
** SOFTWARE WEBSITE: https://sites.google.com/site/rdpackages/
********************************************************************************
** TO INSTALL STATA PACKAGES:
** RDROBUST: net install rdrobust, from(https://sites.google.com/site/rdpackages/rdrobust/stata) replace
** RDDENSITY: net install rddensity, from(https://sites.google.com/site/rdpackages/rddensity/stata) replace
** LPDENSITY: net install lpdensity, from(https://sites.google.com/site/nppackages/lpdensity/stata) replace
** RDLOCRAND: net install rdlocrand, from(https://sites.google.com/site/rdpackages/rdlocrand/stata) replace
********************************************************************************
* Score:   mv_incparty (margin of victory of incumbent party at t)
* Outcome: indicator for victory of incumbent party at t+1 
* Cutoff:  0
********************************************************************************


********************************************************************************
** Load Data
********************************************************************************

* set your own directory
cd "D:\User\Document\Service\計量方法研習營\2019\RD\RD designs\Cattaneo, Titiunik & Vazquez-Bare 2019 The RDD in Sage Handbook"

use "CTV_2019_Sage.dta", clear
describe mv_incpartyfor1 mv_incparty pibpc population numpar_candidates_eff ///
  party_DEM_wonlag1_b1 party_PSDB_wonlag1_b1 party_PT_wonlag1_b1 party_PMDB_wonlag1_b1
list mv_incpartyfor1 mv_incparty in 5833/5842

global y mv_incpartyfor1 
global x mv_incparty
global covs "pibpc population numpar_candidates_eff party_DEM_wonlag1_b1 party_PSDB_wonlag1_b1 party_PT_wonlag1_b1 party_PMDB_wonlag1_b1"

* Scatter plot: often hard to see discontinuities

twoway(scatter $y $x, mcolor(black) xline(0, lcolor(black))), ///
  graphregion(color(white)) xtitle(Incumbent Party's Margin of Victory at t) ///
  ytitle(Incumbent Party's Margin of Victory at t+1)

********************************************************************************
** Falsification analysis
********************************************************************************

* Density discontinuity test

rddensity $x, plot

* Placebo tests on pre-determined covariates

foreach var of global covs {
	rdrobust `var' $x
	qui rdplot `var' $x if abs($x)<=30, graph_options(graphregion(color(white)) ///
													  xlabel(-30(10)30) ///
													  ytitle(`var'))
}


********************************************************************************
* Outcome analysis 
********************************************************************************

* RD plot 

rdplot $y $x, graph_options(graphregion(color(white)) ///
							xtitle(Margin of Victory at t) ///
							ytitle(Margin of Victory at t+1)) 

* Continuity based approach  

rdrobust $y $x 

* with covariates

rdrobust $y $x, covs($covs) 


* Local randomization approach 

* Window selector: uncomment to run. Can take a long time.

*rdwinselect $x $covs, wmin(0.05) wstep(0.01) nwindows(200) seed(765) plot graph_options(xtitle(Half window length) ytitle(Minimum p-value across all covariates) graphregion(color(white)))

* Randomization inference

local  w =  0.15
rdrandinf $y $x, wl(-`w') wr(`w') reps(1000) seed(765)

