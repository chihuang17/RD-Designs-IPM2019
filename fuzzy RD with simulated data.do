******************************************************************************* 
* Illustrate two-sided fuzzy RD design and analysis with simulated data
/* Install CCFT's (2017) rdrobust program */
** RDROBUST: net install rdrobust, from(https://sites.google.com/site/rdpackages/rdrobust/stata) replace
*******************************************************************************

**********************************************************
** Data Generation Precess (DGP) of Simulated Data
**********************************************************

mat c=(1,.5\.5,1)

set seed 135790

drawnorm e pretest, n(10000) corr(c) clear

gen x=pretest-0 // x is the score (or running/forcing variable)

gen z=x>0   // z is exogenous assignment (above-the-cutoff indicator) 

gen d=cond(uniform()<=.8,z,1-z) // d is actually treated indicator

/* define y with potential outcomes y1 and y0 */
gen y1=1+x-x^2+e 
gen y0=0+x-x^2+e
gen y=y0+d*(y1-y0)

/* define triangular kernel weight */
gen w=max(0,1-abs(x)) 

/* Show 2-sided noncompliance: z != d */
list x z d y in 1/20  // note the relationship between z and d
tab z d, row cell

***************************************************
** Plot the treatment-receipt probability discontinuity
***************************************************

rdplot d x, c(0) nbins(10000) p(1)

***************************************************
** Plot the outcome v. the score 
***************************************************

rdplot y x, c(0) nbins(10000) 

***************************************************
** estimate CACE of 2-sided fuzzy RD 
***************************************************

/* IV Version, see Wooldridge 2010, p.958 */
ivregress 2sls y (d=i.z) x c.x#i.z [pw=w], first

/* FRD Version with rdrobust */
rdrobust y x, fuzzy(d) kernel(triangular) h(1) bwselect(mserd)
*The "conventional" coefficient of the Treatment effect estimates 
* is the FRD treatment effect.
/* Plot the point estimate */
rdplot y x if -e(h_l)<= x & x <= e(h_r), binselect(esmv) kernel(triangular) h(1) p(1) ///
  graph_options(title("Two-Sided Fuzzy RD Plot: Simulated Data") ///
                ytitle(Outcome Y) xtitle(Running Variable X))

/* What if we let rdrobust do the data-driven decisions on h and p? */
rdrobust y x if abs(x)<=3.5, fuzzy(d)
/* Plot this point estimate */
rdplot y x if -e(h_l)<= x & x <= e(h_r), binselect(esmv) kernel(triangular) ///
  graph_options(title("Two-Sided Fuzzy RD Plot: Data-Driven") ///
                ytitle(Outcome Y) xtitle(Running Variable X))

/* ssc install ted */
* complier probability derivative (CPD) & treatment effect derivative (TED)
ted y x d, model(fuzzy) h(0.467) c(0) m(2) l(1) k(triangular) graph				
