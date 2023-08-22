# MIIVefa

A quick tutotial for using the MIIVefa package in R.

MIIVefa uses Model Implied Instrumental Variables (MIIVs) to perform Exploratory Factor Analysis (EFA). 

# The Basics

1, Please manually read in all the functions in 'EFAmiiv_R/R/' besides 'for testing.R.' 

2, The list of all functions (only EFAmiiv is used by users, all the remaining are internal):

- r2_order.R: calculate the R2 value of each variable when all other variables are regressed on it. For example, r1 for x1 is calculated as x1 ~ x2 + x3 + x4 + x5 if x1-x5 are all the variables in the data.

- getbadvar.R: find 'bad variables' in the initial one factor model, which are defined as having either non-significant factor loading or significant sargan, or both.

- getbadvar_multi.R: find bad variables when multiple factors exist.

- crossloadcheck.R: checks if any bad variable can cross load on any other factor.

- getsigsarganNnonsigloading.R: used when selecting scaling indicators, similar to getbadvar_multi but has different output format.

- select_scalingind.R: selects the scaling indicator for the initial one factor model.

- select_scalingind_stepN.R: selects the scaling indicator when more than one factor exist.

- step1_EFAmiiv.R: creates the first one factor model and evaluates.

- stepN_EFAmiiv.R: used to evaluate model fit when multiple factor exists.

- EFAmiiv.R: the function used by users, outputs suggested model and model fit.

3, After reading in all functions, use the 'EFAmiiv' function to perform EFA. sigLevel is the significance level default at .05 and can be specified differently,
scalingCrit is the criterion(a) used to select the scaling indicator and default is 'sargan+factorloading_R2,' more details documentation can be found in the
'select_scalingind.R' file.

                    EFAmiiv <- function(data,
 
                    sigLevel = .05,
                    
                    scalingCrit = "sargan+factorloading_R2",
                    
                    correlatedErrors = NULL)
                    
             
  

  
