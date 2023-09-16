#' @name select_scalingind
#' @title Exploratory Factor Analysis Using Model Implied Instrumental Variables.
#' @description This function selects a scaling indicator for an initial one factor model given a raw data frame matrix.
#' @usage
#'select_scalingind(data,
#'                  sigLevel = .05,
#'                  scalingCrit = "sargan+factorloading_R2",
#'                  correlatedErrors = NULL)
#' @param data A data frame, list or environment or an object coercible by as.data.frame to data frame.
#' The most common application is to supply a data.frame.
#' @param sigLevel The significance level threshold, default is .05.
#' @param scalingCrit The criterion used to select the scaling indicators, default is 'sargan+factorloading_R2.'
#' @param correlatedErrors The pairs of variables whose errors should be correlated in the model search procedure, default is NULL.
#' @import MIIVsem
#' @details
#' \itemize{
#' \item{\code{All possible criteria for scalingCrit}}{
#'
#' order: uses the first appearing variable as the scaling indicator
#'
#' sargan: uses the variable with the least number of significant sargans as the scaling indicator.
#' if multiple variables have the same least number of significant sargans, chooses the first appearing variable.
#'
#' R2: uses the variable with the highest R2 as the scaling indicator.
#' if multiple variables have the same R2 value, chooses the first appearing variable.
#'
#' factor loading: uses the variable with the most number of significant factor loadings as the scaling indicator.
#' if multiple variables have the same most number of significant factor loadings, chooses the first appearing variable.
#'
#' sargan_R2: uses the variable with the least number of significant sargans as the scaling indicator.
#' if multiple variables have the same least number of significant sargans, chooses the one with higher R2.
#' if still multiple options, chooses the first appearing variable.
#'
#' sargan_factorloading: uses the variable with the least number of significant sargans as the scaling indicator.
#' if multiple variables have the same least number of significant sargans, chooses the one with more significant factor loadings.
#' if still multiple options, chooses the first appearing variable.
#'
#' sargan_factorloading_R2: uses the variable with the least number of significant sargans as the scaling indicator.
#' if multiple variables have the same least number of significant sargans, chooses the one with more significant factor loadings.
#' if still multiple options, chooses the one with higher R2.
#'
#' factorloading_R2: uses the variable with the most number of significant factor loadings as the scaling indicator.
#' if multiple variables have the same nist number of significant factor loadings, chooses the one with higher R2.
#' if still multiple options, chooses the first appearing variable.
#'
#' factorloading_sargan: uses the variable with the most number of significant factor loadings as the scaling indicator.
#' if multiple variables have the same nist number of significant factor loadings, chooses the one with less significant sargans.
#' if still multiple options, chooses the first appearing variable.
#'
#' factorloading_sargan_R2: uses the variable with the most number of significant factor loadings as the scaling indicator.
#' if multiple variables have the same nist number of significant factor loadings, chooses the one with less significant sargans.
#' if still multiple options, chooses the one with higher R2.
#'
#' sargan+factorloading: uses the variable with the least sum of significant sargans and non-signficant factor loadings.
#' if multiple variables have the same least sum of significnat sargans and non-significant factor loadings, chooses the first appearing variable.
#'
#' sargan+factorloading_R2: uses the variable with the least sum of significant sargans and non-signficant factor loadings.
#' if multiple variables have the same least sum of significnat sargans and non-significant factor loadings, chooses the one with higher R2.

#' }
#' }
#' @return A vector indicating the variable (column name) that is suggested to be the scaling indicator for the initial one factor model.
#' @export

#########scaling indicator selection########
#scaling indicator selection criterion

#1, order: uses the first appearing variable as the scaling indicator

#2, sargan: uses the variable with the least number of significant sargans as the scaling indicator.
#if multiple variables have the same least number of significant sargans, chooses the first appearing variable.

#3, R2: uses the variable with the highest R2 as the scaling indicator.
#if multiple variables have the same R2 value, chooses the first appearing variable.

#4, factor loading: uses the variable with the most number of significant factor loadings as the scaling indicator.
#if multiple variables have the same most number of significant factor loadings, chooses the first appearing variable.

#5, sargan_R2: uses the variable with the least number of significant sargans as the scaling indicator.
#if multiple variables have the same least number of significant sargans, chooses the one with higher R2.
#if still multiple options, chooses the first appearing variable.

#6, sargan_factorloading: uses the variable with the least number of significant sargans as the scaling indicator.
#if multiple variables have the same least number of significant sargans, chooses the one with more significant factor loadings.
#if still multiple options, chooses the first appearing variable.

#7, sargan_factorloading_R2: uses the variable with the least number of significant sargans as the scaling indicator.
#if multiple variables have the same least number of significant sargans, chooses the one with more significant factor loadings.
#if still multiple options, chooses the one with higher R2.

#8, factorloading_R2: uses the variable with the most number of significant factor loadings as the scaling indicator.
#if multiple variables have the same nist number of significant factor loadings, chooses the one with higher R2.
#if still multiple options, chooses the first appearing variable.

#9, factorloading_sargan: uses the variable with the most number of significant factor loadings as the scaling indicator.
#if multiple variables have the same nist number of significant factor loadings, chooses the one with less significant sargans.
#if still multiple options, chooses the first appearing variable.

#10, factorloading_sargan_R2: uses the variable with the most number of significant factor loadings as the scaling indicator.
#if multiple variables have the same nist number of significant factor loadings, chooses the one with less significant sargans.
#if still multiple options, chooses the one with higher R2.

#11, sargan+factorloading: uses the variable with the least sum of significant sargans and non-signficant factor loadings.
#if multiple variables have the same least sum of significnat sargans and non-significant factor loadings, chooses the first appearing variable.

#12, sargan+factorloading_R2: uses the variable with the least sum of significant sargans and non-signficant factor loadings.
#if multiple variables have the same least sum of significnat sargans and non-significant factor loadings, chooses the one with higher R2.



select_scalingind <- function(data,
                              sigLevel = .05,
                              scalingCrit = "sargan+factorloading_R2",
                              correlatedErrors = NULL){

  #1, if columns are not named, stop and return error message.
  if(is.null(colnames(data))){ #have to name columns.
    stop('Please name columns.')
  }

  #2, if just based on order, don't need to run subsequent code
  #if scaling indicator selection is order, just use the first variable as the scaling indicator.
  if(scalingCrit == 'order'){
    scalingindicator <- colnames(data)[1]
  }else{
    #print('not order')

    ##order of R2
    R2_order <- colnames(r2_order(data))


    #3, if just based on r2 code, don't need to run subsequent code.
    #return scaling indicator already if just based on r2 value
    if(scalingCrit == 'R2'){
      scalingindicator <- R2_order[1]
    }else{ #then calculate sargan, factor loadings for each variable iteratively

      #print('scalingCrit!=R2')


      ##fit for each indicator as the scaling indicator
      model <- list()
      fit <- list()
      for(p in 1:ncol(data)){
        model[[p]] <- paste0('f1=~', paste0(colnames(data)[p]),  #scaling indicator
                             '+', #and the rest
                             paste0(colnames(data)[-p], collapse = '+'))
        if(!is.null(correlatedErrors)){ #add correlated errors to the model when present
          model[[p]] <- paste0(model[[p]], '\n', correlatedErrors)
        }
        fit[[p]] <- miive(model[[p]], data, var.cov = T)
        names(model)[p] <- names(fit)[p] <- colnames(data)[p] #name the lists using the variable used as the scaling indicator
      }

      ##get the variables with significant sargans and nonsignificant factor loadings for all p models
      ##returns a list of 3 lists:
      ##1, variables with significant sargans
      ##2, variables with nonsignificant factor loadings
      ##3, variables with either of the above two

      badvarinfo <- getsigsarganNnonsigloading(modellist = model,
                                               fitlist = fit,
                                               sigLevel)

      ##get the least number of each of the three lists
      minsigsargan <- min(sapply(badvarinfo$var_sigsargan, length))
      minnonsigloading <- min(sapply(badvarinfo$var_nonsigfactorloading, length))
      minboth <- min(sapply(badvarinfo$var_sarganNloading, length))


      ##when only one criterion specified, eg., 'sargan', and multiple models have the same number of min sig sargans
      ##return the first one based on their order in the data

      sarganallmin <- colnames(t(which(sapply(badvarinfo$var_sigsargan,
                                              length)==minsigsargan)))
      loadingallmin <- colnames(t(which(sapply(badvarinfo$var_nonsigfactorloading,
                                               length)==minnonsigloading)))
      bothallmin <- colnames(t(which(sapply(badvarinfo$var_sarganNloading,
                                            length)==minboth)))


      ##return scaling indicator based on prespecified criterion
      if(scalingCrit == 'sargan'){
        scalingindicator <- sarganallmin[1]
      }
      if(scalingCrit == 'factorloading'){
        scalingindicator <- loadingallmin[1]
      }

      ## when multiple criteria specified, use the subsequent criteria when multiple candidates present under the fisrt criterion
      if(scalingCrit == 'sargan_R2'){
        scalingindicator <- sarganallmin[order(match(sarganallmin, R2_order))][1]
      }
      if(scalingCrit == 'sargan_factorloading'){
        scalingindicator <- sarganallmin[order(match(sarganallmin,
                                                     loadingallmin)[!is.na(match(sarganallmin, loadingallmin))])][1]
      }
      if(scalingCrit == 'sargan_factorloading_R2'){
        #the first varible of temp equals when scalingCrit = 'sargan_factorloading'
        temp <- sarganallmin[order(match(sarganallmin,
                                         loadingallmin)[!is.na(match(sarganallmin, loadingallmin))])]
        #then match it again with R2_order
        scalingindicator <- temp[order(match(temp, R2_order))][1]
      }
      if(scalingCrit == 'factorloading_R2'){
        scalingindicator <- loadingallmin[order(match(loadingallmin, R2_order))][1]
      }
      if(scalingCrit == 'factorloading_sargan'){
        scalingindicator <- sarganallmin[order(match(loadingallmin,
                                                     sarganallmin)[!is.na(match(loadingallmin,sarganallmin))])][1]
      }
      if(scalingCrit == 'factorloading_sargan_R2'){
        temp <- sarganallmin[order(match(loadingallmin,
                                         sarganallmin)[!is.na(match(loadingallmin,sarganallmin))])]
        scalingindicator <- temp[order(match(temp, R2_order))][1]
      }
      if(scalingCrit =='sargan+factorloading'){ ##NOTE: this is different from sargan_factorloading
        scalingindicator <- bothallmin[1]
      }
      if(scalingCrit =='sargan+factorloading_R2'){
        scalingindicator <- bothallmin[order(match(bothallmin, R2_order))][1]
      }
    }
  }
  return(scalingindicator)
}
