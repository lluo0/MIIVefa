#' @name miivefa
#' @title Exploratory Factor Analysis Using Model Implied Instrumental Variables.
#' @description This function is the user-level function that conducts miivefa.
#' @usage
#'  miivefa(data,
#'        sigLevel = .05,
#'        scalingCrit = "sargan+factorloading_R2",
#'        correlatedErrors = NULL)
#' @param data A data frame, list or environment or an object coercible by as.data.frame to data frame.
#' The most common application is to supply a data.frame.
#' @param sigLevel The significance level threshold, default is .05.
#' @param scalingCrit The criterion used to select the scaling indicators, default is 'factorloading_R2.'
#' More details see \code{\link{select_scalingind}}
#' @param correlatedErrors The pairs of variables whose errors should be correlated in the model search procedure, default is NULL.
#' @importFrom utils packageVersion
#' @return A miivefa object containing a vector of the recovered model syntax, and a miivsem object of model fit of the recovered model when applicable.
#' The recovered model syntax and miivsem fit object can be accessed using output$model and output$fit.
#' The output$model miivsem object contains both parameter estimation and model fit information.
#' @export

##final function
miivefa <- function(data,
                    sigLevel = .05,
                    scalingCrit = "sargan+factorloading_R2",
                    correlatedErrors = NULL){
  ## need to add several sanity checks here later

  temp <- step1_miivefa(data, sigLevel, scalingCrit, correlatedErrors)
  while(temp$nextstep == 'yes'){ #while nextstep=yes, create a new factor
    temp <- stepN_miivefa(data, sigLevel, scalingCrit, stepPrev = temp)
    if(temp$nextstep == 'yes'){#then check crossloading first
      temp <- crossloadcheck(data, sigLevel, scalingCrit, stepPrev = temp)
    }
  }
  finalobj <- temp[1:(length(temp)-1)]
  class(finalobj) <- c("miivefa", class(finalobj))
  return(finalobj)
}
