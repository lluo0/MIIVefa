#' @name miivefa
#' @title Exploratory Factor Analysis Using Model Implied Instrumental Variables.
#' @description This function is the user-level function that conducts miivefa.
#' @usage
#'  miivefa(data = mydata,
#'        sigLevel = .05,
#'        scalingCrit = "sargan+factorloading_R2",
#'        correlatedErrors = NULL)
#' @param data The data matrix.
#' @param sigLevel The significance level threshold, default is .05.
#' @param scalingCrit The criterion used to select the scaling indicators, default is 'factorloading_R2.'
#' @param correlatedErrors The pairs of variables whose errors should be correlated in the model search procedure, default is NULL.
#' @author Lan Luo
#' @importFrom utils packageVersion
#' @examples
#' \dontrun{
#' myfinalobj <- miivefa(data = mydata, correlatedErrors = 'v3~~v4')
#' }
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
  return(temp[1:(length(temp)-1)])
}
