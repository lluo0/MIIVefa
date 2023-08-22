#' @name crossloadcheck
#' @title Exploratory Factor Analysis Using Model Implied Instrumental Variables.
#' @description This function checks for crossloadings when there are more than 2 problematic variables after creating a new factor.
#' @usage
#'  crossloadcheck(data = '',
#'                 sigLevel = .05,
#'                 scalingCrit = '',
#'                 stepPrev = '')
#' @param data The data matrix to be examined.
#' @param sigLevel The significance level threshold, default is .05.
#' @param scalingCrit The criterion used to select the scaling indicators, default is 'sargan+factorloading_R2.'
#' @param stepPrev The output object from the last step from the function stepN_miivefa.
#' @author Lan Luo
#' @examples
#' \dontrun{
#' crossloadcheckobj <- crossloadcheck(data = mydata, stepPrev = step1)
#' }
#' @import MIIVsem
#' @keywords internal
#' @noRd


crossloadcheck <- function(data,
                           sigLevel =.05,
                           scalingCrit = 'sargan+factorloading_R2',
                           stepPrev){
  #read in relevant objetcs
  newgoodvarlist <- stepPrev$varPerFac
  badvar <- stepPrev$badvar
  correlatedErrors <- stepPrev$correlatedErrors

  #varPerFac is a temporary list storing the current variables on each factor.
  #it is used to check for badvars on each factor
  #when we return info, only return newgoodvarlist and newbadvarlist
  #when no badvar present, varPerFac equals newgoodvarlist and no further model searching is needed.
  crossloadmodelpart <- varPerFac <- list()
  num_factor <- length(newgoodvarlist)
  for(n in 1:length(newgoodvarlist)){
    varPerFac[[n]] <- c(newgoodvarlist[[n]], badvar)
    crossloadmodelpart[[n]] <- paste0('f', n, '=~', paste0(varPerFac[[n]], collapse = '+'))
  }

  crossloadmodel <- paste0(crossloadmodelpart, collapse = '\n')
  if(!is.null(correlatedErrors)){
    crossloadmodel <- paste0(crossloadmodel, '\n', correlatedErrors)
  }

  ##catch error when model is underidentified!
  crossfit <- tryCatch( miive(model = crossloadmodel, data = data, var.cov = T),
                        error = function(e)
                          return(0)) #0 for error
  #if(class(crossfit)!='miive'){ #aka it is 0, then we retain everything from the previous step and don't test for crossloadings
  if(!inherits(crossfit, "miive")){
    finalobj <- stepPrev
  } else {
    #get new badvars after crossloading
    allcrossbadvarlist <- getbadvar_multi(crossfit, sigLevel, num_factor = num_factor, varPerFac)
    crossbadvarlist <- allcrossbadvarlist[[1]]
    crossbadvarlist_sargan <- allcrossbadvarlist[[2]]
    crossbadvarlist_loading <- allcrossbadvarlist[[3]]

    #
    crossbadvar_unlist <- unique(unlist(crossbadvarlist))
    crossbadvarlist_sargan_unlist <- unique(unlist(crossbadvarlist_sargan))
    crossbadvarlist_loading_unlist <- unique(unlist(crossbadvarlist_loading))

    #update goodvars (varperfac) and badvar
    varPerFac <- Map(c, newgoodvarlist,
                     lapply(crossbadvarlist, function(x) setdiff(badvar, x))) ##this is updating goodvar for each factor


    badvar <- setdiff(colnames(data),unique(unlist(varPerFac)))

    ##if only one badvar now, re-estimate loading it just on the latest factor
    if(length(badvar)==1){
      if(sum(sapply(crossbadvarlist_sargan, function(x) badvar %in% x)) > sum(sapply(crossbadvarlist_loading, function(x) badvar %in% x))){
        varPerFac[[length(varPerFac)]] <- c(varPerFac[[length(varPerFac)]], badvar)
        model_temp <- list()
        for(n in 1:length(varPerFac)){
          model_temp[[n]] <- paste0('f', n, '=~', paste0(varPerFac[[n]], collapse = '+'))
        }
        model_temp <- paste0(model_temp, collapse  = '\n')
        if(!is.null(correlatedErrors)){
          model_temp <- paste0(model_temp, '\n', correlatedErrors)
        }
        fit_temp <- miive(model = model_temp, data, var.cov = T)
        model <- model_temp
        fit <- fit_temp
        nextstep <- 'no'
      }else{
        nextstep <- 'yes'
        num_factor <- length(varPerFac)
        correlatedErrors <- correlatedErrors
      }
      # varPerFac[[length(varPerFac)]] <- c(varPerFac[[length(varPerFac)]], badvar)
      # model_temp <- list()
      # for(n in 1:length(varPerFac)){
      #   model_temp[[n]] <- paste0('f', n, '=~', paste0(varPerFac[[n]], collapse = '+'))
      # }
      # model_temp <- paste0(model_temp, collapse  = '\n')
      # if(!is.null(correlatedErrors)){
      #   model_temp <- paste0(model_temp, '\n', correlatedErrors)
      # }
      # fit_temp <- miive(model = model_temp, data, var.cov = T)
      #
      # allcrossbadvarlist <- getbadvar_multi(crossfit, sigLevel, num_factor = num_factor, varPerFac)
      # #if only have new badvar now and it is NOT because of non-significant loading
      # if(length(unique(unlist(allcrossbadvarlist[[3]])))==0 & length(unique(unlist(allcrossbadvarlist[[1]])))==1){
      #   model <- model_temp
      #   fit <- fit_temp
      #   nextstep <- 'no'
      # }else{#still need next step
      #   nextstep <- 'yes'
      #   #and need to redo varperfac. but too much work
      # }
    }else if(length(badvar) > 1){#or, we need to clean up unnecessary crossloadings before moving on

      num_factor <- length(varPerFac)
      nextstep <- 'yes'
      correlatedErrors <- correlatedErrors
      num_badvar <- length(badvar)
    } else{##if ==0, nextstep=no
      nextstep <- 'no'
      model_temp <- list()
      for(n in 1:length(varPerFac)){
        model_temp[[n]] <- paste0('f', n, '=~', paste0(varPerFac[[n]], collapse = '+'))
      }
      model_temp <- paste0(model_temp, collapse  = '\n')
      if(!is.null(correlatedErrors)){
        model_temp <- paste0(model_temp, '\n', correlatedErrors)
      }
      fit_temp <- miive(model = model_temp, data, var.cov = T)
      model <- model_temp
      fit <- fit_temp
    }
    if(nextstep=='yes'){
      finalobj <- list(varPerFac = varPerFac,
                       badvar = badvar,
                       num_factor = num_factor,
                       correlatedErrors = correlatedErrors,
                       nextstep = 'yes')
    }else{
      finalobj <- list(model = model,
                       fit = fit,
                       nextstep = 'no')
    }

  }



return(finalobj)

}
