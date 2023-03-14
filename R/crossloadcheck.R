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
#' @param scalingCrit The criterion used to select the scaling indicators, default is 'factorloading_R2.'
#' @param stepPrev The output object from the last step from the function stepN_EFAmiiv.
#' @author Lan Luo
#' @examples
#' \dontrun{
#' crossloadcheckobj <- crossloadcheck(data = mydata, stepPrev = step1)
#' }
#' @export


crossloadcheck <- function(data,
                           sigLevel,
                           scalingCrit,
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
  if(class(crossfit)!='miive'){ #aka it is 0, then we retain everything from the previous step and don't test for crossloadings
    finalobj <- stepPrev
  } else {
    #get new badvars after crossloading
    crossbadvarlist <- getbadvar_multi(crossfit, sigLevel, num_factor = num_factor, varPerFac)


    #
    crossbadvar_unlist <- unique(unlist(crossbadvarlist))
    #badvar_unlist <- unique(unlist(newbadvarlist))
    badvar_unlist <- badvar
    ##new badvar after crossloading
    newbadvaraftercrossload <- setdiff(crossbadvar_unlist, badvar_unlist)
    ##old badvars that disappears after acrossloading
    badvarturnedgoodaftercrossload <- setdiff(badvar_unlist, crossbadvar_unlist)


    ##if still have ALL the old badvar on the latest factor: no crossloading and move on to next step
    if(identical(badvar,crossbadvarlist[[length(crossbadvarlist)]]) & length(badvar)!=1){
      varPerFac <- stepPrev$varPerFac
      #badvarlist <- newbadvarlist
      #badvar <- unique(unlist(newbadvarlist))
      badvar <- stepPrev$badvar
      num_factor <- length(varPerFac)
      nextstep <- 'yes'
      correlatedErrors <- correlatedErrors
    }else if(identical(badvar,crossbadvarlist[[length(crossbadvarlist)]]) & length(badvar)==1) { #only one badvar and didn't turn good
      nextstep <- 'no'
      varPerFac <- stepPrev$varPerFac
      varPerFac[[length(varPerFac)]] <- c(varPerFac[[length(varPerFac)]], stepPrev$badvar)
      #then create a model and fit
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
    }else{#if some more new badvar and some old bad var disappear

      #update goodvar and badvar list

      varPerFac <- Map(c, newgoodvarlist,
                       lapply(crossbadvarlist, function(x) setdiff(badvar, x))) ##this is updating goodvar for each factor


      badvar <- setdiff(colnames(data),unique(unlist(varPerFac)))

      ##if new badvar after cleaning up unnecessary crossloading is 1, no need to move on to the next step but just load it on the last factor
      if(length(badvar)==1){
        nextstep <- 'no'
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
      }else if(length(badvar)>1){
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
