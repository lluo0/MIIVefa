#' @name stepN_miivefa
#' @title Exploratory Factor Analysis Using Model Implied Instrumental Variables.
#' @description This function checks if a new factor is needed from the last step model, or if crossloadings should be checked.
#' @usage
#'stepN_miivefa(data='',
#'                  sigLevel = .05,
#'                  scalingCrit = "sargan+factorloading_R2",
#'                  stepPrev = '')
#' @param data The data matrix to be examined.
#' @param sigLevel The significance level threshold, default is .05.
#' @param scalingCrit The criterion used to select the scaling indicators, default is 'sargan+factorloading_R2.'
#' @param stepPrev The output object from the last step from the function crossloadcheck.
#' @author Lan Luo
#' @import MIIVsem
#' @keywords internal
#' @noRd

stepN_miivefa <- function(data,
                          sigLevel,
                          scalingCrit = 'sargan+factorloading_R2',
                          stepPrev){

  ##first read in relevant info from the last step
  #badvarlist <- stepPrev$badvarlist
  varPerFac <- stepPrev$varPerFac
  #goodmodelpart <- stepPrev$goodmodelpart
  correlatedErrors <- stepPrev$correlatedErrors
  num_factor_new <- length(varPerFac)+1

  ##then select the scaling indicator for potential new factor
  scalingindicator <- select_scalingind_stepN(data,
                                              sigLevel = sigLevel,
                                              scalingCrit = scalingCrit,
                                              stepPrev = stepPrev)
  #order of the new scaling indicator in the BADVAR
  #badvar_unlist <- unique(unlist(badvarlist))
  badvar_unlist <- stepPrev$badvar
  order_scalingind <- which(badvar_unlist==scalingindicator)

  ##then create new model
  ##all badvars first loaded on this new factor
  varPerFac[[num_factor_new]] <- c(scalingindicator, badvar_unlist[-order_scalingind])
  modelpart <- list()
  for(n in 1:num_factor_new){
    modelpart[[n]] <- paste0('f', n, '=~', paste0(varPerFac[[n]], collapse = '+'))
  }
  model <- paste0(modelpart, collapse = '\n')

  # model <- paste0(paste0(goodmodelpart,collapse = ' \n '), ' \n ',
  #                 #then create a new factor and load all badvars
  #                 paste0('f',num_factor_new, '=~', paste0(varPerFac[[num_factor_new]], collapse = '+')))
  ##add in correlated errors when provided
  if(!is.null(correlatedErrors)){
    model <- paste0(model, '\n', correlatedErrors)
  }

  ##fit model using miivSEM
  ##catch error when model is overidentified!
  fit <- tryCatch( miive(model = model, data, var.cov = T),
                   error = function(e)
                     return(0)) #0 for error

  #####NEED TO DOUBLE CHECK HERE######
  ###HOW TO BEST HANDLE THIS? RETURN THE MODEL FROM THE PREVIOUS STEP?
  #simply print out the model but not MIIVsem estimate
  #if(class(fit)!='miive'){
  if(!inherits(fit, "miive")){
    #stop('Model is overidentified.')

    #if model is overidentified, then retain all badvars on the last factor and stops here.
    # modelpart <- list()
    # for(n in 1:length(stepPrev$varPerFac)){
    #   modelpart[[n]] <- paste0('f', n, '=~', paste0(stepPrev$varPerFac[[n]], collapse = '+'))
    # }
    # modelpart[[length(modelpart)]] <- paste0(modelpart[[length(modelpart)]], '+', paste0(stepPrev$badvar, collapse = '+'))
    # model <- paste0(modelpart, collapse = '\n')
    # fit <- miive(model, data, var.cov = T)

    message('No latent variables found. \n')
    #warning('No latent variables found.')
    finalobj <- list(model = model,
                     nextstep = 'no')
  } else{

    ##then get new badvars after creating the new factor
    newbadvarlist <- getbadvar_multi(fit, sigLevel, num_factor = num_factor_new, varPerFac)[[1]]

    ##and new goodvar list
    newgoodvarlist <- Map(setdiff, varPerFac, newbadvarlist)

    ##if newgoodvar contains all variables in the data, then return the model after removing these badvars
    if(length(unique(unlist(newgoodvarlist))) == ncol(data)){
      modelpart <- list()
      for(n in 1:length(newgoodvarlist)){
        modelpart[[n]] <- paste0('f', n, '=~', paste0(newgoodvarlist[[n]], collapse = '+'))
      }
      model <- paste0(modelpart, collapse = '\n')
      ##add in correlated errors when provided
      if(!is.null(correlatedErrors)){
        model <- paste0(model, '\n', correlatedErrors)
      }
      fit <- miive(model = model, data = data, var.cov = T)
      finalobj <- list(model = model,
                       fit = fit,
                       nextstep = 'no')
    } else{
      finalobj <- list(varPerFac = newgoodvarlist,
                       #badvarlist = newbadvarlist,
                       badvar = unique(unlist(newbadvarlist)),
                       correlatedErrors = correlatedErrors,
                       nextstep = 'yes')
    }
  }

  return(finalobj)


}


