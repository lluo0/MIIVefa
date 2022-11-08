
##check for crossload
crossloadcheck <- function(data,
                           sigLevel,
                           scalingCrit,
                           stepPrev){
  #read in relevant objetcs
  newgoodvarlist <- stepPrev$varPerFac
  #newbadvarlist <- stepPrev$badvarlist
  badvar <- stepPrev$badvar
  correlatedErrors <- stepPrev$correlatedErrors

  #varPerFac is a temporary list storing the current variables on each factor.
  #it is used to check for badvars on each factor
  #when we return info, only return newgoodvarlist and newbadvarlist
  #when no badvar present, varPerFac equals newgoodvarlist and no further model searching is needed.
  crossloadmodelpart <- varPerFac <- list()
  num_factor <- length(newgoodvarlist)
  for(n in 1:length(newgoodvarlist)){
    #varPerFac[[n]] <- c(newgoodvarlist[[n]], unique(unlist(newbadvarlist)))
    varPerFac[[n]] <- c(newgoodvarlist[[n]], badvar)
    crossloadmodelpart[[n]] <- paste0('f', n, '=~', paste0(varPerFac[[n]], collapse = '+'))
  }

  crossloadmodel <- paste0(crossloadmodelpart, collapse = '\n')
  if(!is.null(correlatedErrors)){
    crossloadmodel <- paste0(crossloadmodel, '\n', correlatedErrors)
  }

  crossfit <- miive(model = crossloadmodel, data = data, var.cov = T)

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
  #if(newbadvarlist[[length(newbadvarlist)]] == crossbadvarlist[[length(crossbadvarlist)]]){
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
    if(length(badvar_unlist[!badvar_unlist %in% badvarturnedgoodaftercrossload])>1){ #if we have still multiple badvars
      #update goodvar and badvar list
      # varPerFac <- Map(c, newgoodvarlist,
      #                  Map(setdiff, newbadvarlist, crossbadvarlist)) ##this is updating goodvar for each factor
      varPerFac <- Map(c, newgoodvarlist,
                       lapply(crossbadvarlist, function(x) setdiff(badvar, x))) ##this is updating goodvar for each factor

      #badvarlist <- crossbadvarlist
      badvar <- setdiff(colnames(data),unique(unlist(varPerFac)))
      # newbadvarlist <- Map(c, Map(setdiff, newbadvarlist, #first delete old variables that turn good after crossloading
      #                      Map(setdiff, newbadvarlist, crossbadvarlist)),
      #                      Map(setdiff, crossbadvarlist, newbadvarlist)) #then add new badvars that appeared after crossloading

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
      }else{
        num_factor <- length(varPerFac)
        nextstep <- 'yes'
        correlatedErrors <- correlatedErrors
        num_badvar <- length(badvar)
      }


    }else{##if <=1, nextstep=no
      nextstep <- 'no'
      #check if need to get rid of this one crossload!!
      if(length(unique(unlist(crossbadvarlist)))==1){
        modelpart_temp <- Map(c, newgoodvarlist,
                              lapply(crossbadvarlist, function(x) setdiff(badvar, x)))
                         #lapply(crossbadvarlist, function(x) setdiff(unique(unlist(newbadvarlist)), x)))

        model_temp <- list()
        for(n in 1:length(modelpart_temp)){
          model_temp[[n]] <- paste0('f', n, '=~', paste0(modelpart_temp[[n]], collapse = '+'))
        }
        model_temp <- paste0(model_temp, collapse  = '\n')
        if(!is.null(correlatedErrors)){
          model_temp <- paste0(model_temp, '\n', correlatedErrors)
        }
        fit_temp <- miive(model = model_temp, data, var.cov = T)
        badvar_temp <- getbadvar_multi(fit_temp, sigLevel, num_factor = num_factor, modelpart_temp)
        if(!length(unique(unlist(badvar_temp)))>2){
          #if this badvar disappeared OR still a badvar, we retain this uncrossloaded model for this variablebecause of parsimony.
          model <- model_temp
          fit <- fit_temp
        }else{
          model <- crossloadmodel
          fit <- crossfit
        }

      }else{
        model <- crossloadmodel
        fit <- crossfit
      }
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

  ##if some more new badvar and some old bad var disappear: ?? what should we do?

  # ##if no new badvar and some old badvar disappear:
  # if(length(newbadvaraftercrossload)== 0){
  #   ##if number of current badvar > 1: move on to next step aka create new factor
  #   if(length(badvar_unlist[!badvar_unlist %in% badvarturnedgoodaftercrossload])>1){
  #     #update goodvar and badvar list
  #     varPerFac <- Map(c, newgoodvarlist,
  #                           Map(setdiff, newbadvarlist, crossbadvarlist)) ##this is updating goodvar for each factor
  #
  #     badvarlist <- crossbadvarlist
  #     # newbadvarlist <- Map(c, Map(setdiff, newbadvarlist, #first delete old variables that turn good after crossloading
  #     #                      Map(setdiff, newbadvarlist, crossbadvarlist)),
  #     #                      Map(setdiff, crossbadvarlist, newbadvarlist)) #then add new badvars that appeared after crossloading
  #
  #     num_factor <- length(varPerFac)
  #     nextstep <- 'yes'
  #     correlatedErrors <- correlatedErrors
  #     num_badvar <- length(unique(unlist(badvarlist)))
  #
  #
  #
  #   }else{##if <=1, retain this crossload model and nextstep=no
  #     model <- crossloadmodel
  #     fit <- crossfit
  #     nextstep <- 'no'
  #   }
  # }

return(finalobj)

}
