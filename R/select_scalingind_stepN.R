#' @name select_scalingind_stepN
#' @title Exploratory Factor Analysis Using Model Implied Instrumental Variables.
#' @description This function selects the scaling indicator for a multi-factor model when creating a new factor.
#' @usage
#'select_scalingind_stepN(data='',
#'                  sigLevel = .05,
#'                  scalingCrit = "sargan+factorloading_R2",
#'                  stepPrev = '')
#' @param data The data matrix to be examined.
#' @param sigLevel The significance level threshold, default is .05.
#' @param scalingCrit The criterion used to select the scaling indicators, default is 'factorloading_R2.'
#' @param stepPrev The output object from the last step from the function crossloadcheck.
#' @author Lan Luo
#' @examples
#' \dontrun{
#' myscalingindicator_stepN <- select_scalingind_stepN(data = mydata, stepPrev = crossloadcheckobj)
#' }
#' @import MIIVsem
#' @keywords internal
#' @noRd


select_scalingind_stepN <- function(data,
                                    sigLevel = .05,
                                    scalingCrit = "sargan+factorloading_R2",
                                    stepPrev){

  ## extract info from  the previous step finalobj, aka stepPrev
  #goodmodelpart <- stepPrev$goodmodelpart
  varPerFac <- stepPrev$varPerFac
  #badvar <- stepPrev$badvarlist  #these would be the candidates for scaling indicator for the new potential factor
  badvar_unlist <- stepPrev$badvar
  num_factor <- stepPrev$num_factor
  correlatedErrors <- stepPrev$correlatedErrors


  ## 1, if columns are not named, stop and return error message.
  if(is.null(colnames(data))){ #have to name columns.
    stop('Please name columns.')
  }

  #2, if just based on order, don't need to run subsequent code
  #if scaling indicator selection is order, just use the first variable as the scaling indicator.
  if(scalingCrit == 'order'){
    scalingindicator <- colnames(data)[1]
  }else if(length(badvar_unlist)==1){
    scalingindicator <- badvar_unlist #if only one badvar, then it'd be the sacling indicator
  }else{

    #badvar_unlist <- unique(unlist(badvar)) #because badvar is a list of badvars for each factor
    #get order of R2 values of the BADVARS
    R2_order <- colnames(r2_order(data[,badvar_unlist]))

    #3, if just based on r2 code, don't need to run subsequent code.
    #return scaling indicator already if just based on r2 value
    if(scalingCrit == 'R2'){
      scalingindicator <- R2_order[1]
    }else{

      ##create good model part first based on varPerFac
      goodmodelpartlist <- list()
      for(n in 1:length(varPerFac)){
        goodmodelpartlist[[n]] <- paste0('f', n, '=~', paste0(varPerFac[[n]], collapse = '+'))
      }
      goodmodelpart <- paste0(goodmodelpartlist, collapse = '\n')
      ##fit for each indicator as the scaling indicator
      model <- list()
      fit <- list()
      for(p in 1:length(badvar_unlist)){
        ###NO CROSSLOAD HERE!!
        # model[[p]] <- paste(paste0(goodmodelpart,'+' ,paste0(badvar_unlist[-p], collapse = "+")),
        #                     paste(paste0("f",num_factor+1), "=~",paste0(badvar_unlist[p]), '+',
        #                           paste0(badvar_unlist[-p], collapse = "+"), sep = ""),
        #                     sep = "\n")

        model[[p]] <- paste0(goodmodelpart, '\n',
                             paste0('f', num_factor+1, '=~', badvar_unlist[p], '+'),
                             paste0(badvar_unlist[-p], collapse = '+'))
        if(!is.null(correlatedErrors)){ #add correlated errors to the model when present
          model[[p]] <- paste0(model[[p]], '\n', correlatedErrors)
        }
        fit[[p]] <- miive(model[[p]], data, var.cov = T)
        names(model)[p] <- names(fit)[p] <- badvar_unlist[p] #name the lists using the variable used as the scaling indicator
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
      # minsigsargan <- min(sapply(badvarinfo[['var_sigsargan']], length))
      # minnonsigloading <- min(sapply(badvarinfo[['var_nonsigfactorloading']], length))
      # minboth <- min(sapply(badvarinfo[['var_sarganNloading']], length))
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
      # sarganallmin <- colnames(t(which(sapply(badvarinfo[['var_sigsargan']],
      #                                         length)==minsigsargan)))
      # loadingallmin <- colnames(t(which(sapply(badvarinfo[['var_nonsigfactorloading']],
      #                                          length)==minnonsigloading)))
      # bothallmin <- colnames(t(which(sapply(badvarinfo[['var_sarganNloading']],
      #                                       length)==minboth)))

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


