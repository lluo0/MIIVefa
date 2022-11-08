

##final function
EFAmiiv <- function(data,
                    sigLevel = .05,
                    scalingCrit = "sargan+factorloading_R2",
                    correlatedErrors = NULL){
  ## need to add several sanity checks here later

  temp <- step1_EFAmiiv(data, sigLevel, scalingCrit, correlatedErrors)
  while(temp$nextstep == 'yes'){ #while nextstep=yes, create a new factor
    temp <- stepN_EFAmiiv(data, sigLevel, scalingCrit, stepPrev = temp)
    if(temp$nextstep == 'yes'){#then check crossloading first
      temp <- crossloadcheck(data, sigLevel, scalingCrit, stepPrev = temp)
    }
  }
  return(temp[1:2])
}
