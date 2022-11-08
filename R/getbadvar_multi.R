getbadvar_multi <- function(fit,
                                sigLevel = .05,
                                num_factor = 1,
                                varPerFac){
  newbadvar_loading <- list()
  coeftable <- estimatesTable(fit)[estimatesTable(fit)[,2] == '=~' &
                                    !is.na(estimatesTable(fit)$pvalue),] #removes factor loadings

  #the bad var with non-siginificant factor loading coefficients for each factor
  for(p in 1:num_factor){
    newbadvar_loading[[p]] <- vector()
    for(i in 1:nrow(coeftable)){
      if(paste0('f',p)%in%coeftable[i,1]
         & coeftable[i,7] > sigLevel){
        newbadvar_loading[[p]] <- append(newbadvar_loading[[p]],coeftable[i,3])
      }
    }
  }
  #the bad var with significant sargans.
  newbadvar_sargan <- vector()
  for (p in 1:length(fit$eqn)){
    if (fit$eqn[[p]]$sargan.p < sigLevel){
      newbadvar_sargan <- append(newbadvar_sargan,fit$eqn[[p]]$DVobs)
    }
  }

  #make this per factor, and only return variables that are on that factor.
  #eg., if x5 has significant sargan but only loads on f2 not f1, only return it as badvar for f2
  newbadvar_sargan <- lapply(varPerFac, function(x)
    newbadvar_sargan[newbadvar_sargan %in% x])

  #then combine the two lists and return unique variables
  #eg., if x5 has both significant sargan and nonsignificant factor loading on f2, only return it twice.
  newbadvar <- lapply(Map(c, newbadvar_loading, newbadvar_sargan), unique)
  # newbadvar <- lapply(newbadvar_loading, function(x)
  #   unique(c(x, newbadvar_sargan)))
  return(newbadvar)

}
