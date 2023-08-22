#' @name getsigsarganNnonsigloading
#' @title Exploratory Factor Analysis Using Model Implied Instrumental Variables.
#' @description This function extracts variables that have significant sargan statistics and/or non-significant factor loadings from a list of models.
#' @usage
#'getsigsarganNnonsigloading(modellist = '',
#'                            fitlist = '',
#'                            sigLevel = .05)
#' @param modellist A list of models with the same variables that only differ by the scaling indicator.
#' @param fitlist A list of miive fit objects based on the models in modellist.
#' @param sigLevel The significance level threshold, default is .05.
#' @author Lan Luo
#' @examples
#' \dontrun{
#' myproblematicvariables <- getsigsarganNnonsigloading(modellist = mymodellist, fitlist = myfitlist)
#' }
#' @import MIIVsem
#' @keywords internal
#' @noRd


##get the VARIABLE NAME (COLUMN NAME) for significant sargans + non-significant factor loadings
#for EACH variable as the scaling indicator
#note that model and fit objetcs are lists, as each variable is iteratedly used as the scaling indicator
getsigsarganNnonsigloading <- function(modellist,
                                       fitlist,
                                       sigLevel){
  var_sigsargan <- var_nonsigfactorloading <- var_sarganNloading <- list()
  ##sargan+scaling indicator
  ## see negfisher[[12]] for example - a variable with both significant sargan and non-significant factor loading will only be counted once
  for(p in 1:length(modellist)){
    var_sigsargan[[p]] <- var_nonsigfactorloading[[p]] <- var_sarganNloading[[p]] <- vector()
    names(var_sigsargan)[p] <- names(var_nonsigfactorloading)[p] <- names(var_sarganNloading)[p]  <- names(fitlist)[p]
    for(i in 1:length(fitlist[[p]]$eqn)){
      if(fitlist[[p]]$eqn[[i]]$sargan.p < sigLevel){
        var_sigsargan[[p]] <- c(var_sigsargan[[p]], fitlist[[p]]$eqn[[i]]$DVobs)
      }
    }
    ##because the sargan statistics for each variable are NOT factor specific
    ##aka, sargan for x5 on f1, f2, ..., fn are identical
    ##however, factor loadings are factor specific
    ##aka, factor loading for x5 on f1, f2, ..., fn are not necessarily identical (usually they are completely different)
    ##so no need to use unique() on var_nonsigfactorloading
    ##so if x5 has non-significant factor loading on both f1 and f2, we count it twice
    var_sigsargan[[p]] <- unique(var_sigsargan[[p]])

    loadingtable <- estimatesTable(fitlist[[p]])[estimatesTable(fitlist[[p]])$op == "=~",]
    for(i in 1:nrow(loadingtable)){
      if(loadingtable[i,]$pvalue > sigLevel && !is.na(loadingtable[i,]$pvalue)){
        var_nonsigfactorloading[[p]] <- c(var_nonsigfactorloading[[p]], loadingtable[i,3])
      }
    }

    var_sarganNloading[[p]] <- c(var_sigsargan[[p]], var_nonsigfactorloading[[p]])
    #var_sarganNloading[[p]] <- unique(c(var_sigsargan[[p]], var_nonsigfactorloading[[p]]))
  }

  finalobj <- list(var_sigsargan = var_sigsargan,
                   var_nonsigfactorloading = var_nonsigfactorloading,
                   var_sarganNloading = var_sarganNloading)
  return(finalobj)
}

