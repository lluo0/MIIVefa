#' @name getbadvar
#' @title Exploratory Factor Analysis Using Model Implied Instrumental Variables.
#' @description This function extracts problematic variables from a model fit object.
#' @usage
#'  getbadvar(fit = '',
#'                 sigLevel = .05)
#' @param fit The fit object obtained from miive function from the MIIVsem package.
#' @param sigLevel The significance level threshold, default is .05.
#' @author Lan Luo
#' @examples
#' \dontrun{
#' mybadvar <- getbadvar(fit = mydata)
#' }
#' @import MIIVsem
#' @keywords internal
#' @noRd

getbadvar <- function(fit,
                      sigLevel=.05){
  v_list_sigsargan <- vector()
  for (p in 1:length(fit$eqn))
    if (fit$eqn[[p]]$sargan.p < sigLevel){
      v_list_sigsargan <- append(v_list_sigsargan,fit$eqn[[p]]$DVobs)
    }

  v_list_nonsigloading <- vector()
  table <- na.omit(estimatesTable(fit))
  #this resolves the different order of rows by estimatesTable between windows and mac
  table <- table[table$op=='=~',] #added 4.13.2022
  for (p in 1:length(fit$eqn))
    if (table[p,7] > sigLevel){
      v_list_nonsigloading <- append(v_list_nonsigloading, table[p,3])
    }
  v_list_final <- unique(c(v_list_sigsargan, v_list_nonsigloading))


  return(list(badvar = v_list_final,
              badsargan = v_list_sigsargan,
              badloading = v_list_nonsigloading))
}
