#' @name r2_order
#' @title Exploratory Factor Analysis Using Model Implied Instrumental Variables.
#' @description This function calculates the R2 values for each variable in the data when the rest variables are regressed on it.
#' @usage
#'r2_order(data = '')
#' @param data The data matrix.
#' @author Lan Luo
#' @importFrom stats lm na.omit
#' @noRd


r2_order <- function(data){
  ##check the r2 for each variable as regressing all other variables on it
  #and use the highest r2 as the initial scaling indicator
  r2 <- matrix(NA, nrow = 1, ncol = dim(data)[2])
  colnames(r2) <- colnames(data)
  for (i in 1:dim(data)[2]){
    r2[,i] <- summary(lm(paste(colnames(data)[i], paste(colnames(data)[-i], collapse = "+"), sep = "~"), data = data))$r.squared
  }
  r2 <- as.matrix(t(r2[,order(r2[nrow(r2),],decreasing=TRUE)]))
  return(r2) #returns column names in the order from largest to lowest
  }
