#Mallow's CP Function

#Input: List of lm objects 

#Output: Vector of CP statistic for each lm object


cp.lm <- function(lm.list) {
  n <- nobs(lm.list[[1]])
  DoFs <- sapply(lm.list, function(a) { sum(hatvalues(a)) })
  MSEs <- sapply(lm.list, function(a) { mean(residuals(a)^2) })
  #Model w most parameters
  biggest <- which.max(DoFs)
  #Use most inclusive model MSE to estimate sigma^2
  sigma2.hat <- MSEs[[biggest]]*n/(n-DoFs[[biggest]])
  Cp <- MSEs + 2*sigma2.hat*DoFs/n
  return(Cp)
  
}