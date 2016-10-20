
#Best Subset selection algorithm.

#Input: 
#data of named matrix form, with dependent variable in last column

#Output:

# x, a list, storing each 'best' lm object for each number of predictors according to R2
# y, a list, the summaries of each lm object in x
# interim.models, a matrix of each successive best predictor combination and its R2 value


bss <- function(data){

  
  numb.preds <- dim(data)[2] - 1
  name.dep <- names(data)[numb.preds + 1]
  interim.models <- matrix(nrow = numb.preds + 1, ncol = numb.preds)
  x <- list(1:numb.preds)
  y <- list(1:numb.preds)
    
for(j in 1:numb.preds){
  
  z <- matrix(nrow = j + 1, ncol = choose(numb.preds, j))
  
  for(i in 1:choose(numb.preds, j)){
    
    names.comb <- combn(names(data[,1:numb.preds]), j)
    f <- 1:(length(names.comb)/j)
    
    interim.names <- paste(names.comb[,f[i]], collapse = " + ") 
    interim <- lm(as.formula(paste(name.dep, '~', interim.names, sep = "")))
    interim.summ <- summary(interim)
    z[,i] <- c(names.comb[,f[i]],interim.summ$r.squared)
    
  }
  
  first <- z[,which.max(z[j+1,])]
  
  if(length(first) < numb.preds + 1){
    second <- (length(first)+1):(numb.preds + 1)
    second <- NA[1:length(second)]
    
    interim.models[,j] <- c(first, second)}
  
  if(length(first) == (numb.preds + 1)){
    
    interim.models[,j] <- first}
  
  abc <- paste(first[1:j], collapse = " + ") 
  x[[j]] <- lm(as.formula(paste(name.dep, '~', abc, sep = "")))
  y[[j]] <- summary(x[[j]])
  
}
  list(interim.models, x, y)
  
}  


