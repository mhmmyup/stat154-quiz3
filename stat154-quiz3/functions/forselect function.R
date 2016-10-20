
#Forward selection algorithm.

#Input: 
#data of named matrix form, with dependent variable in last column

#Output:

# xx, a list, storing each 'best' lm object for each added predictor according to R2
# yy, a list, the summaries of each lm object in x
# interim.models2, a matrix of each best added predictor combination and its R2 value

forselect <- function(data){
  
  numb.preds <- dim(data)[2] - 1
  interim.models2 <- matrix(nrow = numb.preds + 1, ncol = numb.preds)
  xx <- list(1:numb.preds)
  yy <- list(1:numb.preds)
  
  name.dep <- names(data)[numb.preds + 1]
  names.avail <- names(data)[1:numb.preds]
  names.sofar <- NA

for(j in 1:numb.preds){
  
  z <- matrix(nrow = j + 1, ncol = numb.preds)
  
  for(i in 1:(13-j)){
    
    if(is.na(names.sofar[1])){
      interim.names <- names.avail[i]
    } else {
      interim.names <- paste(names.sofar, names.avail[i], sep = " + ", collapse = " + ") }
    
    interim <- lm(as.formula(paste(name.dep,'~', interim.names, sep = "")))
    interim.summ <- summary(interim)
    
    if(is.na(names.sofar[1])){
      z[,i] <- c(names.avail[i], interim.summ$r.squared)
    } else {
      z[,i] <- c(c(names.sofar, names.avail[i]),interim.summ$r.squared)}
    
  }
  
  first <- z[,which.max(z[j+1,])]
  
  if(length(first) < (numb.preds + 1)){
    second <- (length(first)+1):(numb.preds +1)
    second <- NA[1:length(second)]
    
    interim.models2[,j] <- c(first, second)}
  
  if(length(first) == (numb.preds+1)){
    
    interim.models2[,j] <- first}
  
  abc <- paste(first[1:j], collapse = " + ") 
  xx[[j]] <- lm(as.formula(paste(name.dep, '~', abc, sep = "")))
  yy[[j]] <- summary(x[[j]])
  
  if(!is.na(names.sofar[1])) {
    names.sofar <- append(names.sofar, first[j])}
  if(is.na(names.sofar[1])) { names.sofar <- first[j]}
  
  names.avail <- setdiff(names(data)[1:numb.preds], names.sofar)
}
  
  list(interim.models2, xx, yy)
   
}  




