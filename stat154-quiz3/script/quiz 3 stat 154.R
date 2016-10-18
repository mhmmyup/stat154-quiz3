housing <- read.csv("~/housing.csv", header = TRUE)
attach(housing)
View(housing)
names(housing)
#506/10
#506-51
#?sample

#Question 1

set.seed(82)
sample.set <- sample(506,455)
housing.train <- housing[sample.set,]
housing.test <- housing[-sample.set,]

detach(housing)
attach(housing.train)

#Question 2

cor.matrix <- cor(housing.train)

save(housing, housing.test, housing.train, cor.matrix,
     file = '~/stat154-quiz3/data/data.RData')

#455/5

#lm(MEDV ~ LSTAT)
examp <- lm(MEDV ~ LSTAT)
examp.summ <- summary(examp)
#plot(LSTAT, MEDV)
#abline(lm(MEDV ~ LSTAT))

#lm(MEDV ~ RM + LSTAT)
#summary(lm(MEDV ~ RM + LSTAT))


#Question 3
#Best Subset Selection

#z <- matrix(1:24, nrow = 2, ncol = 12)
#z <- matrix(nrow = 12, ncol = 12)

#lm(MEDV ~ housing[,i+1])
#housing.train2[,i][1:6]

#names(housing.train2)[i]

#zzz <- summary(lm(MEDV ~ LSTAT))
#zzz$r.squared

#interim <- lm(MEDV ~ housing.train2[,i:(length(i:j))])

#interim.names <- paste(names(housing.train2[i:length(i:j)]), collapse = " + ") 
#paste("MEDV ~ ", interim.names, sep = "")
#as.formula(paste("MEDV ~ ", interim.names, sep = ""))

#View(housing.train2)
#View(housing.train3)

#length(combn(names(housing.train3), 2))/j

interim.models <- matrix(nrow = 13, ncol = 12)
housing.train2 <- housing.train[, 2:14]
housing.train3 <- housing.train2[,1:12]

x <- list(1:12)
y <- list(1:12)

#Best Subset selection algorithm.
#Output:

# x, a list, storing each 'best' lm object for each number of predictors according to R2
# y, a list, the summaries of each lm object in x
# interim.models, a matrix of each successive best predictor combination and its R2 value

for(j in 1:12){
  
  z <- matrix(nrow = j + 1, ncol = choose(12,j))

for(i in 1:choose(12,j)){
  
  names.comb <- combn(names(housing.train3), j)
  f <- 1:(length(names.comb)/j)
  
  interim.names <- paste(names.comb[,f[i]], collapse = " + ") 
  interim <- lm(as.formula(paste("MEDV ~ ", interim.names, sep = "")))
  interim.summ <- summary(interim)
  z[,i] <- c(names.comb[,f[i]],interim.summ$r.squared)
  
}
  
  first <- z[,which.max(z[j+1,])]
  
    if(length(first) < 13){
      second <- (length(first)+1):13
      second <- NA[1:length(second)]
      
      interim.models[,j] <- c(first, second)}
  
    if(length(first) == 13){
      
      interim.models[,j] <- first}
  
  abc <- paste(first[1:j], collapse = " + ") 
  x[[j]] <- lm(as.formula(paste("MEDV ~ ", abc, sep = "")))
  y[[j]] <- summary(x[[j]])
  
}


#Question 3 part 2: Model selection


#CP Function
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

cp.vect <- as.matrix(cp.lm(x))
rownames(cp.vect) <- 1:12
cp.min <- cp.vect[which.min(cp.vect),]

#cp gives the 10 predictor model

bic.vect <- as.matrix(sapply(x,BIC))
rownames(bic.vect) <- 1:12
bic.min <- bic.vect[which.min(bic.vect),]

#BIC gives the 6 predictor model

adj.rsq <- as.matrix(sapply(y, function(a) { a$adj.r.sq }))
rownames(adj.rsq) <- 1:12
adj.max <- adj.rsq[which.max(adj.rsq),]

#adjusted r squared gives the 10 predictor model

#lm.cv5 <- lm(MEDV ~ LSTAT, data=housing.train2[-ind.CV[[i]], ])

set.seed(981)
CV5 <- matrix(nrow = 12, ncol = 5)
n <- nobs(x[[1]])
ind.CV <- split(sample(1:n), f = rep(1:5, each = (n/5)))

for(k in 1:12){
for (i in 1:5) {
  
  model.names <- paste(interim.models[1:k,k], collapse = " + ") 
  cv.model <- lm(as.formula(paste("MEDV ~ ", model.names, sep = "")),
                 data=housing.train2[-ind.CV[[i]], ])
  
  CV5[k,i] <- mean((MEDV[ind.CV[[i]]] - 
                  predict(cv.model, newdata=housing.train2[ind.CV[[i]], ]))^2)
}
}

cv.vect <- as.matrix(apply(CV5, 1, mean))
rownames(cv.vect) <- 1:12
cv.min <- cv.vect[which.min(cv.vect),]

#5-fold cross validation chooses the 10 predictor model

#Only BIC gives a different response, the 6 predictor model. Otherwise, all others say 10.

x[[10]]

#is apparently the best possible model for the data.

save(x, y, interim.models, cp.vect, cp.min, bic.vect, bic.min, adj.rsq, adj.max, 
     cv.vect, cv.min, file = '~/stat154-quiz3/data/bssdata.RData')


#Question 4
#Forward Selection

interim.models2 <- matrix(nrow = 13, ncol = 12)

xx <- list(1:12)
yy <- list(1:12)

names.avail <- names(housing.train3)
names.sofar <- NA

#Forward selection algorithm.
#Output:

# xx, a list, storing each 'best' lm object for each added predictor according to R2
# yy, a list, the summaries of each lm object in x
# interim.models2, a matrix of each best added predictor combination and its R2 value

for(j in 1:12){
  
  z <- matrix(nrow = j + 1, ncol = 12)
  
  for(i in 1:(13-j)){
    
    if(is.na(names.sofar[1])){
      interim.names <- names.avail[i]
    } else {
    interim.names <- paste(names.sofar, names.avail[i], sep = " + ", collapse = " + ") }
    
    interim <- lm(as.formula(paste("MEDV ~ ", interim.names, sep = "")))
    interim.summ <- summary(interim)
    
    if(is.na(names.sofar[1])){
      z[,i] <- c(names.avail[i], interim.summ$r.squared)
    } else {
    z[,i] <- c(c(names.sofar, names.avail[i]),interim.summ$r.squared)}
    
  }
  
  first <- z[,which.max(z[j+1,])]
  
  if(length(first) < 13){
    second <- (length(first)+1):13
    second <- NA[1:length(second)]
    
    interim.models2[,j] <- c(first, second)}
  
  if(length(first) == 13){
    
    interim.models2[,j] <- first}
  
  abc <- paste(first[1:j], collapse = " + ") 
  xx[[j]] <- lm(as.formula(paste("MEDV ~ ", abc, sep = "")))
  yy[[j]] <- summary(x[[j]])
  
  if(!is.na(names.sofar[1])) {
    names.sofar <- append(names.sofar, first[j])}
  if(is.na(names.sofar[1])) { names.sofar <- first[j]}
  
  names.avail <- setdiff(names(housing.train3), names.sofar)
}


#Question 4 part 2
#Model Selection


#Use CP calculation function as created above.

cp.vect.for <- as.matrix(cp.lm(xx))
rownames(cp.vect.for) <- 1:12
cp.min.for <- cp.vect.for[which.min(cp.vect.for),]

#cp gives the 10 predictor model

bic.vect.for <- as.matrix(sapply(xx, BIC))
rownames(bic.vect.for) <- 1:12
bic.min.for <- bic.vect.for[which.min(bic.vect.for),]

#BIC gives the 6 predictor model

adj.rsq.for <- as.matrix(sapply(yy, function(a) { a$adj.r.sq }))
rownames(adj.rsq.for) <- 1:12
adj.max.for <- adj.rsq.for[which.max(adj.rsq.for),]

#adjusted r squared gives the 10 predictor model

#lm.cv5 <- lm(MEDV ~ LSTAT, data=housing.train2[-ind.CV[[i]], ])

set.seed(983)
CV5 <- matrix(nrow = 12, ncol = 5)
n <- nobs(xx[[1]])
ind.CV <- split(sample(1:n), f = rep(1:5, each = (n/5)))

for(k in 1:12){
  for (i in 1:5) {
    
    model.names <- paste(interim.models2[1:k,k], collapse = " + ") 
    cv.model <- lm(as.formula(paste("MEDV ~ ", model.names, sep = "")),
                   data=housing.train2[-ind.CV[[i]], ])
    
    CV5[k,i] <- mean((MEDV[ind.CV[[i]]] - 
                        predict(cv.model, newdata=housing.train2[ind.CV[[i]], ]))^2)
  }
}

cv.vect.for <- as.matrix(apply(CV5, 1, mean))
rownames(cv.vect.for) <- 1:12
cv.min.for <- cv.vect.for[which.min(cv.vect.for),]

#5-fold CV gives the 10 predictor model

#while these may give superficially same results, when you observe

x[[10]]

#vs.

xx[[10]]

#You can see that, while the predictors are the same, they were added in a different order!
#Of course, the conclusion here is still the same. forward selection gives us the same
#10 predictor model as best subset selection, and if it is the literal best subset possible,
#it's pretty cool that we were able to get it with forward selection,
#a method of significantly less computational runtime!



save(xx, yy, interim.models2, cp.vect.for, cp.min.for, bic.vect.for, bic.min.for, 
     adj.rsq.for, adj.max.for, cv.vect.for, cv.min.for, 
     file = '~/stat154-quiz3/data/fwddata.RData')


#Question 5

#install.packages("glmnet")
require(glmnet)

#model.matrix(MEDV ~ ., housing.train2)
#housing.train3

ridge.reg <- glmnet(model.matrix(MEDV ~ ., housing.train2), MEDV, alpha = 0)
#summary(ridge.reg)

#ridge.reg$lambda[50]
#coef(ridge.reg)[,50]
#ridge.reg$lambda[60]
#coef(ridge.reg)[,60]

ridge.cv <-  cv.glmnet(model.matrix(MEDV ~ ., housing.train2), MEDV, alpha = 0)
plot(ridge.cv)
ridge.cv$lambda.min

#this gives us the minimizing lamba of .7445355

lasso.reg <- glmnet(model.matrix(MEDV ~ ., housing.train2), MEDV, alpha = 1)
#lasso.reg
lasso.cv <-  cv.glmnet(model.matrix(MEDV ~ ., housing.train2), MEDV, alpha = 1)
plot(lasso.cv)
lasso.cv$lambda.min
#lasso.cv

#this gives us the minimizing lambda of .01604053


save(ridge.reg, ridge.cv, lasso.reg, lasso.cv, file = "~/stat154-quiz3/data/shrinkdata.RData")

#Question 5

#Best subset and Forward selection both gave us the 10 predictor model as a favorite, with the
#BIC the sole believer in a 6 predictor model both times. Both the ridge and lasso regressions
#are quite different from this 10 predictor model. So let's try the 10 predictor and the ridge
#and lasso models. 


#Have to do this manipulation for x data to work in glmnet
x.matrix <- model.matrix(MEDV ~ ., housing.train2)

#Our three regression functions
x[[10]]
ridge.lamb <- glmnet(x.matrix,MEDV, alpha = 0, lambda = ridge.cv$lambda.min)
ridge.lamb$beta

lasso.lamb <- glmnet(x.matrix,MEDV, alpha = 1, lambda = lasso.cv$lambda.min)
lasso.lamb$beta

test.matrix <- model.matrix(housing.test$MEDV ~., housing.test)

ridge.pred <- predict(ridge.reg, s = ridge.cv$lambda.min, 
                      newx = test.matrix[,-2])

length(ridge.pred)==length(housing.test$MEDV)

#Residuals of predicted - actual
ridge.pred - housing.test$MEDV

mean((ridge.pred - housing.test$MEDV)^2)

#MSE of ridge: 24.45394

lasso.pred <- predict(lasso.reg, s = lasso.cv$lambda.min, 
                      newx = test.matrix[,-2])

length(lasso.pred)==length(housing.test$MEDV)

#Residuals of predicted - actual
lasso.pred - housing.test$MEDV
mean((lasso.pred - housing.test$MEDV)^2)

#MSE of lasso: 24.17735

#names(x[[10]])
#terms(x[[10]])
#gives us: MEDV ~ CRIM + ZN + CHAS + NOX + RM + DIS + RAD + TAX + PTRATIO + LSTAT

bss.pred <- predict(x[[10]], newdata = housing.test)
length(bss.pred) == length(housing.test$MEDV)
bss.pred - housing.test$MEDV
mean((bss.pred - housing.test$MEDV)^2)

#MSE of BSS: 23.77013

#for kicks, we can test the 6 predictor model because the BIC wants us to.

bss.pred6 <- predict(x[[6]], newdata = housing.test)
length(bss.pred6) == length(housing.test$MEDV)
bss.pred6 - housing.test$MEDV
mean((bss.pred6 - housing.test$MEDV)^2)

#As suspected, it's not nearly as good as the other 3. 
#MSE = 28.63179

#Thus, our 3 best models are all relatively similar, with the BSS regression function
#slightly edging ridge and lasso out for lowest MSE. 

save(housing.test, ridge.lamb, lasso.lamb, ridge.pred, lasso.pred, bss.pred, bss.pred6, 
     file = '~/stat154-quiz3/data/preddata.RData')




