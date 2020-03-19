rm(list = ls())
gc()
require(tidyverse)
require(ranger)
require(pROC)
require(ggplot2)
require(ggridges)
require(bartMachine)
require(grf)
set.seed(102182)


#################################################################################
#################################################################################
##
##  Effect of bootstrapping on random forest performance
##
#################################################################################
#################################################################################


# Model performance according to OOB metrics for ranger
data(iris)
rang.class <- ranger(Species ~ ., data = iris)
cat("Classification Error:",rang.class$prediction.error,"\n")

rang.prob <- ranger(Species ~ ., data = iris,probability = T)
cat("Brier's Score:",round(rang.prob$prediction.error,2),"\n")



# Model performance according to bootstrapping with and without replacement
perf.bs <- NULL
for(i in 1:50) {
  # replace = T
    inds <- sample(1:nrow(iris),nrow(iris),replace = T)
    rang.bsT <- ranger(Species ~., data = iris[inds,],probability = T)
    bart.bsT <- bartMachine(X = iris[inds,-5],y = as.integer(as.factor(iris[inds,5])),verbose = F)
    grf.bsT <- regression_forest(X = iris[inds,-5],Y = as.integer(as.factor(iris[inds,5])))
    grf.bsT.err <- sqrt(mean((as.integer(as.factor(iris[inds,5])) - predict(grf.bsT)$predictions)^2))
    
  # replace = F
    inds <- sample(1:nrow(iris),round(nrow(iris)*.6),replace = F)
    rang.bsF <- ranger(Species ~., data = iris[inds,],probability = T)
    bart.bsF <- bartMachine(X = iris[inds,-5],y = as.integer(as.factor(iris[inds,5])),verbose = F)
    grf.bsF <- regression_forest(X = iris[inds,-5],Y = as.integer(as.factor(iris[inds,5])))
    grf.bsF.err <- sqrt(mean((as.integer(as.factor(iris[inds,5])) - predict(grf.bsF)$predictions)^2))
    
    perf.bs <- bind_rows(perf.bs,
                         data.frame(err = c(sqrt(rang.bsT$prediction.error),
                                            sqrt(rang.bsF$prediction.error)),
                                    bsType = c("replace=T","replace=F"),
                                    meth = "ranger"),
                         data.frame(err = c(bart.bsT$rmse_train,bart.bsF$rmse_train),
                                    bsType = c("replace=T","replace=F"),
                                    meth = "bartMachine"),
                         data.frame(err = c(grf.bsT.err,grf.bsF.err),
                                    bsType = c("replace=T","replace=F"),
                                    meth = "grf"))
    cat(i,"\n")
}

perf.bs %>% 
  ggplot(aes(x = err,y = bsType,fill = meth)) +
  geom_density_ridges(scale = 2,rel_min_height = .001,alpha = .5) +
  theme_ridges() + ylab("") + xlab("")



# Simulated data
n = 2000; p = 10
X = matrix(rnorm(n*p), n, p)
W = rbinom(n, 1, 0.5) 
Y = pmax(X[,1], 0) * W + X[,2] + pmin(X[,3], 0) + rnorm(n)


perf.bs <- NULL
for(i in 1:50) {
  # replace = T
  inds <- sample(1:nrow(X),nrow(X),replace = T)
  rang.bsT <- ranger(Y ~., data = data.frame(X[inds,],Y = Y[inds]))
  bart.bsT <- bartMachine(X = data.frame(X[inds,]),y = Y[inds],verbose = F)
  grf.bsT <- regression_forest(X = X[inds,],Y = Y[inds])
  grf.bsT.err <- sqrt(mean((Y[inds] - predict(grf.bsT)$predictions)^2))
  
  # replace = F
  inds <- sample(1:nrow(X),round(nrow(X)*.6),replace = F)
  rang.bsF <- ranger(Y ~., data = data.frame(X[inds,],Y = Y[inds]))
  bart.bsF <- bartMachine(X = data.frame(X[inds,]),y = Y[inds],verbose = F)
  grf.bsF <- regression_forest(X = X[inds,],Y = Y[inds])
  grf.bsF.err <- sqrt(mean((Y[inds] - predict(grf.bsF)$predictions)^2))
  
  perf.bs <- bind_rows(perf.bs,
                       data.frame(err = c(sqrt(rang.bsT$prediction.error),
                                          sqrt(rang.bsF$prediction.error)),
                                  bsType = c("replace=T","replace=F"),
                                  meth = "ranger"),
                       data.frame(err = c(bart.bsT$rmse_train,bart.bsF$rmse_train),
                                  bsType = c("replace=T","replace=F"),
                                  meth = "bartMachine"),
                       data.frame(err = c(grf.bsT.err,grf.bsF.err),
                                  bsType = c("replace=T","replace=F"),
                                  meth = "grf"))
  cat(i,"\n")
}

perf.bs %>% 
  ggplot(aes(x = err,y = bsType,fill = meth)) +
  geom_density_ridges(scale = 2,rel_min_height = .001,alpha = .5) +
  theme_ridges() + ylab("") + xlab("")
