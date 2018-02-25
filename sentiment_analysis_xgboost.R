# BEST PARAMETERS FOR XGBOOST ACCORDING TO PREVIOUS TEST

rm(list = ls())
#install.packages("xgboost",dependencies = TRUE)
library(xgboost)
library(ggplot2)
library(tm)
library(MLmetrics)

#load test results
setwd("D:/ENSAE/2emeannee/Statsapp")
table<-read.csv('tableau_calibrage.csv')
dtm_ep <- readRDS(file = "dtm_epargne.RDS")
verbatim <- read.csv2("verbatims_SRC_230118_ENSAE.csv", encoding = "latin1")
epargne <- subset(verbatim, REPRISE_ACTIVITE == "Epargne")

creation_class<-function(x,number_class){
  if (number_class=="2"){
    if (x <6){
      x<-0
    }
    else{
      x<-1
    }
  }
  if (number_class=="3"){
    if (x<5){
      x<-0
    }
    if (x %in% 5:7){
      x<-1
    }
    if(x>7){
      x<-2
    }
  }
  x
}

get_data_xgboost<-function(dtm_ep){
  d1 <- as.data.frame(as.matrix(dtm_ep))
  data_tot<-cbind(epargne$recommandation_SGK,d1)
  colnames(data_tot)<-c("recommandation_SGK",colnames(d1))
  data_tot$recommandation_SGK<-sapply(data_tot$recommandation_SGK,FUN=function(x){creation_class(x,3)})
  sample1 = sample.int(n = nrow(data_tot), size = floor(.8*nrow(data_tot)), replace = F)
  train<-data_tot[sample1,]
  test<-data_tot[-sample1,]
  sample2 = sample.int(n = nrow(train), size = floor(.8*nrow(train)), replace = F)
  train_t = train[sample2, ] 
  valid  = train[-sample2, ] 
  dtrain<-xgb.DMatrix(data = as.matrix(train_t[,2:ncol(data_tot)]),label = as.matrix(train_t[,1])) 
  dvalid<-xgb.DMatrix(data = as.matrix(valid[,2:ncol(data_tot)]),label = as.matrix(valid[,1]))
  watchlist = list(train = dtrain, valid = dvalid)
  list(test,watchlist)
}

search_best_config<-function(number_class,dtm_ep){
  eta_par <- 0.1
  nrounds_par <- 5 / eta_par
  # XGB.TRAIN WITH PREVIOUS PARAMETER + test on eta 
  watchlist<-get_data_xgboost(dtm_ep)[[1]]
  dtrain<-watchlist$train
  Logloss_list<-c()
  param<-data.frame(eta=c(),max_depth=c(),subsample=c(),colsample_bytree=c(),objective=c(),eval_metric=c(),num_class=c(),nthread=c(),tree_method=c(),silent=c())
  for(depth_par in seq(6, 14, by = 2)) {
    for(subsample_par in seq(0.1, 1, by = 0.1)) {
      for(colsample_bytree_par in seq(0.1, 1, by = 0.1)) {
          bstSparse <- xgb.train(data = dtrain,eta = eta_par, max_depth = depth_par, subsample = subsample_par,
                                     colsample_bytree = colsample_bytree_par,objective = "multi:softmax",eval_metric="mlogloss",
                                 num_class = number_class,nrounds = nrounds_par,nthread = 2, tree_method = "auto", 
                                 watchlist = watchlist,verbose=0,early_stopping_rounds=50)
          Logloss_list<-append(Logloss_list,bstSparse$best_score)
          param<-rbind(param,bstSparse$params)
      }
    }
  }
  data<-cbind(data.frame(logloss=Logloss_list),param)
  data
}
#t<-search_best_config(3,dtm_ep)
#write.csv2(params[,2:ncol(params)],"logloss.csv",row.names = FALSE)
# XGB.TRAIN WITH PREVIOUS PARAMETER + test on eta 
params<-read.csv2("logloss.csv")
predictor<-funtion(params,dtm_ep){
eta<-params[params$logloss==min(params$logloss),2:ncol(params)]$eta
xgb.train(data=dtrain,nrounds=5/eta,t[t$logloss==min(t$logloss),2:ncol(t)])
data<-get_data_xgboost(dtm_ep)
watchlist<-data[[2]]
dtrain<-watchlist$train
test<-data[[1]]
}

pred <- predict(bstSparse, xgb.DMatrix(data = as.matrix(data_tot[(n+1):nrow(data_tot),2:ncol(data_tot)])))
bstSparse
