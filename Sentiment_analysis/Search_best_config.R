library(magrittr)
library(xgboost)
library(data.table)

search_best_config<-function(donnees,number_class,dtm_ep,colonne){
  nom_colonnes<-c(dtm_ep$dimnames$Terms,"categorie")
  dtm_ep<-as.data.frame(as.matrix(dtm_ep))%>%cbind(donnees[,colonne])
  colnames(dtm_ep)<-nom_colonnes
  breaking<-unique(quantile(dtm_ep$categorie, probs = seq(0, 1, by = round(1/number_class,digits=2))))
  dtm_ep$categorie<-cut(dtm_ep$categorie,breaks=breaking,labels=as.factor(0:(length(breaking)-2)))
  dtm_ep[is.na(dtm_ep$categorie),"categorie"]<-0
  eta_par <- 0.1
  nrounds_par <- 5 / eta_par
  # XGB.TRAIN WITH PREVIOUS PARAMETER + test on eta 
  sample2 <- sample.int(n = nrow(dtm_ep), size = floor(.8*nrow(dtm_ep)), replace = F)
  dtrain <- xgb.DMatrix(data = as.matrix(dtm_ep[sample2,1:(ncol(dtm_ep)-1)]),label = as.matrix(dtm_ep[sample2,ncol(dtm_ep)])) 
  dvalid<-xgb.DMatrix(data = as.matrix(dtm_ep[-sample2,1:(ncol(dtm_ep)-1)]),label = as.matrix(dtm_ep[-sample2,ncol(dtm_ep)])) 
  watchlist<-list(eval=dvalid,train=dtrain)
  param<-data.frame(eta=c(),max_depth=c(),subsample=c(),colsample_bytree=c(),objective=c(),eval_metric=c(),num_class=c(),nthread=c(),tree_method=c(),silent=c())
  parametrage<-expand.grid(depth_par=seq(6, 14, by = 2),subsample_par=seq(0.1, 1, by = 0.1),colsample_bytree_par=seq(0.1, 1, by = 0.1))
  Logloss_list<-vector("list",length(parametrage))
  for (i in 1:nrow(parametrage)){
      bstSparse <- xgb.train(data = dtrain,eta = eta_par, parametrage[i,],objective = "multi:softmax",eval_metric="mlogloss",
                               num_class = number_class,nrounds = nrounds_par,nthread = 2, tree_method = "auto", 
                               watchlist = watchlist,verbose=0,early_stopping_rounds=50)
      Logloss_list[i]<-bstSparse$best_score
      param<-rbind(param,bstSparse$params)
      }
  data<-cbind(data.frame(logloss=Logloss_list),param)
  data
}
