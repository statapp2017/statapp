library(xgboost)
library(tm)

gestion_dtm<-function(dtm,bigrams,nom_colonnes){
  dtm<-as.data.frame(as.matrix(dtm))
  bigrams<-as.data.frame(as.matrix(bigrams))
  dtm<-cbind(dtm,bigrams)
  a_enlever<-colnames(dtm)[which(!colnames(dtm) %in% nom_colonnes)]
  a_rajouter<-nom_colonnes[which(!nom_colonnes %in% colnames(dtm))]
  if(length(a_enlever)>1){
    dtm$mots_supplementaire<-rowSums(dtm[,colnames(dtm) %in% a_enlever])
    dtm<-dtm[,!colnames(dtm) %in% a_enlever]
  }
  else{
    dtm$mots_supplementaire<-rep(0,nrow(dtm))
  }
  for (mots in a_rajouter){
    dtm[,mots]<-rep(0,nrow(dtm))
  }
  dtm<-dtm[,order(colnames(dtm))]
  dtm
}

train_model_xgboost<-function(params,train_test,dvalid,number_models,train_test_split){
  eta<-params$eta
  params$objective<-"multi:softprob"
  models<-vector("list",number_models)
  for (i in 2:ncol(train_test)){
    train_test[,i]<-sapply(train_test[,i],as.numeric)
  }
  train_test[,1]<-sapply(train_test[,1],as.factor)
  division<-vector("list",number_models)
  for(i in 1:number_models){
    sample2 <- sample.int(n = nrow(train_test), size = floor(train_test_split*nrow(train_test)), replace = F)
    train_t <- xgb.DMatrix(data = as.matrix(train_test[sample2,2:ncol(train_test)]),label = as.matrix(train_test[sample2,1])) 
    models[[i]]<-xgb.train(data=train_t,nrounds=10/eta,params,watchlist = list(train=train_t,dvalid=dvalid),verbose=0,early_stopping_rounds=20)
    division[[i]]<-sample2
  }
  list(models=models,entrainement=division)
}

models_xgboost<-function(params,dtm_ep,data,column,number_class,number_models,train_test_split=.8,train_valid_split=.8){
  params$num_class<-number_class
  data_tot<-cbind(data[,column],as.data.frame(as.matrix(dtm_ep)))
  colnames(data_tot)<-c("categorie",colnames(dtm_ep))
  data_tot$categorie<-cut(data_tot$categorie,breaks=number_class,labels=as.factor(0:(number_class-1)))
  data_tot[is.na(data_tot$categorie),"categorie"]<-0
  sample1 = sample.int(n = nrow(data_tot), size = floor(train_valid_split*nrow(data_tot)), replace = F)
  train_test<-data_tot[sample1,]
  valid<-data_tot[-sample1,] 
  dvalid<-xgb.DMatrix(data = as.matrix(valid[,2:ncol(data_tot)]),label = as.matrix(valid[,1]))
  mod<-train_model_xgboost(params,train_test,dvalid,number_models,train_test_split)
  list(models=mod$models,dvalid=valid,place=mod$entrainement,notes=data_tot[-sample1,1],train_test=train_test,nom_colonne=colnames(dtm_ep))
}