
gestion_dtm<-function(dtm,nom_colonnes){
  dtm<-as.data.frame(as.matrix(dtm))
  a_enlever<-colnames(dtm)[which(!colnames(dtm) %in%nom_colonnes)]
  if(length(a_enlever)>1){
      dtm$mots_supplementaire<-rowSums(dtm[,colnames(dtm) %in% a_enlever])
      dtm<-dtm[,!colnames(dtm) %in% a_enlever]
    }
  else{
    dtm$mots_supplementaire<-rep(0,nrow(dtm))
  }
  dtm<-dtm[,order(colnames(dtm))]
  dtm
}

creation_class<-function(x,number_class){
  if (number_class==2){
    if (x <6){
      x<-0
    }
    else{
      x<-1
    }
  }
  if (number_class==3){
    if (x<6){
      x<-0
    }
    if (x %in% 6:8){
      x<-1
    }
    if(x>8){
      x<-2
    }
  }
  as.factor(x)
}

get_data_xgboost<-function(dtm_ep,data,number_class,train_test_split=.8,train_valid_split=.8){
  d1 <- as.data.frame(as.matrix(dtm_ep))
  data_tot<-cbind(data$recommandation_SGK,d1)
  colnames(data_tot)<-c("recommandation_SGK",colnames(d1))
  data_tot$recommandation_SGK<-sapply(data_tot$recommandation_SGK,FUN=function(x){creation_class(x,number_class)})
  sample1 = sample.int(n = nrow(data_tot), size = floor(train_test_split*nrow(data_tot)), replace = F)
  train<-data_tot[sample1,]
  test<-data_tot[-sample1,]
  sample2 = sample.int(n = nrow(train), size = floor(train_valid_split*nrow(train)), replace = F)
  train_t = train[sample2,] 
  valid  = train[-sample2,] 
  dtrain<-xgb.DMatrix(data = as.matrix(train_t[,2:ncol(data_tot)]),label = as.matrix(train_t[,1])) 
  dvalid<-xgb.DMatrix(data = as.matrix(valid[,2:ncol(data_tot)]),label = as.matrix(valid[,1]))
  watchlist = list(train = dtrain, valid = dvalid)
  list(valid,train_t,watchlist,sample2,test)
}

search_best_config<-function(number_class,dtm_ep){
  eta_par <- 0.1
  nrounds_par <- 5 / eta_par
  # XGB.TRAIN WITH PREVIOUS PARAMETER + test on eta 
  watchlist<-get_data_xgboost(dtm_ep,number_class)[[2]]
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
parameters<-read.csv2("logloss.csv")
params<-parameters[parameters$logloss==min(parameters$logloss),2:ncol(parameters),]
params$objective<-as.character(params$objective)

bagging<-function(data,column,params,dtm_ep,number_class,nom_colonnes,length_divisor=4,iterations=2){
  dtm_ep<-gestion_dtm(dtm_ep,nom_colonnes)
  predictions<-foreach(m=1:iterations,.export=c("get_data_xgboost"),.packages=c('xgboost'),.combine=cbind) %do% {
    data_tot<-get_data_xgboost(dtm_ep,data,number_class,train_test_split=1,train_valid_split=.8)
    watchlist<-data_tot[[3]]
    new_sample<-data_tot[[4]]
    train_t<-data_tot[[2]]
    test_t<-data_tot[[1]]
    dtest<-watchlist$valid
    dtrain<-watchlist$train
    eta<-params$eta
    training_positions <- sample(nrow(dtrain), size=floor((nrow(dtrain)/2)))
    sub_dtrain<-dtrain[training_positions,]
    bstSparse <- xgb.train(data=sub_dtrain,nrounds=5/eta,params,watchlist = list(train=sub_dtrain,valid=dtest),verbose=0,early_stopping_rounds=50)
    importance<-xgb.importance(colnames(dtm_ep),model = bstSparse)
    print(xgb.ggplot.importance(importance_matrix = importance,top_n = 30,n_clusters = 1))
    pred<-predict(bstSparse,newdata=dtest)
    #new_test<-cbind(pred,test_t)
    #text_correct<-new_test[new_test$pred==new_test$recommandation_SGK,]
    #test<-rbind(rbind(head(text_correct[text_correct[,pred]==0,],2),head(text_correct[text_correct[,pred]==1,],2)),head(text_correct[text_correct[,pred]==2,],2))
    #explainer <- lime(train_t[training_positions,2:ncol(train_t)], bstSparse,bin_continuous=FALSE)
    #explanations <- explain(test[,3:ncol(test)], explainer, n_labels=1, n_features = 20)
    #print(plot_features(explanations),ncol=3)
    t<-multiclass.roc(test_t[,1],pred)
    print(auc(t))
    pred
  }
  num_class<-(params$num_class-1)
  predictions<-as.data.frame(predictions)
  for (col in 1:ncol(predictions)){
    predictions[,col]<-sapply(predictions[,col],as.character)
  }
  prediction<-c()
  for (j in 1:nrow(predictions)){
    pred<-0
    max_sum<-sum(ifelse(predictions[j,]=="0", 1, 0))
    for (i in 1:num_class){
      sum_inter<-sum(ifelse(predictions[j,]==as.character(i), 1, 0))
      if (sum_inter>max_sum){
        pred<-i
        max_sum<-sum_inter
      }
    }
    prediction[length(prediction)+1]<-pred
  }
  t<-multiclass.roc(test_t[,1],prediction)
  print(auc(t))
  prediction
}


predictions<-bagging(so_ge_prevoyance,"recommandation_SGK",params,dtm_ep,3,colnames(dtm_ep))
