library(xgboost)

bagging_xgboost_prediction<-function(list_models,dtm_ep,number_class){
  mod<-list_models$models
  dvalid<-list_models$dvalid
  model<-mod[[1]]
  prediction<-predict(model,dvalid,reshape=TRUE)
  for (i in 2:length(mod)){
    model<-mod[[i]]
    prediction_inter<-data.frame(predict(model,dvalid,reshape=TRUE))
    prediction<-cbind(prediction,prediction_inter)
  }
  k<-ncol(prediction)/number_class
  for (i in (1:number_class)){
    prediction[,paste("prob_class",as.character(i),sep="_")]<-rowMeans(prediction[,i-number_class+number_class*(1:k)])
  }
  prediction[,(ncol(prediction)-number_class+1):ncol(prediction)]
}

bagging_xgboost_importance<-function(list_models,dtm_ep){
  mod<-list_models$models
  model<-mod[[1]]
  importance<-xgb.importance(colnames(dtm_ep),model=model)
  for (i in 2:length(mod)){
    model<-mod[[i]]
    importance_inter<-xgb.importance(colnames(dtm_ep),model = model)
    importance<-merge(importance,importance_inter,by="Feature",suffixes=c(as.character(i),as.character(i-1)))
  }
  importance<-xgb.importance(colnames(dtm_ep),model = model)
  importance_bagging<-data.frame(Feature=importance$Feature)
  importance_bagging$Gain<-rowMeans(subset(importance,select=colnames(importance)[str_detect(colnames(importance),"Gain")]))
  importance_bagging$Cover<-rowMeans(subset(importance,select=colnames(importance)[str_detect(colnames(importance),"Cover")]))
  importance_bagging$Frequency<-rowMeans(subset(importance,select=colnames(importance)[str_detect(colnames(importance),"Frequency")]))
  importance_bagging
}

make_prediction<-function(prediction){
  pred<-vector("list",nrow(prediction))
  for (i in 1:nrow(prediction)){
    pred[i]<-(which(prediction[i,]==max(prediction[i,]))-1)
  }
  pred<-unlist(pred)
  pred
}