library(xgboost)
library(stats)

#' Gives the prediction of a list of models as the mean of the probabilty  
#' predicted by the different models
#' @param dtm_newdata The dataframe of the Document Term Matrix obtained on 
#' the new data
#' @param list_models The list of models. A list of xgb.train obtained with 
#' models_xgboost
#' @param number_class The number of class the marks are divided in. 
#' @return A new dataframe with the average prediction of the models

bagging_xgboost_prediction<-function(dtm_newdata,list_models,number_class){
  model<-list_models[[1]]
  prediction<-predict(model,dtm_newdata,reshape=TRUE)
  for (i in 2:length(mod)){
    model<-list_models[[i]]
    prediction_inter<-data.frame(predict(model,dtm_newdata,reshape=TRUE))
    prediction<-cbind(prediction,prediction_inter)
  }
  k<-ncol(prediction)/number_class
  for (i in (1:number_class)){
    prediction[,paste("prob_class",as.character(i),sep="_")]<-rowMeans(prediction[,i-number_class+number_class*(1:k)])
  }
  prediction[,(ncol(prediction)-number_class+1):ncol(prediction)]
}

#' Gives the importance of a list of models as the mean of the importance
#' given by the different models
#' @param dtm_ep The dataframe of the Document Term Matrix obtained on 
#' the original database.
#' @param list_models The list of models. A list of xgb.train obtained with 
#' models_xgboost
#' @return A new dataframe with the average features importance of the models

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

#' Gives the label predicted based on the probabilty in prediction
#' @param prediction A dataframe containing n columns where n is the number of 
#' classes with the probability of each document to be in a class
#' @return A list given the label predicted of each observation

make_prediction<-function(prediction){
  pred<-vector("list",nrow(prediction))
  for (i in 1:nrow(prediction)){
    pred[i]<-(which(prediction[i,]==max(prediction[i,]))-1)
  }
  pred<-unlist(pred)
  pred
}
