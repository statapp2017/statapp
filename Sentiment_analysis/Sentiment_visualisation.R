library(lime)
setwd("D:/ENSAE/2emeannee/Statsapp")
source("lime-master/R/explain.R")
source("lime-master/R/dataframe.R")


sentiments_results_visualisation<-function(new_data,list_models){
  new_data<-as.data.frame(new_data)
  train_test<-list_models$train_test
  explanation<-vector("list",length(list_models$models))
  explainer_xgb <- lime(train_test[,2:ncol(train_test)],list_models$models,bin_continuous=FALSE)
  new_data<-new_data[,colnames(new_data)!="mots_supplementaire"]
  ex_xgb<-explain(new_data,explainer_xgb,n_labels=3,n_features=10)
  print(plot_features(ex_xgb,ncol=3))
}
