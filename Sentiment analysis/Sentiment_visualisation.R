library(lime)

sentiments_results_visualisation<-function(list_models){
  train_test<-list_models$train_test
  explanation<-vector("list",length(list_models$models))
  for (i in 1:length(list_models$models)){
    explainer_xgb <- lime(train_test[list_models$place[[i]],2:ncol(train_test)],list_models$models[[i]],bin_continuous=FALSE)
    ex_xgb<-explain(train_test[1,2:ncol(train_test)],explainer_xgb,n_labels=3,n_features=10)
    explanation[[i]]<-ex_xgb
  #print(plot_features(ex_xgb,ncol=3))
  #print(plot_explanations(ex_xgb))
  }
  explanation
}
