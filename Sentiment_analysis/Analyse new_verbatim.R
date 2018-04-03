library(topicmodels) 
source("Sentiment_analysis/Bagging.R")
source("5_Create DTM.R")

#' Gives the probabilities of each observation to be in a subject and in a class of 
#' sentiment
#' @param new_verbatim A list of new verbatims to analyse.
#' @return A list which contains the probability of each observation of new_verbatim to 
#' belong to a subject and to a class of sentiment

analyse_new_verbatim<-function(new_verbatim){
    donnees<-data.frame(text=new_verbatim)
    sentiment<-model_sentiment$models
    params<-sentiment[[1]]$params
    number_class<-params$num_class
    result_preprocess<-preprocess_text(donnees,"text")
    dtm<-gestion_dtm(result_preprocess$dtm,result_preprocess$tdm,nom_colonnes)
    dtm_sentiment<-as.matrix(dtm)
    prediction<-bagging_xgboost_prediction(dtm_sentiment,sentiment,number_class = number_class)
    posterior_distribution <- posterior(model_theme,newdata= dtm)
    pos<-posterior_distribution$topics 
    list(topics=pos,sentiment=prediction)
}
