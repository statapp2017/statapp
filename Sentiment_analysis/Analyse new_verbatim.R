setwd("D:/ENSAE/2emeannee/Statsapp")
source("Fonction_principale.R")
source("Sentiment_analysis/Models_xgboost.R")
source("Sentiment_analysis/Bagging.R")
source("Sentiment_analysis/Sentiment_visualisation.R")
source("Sentiment_analysis/Quality_measurement.R")
  
dtm_ep<-readRDS("dtm_ep.RDS")
so_ge<-read.csv2("verbatims_SRC_230118_ENSAE.csv")
so_ge_prevoyance<-so_ge[so_ge$REPRISE_ACTIVITE=="Epargne",]
parameter<-read.csv2("logloss.csv")
params<-parameter[parameter$logloss==max(parameter$logloss),2:ncol(parameter)]
colonne<-"recommandation_SGK"
number_models<-4
number_class<-3
so_ge_epargne<-so_ge[so_ge$REPRISE_ACTIVITE!="Epargne",]

model_sentiment<-models_xgboost(params,dtm_ep,so_ge_prevoyance,colonne,number_class,number_models)
model_theme <- readRDS("model_theme.RDS")
nom_colonnes<-model_sentiment$nom_colonne
dvalid<-model_sentiment$dvalid
prediction<-bagging_xgboost_prediction(as.matrix(dvalid[,2:ncol(dvalid)]),model_sentiment$models,number_class)
help(auc)
plot_confusion_matrix(make_prediction(prediction),model_sentiment$notes)
measure_quality(prediction,model_sentiment$notes)

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
    sentiments_results_visualisation(dtm_sentiment,model_sentiment)
    list(topics=pos,sentiment=prediction)
}

so_ge_epargne[345,"raisons_recommandation"]
analyse_new_verbatim(so_ge_epargne[3:5,"raisons_recommandation"])
importance<-bagging_xgboost_importance(model_sentiment,dtm_ep)
xgb.ggplot.importance(as.data.table(importance),top_n = 50)
