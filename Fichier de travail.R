rm(list = ls())

### Change here ###
setwd("/Users/okabeshu/Documents/ENSAE/StatApp") # Path of the main folder
name_data <- "verbatims.csv"

### Import data ###
verbatim <- read.csv2(name_data, encoding = "latin1")
epargne <- subset(verbatim, REPRISE_ACTIVITE == "Prévoyance")
dico <- read.table("liste_francais.txt", encoding = "Latin1")
model_theme <- readRDS("model_theme.RDS")
parameter<-read.csv2("logloss.csv")
params<-parameter[parameter$logloss==max(parameter$logloss),2:ncol(parameter)]
colonne<-"recommandation_SGK"
number_models<-20
number_class<-3
dtm<-readRDS("dtm_ep.RDS")
dtm_tot<-readRDS("dtm_tot.RDS")

### Import functions ###
# Preprocessing
source("Preprocessing/1_Description.R")
source("Preprocessing/2_Spell checking.R")
source("Preprocessing/3_Creation dico.R")
source("Preprocessing/4_Lemmatiser.R")
source("Preprocessing/5_Create DTM.R")
source("Preprocessing/6_Graphs.R")
source("Preprocessing/7_Community algorithm.R")

# Topic Processing
source("Topic Processing/1 Number of themes.R")
source("Topic Processing/2 Characteristics.R")
source("Topic Processing/3 TSNE.R")
source("Topic Processing/4 Theme i.R")
source("Topic Processing/5 Model Theme.R")
source("Topic Processing/7 Visualisation.R")

#Sentiment analysis
source("Sentiment_analysis/Bagging.R")
source("Sentiment_analysis/Models_xgboost.R")
source("Sentiment_analysis/Quality_measurement.R")
source("Sentiment_analysis/Sentiment_visualisation.R")

### Test ###
# Preprocessing
describe_corpus(epargne)
par(mfrow = c(2,1), bg="beige")
compar(epargne, "raisons_recommandation", dtm)

# Create DTM
#dtm_test <- preprocess_text(epargne, "raisons_recommandation")
#dtm <- dtm_test$dtm

# Topic Processing
model_theme <- give_theme(dtm)
visualise_LDA(model_theme, dtm) # Visualisation

model_sentiment<-models_xgboost(params,dtm_tot,epargne,colonne,number_class,number_models)
nom_colonnes<-model_sentiment$nom_colonne
dvalid<-model_sentiment$dvalid
prediction<-bagging_xgboost_prediction(as.matrix(dvalid[,2:ncol(dvalid)]),model_sentiment$models,number_class)
plot_confusion_matrix(make_prediction(prediction),model_sentiment$notes)
measure_quality(prediction,model_sentiment$notes)
analyse_new_verbatim(epargne[3:5,"raisons_recommandation"])
importance<-bagging_xgboost_importance(model_sentiment,dtm_tot)
xgb.ggplot.importance(as.data.table(importance),top_n = 50)
a_eliminer<-importance$Feature[1:50]
caracterise(model_theme$phi_t,a_eliminer)
