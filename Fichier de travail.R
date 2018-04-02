rm(list = ls())

### Change here ###
setwd("/Users/okabeshu/Documents/ENSAE/StatApp") # Path of the main folder
name_data <- "verbatims.csv"

### Import data ###
verbatim <- read.csv2(name_data, encoding = "latin1")
epargne <- subset(verbatim, REPRISE_ACTIVITE == "Epargne")
dico <- read.table("liste_francais.txt", encoding = "Latin1")

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


### Test ###
# Preprocessing
#describe_corpus(epargne)
#par(mfrow = c(2,1), bg="beige")
#compar(epargne, "raisons_recommandation", dtm_ep)

# Create DTM
dtm_test <- preprocess_text(epargne, "raisons_recommandation")
dtm <- dtm_test$dtm
dtm_ep<-readRDS("dtm_ep.RDS")
so_ge<-read.csv2("verbatims_SRC_230118_ENSAE.csv")
so_ge_prevoyance<-so_ge[so_ge$REPRISE_ACTIVITE=="Epargne",]
so_ge_epargne<-so_ge[so_ge$REPRISE_ACTIVITE!="Epargne",]
model_theme <- readRDS("model_theme.RDS")
parameter<-read.csv2("logloss.csv")
params<-parameter[parameter$logloss==max(parameter$logloss),2:ncol(parameter)]
colonne<-"recommandation_SGK"
number_models<-4
number_class<-3

# Topic Processing
model_theme <- give_theme(dtm)
visualise_LDA(model_theme, dtm) # Visualisation

model_sentiment<-models_xgboost(params,dtm_ep,so_ge_prevoyance,colonne,number_class,number_models)
nom_colonnes<-model_sentiment$nom_colonne
dvalid<-model_sentiment$dvalid
prediction<-bagging_xgboost_prediction(as.matrix(dvalid[,2:ncol(dvalid)]),model_sentiment$models,number_class)
plot_confusion_matrix(make_prediction(prediction),model_sentiment$notes)
measure_quality(prediction,model_sentiment$notes)
analyse_new_verbatim(so_ge_epargne[3:5,"raisons_recommandation"])
importance<-bagging_xgboost_importance(model_sentiment,dtm_ep)
xgb.ggplot.importance(as.data.table(importance),top_n = 50)
