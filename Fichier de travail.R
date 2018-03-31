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

# Topic Processing
model_theme <- give_theme(dtm)
visualise_LDA(model_theme, dtm) # Visualisation


