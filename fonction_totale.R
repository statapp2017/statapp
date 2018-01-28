rm(list=ls())
library("hunspell")
library("tm")
library("RColorBrewer")
library("wordcloud")
library("stringr")
library("igraph")
library("graphics")
library("koRpus")
library("SnowballC")
library("tokenizers")

setwd("D:/ENSAE/2èmeannée/Statsapp")
#Importation des données
donnees <- read.csv2("Extraction.csv")
#Dictionnaire de mots français pour appuer hunspell
dico <- read.table("D:/Téléchargement/liste_francais.txt")

correcteur_orthographique1<- function(phrase){
  #Séparation de la phrase en mots 
  liste <- tokenize_words(as.character(gsub(" \n", ". ",phrase)), lowercase = TRUE)[[1]]
  chaine<- ""
  for (x in liste){
    #On regarde si l'orthographe du mots est correct
    if (hunspell_check(x,dict=dictionary("fr"))|str_length(x)==1){
      chaine <- paste(chaine,x)
    }
    else{
      #Si l'orthographe n'est pas juste on extrait les corrections possibles
      suggestion<-hunspell_suggest(x, dict = dictionary("fr"))[[1]]
      vrai <- TRUE
      i <- 1
      while (vrai & i<5){
        #On vérifie que le mot est français avant de le remplacer dans la phrase
        if (suggestion[i] %in% dico[[1]]){
          chaine <- paste(chaine,suggestion[i])
          vrai <- FALSE
        }
        else{
          i <- i+1
        }
      }
      #Si on ne trouve pas de correction française alors on laisse tel quel
      if (i==5){
        chaine <- paste(chaine,x)
      }
    }
  }
  chaine
}

#Application de la correction orthographique au dataframe
correction_dataframe <- function(dataframe,colonne){
  ref <- dataframe[[colonne]]
  for (i in c(1:nrow(dataframe))){
    dataframe$corrige[i]<-correcteur_orthographique1(ref[i])
  }
  dataframe
}

lemmatizer<-function(file,adresse){
  #Lemmatisation et part-of-speech tagging du texte
  text <- data.frame(treetag(file,treetagger = "manual",TT.tknz=FALSE,lang="fr",encoding="Latin1",TT.options=list(path=adresse,preset = "fr"))@TT.res[,c(1,2,3)])
  lemme <- ""
  #On récupère la phrase lemmatiser
  for (i in c(1:nrow(text))){
    if (text$lemma[i]=="<unknown>"){
      lemme <- paste(lemme,text$token[i])
    }
    else{
      liste <- str_split(text$lemma[i],"")[[1]]
      if ("|" %in% liste){
        for (j in c(1:length(liste))){
          if (liste[j]=="|"){
            reference <- j+1
          }
        }
        lemme <- paste(lemme,str_sub(text$lemma[i],reference))
      }
      else{
        lemme <- paste(lemme,text$lemma[i])
      }
    }
  }
  lemme <- str_replace_all(lemme,"Ãª","ê")
  lemme <- str_replace_all(lemme," x \" \" @card@ " ,"")
  lemme <- str_replace_all(lemme,"\"","")
  lemme <- str_replace_all(lemme,"\ \"","")
  #On récupère maintenant la catégorie grammaticale de l'ensemble des mots de la phrase
  v <- array(dim = c(1,nrow(text)-8))
  for (i in 8:nrow(text)){
    v[i-7] <- text$tag[i]
  }
  names(v) <- text$token[c(8:nrow(text))]
  return (list(lemme,v))
}

#Application au dataframe
lemmatizer_dataframe <- function(data,adresse,colonne){
  colref <-data[[colonne]]
  for (i in 1:nrow(data)){
    file.create(paste(toString(i),".txt",sep=""))
    write.table(tolower(colref[i]),file=paste(toString(i),".txt",sep=""),fileEncoding = "Latin1")
    result<- lemmatizer(paste(toString(i),".txt",sep=""),adresse)
    data$corrige[i] <- result[[1]][1]
    data$tag[i] <- result[2:length(result)]
    file.remove(paste(toString(i),".txt",sep=""))
  }
  data
}

#On enlève l'ensemble des accents
supprime_accent<-function(table_tm){
  table_tm<-str_replace_all(table_tm, "[AÁÀÂÄÃÅáàâäãå]", "a")
  table_tm<-str_replace_all(table_tm, "[EÉÈÊËéèêë]", "e")
  table_tm<-str_replace_all(table_tm, "[IÍÏÎÌíìîï]", "i")
  table_tm<-str_replace_all(table_tm, "[NÑñ]", "n")
  table_tm<-str_replace_all(table_tm, "[OÓÒÔÖÕóòôöõ]", "o")
  table_tm<-str_replace_all(table_tm, "[UÚÙÛÜúùûü]", "u")
  table_tm<-str_replace_all(table_tm, "[YÝýÿ]", "y")
  corpus <- Corpus(VectorSource(table_tm), readerControl=list(reader=readPlain, language="fr"))
}

#On supprime nombres et ponctuations 
harmonise_notation<-function(corpus){
  corpus <- tm_map(corpus, content_transformer(removeNumbers))
  corpus <- tm_map(corpus, content_transformer(removePunctuation))
  corpus
}

#Suppression des espaces
enleve_espace<-function(corpus){
  corpus <- tm_map(corpus, content_transformer(stripWhitespace))
  corpus
}

#Suppression des stopwords 
enleve_stopwords<-function(corpus){
  myStopWords<-c(stopwords("fr"))
  corpus <- tm_map(corpus, removeWords, myStopWords)
  corpus
}

#On créé la DTM
creation_DTM<-function(corpus,sparseness){
  dtm <- DocumentTermMatrix(corpus,control=list(weighting=weightTf))
  dtm<-removeSparseTerms(dtm,sparseness)
  dtm
}

#Fonction de préprocessing, colonne contient le nom de la colonne a étudié et file est l'adresse du dossier TreeTagger
preprocess_text<-function(data,colonne,sparseness=0.99,file="C:/TreeTagger"){
  dataframe_corrige <- correction_dataframe(data,colonne)
  dataframe_corrige <- lemmatizer_dataframe(dataframe_corrige,file,colonne)
  table_tm<- dataframe_corrige$corrige
  corpus <- supprime_accent(table_tm)
  corpus <- harmonise_notation(corpus)
  corpus <- enleve_espace(corpus)
  corpus <- enleve_stopwords(corpus)
  dtm <- creation_DTM(corpus,sparseness)
  list(dtm,dataframe_corrige)
}

#saveRDS(dtm, file = "dtm.rds")
#saveRDS(dataframe_corrige,file="dataframe_corrige.rds")

#On regarde l'effet du preprocessing sur le nombre de mots 
compar<-function(colonne,dtm){
  par(mfrow=c(2,1),bg="beige")
  words_preprocessing<-sapply(strsplit(as.character(donnees[[colonne]]), " "), length)
  barplot(table(words_preprocessing), xlab="Nombre de termes avant le préprocessiong"
          ,col='blue')
  barplot(table(apply(dtm, 1, sum)), xlab="Nombre de termes dans la DTM",col='blue')
}

freq_word<-function(dtm_in){
  # Somme des occurrences de mots et tri par ordre décroissant
  v <- sort(colSums(as.matrix(dtm_in)),decreasing=TRUE)
  # Table des fréquences
  d1 <- data.frame(word = names(v),freq=v)
  # Pourcentage
  d1[,3]<-round(as.numeric(d1[,2])*100/nrow(as.matrix(dtm_in)),2)
  d1
}

#Visualisation des mots les plus utilisés dans les articles 
nuage_de_mots <- function(dtm){
  par(mfrow=c(1,1),bg="white")
  dd<-freq_word(dtm)
  dd<-dd[ !dd$word %in% c("avoir","etre","card"),]
  palette_couleur <- brewer.pal(n = 8, name = "Dark2")
  wordcloud(dd$word,dd$freq, scale=c(3,2),min.freq = 20, max.words = 50, random.order = F,colors= palette_couleur)
}

cooccurrence<-function(dtm_in){
  # Préparation des données
  d <- t(as.matrix(dtm_in))
  # Calcul de la matrice de distance utilisant méthode binaire (absence/presence)
  d <- dist(d,method="binary")
  d <- as.matrix(d)
  d <- 1 - d
  # Création d’un graphe de toutes les co-occurrences possibles de mots
  # Un graphe non orienté dont seul le triangle inférieur gauche est utilisé pour créer les liens.
  n <- graph.adjacency(d, mode="lower", weighted=T, diag=F)
  # Attributs associés au graphe
  n <- set.vertex.attribute(
    n, # graphe
    "name", # nom de l’attribut
    1:(length(d[1,])), # index des sommets
    as.character( colnames(d)) # nom des termes
  )
  # Désigne chaque co-occurrence - edge
  el <- data.frame(
    edge1 = get.edgelist(n,name=T)[,1], # Terme 1
    edge2 = get.edgelist(n,name=T)[,2], # Terme 2
    weight = get.edge.attribute(n, "weight"), # Nb de documents où les Termes 1 et 2 sont cooccurrents
    stringsAsFactors = FALSE
  )
  #Sélection des liens (Co-occurrences) les plus fréquents
  # Désigne chaque co-occurrence possible de mot de la DTM
  edges <- length(el[,1])
  # Sélectionne les co-occurences existantes
  # co-occurrence qui apparaisse au moins 1 fois dans la DTM)
  el2 <- subset(el, el[,3] >= 0) # le poids du lien (où 0= les deux termes n’apparaissent jamais ensemble)
# Création d’un graphe de co-occurrences non nulles des mots
n2 <- graph.edgelist( matrix( as.matrix(el2)[,1:2], ncol=2 ), directed =F)
# Pondération des liens
# Plus deux mots sont associés fréquemment dans les titres, plus le poids sera élevé
n2 <- set.edge.attribute(n2,"weight",
                         1:(length(get.edgelist(n2)[,1])),el2[,3])
# Vertex identification : le nom des sommets du graphique correspond au mot de la DTM
V(n2)$id <-as.character( colnames(d))
return(n2)
} # fin de la fonction
# Application de la fonction de cooccurrence

######################Algorithme de communauté###########################
communaute<-function(graph,method_com,freq_var,suppr){
  # Detection de communauté - minimum spanning tree
  mst <- minimum.spanning.tree(graph,
                               weights = 1 - get.edge.attribute(graph, "weight"),
                               algorithm="prim")
  # Type de communauté - Algorithmes
  if (method_com=="com_fg"){ # Communautés fast greedy
    com<-fastgreedy.community(mst, merges=TRUE, modularity=TRUE)
    groups <- as.matrix( table(fastgreedy.community(mst)$membership) ) 
    titre<-"fastgreedy"}
  
  if (method_com=="com_bw"){ # Communautés betweenness
    com <- edge.betweenness.community(mst, directed=F)
    groups <- as.matrix( table(edge.betweenness.community(mst)$membership) ) 
    titre<-"intermédiarité"}
  
  if (method_com=="com_rw"){ # Communautés Random walks
    com <- walktrap.community(mst,merges=TRUE, modularity=TRUE)
    groups <- as.matrix( table(walktrap.community(mst)$membership) ) 
    titre <-"walktrap"}
  # Création d'une table de groupe
  com_m <- membership(com)
  groupe <-list()
  for (i in c(1:length(com_m))){
    groupe[i]<-com_m[[i]]
  }
  groups <- data.frame( name = rownames(groups), freq = groups[,1])
  group_members <- data.frame( vid = V(mst)$id)
  for (i in c(1:nrow(group_members))){
    group_members$communaute[i]<-com_m[[i]]
  }
  mst <- delete.vertices(mst,V(mst)[name %in% suppr])
  mst <- delete.vertices(mst,V(mst)[name %in% c("etre","card","avoir")])
  # Graphique
  titre_tot <- paste("Détection de communauté basée sur la modularité",titre)
  multiplicateur <- 20/log(max(freq_var))
  plot(mst, layout=layout.fruchterman.reingold,
        vertex.color=com_m,
         vertex.label=V(mst)$id, # name
         vertex.size=log(freq_var)*multiplicateur,
         edge.color="grey55",
         edge.arrow.size=1,main=titre_tot)
  return(list(groups,group_members))
} # fin de la fonction commnauté

#On représente dans le graphe les n termes les plus utilisés
#dans les verbatims, algo contient la méthode de calcul des 
#communautés, on choisit la méthode fastgreedy. 
communaute_freq<-function(g,n,algo,dtm){
  d <- t(as.matrix(dtm[,-1]))
  freq <- NULL
  for (i in 1:length( rownames(d) )) {
    freq[i] = sum( d[i,] ) }
  freq<-as.matrix(freq)
  rownames(freq)<-rownames(d)
  freq<-freq[order(freq[,1],decreasing=T),]
  t <-n+1
  suppr <-names(freq[t:length(freq)])
  freq<-freq[1:n]
  communaute_fg<-communaute(g,algo,freq,suppr)
  communaute_fg
}


library(survival)
library(topicmodels) 
library(Rmpfr) 
library(wordcloud)
library(RColorBrewer) 
library(ggplot2)
library(lattice)
library(rjmcmc)
library(magrittr)



## A) TROUVER LE NOMBRE OPTIMAL DE THEMES: k

# Construction des modèles pour toutes les valeurs de k que l'on veut tester, par la méthode de Gibbs
#all.k un vecteur représentant les valeurs des nombres de thèmes à tester 
#dtm_in matrice en format Document-Terme
build.topic.models<-function(dtm, all.ks, alpha=0.1){ 
  models <- list()
  burnin <-0
  iter <- 1000
  keep <- 50 
  seed <- 1
  for(k in all.ks){
    models[as.character(k)] <-LDA(dtm, k = k, method = "Gibbs",control = list(burnin=burnin, iter=iter, keep=keep, seed=seed))
  }
  return(models)
}

#Calcul de la moyenne harmonique pour tous les modèles
harmonicMean <- function(logLikelihoods) {
  precision <- 2000L
  llMed <- median(logLikelihoods)
  as.double(llMed - log(mean(exp(-mpfr(logLikelihoods, prec = precision) + llMed))))
}

# Comparaison des modèles
evaluate.topic.models <- function(all.topic.models) {
  all.ks <- as.integer(names(all.topic.models)) # Nombre de thèmes 
  model.harmonic.means <- sapply(unlist(lapply(all.topic.models,logLik)),harmonicMean) #Moyenne Harm.
  return(list(k=all.ks[which.max(model.harmonic.means)], scores=model.harmonic.means, all.ks=all.ks)) } #retourner tous les k maximisant la vraisemblance

# Representation graphique des résultats
plot.ks <- function(res) { 
  all.ks <- res$all.ks
  scores <- res$scores
  best.k <- res$k
  par(list(oma=c(0, 0, 0, 0), mai=c(0.50, 0.30, 0.10, 0.05), cex=0.6))
  plot(all.ks, scores, type = "l", xlab = "Number of topics", axes=F) 
  axis(1, at=c(2:30), col.axis="black", tick=T, las=1)
  axis(2, at=range(scores), labels=F, col.axis="black", las=1)
  if(is.null(best.k)==F) { 
    abline(v=best.k, col="red", lty=3)
    y.text.pos <- range(scores)[1] + (0.8*(range(scores)[2]-range(scores)[1])) 
    text(x=best.k, y=y.text.pos, labels=as.character(best.k))
  }
}

# Meilleur modèle - Optimum nombre de thème (aussi reperable sur le graphique obtenu precedement)
get.best.model <- function(all.topic.models) {
  evaluation.results <- evaluate.topic.models(all.topic.models)
  plot.ks(evaluation.results)
  all.topic.models[[as.character(evaluation.results$k)]]}

################################################################################################
give_best_model<-function(dtm){                                                                #
  # Nombre de thèmes testés                                                                      #
  all.ks <- seq(2, 22, 1)                                                                        #
  # Matrice dont les documents (lignes) contiennent au moins un terme (colonne)                  #
  documents<-subset(as.matrix(dtm),(rowSums(as.matrix(dtm)) >0) ==TRUE)                          #
  #
  # Nombre optimal de thème via LDA avec Gibbs                                                   #
  all.topic.models <- build.topic.models(documents, all.ks, alpha=0.1)                           #
  best.model <- get.best.model(all.topic.models)                                                 #
  #
  #Output                                                                                        #
  # Modèle selectionné                                                                           #
  best.model     
}                                                                                              #
                                                                     #
################################################################################################



## B) ANALYSE THEMATIQUE

# Caracteristiques du modèle
get.topic.assignments <- function(best){
  gammaDF <- as.data.frame(best@gamma)
  names(gammaDF) <- 1:length(names(gammaDF))
  as.data.frame(cbind(document = row.names(gammaDF), topic = apply(gammaDF,1,function(x) names(gammaDF)[which(x==max(x))])))
}
# Calcul des distances entre les thèmes
dist_topic<-function(phi, gp_topic){
  dist.mat <- dist(phi)
  # MDS (positionnement multidimensionnel)
  par(mfrow=c(1,1))
  fit <- cmdscale(dist.mat, eig = TRUE, k = 2)
  points <- data.frame(x = fit$points[, 1], y = fit$points[, 2])
  d<-ggplot(points, aes(x = x, y = y))
  d + geom_point() + geom_text(data = points, aes(x = x, y = y - 0.005),label = rownames(phi))
  # Clustering
  km <- kmeans(points, centers= gp_topic, nstart=5)
  ggdata <- data.frame(points, Cluster=km$cluster, Topic=rownames(phi))
  # Figure
  ggplot(ggdata) +
    geom_point(aes(x=fit$points[, 1], y=fit$points[, 2], color=factor(Cluster)), size=6, shape=20) + 
    stat_ellipse(aes(x=fit$points[, 1],y=fit$points[, 2],fill=factor(Cluster)),
                 geom="polygon", level=0.95, alpha=0.2) + 
    guides(color=guide_legend("Cluster"),fill=guide_legend("Cluster")) + 
    geom_text(data = points, aes(x = x + 0.02, y = y),label = rownames(phi))}

#carateriser le i-eme theme
caracterise<-function(phi_t,i){
  colonne<-phi_t[i,]
  col.tri <- sort(colonne,decreasing = T)
  liste<-c(col.tri[1],col.tri[2],col.tri[3],col.tri[4],col.tri[5],col.tri[6],col.tri[7])
  n<-length(col.tri)
  titre = paste('Theme',i)
  barplot(liste, main = titre,ylim = c(0,col.tri[1]+0.05),axes = T)
}

################################################################################################
#
give_theme<-function(dtm){
  #liste des themes attribuables a chaque document
  best.model<-give_best_model(dtm)  
  print(best.model@k) #
  document.topic.assignments <- get.topic.assignments(best.model)   
  # Nombre de themes retenus                                                                     #
   
  #
  # Assignement des documents au thème le plus probable                                          #
  Topic <- topics(best.model, 1)                                                                 #
  #
  # Probabilité de chaque mot d’appartenir à un theme                                            #
  pos<-posterior(best.model)$topics                                                            #
  phi_t <- posterior(best.model)$terms %>% as.matrix          
  # Fréquence des thèmes 
  topic.freqs <- sort(table(unlist(document.topic.assignments$topic)), decreasing=T) 
  #MDS afin d’analyser la distance entre les thèmes                                              #
  topic_dist<-dist_topic (phi_t, 5)                                                              #
  #Sur le graphique sont affiches les numeros designat chaque theme,                             #
  #les themes devant etre interprettes avec la figure obtenue avec la fonction caracterise       #
  return(list(topic.freqs,topic_dist,phi_t,document.topic.assignments))                                                                                     #
}#

#caracterisations des themes                                                                   #
par(mfrow=c(2,2))                                                                              #
caracterise(phi_t,5)                                                                           #
caracterise(phi_t,6)                                                                           #
caracterise(phi_t,7)                                                                           #
caracterise(phi_t,8)                                                                           #
#
##

################################################################################################

library(translate)
library(tidytext)
library(stringr)
library(dplyr)
library(data.table)
library(sentimentr)
sentiment_message <- function(message_brut) {
  print(message_brut)
  message <- gsub(" \n", ". ", message_brut)
  #message <- as.character(message_brut)
  # Ãvaluer le sentiment
  sent <- sentiment(message, polarity_dt = hash_sentiment)
  # Retourne le score des sentiments en tant que liste
  liste <- sent[, 4]
  liste
}

# Exemple

# Ajouter la colonne sentiment au DataFrame
sentimenter <- function(data,colonne) {
  t<-which(colnames(data) == colonne)
  print(t)
  data$sentiment <- apply(data, 1, function(x) sentiment_message(x[t]))
  data
}

donnees <- read.csv2("Extraction.csv")
hash_valence <- read.csv("hash_valence_fr.csv", encoding = "utf8")
hash_sentiment <- read.csv("hash_sentiment_fr.csv", encoding = "utf8")

fonction_totale<-function(donnees,n,colonne,sparseness){
  result<- preprocess_text(donnees,colonne,sparseness)
  dtm<-result[[1]]
  dataframe_corrige <-result[[2]]
  nuage_de_mots(dtm)
  g<-cooccurrence(dtm)
  algo<-"com_fg" # Communautés fast greedy
  communaute_fg <- communaute_freq(g,n,algo,dtm)
  theme <- give_theme(dtm)
  topics.freqs<-theme[[1]]
  topic_dist <- theme[[2]]
  phi_t<-theme[[3]]
  topics.assignement <- theme [[4]]
  par(mfrow=c(2,2)) 
  caracterise(phi_t,5)                                                                           
  caracterise(phi_t,6)                                                                           
  caracterise(phi_t,7)                                                                           
  caracterise(phi_t,8)
  # DataFrame pour l'Ã©tude des sentiments
  setDT(hash_sentiment) # Transformer en data.tables
  setkey(hash_sentiment)
  # DataFrame des valence shifters pour l'Ã©tude des sentiments
  setDT(hash_valence)
  setkey(hash_valence)
  dataframe_corrige<-sentimenter(dataframe_corrige,"corrige")
  return(list(dtm,dataframe_corrige,topics.freqs,topic_dist,topics.assignement))
}
k<-donnees[1:10,]
debut <- Sys.time()
result<- fonction_totale(k,12,"Resume",sparseness = 0.95)
Sys.time()-debut
