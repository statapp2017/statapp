rm(list=ls())
library(hunspell)
library(tm)
library(RColorBrewer)
library(wordcloud)
library(stringr)
library(igraph)
library(graphics)
library(tsne)
library(Rtsne)
library(koRpus)
library(tokenizers)
library(plotly)
library(microbenchmark)
library(survival)
library(topicmodels) 
library(Rmpfr) 
library(xtable)
library(ggplot2)
library(lattice)
library(rjmcmc)
library(magrittr)
library(dplyr)
library(data.table)
library(sentimentr)
library(plotly)

setwd("D:/ENSAE/2emeannee/Statsapp")
#Import the dataset
data <- read.csv2("Extraction.csv",encoding = "Latin1")
#A dictionnary to help the process of hunspell
dico <- read.table("liste_francais.txt",encoding = "Latin1")
#A few dictionnaries for the sentiment analysis
hash_valence <- read.csv2("hash_valence_fr.csv")
hash_sentiment <- read.csv2("hash_sentiment_fr.csv")

#############################
#                           #
#       Preprocessing       #
#                           #
#############################

#' Give the distribution  of the marks given by the clients and the numbers of clients studied
#' 
#' @param data A dataframe containing the evaluation made by the clients
#' @return Plot the histogram of the marks and give the numbers of clients studied
#' @examples
#' spell_checker(dataframe_clients)

describe_corpus<-function(data){
  par(bg="beige",mfrow=c(1,1))
  hist(data$recommandation_SGK,col="cyan",main="Notes données par les clients",ylab="Notes",xlab="Effectif")
  chaine<-paste(paste("Il y a",as.character(nrow(data))),"documents")
  chaine
}

#' Transform a sentence in a given \code{file} to lemmatize all the words
#' and find also the grammatical class of all words. This algorithm is based
#' on TreeTagger
#' 
#' @param x The location of the file containing the sentence to work on 
#' @param adress The location of the folder containing the TreeTagger used.
#' @return The sentence with all words lemmatized 
#' @examples
#' dataframe_correction("Myfile.txt","C:/TreeTagger")
lemmatizeCorpus <- function(x,adress) {
  if (x!=""){
    suppressMessages(words.cc <- treetag(x, treetagger="manual", format="obj",
                        TT.tknz=FALSE, lang="fr",encoding="Latin1",
                        TT.options=list(path=adress, preset="fr")))
    
    words.lm <- ifelse(words.cc@TT.res$token != words.cc@TT.res$lemma, 
                       ifelse(words.cc@TT.res$lemma != "<unknown>", words.cc@TT.res$lemma, words.cc@TT.res$token),
                       words.cc@TT.res$token)
    lemme <- toString(paste(words.lm, collapse = " "))
    lemme <- str_replace_all(lemme,"ê","?")%>%str_replace_all("@card@" ,"")%>%
      str_replace_all("\"","")%>%str_replace_all("\ \"","")%>%str_replace_all("suivre|Ãªtre","etre")%>%
      str_replace_all("Ã©","e")%>%str_replace_all("Ã¨","e")%>%str_replace_all("Ã","a")%>%
      str_replace_all( "Ãª","e")%>%str_replace_all("Ã§","c")%>%str_replace_all("Å","oe")%>%
      str_replace_all("Ã¹","u")%>%str_replace_all("Ã«","e")%>%str_replace_all("[EÉÈÊËéèêë]", "e")%>%
      str_replace_all("a©","e")%>%str_replace_all("a¨","e")%>%str_replace_all( "aª","e")%>%
      str_replace_all("Ã","e")%>%str_replace_all("etre","")%>%str_replace_all("avoir","")
  }
  else{ 
    x
    }
}

#' Correct the spelling of a given sentence.
#' 
#' @param sentence a french string
#' @return The correction of \code{sentence}.
#' @examples
#' spell_checker("Il pleut aujourd'hui.")
#' spell_checker("Je sui contre ce projet.")

spell_checker<- function(sentence){
  #Tokenization of the sentence 
  list_words <- tokenize_words(as.character(gsub(" \n", ". ",sentence)), lowercase = TRUE)[[1]]
  string_correct<- ""
  for (x in list_words){
    #Check if the spelling of the words is correct
    if (hunspell_check(x,dict=dictionary("fr"))|str_length(x)==1){
      string_correct<- paste(string_correct,x)
    }
    else{
      #If the spelling isn't accurate we recover hunspell suggestions
      suggestion<-hunspell_suggest(x, dict = dictionary("fr"))[[1]]
      stop <- TRUE
      i <- 1
      while (stop & i<3){
        #We check that the word is french before we accept the change
        if (suggestion[i] %in% dico[[1]]){
          string_correct <- paste(string_correct,suggestion[i])
          stop <- FALSE
        }
        else{
          i <- i+1
        }
      }
      #If the algorithm isn't able to find a good spelling we let the way it was
      if (i==5){
        string_correct <- paste(string_correct,x)
      }
    }  
  }
  string_correct
}

#' Transform all the sentences contained in a given column of a dataframe. 
#' See lemmatizer for the precise transformations.
#' 
#' @param data The dataframe containing the sentences
#' @param adress The location of the folder containing the TreeTagger used.
#' @param column The column of \code{data} containing the sentences
#' @return A new dataframe with the same columns as \code{data} but with two 
#' new columns \code{lemme} containing all the sentences lemmatized and 
#' \code{tag} containing all the grammatical class of the words in the sentences 
#' in column
#' @examples
#' dataframe_correction(my_data,"C:/TreeTagger","Text")

lemmatizer_dataframe <- function(data,adress,column){
  data[,column]<-sapply(data[,column],as.character)
  data$corrige<-sapply(data[,column],spell_checker)
  data$lemme<-sapply(X = data$corrige, FUN = function(x){ lemmatizeCorpus(x,adress)},USE.NAMES = F)
  data
}

#' Delete all the numbers, spaces and ponctuations present in corpus
#' @param corpus A collection of documents containing text (obtained with Corpus)
#' @return A new corpus without the numbers,the spaces and ponctuations

notation_harmonisation<-function(table_tm){
  corpus <- Corpus(VectorSource(table_tm), readerControl=list(reader=readPlain, language="fr"))%>%
  tm_map( content_transformer(removeNumbers))%>%tm_map(content_transformer(removePunctuation))%>%
  tm_map(content_transformer(stripWhitespace))
  corpus
}

#' Delete all the stopwords present in corpus
#' @param corpus A collection of documents containing text (obtained with Corpus)
#' @return A new corpus without the stopwords
 
delete_stopwords<-function(corpus){
  myStopWords<-c(stopwords("fr"))
  corpus <- tm_map(corpus, removeWords, myStopWords)
  corpus
}


#' Create the Document-Term Matrix based on the given corpus. It uses the term frequency weighthing
#' @param corpus A collection of documents containing text (obtained with Corpus)
#' @param sparseness A numeric for the maximal allowed sparsity in the range from bigger zero to smaller one.
#' @return The document Term Matrix corresponding to the given corpus
creation_DTM<-function(corpus,sparseness){
  dtm <- DocumentTermMatrix(corpus,control=list(weighting=weightTf))
  dtm<-removeSparseTerms(dtm,sparseness)
  dtm
}

#' Given a data containing sentences in \code{column} give the Document-Term 
#' Matrix after text transformations (spell correction, lemmatization, removal 
#' of stopwords, ponctuations etc...)
#' @param data A dataframe containing the sentences 
#' @param column The column of the dataframe containing the sentences to work on
#' @param sparseness A numeric for the maximal allowed sparsity in the range from bigger zero to smaller one.
#' @param adress The location of the folder containing the TreeTagger used.
#' @return Both the document Term Matrix corresponding to the given sentences and
#' a new dataframe containing the lemmatisation of the sentences after spell correction

preprocess_text<-function(data,column,sparseness=0.99,file="C:/TreeTagger"){
  dataframe_corrige <- lemmatizer_dataframe(data,file,column)
  table_tm<- dataframe_corrige$lemme
  corpus <-notation_harmonisation(table_tm)%>%delete_stopwords()
  dtm <- creation_DTM(corpus,sparseness)
  list(dtm,dataframe_corrige)
}

dtm<-readRDS("dtm_epargne.RDS")

#' Display the histogram reprensenting the distribution of words before and after the text preprocessing
#' @param data The dataframe containing the original sentences
#' @param column The column of \code{data} containing the sentences.
#' @param dtm The document-term matrix obtained after the preprocessing
#' @return The histogram of the repartition of words before and after the lemmatisation and also 
#' the number they are

compar<-function(data,column,dtm){
  par(mfrow=c(2,1),bg="beige")
  words_preprocessing<-sapply(strsplit(as.character(data[[column]]), " "), length)
  nombre <- ncol(DocumentTermMatrix(Corpus(VectorSource(data[[column]]), readerControl=list(reader=readPlain, language="fr")),control=list(weighting=weightTf)))
  print(paste(paste("Il y a",as.character(nombre)),"mots avant la lemmatisation"))
  barplot(words_preprocessing, xlab="Nombre de termes avant le préprocessiong"
          ,col='blue')
  nombre2 <- ncol(dtm)
  print(paste(paste("Il y a",as.character(nombre2)),"mots après la lemmatisation"))
  barplot(apply(dtm, 1, sum), xlab="Nombre de termes dans la DTM",col='blue')
}

#' Give the frequency of each words used in a document-term matrix 
#' @param dtm_in The document-term matrix obtained after the preprocessing 
#' @return A dataframe sorted by frequency given the frequency for each words in \code{dtm_in}

freq_word<-function(dtm_in){
  # Somme des occurrences de mots et tri par ordre d?croissant
  v <- sort(colSums(as.matrix(dtm_in)),decreasing=TRUE)
  # Table des fr?quences
  d1 <- data.frame(word = names(v),freq=v)
  # Pourcentage
  d1
}

#' Display the wordcloud corresponding to a given Document-Term Matrix
#' @param dtm The document-term matrix obtained after the preprocessing 
#' @return Plot the worcloud corresponding to a given Document-Term Matrix

nuage_de_mots <- function(dtm){
  par(mfrow=c(1,1),bg="white")
  dd<-freq_word(dtm)
  dd<-dd[ !dd$word %in% c("avoir","etre","card"),]
  palette_couleur <- brewer.pal(n = 8, name = "Dark2")
  wordcloud(dd$word,dd$freq, scale=c(3,2),min.freq = 20, max.words = 150, random.order = F,colors= palette_couleur)
}

#' Create an igraph object with the words as vertices and the edges indexed by the weight of their relationship
#' the weigth choosen is absence prensence. (1 if their are always in the same documents, 0 if they never are). 
#' Only the edges with a positive weigth are present in the igraph. 
#' @param dtm_in The document-term matrix obtained after the preprocessing 
#' @return Return an igraph object reprensenting the coocurrences between the words in the \code{dtm_in}

cooccurrence<-function(dtm_in){
  # Preparation of the data, d contained a matrix with every distance between words with the distance choosed as
  # the fact of being present in the same document or not. 
  d <- t(as.matrix(dtm_in)) %>% dist(method="binary") %>% as.matrix()
  d <- 1 - d
  #We create and igraph put in n with two vertices connected with an edge giving the distance between the two
  #created during the previous step
  n <- graph.adjacency(d, mode="lower", weighted=T, diag=F) %>%  set.vertex.attribute(    "name", 
   1:(length(d[1,])),
    as.character( colnames(d)) 
  )
  #We stock the information in a dataframe
  el <- data.frame(
    edge1 = get.edgelist(n,name=T)[,1],
    edge2 = get.edgelist(n,name=T)[,2], 
    weight = get.edge.attribute(n, "weight"), 
    stringsAsFactors = FALSE
  )
  
  edges <- length(el[,1])
  #We keep the edges only if their are present at least once in the same document together
  el2 <- subset(el, el[,3] >= 0) 
  #We create in n2 the new igraph based on the same principle as n but only with the connected word
  n2 <- graph.edgelist( matrix( as.matrix(el2)[,1:2], ncol=2 ), directed =F)
  #Ponderation of the links, the more two words are associated in te documents the more important the weight of 
  #the link is
  n2 <- set.edge.attribute(n2,"weight",
                           1:(length(get.edgelist(n2)[,1])),el2[,3])
  # Vertex identification : le nom des sommets du graphique correspond au mot de la DTM
  V(n2)$id <-as.character( colnames(d))
  return(n2)
} 
######################Algorithme de communauté###########################

communaute<-function(graph,freq_var,suppr){
  mst <- minimum.spanning.tree(graph,
                               weights = 1 - get.edge.attribute(graph, "weight"),
                               algorithm="prim")
  #Calculation of the community with the fastgreedy method
  com<-fastgreedy.community(mst, merges=TRUE, modularity=TRUE)
  groups <- as.matrix( table(fastgreedy.community(mst)$membership) ) 
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
  titre <-"Détection de communauté basée sur la modularité fastgreedy"
  multiplicateur <- 20/log(max(freq_var))
  plot(mst, layout=layout.fruchterman.reingold,
       vertex.color=com_m,
       vertex.label=V(mst)$id, # name
       vertex.size=log(freq_var)*multiplicateur,
       edge.color="grey55",
       edge.arrow.size=1,main=titre)
  legend(x="bottomleft",legend=paste(paste("Il y a",as.character(nrow(groups))),"communautés"),bty="n")
  return(list(groups,group_members))
} 

#' Plot the graphs of the community. It only keeps the \code{n} most frequent term in the database.
#' In the graph a community is represented by a color and the size of a vertices is proportionnal
#' to the log of its use frequency.
#' @param n The number of words to keep in the graph. We recommend n<100 for more clarity.
#' @param dtm The document-term matrix obtained after the preprocessing 
#' @return Plot the graph illustrating the communities associated with the n most
#' used words in the set of documents 

communaute_freq<-function(n,dtm){
  g <-cooccurrence(dtm)
  par(mfrow=c(1,1))
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
  communaute_fg<-communaute(g,freq,suppr)
  communaute_fg
}

#############################
#                           #
#    Allocation de thème    #
#                           #
#############################

## A) TROUVER LE NOMBRE OPTIMAL DE THEMES: k
# Construction des mod?les pour toutes les valeurs de k que l'on veut tester, par la m?thode de Gibbs
#all.k un vecteur repr?sentant les valeurs des nombres de th?mes ? tester 
#dtm_in matrice en format Document-Terme
build.topic.models<-function(dtm, all.ks){ 
  models<-list()
  burnin <-1500
  iter <- 4000
  keep <- 50 
  seed <- 1
  for(k in all.ks){
    models[as.character(k)] <-LDA(dtm, k = k, method = "Gibbs",control = list(burnin=burnin,iter=iter,keep=keep, seed=seed,best=TRUE))
  }
  return(models)
}

coherence_tfidf<-function(model,dtm,n){ # n = number of topics in the model considered
  dtm2 <- as.matrix(weightSMART(dtm,spec="atn"))
  dtm<-as.matrix(dtm)
  phi_t <- posterior(model)$terms %>% as.matrix
  liste_score <- c()
  for (topics in 1:n){ #n -> number of topics
    col.tri <- sort(phi_t[topics,],decreasing = T)
    #We calculate the metric over the 10 best words of the topics
    w<-col.tri[1:10]
    u<-names(w)
    somme <-0
    for (i in 1:(length(u)-1)){
      for (j in (i+1):length(u)){
        sub_dtm<-dtm[dtm[,u[i]]>0 & dtm[,u[j]]>0,]
        high_sum<-sum(dtm2[rownames(sub_dtm),u[i]]*dtm2[rownames(sub_dtm),u[j]])+1
        low_sum<- sum(dtm[,u[i]])
        somme<-somme+log(high_sum/low_sum)
      }
    }
    liste_score[topics]<-somme
  }
  liste_score
}

get_best_model<-function(all.topics.models,dtm){
  all_coherences<-list()
  indice<-1
  for(model in all.topics.models){
    all_coherences[[indice]]<-coherence_tfidf(model,dtm,indice+1)
    indice<-indice + 1
  }
  #calculer la moy des coherences pour le thÃ¨me i
  moyenne<-function(all_coherences,i){
    mean(all_coherences[[i]])
  }
  #liste des cohÃ©rences moyennes pour tous les nombres de themes testÃ©s
  average_coherences<-c()
  Nombre_de_k<-length(all_coherences)
  for(i in 1:Nombre_de_k){
    average_coherences[i]<-moyenne(all_coherences,i)
  }
  average_coherences
  plot(2:10,average_coherences)
  #BEST MODEL:
  best_model<-all.topics.models[[as.character(which(average_coherences ==max(average_coherences))+1)]]
  best_model
}
################################################################################################
give_best_model<-function(dtm){                                                                #
  # Nombre de th?mes test?s                                                                      #
  all.ks <- seq(2, 10, 1)                                                                        #
  # Matrice dont les documents (lignes) contiennent au moins un terme (colonne)                  #
  documents<-subset(as.matrix(dtm),(rowSums(as.matrix(dtm)) >0) ==TRUE)                          #
  #
  # Nombre optimal de th?me via LDA avec Gibbs                                                   #
  all.topic.models <- build.topic.models(documents, all.ks)
  best.model <- get_best_model(all.topic.models,dtm) 
  #
  #Output                                                                                        #
  # Mod?le selectionn?                                                                           #
  best.model     
}                                                                                              #
################################################################################################

## B) ANALYSE THEMATIQUE

# Caracteristiques du mod?le
get.topic.assignments <- function(best){
  gammaDF <- as.data.frame(best@gamma)
  names(gammaDF) <- 1:length(names(gammaDF))
  as.data.frame(cbind(document = row.names(gammaDF), topic = apply(gammaDF,1,function(x) names(gammaDF)[which(x==max(x))])))
}

dist_topic<-function(phi){
  bp <- ggplot(diamonds, aes(clarity, fill = cut)) +
    geom_bar()
  dist.mat <- dist(phi)
  par(mfrow=c(1,1))
  fit <- cmdscale(dist.mat, eig = TRUE, k = (nrow(phi)-1))
  points <- data.frame(x = fit$points[, 1], y = fit$points[, 2])
  inertie.expl <- rep(0,times=(nrow(phi)-1))
  for (k in 1:(nrow(phi)-1)){
    clus <- kmeans(points,centers=k,nstart=5)
    inertie.expl[k] <- clus$betweenss/clus$totss
  }
  diff<-rep(0,times=(nrow(phi)-2))
  for (k in 2:(nrow(phi)-1)){
    diff[k-1]<-inertie.expl[k]-inertie.expl[k-1]
  }
  plot(1:(nrow(phi)-1),inertie.expl,type="b",xlab="Nb. de groupes",ylab="% inertie expliquée")
  k<-length(diff[diff>0.05])+1
  # Clustering
  km <- kmeans(points, centers= k, nstart=5)
  ggdata <- data.frame(points, Cluster=km$cluster, Topic=rownames(phi))
  # Figure
  ggplot(ggdata) +
    geom_point(aes(x=x, y=y, color=factor(Cluster)), size=6, shape=20) + 
    guides(color=guide_legend("Cluster"),fill=guide_legend("Cluster")) + 
    geom_text(data = points, aes(x = x + 0.02, y = y),label = rownames(phi))
}

try_tsne<-function(phi){
  train<-as.data.frame(t(phi))
  Labels<-c()
  rown <- rownames(train)
  for (i in 1:nrow(train)){
    Labels[i] <- which(train[rown[i],]==max(train[rown[i],]))
  }
  Labels<-as.factor(Labels)
  colors =brewer.pal(length(unique(colnames(train))),"Paired")
  lab<-unique(Labels)
  names(colors) <- lab
  legends<-c()
  for (i in 1:length(unique(Labels))){
    legends[i]<-paste("Thème",as.character(lab[i]))
  }
  tsne <- Rtsne(train, dims = 2, perplexity=30, verbose=TRUE, max_iter = 500,check_duplicates = FALSE)
  plot(tsne$Y, t='p',pch=21, main="tsne",col=colors[Labels],bg=colors[Labels])
  legend("bottomleft",legend=legends,col=colors[unique(Labels)],pt.bg=colors[unique(Labels)],pch = rep(21,length(Labels)))
  }

#carateriser le i-eme theme
caracterise<-function(phi_t,topics.freqs){
  freq <- as.matrix(topics.freqs)
  freq2 <- as.data.frame(freq)
  freq2$theme<-paste("Thème",rownames(freq2))
  bp<-ggplot(freq2,aes(x=theme,y=V1))+geom_bar(stat="identity")+labs(title="Nombre de mots par thème",x="Thème",y="Nombre de document")
  p2<-ggplotly(bp)
  print(p2)
  df <- as.data.frame(t(phi_t))
  df$nom<-rownames(df)
  col<-list()
  for (i in (1:(ncol(df)-1))){
    titre <-paste("Theme",as.character(i),sep="")
    col[i]<-titre
  }
  col[length(col)+1]<-"nom"
  colnames(df)<-col
  a_garder<-c()
  for (i in (1:(ncol(df)-1))){
    df<-df[order(df[,i],decreasing=T),]
    a_garder<-c(a_garder,c(rownames(df[1:10,])))
    ref <- df[,i]
    for (j in 1:10){
    df$theme[j]<-paste("Thème",as.character(i))
    df$valeur[j]<-ref[j]
    }
  }
  df<-df[a_garder,]
  df <- df[order(df[,which(colnames(df)=="theme")],decreasing=F),]
  df$nom<-factor(df$nom, levels=df$nom)
  p<-ggplot(data=df,aes(x=nom,y=valeur,color=theme)) +
    geom_bar(stat="identity")+labs(title="Mots illustrant les différents thèmes",
                                  x ="Mots", y = "Probabilité")+theme(axis.text.x = element_text(angle=45))
  p1<-ggplotly(p)
  print(p1)
}

################################################################################################
#
give_theme<-function(dtm){
  #liste des themes attribuables a chaque document
  best.model<-give_best_model(dtm)
  document.topic.assignments <- get.topic.assignments(best.model)   
  # Nombre de themes retenus                                                                     #
  #
  # Assignement des documents au th?me le plus probable                                          #
  Topic <- topics(best.model, 1)     
  #
  # Probabilit? de chaque mot d'appartenir ? un theme                                            #
  pos<-posterior(best.model)$topics   
  phi_t <- posterior(best.model)$terms %>% as.matrix  
  # Fr?quence des th?mes 
  topic.freqs <- sort(table(unlist(document.topic.assignments$topic)), decreasing=T) 
  #MDS afin d'analyser la distance entre les th?mes                                              #
  topic_dist<-dist_topic (phi_t)                                                              #
  #Sur le graphique sont affiches les numeros designat chaque theme,                             #
  #les themes devant etre interprettes avec la figure obtenue avec la fonction caracterise       #
  return(list(topic.freqs,topic_dist,phi_t,document.topic.assignments))                                                                                     #
}

################################################################################################
############################
#                          #
#    Sentiment Analysis    #
#                          #
############################


sentiment_message <- function(message_brut) {
  message <- gsub(" \n", ". ", message_brut)
  #message <- as.character(message_brut)
  # Évaluer le sentiment
  sent <- sentiment(message, polarity_dt = hash_sentiment,valence_shifters_dt = hash_valence)
  # Retourne le score des sentiments en tant que liste
  liste <- sent[, 4]
  liste
}

# Ajouter la colonne sentiment au DataFrame
sentimenter <- function(data,colonne) {
  t<-which(colnames(data) == colonne)
  data$sentiment <- apply(data, 1, function(x) sentiment_message(x[t]))
  data
}

fonction_totale<-function(donnees,n,colonne,sparseness){
  describe_corpus(donnees)
  result<- preprocess_text(donnees,colonne,sparseness)
  dtm<-result[[1]]
  dataframe_corrige <-result[[2]]
  nuage_de_mots(dtm)
  g<-cooccurrence(dtm)
  communaute_fg <- communaute_freq(n,dtm)
  View(dataframe_corrige)
  theme <- give_theme(dtm)
  topics.freqs<-theme[[1]]
  topic_dist <- theme[[2]]
  phi_t<-theme[[3]]
  topics.assignement <- theme [[4]]
  caracterise(phi_t,topics.freqs)
  dist_topic(phi_t)
  try_tsne(phi_t)
  return(list(dtm,dataframe_corrige,topics.freqs,phi_t,topic_dist,topics.assignement))
}

so_ge<-read.csv2("verbatims_SRC_230118_ENSAE.csv")
so_ge_prevoyance<-so_ge[so_ge$REPRISE_ACTIVITE=="Epargne",]
hash_sentiment<-hash_sentiment[!hash_sentiment$x %in% hash_valence$x,]
hash_sentiment<-hash_sentiment[!duplicated(hash_sentiment),]

debut<-Sys.time()
Rprof("D:/ENSAE/2emeannee/Statsapp/Profile.txt") # L'adresse où on veut enregistrer le résultat
result<- fonction_totale(so_ge_prevoyance,80,"raisons_recommandation",sparseness = 0.995)
Rprof(NULL) # Arrêter le Profiler
summaryRprof("D:/ENSAE/2emeannee/Statsapp/Profile.txt") 
Sys.time()-debut
