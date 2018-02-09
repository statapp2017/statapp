rm(list=ls())
library(hunspell)
library(tm)
library(RColorBrewer)
library(wordcloud)
library(stringr)
library(igraph)
library(graphics)
library(tsne)
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
#' @param file The location of the file containing the sentence to work on 
#' @param adress The location of the folder containing the TreeTagger used.
#' @return A list containing both the sentence with all words lemmatized and 
#' a list with all the grammatical class of the words
#' @examples
#' dataframe_correction("Myfile.txt","C:/TreeTagger")

lemmatizer<-function(file,adress){
  #Lemmatisation and part-of-speech tagging of the text
  suppressMessages(
    text <- data.frame(treetag(file,treetagger = "manual",TT.tknz=FALSE,lang="fr",encoding="Latin1",TT.options=list(path=adress,preset = "fr"))@TT.res[,c(1,2,3)])
  )
  lemme <- ""
  #We get the lemmatize sentence
  for (i in c(1:nrow(text))){
    if (text$lemma[i]=="<unknown>"){
      lemme <- paste(lemme,text$token[i])
    }
    else{
      list_letters <- str_split(text$lemma[i],"")[[1]]
      if ("|" %in% list_letters){
        for (j in c(1:length(list_letters))){
          if (list_letters[j]=="|"){
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
  lemme <- str_replace_all(lemme,"ê","?")%>% str_replace_all(" x \" \" @card@ " ,"")%>%
    str_replace_all("@card@" ,"")%>%str_replace_all("\"","")%>%str_replace_all("\ \"","")%>%
    str_replace_all("Ã©","e")%>%str_replace_all("Ã¨","e")%>%str_replace_all("Ã","a")%>%
    str_replace_all( "Ãª","e")%>%str_replace_all("Ã§","c")%>%str_replace_all("Å","oe")%>%
    str_replace_all("Ã¹","u")%>%str_replace_all("Ã«","e")%>%str_replace_all("[EÉÈÊËéèêë]", "e")%>%
    str_replace_all("a©","e")%>%str_replace_all("a¨","e")%>%str_replace_all( "aª","e")%>%
    str_replace_all("Ã","e")
  #We get the grammatical class of all words.
  v <- array(dim = c(1,nrow(text)-8))
  for (i in 8:nrow(text)){
    v[i-7] <- text$tag[i]
  }
  names(v) <- text$token[c(8:nrow(text))]
  return (list(lemme,v))
}

#' Correct the spelling of a given sentence.
#' 
#' @param sentence a french string
#' @return The correction of \code{sentence}.
#' @examples
#' spell_checker("Il pleut aujourd'hui.")
#' spell_checker("Je sui contre ce projet.")


spell_checker<- function(sentence,adress){
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
  file.create(paste(toString(1),".txt",sep=""))
  write.table(tolower(string_correct),file=paste(toString(1),".txt",sep=""),fileEncoding = "Latin1")
  result<- lemmatizer(paste(toString(1),".txt",sep=""),adress)
  file.remove(paste(toString(1),".txt",sep=""))
  c(string_correct,result)
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
  colref <-data[[column]]
  for (i in 1:nrow(data)){
    result<-spell_checker(colref[i],adress)
    data$corrige[i]<-result[1]
    data$lemme[i]<-result[2]
    data$tag[i]<-result[3:length(result)]
  }
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
#k<-data[1:100,]
#debut <- Sys.time()
#result <- preprocess_text(k,"Resume")
#Sys.time()-debut
#dtm <- result[[1]]

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
compar(data,"Resume",dtm)
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

t<-communaute_freq(82,dtm)
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

#Calcul de la moyenne harmonique pour tous les mod?les
harmonicMean <- function(logLikelihoods) {
  precision <- 2000L
  llMed <- median(logLikelihoods)
  as.double(llMed - log(mean(exp(-mpfr(logLikelihoods, prec = precision) + llMed))))
}

# Comparaison des mod?les
evaluate.topic.models <- function(all.topic.models) {
  all.ks <- as.integer(names(all.topic.models)) # Nombre de th?mes 
  model.harmonic.means <- sapply(unlist(lapply(all.topic.models,logLik)),harmonicMean) #Moyenne Harm.
  return(list(k=all.ks[which.max(model.harmonic.means)], scores=model.harmonic.means, all.ks=all.ks)) } #retourner tous les k maximisant la vraisemblance

# Representation graphique des r?sultats
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
# Meilleur mod?le - Optimum nombre de th?me (aussi reperable sur le graphique obtenu precedement)
get.best.model <- function(all.topic.models) {
  evaluation.results <- evaluate.topic.models(all.topic.models)
  plot.ks(evaluation.results)
  all.topic.models[[as.character(evaluation.results$k)]]}

################################################################################################
give_best_model<-function(dtm){                                                                #
  # Nombre de th?mes test?s                                                                      #
  all.ks <- seq(2, 10, 1)                                                                        #
  # Matrice dont les documents (lignes) contiennent au moins un terme (colonne)                  #
  documents<-subset(as.matrix(dtm),(rowSums(as.matrix(dtm)) >0) ==TRUE)                          #
  #
  # Nombre optimal de th?me via LDA avec Gibbs                                                   #
  all.topic.models <- build.topic.models(documents, all.ks)                                      #
  best.model <- get.best.model(all.topic.models)                                                 #
  #
  #Output                                                                                        #
  # Mod?le selectionn?                                                                           #
  best.model     
}                                                                                              #
#
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
  fit <- cmdscale(dist.mat, eig = TRUE, k = 5)
  points <- data.frame(x = fit$points[, 1], y = fit$points[, 2],z=fit$points[,3])
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
dist_topic(phi_t)

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
try_tsne(phi_t)

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

caracterise(phi_t,topic_freq)
thm<-give_theme(dtm)
phi_t<-thm[[3]]
topic_freq<-thm[[1]]
################################################################################################
#
give_theme<-function(dtm){
  #liste des themes attribuables a chaque document
  best.model<-give_best_model(dtm)  
  document.topic.assignments <- get.topic.assignments(best.model)   
  # Nombre de themes retenus                                                                     #
  
  #
  # Assignement des documents au th?me le plus probable                                          #
  Topic <- topics(best.model, 1)                                                                 #
  #
  # Probabilit? de chaque mot d'appartenir ? un theme                                            #
  pos<-posterior(best.model)$topics                                                            #
  phi_t <- posterior(best.model)$terms %>% as.matrix          
  # Fr?quence des th?mes 
  topic.freqs <- sort(table(unlist(document.topic.assignments$topic)), decreasing=T) 
  #MDS afin d'analyser la distance entre les th?mes                                              #
  topic_dist<-dist_topic (phi_t, 5)                                                              #
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
  result<- preprocess_text(donnees,colonne,sparseness)
  dtm<-result[[1]]
  dataframe_corrige <-result[[2]]
  nuage_de_mots(dtm)
  g<-cooccurrence(dtm)
  algo<-"com_fg" # Communaut?s fast greedy
  communaute_fg <- communaute_freq(g,n,algo,dtm)
  theme <- give_theme(dtm)
  topics.freqs<-theme[[1]]
  topic_dist <- theme[[2]]
  phi_t<-theme[[3]]
  topics.assignement <- theme [[4]]
  # DataFrame pour l'?tude des sentiments
  setDT(hash_sentiment) # Transformer en data.tables
  setkey(hash_sentiment)
  # DataFrame des valence shifters pour l'étude des sentiments
  setDT(hash_valence)
  setkey(hash_valence)
  dataframe_corrige<-sentimenter(dataframe_corrige,"corrige")
  return(list(dtm,dataframe_corrige,topics.freqs,phi_t,topic_dist,topics.assignement))
}

so_ge<-read.csv2("verbatims_SRC_230118_ENSAE.csv")
so_ge_prevoyance<-so_ge[so_ge$REPRISE_ACTIVITE=="Epargne",]
hash_sentiment<-hash_sentiment[!hash_sentiment$x %in% hash_valence$x,]
hash_sentiment<-hash_sentiment[!duplicated(hash_sentiment),]
debut <- Sys.time()
result<- fonction_totale(so_ge_prevoyance,80,"raisons_recommandation",sparseness = 0.995)
Sys.time()-debut
