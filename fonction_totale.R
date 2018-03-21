rm(list=ls())

library(pROC)
library(tm)
library(RColorBrewer)
library(wordcloud)
library(stringr)
library(igraph)
library(graphics)
library(tsne)
library(Rtsne)
library(koRpus)
library(hunspell)
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
library(data.table)
library(sentimentr)
library(plotly)
library(SnowballC)
library(snowfall)
library(xgboost)
library(MLmetrics)
library(iterators)
library(caret)

setwd("D:/ENSAE/2emeannee/Statsapp")
#Import the dataset
data <- read.csv2("Extraction.csv",encoding = "Latin1")
#A dictionnary to help the process of hunspell
dico <- read.table("liste_francais.txt",encoding = "Latin1")
FEEL <- read.csv2("FEEL.csv",encoding="Latin1")
#A few dictionnaries for the sentiment analysis
hash_valence <- read.csv2("hash_valence_fr.csv")
hash_sentiment <- read.csv2("hash_sentiment_fr.csv")
so_ge<-read.csv2("verbatims_SRC_230118_ENSAE.csv")
so_ge_prevoyance<-so_ge[so_ge$REPRISE_ACTIVITE=="Epargne",]
dtm_ep<-readRDS("dtm_ep.RDS")

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
  #hist(data$recommandation_SGK,col="cyan",main="Notes données par les clients",ylab="Notes",xlab="Effectif")
  chaine<-paste(paste("Il y a",as.character(nrow(data))),"documents")
  print(chaine)
  q1 <- qplot(factor(recommandation_SGK), data=data, geom="bar")
  q1 + geom_bar(fill="steelblue") + 
    stat_count(aes(label=..count..), vjust=1.5, geom="text", position="identity",color="white") +
    stat_count(aes(label = paste(sprintf("%.02f", ..count../sum(..count..)*100), "%")), 
             geom="text",vjust=3.5,color="white")+
    labs(title="Histogramme des notes données par les clients",x="Notes attribuées",y="Effectifs")+
    theme(plot.title = element_text(colour="blue",size=20,hjust=0.5))  
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
    if (hunspell_check(x,dict=dictionary("fr"))|str_length(x)<3){
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
        print(1)
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
  stemm<-function(words){
    t<-hunspell_stem(words,dict=dictionary("fr"))
    for(i in c(1:length(t))){
      effect<-(length(t[[i]])>1)
      if (effect){
        t[[i]]<-t[[i]][2]
      }
      else{
        if(length(t[[i]])==0){
          t[[i]]<-words[[i]]
        }
      }
    }
    t
  }
  handle_accent<-function(lemme){
    lemme <- str_replace_all(lemme,"ê","e")%>%str_replace_all("@card@" ,"")%>%
      str_replace_all("\"","")%>%str_replace_all("\ \"","")%>%str_replace_all("Ã®","i")%>%
      str_replace_all("suivre|Ãªtre","etre")%>%
      str_replace_all("Ã©","e")%>%str_replace_all("Ã¨","e")%>%str_replace_all("Ã","a")%>%
      str_replace_all( "Ãª","e")%>%str_replace_all("Ã§","c")%>%str_replace_all("Å","oe")%>%
      str_replace_all("Ã¹","u")%>%str_replace_all("Ã«","e")%>%str_replace_all("[EÉÈÊËéèêë]", "e")%>%
      str_replace_all("a©","e")%>%str_replace_all("a¨","e")%>%str_replace_all( "aª","e")%>%
      str_replace_all("Ã","e")%>%str_replace_all("etre","")%>%str_replace_all("avoir","")
    lemme
  }
  lemmatizeCorpus <- function(x,adress) {
    print(x)
    if (x!=""){
      suppressMessages(words.cc <- treetag(x, treetagger="manual", format="obj",
                                           TT.tknz=TRUE, lang="fr",encoding="utf-8",
                                           TT.options=list(path=adress, preset="fr")))
      words.lm <- ifelse(words.cc@TT.res$token != words.cc@TT.res$lemma, 
                         ifelse(words.cc@TT.res$lemma != "<unknown>", words.cc@TT.res$lemma,stemm(words.cc@TT.res$token)),
                         words.cc@TT.res$token)
      words.lm<-handle_accent(words.lm)
      words_tags<-words.cc@TT.res$tag
      names(words_tags)<-words.lm
      lemme <- toString(paste(words.lm, collapse = " "))
      return(c(lemme,words_tags))
    }
    else{ 
      x
    }
  }
  data$corrige<-sapply(X=data[,column],FUN=spell_checker)
  sfInit(parallel=TRUE,cpus=4,type="SOCK")
  sfLibrary(koRpus)
  sfLibrary(hunspell)
  sfLibrary(magrittr)
  sfLibrary(stringr)
  traitement <- function(x){ lemmatizeCorpus(x,adress)}
  data$enregistrement<-sfLapply(x = data$corrige, fun =traitement)
  sfStop()
  recupere2<-function(x){
    x[1]
  }
  recupere<-function(x){
    x[2:length(x)]
  }
  data$lemme<-sapply(data$enregistrement,recupere2)
  data$tags<-sapply(data$enregistrement,recupere)
  data[,-which(colnames(data)=="enregistrement")]
}

#' Delete all the numbers, spaces and ponctuations present in corpus
#' @param corpus A collection of documents containing text (obtained with Corpus)
#' @return A new corpus without the numbers,the spaces and ponctuations

notation_harmonisation<-function(table_tm){
  corpus <- VCorpus(VectorSource(table_tm), readerControl=list(reader=readPlain, language="fr"))%>%
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

library(NLP)
BigramTokenizer <-function(x){
  unlist(lapply(ngrams(words(x), 2), paste, collapse = " "), use.names = FALSE)
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
  tdm <- TermDocumentMatrix(corpus, control = list(tokenize = BigramTokenizer))
  list(dtm=dtm,dataframe_corrige=dataframe_corrige,tdm=tdm)
}

get_all_tags<-function(data){
  tags<-c()
  noms<-c()
  for (i in 1:nrow(data)){
    liste<-data$tags[i][[1]]
    for (j in 1:length(liste)){
      try({
      noms[length(noms)+1]<-names(liste)[j]
      tags[length(tags)+1]<-str_sub(liste[j],1,3)
      })
    }
  }
  grammar<-data.frame(noms=noms,tags=tags)
  grammar<-grammar[!duplicated(grammar),]
  grammar
}

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
  wordcloud(dd$word,dd$freq, scale=c(3,2),min.freq = 20, max.words = 100, random.order = F,colors= palette_couleur)
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
build_topic_models<-function(dtm, all_ks){ 
  models<-list()
  burnin <-1500
  iter <- 6000
  keep <- 100 
  seed <- 1
  for(k in all_ks){
    models[as.character(k)] <-LDA(dtm, k = k, method = "Gibbs",control = list(burnin=burnin,iter=iter,keep=keep, seed=seed))
  }
  return(models)
}

test_convergence<-function(dtm,n){
  par(mfrow=c(1,2))
  liste<-c()
  for (i in 1:n){
    liste[i]<-LDA(subset(as.matrix(dtm),(rowSums(as.matrix(dtm)) >0) ==TRUE), k = 5, method = "Gibbs",control = list(burnin=500,thin=10,iter=1500,keep=50))@loglikelihood
  }
  boxplot(liste)
  plot(liste,ylim=c(min(liste)-10,max(liste)+10))
  par(mfrow=c(1,1))
  sd(liste)
}

#test_convergence(dtm_ep,100)

coherence_tfidf<-function(model,dtm,n){ # n = number of topics in the model considered
  dtm2 <- as.matrix(weightSMART(dtm,spec="atn"))
  dtm<-as.matrix(dtm)
  phi_t <- posterior(model)$terms %>% as.matrix
  liste_score <- c()
  for (topics in 1:n){ #n -> number of topics
    col_tri <- sort(phi_t[topics,],decreasing = T)
    #We calculate the metric over the 10 best words of the topics
    w<-col_tri[1:10]
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

get_best_model<-function(all_topics_models,dtm){
  all_coherences<-list()
  indice<-1
  for(model in all_topics_models){
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
  data <- data.frame(x=2:10, y=average_coherences)
  choix <- list(
    x = (which(average_coherences ==max(average_coherences))+1),
    y=(min(average_coherences)-1),
    text = ~paste("Nombre de thèmes choisis:",as.character((which(average_coherences ==max(average_coherences))+1))),
    font = list(family = 'Arial',
                size = 16,
                color = 'rgba(49,130,189, 1)'),
    showarrow = FALSE)
  p <- plot_ly(data, x = ~x, y = ~y,name="Cohérence tf-idf moyenne", type = 'scatter', mode = 'lines')%>%
  add_trace(x= ~(which(average_coherences ==max(average_coherences))+1),name="Nombre de thèmes choisis", line = list(color = 'rgb(22, 96, 167)', width = 4))%>%
    layout(title = "Cohérence tf-idf moyenne en fonction du nombre de thèmes",
           xaxis = list(title = "Nombre de thèmes"),
           yaxis = list (title = "Cohérence tf-idf moyenne"))%>%layout(annotations=choix)%>%
    add_trace(x = ~(which(average_coherences ==max(average_coherences))+1), y = ~average_coherences[which(average_coherences ==max(average_coherences))],name="Cohérence tf-idf maximale", type = 'scatter', mode = 'markers', marker = list(color = 'rgba(67,67,67,1)', size = 8)) 
  print(p)
  #BEST MODEL:
  best_model<-all_topics_models[[as.character(which(average_coherences ==max(average_coherences))+1)]]
  best_model
}

give_best_model<-function(dtm){                                                                
  # Nombre de th?mes test?s                                                                      
  all_ks <- seq(2, 10, 1)                                                                        
  # Matrice dont les documents (lignes) contiennent au moins un terme (colonne)                  
  documents<-subset(as.matrix(dtm),(rowSums(as.matrix(dtm)) >0) ==TRUE)                          
  # Nombre optimal de th?me via LDA avec Gibbs                                                   
  all_topic_models <- build_topic_models(documents, all_ks)
  best_model <- get_best_model(all_topic_models,dtm) 
  #Output                                                                                        
  # Modele selectionne                                                                           
  best_model     
}                                                                                              


## B) ANALYSE THEMATIQUE

# Caracteristiques du modele
get_topic_assignments <- function(best){
  gammaDF <- as.data.frame(best@gamma)
  names(gammaDF) <- 1:length(names(gammaDF))
  as.data.frame(cbind(document = row.names(gammaDF), topic = apply(gammaDF,1,function(x) names(gammaDF)[which(x==max(x))])))
}

dist_topic<-function(phi){
  bp <- ggplot(diamonds, aes(clarity, fill = cut)) +
    geom_bar()
  dist_mat <- dist(phi)
  par(mfrow=c(1,1))
  fit <- cmdscale(dist_mat, eig = TRUE, k = (nrow(phi)-1))
  points <- data.frame(x = fit$points[, 1], y = fit$points[, 2])
  inertie_expl <- rep(0,times=(nrow(phi)-1))
  for (k in 1:(nrow(phi)-1)){
    clus <- kmeans(points,centers=k,nstart=5)
    inertie_expl[k] <- clus$betweenss/clus$totss
  }
  diff<-rep(0,times=(nrow(phi)-2))
  for (k in 2:(nrow(phi)-1)){
    diff[k-1]<-inertie_expl[k]-inertie_expl[k-1]
  }
  plot(1:(nrow(phi)-1),inertie_expl,type="b",xlab="Nb. de groupes",ylab="% inertie expliquée")
  k<-length(diff[diff>0.05])+1
  # Clustering
  km <- kmeans(points, centers= k, nstart=5)
  ggdata <- data.frame(points, Cluster=km$cluster, Topic=rownames(phi))
  # Figure
  p<-ggplot(ggdata) +
    geom_point(aes(x=x, y=y, color=factor(Cluster)), size=6, shape=20) + 
    guides(color=guide_legend("Cluster"),fill=guide_legend("Cluster")) + 
    geom_text(data = points, aes(x = x + 0.02, y = y),label = rownames(phi))+
    ggtitle("Représentation de la proximité entre les thèmes")
  ggplotly(p)
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
caracterise<-function(phi_t,topics_freqs,donnees){
  freq <- as.matrix(topics_freqs)
  freq2 <- as.data.frame(freq)
  freq2$theme<-paste("Thème",rownames(freq2))
  bp<-ggplot(freq2,aes(x=theme,y=V1))+geom_bar(stat="identity")+labs(title="Nombre de documents par thème",x="Thème",y="Nombre de document")
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
  tags<-get_all_tags(donnees)
  a_eliminer<-tags[tags$tags %in% c("ADJ","KON","PRP","PUN","DET","PRO","ADV","NUM"),1]
  df<- df[!df$nom %in% a_eliminer,]
  for (i in (1:(ncol(df)-1))){
    df<-df[order(df[,i],decreasing=T),]
    df<-df[!rownames(df)%in%hash_valence$x,]
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

give_theme<-function(dtm){
  #liste des themes attribuables a chaque document
  best_model<-give_best_model(dtm)
  print(best_model)
  document_topic_assignments <- get_topic_assignments(best_model)                                             #
  Topic <- topics(best_model, 1)     
  # Probabilit? de chaque mot d'appartenir ? un theme                                            
  pos<-posterior(best_model)$topics   
  phi_t <- posterior(best_model)$terms %>% as.matrix  
  # Fr?quence des th?mes 
  topic_freqs <- sort(table(unlist(document_topic_assignments$topic)), decreasing=T) 
  #MDS afin d'analyser la distance entre les th?mes                                              
  #topic_dist<-dist_topic (phi_t)                                                              
  #Sur le graphique sont affiches les numeros designat chaque theme,                            
  #les themes devant etre interprettes avec la figure obtenue avec la fonction caracterise       
  return(list(topics_freq=topic_freqs,phi_t=phi_t,document_topic_assignements=document_topic_assignments,models=best_model))                                                                                     #
}
model_theme<-give_theme(dtm_ep)
saveRDS(model_theme,"model_theme.RDS")

fonction_totale<-function(donnees,n,colonne,sparseness){
  describe_corpus(donnees)
  result<- preprocess_text(donnees,colonne,sparseness)
  dtm<-result[[1]]
  dataframe_corrige <-result[[2]]
  compar(donnees,colonne,dtm)
  nuage_de_mots(dtm)
  g<-cooccurrence(dtm)
  communaute_fg <- communaute_freq(n,dtm)
  theme <- give_theme(dtm)
  topics.freqs<-theme[[1]]
  phi_t<-theme[[2]]
  topics.assignement <- theme [[3]]
  caracterise(phi_t,topics.freqs,dataframe_corrige)
  try_tsne(phi_t)
  return(list(dtm,dataframe_corrige,topics.freqs,phi_t,topics.assignement))
}

#debut<-Sys.time()
#Rprof("D:/ENSAE/2emeannee/Statsapp/Profile.txt") # L'adresse où on veut enregistrer le résultat
#result<- fonction_totale(so_ge_prevoyance,80,"raisons_recommandation",sparseness = 0.995)
#Rprof(NULL) # Arrêter le Profiler
#summaryRprof("D:/ENSAE/2emeannee/Statsapp/Profile.txt") 
#Sys.time()-debut
#dtm_ep<-result[[1]]
#saveRDS(data,"minifichier.RDS")
