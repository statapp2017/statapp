library(tm)
library(magrittr)
library(NLP)

#' Delete all the numbers, spaces and ponctuations present in corpus.
#' 
#' @param corpus A collection of documents containing text (obtained with Corpus).
#' @return A new corpus without the numbers, the spaces and the ponctuations.
#' @examples notation_harmonisation(dataframe_corrige$lemme)

notation_harmonisation<-function(table_tm){
  table_tm<-str_replace_all(table_tm, "[AÁÀÂÄÃÅáàâäãå]", "a")%>%str_replace_all("[EÉÈÊËéèêë]", "e")%>%
    str_replace_all("[IÍÏÎÌíìîï]", "i")%>%str_replace_all("[NÑñ]", "n")%>%str_replace_all("[OÓÒÔÖÕóòôöõ]", "o")%>%
    str_replace_all("[UÚÙÛÜúùûü]", "u")%>%str_replace_all("[YÝýÿ]", "y")
  corpus <- VCorpus(VectorSource(table_tm), readerControl=list(reader=readPlain, language="fr"))%>%
  tm_map( content_transformer(removeNumbers))%>%tm_map(content_transformer(removePunctuation))%>%
  tm_map(content_transformer(stripWhitespace))
  corpus
}

#' Delete all the stopwords present in corpus.
#' 
#' @param corpus A collection of documents containing text (obtained with Corpus).
#' @return A new corpus without the stopwords.
#' @examples delete_stopwords(notation_harmonisation(table_tm)) 

delete_stopwords<-function(corpus){
  stopwords_plus<-c("ete","etre","avoir","j","m","car","donc","encore","avoir","etre","parce","a")
  myStopWords<-c(c(stopwords("fr")),stopwords_plus)
  corpus <- tm_map(corpus, removeWords, myStopWords)
  corpus
}

#' Create the Document-Term Matrix based on the given corpus, using the term frequency weighthing.
#' 
#' @param corpus A collection of documents containing text (obtained with Corpus).
#' @param sparseness A numeric for the maximal allowed sparsity in the range from bigger zero to smaller one.
#' @return The Document Term Matrix corresponding to the given corpus.

creation_DTM <- function(corpus, sparseness) {
  dtm <- DocumentTermMatrix(corpus, control = list(weighting = weightTf))
  removeSparseTerms(dtm, sparseness)
}

#' Create the bigram matrix based on a collection of words in the documents.
#' 
#' @param word_collection A collection of words contained in the documents (obtained with Vcorpus).
#' @return The bigram matrix corresponding to the words in the given corpus.

bigramTokenizer <- function(word_collection) {
  unlist(lapply(ngrams(words(word_collection), 2), paste, collapse = " "), use.names = FALSE)
}

#' Given a data containing sentences in \code{column}, give the Document-Term Matrix
#' after text transformations (spell correction, lemmatization, removal 
#' of stopwords, ponctuations etc...). Preprocessing of the database.
#' 
#' @param data A dataframe containing the sentences.
#' @param column The column of the dataframe containing the sentences to work on.
#' @param sparseness A numeric for the maximal allowed sparsity in the range from bigger zero to smaller one.
#' @param path The location of the folder containing the TreeTagger used.
#' @return Both the Document Term Matrix corresponding to the given sentences and
#' a new dataframe containing the lemmatisation of the sentences after spell correction.

preprocess_text<-function(data,column,sparseness=0.99){
  data$corrige<-sapply(X=data[,column],FUN=spell_checker)
  dico<-creation_dico(data$corrige)
  dataframe_corrige <- lemmatizer_dataframe(data,dico,column)
  table_tm<- dataframe_corrige$lemme
  corpus <-notation_harmonisation(table_tm)%>%delete_stopwords()
  dtm <- creation_DTM(corpus,sparseness)
  tdm <- DocumentTermMatrix(corpus, control = list(tokenize = BigramTokenizer))
  tdm<-removeSparseTerms(tdm,0.8)
  list(dtm=dtm,dataframe_corrige=dataframe_corrige,tdm=tdm)
}
