library(tm)

#' Delete all the numbers, spaces and ponctuations present in corpus
#' @param corpus A collection of documents containing text (obtained with Corpus)
#' @return A new corpus without the numbers,the spaces and ponctuations

notation_harmonisation <- function(table_tm) {
  corpus <- VCorpus(VectorSource(table_tm), readerControl=list(reader=readPlain, language="fr"))%>%
    tm_map( content_transformer(removeNumbers))%>%tm_map(content_transformer(removePunctuation))%>%
    tm_map(content_transformer(stripWhitespace))
  corpus
}

#' Delete all the stopwords present in corpus
#' @param corpus A collection of documents containing text (obtained with Corpus)
#' @return A new corpus without the stopwords

delete_stopwords <- function (corpus) {
  myStopWords <- c(stopwords("fr"))
  tm_map(corpus, removeWords, myStopWords)
}

#' Create the Document-Term Matrix based on the given corpus. It uses the term frequency weighthing
#' @param corpus A collection of documents containing text (obtained with Corpus)
#' @param sparseness A numeric for the maximal allowed sparsity in the range from bigger zero to smaller one.
#' @return The document Term Matrix corresponding to the given corpus

creation_DTM <- function(corpus, sparseness) {
  dtm <- DocumentTermMatrix(corpus, control = list(weighting = weightTf))
  dtm <- removeSparseTerms(dtm, sparseness)
  dtm
}

library(NLP)
bigramTokenizer <-function(x){
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

preprocess_text<-function(data, column, sparseness = 0.99, file="C:/TreeTagger") {
  dataframe_corrige <- lemmatizer_dataframe(data, file, column)
  table_tm <- dataframe_corrige$lemme
  corpus <-notation_harmonisation(table_tm)%>%delete_stopwords()
  dtm <- creation_DTM(corpus, sparseness)
  tdm <- TermDocumentMatrix(corpus, control = list(tokenize = BigramTokenizer))
  list(dtm = dtm, dataframe_corrige = dataframe_corrige, tdm = tdm)
}

get_all_tags <- function(data) {
  tags <- c()
  noms <- c()
  for (i in 1:nrow(data)) {
    liste <- data$tags[i][[1]]
    for (j in 1:length(liste)) {
      try({noms[length(noms) + 1] <- names(liste)[j]
          tags[length(tags) + 1] <- str_sub(liste[j], 1, 3)
      })
    }
  }
  grammar <- data.frame(noms = noms, tags = tags)
  grammar[!duplicated(grammar), ]
}
