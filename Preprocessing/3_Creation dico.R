
creation_dico <- function(table_tm) {
  corpus <- VCorpus(VectorSource(table_tm), readerControl=list(reader=readPlain, language="fr"))%>%
    tm_map(content_transformer(removeNumbers))%>%tm_map(content_transformer(removePunctuation))%>%
    tm_map(content_transformer(stripWhitespace))
  dtm <- DocumentTermMatrix(corpus,control = list(weighting = weightTf))$dimnames
  dico <- data.frame(mots = dtm$Terms)
  stemming <- function(word) {
    stemm <- hunspell_stem(as.character(word), dict = dictionary("fr"))[[1]]
    if (identical(stemm, character(0))) {
      stemm <- as.character(word)
    } else {
      stemm <- stemm[1]
    }
    stemm
  }
  dico$stem <- sapply(dico$mots, FUN=stemming)
  dico
}
