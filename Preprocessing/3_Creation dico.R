#' 
#' 
#' @param table_tm 
#' @return 
#' @examples

creation_dico <- function(table_tm) {
  # Suppress numbers, punctuations and spaces from table_tm.
  corpus <- VCorpus(VectorSource(table_tm), readerControl = list(reader = readPlain, language="fr")) %>%
    tm_map(content_transformer(removeNumbers)) %>% tm_map(content_transformer(removePunctuation)) %>%
    tm_map(content_transformer(stripWhitespace))
  dtm <- DocumentTermMatrix(corpus, control = list(weighting = weightTf))$dimnames
  dico <- data.frame(mots = dtm$Terms)
  # Stemming of words with hunspell.
  stemming <- function(word) {
    stem <- hunspell_stem(as.character(word), dict = dictionary("fr"))[[1]] # Possibilities of stem.
    if (identical(stem, character(0))) { # If the stem is not in the dictionary.
      stem <- as.character(word)
    } else { # If there are possible stems in the dictionary
      stem <- stem[1]
    }
    stem
  }
  dico$stem <- sapply(dico$mots, FUN = stemming)
  dico
}

