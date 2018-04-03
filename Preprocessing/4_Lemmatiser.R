library(stringr)
library(koRpus)
library(magrittr)
library(hunspell)

#' Transform all the sentences contained in a given column of a dataframe. 
#' See lemmatizer for the precise transformations.
#' 
#' @param data The dataframe containing the sentences.
#' @param dico A dictionnary created with creation_dico which contains all
#' the words of the database and their stem.
#' @param column The column of \code{data} containing the sentences.
#' @return A new dataframe with the same columns as \code{data} but with a
#' new column \code{lemme} containing all the sentences lemmatized 
#' @examples
#' lemmatizer_dataframe(my_data, dictionnary, "Text")

lemmatizer_dataframe <- function(data, dico, column) {
  data[, column] <- sapply(data[, column], as.character)
  lemmatizeCorpus <- function(x) {
    list_words <- tokenize_words(as.character(gsub(" \n", ". ", x)), lowercase = TRUE)[[1]] 
    for (i in (1:length(list_words))){
      if (!identical(is.na(list_words[i] == "NA"),logical(0))) {
        if (list_words[i] %in% dico[, "mots"]) {
          list_words[i] <- dico[dico[, "mots"] == list_words[i], "stem"]
        }
      }
    }
    lemme <- toString(paste(list_words, collapse = " "))
    lemme
  }
  for (i in (1:nrow(data))) {
    data$lemme[i] <- lemmatizeCorpus(str_replace_all(data$corrige[i],"'"," "))
  }
  data
}

