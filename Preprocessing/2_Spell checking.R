library(tokenizers)
library(hunspell)

#' Correct the spelling of a given sentence.
#' 
#' @param sentence A string in French.
#' @return The correction of \code{sentence}.
#' @examples
#' spell_checker("Il pleut aujourd'hui.")
#' spell_checker("Je sui contre ce projet.")

spell_checker <- function(sentence) {
  # Tokenization of the sentence 
  list_words <- tokenize_words(as.character(gsub(" \n", ". ", sentence)), lowercase = TRUE)[[1]]
  correct_string <- ""
  for (word in list_words) {
    # Check if the spelling of the words is correct.
    if (hunspell_check(word, dict = dictionary("fr"))) {
      correct_string <- paste(correct_string, word)
    } else {
      # If the spelling is not accurate, we recover hunspell suggestions.
      suggestion <- hunspell_suggest(word, dict = dictionary("fr"))[[1]]
      stop <- TRUE
      i <- 1
      while (stop & i < 3) {
        # We check that the word is French before we accept the modification.
        if (suggestion[i] %in% dico[[1]]) {
          correct_string <- paste(correct_string, suggestion[i])
          stop <- FALSE
        } else {
          i <- i + 1
        }
      }
      # If the algorithm is not able to find a good spelling, we leave it as it was.
      if (i == 5) {
        correct_string <- paste(correct_string, word)
      }
    }  
  }
  correct_string
}
