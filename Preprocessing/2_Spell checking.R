library(tokenizers)
library(hunspell)

#' Correct the spelling of a given sentence.
#' 
#' @param sentence A string in French
#' @return The correction of \code{sentence}.
#' @examples
#' spell_checker("Il pleut aujourd'hui.")
#' spell_checker("Je sui contre ce projet.")

spell_checker <- function(sentence) {
  # Tokenization of the sentence 
  list_words <- tokenize_words(as.character(gsub(" \n", ". ", sentence)), lowercase = TRUE)[[1]]
  correct_string <- ""
  for (x in list_words) {
    # Check if the spelling of the words is correct.
    if (hunspell_check(x, dict = dictionary("fr"))|str_length(x) < 3) {
      correct_string <- paste(correct_string,x)
    } else {
      # If the spelling isn't accurate we recover hunspell suggestions
      suggestion<-hunspell_suggest(x, dict = dictionary("fr"))[[1]]
      stop <- TRUE
      i <- 1
      while (stop & i < 3) {
        # We check that the word is French before we accept the change
        if (suggestion[i] %in% dico[[1]]){
          correct_string <- paste(correct_string, suggestion[i])
          stop <- FALSE
        } else {
          i <- i + 1
        }
      }
      # If the algorithm isn't able to find a good spelling, we leave it as it was
      if (i == 5) {
        correct_string <- paste(correct_string, x)
      }
    }  
  }
  correct_string
}
