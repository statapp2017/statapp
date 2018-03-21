library(tokenizers)
library(hunspell)


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
