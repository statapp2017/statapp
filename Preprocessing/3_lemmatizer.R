#library(SnowballC)
library(stringr)
library(koRpus)
library(magrittr)

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

lemmatizer_dataframe <- function(data, adress, column) {
  data[,column] <- sapply(data[, column],as.character)
  stemm <- function(words) {
    t <- hunspell_stem(words, dict = dictionary("fr"))
    for(i in c(1:length(t))) {
      effect <- (length(t[[i]])>1)
      if (effect){
        t[[i]] <- t[[i]][2]
      } else {
        if(length(t[[i]]) == 0){
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
  lemmatizeCorpus <- function(x, adress) {
    print(x)
    if (x!="") {
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
    } else { 
      x
    }
  }
  data$corrige <- sapply(X=data[,column],FUN=spell_checker)
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
