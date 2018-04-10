library(tm)
library(plotly)
library(topicmodels) 
library(ggplot2)
library(magrittr)

#' Gives
#' 
#' @param dtm A document-term format matrix
#' @return A list

give_theme <- function(dtm) {
  # List of topics attributable to each document
  best_model <- give_best_model(dtm)
  print(best_model)
  document_topic_assignments <- get_topic_assignments(best_model) 
  Topic <- topics(best_model, 1)     
  # Probability of each word belonging to a topic                                   
  pos <- posterior(best_model)$topics   
  phi_t <- posterior(best_model)$terms %>% as.matrix  
  # Frequency of topics
  topic_freqs <- sort(table(unlist(document_topic_assignments$topic)), decreasing = T) 
  # MDS to analyse the distance beetween each topic                                   
  #topic_dist <- dist_topic (phi_t)                                                              
  # On the graph are displayed the numbers designating each topic,                          
  # the topics must be interpreted with the figure obtained with the function caracterise       
  return(list(topics_freq = topic_freqs, phi_t = phi_t, theta = pos,
              models = best_model)) #document_topic_assignements = document_topic_assignments                                                                                     #
}

