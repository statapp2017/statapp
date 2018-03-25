library(tm)
library(plotly)
library(topicmodels) 
library(ggplot2)
library(magrittr)

## ANALYSE THEMATIQUE ##
give_theme <- function(dtm) {
  # Liste des themes attribuables a chaque document
  best_model <- give_best_model(dtm)
  print(best_model)
  document_topic_assignments <- get_topic_assignments(best_model) 
  Topic <- topics(best_model, 1)     
  # Probabilite de chaque mot d'appartenir a un theme                                            
  pos <- posterior(best_model)$topics   
  phi_t <- posterior(best_model)$terms %>% as.matrix  
  # Frequence des themes 
  topic_freqs <- sort(table(unlist(document_topic_assignments$topic)), decreasing = T) 
  # MDS afin d'analyser la distance entre les themes                                              
  #topic_dist<-dist_topic (phi_t)                                                              
  #Sur le graphique sont affiches les numeros designant chaque theme,                            
  #les themes devant etre interpretes avec la figure obtenue avec la fonction caracterise       
  return(list(topics_freq = topic_freqs, phi_t = phi_t, theta = pos,
              models = best_model)) #document_topic_assignements = document_topic_assignments                                                                                     #
}

model_theme <- give_theme(dtm_ep)
#saveRDS(model_theme,"model_theme.RDS")
