library(tm)
library(plotly)
library(topicmodels) 
library(magrittr)

## FIND THE OPTIMAL NUMBER OF TOPICS: k ##

#' Gets the list of all the topic models we want to test (one for each value of k we want to test) 
#' using Gibbs sampling.
#' 
#' @param dtm A document-term format matrix
#' @param all_ks A vector of all the values of k (number of topics) we will test
#' @return A list of all the topic models

build_topic_models <- function(dtm, all_ks) { 
  models <- list()
  # LDA parameters
  burnin <- 500
  iter <- 6000
  keep <- 100 
  seed <- 1
  for(k in all_ks) {
    models[as.character(k)] <- LDA(dtm, k = k, method = "Gibbs", 
                                   control = list(burnin = burnin, iter = iter, keep = keep, seed = seed))
  }
  return(models)
}

#' Gives the metric to assess the quality of a topic.
#' 
#' @param model A given topic model 
#' @param dtm A document-term matrix
#' @param n The number of topics in the topic model considered
#' 

coherence_tfidf <- function(model, dtm, n) {
  dtm2 <- as.matrix(weightSMART(dtm, spec = "atn"))
  dtm <- as.matrix(dtm)
  phi_t <- posterior(model)$terms %>% as.matrix
  liste_score <- c()
  for (topics in 1:n) {
    col_tri <- sort(phi_t[topics, ], decreasing = T)
    # We calculate the metric over the 10 best words of the topics
    w <- col_tri[1:10]
    u <- names(w)
    somme <- 0
    for (i in 1:(length(u) - 1)) {
      for (j in (i + 1):length(u)) {
        sub_dtm <- dtm[dtm[, u[i]] > 0 & dtm[, u[j]] > 0, ]
        high_sum <- sum(dtm2[rownames(sub_dtm), u[i]] * dtm2[rownames(sub_dtm), u[j]]) + 1
        low_sum <- sum(dtm[, u[i]])
        somme <- somme + log(high_sum / low_sum)
      }
    }
    liste_score[topics] <- somme
  }
  liste_score
}

#' Gets the topic model whose topics maximize the average coherence tf-idf.
#' 
#' @param all_topics_models The list of all topics models
#' @param dtm A document-term matrix

get_best_model <- function(all_topics_models, dtm) {
  all_coherences <- list()
  indice <- 1
  for(model in all_topics_models) {
    all_coherences[[indice]] <- coherence_tfidf(model, dtm, indice + 1)
    indice <- indice + 1
  }
  # Get the average coherence for the i-th topic model
  average <- function(all_coherences, i) {
    mean(all_coherences[[i]])
  }
  # List of average tf-idf coherence for all numbers of topics tested
  average_coherences <- c()
  Number_of_k <- length(all_coherences)
  for(i in 1:Number_of_k) {
    average_coherences[i] <- average(all_coherences, i)
  }
  data <- data.frame(x = 2:10, y = average_coherences)
  choice <- list(
    x = (which(average_coherences == max(average_coherences)) + 1),
    y = (min(average_coherences) - 1),
    text = ~paste("Nombre de thèmes choisis :", as.character((which(average_coherences == max(average_coherences))+1))),
    font = list(family = 'Arial', size = 16, color = 'rgba(49,130,189, 1)'),
    showarrow = FALSE)
  p <- plot_ly(data, x = ~x, y = ~y, name = "Cohérence tf-idf moyenne", type = "scatter", mode = "lines") %>%
    add_trace(x= ~(which(average_coherences == max(average_coherences)) + 1), name="Nombre de thèmes choisis", line = list(color = 'rgb(22, 96, 167)', width = 4)) %>%
    layout(title = "Cohérence tf-idf moyenne en fonction du nombre de thèmes", xaxis = list(title = "Nombre de thèmes"),
           yaxis = list(title = "Cohérence tf-idf moyenne"))%>%layout(annotations = choice) %>%
    add_trace(x = ~(which(average_coherences == max(average_coherences)) + 1), y = ~average_coherences[which(average_coherences == max(average_coherences))], 
              name = "Cohérence tf-idf maximale", type = 'scatter', mode = 'markers', marker = list(color = 'rgba(67,67,67,1)', size = 8)) 
  print(p)
  # BEST MODEL :
  all_topics_models[[as.character(which(average_coherences == max(average_coherences)) + 1)]]
}

#' Gives the best topic model for a given document-term matrix.
#' 
#' @param dtm A document-term matrix

give_best_model <- function(dtm) {                                                                
  # Number of tested topics                                                              
  all_ks <- seq(2, 10, 1)                                                                        
  # Matrix whose documents (lines) have at least one element (column)
  # Matrice dont les documents (lignes) contiennent au moins un terme (colonne)                  
  documents <- subset(as.matrix(dtm), (rowSums(as.matrix(dtm)) > 0) == TRUE)                          
  # Best number of topics via LDA with Gibbs                                                   
  all_topic_models <- build_topic_models(documents, all_ks)
  # Selected model
  get_best_model(all_topic_models, dtm) 
}      
