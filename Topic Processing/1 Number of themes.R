library(tm)
library(plotly)
library(topicmodels) 
library(magrittr)

## TROUVER LE NOMBRE OPTIMAL DE THEMES: k ##
# Construction des modeles pour toutes les valeurs de k que l'on veut tester, par la methode de Gibbs
# all.k : vecteur representant les valeurs des nombres de themes a tester 
# dtm_in : matrice en format Document-Terme

build_topic_models <- function(dtm, all_ks) { 
  models <- list()
  burnin <- 1500
  iter <- 6000
  keep <- 100 
  seed <- 1
  for(k in all_ks) {
    models[as.character(k)] <- LDA(dtm, k = k, method = "Gibbs", control = list(burnin = burnin, iter=iter, keep = keep, seed = seed))
  }
  return(models)
}

# Visualisation
test_convergence <- function(dtm, n) {
  par(mfrow = c(1,2))
  liste <- c()
  for (i in 1:n) {
    liste[i] <- LDA(subset(as.matrix(dtm), (rowSums(as.matrix(dtm)) > 0) == TRUE), k = 5, method = "Gibbs", 
                    control = list(burnin = 500, thin = 10, iter = 1500,keep = 50))@loglikelihood
  }
  boxplot(liste)
  plot(liste, ylim = c(min(liste) - 10, max(liste) + 10))
  par(mfrow = c(1, 1))
  sd(liste)
}

#test_convergence(dtm_ep, 100)

coherence_tfidf <- function(model, dtm, n) { # n = number of topics in the model considered
  dtm2 <- as.matrix(weightSMART(dtm,spec="atn"))
  dtm <- as.matrix(dtm)
  phi_t <- posterior(model)$terms %>% as.matrix
  liste_score <- c()
  for (topics in 1:n) {# n -> number of topics
    col_tri <- sort(phi_t[topics,],decreasing = T)
    # We calculate the metric over the 10 best words of the topics
    w <- col_tri[1:10]
    u <- names(w)
    somme <- 0
    for (i in 1:(length(u)-1)) {
      for (j in (i+1):length(u)) {
        sub_dtm <- dtm[dtm[,u[i]] > 0 & dtm[, u[j]] > 0, ]
        high_sum <- sum(dtm2[rownames(sub_dtm), u[i]]*dtm2[rownames(sub_dtm), u[j]]) + 1
        low_sum <- sum(dtm[,u[i]])
        somme <- somme+log(high_sum/low_sum)
      }
    }
    liste_score[topics] <- somme
  }
  liste_score
}

get_best_model <- function(all_topics_models, dtm) {
  all_coherences <- list()
  indice <- 1
  for(model in all_topics_models) {
    all_coherences[[indice]] <- coherence_tfidf(model,dtm,indice+1)
    indice <- indice + 1
  }
  # calculer la moy des coherences pour le theme i
  moyenne <- function(all_coherences,i) {
    mean(all_coherences[[i]])
  }
  #liste des coherences moyennes pour tous les nombres de themes tests
  average_coherences <- c()
  Nombre_de_k <- length(all_coherences)
  for(i in 1:Nombre_de_k) {
    average_coherences[i] <- moyenne(all_coherences, i)
  }
  data <- data.frame(x=2:10, y=average_coherences)
  choix <- list(
    x = (which(average_coherences == max(average_coherences))+1),
    y =(min(average_coherences)-1),
    text = ~paste("Nombre de thèmes choisis :", as.character((which(average_coherences == max(average_coherences))+1))),
    font = list(family = 'Arial', size = 16, color = 'rgba(49,130,189, 1)'),
    showarrow = FALSE)
  p <- plot_ly(data, x = ~x, y = ~y,name="Cohérence tf-idf moyenne", type = 'scatter', mode = 'lines')%>%
    add_trace(x= ~(which(average_coherences == max(average_coherences))+1), name="Nombre de thèmes choisis", line = list(color = 'rgb(22, 96, 167)', width = 4))%>%
    layout(title = "Cohérence tf-idf moyenne en fonction du nombre de thèmes",
           xaxis = list(title = "Nombre de thèmes"),
           yaxis = list(title = "Cohérence tf-idf moyenne"))%>%layout(annotations=choix)%>%
    add_trace(x = ~(which(average_coherences == max(average_coherences))+1), y = ~average_coherences[which(average_coherences ==max(average_coherences))],name="Cohérence tf-idf maximale", type = 'scatter', mode = 'markers', marker = list(color = 'rgba(67,67,67,1)', size = 8)) 
  print(p)
  #BEST MODEL :
  best_model<-all_topics_models[[as.character(which(average_coherences ==max(average_coherences))+1)]]
  best_model
}

give_best_model <- function(dtm) {                                                                
  # Nombre de themes testes                                                                      
  all_ks <- seq(2, 10, 1)                                                                        
  # Matrice dont les documents (lignes) contiennent au moins un terme (colonne)                  
  documents <- subset(as.matrix(dtm),(rowSums(as.matrix(dtm)) >0) ==TRUE)                          
  # Nombre optimal de theme via LDA avec Gibbs                                                   
  all_topic_models <- build_topic_models(documents, all_ks)
  # Modele selectionne
  get_best_model(all_topic_models, dtm) 
}      
