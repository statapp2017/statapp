#chargement des packages
install.packages('survival')
install.packages('topicmodels') 
install.packages('Rmpfr') 
install.packages("wordcloud")
install.packages('RColorBrewer') 
install.packages('ggplot2')
install.packages('lattice')
install.packages('rjmcmc')
install.packages('magrittr')

library(survival)
library(topicmodels) 
library(Rmpfr) 
library(wordcloud)
library(RColorBrewer) 
library(ggplot2)
library(lattice)
library(rjmcmc)
library(magrittr)



## A) TROUVER LE NOMBRE OPTIMAL DE THEMES: k

# Construction des modèles pour toutes les valeurs de k que l'on veut tester, par la méthode de Gibbs
#all.k un vecteur représentant les valeurs des nombres de thèmes à tester 
#dtm_in matrice en format Document-Terme
build.topic.models<-function(dtm, all.ks, alpha=0.1){ 
  models <- list()
  burnin <-0
  iter <- 1000
  keep <- 50 
  seed <- 1
  for(k in all.ks){
    models[as.character(k)] <-LDA(dtm, k = k, method = "Gibbs",control = list(burnin=burnin, iter=iter, keep=keep, seed=seed))
  }
  return(models)
}

#Calcul de la moyenne harmonique pour tous les modèles
harmonicMean <- function(logLikelihoods) {
  precision <- 2000L
  llMed <- median(logLikelihoods)
  as.double(llMed - log(mean(exp(-mpfr(logLikelihoods, prec = precision) + llMed))))
}

# Comparaison des modèles
evaluate.topic.models <- function(topic.models) {
  all.ks <- as.integer(names(all.topic.models)) # Nombre de thèmes 
  model.harmonic.means <- sapply(unlist(lapply(topic.models,logLik)),harmonicMean) #Moyenne Harm.
  return(list(k=all.ks[which.max(model.harmonic.means)], scores=model.harmonic.means, all.ks=all.ks)) } #retourner tous les k maximisant la vraisemblance

# Representation graphique des résultats
plot.ks <- function(res) { 
  all.ks <- res$all.ks
  scores <- res$scores
  best.k <- res$k
  par(list(oma=c(0, 0, 0, 0), mai=c(0.50, 0.30, 0.10, 0.05), cex=0.6))
  plot(all.ks, scores, type = "l", xlab = "Number of topics", axes=F) 
  axis(1, at=c(2:30), col.axis="black", tick=T, las=1)
  axis(2, at=range(scores), labels=F, col.axis="black", las=1)
  if(is.null(best.k)==F) { 
    abline(v=best.k, col="red", lty=3)
    y.text.pos <- range(scores)[1] + (0.8*(range(scores)[2]-range(scores)[1])) 
    text(x=best.k, y=y.text.pos, labels=as.character(best.k))
  }
}

# Meilleur modèle - Optimum nombre de thème (aussi reperable sur le graphique obtenu precedement)
get.best.model <- function(all.topic.models) {
  evaluation.results <- evaluate.topic.models(all.topic.models)
  plot.ks(evaluation.results)
  all.topic.models[[as.character(evaluation.results$k)]]}

################################################################################################
#charger la dtm                                                                                #
dtm<-readRDS(file = "dtm.rds")                                                                 #
                                                                                               #
# Nombre de thèmes testés                                                                      #
all.ks <- seq(2, 22, 2)                                                                        #
# Matrice dont les documents (lignes) contiennent au moins un terme (colonne)                  #
documents<-subset(as.matrix(dtm),(rowSums(as.matrix(dtm)) >0) ==TRUE)                          #
                                                                                               #
# Nombre optimal de thème via LDA avec Gibbs                                                   #
all.topic.models <- build.topic.models(documents, all.ks, alpha=0.1)                           #
get.best.model(all.topic.models)                                                               #
best.model <- get.best.model(all.topic.models)                                                 #
                                                                                               #
#Output                                                                                        #
# Modèle selectionné                                                                           #
best.model                                                                                     #
# Nombre de themes retenus                                                                     #
k<-best.model@k                                                                                #
################################################################################################



## B) ANALYSE THEMATIQUE

# Caracteristiques du modèle
get.topic.assignments <- function(best){
  gammaDF <- as.data.frame(best@gamma)
  names(gammaDF) <- 1:length(names(gammaDF))
  as.data.frame(cbind(document = row.names(gammaDF), topic = apply(gammaDF,1,function(x) names(gammaDF)[which(x==max(x))])))
}
# Calcul des distances entre les thèmes
dist_topic<-function(phi, gp_topic){
  dist.mat <- dist(phi)
  # MDS (positionnement multidimensionnel)
  par(mfrow=c(1,1))
  fit <- cmdscale(dist.mat, eig = TRUE, k = 2)
  points <- data.frame(x = fit$points[, 1], y = fit$points[, 2])
  d<-ggplot(points, aes(x = x, y = y))
  d + geom_point() + geom_text(data = points, aes(x = x, y = y - 0.005),label = rownames(phi))
  # Clustering
  km <- kmeans(points, centers= gp_topic, nstart=5)
  ggdata <- data.frame(points, Cluster=km$cluster, Topic=rownames(phi))
  # Figure
  ggplot(ggdata) +
    geom_point(aes(x=fit$points[, 1], y=fit$points[, 2], color=factor(Cluster)), size=6, shape=20) + 
    stat_ellipse(aes(x=fit$points[, 1],y=fit$points[, 2],fill=factor(Cluster)),
                 geom="polygon", level=0.95, alpha=0.2) + 
    guides(color=guide_legend("Cluster"),fill=guide_legend("Cluster")) + 
    geom_text(data = points, aes(x = x + 0.02, y = y),label = rownames(phi))}

#carateriser le i-eme theme
caracterise<-function(phi_t,i){
    colonne<-phi_t[i,]
    col.tri <- sort(colonne,decreasing = T)
    liste<-c(col.tri[1],col.tri[2],col.tri[3],col.tri[4],col.tri[5],col.tri[6],col.tri[7])
    n<-length(col.tri)
    titre = paste('Theme',i)
    barplot(liste, main = titre,ylim = c(0,col.tri[1]+0.05),axes = T)
}

################################################################################################
                                                                                               #
#liste des themes attribuables a chaque document                                               #
document.topic.assignments <- get.topic.assignments(best.model)                                #
                                                                                               #
# Assignement des documents au thème le plus probable                                          #
Topic <- topics(best.model, 1)                                                                 #
                                                                                               #
# Probabilité de chaque mot d’appartenir à un theme                                            #
pos<-posterior(select.model)$topics                                                            #
phi_t <- posterior(select.model)$terms %>% as.matrix                                           #
                                                                                               #
                                                                                               #
#caracterisations des themes                                                                   #
par(mfrow=c(2,2))                                                                              #
caracterise(phi_t,5)                                                                           #
caracterise(phi_t,6)                                                                           #
caracterise(phi_t,7)                                                                           #
caracterise(phi_t,8)                                                                           #
                                                                                               #
# Fréquence des thèmes                                                                         #
topic.freqs <- sort(table(unlist(document.topic.assignments$topic)), decreasing=T)             #
topic.freqs                                                                                    #
                                                                                               #
#MDS afin d’analyser la distance entre les thèmes                                              #
topic_dist<-dist_topic (phi_t, 5)                                                              #
#Sur le graphique sont affiches les numeros designat chaque theme,                             #
#les themes devant etre interprettes avec la figure obtenue avec la fonction caracterise       #
topic_dist                                                                                     #
                                                                                               #
################################################################################################
