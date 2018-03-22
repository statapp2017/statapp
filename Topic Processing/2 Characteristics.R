library(plotly)
library(ggplot2)

## ANALYSE THEMATIQUE ##
# Caracteristiques du modele
get_topic_assignments <- function(best){
  gammaDF <- as.data.frame(best@gamma)
  names(gammaDF) <- 1:length(names(gammaDF))
  as.data.frame(cbind(document = row.names(gammaDF), topic = apply(gammaDF, 1, function(x) names(gammaDF)[which(x==max(x))])))
}

dist_topic<-function(phi){
  bp <- ggplot(diamonds, aes(clarity, fill = cut)) + geom_bar()
  dist_mat <- dist(phi)
  par(mfrow = c(1, 1))
  fit <- cmdscale(dist_mat, eig = TRUE, k = (nrow(phi)-1))
  points <- data.frame(x = fit$points[, 1], y = fit$points[, 2])
  inertie_expl <- rep(0, times = (nrow(phi)-1))
  for (k in 1:(nrow(phi)-1)) {
    clus <- kmeans(points,centers=k,nstart=5)
    inertie_expl[k] <- clus$betweenss/clus$totss
  }
  diff <- rep(0, times=(nrow(phi)-2))
  for (k in 2:(nrow(phi)-1)) {
    diff[k-1] <- inertie_expl[k]-inertie_expl[k-1]
  }
  plot(1:(nrow(phi)-1),inertie_expl,type="b",xlab="Nb. de groupes",ylab="% inertie expliquée")
  k <- length(diff[diff>0.05])+1
  # Clustering
  km <- kmeans(points, centers= k, nstart=5)
  ggdata <- data.frame(points, Cluster=km$cluster, Topic=rownames(phi))
  ggdata
}

# Figure
dist_topic_figure <- function(phi){
  ggdata <- dist_topic(phi)
  p <- ggplot(ggdata) +
    geom_point(aes(x=x, y=y, color=factor(Cluster)), size=6, shape=20) + 
    guides(color=guide_legend("Cluster"), fill=guide_legend("Cluster")) + 
    geom_text(data = points, aes(x = x + 0.02, y = y), label = rownames(phi))+
    ggtitle("Représentation de la proximité entre les thèmes")
  ggplotly(p)
}