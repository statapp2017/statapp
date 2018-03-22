######################Algorithme de communauté###########################

communaute <- function(graph, freq_var, suppr){
  mst <- minimum.spanning.tree(graph, weights = 1 - get.edge.attribute(graph, "weight"), algorithm = "prim")
  # Calculation of the community with the fastgreedy method
  com <- fastgreedy.community(mst, merges=TRUE, modularity=TRUE)
  groups <- as.matrix( table(fastgreedy.community(mst)$membership) ) 
  com_m <- membership(com)
  groupe <- list()
  for (i in c(1:length(com_m))) {
    groupe[i] <- com_m[[i]]
  }
  groups <- data.frame(name = rownames(groups), freq = groups[,1])
  group_members <- data.frame(vid = V(mst)$id)
  for (i in c(1:nrow(group_members))) {
    group_members$communaute[i]<-com_m[[i]]
  }
  mst <- delete.vertices(mst, V(mst)[name %in% suppr])
  mst <- delete.vertices(mst, V(mst)[name %in% c("etre","card","avoir")])
  titre <- "Détection de communauté basée sur la modularité fastgreedy"
  multiplicateur <- 20 / log(max(freq_var))
  plot(mst, layout = layout.fruchterman.reingold, vertex.color = com_m,
       vertex.label = V(mst)$id, # name
       vertex.size = log(freq_var) * multiplicateur, 
       edge.color = "grey55", edge.arrow.size = 1, main = titre)
  legend(x = "bottomleft", legend=paste(paste("Il y a", as.character(nrow(groups))), "communautés."), bty="n")
  return(list(groups, group_members))
} 

#' Plot the graphs of the community. It only keeps the \code{n} most frequent term in the database.
#' In the graph a community is represented by a color and the size of a vertices is proportionnal
#' to the log of its use frequency.
#' @param n The number of words to keep in the graph. We recommend n < 100 for more clarity.
#' @param dtm The document-term matrix obtained after the preprocessing 
#' @return Plot the graph illustrating the communities associated with the n most
#' used words in the set of documents 

communaute_freq <- function(n, dtm){
  cooc_dtm <- cooccurrence(dtm)
  d <- t(as.matrix(dtm[, -1]))
  freq <- NULL
  for (i in 1:length(rownames(d))) {
    freq[i] = sum(d[i, ])}
  freq <- as.matrix(freq)
  rownames(freq) <- rownames(d)
  freq <- freq[order(freq[, 1], decreasing = T), ]
  suppr <- names(freq[(n + 1):length(freq)])
  freq <- freq[1:n]
  communaute(cooc_dtm, freq, suppr)
}
