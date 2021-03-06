library(igraph)

### Community Algorithm ###

#' For a given data about words (frequency of each words, words not to consider, etc.), plots the
#' graph of community.
#' 
#' @param graph An output of the function \code{\link{cooccurrence}}.
#' @param freq_var A list with the frequency of each word.
#' @param suppr A list of words not considered in the community.
#' @return A graph illustrating the communities for the considered words (i.e. without \code{suppr}).

communaute <- function(graph, freq_var, suppr) {
  mst <- minimum.spanning.tree(graph, weights = 1 - get.edge.attribute(graph, "weight"), algorithm = "prim")
  # Calculation of the community with the fastgreedy method.
  com <- fastgreedy.community(mst, merges = TRUE, modularity = TRUE)
  groups <- as.matrix(table(fastgreedy.community(mst)$membership)) 
  com_m <- membership(com)
  groupe <- list()
  for (i in c(1:length(com_m))) {
    groupe[i] <- com_m[[i]]
  }
  groups <- data.frame(name = rownames(groups), freq = groups[,1])
  group_members <- data.frame(vid = V(mst)$id)
  for (i in c(1:nrow(group_members))) {
    group_members$communaute[i] <- com_m[[i]]
  }
  mst <- delete.vertices(mst, V(mst)[name %in% suppr])
  titre <- "Detection de communaute basee sur la modularite fastgreedy"
  multiplicateur <- 20 / log(max(freq_var))
  plot(mst, layout = layout.fruchterman.reingold, vertex.color = com_m, vertex.label = V(mst)$id, #name
       vertex.size = log(freq_var) * multiplicateur, edge.color = "grey55", edge.arrow.size = 1, 
       main = titre)
  legend(x = "bottomleft", legend = paste("Il y a", as.character(nrow(groups)), "communautés."), bty="n")
  return(list(groups, group_members))
} 

#' Plots the graphs of the community. It only keeps the \code{n} most frequent terms in the database.
#' In the graph, a community is represented by a color and the size of a vertex is proportionnal
#' to the log of its use frequency.
#' 
#' @param n The number of words to keep in the graph. We recommend n < 100 for more clarity.
#' @param dtm The document-term matrix obtained after the preprocessing.
#' @return Plot the graph illustrating the communities associated with the \code{n} most
#' used words in the set of documents.

communaute_freq <- function(n, dtm) {
  cooc_dtm <- cooccurrence(dtm)
  dtm_matrix <- t(as.matrix(dtm[, - 1]))
  freq <- NULL
  for (i in 1:length(rownames(dtm_matrix))) {
    freq[i] <- sum(dtm_matrix[i, ])}
  freq <- as.matrix(freq)
  rownames(freq) <- rownames(dtm_matrix)
  freq <- freq[order(freq[, 1], decreasing = T), ]
  suppr <- names(freq[(n + 1):length(freq)])
  communaute(cooc_dtm, freq[1:n], suppr)
}
