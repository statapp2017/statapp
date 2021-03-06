library(RColorBrewer)
library(wordcloud)
library(igraph)
library(graphics)

#' Display the histogram reprensenting the distribution of words before and after
#' the text preprocessing.
#' 
#' @param data The dataframe containing the original sentences.
#' @param column The column of \code{data} containing the sentences.
#' @param dtm The document-term matrix obtained after the preprocessing.
#' @return The histogram of the repartition of words before and after the lemmatisation and also 
#' their numbers.

compar <- function(data, column, dtm) {
  words_preprocessing <- sapply(strsplit(as.character(data[[column]]), " "), length)
  nombre <- ncol(DocumentTermMatrix(Corpus(VectorSource(data[[column]]), 
                                           readerControl = list(reader = readPlain, language = "fr")), 
                                    control = list(weighting = weightTf)))
  print(paste("Il y a", as.character(nombre), "mots avant la lemmatisation."))
  barplot(words_preprocessing, xlab = "Nombre de termes avant le preprocessing.", col = 'blue')
  # After the lemmatisation
  print(paste("Il y a", as.character(ncol(dtm)), "mots apres la lemmatisation."))
  barplot(apply(dtm, 1, sum), xlab = "Nombre de termes dans la DTM", col = 'blue')
}

#' Give the frequency of each words used in a document-term matrix.
#' 
#' @param dtm_in The document-term matrix obtained after the preprocessing.
#' @return A dataframe sorted by frequency given the frequency for each words in \code{dtm_in}.

freq_word <- function(dtm_in) {
  # Sum of word occurrences and sorting in descending order.
  words_sum <- sort(colSums(as.matrix(dtm_in)), decreasing=TRUE)
  # Frequency table
  data.frame(word = names(words_sum), freq = words_sum)
}

#' Display the wordcloud corresponding to a given Document-Term Matrix.
#' 
#' @param dtm The document-term matrix obtained after the preprocessing.
#' @return Plot the worcloud corresponding to a given Document-Term Matrix.

nuage_de_mots <- function(dtm) {
  data_freq <- freq_word(dtm)
  couleur <- brewer.pal(n = 8, name = "Dark2")
  wordcloud(data_freq$word, data_freq$freq, scale = c(3, 2), min.freq = 20, max.words = 100, random.order = F, colors = couleur)
}

#' Create an igraph object with the words as vertices and the edges indexed by the weight of their relationship.
#' The choosen weight is absence prensence. (1 if there are always in the same documents, 0 if they never are). 
#' Only the edges with a positive weigth are present in the igraph. 
#' 
#' @param dtm_in The document-term matrix obtained after the preprocessing.
#' @return Return an igraph object reprensenting the coocurrences between the words in the \code{dtm_in}.

cooccurrence <- function(dtm_in) {
  # Preparation of the data, d contains a matrix with every distance between words
  # with the distance chosen as the fact of being present in the same document or not. 
  distance_matrix <- t(as.matrix(dtm_in)) %>% dist(method="binary") %>% as.matrix()
  dist <- 1 - distance_matrix
  # We create and igraph put in igraph_n with two vertices connected with an edge giving the distance
  # between the two created during the previous step.
  igraph_n <- graph.adjacency(dist, mode = "lower", weighted = T, diag = F) %>% 
    set.vertex.attribute("name", 1:(length(dist[1, ])), as.character(colnames(dist)))
  # We stock the information in a dataframe.
  data_edges <- data.frame(edge1 = get.edgelist(igraph_n, name = T)[, 1],
                   edge2 = get.edgelist(igraph_n, name = T)[, 2], 
                   weight = get.edge.attribute(igraph_n, "weight"), stringsAsFactors = FALSE)
  edges <- length(data_edges[, 1])
  # We keep the edges only if their are present at least once in the same document together.
  data_edges_filtre <- subset(data_edges, data_edges[, 3] >= 0) 
  # We create in igraph_n2 the new igraph based on the same principle as igraph_n 
  # but only with the connected word.
  igraph_n2 <- graph.edgelist(matrix(as.matrix(data_edges_filtre)[,1:2], ncol = 2), directed = F)
  # Ponderation of the links: the more two words are associated in the documents, 
  # the more important the weight of the link is.
  igraph_n2 <- set.edge.attribute(igraph_n2, "weight", 1:(length(get.edgelist(igraph_n2)[, 1])), 
                                  data_edges_filtre[, 3])
  # Vertex identification: the name of the vertices of the graph corresponds to the word of the DTM.
  V(igraph_n2)$id <- as.character(colnames(dist))
  return(igraph_n2)
} 
