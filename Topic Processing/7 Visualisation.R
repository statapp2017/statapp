library(LDAvis)

#' Visualises dynamically the results of the topic processing with LDA thanks to LDAvis.
#' 
#' @param model The LDA model, output of \code{\link{give_theme}}.
#' @param dtm The document-term matrix obtained after the preprocessing.
#' @return An interactive visualisation of the topic processing.
#' @examples
#' visualise_LDA(model_theme, dtm_ep)

visualise_LDA <- function(model, dtm) {
  #model <- give_theme(dtm)
  phi <- model$phi_t # Probability that a word belongs to a topic 
  theta <- model$theta # Probability that a document belongs to a topic 
  doc.length <- as.data.frame(table(dtm$i))[2]$Freq # Number of words per document 
  vocab <- dtm$dimnames$Terms # Words list
  term.frequency <- as.data.frame(table(dtm$j))[2]$Freq # Frequency of words in the whole document
  json <- createJSON(phi = phi, theta = theta, doc.length = doc.length, vocab = vocab, 
                     term.frequency = term.frequency)
  serVis(json, out.dir = "vis", open.browser = TRUE)
}
