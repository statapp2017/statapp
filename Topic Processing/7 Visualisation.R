library(LDAvis)

phi_v <- model_theme$phi_t # Probability that a word belongs to a topic (ok)
theta_v <- model_theme$theta # Probability that a document belongs to a topic (ok)
doc.length_v <- as.data.frame(table(dtm_ep$i))[2]$Freq # Number of words per document (ok)
vocab_v <- dtm_ep$dimnames$Terms # Words list
vocab_v[92] <- "fa"
vocab_v[93] <- "re"
term.frequency_v <- as.data.frame(table(dtm_ep$j))[2]$Freq # Fréquence des mots dans tout le document"

list_themes <- list(phi = phi_v, theta = theta_v, doc.length = doc.length_v, vocab = vocab_v, 
                    term.frequency = term.frequency_v)

json <- createJSON(phi = list_themes$phi, theta = list_themes$theta, 
                   doc.length = list_themes$doc.length, vocab = list_themes$vocab, 
                   term.frequency = list_themes$term.frequency)

serVis(json, out.dir = 'vis', open.browser = TRUE)
