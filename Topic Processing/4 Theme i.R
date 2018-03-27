library(plotly)
library(ggplot2)

## ANALYSE THEMATIQUE ##
# Characterize the i-th topic
# topics_freq = table of frenquency of topics by documents
# imput_data = the data frame containing the verbatims
caracterise <- function(phi_t, topics_freqs, imput_data) {
  freq <- as.matrix(topics_freqs)
  freq2 <- as.data.frame(freq)
  freq2$theme <- paste("Thème", rownames(freq2))
  bp <- ggplot(freq2, aes(x = theme, y = V1)) + geom_bar(stat = "identity") + 
    labs(title = "Nombre de documents par thème", x = "Thème", y = "Nombre de document")
  p2 <- ggplotly(bp)
  print(p2)
  df <- as.data.frame(t(phi_t))
  df$name <- rownames(df)
  col <- list()
  for (i in (1:(ncol(df) - 1))) {
    col[i] <- paste("Thème", as.character(i), sep = "")
  }
  col[length(col) + 1] <- "nom"
  colnames(df) <- col
  to_keep <- c()
  tags <- get_all_tags(imput_data)
  to_delete <- tags[tags$tags %in% c("ADJ", "KON", "PRP", "PUN", "DET", "PRO", "ADV", "NUM"), 1]
  df <- df[!df$name %in% to_delete, ]
  for (i in (1:(ncol(df)-1))) {
    df <- df[order(df[, i], decreasing = T), ]
    df <- df[!rownames(df)%in%hash_valence$x, ]
    to_keep <- c(to_keep, c(rownames(df[1:10, ])))
    ref <- df[, i]
    for (j in 1:10) {
      df$theme[j] <- paste("Thème", as.character(i))
      df$valeur[j] <- ref[j]
    }
  }
  df <- df[to_keep, ]
  df <- df[order(df[, which(colnames(df) == "theme")], decreasing = F), ]
  df$name <- factor(df$name, levels = df$name)
  p <- ggplot(data = df, aes(x = name, y = valeur, color = theme)) + geom_bar(stat = "identity") + 
    labs(title = "Mots illustrant les différents thèmes", x = "Mots", 
         y = "Probabilité") + theme(axis.text.x = element_text(angle = 45))
  p1 <- ggplotly(p)
  print(p1)
}
