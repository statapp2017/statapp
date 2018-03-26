library(plotly)
library(ggplot2)

## ANALYSE THEMATIQUE ##
# Carateriser le i-eme theme
caracterise <- function(phi_t, topics_freqs, donnees) {
  freq <- as.matrix(topics_freqs)
  freq2 <- as.data.frame(freq)
  freq2$theme <- paste("Thème", rownames(freq2))
  bp <- ggplot(freq2, aes(x = theme, y = V1)) + geom_bar(stat = "identity") + 
    labs(title = "Nombre de documents par thème", x = "Thème", y = "Nombre de document")
  p2 <- ggplotly(bp)
  print(p2)
  df <- as.data.frame(t(phi_t))
  df$nom<-rownames(df)
  col <- list()
  for (i in (1:(ncol(df)-1))) {
    titre <- paste("Thème", as.character(i), sep = "")
    col[i]<-titre
  }
  col[length(col)+1] <- "nom"
  colnames(df) <- col
  a_garder <- c()
  tags <- get_all_tags(donnees)
  a_eliminer <- tags[tags$tags %in% c("ADJ", "KON", "PRP", "PUN", "DET", "PRO", "ADV","NUM"), 1]
  df <- df[!df$nom %in% a_eliminer, ]
  for (i in (1:(ncol(df)-1))) {
    df <- df[order(df[, i], decreasing = T), ]
    df <- df[!rownames(df)%in%hash_valence$x, ]
    a_garder <- c(a_garder, c(rownames(df[1:10, ])))
    ref <- df[, i]
    for (j in 1:10) {
      df$theme[j] <- paste("Thème", as.character(i))
      df$valeur[j] <- ref[j]
    }
  }
  df <- df[a_garder, ]
  df <- df[order(df[, which(colnames(df) == "theme")], decreasing = F), ]
  df$nom <- factor(df$nom, levels = df$nom)
  p <- ggplot(data = df, aes(x = nom, y = valeur, color = theme)) + geom_bar(stat = "identity") + 
    labs(title = "Mots illustrant les différents thèmes", x = "Mots", 
         y = "Probabilité") + theme(axis.text.x = element_text(angle = 45))
  p1 <- ggplotly(p)
  print(p1)
}
