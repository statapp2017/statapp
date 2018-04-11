library(plotly)
library(ggplot2)

#' Characterises the i-th topic.
#' 
#' @param phi_t The distribution of words by documents (matrix)
#' @param a_eliminer The words associated with a strong sentiment by xgboost

caracterise <- function(phi_t, a_eliminer) {
  df <- as.data.frame(t(phi_t))
  df$nom <- rownames(df)
  col <- list()
  for (i in (1:(ncol(df) - 1))) {
    titre <- paste("Theme", as.character(i), sep = "")
    col[i] <- titre
  }
  col[length(col) + 1] <- "nom"
  colnames(df) <- col
  df <- df[!df$nom %in% a_eliminer, ]
  a_garder <- c()
  for (i in (1:(ncol(df) - 1))) {
    df <- df[order(df[,i], decreasing = T), ] #sort words assigned to each topic by decreasing probability of belonging to this topic
    a_garder <- c(a_garder, c(rownames(df[1:10, ]))) #keep the best 10 words
    ref <- df[,i]
    for (j in 1:10) {
    df$theme[j] <- paste("Theme", as.character(i))
    df$valeur[j] <- ref[j]
    }
  }
  df <- df[a_garder, ]
  df <- df[order(df[, which(colnames(df) == "theme")], decreasing = F), ]
  df$nom <- factor(df$nom, levels = df$nom)
  p <- ggplot(data = df, aes(x = nom, y = valeur, color = theme)) +
    geom_bar(stat = "identity") + labs(title = "Mots illustrant les différents thèmes",
                                  x = "Mots", y = "Probabilité") + theme(axis.text.x = element_text(angle = 45))
  print(ggplotly(p))
}
