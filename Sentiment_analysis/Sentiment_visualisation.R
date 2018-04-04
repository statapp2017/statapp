library(lime)
source("Lime modifiée/explain.R")
source("Lime modifiée/dataframe.R")

#' Gives a visual explanation on the verbatims classification. The algorithm
#' is based on lime slightly modified to take into account the bagging
#' @param new_data The Document Term Matrix of the verbatim to work on.
#' It is necessary to transform it with gestion_dtm before.
#' @param list_models The list of xgb.train models used for the prediction.

sentiments_results_visualisation <- function(new_verbatim, list_models) {
  donnees <- data.frame(text=new_verbatim)
  sentiment <- model_sentiment$models
  params <- sentiment[[1]]$params
  number_class <- params$num_class
  result_preprocess <- preprocess_text(donnees, "text")
  new_data <- gestion_dtm(result_preprocess$dtm, result_preprocess$tdm, nom_colonnes)
  train_test <- list_models$train_test
  explanation <- vector("list", length(list_models$models))
  explainer_xgb <- lime(train_test[, 2:ncol(train_test)],list_models$models, bin_continuous = FALSE)
  new_data <- new_data[, colnames(new_data) != "mots_supplementaire"]
  ex_xgb <- explain(new_data, explainer_xgb, n_labels = 3, n_features = 10)
  print(plot_features(ex_xgb, ncol = 3))
}
