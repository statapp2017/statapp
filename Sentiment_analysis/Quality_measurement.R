library(ggplot2)
library(magrittr)
library(pROC)

#' Gives the multiclass.roc associated with our model and plot the error 
#' of the model
#' @param preidction The dataframe of probability associating each document 
#' to a class of sentiment
#' @param vecteur_note The real class of the documents
#' @param nom_colonnes Les features à garder dans la nouvelle dtm

measure_quality<-function(prediction,vecteur_note){
  pred<-vector("list",nrow(prediction))
  for (i in 1:nrow(prediction)){
    pred[i]<-(which(prediction[i,]==max(prediction[i,]))-1)
  }
  pred<-unlist(pred)  
  multi_auc<-multiclass.roc(vecteur_note,pred)
  print(multi_auc)
  graph_pred<-data.frame(prediction=pred,vecteur_note=vecteur_note)
  graph<-ggplot(graph_pred, aes(prediction, ..count..)) + geom_bar(aes(fill = vecteur_note), position = "dodge")+ labs(title="Erreur algorithme XGBoost", x= "Categorie predite", y ="Nombre")+scale_fill_discrete(name="Categorie\nReelle")
  print(graph)
}


#' Plot the confusion_matrix of the models
#' @param preidction The vecotr of predicted class
#' @param vecteur_note The real class of the documents

plot_confusion_matrix<-function(pred,vecteur_note){
  contingency<-table(pred,vecteur_note)
  donnes<-as.data.frame(expand.grid(sort(unique(pred)),sort(unique(vecteur_note))))
  donnes$contingency<-as.vector(contingency)
  p <- plot_ly(donnes,
               x = ~Var1, y =~Var2,
               z = ~contingency, type = "heatmap"
  )%>%add_annotations( text = ~contingency, showarrow = FALSE, font=list(color='white',size=30))%>%
    layout(
      title = "Matrice de confusion",
      titlefont = list(
        size = 20
      ),
      xaxis = list(
        title="Categorie predite",
        dtick = 1
      ),
      yaxis = list(
        title="Categorie reelle",
        dtick=1
      ),
      showlegend=FALSE
    )
  p
}
