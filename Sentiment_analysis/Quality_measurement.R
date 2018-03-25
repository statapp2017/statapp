library(ggplot2)
library(magrittr)
library(pROC)

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