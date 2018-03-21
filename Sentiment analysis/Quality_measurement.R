library(ggplot2)
library(magrittr)
library(pROC)

measure_quality<-function(prediction,vecteur_note,place){
  pred<-vector("list",nrow(prediction))
  for (i in 1:nrow(prediction)){
    pred[i]<-(which(prediction[i,]==max(prediction[i,]))-1)
  }
  pred<-unlist(pred)  
  t<-multiclass.roc(vecteur_note[-place],pred)
  print(auc(t))
  graph_pred<-data.frame(prediction=pred,vecteur_note=vecteur_note[-place])
  graph<-ggplot(graph_pred, aes(prediction, ..count..)) + geom_bar(aes(fill = vecteur_note), position = "dodge")+ labs(title="Erreur algorithme XGBoost", x= "Catégorie prédite", y ="Nombre")+scale_fill_discrete(name="Catégorie\nRéelle")
  print(graph)
}

plot_confusion_matrix<-function(pred,vecteur_note){
  contingency<-table(pred,vecteur_note)
  print(contingency)
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
        title="Catégorie prédite",
        dtick = 1
      ),
      yaxis = list(
        title="Catégorie réelle",
        dtick=1
      ),
      showlegend=FALSE
    )
  p
}