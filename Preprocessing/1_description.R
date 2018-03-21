rm(list=ls())

library(ggplot2)
library(xgboost)
#' Give the distribution  of the marks given by the clients and the numbers of clients studied
#' 
#' @param data A dataframe containing the evaluation made by the clients
#' @return Plot the histogram of the marks and give the numbers of clients studied
#' @examples
#' spell_checker(dataframe_clients)

describe_corpus<-function(data){
  par(bg="beige",mfrow=c(1,1))
  #hist(data$recommandation_SGK,col="cyan",main="Notes données par les clients",ylab="Notes",xlab="Effectif")
  chaine<-paste(paste("Il y a",as.character(nrow(data))),"documents")
  print(chaine)
  q1 <- qplot(factor(recommandation_SGK), data=data, geom="bar")
  q1 + geom_bar(fill="steelblue") + 
    stat_count(aes(label=..count..), vjust=1.5, geom="text", position="identity",color="white") +
    stat_count(aes(label = paste(sprintf("%.02f", ..count../sum(..count..)*100), "%")), 
               geom="text",vjust=3.5,color="white")+
    labs(title="Histogramme des notes données par les clients",x="Notes attribuées",y="Effectifs")+
    theme(plot.title = element_text(colour="blue",size=20,hjust=0.5))  
}



