library(RColorBrewer)
library(Rtsne)

## THEMATIC ANALYSIS ##
#phi = distribution of words by docuements (matrix)
try_tsne<-function(phi){
  train<-as.data.frame(t(phi))
  Labels<-c()
  rown <- rownames(train)
  for (i in 1:nrow(train)){
    Labels[i] <- which(train[rown[i],]==max(train[rown[i],]))
  }
  Labels<-as.factor(Labels)
  colors =brewer.pal(length(unique(colnames(train))),"Paired")
  lab<-unique(Labels)
  names(colors) <- lab
  legends<-c()
  for (i in 1:length(unique(Labels))){
    legends[i]<-paste("Theme",as.character(lab[i]))
  }
  tsne <- Rtsne(train, dims = 2, perplexity=30, verbose=TRUE, max_iter = 500,check_duplicates = FALSE)
  plot(tsne$Y, t='p',pch=21, main="Tsne mots",col=colors[Labels],bg=colors[Labels],xlab="Axe 1",ylab="Axe 2")
  legend("bottomleft",legend=legends,col=colors[unique(Labels)],pt.bg=colors[unique(Labels)],pch = rep(21,length(Labels)))
}
