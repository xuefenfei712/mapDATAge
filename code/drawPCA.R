
drawpca=function(data,sex,species,st,end,pca,lat1,lat2,log1,log2,pc1,pc2){
  st=as.numeric(st);end=as.numeric(end)
  lat1=as.numeric(lat1)
  lat2=as.numeric(lat2)
  log1=as.numeric(log1)
  log2=as.numeric(log2)
data1=data[data$Sex%in%sex & data$Species%in%species & data$Age >=st & data$Age <=end & data$latitude >=lat1 & data$latitude <lat2 & data$longitude >=log1 & data$longitude <log2,]
data1$PC1=round(data1[,pc1],4);data1$PC2=round(data1[,pc2],4)

plot_ly(data=data1,x=~PC1,y=~PC2,color = ~Age,type = 'scatter', mode = 'markers',colors=brewer_pal(palette = "RdYlGn")(11),symbol=~Species,
        symbols = c('circle','x','o'),text=~paste("Sample: ",Sample, '<br>Age: ', Age)
) %>%
  layout(title=list(text="PCA",size=20),xaxis=list(title=paste(pc1,"(",round(pca[pca[,1]==pc1,2],2),"%)",sep="")),
         yaxis=list(title=paste(pc2,"(" ,round(pca[pca[,1]==pc2,2],2),"%)",sep="")))
}

