library(ggplot2)
library("forcats")
AllePlot=function(dat,sex,species,gt,windowsize,stepsize,st,end,snp,lat1,lat2,long1,long2,ymin,ymax){
  d=dat[,c("Sample","Age","latitude","longitude","Sex","Site","Species",names(dat)[grep(snp,names(dat))])]
  d$count=apply(d[grep(snp,colnames(d))],1,sum)
  d=d[d$count>0,]
  data=d[d$Sex%in%sex & d$Species%in%species & d$Age >=st & d$Age <end & d$latitude >= as.numeric(lat1) & d$latitude <as.numeric(lat2) & d$longitude >= as.numeric(long1) & d$longitude < as.numeric(long2),]

time1=seq(floor(st/100)*100,ceiling(end/100)*100-windowsize,stepsize)
time2=seq(floor(st/100)*100+windowsize,ceiling(end/100)*100,stepsize)
label=paste(time1,time2,sep="-")
#label[length(label)]=paste(">",time1[length(label)],sep="")
sort=factor(paste("Time",seq(01,length(label)),sep=""))
timtab=NA
timtab=as.data.frame(cbind(time1,time2,label,sort))
timtab$mean=NA;timtab$sd=NA;timtab$count=NA
a=NA;b=NA
gtt=paste("SNP_",snp,"_",gt,sep="")
for(i in 1:nrow(timtab)){
  a=data[data$Age>=as.numeric(timtab[i,1]) & data$Age<as.numeric(timtab[i,2]),gtt]
  if(length(a)==0){
    timtab[i,5]=0
    timtab[i,6]=0
    timtab[i,7]=0
  }else{
     timtab[i,5]=mean(a, na.rm = TRUE)
      timtab[i,6]=sqrt(timtab[i,5]*(1-timtab[i,5])/length(a))
      timtab[i,7]=length(a)
  }
 
}
#timtab[is.na(timtab)] <- 0
timtab$label=fct_inorder(timtab$label)
 plot_ly(timtab,x=~label,y=~mean,type="scatter",name="frequency",mode = 'lines+markers',error_y=~list(array=sd,color="#000000"),
        hoverinfo = 'text',text=~paste('</br> timebin: ',label,'</br> mean: ',round(mean,2),
                                       '</br> sd: ',round(sd,2),'</br> n: ',count)) %>%#y=ymax+0.1,x=0.1,
   add_trace(y=~count,type="scatter",mode="markers",yaxis="y2", name="Counts") %>%
  layout(title=list(text="Allele trajectory",size=22,xref="paper",yref="paper",x=0,y=ymax+0.1,xanchor = "left", yanchor =  "top",color="black",family="Arial"),
         xaxis=list(title="",tickangle = 290),yaxis=list(title=paste("F(",snp,":",gt,")",sep=""),
                                                         range=c(ymin,ymax),side="left"),
         yaxis2=list(side="right",overlaying="y",showlegend=FALSE,title="Counts"),
         legend = list(orientation = "v", xanchor = "right",x=1, y = ymax+0.02),
         margin=list(l=30,r=50,b=10,t=80,pad=4))

}

AllePlotCounts=function(dat,sex,species,gt,windowsize,stepsize,st,end,snp,lat1,lat2,long1,long2,ymin,ymax,sampletime){
    sampletime=as.numeric(sampletime)
  d=dat[,c("Sample","Age","latitude","longitude","Sex","Site","Species",names(dat)[grep(snp,names(dat))])]
   data=d[d$Sex%in%sex & d$Species%in%species & d$Age >=st & d$Age <end & d$latitude >= as.numeric(lat1) & d$latitude <as.numeric(lat2) & d$longitude >= as.numeric(long1) & d$longitude < as.numeric(long2),]
   data$Count=if_else(rowSums(data[,names(data)[grep(snp,names(data))]])>0,1,0)
   data=data[data$Count>0,]
   data$CountReads=apply(data %>% select(matches(snp)),1,sum)
  
  time1=seq(floor(st/100)*100,ceiling(end/100)*100-windowsize,stepsize)
  time2=seq(floor(st/100)*100+windowsize,ceiling(end/100)*100,stepsize)
  label=paste(time1,time2,sep="-")
  #label[length(label)]=paste(">",time1[length(label)],sep="")
  sort=factor(paste("Time",seq(01,length(label)),sep=""))
  timtab=NA
  timtab=as.data.frame(cbind(time1,time2,label,sort))
  timtab$mean=NA;timtab$sd=NA;timtab$count=NA
  a=NA;b=NA
  gtt=paste("SNP_",snp,"_",gt,sep="")
  
  for(i in 1:nrow(timtab)){
    a=data[as.numeric(data$Age)>=as.numeric(timtab[i,1]) & as.numeric(data$Age)<as.numeric(timtab[i,2]),c(gtt,"CountReads")]
    if(is.null(a[1,1])){
      timtab[i,5]=0
      timtab[i,6]=0
      timtab[i,7]=0
    }else if(!is.null(a[1,1])){
      outmatrix=as.data.frame(matrix(data="",ncol=(sampletime),nrow=as.numeric(dim(a)[1])))
      for(s in 1:sampletime){
        for(j in 1:nrow(a)){
          if(isTRUE(a[j,2]>0)){
            outmatrix[j,s]=sample(c(rep(1,a[j,1]),rep(0,(a[j,2]-a[j,1]))),1)
          }else if(isTRUE(a[j,2]>0)){
            outmatrix[j,s]=1
			}else{
            outmatrix[j,s]=0
          }
        }
      }
      outmatrix[is.na(outmatrix)]=0
      timtab[i,5]=mean(apply(apply(outmatrix,1,as.numeric),2,mean))
      timtab[i,6]=mean(apply(apply(outmatrix,1,as.numeric),2,sd))
      timtab[i,7]=nrow(a)
    }
    
  }
  #timtab[is.na(timtab)] <- 0
  timtab$label=fct_inorder(timtab$label)
  plot_ly(timtab,x=~label,y=~mean,type="scatter",name="frequency",mode = 'lines+markers',yaxis="y",error_y=~list(array=sd,color="#000000"),
          hoverinfo = 'text',text=~paste('</br> timebin: ',label,'</br> mean: ',round(mean,2),
                                         '</br> sd: ',round(sd,2),'</br> n: ',count)) %>%#y=ymax+0.1,x=0.1,
    add_trace(y=~count,type="scatter",mode="markers",yaxis="y2", name="Counts") %>%
    layout(title=list(text="Allele trajectory",size=22,xref="paper",yref="paper",x=0,y=ymax+0.1,xanchor = "left", yanchor =  "top",color="black",family="Arial"),
           xaxis=list(title="",tickangle = 290),yaxis=list(title=paste("F(",snp,":",gt,")",sep=""),
                                                           range=c(ymin,ymax),side="left"),
           yaxis2=list(side="right",overlaying="y",showlegend=FALSE,title="Counts"),
           margin=list(l=25,r=35,b=10,t=80,pad=4),
           legend = list(orientation = "v", xanchor = "right",x=1, y = ymax+0.02))
    
  
}
