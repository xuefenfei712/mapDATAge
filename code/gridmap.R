#######plot grid in the map

gridmap=function(data,gra,type=NULL,drop=drop){
  gra=as.numeric(gra)
  data$Latitude=as.numeric(data$Latitude)
  data$Longitude=as.numeric(data$Longitude)
if(min(data$Latitude)>=0){
  latgr=seq(0,max(data$Latitude),gra)
}else{
  latgr=c(seq(floor(min(data$Latitude)),0,gra),seq(0,max(data$Latitude),gra))
}
if(min(data$Longitude)>=0){
  loggr=seq(0,max(data$Longitude),gra)
}else{
  loggr=c(seq(floor(min(data$Longitude)),0,gra),seq(0,ceiling(max(data$Longitude)),gra))
}
require(utils)
latlog=expand.grid(lat=latgr,log=loggr)
if(type=="snp" & length(getsnpname(data))>0 & length(grep("Anc",names(data)))==0){
latlogtab=cbind(latlog$lat+(gra/2),latlog$log+(gra/2),0,0,0,0,NA,NA,NA,NA)
colnames(latlogtab)=c("Latitude","Longitude","Count",names(data)[grep("SNP",names(data))],"NON","Sample","Age","Sex","Species")
for(i in 1:nrow(latlogtab)){
  ranks<-data[data$Longitude>=latlog[i,2] & data$Longitude<(latlog[i,2]+gra) & 
                data$Latitude>=latlog[i,1] & data$Latitude<(latlog[i,1]+gra),]
  if(!is.na(ranks[1,1])){
    latlogtab[i,"Count"]=as.numeric(sum(ranks$Count,na.rm=T))
    latlogtab[i,names(data)[grep("SNP",names(data))][1]]=as.numeric(round(mean(ranks[,names(data)[grep("SNP",names(data))][1]],na.rm=T),2))
    latlogtab[i,names(data)[grep("SNP",names(data))][2]]=as.numeric(round(mean(ranks[,names(data)[grep("SNP",names(data))][2]],na.rm=T),2))
    if(any(names(ranks)%in%"NON")){
      latlogtab[i,"NON"]=as.numeric(round(mean(ranks[,"NON"],na.rm=T),2))
    }
    latlogtab[i,"Sample"] =stringr::str_flatten(ranks$Sample, collapse = ", ")
    latlogtab[i,"Age"] =as.numeric(round(mean(ranks[,"Age"],na.rm=T),2))#stringr::str_flatten(ranks$Age, collapse = ", ")
    latlogtab[i,"Sex"] =stringr::str_flatten(ranks$Sex, collapse = ", ")
    latlogtab[i,"Species"] =stringr::str_flatten(ranks$Species, collapse = ", ")
  }        
}

latlogtab=data.frame(latlogtab)
latlogtab$Site=paste("Site",seq(1,nrow(latlogtab)),sep="")
latlogtab$SecondSite=paste("Site",seq(1,nrow(latlogtab)),sep="")
latlogtab=latlogtab[latlogtab$Count>0,]
latlogtab[,names(data)[grep("SNP",names(data))][1]]=as.numeric(latlogtab[,names(data)[grep("SNP",names(data))][1]])
latlogtab[,names(data)[grep("SNP",names(data))][2]]=as.numeric(latlogtab[,names(data)[grep("SNP",names(data))][2]])
latlogtab$Latitude=as.numeric(latlogtab$Latitude)
latlogtab$Longitude=as.numeric(latlogtab$Longitude)
latlogtab$Age=as.numeric(latlogtab$Age)
latlogtab$Count=as.numeric(latlogtab$Count)
as.data.frame(latlogtab)
 }else if(!is.null(type) & length(grep(type,names(data)))>0){
   ancname=names(data)[grep(type,names(data))]
   a=matrix(0,ncol=length(ancname)+1,nrow=nrow(latlog))
   b=matrix(NA,ncol=4,nrow=nrow(latlog))
   colnames(a)=c("Count",ancname)
   latlogtab=cbind(latlog$lat+(gra/2),latlog$log+(gra/2),a,b)
   colnames(latlogtab)=c("Latitude","Longitude","Count",ancname,"Sample","Age","Sex","Species")
   for(i in 1:nrow(latlogtab)){
     ranks<-data[data$Longitude>=latlog[i,2] & data$Longitude<(latlog[i,2]+gra) & 
                   data$Latitude>=latlog[i,1] & data$Latitude<(latlog[i,1]+gra),]
     if(nrow(ranks)>0){
       latlogtab[i,"Count"]=sum(ranks$Count,na.rm=T)
       latlogtab[i,"Sample"] =stringr::str_flatten(ranks$Sample, collapse = ", ")
       latlogtab[i,"Age"] =mean(ranks$Age, na.rm=T)
       latlogtab[i,"Sex"] =stringr::str_flatten(ranks$Sex, collapse = ", ")
       latlogtab[i,"Species"] =stringr::str_flatten(ranks$Species, collapse = ", ")
       for(j in 1:length(ancname)){
         latlogtab[i,ancname[j]]=as.numeric(round(mean(ranks[,ancname[j]],na.rm=T),2))
       }
     }        
   }
   latlogtab=as.data.frame(latlogtab,stringsAsFactors=FALSE)
    latlogtab$Site=paste("Site",seq(1,nrow(latlogtab)),sep="")
   latlogtab$SecondSite=paste("Site",seq(1,nrow(latlogtab)),sep="")
   latlogtab$Latitude=as.numeric(latlogtab$Latitude)
   latlogtab$Longitude=as.numeric(latlogtab$Longitude)
   latlogtab$Count=as.numeric(latlogtab$Count)
   latlogtab$Age=as.numeric(latlogtab$Age)
   latlogtab=latlogtab[latlogtab$Count>0,]
   as.data.frame(latlogtab,stringsAsFactors=FALSE)
 }
}
