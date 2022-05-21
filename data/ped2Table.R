#setwd("C:\\MapR\\RevisionmapDATAge\\data")
library(foreach)
pedfile="SNPrs4988235.ped"
ped=read.table(pedfile,header=F,sep="\t",stringsAsFactors = F)
ped=ped[ped[,7]!=0 & ped[,8]!=0,]
infofile="human-EU-info.txt"
info=read.table(infofile,header=T,sep="\t",stringsAsFactors = F)
snp=unique(ped[,7])[2:3]
names=c(paste0("SNP_rs4988253_",snp))[2:3]

oname=c("Site","Age" ,"Latitude","Longitude","Sex", "CAT_mtDNA","CAT_Y")
for(i in 1:nrow(ped)){
  ped[i,2]=stringr::str_count(paste(ped[i,7:8],collapse=""),snp[1])#length(strsplit(paste(ped[i,7:8],collapse=""),snp[1])[[1]])
  ped[i,3]=stringr::str_count(paste(ped[i,7:8],collapse=""),snp[2])#length(strsplit(paste(ped[i,7:8],collapse=""),snp[2])[[1]])
  foreach(j=1:length(oname)) %do% {
    ped[i,oname[j]]=info[info$Sample%in%ped[i,1],oname[j]]
  }
}

result=cbind(ped[,c(1,2,3)],ped[,oname])
colnames(result)=c("Sample",names,oname)
result$Species="human"
write.table(result,"mapDATAge_rs4988235.txt",col.names=T,row.names = F,sep="\t",quote=F)

yong=read.table("mapDATAge-rs4988235_MCM6.txt",header=T,sep="\t",stringsAsFactors = F)
