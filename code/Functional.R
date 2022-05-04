colname=c( "Sample","Age","Latitude","Longitude","Site","Species","Sex")
validate_input_files <- function(table) {
  # 1. Column names exist.
  if (is.null(colnames(table))) {
    stop("The input table should have column names.")
  }
  # 2. One of the columns is inside"
  if (!(all(colname %in% colnames(table)))) {
    stop("Please make sure that all required columns are provided in the uploaded table")
  }
  # 3. All columns apart from sum.taxonomy should be numeric
  if (!(all(sapply(table[,c("Age","Latitude","Longitude")],is.numeric)))) {
    stop("Please make sure that all required columns are provided in the uploaded table!")
  }
}
 ##freq plot
 AllePlot=function(dat,sex,species,gt,windowsize,stepsize,st,end,snp,lat1,lat2,long1,long2,ymin,ymax,sampletime,gtp){
  d=dat[,c("Sample","Age","Latitude","Longitude","Sex","Site","Species",names(dat)[grep(snp,names(dat))])]
  d$count=dplyr::if_else(rowSums(d[,names(d)[grep(snp,names(d))]])>0,1,0)
  d=d[d$count>0,]
  if(gtp=="ReadCounts"){
    d$CountReads=apply(d %>% select(matches(snp)),1,sum)
  }
  data=d[d$Sex%in%sex & d$Species%in%species & d$Age >=st & d$Age <end & d$Latitude >= as.numeric(lat1) & d$Latitude <as.numeric(lat2) & d$Longitude >= as.numeric(long1) & d$Longitude < as.numeric(long2),]
  
  time1=seq(floor(st/100)*100,ceiling(end/100)*100-windowsize,stepsize)
  time2=seq(floor(st/100)*100+windowsize,ceiling(end/100)*100,stepsize)
  label=paste(time1,time2,sep="-")
  #label[length(label)]=paste(">",time1[length(label)],sep="")
  sort=factor(paste("Time",seq(01,length(label)),sep=""))
  timtab=NA
  timtab=as.data.frame(cbind(time1,time2,label,sort))
  timtab$mean=NA;timtab$sd=NA;timtab$count=NA
  a=NA;b=NA
  gtt=paste("Snp_",snp,"_",tolower(gt),sep="")
  if(gtp=="Genotype"){
    foreach(i=1:nrow(timtab)) %do% {
    #for(i in 1:nrow(timtab)){
    a=data[data$Age>=as.numeric(timtab[i,1]) & data$Age<as.numeric(timtab[i,2]),gtt]
    timtab[i,5]=mean(a, na.rm = TRUE)/2
    timtab[i,6]=ifelse(length(a)==0,0,sqrt(timtab[i,5]*(1-timtab[i,5])/length(a)))#sqrt(sd(a, na.rm = TRUE)/length(a))#
    timtab[i,7]=length(a)
    }
  }else if(gtp=="ReadCounts"){
    sampletime=as.numeric(sampletime)
    foreach(i=1:nrow(timtab)) %do% {
      #for(i in 1:nrow(timtab)){
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
  }
  #timtab[is.na(timtab)] <- 0
  timtab$label=forcats::fct_inorder(timtab$label)
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
###pca
drawpca=function(data,sex,species,st,end,pca,lat1,lat2,log1,log2,pc1,pc2){
  st=as.numeric(st);end=as.numeric(end)
  lat1=as.numeric(lat1)
  lat2=as.numeric(lat2)
  log1=as.numeric(log1)
  log2=as.numeric(log2)
data1=data[data$Sex%in%sex & data$Species%in%species & data$Age >=st & data$Age <=end & data$Latitude >=lat1 & data$Latitude <lat2 & data$Longitude >=log1 & data$Longitude <log2,]
data1$PC1=round(data1[,pc1],4);data1$PC2=round(data1[,pc2],4)

plot_ly(data=data1,x=~PC1,y=~PC2,color = ~Age,type = 'scatter', mode = 'markers',colors=brewer_pal(palette = "RdYlGn")(11),symbol=~Species,
        symbols = c('circle','x','o'),text=~paste("Sample: ",Sample, '<br>Age: ', Age)
) %>%
  layout(title=list(text="PCA",size=20),xaxis=list(title=paste(pc1,"(",round(pca[pca[,1]==pc1,2],2),"%)",sep="")),
         yaxis=list(title=paste(pc2,"(" ,round(pca[pca[,1]==pc2,2],2),"%)",sep="")))
}
getsnpname=function(data){
  unique(stringi::stri_replace_first_regex(stringi::stri_replace_last_regex(names(data)[grep("Snp_",names(data))],pattern="_[A,C,G,T,D,a,c,g,t,d]",replacement = ""),"Snp_",""))
}
getallename=function(data,snp){
  toupper(substring(names(data)[grep(snp,names(data))],nchar(names(data)[grep(snp,names(data))]),nchar(names(data)[grep(snp,names(data))])))
}
addreadCounts=function(data,snp){
  ReadCount=apply(data %>% select(matches(snp)),1,sum)
  as.data.frame(cbind(data,ReadCount))
}

meansampling=function(xx,tt,sampletime=100){
  tab=cbind(xx,as.numeric(tt))
  freq=""
  if(nrow(tab)==0){
    freq=0
  }else if(nrow(tab)>0){
    outmatrix=matrix("",ncol=sampletime,nrow=nrow(tab))
    for(s in 1:sampletime){
      for(j in 1:nrow(tab)){
        if((tab[j,2]-tab[j,1])>0 & tab[j,2]>1){
          outmatrix[j,s]=sample(c(rep(1,tab[j,1]),rep(0,(tab[j,2]-tab[j,1]))),1)
        }else if(tab[j,2]==tab[j,1] & tab[j,2]>0){
          outmatrix[j,s]=1
        }else if((tab[j,2]-tab[j,1])>0 & tab[j,2]==1){
          outmatrix[j,s]=0
        }else{
          outmatrix[j,s]=0
        }
      }
    }
  }
  outmatrix[is.na(outmatrix)]=0
  freq=mean(apply(apply(outmatrix,1,as.numeric),2,mean))
  as.numeric(freq)
}

filte=function(data){
  for(i in getsnpname(data)){
    data[,paste(i,"TotalCounts",sep="")]=apply(data[names(data)[grep(getsnpname(data)[1],names(data))]],1,sum)
  }
  as.data.frame(data[apply(data %>% select(ends_with("TotalCounts")),1,function(x) all(x>0))==TRUE,])
  data[!names(data)%in%"TotalCounts"]
  data=data[,-grep("Total",names(data))]
}
catsorting=function(data){
  sub=as.data.frame(table(data))
  sub2=matrix(as.numeric(sum(sub[,2])),ncol=nrow(sub))
  colnames(sub2)=paste("Cat_",sub[,1],sep="")
  data.frame(sub2)
}
Mergeawsome=function(data,snp=NULL,type=NULL,gtp=NULL,mult=FALSE,drop=FALSE){
  charvec=c("Site","Sample","Species","Sex")
  if(length(grep("Snp",names(data)))>0 && !is.null(snp)){
    d=data[,c(charvec,names(data)[grep(snp,names(data))],"Latitude","Longitude","Age")]
    d$Count=dplyr::if_else(rowSums(d[,names(d)[grep(snp,names(d))]])>0,1,0)
    d=d[d$Count>0,]
    d$ReadCount=apply(d %>% select(matches(snp)),1,sum)
  }else if(!is.null(type) && type!="Anc" && length(grep(type,names(data)))>0){
      cat=paste("Cat_",type,sep="")
      d=data[,c(charvec,cat,"Latitude","Longitude","Age")]
      d=d[d[,cat] !="unknown",]
      yname=sort(unique(d[,cat]))
      d$Count=rep(1,nrow(d))
  }else if(!is.null(type) && type=="Anc"){
    d=data[,c(charvec,names(data)[grep("Anc",names(data))],"Latitude","Longitude","Age")]
    d$Count=rep(1,nrow(d))
  }else{
    d=data
    d$Count=rep(1,nrow(d))
  }
  d2 <- sf::st_as_sf(d, coords = c("Longitude", "Latitude"))
  d3 <- sf::st_intersection(d2)
  list <- parallel::mclapply(1:length(d3$origins), function(x) {
    point <- d3[x, ]
    point[,charvec]=foreach(i=1:length(charvec), .combine=cbind) %dopar% {
      point[,charvec[i]]=stringr::str_flatten(data.frame(d2[d3$origins[[x]], charvec[i]])[,charvec[i]], collapse = ", ")
    }
    point$Age <- round(mean(d2[d3$origins[[x]], ]$Age, na.rm = TRUE),2)
    point$Count <- sum(d2[d3$origins[[x]], ]$Count, na.rm = TRUE)
    ###merge snp both genoty and readc
    if(!is.null(snp) && (length(names(d)[grep(snp,names(d))])>1) && gtp=="genotype" && mult==FALSE){
      a=names(d)[grep(snp,names(d))]
      point[,a]=foreach(i=1:length(a), .combine=cbind) %dopar% {
        point[,a[i]]=round(mean(data.frame(d2[d3$origins[[x]], a[i]])[,a[i]],na.rm=TRUE),2)
      }
    }else if(is.null(snp) && length(names(d)[grep("Snp",names(d))])>2 && gtp=="genotype" && mult==TRUE){
      a=names(d)[grep("Snp",names(d))]
      point[,a]=foreach(i=1:length(a), .combine=cbind) %dopar% {
        point[,a[i]]=round(mean(data.frame(d2[d3$origins[[x]], a[i]])[,a[i]],na.rm=TRUE),2)
      }
    }else if(!is.null(snp) && length(names(d)[grep(snp,names(d))])>1 && gtp=="ReadCounts"){
      a=names(d)[grep(snp,names(d))]
      point[,a]=foreach(i=1:length(a), .combine=cbind) %dopar% {
        point[,a[i]]= round(meansampling(matrix(unlist(d2[d3$origins[[x]],a[i]]),ncol=3,byrow=FALSE)[,1],d2[d3$origins[[x]], ]$ReadCount),2)
      }
    }
    ###character type
    if(!is.null(type) && type=="Anc"){
      char=names(d)[grep("Anc",names(d))]
      point[,char]=foreach(i=1:length(char), .combine=cbind) %dopar% {
        point[,char[i]]=round(mean(data.frame(d2[d3$origins[[x]], char[i]])[,char[i]],na.rm=TRUE),2)
      }
    }else if(!is.null(type) && type!="Anc" && length(grep(type,names(d)))>0){
      catyname=paste(type,"_",yname,sep="")
      point[,catyname]=matrix(0,ncol=length(catyname),nrow=1)
      point[,catyname]=catsorting(data.frame(d2[d3$origins[[x]], cat][,cat])[,1])
    }
    return(point)
  })
  new_d <- do.call(rbind, list)
  new_d$Longitude <- sf::st_coordinates(new_d)[, "X"]
  new_d$Latitude <- sf::st_coordinates(new_d)[, "Y"]
  sf::st_geometry(new_d) <- NULL
  as.data.frame(new_d)
  if(!is.null(type) && type!="Anc"){
    new_d[,cat]=NULL
  }
  new_d$SecondSite=paste(as.character(new_d$Site),"_layer",sep="")
  as.data.frame(new_d)
}
Merge=function(data){
  d=data
  d$Count=rep(1,nrow(d))
  charvec=c("Site","Sample","Species","Sex")
  d2 <- sf::st_as_sf(d, coords = c("Longitude", "Latitude"))
  d3 <- sf::st_intersection(d2)
  list <- parallel::mclapply(1:length(d3$origins), function(x) {
    point <- d3[x, ]
    point[,charvec]=foreach(i=1:length(charvec), .combine=cbind) %dopar% {
      point[,charvec[i]]=stringr::str_flatten(data.frame(d2[d3$origins[[x]], charvec[i]])[,charvec[i]], collapse = ", ")
    }
    point$Age <- round(mean(d2[d3$origins[[x]], ]$Age, na.rm = TRUE),2)
    point$Count <- sum(d2[d3$origins[[x]], ]$Count, na.rm = TRUE)
    return(point)
  })
  new_d <- do.call(rbind, list)
  new_d$Longitude <- sf::st_coordinates(new_d)[, "X"]
  new_d$Latitude <- sf::st_coordinates(new_d)[, "Y"]
  sf::st_geometry(new_d) <- NULL
  new_d$SecondSite=paste(as.character(new_d$Site),"_layer",sep="")
  as.data.frame(new_d)
}

MergeSNP=function(data,snp=NULL,gtp=NULL,drop=FALSE){
  chc=c("Site","Sample","Species","Sex")
  d=data[,c(chc,names(data)[grep(snp,names(data))],"Latitude","Longitude","Age")]
  d$Count=dplyr::if_else(rowSums(d[,names(d)[grep(snp,names(d))]])>0,1,0)
  d=d[d$Count>0,]
  d$ReadCount=apply(d %>% select(matches(snp)),1,sum)
  d2 <- sf::st_as_sf(d, coords = c("Longitude", "Latitude"))
  d3 <- sf::st_intersection(d2)
  list <- parallel::mclapply(1:length(d3$origins), function(x) {
    point <- d3[x, ]
    point[,chc]=foreach(i=1:length(chc), .combine=cbind) %dopar% {
      point[,chc[i]]=stringr::str_flatten(data.frame(d2[d3$origins[[x]], chc[i]])[,chc[i]], collapse = ", ")
    }
    point$Age <- round(mean(d2[d3$origins[[x]], ]$Age, na.rm = TRUE),2)
    point$Count <- sum(d2[d3$origins[[x]], ]$Count, na.rm = TRUE)
    a=names(d)[grep(snp,names(d))]
    if(gtp=="genotype"){
      point[,a]=foreach(i=1:length(a), .combine=cbind) %dopar% {
        point[,a[i]]=round(mean(data.frame(d2[d3$origins[[x]], a[i]])[,a[i]],na.rm=TRUE),2)
      }}else if(gtp=="ReadCounts"){
        point[,a]=foreach(i=1:length(a), .combine=cbind) %dopar% {
          point[,a[i]]= round(meansampling(matrix(unlist(d2[d3$origins[[x]],a[i]]),ncol=3,byrow=FALSE)[,1],d2[d3$origins[[x]], ]$ReadCount),2)
        }}
    return(point)
  })
  new_d <- do.call(rbind, list)
  new_d$Longitude <- sf::st_coordinates(new_d)[, "X"]
  new_d$Latitude <- sf::st_coordinates(new_d)[, "Y"]
  sf::st_geometry(new_d) <- NULL
  new_d$SecondSite=paste(as.character(new_d$Site),"_lay",sep="")
  as.data.frame(new_d)
}
Mergetype=function(data,type=NULL,drop=FALSE){
  chc=c("Site","Sample","Species","Sex");num=c("Latitude","Longitude","Age")
  if(type=="Anc"){
    char=names(data)[grep("Anc",names(data))]
    d=data[,c(chc,char,num)]
  }else if(!is.null(type) && type!="Anc"){
    cat=paste("Cat_",type,sep="")
    d=data[,c(chc,cat,num)]
    d=d[d[,cat] !="unknown",]
    yname=sort(unique(d[,cat]))
  }
  d$Count=rep(1,nrow(d))
  d2 <- sf::st_as_sf(d, coords = c("Longitude", "Latitude"))
  d3 <- sf::st_intersection(d2)
  list <- parallel::mclapply(1:length(d3$origins), function(x) {
    point <- d3[x, ]
    point[,chc]=foreach(i=1:length(chc), .combine=cbind) %dopar% {
      point[,chc[i]]=stringr::str_flatten(data.frame(d2[d3$origins[[x]], chc[i]])[,chc[i]], collapse = ", ")
    }
    point$Age <- round(mean(d2[d3$origins[[x]], ]$Age, na.rm = TRUE),2)
    point$Count <- sum(d2[d3$origins[[x]], ]$Count, na.rm = TRUE)
    ###character type
    if(type=="Anc"){
      point[,char]=foreach(i=1:length(char), .combine=cbind) %dopar% {
        point[,char[i]]=round(mean(data.frame(d2[d3$origins[[x]], char[i]])[,char[i]],na.rm=TRUE),2)
      }
    }else if(!is.null(type) && type!="Anc"){
      catyname=paste(type,"_",yname,sep="")
      point[,catyname]=matrix(0,ncol=length(catyname),nrow=1)
      point[,catyname]=catsorting(data.frame(d2[d3$origins[[x]], cat][,cat])[,1])
    }
    return(point)
  })
  new_d <- do.call(rbind, list)
  new_d$Longitude <- sf::st_coordinates(new_d)[, "X"]
  new_d$Latitude <- sf::st_coordinates(new_d)[, "Y"]
  sf::st_geometry(new_d) <- NULL
  as.data.frame(new_d)
  if(!is.null(type) && type!="Anc"){
    new_d[,cat]=NULL
  }
  new_d$SecondSite=paste(as.character(new_d$Site),"_layer",sep="")
  as.data.frame(new_d)
}

MergeawsomeCount=function(data){
    data$Count=if_else(rowSums(data[,names(data)[grep("Snp",names(data))]])>0,1,0)
      data=data[data$Count>0,]
      snpname=getsnpname(data)
    if(length(snpname)>2){
      foreach(i=1:length(snpname), .combine=cbind) %dopar% {
      total=apply(data%>% select(matches(snpname[i])),1,sum)
        inpd=cbind(data[,names(data)[grep(snpname[i],names(data))]],total)
        foreach(i=1:nrow(inpd), .combine=cbind) %dopar% {
          snpn=names(inpd)[1:2]
          data[i,snpn[1]]=meansampling(inpd[i,1],inpd[i,3])
          data[i,snpn[2]]=meansampling(inpd[i,2],inpd[i,3])
        }
    }
    d=data
    d2 <- sf::st_as_sf(d, coords = c("Longitude", "Latitude"))
    d3 <- sf::st_intersection(d2)

        list <- parallel::mclapply(1:length(d3$origins), function(x) { 
        point <- d3[x, ]
        chc=c("Site","Sample","Species","Sex")
        point[,chc]=foreach(i=1:length(chc), .combine=cbind) %dopar% {
          point[,chc[i]]=stringr::str_flatten(data.frame(d2[d3$origins[[x]], chc[i]])[,chc[i]], collapse = ", ")
        }
        point$Age <- round(mean(d2[d3$origins[[x]], ]$Age, na.rm = TRUE),2)
        point$Count <- sum(d2[d3$origins[[x]], ]$Count, na.rm = TRUE)
        a=names(d)[grep("Snp",names(d))]
        point[,a]=foreach(i=1:length(a), .combine=cbind) %dopar% {
          point[,a[i]]=round(mean(data.frame(d2[d3$origins[[x]], a[i]])[,a[i]],na.rm=TRUE),2)
        }
        return(point)
      })
      new_d <- do.call(rbind, list)
      new_d$Longitude <- sf::st_coordinates(new_d)[, "X"]
      new_d$Latitude <- sf::st_coordinates(new_d)[, "Y"]
      sf::st_geometry(new_d) <- NULL
      as.data.frame(new_d)
      new_d$Age=as.numeric(new_d$Age)
      filte(new_d)
      as.data.frame(new_d)
    }
}

automaticFigures=function(par,path){
  spexa=c(unique(unlist(strsplit(as.character(par[2,2]),"[_]"))))
  sexa=c(unlist(strsplit(as.character(par[1,2]),"[_]"))[1],unlist(strsplit(as.character(par[1,2]),"[_]"))[2])
  st=as.numeric(par[3,2])
  end=as.numeric(par[4,2])
  windowsize=as.numeric(par[5,2])
  stepsize=as.numeric(par[6,2])
  gt=par[7,2]
  long1=as.numeric(par[8,2])
  long2=as.numeric(par[9,2])
  lat1=as.numeric(par[10,2])
  lat2=as.numeric(par[11,2])
  type=as.character(par[12,2])###ReadsCounts or Genotype
  #dir.create(paste(path,"/",sep=""))
  setwd(paste(path,sep=""))
  files=list.files()[grep("mapDATAge",list.files())]
  if(length(files)==0){
  print("no file")
  }
  foreach(fi = 1:length(files)) %do% {
    if (grepl(files[fi], pattern = ".txt") |
        grepl(files[fi], pattern = ".tsv")){
      data=read.table(files[fi],header=T,sep="\t",stringsAsFactors = F,
                      quote = "", comment.char = "")
    }
    colnames(data)=stringr::str_to_title(names(data))
    base=c("A","C","G","T","D","a","c","g","t","d")
    if(par[7,2]%in%base){
      gt1=tolower(par[7,2])
    }else{
      gt1=tolower(getallename(data,getsnpname(data)))
    }
    foreach(i = 1:2) %do% {
      p=AllePlot(data,sexa,spexa,gt1[i],windowsize,stepsize,st,end,getsnpname(data),lat1,lat2,long1,long2,0,1,100,gtp=type)
     htmlwidgets::saveWidget(p,file=paste(path,"/",getsnpname(data),"-",st,"-",end,"-",gt1[i],"-allefreq.html",sep=""))
    }
    #########drawmap################
  datamap=Mergeawsome(data,snp=getsnpname(data),gtp=type,mult=FALSE)
    colorpal <- colorNumeric("Set1", datamap$Age)
    sizes <- sizeNumeric((datamap$Count), baseSize = mean(datamap$Count))
   # tilesURL <- "http://server.arcgisonline.com/ArcGIS/rest/services/Canvas/World_Light_Gray_Base/MapServer/tile/{z}/{y}/{x}"
    basemap <- leaflet(options = leafletOptions(zoomControl = FALSE, minZoom = 2, maxZoom = 5, dragging = T)) %>%
      addTiles() %>%
      fitBounds(min(datamap$Longitude),min(datamap$Latitude),max(datamap$Longitude),max(datamap$Latitude))
    map=basemap %>%
      addCircleMarkers(datamap$Longitude, datamap$Latitude,color = "#777777",radius=sqrt(sizes)*3#datamap$Count*1.5
                       ,weight=1,
                       fillColor = colorpal(datamap$Age), fillOpacity = 0.9,popup= paste0(
                         "<div>","<h6>","<br>",datamap$Sample,"<h6>",
                         "<h7>","Sex: ",datamap$Sex,"<br>",
                         "Species: ",datamap$Species,"<br>",
                         "Age: ",datamap$Age,"<br>",
                         "Count: ",datamap$Count,"</div>")
      ) %>%
      addLegendSize(position = "bottomleft",col="black",fillColor="white",baseSize =min(sizeNumeric(datamap$Count, baseSize = mean(datamap$Count)*4)),
                    values = sizeNumeric((datamap$Count), baseSize = mean(datamap$Count)),shape="circle",orientation="horizontal",breaks=5) %>%
      addLegendNumeric(position = "bottomright",
                       pal = colorpal, values = datamap$Age)
    
    htmlwidgets::saveWidget(map,paste(path,"/",getsnpname(data),"-",st,"-",end,"-map.html",sep=""))
    #####draw pie map
    sizes <- sizeNumeric((datamap$Count), baseSize = mean(datamap$Count))
    #########drawmap################
     basemap <- leaflet(options = leafletOptions(zoomControl = FALSE, minZoom = 2, maxZoom = 5, dragging = T)) %>%
      addTiles() %>%
      fitBounds(min(datamap$Longitude)+1,min(datamap$Latitude)+1,max(datamap$Longitude)-1,max(datamap$Latitude)-1) %>%
      addResetMapButton() %>% addMiniMap(width=100,height=100,toggleDisplay = TRUE,position = "topright")
    piemap=basemap %>%
      addMinicharts(
        datamap$Longitude, datamap$Latitude,
        chartdata =  cbind(datamap %>% select(starts_with("Snp_"))),
        maxValues = 1,
        type ="pie",
        colorPalette = brewer.pal(11, "RdYlGn")[c(1,9,3,5,6)],
        popup = popupArgs(
          labels = c(substring(names(datamap)[grep("Snp",names(datamap))],nchar(names(datamap)[grep("Snp",names(datamap))]),nchar(names(datamap)[grep("Snp",names(datamap))]))),
          html = paste0(
            "<div>","<h6>","<br>",datamap$Sample,"<h6>",
            "<h7>","Sex: ",datamap$Sex,"<br>",
            "Species: ",datamap$Species,"<br>",
            "Age: ",datamap$Age,"<br>",
            "Count: ",datamap$Count,"</div>"
          )
        ),
        width = 6*sqrt(datamap$Count),transitionTime = 0
      ) %>%
      addLegendSize(position = "bottomleft",col="black",fillColor="white",baseSize = quantile(sqrt(datamap$Count),0.98),
                    values = sizes,shape="circle",orientation="horizontal",breaks=4)
    htmlwidgets::saveWidget(piemap,paste(path,"/",name,"-",st,"-",end,"-piemap.html",sep=""))
  }
}
gridplot=function(data,st,end,grindsize,type,path){
  st=floor(st/10)*10
  end=ceiling(end/10)*10
  tim1=seq(st,end-grindsize,grindsize)
  tim2=seq(st+grindsize,end,grindsize)
  timtab=cbind(tim1,tim2)
  data=data[data$Count>0,]
  #####draw pie map
  maxValue <- 1
  #########drawmap################
  # tilesURL <- "http://server.arcgisonline.com/ArcGIS/rest/services/Canvas/World_Light_Gray_Base/MapServer/tile/{z}/{y}/{x}"
    basemap <- leaflet(options = leafletOptions(zoomControl = FALSE, minZoom = 2, maxZoom = 5, dragging = T)) %>%
    addTiles() %>%
    fitBounds(min(data$Longitude),min(data$Latitude),max(data$Longitude),max(data$Latitude))
  foreach(i=1:nrow(timtab)) %do% {
    if(type=="Snp"){
      coltyp=c(brewer.pal(11, "RdYlGn")[c(1,9,3,5,6)])
      snpname=getsnpname(data)
    }else if(type=="Anc"){
      coltyp=c(brewer.pal(11, "Spectral")[c(1,5,11,3,9,2,8,4)])
      snpname="Anc"
    }
    datamap=data[data$Age>=as.numeric(timtab[i,1]) & data$Age<as.numeric(timtab[i,2]),]
    if(nrow(datamap)>0){
      piemap=basemap %>%
        addMinicharts(
          datamap$Longitude, datamap$Latitude,
          chartdata = cbind(datamap %>% select(starts_with(type))),
          maxValues = maxValue,
          type ="pie",
          colorPalette = coltyp,
          popup = popupArgs(
            labels = names(datamap)[grep(type,names(datamap))] ,
            html = paste0(
              "<div>","<h6>","<br>",datamap$Sample,"<h6>",
              "<div>","<h7>","Sex: ",datamap$Sex,"<br>",
              "Species: ",datamap$Species,"<br>",
              "Age: ",datamap$Age,"<br>",
              "Count: ",datamap$Count,"</div>"
            )
          ),
          width = 10*sqrt(datamap$Count),transitionTime = 0
        ) %>%
        addLegendSize(position = "bottomleft",col="black",fillColor="white",
                      values = datamap$Count,shape="circle",orientation="horizontal",breaks=4) %>%
        addLabelOnlyMarkers(min(data$Longitude)+3,max(data$Latitude)+5,label=paste(timtab[i,2]," - ",timtab[i,1],"BC",sep=""),
                            labelOptions = labelOptions(noHide = T, direction = 'top', textOnly = T,style=list("font-size"="20px","color" = "darkblue","font-style"="bold","box-shadow"="3px 3px rgba(0,0,0,0.25)", "border-color" = "rgba(0,0,0,0.5)")))
      
      htmlwidgets::saveWidget(piemap,paste(path,"/",timtab[i,2],"-",timtab[i,1],"BP-",snpname,".html",sep=""))
    }else{
      htmlwidgets::saveWidget(basemap,paste(path,"/",timtab[i,2],"-",timtab[i,1],"BP-",snpname,".html",sep=""))
    }
  }
}
#######plot grid in the map
gridmap=function(data,gra,type=NULL,drop=drop){
  gra=as.numeric(gra)
  data$Latitude=as.numeric(data$Latitude)
  data$Longitude=as.numeric(data$Longitude)
  data=data[,!names(data)%in%paste("Cat_",type,sep="")]
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
  latlog=expand.grid(lat=latgr,log=loggr)
  snpancname=names(data)[grep(type,names(data))]
  a=matrix(0,ncol=length(snpancname)+1,nrow=nrow(latlog))
  b=matrix(NA,ncol=4,nrow=nrow(latlog))
  colnames(a)=c("Count",snpancname)
  latlogtab=cbind(latlog$lat+(gra/2),latlog$log+(gra/2),a,b)
  colnames(latlogtab)=c("Latitude","Longitude","Count",snpancname,"Sample","Age","Sex","Species")
    for(i in 1:nrow(latlogtab)){
      ranks<-data[data$Longitude>=latlog[i,2] & data$Longitude<(latlog[i,2]+gra) & 
                    data$Latitude>=latlog[i,1] & data$Latitude<(latlog[i,1]+gra),]
      if(type=="Snp" && length(getsnpname(data))>0 && !is.na(ranks[1,1])){
        latlogtab[i,"Count"]=as.numeric(sum(ranks$Count,na.rm=T))
        charvec=c("Sample","Species","Sex")
        latlogtab[i,charvec]=foreach(x=1:length(charvec), .combine=cbind) %dopar% {
          latlogtab[i,charvec[x]]=stringr::str_flatten(ranks[,charvec[x]], collapse = ", ")
        }
        latlogtab[i,"Age"] =as.numeric(round(mean(ranks[,"Age"],na.rm=T),2))#stringr::str_flatten(ranks$Age, collapse = ", ")
        #if(any(names(ranks)%in%"NON")){
        # latlogtab[i,"NON"]=as.numeric(round(mean(ranks[,"NON"],na.rm=T),2))
        # }
        snpname=names(data)[grep("Snp",names(data))]
        latlogtab[i,snpname]=foreach(x=1:length(snpname), .combine=cbind) %dopar% {
          latlogtab[i,snpname[x]]=mean(ranks[,snpname[x]],na.rm=T)
        }
        }else if(!is.null(type) && length(grep(type,names(data)))>0 && !is.na(ranks[1,1])){
          ancname=names(data)[grep(type,names(data))]
          latlogtab[i,"Count"]=sum(ranks$Count,na.rm=T)
          latlogtab[i,"Age"] =mean(ranks$Age, na.rm=T)
         charvec=c("Sample","Species","Sex")
          latlogtab[i,charvec]=foreach(x=1:length(charvec), .combine=cbind) %dopar% {
            latlogtab[i,charvec[x]]=stringr::str_flatten(ranks[,charvec[x]], collapse = ", ")
          }
          latlogtab[i,ancname]=foreach(x=1:length(ancname), .combine=cbind) %dopar% {
            latlogtab[i,ancname[x]]=as.numeric(round(mean(ranks[,ancname[x]],na.rm=TRUE),2))
          }
        }
    }        
  latlogtab=as.data.frame(latlogtab,stringsAsFactors=FALSE)
  latlogtab$Count=as.numeric(latlogtab$Count)
  latlogtab=latlogtab[latlogtab$Count>0,]
  latlogtab$Site=paste("Site",seq(1,nrow(latlogtab)),sep="")
  latlogtab$Latitude=as.numeric(latlogtab$Latitude)
  latlogtab$Longitude=as.numeric(latlogtab$Longitude)
  latlogtab[,snpancname]=apply(latlogtab[,snpancname],1,as.numeric)
   latlogtab$Age=as.numeric(latlogtab$Age)
  as.data.frame(latlogtab)
}
findLocations <- function(shape, location_coordinates, location_id_colname){
  polygon_coordinates <- shape$geometry$coordinates
  feature_type <- shape$properties$feature_type
    if(feature_type %in% c("rectangle","polygon")) {
    drawn_polygon <- Polygon(do.call(rbind,lapply(polygon_coordinates[[1]],function(x){c(x[[1]][1],x[[2]][1])})))
    selected_locs <- sp::over(location_coordinates
                              , sp::SpatialPolygons(list(sp::Polygons(list(drawn_polygon),"drawn_polygon"))))
    x = (location_coordinates[which(!is.na(selected_locs)), location_id_colname])
    selected_loc_id = as.character(x[[location_id_colname]])
    return(selected_loc_id)
  } else if (feature_type == "circle") {
  center_coords <- matrix(c(polygon_coordinates[[1]], polygon_coordinates[[2]])
                        , ncol = 2)
   dist_to_center <- spDistsN1(location_coordinates, center_coords, longlat=TRUE)
  x <- location_coordinates[dist_to_center < shape$properties$radius/100000, location_id_colname]
  selected_loc_id = as.character(x[[location_id_colname]])
  return(selected_loc_id)
   }
}