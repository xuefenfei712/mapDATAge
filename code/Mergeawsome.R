
getsnpname=function(data){
  unique(stri_replace_first_regex(stri_replace_last_regex(names(data)[grep("SNP_",names(data))],pattern="_[A,C,G,T,D]",replacement = ""),"SNP_",""))
}
getallename=function(data,snp){
  #c(gsub("_","",stri_extract_last_regex(names(data)[grep("SNP_",names(data))],"[A,C,G,T]")))
  substring(names(data)[grep(snp,names(data))],nchar(names(data)[grep(snp,names(data))]),nchar(names(data)[grep(snp,names(data))]))
}
addreadCounts=function(data,snp){
  ReadCount=apply(data %>% select(matches(snp)),1,sum)
  as.data.frame(cbind(data,ReadCount))
}

meansampling=function(xx,tt){
tab=cbind(xx,as.numeric(tt))
freq=""
sampletime=100
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

filter=function(data){
  for(i in getsnpname(data)){
    data[,paste(i,"TotalCounts",sep="")]=apply(data[names(data)[grep(getsnpname(data)[1],names(data))]],1,sum)
  }
  as.data.frame(data[apply(data %>% select(ends_with("TotalCounts")),1,function(x) all(x>0))==TRUE,])
  data[!names(data)%in%"TotalCounts"]
  data=data[,-grep("Total",names(data))]
}
MergeawsomeCount=function(data,snp=NULL,mult=FALSE,drop=FALSE){
    if(!is.null(snp)){
      d=data[,c("Sample","Age","Latitude","Longitude","Sex","Site","Species",names(data)[grep(snp,names(data))])]
      #d$Count=rep(1,nrow(d))
      d$Count=if_else(rowSums(d[,names(d)[grep(snp,names(d))]])>0,1,0)
      d=d[d$Count>0,]
      d$ReadCount=apply(d %>% select(matches(snp)),1,sum)
      # Convert to simple feature collection object (points)
      d2 <- st_as_sf(d, coords = c("Longitude", "Latitude"))
      # Get intersection of the same object
      d3 <- st_intersection(d2)
      # See origins columns (which elements intersects)
      d3$origins
      #a=names(d)[grep("Ance",names(d))]
      list <- lapply(1:length(d3$origins), function(x) {
        point <- d3[x, ]
        point$Site <-
          stringr::str_flatten(d2[d3$origins[[x]], ]$Site, collapse = ",")
        point$Sample <-
          stringr::str_flatten(d2[d3$origins[[x]], ]$Sample, collapse = ", ")
        point$Species <-
          stringr::str_flatten(d2[d3$origins[[x]], ]$Species, collapse = ", ")
        point$Sex <-
          stringr::str_flatten(d2[d3$origins[[x]], ]$Sex, collapse = ", ")
        point$Age <- round(mean(d2[d3$origins[[x]], ]$Age, na.rm = TRUE),2)
        point$Count <- round(sum(d2[d3$origins[[x]], ]$Count, na.rm = TRUE),2)
        point$ReadCount <- round(sum(d2[d3$origins[[x]], ]$ReadCount, na.rm = TRUE),2)
        aname=getallename(data,snp)
        a=paste("SNP",snp,aname[1],sep="_")
        point[,a]<- round(meansampling(matrix(unlist(d2[d3$origins[[x]],a]),ncol=3,byrow=FALSE)[,1],d2[d3$origins[[x]], ]$ReadCount),2)
        d=paste("SNP",snp,aname[2],sep="_")
        point[,d] <- round(meansampling(matrix(unlist(d2[d3$origins[[x]],d]),ncol=3,byrow=FALSE)[,1], d2[d3$origins[[x]], ]$ReadCount),2)
        return(point)
      })
      # Bind list points
      new_d <- do.call(rbind, list)
      # Add lat and long data as columns
      new_d$Longitude <- st_coordinates(new_d)[, "X"]
      new_d$Latitude <- st_coordinates(new_d)[, "Y"]
      # Transform to data frame adding null to geometries
      st_geometry(new_d) <- NULL
      as.data.frame(new_d)
      new_d[,"NON"]=1-new_d[,paste("SNP",snp,getallename(data,snp)[1],sep="_")]-new_d[,paste("SNP",snp,getallename(data,snp)[2],sep="_")]
      new_d$SecondSite=paste(as.character(new_d$Site),"_layer",sep="")
      as.data.frame(new_d)
    }else if(mult==TRUE){
     
      data=data[apply(data %>% select(starts_with("SNP")),1,sum)>0,]
      snpname=getsnpname(data)
      for(name in snpname){
        data$Counts=if_else(rowSums(data[,names(data)[grep("SNP",names(data))]])>0,1,0)
        data=data[data$Count>0,]
        total=apply(data%>% select(matches(name)),1,sum)
        for(j in names(data)[grep(name,names(data))]){
          inputdata=cbind(data[,j],total)
          for(i in 1:nrow(inputdata)){
            data[i,j]=meansampling(inputdata[i,1],inputdata[i,2])
          }
        }
      }
      d=data
      d2 <- st_as_sf(d, coords = c("Longitude", "Latitude"))
      d3 <- st_intersection(d2)
      d3$origins
      if(length(grep("SNP",names(d)))>0){
        a=names(d)[grep("SNP",names(d))]
        list <- lapply(1:length(d3$origins), function(x) {
          point <- d3[x, ]
          point$Site <-
            stringr::str_flatten(d2[d3$origins[[x]], ]$Site, collapse = ",")
          point$Sample <-
            stringr::str_flatten(d2[d3$origins[[x]], ]$Sample, collapse = ", ")
          point$Species <-
            stringr::str_flatten(d2[d3$origins[[x]], ]$Species, collapse = ", ")
          point$Sex <-
            stringr::str_flatten(d2[d3$origins[[x]], ]$Sex, collapse = ", ")
          point$Age <- round(mean(d2[d3$origins[[x]], ]$Age, na.rm = TRUE),2)
          point$Counts <- sum(d2[d3$origins[[x]], ]$Counts, na.rm = TRUE)
          for(i in a){
            point[,i] <- round(mean(data.frame(d2[d3$origins[[x]], i])[,i], na.rm = TRUE),2)
          }
          return(point)
        })
        new_d <- do.call(rbind, list)
        new_d$Longitude <- st_coordinates(new_d)[, "X"]
        new_d$Latitude <- st_coordinates(new_d)[, "Y"]
        #new_d$SecondSite=new_d$Site
        st_geometry(new_d) <- NULL
        #new_d=new_d[apply(new_d %>% select(starts_with("SNP")),1,sum)>0,]
        as.data.frame(new_d)
        new_d$Age=as.numeric(new_d$Age)
        filter(new_d)
        as.data.frame(new_d)
      }
    }
}
  Mergeawsome=function(data,snp=NULL,type=NULL,mult=FALSE,drop=FALSE){
    if(!is.null(snp)){
      d=data[,c("Sample","Age","Latitude","Longitude","Sex","Site","Species",names(data)[grep(snp,names(data))])]
      #d$Count=rep(1,nrow(d))
      d$Count=if_else(rowSums(d[,names(d)[grep(snp,names(d))]])>0,1,0)
      d=d[d$Count>0,]
      d2 <- st_as_sf(d, coords = c("Longitude", "Latitude"))
      d3 <- st_intersection(d2)
      d3$origins
      list <- lapply(1:length(d3$origins), function(x) {
        point <- d3[x, ]
        point$Site <-
          stringr::str_flatten(d2[d3$origins[[x]], ]$Site, collapse = ",")
        point$Sample <-
          stringr::str_flatten(d2[d3$origins[[x]], ]$Sample, collapse = ", ")
        point$Species <-
          stringr::str_flatten(d2[d3$origins[[x]], ]$Species, collapse = ", ")
        point$Sex <-
          stringr::str_flatten(d2[d3$origins[[x]], ]$Sex, collapse = ", ")
        point$Age <- round(mean(d2[d3$origins[[x]], ]$Age, na.rm = TRUE),2)
        point$Count <- round(sum(d2[d3$origins[[x]], ]$Count, na.rm = TRUE),2)
        aname=getallename(data,snp)
        a=paste("SNP_",snp,"_",aname[1],sep="")
        point[,a] <- round(mean(matrix(unlist(d2[d3$origins[[x]],a]),ncol=3,byrow=FALSE)[,1], na.rm = TRUE)/2,2)
        d=paste("SNP_",snp,"_",aname[2],sep="")
        point[,d] <- round(mean(matrix(unlist(d2[d3$origins[[x]],d]),ncol=3,byrow=FALSE)[,1], na.rm = TRUE)/2,2)
        return(point)
      })
      # Bind list points
      new_d <- do.call(rbind, list)
      new_d$Longitude <- st_coordinates(new_d)[, "X"]
      new_d$Latitude <- st_coordinates(new_d)[, "Y"]
      st_geometry(new_d) <- NULL
      
      as.data.frame(new_d)
      new_d[,"NON"]=1-new_d[,paste("SNP",snp,getallename(data,snp)[1],sep="_")]-new_d[,paste("SNP",snp,getallename(data,snp)[2],sep="_")]
      new_d$SecondSite=paste(as.character(new_d$Site),"_layer",sep="")
      as.data.frame(new_d)
    }else if(!is.null(type)){
      d=data
      d$Count=rep(1,nrow(d))
      d2 <- st_as_sf(d, coords = c("Longitude", "Latitude"))
      d3 <- st_intersection(d2)
      d3$origins
      if(length(grep(type,names(d)))>0){
        a=names(d)[grep(type,names(d))]
        list <- lapply(1:length(d3$origins), function(x) {
          point <- d3[x, ]
          point$Site <-
            stringr::str_flatten(d2[d3$origins[[x]], ]$Site, collapse = ",")
          point$Sample <-
            stringr::str_flatten(d2[d3$origins[[x]], ]$Sample, collapse = ", ")
          point$Species <-
            stringr::str_flatten(d2[d3$origins[[x]], ]$Species, collapse = ", ")
          point$Sex <-
            stringr::str_flatten(d2[d3$origins[[x]], ]$Sex, collapse = ", ")
          point$Age <- round(mean(d2[d3$origins[[x]], ]$Age, na.rm = TRUE),2)
          point$Count <- sum(d2[d3$origins[[x]], ]$Count, na.rm = TRUE)
          for(i in a){
            point[,i] <- round(mean(data.frame(d2[d3$origins[[x]], i])[,i], na.rm = TRUE),2)
          }
          return(point)
        })
        new_d <- do.call(rbind, list)
        new_d$Longitude <- st_coordinates(new_d)[, "X"]
        new_d$Latitude <- st_coordinates(new_d)[, "Y"]
        new_d$SecondSite=new_d$Site
        st_geometry(new_d) <- NULL
        as.data.frame(new_d)
      }}else if(mult==TRUE){
        data=data[apply(data %>% select(starts_with("SNP")),1,sum)>0,]
        d=data
        d$Counts=rep(1,nrow(d))
        d2 <- st_as_sf(d, coords = c("Longitude", "Latitude"))
        d3 <- st_intersection(d2)
        d3$origins
        if(length(grep("SNP",names(d)))>0){
          a=names(d)[grep("SNP",names(d))]
          list <- lapply(1:length(d3$origins), function(x) {
            point <- d3[x, ]
            point$Site <-
              stringr::str_flatten(d2[d3$origins[[x]], ]$Site, collapse = ",")
            point$Sample <-
              stringr::str_flatten(d2[d3$origins[[x]], ]$Sample, collapse = ", ")
            point$Species <-
              stringr::str_flatten(d2[d3$origins[[x]], ]$Species, collapse = ", ")
            point$Sex <-
              stringr::str_flatten(d2[d3$origins[[x]], ]$Sex, collapse = ", ")
            point$Age <- round(mean(d2[d3$origins[[x]], ]$Age, na.rm = TRUE),2)
            point$Counts <- sum(d2[d3$origins[[x]], ]$Counts, na.rm = TRUE)
            for(i in a){
              point[,i] <- round(mean(data.frame(d2[d3$origins[[x]], i])[,i], na.rm = TRUE),2)
            }
            return(point)
          })
          new_d <- do.call(rbind, list)
          new_d$Longitude <- st_coordinates(new_d)[, "X"]
          new_d$Latitude <- st_coordinates(new_d)[, "Y"]
          #new_d$SecondSite=new_d$Site
          st_geometry(new_d) <- NULL
          #new_d=new_d[apply(new_d %>% select(starts_with("SNP")),1,sum)>0,]
          as.data.frame(new_d)
          new_d$Age=as.numeric(new_d$Age)
          filter(new_d)
          as.data.frame(new_d)
        }
        }else{
        d=data
        d$Count=rep(1,nrow(d))
        d2 <- st_as_sf(d, coords = c("Longitude", "Latitude"))
        d3 <- st_intersection(d2)
        d3$origins
        list <- lapply(1:length(d3$origins), function(x) {
          point <- d3[x, ]
          point$Site <-
            stringr::str_flatten(d2[d3$origins[[x]], ]$Site, collapse = ", ")
          point$Sample <-
            stringr::str_flatten(d2[d3$origins[[x]], ]$Sample, collapse = ", ")
          point$Species <-
            stringr::str_flatten(d2[d3$origins[[x]], ]$Species, collapse = ", ")
          point$Sex <-
            stringr::str_flatten(d2[d3$origins[[x]], ]$Sex, collapse = ", ")
          point$Age <- round(mean(d2[d3$origins[[x]], ]$Age, na.rm = TRUE),2)
          point$Count <- sum(d2[d3$origins[[x]], ]$Count, na.rm = TRUE)
          if(any(names(d2)%in%"Coverage")){
            point$Coverage <-
              stringr::str_flatten(d2[d3$origins[[x]], ]$Coverage, collapse = ", ")
          }
          return(point)
        })
        new_d <- do.call(rbind, list)
        # Add lat and long data as columns
        new_d$Longitude <- st_coordinates(new_d)[, "X"]
        new_d$Latitude <- st_coordinates(new_d)[, "Y"]
        # Transform to data frame adding null to geometries
        st_geometry(new_d) <- NULL
        as.data.frame(new_d)
        new_d$SecondSite=paste(as.character(new_d$Site),"_layer",sep="")
        filter(new_d)
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
  longitudest=as.numeric(par[8,2])
  longitudeend=as.numeric(par[9,2])
  latitudest=as.numeric(par[10,2])
  latitudeend=as.numeric(par[11,2])
  type=as.character(par[12,2])###ReadsCounts or Genotype
  #dir.create(paste(path,"/",sep=""))
  setwd(paste(path,sep=""))
  files=list.files()[grep("mapDATAge",list.files())]
  for(fi in 1:length(files)){
    if (grepl(files[fi], pattern = ".txt") |
        grepl(files[fi], pattern = ".tsv")){
      data=read.table(files[fi],header=T,sep="\t",stringsAsFactors = F,
                      quote = "", comment.char = "")
    }
    
  #######plot map#############
  dat=data[data$Age >=st & data$Age <end & data$Longitude >=longitudest & data$Longitude <longitudeend & data$Latitude>=latitudest &  data$Latitude <latitudeend,]
  data1=dat[dat$Sex%in%sexa & dat$Species%in%spexa,]
  data1$Count=if_else(apply(data1[,names(data1)[grep("SNP_",names(data1))]],1,sum)>0,1,0)
  data1=data1[data1$Count>0,]
  if(type=="ReadsCounts"){
  CountReads=apply(data1 %>% select(starts_with("SNP_")),1,sum)
  data1=cbind(data1,CountReads)
  }
  time1=seq(floor(st/10)*10,ceiling(end/10)*10-windowsize,stepsize)
  time2=seq(floor(st/10)*10+windowsize,ceiling(end/10)*10,stepsize)
  label=paste(time1,time2,sep="-")
  #label[length(label)]=paste(">",time1[length(label)],sep="")
  sort=paste("Time",seq(1,length(label)),sep="")
  timtab=as.data.frame(cbind(time1,time2,label,sort))
  timtab$time1=as.numeric(timtab$time1);
  timtab$time2=as.numeric(timtab$time2)
  timtab$mean=NA;timtab$sd=NA;timtab$count=NA
  a=NA;b=NA
  base=c("A","C","G","T")
  if(par[7,2]%in%base){
    gt1=par[7,2]
  }else{
    gt1=getallename(data1,gsub("mapDATAge-","",gsub(".txt","",files[fi])))
  }
for(n in gt1){
  gtt=paste("SNP_",getsnpname(data1),"_",n,sep="")
  #gtt=names(data1)[grep(paste("_",gt,sep=""),names(data1))]
  if(type=="ReadsCounts"){
    for(i in 1:nrow(timtab)){
      a=data1[data1$Age>=as.numeric(timtab[i,1]) & data1$Age<as.numeric(timtab[i,2]),c(gtt,"CountReads")]
      if(is.na(a[1,1])){
        timtab[i,5]=0
        timtab[i,6]=0
        timtab[i,7]=0
      }else if(!is.na(a[1,1])){
        sampletime=100
        outmatrix=as.data.frame(matrix(data="",ncol=as.numeric(sampletime),nrow=nrow(a)))
        for(s in 1:sampletime){
          for(j in 1:nrow(a)){
            if(isTRUE(a[j,2]-a[j,1]>0)){
              outmatrix[j,s]=as.numeric(sample(c(rep(1,a[j,1]),rep(0,(a[j,2]-a[j,1]))),1))
            }else if(isTRUE(a[j,2]==a[j,1] & a[j,2]>0)){
              outmatrix[j,s]=1
            }
            else{
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
  }else if(type=="Genotype"){
    for(i in 1:nrow(timtab)){
      a=as.numeric(data1[(data1$Age>=as.numeric(timtab[i,1]) & data1$Age<as.numeric(timtab[i,2])),gtt])
      if(length(a)==0){
        timtab[i,5]=0
        timtab[i,6]=0
        timtab[i,7]=0
      }else{
        timtab[i,5]=mean(a, na.rm = TRUE)/2
        timtab[i,6]=sqrt(timtab[i,5]*(1-timtab[i,5])/length(a))
        timtab[i,7]=length(a)
      }
    }
  }
  timtab[is.na(timtab)] <- 0
  name=getsnpname(data1)
  # Utiliser geom_line()+geom_pointrange()
  #ggplot(timtab, aes(x=fct_inorder(sort),y=mean,group=1)) + 
   # geom_line()+
   # geom_pointrange(aes(ymin=mean-sd, ymax=mean+sd))+
   # labs(x="",y=paste("F_",name,"(",n,")",sep=""))+theme(axis.text.x=element_text(angle=90, hjust=1))+
  #  scale_x_discrete(labels=label)+ylim(0,1.2)+
   # geom_point(timtab,aes(x=fct_inorder(sort), y=count,angle=0),size = 2,col="orange")
 # ggsave(paste(path,"/",name,"-",st,"-",end,"-",n,"-allefreq.pdf",sep=""),width=8,height=6,units="in")
  timtab$label=fct_inorder(timtab$label)
  p=plot_ly(timtab,x=~label,y=~mean,type="scatter",name="frequency",mode = 'lines+markers',error_y=~list(array=sd,color="#000000"),
          hoverinfo = 'text',text=~paste('</br> timebin: ',label,'</br> mean: ',round(mean,2),
                                         '</br> sd: ',round(sd,2),'</br> n: ',count),
          width = 800, height = 500) %>%#y=ymax+0.1,x=0.1,
    add_trace(y=~count,type="scatter",mode="markers",yaxis="y2", name="Counts") %>%
    layout(title=list(text="Allele trajectory",size=30,xref="paper",yref="paper",x=0,y=11,xanchor = "left", yanchor =  "top",color="black",family="Arial"),
           xaxis=list(title="",tickangle = 290),
           yaxis=list(title=paste("F_",name,"(",n,")",sep=""),range=c(0,1.1),side="left"),
           yaxis2=list(side="right",zeroline=FALSE,autorange=TRUE,overlaying="y",showlegend=FALSE,title="Counts"),
           legend = list(orientation = "v", xanchor = "right",x=1, y = 1.02),
           margin=list(l=45,r=50,b=20,t=80,pad=4))
  saveWidget(p,file=paste(path,"/",name,"-",st,"-",end,"-",n,"-allefreq.html",sep=""))
}
   ###getmap data##
  if(type=="Genotype"){
    datamap=Mergeawsome(data1,snp=getsnpname(data1))
  }else if(type=="ReadsCounts"){
    datamap=MergeawsomeCount(data1,snp=getsnpname(data1))
  }

#########drawmap################
tilesURL <- "http://server.arcgisonline.com/ArcGIS/rest/services/Canvas/World_Light_Gray_Base/MapServer/tile/{z}/{y}/{x}"
basemap <- leaflet(options = leafletOptions(zoomControl = FALSE, minZoom = 2, maxZoom = 5, dragging = T)) %>%
  addTiles(tilesURL) %>%
  fitBounds(min(datamap$Longitude),min(datamap$Latitude),max(datamap$Longitude),max(datamap$Latitude))

colorpal <- colorNumeric("Set1", datamap$Age)
sizes <- sizeNumeric((datamap$Count), baseSize = mean(datamap$Count))
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

saveWidget(map,paste(path,"/",name,"-",st,"-",end,"-map.html",sep=""))
#####draw pie map
maxValue <- 1
if(type=="Genotype"){
  datamap=Mergeawsome(data1)
}else if(type=="ReadsCounts"){
  datamap=MergeawsomeCount(data1,snp=getsnpname(data1))
}
#datamap=datamap[datamap$Count>0,]
sizes <- sizeNumeric((datamap$Count), baseSize = mean(datamap$Count))
#########drawmap################
#tilesURL <- "http://server.arcgisonline.com/ArcGIS/rest/services/Canvas/World_Light_Gray_Base/MapServer/tile/{z}/{y}/{x}"
basemap <- leaflet(options = leafletOptions(zoomControl = FALSE, minZoom = 2, maxZoom = 5, dragging = T)) %>%
  addTiles(tilesURL) %>%
   fitBounds(min(datamap$Longitude)+1,min(datamap$Latitude)+1,max(datamap$Longitude)-1,max(datamap$Latitude)-1) %>%
  addResetMapButton() %>% addMiniMap(width=100,height=100,toggleDisplay = TRUE,position = "topright")
piemap=basemap %>%
  addMinicharts(
    datamap$Longitude, datamap$Latitude,
    chartdata =  cbind(datamap %>% select(starts_with("SNP_")),datamap$NON),
    maxValues = maxValue,
    type ="pie",
    colorPalette = brewer.pal(11, "RdYlGn")[c(1,9,3,5,6)],
    popup = popupArgs(
      labels = c(substring(names(datamap)[grep("SNP",names(datamap))],nchar(names(datamap)[grep("SNP",names(datamap))]),nchar(names(datamap)[grep("SNP",names(datamap))])),"NON"),
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
saveWidget(piemap,paste(path,"/",name,"-",st,"-",end,"-piemap.html",sep=""))
########ancestral map##############

if(length(grep("Anc",names(data1)))>0){
  datamap1=Mergeawsome(data1,type="Anc")
  sizes <- sizeNumeric((datamap1$Count), baseSize = mean(datamap1$Count))
  #########drawmap################
  #tilesURL <- "http://server.arcgisonline.com/ArcGIS/rest/services/Canvas/World_Light_Gray_Base/MapServer/tile/{z}/{y}/{x}"
  basemap <- leaflet(options = leafletOptions(zoomControl = FALSE, minZoom = 2, maxZoom = 5, dragging = T)) %>%
    addTiles(tilesURL) %>%
    fitBounds(min(datamap1$Longitude)+1,min(datamap1$Latitude)+1,max(datamap1$Longitude)-1,max(datamap1$Latitude)-1) %>%
    addResetMapButton() %>% addMiniMap(width=100,height=100,toggleDisplay = TRUE,position = "topright")
  maxValue=1
  ancescomp=names(datamap1)[grep("Anc",names(datamap1))]
  ancesmap=basemap %>%
    addMinicharts(
      datamap1$Longitude, datamap1$Latitude,
      chartdata =  datamap1[,ancescomp],
      maxValues = maxValue,
      type ="pie",legend=TRUE,
      colorPalette = brewer.pal(11, "RdYlGn")[c(1,5,11,3,9,2,8,4)],
      popup = popupArgs(
        labels = datamap1$ancescomp,
        html = paste0(
          "<div>","<h6>","<br>",datamap1$Sample,"<h6>",
          "<h7>","Sex: ",datamap1$Sex,"<br>",
          "Species: ",datamap1$Species,"<br>",
          "Age: ",datamap1$Age,"<br>",
          "Count: ",datamap1$Count,"</div>"
        )
      ),
      width = 10*sqrt(datamap1$Count),transitionTime = 0
    ) %>%
    addLegendSize(position = "bottomleft",col="black",fillColor="white",baseSize = quantile(sqrt(datamap$Count),0.98),
                  values = sizes,shape="circle",orientation="horizontal",breaks=4
    )
  saveWidget(ancesmap,paste(path,"/",name,"-",st,"-",end,"-ancmap.html",sep=""))
  
}else{
  basemap
}

  }
}

#####mergeCAT#######
mergeCAT=function(data,catname){
  #cat=names(data)[grep("CAT_",names(data))]
 # name=unique(stri_replace_first_regex(names(data)[grep("CAT",names(data))],"CAT_",""))
  cat=paste("CAT_",catname,sep="")
  data2=data[data[,cat] !="unknown",]
  yname=sort(unique(data2[,cat]))
  ymax=matrix(0,nrow=nrow(data2),ncol=length(yname))
  colnames(ymax)=yname
  for(i in 1:nrow(ymax)){
    ymax[i,data2[i,cat]]=1
  }
  ymax=as.data.frame(ymax)
  colnames(ymax)=paste(catname,yname,sep="_")
  ymax$Count=apply(ymax,1,sum)
  yresult=cbind(data2[,c("Sample","Age","Site","Latitude","Longitude","Sex","Species")],ymax)
  as.data.frame(yresult)
  Mergeawsome(yresult,type=catname)
 }
