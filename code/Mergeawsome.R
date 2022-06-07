
colname=c("SAMPLE","AGE","LATITUDE","LONGITUDE","SITE","SPECIES","SEX")
validate_input_files <- function(table) {
  if (is.null(colnames(table))) {
    stop("The input table should have column names.")
  }
  if (!(all(colname %in% colnames(table)))) {
    stop("Please make sure that all required columns are provided in the uploaded table")
  }
  if (!(all(sapply(table[,c("AGE","LATITUDE","LONGITUDE")],is.numeric)))) {
    stop("Please make sure that age, latitude and longitude columns are numeric!")
  }
  if (!(all(sapply(table[,c("SITE","SPECIES","SEX","SAMPLE")],is.character)))) {
    stop("Please make sure that sample, sex, species, site columns are numeric!")
  }
}
getsnpname=function(data){
  unique(stringi::stri_replace_first_regex(stringi::stri_replace_last_regex(names(data)[grep("SNP_",names(data))],pattern="_[A,C,G,T,D]",replacement = ""),"SNP_",""))
}
getallename=function(data,snp){
  substring(names(data)[grep(snp,names(data))],nchar(names(data)[grep(snp,names(data))]),nchar(names(data)[grep(snp,names(data))]))
}
addreadCounts=function(data,snp){
  ReadCount=apply(data %>% select(matches(snp)),1,sum)
  as.data.frame(cbind(data,ReadCount))
}

samtime=function(xx,tt){
  sampletime=100
  indata=cbind(xx,tt)
  for(i in 1:nrow(indata)){
    if(indata[i,2]>=1 & indata[i,1]>=1  &  nrow(indata)>0){
      indata[i,1]=mean(sample(c(rep(1,indata[i,1]),rep(0,indata[i,2])),sampletime,replace =TRUE),na.rm = T)
    }else if(indata[i,2]==0 &  indata[i,1]>0){
      indata[i,1]=1
    }else{
      indata[i,1]=0
    }
  }
  indata[,2]=1-indata[,1]
  as.data.frame(indata)
}

filter=function(data){
  for(i in getsnpname(data)){
    data[,paste(i,"TotalCounts",sep="")]=apply(data[names(data)[grep(getsnpname(data)[1],names(data))]],1,sum)
  }
  as.data.frame(data[apply(data %>% select(ends_with("TotalCounts")),1,function(x) all(x>0))==TRUE,])
  data[!names(data)%in%"TotalCounts"]
  data=data[,-grep("Total",names(data))]
}

  Mergeawsome=function(data,mult=FALSE,snplist=NULL,drop=FALSE){
    charvec=c("SITE","SAMPLE","SPECIES","SEX")
    if(mult==TRUE & length(snplist)>0 &nrow(data)>0){
       snpname=unique(stringi::stri_replace_first_regex(stringi::stri_replace_last_regex(grep("SNP_",snplist,value = T),pattern="_[A,C,G,T,D]",replacement = ""),"SNP_",""))
        d=data[,c(charvec,snplist,"LATITUDE","LONGITUDE","AGE")]
        d$Counts=rep(1,nrow(d))
        d2 <- st_as_sf(d, coords = c("LONGITUDE", "LATITUDE"))
        d3 <- st_intersection(d2)
          a=names(d)[grep("SNP",names(d))]
          list <- lapply(1:length(d3$origins), function(x) {
            point <- d3[x, ]
            point[,charvec]=apply((data.frame(d2[d3$origins[[x]], charvec])[,charvec]),2,function(x) paste(x, collapse=", "))
            point$AGE <- round(mean(d2[d3$origins[[x]], ]$AGE, na.rm = TRUE),2)
            point$Counts <- sum(d2[d3$origins[[x]], ]$Counts, na.rm = TRUE)
            point[,a]=apply(as.data.frame(data.frame(d2[d3$origins[[x]], a])[,a]),2,mean)
            return(point)
          })
          new_d <- do.call(rbind, list)
          new_d$LONGITUDE <- sf::st_coordinates(new_d)[, "X"]
          new_d$LATITUDE <- sf::st_coordinates(new_d)[, "Y"]
          st_geometry(new_d) <- NULL
          as.data.frame(new_d)
          new_d$AGE=as.numeric(new_d$AGE)
          filter(new_d)
          as.data.frame(new_d)
        }
  }

  Mergetype=function(data,type=NULL,drop=FALSE){
    chc=c("SITE","SAMPLE","SPECIES","SEX");num=c("LATITUDE","LONGITUDE","AGE")
    if(type=="ANC" & nrow(data)>0){
      char=names(data)[grep("ANC",names(data))]
      d=data[,c(chc,char,num)]
   # }
    d$Count=rep(1,nrow(d))
    d2 <- sf::st_as_sf(d, coords = c("LONGITUDE", "LATITUDE"))
    d3 <- sf::st_intersection(d2)
    list <- lapply(1:length(d3$origins), function(x) {
      point <- d3[x, ]
      point[,chc]=apply((data.frame(d2[d3$origins[[x]], chc])[,chc]),2,function(x) paste(x, collapse=", "))
      point$AGE <- round(mean(d2[d3$origins[[x]], ]$AGE, na.rm = TRUE),2)
      point$Count <- sum(d2[d3$origins[[x]], ]$Count, na.rm = TRUE)
        point[,char]=apply(data.frame(d2[d3$origins[[x]], char])[,1:length(char)],2,mean)
      return(point)
    })
    new_d <- do.call(rbind, list)
    new_d$LONGITUDE <- sf::st_coordinates(new_d)[, "X"]
    new_d$LATITUDE <- sf::st_coordinates(new_d)[, "Y"]
    sf::st_geometry(new_d) <- NULL
    as.data.frame(new_d)
    #new_d[,"NA"]=1-apply(new_d%>%select(matches("ANC")),1,sum)
    new_d$SecondSite=paste(as.character(new_d$SITE),"_layer",sep="")
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
  LONGITUDEst=as.numeric(par[8,2])
  LONGITUDEend=as.numeric(par[9,2])
  LATITUDEst=as.numeric(par[10,2])
  LATITUDEend=as.numeric(par[11,2])
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
    colnames(data)=toupper(names(data))
  #######plot map#############
  dat=data[data$AGE >=st & data$AGE <end & data$LONGITUDE >=LONGITUDEst & data$LONGITUDE <LONGITUDEend & data$LATITUDE>=LATITUDEst &  data$LATITUDE <LATITUDEend,]
  data1=dat[dat$SEX%in%sexa & dat$SPECIES%in%spexa,]
  data1$Count=dplyr::if_else(apply(data1[,names(data1)[grep("SNP_",names(data1))]],1,sum)>0,1,0)
  data1=data1[data1$Count>0,]
  if(type=="ReadCounts"){
  CountReads=apply(data1 %>% select(starts_with("SNP_")),1,sum)
  data1=cbind(data1,CountReads)
  }
  time1=seq(floor(st/10)*10,ceiling(end/10)*10-windowsize,stepsize)
  time2=seq(floor(st/10)*10+windowsize,ceiling(end/10)*10,stepsize)
  label=paste(time1,time2,sep="-")
  sort=paste("Time",seq(1,length(label)),sep="")
  timtab=as.data.frame(cbind(time1,time2,label,sort))
  timtab$time1=as.numeric(timtab$time1);
  timtab$time2=as.numeric(timtab$time2)
  timtab$mean=NA;timtab$sd=NA;timtab$count=NA
  a=NA;b=NA
  base=c("A","C","G","T","D")
  if(par[7,2]%in%base){
    gt1=par[7,2]
  }else{
    gt1=getallename(data1,getsnpname(data1))
  }
for(n in gt1){
  gtt=paste("SNP_",getsnpname(data1),"_",n,sep="")
 if(type=="Genotype"){
    foreach(i=1:nrow(timtab)) %do% {
      a=data1[data1$AGE>=as.numeric(timtab[i,1]) & data1$AGE<as.numeric(timtab[i,2]),gtt]
      timtab[i,5]=ifelse(length(a)==0,0,mean(a,na.rm=TRUE)/2)
      timtab[i,6]=ifelse(length(a)==0,0,1.96*sqrt(timtab[i,5]*(1-timtab[i,5])/length(a)/2))#sqrt(sd(a, na.rm = TRUE)/length(a))#
      timtab[i,7]=length(a)
    }
  }else if(type=="ReadCounts"){
    sampletime=100
    foreach(i=1:nrow(timtab)) %do% {
      a=data1[as.numeric(data1$AGE)>=as.numeric(timtab[i,1]) & as.numeric(data1$AGE)<as.numeric(timtab[i,2]),c(gtt,"CountReads")]
      timtab[i,5:7]=rbind(samtimeSD(a))
    }}
  timtab[is.na(timtab)] <- 0
  name=getsnpname(data1)
 timtab$label=forcats::fct_inorder(timtab$label)
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
    datamap=Merge(data1)
#########drawmap################
tilesURL <- "http://server.arcgisonline.com/ArcGIS/rest/services/Canvas/World_Light_Gray_Base/MapServer/tile/{z}/{y}/{x}"
basemap <- leaflet(options = leafletOptions(zoomControl = FALSE, minZoom = 2, maxZoom = 5, dragging = T)) %>%
  addTiles(tilesURL) %>%
  fitBounds(min(datamap$LONGITUDE),min(datamap$LATITUDE),max(datamap$LONGITUDE),max(datamap$LATITUDE))

colorpal <- colorNumeric("Set1", datamap$AGE)
sizes <- sizeNumeric((datamap$Count), baseSize = mean(datamap$Count))
map=basemap %>%
  addCircleMarkers(datamap$LONGITUDE, datamap$LATITUDE,color = "#777777",radius=sqrt(sizes)*3#datamap$Count*1.5
                   ,weight=1,
                   fillColor = colorpal(datamap$AGE), fillOpacity = 0.9,popup= paste0(
                     "<div>","<h6>","<br>",datamap$SAMPLE,"<h6>",
                     "<h7>","Sex: ",datamap$SEX,"<br>",
                     "Species: ",datamap$SPECIES,"<br>",
                     "AGE: ",datamap$AGE,"<br>",
                     "Count: ",datamap$Count,"</div>")
  ) %>%
  addLegendSize(position = "bottomleft",col="black",fillColor="white",baseSize =min(sizeNumeric(datamap$Count, baseSize = mean(datamap$Count)*4)),
                values = sizeNumeric((datamap$Count), baseSize = mean(datamap$Count)),shape="circle",orientation="horizontal",breaks=5) %>%
  addLegendNumeric(position = "bottomright",
                   pal = colorpal, values = datamap$AGE)

saveWidget(map,paste(path,"/",name,"-",st,"-",end,"-map.html",sep=""))
#####draw pie map
maxValue <- 1
if(type=="Genotype"){
  datamap=MergeSNP(data1,snp=getsnpname(data1),gtp="Genotype")
}else if(type=="ReadCounts"){
  datamap=MergeSNP(data1,snp=getsnpname(data1),gtp="ReadCounts")
}
#datamap=datamap[datamap$Count>0,]
sizes <- sizeNumeric((datamap$Count), baseSize = mean(datamap$Count))
#########drawmap################
basemap <- leaflet(options = leafletOptions(zoomControl = FALSE, minZoom = 2, maxZoom = 5, dragging = T)) %>%
  addTiles(tilesURL) %>%
   fitBounds(min(datamap$LONGITUDE)+1,min(datamap$LATITUDE)+1,max(datamap$LONGITUDE)-1,max(datamap$LATITUDE)-1) %>%
  addResetMapButton() %>% addMiniMap(width=100,height=100,toggleDisplay = TRUE,position = "topright")
piemap=basemap %>%
  addMinicharts(
    datamap$LONGITUDE, datamap$LATITUDE,
    chartdata =  cbind(datamap %>% select(starts_with("SNP_"))),
    maxValues = maxValue,
    type ="pie",
    colorPalette = brewer.pal(11, "RdYlGn")[c(1,9,3,5,6)],
    popup = popupArgs(
      labels = c(substring(names(datamap)[grep("SNP",names(datamap))],nchar(names(datamap)[grep("SNP",names(datamap))]),nchar(names(datamap)[grep("SNP",names(datamap))]))),
      html = paste0(
        "<div>","<h6>","<br>",datamap$SAMPLE,"<h6>",
        "<h7>","Sex: ",datamap$SEX,"<br>",
        "Species: ",datamap$SPECIES,"<br>",
        "AGE: ",datamap$AGE,"<br>",
        "Count: ",datamap$Count,"</div>"
      )
    ),
    width = 6*sqrt(datamap$Count),transitionTime = 0
  ) %>%
  addLegendSize(position = "bottomleft",col="black",fillColor="white",baseSize = quantile(sqrt(datamap$Count),0.98),
                values = sizes,shape="circle",orientation="horizontal",breaks=4)
saveWidget(piemap,paste(path,"/",name,"-",st,"-",end,"-piemap.html",sep=""))
########ancestral map##############

if(length(grep("ANC",names(data1)))>0){
  datamap1=Mergetype(data1,type="ANC")
  sizes <- sizeNumeric((datamap1$Count), baseSize = mean(datamap1$Count))
  
basemap <- leaflet(options = leafletOptions(zoomControl = FALSE, minZoom = 2, maxZoom = 5, dragging = T)) %>%
    addTiles(tilesURL) %>%
    fitBounds(min(datamap1$LONGITUDE)+1,min(datamap1$LATITUDE)+1,max(datamap1$LONGITUDE)-1,max(datamap1$LATITUDE)-1) %>%
    addResetMapButton() %>% addMiniMap(width=100,height=100,toggleDisplay = TRUE,position = "topright")
  maxValue=1
  ancescomp=names(datamap1)[grep("ANC",names(datamap1))]
  ancesmap=basemap %>%
    addMinicharts(
      datamap1$LONGITUDE, datamap1$LATITUDE,
      chartdata =  data.frame(lapply(datamap1[,ancescomp],as.numeric)),
      maxValues = maxValue,
      type ="pie",legend=TRUE,
      colorPalette = brewer.pal(11, "RdYlGn")[c(1,5,11,3,9,2,8,4)],
      popup = popupArgs(
        labels = datamap1$ancescomp,
        html = paste0(
          "<div>","<h6>","<br>",datamap1$SAMPLE,"<h6>",
          "<h7>","Sex: ",datamap1$SEX,"<br>",
          "Species: ",datamap1$SPECIES,"<br>",
          "AGE: ",datamap1$AGE,"<br>",
          "Count: ",datamap1$COUNT,"</div>"
        )
      ),
      width = 10*sqrt(datamap1$COUNT),transitionTime = 0
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
###newadd
Merge=function(data){
  if(!is.null(data) &nrow(data)>0){
  d=data[,c("SITE","SAMPLE","SPECIES","SEX","AGE","LONGITUDE", "LATITUDE")]
  d$Count=rep(1,nrow(d))
  charvec=c("SITE","SAMPLE","SPECIES","SEX")
  d2 <- sf::st_as_sf(d, coords = c("LONGITUDE", "LATITUDE"))
  d3 <- sf::st_intersection(d2)#parallel::mc
  list <- lapply(1:length(d3$origins), function(x) {
    point <- d3[x, ]
    point[,charvec]=apply((data.frame(d2[d3$origins[[x]], charvec])[,charvec]),2,function(x) paste(x, collapse=", "))
    point$AGE <- round(mean(d2[d3$origins[[x]], ]$AGE, na.rm = TRUE),2)
    point$Count <- sum(d2[d3$origins[[x]], ]$Count, na.rm = TRUE)
    return(point)
  })
  new_d <- do.call(rbind, list)
  new_d$LONGITUDE <- sf::st_coordinates(new_d)[, "X"]
  new_d$LATITUDE <- sf::st_coordinates(new_d)[, "Y"]
  sf::st_geometry(new_d) <- NULL
  new_d$SecondSite=paste(as.character(new_d$SITE),"_layer",sep="")
  as.data.frame(new_d)
  }
}
samtime=function(indata){
  sampletime=100
  for(i in 1:nrow(indata)){
    if(indata[i,2]>=1 & indata[i,1]>=1  &  nrow(indata)>0){
      indata[i,1]=mean(sample(c(rep(1,indata[i,1]),rep(0,indata[i,2])),sampletime,replace =TRUE),na.rm = T)
      indata[i,2]=1-indata[i,1]
      }else if(indata[i,2]==0 &  indata[i,1]>0){
      indata[i,1]=1
      indata[i,2]=0
    }else if(indata[i,2]>0 &  indata[i,1]==0){
      indata[i,1]=0
      indata[i,2]=1
    }else{
      indata[i,1]=0
      indata[i,2]=0
    }
  }
  apply(as.data.frame(indata),2,mean)
}
MergeSNP=function(data,snp=NULL,gtp=NULL,drop=FALSE){
  chc=c("SITE","SAMPLE","SPECIES","SEX")
  if(!is.null(data)&nrow(data)>0){
  d=data[,c(chc,names(data)[grep(snp,names(data))],"LONGITUDE", "LATITUDE","AGE")]
  d$Count=dplyr::if_else(rowSums(d[,names(d)[grep(snp,names(d))]])>0,1,0)
  if(sum(d$Count)>0){
  d=d[d$Count>0,]
  d$ReadCount=apply(d %>% select(matches(snp)),1,sum)
  d2 <- sf::st_as_sf(d, coords = c("LONGITUDE", "LATITUDE"))
  d3 <- sf::st_intersection(d2)
  list <- lapply(1:length(d3$origins), function(x) {
    point <- d3[x, ]
    point[,chc]=apply((data.frame(d2[d3$origins[[x]], chc])[,chc]),2,function(x) paste(x, collapse=", "))
    point$AGE <- round(mean(d2[d3$origins[[x]], ]$AGE, na.rm = TRUE),2)
    point$Count <- sum(d2[d3$origins[[x]], ]$Count, na.rm = TRUE)
    a=names(d)[grep(snp,names(d))]
    if(gtp=="Genotype"){
      point[,a]=apply(data.frame(d2[d3$origins[[x]], a])[,1:2],2,mean)
      }else if(gtp=="ReadCounts"){
        point[,a]=samtime(data.frame(d2[d3$origins[[x]],a])[,1:2])
        }
    return(point)
  })
  new_d <- do.call(rbind, list)
  new_d$LONGITUDE <- sf::st_coordinates(new_d)[, "X"]
  new_d$LATITUDE <- sf::st_coordinates(new_d)[, "Y"]
  sf::st_geometry(new_d) <- NULL
  new_d$SecondSite=paste(as.character(new_d$SITE),"_lay",sep="")
  new_d=as.data.frame(new_d)
  new_d[,"NA"]=ifelse(gtp=="Genotype",2-apply(new_d %>% select(matches(snp)),1,sum),1-apply(new_d %>% select(matches(snp)),1,sum))
  as.data.frame(new_d)
  }
}}
###pca plot
drawpca=function(data,sex,species,st,end,pca,lat1,lat2,log1,log2,pc1,pc2){
  st=as.numeric(st);end=as.numeric(end)
  lat1=as.numeric(lat1)
  lat2=as.numeric(lat2)
  log1=as.numeric(log1)
  log2=as.numeric(log2)
  data1=data[data$SEX%in%sex & data$SPECIES%in%species & data$AGE >=st & data$AGE <=end & data$LATITUDE >=lat1 & data$LATITUDE <lat2 & data$LONGITUDE >=log1 & data$LONGITUDE <log2,]
  data1$PC1=round(data1[,pc1],4);data1$PC2=round(data1[,pc2],4)
  
  plot_ly(data=data1,x=~PC1,y=~PC2,color = ~AGE,type = 'scatter', mode = 'markers',colors=brewer_pal(palette = "RdYlGn")(11),symbol=~SPECIES,
          symbols = c('circle','x','o'),text=~paste("Sample: ",SAMPLE, '<br>Age: ', AGE)
  ) %>%
    layout(title=list(text="PCA",size=20),xaxis=list(title=paste(pc1,"(",round(pca[pca[,1]==pc1,2],2),"%)",sep="")),
           yaxis=list(title=paste(pc2,"(" ,round(pca[pca[,1]==pc2,2],2),"%)",sep="")))
}

#######plot grid in the map
gridmap=function(data,gra,type=NULL,comp=NULL,drop=drop){
  chc=c("SAMPLE","SPECIES","SEX")
  gra=as.numeric(gra)
  data$LATITUDE=as.numeric(data$LATITUDE)
  data$LONGITUDE=as.numeric(data$LONGITUDE)
  if(min(data$LATITUDE)>=0){
    latgr=seq(0,max(data$LATITUDE),gra)
  }else{
    latgr=c(seq(floor(min(data$LATITUDE)),0,gra),seq(0,max(data$LATITUDE),gra))
  }
  if(min(data$LONGITUDE)>=0){
    loggr=seq(0,max(data$LONGITUDE),gra)
  }else{
    loggr=c(seq(floor(min(data$LONGITUDE)),0,gra),seq(0,ceiling(max(data$LONGITUDE)),gra))
  }
  
  latlog=expand.grid(lat=latgr,log=loggr)
  if(type=="SNP" & length(getsnpname(data))>0 & length(grep("ANC",names(data)))==0){
    latlogtab=cbind(latlog$lat+(gra/2),latlog$log+(gra/2),0,0,0,0,NA,NA,NA,NA)
    colnames(latlogtab)=c("LATITUDE","LONGITUDE","Count",names(data)[grep("SNP",names(data))],"NA","SAMPLE","SPECIES","SEX","AGE")
    #latlogtab=apply(latlogtab,1,function(i){
    for(i in 1:nrow(latlogtab)){
      ranks<-data[data$LONGITUDE>=latlog[i,2] & data$LONGITUDE<(latlog[i,2]+gra) & 
                    data$LATITUDE>=latlog[i,1] & data$LATITUDE<(latlog[i,1]+gra),]
      if(!is.na(ranks[1,1])){
        latlogtab[i,"Count"]=as.numeric(sum(ranks$Count,na.rm=T))
        latlogtab[i,names(data)[grep("SNP",names(data))][1]]=as.numeric(round(mean(ranks[,names(data)[grep("SNP",names(data))][1]],na.rm=T),2))
        latlogtab[i,names(data)[grep("SNP",names(data))][2]]=as.numeric(round(mean(ranks[,names(data)[grep("SNP",names(data))][2]],na.rm=T),2))
        if(any(names(ranks)%in%"NA")){
          latlogtab[i,"NA"]=as.numeric(round(mean(ranks[,"NA"],na.rm=T),2))
        }
        latlogtab[i,chc]=apply(ranks[,chc],2,function(x) paste(x, collapse=", "))
       latlogtab[i,"AGE"] =as.numeric(round(mean(ranks[,"AGE"],na.rm=T),2))#stringr::str_flatten(ranks$Age, collapse = ", ")
      }        
    }#)
    
    latlogtab=data.frame(latlogtab)
    latlogtab$SITE=paste("Site",seq(1,nrow(latlogtab)),sep="")
    latlogtab$SecondSite=paste("Site",seq(1,nrow(latlogtab)),sep="")
    latlogtab=latlogtab[latlogtab$Count>0,]
    latlogtab[,names(data)[grep("SNP",names(data))][1]]=as.numeric(latlogtab[,names(data)[grep("SNP",names(data))][1]])
    latlogtab[,names(data)[grep("SNP",names(data))][2]]=as.numeric(latlogtab[,names(data)[grep("SNP",names(data))][2]])
    latlogtab$LATITUDE=as.numeric(latlogtab$LATITUDE)
    latlogtab$LONGITUDE=as.numeric(latlogtab$LONGITUDE)
    latlogtab$AGE=as.numeric(latlogtab$AGE)
    latlogtab$Count=as.numeric(latlogtab$Count)
    as.data.frame(latlogtab)
  }else if(!is.null(type) & length(comp)>0){
    if(length(comp)<length(grep(type,names(data),value = T))){
      ancname=c(comp,"NA")
    }else{
      ancname=comp#
    }
    a=matrix(0,ncol=length(ancname)+1,nrow=nrow(latlog))
    b=matrix(NA,ncol=4,nrow=nrow(latlog))
    colnames(a)=c("Count",ancname)
    latlogtab=cbind(latlog$lat+(gra/2),latlog$log+(gra/2),a,b)
    colnames(latlogtab)=c("LATITUDE","LONGITUDE","Count",ancname,"SAMPLE","AGE","SEX","SPECIES")
    for(i in 1:nrow(latlogtab)){
      ranks<-data[data$LONGITUDE>=latlog[i,2] & data$LONGITUDE<(latlog[i,2]+gra) & 
                    data$LATITUDE>=latlog[i,1] & data$LATITUDE<(latlog[i,1]+gra),]
      
      if(nrow(ranks)>0 & length(comp)>0){
        latlogtab[i,"Count"]=sum(ranks$Count,na.rm=T)
        latlogtab[i,chc]=apply(ranks[,chc],2,function(x) paste(x, collapse=", "))
        latlogtab[i,"AGE"] =mean(ranks$AGE, na.rm=T)
        latlogtab[i,comp]=apply(as.data.frame(ranks[,comp]),2,function(x) round(mean(x),2))
        }        
    }
    latlogtab=as.data.frame(latlogtab,stringsAsFactors=FALSE)
    if(length(comp)<length(grep(type,names(data)))){
      latlogtab[,"NA"]=1-apply(as.data.frame(latlogtab[,comp]),1,function(x) sum(as.numeric(x)))
    }
    latlogtab$SITE=paste("Site",seq(1,nrow(latlogtab)),sep="")
    latlogtab$SecondSite=paste("Site",seq(1,nrow(latlogtab)),sep="")
    latlogtab$LATITUDE=as.numeric(latlogtab$LATITUDE)
    latlogtab$LONGITUDE=as.numeric(latlogtab$LONGITUDE)
    latlogtab$Count=as.numeric(latlogtab$Count)
    latlogtab$AGE=as.numeric(latlogtab$AGE)
    latlogtab=latlogtab[latlogtab$Count>0,]
    as.data.frame(latlogtab,stringsAsFactors=FALSE)
  }
}
###gridplot
gridplot=function(data,st,end,grindsize,type,comp=NULL,path){
  st=floor(st/10)*10
  end=ceiling(end/10)*10
  if(end-st>grindsize &grindsize>0){
  tim1=seq(st,end-grindsize,grindsize)
  tim2=seq(st+grindsize,end,grindsize)
  }else{
  tim1=st;tim2=end
  }
  timtab=as.data.frame(cbind(tim1,tim2))
  data=data[data$Count>0,]
  #####draw pie map
  maxValue <- 1
  #########drawmap################
  tilesURL <- "http://server.arcgisonline.com/ArcGIS/rest/services/Canvas/World_Light_Gray_Base/MapServer/tile/{z}/{y}/{x}"
  basemap <- leaflet(options = leafletOptions(zoomControl = FALSE, minZoom = 2, maxZoom = 5, dragging = T)) %>%
    addTiles(tilesURL) %>%
    fitBounds(min(data$LONGITUDE),min(data$LATITUDE),max(data$LONGITUDE),max(data$LATITUDE))
  foreach(i=1:nrow(timtab)) %do% {
     datamap=data[data$AGE>=as.numeric(timtab[i,1]) & data$AGE<as.numeric(timtab[i,2]),]
    if(type=="SNP" & !is.null(datamap) & nrow(datamap)>0 ){
      coltyp=c(brewer.pal(11, "RdYlGn")[c(1,9,3,5,6)])
      snpname=getsnpname(datamap)
      chartdata = cbind(datamap %>% select(starts_with(type)),datamap[,"NA"])
      colnames(chartdata)=c(grep("SNP",names(datamap),value = T),"NA")
    }else if(type!="SNP"& nrow(datamap)>0 &length(comp)==length(grep(type,names(datamap),value = T))){
      coltyp=c(brewer.pal(11, "Spectral")[c(1,5,11,3,9,2,8,4)])
      snpname=type
     # chartdata=datamap %>% select(starts_with(type))
      chartdata=datamap[,comp]
    }else if(type!="SNP"& nrow(datamap)>0 &length(comp)<length(grep(type,names(datamap),value = T))){
      chartdata=cbind(datamap[,comp],1-apply(as.data.frame(datamap[,comp]),1,sum))
      colnames(chartdata)=c(comp,"NA")
      snpname=type
     }else{snpname=NA}
    if(nrow(datamap)>0 &!is.na(snpname)){
      if(type%in%c("SNP","ANC")){
        coltyp=c(brewer.pal(11, "Spectral")[c(1,5,11,3,9,2,8,4)])
      }else{ 
        coltyp=c(get_color(rcolors$t2m_29lev, n = length(comp)+1))
     }
      
      piemap=basemap %>%
        addMinicharts(
          datamap$LONGITUDE, datamap$LATITUDE,
          chartdata =chartdata,
          maxValues = maxValue,
          type ="pie",
          colorPalette = coltyp,
          popup = popupArgs(
            labels = names(chartdata) ,
            html = paste0(
              "<div>","<h6>","<br>",datamap$SAMPLE,"<h6>",
              "<div>","<h7>","Sex: ",datamap$SEX,"<br>",
              "Species: ",datamap$SPECIES,"<br>",
              "Age: ",datamap$AGE,"<br>",
              "Count: ",datamap$Count,"</div>"
            )
          ),
          width = 5*sqrt(datamap$Count),transitionTime = 0
        ) %>%
        addLegendSize(position = "bottomleft",col="black",fillColor="white",baseSize=quantile(sqrt(datamap$Count),0.9),
                      values = datamap$Count,shape="circle",orientation="horizontal",breaks=5) %>%
        addLabelOnlyMarkers(min(data$LONGITUDE)+3,max(data$LATITUDE)+5,label=paste(timtab[i,2]," - ",timtab[i,1],"BP",sep=""),
                            labelOptions = labelOptions(noHide = T, direction = 'top', textOnly = T,style=list("font-size"="20px","color" = "darkblue","font-style"="bold","box-shadow"="3px 3px rgba(0,0,0,0.25)", "border-color" = "rgba(0,0,0,0.5)")))
      
      htmlwidgets::saveWidget(piemap,paste(path,"/",timtab[i,2],"-",timtab[i,1],"BP-",snpname,"-",paste(gsub(paste0(type,"_"),"",comp),collapse="-"),".html",sep=""))
    }else{
      htmlwidgets::saveWidget(basemap,paste(path,"/",timtab[i,2],"-",timtab[i,1],"BP-",snpname,"-",paste(gsub(paste0(type,"_"),"",comp),collapse="-"),".html",sep=""))
    }
  }
}
gridplotMap=function(data,st,end,path){
  if(nrow(data)>0){
    datamap=data[data$AGE>=st &data$AGE<=end,]
    tilesURL <- "http://server.arcgisonline.com/ArcGIS/rest/services/Canvas/World_Light_Gray_Base/MapServer/tile/{z}/{y}/{x}"
    basemap <- leaflet(options = leafletOptions(zoomControl = FALSE, minZoom = 2, maxZoom = 5, dragging = T)) %>%
      addTiles(tilesURL) %>%
      fitBounds(min(datamap$LONGITUDE),min(datamap$LATITUDE),max(datamap$LONGITUDE),max(datamap$LATITUDE))
    colorpal <- colorNumeric("Set1", datamap$AGE)
    sizes <- sizeNumeric((datamap$Count), baseSize = mean(datamap$Count))
    map=basemap %>%
      addCircleMarkers(datamap$LONGITUDE, datamap$LATITUDE,color = "#777777",radius=sqrt(sizes)*3#datamap$Count*1.5
                       ,weight=1,
                       fillColor = colorpal(datamap$AGE), fillOpacity = 0.9,popup= paste0(
                         "<div>","<h6>","<br>",datamap$SAMPLE,"<h6>",
                         "<h7>","Sex: ",datamap$SEX,"<br>",
                         "Species: ",datamap$SPECIES,"<br>",
                         "AGE: ",datamap$AGE,"<br>",
                         "Count: ",datamap$Count,"</div>")
      ) %>%
      addLegendSize(position = "bottomleft",col="black",fillColor="white",baseSize =min(sizeNumeric(datamap$Count, baseSize = mean(datamap$Count)*2.5)),
                    values = sizeNumeric((datamap$Count), baseSize = mean(datamap$Count)),shape="circle",orientation="horizontal",breaks=5) %>%
      addLegendNumeric(position = "bottomright",
                       pal = colorpal, values = datamap$AGE)
    
    saveWidget(map,paste(path,"/","Smap",st,"-",end,"-map.html",sep=""))
  }else{NULL}
}
###freqplot
samtimeSD=function(indata){
  if(nrow(indata)==0){
    matrix(0,ncol=3)
  }else if(!is.null(indata[1,1])){
    list=lapply(1:nrow(indata),function(i){
      point=indata[i,]
      #point[,"mean"]="";point[i,"sd"]=""
      if(point[2]>=1 & point[1]>=1  &  nrow(indata)>0){
        s=""
        #for(j in 1:100){s[j]=sample(c(rep(1,point[1]),rep(0,point[2]-point[1])),1)}
        s=sample(c(rep(1,point[1]),rep(0,point[2]-point[1])),100,replace = T)
        point$mean=mean(as.numeric(s),na.rm = T)
        point$sd=sd(as.numeric(s),na.rm = T)
      }else if(indata[i,2]==0 &  indata[i,1]>0){
        point$mean=1
        point$sd=0
      }else{
        point$mean=0
        point$sd=0
      }
      return(point)
    })
    indata1=do.call(rbind,list)
    matrix(c(apply(indata1[,3:4],2,mean),nrow(indata)),nrow=1)
  }
}

AllePlot=function(dat,sex,species,gt,windowsize,stepsize,st,end,snp,lat1,lat2,long1,long2,ymin,ymax,sampletime,gtp){
  d=dat[,c("SAMPLE","AGE","LATITUDE","LONGITUDE","SEX","SITE","SPECIES",names(dat)[grep(snp,names(dat))])]
  d$count=dplyr::if_else(rowSums(d[,names(d)[grep(snp,names(d))]])>0,1,0)
  d=d[d$count>0,]
  if(gtp=="ReadCounts"){
    d$CountReads=apply(d %>% select(matches(snp)),1,sum)
  }
  data=d[d$SEX%in%sex & d$SPECIES%in%species & d$AGE >=st & d$AGE <end & d$LATITUDE >= as.numeric(lat1) & d$LATITUDE <as.numeric(lat2) & d$LONGITUDE >= as.numeric(long1) & d$LONGITUDE < as.numeric(long2),]
  
  time1=seq(floor(st/100)*100,ceiling(end/100)*100-windowsize,stepsize)
  time2=seq(floor(st/100)*100+windowsize,ceiling(end/100)*100,stepsize)
  label=paste(time1,time2,sep="-")
  sort=factor(paste("Time",seq(01,length(label)),sep=""))
  timtab=NA
  timtab=as.data.frame(cbind(time1,time2,label,sort))
  timtab$mean=NA;timtab$sd=NA;timtab$count=NA
  a=NA;b=NA
  gtt=paste("SNP_",snp,"_",gt,sep="")
  if(gtp=="Genotype"){
    foreach(i=1:nrow(timtab)) %do% {
      a=data[data$AGE>=as.numeric(timtab[i,1]) & data$AGE<as.numeric(timtab[i,2]),gtt]
      timtab[i,5]=ifelse(length(a)==0,0,mean(a,na.rm=TRUE)/2)
      timtab[i,6]=ifelse(length(a)==0,0,1.96*sqrt(timtab[i,5]*(1-timtab[i,5])/length(a)/2))#sqrt(sd(a, na.rm = TRUE)/length(a))#
      timtab[i,7]=length(a)
    }
  }else if(gtp=="ReadCounts"){
    sampletime=as.numeric(sampletime)
    foreach(i=1:nrow(timtab)) %do% {
      a=data[as.numeric(data$AGE)>=as.numeric(timtab[i,1]) & as.numeric(data$AGE)<as.numeric(timtab[i,2]),c(gtt,"CountReads")]
      timtab[i,5:7]=rbind(samtimeSD(a))
    }}
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
###findlocation
findLocations <- function(shape, location_coordinates, location_id_colname){
  # derive polygon coordinates and feature_type from shape input
  polygon_coordinates <- shape$geometry$coordinates
  feature_type <- shape$properties$feature_type
  if(feature_type %in% c("rectangle","polygon")) {
    # transform into a spatial polygon
    drawn_polygon <- Polygon(do.call(rbind,lapply(polygon_coordinates[[1]],function(x){c(x[[1]][1],x[[2]][1])})))
    # use 'over' from the sp package to identify selected locations
    selected_locs <- sp::over(location_coordinates
                              , sp::SpatialPolygons(list(sp::Polygons(list(drawn_polygon),"drawn_polygon"))))
    
    # get location ids
    x = (location_coordinates[which(!is.na(selected_locs)), location_id_colname])
    selected_loc_id = as.character(x[[location_id_colname]])
    return(selected_loc_id)
  } else if (feature_type == "circle") {
    center_coords <- matrix(c(polygon_coordinates[[1]], polygon_coordinates[[2]])
                            , ncol = 2)
    
    dist_to_center <- spDistsN1(location_coordinates, center_coords, longlat=TRUE)
    # radius is in meters
    x <- location_coordinates[dist_to_center < shape$properties$radius/100000, location_id_colname]
    selected_loc_id = as.character(x[[location_id_colname]])
    return(selected_loc_id)
  }
}
##mergeMT
catsorting=function(data,type){
  sub=as.data.frame(table(data))
  sub2=matrix(as.numeric((sub[,2])),ncol=nrow(sub))
  colnames(sub2)=paste(type,"_",sub[,1],sep="")
  data.frame(sub2)
}
MergeMT=function(data,type=NULL,drop=FALSE){
  chc=c("SITE","SAMPLE","SPECIES","SEX");num=c("LATITUDE","LONGITUDE","AGE")
  cat=paste("CAT_",type,sep="")
  if(nrow(data)>0){
  d=data[,c(chc,cat,num)]
  d=d[d[,cat] !="unknown",]
  yname=sort(unique(d[,cat]))
  d$Count=rep(1,nrow(d))
  d2 <- sf::st_as_sf(d, coords = c("LONGITUDE", "LATITUDE"))
  d3 <- sf::st_intersection(d2)
  list <- lapply(1:length(d3$origins), function(x) {
    point <- d3[x, ]
    point[,chc]=apply((data.frame(d2[d3$origins[[x]], chc])[,chc]),2,function(x) paste(x, collapse=", "))
    point$AGE <- round(mean(d2[d3$origins[[x]], ]$AGE, na.rm = TRUE),2)
    point$Count <- sum(d2[d3$origins[[x]], ]$Count, na.rm = TRUE)
    ###character type
    catyname=paste(type,"_",yname,sep="")
    point[,catyname]=matrix(0,ncol=length(catyname),nrow=1)
    point[,names(catsorting(data.frame(d2[d3$origins[[x]], cat][,cat])[,1],type=type))]=catsorting(data.frame(d2[d3$origins[[x]], cat][,cat])[,1],type=type)
    return(point)
  })
  new_d <- do.call(rbind, list)
  new_d$LONGITUDE <- sf::st_coordinates(new_d)[, "X"]
  new_d$LATITUDE<- sf::st_coordinates(new_d)[, "Y"]
  sf::st_geometry(new_d) <- NULL
  as.data.frame(new_d)
  new_d[,cat]=NULL
  new_d$SecondSite=paste(as.character(new_d$SITE),"_layer",sep="")
  as.data.frame(new_d)
  }
}
##merge multiple snps
samtimeFr=function(indata){
  list=lapply(1:nrow(indata),function(i){
    point=indata[i,]
    point[,1:2]=if(point[2]>=1 & point[1]>=1){
      cbind(mean(sample(c(rep(1,point[1]),rep(0,point[2])),100,replace = T)),1-mean(sample(c(rep(1,point[1]),rep(0,point[2])),100,replace = T)))
      #s=""
      #for(j in 1:10){s[j]=sample(c(rep(1,point[1]),rep(0,point[2])),1)}
     # mean(as.numeric(s),na.rm = T)
    }else if(point[2]==0 &  point[1]>0){
      cbind(1,0)
    }else if(point[2]>0 &  point[1]==0){
      cbind(0,1)
    }else{
      cbind(0,0)
    }
  })
  indata=do.call(rbind,list)
  as.data.frame(indata)
}

MergeawsomeCount=function(data,mult=FALSE,snplist=NULL,drop=FALSE){
  charvec=c("SITE","SAMPLE","SPECIES","SEX")
  if(!is.null(data) & nrow(data)>0){ 
  snpname=sort(unique(stringi::stri_replace_first_regex(stringi::stri_replace_last_regex(grep("SNP_",snplist,value = T),pattern="_[A,C,G,T,D]",replacement = ""),"SNP_","")))
   snpnamefu=names(data %>% select(matches(snpname)))}
  #snpname=getsnpname(data)
  if(length(snpname)>0 & nrow(data)>0){
    data=data[apply(data[,snpnamefu],1,sum)>0,]
  d=data[,c(charvec,snpnamefu,"LATITUDE","LONGITUDE","AGE")]
    d$Counts=if_else(rowSums(d %>% select(matches(snpname)))>0,1,0)
    d=d[d$Counts>0,]
    #lapply parallel::mc
    list=lapply(1:length(snpname),function(i){
      point=d[,grep(snpname[i],names(d),value = T)]
      point[,grep(snpname[i],names(d),value = T)]=samtimeFr(point)
      return(point)
    })
    d[,snpnamefu]=do.call(cbind,list)
    #loop
    d2 <- st_as_sf(d, coords = c("LONGITUDE", "LATITUDE"))
    d3 <- st_intersection(d2)
      a=grep("SNP",names(d),value = T)
      list <- lapply(1:length(d3$origins), function(x) {
        point <- d3[x, ]
        point[,charvec]=apply((data.frame(d2[d3$origins[[x]], charvec])[,charvec]),2,function(x) paste(x, collapse=", "))
        point$AGE <- round(mean(d2[d3$origins[[x]], ]$AGE, na.rm = TRUE),2)
        point$Counts <- sum(d2[d3$origins[[x]], ]$Counts, na.rm = TRUE)
        point[,a]=apply(data.frame(d2[d3$origins[[x]], a])[,1:length(a)],2,mean)
        return(point)
      })
      new_d <- do.call(rbind, list)
      new_d$LONGITUDE <- st_coordinates(new_d)[, "X"]
      new_d$LATITUDE <- st_coordinates(new_d)[, "Y"]
      st_geometry(new_d) <- NULL
      as.data.frame(new_d)
      new_d$AGE=as.numeric(new_d$AGE)
      filter(new_d)
      as.data.frame(new_d)
  }
}
