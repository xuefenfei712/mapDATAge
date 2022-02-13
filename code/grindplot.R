gridplot=function(data,st,end,grindsize,snp,path){
 # data1=Mergesite(data)
  st=floor(st/10)*10
  end=ceiling(end/10)*10
  tim1=seq(st,end-grindsize,grindsize)
  tim2=seq(st+grindsize,end,grindsize)
  timetab=cbind(tim1,tim2)
  #dir.create(paste(path,"/",sep=""))
  for(i in 1:nrow(timetab)){
    datamap=data[data$Age>=as.numeric(timetab[i,1]) & data$Age<as.numeric(timetab[i,2]),]
    if(nrow(datamap)>0){
      #####draw pie map
  maxValue <- 1
  #########drawmap################
  tilesURL <- "http://server.arcgisonline.com/ArcGIS/rest/services/Canvas/World_Light_Gray_Base/MapServer/tile/{z}/{y}/{x}"
  basemap <- leaflet(options = leafletOptions(zoomControl = FALSE, minZoom = 2, maxZoom = 5, dragging = T)) %>%
    addTiles(tilesURL) %>%
    fitBounds(min(data$Longitude),min(data$Latitude),max(data$Longitude),max(data$Latitude))
  piemap=basemap %>%
    addMinicharts(
      datamap$Longitude, datamap$Latitude,
      chartdata =  datamap[,c(paste("SNP_",snp,"_A",sep=""),paste("SNP_",snp,"_D",sep=""))],
      maxValues = maxValue,
      type ="pie",
      colorPalette = brewer.pal(11, "RdYlGn")[c(1,11)],
      popup = popupArgs(
        labels = c("A", "D"),
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
   addLabelOnlyMarkers(min(data$Longitude)+3,max(data$Latitude)+5,label=paste(timetab[i,2]," - ",timetab[i,1],"BC",sep=""),
                       labelOptions = labelOptions(noHide = T, direction = 'top', textOnly = T,style=list("font-size"="20px","color" = "darkblue","font-style"="bold","box-shadow"="3px 3px rgba(0,0,0,0.25)", "border-color" = "rgba(0,0,0,0.5)")))
  
  saveWidget(piemap,paste(path,"/",timetab[i,2],"-",timetab[i,1],"BP-piemap.html",sep=""))
    }else{
     # return (NULL)
      saveWidget(basemap,paste(path,"/",timetab[i,2],"-",timetab[i,1],"BP-piemap.html",sep=""))
    }
  }
}
gridanceplot=function(data,st,end,grindsize,path){
  #data1=Mergesiteances(data)
  st=floor(st/10)*10
  end=ceiling(end/10)*10
  tim1=seq(st,end-grindsize,grindsize)
  tim2=seq(st+grindsize,end,grindsize)
  timetab=cbind(tim1,tim2)
  #dir.create(paste(path,"/",sep=""))
  #########drawmap################
      tilesURL <- "http://server.arcgisonline.com/ArcGIS/rest/services/Canvas/World_Light_Gray_Base/MapServer/tile/{z}/{y}/{x}"
      basemap <- leaflet(options = leafletOptions(zoomControl = FALSE, minZoom = 2, maxZoom = 5, dragging = T)) %>%
        addTiles(tilesURL) %>%
        fitBounds(min(data$Longitude),min(data$Latitude),max(data$Longitude),max(data$Latitude))
  for(i in 1:nrow(timetab)){
    datamap1=data[data$Age >= as.numeric(timetab[i,1]) & data$Age < as.numeric(timetab[i,2]),]
    if(nrow(datamap1)>0){
      maxValue <- 1
      ancescomp=names(datamap1)[grep("Anc",names(datamap1))]
      ancesmap=basemap %>%
        addMinicharts(
          datamap1$Longitude, datamap1$Latitude,
          chartdata =  datamap1[,ancescomp],
          maxValues = maxValue,
          type ="pie",legend=TRUE,
          colorPalette = brewer.pal(11, "Spectral")[c(2,5,11,1,9,3,8,4)],
          popup = popupArgs(
            labels = ancescomp,
            html = paste0(
              "<div>","<h6>","<br>",datamap1$Sample,"<h6>",
              "<div>","<h7>","Sex: ",datamap1$Sex,"<br>",
              "Species: ",datamap1$Species,"<br>",
              "Age: ",datamap1$Age,"<br>",
              "Count: ",datamap1$Count,"</div>"
            )
          ),
          width = 12*sqrt(datamap1$Count),transitionTime = 0
        ) %>%
        addLegendSize(position = "bottomleft",col="black",fillColor="white",
                      values = datamap1$Count,shape="circle",orientation="horizontal",breaks=4
        ) %>%
        addLabelOnlyMarkers(min(data$Longitude)+30,max(data$Latitude)-1,label=paste(timetab[i,2]," - ",timetab[i,1],"BP",sep=""),
                            labelOptions = labelOptions(noHide = T, direction = 'top', textOnly = T,style=list("font-size"="20px","color" = "darkblue","font-style"="bold","box-shadow"="3px 3px rgba(0,0,0,0.25)", "border-color" = "rgba(0,0,0,0.5)")))
      saveWidget(ancesmap,paste(path,"/",timetab[i,2],"-",timetab[i,1],"BP-ancemap.html",sep=""))
          }else{
     # return (NULL)
     saveWidget(basemap,paste(path,"/",timetab[i,2],"-",timetab[i,1],"BP-ancemap.html",sep=""))
    }
  }
}
  
####snp
gridsnpplot=function(data,st,end,grindsize){
  #data1=Mergesiteances(data)
  st=floor(st/10)*10
  end=ceiling(end/10)*10
  tim1=seq(st,end-grindsize,grindsize)
  tim2=seq(st+grindsize,end,grindsize)
  timetab=cbind(tim1,tim2)
  for(i in 1:nrow(timetab)){
    datamap2=data[data$Age >= as.numeric(timetab[i,1]) & data$Age < as.numeric(timetab[i,2]),]
    if(nrow(datamap2)>0){
      maxValue <- max(datamap2[,names(datamap2)[grep("SNP",names(datamap2))]])
      #########drawmap################
      tilesURL <- "http://server.arcgisonline.com/ArcGIS/rest/services/Canvas/World_Light_Gray_Base/MapServer/tile/{z}/{y}/{x}"
      basemap <- leaflet(options = leafletOptions(zoomControl = FALSE, minZoom = 2, maxZoom = 5, dragging = T)) %>%
        addTiles(tilesURL) %>%
        fitBounds(min(data$Longitude)-0.3,min(data$Latitude),max(data$Longitude),max(data$Latitude))
      
      snpcomp=names(datamap2)[grep("SNP",names(datamap2))]
      snpmap=basemap %>%
        addMinicharts(
          datamap2$Longitude, datamap2$Latitude,
          chartdata =  datamap2[,snpcomp],
          maxValues = maxValue,
          type ="pie",legend=TRUE,
          colorPalette = brewer.pal(11, "RdYlGn")[c(1,5,11,3,9,2,8,4)],
          popup = popupArgs(
            labels = snpcomp,
            html = paste0(
              "<div>","<h6>","<br>",datamap2$Sample,"<h6>",
              "<div>","<h7>","Sex: ",datamap2$Sex,"<br>",
              "Species: ",datamap2$Species,"<br>",
              "Age: ",datamap2$Age,"<br>",
              "Count: ",datamap2$Count,"</div>"
            )
          ),
          width = sqrt(datamap2$Count)*3,transitionTime = 0
        ) %>%
        addLegendSize(position = "bottomleft",col="black",fillColor="white",baseSize = 2,
                      values = datamap2$Count,shape="circle",orientation="horizontal",breaks=4
        ) %>%
        addLabelOnlyMarkers(min(data$Longitude)+3,max(data$Latitude)+5,label=paste("BP",timetab[i,1],"-",timetab[i,2],sep=""),
                            labelOptions = labelOptions(noHide = T, direction = 'top', textOnly = T,style=list("font-size"="20px","color" = "darkblue","font-style"="bold","box-shadow"="3px 3px rgba(0,0,0,0.25)", "border-color" = "rgba(0,0,0,0.5)")))
      saveWidget(snpmap,paste(timetab[i,1],"-",timetab[i,2],"BP-snpmap.html",sep=""))
    }else{
     # return (NULL)
      saveWidget(basemap,paste(timetab[i,1],"-",timetab[i,2],"BP-snpmap.html",sep=""))
    }
  }
}






