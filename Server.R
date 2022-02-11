options(digits = 5, shiny.maxRequestSize = 10 * 1024 ^ 2)
server <- function(input, output,session)({
   # Read in data files and validate 
  taxonomy_table <- reactive({
    req(input$in_taxon_table)
    if (grepl(input$in_taxon_table$datapath, pattern = ".txt") |
        grepl(input$in_taxon_table$datapath, pattern = ".tsv")) {
      read.table(input$in_taxon_table$datapath, header = T,
                 sep = "\t", stringsAsFactors = F,
                 quote = "", comment.char = "")#,fileEncoding = "UTF-16LE"
    }
  })
  
  output$fileStatus <- eventReactive(input$go, {
    if (is.null(validate_input_files(taxonomy_table()))) {
      paste("Congrats, no error detected!")
    }
  })
  ###############################draw map#################################
  ####################### Render UIs for Panel 3 (map)##################
  output$mapoutsp <- renderUI({
    req(taxonomy_table())
    selectInput("mapsp",
                label = "Select species",
                choices = unique(taxonomy_table()$Species),
                multiple = TRUE,selected=unique(taxonomy_table()$Species)[1])
  })
  output$mapoutsx <- renderUI({
    req(taxonomy_table())
    selectInput("mapsx",
                label = "Select sex",
                choices = unique(taxonomy_table()$Sex),
                multiple = TRUE,selected=unique(taxonomy_table()$Sex)[1])
  })
  output$mapout <- renderUI({
    req(taxonomy_table())
    sliderInput("mapage",
                label = "Select a time range",
                min = 0, #taxonomy_table()$Age %>%
                  #as.numeric() %>%
                 # min(na.rm = TRUE),
                max = taxonomy_table()$Age %>%
                  as.numeric %>%
                  max(na.rm = TRUE),
                step = 10,
                value = range(taxonomy_table()$Age))
  })
  output$mapoutlog <- renderUI({
    sliderInput("maplog",
                label = "Select a longitude range",
                min = taxonomy_table()$longitude %>%
                  as.numeric() %>%
                  round(2)%>%
                  min(na.rm = TRUE),
                max = taxonomy_table()$longitude %>%
                  as.numeric %>%
                  round(2)%>%
                  max(na.rm = TRUE),
                step = 0.1,
                value = range(taxonomy_table()$longitude))
  })
  output$mapoutlat <- renderUI({
    sliderInput("maplat",
                label = "Select a latitude range",
                min = taxonomy_table()$latitude %>%
                  as.numeric() %>%
                  round(2)%>%
                  min(na.rm = TRUE),
                max = taxonomy_table()$latitude %>%
                  as.numeric %>%
                  round(2)%>%
                  max(na.rm = TRUE),
                step = 1,
                value = range(taxonomy_table()$latitude))
  })
  
  ##basemap
  tilesURL <- "http://server.arcgisonline.com/ArcGIS/rest/services/Canvas/World_Light_Gray_Base/MapServer/tile/{z}/{y}/{x}"
  
  # Reactive expression for the data subsetted to what the user selected
  filteredData <- reactive({
    req(input$mapage)
    Mergeawsome(taxonomy_table()[taxonomy_table()$Sex%in%input$mapsx & taxonomy_table()$Species%in%input$mapsp & taxonomy_table()$Age >= as.numeric(input$mapage[1]) & taxonomy_table()$Age < as.numeric(input$mapage[2])
                                 & taxonomy_table()$latitude>=as.numeric(input$maplat[1]) & taxonomy_table()$latitude<=as.numeric(input$maplat[2]) & taxonomy_table()$longitude>=as.numeric(input$maplog[1]) & taxonomy_table()$longitude<=as.numeric(input$maplog[2]),])
  })
  output$mapoutlab <- renderUI({
    req(filteredData())
    selectInput("maplab",
                label = "Select labels",
                choices = names(filteredData())[names(filteredData())%in% c("n.overlaps","origins","SecondSite")==FALSE],
                multiple = TRUE,selected="Sample")
  })
  # render color
  colorpal <- reactive({
    req(filteredData()$Age)
    colorNumeric(input$colors,filteredData()$Age)
  })
  labelpop <- function(data,inlist){
    a=NULL;res=NULL
    for(i in 1:length(inlist)){
      if(is.null(data[,inlist[i]])){
        a=NA
      }else{
        a=paste0("<div>",paste(inlist[i],data[,inlist[i]], sep=": "),"<br>")
        res=paste0(res,a)
      }
    }
    res
  }
  #######header of map
  output$maphead <- renderText({
    paste("Geographic distribution of the samples ",sep=" ")
  })
  # list to store the selections for tracking
  data_of_click <- reactiveValues(clickedMarker = list())
  ##draw map
  output$drawmap <- renderLeaflet({
    req(filteredData())
    pal <- colorpal()
    leaflet(options = leafletOptions(zoomControl = FALSE, minZoom = 1, maxZoom = 5, dragging = T)) %>%
      addTiles(tilesURL) %>%
      fitBounds(min(filteredData()$longitude)-0.3,min(filteredData()$latitude)-0.3,max(filteredData()$longitude)+0.5,max(filteredData()$latitude)+0.5) %>%
      #addCircleMarkers(filteredData()$longitude, filteredData()$latitude,layerId = filteredData()$Site,radius=0.001,fillOpacity = 1,color = "#777777"#,radius=sqrt(filteredData()$Count)*2
      #,weight=1, popup = (labelpop(filteredData(),input$maplab))
      #  ) %>%
      addResetMapButton() %>% addMiniMap(width=100,height=100,toggleDisplay = TRUE,position = "topright")%>%
      addDrawToolbar(
        targetGroup='Selected',
        polylineOptions=FALSE,
        markerOptions = FALSE,
        polygonOptions = drawPolygonOptions(shapeOptions=drawShapeOptions(fillOpacity = 0,color = 'white',weight = 3)),
        rectangleOptions = drawRectangleOptions(shapeOptions=drawShapeOptions(fillOpacity = 0 ,color = 'white',weight = 3)),
        circleOptions = FALSE,circleMarkerOptions = FALSE,
        editOptions = editToolbarOptions(edit = FALSE, selectedPathOptions = selectedPathOptions())) 
  })
  # reactive to the map
  observe({
    pal <- colorpal()
    leafletProxy("drawmap", data = filteredData()) %>%
      clearShapes() %>% clearMarkers() %>%
      addCircleMarkers(filteredData()$longitude, filteredData()$latitude,color = "#777777",radius=sqrt(filteredData()$Count)*2#mean(filteredData()$Count)))#~sqrt(Count)
                       ,weight=1,fillColor = ~pal(Age), fillOpacity = 0.9, popup = (labelpop(filteredData(),input$maplab)),layerId = filteredData()$Site
      )
  })
  
  ###add a size legend
  observe({
    proxy <- leafletProxy("drawmap", data = filteredData())
    proxy %>% clearControls()
    pal <- colorpal()
    proxy %>% addLegendSize(position = "bottomleft",col="black",fillColor="white",baseSize=min(sizeNumeric(filteredData()$Count, baseSize = mean(filteredData()$Count)*2.5))#, baseSize = mean(filteredData()$Count)))*2#quantile(sqrt(filteredData()$Count),0.55),
                            ,values = sizeNumeric((filteredData()$Count), baseSize = mean(filteredData()$Count)),shape="circle",orientation="horizontal",breaks=5) %>%
      addLegendNumeric(position = "bottomright",
                       pal = pal, values = filteredData()$Age) #%>%
    #addLegendCustom(colors=rep("white", 5), labels=c(1,(seq(0,floor(max(filteredData()$Count)/10)*10,length=5)[-1])),sizes=c(1,seq(0,ceiling(max(sqrt(filteredData()$Count))/10)*10,length=5)[-1])*2, 
    #     shapes=rep("circle", 5), borders=rep("black", 5))
  })
  ############################################### click response ##################################################
  observeEvent(input$drawmap_draw_new_feature,{
    req(filteredData())
    #Only add new layers for bounded locations
    found_in_bounds <- findLocations(shape = input$drawmap_draw_new_feature
                                     , location_coordinates = SpatialPointsDataFrame(filteredData()[,c('longitude', 'latitude')] , filteredData())
                                     , location_id_colname = "Site")
    for(id in found_in_bounds){
      if(id %in% data_of_click$clickedMarker){
        # don't add id
      } else {
        # add id
        data_of_click$clickedMarker<-append(data_of_click$clickedMarker, id, 0)
      }
    }
    # look up airports by ids found
    selected <- subset(filteredData(), Site %in% data_of_click$clickedMarker)
    ######age count plot
    output$drawmapCount <- renderPlotly({
      req(selected)
      ggplot(selected,aes(x=Age,y=Count))+geom_point()+ggtitle("Temporal distribution of the samples")+theme(plot.title = element_text(size=15,family="Arial",face="plain"),
                                                                                                             axis.title= element_text(size=13,family="Arial",face="plain"),plot.margin=unit(c(1,0.3,0.5,0.3),'cm'))
    })
    leafletProxy("drawmap",data=selected)%>% 
      addCircleMarkers(lng=selected$longitude,lat=selected$latitude,
                       color = "red",radius=sqrt(selected$Count)*1.2,weight=1,
                       fillOpacity = 0.6,            
                       layerId = (selected$SecondSite))
  })
  ############################################### click response ##################################################
  observeEvent(input$drawmap_draw_deleted_features,{
    # loop through list of one or more deleted features/ polygons
    for(feature in input$drawmap_draw_deleted_features$features){
      
      # get ids for locations within the bounding shape
      bounded_layer_ids <- findLocations(shape = feature
                                         , location_coordinates = SpatialPointsDataFrame(filteredData()[,c('longitude', 'latitude')] , filteredData())
                                         , location_id_colname = "SecondSite")
      output$drawmapCount <- renderPlotly({
        return(NULL)
      })
      # remove second layer representing selected locations
      proxy <- leafletProxy("drawmap")
      proxy %>% removeMarker(layerId = as.character(bounded_layer_ids))
      first_layer_ids <- subset(filteredData(), SecondSite %in% bounded_layer_ids)$Site
      data_of_click$clickedMarker <- data_of_click$clickedMarker[!data_of_click$clickedMarker
                                                                 %in% first_layer_ids]
    }
  })
  ############################pie map############################################  
  ################ Render UIs for Panel 4 (pie)#################################
  output$pieoutsp <- renderUI({
    req(taxonomy_table())
    selectInput("piesp",
                label = "Select species",
                choices = unique(taxonomy_table()$Species),
                multiple = TRUE,selected=unique(taxonomy_table()$Species)[1])
  })
  output$pieoutsx <- renderUI({
    req(taxonomy_table())
    selectInput("piesx",
                label = "Select sex",
                choices = unique(taxonomy_table()$Sex),
                multiple = TRUE,selected=unique(taxonomy_table()$Sex)[1])
  })
  output$pieout <- renderUI({
    req(taxonomy_table())
    sliderInput("pieage",
                label = "Select a time range",
                min = 0,#taxonomy_table()$Age %>%
                  #as.numeric() %>%
                 # min(na.rm = TRUE),
                max = taxonomy_table()$Age %>%
                  as.numeric %>%
                  max(na.rm = TRUE),
                step = 10,
                value = range(taxonomy_table()$Age))
  })
  
  output$pieoutsnp <- renderUI({
    req(taxonomy_table())
    selectInput("piesnp",
                label = "Select your related variations",
                choices = getsnpname(taxonomy_table()),
                multiple = FALSE)
  })
  output$errpie <- renderText({
    if(length(select(taxonomy_table(),starts_with("SNP_")))==0){
      validate("The required data were not part of your Input File. Please make sure that columns labelled 'SNP_snpname_A' and 'SNP_snpname_D' are provided")
    }
  })
  
  output$pieoutlog <- renderUI({
    req(taxonomy_table())
    sliderInput("pielog",
                label = "Select a longitude range",
                min = taxonomy_table()$longitude %>%
                  as.numeric() %>%
                  round(2) %>%
                  min(na.rm = TRUE),
                max = taxonomy_table()$longitude %>%
                  as.numeric %>%
                  #round(2) %>%
                  max(na.rm = TRUE),
                step = 0.1,
                value = range(taxonomy_table()$longitude))
  })
  output$pieoutlat <- renderUI({
    req(taxonomy_table())
    sliderInput("pielat",
                label = "Select a latitude range",
                min = taxonomy_table()$latitude %>%
                  as.numeric() %>%
                  round(2) %>%
                  min(na.rm = TRUE),
                max = taxonomy_table()$latitude %>%
                  as.numeric %>%
                  round(2) %>%
                  max(na.rm = TRUE),
                step = 1,
                value = range(taxonomy_table()$latitude))
  })
  output$pieout1 <- renderUI({
    req(taxonomy_table())
    numericInput("GridSize","Time interval (years)",1000,min=100,max=10000)
  })
  output$pieout2 <- renderUI({
    req(taxonomy_table())
    actionButton("Down", "Drawpictures!")
  })
  
  ##active data
  filteredData1 <- reactive({
    req(input$pieage,taxonomy_table())
    if(length(select(taxonomy_table(),starts_with("SNP_")))>0 & input$pietype=="ReadCounts"){
      (MergeawsomeCount(taxonomy_table()[taxonomy_table()$Sex%in%input$piesx & taxonomy_table()$Species%in%input$piesp & taxonomy_table()$Age >= as.numeric(input$pieage[1]) & taxonomy_table()$Age < as.numeric(input$pieage[2]) &
                                      taxonomy_table()$latitude>=input$pielat[1] & taxonomy_table()$latitude<=input$pielat[2] & taxonomy_table()$longitude>=input$pielog[1] & taxonomy_table()$longitude<=input$pielog[2],],snp=input$piesnp))
    } else if(length(select(taxonomy_table(),starts_with("SNP_")))>0 & input$pietype=="Genotype"){
	(Mergeawsome(taxonomy_table()[taxonomy_table()$Sex%in%input$piesx & taxonomy_table()$Species%in%input$piesp & taxonomy_table()$Age >= as.numeric(input$pieage[1]) & taxonomy_table()$Age < as.numeric(input$pieage[2]) &
                                      taxonomy_table()$latitude>=input$pielat[1] & taxonomy_table()$latitude<=input$pielat[2] & taxonomy_table()$longitude>=input$pielog[1] & taxonomy_table()$longitude<=input$pielog[2],],snp=input$piesnp))
     }
  })
  ############labels for shown#####
  output$pieoutlab <- renderUI({
    req(filteredData1())
    if(length(select(taxonomy_table(),starts_with("SNP_")))>0){
      selectInput("pielab",
                  label = "Select labels",
                  choices = names(filteredData1())[names(filteredData1())%in% c("n.overlaps","origins","SecondSite")==FALSE],
                  multiple = TRUE,selected="Sample")
    }
  }) 
  #######header of map
  output$piehead <- renderText({
    if(length(select(taxonomy_table(),starts_with("SNP_")))>0){
      paste("Allele distribution on map",sep=" ")
    }
  })
  #########################output map#########################
  output$pie <- renderLeaflet({
    if(length(select(taxonomy_table(),starts_with("SNP_")))>0){
      req(filteredData1())
      leaflet(options = leafletOptions(zoomControl = FALSE, minZoom = 1, maxZoom = 5, dragging = T)) %>%
        addTiles(tilesURL) %>% #addControl(title, position = "topleft") %>%
        fitBounds(min(filteredData1()$longitude)-0.3,min(filteredData1()$latitude)-0.3,max(filteredData1()$longitude)+0.3,max(filteredData1()$latitude)+0.3)%>%
        addMinicharts(
          filteredData1()$longitude, filteredData1()$latitude,
          layerId = as.character(filteredData1()$Site),width=4,height=4) %>% addResetMapButton() %>% addMiniMap(width=100,height=100,toggleDisplay = TRUE,position = "topright")%>%
        addDrawToolbar(
          targetGroup='Selected',
          polylineOptions=FALSE,
          markerOptions = FALSE,
          polygonOptions = drawPolygonOptions(shapeOptions=drawShapeOptions(fillOpacity = 0,color = 'white',weight = 3)),
          rectangleOptions = drawRectangleOptions(shapeOptions=drawShapeOptions(fillOpacity = 0 ,color = 'white',weight = 3)),
          circleOptions = FALSE,circleMarkerOptions = FALSE,
          editOptions = editToolbarOptions(edit = FALSE, selectedPathOptions = selectedPathOptions()))# %>%addControl(title, position = "topleft") 
    }
  })
  maxValue <- 1
  
  observe({
    req(filteredData1())
    if(input$type=="bar"){
      leafletProxy("pie",data=filteredData1()) %>%
        clearMarkers() %>% clearControls() %>%
        updateMinicharts(
          layerId = filteredData1()$Site,
          chartdata =  cbind(filteredData1() %>% select(matches(input$piesnp))),#cbind(filteredData1()$A,filteredData1()$D),
          maxValues = maxValue,width=15,height=15,#legend=TRUE,
          type =input$type,showLabels = input$labels,
          colorPalette = brewer.pal(11, input$colors1)[c(1,9,3,5)],
          popup = popupArgs(
            labels = getallename(filteredData1(),input$piesnp),#c("A", "D"),
            html = labelpop(filteredData1(),input$pielab
            )
          )
        )
    }else if(input$type=="pie" & length(select(taxonomy_table(),starts_with("SNP_")))>0){
      leafletProxy("pie",data=filteredData1()) %>%
        clearMarkers() %>% clearControls() %>%
        updateMinicharts(
          layerId = filteredData1()$Site,
          chartdata =  cbind(filteredData1() %>% select(matches(input$piesnp))),#cbind(filteredData1()$A,filteredData1()$D),
          maxValues = maxValue,
          type =input$type,showLabels = input$labels,
          colorPalette = brewer.pal(11, input$colors1)[c(1,9,3,5)],
          popup = popupArgs(
            labels = getallename(filteredData1(),input$piesnp),#c("A", "D"),
            html = labelpop(filteredData1(),input$pielab
            )
          ),
          width = sqrt(filteredData1()$Count)*5,transitionTime = 0
        ) %>% addLegendSize(position = "bottomleft",col="black",fillColor="white",baseSize=quantile(sqrt(filteredData1()$Count),0.85),
                            values = filteredData1()$Count,shape="circle",orientation="horizontal",breaks=5)
    } else if(length(select(taxonomy_table(),starts_with("SNP_")))>0){ 
      leafletProxy("pie",data=filteredData1()) %>%
        clearMarkers() %>% #clearControls() %>%
        updateMinicharts(
          layerId = filteredData1()$Site,
          chartdata =  cbind(filteredData1() %>% select(matches(input$piesnp))),#cbind(filteredData1()$A,filteredData1()$D),
          maxValues = maxValue,
          type =input$type,showLabels = input$labels,
          colorPalette = brewer.pal(11, input$colors1)[c(2,9,4,5)],
          popup = popupArgs(
            labels = getallename(filteredData1(),input$piesnp),#c("A", "D"),
            html = labelpop(filteredData1(),input$pielab
            )
          ),
          width = 5*sqrt(filteredData1()$Count),transitionTime = 0
        ) 
    }   
  })
  ########addgrid map and merge###############
  griddata<- reactive({
    req(filteredData1())
    req(input$Gridpie)
    gridmap(filteredData1(),input$Gridpie,type="snp")
  })
  observe({
    if(input$Gridpie>0){
      leafletProxy("pie",data=griddata()) %>%
        clearMarkers() %>% clearControls() %>%
        removeMinicharts(layerId = filteredData1()$Site) %>%
        addMinicharts(as.numeric(griddata()$longitude), as.numeric(griddata()$latitude),layerId = as.character(griddata()$Site),
                      chartdata= griddata() %>% select(matches(input$piesnp)),#cbind(as.numeric(griddata()$A),as.numeric(griddata()$D)),
					  type="pie",
                      colorPalette = brewer.pal(11, input$colors1)[c(2,9,4,5)],
                      popup = popupArgs(
                        labels = getallename(griddata(),input$piesnp),#c("A", "D"),
                        html = labelpop(griddata(),input$pielab
                        )),
                      width = 5*sqrt(as.numeric(griddata()$Count)))%>%
        addGraticule(interval = input$Gridpie, style = list(color = "#FF0000", weight = 0.1,fillOpacity=0.1))%>% 
        addLegendSize(position = "bottomleft",col="black",fillColor="white",baseSize=quantile(sqrt(as.numeric(griddata()$Count)),0.85),
                      values = as.numeric(griddata()$Count),shape="circle",orientation="horizontal",breaks=5)
    }
  })
  
  ############################################### click response ##################################################
  observeEvent(input$pie_draw_new_feature,{
    req(filteredData1())
	if(input$Gridpie==0){
    #Only add new layers for bounded locations
    found_in_bounds <- findLocations(shape = input$pie_draw_new_feature
                                     , location_coordinates = SpatialPointsDataFrame(filteredData1()[,c('longitude', 'latitude')] , filteredData1())
                                     , location_id_colname = "Site")
    for(id in found_in_bounds){
      if(id %in% data_of_click$clickedMarker){
        # don't add id
      } else {
        # add id
        data_of_click$clickedMarker<-append(data_of_click$clickedMarker, id, 0)
      }
    }
    # look up airports by ids found
    selected <- subset(filteredData1(), Site %in% data_of_click$clickedMarker)
    ######age count plot
    output$pieCount <- renderPlotly({
      req(selected)
      ggplot(selected,aes(x=Age,y=Count))+geom_point()+ggtitle("Temporal distribution of the samples")+theme(
        plot.title = element_text(size=15, family="Arial"),plot.margin=unit(c(1,0.3,0.5,0.3),'cm'))
    })
    leafletProxy("pie",data=selected)%>% 
      addCircleMarkers(lng=selected$longitude,lat=selected$latitude,
                       color = "red",radius=sqrt(selected$Count)*3,weight=1,
                       fillOpacity = 0.6,            
                       layerId = (selected$SecondSite))
  }
  })
  observeEvent(input$pie_draw_new_feature,{
  #req(griddata())
  if(as.numeric(input$Gridpie>0)){
    #Only add new layers for bounded locations
    found_in_bounds1 <- findLocations(shape = input$pie_draw_new_feature
                                     , location_coordinates = SpatialPointsDataFrame(griddata()[,c('longitude', 'latitude')] , griddata())
                                     , location_id_colname = "Site")
    for(id in found_in_bounds1){
      if(id %in% data_of_click$clickedMarker){
        # don't add id
     } else {
        # add id
        data_of_click$clickedMarker<-append(data_of_click$clickedMarker, id, 0)
      }
    }
    # look up airports by ids found
    selected <- subset(griddata(), Site %in% data_of_click$clickedMarker)
    ######age count plot
    output$pieCount <- renderPlotly({
   #   req(selected)
      ggplot(selected,aes(x=Age,y=Count))+geom_point()+ggtitle("Temporal distribution of the samples")+theme(
        plot.title = element_text(size=15, family="Arial"),plot.margin=unit(c(1,0.3,0.5,0.3),'cm'))
    })
    leafletProxy("pie",data=selected)%>% 
      addCircleMarkers(lng=selected$longitude,lat=selected$latitude,
                       color = "red",radius=sqrt(selected$Count)*3,weight=1,
                       fillOpacity = 0.6,            
                       layerId = (selected$SecondSite))
  }
  })
  ############################################### click response ##################################################
  observeEvent(input$pie_draw_deleted_features,{
	if(input$Gridpie==0){
    # loop through list of one or more deleted features/ polygons
    for(feature in input$pie_draw_deleted_features$features){
      
      # get ids for locations within the bounding shape
      bounded_layer_ids <- findLocations(shape = feature
                                         , location_coordinates = SpatialPointsDataFrame(filteredData1()[,c('longitude', 'latitude')] , filteredData1())
                                         , location_id_colname = "SecondSite")
      output$pieCount <- renderPlotly({
        return(NULL)
      })
      # remove second layer representing selected locations
      proxy <- leafletProxy("pie")
      proxy %>% removeMarker(layerId = as.character(bounded_layer_ids))
      first_layer_ids <- subset(filteredData1(), SecondSite %in% bounded_layer_ids)$Site
      
      data_of_click$clickedMarker <- data_of_click$clickedMarker[!data_of_click$clickedMarker
                                                                 %in% first_layer_ids]
    }
	}
})
 observeEvent(input$pie_draw_deleted_features,{	
  if(as.numeric(input$Gridpie>0)){
    # loop through list of one or more deleted features/ polygons
    for(feature in input$pie_draw_deleted_features$features){
      
      # get ids for locations within the bounding shape
      bounded_layer_ids <- findLocations(shape = feature
                                         , location_coordinates = SpatialPointsDataFrame(griddata()[,c('longitude', 'latitude')] , griddata())
                                         , location_id_colname = "SecondSite")
      output$pieCount <- renderPlotly({
        return(NULL)
      })
      # remove second layer representing selected locations
      proxy <- leafletProxy("pie")
      proxy %>% removeMarker(layerId = as.character(bounded_layer_ids))
      first_layer_ids <- subset(griddata(), SecondSite %in% bounded_layer_ids)$Site
      
      data_of_click$clickedMarker <- data_of_click$clickedMarker[!data_of_click$clickedMarker
                                                                 %in% first_layer_ids]
    }
	}
  })
  ###########################select output folder################################
  Theroots <- reactive({
    root <- input$root
    req(root, dir.exists(root))
    
    if(length(root) == 0 || root == ""){
      volumes <- getVolumes()()
      c(volumes)
    } else{
      c(project_root = root)
    }
  })
  
  Thesheets_dir <- reactive({
    shinyDirChoose(input, 'sheets_dir', roots = Theroots(), session = session)
    parseDirPath(roots = Theroots(), input$sheets_dir)
  })
  
  output$sheets_dir <- renderPrint({
    Thesheets_dir()
  }) 
  #######################down grid figures######
  observeEvent(input$Down,{
    req(filteredData1())
    outpath=Thesheets_dir()
    gridplot(filteredData1(),as.numeric(input$pieage[1]),as.numeric(input$pieage[2]),input$GridSize,input$piesnp,outpath)
  })
  
  ###################draw allele##########################  
  ########## Render UIs for Panel 5 (allele)##############
  output$alloutsp <- renderUI({
    req(taxonomy_table())
    selectInput("allsp",
                label = "Select species",
                choices = unique(taxonomy_table()$Species),
                multiple = TRUE,selected=unique(taxonomy_table()$Species)[1])
  })
  output$alloutsx <- renderUI({
    req(taxonomy_table())
    selectInput("allsx",
                label = "Select sex",
                choices = unique(taxonomy_table()$Sex),
                multiple = TRUE,selected=unique(taxonomy_table()$Sex)[1])
  })
  output$allout <- renderUI({
    sliderInput("alleage",
                label = "Select a time range",
                min = 0,#taxonomy_table()$Age %>%
                  #as.numeric() %>%
                  #min(na.rm = TRUE),
                max = taxonomy_table()$Age %>%
                  as.numeric %>%
                  max(na.rm = TRUE),
                step = 10,
                value = range(taxonomy_table()$Age))
  })
  output$allsnp <- renderUI({
    selectInput("allsnp",
                label = "Select a variation",
                choices = getsnpname(taxonomy_table()),
				#selected= getsnpname(taxonomy_table())[1],
                multiple = FALSE)
  })
  output$allall <- renderUI({
  req(input$allsnp)
  if(!is.null(input$allsnp)){
    selectInput("allall",
                label = "Select allele",
                choices = c(Choose='',getallename(taxonomy_table(),input$allsnp)),
				selectize=FALSE,#getallename(taxonomy_table(),input$allsnp)[1],
                selected='')
				}
  })
  output$alloutlog <- renderUI({
    sliderInput("alllog",
                label = "Select a longitude range",
                min = taxonomy_table()$longitude %>%
                  as.numeric() %>% 
                  round(2) %>%
                  min(na.rm = TRUE),
                max = taxonomy_table()$longitude %>%
                  as.numeric %>% 
                  round(2) %>%
                  max(na.rm = TRUE),
                step = 0.1,
                value = range(taxonomy_table()$longitude))
  })
  output$alloutlat <- renderUI({
    sliderInput("alllat",
                label = "Select a latitude range",
                min = taxonomy_table()$latitude %>%
                  as.numeric() %>% 
                  round(2) %>%
                  min(na.rm = TRUE),
                max = taxonomy_table()$latitude %>%
                  as.numeric %>%
                  round(2) %>%
                  max(na.rm = TRUE),
                step = 1,
                value = range(taxonomy_table()$latitude))
  })
  output$alloutymin <- renderUI({
    numericInput("allymin",
                 label = "Input the min y",
                 min = -1,
                 max = 1,value=-0.5
    )
  })
  output$alloutymax <- renderUI({
    numericInput("allymax",
                 label = "Input the max y",
                 min = -1,
                 max = 2,value=1.5
    )
  })
  output$allsampling <- renderUI({
  if(input$alltype=="ReadCounts"){
    numericInput("allsampling",
                 label = "Sampling number",
                 min = 100,
                 max = 10000,value=100)
	}
  })
  
  output$alle <- renderPlotly({
    req(input$alleage)
	req(input$allsnp)
	req(input$allall)
	req(input$allsampling)
    if(length(select(taxonomy_table(),starts_with("SNP_")))>0 & input$alltype=="ReadCounts"){
      AllePlotCounts(taxonomy_table(),as.character(input$allsx),as.character(input$allsp),input$allall,as.numeric(input$WinSize),as.numeric(input$StepSize),as.numeric(input$alleage[1]),as.numeric(input$alleage[2]),input$allsnp,
               input$alllat[1],input$alllat[2],input$alllog[1],input$alllog[2],input$allymin,input$allymax,input$allsampling)
    }else if(length(select(taxonomy_table(),starts_with("SNP_")))>0 & input$alltype=="Genotype")
	 AllePlot(taxonomy_table(),as.character(input$allsx),as.character(input$allsp),input$allall,as.numeric(input$WinSize),as.numeric(input$StepSize),as.numeric(input$alleage[1]),as.numeric(input$alleage[2]),input$allsnp,
               input$alllat[1],input$alllat[2],input$alllog[1],input$alllog[2],input$allymin,input$allymax)

  })
  output$errtraj <- renderText({
    if(length(select(taxonomy_table(),starts_with("SNP_")))==0){
      validate("The required data were not part of your Input File. Please make sure that columns labelled 'SNP_snpname_A' and 'SNP_snpname_D' are provided")
    }
  })
  #head
  output$trajhead <- renderText({
    if(length(select(taxonomy_table(),starts_with("SNP_")))>0){
      paste("Allele distribution",sep=" ")
    }
  })

  ############active data
  filteredData3 <- reactive({
    req(input$alleage)
	req(input$allsnp)
	req(input$allall)
    if(length(select(taxonomy_table(),starts_with("SNP_")))>0 & length(input$allall)==1){
      allname=paste("SNP_",input$allsnp,"_",input$allall,sep="")
	   addreadCounts(taxonomy_table()[taxonomy_table()$Sex%in%input$allsx & taxonomy_table()$Species%in%input$allsp & taxonomy_table()$Age >= as.numeric(input$alleage[1]) & taxonomy_table()$Age < as.numeric(input$alleage[2]) & taxonomy_table()[,allname]>0 &
                          taxonomy_table()$latitude >= as.numeric(input$alllat[1]) & taxonomy_table()$latitude <= input$alllat[2] & taxonomy_table()$longitude>=as.numeric(input$alllog[1]) & taxonomy_table()$longitude<=as.numeric(input$alllog[2]),],input$allsnp)
    }
  })
  #colorpal2 <- reactive({
    #colorNumeric(input$colors, filteredData3()$Age)
  #})
  ##########draw map
  output$deallele <- renderLeaflet({
    if(length(select(taxonomy_table(),starts_with("SNP_")))>0){
      req(filteredData3())
      leaflet(options = leafletOptions(zoomControl = FALSE, minZoom = 1, maxZoom = 5, dragging = T)) %>%
        addTiles(tilesURL) %>%#addControl(titletraj, position = "topleft") %>%
        fitBounds(min(taxonomy_table()$longitude)-0.3,min(taxonomy_table()$latitude)-0.3,max(taxonomy_table()$longitude)+0.3,max(taxonomy_table()$latitude)+0.3) %>%
        addCircleMarkers(filteredData3()$longitude, filteredData3()$latitude,radius=filteredData3()[,paste("SNP_",input$allsnp,"_",input$allall,sep="")]) %>% addResetMapButton()%>% addMiniMap(width=100,height=100,toggleDisplay = TRUE,position = "topright")
    }  
  })
  observe({ 
    if(length(select(taxonomy_table(),starts_with("SNP_")))>0){
      req(filteredData3())
      #pal <- colorpal2()
      leafletProxy("deallele",data=filteredData3()) %>%
        clearMarkers() %>% clearShapes() %>% 
        addAwesomeMarkers(filteredData3()$longitude, filteredData3()$latitude,clusterOptions = markerClusterOptions(),#radius=~as.numeric(D)*5,color = "#777777",fillColor=~pal(Age),fillOpacity = 0.9, 
                          popup = paste0(filteredData3()$Sample,"<br>","Sex: ",filteredData3()$Sex,"<br>","Age: ",filteredData3()$Age,"<br>","Rs/Total:",round(filteredData3()[,paste("SNP_",input$allsnp,"_",input$allall,sep="")],2),"/",filteredData3()$ReadCount)
        )  
    }
  })
  
  ##############print table#################
  output$print_taxon_table <- DT::renderDataTable({
    req(taxonomy_table())
    table <- taxonomy_table() 
    DT::datatable(table, options = list(scrollX = TRUE))
  })
  
  ################################ancestral piemap########################
  ###################render UI for ances map#####################
  # Read in data files and validate 
  output$ancesoutsp <- renderUI({
    req(taxonomy_table())
    selectInput("ancessp",
                label = "Select species",
                choices = unique(taxonomy_table()$Species),
                multiple = TRUE,selected=unique(taxonomy_table()$Species)[1])
  })
  output$ancesoutsx <- renderUI({
    req(taxonomy_table())
    selectInput("ancessx",
                label = "Select sex",
                choices = unique(taxonomy_table()$Sex),
                multiple = TRUE,selected=unique(taxonomy_table()$Sex)[1])
  })
  output$ancesout <- renderUI({
    sliderInput("anceage",
                label = "Select a time span",
                min =0, #taxonomy_table()$Age %>%
                #as.numeric() %>%
                # min(na.rm = TRUE),
                max = taxonomy_table()$Age %>%
                  as.numeric %>%
                  max(na.rm = TRUE),
                step = 10,
                value = range(taxonomy_table()$Age))
  })
  output$ancesout1 <- renderUI({
    if(length(grep("Anc",names(taxonomy_table())))>0){
      selectInput("ancecomp",
                  label = "Select ancestral origins",
                  choices = names(taxonomy_table())[grep("Anc",names(taxonomy_table()))],
                  multiple = TRUE,selected="Anc1")
    }
  })
  output$ancesoutcut <- renderUI({
    sliderInput("ancecut",
                label = "Time Split",
                min = taxonomy_table()$Age %>%
                  as.numeric() %>%
                  min(na.rm = TRUE),
                max = taxonomy_table()$Age %>%
                  as.numeric %>%
                  max(na.rm = TRUE),
                value=median(taxonomy_table()$Age)%>%
                  as.numeric() 
    )
  })
  
  output$errance <- renderText({
    if(length(grep("Anc",names(taxonomy_table())))==0){
      validate("The required data were not part of your Input File. Please make sure that columns labelled 'Anc' are provided")
    }
  })
  #############active data
  filteredData2T <- reactive({
    req(input$anceage)
    req(input$ancecut)
    if(length(grep("Anc",names(taxonomy_table())))>0){
      Mergeawsome(taxonomy_table()[taxonomy_table()$Sex%in%input$ancessx & taxonomy_table()$Species%in%input$ancessp & taxonomy_table()$Age >= as.numeric(input$anceage[1]) & taxonomy_table()$Age < as.numeric(input$anceage[2]),],type="Anc")
    }
  })
  output$ancesoutlab <- renderUI({
    req(filteredData2T())
    selectInput("anceslab",
                label = "Select labels",
                choices = names(filteredData2T())[names(filteredData2T())%in% c("n.overlaps","origins","SecondSite")==FALSE],
                multiple = TRUE,selected="Sample")
  })
  filteredData2 <- reactive({
    req(input$anceage)
    req(input$ancecut)
    if(length(grep("Anc",names(taxonomy_table())))>0){
      Mergeawsome(taxonomy_table()[taxonomy_table()$Sex%in%input$ancessx & taxonomy_table()$Species%in%input$ancessp & taxonomy_table()$Age >= as.numeric(input$anceage[1]) & taxonomy_table()$Age < as.numeric(input$anceage[2]) & taxonomy_table()$Age >= as.numeric(input$ancecut),],type="Anc")
    }
  })
  filteredData22 <- reactive({
    req(input$anceage)
    req(input$ancecut)
    if(length(grep("Anc",names(taxonomy_table())))>0){
      Mergeawsome(taxonomy_table()[taxonomy_table()$Sex%in%input$ancessx & taxonomy_table()$Species%in%input$ancessp & taxonomy_table()$Age >= as.numeric(input$anceage[1]) & taxonomy_table()$Age < as.numeric(input$anceage[2]) & taxonomy_table()$Age< as.numeric(input$ancecut),],type="Anc")
    }
  })
  
  ###########output ance map
  output$drawance <- renderLeaflet({
    #if(length(grep("Anc",names(taxonomy_table())))>0){
    req(filteredData2T())
    leaflet(options = leafletOptions(zoomControl = FALSE, minZoom = 2.48, maxZoom = 10, dragging = T)) %>%
      addTiles(tilesURL) %>% 
      fitBounds(min(filteredData2T()$longitude)+1,min(filteredData2T()$latitude),max(filteredData2T()$longitude)-1,max(filteredData2T()$latitude)-0.5) %>%
      addMinicharts(
        filteredData2()$longitude, filteredData2()$latitude,
        layerId = filteredData2()$Site,width=4,height=4) %>% addResetMapButton()%>% addMiniMap(width=100,height=100,toggleDisplay = TRUE,position = "bottomright")
    #}
  })
  observe({
    if (length(input$ancecomp) == 0) {
      data <- 1
    } else if(length(input$ancecomp) <length(grep("Anc",names(taxonomy_table())))){
      data=as.data.frame(cbind(filteredData2()[,input$ancecomp],(1-apply(as.data.frame(filteredData2()[,input$ancecomp]),1,sum))))
      colnames(data)=c(input$ancecomp,"NON")
    } else {
      data <- filteredData2()[,input$ancecomp]
    }
    maxValue <- 1# max(as.matrix(data))
    if(input$type1=="bar"){
      leafletProxy("drawance") %>%
        clearMarkers() %>%
        updateMinicharts(
          layerId = filteredData2()$Site,
          chartdata =  data,
          maxValues = maxValue,width=15,height=15,
          type =input$type1,showLabels = input$labels1,legend=TRUE,
          colorPalette = brewer.pal(11, input$colors2)[c(2,5,11,1,9,3,8,4)],
          popup = popupArgs(
            labels = names(data),
            html = labelpop(filteredData2(),input$anceslab
                            
            )
          )
        )
    }else if(input$type1=="pie" & length(grep("Anc",names(taxonomy_table())))>0){
      leafletProxy("drawance") %>%
        clearMarkers() %>% clearControls() %>%
        updateMinicharts(
          layerId = filteredData2()$Site,
          chartdata =  data,
          maxValues = maxValue,
          type ="pie",showLabels = input$labels1,legend=TRUE,legendPosition="bottomright",
          colorPalette = brewer.pal(11, input$colors2)[c(2,5,11,1,9,3,8,4)],
          popup = popupArgs(
            labels =  names(data),
            html = labelpop(filteredData2(),input$anceslab
            )
          ),
          width = sqrt(filteredData2()$Count)*10,transitionTime = 0
        ) #%>% addLegendSize(position = "bottomleft",col="black",fillColor="white",baseSize=quantile(sqrt(filteredData2()$Count)*3,0.8),#median(sqrt(filteredData2()$Count)*5*0.6),
           #                 values = (filteredData2()$Count),shape="circle",orientation="horizontal",breaks=4) #%>%
      # addLabelOnlyMarkers(min(filteredData2()$longitude)+3,max(filteredData2()$latitude)+2,label=paste(input$anceage[2],"-",input$ancecut,"BP",sep=""),
      #        labelOptions = labelOptions(noHide = T, direction = 'top', textOnly = T,style=list("font-size"="20px","color" = "darkblue","font-style"="bold","box-shadow"="3px 3px rgba(0,0,0,0.25)", "border-color" = "rgba(0,0,0,0.5)")))
    }else  if(length(select(taxonomy_table(),starts_with("Anc_")))>0){
      leafletProxy("drawance") %>%
        clearMarkers() %>%
        updateMinicharts(
          layerId = filteredData2()$Site,
          chartdata =  data,
          maxValues = maxValue,
          type =input$type1,showLabels = input$labels1,legend=TRUE,
          colorPalette = brewer.pal(11, input$colors2)[c(2,5,11,1,9,3,8,4)],
          popup = popupArgs(
            labels =  names(data),
            html = labelpop(filteredData2(),input$anceslab
            )
          ),
          width = 10*sqrt(filteredData2()$Count),transitionTime = 0
        )
    }
  })
  ############addgrid map and merge###############
  griddataanc1<- reactive({
    req(filteredData2())
    req(input$Gridanc)
	if(length(filteredData2())>0 &input$Gridanc>0){
    gridmap(filteredData2(),input$Gridanc,type="Anc")
	}
  })
  observe({
    if(input$Gridanc>0){
      leafletProxy("drawance",data=griddataanc1()) %>%
        clearMarkers() %>% clearControls() %>%
        removeMinicharts(layerId = filteredData2()$Site) %>%
        addMinicharts(as.numeric(griddataanc1()$longitude), as.numeric(griddataanc1()$latitude),
                      chartdata=data.frame(lapply(griddataanc1()[,names(griddataanc1())[grep("Anc",names(griddataanc1()))]],as.numeric)),type="pie",
                      colorPalette = brewer.pal(11, input$colors2)[c(2,5,11,1,9,3,8,4)],
                      width = 10*sqrt(as.numeric(griddataanc1()$Count)))%>%
        addGraticule(interval = input$Gridanc, style = list(color = "#FF0000", weight = 0.1,fillOpacity=0.1))
    }
  })
  output$anchead <- renderText({
    if (length(input$ancecomp)> 0){
      paste("Geographic distribution of ancestry components",sep=" ")
    }
  })
  output$upanc <- renderText({
    if (length(input$ancecomp)> 0){
      paste("Before "," ",input$ancecut," BP",sep=" ")
    }
  })
  output$downanc <- renderText({
    if (length(input$ancecomp)> 0){
      paste("After "," ",input$ancecut," BP",sep=" ")
    }
  })
  ##############cut 2, less than cut####
  ##output map
  output$drawance2 <- renderLeaflet({
    req(filteredData2T())
    #basemap %>%
    leaflet(options = leafletOptions(zoomControl = FALSE, minZoom = 2.48, maxZoom = 10, dragging = T)) %>%
      addTiles(tilesURL) %>%
      fitBounds(min(filteredData2T()$longitude)+1,min(filteredData2T()$latitude),max(filteredData2T()$longitude)-1,max(filteredData2T()$latitude)-0.5) %>%
      
      addMinicharts(
        filteredData22()$longitude, filteredData22()$latitude,
        layerId = filteredData22()$Site,width=4,height=4) %>% addResetMapButton()%>% addMiniMap(width=100,height=100,toggleDisplay = TRUE,position = "bottomright")
  })
  observe({
    if (length(input$ancecomp) == 0) {
      data22 <- 1
    } else if(length(input$ancecomp) <length(grep("Anc",names(taxonomy_table())))){
      data22=(cbind(filteredData22()[,input$ancecomp],(1-apply(as.data.frame(filteredData22()[,input$ancecomp]),1,sum))))
      colnames(data22)=c(input$ancecomp,"NON")
    }
    else {
      data22 <- filteredData22()[,input$ancecomp]
    }
    maxValue <- 1#max(as.matrix(data22))
    if(input$type1=="bar"){
      leafletProxy("drawance2") %>%
        clearMarkers() %>%
        updateMinicharts(
          layerId = filteredData22()$Site,
          chartdata =  data22,
          maxValues = maxValue,width=15,height=15,
          type =input$type1,showLabels = input$labels1,legend=TRUE,
          colorPalette = brewer.pal(11, input$colors2)[c(2,5,11,1,9,3,8,4)],
          popup = popupArgs(
            labels =  names(data),
            html = labelpop(filteredData22(),input$anceslab
            )
          )
        )
    }else if(input$type1=="pie" & length(grep("Anc",names(taxonomy_table())))>0){
      leafletProxy("drawance2") %>%
        clearMarkers() %>% clearControls() %>%
        updateMinicharts(
          layerId = filteredData22()$Site,
          chartdata =  data22,
          maxValues = maxValue,
          type ="pie",showLabels = input$labels1,legend=TRUE,legendPosition="bottomright",
          colorPalette = brewer.pal(11, input$colors2)[c(2,5,11,1,9,3,8,4)],
          popup = popupArgs(
            labels = input$ancescomp,
            html = labelpop(filteredData22(),input$anceslab
            )
          ),
          width = sqrt(filteredData22()$Count)*12,transitionTime = 0
        ) #%>% addLegendSize(position = "bottomleft",col="black",fillColor="white",baseSize=quantile(sqrt(filteredData22()$Count)*3,1),#median(sqrt(filteredData22()$Count)*5*0.6),
                #            values = (filteredData22()$Count),shape="circle",orientation="horizontal",breaks=3)# %>%
      #addLabelOnlyMarkers(min(filteredData22()$longitude)+3,max(filteredData22()$latitude)+2,label=paste(input$ancecut,"-",input$anceage[1],"BP",sep=""),
      #   labelOptions = labelOptions(noHide = T, direction = 'top', textOnly = T,style=list("font-size"="20px","color" = "darkblue","font-style"="bold","box-shadow"="3px 3px rgba(0,0,0,0.25)", "border-color" = "rgba(0,0,0,0.5)")))
      
    }else if(length(select(taxonomy_table(),starts_with("Anc_")))>0){
      leafletProxy("drawance2") %>%
        clearMarkers() %>%
        updateMinicharts(
          layerId = filteredData22()$Site,
          chartdata =  data22,
          maxValues = maxValue,
          type =input$type1,showLabels = input$labels1,legend=TRUE,
          colorPalette = brewer.pal(11, input$colors2)[c(2,5,11,1,9,3,8,4)],
          popup = popupArgs(
            labels = input$ancescomp,
            html = labelpop(filteredData22(),input$anceslab
            )
          ),
          width = 10*sqrt(filteredData22()$Count),transitionTime = 0
        )
    }
  })
  ############addgrid map and merge###############
  griddataanc2<- reactive({
    req(filteredData22())
    req(input$Gridanc)
	if(length(filteredData22())>0 &input$Gridanc>0){
    gridmap(filteredData22(),input$Gridanc,type="Anc")
	}
  })
  observe({
    if(input$Gridanc>0){
      leafletProxy("drawance2",data=griddataanc2()) %>%
        clearMarkers() %>% clearControls() %>%
        removeMinicharts(layerId = filteredData22()$Site) %>%
        addMinicharts(as.numeric(griddataanc2()$longitude), as.numeric(griddataanc2()$latitude),
                      chartdata=data.frame(lapply(griddataanc2()[,names(griddataanc2())[grep("Anc",names(griddataanc2()))]],as.numeric)),type="pie",
                      colorPalette = brewer.pal(11, input$colors2)[c(2,5,11,1,9,3,8,4)],
                      width = 10*sqrt(as.numeric(griddataanc2()$Count)))%>%
        addGraticule(interval = input$Gridanc, style = list(color = "#FF0000", weight = 0.1,fillOpacity=0.1))
    }
  })
  ###########################select output folder################################
  TherootsA <- reactive({
    rootA <- input$rootA
    req(rootA, dir.exists(rootA))
    
    if(length(rootA) == 0 || rootA == ""){
      volumes <- getVolumes()()
      c(volumes)
    } else{
      c(project_root = rootA)
    }
  })
  
  Thesheets_dirA <- reactive({
    shinyDirChoose(input, 'sheets_dirA', roots = TherootsA(), session = session)
    parseDirPath(roots = TherootsA(), input$sheets_dirA)
  })
  
  output$sheets_dirA <- renderPrint({
    Thesheets_dirA()
  }) 
  #######down grid figures######
  observeEvent(input$Downa, {
    req(filteredData2T())
    outpath=Thesheets_dirA()
    gridanceplot(filteredData2T(),as.numeric(input$anceage[1]),as.numeric(input$anceage[2]),input$GridSizea,outpath)
  })
  
  ##draw count plot
  output$drawanceCount <- renderPlotly({
    req(filteredData2())
    req(filteredData22())
    dataplot=rbind(filteredData2(),filteredData22())
    dataplot$type=NA
    dataplot[dataplot$Age>=input$ancecut,"type"]=paste("Before",input$ancecut,sep="")
    dataplot[dataplot$Age<input$ancecut,"type"]=paste("After",input$ancecut,sep="")
    ggplot(dataplot,aes(x=Age,y=Count,color=type))+geom_point()+ggtitle("Temporal distribution of the samples")+theme(
      plot.title = element_text(size=15, family="Arial"),plot.margin=unit(c(1,0.3,0.5,0.3),'cm'))
  })
  
  ##################################pca plot#########################
  ################read table##################################
  PCA_table <- reactive({
    req(input$in_pca_table)
    if (grepl(input$in_pca_table$datapath, pattern = ".txt") |
        grepl(input$in_pca_table$datapath, pattern = ".tsv")) {
      read.table(input$in_pca_table$datapath, header = F,
                 sep = "\t", stringsAsFactors = F,
                 quote = "", comment.char = "")#,row.names = 1)
    }
  })
  
  #############render age UI#####
  output$pcaoutsp <- renderUI({
    req(taxonomy_table())
    selectInput("pcasp",
                label = "Select species",
                choices = unique(taxonomy_table()$Species),
                multiple = TRUE,selected=unique(taxonomy_table()$Species)[1])
  })
  output$pcaoutsx <- renderUI({
    req(taxonomy_table())
    selectInput("pcasx",
                label = "Select sex",
                choices = unique(taxonomy_table()$Sex),
                multiple = TRUE,selected=unique(taxonomy_table()$Sex)[1])
  })
  output$pcaout <- renderUI({
    sliderInput("pcaage",
                label = "Select a time range",
                min = 0,#taxonomy_table()$Age %>%
                  #as.numeric() %>%
                  #min(na.rm = TRUE),
                max = taxonomy_table()$Age %>%
                  as.numeric %>%
                  max(na.rm = TRUE),
                step = 100,
                value = range(taxonomy_table()$Age))
  })
  output$pcaout1 <- renderUI({
    selectInput("PC1",
                label = "Select PC1",
                choices = names(taxonomy_table())[grep("PC",names(taxonomy_table()))],
                multiple = FALSE)
  })
  output$pcaout2 <- renderUI({
    selectInput("PC2",
                label = "Select PC2",
                choices = names(taxonomy_table())[grep("PC",names(taxonomy_table()))],
                multiple = FALSE)
  })
  output$pcaoutlog <- renderUI({
    sliderInput("pcalog",
                label = "Select a longitude range",
                min = taxonomy_table()$longitude %>%
                  as.numeric() %>%
                  # round(2) %>%
                  min(na.rm = TRUE),
                max = taxonomy_table()$longitude %>%
                  as.numeric %>%
                  #round(2) %>%
                  max(na.rm = TRUE),
                step = 0.1,
                value = range(taxonomy_table()$longitude))
  })
  output$pcaoutlat <- renderUI({
    sliderInput("pcalat",
                label = "Select a latitude range",
                min = taxonomy_table()$latitude %>%
                  as.numeric() %>%
                  min(na.rm = TRUE),
                max = taxonomy_table()$latitude %>%
                  as.numeric %>%
                  max(na.rm = TRUE),
                step = 1,
                value = range(taxonomy_table()$latitude))
  })
  ############draw pca#########################
  output$pca <- renderPlotly({
    req(taxonomy_table())
    req(PCA_table())
    # req(input$pcaage)
    #req(input$PC1)
    #req(input$PC2)
    # req(input$pcalat)
    #req(input$pcalog)
    drawpca(taxonomy_table(),as.character(input$pcasx),as.character(input$pcasp),as.numeric(input$pcaage[1]),as.numeric(input$pcaage[2]),PCA_table(),input$pcalat[1],input$pcalat[2],input$pcalog[1],input$pcalog[2],(input$PC1),(input$PC2))
  })
  
  #######reactiveData#######################
  filteredData4 <- reactive({
    req(input$pcaage)
    Mergeawsome(taxonomy_table()[taxonomy_table()$Sex%in%input$pcasx & taxonomy_table()$Species%in%input$pcasp & taxonomy_table()$Age >= as.numeric(input$pcaage[1]) & taxonomy_table()$Age < as.numeric(input$pcaage[2]) & taxonomy_table()$longitude >=as.numeric(input$pcalog[1]) & taxonomy_table()$longitude <=as.numeric(input$pcalog[2]) & taxonomy_table()$latitude >=as.numeric(input$pcalat[1]) &taxonomy_table()$latitude <=as.numeric(input$pcalat[2]),])
  })
  #color 
  colorpal1 <- reactive({
    colorNumeric(brewer_pal(palette = "RdYlGn")(11), filteredData4()$Age)
  })
  ###err
  output$errpca <- renderText({
    if(length(select(taxonomy_table(),starts_with("PC")))==0){
      validate("The required data were not part of your Input File. Please make sure that columns labelled 'PC' are provided")
    }
  })
  #head
  output$pcahead <- renderText({
    if(length(PCA_table())>0){
      paste("Temporal distribution of the samples",sep=" ")
    }
  })
  ##draw pca map
  output$pcamap <- renderLeaflet({
    req(filteredData4())
    req(PCA_table())
    pal <- colorpal1()
    leaflet(options = leafletOptions(zoomControl = FALSE, minZoom = 2, maxZoom = 10, dragging = T)) %>%
      addTiles(tilesURL) %>% #addControl(titlepca, position = "topleft") %>%
      fitBounds(min(taxonomy_table()$longitude),min(taxonomy_table()$latitude),max(taxonomy_table()$longitude),max(taxonomy_table()$latitude)) %>%
      addCircleMarkers(filteredData4()$longitude, filteredData4()$latitude,radius=sqrt(filteredData4()$Count)*2
                       ,weight=1,popup=paste0(filteredData4()$Sample),color = "#777777",fillColor = pal(filteredData4()$Age), fillOpacity = 0.9,stroke = FALSE) %>% 
      addResetMapButton() %>% addMiniMap(width=100,height=100,toggleDisplay = TRUE,position = "topright") %>%
      addLegendSize(position = "bottomleft",col="black",fillColor="white",baseSize=min(sizeNumeric(filteredData4()$Count*2,baseSize = mean(filteredData4()$Count)*2.5))
                    ,values = sizeNumeric(filteredData4()$Count,baseSize = mean(filteredData4()$Count)),shape="circle",orientation="horizontal",breaks=5)
  })
  
  ##################multiple SNPs########################
  #######render UI for multiple SNPs##################
  output$snpoutsp <- renderUI({
    req(taxonomy_table())
    selectInput("snpsp",
                label = "Select Species",
                choices = unique(taxonomy_table()$Species),
                multiple = TRUE,selected=unique(taxonomy_table()$Species)[1])
  })
  output$snpoutsx <- renderUI({
    req(taxonomy_table())
    selectInput("snpsx",
                label = "Select sex",
                choices = unique(taxonomy_table()$Sex),
                multiple = TRUE,selected=unique(taxonomy_table()$Sex)[1])
  })

  output$snpoutage <- renderUI({
    req(taxonomy_table())
    sliderInput("snpage",
                label = "Select a time range",
                min = 0,#taxonomy_table()[apply(as.matrix(taxonomy_table()[,names(taxonomy_table())[grepl("SNP_",names(taxonomy_table())) & grepl("_[A,C,G,T,D]",names(taxonomy_table()))]]),1,sum)>0,"Age"] %>%#taxonomy_table()$Age %>%
                  #as.numeric() %>%
                  #min(na.rm = TRUE),
                max = taxonomy_table()$Age %>% #max(taxonomy_table()[apply(as.matrix(taxonomy_table()[,names(taxonomy_table())[grepl("SNP_",names(taxonomy_table())) & grepl("_[A,C,G,T,D]",names(taxonomy_table()))]]),1,sum)>0,"Age"]) %>%#taxonomy_table()$Age %>%
                  as.numeric %>%
                  max(na.rm = TRUE),
                step = 10,
                value = range(taxonomy_table()$Age))
  })
  output$snpoutsnp <- renderUI({
    selectInput("snpsnp",
                label = "Select your variations",
                choices = unique(names(taxonomy_table() %>% select(starts_with("SNP_")))),#names(taxonomy_table())[grepl("SNP_",names(taxonomy_table())) & grepl("_[A,C,G,T,D]",names(taxonomy_table()))],
                multiple = TRUE)
  })
  output$snpoutlog <- renderUI({
   req(taxonomy_table())
    sliderInput("snplog",
                label = "Select a longitude range",
                min = taxonomy_table()$longitude %>%
                  as.numeric() %>%
                  min(na.rm = TRUE),
                max = taxonomy_table()$longitude%>%
				as.numeric %>%
                  max(na.rm = TRUE),
                step = 0.1,
                value = range(as.numeric(taxonomy_table()$longitude)))
  })
  output$snpoutlat <- renderUI({
  req(taxonomy_table())
    sliderInput("snplat",
                label = "Select a latitude range",
                min = taxonomy_table()$latitude %>%
                  as.numeric() %>%
                  min(na.rm = TRUE),
                max = taxonomy_table()$latitude %>%
				as.numeric %>%
                  max(na.rm = TRUE),
                step = 1,
                value = range(as.numeric(taxonomy_table()$latitude)))
				})
 # output$snpoutwins <- renderUI({
 #   numericInput("GridSize2","Time interval (years)",1000,min=100,max=10000)
 # })
 # output$snpoutgo <- renderUI({
 #   actionButton("Down2", "Drawpictures!")
 # })
  output$errmultsnp <- renderText({
    if(length(select(taxonomy_table(),starts_with("SNP_")))==0){
      validate("The required data were not part of your Input File. Please make sure that columns labelled 'SNP_snpname_A' and 'SNP_snpname_D' are provided")
    }
  })
  ##active data
  filteredData5 <- reactive({
    req(input$snpage,taxonomy_table())
	req(input$alltypesnp,input$snplog,input$snplat)
    if(length(select(taxonomy_table(),starts_with("SNP_")))>0 & input$alltypesnp=="ReadCounts"){
      MergeawsomeCount(taxonomy_table()[taxonomy_table()$Sex%in%input$snpsx & taxonomy_table()$Species%in%input$snpsp & taxonomy_table()$Age >= as.numeric(input$snpage[1]) & taxonomy_table()$Age <= as.numeric(input$snpage[2])
                                   & taxonomy_table()$longitude>=input$snplog[1] & taxonomy_table()$longitude<=input$snplog[2] & taxonomy_table()$latitude>=input$snplat[1] & taxonomy_table()$latitude<=input$snplat[2],],mult=TRUE)
    }else if(input$alltypesnp=="Genotype"){
	Mergeawsome(taxonomy_table()[taxonomy_table()$Sex%in%input$snpsx & taxonomy_table()$Species%in%input$snpsp & taxonomy_table()$Age >= as.numeric(input$snpage[1]) & taxonomy_table()$Age <= as.numeric(input$snpage[2])
                                   & taxonomy_table()$longitude>=input$snplog[1] & taxonomy_table()$longitude<=input$snplog[2] & taxonomy_table()$latitude>=input$snplat[1] & taxonomy_table()$latitude<=input$snplat[2],],mult=TRUE)
	}
	else{
      return (NULL)
      #stop("Please make sure that all required columns are provided in the uploaded table!")
    }
  })

  output$snpoutlab <- renderUI({
    req(filteredData5())
    selectInput("snplab",
                label = "Select labels",
                choices =  names(filteredData5())[names(filteredData5())%in% c("n.overlaps","origins","SecondSite")==FALSE],
                multiple = TRUE,selected="Sample")
  })
  # render color
  colorpall <- reactive({
    colorNumeric(input$colors3, filteredData5()$Count)
  })
  #head
  output$mulhead <- renderText({
    if(length(select(taxonomy_table(),starts_with("SNP_")))>0){
      paste("Allele combination at multiple SNPs",sep=" ")
    }
  })
   filteredData5T <- reactive({
 req((input$snpsnp),filteredData5())
 as.data.frame(filteredData5()[apply(as.data.frame(filteredData5()[,input$snpsnp]),1,sum)>0,])
 })
  ##output snp multiple map
  output$snpmap <- renderLeaflet({
    if(length(select(taxonomy_table(),starts_with("SNP_")))>0){
      req(filteredData5())
      leaflet(options = leafletOptions(zoomControl = FALSE, minZoom = 2, maxZoom = 10, dragging = T)) %>%
        addTiles(tilesURL) %>%
        fitBounds(min(filteredData5T()$longitude),min(filteredData5T()$latitude),max(filteredData5T()$longitude),max(filteredData5T()$latitude)) %>%
        addMinicharts(
          filteredData5T()$longitude, filteredData5T()$latitude,
          layerId = filteredData5T()$Sample) %>% addResetMapButton() %>% addMiniMap(width=100,height=100,toggleDisplay = TRUE,position = "topright")
    }
  })

  observe({
  req(filteredData5T())
    if (length(input$snpsnp) == 0) {
      datasnp <- 1
    }else if(length(input$snpsnp) == 1){
	datasnp=filteredData5T()[,input$snpsnp]
	} #else if(length(input$snpsnp) < length(names(taxonomy_table() %>% select(starts_with("SNP"))))){#length(names(taxonomy_table())[grepl("SNP_",names(taxonomy_table())) & grepl("_D",names(taxonomy_table()))])){
      #datasnp <- cbind(filteredData5()[,input$snpsnp],(apply(as.data.frame(filteredData5()[,input$snpsnp]),1,sum)))
      #colnames(datasnp)=c(input$snpsnp,"NON")
	  #datasnp=datasnp[datasnp$NON>0,]
    #} 
    else if(length(input$snpsnp) > 1) {
      datasnp <- filteredData5T()[,input$snpsnp]#as.data.frame(filteredData5()[rowSums(as.data.frame(filteredData5()[,input$snpsnp]))>0,input$snpsnp])
	  
    }
    maxValue <- max(as.matrix(datasnp))
    if(input$type3=="bar" & length(nrow(datasnp)>0)){
      leafletProxy("snpmap") %>%
        clearMarkers() %>%
        updateMinicharts(
          layerId = filteredData5T()$Sample,
          chartdata =  datasnp,
          maxValues = maxValue,width=20,height=30,opacity = 0.8,
          type ="bar",showLabels = input$labels3,legend=TRUE,legendPosition="bottomright",
          colorPalette = brewer.pal(9, input$colors3)[c(2,9,1,7,3,8,4,8,1,8,3,9,2,6,4,1,7,3,9,2,8,4)],
          popup = popupArgs(
            labels = colnames(datasnp),
            html = labelpop(filteredData5T(),input$snplab
            )
          )
        )
    }else if(input$type3=="pie" & length(select(taxonomy_table(),starts_with("SNP_")))>0){
      leafletProxy("snpmap") %>%
        clearMarkers() %>% clearControls() %>%
        updateMinicharts(
          layerId = filteredData5T()$Sample,
          chartdata =  datasnp,
          maxValues = maxValue,
          type ="pie",showLabels = input$labels3,legend=TRUE,legendPosition="bottomright",
          colorPalette = brewer.pal(9, input$colors3)[c(2,9,1,7,3,8,4,8,1,8,3,9,2,6,4,1,7,3,9,2,8,4)],
          popup = popupArgs(
            labels = colnames(datasnp),
            html = labelpop(filteredData5T(),input$snplab
            )
          ),
          width = sqrt(filteredData5()$Counts)*5,transitionTime = 0
        ) #%>% addLegendSize(position = "bottomleft",col="black",fillColor="white",baseSize=quantile(sqrt(filteredData5()$Count)*2,0.85),
                       #     values = (filteredData5()$Count),shape="circle",orientation="horizontal",breaks=3)
      
    }else if(length(select(taxonomy_table(),starts_with("SNP_")))>0){
      leafletProxy("snpmap") %>%
        clearMarkers() %>%
        updateMinicharts(
          layerId = filteredData5T()$Sample,
          chartdata =  datasnp,
          maxValues = maxValue,
          type =input$type3,showLabels = input$labels3,legend=TRUE,
          colorPalette = brewer.pal(9, input$colors3)[c(2,9,1,7,3,8,4,8,1,8,3,9,2,6,4,1,7,3,9,2,8,4)],
          popup = popupArgs(
            labels = colnames(datasnp),
            html = labelpop(filteredData5T(),input$snplab
            )
          ),
          width = 15*(filteredData5T()$Counts),transitionTime = 0
        )
    }
  })
  
  ##draw count plot
  output$snpCount <- renderPlotly({
    req(filteredData5T())
	req(input$snpsnp)#,Age>=as.numeric(input$snpage[1]) &  Age<as.numeric(input$snpage[2])
	#plot_ly(filter(filteredData5(),Age>=as.numeric(input$snpage[1]) &  Age<=as.numeric(input$snpage[2])),x=~Age,y=~Counts,type="scatter",mode = 'markers',hoverinfo = 'text',text=~paste("Sample: ",Sample, '<br>Age: ', Age)) %>%
	#layout(title=list(text="Temporal distribution of the samples",size=20,xref="paper",yref="paper",x=0,y=~max(Counts)+0.1,xanchor = "left", yanchor =  "top",color="black",family="Arial"),margin=list(l=25,r=35,b=10,t=80,pad=4))
    ggplot(filteredData5T(),aes(x=Age,y=Counts))+ggtitle("Temporal distribution of the samples")+theme(
      plot.title = element_text(size=15, family="Arial"))+geom_point()#geom_bar(stat="identity",fill="steelblue")
	  #geom_histogram(aes(y = ..density..),
             #    colour = 1, fill = "white",bins=50) +
  #geom_density(lwd = 0.6,linetype = 1,colour = 2)
  })
  
  #######down grid figures######
  #observeEvent(input$Down2, {
  #  req(filteredData5())
    #gridsnpplot(filteredData5(),as.numeric(input$snpage[1]),as.numeric(input$snpage[2]),as.numeric(input$GridSize2))
 # })
  ###############################mutiple haplo#########################
  ##########render UI for haplo#########################
   output$hapoutty <- renderUI({
    req(taxonomy_table())
	if(length(grep("CAT_",names(taxonomy_table())))>0){
    selectInput("hapty", 
	label="Select(sub)haplogroups", 
	choices = unique(stri_replace_first_regex(names(taxonomy_table())[grep("CAT",names(taxonomy_table()))],"CAT_","")),
	multiple = FALSE,
	selected=unique(stri_replace_first_regex(names(taxonomy_table())[grep("CAT",names(taxonomy_table()))],"CAT_",""))[1])
	}
  })
  output$hapoutsp <- renderUI({
    req(taxonomy_table())
    selectInput("hapsp",
                label = "Select species",
                choices = unique(taxonomy_table()$Species),
                multiple = TRUE,selected=unique(taxonomy_table()$Species))
  })
  output$hapoutsx <- renderUI({
    req(taxonomy_table())
	req(input$hapty)
      if(length(input$hapty)==1){
	selectInput("hapsx",
                  label = "Select sex",
                  choices = c(Choose='',unique(taxonomy_table()[taxonomy_table()[,paste("CAT_",input$hapty,sep="")]!="unknown","Sex"])),#unique(taxonomy_table()$Sex),
                  multiple = TRUE)#,selected=unique(taxonomy_table()[taxonomy_table()[,paste("CAT_",input$hapty,sep="")]!="unknown","Sex"])[1])#unique(taxonomy_table()$Sex))
	}
  })
  output$hapoutage <- renderUI({
    req(taxonomy_table())
	#req(input$hapty)
    sliderInput("hapage",
                label = "Select a time range",
                min = 0,#taxonomy_table()$Age %>%
                  #as.numeric() %>%
                  #min(na.rm = TRUE),
                max = taxonomy_table()$Age %>%
                  as.numeric %>%
                  max(na.rm = TRUE),
                step = 10,
                value = range(taxonomy_table()$Age))
  })
   output$hapoutlog <- renderUI({
    sliderInput("haplog",
                label = "Select a longitude range",
                min = taxonomy_table()$longitude %>%
                  as.numeric() %>%
                  min(na.rm = TRUE),
                max = taxonomy_table()$longitude %>%
                  as.numeric %>%
                  max(na.rm = TRUE),
                step = 0.1,
                value = range(taxonomy_table()$longitude))
  })
  output$hapoutlat <- renderUI({
    sliderInput("haplat",
                label = "Select a latitude range",
                min = taxonomy_table()$latitude %>%
                  as.numeric() %>%
                  min(na.rm = TRUE),
                max = taxonomy_table()$latitude %>%
                  as.numeric %>%
                  max(na.rm = TRUE),
                step = 1,
                value = range(taxonomy_table()$latitude))
  })
  output$errhap <- renderText({
    if(length(select(taxonomy_table(),starts_with("CAT")))==0){
      validate("The required data were not part of your Input File. Please make sure that columns start with 'CAT' was provided")
    }
  })
  #head
  output$haphead <- renderText({
    if(length(select(taxonomy_table(),starts_with("CAT")))>0){
      paste("Haplo groups",sep=" ")
    }
  })
  
  #######active data##############
  filteredData6 <- reactive({
    req(input$hapage)
	req(input$hapty)
	req(input$hapsx)
	req(input$hapsp)
	req(taxonomy_table())
     if(length(select(taxonomy_table(),starts_with("CAT_")))>0 & length(input$hapty)==1){
	 mergeCAT(taxonomy_table()[taxonomy_table()$Sex%in%input$hapsx & 
	 taxonomy_table()$Species%in%input$hapsp & taxonomy_table()$Age >= as.numeric(input$hapage[1]) &  taxonomy_table()$Age < as.numeric(input$hapage[2]) &
	 taxonomy_table()$latitude >= as.numeric(input$haplat[1]) &  taxonomy_table()$latitude < as.numeric(input$haplat[2]) &
	 taxonomy_table()$longitude >= as.numeric(input$haplog[1]) &  taxonomy_table()$longitude < as.numeric(input$haplog[2]),],input$hapty)
	}
  })
  output$hapoutlab <- renderUI({
    req(filteredData6())
    selectInput("haplab",
                label = "Select labels",
                choices = names(filteredData6())[names(filteredData6())%in% c("n.overlaps","origins","SecondSite")==FALSE],
                multiple = TRUE,selected="Sample")
  })
  output$hapouthap <- renderUI({
  req(input$hapty)
  req(filteredData6())
    if(length(select(taxonomy_table(),starts_with("CAT")))>0 & length(input$hapty)==1){
      selectInput("haphap",
                  label = "Select haplotype groups",
                  choices = c(Choose='',names(filteredData6())[grep(paste(input$hapty,"_",sep=""),names(filteredData6()))]),#gethapname(sort(unique(filteredData6()$mtDNAhaplogroup)),"MT_"),
                  multiple = TRUE)
    }
  })
  ############DRAWPLOT#########################
  ##output haplotype map
  output$hapmap <- renderLeaflet({
    if(length(select(taxonomy_table(),starts_with("CAT")))>0){
      req(filteredData6())
      leaflet(options = leafletOptions(zoomControl = FALSE, minZoom = 2, maxZoom = 10, dragging = T)) %>%
        addTiles(tilesURL) %>%
        fitBounds(min(filteredData6()$longitude)-0.3,min(filteredData6()$latitude)-0.3,max(filteredData6()$longitude)+0.3,max(filteredData6()$latitude)+0.3) %>%
        addMinicharts(filteredData6()$longitude, filteredData6()$latitude,
                      layerId = filteredData6()$Site,width=4,height=4) %>% addResetMapButton()%>% addMiniMap(width=100,height=100,toggleDisplay = TRUE,position = "topleft") %>%
        addDrawToolbar(
          targetGroup='Selected',
          polylineOptions=FALSE,
          markerOptions = FALSE,
          polygonOptions = drawPolygonOptions(shapeOptions=drawShapeOptions(fillOpacity = 0,color = 'white',weight = 3)),
          rectangleOptions = drawRectangleOptions(shapeOptions=drawShapeOptions(fillOpacity = 0 ,color = 'white',weight = 3)),
          circleOptions = FALSE,circleMarkerOptions = FALSE,
          editOptions = editToolbarOptions(edit = FALSE, selectedPathOptions = selectedPathOptions())) 
    }
  })
  observe({
  req(input$hapty)
    if (length(input$haphap) == 0){
      datahap <- 1
    }else if(length(input$haphap)<length(unique(grep(input$hapty,names(filteredData6()))))){
      datahap <- cbind(filteredData6()[,input$haphap],(1-apply(as.data.frame(filteredData6()[,input$haphap]),1,sum)))
      colnames(datahap)=c(input$haphap,"NON")
    }else if (length(input$haphap)==length(unique(grep(input$hapty,names(filteredData6()))))){
      datahap <- filteredData6()[,input$haphap]
    }
    
    maxValue <- max(as.matrix(datahap))#length(unique(grep("MT_",names(filteredData6()))))
    
    #factorPal <- colorFactor(rcolors$rainbow,names(datahap))
    if(length(select(taxonomy_table(),starts_with("CAT")))>0){
      leafletProxy("hapmap") %>%
        clearMarkers() %>% clearControls() %>%
        updateMinicharts(
          layerId = filteredData6()$Site,
          chartdata =  datahap,
          maxValues = maxValue,
          type ="pie",showLabels = input$labels4,legend=TRUE,legendPosition ="bottomright",
          colorPalette = get_color(rcolors$t2m_29lev, n = length(colnames(datahap))+1),#brewer.pal(11, input$colors4)[c(1,11,3,9,2,8,4,11,5,1,6,4,7,2,1,11,3,9,2,8,4,11,3,7,2,9)],
          popup = popupArgs(
            #labels = input$haphap,
            html = labelpop(filteredData6(),input$haplab
            )
          ),
          width = sqrt(filteredData6()$Count)*3,transitionTime = 0
        ) %>% addLegendSize(position = "bottomleft",col="black",fillColor="white",baseSize=quantile(sqrt(filteredData6()$Count)*1.5,0.5),#median(sqrt(filteredData6()$Count)*3*0.6),
                            values = (filteredData6()$Count),shape="circle",orientation="horizontal",breaks=5) #%>%
      # addLegendFactor(pal=factorPal,values =names(datahap),
      #   position = 'bottomleft', shape = 'circle',orientation = 'horizontal',
      #   width = 5, height = 5)
    }
  })
  ############addgrid map and merge###############
  griddatahap<- reactive({
    req(filteredData6())
	req(input$hapty)
    req(input$Gridhap)
	if(length(filteredData6())>1 & input$Gridhap>0){
    gridmap(filteredData6(),input$Gridhap,type=input$hapty)
	}
  })
 observe({
  req(griddatahap())
    if(input$Gridhap>0){
      leafletProxy("hapmap",data=griddatahap()) %>%
        clearMarkers() %>% clearControls() %>%
        removeMinicharts(layerId = filteredData6()$Site) %>%
        addMinicharts(as.numeric(griddatahap()$longitude), as.numeric(griddatahap()$latitude),legend=TRUE,legendPosition ="bottomright",
                      chartdata=data.frame(lapply(griddatahap()[,names(griddatahap())[grep(input$hapty,names(griddatahap()))]],as.numeric)),type="pie",
                      colorPalette = get_color(rcolors$t2m_29lev, n = length(grep(input$hapty,names(griddatahap())))+1),
                      width = 2*sqrt(as.numeric(griddatahap()$Count)), popup = popupArgs(
                        #labels = input$haphap,
                        html = labelpop(griddatahap(),input$haplab
                        )))%>%
        addGraticule(interval = input$Gridhap, style = list(color = "#FF0000", weight = 0.1,fillOpacity=0.1))
    }
  })
  ############################################### click response ##################################################
  observeEvent(input$hapmap_draw_new_feature,{
    req(filteredData6())
	if(input$Gridhap==0){
    #Only add new layers for bounded locations
    found_in_bounds <- findLocations(shape = input$hapmap_draw_new_feature
                                     , location_coordinates = SpatialPointsDataFrame(filteredData6()[,c('longitude', 'latitude')] , filteredData6())
                                     , location_id_colname = "Site")
    for(id in found_in_bounds){
      if(id %in% data_of_click$clickedMarker){
        # don't add id
      } else {
        # add id
        data_of_click$clickedMarker<-append(data_of_click$clickedMarker, id, 0)
      }
    }
    # look up airports by ids found
    selected <- subset(filteredData6(), Site %in% data_of_click$clickedMarker)
    ######age count plot
    output$hapCount <- renderPlotly({
      req(selected)
      ggplot(selected,aes(x=Age,y=Count))+geom_point()+ggtitle("Temporal distribution of the samples")+theme(
        plot.title = element_text(size=15, family="Arial"),plot.margin=unit(c(1,1,2,1),'lines'))
    })
    leafletProxy("hapmap",data=selected)%>% 
      addCircleMarkers(lng=selected$longitude,lat=selected$latitude,
                       color = "red",radius=sqrt(selected$Count)*2,weight=1,
                       fillOpacity = 0.5,            
                       layerId = (selected$SecondSite))
  }
  })
  #for grid
  observeEvent(input$hapmap_draw_new_feature,{
   # req(griddatahap())
	if(as.numeric(input$Gridhap)>0){
    #Only add new layers for bounded locations
    found_in_bounds1 <- findLocations(shape = input$hapmap_draw_new_feature
                                     , location_coordinates = SpatialPointsDataFrame(griddatahap()[,c('longitude', 'latitude')] , griddatahap())
                                     , location_id_colname = "Site")
    for(id in found_in_bounds1){
      if(id %in% data_of_click$clickedMarker){
        # don't add id
      } else {
        # add id
        data_of_click$clickedMarker<-append(data_of_click$clickedMarker, id, 0)
      }
    }
    # look up airports by ids found
    selected <- subset(griddatahap(), Site %in% data_of_click$clickedMarker)
    ######age count plot
    output$hapCount <- renderPlotly({
      req(selected)
      ggplot(selected,aes(x=Age,y=Count))+geom_point()+ggtitle("Temporal distribution of the samples")+theme(
        plot.title = element_text(size=15, family="Arial"),plot.margin=unit(c(1,0.3,0.5,0.3),'cm'))
    })
    leafletProxy("hapmap",data=selected)%>% 
      addCircleMarkers(lng=selected$longitude,lat=selected$latitude,
                       color = "red",radius=sqrt(selected$Count)/1.2,weight=1,
                       fillOpacity = 0.5,            
                       layerId = (selected$SecondSite))
  }
  })
  ############################################### click response ##################################################
  observeEvent(input$hapmap_draw_deleted_features,{
  if(input$Gridhap==0){
    # loop through list of one or more deleted features/ polygons
    for(feature in input$hapmap_draw_deleted_features$features){
      
      # get ids for locations within the bounding shape
      bounded_layer_ids <- findLocations(shape = feature
                                         , location_coordinates = SpatialPointsDataFrame(filteredData6()[,c('longitude', 'latitude')] , filteredData6())
                                         , location_id_colname = "SecondSite")
      
      output$hapCount <- renderPlotly({
        return(NULL)
      })
      # remove second layer representing selected locations
      proxy <- leafletProxy("hapmap")
      proxy %>% removeMarker(layerId = as.character(bounded_layer_ids))
      first_layer_ids <- subset(filteredData6(), SecondSite %in% bounded_layer_ids)$Site
      
      data_of_click$clickedMarker <- data_of_click$clickedMarker[!data_of_click$clickedMarker
                                                                 %in% first_layer_ids]
    }
	}
  })
   observeEvent(input$hapmap_draw_deleted_features,{
  if(input$Gridhap>0){
    # loop through list of one or more deleted features/ polygons
    for(feature in input$hapmap_draw_deleted_features$features){
      
      # get ids for locations within the bounding shape
      bounded_layer_ids <- findLocations(shape = feature
                                         , location_coordinates = SpatialPointsDataFrame(griddatahap()[,c('longitude', 'latitude')] , griddatahap())
                                         , location_id_colname = "SecondSite")
      
      output$hapCount <- renderPlotly({
        return(NULL)
      })
      # remove second layer representing selected locations
      proxy <- leafletProxy("hapmap")
      proxy %>% removeMarker(layerId = as.character(bounded_layer_ids))
      first_layer_ids <- subset(griddatahap(), SecondSite %in% bounded_layer_ids)$Site
      
      data_of_click$clickedMarker <- data_of_click$clickedMarker[!data_of_click$clickedMarker
                                                                 %in% first_layer_ids]
    }
	}
  })
  
  ########################################################automatic plot#########################
  ########################################read par table####################
  Para_table <- reactive({
    req(input$in_para_table)
    if (grepl(input$in_para_table$datapath, pattern = ".txt") |
        grepl(input$in_para_table$datapath, pattern = ".tsv")) {
      read.table(input$in_para_table$datapath, header = T,
                 sep = "\t", stringsAsFactors = F,
                 quote = "", comment.char = "")
    }
  })
  
  output$print_para_table <- DT::renderDataTable({
    req(Para_table())
    table <- Para_table() 
    DT::datatable(table, options = list(scrollX = TRUE))
  })
  ###########################select output folder################################
  Theroots1 <- reactive({
    root1 <- input$root1
    req(root1, dir.exists(root1))
    if(length(root1) == 0 || root1 == ""){
      volumes <- getVolumes()()
      c(volumes)
    } else{
      c(project_root = root1)
    }
  })
  
  Thesheets_dir1 <- reactive({
    shinyDirChoose(input, 'sheets_dir1', roots = Theroots1(), session = session)
    parseDirPath(roots = Theroots1(), input$sheets_dir1)
  })
  
  output$sheets_dir1 <- renderPrint({
    Thesheets_dir1()
  }) 
  #output$downloadFigures <- eventReactive
  observeEvent(input$Draw, {
    req(Para_table())
    outpath=Thesheets_dir1()
    automaticFigures(Para_table(),outpath)
  })
})