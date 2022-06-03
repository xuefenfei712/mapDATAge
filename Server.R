options(digits = 5, shiny.maxRequestSize = 10 * 1024 ^ 2)
server <- function(input, output,session)({
   # Read in data files and validate 
  tabl <- reactive({
    req(input$in_table)
    if (grepl(input$in_table$datapath, pattern = ".txt") |
        grepl(input$in_table$datapath, pattern = ".tsv")) {
      read.table(input$in_table$datapath, header = T,
                 sep = "\t", stringsAsFactors = F,
                 quote = "", comment.char = "")
    }
  })
  taxonomy_table <- reactive({
    req(tabl())
    test=tabl()
    colnames(test)=toupper(names(tabl()))
    test$LATITUDE=round(as.numeric(test$LATITUDE),2)
    test$LONGITUDE=round(as.numeric(test$LONGITUDE),2)
    test$AGE=round(as.numeric(test$AGE),2)
    test
  })
  output$fileStatus <- eventReactive(input$go, {
    if (is.null(validate_input_files(taxonomy_table()))) {
      paste("Congrats, no errors detected!")
    }
  })
  ###############################draw map#################################
  ####################### Render UIs for Panel 3 (map)##################
  output$mapoutsp <- renderUI({
    req(taxonomy_table())
    selectInput("mapsp",
                label = "Select species",
                choices = unique(taxonomy_table()$SPECIES),
                multiple = TRUE,selected=unique(taxonomy_table()$SPECIES)[1])
  })
  output$mapoutsx <- renderUI({
    req(taxonomy_table())
    selectInput("mapsx",
                label = "Select sex",
                choices = unique(taxonomy_table()$SEX),
                multiple = TRUE,selected=unique(taxonomy_table()$SEX)[1])
  })
  output$mapout <- renderUI({
    req(taxonomy_table())
    sliderInput("mapage",
                label = "Select a time range",
                min = 0, 
                max = taxonomy_table()$AGE %>%
                  as.numeric %>%
                  max(na.rm = TRUE),
                step = 10,
                value = range(taxonomy_table()$AGE))
  })
  output$mapoutlog <- renderUI({
    sliderInput("maplog",
                label = "Select a longitude range",
                min = taxonomy_table()$LONGITUDE %>%
                  as.numeric() %>%
                  #round(2)%>%
                  min(na.rm = TRUE),
                max = taxonomy_table()$LONGITUDE %>%
                  as.numeric %>%
                  #round(2)%>%
                  max(na.rm = TRUE),
                step = 0.01,
                value = range(taxonomy_table()$LONGITUDE))
  })
  output$mapoutlat <- renderUI({
    sliderInput("maplat",
                label = "Select a latitude range",
                min = taxonomy_table()$LATITUDE %>%
                  as.numeric() %>%
                  #round(2)%>%
                  min(na.rm = TRUE),
                max = taxonomy_table()$LATITUDE %>%
                  as.numeric %>%
                  #round(2)%>%
                  max(na.rm = TRUE),
                step = 0.01,
                value = range(taxonomy_table()$LATITUDE))
  })
  
  ##basemap
  tilesURL <- "http://server.arcgisonline.com/ArcGIS/rest/services/Canvas/World_Light_Gray_Base/MapServer/tile/{z}/{y}/{x}"
  maxValue=1
  # Reactive expression for the data subsetted to what the user selected
  filteredData <- reactive({
    req(input$mapage)
	if(as.numeric(input$mapage[2])-as.numeric(input$mapage[1])>0 & as.numeric(input$maplat[2])-as.numeric(input$maplat[1]) & as.numeric(input$maplog[2])-as.numeric(input$maplog[1])>0){
    Merge(taxonomy_table()[taxonomy_table()$SEX%in%input$mapsx & taxonomy_table()$SPECIES%in%input$mapsp & taxonomy_table()$AGE >= as.numeric(input$mapage[1]) & taxonomy_table()$AGE < as.numeric(input$mapage[2])
                                 & taxonomy_table()$LATITUDE>=as.numeric(input$maplat[1]) & taxonomy_table()$LATITUDE<=as.numeric(input$maplat[2]) & taxonomy_table()$LONGITUDE>=as.numeric(input$maplog[1]) & taxonomy_table()$LONGITUDE<=as.numeric(input$maplog[2]),])
								 }else{NULL}
  })
  output$mapoutlab <- renderUI({
    req(filteredData())
    selectInput("maplab",
                label = "Select labels",
                choices = names(filteredData())[names(filteredData())%in% c("n.overlaps","origins","SecondSite")==FALSE],
                multiple = TRUE,selected="SAMPLE")
  })
  # render color
  colorpal <- reactive({
    req(filteredData()$AGE)
    colorNumeric(input$colors,filteredData()$AGE)
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
      fitBounds(min(filteredData()$LONGITUDE)-0.3,min(filteredData()$LATITUDE)-0.3,max(filteredData()$LONGITUDE)+0.5,max(filteredData()$LATITUDE)+0.5) %>%
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
      addCircleMarkers(filteredData()$LONGITUDE, filteredData()$LATITUDE,color = "#777777",radius=sqrt(filteredData()$Count)*2#mean(filteredData()$Count)))#~sqrt(Count)
                       ,weight=1,fillColor = ~pal(AGE), fillOpacity = 0.9, popup = (labelpop(filteredData(),input$maplab)),layerId = filteredData()$SITE
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
                       pal = pal, values = filteredData()$AGE) })
  ############################################### click response ##################################################
  observeEvent(input$drawmap_draw_new_feature,{
    req(filteredData())
    found_in_bounds <- findLocations(shape = input$drawmap_draw_new_feature
                                     , location_coordinates = SpatialPointsDataFrame(filteredData()[,c('LONGITUDE', 'LATITUDE')] , filteredData())
                                     , location_id_colname = "SITE")
    for(id in found_in_bounds){
      if(id %in% data_of_click$clickedMarker){
        # don't add id
      } else {
        # add id
        data_of_click$clickedMarker<-append(data_of_click$clickedMarker, id, 0)
      }
    }
    
    selected <- subset(filteredData(), SITE %in% data_of_click$clickedMarker)
    ######age count plot
    output$drawmapCount <- renderPlotly({
      req(selected)
      ggplot(selected,aes(x=AGE,y=Count))+geom_point()+ggtitle("Temporal distribution of the samples")+theme(plot.title = element_text(size=15,family="Arial",face="plain"),
                                                                                                             axis.title= element_text(size=13,family="Arial",face="plain"),plot.margin=unit(c(1,0.3,0.5,0.3),'cm'))
    })
    leafletProxy("drawmap",data=selected)%>% 
      addCircleMarkers(lng=selected$LONGITUDE,lat=selected$LATITUDE,
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
                                         , location_coordinates = SpatialPointsDataFrame(filteredData()[,c('LONGITUDE', 'LATITUDE')] , filteredData())
                                         , location_id_colname = "SecondSite")
      output$drawmapCount <- renderPlotly({
        return(NULL)
      })
      # remove second layer representing selected locations
      proxy <- leafletProxy("drawmap")
      proxy %>% removeMarker(layerId = as.character(bounded_layer_ids))
      first_layer_ids <- subset(filteredData(), SecondSite %in% bounded_layer_ids)$SITE
      data_of_click$clickedMarker <- data_of_click$clickedMarker[!data_of_click$clickedMarker
                                                                 %in% first_layer_ids]
    }
  })
  ###########################select output folder################################
  TherootsAS <- reactive({
    rootAS <- input$rootAS
    req(rootAS, dir.exists(rootAS))
    
    if(length(rootAS) == 0 || rootAS == ""){
      volumes <- getVolumes()()
      c(volumes)
    } else{
      c(project_root = rootAS)
    }
  })
  
  Thesheets_dirAS <- reactive({
    shinyDirChoose(input, 'sheets_dirAS', roots = TherootsAS(), session = session)
    parseDirPath(roots = TherootsAS(), input$sheets_dirAS)
  })
  
  output$sheets_dirAS <- renderPrint({
    Thesheets_dirAS()
  }) 
  #######down grid figures######
  observeEvent(input$Downs, {
    req(filteredData(),input$mapage)
    outpath=Thesheets_dirAS()
	if(input$mapage[2]>input$mapage[1] &nrow(filteredData())>0){
    gridplotMap(filteredData(),as.numeric(input$mapage[1]),as.numeric(input$mapage[2]),outpath)
	}else{NULL}
  })
  ############################pie map############################################  
  ################ Render UIs for Panel 4 (pie)#################################
  output$pieoutsp <- renderUI({
    req(taxonomy_table())
    selectInput("piesp",
                label = "Select species",
                choices = unique(taxonomy_table()$SPECIES),
                multiple = TRUE,selected=unique(taxonomy_table()$SPECIES)[1])
  })
  output$pieoutsx <- renderUI({
    req(taxonomy_table())
    selectInput("piesx",
                label = "Select sex",
                choices = unique(taxonomy_table()$SEX),
                multiple = TRUE,selected=unique(taxonomy_table()$SEX)[1])
  })
  output$pieout <- renderUI({
    req(taxonomy_table())
    sliderInput("pieage",
                label = "Select a time range",
                min = 0,
                max = taxonomy_table()$AGE %>%
                  as.numeric %>%
                  max(na.rm = TRUE),
                step = 10,
                value = range(taxonomy_table()$AGE))
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
                min = taxonomy_table()$LONGITUDE %>%
                  as.numeric() %>%
                  #round(2) %>%
                  min(na.rm = TRUE),
                max = taxonomy_table()$LONGITUDE %>%
                  as.numeric %>%
                  #round(2) %>%
                  max(na.rm = TRUE),
                step = 0.01,
                value = range(taxonomy_table()$LONGITUDE))
  })
  output$pieoutlat <- renderUI({
    req(taxonomy_table())
    sliderInput("pielat",
                label = "Select a latitude range",
                min = taxonomy_table()$LATITUDE %>%
                  as.numeric() %>%
                 # round(2) %>%
                  min(na.rm = TRUE),
                max = taxonomy_table()$LATITUDE %>%
                  as.numeric %>%
                 # round(2) %>%
                  max(na.rm = TRUE),
                step = 0.01,
                value = range(taxonomy_table()$LATITUDE))
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
    if(length(select(taxonomy_table(),starts_with("SNP_")))>0 & input$pietype=="ReadCounts" & as.numeric(input$pieage[2])-as.numeric(input$pieage[1])>0 & input$pielog[2]-input$pielog[1]>0 & input$pielog[2]-input$pielog[1]>0){
      MergeSNP(taxonomy_table()[taxonomy_table()$SEX%in%input$piesx & taxonomy_table()$SPECIES%in%input$piesp & taxonomy_table()$AGE >= as.numeric(input$pieage[1]) & taxonomy_table()$AGE < as.numeric(input$pieage[2]) &
                                      taxonomy_table()$LATITUDE>=input$pielat[1] & taxonomy_table()$LATITUDE<=input$pielat[2] & taxonomy_table()$LONGITUDE>=input$pielog[1] & taxonomy_table()$LONGITUDE<=input$pielog[2],],snp=input$piesnp,gtp="ReadCounts")
    } else if(length(select(taxonomy_table(),starts_with("SNP_")))>0 & input$pietype=="Genotype" & as.numeric(input$pieage[2])-as.numeric(input$pieage[1])>0 & input$pielog[2]-input$pielog[1]>0 & input$pielog[2]-input$pielog[1]>0){
	MergeSNP(taxonomy_table()[taxonomy_table()$SEX%in%input$piesx & taxonomy_table()$SPECIES%in%input$piesp & taxonomy_table()$AGE >= as.numeric(input$pieage[1]) & taxonomy_table()$AGE < as.numeric(input$pieage[2]) &
                                      taxonomy_table()$LATITUDE>=input$pielat[1] & taxonomy_table()$LATITUDE<=input$pielat[2] & taxonomy_table()$LONGITUDE>=input$pielog[1] & taxonomy_table()$LONGITUDE<=input$pielog[2],],snp=input$piesnp,gtp="Genotype")
     }else{NULL}
  })
  ############labels for shown#####
  output$pieoutlab <- renderUI({
    req(filteredData1())
    if(length(select(taxonomy_table(),starts_with("SNP_")))>0 &nrow(filteredData1())>0){
      selectInput("pielab",
                  label = "Select labels",
                  choices = names(filteredData1())[names(filteredData1())%in% c("n.overlaps","origins","SecondSite")==FALSE],
                  multiple = TRUE,selected="SAMPLE")
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
        fitBounds(min(filteredData1()$LONGITUDE)-0.3,min(filteredData1()$LATITUDE)-0.3,max(filteredData1()$LONGITUDE)+0.3,max(filteredData1()$LATITUDE)+0.3)%>%
        addMinicharts(
          filteredData1()$LONGITUDE, filteredData1()$LATITUDE,
          layerId = as.character(filteredData1()$SITE),width=4,height=4) %>% addResetMapButton() %>% addMiniMap(width=100,height=100,toggleDisplay = TRUE,position = "topright")%>%
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
  
  observe({
    req(filteredData1())
    if(input$type=="bar"){
      leafletProxy("pie",data=filteredData1()) %>%
        clearMarkers() %>% clearControls() %>%
        updateMinicharts(
          layerId = filteredData1()$SITE,
          chartdata =  cbind(filteredData1() %>% select(matches(input$piesnp)),filteredData1()[,"NA"]),#cbind(filteredData1()$A,filteredData1()$D),
          maxValues = maxValue,width=15,height=15,#legend=TRUE,
          type =input$type,showLabels = input$labels,
          colorPalette = brewer.pal(11, input$colors1)[c(2,9,4,5)],
          popup = popupArgs(
            labels = c(getallename(filteredData1(),input$piesnp),"NA"),#c("A", "D"),
            html = labelpop(filteredData1(),input$pielab
            )
          )
        )
    }else if(input$type=="pie" & length(select(taxonomy_table(),starts_with("SNP_")))>0){
      leafletProxy("pie",data=filteredData1()) %>%
        clearMarkers() %>% clearControls() %>%
        updateMinicharts(
          layerId = filteredData1()$SITE,
          chartdata =  cbind(filteredData1() %>% select(matches(input$piesnp)),filteredData1()[,"NA"]),#cbind(filteredData1()$A,filteredData1()$D),
          maxValues = maxValue,
          type =input$type,showLabels = input$labels,
          colorPalette = brewer.pal(11, input$colors1)[c(2,9,4,5)],
          popup = popupArgs(
            labels = c(getallename(filteredData1(),input$piesnp),"NA"),
            html = labelpop(filteredData1(),input$pielab
            )
          ),
          width = sqrt(filteredData1()$Count)*5,transitionTime = 0
        ) %>% addLegendSize(position = "bottomleft",col="black",fillColor="white",baseSize=quantile(sqrt(filteredData1()$Count),0.85),
                            values = filteredData1()$Count,shape="circle",orientation="horizontal",breaks=5)
    } else if(length(select(taxonomy_table(),starts_with("SNP_")))>0){ 
      leafletProxy("pie",data=filteredData1()) %>%
        clearMarkers() %>% 
        updateMinicharts(
          layerId = filteredData1()$SITE,
          chartdata =  cbind(filteredData1() %>% select(matches(input$piesnp)),filteredData1()[,"NA"]),
          maxValues = maxValue,
          type =input$type,showLabels = input$labels,
          colorPalette = brewer.pal(11, input$colors1)[c(2,9,4,5)],
          popup = popupArgs(
            labels = c(getallename(filteredData1(),input$piesnp),"NA"),
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
	if(!is.null(filteredData1())){
    gridmap(filteredData1(),input$Gridpie,type="SNP")}else{NULL}
  })
  observe({
    if(input$Gridpie>0){
      leafletProxy("pie",data=griddata()) %>%
        clearMarkers() %>% clearControls() %>%
        removeMinicharts(layerId = filteredData1()$SITE) %>%
        addMinicharts(as.numeric(griddata()$LONGITUDE), as.numeric(griddata()$LATITUDE),layerId = as.character(griddata()$SITE),
                      chartdata= cbind(griddata() %>% select(matches(input$piesnp)),as.numeric(griddata()[,"NA."])),#cbind(as.numeric(griddata()$A),as.numeric(griddata()$D)),
					  type="pie",
                      colorPalette = brewer.pal(11, input$colors1)[c(2,9,4,5)],
                      popup = popupArgs(
                        labels = c(getallename(griddata(),input$piesnp),"NA"),#c("A", "D"),
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
                                     , location_coordinates = SpatialPointsDataFrame(filteredData1()[,c('LONGITUDE', 'LATITUDE')] , filteredData1())
                                     , location_id_colname = "SITE")
    for(id in found_in_bounds){
      if(id %in% data_of_click$clickedMarker){
        # don't add id
      } else {
        # add id
        data_of_click$clickedMarker<-append(data_of_click$clickedMarker, id, 0)
      }
    }
    
    selected <- subset(filteredData1(), SITE %in% data_of_click$clickedMarker)
    ######age count plot
    output$pieCount <- renderPlotly({
      req(selected)
      ggplot(selected,aes(x=AGE,y=Count))+geom_point()+ggtitle("Temporal distribution of the samples")+theme(
        plot.title = element_text(size=15, family="Arial"),plot.margin=unit(c(1,0.3,0.5,0.3),'cm'))
    })
    leafletProxy("pie",data=selected)%>% 
      addCircleMarkers(lng=selected$LONGITUDE,lat=selected$LATITUDE,
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
                                     , location_coordinates = SpatialPointsDataFrame(griddata()[,c('LONGITUDE', 'LATITUDE')] , griddata())
                                     , location_id_colname = "SITE")
    for(id in found_in_bounds1){
      if(id %in% data_of_click$clickedMarker){
        # don't add id
     } else {
        # add id
        data_of_click$clickedMarker<-append(data_of_click$clickedMarker, id, 0)
      }
    }
    
    selected <- subset(griddata(), SITE %in% data_of_click$clickedMarker)
    ######age count plot
    output$pieCount <- renderPlotly({
   #   req(selected)
      ggplot(selected,aes(x=AGE,y=Count))+geom_point()+ggtitle("Temporal distribution of the samples")+theme(
        plot.title = element_text(size=15, family="Arial"),plot.margin=unit(c(1,0.3,0.5,0.3),'cm'))
    })
    leafletProxy("pie",data=selected)%>% 
      addCircleMarkers(lng=selected$LONGITUDE,lat=selected$LATITUDE,
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
                                         , location_coordinates = SpatialPointsDataFrame(filteredData1()[,c('LONGITUDE', 'LATITUDE')] , filteredData1())
                                         , location_id_colname = "SecondSite")
      output$pieCount <- renderPlotly({
        return(NULL)
      })
      # remove second layer representing selected locations
      proxy <- leafletProxy("pie")
      proxy %>% removeMarker(layerId = as.character(bounded_layer_ids))
      first_layer_ids <- subset(filteredData1(), SecondSite %in% bounded_layer_ids)$SITE
      
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
                                         , location_coordinates = SpatialPointsDataFrame(griddata()[,c('LONGITUDE', 'LATITUDE')] , griddata())
                                         , location_id_colname = "SecondSite")
      output$pieCount <- renderPlotly({
        return(NULL)
      })
      # remove second layer representing selected locations
      proxy <- leafletProxy("pie")
      proxy %>% removeMarker(layerId = as.character(bounded_layer_ids))
      first_layer_ids <- subset(griddata(), SecondSite %in% bounded_layer_ids)$SITE
      
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
    gridplot(filteredData1(),as.numeric(input$pieage[1]),as.numeric(input$pieage[2]),input$GridSize,type="SNP",comp=NULL,outpath)
  })
  
  ###################draw allele##########################  
  ########## Render UIs for Panel 5 (allele)##############
  output$alloutsp <- renderUI({
    req(taxonomy_table())
    selectInput("allsp",
                label = "Select species",
                choices = unique(taxonomy_table()$SPECIES),
                multiple = TRUE,selected=unique(taxonomy_table()$SPECIES)[1])
  })
  output$alloutsx <- renderUI({
    req(taxonomy_table())
    selectInput("allsx",
                label = "Select sex",
                choices = unique(taxonomy_table()$SEX),
                multiple = TRUE,selected=unique(taxonomy_table()$SEX)[1])
  })
  output$allout <- renderUI({
    sliderInput("alleage",
                label = "Select a time range",
                min = 0,
                max = taxonomy_table()$AGE %>%
                  as.numeric %>%
                  max(na.rm = TRUE),
                step = 10,
                value = range(taxonomy_table()$AGE),dragRange=TRUE)
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
                label = "Select Allele",
                choices = c(Choose='',getallename(taxonomy_table(),input$allsnp)),
				selectize=FALSE,#getallename(taxonomy_table(),input$allsnp)[1],
                selected='')
				}
  })

  output$alloutlog <- renderUI({
    sliderInput("alllog",
                label = "Select a longitude range",
                min = taxonomy_table()$LONGITUDE %>%
                  as.numeric() %>% 
                  #round(2) %>%
                  min(na.rm = TRUE),
                max = taxonomy_table()$LONGITUDE %>%
                  as.numeric %>% 
                 # round(2) %>%
                  max(na.rm = TRUE),
                step = 0.01,
                value = range(taxonomy_table()$LONGITUDE),dragRange=TRUE)
  })
  output$alloutlat <- renderUI({
    sliderInput("alllat",
                label = "Select a latitude range",
                min = taxonomy_table()$LATITUDE %>%
                  as.numeric() %>% 
                  #round(2) %>%
                  min(na.rm = TRUE),
                max = taxonomy_table()$LATITUDE %>%
                  as.numeric %>%
                  #round(2) %>%
                  max(na.rm = TRUE),
                step = 0.01,
                value = range(taxonomy_table()$LATITUDE))
  })
  output$alloutymin <- renderUI({
    numericInput("allymin",
                 label = "Input the min y",
                 min = -1,
                 max = 1,value=0
    )
  })
  output$alloutymax <- renderUI({
    numericInput("allymax",
                 label = "Input the max y",
                 min = -1,
                 max = 2,value=1
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
    if(length(select(taxonomy_table(),starts_with("SNP_")))>0 & input$alltype=="ReadCounts" & input$allall%in%getallename(taxonomy_table(),input$allsnp) & input$WinSize>input$StepSize & input$StepSize>0 & input$alleage[2]-input$alleage[1]>0){
	req(input$allsampling)
      AllePlot(taxonomy_table(),as.character(input$allsx),as.character(input$allsp),input$allall,as.numeric(input$WinSize),as.numeric(input$StepSize),as.numeric(input$alleage[1]),as.numeric(input$alleage[2]),input$allsnp,
               input$alllat[1],input$alllat[2],input$alllog[1],input$alllog[2],input$allymin,input$allymax,input$allsampling,gtp=input$alltype)
    }else if(input$alltype=="Genotype" & length(select(taxonomy_table(),starts_with("SNP_")))>0 & input$allall%in%getallename(taxonomy_table(),input$allsnp) & input$WinSize>input$StepSize & input$StepSize>0 & input$alleage[2]-input$alleage[1]>0){
	 AllePlot(taxonomy_table(),as.character(input$allsx),as.character(input$allsp),input$allall,as.numeric(input$WinSize),as.numeric(input$StepSize),as.numeric(input$alleage[1]),as.numeric(input$alleage[2]),input$allsnp,
               input$alllat[1],input$alllat[2],input$alllog[1],input$alllog[2],input$allymin,input$allymax,gtp=input$alltype)
}else if(!input$allall%in%getallename(taxonomy_table(),input$allsnp)){NULL}

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
    if(length(select(taxonomy_table(),starts_with("SNP_")))>0 & length(input$allall)==1 & input$allall%in%getallename(taxonomy_table(),input$allsnp)){
      allname=paste("SNP_",input$allsnp,"_",input$allall,sep="")
	   addreadCounts(taxonomy_table()[taxonomy_table()$SEX%in%input$allsx & taxonomy_table()$SPECIES%in%input$allsp & taxonomy_table()$AGE >= as.numeric(input$alleage[1]) & taxonomy_table()$AGE < as.numeric(input$alleage[2]) & taxonomy_table()[,allname]>0 &
                          taxonomy_table()$LATITUDE >= as.numeric(input$alllat[1]) & taxonomy_table()$LATITUDE <= input$alllat[2] & taxonomy_table()$LONGITUDE>=as.numeric(input$alllog[1]) & taxonomy_table()$LONGITUDE<=as.numeric(input$alllog[2]),],input$allsnp)
    }else{NULL}
  })

  ##########draw map
  output$deallele <- renderLeaflet({
  req(input$allsnp,input$allall)
    if(length(select(taxonomy_table(),starts_with("SNP_")))>0 & input$allall%in%getallename(taxonomy_table(),input$allsnp)){
      req(filteredData3())
      leaflet(options = leafletOptions(zoomControl = FALSE, minZoom = 1, maxZoom = 5, dragging = T)) %>%
        addTiles(tilesURL) %>%#addControl(titletraj, position = "topleft") %>%
        fitBounds(min(taxonomy_table()$LONGITUDE)-0.3,min(taxonomy_table()$LATITUDE)-0.3,max(taxonomy_table()$LONGITUDE)+0.3,max(taxonomy_table()$LATITUDE)+0.3) %>%
        addCircleMarkers(filteredData3()$LONGITUDE, filteredData3()$LATITUDE,radius=filteredData3()[,paste("SNP_",input$allsnp,"_",input$allall,sep="")]) %>% addResetMapButton()%>% addMiniMap(width=100,height=100,toggleDisplay = TRUE,position = "topright")
    }else if(!input$allall%in%getallename(taxonomy_table(),input$allsnp)){NULL}
  })
   observe({ 
      req(filteredData3())
      #pal <- colorpal2()
      leafletProxy("deallele",data=filteredData3()) %>%
        clearMarkers() %>% clearShapes() %>% 
        addAwesomeMarkers(filteredData3()$LONGITUDE, filteredData3()$LATITUDE,clusterOptions = markerClusterOptions(),#radius=~as.numeric(D)*5,color = "#777777",fillColor=~pal(Age),fillOpacity = 0.9, 
                          popup = paste0(filteredData3()$SAMPLE,"<br>","Sex: ",filteredData3()$SEX,"<br>","Age: ",filteredData3()$AGE,"<br>","Rs/Total:",round(filteredData3()[,paste("SNP_",input$allsnp,"_",input$allall,sep="")],2),"/",filteredData3()$ReadCount)
        )  
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
                choices = unique(taxonomy_table()$SPECIES),
                multiple = TRUE,selected=unique(taxonomy_table()$SPECIES)[1])
  })
  output$ancesoutsx <- renderUI({
    req(taxonomy_table())
    selectInput("ancessx",
                label = "Select sex",
                choices = unique(taxonomy_table()$SEX),
                multiple = TRUE,selected=unique(taxonomy_table()$SEX)[1])
  })
  output$ancesout <- renderUI({
    sliderInput("anceage",
                label = "Select a time span",
                min =0, #taxonomy_table()$AGE %>%
                #as.numeric() %>%
                # min(na.rm = TRUE),
                max = taxonomy_table()$AGE %>%
                  as.numeric %>%
                  max(na.rm = TRUE),
                step = 10,
                value = range(taxonomy_table()$AGE))
  })
  output$ancesout1 <- renderUI({
    if(length(grep("ANC",names(taxonomy_table())))>0){
      selectInput("ancecomp",
                  label = "Select ancestral origins",
                  choices = names(taxonomy_table())[grep("ANC",names(taxonomy_table()))],
                  multiple = TRUE,selected="ANC1")
    }
  })
  output$ancesoutcut <- renderUI({
    sliderInput("ancecut",
                label = "Time Split",
                min = taxonomy_table()$AGE %>%
                  as.numeric() %>%
                  min(na.rm = TRUE),
                max = taxonomy_table()$AGE %>%
                  as.numeric %>%
                  max(na.rm = TRUE),
                value=median(taxonomy_table()$AGE)%>%
                  as.numeric() 
    )
  })
  
  output$errance <- renderText({
    if(length(grep("ANC",names(taxonomy_table())))==0){
      validate("The required data were not part of your Input File. Please make sure that columns labelled 'ANC' are provided")
    }
  })
  #############active data
  filteredData2T <- reactive({
    req(input$anceage)
    req(input$ancecut)
    if(length(grep("ANC",names(taxonomy_table())))>0 & as.numeric(input$anceage[2])-as.numeric(input$anceage[1])>0){
      Mergetype(taxonomy_table()[taxonomy_table()$SEX%in%input$ancessx & taxonomy_table()$SPECIES%in%input$ancessp & taxonomy_table()$AGE >= as.numeric(input$anceage[1]) & taxonomy_table()$AGE < as.numeric(input$anceage[2]),],type="ANC")
    }else{NULL}
  })
  output$ancesoutlab <- renderUI({
    req(filteredData2T())
    selectInput("anceslab",
                label = "Select labels",
                choices = names(filteredData2T())[names(filteredData2T())%in% c("n.overlaps","origins","SecondSite")==FALSE],
                multiple = TRUE,selected="SAMPLE")
  })
  filteredData2 <- reactive({
    req(input$anceage)
    req(input$ancecut)
    if(length(grep("ANC",names(taxonomy_table())))>0 & as.numeric(input$ancecut)-as.numeric(input$anceage[2])<0 & as.numeric(input$ancecut)-as.numeric(input$anceage[1])>0 & as.numeric(input$anceage[2])>as.numeric(input$anceage[1])){
      Mergetype(taxonomy_table()[taxonomy_table()$SEX%in%input$ancessx & taxonomy_table()$SPECIES%in%input$ancessp & taxonomy_table()$AGE >= as.numeric(input$anceage[1]) & taxonomy_table()$AGE < as.numeric(input$anceage[2]) & taxonomy_table()$AGE >= as.numeric(input$ancecut),],type="ANC")
    }else{NULL}
  })
  filteredData22 <- reactive({
    req(input$anceage)
    req(input$ancecut)
    if(length(grep("ANC",names(taxonomy_table())))>0 &  as.numeric(input$ancecut)>input$anceage[1] &  as.numeric(input$ancecut)<input$anceage[2]){
      Mergetype(taxonomy_table()[taxonomy_table()$SEX%in%input$ancessx & taxonomy_table()$SPECIES%in%input$ancessp & taxonomy_table()$AGE > as.numeric(input$anceage[1]) & taxonomy_table()$AGE <= as.numeric(input$anceage[2]) & taxonomy_table()$AGE< as.numeric(input$ancecut),],type="ANC")
    }else{NULL}
  })
  
  ###########output ance map
  output$drawance <- renderLeaflet({
   req(filteredData2T(),filteredData2())
    if(nrow(filteredData2())>0 & nrow(filteredData2T())>0){
    leaflet(options = leafletOptions(zoomControl = FALSE, minZoom = 2.48, maxZoom = 10, dragging = T)) %>%
      addTiles(tilesURL) %>% 
      fitBounds(min(filteredData2T()$LONGITUDE)+1,min(filteredData2T()$LATITUDE),max(filteredData2T()$LONGITUDE)-1,max(filteredData2T()$LATITUDE)-0.5) %>%
      addMinicharts(
        filteredData2()$LONGITUDE, filteredData2()$LATITUDE,
        layerId = filteredData2()$SITE,width=4,height=4) %>% addResetMapButton()%>% addMiniMap(width=100,height=100,toggleDisplay = TRUE,position = "bottomright")
    }else{NULL}
  })
  observe({
  filteredData2()
    if (length(input$ancecomp) == 0) {
      data2 <- 1
    } else if(length(input$ancecomp) <length(grep("ANC",names(taxonomy_table()))) &!is.null(filteredData2())){
      data2=(cbind(filteredData2()[,input$ancecomp],(1-apply(as.data.frame(filteredData2()[,input$ancecomp]),1,sum))))
      colnames(data2)=c(input$ancecomp,"NA")
    }else if(length(input$ancecomp) ==length(grep("ANC",names(taxonomy_table())))&!is.null(filteredData2())){
      data2 <- filteredData2()[,input$ancecomp]
    }
    maxValue <- 1#max(as.matrix(data22))
    if(input$type1=="bar" &!is.null(filteredData2())){
      leafletProxy("drawance") %>%
        clearMarkers() %>%
        updateMinicharts(
          layerId = filteredData2()$SITE,
          chartdata =  data2,
          maxValues = maxValue,width=15,height=15,
          type =input$type1,showLabels = input$labels1,legend=TRUE,
          colorPalette = brewer.pal(11, input$colors2)[c(2,5,11,1,9,3,8,4)],
          popup = popupArgs(
            labels =  names(data2),
            html = labelpop(filteredData2(),input$anceslab
            )
          )
        )
    }else if(input$type1=="pie" & length(grep("ANC",names(taxonomy_table())))>0 &!is.null(filteredData2())){
      leafletProxy("drawance") %>%
        clearMarkers() %>% clearControls() %>%
        updateMinicharts(
          layerId = filteredData2()$SITE,
          chartdata =  data2,
          maxValues = maxValue,
          type ="pie",showLabels = input$labels1,legend=TRUE,legendPosition="bottomright",
          colorPalette = brewer.pal(11, input$colors2)[c(2,5,11,1,9,3,8,4)],
          popup = popupArgs(
            #labels = input$ancecomp,
            html = labelpop(filteredData2(),input$anceslab
            )
          ),
          width = sqrt(filteredData2()$Count)*12,transitionTime = 0
        )   
    }else if(length(select(taxonomy_table(),starts_with("ANC")))>0 &!is.null(filteredData2())){
      leafletProxy("drawance") %>%
        clearMarkers() %>%
        updateMinicharts(
          layerId = filteredData2()$SITE,
          chartdata =  data2,
          maxValues = maxValue,
          type =input$type1,showLabels = input$labels1,legend=TRUE,
          colorPalette = brewer.pal(11, input$colors2)[c(2,5,11,1,9,3,8,4)],
          popup = popupArgs(
            labels = input$ancescomp,
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
    req(input$Gridanc,input$ancecomp)
	if(length(filteredData2())>1 &input$Gridanc>0&input$anceage[2]-input$anceage[1]>0){
    gridmap(filteredData2(),input$Gridanc,type="ANC",comp=input$ancecomp)
	}else{NULL}
  })

  observe({
  req(griddataanc1())
    if(input$Gridanc>0 & length(input$ancecomp)==length(grep("ANC",names(filteredData2())))){
      leafletProxy("drawance",data=griddataanc1()) %>%
        clearMarkers() %>% clearControls() %>%
        removeMinicharts(layerId = filteredData2()$SITE) %>%addGraticule(interval = input$Gridanc, style = list(color = "#FF0000", weight = 0.1,fillOpacity=0.1)) %>%
        addMinicharts(as.numeric(griddataanc1()$LONGITUDE), as.numeric(griddataanc1()$LATITUDE),
                      chartdata=data.frame(lapply(griddataanc1()[,grep("ANC",names(griddataanc1()),value=TRUE)],as.numeric)),type="pie",
                      colorPalette = brewer.pal(11, input$colors2)[c(2,5,11,1,9,3,8,4)],
                      width = 10*sqrt(as.numeric(griddataanc1()$Count)))
    }else if(input$Gridanc>0 & length(input$ancecomp)>0 &length(input$ancecomp)<length(grep("ANC",names(filteredData2())))){
	leafletProxy("drawance",data=griddataanc1()) %>%
        clearMarkers() %>% clearControls() %>%
        removeMinicharts(layerId = filteredData2()$SITE)%>%
        addGraticule(interval = input$Gridanc, style = list(color = "#FF0000", weight = 0.1,fillOpacity=0.1)) %>%
        addMinicharts(as.numeric(griddataanc1()$LONGITUDE), as.numeric(griddataanc1()$LATITUDE),
                      chartdata=data.frame(lapply(griddataanc1()[,c(grep("ANC",names(griddataanc1()),value=TRUE),"NA")],as.numeric)),type="pie",
                      colorPalette = brewer.pal(11, input$colors2)[c(2,5,11,1,9,3,8,4)],
                      width = 10*sqrt(as.numeric(griddataanc1()$Count)))
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
    req(filteredData2T(),filteredData22())
    if(!is.null(filteredData2T()) & !is.null(filteredData22())){
    leaflet(options = leafletOptions(zoomControl = FALSE, minZoom = 2.48, maxZoom = 10, dragging = T)) %>%
      addTiles(tilesURL) %>%
      fitBounds(min(filteredData2T()$LONGITUDE)+1,min(filteredData2T()$LATITUDE),max(filteredData2T()$LONGITUDE)-1,max(filteredData2T()$LATITUDE)-0.5) %>%
      addMinicharts(
        filteredData22()$LONGITUDE, filteredData22()$LATITUDE,
        layerId = filteredData22()$SITE,width=4,height=4) %>% addResetMapButton()%>% addMiniMap(width=100,height=100,toggleDisplay = TRUE,position = "bottomright")
  }else{leaflet(data=NULL)}
  })
  observe({
  req(filteredData22())
    if (length(input$ancecomp) == 0) {
      data22 <- 1
    } else if(length(input$ancecomp) <length(grep("ANC",names(taxonomy_table()))) &!is.null(filteredData22())){
      data22=(cbind(filteredData22()[,input$ancecomp],(1-apply(as.data.frame(filteredData22()[,input$ancecomp]),1,sum))))
      colnames(data22)=c(input$ancecomp,"NA")
    } else if( length(input$ancecomp) ==length(grep("ANC",names(taxonomy_table())))&!is.null(filteredData22())){
      data22 <- filteredData22()[,input$ancecomp]
    }
    maxValue <- 1
    if(input$type1=="bar" & !is.null(filteredData2T()) &!is.null(filteredData22())){
      leafletProxy("drawance2") %>%
        clearMarkers() %>%
        updateMinicharts(
          layerId = filteredData22()$SITE,
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
    }else if(input$type1=="pie" & length(grep("ANC",names(taxonomy_table())))>0  &!is.null(filteredData2T()) &!is.null(filteredData22())){
      leafletProxy("drawance2") %>%
        clearMarkers() %>% clearControls() %>%
        updateMinicharts(
          layerId = filteredData22()$SITE,
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
        )  
    }else if(length(select(taxonomy_table(),starts_with("ANC")))>0 &!is.null(filteredData2T()) &!is.null(filteredData22())){
      leafletProxy("drawance2") %>%
        clearMarkers() %>%
        updateMinicharts(
          layerId = filteredData22()$SITE,
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
    req(input$Gridanc,input$ancecomp)
	if(length(filteredData22())>0 &input$Gridanc>0 &input$anceage[2]-input$anceage[1]>0){
    gridmap(filteredData22(),input$Gridanc,type="ANC",comp=input$ancecomp)
	}else{NULL}
  })
  observe({
   req(griddataanc2())
   req(filteredData22())
    if(input$Gridanc>0 &length(input$ancecomp)==length(grep("ANC",names(filteredData22())))){
      leafletProxy("drawance2",data=griddataanc2()) %>%
        clearMarkers() %>% clearControls() %>%
        removeMinicharts(layerId = filteredData22()$SITE) %>%
		addGraticule(interval = input$Gridanc, style = list(color = "#FF0000", weight = 0.1,fillOpacity=0.1)) %>%
        addMinicharts(as.numeric(griddataanc2()$LONGITUDE), as.numeric(griddataanc2()$LATITUDE),
                      chartdata=data.frame(lapply(griddataanc2()[,input$ancecomp],as.numeric)),type="pie",
                      colorPalette = brewer.pal(11, input$colors2)[c(2,5,11,1,9,3,8,4)],
                      width = 10*sqrt(as.numeric(griddataanc2()$Count)))
    }else if(input$Gridanc>0 &length(input$ancecomp)<length(grep("ANC",names(filteredData22())))){
      leafletProxy("drawance2",data=griddataanc2()) %>%
        clearMarkers() %>% clearControls() %>%
        removeMinicharts(layerId = filteredData22()$SITE) %>%
        addMinicharts(as.numeric(griddataanc2()$LONGITUDE), as.numeric(griddataanc2()$LATITUDE),
                      chartdata=data.frame(lapply(griddataanc2()[,c(grep("ANC",names(griddataanc2()),value=TRUE),"NA")],as.numeric)),type="pie",
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
	if(input$anceage[2]-input$anceage[1]>0){
    gridplot(filteredData2T(),as.numeric(input$anceage[1]),as.numeric(input$anceage[2]),input$GridSizea,type="ANC",comp=input$ancecomp,outpath)
	}else{NULL}
  })
  
  ##draw count plot
  output$drawanceCount <- renderPlotly({
    req(filteredData2())
    req(filteredData22())
    dataplot=rbind(filteredData2(),filteredData22())
    dataplot$type=NA
    dataplot[dataplot$AGE>=input$ancecut,"type"]=paste("Before",input$ancecut,sep="")
    dataplot[dataplot$AGE<input$ancecut,"type"]=paste("After",input$ancecut,sep="")
    ggplot(dataplot,aes(x=AGE,y=Count,color=type))+geom_point()+ggtitle("Temporal distribution of the samples")+theme(
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
                choices = unique(taxonomy_table()$SPECIES),
                multiple = TRUE,selected=unique(taxonomy_table()$SPECIES)[1])
  })
  output$pcaoutsx <- renderUI({
    req(taxonomy_table())
    selectInput("pcasx",
                label = "Select sex",
                choices = unique(taxonomy_table()$SEX),
                multiple = TRUE,selected=unique(taxonomy_table()$SEX)[1])
  })
  output$pcaout <- renderUI({
    sliderInput("pcaage",
                label = "Select a time range",
                min = 0,
                max = taxonomy_table()$AGE %>%
                  as.numeric %>%
                  max(na.rm = TRUE),
                step = 100,
                value = range(taxonomy_table()$AGE))
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
                min = taxonomy_table()$LONGITUDE %>%
                  as.numeric() %>%
                  # round(2) %>%
                  min(na.rm = TRUE),
                max = taxonomy_table()$LONGITUDE %>%
                  as.numeric %>%
                  #round(2) %>%
                  max(na.rm = TRUE),
                step = 0.01,
                value = range(taxonomy_table()$LONGITUDE))
  })
  output$pcaoutlat <- renderUI({
    sliderInput("pcalat",
                label = "Select a latitude range",
                min = taxonomy_table()$LATITUDE %>%
                  as.numeric() %>%
                  min(na.rm = TRUE),
                max = taxonomy_table()$LATITUDE %>%
                  as.numeric %>%
                  max(na.rm = TRUE),
                step = 0.01,
                value = range(taxonomy_table()$LATITUDE))
  })
  ############draw pca#########################
  output$pca <- renderPlotly({
    req(taxonomy_table())
    req(PCA_table())
    drawpca(taxonomy_table(),as.character(input$pcasx),as.character(input$pcasp),as.numeric(input$pcaage[1]),as.numeric(input$pcaage[2]),PCA_table(),input$pcalat[1],input$pcalat[2],input$pcalog[1],input$pcalog[2],(input$PC1),(input$PC2))
  })
  #######reactiveData#######################
  filteredData4 <- reactive({
    req(input$pcaage)
	if(input$pcaage[2]-input$pcaage[1]>0 & input$pcalog[2]-input$pcalog[1]>0 & input$pcalat[2]-input$pcalat[1]>0){
    Merge(taxonomy_table()[taxonomy_table()$SEX%in%input$pcasx & taxonomy_table()$SPECIES%in%input$pcasp & taxonomy_table()$AGE >= as.numeric(input$pcaage[1]) & taxonomy_table()$AGE < as.numeric(input$pcaage[2]) & taxonomy_table()$LONGITUDE >=as.numeric(input$pcalog[1]) & taxonomy_table()$LONGITUDE <=as.numeric(input$pcalog[2]) & taxonomy_table()$LATITUDE >=as.numeric(input$pcalat[1]) &taxonomy_table()$LATITUDE <=as.numeric(input$pcalat[2]),])
  }else{NULL}
  })
  #color 
  colorpal1 <- reactive({
    colorNumeric(brewer_pal(palette = "RdYlGn")(11), filteredData4()$AGE)
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
      fitBounds(min(taxonomy_table()$LONGITUDE),min(taxonomy_table()$LATITUDE),max(taxonomy_table()$LONGITUDE),max(taxonomy_table()$LATITUDE)) %>%
      addCircleMarkers(filteredData4()$LONGITUDE, filteredData4()$LATITUDE,radius=sqrt(filteredData4()$Count)*2
                       ,weight=1,popup=paste0(filteredData4()$SAMPLE),color = "#777777",fillColor = pal(filteredData4()$AGE), fillOpacity = 0.9,stroke = FALSE) %>% 
      addResetMapButton() %>% addMiniMap(width=100,height=100,toggleDisplay = TRUE,position = "topright") %>%
      addLegendSize(position = "bottomleft",col="black",fillColor="white",baseSize=min(sizeNumeric(filteredData4()$Count*2,baseSize = mean(filteredData4()$Count)*2.5)) ,values = sizeNumeric(filteredData4()$Count,baseSize = mean(filteredData4()$Count)),shape="circle",orientation="horizontal",breaks=5)
  })
  
  ##################multiple SNPs########################
  #######render UI for multiple SNPs##################
  output$snpoutsp <- renderUI({
    req(taxonomy_table())
    selectInput("snpsp",
                label = "Select Species",
                choices = unique(taxonomy_table()$SPECIES),
                multiple = TRUE,selected=unique(taxonomy_table()$SPECIES)[1])
  })
  output$snpoutsx <- renderUI({
    req(taxonomy_table())
    selectInput("snpsx",
                label = "Select sex",
                choices = unique(taxonomy_table()$SEX),
                multiple = TRUE,selected=unique(taxonomy_table()$SEX)[1])
  })

  output$snpoutage <- renderUI({
    req(taxonomy_table())
    sliderInput("snpage",
                label = "Select a time range",
                min = 0,
                max = taxonomy_table()$AGE %>% 
                  as.numeric %>%
                  max(na.rm = TRUE),
                step = 10,
                value = range(taxonomy_table()$AGE))
  })
  output$snpoutsnp <- renderUI({
    selectInput("snpsnp",
                label = "Select your variations",
                choices = unique(names(taxonomy_table() %>% select(starts_with("SNP_")))),
                multiple = TRUE)
  })
  output$snpoutlog <- renderUI({
   req(taxonomy_table())
    sliderInput("snplog",
                label = "Select a longitude range",
                min = taxonomy_table()$LONGITUDE %>%
                  as.numeric() %>%
                  min(na.rm = TRUE),
                max = taxonomy_table()$LONGITUDE%>%
				as.numeric %>%
                  max(na.rm = TRUE),
                step = 0.01,
                value = range(as.numeric(taxonomy_table()$LONGITUDE)))
  })
  output$snpoutlat <- renderUI({
  req(taxonomy_table())
    sliderInput("snplat",
                label = "Select a latitude range",
                min = taxonomy_table()$LATITUDE %>%
                  as.numeric() %>%
                  min(na.rm = TRUE),
                max = taxonomy_table()$LATITUDE %>%
				as.numeric %>%
                  max(na.rm = TRUE),
                step = 0.01,
                value = range(as.numeric(taxonomy_table()$LATITUDE)))
				})
  output$errmultsnp <- renderText({
    if(length(select(taxonomy_table(),starts_with("SNP_")))==0){
      validate("The required data were not part of your Input File. Please make sure that columns labelled 'SNP_snpname_A' and 'SNP_snpname_D' are provided")
    }
  })
  ##active data
  filteredData5 <- reactive({
    req(input$snpage,taxonomy_table())
	req(input$alltypesnp,input$snplog,input$snplat,input$snpsnp)
    if(length(select(taxonomy_table(),starts_with("SNP_")))>0 & input$alltypesnp=="ReadCounts" & length(input$snpsnp)>0 & input$snpage[2]-input$snpage[1]>0 & input$snplog[2]-input$snplog[1]>0 & input$snplat[2]-input$snplat[1]>0){
      MergeawsomeCount(taxonomy_table()[taxonomy_table()$SEX%in%input$snpsx & taxonomy_table()$SPECIES%in%input$snpsp & taxonomy_table()$AGE >= as.numeric(input$snpage[1]) & taxonomy_table()$AGE <= as.numeric(input$snpage[2])
                                   & taxonomy_table()$LONGITUDE>=input$snplog[1] & taxonomy_table()$LONGITUDE<=input$snplog[2] & taxonomy_table()$LATITUDE>=input$snplat[1] & taxonomy_table()$LATITUDE<=input$snplat[2],],mult=TRUE,snplist=c(input$snpsnp))
    }else if(input$alltypesnp=="Genotype" & input$snpage[2]-input$snpage[1]>0 & input$snplog[2]-input$snplog[1]>0 & input$snplat[2]-input$snplat[1]>0){
	Mergeawsome(taxonomy_table()[taxonomy_table()$SEX%in%input$snpsx & taxonomy_table()$SPECIES%in%input$snpsp & taxonomy_table()$AGE >= as.numeric(input$snpage[1]) & taxonomy_table()$AGE <= as.numeric(input$snpage[2])
                                   & taxonomy_table()$LONGITUDE>=input$snplog[1] & taxonomy_table()$LONGITUDE<=input$snplog[2] & taxonomy_table()$LATITUDE>=input$snplat[1] & taxonomy_table()$LATITUDE<=input$snplat[2],],mult=TRUE,snplist=c(input$snpsnp))
	}
	else{
      return (NULL)
      #stop("Please make sure that all required columns are provided in the uploaded table!")
    }
  })

  output$snpoutlab <- renderUI({
    req(filteredData5())
	if(nrow(filteredData5())>0){
    selectInput("snplab",
                label = "Select labels",
                choices =  names(filteredData5())[names(filteredData5())%in% c("n.overlaps","origins","SecondSite")==FALSE],
                multiple = TRUE,selected="SAMPLE")
  }
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
   req(filteredData5())
    if(length(select(taxonomy_table(),starts_with("SNP_")))>0 & nrow(filteredData5())>0){
      leaflet(options = leafletOptions(zoomControl = FALSE, minZoom = 2, maxZoom = 10, dragging = T)) %>%
        addTiles(tilesURL) %>%
        fitBounds(min(filteredData5T()$LONGITUDE)-0.5,min(filteredData5T()$LATITUDE)-0.3,max(filteredData5T()$LONGITUDE)+0.5,max(filteredData5T()$LATITUDE)+0.3) %>%
        addMinicharts(
          filteredData5T()$LONGITUDE, filteredData5T()$LATITUDE,
          layerId = filteredData5T()$SAMPLE) %>% addResetMapButton() %>% addMiniMap(width=100,height=100,toggleDisplay = TRUE,position = "topright")
    }
  })

  observe({
  req(filteredData5T())
    if (length(input$snpsnp) == 0) {
      datasnp <- 1
    }else if(length(input$snpsnp) == 1){
	datasnp=filteredData5T()[,input$snpsnp]
	} 
    else if(length(input$snpsnp) > 1) {
      datasnp <- filteredData5T()[,input$snpsnp]#as.data.frame(filteredData5()[rowSums(as.data.frame(filteredData5()[,input$snpsnp]))>0,input$snpsnp])
	  
    }
    maxValue <- max(as.matrix(datasnp))
    if(input$type3=="bar" & length(nrow(datasnp)>0)){
      leafletProxy("snpmap") %>%
        clearMarkers() %>%
        updateMinicharts(
          layerId = filteredData5T()$SAMPLE,
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
    }#else if(input$type3=="pie" & length(select(taxonomy_table(),starts_with("SNP_")))>0){
     # leafletProxy("snpmap") %>%
      #  clearMarkers() %>% clearControls() %>%
       # updateMinicharts(
        #  layerId = filteredData5T()$SAMPLE,
        #  chartdata =  datasnp,
         # maxValues = maxValue,
         # type ="pie",showLabels = input$labels3,legend=TRUE,legendPosition="bottomright",
         # colorPalette = brewer.pal(9, input$colors3)[c(2,9,1,7,3,8,4,8,1,8,3,9,2,6,4,1,7,3,9,2,8,4)],
        #  popup = popupArgs(
        #    labels = colnames(datasnp),
          #  html = labelpop(filteredData5T(),input$snplab)),
          #width = sqrt(filteredData5()$Counts)*5,transitionTime = 0
       # )}
    else if(length(select(taxonomy_table(),starts_with("SNP_")))>0){
      leafletProxy("snpmap") %>%
        clearMarkers() %>%
        updateMinicharts(
          layerId = filteredData5T()$SAMPLE,
          chartdata =  datasnp,
          maxValues = maxValue,
          type =input$type3,showLabels = input$labels3,legend=TRUE,
          colorPalette = brewer.pal(9, input$colors3)[c(2,9,1,7,3,8,4,8,1,8,3,9,2,6,4,1,7,3,9,2,8,4)],
          popup = popupArgs(
            labels = colnames(datasnp),
            html = labelpop(filteredData5T(),input$snplab
            )
          ),
          width = (filteredData5T()$Counts)*2,transitionTime = 0
        )
    }
  })
  
  ##draw count plot
  output$snpCount <- renderPlotly({
    req(filteredData5T())
	req(input$snpsnp)
	ggplot(filteredData5T(),aes(x=AGE,y=Counts))+ggtitle("Temporal distribution of the samples")+theme(
      plot.title = element_text(size=15, family="Arial"))+geom_point()#geom_bar(stat="identity",fill="steelblue")
  })
  
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
                choices = unique(taxonomy_table()$SPECIES),
                multiple = TRUE,selected=unique(taxonomy_table()$SPECIES))
  })
  output$hapoutsx <- renderUI({
    req(taxonomy_table())
	req(input$hapty)
      if(length(input$hapty)==1){
	selectInput("hapsx",
                  label = "Select sex",
                  choices = c(Choose='',unique(taxonomy_table()[taxonomy_table()[,paste("CAT_",input$hapty,sep="")]!="unknown","SEX"])),#unique(taxonomy_table()$SEX)
                  multiple = TRUE,selected=unique(taxonomy_table()[taxonomy_table()[,paste("CAT_",input$hapty,sep="")]!="unknown","SEX"])[1])
	}
  })
  output$hapoutage <- renderUI({
    req(taxonomy_table())
    sliderInput("hapage",
                label = "Select a time range",
                min = 0,
                max = taxonomy_table()$AGE %>%
                  as.numeric %>%
                  max(na.rm = TRUE),
                step = 10,
                value = range(taxonomy_table()$AGE))
  })
   output$hapoutlog <- renderUI({
    sliderInput("haplog",
                label = "Select a longitude range",
                min = taxonomy_table()$LONGITUDE %>%
                  as.numeric() %>%
                  min(na.rm = TRUE),
                max = taxonomy_table()$LONGITUDE %>%
                  as.numeric %>%
                  max(na.rm = TRUE),
                step = 0.01,
                value = range(taxonomy_table()$LONGITUDE))
  })
  output$hapoutlat <- renderUI({
    sliderInput("haplat",
                label = "Select a latitude range",
                min = taxonomy_table()$LATITUDE %>%
                  as.numeric() %>%
                  min(na.rm = TRUE),
                max = taxonomy_table()$LATITUDE %>%
                  as.numeric %>%
                  max(na.rm = TRUE),
                step = 0.01,
                value = range(taxonomy_table()$LATITUDE))
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
     if(length(select(taxonomy_table(),starts_with("CAT_")))>0& length(input$hapty)==1 & input$hapage[2]-input$hapage[1]>0 & input$haplog[2]-input$haplog[1]>0 & input$haplat[2]-input$haplat[1]>0){
       MergeMT(taxonomy_table()[taxonomy_table()$SEX%in%input$hapsx & 
	 taxonomy_table()$SPECIES%in%input$hapsp & taxonomy_table()$AGE >= as.numeric(input$hapage[1]) &  taxonomy_table()$AGE < as.numeric(input$hapage[2]) &
	 taxonomy_table()$LATITUDE >= as.numeric(input$haplat[1]) &  taxonomy_table()$LATITUDE < as.numeric(input$haplat[2]) &
	 taxonomy_table()$LONGITUDE >= as.numeric(input$haplog[1]) &  taxonomy_table()$LONGITUDE < as.numeric(input$haplog[2]),],type=input$hapty)
	}else{NULL}
  })
  output$hapoutlab <- renderUI({
    req(filteredData6())
    selectInput("haplab",
                label = "Select labels",
                choices = names(filteredData6())[names(filteredData6())%in% c("n.overlaps","origins","SecondSite")==FALSE],
                multiple = TRUE,selected="SAMPLE")
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
   req(filteredData6())
       leaflet(options = leafletOptions(zoomControl = FALSE, minZoom = 2, maxZoom = 10, dragging = T)) %>%
        addTiles(tilesURL) %>%
        fitBounds(min(filteredData6()$LONGITUDE)-0.3,min(filteredData6()$LATITUDE)-0.3,max(filteredData6()$LONGITUDE)+0.3,max(filteredData6()$LATITUDE)+0.3) %>%
        addMinicharts(filteredData6()$LONGITUDE, filteredData6()$LATITUDE,
                      layerId = filteredData6()$SITE,width=4,height=4) %>% addResetMapButton()%>% addMiniMap(width=100,height=100,toggleDisplay = TRUE,position = "topleft") %>%
        addDrawToolbar(
          targetGroup='Selected',
          polylineOptions=FALSE,
          markerOptions = FALSE,
          polygonOptions = drawPolygonOptions(shapeOptions=drawShapeOptions(fillOpacity = 0,color = 'white',weight = 3)),
          rectangleOptions = drawRectangleOptions(shapeOptions=drawShapeOptions(fillOpacity = 0 ,color = 'white',weight = 3)),
          circleOptions = FALSE,circleMarkerOptions = FALSE,
          editOptions = editToolbarOptions(edit = FALSE, selectedPathOptions = selectedPathOptions())) 
  })
  observe({
  req(input$hapty,filteredData6(),input$haphap,input$haplab)
    if (length(input$haphap) == 0){
      datahap <- 1
    }else if(length(input$haphap)<length(unique(grep(input$hapty,names(filteredData6())))) &all(input$haphap%in%names(filteredData6())) &all(input$haplab%in%names(filteredData6()))){
      datahap <- cbind(filteredData6()[,input$haphap],(1-apply(as.data.frame(filteredData6()[,input$haphap]),1,sum)))
      colnames(datahap)=c(input$haphap,"NA")
	  haplap=c(input$haplab)
    }else if(length(input$haphap)==length(unique(grep(input$hapty,names(filteredData6()))))&all(input$haphap%in%names(filteredData6())) &all(input$haplab%in%names(filteredData6()))){
      datahap <- filteredData6()[,input$haphap]
	   haplap=c(input$haplab)
    }else{datahap=1
	haplap="SAMPLE"}
    
    maxValue <- max(as.matrix(datahap))#length(unique(grep("MT_",names(filteredData6()))))
	if(all(input$haplab%in%names(filteredData6()))){htm=labelpop(filteredData6(),input$haplab)}else{NULL}
  #  if(length(select(taxonomy_table(),starts_with("CAT")))>0 & all(input$haphap%in%names(filteredData6()))){
      leafletProxy("hapmap") %>%
        clearMarkers() %>% clearControls() %>%
        updateMinicharts(
          layerId = filteredData6()$SITE,
          chartdata =  datahap,
          maxValues = maxValue,
          type ="pie",showLabels = input$labels4,legend=TRUE,legendPosition ="bottomright",
          colorPalette = get_color(rcolors$t2m_29lev, n = length(colnames(datahap))+1),
          popup = popupArgs(
            html = labelpop(filteredData6(),haplap)
          ),
          width = sqrt(filteredData6()$Count)*3,transitionTime = 0
        ) %>% addLegendSize(position = "bottomleft",col="black",fillColor="white",baseSize=quantile(sqrt(filteredData6()$Count)*1.4,0.5),#median(sqrt(filteredData6()$Count)*3*0.6),
                            values = (filteredData6()$Count),shape="circle",orientation="horizontal",breaks=5) #%>%
    #}
  })
  ############addgrid map and merge###############
  griddatahap<- reactive({
    req(filteredData6())
	req(input$hapty,input$haphap)
    req(input$Gridhap)
	if(nrow(filteredData6())>1 & input$Gridhap>0 &length(input$haphap)>0 &all(input$haphap%in%names(filteredData6()))){
    gridmap(filteredData6(),input$Gridhap,type=input$hapty,comp=input$haphap)
	}
  })
 observe({
  req(griddatahap(),input$hapty,input$haphap)
    if(input$Gridhap>0 &length(input$haphap)==length(grep(input$hapty,names(filteredData6())))){
	chartdata=data.frame(lapply(griddatahap()[,input$haphap],as.numeric))
	 }else if(input$Gridhap>0 &length(input$haphap)<length(grep(input$hapty,names(filteredData6())))){
	 chartdata=data.frame(lapply(griddatahap()[,c(grep(input$hapty,names(griddatahap()),value=TRUE),"NA")],as.numeric))
	 }else{NULL}
      leafletProxy("hapmap",data=griddatahap()) %>%
        clearMarkers() %>% clearControls() %>%
        removeMinicharts(layerId = filteredData6()$SITE) %>%
        addMinicharts(as.numeric(griddatahap()$LONGITUDE), as.numeric(griddatahap()$LATITUDE),legend=TRUE,legendPosition ="bottomright",
                      chartdata=chartdata,type="pie",
                      colorPalette = get_color(rcolors$t2m_29lev, n = length(grep(input$hapty,names(griddatahap())))+1),
                      width = 2*sqrt(as.numeric(griddatahap()$Count)), popup = popupArgs(
                        #labels = input$haphap,
                        html = labelpop(griddatahap(),input$haplab
                        )))%>%
        addGraticule(interval = input$Gridhap, style = list(color = "#FF0000", weight = 0.1,fillOpacity=0.1))
  })
  ############################################### click response ##################################################
  observeEvent(input$hapmap_draw_new_feature,{
    req(filteredData6())
	if(input$Gridhap==0 &length(input$haphap>0)){
    #Only add new layers for bounded locations
    found_in_bounds <- findLocations(shape = input$hapmap_draw_new_feature
                                     , location_coordinates = SpatialPointsDataFrame(filteredData6()[,c('LONGITUDE', 'LATITUDE')] , filteredData6())
                                     , location_id_colname = "SITE")
    for(id in found_in_bounds){
      if(id %in% data_of_click$clickedMarker){
        # don't add id
      } else {
        # add id
        data_of_click$clickedMarker<-append(data_of_click$clickedMarker, id, 0)
      }
    }
    
    selected <- subset(filteredData6(), SITE %in% data_of_click$clickedMarker)
    ######age count plot
    output$hapCount <- renderPlotly({
      req(selected)
      ggplot(selected,aes(x=AGE,y=Count))+geom_point()+ggtitle("Temporal distribution of the samples")+theme(
        plot.title = element_text(size=15, family="Arial"),plot.margin=unit(c(1,1,2,1),'lines'))
    })
    leafletProxy("hapmap",data=selected)%>% 
      addCircleMarkers(lng=selected$LONGITUDE,lat=selected$LATITUDE,
                       color = "red",radius=sqrt(selected$Count)*2,weight=1,
                       fillOpacity = 0.5,            
                       layerId = (selected$SecondSite))
  }
  })
  #for grid
  observeEvent(input$hapmap_draw_new_feature,{
   # req(griddatahap())
	if(as.numeric(input$Gridhap)>0 &length(input$haphap>0)){
    #Only add new layers for bounded locations
    found_in_bounds1 <- findLocations(shape = input$hapmap_draw_new_feature
                                     , location_coordinates = SpatialPointsDataFrame(griddatahap()[,c('LONGITUDE', 'LATITUDE')] , griddatahap())
                                     , location_id_colname = "SITE")
    for(id in found_in_bounds1){
      if(id %in% data_of_click$clickedMarker){
      } else {
        data_of_click$clickedMarker<-append(data_of_click$clickedMarker, id, 0)
      }
    }
    
    selected <- subset(griddatahap(), SITE %in% data_of_click$clickedMarker)
    ######age count plot
    output$hapCount <- renderPlotly({
      req(selected)
      ggplot(selected,aes(x=AGE,y=Count))+geom_point()+ggtitle("Temporal distribution of the samples")+theme(
        plot.title = element_text(size=15, family="Arial"),plot.margin=unit(c(1,0.3,0.5,0.3),'cm'))
    })
    leafletProxy("hapmap",data=selected)%>% 
      addCircleMarkers(lng=selected$LONGITUDE,lat=selected$LATITUDE,
                       color = "red",radius=sqrt(selected$Count)/1.2,weight=1,
                       fillOpacity = 0.5,            
                       layerId = (selected$SecondSite))
  }
  })
  ############################################### click response ##################################################
  observeEvent(input$hapmap_draw_deleted_features,{
  if(input$Gridhap==0 &length(input$haphap>0)){
    # loop through list of one or more deleted features/ polygons
    for(feature in input$hapmap_draw_deleted_features$features){
      # get ids for locations within the bounding shape
      bounded_layer_ids <- findLocations(shape = feature
                                         , location_coordinates = SpatialPointsDataFrame(filteredData6()[,c('LONGITUDE', 'LATITUDE')] , filteredData6())
                                         , location_id_colname = "SecondSite")
      output$hapCount <- renderPlotly({
        return(NULL)
      })
      # remove second layer representing selected locations
      proxy <- leafletProxy("hapmap")
      proxy %>% removeMarker(layerId = as.character(bounded_layer_ids))
      first_layer_ids <- subset(filteredData6(), SecondSite %in% bounded_layer_ids)$SITE
      data_of_click$clickedMarker <- data_of_click$clickedMarker[!data_of_click$clickedMarker
                                                                 %in% first_layer_ids]
    }
	}
  })
   observeEvent(input$hapmap_draw_deleted_features,{
  if(input$Gridhap>0 &length(input$haphap>0)){
    # loop through list of one or more deleted features/ polygons
    for(feature in input$hapmap_draw_deleted_features$features){
      # get ids for locations within the bounding shape
      bounded_layer_ids <- findLocations(shape = feature
                                         , location_coordinates = SpatialPointsDataFrame(griddatahap()[,c('LONGITUDE', 'LATITUDE')] , griddatahap())
                                         , location_id_colname = "SecondSite")
      output$hapCount <- renderPlotly({
        return(NULL)
      })
      # remove second layer representing selected locations
      proxy <- leafletProxy("hapmap")
      proxy %>% removeMarker(layerId = as.character(bounded_layer_ids))
      first_layer_ids <- subset(griddatahap(), SecondSite %in% bounded_layer_ids)$SITE
      data_of_click$clickedMarker <- data_of_click$clickedMarker[!data_of_click$clickedMarker
                                                                 %in% first_layer_ids]
    }
	}
  })
  ###download map
  TherootsHS <- reactive({
    root <- input$rootHS
    req(root, dir.exists(root))
    
    if(length(root) == 0 || root == ""){
      volumes <- getVolumes()()
      c(volumes)
    } else{
      c(project_root = root)
    }
  })
  
  Thesheets_dirHS <- reactive({
    shinyDirChoose(input, 'sheets_dirHS', roots = TherootsHS(), session = session)
    parseDirPath(roots = TherootsHS(), input$sheets_dirHS)
  })
  
  output$sheets_dirHS <- renderPrint({
    Thesheets_dirHS()
  }) 
  #######################down grid figures######
  observeEvent(input$DownHS,{
    req(filteredData6(),input$hapty,input$haphap)
    outpath=Thesheets_dirHS()
    gridplot(filteredData6(),as.numeric(input$hapage[1]),as.numeric(input$hapage[2]),0,type=input$hapty,comp=input$haphap,outpath)
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

