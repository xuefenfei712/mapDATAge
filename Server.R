#options(digits = 5, shiny.maxRequestSize = 5 * 1024 ^ 2)
  ##basemap
  tilesURL <- "http://server.arcgisonline.com/ArcGIS/rest/services/Canvas/World_Light_Gray_Base/MapServer/tile/{z}/{y}/{x}"
server <- function(input, output,session)({
  tabl <- reactive({
    req(input$in_taxon_table)
    if (grepl(input$in_taxon_table$datapath, pattern = ".txt") |
        grepl(input$in_taxon_table$datapath, pattern = ".tsv")) {
      read.table(input$in_taxon_table$datapath, header = T,
                 sep = "\t", stringsAsFactors = F,
                 quote = "", comment.char = "")
    }
  })
  table <- reactive({
    req(tabl())
    test=tabl()
    colnames(test)=stringr::str_to_title(names(tabl()))
    test
  })
  output$fileStatus <- eventReactive(input$go, {
    if (is.null(validate_input_files(table()))) {
      paste("Congrats, no error detected!")
    }
  })
  ###############################draw map#################################
  ####################### Render UIs for Panel 3 (map)##################
  output$mapoutsp <- renderUI({
    req(table())
    selectInput("mapsp",
                label = "Select specie(s)",
                choices = unique(table()$Species),
                multiple = TRUE,selected=unique(table()$Species)[1])
  })
  output$mapoutsx <- renderUI({
    req(table())
    selectInput("mapsx",
                label = "Select sex",
                choices = unique(table()$Sex),
                multiple = TRUE,selected=unique(table()$Sex)[1])
  })
  output$mapout <- renderUI({
    req(table())
    sliderInput("mapage",
                label = "Select a time range",
                min = 0, 
                max = table()$Age %>% as.numeric %>% max(na.rm = TRUE),
                step = 10,
                value = range(table()$Age))
  })
  output$mapoutlog <- renderUI({
    sliderInput("maplog",
                label = "Select a longitude range",
                min = table()$Longitude %>% as.numeric() %>% round(2)%>% min(na.rm = TRUE),
                max = table()$Longitude %>% as.numeric %>% round(2)%>% max(na.rm = TRUE),
                step = 0.1,
                value = range(table()$Longitude))
  })
  output$mapoutlat <- renderUI({
    sliderInput("maplat",
                label = "Select a latitude range",
                min = table()$Latitude %>% as.numeric() %>% round(2)%>% min(na.rm = TRUE),
                max = table()$Latitude %>% as.numeric %>% round(2)%>% max(na.rm = TRUE),
                step = 1,
                value = range(table()$Latitude))
  })
    filtData <- reactive({
    req(input$mapage)
    Merge(table()[table()$Sex%in%input$mapsx & table()$Species%in%input$mapsp & table()$Age >= as.numeric(input$mapage[1]) & table()$Age < as.numeric(input$mapage[2])
                                 & table()$Latitude>=as.numeric(input$maplat[1]) & table()$Latitude<=as.numeric(input$maplat[2]) & table()$Longitude>=as.numeric(input$maplog[1]) & table()$Longitude<=as.numeric(input$maplog[2]),])
  })
  output$mapoutlab <- renderUI({
    req(filtData())
    selectInput("maplab",
                label = "Select labels",
                choices = names(filtData())[names(filtData())%in% c("n.overlaps","origins","SecondSite")==FALSE],
                multiple = TRUE,selected="Sample")
  })
  # render color
  colorpal <- reactive({
    req(filtData()$Age)
    colorNumeric(input$colors,filtData()$Age)
  })
 
  #######header of map
  output$maphead <- renderText({
    paste("Geographic distribution of the samples ",sep=" ")
  })
  # list to store the selections for tracking
  data_of_click <- reactiveValues(clickedMarker = list())
  ##draw map
  output$drawmap <- renderLeaflet({
    req(filtData())
    pal <- colorpal()
    leaflet(options = leafletOptions(zoomControl = FALSE, minZoom = 1, maxZoom = 5, dragging = T)) %>%
      addTiles(tilesURL) %>%
      fitBounds(min(filtData()$Longitude)-0.3,min(filtData()$Latitude)-0.3,max(filtData()$Longitude)+0.5,max(filtData()$Latitude)+0.5) %>%
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
    leafletProxy("drawmap", data = filtData()) %>%
      clearShapes() %>% clearMarkers() %>%
      addCircleMarkers(filtData()$Longitude, filtData()$Latitude,color = "#777777",radius=sqrt(filtData()$Count)*2#mean(filtData()$Count)))#~sqrt(Count)
                       ,weight=1,fillColor = ~pal(Age), fillOpacity = 0.9, popup = (labelpop(filtData(),input$maplab)),layerId = filtData()$Site
      )
  })
  ###add a size legend
  observe({
    proxy <- leafletProxy("drawmap", data = filtData())
    proxy %>% clearControls()
    pal <- colorpal()
    proxy %>% addLegendSize(position = "bottomleft",col="black",fillColor="white",baseSize=min(sizeNumeric(filtData()$Count, baseSize = mean(filtData()$Count)*2.5))#, baseSize = mean(filtData()$Count)))*2#quantile(sqrt(filtData()$Count),0.55),
                            ,values = sizeNumeric((filtData()$Count), baseSize = mean(filtData()$Count)),shape="circle",orientation="horizontal",breaks=5) %>%
      addLegendNumeric(position = "bottomright",
                       pal = pal, values = filtData()$Age)
  })
  ############################################### click response ##################################################
  observeEvent(input$drawmap_draw_new_feature,{
    req(filtData())
    found_in_bounds <- findLocations(shape = input$drawmap_draw_new_feature
                                     , location_coordinates = SpatialPointsDataFrame(filtData()[,c('Longitude', 'Latitude')] , filtData())
                                     , location_id_colname = "Site")
    for(id in found_in_bounds){
      if(id %in% data_of_click$clickedMarker){
      } else {
        data_of_click$clickedMarker<-append(data_of_click$clickedMarker, id, 0)
      }
    }
    selected <- subset(filtData(), Site %in% data_of_click$clickedMarker)
    ######age count plot
    output$drawmapCount <- renderPlotly({
      req(selected)
      ggplot(selected,aes(x=Age,y=Count))+geom_point()+ggtitle("Temporal distribution of the samples")+theme(plot.title = element_text(size=15,family="Arial",face="plain"),
                                                                                                             axis.title= element_text(size=13,family="Arial",face="plain"),plot.margin=unit(c(1,0.3,0.5,0.3),'cm'))
    })
    leafletProxy("drawmap",data=selected)%>% 
      addCircleMarkers(lng=selected$Longitude,lat=selected$Latitude,
                       color = "red",radius=sqrt(selected$Count)*1.2,weight=1,
                       fillOpacity = 0.6,            
                       layerId = (selected$SecondSite))
  })
  ############################################### click response ##################################################
  observeEvent(input$drawmap_draw_deleted_features,{
    for(feature in input$drawmap_draw_deleted_features$features){
      bounded_layer_ids <- findLocations(shape = feature
                                         , location_coordinates = SpatialPointsDataFrame(filtData()[,c('Longitude', 'Latitude')] , filtData())
                                         , location_id_colname = "SecondSite")
      output$drawmapCount <- renderPlotly({
        return(NULL)
      })
      # remove second layer representing selected locations
      proxy <- leafletProxy("drawmap")
      proxy %>% removeMarker(layerId = as.character(bounded_layer_ids))
      first_layer_ids <- subset(filtData(), SecondSite %in% bounded_layer_ids)$Site
      data_of_click$clickedMarker <- data_of_click$clickedMarker[!data_of_click$clickedMarker
                                                                 %in% first_layer_ids]
    }
  })
  ############################pie map############################################  
  ################ Render UIs for Panel 4 (pie)#################################
  output$pieoutsp <- renderUI({
    req(table())
    selectInput("piesp",
                label = "Select specie(s)",
                choices = unique(table()$Species),
                multiple = TRUE,selected=unique(table()$Species)[1])
  })
  output$pieoutsx <- renderUI({
    req(table())
    selectInput("piesx",
                label = "Select sex",
                choices = unique(table()$Sex),
                multiple = TRUE,selected=unique(table()$Sex)[1])
  })
  output$pieout <- renderUI({
    req(table())
    sliderInput("pieage",
                label = "Select a time range",
                min = 0,  max = table()$Age %>%
                  as.numeric %>%
                  max(na.rm = TRUE),
                step = 10,
                value = range(table()$Age))
  })
 output$pieoutsnp <- renderUI({
    req(table())
    selectInput("piesnp",
                label = "Select your related variations",
                choices = getsnpname(table()),
                multiple = FALSE)
  })
  output$errpie <- renderText({
    if(length(select(table(),starts_with("SNP_")))==0){
      validate("The required data were not part of your Input File. Please make sure that columns labelled 'SNP_snpname_A' and 'SNP_snpname_D' are provided")
    }
  })
  output$pieoutlog <- renderUI({
    req(table())
    sliderInput("pielog",
                label = "Select a longitude range",
                min = table()$Longitude %>% as.numeric() %>%round(2) %>%min(na.rm = TRUE),
                max = table()$Longitude %>% as.numeric %>%max(na.rm = TRUE),
                step = 0.1,
                value = range(table()$Longitude))
  })
  output$pieoutlat <- renderUI({
    req(table())
    sliderInput("pielat",
                label = "Select a latitude range",
                min = table()$Latitude %>% as.numeric() %>% round(2) %>% min(na.rm = TRUE),
                max = table()$Latitude %>% as.numeric %>% round(2) %>% max(na.rm = TRUE),
                step = 1,
                value = range(table()$Latitude))
  })
  output$pieout1 <- renderUI({
    req(table())
    numericInput("GridSize","Time interval (years)",1000,min=100,max=10000)
  })
  output$pieout2 <- renderUI({
    req(table())
    actionButton("Down", "Drawpictures!")
  })
  ##active data
  filtData1 <- reactive({
    req(input$pieage,table())
    if(length(select(table(),starts_with("Snp_")))>0 & input$pietype=="ReadCounts"){
      MergeSNP(table()[table()$Sex%in%input$piesx & table()$Species%in%input$piesp & table()$Age >= as.numeric(input$pieage[1]) & table()$Age < as.numeric(input$pieage[2]) &
                                      table()$Latitude>=input$pielat[1] & table()$Latitude<=input$pielat[2] & table()$Longitude>=input$pielog[1] & table()$Longitude<=input$pielog[2],],snp=input$piesnp,gtp="ReadCounts")
    }else if(length(select(table(),starts_with("Snp_")))>0 & input$pietype=="Genotype"){
	MergeSNP(table()[table()$Sex%in%input$piesx & table()$Species%in%input$piesp & table()$Age >= as.numeric(input$pieage[1]) & table()$Age < as.numeric(input$pieage[2]) &
                                      table()$Latitude>=input$pielat[1] & table()$Latitude<=input$pielat[2] & table()$Longitude>=input$pielog[1] & table()$Longitude<=input$pielog[2],],snp=input$piesnp,gtp="genotype")
     }
  })
  ############labels for shown#####
  output$pieoutlab <- renderUI({
    req(filtData1())
    if(length(select(table(),starts_with("SNP_")))>0){
      selectInput("pielab",
                  label = "Select labels",
                  choices = names(filtData1())[names(filtData1())%in% c("n.overlaps","origins","SecondSite")==FALSE],
                  multiple = TRUE,selected="Sample")
    }
  }) 
  #######header of map
  output$piehead <- renderText({
    if(length(select(table(),starts_with("Snp_")))>0){
      paste("Allele distribution on map",sep=" ")
    }
  })
  #########################output map#########################
  output$pie <- renderLeaflet({
    if(length(select(table(),starts_with("Snp_")))>0){
      req(filtData1())
      leaflet(options = leafletOptions(zoomControl = FALSE, minZoom = 1, maxZoom = 5, dragging = T)) %>%
        addTiles(tilesURL) %>% 
        fitBounds(min(filtData1()$Longitude)-0.3,min(filtData1()$Latitude)-0.3,max(filtData1()$Longitude)+0.3,max(filtData1()$Latitude)+0.3)%>%
        addMinicharts(
          filtData1()$Longitude, filtData1()$Latitude,
          layerId = as.character(filtData1()$Site),width=4,height=4) %>% addResetMapButton() %>% addMiniMap(width=100,height=100,toggleDisplay = TRUE,position = "topright")%>%
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
    req(filtData1())
    if(input$type=="bar"){
      leafletProxy("pie",data=filtData1()) %>%
        clearMarkers() %>% clearControls() %>%
        updateMinicharts(
          layerId = filtData1()$Site,
          chartdata =  cbind(filtData1() %>% select(matches(input$piesnp))),
          #maxValues = maxValue,
		  width=15,height=15,
          type =input$type,showLabels = input$labels,
          colorPalette = brewer.pal(11, input$colors1)[c(1,9,3,5)],
          popup = popupArgs(
            labels = toupper(c(getallename(filtData1(),input$piesnp))),
            html = labelpop(filtData1(),input$pielab
            )
          )
        )
    }else if(input$type=="pie" & length(select(table(),starts_with("Snp_")))>0){
      leafletProxy("pie",data=filtData1()) %>%
        clearMarkers() %>% clearControls() %>%
        updateMinicharts(
          layerId = filtData1()$Site,
          chartdata =  cbind(filtData1() %>% select(matches(input$piesnp))),
          maxValues = maxValue,
          type =input$type,showLabels = input$labels,
          colorPalette = brewer.pal(11, input$colors1)[c(1,9,3,5)],
          popup = popupArgs(
            labels = toupper(c(getallename(filtData1(),input$piesnp))),
            html = labelpop(filtData1(),input$pielab
            )
          ),
          width = sqrt(filtData1()$Count)*5,transitionTime = 0
        ) %>% addLegendSize(position = "bottomleft",col="black",fillColor="white",baseSize=quantile(sqrt(filtData1()$Count),0.85),
                            values = filtData1()$Count,shape="circle",orientation="horizontal",breaks=5)
    } else if(length(select(table(),starts_with("Snp_")))>0){ 
      leafletProxy("pie",data=filtData1()) %>%
        clearMarkers() %>% #clearControls() %>%
        updateMinicharts(
          layerId = filtData1()$Site,
          chartdata =  cbind(filtData1() %>% select(matches(input$piesnp))),
          maxValues = maxValue,
          type =input$type,showLabels = input$labels,
          colorPalette = brewer.pal(11, input$colors1)[c(2,9,4,5)],
          popup = popupArgs(
            labels = toupper(c(getallename(filtData1(),input$piesnp))),
            html = labelpop(filtData1(),input$pielab
            )
          ),
          width = 5*sqrt(filtData1()$Count),transitionTime = 0
        ) 
    }   
  })
  ########addgrid map and merge###############
  griddata<- reactive({
    req(filtData1(),input$Gridpie)
    gridmap(filtData1(),input$Gridpie,type="Snp")
  })
  observe({
    if(input$Gridpie>0){
      leafletProxy("pie",data=griddata()) %>%
        clearMarkers() %>% clearControls() %>%
        removeMinicharts(layerId = filtData1()$Site) %>%
        addMinicharts(as.numeric(griddata()$Longitude), as.numeric(griddata()$Latitude),layerId = as.character(griddata()$Site),
                      chartdata= cbind(griddata() %>% select(matches(input$piesnp))),
					  type="pie",
                      colorPalette = brewer.pal(11, input$colors1)[c(2,9,4,5)],
                      popup = popupArgs(
                        labels = toupper(c(getallename(griddata(),input$piesnp))),
                        html = labelpop(griddata(),input$pielab
                        )),
                      width = 5*sqrt(as.numeric(griddata()$Count)))%>%
        addGraticule(interval = input$Gridpie, style = list(color = "#FF0000", weight = 0.1,fillOpacity=0.1))%>% 
        leaflegend::addLegendSize(position = "bottomleft",col="black",fillColor="white",baseSize=quantile(sqrt(as.numeric(griddata()$Count)),0.85),
                      values = as.numeric(griddata()$Count),shape="circle",orientation="horizontal",breaks=5)
    }
  })
  ############################################### click response ##################################################
  observeEvent(input$pie_draw_new_feature,{
    req(filtData1())
	if(input$Gridpie==0){
    found_in_bounds <- findLocations(shape = input$pie_draw_new_feature
                                     , location_coordinates = SpatialPointsDataFrame(filtData1()[,c('Longitude', 'Latitude')] , filtData1())
                                     , location_id_colname = "Site")
    for(id in found_in_bounds){
      if(id %in% data_of_click$clickedMarker){
      } else {
        data_of_click$clickedMarker<-append(data_of_click$clickedMarker, id, 0)
      }
    }
    selected <- subset(filtData1(), Site %in% data_of_click$clickedMarker)
    ######age count plot
    output$pieCount <- renderPlotly({
      req(selected)
      ggplot(selected,aes(x=Age,y=Count))+geom_point()+ggtitle("Temporal distribution of the samples")+theme(
        plot.title = element_text(size=15, family="Arial"),plot.margin=unit(c(1,0.3,0.5,0.3),'cm'))
    })
    leafletProxy("pie",data=selected)%>% 
      addCircleMarkers(lng=selected$Longitude,lat=selected$Latitude,
                       color = "red",radius=sqrt(selected$Count)*3,weight=1,
                       fillOpacity = 0.6,            
                       layerId = (selected$SecondSite))
  }
  })
  observeEvent(input$pie_draw_new_feature,{
  if(as.numeric(input$Gridpie>0)){
    found_in_bounds1 <- findLocations(shape = input$pie_draw_new_feature
                                     , location_coordinates = SpatialPointsDataFrame(griddata()[,c('Longitude', 'Latitude')] , griddata())
                                     , location_id_colname = "Site")
    for(id in found_in_bounds1){
      if(id %in% data_of_click$clickedMarker){
     } else {
        data_of_click$clickedMarker<-append(data_of_click$clickedMarker, id, 0)
      }
    }
    selected <- subset(griddata(), Site %in% data_of_click$clickedMarker)
    ######age count plot
    output$pieCount <- renderPlotly({
      ggplot(selected,aes(x=Age,y=Count))+geom_point()+ggtitle("Temporal distribution of the samples")+theme(
        plot.title = element_text(size=15, family="Arial"),plot.margin=unit(c(1,0.3,0.5,0.3),'cm'))
    })
    leafletProxy("pie",data=selected)%>% 
      addCircleMarkers(lng=selected$Longitude,lat=selected$Latitude,
                       color = "red",radius=sqrt(selected$Count)*3,weight=1,
                       fillOpacity = 0.6,            
                       layerId = (selected$SecondSite))
  }
  })
  ############################################### click response ##################################################
  observeEvent(input$pie_draw_deleted_features,{
	if(input$Gridpie==0){

    for(feature in input$pie_draw_deleted_features$features){
     
      bounded_layer_ids <- findLocations(shape = feature
                                         , location_coordinates = SpatialPointsDataFrame(filtData1()[,c('Longitude', 'Latitude')] , filtData1())
                                         , location_id_colname = "SecondSite")
      output$pieCount <- renderPlotly({
        return(NULL)
      })
      # remove second layer representing selected locations
      proxy <- leafletProxy("pie")
      proxy %>% removeMarker(layerId = as.character(bounded_layer_ids))
      first_layer_ids <- subset(filtData1(), SecondSite %in% bounded_layer_ids)$Site
      data_of_click$clickedMarker <- data_of_click$clickedMarker[!data_of_click$clickedMarker
                                                                 %in% first_layer_ids]
    }
	}
})
 observeEvent(input$pie_draw_deleted_features,{	
  if(as.numeric(input$Gridpie>0)){

    for(feature in input$pie_draw_deleted_features$features){
     
      bounded_layer_ids <- findLocations(shape = feature
                                         , location_coordinates = SpatialPointsDataFrame(griddata()[,c('Longitude', 'Latitude')] , griddata())
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
      volumes <- shinyFiles::getVolumes()()
      c(volumes)
    }else{
      c(project_root = root)
    }
  })
  Thesheets_dir <- reactive({
    shinyFiles::shinyDirChoose(input, 'sheets_dir', roots = Theroots(), session = session)
    shinyFiles::parseDirPath(roots = Theroots(), input$sheets_dir)
  })
  output$sheets_dir <- renderPrint({
    Thesheets_dir()
  }) 
  #######################down grid figures######
  observeEvent(input$Down,{
    req(filtData1())
    outpath=Thesheets_dir()
    gridplot(filtData1(),as.numeric(input$pieage[1]),as.numeric(input$pieage[2]),input$GridSize,type="Snp",outpath)
  })
  ###################draw allele##########################  
  ########## Render UIs for Panel 5 (allele)##############
  output$alloutsp <- renderUI({
    req(table())
    selectInput("allsp",
                label = "Select specie(s)",
                choices = unique(table()$Species),
                multiple = TRUE,selected=unique(table()$Species)[1])
  })
  output$alloutsx <- renderUI({
    req(table())
    selectInput("allsx",
                label = "Select sex",
                choices = unique(table()$Sex),
                multiple = TRUE,selected=unique(table()$Sex)[1])
  })
  output$allout <- renderUI({
    sliderInput("alleage",
                label = "Select a time range",
                min = 0,
                max = table()$Age %>%
                  as.numeric %>%
                  max(na.rm = TRUE),
                step = 10,
                value = range(table()$Age))
  })
  output$allsnp <- renderUI({
    selectInput("allsnp",
                label = "Select a variation",
                choices = getsnpname(table()),
                multiple = FALSE)
  })
  output$allall <- renderUI({
  req(input$allsnp)
  if(!is.null(input$allsnp)){
    selectInput("allall",
                label = "Select allele",
                choices = c(Choose='',getallename(table(),input$allsnp)),
				selectize=FALSE,
                selected='')
				}
  })
  output$alloutlog <- renderUI({
    sliderInput("alllog",
                label = "Select a longitude range",
                min = table()$Longitude %>% as.numeric() %>%  round(2) %>% min(na.rm = TRUE),
                max = table()$Longitude %>% as.numeric %>%  round(2) %>% max(na.rm = TRUE),
                step = 0.1,
                value = range(table()$Longitude))
  })
  output$alloutlat <- renderUI({
    sliderInput("alllat",
                label = "Select a latitude range",
                min = table()$Latitude %>% as.numeric() %>%  round(2) %>% min(na.rm = TRUE),
                max = table()$Latitude %>% as.numeric %>% round(2) %>% max(na.rm = TRUE),
                step = 1,
                value = range(table()$Latitude))
  })
  output$alloutymin <- renderUI({
    numericInput("allymin",
                 label = "Input the min y",
                 min = -1,
                 max = 1,value=0)
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
    req(input$alleage,input$allsnp,input$allall)
	if(input$alltype=="ReadCounts"){
	req(input$allsampling)
	AllePlot(table(),as.character(input$allsx),as.character(input$allsp),input$allall,as.numeric(input$WinSize),as.numeric(input$StepSize),as.numeric(input$alleage[1]),as.numeric(input$alleage[2]),input$allsnp,
               input$alllat[1],input$alllat[2],input$alllog[1],input$alllog[2],input$allymin,input$allymax,input$allsampling,gtp=input$alltype)
  }else{
  AllePlot(table(),as.character(input$allsx),as.character(input$allsp),input$allall,as.numeric(input$WinSize),as.numeric(input$StepSize),as.numeric(input$alleage[1]),as.numeric(input$alleage[2]),input$allsnp,
               input$alllat[1],input$alllat[2],input$alllog[1],input$alllog[2],input$allymin,input$allymax,gtp=input$alltype)
			   }
  })
  output$errtraj <- renderText({
    if(length(select(table(),starts_with("Snp_")))==0){
      validate("The required data were not part of your Input File. Please make sure that columns labelled 'SNP_snpname_A' and 'SNP_snpname_D' are provided")
    }
  })
  #head
  output$trajhead <- renderText({
    if(length(select(table(),starts_with("Snp_")))>0){
      paste("Allele distribution",sep=" ")
    }
  })
  ############active data
  filtData3 <- reactive({
    req(input$alleage,input$allsnp,input$allall)
    if(length(select(table(),starts_with("Snp_")))>0 & length(input$allall)==1){
      allname=paste("Snp_",input$allsnp,"_",tolower(input$allall),sep="")
	   addreadCounts(table()[table()$Sex%in%input$allsx & table()$Species%in%input$allsp & table()$Age >= as.numeric(input$alleage[1]) & table()$Age < as.numeric(input$alleage[2]) & table()[,allname]>0 &
                          table()$Latitude >= as.numeric(input$alllat[1]) & table()$Latitude <= input$alllat[2] & table()$Longitude>=as.numeric(input$alllog[1]) & table()$Longitude<=as.numeric(input$alllog[2]),],input$allsnp)
    }
  })
  ##########draw map
  output$deallele <- renderLeaflet({
    if(length(select(table(),starts_with("Snp_")))>0){
      req(filtData3())
      leaflet(options = leafletOptions(zoomControl = FALSE, minZoom = 1, maxZoom = 5, dragging = T)) %>%
        addTiles(tilesURL) %>%#addControl(titletraj, position = "topleft") %>%
        fitBounds(min(table()$Longitude)-0.3,min(table()$Latitude)-0.3,max(table()$Longitude)+0.3,max(table()$Latitude)+0.3) %>%
        addCircleMarkers(filtData3()$Longitude, filtData3()$Latitude,radius=filtData3()[,paste("Snp_",input$allsnp,"_",tolower(input$allall),sep="")]) %>% addResetMapButton()%>% addMiniMap(width=100,height=100,toggleDisplay = TRUE,position = "topright")
    }  
  })
  observe({ 
    if(length(select(table(),starts_with("Snp_")))>0){
      req(filtData3())
      leafletProxy("deallele",data=filtData3()) %>%
        clearMarkers() %>% clearShapes() %>% 
        addAwesomeMarkers(filtData3()$Longitude, filtData3()$Latitude,clusterOptions = markerClusterOptions(),#radius=~as.numeric(D)*5,color = "#777777",fillColor=~pal(Age),fillOpacity = 0.9, 
                          popup = paste0(filtData3()$Sample,"<br>","Sex: ",filtData3()$Sex,"<br>","Age: ",filtData3()$Age,"<br>","Rs/Total:",round(filtData3()[,paste("Snp_",input$allsnp,"_",tolower(input$allall),sep="")],2),"/",filtData3()$ReadCount)
        )  
    }
  })
  ##############print table#################
  output$print_taxon_table <- DT::renderDataTable({
    req(table())
    table <- table() 
    DT::datatable(table, options = list(scrollX = TRUE))
  })
  
  ################################ancestral piemap########################
  ###################render UI for ances map#####################
  # Read in data files and validate 
  output$ancesoutsp <- renderUI({
    req(table())
    selectInput("ancessp",
                label = "Select specie(s)",
                choices = unique(table()$Species),
                multiple = TRUE,selected=unique(table()$Species)[1])
  })
  output$ancesoutsx <- renderUI({
    req(table())
    selectInput("ancessx",
                label = "Select sex",
                choices = unique(table()$Sex),
                multiple = TRUE,selected=unique(table()$Sex)[1])
  })
  output$ancesout <- renderUI({
    sliderInput("anceage",
                label = "Select a time span", min =0, max = table()$Age %>% as.numeric %>%
                  max(na.rm = TRUE),
                step = 10,
                value = range(table()$Age))
  })
  output$ancesout1 <- renderUI({
    if(length(grep("Anc",names(table())))>0){
      selectInput("ancecomp",
                  label = "Select ancestral origins",
                  choices = names(table())[grep("Anc",names(table()))],
                  multiple = TRUE,selected="Anc1")
    }
  })
  output$ancesoutcut <- renderUI({
    sliderInput("ancecut",
                label = "Time Split",
                min = table()$Age %>% as.numeric() %>% min(na.rm = TRUE),
                max = table()$Age %>% as.numeric %>% max(na.rm = TRUE),
                value=median(table()$Age)%>%
                  as.numeric() 
    )
  })
  output$errance <- renderText({
    if(length(grep("Anc",names(table())))==0){
      validate("The required data were not part of your Input File. Please make sure that columns labelled 'Anc' are provided")
    }
  })
  #############active data
  filtData2T <- reactive({
    req(input$anceage,input$ancecut)
    if(length(grep("Anc",names(table())))>0){
      Mergeawsome(table()[table()$Sex%in%input$ancessx & table()$Species%in%input$ancessp & table()$Age >= as.numeric(input$anceage[1]) & table()$Age < as.numeric(input$anceage[2]),],type="Anc")
    }
  })
  output$ancesoutlab <- renderUI({
    req(filtData2T())
    selectInput("anceslab",
                label = "Select labels",
                choices = names(filtData2T())[names(filtData2T())%in% c("n.overlaps","origins","SecondSite")==FALSE],
                multiple = TRUE,selected="Sample")
  })
  filtData2 <- reactive({
    req(input$anceage,input$ancecut)
    if(length(grep("Anc",names(table())))>0){
      Mergeatype(table()[table()$Sex%in%input$ancessx & table()$Species%in%input$ancessp & table()$Age >= as.numeric(input$anceage[1]) & table()$Age < as.numeric(input$anceage[2]) & table()$Age >= as.numeric(input$ancecut),],type="Anc")
    }
  })
  filtData22 <- reactive({
    req(input$anceage,input$ancecut)
    if(length(grep("Anc",names(table())))>0){
      Mergetype(table()[table()$Sex%in%input$ancessx & table()$Species%in%input$ancessp & table()$Age >= as.numeric(input$anceage[1]) & table()$Age < as.numeric(input$anceage[2]) & table()$Age< as.numeric(input$ancecut),],type="Anc")
    }
  })
  ###########output ance map
  output$drawance <- renderLeaflet({
    req(filtData2T())
    leaflet(options = leafletOptions(zoomControl = FALSE, minZoom = 2.3, maxZoom = 10, dragging = T)) %>%
      addTiles(tilesURL) %>% 
      fitBounds(min(filtData2T()$Longitude)+1,min(filtData2T()$Latitude)+2,max(filtData2T()$Longitude)+2,max(filtData2T()$Latitude)-1) %>%
      addMinicharts(
        filtData2()$Longitude, filtData2()$Latitude,
        layerId = filtData2()$Site,width=4,height=4) %>% addResetMapButton()%>% addMiniMap(width=100,height=100,toggleDisplay = TRUE,position = "bottomright")
  })
  observe({
    if (length(input$ancecomp) == 0) {
      data <- 1
    } else if(length(input$ancecomp) <length(grep("Anc",names(table())))){
      data=as.data.frame(cbind(filtData2()[,input$ancecomp],(1-apply(as.data.frame(filtData2()[,input$ancecomp]),1,sum))))
      colnames(data)=c(input$ancecomp,"NON")
    } else {
      data <- filtData2()[,input$ancecomp]
    }
    if(input$type1=="bar"){
      leafletProxy("drawance") %>%
        clearMarkers() %>%
        updateMinicharts(
          layerId = filtData2()$Site,
          chartdata =  data,
          maxValues = maxValue,width=15,height=15,
          type =input$type1,showLabels = input$labels1,legend=TRUE,
          colorPalette = brewer.pal(11, input$colors2)[c(2,5,11,1,9,3,8,4)],
          popup = popupArgs(
            labels = names(data),
            html = labelpop(filtData2(),input$anceslab 
            )
          )
        )
    }else if(input$type1=="pie" & length(grep("Anc",names(table())))>0){
      leafletProxy("drawance") %>%
        clearMarkers() %>% clearControls() %>%
        updateMinicharts(
          layerId = filtData2()$Site,
          chartdata =  data,
          maxValues = maxValue,
          type ="pie",showLabels = input$labels1,legend=TRUE,legendPosition="bottomright",
          colorPalette = brewer.pal(11, input$colors2)[c(2,5,11,1,9,3,8,4)],
          popup = popupArgs(
            labels =  names(data),
            html = labelpop(filtData2(),input$anceslab
            )
          ),
          width = sqrt(filtData2()$Count)*10,transitionTime = 0
        )}else  if(length(select(table(),starts_with("Anc_")))>0){
      leafletProxy("drawance") %>%
        clearMarkers() %>%
        updateMinicharts(
          layerId = filtData2()$Site,
          chartdata =  data,
          maxValues = maxValue,
          type =input$type1,showLabels = input$labels1,legend=TRUE,
          colorPalette = brewer.pal(11, input$colors2)[c(2,5,11,1,9,3,8,4)],
          popup = popupArgs(
            labels =  names(data),
            html = labelpop(filtData2(),input$anceslab
            )
          ),
          width = 10*sqrt(filtData2()$Count),transitionTime = 0
        )
    }
  })
  ############addgrid map and merge###############
  griddataanc1<- reactive({
    req(filtData2(),input$Gridanc)
	if(length(filtData2())>0 &input$Gridanc>0){
    gridmap(filtData2(),input$Gridanc,type="Anc")
	}
  })
  observe({
    if(input$Gridanc>0){
      leafletProxy("drawance",data=griddataanc1()) %>%
        clearMarkers() %>% clearControls() %>%
        removeMinicharts(layerId = filtData2()$Site) %>%
        addMinicharts(as.numeric(griddataanc1()$Longitude), as.numeric(griddataanc1()$Latitude),
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
    req(filtData2T())
    #basemap %>%
    leaflet(options = leafletOptions(zoomControl = FALSE, minZoom = 2.48, maxZoom = 10, dragging = T)) %>%
      addTiles(tilesURL) %>%
      fitBounds(min(filtData2T()$Longitude)+1,min(filtData2T()$Latitude),max(filtData2T()$Longitude)-1,max(filtData2T()$Latitude)-0.5) %>%
      addMinicharts(
        filtData22()$Longitude, filtData22()$Latitude,
        layerId = filtData22()$Site,width=4,height=4) %>% addResetMapButton()%>% addMiniMap(width=100,height=100,toggleDisplay = TRUE,position = "bottomright")
  })
  observe({
    if (length(input$ancecomp) == 0) {
      data22 <- 1
    } else if(length(input$ancecomp) <length(grep("Anc",names(table())))){
      data22=(cbind(filtData22()[,input$ancecomp],(1-apply(as.data.frame(filtData22()[,input$ancecomp]),1,sum))))
      colnames(data22)=c(input$ancecomp,"NON")
    }else {
      data22 <- filtData22()[,input$ancecomp]
    }
    if(input$type1=="bar"){
      leafletProxy("drawance2") %>%
        clearMarkers() %>%
        updateMinicharts(
          layerId = filtData22()$Site,
          chartdata =  data22,
          maxValues = maxValue,width=15,height=15,
          type =input$type1,showLabels = input$labels1,legend=TRUE,
          colorPalette = brewer.pal(11, input$colors2)[c(2,5,11,1,9,3,8,4)],
          popup = popupArgs(
            labels =  names(data),
            html = labelpop(filtData22(),input$anceslab
            )
          )
        )
    }else if(input$type1=="pie" & length(grep("Anc",names(table())))>0){
      leafletProxy("drawance2") %>%
        clearMarkers() %>% clearControls() %>%
        updateMinicharts(
          layerId = filtData22()$Site,
          chartdata =  data22,
          maxValues = maxValue,
          type ="pie",showLabels = input$labels1,legend=TRUE,legendPosition="bottomright",
          colorPalette = brewer.pal(11, input$colors2)[c(2,5,11,1,9,3,8,4)],
          popup = popupArgs(
            labels = input$ancescomp,
            html = labelpop(filtData22(),input$anceslab
            )
          ),
          width = sqrt(filtData22()$Count)*12,transitionTime = 0
        )
    }else if(length(select(table(),starts_with("Anc_")))>0){
      leafletProxy("drawance2") %>%
        clearMarkers() %>%
        updateMinicharts(
          layerId = filtData22()$Site,
          chartdata =  data22,
          maxValues = maxValue,
          type =input$type1,showLabels = input$labels1,legend=TRUE,
          colorPalette = brewer.pal(11, input$colors2)[c(2,5,11,1,9,3,8,4)],
          popup = popupArgs(
            labels = input$ancescomp,
            html = labelpop(filtData22(),input$anceslab
            )
          ),
          width = 10*sqrt(filtData22()$Count),transitionTime = 0
        )
    }
  })
  ############addgrid map and merge###############
  griddataanc2<- reactive({
    req(filtData22(),input$Gridanc)
	if(length(filtData22())>0 &input$Gridanc>0){
    gridmap(filtData22(),input$Gridanc,type="Anc")
	}
  })
  observe({
    if(input$Gridanc>0){
      leafletProxy("drawance2",data=griddataanc2()) %>%
        clearMarkers() %>% clearControls() %>%
        removeMinicharts(layerId = filtData22()$Site) %>%
        addMinicharts(as.numeric(griddataanc2()$Longitude), as.numeric(griddataanc2()$Latitude),
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
      volumes <- shinyFiles::getVolumes()()
      c(volumes)
    } else{
      c(project_root = rootA)
    }
  })
  Thesheets_dirA <- reactive({
    shinyFiles::shinyDirChoose(input, 'sheets_dirA', roots = TherootsA(), session = session)
    shinyFiles::parseDirPath(roots = TherootsA(), input$sheets_dirA)
  })
  output$sheets_dirA <- renderPrint({
    Thesheets_dirA()
  }) 
  #######down grid figures######
  observeEvent(input$Downa, {
    req(filtData2T())
    outpath=Thesheets_dirA()
    gridplot(filtData2T(),as.numeric(input$anceage[1]),as.numeric(input$anceage[2]),input$GridSizea,type="Anc",outpath)
  })
  ##draw count plot
  output$drawanceCount <- renderPlotly({
    req(filtData2(),filtData22())
    dataplot=rbind(filtData2(),filtData22())
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
    req(table())
    selectInput("pcasp",
                label = "Select specie(s)",
                choices = unique(table()$Species),
                multiple = TRUE,selected=unique(table()$Species)[1])
  })
  output$pcaoutsx <- renderUI({
    req(table())
    selectInput("pcasx",
                label = "Select sex",
                choices = unique(table()$Sex),
                multiple = TRUE,selected=unique(table()$Sex)[1])
  })
  output$pcaout <- renderUI({
    sliderInput("pcaage",
                label = "Select a time range",
                min = 0,
                max = table()$Age %>% as.numeric %>% max(na.rm = TRUE),
                step = 100,
                value = range(table()$Age))
  })
  output$pcaout1 <- renderUI({
    selectInput("PC1",
                label = "Select PC1",
                choices = names(table())[grep("Pc",names(table()))],
                multiple = FALSE)
  })
  output$pcaout2 <- renderUI({
    selectInput("PC2",
                label = "Select PC2",
                choices = names(table())[grep("Pc",names(table()))],
                multiple = FALSE)
  })
  output$pcaoutlog <- renderUI({
    sliderInput("pcalog",
                label = "Select a longitude range",
                min = table()$Longitude %>% as.numeric() %>% min(na.rm = TRUE),
                max = table()$Longitude %>% as.numeric %>% max(na.rm = TRUE),
                step = 0.1,
                value = range(table()$Longitude))
  })
  output$pcaoutlat <- renderUI({
    sliderInput("pcalat",
                label = "Select a latitude range",
                min = table()$Latitude %>% as.numeric() %>% min(na.rm = TRUE),
                max = table()$Latitude %>% as.numeric %>% max(na.rm = TRUE),
                step = 1,
                value = range(table()$Latitude))
  })
  ############draw pca#########################
  output$pca <- renderPlotly({
    req(table(),PCA_table())
    drawpca(table(),as.character(input$pcasx),as.character(input$pcasp),as.numeric(input$pcaage[1]),as.numeric(input$pcaage[2]),PCA_table(),input$pcalat[1],input$pcalat[2],input$pcalog[1],input$pcalog[2],(input$PC1),(input$PC2))
  })
  #######reactiveData#######################
  filtData4 <- reactive({
    req(input$pcaage)
    Mergeawsome(table()[table()$Sex%in%input$pcasx & table()$Species%in%input$pcasp & table()$Age >= as.numeric(input$pcaage[1]) & table()$Age < as.numeric(input$pcaage[2]) & table()$Longitude >=as.numeric(input$pcalog[1]) & table()$Longitude <=as.numeric(input$pcalog[2]) & table()$Latitude >=as.numeric(input$pcalat[1]) &table()$Latitude <=as.numeric(input$pcalat[2]),])
  })
  #color 
  colorpal1 <- reactive({
    colorNumeric(brewer_pal(palette = "RdYlGn")(11), filtData4()$Age)
  })
  ###err
  output$errpca <- renderText({
    if(length(select(table(),starts_with("PC")))==0){
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
    req(filtData4(),PCA_table())
    pal <- colorpal1()
    leaflet(options = leafletOptions(zoomControl = FALSE, minZoom = 2, maxZoom = 10, dragging = T)) %>%
      addTiles() %>% 
      fitBounds(min(table()$Longitude),min(table()$Latitude),max(table()$Longitude),max(table()$Latitude)) %>%
      addCircleMarkers(filtData4()$Longitude, filtData4()$Latitude,radius=sqrt(filtData4()$Count)*2
                       ,weight=1,popup=paste0(filtData4()$Sample),color = "#777777",fillColor = pal(filtData4()$Age), fillOpacity = 0.9,stroke = FALSE) %>% 
      addResetMapButton() %>% addMiniMap(width=100,height=100,toggleDisplay = TRUE,position = "topright") %>%
      leaflegend::addLegendSize(position = "bottomleft",col="black",fillColor="white",baseSize=min(sizeNumeric(filtData4()$Count*2,baseSize = mean(filtData4()$Count)*2.5))
                    ,values = sizeNumeric(filtData4()$Count,baseSize = mean(filtData4()$Count)),shape="circle",orientation="horizontal",breaks=5)
  })
  ##################multiple SNPs########################
  #######render UI for multiple SNPs##################
  output$snpoutsp <- renderUI({
    req(table())
    selectInput("snpsp",
                label = "Select Specie(s)",
                choices = unique(table()$Species),
                multiple = TRUE,selected=unique(table()$Species)[1])
  })
  output$snpoutsx <- renderUI({
    req(table())
    selectInput("snpsx",
                label = "Select sex",
                choices = unique(table()$Sex),
                multiple = TRUE,selected=unique(table()$Sex)[1])
  })
  output$snpoutage <- renderUI({
    req(table())
    sliderInput("snpage",
                label = "Select a time range",
                min = 0,
                max = table()$Age %>%
				as.numeric %>%
                  max(na.rm = TRUE),
                step = 10,
                value = range(table()$Age))
  })
  output$snpoutsnp <- renderUI({
    selectInput("snpsnp",
                label = "Select your variations",
                choices = unique(names(table() %>% select(starts_with("SNP_")))),
                multiple = TRUE)
  })
  output$snpoutlog <- renderUI({
   req(table())
    sliderInput("snplog",
                label = "Select a longitude range",
                min = table()$Longitude %>%
                  as.numeric() %>%
                  min(na.rm = TRUE),
                max = table()$Longitude%>%
				as.numeric %>%
                  max(na.rm = TRUE),
                step = 0.1,
                value = range(as.numeric(table()$Longitude)))
  })
  output$snpoutlat <- renderUI({
  req(table())
    sliderInput("snplat",
                label = "Select a latitude range",
                min = table()$Latitude %>%
                  as.numeric() %>%
                  min(na.rm = TRUE),
                max = table()$Latitude %>%
				as.numeric %>%
                  max(na.rm = TRUE),
                step = 1,
                value = range(as.numeric(table()$Latitude)))
				})
  output$errmultsnp <- renderText({
    if(length(select(table(),starts_with("SNP_")))==0){
      validate("The required data were not part of your Input File. Please make sure that columns labelled 'SNP_snpname_A' and 'SNP_snpname_D' are provided")
    }
  })
  ##active data
  filtData5 <- reactive({
	req(input$snpage,table(),input$alltypesnp,input$snplog,input$snplat)
    if(length(select(table(),starts_with("Snp_")))>0 & input$alltypesnp=="ReadCounts"){
      MergeawsomeCount(table()[table()$Sex%in%input$snpsx & table()$Species%in%input$snpsp & table()$Age >= as.numeric(input$snpage[1]) & table()$Age <= as.numeric(input$snpage[2])
                                   & table()$Longitude>=input$snplog[1] & table()$Longitude<=input$snplog[2] & table()$Latitude>=input$snplat[1] & table()$Latitude<=input$snplat[2],])
    }else if(input$alltypesnp=="Genotype"){
	Mergeawsome(table()[table()$Sex%in%input$snpsx & table()$Species%in%input$snpsp & table()$Age >= as.numeric(input$snpage[1]) & table()$Age <= as.numeric(input$snpage[2])
                                   & table()$Longitude>=input$snplog[1] & table()$Longitude<=input$snplog[2] & table()$Latitude>=input$snplat[1] & table()$Latitude<=input$snplat[2],],mult=TRUE,gtp="genotype")
	}else{
      return (NULL)
    }
  })
  output$snpoutlab <- renderUI({
    req(filtData5())
    selectInput("snplab",
                label = "Select labels",
                choices =  names(filtData5())[names(filtData5())%in% c("n.overlaps","origins","SecondSite")==FALSE],
                multiple = TRUE,selected="Sample")
  })
  # render color
  colorpall <- reactive({
    colorNumeric(input$colors3, filtData5()$Count)
  })
  #head
  output$mulhead <- renderText({
    if(length(select(table(),starts_with("Snp_")))>0){
      paste("Allele combination at multiple SNPs",sep=" ")
    }
  })
   filtData5T <- reactive({
 req((input$snpsnp),filtData5())
 as.data.frame(filtData5()[apply(as.data.frame(filtData5()[,input$snpsnp]),1,sum)>0,])
 })
  ##output snp multiple map
  output$snpmap <- renderLeaflet({
    if(length(select(table(),starts_with("Snp_")))>0){
      req(filtData5())
      leaflet(options = leafletOptions(zoomControl = FALSE, minZoom = 2, maxZoom = 10, dragging = T)) %>%
        addTiles(tilesURL) %>%
        fitBounds(min(filtData5T()$Longitude),min(filtData5T()$Latitude),max(filtData5T()$Longitude),max(filtData5T()$Latitude)) %>%
        addMinicharts(
          filtData5T()$Longitude, filtData5T()$Latitude,
          layerId = filtData5T()$Sample) %>% addResetMapButton() %>% addMiniMap(width=100,height=100,toggleDisplay = TRUE,position = "topright")
    }
  })
  observe({
  req(filtData5T())
    if (length(input$snpsnp) == 0) {
      datasnp <- 1
    }else if(length(input$snpsnp) == 1){
	datasnp=filtData5T()[,input$snpsnp]
	} 
    else if(length(input$snpsnp) > 1) {
      datasnp <- filtData5T()[,input$snpsnp]#as.data.frame(filtData5()[rowSums(as.data.frame(filtData5()[,input$snpsnp]))>0,input$snpsnp])
    }
    if(input$type3=="bar" & length(nrow(datasnp)>0)){
      leafletProxy("snpmap") %>%
        clearMarkers() %>%
        updateMinicharts(
          layerId = filtData5T()$Sample,
          chartdata =  datasnp,
          maxValues = maxValue,width=20,height=30,opacity = 0.8,
          type ="bar",showLabels = input$labels3,legend=TRUE,legendPosition="bottomright",
          colorPalette = brewer.pal(9, input$colors3)[c(2,9,1,7,3,8,4,8,1,8,3,9,2,6,4,1,7,3,9,2,8,4)],
          popup = popupArgs(
            labels = colnames(datasnp),
            html = labelpop(filtData5T(),input$snplab
            )
          )
        )
    }else if(input$type3=="pie" & length(select(table(),starts_with("Snp_")))>0){
      leafletProxy("snpmap") %>%
        clearMarkers() %>% clearControls() %>%
        updateMinicharts(
          layerId = filtData5T()$Sample,
          chartdata =  datasnp,
          maxValues = maxValue,
          type ="pie",showLabels = input$labels3,legend=TRUE,legendPosition="bottomright",
          colorPalette = brewer.pal(9, input$colors3)[c(2,9,1,7,3,8,4,8,1,8,3,9,2,6,4,1,7,3,9,2,8,4)],
          popup = popupArgs(
            labels = colnames(datasnp),
            html = labelpop(filtData5T(),input$snplab
            )
          ),
          width = sqrt(filtData5()$Counts)*5,transitionTime = 0
        )
    }else if(length(select(table(),starts_with("Snp_")))>0){
      leafletProxy("snpmap") %>%
        clearMarkers() %>%
        updateMinicharts(
          layerId = filtData5T()$Sample,
          chartdata =  datasnp,
          maxValues = maxValue,
          type =input$type3,showLabels = input$labels3,legend=TRUE,
          colorPalette = brewer.pal(9, input$colors3)[c(2,9,1,7,3,8,4,8,1,8,3,9,2,6,4,1,7,3,9,2,8,4)],
          popup = popupArgs(
            labels = colnames(datasnp),
            html = labelpop(filtData5T(),input$snplab)),
          width = 15*(filtData5T()$Counts),transitionTime = 0)
    }
  })
  ##draw count plot
  output$snpCount <- renderPlotly({
    req(filtData5T(),input$snpsnp)
	ggplot(filtData5T(),aes(x=Age,y=Count))+ggtitle("Temporal distribution of the samples")+theme(
      plot.title = element_text(size=15, family="Arial"))+geom_point()#geom_bar(stat="identity",fill="steelblue")
  })
  ###############################mutiple haplo#########################
  ##########render UI for haplo#########################
   output$hapoutty <- renderUI({
    req(table())
	if(length(grep("Cat_",names(table())))>0){
    selectInput("hapty", 
	label="Select(sub)haplogroups", 
	choices = unique(stringi::stri_replace_first_regex(names(table())[grep("Cat",names(table()))],"Cat_","")),
	multiple = FALSE,
	selected=unique(stringi::stri_replace_first_regex(names(table())[grep("Cat",names(table()))],"Cat_",""))[1])
	}
  })
  output$hapoutsp <- renderUI({
    req(table())
    selectInput("hapsp",
                label = "Select specie(s)",
                choices = unique(table()$Species),
                multiple = TRUE,selected=unique(table()$Species))
  })
  output$hapoutsx <- renderUI({
    req(table())
	req(input$hapty)
      if(length(input$hapty)==1){
	selectInput("hapsx",
                  label = "Select sex",
                  choices = c(Choose='',unique(table()[table()[,paste("Cat_",input$hapty,sep="")]!="unknown","Sex"])),#unique(table()$Sex),
                  multiple = TRUE,unique(table()[,"Sex"][1]))}
  })
  output$hapoutage <- renderUI({
    req(table())
    sliderInput("hapage",
                label = "Select a time range",
                min = 0,
                max = table()$Age %>% as.numeric %>% max(na.rm = TRUE),
                step = 10,
                value = range(table()$Age))
  })
   output$hapoutlog <- renderUI({
    sliderInput("haplog",
                label = "Select a longitude range",
                min = table()$Longitude %>% as.numeric() %>% min(na.rm = TRUE),
                max = table()$Longitude %>%  as.numeric %>% max(na.rm = TRUE),
                step = 0.1,
                value = range(table()$Longitude))
  })
  output$hapoutlat <- renderUI({
    sliderInput("haplat",
                label = "Select a latitude range",
                min = table()$Latitude %>% as.numeric() %>% min(na.rm = TRUE),
                max = table()$Latitude %>% as.numeric %>% max(na.rm = TRUE),
                step = 1,
                value = range(table()$Latitude))
  })
  output$errhap <- renderText({
    if(length(select(table(),starts_with("Cat")))==0){
      validate("The required data were not part of your Input File. Please make sure that columns start with 'Cat' was provided")
    }
  })
  #head
  output$haphead <- renderText({
    if(length(select(table(),starts_with("Cat")))>0){
      paste("Haplo groups",sep=" ")
    }
  })
  #######active data##############
  filtData6 <- reactive({
    req(input$hapage,input$hapty,input$hapsx,input$hapsp,table())
     if(length(select(table(),starts_with("Cat_")))>0 & length(input$hapty)==1){
	 Mergetype(table()[table()$Sex%in%input$hapsx & 
	 table()$Species%in%input$hapsp & table()$Age >= as.numeric(input$hapage[1]) &  table()$Age < as.numeric(input$hapage[2]) &
	 table()$Latitude >= as.numeric(input$haplat[1]) &  table()$Latitude < as.numeric(input$haplat[2]) &
	 table()$Longitude >= as.numeric(input$haplog[1]) &  table()$Longitude < as.numeric(input$haplog[2]),],type=input$hapty)
	}
  })
  output$hapoutlab <- renderUI({
    req(filtData6())
    selectInput("haplab",
                label = "Select labels",
                choices = names(filtData6())[names(filtData6())%in% c("n.overlaps","origins","SecondSite")==FALSE],
                multiple = TRUE,selected="Sample")
  })
  output$hapouthap <- renderUI({
  req(input$hapty,filtData6())
    if(length(select(table(),starts_with("Cat")))>0 & length(input$hapty)==1){
      selectInput("haphap",
                  label = "Select haplotype groups",
                  choices = c(Choose='',names(filtData6())[grep(paste(input$hapty,"_",sep=""),names(filtData6()))]),#gethapname(sort(unique(filtData6()$mtDNAhaplogroup)),"MT_"),
                  multiple = TRUE)
    }
  })
  ############DRAWPLOT#########################
  ##output haplotype map
  output$hapmap <- renderLeaflet({
    if(length(select(table(),starts_with("Cat")))>0){
      req(filtData6())
      leaflet(options = leafletOptions(zoomControl = FALSE, minZoom = 2, maxZoom = 10, dragging = T)) %>% #addProviderTiles("CartoDB.Positron") %>%
        addTiles(tilesURL) %>%
        fitBounds(min(filtData6()$Longitude)-0.3,min(filtData6()$Latitude)-0.3,max(filtData6()$Longitude)+0.3,max(filtData6()$Latitude)+0.3) %>%
        addMinicharts(filtData6()$Longitude, filtData6()$Latitude,
                      layerId = filtData6()$Site,width=4,height=4) %>% addResetMapButton()%>% addMiniMap(width=100,height=100,toggleDisplay = TRUE,position = "topleft") %>%
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
    }else if(length(input$haphap)<length(unique(grep(input$hapty,names(filtData6()))))){
      datahap <- cbind(filtData6()[,input$haphap],(1-apply(as.data.frame(filtData6()[,input$haphap]),1,sum)))
      colnames(datahap)=c(input$haphap,"NON")
    }else if (length(input$haphap)==length(unique(grep(input$hapty,names(filtData6()))))){
      datahap <- filtData6()[,input$haphap]
    }
    if(length(select(table(),starts_with("Cat")))>0){
      leafletProxy("hapmap") %>%
        clearMarkers() %>% clearControls() %>%
        updateMinicharts(
          layerId = filtData6()$Site,
          chartdata =  datahap,
          maxValues = maxValue,
          type ="pie",showLabels = input$labels4,legend=TRUE,legendPosition ="bottomright",
          colorPalette = rcolors::get_color(rcolors::rcolors$t2m_29lev, n = length(colnames(datahap))+1),
          popup = popupArgs(
            html = labelpop(filtData6(),input$haplab
            )
          ),
          width = sqrt(filtData6()$Count)*3,transitionTime = 0
        ) %>% addLegendSize(position = "bottomleft",col="black",fillColor="white",baseSize=quantile(sqrt(filtData6()$Count)*1.5,0.5),
                            values = (filtData6()$Count),shape="circle",orientation="horizontal",breaks=5)
    }
  })
  ############addgrid map and merge###############
  griddatahap<- reactive({
    req(filtData6(),input$hapty,input$Gridhap)
	if(length(filtData6())>1 & input$Gridhap>0){
    gridmap(filtData6(),input$Gridhap,type=input$hapty)
	}
  })
 observe({
  req(griddatahap())
    if(input$Gridhap>0){
      leafletProxy("hapmap",data=griddatahap()) %>%
        clearMarkers() %>% clearControls() %>%
        removeMinicharts(layerId = filtData6()$Site) %>%
        addMinicharts(as.numeric(griddatahap()$Longitude), as.numeric(griddatahap()$Latitude),legend=TRUE,legendPosition ="bottomright",
                      chartdata=data.frame(lapply(griddatahap()[,names(griddatahap())[grep(input$hapty,names(griddatahap()))]],as.numeric)),type="pie",
                      colorPalette = rcolors::get_color(rcolors::rcolors$t2m_29lev, n = length(grep(input$hapty,names(griddatahap())))+1),
                      width = 2*sqrt(as.numeric(griddatahap()$Count)), popup = popupArgs(
                        #labels = input$haphap,
                        html = labelpop(griddatahap(),input$haplab
                        )))%>%
        addGraticule(interval = input$Gridhap, style = list(color = "#FF0000", weight = 0.1,fillOpacity=0.1))
    }
  })
  ############################################### click response ##################################################
  observeEvent(input$hapmap_draw_new_feature,{
    req(filtData6())
	if(input$Gridhap==0){
    found_in_bounds <- findLocations(shape = input$hapmap_draw_new_feature
                                     , location_coordinates = SpatialPointsDataFrame(filtData6()[,c('Longitude', 'Latitude')] , filtData6())
                                     , location_id_colname = "Site")
    for(id in found_in_bounds){
      if(id %in% data_of_click$clickedMarker){
      } else {
        data_of_click$clickedMarker<-append(data_of_click$clickedMarker, id, 0)
      }
    }
    selected <- subset(filtData6(), Site %in% data_of_click$clickedMarker)
    ######age count plot
    output$hapCount <- renderPlotly({
      req(selected)
      ggplot(selected,aes(x=Age,y=Count))+geom_point()+ggtitle("Temporal distribution of the samples")+theme(
        plot.title = element_text(size=15, family="Arial"),plot.margin=unit(c(1,1,2,1),'lines'))
    })
    leafletProxy("hapmap",data=selected)%>% 
      addCircleMarkers(lng=selected$Longitude,lat=selected$Latitude,
                       color = "red",radius=sqrt(selected$Count)*2,weight=1,
                       fillOpacity = 0.5,            
                       layerId = (selected$SecondSite))
  }
  })
  #for grid
  observeEvent(input$hapmap_draw_new_feature,{
	if(as.numeric(input$Gridhap)>0){
    found_in_bounds1 <- findLocations(shape = input$hapmap_draw_new_feature
                                     , location_coordinates = SpatialPointsDataFrame(griddatahap()[,c('Longitude', 'Latitude')] , griddatahap())
                                     , location_id_colname = "Site")
    for(id in found_in_bounds1){
      if(id %in% data_of_click$clickedMarker){
      } else {
        data_of_click$clickedMarker<-append(data_of_click$clickedMarker, id, 0)
      }
    }
    selected <- subset(griddatahap(), Site %in% data_of_click$clickedMarker)
    ######age count plot
    output$hapCount <- renderPlotly({
      req(selected)
      ggplot(selected,aes(x=Age,y=Count))+geom_point()+ggtitle("Temporal distribution of the samples")+theme(
        plot.title = element_text(size=15, family="Arial"),plot.margin=unit(c(1,0.3,0.5,0.3),'cm'))
    })
    leafletProxy("hapmap",data=selected)%>% 
      addCircleMarkers(lng=selected$Longitude,lat=selected$Latitude,
                       color = "red",radius=sqrt(selected$Count)/1.2,weight=1,
                       fillOpacity = 0.5,            
                       layerId = (selected$SecondSite))
  }
  })
  ############################################### click response ##################################################
  observeEvent(input$hapmap_draw_deleted_features,{
  if(input$Gridhap==0){
    for(feature in input$hapmap_draw_deleted_features$features){
      bounded_layer_ids <- findLocations(shape = feature
                                         , location_coordinates = SpatialPointsDataFrame(filtData6()[,c('Longitude', 'Latitude')] , filtData6())
                                         , location_id_colname = "SecondSite")
      output$hapCount <- renderPlotly({
        return(NULL)
      })
      proxy <- leafletProxy("hapmap")
      proxy %>% removeMarker(layerId = as.character(bounded_layer_ids))
      first_layer_ids <- subset(filtData6(), SecondSite %in% bounded_layer_ids)$Site
      data_of_click$clickedMarker <- data_of_click$clickedMarker[!data_of_click$clickedMarker
                                                                 %in% first_layer_ids]
    }
	}
  })
   observeEvent(input$hapmap_draw_deleted_features,{
  if(input$Gridhap>0){
    for(feature in input$hapmap_draw_deleted_features$features){
      bounded_layer_ids <- findLocations(shape = feature
                                         , location_coordinates = SpatialPointsDataFrame(griddatahap()[,c('Longitude', 'Latitude')] , griddatahap())
                                         , location_id_colname = "SecondSite")
      output$hapCount <- renderPlotly({
        return(NULL)
      })
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
      volumes <- shinyFiles::getVolumes()()
      c(volumes)
    } else{
      c(project_root = root1)
    }
  })
  Thesheets_dir1 <- reactive({
    shinyFiles::shinyDirChoose(input, 'sheets_dir1', roots = Theroots1(), session = session)
    shinyFiles::parseDirPath(roots = Theroots1(), input$sheets_dir1)
  })
  output$sheets_dir1 <- renderPrint({
    Thesheets_dir1()
  })
  observeEvent(input$Draw, {
    req(Para_table())
    outpath=Thesheets_dir1()
    automaticFigures(Para_table(),outpath)
  })
})
