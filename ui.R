library(DT)
library(plotly)
library(shiny)
library(shinyFiles)
library(leaflet)
library(shinythemes)
library(RColorBrewer)
library(esquisse)
library(scales)
library(ggplot2)
library(markdown)
library(leaflet.minicharts)
library(htmltools)
library(leaflegend)
library(sf)
library(sp)
library(stringi)
library(leaflet.extras)
library(dplyr)
library(rcolors)
library(forcats)

source("code/validate_input.R")
source("code/freq-plot.R")
source("code/drawPCA.R")
source("code/grindplot.R")
source("code/findLocations.R")
source("code/gridmap.R")
source("code/Mergeawsome.R")
ui=shinyUI(bootstrapPage(theme = shinytheme("sandstone"),
                         # headerPanel("aDNA Data exploration"),
                         titlePanel(
                           title=div(
                             span("mapDATAge: ", style = "font-size:36px;color:white;"), 
                             span("a ShinyR package to chart ancient DNA data through space and time", style = "font-size:28px;color:white;"),
                             style = "background-color:#191970;margin-left: 15px;margin-right: 15px;margin-top: 20px;margin-bottom: 10px;"
                           ), windowTitle = "Welcome to mapDATAge!"
                         ),
                         sidebarPanel(     
                           ## conditionalPanel() functions for selected tab
                           conditionalPanel(condition = "input.tabselected == -999"),#img(src="cagt.jpg",height="10%",width="10%")),
                           # For Panel 1, have an input option
                           conditionalPanel(condition = "input.tabselected == 1",
                                            h3("Import Data"),
                                            fileInput("in_taxon_table", "Please select your table.\n
                                                   Note: this should be saved either as *.txt or *.tsv",
                                                      accept = c(".txt", ".tsv")),
                                            h3("Press the button below to varify the Input data!"),
                                            actionButton("go", "Run mapDATAge!"),
                                            textOutput("fileStatus")
                           ),
                           # On panel 2 (map), ask whether users want select sex and species
                           conditionalPanel(condition = "input.tabselected == 2",
                                              palettePicker(
                                              inputId = "colors", 
                                              label = "Select your colors:", 
                                              choices = list(
                                                "BrBG" = brewer_pal(palette = "BrBG")(11),
                                                "PiYG" = brewer_pal(palette = "PiYG")(11),
                                                "PRGn" = brewer_pal(palette = "PRGn")(11),
                                                "PuOr" = brewer_pal(palette = "PuOr")(11),
                                                "RdBu" = brewer_pal(palette = "RdBu")(11),
                                                "RdGy" = brewer_pal(palette = "RdGy")(11),
                                                "RdYlBu" = brewer_pal(palette = "RdYlBu")(11),
                                                "RdYlGn" = brewer_pal(palette = "RdYlGn")(11),
                                                "Spectral" = brewer_pal(palette = "Spectral")(11),
                                                "Accent" = brewer_pal(palette = "Accent")(8),
                                                "Dark2" = brewer_pal(palette = "Dark2")(8),
                                                "Paired" = brewer_pal(palette = "Paired")(12),
                                                "Pastel1" = brewer_pal(palette = "Pastel1")(9),
                                                "Pastel2" = brewer_pal(palette = "Pastel2")(8),
                                                "Set1" = brewer_pal(palette = "Set1")(9),
                                                "Set2" = brewer_pal(palette = "Set2")(8),
                                                "Set3" = brewer_pal(palette = "Set3")(12)),selected="Set1",
                                              plainColor = TRUE, 
                                              textColor = "white"),
                                            fluidRow(
                                              column(6,uiOutput("mapoutsx")),column(6,uiOutput("mapoutsp"))),uiOutput("mapout"),
                                            uiOutput("mapoutlat"),uiOutput("mapoutlog"),uiOutput("mapoutlab")
                           ),
                           # On panel 3 (pie), ask whether users want select sex and species
                           conditionalPanel(condition = "input.tabselected == 3 ", 
                                            palettePicker(
                                              inputId = "colors1", 
                                              label = "Select your colors:", 
                                              choices = list(
                                                "BrBG" = brewer_pal(palette = "BrBG")(11),
                                                "PiYG" = brewer_pal(palette = "PiYG")(11),
                                                "PRGn" = brewer_pal(palette = "PRGn")(11),
                                                "PuOr" = brewer_pal(palette = "PuOr")(11),
                                                "RdBu" = brewer_pal(palette = "RdBu")(11),
                                                "RdGy" = brewer_pal(palette = "RdGy")(11),
                                                "RdYlBu" = brewer_pal(palette = "RdYlBu")(11),
                                                "RdYlGn" = brewer_pal(palette = "RdYlGn")(11),
                                                "Spectral" = brewer_pal(palette = "Spectral")(11)),
                                              selected="RdYlGn",
                                              plainColor = TRUE, 
                                              textColor = "white"),uiOutput("pieoutsnp"),
                                            fluidRow(
                                              column(6,selectInput("pietype", "Select Allele type", choices = c(Choose='',"ReadCounts","Genotype"), multiple = FALSE,selectize=FALSE)),
                                              column(6,selectInput("type", "Chart type", choices = c("bar","pie","polar-area", "polar-radius"),selected="pie")),
                                              #column(1,checkboxInput("labels", "Show values"))
                                              ),
                                            fluidRow(
                                              column(6,uiOutput("pieoutsx")),column(6,uiOutput("pieoutsp"))),uiOutput("pieout"),
                                            uiOutput("pieoutlat"),uiOutput("pieoutlog"),uiOutput("pieoutlab"),
                                            numericInput("GridSize","Time interval (years)",1000,min=100,max=10000),
                                            textInput("root", "Please enter your project root:"),
                                            shinyFiles::shinyDirButton(id = 'sheets_dir', label = "Path to your output folder", title = "Sheets Folder Select"),
                                            verbatimTextOutput("sheets_dir"),
                                            actionButton("Down","Draw temporal maps"),
                                            numericInput("Gridpie","Grid Size for geographic binning",0,min=0,max=10)
                           ),
                           # On panel 4 (allefreq),  ask whether users want select sex and species
                           conditionalPanel(condition = "input.tabselected == 4 ",
                                            fluidRow(column(8,uiOutput("allsnp")),column(4,uiOutput("allall"))),
                                            fluidRow(
                                              column(6,uiOutput("alloutsx")),column(6,uiOutput("alloutsp"))),
                                            fluidRow(column(6,numericInput("WinSize","Window Size",1000,min=100,max=10000)), column(6,numericInput("StepSize","Step Size",500,min=100,max=10000))),
                                            uiOutput("allout"),
                                            uiOutput("alloutlat"),uiOutput("alloutlog"),
											                      fluidRow(column(6,selectInput("alltype", "Select Allele type", choices = c(Choose='',"ReadCounts","Genotype"), multiple = FALSE,selectize=FALSE)),column(6,uiOutput("allsampling"))),
                                            fluidRow(column(6,uiOutput("alloutymin")),column(6,uiOutput("alloutymax")))),
                           ### On panel 5 (ancestral component), ask whether uses want to select sex and species
                           conditionalPanel(condition = "input.tabselected == 5 ",
                                            palettePicker(
                                              inputId = "colors2", 
                                              label = "Select your colors:", 
                                              choices = list(
                                                "BrBG" = brewer_pal(palette = "BrBG")(11),
                                                "PiYG" = brewer_pal(palette = "PiYG")(11),
                                                "PRGn" = brewer_pal(palette = "PRGn")(11),
                                                "PuOr" = brewer_pal(palette = "PuOr")(11),
                                                "RdBu" = brewer_pal(palette = "RdBu")(11),
                                                "RdGy" = brewer_pal(palette = "RdGy")(11),
                                                "RdYlBu" = brewer_pal(palette = "RdYlBu")(11),
                                                "RdYlGn" = brewer_pal(palette = "RdYlGn")(11),
                                                "Spectral" = brewer_pal(palette = "Spectral")(11)),
                                              selected="RdYlGn",
                                              plainColor = TRUE, 
                                              textColor = "white"),
                                            fluidRow(
                                              column(6,selectInput("type1", "Chart type", choices = c("bar","pie", "polar-area", "polar-radius"),selected="pie")),
                                              column(6,checkboxInput("labels1", "Show proportions"))),
                                            fluidRow(
                                              column(6,uiOutput("ancesoutsx")),column(6,uiOutput("ancesoutsp"))),uiOutput("ancesout"),uiOutput("ancesout1"),
                                            uiOutput("ancesoutcut"),uiOutput("ancesoutlab"),
                                            numericInput("GridSizea","Time interval (years)",1000,min=100,max=10000),
											textInput("rootA", "Please enter your project root:"),
                                            shinyFiles::shinyDirButton(id = 'sheets_dirA', label = "Path to your output folder", title = "Sheets Folder Select"),
                                            verbatimTextOutput("sheets_dirA"),
                                            actionButton("Downa","Draw temporal maps"),
                                            numericInput("Gridanc","Grid Size for geographic binning",0,min=0,max=10)
                           ),
                           # On panel 6 (pca), ask whether users want select sex and species
                           conditionalPanel(condition = "input.tabselected == 6",
                                            h3("Import eigenval file"),
                                            fileInput("in_pca_table", "Upload the proportion of variance explained by each axis (evalout-like file).\n
                                                      Note: this should be saved either as *.txt or *.csv",
                                                      accept = c(".txt", ".tsv")),
                                            fluidRow(
                                              column(6,uiOutput("pcaoutsx")),column(6,uiOutput("pcaoutsp"))),uiOutput("pcaout"),
                                            fluidRow(
                                              column(6,uiOutput("pcaout1")),column(6,uiOutput("pcaout2"))),
                                            uiOutput("pcaoutlog"),uiOutput("pcaoutlat")
                           ),
                           # On panel 7 (snp), ask whether users want how many SNPs used
                           conditionalPanel(condition = "input.tabselected == 7",
                                            palettePicker(
                                              inputId = "colors3", 
                                              label = "Select your colors:", 
                                              choices = list(
                                                "BrBG" = brewer_pal(palette = "BrBG")(11),
                                                "PiYG" = brewer_pal(palette = "PiYG")(11),
                                                "PRGn" = brewer_pal(palette = "PRGn")(11),
                                                "PuOr" = brewer_pal(palette = "PuOr")(11),
                                                "RdBu" = brewer_pal(palette = "RdBu")(11),
                                                "RdGy" = brewer_pal(palette = "RdGy")(11),
                                                "RdYlBu" = brewer_pal(palette = "RdYlBu")(11),
                                                "RdYlGn" = brewer_pal(palette = "RdYlGn")(11),
                                                "Spectral" = brewer_pal(palette = "Spectral")(11)),
                                              selected="RdYlGn",
                                              plainColor = TRUE, 
                                              textColor = "white"),
											  fluidRow(column(8,uiOutput("snpoutsnp")),column(4,selectInput("alltypesnp", "Select Allele type", choices = c(Choose='',"ReadCounts","Genotype"), multiple = FALSE,selectize=FALSE))),
                                            fluidRow(column(6,
                                                            selectInput("type3", "Chart type", choices = c("bar","pie","polar-area", "polar-radius"),selected="bar")),
                                                     column(6,checkboxInput("labels3", "Show values"))),
                                            fluidRow(
                                              column(6,uiOutput("snpoutsx")),column(6,uiOutput("snpoutsp"))),
                                            uiOutput("snpoutage"),
                                            uiOutput("snpoutlat"),uiOutput("snpoutlog"),uiOutput("snpoutlab")#,
                                            #numericInput("GridSize2","Time interval (years)",1000,min=100,max=10000),
                           ),
                           # On panel 8 (haplo), ask whether users want select sex and species
                           conditionalPanel(condition = "input.tabselected == 8 ", 
                                            fluidRow(
                                              column(6,uiOutput("hapoutty")),
                                              column(6,checkboxInput("labels4", "Show values"))),
                                            fluidRow(
                                              column(6,uiOutput("hapoutsx")),column(6,uiOutput("hapoutsp"))),
                                            uiOutput("hapoutage"), uiOutput("hapoutlat"),uiOutput("hapoutlog"),
                                            uiOutput("hapouthap"),uiOutput("hapoutlab"),
                                            numericInput("Gridhap","Grid Size for geographic binning",0,min=0,max=10)
                           ),
                           ####on panel 9 autom download
                           conditionalPanel(condition = "input.tabselected == 9 ",
                                            h3("Import parameter file"),
                                            fileInput("in_para_table", "Please select your parameter file. Note: this should be saved either as *.txt or *.tsv",
                                                      accept = c(".txt", ".tsv")),
                                            textInput("root1", "Please enter your project root:"),
                                            shinyFiles::shinyDirButton(id = 'sheets_dir1', label = "Path to your project folder", title = "Sheets Folder Selector"),
                                            verbatimTextOutput("sheets_dir1"),
                                            actionButton("Draw", "Generate Plots")
                           )),
                         
                         mainPanel(
                           tags$head(
                             tags$style(HTML("
                          .shiny-output-error-validation {
                          color: #ff0000;
                          font-weight: bold;
                          }
                        "))
                           )  ,
                           tabsetPanel(
                             tabPanel("Welcome", value = -999,
                                      includeMarkdown("md/welcome-page.md")
                             ),
                             tabPanel("Data", value = 1,
                                      h4("Please verify the input file, and click on RUN MAPDATAGE!"),
                                      h4("Input Data file"),
                                      DT::dataTableOutput("print_taxon_table")
                             ),
                             #map
                             tabPanel("smap", value = 2,
                                      includeMarkdown("md/drawmap.md"),
                                      textOutput("maphead"),
                                    leafletOutput("drawmap"),
                                      plotlyOutput("drawmapCount")
                                      ,tags$head(tags$style("#maphead{color: black;
                                 font-size: 25px;
                                 font-style: bold;
                                 }"))),
                             # pie plots
                             tabPanel("amap", value = 3,
                                      includeMarkdown("md/pie.md"),
                                      textOutput("errpie"),
                                      textOutput("piehead"),
                                      leafletOutput("pie"),
                                      plotlyOutput("pieCount"),
                                      tags$head(tags$style("#piehead{color: black;
                                 font-size: 25px;
                                 font-style: bold;
                                 }"))
                             ),
                             # allele plot
                             tabPanel("trajectory", value = 4,
                                      includeMarkdown("md/allele.md"),
                                      textOutput("errtraj"),
                                      textOutput("trajhead"),
                                      leafletOutput("deallele"),
                                      plotlyOutput("alle"),
                                      tags$head(tags$style("#trajhead{color: black;
                                 font-size: 25px;
                                 font-style: bold;
                                 }"))
                             ),
                             ###ancestral plot
                             tabPanel("ancestry", value = 5,
                                      includeMarkdown("md/drawance.md"),
                                      textOutput("errance"),
                                      textOutput("anchead"),
                                      textOutput("upanc"),
                                      leafletOutput("drawance"),textOutput("downanc"),leafletOutput("drawance2"),
                                      plotlyOutput("drawanceCount"),
                                      tags$head(tags$style("#upanc{color: black;
                                 font-size: 15px;
                                 #font-style: plain;
                                 }")),
                                      tags$head(tags$style("#downanc{color: black;
                                 font-size: 15px;
                                 #font-style: plain;
                                 }")),
                                      tags$head(tags$style("#anchead{color: black;
                                 font-size: 25px;
                                 font-style: bold;
                                 }"))
                             ),
                             ###pca plot
                             tabPanel("pca", value = 6,
                                      textOutput("errpca"),
                                      includeMarkdown("md/pca.md"),
                                      plotlyOutput("pca"),
                                      textOutput("pcahead"),
                                      leafletOutput("pcamap"),
                                      tags$head(tags$style("#pcahead{color: black;
                                 font-size: 23px;
                                 font-style: bold;
                                 }"))
                             ),
                             ###snp plot
                             tabPanel("MultipSNPs", value = 7,
                                      textOutput("errmultsnp"),
                                      includeMarkdown("md/allsnp.md"),
                                      textOutput("mulhead"),
                                      leafletOutput("snpmap"),
                                      plotlyOutput("snpCount"),
                                      tags$head(tags$style("#mulhead{color: black;
                                 font-size: 23px;
                                 font-style: bold;
                                 }"))
                             ),
                             ###allsnp plot
                             tabPanel("haplo", value = 8,
                                      textOutput("errhap"),
                                      includeMarkdown("md/hap.md"),
                                      textOutput("haphead"),
                                      leafletOutput("hapmap"),
                                      plotlyOutput("hapCount"),
                                      tags$head(tags$style("#haphead{color: black;
                                 font-size: 23px;
                                 font-style: bold;
                                 }"))
                             ),
                             ######automatic
                             tabPanel("oneclick",value=9,
                                      includeMarkdown("md/automatic.md"),
                                      DT::dataTableOutput("print_para_table")
                             ),
                             id = "tabselected"
                           )
                         )
))