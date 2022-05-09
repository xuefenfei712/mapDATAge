# Welcome to mapDATAge

## Description

The mapDATAge package is designed to explore the presence of geographic and temporal patterns in ancient DNA data. It takes simple tabulated text files as input and contains different panels to draw a number of maps and plots that are common to most ancient DNA studies, including drawing: 
(1) the spatial and temporal distributions of a given set of samples (SMAP) or alleles (AMAP);
(2) temporal allelic trajectories (TRAJECTORY);
(3) maps of individual ancestry profiles on a map (ANCESTRY);
(4) PCA (or MDS) and map related maps of geographicspatio--temporal distributions (PCA);
(5) geographicspatio-temporal distributions of alleles at one or multiple loci (MULTIPLESNPS);
(6) (sub)haplogroup on adistribution maps (HAPLO), and;
(7) finally, options are provided to automatically generate figures applying a preselected range of temporal and spatial and temporal parameters, which can be useful to contrast data from different loci and/or species (ONECLICK). 

## Getting Started
Help manual for the useage of mapDATAge ![click here!](https://github.com/xuefenfei712/mapDATAge/blob/main/mapDATAgeInstructions.pdf)

### Installing

`packages=c("plotly","shiny","shinyFiles","leaflet","shinythemes","RColorBrewer","esquisse","scales",
"ggplot2","plotly","markdown","leaflet.minicharts","htmltools","leaflegend","sf","sp","stringi","leaflet.extras",
"dplyr","rcolors","DT","forcats","foreach")`

`install.packages(packages, repo="http://cran.rstudio.org", dependencies=TRUE)`

### Launch mapDATAge from R and Github

Plese run this command below to lunch mapDATAge

`shiny::runUrl("https://github.com/xuefenfei712/mapDATAge/archive/refs/heads/main.zip")`
OR
`shiny::runGitHub("mapDATAge","xuefenfei712", ref="main")`

These two command will download the code of mapDATAge from Github to a temporary direction of your computer and then launch the mapDATAge in the web browser. Once the web brower was closed, the downloaded code would be deleted from your computer.

If you want to use it locally, please download the source code of mapDATAge from Github to a fixed directory of your computer, such as 'C:\\MapR' on windows. Following the procedure illustrated in the following figure, a zip file named 'mapDATAge-master.zip' would be downloaded to the disk of your computer. Move this file to 'C:\\MapR' and unzip this file. Then a directory named 'mapDATAge-main' would be generated in 'C:\\MapR'. The scripts 'Server.R' and 'ui.R' could be found in 'C:\\MapR\\mapDATAge-main'. 

![image](https://github.com/xuefenfei712/mapDATAge/blob/main/mapdatege-download.png)

Then you can start mapDATAge app by running the command line below:

`path="C:\\MapR\\mapDATAge-main"
shiny::runApp(appDir = path)`

## Authors

Xuexue Liu: xuexue.liu@univ-tlse3.fr

## Citation

XL, OL, [mapDATAge: a ShinyR package to chart ancient DNA data through space and time]

Centre for Anthropobiology and Genomics of Toulouse (CAGT) (https://cagt.cnrs.fr/)

## Acknowledgement

We thank Dr Pablo Librado for fruitful comments on earlier versions of the manuscript and for formatting the horse ancestry data.
