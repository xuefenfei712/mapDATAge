# Welcome to mapDATAge

## Description

The mapDATAge package is designed to explore the presence of geographic and temporal patterns in ancient DNA data. It takes simple tabulated text files as input and contains different panels to draw a number of maps and plots that are common to most ancient DNA studies, including drawing: 
(1) the spatial and temporal distributions of a given set of samples (SMAP) or alleles (AMAP);
(2) temporal allelic trajectories (TRAJECTORY);
(3) maps of individual ancestry profiles on a map (ANCESTRY);
(4) PCA (or MDS) and map related maps of spatio-temporal distributions (PCA);
(5) spatio-temporal distributions of alleles at one or multiple loci (MULTIPLESNPS);
(6) maps of (sub-)haplogroups (HAPLO), and;
(7) finally, options are provided to automatically generate figures, following the selection of a preselected range of temporal and spatial and temporal parameters, which can be useful to contrast data from different loci and/or species (ONECLICK). 

## Getting Started
A manual providing instructions and examples for using mapDATAge can be found [here!](https://github.com/xuefenfei712/mapDATAge/blob/main/mapDATAgeInstructions.pdf)
And a video tutorial is linked [here!](https://youtu.be/hx83sUsEYh8)
### Installing

`packages=c("plotly","shiny","leaflet","shinythemes","RColorBrewer","esquisse","scales",
"ggplot2","plotly","markdown","leaflet.minicharts","htmltools","leaflegend","sf","sp","stringi","leaflet.extras",
"dplyr","rcolors","DT","forcats","foreach","htmlwidgets")`

`install.packages(packages, repo="http://cran.rstudio.org", dependencies=TRUE)`

`install.packages("remotes")`
`remotes::install_github("thomasp85/shinyFiles", upgrade = "never")`

### Launch mapDATAge from a URL or Github

Please run one of the two commands below to launch mapDATAge in R or Rstudio

`shiny::runUrl("https://github.com/xuefenfei712/mapDATAge/archive/refs/heads/main.zip")`


`shiny::runGitHub("mapDATAge","xuefenfei712", ref="main")`

These two commands will download the code of mapDATAge from Github to a local, temporary directory on your computer and then launch the mapDATAge in your default web browser. Once the web browser is closed, the downloaded code is deleted from your computer.

If you want to use mapDATAge locally, please download the source code from Github to a local directory on your computer, such as 'C:\\MapR' on windows. Following the procedure illustrated in the following figure, a zip file named 'mapDATAge-master.zip' can be downloaded to your local computer disk. Move this file to 'C:\\MapR' and unzip it. Then a directory named 'mapDATAge-main' can be found in 'C:\\MapR'. The scripts 'Server.R' and 'ui.R' could be found in 'C:\\MapR\\mapDATAge-main'. 

![image](https://github.com/xuefenfei712/mapDATAge/blob/main/mapdatege-download.png)

Then you can start mapDATAge by running the command line below:

`path="C:\\MapR\\mapDATAge-main"`


`shiny::runApp(appDir = path)`

## Authors

Xuexue Liu: xuexue.liu@univ-tlse3.fr

## Citation

Xuexue Liu, Ludovic Orlando, mapDATAge: a ShinyR package to chart ancient DNA data through space and time.

Centre for Anthropobiology and Genomics of Toulouse (CAGT) (https://cagt.cnrs.fr/)

## Acknowledgements

We thank Dr Pablo Librado for fruitful comments on earlier versions of the manuscript and for formatting the horse ancestry data.
