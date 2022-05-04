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
[Here is a detail instruction for running] (https://github.com/xuefenfei712/mapDATAge/mapDATAgeInstructions.docx)

### Installing

packages=c("plotly","shiny","shinyFiles","leaflet","shinythemes","RColorBrewer","esquisse","scales",
"ggplot2","plotly","markdown","leaflet.minicharts","htmltools","leaflegend","sf","sp","stringi","leaflet.extras",
"dplyr","rcolors","DT","forcats")

<<<<<<< HEAD
install.packages(packages, repo="http://cran.rstudio.org", dependencies=TRUE)

### Runing via a URL

Plese run this command below to lunch mapDATAge
if(interactive()){
shiny::runUrl("https://github.com/xuefenfei712/mapDATAge/archive/refs/heads/main.zip")
}

## Authors

Xuexue Liu: xuexue.liu@univ-tlse3.fr
>>>>>>> a6204225e4d719d6fdad8bcaef5cf23bbafc4e49

## Citation

XL, OL, [mapDATAge: a ShinyR package to chart ancient DNA data through space and time]

Centre for Anthropobiology and Genomics of Toulouse (CAGT) (https://cagt.cnrs.fr/)

## Acknowledgement

We thank Dr Pablo Librado for fruitful comments on earlier versions of the manuscript and for formatting the horse ancestry data.
