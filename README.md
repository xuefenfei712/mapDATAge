## Welcome to mapDATAge

The mapDATAge package is designed to explore the presence of geographic and temporal patterns in ancient DNA data.<br/>
It takes simple tabulated text files as input and contains different modules to draw a number of maps and plots that are common to many ancient DNA analyses, including drawing: <br/>
(1) the spatial and temporal distributions of a given set of samples (SMAP) or alleles (AMAP);<br/>
(2) temporal allelic trajectories (TRAJECTORY);<br/>
(3) individual ancestry profiles on a map (ANCESTRY);<br/>
(4) PCA (MDS) and map geographic-temporal distributions (PCA);<br/>
(5) geographic-temporal distributions of alleles at one or multiple loci (MULTIPLESNPS);<br/>
(6) (sub)haplogroup on a map(HAPLO), and;<br/>
(7) Finally, options are provided to automatically generate figures applying a preselected range of time and spatial parameters, which can be useful to contrast data from different loci and/or species (ONECLICK).<br/>
mapDATAge is implemented and maintained in ShinyR by Dr [Xuexue Liu] (https://cagt.cnrs.fr/liu-xuexue/) and supervised by Prof [Ludovic Orlando] (https://cagt.cnrs.fr/orlando-ludovic/) at the [Centre for Anthropobiology and Genomics of Toulouse (CAGT)](https://cagt.cnrs.fr/).
The underlying code is available at [Github].
Please cite the following publication XL, OL, [mapDATAge: a ShinyR package to chart ancient DNA data through space and time]
Please contact Dr [Xuexue Liu] xuexue.liu@univ-tlse3.fr, if you have any questions about running, or suggestions for improvement.


#To install the packages used in mapDATAge, please use:<br/>

packages=c("plotly","shiny","shinyFiles","leaflet","shinythemes","RColorBrewer","esquisse","scales",
"ggplot2","plotly","markdown","leaflet.minicharts","htmltools","leaflegend","sf","sp","stringi","leaflet.extras",
"dplyr","rcolors").<br/>
