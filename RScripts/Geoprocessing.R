#Geoprocessing

#install.packages("whitebox", repos="http://R-Forge.R-project.org")

library(tidyverse)
library(raster)
library(sf)
library(whitebox)
library(tmap)
library(stars)
library(rayshader)
library(rgl)
library(here)

whitebox::wbt_init()

knitr::knit_hooks$set(webgl = hook_webgl)

theme_set(theme_classic())


##SO! Our process will look like this:
#  Read in DEM
tmap_mode("view")


#Fill single cell sinks then breach breach larger sinks
#Create D8 flow accumulation and D8 pointer grids
#Read in pour points
#Create stream raster
#Snap pour points to stream raster
#*Run watershed function