library(raster)
library(rgdal)

setwd("D:/Trabalhando/PT_WAWASAN/DataBase")

s1 = shapefile("Papua_ForestLost_2001_2018.shp")

def_2001 = subset(s1, s1@data$Year == "2001")
def_2018 = subset(s1,s1@data$Year == "2018")

writeOGR(def_2001, ".", "def_2001", driver="ESRI Shapefile") 
writeOGR(def_2018, ".", "def_2018", driver="ESRI Shapefile")
