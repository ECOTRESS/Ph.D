# Install rgee
#remotes::install_github("r-spatial/rgee", force = T)
#install.packages("devtools")
#install.packages("geojsonio")
#rgee::ee_install() # Set up phyton env (not needed)

library(devtools)
#install_github("JesJehle/earthEngineGrabR")
library(earthEngineGrabR)

# Load packs
library(rgee)
library(sf)
library(tidyverse)
library(geojsonio)
library(mapview)
library(webshot)
library(earthEngineGrabR)

# Request hotpixels
for (i in 2001:2019) {
  print(i)

    fire_data = ee_grab(data = ee_data_collection(datasetID =  'FIRMS',
                                                spatialReducer = "max",
                                                temporalReducer = "sum",
                                                resolution = 1000,
                                                timeStart = paste0(i,"-01-01"),
                                                timeEnd = paste0(i,"-12-31"),
                                                bandSelection = 'T21'
  ),
  targetAreaAssetPath = "users/lrmodelagem/MAPBIOMAS/xingu_municipios_spera", # Use your username instead
  download_path = getwd()
  )
}

# import hotpixels into R

hotpixel_processing = function(caminho) {

  l = list.files(caminho, pattern = "FIRMS_s-max_t-sum_") # List dowloaded files
  
  fire_list = lapply(l, st_read) # Read all files
  
  fire_list_sub = lapply(fire_list, "[",c(14,20)) # Subset list data frames
  
  ID = c(seq(2001,2019,1)) # Years
  
  fire_list_sub_year = mapply(cbind, fire_list_sub, "Year"=ID, SIMPLIFY=F) # Add year into list
  
  new_col_name <- c("Municipality","HotPixels", "Year","geometry")
  
  fire_list_sub_year_named = lapply(fire_list_sub_year, setNames, nm = new_col_name) # Change col names
  
  fire_list_sub_year_named_complete = do.call(rbind, fire_list_sub_year_named) # Full df
 
  return(df_hotpixels = fire_list_sub_year_named_complete) 
  
}

# Visualize
fire = hotpixel_processing("/Users/usuario/")

ggplot(fire) + aes(x = Year,y = HotPixels) +
  geom_point() + geom_smooth(se = F, color = "red") +  xlab("") + ylab("Hotpixels(Counts)") +
  scale_x_continuous(breaks = c(seq(2000,2019,1))) +
  theme_gray(base_size = 18) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        legend.position = "none") +
  facet_wrap(~Municipality)

# Request burn area Burn Area 

for (i in 2001:2019) {
  print(i)
  
  ba_data = ee_grab(data = ee_data_collection(datasetID = 'MODIS/006/MCD64A1',
                                              spatialReducer = "max",
                                              temporalReducer = "sum",
                                              resolution = 1000,
                                              timeStart = paste0(i,"-01-01"),
                                              timeEnd = paste0(i,"-12-01"),
                                              bandSelection = 'BurnDate'
  ),
  targetAreaAssetPath = "users/lrmodelagem/MAPBIOMAS/xingu_municipios_spera", # Use your username instead
  download_path = getwd()
  )
}

ba_processing = function(caminho) {
  
  b = list.files(caminho, pattern = "MODIS-006-MCD64A1_s") # List dowloaded files
  
  ba_list = lapply(b, st_read) # Read all files
 
  ba_list_sub = lapply(ba_list, "[",c(15,12)) # Subset list data frames
  
  ID_ba = c(seq(2001,2019,1)) # Years
  
  ba_list_sub_year = mapply(cbind, ba_list_sub, "Year"=ID_ba, SIMPLIFY=F) # Add year into list
  
  new_col_name_ba <- c("Municipality","BA", "Year","geometry")
  
  ba_list_sub_year_named = lapply(ba_list_sub_year, setNames, nm = new_col_name_ba) # Change col names
  
  ba_list_sub_year_named_complete = do.call(rbind, ba_list_sub_year_named) # Full df
  
  return(df_ba = ba_list_sub_year_named_complete) 
  
}

# Visualize
ba_df = ba_processing("/Users/usuario/")

ggplot(ba_df) + aes(x = Year,y = BA) +
  geom_point() + geom_smooth(se = F, color = "red") +  xlab("") + ylab("BA (Km2)") +
  scale_x_continuous(breaks = c(seq(2000,2019,1))) +
  theme_gray(base_size = 18) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        legend.position = "none") +
  facet_wrap(~Municipality)

# Focos - BA 
focos_ba = cbind(fire[,1:3],ba_df[,2])

# Visualize it
ggplot(focos_ba) + aes(x = HotPixels,y = BA) +
  #geom_text(aes(label=Year),hjust=0, vjust=0, size = 2) +
  geom_point() + 
  #geom_abline(slope = 1) +
  geom_smooth(se = F, color = "red") + 
  xlab("Hotpixels (Counts)") + ylab("Burned Area (km2)") + 
  theme_gray(base_size = 18) +
  facet_wrap(~Municipality, scales = "free")

# Check Hansen forest cover data
ee_Initialize()

# Load an image.
image <- ee$Image("LANDSAT/LC08/C01/T1/LC08_044034_20140318")
band345 <- image$select("B[3-5]")
bandNames <- band345$bandNames()
print(bandNames$getInfo())
print(image$getInfo())


colection_hansen_tree_cover = ee$Image('UMD/hansen/global_forest_change_2018_v1_6')

head(colection_hansen_tree_cover$getInfo())

for (i in 2001:2019) {
  print(i)
  
  ba_data = ee_grab(data = ee_data_collection(datasetID = 'UMD/hansen/global_forest_change_2018_v1_6',
                                              spatialReducer = "median",
                                              temporalReducer = "sum",
                                              resolution = 1000,
                                              timeStart = paste0(i,"-01-01"),
                                              timeEnd = paste0(i,"-12-01")#,
                                              #bandSelection = 'BurnDate'
  ),
  targetAreaAssetPath = "users/lrmodelagem/MAPBIOMAS/xingu_municipios_spera", # Use your username instead
  download_path = getwd()
  )
}


