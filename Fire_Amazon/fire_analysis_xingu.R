## Focos por municipio Xingu ##
## LRSS 05.17.2020 ############

install.packages("stringdist")
install.packages("lme4")
install.packages("caret")

library(raster)
library(rgeos)
library(rgdal)
library(ggplot2)
library(tidyverse)
library(stringdist)
library(lme4)
library(gridExtra)
library(caret)
library(corrplot)
library(e1071)
library(lattice)

# Set theme and directory
#-------------------------
theme_set(
  theme_classic() + 
    theme(legend.position = "right", legend.title = element_blank())
)


setwd("D:/Trabalhando/IRVINE/data")

# Import Xingu shpefile
#-------------------------
xingu = shapefile(paste0(getwd(),"/municipalities/xingu_with_conservation_unit.shp"))


# Import hotpixels
#-------------------------
# Only needed if new data is applied otherwise run third line
#focos = list.files(paste0(getwd(),"/fire/amz_legal"), "*.shp", full.names = T)
#f = lapply(focos, shapefile)
load("focos_xingu.RData")

# Subset hotpixels -- xingu
#-------------------------
#f_xingu = lapply(f, function(x){crop(x,extent(xingu))})

# Subset hotpixels -- querencia
#-------------------------
#S = function(x){subset(x,x@data$Municipi == "QUERENCIA")} 
#f_filter = lapply(f,S) # Subset querencia


# Saving on object in RData format
#-------------------------
#save(f_xingu, file = "focos_xingu.RData")
#load("focos_xingu.RData")

# Funcoes
#-------------------------

# Ajusta string names 
adj_and_rem_string_proble = function(df){
  library(stringr)
  
  if(sum(names(df) == "MUNICIPIO")){
    
    tmp = as.data.frame(str_replace_all(df$MUNICIPIO, "[[:punct:]]", " ")); colnames(tmp) = "V1"
    df_sub = as.data.frame(gsub("[^[:alnum:]]", "", as.matrix(tmp$V1)))
    df_sub = as.data.frame((iconv(df_sub$V1, "latin1", "ASCII", sub="")))
    
    df_final = cbind(df_sub,df) # Bind it
    
    df_f = df_final[,c(1,ncol(df_final),ncol(df_final) -1)]
    colnames(df_f)[1] = "V1"
    df_f =as.data.frame(df_f %>% 
                          group_by(V1,type,Year) %>%
                          summarise(n = n()) %>% 
                          na.omit() %>%
                          filter(n>0))
  }
  
  if(sum(names(df) == "Region.")){
    
    tmp = as.data.frame(str_replace_all(df$Region., "[[:punct:]]", " ")); colnames(tmp) = "V1"
    df_sub = as.data.frame(gsub("[^[:alnum:]]", "", as.matrix(tmp$V1)))
    df_sub = as.data.frame((iconv(df_sub$V1, "latin1", "ASCII", sub="")))
    
    df_final = cbind(df_sub,df) # Bind it
    
    df_f = df_final[,-3]
    colnames(df_f)[1] = "V1"
    
  }
  
  if(sum(names(df) == "Region")) {
    tmp = as.data.frame(str_replace_all(df$Region, "[[:punct:]]", " ")); colnames(tmp) = "V1"
    df_sub = as.data.frame(gsub("[^[:alnum:]]", "", as.matrix(tmp$V1)))
    df_sub = as.data.frame((iconv(df_sub$V1, "latin1", "ASCII", sub="")))
    
    df_final = cbind(df_sub,df) # Bind it
    
    df_f = df_final[,-2]
    colnames(df_f)[1] = "V1"
    
  }
  return(df_f)
}

# Focos por municipio
A = function(x,y) {

    crs(x) = crs(y)
  
  df = data.frame(coordinates(x),
                  x$DataHora,
                  x$Estado,
                  x$Municipi,
                  x$Bioma,
                  x$LandUseType,
                  over(x,y))
  
  df = df[,1:7]
  
  df %>%
    separate(x.DataHora, c("Ano","Mes","Dia")) %>%
    group_by(Ano,x.Municipi,x.LandUseType) %>%
    summarise(n = n())
}

# Import landuse (mapbiomas)
#-------------------------
setwd("D:/Trabalhando/IRVINE/data/land_use/mapbiomas/500m")

l_mapbiomas = list.files(getwd(),pattern = ".tif")

list_mapbio = lapply(l_mapbiomas, raster) # Ordered list yearly

# Add landuse locality of fires
#-------------------------

for (i in 1:length(list_mapbio)) {
  crs(list_mapbio[[i]]) = crs(f_xingu[[i]]) 

  f_xingu[[i]]$LandUseType = raster::extract(list_mapbio[[i]],f_xingu[[i]])
  }

# Focos por municipio
#-------------------------

f_por_mun = lapply(f_xingu,A,xingu)

focos_por_municipio1 = bind_rows(f_por_mun, .id = "column_label"); head(focos_por_municipio1)

focos_por_municipio2 = focos_por_municipio1  %>%
  rename("Fire_counts" = "n") %>%
  filter(x.LandUseType != 0 | x.LandUseType != 0) %>%
  mutate(land_type = case_when(x.LandUseType == 3 | x.LandUseType == 4 | x.LandUseType == 9 ~ "Forest",
            x.LandUseType == 12 | x.LandUseType == 13 ~ "Natural Non-Forest",
            x.LandUseType == 15 ~ "Pasture", 
            x.LandUseType == 18 | x.LandUseType == 19 ~ "Agriculture",
            x.LandUseType == 24 | x.LandUseType == 25  ~ "Non-vegeted Area")) %>%
  na.omit();head(focos_por_municipio2) 


# Visualization
#-------------------------
p1 =  ggplot(focos_por_municipio2) +
  aes(x = as.numeric(Ano), y = Fire_counts, color = land_type) + geom_point() +
  geom_line() + geom_smooth(method = "lm",
                            color = "black", se = F, 
                            linetype = 3, size = 1.2) +  
  ylab("Fire counts") + xlab("Year") +
  scale_x_continuous(breaks = 2003:2018) +
  theme(axis.text.x = element_text(angle = 90)) +
  facet_wrap(~x.Municipi, scales = "free_y") 

# Desmatamento mapbiomas
#-------------------------
# input
def = read.csv("D:/Trabalhando/IRVINE/outputs/tables/deforestation_per_year_regionalized_xingu.csv")

def_string = adj_and_rem_string_proble(def) # Adjust string def data

# Change string name (get soundex)
focos_por_municipio2$soundex = phonetic(focos_por_municipio2$x.Municipi, 
                                       method = c("soundex"), 
                        useBytes = FALSE)

def_string$soundex = phonetic(def_string$V1, method = c("soundex"), 
                        useBytes = FALSE)

def_string2 = def_string %>%
rename(Ano = Year.)

# merge deforestation on focos df
#-------------------------
focos_def = merge(focos_por_municipio2, def_string2,
                #by.x="soundex",
                by = c("soundex","Ano"),
                all=F)

# Visualize Fire ~ Deforestation
#-------------------------
p2 = focos_def  %>%
  select(Ano,Fire_counts,land_type,V1,Value) %>%
  ggplot() +
  aes(x = as.numeric(Value), y = Fire_counts, color = land_type) + 
  geom_point(shape = 1) + geom_smooth(se = F, method = "lm") +
  ylab("Fire counts") + xlab("Deforestation (ha)") + 
    facet_wrap(~V1, scales = "free")

# focos_def = cbind(data.frame(focos_por_municipio),def[4:19,]) %>%
#   rename( "Yearly Deforestation" = "Area_In_Hectares")

# VPD data
#-------------------------
vpd = read.csv("D:/Trabalhando/IRVINE/outputs/tables/vpd_per_year_regionalized_xingu.csv")

# Adjust string
vpd = adj_and_rem_string_proble(vpd)

vpd_sub = vpd %>% select(1,2,4) %>%
  rename(Ano = Year., VPD = Value)

# Change string name (get soundex)
vpd_sub$soundex = phonetic(vpd_sub$V1, 
                           method = c("soundex"), 
                           useBytes = FALSE); head(vpd_sub)

focos_def_vpd = merge(focos_def, vpd_sub,
                              by = c("soundex","Ano","V1"),
                              all=T); head(focos_def_vpd)
# Visualize Fire ~ VPD
#-------------------------
p3 = focos_def_vpd  %>%
  select(Ano,Fire_counts,land_type,V1,Value,VPD) %>%
  na.omit() %>%
  ggplot() +
  aes(x = as.numeric(VPD), y = Fire_counts, color = land_type) + 
  geom_point(shape = 1) + geom_smooth(se = F, method = "loess") +
  ylab("Fire counts") + xlab("VPD (kpa)") + 
  facet_wrap(~V1, scales = "free")

grid.arrange(p1,p2,p3, ncol = 2)

# Fire lm per municipality based on deforestation and agriculture


















# Landuse Mapbiomas
#-------------------------
land_use = read.csv("D:/Trabalhando/IRVINE/outputs/tables/cobertura_per_year_regionalized_xingu.csv")

# Municipalities area and proportion of land use 

land_use_sub = land_use %>% 
  group_by(Region., Year.) %>%
  mutate(Area_proportion = Area_In_Cells/sum(Area_In_Cells)) %>%
  select(1,2,3,5,8) %>%
  #filter(Category. == 15 | Category. == 19) %>%
  rename(Ano = Year.) %>%
  as.data.frame()

land_use_sub = adj_and_rem_string_proble(land_use_sub)

# Change string name (get soundex)
land_use_sub$soundex = phonetic(land_use_sub$V1, 
                                method = c("soundex"), 
                                useBytes = FALSE)

# Merge focos_def e landuse
focos_def_vpd_landUse = merge(focos_def_vpd, land_use_sub,
                          by = c("soundex","Ano","V1"),
                          all=T); head(focos_def_vpd_landUse)


# Regression per municipility

# Model as a function of agriculture and deforestation
# 1 - visualize as a function of agriculture percentage
 focos_def_vpd_landUse %>%
  filter(Category. == 19) %>%
  select(2,3,7,8,10,12,15) %>%
  mutate(n_fire = case_when(Fire_counts>150 ~ "Too much",
                            Fire_counts<= 150 ~ "Too little")) %>%
  na.omit() %>%
  ggplot() + aes(x = Area_proportion, 
                 y = Fire_counts,
                 color = land_type) +
  geom_point() + geom_smooth(method = "lm", se = F) +
   xlab("Agriculture Proportion") +
  facet_wrap(~V1, scales = "free")

# 2 - One lm per municipality
 library(broom)
 agr_model = focos_def_vpd_landUse %>%
   filter(Category. == 19) %>%
   select(2,3,7,8,10,12,15) %>%
   mutate(n_fire = case_when(Fire_counts>150 ~ "Too much",
                             Fire_counts<= 150 ~ "Too little")) %>%
   na.omit() %>% 
   group_by(V1) %>% do(tidy(lm(Fire_counts ~ Area_proportion, .))) %>%
   mutate(factor = ifelse(term == "(Intercept)",0,1))
 
 agr_model_sub = select(agr_model,-2)
 write.csv(agr_model_sub, "D:/Trabalhando/IRVINE/outputs/tables/tables_to_the_model/agr_model.csv")
 
 #####
 ####
agr = focos_def_vpd_landUse %>%
  as.tbl() %>%
  rename(Municipalities = V1, Type = Category.) %>%
  select(-X) %>%
  filter(Type == 19) %>%
  na.omit() %>%
  ggplot() + aes(Area_proportion, Fire_counts, 
                 color = land_type) +
  geom_point(shape = 1,alpha= 0.3) + 
  geom_smooth(se = F) + ylim(c(0,300)) + 
  xlab("Agriculture Proportion") 

past = focos_def_vpd_landUse %>%
  as.tbl() %>%
  rename(Municipalities = V1, Type = Category.) %>%
  select(-X) %>%
  filter(Type == 15) %>%
  na.omit() %>%
  ggplot() + aes(Area_proportion, Fire_counts, 
                 color = land_type) +
  geom_point(shape = 1,alpha= 0.3) + geom_smooth(se = F) + ylim(c(0,300)) + xlab("Pasture Proportion")

grid.arrange(agr,past,ncol = 2)

# MCWD CHIRPS
#-------------------------
mcwd = read.csv("D:/Trabalhando/IRVINE/outputs/tables/mcwd_per_year.csv")

mcwd_sub = mcwd %>% select(1,2,4) %>%
  rename(Ano = Year., VPD = Value)

# Change string name (get soundex)
vpd_sub$soundex = phonetic(vpd_sub$V1, 
                           method = c("soundex"), 
                           useBytes = FALSE); head(vpd_sub)

focos_def_vpd = merge(focos_def, vpd_sub,
                      by = c("soundex","Ano","V1"),
                      all=T); head(focos_def_vpd)



# Gathering some visulizations
#-------------------------

attach(focos_def_landUse_vpd)

# Fire over year
p1 = ggplot() + aes(as.numeric(Ano), Fire_counts, color = V1) + 
  geom_point(alpha = 0.5) + stat_smooth(geom='line', alpha=0.5, se=FALSE) +
  ylim(0,3000) + xlab("Year")

  
# Fire given deforestation  

p2 =  ggplot() + aes(Value, Fire_counts, color = V1) + 
  geom_point(alpha = 0.5) + stat_smooth(geom='line', alpha=0.5, se=FALSE) +
  ylim(0,3000)
  
# Fire given Agriculture proportion

p3 =  ggplot() + aes(Area_proportion, Fire_counts, color = V1) + 
  geom_point(alpha = 0.5) + stat_smooth(geom='line', alpha=0.5, se=FALSE) +
  ylim(0,3000) + xlab("Farming proportion")

# Fire given VPD

p4 =  ggplot() + aes(VPD, Fire_counts, color = V1) + 
  geom_point(alpha = 0.5) + stat_smooth(geom='line', alpha=0.5, se=FALSE) +
  ylim(0,3000) 

grid.arrange(p1,p2,p3,p4)


# Distribution response variable -- each number is a municipality
#-------------------------
focos_def_landUse_vpd %>% 
  as.tbl() %>%
  mutate(Municipality = as.numeric(as.factor(V1))) %>%
ggplot() + aes(Fire_counts) + geom_histogram() + 
  facet_wrap(~Municipality)


# Simple Model
#-------------------------
df = focos_def_landUse_vpd %>% 
  as.tbl() %>%
  mutate(Municipality = as.numeric(as.factor(V1)),
         LandType = as.numeric(Category.)) %>%
  select(2,4:5,7:11)

# Predictors -> Remove response, year and municipality
predictors = df[,-c(1,2,7)]; head(predictors)

predictors_scaled = scale(predictors)

df_1 = cbind(predictors_scaled, df[,c(1,7)], log10(df$Fire_counts))

fits = lmList(`log10(df$Fire_counts)` ~ Deforestation + Area_proportion + VPD |Municipality, data = df_1)

# xyplot(`log10(df$Fire_counts)` ~ Deforestation, 
#        groups = Municipality, data = df_1)


  lm_fit = lmer(`log10(df$Fire_counts)` ~ Deforestation + Area_proportion + VPD +
                  (1|Municipality),
              data = df_1)
  summary(lm_fit)

  lm_fit1 =  lmer(Fire_counts ~ Value + (1|V1), focos_def_final)
  
  summary(lm_fit1)
  
  predict(lm_fit1, 
          newdata = data.frame(V1 = c("AltoBoaVista","AltoBoaVista"),
            Value = c(40000, 60000)), se.fit = TRUE) 

  # Proporcao agricultura e pasto 






#querencia_area = 1865451.60

land_use = read.csv("D:/Trabalhando/IRVINE/outputs/tables/land_use_per_year.csv")

land_use_sub = land_use[,c(1,2,4)]

land_cumulative = land_use_sub %>% 
  group_by(Category.) %>%
  mutate(cum_sum = cumsum(Area_In_Hectares)) %>%
  filter(Category. == "15" | Category. == "19") %>%
  select(1,2,4) %>%
  spread(Category.,cum_sum) %>%
  rename( "Pasture_cumulative" = `15` , "Agriculture_cumulative" = `19`)

land_proportion = land_use_sub %>% 
  group_by(Category.) %>%
  mutate(Proportion = Area_In_Hectares/querencia_area) %>%
  filter(Category. == "15" | Category. == "19") %>%
  select(1,2,4) %>%
  spread(Category.,Proportion) %>%
  rename( "Pasture_prop" = `15` , "Agriculture_prop" = `19`)

focos_def_land = cbind(focos_def[,c(2,3,4,7)],
                       land_cumulative[,2:3],
                       land_proportion[,2:3])



# MCWD data

mcwd = read.csv("D:/Trabalhando/IRVINE/outputs/tables/mcwd_per_year.csv")

mcwd_sub = mcwd %>% select(3)

focos_def_land_vpd_mcwd = cbind(focos_def_land_vpd, mcwd_sub)

focos_def_land_vpd_mcwd %>% 
select(Ano,Fire_counts,`Yearly Deforestation`, Pasture_prop, Agriculture_prop,
     Pasture_cumulative, Agriculture_cumulative,
     VPD, MCWD) %>%
mutate(Deforestation_cumulative = cumsum(`Yearly Deforestation`)) %>%
reshape2::melt(id = "Ano") %>%
mutate(Unit = as.factor(rep(c("Fire_counts", "Deforestation",
                       "Pasture_proportion",
                       "Agriculture_proportion",
                       "Pasture_cumulative",
                       "Agriculture_cumulative",
                       "VPD",
                       "MCWD",
                       "Deforestation_cumulative"),
                       each = length(focos_def_land$Ano)))) %>%
  ggplot() +
  aes(x = as.numeric(Ano), y = value) + geom_point() + geom_line() +
  ggtitle("Querencia") +
  ylab("Fire and landuse metrics") + xlab("Year") +
  facet_wrap(~Unit, 
             scales = "free_y", 
             nrow = 3, 
             strip.position = "left", 
             labeller = as_labeller(c(Fire_counts = "Fire Counts", 
        Deforestation = "Deforestation (Hectares)",
        Pasture_proportion = "Pasture_proportion (%)",
        Agriculture_proportion = "Agriculture_proportion (%)",
        Pasture_cumulative = "Pasture_cumulative (Hectares)",
        Agriculture_cumulative = "Agriculture_cumulative (Hectares)",
        VPD = "VPD (kpa)",
        MCWD = "MCWD (mm)",
        Deforestation_cumulative = "Deforestation_cumulative (Hectares)"
        )))  +
  ylab(NULL) +
  theme_classic(base_size =  26) +
  scale_x_continuous(breaks = 2003:2018) +
  theme(strip.background = element_blank(),
        strip.placement = "outside",
        axis.text.x = element_text(angle = 90, hjust = 1))

