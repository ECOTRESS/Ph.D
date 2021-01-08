# Libs
library(tidyverse)
library(plotly)
library(sf)
library(raster)
library(kableExtra)
library(knitr)

# Working directory
wd =  "/Users/usuario/Google Drive (santoslr@uci.edu)/UCI/Chapter_1/data/shapefiles/"

##########################################################################
# Focos

#focos_2018 = st_read(file.path(wd,"Focos_BDQueimadas-2/Focos.2018-01-01.2018-12-31.shp"))
#focos_2019 = st_read(file.path(wd,"Focos_BDQueimadas-2/Focos.2019-01-01.2019-12-31.shp"))
focos_2020 = st_read(file.path(wd,"Focos_BDQueimadas-4/Focos_2020-01-01_2020-12-31.shp"))

# Adjust for merging
#focos_2018 = focos_2018 %>% dplyr::select(-AreaIndu)
#focos_2019 = focos_2019 %>% dplyr::select(-regiaoespe)
focos_2020 = focos_2020 %>% dplyr::select(-regiaoespe)

#colnames(focos_2018) = names(focos_2019) # colnames 2018 equals 2019

#focos = rbind(focos_2018,focos_2019,focos_2020)
focos = focos_2020 # Analise apenas para 2020
# Focos 2020
focos_sub = focos %>%
  separate(col = datahora, c("data","hora"), sep = " ") %>%
  filter(data >= "2020/01/01" & data <= "2020/12/31"
  )

# Focos 2019
#focos_sub = focos %>%
#  separate(col = datahora, c("data","hora"), sep = " ") %>%
#  filter(data >= "2019/01/01" & data <= "2019/12/31")


##########################################################################
# Desmatamento
deter = st_read(file.path(wd,"deter-amz-public-2021jan08/deter_public.shp"))

# Desmatamento Only 2019
# deter_sub = deter %>%
#   filter(VIEW_DATE >= paste0("2019/01/01") &
#            VIEW_DATE <= paste0("2019/12/31")) %>%
#   filter(CLASSNAME  == "DESMATAMENTO_CR" |
#            CLASSNAME  == "DESMATAMENTO_VEG")

# Desmatamento Jan 2020 - Dez 2020
deter_sub = deter %>%
  filter(VIEW_DATE >= paste0("2020/01/01") &
          VIEW_DATE <= paste0("2020/12/31")
  ) %>%
  filter(CLASSNAME  == "DESMATAMENTO_CR" |
           CLASSNAME  == "DESMATAMENTO_VEG")

#st_write(deter_sub, paste0(wd, "/", "deter_ago_2018_ago_2020.shp"))
##########################################################################
# Categoria fundiaria

#cat_fund = raster("/Users/usuario/Google Drive (santoslr@uci.edu)/UCI/Chapter_1/data/rasters/Fundiario2019_NT.tif")

#yytmp = setExtent(cat_fund, extent(deter))

#cat_fund_crop = crop(yytmp,extent(deter))

#cat_fund = st_read("/Users/usuario/Google Drive (santoslr@uci.edu)/UCI/Chapter_1/data/shapefiles/Shapes/base_fundiaria_dissolve_Ntecnica.shp")
##########################################################################  
# Run the analysis
f_res = st_transform(focos_sub, crs = st_crs(deter_sub))

st_crs(deter_sub) == st_crs(f_res)  

# Fires within deforestation
def_fires = st_join(deter_sub,f_res, join = st_intersects)

# Difference when fires happens to def
def_fires$date_diff = as.Date(as.character(def_fires$data), format="%Y/%m/%d") -
  as.Date(def_fires$VIEW_DATE, format="%Y/%m/%d")

#st_write(def_fires, paste0(wd, "/", "deter_com_foco.shp"))

##########################################################################
# Include categoria fundiaria nos dados
# ttmp = system.time(data.frame(
#     #def_fires,
#     raster::extract(cat_fund_crop,deter_sub)
#   ))

# Data da diferenca entre o fogo e o desmatamento (conta o poligono de desm)
# no plot

col_class = function(x) {
  transform(as.numeric(x))
}

def_fires_complete = def_fires %>%
  as_tibble() %>%
  mutate_at("date_diff",col_class) %>%
  mutate(tempo = case_when(date_diff < 0 & date_diff >= -30  ~ 0,
                           date_diff < -30 & date_diff >= -60  ~ -1,
                           date_diff < -60 & date_diff >= -90  ~ -2,
                           date_diff < -90 & date_diff >= -120  ~ -3,
                           date_diff < -120 & date_diff >= -150  ~ -4,
                           date_diff < -150 & date_diff >= -180  ~ -5,
                           date_diff < -180 & date_diff >= -210  ~ -6,
                           date_diff < -210 & date_diff >= -240  ~ -7,
                           date_diff < -240 & date_diff >= -270  ~ -8,
                           date_diff < -270 & date_diff >= -300  ~ -9,
                           date_diff < -300 & date_diff >= -330  ~ -10,
                           date_diff < -330 & date_diff >= -360  ~ -11,
                           date_diff < -360 & date_diff >= -390  ~ -12,
                           date_diff < -390 & date_diff >= -410  ~ -13,
                           date_diff < -410 & date_diff >= -440  ~ -14,
                           date_diff < -440 & date_diff >= -470  ~ -15,
                           date_diff < -470 & date_diff >= -500  ~ -16,
                           date_diff < -500 & date_diff >= -530  ~ -17,
                           date_diff < -530 & date_diff >= -560  ~ -18,
                           date_diff < -560 & date_diff >= -590  ~ -19,
                           date_diff < -590 & date_diff >= -610  ~ -20,
                           date_diff < -610 & date_diff >= -630  ~ -21,
                           date_diff < -630 & date_diff >= -660  ~ -22,
                           date_diff < -660 & date_diff >= -690  ~ -23,
                           date_diff < -690 & date_diff >= -720  ~ -24,
                           date_diff < -720 & date_diff >= -750  ~ -25,
                           date_diff < -750 & date_diff >= -780  ~ -26,
                           date_diff > 0 & date_diff <= 30  ~ 0,
                           date_diff > 30 & date_diff <= 60  ~ 1,
                           date_diff > 60 & date_diff <= 90  ~ 2,
                           date_diff > 90 & date_diff <= 120  ~ 3,
                           date_diff > 120 & date_diff <= 150  ~ 4,
                           date_diff > 150 & date_diff <= 180  ~ 5,
                           date_diff > 180 & date_diff <= 210  ~ 6,
                           date_diff > 210 & date_diff <= 240  ~ 7,
                           date_diff > 240 & date_diff <= 270  ~ 8,
                           date_diff > 270 & date_diff <= 300  ~ 9,
                           date_diff > 300 & date_diff <= 330  ~ 10,
                           date_diff > 330 & date_diff <= 360  ~ 11,
                           date_diff > 360 & date_diff <= 390  ~ 12,
                           date_diff > 390 & date_diff <= 410  ~ 13,
                           date_diff > 410 & date_diff <= 440  ~ 14,
                           date_diff > 440 & date_diff <= 470  ~ 15,
                           date_diff > 470 & date_diff <= 500  ~ 16,
                           date_diff > 500 & date_diff <= 530  ~ 17,
                           date_diff > 530 & date_diff <= 560  ~ 18,
                           date_diff > 560 & date_diff <= 590  ~ 19,
                           date_diff > 590 & date_diff <= 610  ~ 20,
                           date_diff > 610 & date_diff <= 630  ~ 21,
                           date_diff > 630 & date_diff <= 660  ~ 22,
                           date_diff > 660 & date_diff <= 690  ~ 23,
                           date_diff > 690 & date_diff <= 720  ~ 24,
                           date_diff > 720 & date_diff <= 750  ~ 25,
                           date_diff > 750 & date_diff <= 780  ~ 26))
#####################################################################
#####################################################################
### Send table to Ane ###############################################

# Time
# send_table_time_2019 = def_fires_complete %>%
#   group_by(tempo, UF) %>%
#   summarise(n = n()) %>%
#   drop_na() 
# 
# WriteXLS(send_table_time_2019, "/Users/usuario/Google Drive (santoslr@uci.edu)/UCI/Chapter_1/results/tables/when_2019.xlsx",
#          AdjWidth = TRUE, BoldHeaderRow = TRUE)
# 
# 
# # Propo
# send_table_prop_2019 = def_fires_complete %>%
#   mutate(antes_depois = ifelse(tempo < 0,"antes","depois")) %>%
#   #mutate(antes_depois_complete = ifelse(is.na(antes_depois),"nunca",antes_depois)) %>%
#   group_by(UF,tempo,antes_depois) %>%
#   summarise(tempo_desmate_fogo = n()) %>%
#   group_by(UF,antes_depois) %>%
#   summarise(sum(tempo_desmate_fogo)) %>%
#   drop_na() %>%
#   group_by(UF) %>%
#   mutate(percent = `sum(tempo_desmate_fogo)`/sum(`sum(tempo_desmate_fogo)`)) 
# 
# WriteXLS(send_table_prop_2019, "/Users/usuario/Google Drive (santoslr@uci.edu)/UCI/Chapter_1/results/tables/proportion_2019.xlsx",
#          AdjWidth = TRUE, BoldHeaderRow = TRUE)




######################################################################
######################################################################

# Time
send_table_time_2020 = def_fires_complete %>%
  group_by(tempo, UF) %>%
  summarise(n = n()) %>%
  drop_na() 

WriteXLS::WriteXLS(send_table_time_2020, "/Users/usuario/Google Drive (santoslr@uci.edu)/UCI/Chapter_1/results/tables/when_2020.xlsx",
         AdjWidth = TRUE, BoldHeaderRow = TRUE)


# Propo
send_table_prop_2020 = def_fires_complete %>%
  mutate(antes_depois = ifelse(tempo < 0,"antes","depois")) %>%
  #mutate(antes_depois_complete = ifelse(is.na(antes_depois),"nunca",antes_depois)) %>%
  group_by(UF,tempo,antes_depois) %>%
  summarise(tempo_desmate_fogo = n()) %>%
  group_by(UF,antes_depois) %>%
  summarise(sum(tempo_desmate_fogo)) %>%
  drop_na() %>%
  group_by(UF) %>%
  mutate(percent = `sum(tempo_desmate_fogo)`/sum(`sum(tempo_desmate_fogo)`)) 

WriteXLS::WriteXLS(send_table_prop_2020, "/Users/usuario/Google Drive (santoslr@uci.edu)/UCI/Chapter_1/results/tables/proportion_2020.xlsx",
         AdjWidth = TRUE, BoldHeaderRow = TRUE)

#########
# Distribuicao geral Amazonia
p1 = def_fires_complete %>%
  group_by(tempo, UF) %>%
  summarise(n = n()) %>%
  drop_na() %>%
  #group_by(antes_depois) %>%
  ggplot() + aes(x = tempo, y = n, fill = UF) +
  geom_col() + ylab("Número de polígonos de desmatamento\n com duplicata") + 
  xlab("tempo em meses") +
  ggtitle("Janeiro 2020 - Dezembro 2020") +
  #ggtitle("Jan 2019 - Set 2020") +
  #scale_fill_manual(values = c("orange", "darkred")) +
  theme_minimal(base_size = 14) +
  theme( legend.title = element_blank(),
         legend.position = c(0.8,0.8)#,
         #     axis.title.x = element_blank()
  )

ggsave("/Users/usuario/Google Drive (santoslr@uci.edu)/UCI/Chapter_1/results/figures/Quando_queima/quando_queima_Jan2020-Ago2020.png",
       p1 ,device = "png")

# Por estado
p2 = def_fires_complete %>%
  group_by(tempo, UF) %>%
  summarise(n = n()) %>%
  drop_na() %>%
  group_by(UF) %>%
  mutate(percent = n/sum(n)) %>%
  #group_by(antes_depois) %>%
  ggplot() + aes(x = tempo, y = n, fill = UF) +
  geom_col() + ylab("Número de polígonos de desmatamento\n com duplicata") + 
  xlab("tempo em meses") +
  ggtitle("Jan 2020 - Dec 2020") +
  #ggtitle("Jan 2019 - Set 2020") +
  geom_text(
    aes(label = paste0(round(percent * 100,1),"%")),
    position = position_stack(vjust = 0.5),
    colour = "black", fontface = "bold",
    size = 3
  ) +
  #scale_fill_manual(values = c("orange", "darkred")) +
  theme_minimal(base_size = 14) +
  theme( legend.title = element_blank(),
         legend.position = "none"#,
         #     axis.title.x = element_blank()
  ) +
  facet_wrap(~UF, scales = "free")

ggsave("/Users/usuario/Google Drive (santoslr@uci.edu)/UCI/Chapter_1/results/figures/Quando_queima/quando_queima_estado_Oct_2020.png",
       p2 ,device = "png")

# # Por mes
# quando_mes = def_fires_complete %>%
#   group_by(VIEW_DATE,tempo, UF) %>%
#   summarise(n = n()) %>%
#   drop_na() %>%
#   group_by(UF) %>%
#   mutate(percent = n/sum(n)) %>%
#   separate(VIEW_DATE, c("ano","mes","dia")) %>%
#   mutate(mes_name = case_when(mes == "01" ~ "Jan",
#                               mes == "02" ~ "Fev",
#                               mes == "03" ~ "Mar",
#                               mes == "04" ~ "Abr",
#                               mes == "05" ~ "Mai",
#                               mes == "06" ~ "Jun",
#                               mes == "07" ~ "Jul",
#                               mes == "08" ~ "Ago",
#                               mes == "09" ~ "Set",
#                               mes == "10" ~ "Out"))
# 
# p3 = ggplot(transform(quando_mes, 
#                       mes_f = factor(mes_name, levels = c("Jan",
#                                                           "Fev",
#                                                           "Mar",
#                                                           "Abr",
#                                                           "Mai",
#                                                           "Jun",
#                                                           "Jul",
#                                                           "Ago",
#                                                           "Set",
#                                                           "Out")))) +
#   aes(x = as.factor(tempo), y = n, fill = UF) +
#   geom_col() + ylab("Quantidade de polígonos de desmatamento\n que queimaram") + 
#   xlab("tempo em meses") +
#   ggtitle("Jan 2020 - Oct 2020") +
#   # geom_text(
#   #   aes(label = paste0(round(percent * 100,1),"%")),
#   #   position = position_stack(vjust = 0.5),
#   #   colour = "black", fontface = "bold",
#   #   size = 3
#   # ) +
#   #scale_fill_manual(values = c("orange", "darkred")) +
#   theme_minimal(base_size = 14) +
#   theme( legend.title = element_blank()#,
#          #legend.position = "none"#,
#          #     axis.title.x = element_blank()
#   ) +
#   facet_wrap(~UF, scales = "free")
# 
# ggsave("/Users/usuario/Google Drive (santoslr@uci.edu)/UCI/Chapter_1/results/figures/Quando_queima/quando_queima_estado_Oct_2020.png",
#        p3 ,device = "png")




# Qual a mediana em meses do que vem antes e do que vem depois Amazonia
med_amz = def_fires_complete %>%
  dplyr::select(VIEW_DATE,tempo) %>%
  mutate(antes_depois = ifelse(tempo < 0, "antes","depois")) %>%
  group_by(antes_depois) %>%
  mutate(mediana_dias = median(tempo)) %>%
  dplyr::select(antes_depois,mediana_dias) %>%
  distinct() %>% 
  drop_na() %>%
  rename(intervalo = antes_depois, Meses = mediana_dias)

save_kable(kbl(med_amz, booktabs = T, caption = "Quando desmatamento\n queima media geral Amazonia 2020") %>%
             kable_styling(latex_options = c("striped", "hold_position"),
                           full_width = F), 
           "/Users/usuario/Google Drive (santoslr@uci.edu)/UCI/Chapter_1/results/figures/Quando_queima/table_mediana_geral_2020.png")


# Qual a mediana em meses do que vem antes e do que vem depois Amazonia
med_meses =  def_fires_complete %>%
  dplyr::select(VIEW_DATE,UF,tempo) %>%
  mutate(antes_depois = ifelse(tempo < 0, "antes","depois")) %>%
  group_by(UF,antes_depois) %>%
  mutate(mediana_dias = median(tempo)) %>%
  dplyr::select(antes_depois,mediana_dias) %>%
  distinct() %>%
  drop_na() %>%
  rename(intervalo = antes_depois, Meses = mediana_dias)

ff = med_meses[order(med_meses$intervalo),]

save_kable(kbl(ff, booktabs = T, caption = "Quando desmatamento queima 2020") %>%
             kable_styling(latex_options = c("striped", "hold_position"),
                           full_width = F),
           "/Users/usuario/Google Drive (santoslr@uci.edu)/UCI/Chapter_1/results/figures/Quando_queima/table_mediana_estado_2020.png")


#####################################################################
#####################################################################
# Percentual de poligonos queimam antes e depois do fogo

p3 = def_fires_complete %>%
  mutate(antes_depois = ifelse(tempo < 0,"antes","depois")) %>%
  #mutate(antes_depois_complete = ifelse(is.na(antes_depois),"nunca",antes_depois)) %>%
  group_by(UF,tempo,antes_depois) %>%
  summarise(tempo_desmate_fogo = n()) %>%
  group_by(UF,antes_depois) %>%
  summarise(sum(tempo_desmate_fogo)) %>%
  drop_na() %>%
  group_by(UF) %>%
  mutate(percent = `sum(tempo_desmate_fogo)`/sum(`sum(tempo_desmate_fogo)`)
  ) %>%
  ggplot() + aes(x = antes_depois, y = `sum(tempo_desmate_fogo)`, fill = antes_depois) +
  geom_col() + ylab("Número de poligonos de desmatamento") +
  geom_text(
    aes(label = paste0(round(percent * 100,1),"%")),
    position = position_stack(vjust = 0.5),
    colour = "black", fontface = "bold",
    size = 5
  ) +
  ggtitle("Jan 2020 - Dec 2020") +
  #ggtitle("Jan 2019 - Set 2020") +
  #scale_fill_manual(values = c("orange", "darkred","gray")) +
  scale_fill_manual(values = c("orange", "darkred")) +
  theme_minimal(base_size = 14) +
  theme(legend.position = "none",
        axis.title.x = element_blank()) +
  facet_wrap(~UF, scales = "free")


ggsave("/Users/usuario/Google Drive (santoslr@uci.edu)/UCI/Chapter_1/results/figures/Quando_queima/prop_quando_queima_2020.png",
       p3 ,device = "png")

#gridExtra::grid.arrange(p1,p2)


# Save to see it on map
#st_write(def_fires_complete, paste0(wd, "/", "deter_com_e_sem_fogo_2019.shp"))

##########################################################################
##########################################################################
##########################################################################
# Quantos desmatamentos por estado:
# deter_sub %>%
#   group_by(UF) %>%
#   summarise(n = n()) %>% 
#   drop_na() %>%
#   ggplot() + aes( x = UF, y = n, fill = "red") + geom_col() +
#   geom_text(
#     aes(label = n),
#     position = position_stack(vjust = 0.5),
#     colour = "black", fontface = "bold",
#     size = 5
#   ) + theme_minimal() + theme(legend.position = "none") 
# 
# 
# # Number of 2019 def polygons with and without fires  
# 
# p1 = def_fires %>%
#   as_tibble() %>%
#   filter(VIEW_DATE < "2020-01-01") %>%
#   mutate(antes_depois = ifelse(is.na(date_diff),
#                                "Desmatamento_Nenhum_Fogo","Desmatamento_com_Fogo")) %>%
#   group_by(antes_depois,UF) %>%
#   summarise(n = n()) %>% 
#   group_by(UF) %>%
#   drop_na() %>%
#   mutate(percent = n/sum(n)) %>% 
#   ggplot() + aes( x = UF, y = n, fill = antes_depois) + geom_col() +
#   geom_text(
#     aes(label = paste0(round(percent * 100,1),"%")),
#     position = position_stack(vjust = 0.5),
#     colour = "black", fontface = "bold",
#     size = 5
#   ) +
#   ylab("Número de polígonos DETER") +
#   ggtitle("Desmatamento com e sem ocorrência de fogo: 2019") +
#   scale_fill_manual(values = c("darkred", "orange")) +
#   theme_minimal(base_size = 14) +
#   theme(legend.title = element_blank())

##########################################################################
##########################################################################
##########################################################################

# Quantos % do desmatamento em 2019 ficou para queimar em 2020  

# Remove duplicates
df_no_dup = def_fires_complete[!duplicated(def_fires_complete[ , 1:11]),]

df_no_dup %>%
  mutate(acontece = ifelse(is.na(tempo),"nao","sim")) %>%
  group_by(acontece) %>%
  summarise(n = n()) %>%
  mutate(percent = n/sum(n))
