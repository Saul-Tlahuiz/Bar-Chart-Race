#Bar chart race con la base de campos

library(readxl)
library(ggplot2)
library(gganimate)
library(tweenr)
library(magick)
library(gifski)
library(dplyr)
library(magrittr)
library(grid)
library(tidyr)
library(readxl)
library(extrafont)
library(scales)
library(lubridate)


#Comenzamos cargando la base de datos que vamos a estar trabajando 
PROD_CAMPOS <- read_excel("C:/Users/A19537/Downloads/PROD_CAMPOS.xlsx", 
                          col_types = c("date", "text", "text", 
                                        "text", "numeric", "numeric", "numeric", 
                                        "numeric", "numeric", "numeric", 
                                        "numeric"), skip = 11)
View(PROD_CAMPOS)

#Le agregamos nuevas columnas que nos seran utiles despues 
prod_campos <- PROD_CAMPOS %>% 
  mutate(Año = format(FECHA, "%Y")) %>% 
  mutate(decada = floor_date(FECHA, years(10))) %>% 
  mutate(decada = format(decada, "%Y"))

#para la agrupacion comenzaremos seccionando la informacion por decada comenzando con la de los sesenta, para esto lo que se hara sera filtrar 
#la base de datos original del año 60 a 69, luego se sacara el promedio por decada de cada campo por tipo de hidrocarburo, por lo tanto se 
#seguira un proceso similar a la codificacion de la grafica con produccion promedio anual


#Codigo para obtener los promedios de produccion por decada de cada campo
bd_60 <- prod_campos %>% 
  group_by(CAMPO_OFICIAL,decada) %>% 
  filter(decada == 1960) %>% 
  summarise_at(vars(PETROLEO_MBD,CONDENSADO_MBD,GAS_ASOC_MMPCD), mean, na.rm = T)
################################################################################

bd_70 <- prod_campos %>% 
  group_by(decada,CAMPO_OFICIAL) %>% 
  filter(decada == 1970) %>% 
  summarise_at(vars(PETROLEO_MBD,CONDENSADO_MBD,GAS_ASOC_MMPCD), mean, na.rm = T)
################################################################################

bd_80 <- prod_campos %>% 
  group_by(decada,CAMPO_OFICIAL) %>%
  filter(decada == 1980) %>% 
  summarise_at(vars(PETROLEO_MBD,CONDENSADO_MBD,GAS_ASOC_MMPCD), mean , na.rm = T)
################################################################################

bd_90 <- prod_campos %>% 
  group_by(decada,CAMPO_OFICIAL) %>% 
  filter(decada == 1990) %>% 
  summarise_at(vars(PETROLEO_MBD,CONDENSADO_MBD,GAS_ASOC_MMPCD),mean, na.rm = T)
################################################################################

bd_2000 <- prod_campos %>% 
  group_by(decada,CAMPO_OFICIAL) %>% 
  filter(decada == 2000) %>% 
  summarise_at(vars(PETROLEO_MBD,CONDENSADO_MBD,GAS_ASOC_MMPCD),mean, na.rm = T)
################################################################################

bd_2010 <- prod_campos %>% 
  group_by(decada,CAMPO_OFICIAL) %>% 
  filter(decada == 2010) %>% 
  summarise_at(vars(PETROLEO_MBD,CONDENSADO_MBD,GAS_ASOC_MMPCD),mean, na.rm = T)
################################################################################

bd_dosmil20 <- prod_campos %>% 
  group_by(decada,CAMPO_OFICIAL) %>% 
  filter(decada == 2020) %>% 
  summarise_at(vars(PETROLEO_MBD,CONDENSADO_MBD,GAS_ASOC_MMPCD),mean, na.rm = T)

################################################################################
#ya tenemos listas las bases con su respectivo promedio por decada de cada asignacion, lo que sigue es juntar todas las bases generadas en 
#una sola 

union1 <- merge(bd_60,bd_70,all = T)
union2 <- merge(union1,bd_80,all = T)
union3 <- merge(union2,bd_90,all = T)
union4 <- merge(union3,bd_2000,all = T)
union5 <- merge(union4,bd_2010,all = T)
union_fin <- merge(union5,bd_dosmil20,all = T)


bd_decade <- union_fin %>% 
  mutate(rank1 = min_rank(desc(PETROLEO_MBD))) %>% 
  mutate(rank2 = min_rank(desc(CONDENSADO_MBD))) %>% 
  mutate(rank3 = min_rank(desc(GAS_ASOC_MMPCD)))

#En este punto ya tenemos la base general con los promedios que vamos a estar usando 

################################################################################
#Lo que sigue es empezar a codificar cada gráfica por tipo de hidrocarburo empezando por el petroleo

db_pet_dec <- bd_decade %>%
  group_by(decada)%>%
  filter(PETROLEO_MBD != 0)%>%#filtramos la información para que solo se vean los valores distintos de 0 en la gráfica
  top_n(n = 7, wt = PETROLEO_MBD)%>%
  mutate(rank1 = min_rank(rank1) * 120)%>%
  ungroup()

db_pet_dec <- db_pet_dec[with(db_pet_dec, order(decada, -PETROLEO_MBD)),]


plot_dec_pet <- ggplot(db_pet_dec,aes(x = rank1, group = CAMPO_OFICIAL)) + 
  geom_col(aes(x = rank1, y = PETROLEO_MBD), alpha = 0.9 , width = 100, fill = "#252E56", color = "black") + # Columnas
  coord_flip(clip = "off", expand = FALSE) + # Flip
  labs(title = paste("Top produccion de Petroleo por Campo 
  {closest_state}", sep=""),subtitle = 'cifras en MBD',  #por verificar las cifras
       caption  = "Fuente: Fondo Mexicano del Petróleo") +# Labels o ejes
  theme_minimal() + # Tema de la grafica
  geom_text(aes(x = rank1, y = 0, label = CAMPO_OFICIAL), hjust = 1) + # Names
  geom_text(aes(x = rank1, y = PETROLEO_MBD + 0, label = as.character(round(PETROLEO_MBD,2))), hjust = 0, color = "black") +#Valores observados
  scale_y_continuous(labels = scales::comma) + # Formato de los valores para el eje y
  scale_x_reverse() + # Ponemos el valor mas alto en el top de la grafica
  transition_states(decada, transition_length = 1, state_length = 1) + # Animamos la grafica pasando como paranmetro de movimiento la columna Fecha
  theme(
    legend.position="none",
    plot.margin = margin(0,2,0,3,"cm"),
    plot.title=element_text(size=22, hjust=0.5, face="plain", colour="grey20", vjust=-1, family = "Verdana"),#tamaño de la fuente de titulo
    plot.subtitle=element_text(size = 20, hjust=0.5, family = "Corbel" , color="grey20"),#tamaño y fuente del subtitulo
    plot.caption =element_text(size = 10, hjust = 0, face = "plain", color="grey20", family = "Trebuchet MS"),#tamaño y fuente de la nota inferior
    axis.text.y  = element_blank(),
    axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    axis.title.y = element_blank()
  )

plot_dec_pet

animate(plot_dec_pet, fps = 60, duration = 80, width = 1250, height = 800)

################################################################################
#Condensado

db_cond_dec <- bd_decade %>%
  group_by(decada)%>%
  filter(CONDENSADO_MBD != 0)%>%#filtramos la informacion para que solo se vean los valores distintos de 0 en la gráfica
  top_n(n = 7, wt = CONDENSADO_MBD)%>%
  mutate(rank2 = min_rank(rank2) * 120)%>%
  ungroup()

db_cond_dec <- db_cond_dec[with(db_cond_dec, order(decada, -CONDENSADO_MBD)),]


plot_dec_cond <- ggplot(db_cond_dec,aes(x = rank2, group = CAMPO_OFICIAL)) + 
  geom_col(aes(x = rank2, y = CONDENSADO_MBD), alpha = 0.9 , width = 100, fill = "#252E56", color = "black") + # Columnas
  coord_flip(clip = "off", expand = FALSE) + # Flip
  labs(title = paste("Top produccion de Condensado por Campo 
  {closest_state}", sep=""),subtitle = 'cifras en MBD',  #por verificar las cifras
       caption  = "Fuente: Fondo Mexicano del Petróleo") +# Labels o ejes
  theme_minimal() + # Tema de la grafica
  geom_text(aes(x = rank2, y = 0, label = CAMPO_OFICIAL), hjust = 1) + # Names
  geom_text(aes(x = rank2, y = CONDENSADO_MBD + 0, label = as.character(round(CONDENSADO_MBD,2))), hjust = 0, color = "black") +#Valores observados
  scale_y_continuous(labels = scales::comma) + # Formato de los valores para el eje y
  scale_x_reverse() + # Ponemos el valor mas alto en el top de la grafica
  transition_states(decada, transition_length = 1, state_length = 1) + # Animamos la grafica pasando como paranmetro de movimiento la columna Fecha
  theme(
    legend.position="none",
    plot.margin = margin(0,2,0,3,"cm"),
    plot.title=element_text(size=22, hjust=0.5, face="plain", colour="grey20", vjust=-1, family = "Verdana"),#tamaño de la fuente de titulo
    plot.subtitle=element_text(size = 20, hjust=0.5, family = "Corbel" , color="grey20"),#tamaño y fuente del subtitulo
    plot.caption =element_text(size = 10, hjust = 0, face = "plain", color="grey20", family = "Trebuchet MS"),#tamaño y fuente de la nota inferior
    axis.text.y  = element_blank(),
    axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    axis.title.y = element_blank()
  )

plot_dec_cond

animate(plot_dec_cond, fps = 60, duration = 80, width = 1250, height = 800)

################################################################################
#Gas asociado

db_gas_dec <- bd_decade %>%
  group_by(decada)%>%
  filter(GAS_ASOC_MMPCD != 0)%>%#filtramos la informacion para que solo se vean los valores distintos de 0 en la gráfica
  top_n(n = 7, wt = GAS_ASOC_MMPCD)%>%
  mutate(rank3 = min_rank(rank3) * 120)%>%
  ungroup()

db_gas_dec <- db_gas_dec[with(db_gas_dec, order(decada, -GAS_ASOC_MMPCD)),]


plot_dec_gas <- ggplot(db_gas_dec,aes(x = rank3, group = GAS_ASOC_MMPCD)) + 
  geom_col(aes(x = rank3, y = GAS_ASOC_MMPCD), alpha = 0.9 , width = 100, fill = "#252E56", color = "black") + # Columnas
  coord_flip(clip = "off", expand = T) + # Flip
  labs(title = paste("Top produccion de Gas por Campo 
  {closest_state}", sep=""),subtitle = 'cifras en MMPCD',  #por verificar las cifras
       caption  = "Fuente: Fondo Mexicano del Petróleo") +# Labels o ejes
  theme_minimal(base_size = 5) + # Tema de la grafica
  geom_text(aes(x = rank3, y = 0, label = CAMPO_OFICIAL), hjust = 1,nudge_x = 0.6, size = 5) + # Names
  geom_text(aes(x = rank3, y = GAS_ASOC_MMPCD + 0, label = as.character(round(GAS_ASOC_MMPCD,2))), hjust = 0, color = "black", size = 5) +#Valores observados
  scale_y_continuous(labels = scales::comma) + # Formato de los valores para el eje y
  scale_x_reverse() + # Ponemos el valor mas alto en el top de la grafica
  transition_states(decada, transition_length = 1, state_length = 1) +# Animamos la grafica pasando como paranmetro de movimiento la columna Fecha
  theme(
    legend.position="none",
    plot.margin = margin(0,1,0,2,"cm"),
    plot.title=element_text(size=22, hjust=0.5, face="plain", colour="grey20", vjust=-1, family = "Verdana"),#tamaño de la fuente de titulo
    plot.subtitle=element_text(size = 20, hjust=0.5, family = "Corbel" , color="grey20"),#tamaño y fuente del subtitulo
    plot.caption =element_text(size = 15, hjust = 0, face = "plain", color="grey20", family = "Trebuchet MS"),#tamaño y fuente de la nota inferior
    axis.text.y  = element_blank(),
    axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    axis.title.y = element_blank()
  )

plot_dec_gas

animate(plot_dec_gas, fps = 60, duration = 80, width = 1250, height = 800)


#Graficas finales de la produccion por decada de 3 tipos de hidrocarburos.











