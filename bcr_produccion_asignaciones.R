#Bar chart con la base de Lalo

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

produccion_asignaciones <- read_excel("C:/Users/A19537/Downloads/PRODUCCION_ASIGNACIONES.xlsx", 
                                      col_types = c("date", "text", "text", 
                                                    "text", "text", "numeric", "numeric", 
                                                    "numeric", "numeric", "numeric", 
                                                    "numeric", "numeric", "numeric"), skip = 11)
View(produccion_asignaciones)

as.data.frame(na.omit(produccion_asignaciones))

#Agregamos la columna de Ranking
produccion_asignaciones <- produccion_asignaciones %>%
  mutate(Rank = min_rank(desc(PETROLEO_MBD))) %>%
  mutate(Rank_2 = min_rank(desc(CONDENSADO_MBD))) %>%
  mutate(Rank_3 = min_rank(desc(GAS_MMPCD)))%>%
  mutate(Rank_4 = min_rank(desc(GAS_ASOC_MMPCD)))%>%
  mutate(Rank_5 = min_rank(desc(GAS_NASOC_MMPCD)))%>%
  mutate(Rank_6 = min_rank(desc(NITROGENO_MMPCD)))
  
##################################################################
#bar chart para petroleo en mbd
bd_petroleo <- produccion_asignaciones%>%
  group_by(FECHA)%>%
  filter(PETROLEO_MBD != 0)%>%#filtramos la informacion para que solo se vean los valores distintos de 0 en la gráfica
  top_n(n = 10, wt = PETROLEO_MBD)%>%
  mutate(Rank = min_rank(Rank) * 120)%>%
  ungroup()

bd_petroleo <- bd_petroleo[with(bd_petroleo, order(FECHA, -PETROLEO_MBD)),]

plot_asignaciones_petroleo <- ggplot(bd_petroleo,aes(x = Rank, group = ASIGNACION)) + 
  geom_col(aes(x = Rank, y = PETROLEO_MBD), alpha = 0.8 , width = 100, fill = "#003249", color = "black") + # Columnas
  coord_flip(clip = "off", expand = FALSE) + # Flip
  labs(title = paste("Produccion de Petróleo 
  {closest_state}", sep=""),subtitle = 'Miles de barriles diarios',  
       caption  = "Fuente: Fondo Mexicano del Petróleo") +# Labels o ejes
  theme_minimal() + # Tema de la grafica
  geom_text(aes(x = Rank, y = 0, label = ASIGNACION), hjust = 1) + # Names
  geom_text(aes(x = Rank, y = PETROLEO_MBD + 0, label = as.character(round(PETROLEO_MBD,2))), hjust = 0, color = "black") +#Valores observados
  scale_y_continuous(labels = scales::comma) + # Formato de los valores para el eje y
  scale_x_reverse() + # Ponemos el valor mas alto en el top de la grafica
  transition_states(FECHA, transition_length = 1, state_length = 1) + # Animamos la grafica pasando como paranmetro de movimiento la columna Fecha
  view_follow()+
  theme(
    legend.position="none",
    plot.margin = margin(0,2,0,3,"cm"),
    plot.title=element_text(size=22, hjust=0.5, face="bold", colour="grey20", vjust=-1),#tamaño de la fuente de titulo
    plot.subtitle=element_text(size = 20, hjust=0.5, family = "Monotype Corsiva" , color="grey20"),#tamaño y fuente del subtitulo
    plot.caption =element_text(size = 10, hjust = 0, face="plain", color="grey20"),#tamaño y fuente de la nota inferior
    axis.text.y  = element_blank(),
    axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    axis.title.y = element_blank()
  )
plot_asignaciones_petroleo

animate(plot_asignaciones_petroleo, fps = 40, duration = 80, width = 1000, height = 600)

##################################################################
#bar chart para condensado en mbd

bd_condensado <- produccion_asignaciones%>%
  group_by(FECHA)%>%
  filter(CONDENSADO_MBD != 0)%>%#filtramos la informacion para que solo se vean los valores distintos de 0 en la gráfica
  top_n(n = 10, wt = CONDENSADO_MBD)%>%
  mutate(Rank_2 = min_rank(Rank_2) * 120)%>%
  ungroup()

bd_condensado <- bd_condensado[with(bd_condensado, order(FECHA, -CONDENSADO_MBD)),]


plot_asignaciones_condensado <- ggplot(bd_condensado,aes(x = Rank_2, group = ASIGNACION)) + 
  geom_col(aes(x = Rank_2, y = CONDENSADO_MBD), alpha = 0.8 , width = 100, fill = "#003249", color = "black") + # Columnas
  coord_flip(clip = "off", expand = FALSE) + # Flip
  labs(title = paste("Produccion de Condensado 
  {closest_state}", sep=""),subtitle = 'Miles de barriles diarios',  
       caption  = "Fuente: Fondo Mexicano del Petróleo") +# Labels o ejes
  theme_minimal() + # Tema de la grafica
  geom_text(aes(x = Rank_2, y = 0, label = ASIGNACION), hjust = 1) + # Names
  geom_text(aes(x = Rank_2, y = CONDENSADO_MBD + 0, label = as.character(round(CONDENSADO_MBD,2))), hjust = 0, color = "black") +#Valores observados
  scale_y_continuous(labels = scales::comma) + # Formato de los valores para el eje y
  scale_x_reverse() + # Ponemos el valor mas alto en el top de la grafica
  transition_states(FECHA, transition_length = 1, state_length = 1) + # Animamos la grafica pasando como paranmetro de movimiento la columna Fecha
  view_follow()+
  theme(
    legend.position="none",
    plot.margin = margin(0,2,0,3,"cm"),
    plot.title=element_text(size=22, hjust=0.5, face="bold", colour="grey20", vjust=-1),#tamaño de la fuente de titulo
    plot.subtitle=element_text(size = 20, hjust=0.5, family = "Monotype Corsiva" , color="grey20"),#tamaño y fuente del subtitulo
    plot.caption =element_text(size = 10, hjust = 0, face="plain", color="grey20"),#tamaño y fuente de la nota inferior
    axis.text.y  = element_blank(),
    axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    axis.title.y = element_blank()
  )
plot_asignaciones_condensado

animate(plot_asignaciones_condensado, fps = 40, duration = 80, width = 1000, height = 600)

##################################################################
#bar chart para gas en mmpcd


bd_gas <- produccion_asignaciones%>%
  group_by(FECHA)%>%
  filter(GAS_MMPCD != 0)%>%#filtramos la informacion para que solo se vean los valores distintos de 0 en la gráfica
  top_n(n = 10, wt = GAS_MMPCD)%>%
  mutate(Rank_3 = min_rank(Rank_3) * 120)%>%
  ungroup()

bd_gas <- bd_gas[with(bd_gas, order(FECHA, -GAS_MMPCD)),]


plot_asignaciones_gas <- ggplot(bd_gas,aes(x = Rank_3, group = ASIGNACION)) + 
  geom_col(aes(x = Rank_3, y = GAS_MMPCD), alpha = 0.8 , width = 100, fill = "#003249", color = "black") + # Columnas
  coord_flip(clip = "off", expand = FALSE) + # Flip
  labs(title = paste("Produccion de Gas
  {closest_state}", sep=""),subtitle = 'Miles de pies cúbicos diarios',  
       caption  = "Fuente: Fondo Mexicano del Petróleo") +# Labels o ejes
  theme_minimal() + # Tema de la grafica
  geom_text(aes(x = Rank_3, y = 0, label = ASIGNACION), hjust = 1) + # Names
  geom_text(aes(x = Rank_3, y = GAS_MMPCD + 0, label = as.character(round(GAS_MMPCD,2))), hjust = 0, color = "black") +#Valores observados
  scale_y_continuous(labels = scales::comma) + # Formato de los valores para el eje y
  scale_x_reverse() + # Ponemos el valor mas alto en el top de la grafica
  transition_states(FECHA, transition_length = 1, state_length = 1) + # Animamos la grafica pasando como paranmetro de movimiento la columna Fecha
  view_follow()+
  theme(
    legend.position="none",
    plot.margin = margin(0,2,0,3,"cm"),
    plot.title=element_text(size=22, hjust=0.5, face="bold", colour="grey20", vjust=-1),#tamaño de la fuente de titulo
    plot.subtitle=element_text(size = 20, hjust=0.5, family = "Monotype Corsiva" , color="grey20"),#tamaño y fuente del subtitulo
    plot.caption =element_text(size = 10, hjust = 0, face="plain", color="grey20"),#tamaño y fuente de la nota inferior
    axis.text.y  = element_blank(),
    axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    axis.title.y = element_blank()
  )
plot_asignaciones_gas

animate(plot_asignaciones_gas, fps = 40, duration = 80, width = 1000, height = 600)

##################################################################
#bar chart para gas asociado en mmpcd

bd_gas_asoc <- produccion_asignaciones%>%
  group_by(FECHA)%>%
  filter(GAS_ASOC_MMPCD != 0)%>%#filtramos la informacion para que solo se vean los valores distintos de 0 en la gráfica
  top_n(n = 10, wt = GAS_ASOC_MMPCD)%>%
  mutate(Rank_4 = min_rank(Rank_4) * 120)%>%
  ungroup()

bd_gas_asoc <- bd_gas_asoc[with(bd_gas_asoc, order(FECHA, -GAS_ASOC_MMPCD)),]


plot_asignaciones_gas_asoc <- ggplot(bd_gas_asoc,aes(x = Rank_4, group = ASIGNACION)) + 
  geom_col(aes(x = Rank_4, y = GAS_ASOC_MMPCD), alpha = 0.8 , width = 100, fill = "#003249", color = "black") + # Columnas
  coord_flip(clip = "off", expand = FALSE) + # Flip
  labs(title = paste("Produccion de Gas asociado
  {closest_state}", sep=""),subtitle = 'Miles de pies cúbicos diarios',  
       caption  = "Fuente: Fondo Mexicano del Petróleo") +# Labels o ejes
  theme_minimal() + # Tema de la grafica
  geom_text(aes(x = Rank_4, y = 0, label = ASIGNACION), hjust = 1) + # Names
  geom_text(aes(x = Rank_4, y = GAS_ASOC_MMPCD + 0, label = as.character(round(GAS_ASOC_MMPCD,2))), hjust = 0, color = "black") +#Valores observados
  scale_y_continuous(labels = scales::comma) + # Formato de los valores para el eje y
  scale_x_reverse() + # Ponemos el valor mas alto en el top de la grafica
  transition_states(FECHA, transition_length = 1, state_length = 1) + # Animamos la grafica pasando como paranmetro de movimiento la columna Fecha
  view_follow()+
  theme(
    legend.position="none",
    plot.margin = margin(0,2,0,3,"cm"),
    plot.title=element_text(size=22, hjust=0.5, face="bold", colour="grey20", vjust=-1),#tamaño de la fuente de titulo
    plot.subtitle=element_text(size = 20, hjust=0.5, family = "Monotype Corsiva" , color="grey20"),#tamaño y fuente del subtitulo
    plot.caption =element_text(size = 10, hjust = 0, face="plain", color="grey20"),#tamaño y fuente de la nota inferior
    axis.text.y  = element_blank(),
    axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    axis.title.y = element_blank()
  )
plot_asignaciones_gas_asoc

animate(plot_asignaciones_gas_asoc, fps = 40, duration = 80, width = 1000, height = 600)


##################################################################
#bar chart para gas no-asociado en mmpcd

bd_gas_no_asoc <- produccion_asignaciones%>%
  group_by(FECHA)%>%
  filter(GAS_NASOC_MMPCD != 0)%>%#filtramos la informacion para que solo se vean los valores distintos de 0 en la gráfica
  top_n(n = 10, wt = GAS_NASOC_MMPCD)%>%
  mutate(Rank_5 = min_rank(Rank_5) * 120)%>%
  ungroup()

bd_gas__no_asoc <- bd_gas_no_asoc[with(bd_gas_no_asoc, order(FECHA, -GAS_NASOC_MMPCD)),]


plot_asignaciones_gas_no_asoc <- ggplot(bd_gas_no_asoc,aes(x = Rank_5, group = ASIGNACION)) + 
  geom_col(aes(x = Rank_5, y = GAS_NASOC_MMPCD), alpha = 0.8 , width = 100, fill = "#003249", color = "black") + # Columnas
  coord_flip(clip = "off", expand = FALSE) + # Flip
  labs(title = paste("Produccion de Gas no asociado  
  {closest_state}", sep=""),subtitle = 'Miles de pies cúbicos diarios',  
       caption  = "Fuente: Fondo Mexicano del Petróleo") +# Labels o ejes
  theme_minimal() + # Tema de la grafica
  geom_text(aes(x = Rank_5, y = 0, label = ASIGNACION), hjust = 1) + # Names
  geom_text(aes(x = Rank_5, y = GAS_NASOC_MMPCD + 0, label = as.character(round(GAS_NASOC_MMPCD,2))), hjust = 0, color = "black") +#Valores observados
  scale_y_continuous(labels = scales::comma) + # Formato de los valores para el eje y
  scale_x_reverse() + # Ponemos el valor mas alto en el top de la grafica
  transition_states(FECHA, transition_length = 1, state_length = 1) + # Animamos la grafica pasando como paranmetro de movimiento la columna Fecha
  view_follow()+
  theme(
    legend.position="none",
    plot.margin = margin(0,2,0,3,"cm"),
    plot.title=element_text(size=22, hjust=0.5, face="bold", colour="grey20", vjust=-1),#tamaño de la fuente de titulo
    plot.subtitle=element_text(size = 20, hjust=0.5, family = "Monotype Corsiva" , color="grey20"),#tamaño y fuente del subtitulo
    plot.caption =element_text(size = 10, hjust = 0, face="plain", color="grey20"),#tamaño y fuente de la nota inferior
    axis.text.y  = element_blank(),
    axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    axis.title.y = element_blank()
  )
plot_asignaciones_gas_no_asoc

animate(plot_asignaciones_gas_no_asoc, fps = 40, duration = 80, width = 1000, height = 600)


##################################################################
#bar chart para nitrogeno 


bd_nitro <- produccion_asignaciones%>%
  group_by(FECHA)%>%
  filter(NITROGENO_MMPCD != 0)%>%#filtramos la informacion para que solo se vean los valores distintos de 0 en la gráfica
  top_n(n = 10, wt = NITROGENO_MMPCD)%>%
  mutate(Rank_6 = min_rank(Rank_6) * 120)%>%
  ungroup()

bd_nitro <- bd_nitro[with(bd_nitro, order(FECHA, -NITROGENO_MMPCD)),]


plot_asignaciones_nitro <- ggplot(bd_nitro,aes(x = Rank_6, group = ASIGNACION)) + 
  geom_col(aes(x = Rank_6, y = NITROGENO_MMPCD), alpha = 0.8 , width = 100, fill = "#003249", color = "black") + # Columnas
  coord_flip(clip = "off", expand = FALSE) + # Flip
  labs(title = paste("Produccion de Nitrogeno  
  {closest_state}", sep=""),subtitle = 'Miles de pies cúbicos diarios',  
       caption  = "Fuente: Fondo Mexicano del Petróleo") +# Labels o ejes
  theme_minimal() + # Tema de la grafica
  geom_text(aes(x = Rank_6, y = 0, label = ASIGNACION), hjust = 1) + # Names
  geom_text(aes(x = Rank_6, y = NITROGENO_MMPCD + 0, label = as.character(round(NITROGENO_MMPCD,2))), hjust = 0, color = "black") +#Valores observados
  scale_y_continuous(labels = scales::comma) + # Formato de los valores para el eje y
  scale_x_reverse() + # Ponemos el valor mas alto en el top de la grafica
  transition_states(FECHA, transition_length = 1, state_length = 1) + # Animamos la grafica pasando como paranmetro de movimiento la columna Fecha
  view_follow()+
  theme(
    legend.position="none",
    plot.margin = margin(0,2,0,3,"cm"),
    plot.title=element_text(size=22, hjust=0.5, face="bold", colour="grey20", vjust=-1),#tamaño de la fuente de titulo
    plot.subtitle=element_text(size = 20, hjust=0.5, family = "Monotype Corsiva" , color="grey20"),#tamaño y fuente del subtitulo
    plot.caption =element_text(size = 10, hjust = 0, face="plain", color="grey20"),#tamaño y fuente de la nota inferior
    axis.text.y  = element_blank(),
    axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    axis.title.y = element_blank()
  )
plot_asignaciones_nitro

animate(plot_asignaciones_gas_no_asoc, fps = 40, duration = 80, width = 1000, height = 600)

#los cheat sheets en r ayudan para buscar funciones de manera mas facil

































