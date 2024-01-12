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


PRODUCCION_CAMPOS <- read_excel("C:/Users/A19537/Downloads/PRODUCCION_CAMPOS.xlsx", 
                                col_types = c("date", "numeric", "numeric", 
                                              "numeric", "numeric", "numeric", 
                                              "numeric", "numeric", "numeric", 
                                              "numeric", "numeric"), skip = 11)
View(PRODUCCION_CAMPOS)


PRODUCCION_CAMPOS <- PRODUCCION_CAMPOS %>%
  mutate(decada = format(FECHA, "%Y") <- 0) %>%
  mutate(ranking_1 = min_rank(desc(PETROLEO_MBD))) %>%
  mutate(ranking_2 = min_rank(desc(CONDENSADO_MBD)))

View(PRODUCCION_CAMPOS)
#Agrupar las fechas por decadas
bd_sesentas <- PRODUCCION_CAMPOS%>%
  group_by(Año)%>%
  filter(Año <= 1969)%>%
  top_n(n = 10, wt = PETROLEO_MBD)%>%
  mutate(ranking_1 = min_rank(ranking_1) * 120)%>%
  mutate(Sesentas = Año <= 1969)%>%
  ungroup()


bd_sesentas <- bd_sesentas[with(bd_sesentas, order(Año, -PETROLEO_MBD)),]

#Código de la grafica
plot_sesentas_pet <- ggplot(bd_sesentas,aes(x = ranking_1, group = bd_sesentas$CAMPO_OFICIAL)) + 
  geom_col(aes(x = ranking_1, y = PETROLEO_MBD), alpha = 0.8 , width = 100, fill = "#003249", color = "black") + # Columnas
  coord_flip(clip = "off", expand = FALSE) + # Flip
  labs(title = paste("Produccion de Petróleo 
  {closest_state}", sep=""),subtitle = 'Miles de barriles diarios',  
       caption  = "Fuente: Fondo Mexicano del Petróleo") +# Labels o ejes
  theme_minimal() + # Tema de la grafica
  geom_text(aes(x = ranking_1, y = 0, label = bd_sesentas$CAMPO_OFICIAL), hjust = 1) + # Names
  geom_text(aes(x = ranking_1, y = PETROLEO_MBD + 0, label = as.character(round(PETROLEO_MBD,2))), hjust = 0, color = "black") +#Valores observados
  scale_y_continuous(labels = scales::comma) + # Formato de los valores para el eje y
  scale_x_reverse() + # Ponemos el valor mas alto en el top de la grafica
  transition_states(Año, transition_length = 1, state_length = 1) + # Animamos la grafica pasando como paranmetro de movimiento la columna Año
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
plot_sesentas_pet



