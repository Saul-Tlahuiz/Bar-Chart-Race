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



PRODUCCION_CAMPOS <- read_excel("C:/Users/A19537/Downloads/PRODUCCION_CAMPOS.xlsx", 
                                col_types = c("date", "text", "text", 
                                              "text", "numeric", "numeric", "numeric", 
                                              "numeric", "numeric", "numeric", 
                                              "numeric"), skip = 11)
View(PRODUCCION_CAMPOS)


PRODUCCION_CAMPOS <- PRODUCCION_CAMPOS %>%
  mutate(Año = format(FECHA, "%Y")) %>%
  mutate_if(Año = Año <= 1969)%>%
  mutate(ranking_1 = min_rank(desc(PETROLEO_MBD)))

View(PRODUCCION_CAMPOS)
#Agrupar las fechas por decadas

bd_campos <- PRODUCCION_CAMPOS%>%
  group_by(FECHA)%>%
  filter()%>%
  top_n(n = 10, wt = PETROLEO_MBD)%>%
  mutate(ranking_1 = min_rank(ranking_1) * 120)%>%
  mutate(sesentas = Año <= 1969)%>%
  mutate(sesentas = case_when(Año <= 1969,TRUE ~ 0))%>%
  ungroup()

View(bd_campos)

?mutate_if



