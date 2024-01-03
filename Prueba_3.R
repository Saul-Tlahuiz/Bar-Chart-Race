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

bd_pt2 <- read_excel("Copia de Nueva base de datos(bar chart).xlsx", 
                     col_types = c("date", "numeric", "numeric", 
                                   "numeric", "numeric"))
View(bd_pt2)

#rm(bd_pt2)

#Lo que sigue a continuacion es lo necesario para preparar la base de datos, aun no estamos codificando la grafica

Contratos <- gather(data = bd_pt2,'Contratos','Produccion',`RF-C003`,`RF-C004`,`RF-C005`,`RF-C051`)#aqui se agregan los contratos nuevos.
Contratos

Base <- as.data.frame(Contratos)
View(Base)

#En esta parte a la base ya transformada con el formato deseado le vamos a agregar las columnas de Mes,Año las cuales nos van a servir en lo siguiente que haremos

base <- Base %>%
  mutate(Mes = format(Fecha,"%B de %Y"))%>%
  mutate(Año = format(Fecha, "%Y"))%>%
  mutate(Rank = min_rank(desc(Produccion)))
base
print(base, n = 200)

#En esta parte lo unico que hacemos es que a la base con las columnas agregadas la convertiremos en data frame y omitiremos los na, para evitar futuros problemas
Base_g <- as.data.frame(na.omit(base))
View(Base_g)

#En este apartado de data y data_2 lo que hacemos es que vamos a tomar el dataframe Base_g y vamos a agrupar por mes,para el primer caso y en el segundo 
#agrupamos por año, lo cual nos va a servir para hacer 2 graficas diferentes en las que cambiaremos la frecuencia de cambio, la cual viene dad por los meses,años respectivamente

data <- Base_g%>%
  group_by(Mes)%>%
  filter()%>%
  top_n(n = 25, wt = Produccion)%>%
  mutate(Rank = Rank)%>%
  ungroup()

data_2 <- Base_g%>%
  group_by(Año)%>%
  filter()%>%
  top_n(n = 10, wt = Produccion)%>%
  mutate(Rank = Rank)%>%
  ungroup()
data_2

data <- data[with(data, order(Fecha, -Produccion)),]
data_2 <- data_2[with(data_2, order(Fecha, -Produccion)),]


#######################################################################################################################################
#Lo que sigue a continuacion es ya teniendo la base preparada, empezar a codificar la bar chart

plot_new_rfc <- ggplot(data) + 
  geom_col(aes(x = Rank, y = Produccion), width = 10, fill = "firebrick", color = "black") + # Columns
  coord_flip(clip = "off", expand = FALSE) + # Flip
  labs(title='{closest_state}', x = "", y = "Miles de barriles") + # Labels
  theme_minimal() + # Theme
  geom_text(aes(x = Rank, y = -600, label = Contratos), hjust = 1) + # Names
  geom_text(aes(x = Rank, y = Produccion + 200, label = as.character(Produccion)), hjust = 0, color = "black") +#Values
  scale_y_continuous(labels = scales::comma) + # Format y-axis values
  scale_x_reverse() + # Highest values on top
  transition_states(Mes, transition_length = 4, state_length = 1) + # Animate
  theme(
    plot.title = element_text(hjust = 0, size = 20),
    plot.margin = margin(0,2,0,3,"cm"),
    axis.text.y  = element_blank()
  )
plot_new_rfc

animate(plot_new_rfc, fps = 15, duration = 30, width = 1000, height = 600)

#La animacion de plot_new_rfc va variando tanto por que esta tomando los valores agrupados por mes de cada año,a diferencia de la grafica de abajo llamada 
#plot_new_rfc2 ya que esta va cambiando por año, esto hace que la variacion sea menos frecuente y por ende tenga una mejor visualizacion.

plot_new_rfc2 <- ggplot(data_2) + 
  geom_col(aes(x = Rank, y = Produccion), width = 10, fill = "firebrick", color = "black") + # Columns
  coord_flip(clip = "off", expand = FALSE) + # Flip
  labs(title='{closest_state}', x = "", y = "Miles de barriles") + # Labels
  theme_minimal() + # Theme
  geom_text(aes(x = Rank, y = -600, label = Contratos), hjust = 1) + # Names
  geom_text(aes(x = Rank, y = Produccion + 200, label = as.character(Produccion)), hjust = 0, color = "black") +#Values
  scale_y_continuous(labels = scales::comma) + # Format y-axis values
  scale_x_reverse() + # Highest values on top
  transition_states(Año, transition_length = 4, state_length = 1) + # Animate
  theme(
    plot.title = element_text(hjust = 0, size = 20),
    plot.margin = margin(0,2,0,3,"cm"),
    axis.text.y  = element_blank()
    )
plot_new_rfc2
  
animate(plot_new_rfc2, fps = 40, duration = 30, width = 1000, height = 600)


################################################################################################################################################
#Buscar la forma que el código lea de la tabla del punto A, los nombres de los campos (columna b) y ese sea el nombre que se ve en la gráfica.

lista_de_campos <- read_excel("C:/Users/A19537/Downloads/lista de campos.xlsx")
View(lista_de_campos)




Base_datos_IQY <- read_excel("Base datos_IQY.xlsx", 
                             col_types = c("date", "numeric", "numeric", 
                                           "numeric", "numeric", "numeric", 
                                           "numeric", "numeric", "numeric", 
                                           "numeric", "numeric", "numeric", 
                                           "numeric", "numeric", "numeric", 
                                           "numeric", "numeric", "numeric", 
                                           "numeric", "numeric"))
View(Base_datos_IQY)

Base_IQY <- gather(data = Base_datos_IQY,'Serie de Producción de Petróleo','Produccion',SN3390,SN4029,
                   SN944,SN963,SN982,SN1001,SN1020,SN1039,SN1153,SN4428,SN1302,SN2512,
                   SN4479,SN2216,SN2235,SN1915,SN2569,SN2791,SN2883)#aqui se agregan los contratos nuevos.
as.data.frame(Base_IQY)


Base_IQY_2 <- Base_IQY %>%
  mutate(Mes = format(Fecha,"%B de %Y"))%>%
  mutate(Año = format(Fecha, "%Y"))%>%
  mutate(Rank = min_rank(desc(Produccion)))

as.data.frame(na.omit(Base_IQY_2))
View(Base_IQY_2)

Base_IQY_2.2 <- merge(Base_IQY_2,lista_de_campos,all.x = TRUE) #Con este comando lo que hacemos es juntar ambos dataframe
View(Base_IQY_2.2)                                            #(Base_IQY_2 Y lista_de_campos) tomando como referencia el numero de serie
                                                              #es decir que si tiene el mismo numero de serie en ambos data frames les asignara el nombre del campo

############################################################################################################################################
#Animacion 1
Data <- Base_IQY_2.2%>%
  group_by(Mes)%>%
  filter()%>%
  top_n(n = 7, wt = Produccion)%>%
  mutate(Rank = Rank)%>%
  ungroup()

Data <- Data[with(Data, order(Año, -Produccion)),]

plot_campos <- ggplot(Data) + 
  geom_col(aes(x = Rank, y = Produccion), width = 30, fill = "#008B8B", color = "black") + # Columns
  coord_flip(clip = "off", expand = FALSE) + # Flip
  labs(title = paste("Produccion ",'{closest_state}', sep=""),subtitle = 'Para Petróleo',  
       caption  = "Fuente: Fondo Mexicano del Petróleo") + # Labels
  theme_minimal() + # Theme
  geom_text(aes(x = Rank, y = -600, label = Campo), hjust = 1) + # Names
  geom_text(aes(x = Rank, y = Produccion + 200, label = as.character(Produccion)), hjust = 0, color = "black") +#Values
  scale_y_continuous(labels = scales::comma) + # Format y-axis values
  scale_x_reverse() + # Highest values on top
  transition_states(Mes, transition_length = 1, state_length = 1) + # Animate
  theme(
    legend.position="none",
    plot.margin = margin(0,2,0,3,"cm"),
    plot.title=element_text(size=18, hjust=0.5, face="bold", colour="grey20", vjust=-1),
    plot.subtitle=element_text(size = 18, hjust=0.5, face="plain", color="grey20"),
    plot.caption =element_text(size = 10, hjust=0.5, face="plain", color="grey20"),
    axis.text.y  = element_blank()
  )
plot_campos

animate(plot_campos, fps = 40, duration = 80, width = 1000, height = 600)

########################################################################################################
#Animacion 2
rm(Data2)
Data2 <- Base_IQY_2.2%>%
  group_by(Mes)%>%
  filter()%>%
  top_n(n = 7, wt = Produccion)%>%
  mutate(Rank = Rank)%>%
  ungroup()

Data2 <- Data2[with(Data2, order(Fecha, -Produccion)),]

plot_campos2 <- ggplot(Data2) + 
  geom_col(aes(x = Rank, y = Produccion), width = 40, fill = "dodgerblue4" , color = "black") + # Columnas
  coord_flip(clip = "off", expand = FALSE) + # Flip
  labs(title = paste("Produccion ",
                   '{closest_state}', sep=""),subtitle = 'Para Petróleo',  
       caption  = "Miles de Barriles | Fuente: Fondo Mexicano del Petróleo") + # Ejes
  theme_light() + # Tema
  geom_text(aes(x = Rank, y = -600, label = Campo), hjust = 1) + # Nombres
  geom_text(aes(x = Rank, y = Produccion + 200, label = as.character(Produccion)), hjust = 0, color = "black") +#Valores
  scale_y_continuous(labels = scales :: comma) + # Format y-axis values
  scale_x_reverse() + #Valor mas alto en el top
  guides(color = F, fill = F)+
  transition_states(Mes, transition_length = 10, state_length = 5, wrap = TRUE) +# Animacion
  theme(
    legend.position="none",
    plot.title=element_text(size=18, hjust=0.5, face="bold", colour="grey20", vjust=-1),
    plot.subtitle=element_text(size = 18, hjust=0.5, face="plain", color="grey20"),
    plot.caption =element_text(size = 10, hjust=0.5, face="plain", color="grey20"),
    axis.line=element_blank(),
    plot.margin = margin(2,2, 2, 4, "cm"), 
    plot.background=element_blank(),
    axis.text.x=element_blank(),
    axis.text.y=element_blank(),
    axis.ticks=element_blank(),
    axis.title.x=element_blank(),
    axis.title.y=element_blank(),
    panel.grid.minor=element_blank(),
    panel.grid.major=element_blank(),
    panel.background=element_blank(),
    panel.border=element_blank(),
    panel.grid.major.x = element_line( size=.1, color="grey8", linetype = "dashed" ),
    panel.grid.minor.x = element_line( size=.1, color="grey8", linetype = "dashed" )
  )
plot_campos2

animate(plot_campos2, fps = 40, duration = 80, width = 1000, height = 600)

#Tal vez las variaciones de fechas y produccion hacen que se mueva tanto la grafica y por lo tanto no se llegue a visualizar de la 
#forma en que se desea, de todas formas seguir intentando de otra forma

#Nota:Lo mas probable hasta el momento es que no se vizualice bien por que los tiempos de la variable Data usada para graficar
#cambian en periodos muy cortos de tiempo y ademas no tenemos informacion para todos los campos, lo que genera tanto cambio en la grafica y
#no se puede mantener estable.


##########################################################################################################################################
#Animacion 3
rm(bd)
bd <- na.omit(Base_IQY_2.2) %>%
  group_by(Mes)%>%
  filter() %>%
  arrange(Rank)%>%  #Nota:El arrange me da el ranking mas alto de cada fecha,no me da el mas alto de toda la base por cada fecha
  top_n(n = 7, wt = Produccion)%>%
  ungroup()

bd <- bd[with(bd, order(Año, -Produccion)),]

plot_bd <- ggplot(bd,aes(Rank,group = Campo, fill = "red")) +
  geom_col(aes(x = Rank,y = Produccion), alpha = 0.9,width = 50, color = "red") +
  coord_flip(clip = "off", expand = FALSE) +
  geom_text(aes(y = 0, label = paste(Campo)), vjust = 0.2, hjust = 1) +
  geom_text(aes(y = Rank, label = as.character(Produccion), hjust = 0)) +
  scale_y_continuous(labels = scales::comma) +
  scale_x_reverse() +
  guides(color = F, fill = F) +
  transition_states(Mes, transition_length = 1, state_length = 1, wrap= F) + 
  labs(title = paste("Produccion de ",'{closest_state}', sep=""),subtitle = 'Para Petróleo',
       caption  = "Miles de Barriles | Fuente: Fondo Mexicano del Petróleo") +
  theme(
    legend.position="none",
    plot.title=element_text(size=18, hjust=0.5, face="bold", colour="grey20", vjust=-1),
    plot.subtitle=element_text(size = 18, hjust=0.5, face="plain", color="grey20"),
    plot.caption =element_text(size = 10, hjust=0.5, face="plain", color="grey20"),
    axis.line=element_blank(),
    plot.margin = margin(2,2, 2, 4, "cm"), 
    plot.background=element_blank(),
    axis.text.x=element_blank(),
    axis.text.y=element_blank(),
    axis.ticks=element_blank(),
    axis.title.x=element_blank(),
    axis.title.y=element_blank(),
    panel.grid.minor=element_blank(),
    panel.grid.major=element_blank(),
    panel.background=element_blank(),
    panel.border=element_blank(),
    panel.grid.major.x = element_line( size=.1, color="grey80", linetype = "dashed" ),
    panel.grid.minor.x = element_line( size=.1, color="grey80", linetype = "dashed" )
  )
plot_bd

animate(plot_bd, fps = 40, duration = 80, width = 1000, height = 600)















