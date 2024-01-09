#Código prefinal(limpio)
#Primero cargamos las librerias que vamos a estar usando para la manipulacion de los datos y creacion de la grafica
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

#Carga de las bases a utilizar:
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

#Primer transformacion de la base de datos para darle el formato deseado
Base_IQY <- gather(data = Base_datos_IQY,'Serie de Producción de Petróleo','Produccion',SN3390,SN4029,
                   SN944,SN963,SN982,SN1001,SN1020,SN1039,SN1153,SN4428,SN1302,SN2512,
                   SN4479,SN2216,SN2235,SN1915,SN2569,SN2791,SN2883)#aqui se agregan los contratos nuevos.
as.data.frame(Base_IQY)

#Agregamos las columnas de Mes,Año y el Ranking a la base de datos previamente usada(Base_IQY) y la renombramos 
Base_IQY_2 <- Base_IQY %>%
  mutate(Mes = format(Fecha,"%B de %Y"))%>%
  mutate(Año = format(Fecha, "%Y"))%>%
  mutate(Rank = min_rank(desc(Produccion)))
#omitimos los na´s ´para evitar problemas al momento de graficar y vemos como quedo la nueva base
as.data.frame(na.omit(Base_IQY_2))
View(Base_IQY_2)

#Una vez transformada la base de datos y con las nuevas columnas que agregamos usaremos la base lista_de_campos
#para juntarla con la Base_IQY_2 esto con el fin  de asignarle a cada serie de produccion su respectivo campo
#usando como referencia la columna Serie de Produccion de Petróleo como identificador(Similar al proceso de usar joins con SQL)

Base_IQY_2.2 <- merge(Base_IQY_2,lista_de_campos,all.x = TRUE)
View(Base_IQY_2.2)  

#Ahora ya podemos empezar a hacer la grafica por lo que primero asignaremos una nueva variable con la informacion de la base
#ya lista y agruparemos respecto a la fecha, luego vamos a filtrar la informacion, despues con top le diremos a la 
#grafica el número de datos que deseamos plotear y con mutate organizaremos el Ranking de mayor a menor produccion asignando el 
#1 al valor mas alto y asi sucesivamente hasta el valor mas bajo de cada mes por año.

Data <- Base_IQY_2.2%>%
  group_by(Fecha)%>%
  filter()%>%
  top_n(n = 7, wt = Produccion)%>%
  mutate(Rank = min_rank(Rank) * 120)%>%
  ungroup()

Data <- Data[with(Data, order(Fecha, -Produccion)),]

#Código de la grafica
plot_campos <- ggplot(Data,aes(x = Rank, group = Campo)) + 
  geom_col(aes(x = Rank, y = Produccion), alpha = 0.5 , width = 100, fill = "#008B8B", color = "black") + # Columnas
  coord_flip(clip = "off", expand = FALSE) + # Flip
  labs(title = paste("Produccion de Petróleo 
  {closest_state}", sep=""),subtitle = 'Barriles diarios',  
       caption  = "Fuente: Fondo Mexicano del Petróleo") +# Labels o ejes
  theme_minimal() + # Tema de la grafica
  geom_text(aes(x = Rank, y = -600, label = Campo), hjust = 1) + # Names
  geom_text(aes(x = Rank, y = Produccion + 200, label = as.character(Produccion)), hjust = 0, color = "black") +#Valores observados
  scale_y_continuous(labels = scales::comma) + # Formato de los valores para el eje y
  scale_x_reverse() + # Ponemos el valor mas alto en el top de la grafica
  transition_states(Fecha, transition_length = 1, state_length = 1) + # Animamos la grafica pasando como paranmetro de movimiento la columna Fecha
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
#Renderizamos la grafica
plot_campos

#Animamos y guardamos en formato de gif la grafica 
animate(plot_campos, fps = 40, duration = 80, width = 1000, height = 600,renderer = gifski_renderer("BCR Campos.gif"))


#Observaciones
#1.El formato de transicion de la grafica se quedo con Fecha ya que despues de varios intentos de agrupar de otra forma
#la transicion para que se viera cada mes por año no se logro y se seguia viendo la transcion en cada mes de cada año.
#2.Hacer el cambio de agregar comas a los valores observados en la grafica ocasiona que la variable Produccion cambie su tipo de dato
#i.e., de ser numerica la variable cambia por caracter y no es posible manipular este tipo de dato para despues plotearlo, aun asi se hizo
#el intento de modificar el tipo de dato a numerico pero esto genera que se pierdan valores y la grafica no se ve como corresponde,por lo que se 
#opto por dejar los valores como ya se tienen desde un principio.



















