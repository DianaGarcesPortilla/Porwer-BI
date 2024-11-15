
#Minería de datos -Proyecto Final -------------------------------------------------

#Presentado por: 

#Linda Vanessa De La Rans
#Diana Liceth Garcés Portilla

#---------------------------------------------------------------------------
# instalacion de librerias en R

install.packages("readr")
install.packages("dplyr")
install.packages("moments")
install.packages("ggplot2")
install.packages("plotly")
install.packages("tidyr")
install.packages("stringr")
install.packages("naniar")
install.packages("skimr")
install.packages("agricolae")
install.packages("corrplot")
install.packages("ggpubr")
install.packages("lubridate",dependencies = TRUE)
install.packages("scales")


#----------------------------------------------------------------------------
# librerias

library(readr)
library(dplyr)
library(moments)
library(ggplot2)
library(plotly)
library(tidyr)
library(stringr)
library(naniar)
library(skimr)
library(agricolae)
library(corrplot)
library(ggpubr)
library(lubridate)
library(scales)

#-----------------------------------------------------------------------------
#Configuracion lugar de trabajo

ruta = "F:/Documents/Doc org Escritorio/Maestria_UC/6_2_mineria_R/Semana_8"
setwd(ruta)
#-----------------------------------------------------------------------------
#Importar datos

ecommerce = read_delim("Ecommerce.txt")
#-----------------------------------------------------------------------------
#Preparación de datos 

str(ecommerce)

#Cambiar el nombre de las variables
ecommerce <- ecommerce %>%  
             rename( Avg_Session_Length = "Avg. Session Length",
                     Time_on_App  = "Time on App",
                     Time_on_Website   = "Time on Website",
                     Length_of_Membership = "Length of Membership",
                     Yearly_Amount_Spent = "Yearly Amount Spent"
                    )
str(ecommerce)

#-----------------------------------------------------------------------------

#3.- Realiza una estadística descriptiva de las variables en estudio 
#(excluyendo email, Address y Avatar). Para las variables cuantitativas 
#utiliza medidas de localización, dispersión y forma. Interpreta los resultados.

# Estadisticas descriptivas para Avg_Session_Length

descriptivos_session <- sapply(ecommerce["Avg_Session_Length"], function(x) {
                             c(Media = mean(x), Desv_estandar = sd(x), Cuartil = quantile(x), 
                             Asimetria= skewness(x), Curtosis= kurtosis(x))
                            })
descriptivos_session

# Estadisticas descriptivas para Time_on_App

descriptivos_app <- sapply(ecommerce["Time_on_App"], function(x) {
                    c(Media = mean(x), Desv_estandar = sd(x), Cuartil = quantile(x), 
                    Asimetria= skewness(x), Curtosis= kurtosis(x))
                    })

descriptivos_app

# Estadisticas descriptivas para Time_on_Website

descriptivos_web <- sapply(ecommerce["Time_on_Website"], function(x) {
                          c(Media = mean(x), Desv_estandar = sd(x), Cuartil = quantile(x), 
                          Asimetria= skewness(x), Curtosis= kurtosis(x))
})

descriptivos_web

# Estadisticas descriptivas para Length_of_Membership

descriptivos_membership <- sapply(ecommerce["Length_of_Membership"], function(x) {
  c(Media = mean(x), Desv_estandar = sd(x), Cuartil = quantile(x), 
    Asimetria= skewness(x), Curtosis= kurtosis(x))
})

descriptivos_membership

# Estadisticas descriptivas para Yearly_Amount_Spent

descriptivos_spent <- sapply(ecommerce["Yearly_Amount_Spent"], function(x) {
                             c(Media = mean(x), Desv_estandar = sd(x), Cuartil = quantile(x), 
                             Asimetria= skewness(x), Curtosis= kurtosis(x))
})

descriptivos_spent

#-----------------------------------------------------------------------------

#4.- Construye gráficos univariados para todas las variables cuantitativas. 
#Luego, construye gráficos bivariados (dispersión) entre la variable “Yearly Amount Spentr” 
#y cada una de las variables restantes. Interpreta los resultados.


# Histograma de la duración promedio de sesión en ecommerce (min) - Avg_Session_Length

min_avg_dprom <- round(min(ecommerce[["Avg_Session_Length"]]),1)
max_avg_dprom<- round(max(ecommerce[["Avg_Session_Length"]]),1)
amplitud_avg_dprom <- round((max_avg_dprom - min_avg_dprom)/nclass.Sturges(ecommerce$Avg_Session_Length),1)

duracion_prom <- ggplot(data = ecommerce, aes(x = Avg_Session_Length))  +
              geom_histogram(alpha = 0.7, 
                           color = "black", 
                           fill	 = "blue",
                           binwidth = amplitud_avg_dprom,
                           boundary = 0,
                           bins	 = nclass.Sturges(ecommerce$Avg_Session_Length))	+
             labs(x	= "Duración promedio (minutos)", y	= "Frecuencia") +
             scale_x_continuous(breaks = seq(min_avg_dprom, max_avg_dprom,amplitud_avg_dprom)) + 
             scale_y_continuous(breaks = seq(0, 130, by=10))
theme_bw()



#Histograma de la duración promedio de sesión en la app (min)- Time_on_App

min_avg <- round(min(ecommerce[["Time_on_App"]]),1)
max_avg<- round(max(ecommerce[["Time_on_App"]]),1)
amplitud_avg <- round((max_avg - min_avg)/nclass.Sturges(ecommerce$Time_on_App),1)

duracion_app  = ggplot(data = ecommerce, aes(x = Time_on_App))  +
                geom_histogram(alpha = 0.7, 
                               color = "black", 
                               fill	 = "blue",
                               binwidth = amplitud_avg, 
                               boundary = 0,
                               bins	 = nclass.Sturges(ecommerce$Time_on_App))	+
                labs(x	= "Duración promedio (minutos)", y	= "Frecuencia") +
                scale_x_continuous(breaks = seq(min_avg, max_avg, by=amplitud_avg)) +
                scale_y_continuous(breaks = seq(0, 130, by=10))
theme_bw()


# Histograma de la duración promedio de la sesión en la web (min)- Time_on_Website

min_avg <- round(min(ecommerce[["Time_on_Website"]]),1)
max_avg<- round(max(ecommerce[["Time_on_Website"]]),1)
amplitud_avg <- round((max_avg - min_avg)/nclass.Sturges(ecommerce$Time_on_Website),1)

duracion_web  = ggplot(data = ecommerce, aes(x = Time_on_Website))  +
                geom_histogram(alpha = 0.7, 
                               color = "black", 
                               fill	 = "blue",
                               binwidth = amplitud_avg, 
                               bins	 = nclass.Sturges(ecommerce$Time_on_Website))	+
                labs(x	= "Duración promedio (minutos)", y	= "Frecuencia") +
                scale_x_continuous(breaks = seq(min_avg, max_avg, by=amplitud_avg)) + 
                scale_y_continuous(breaks = seq(0, 130, by=10))
theme_bw()

#Histograma años de la membresía -Length_of_Membership

min_avg <- round(min(ecommerce[["Length_of_Membership"]]),1)
max_avg<- round(max(ecommerce[["Length_of_Membership"]]),1)
amplitud_avg <- round((max_avg - min_avg)/nclass.Sturges(ecommerce$Length_of_Membership),1)

anos_membresia  = ggplot(data = ecommerce, aes(x = Length_of_Membership))  +
                  geom_histogram(alpha = 0.7, 
                                 color = "black", 
                                 fill	 = "blue",
                                 binwidth = amplitud_avg, 
                                 boundary=-0.5,
                                 bins	 = nclass.Sturges(ecommerce$Length_of_Membership))	+
                  labs(x	= "Años", y	= "Frecuencia") +
                  scale_x_continuous(breaks = seq(min_avg, max_avg, by=amplitud_avg)) + 
                  scale_y_continuous(breaks = seq(0, 160, by=10))
theme_bw()


#Histograma Gasto anual (dólares) -Yearly_Amount_Spent

min_avg <- round(min(ecommerce[["Yearly_Amount_Spent"]]),1)
max_avg<- round(max(ecommerce[["Yearly_Amount_Spent"]]),1)
amplitud_avg<- round((max_avg - min_avg)/nclass.Sturges(ecommerce$Yearly_Amount_Spent),1)
 
gasto_ano  = ggplot(data = ecommerce, aes(x = Yearly_Amount_Spent))  +
               geom_histogram(alpha = 0.7, 
                              color = "black", 
                              fill  = "blue",
                              binwidth = amplitud_avg, 
                              boundary=0,
                              bins  = nclass.Sturges(ecommerce$Yearly_Amount_Spent))	+
               labs(x	= "Dólares", y	= "Frecuencia") +
               scale_x_continuous(breaks = seq(min_avg, max_avg, by=amplitud_avg)) +
               scale_y_continuous(breaks = seq(0, 130, by=10))
theme_bw()
 

plot_hist <- ggarrange(duracion_prom,duracion_app,
                       duracion_web,anos_membresia,gasto_ano,
                       ncol=3,nrow=2,common.legend = TRUE)

plot_hist

# Gráfico de dispersión Yearly_Amount_Spent vs Avg_Session_Length


spent_session <- ggplot(ecommerce, aes(x = Avg_Session_Length,
                                 y = Yearly_Amount_Spent)) +
           geom_point(color = " slategray4 ",alpha =0.7) +
           labs(y = "Yearly_Amount_Spent", x = "Avg_Session_Length",
          title = "Gráfico de dispersión Yearly_Amount_Spent vs Avg_Session_Length") 
theme_bw ()

spent_session


# Gráfico de dispersión Yearly_Amount_Spent vs Time_on_App 


spent_app <- ggplot(ecommerce, aes(x = Time_on_App ,
                                       y = Yearly_Amount_Spent)) +
  geom_point(color = " slategray4 ",alpha =0.7) +
  labs(y = "Yearly_Amount_Spent", x = "Time_on_App",
       title = "Gráfico de dispersión Yearly_Amount_Spent vs Time_on_App ") 
theme_bw ()

spent_app


# Gráfico de dispersión Yearly_Amount_Spent vs Time_on_Website 


spent_web <- ggplot(ecommerce, aes(x = Time_on_Website ,
                                   y = Yearly_Amount_Spent)) +
  geom_point(color = " slategray4 ",alpha =0.7) +
  labs(y = "Yearly_Amount_Spent", x = "Time_on_Website",
       title = "Gráfico de dispersión Yearly_Amount_Spent vs Time_on_Website") 
theme_bw ()

spent_web

# Gráfico de dispersión Yearly_Amount_Spent vs Length_of_Membership 


spent_member <- ggplot(ecommerce, aes(x = Length_of_Membership,
                                   y = Yearly_Amount_Spent)) +
  geom_point(color = " slategray4 ",alpha =0.7) +
  labs(y = "Yearly_Amount_Spent", x = "Length_of_Membership",
       title = "Gráfico de dispersión Yearly_Amount_Spent vs Length_of_Membership") 
theme_bw ()

spent_member

#-----------------------------------------------------------------------------

# 5.- Construye la variable “AvgSessionGroup” a partir de la variable 
#“Avg. Session Length:”, con rangos [29, 32), [32, 33), [33, 34), y [34, 36.2). 
#Descríbela desde la perspectiva de una variable de agrupación, realiza un gráfico 
#adecuado, y para cada una de las nuevas categorías determina la media, la mediana y 
#la desviación estándar de la variable “Yearly Amount Spentr”. Comenta tus resultados.

ecommerce$AvgSessionGroup <- cut(ecommerce$Avg_Session_Length, breaks = c(29,32,33,34,36.2),
                                include.lowest = T, right = F )


#Frecuencia absoluta 
fre_ab <- table(ecommerce$AvgSessionGroup)
fre_ab

#Frecuencia relativa
fre_relativa <- prop.table(fre_ab)*100
fre_relativa

#Convertir la tabla de frecuencias relativas en dataframe

df <- as.data.frame(fre_relativa)
df <- df %>% rename (Duración = Var1, Porcentaje = Freq)


#Gráfico de barras variable AvgSessionGroup

plot_ly(df,x=~Duración,y=~Porcentaje,type="bar")

# Estadisticos descriptivos variable AvgSessionGroup

group_by(ecommerce, AvgSessionGroup)%>%
summarise(media   = mean(Yearly_Amount_Spent),
          mediana = median(Yearly_Amount_Spent), 
          Desv.estandar = sd(Yearly_Amount_Spent), 
          )

#-----------------------------------------------------------------------------

#6.- Para cada una de las categorías de la variable “AvgSessionGroup”, 
#genera un boxplot de la variable “Yearly Amount Spentr” e interpreta cada uno de ellos.


SessionGroup_Spent <- ggplot(ecommerce, aes(x = AvgSessionGroup, 
                             y = Yearly_Amount_Spent)) +
                      geom_boxplot(fill= 'blue' , color = "black",alpha =0.7) +
                      labs (y = "Yearly Amount Spentr", 
                            x = "AvgSessionGroup (minutos)",
                            title = "Boxplot Duración de sesión en línea según gasto" ) +
                      theme_bw () +
                      theme(legend.position = "none")
SessionGroup_Spent

#-----------------------------------------------------------------------------

#7.-Para todas las variables cuantitativas, calcula la matriz de las correlaciones. 
#Interpreta la magnitud de las correlaciones de las variables cuantitativas 
#con la variable “Yearly Amount Spentr”

var_cuantitativas <- ecommerce[c(4:8)]

# Matriz de correlacion

round(cor(var_cuantitativas, method = "pearson"),3)

# Heatmap

corrplot(cor(var_cuantitativas, method = "pearson"),
         method = "color",
         type = "lower",
         diag = FALSE,
         addCoef.col = "black")

#-----------------------------------------------------------------------------

#8.-Supón que se desea realizar un modelo de regresión lineal múltiple 
#considerando como variable respuesta “Yearly Amount Spentr” y como variables 
#independientes: a. x1:“Avg.SessionLength”, b.x2:“TimeonApp”, c. x3:“TimeonWebsite”,
#d.x4:“LengthofMembership”. 

#Utilizando las operaciones y funciones de matrices, encuentra los estimadores 
#de mínimos cuadrados ordinarios para los cinco parámetros (los B’s). 
#Interpreta los resultados en base a los coeficientes estimados y 
#a los análisis realizados durante el proyecto.

# Matriz de la variables respuesta
var_respuesta <- var_cuantitativas$Yearly_Amount_Spent
matriz_var_respuesta <- data.matrix(var_respuesta)
dim(matriz_var_respuesta)

# Convertir el dataframe de variables independientes en una matriz

#Data con variables independientes
var_independientes <- var_cuantitativas[c(1:4)]

#En data se incluye la columna de unos 
var_independientes$colum_unos <- rep(1, 500)

#Se organiza la data
var_independientes <- var_independientes[c("colum_unos", "Avg_Session_Length",
                                            "Time_on_App", "Time_on_Website",
                                           "Length_of_Membership")]
#Matriz de variables independientes
matriz_var_independientes <- data.matrix(var_independientes)
dim(matriz_var_independientes)

#Transpuesta de la matriz de variables independientes
matriz_t <- t(matriz_var_independientes)

# Multiplicación de la matriz de variables independientes y su transpuesta
matriz_m <- matriz_t%*%matriz_var_independientes

# Matriz inversa de matriz_m
matriz_inv <-solve(matriz_m)

# Multiplicación de matriz_t y matriz_var_respuesta
matriz1<- matriz_t%*%matriz_var_respuesta

# Multiplicación de matriz_inv y matriz1 para obtener los coeficientes de regresión
matriz_B<- matriz_inv%*%matriz1
matriz_B

modelo <- lm(Yearly_Amount_Spent ~ Avg_Session_Length + Time_on_App + Time_on_Website +
               Length_of_Membership, data = var_cuantitativas)
summary(modelo)


