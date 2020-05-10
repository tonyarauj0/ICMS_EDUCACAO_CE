setwd("C:/Users/Tony/Documents/Guilherme/Git/ICMS_EDUCACAO_CE")

getwd()

library(readxl)

#ICMS

# Importando uma worksheet para um dataframe
df_ICMS_w <- read_excel("cotaparte_resultados_wide.xlsx", col_names=TRUE)
str(df_ICMS_w)
#Deixando variaveis em percentual
library(tidyverse)
library(dbplyr)
##Variaveis
names(df_ICMS_w)
##Criando lista com nome das variaveis
x = paste0("txdif", 2005:2017)
##Transformando a lista em um vetor
x_v<-unlist(x)
##Definindo a operaçao a ser feita
operacao<-function(x) x*100
##Deixando variaveis em percentual
df_ICMS_w<-mutate_at(df_ICMS_w,x_v,operacao)

#Encerrando pacotes
detach(package:dplyr)
detach(package:tidyverse)

#Carregando pacotes para mapas
library(rgdal)
library(raster)
library(colorspace)
library(rasterVis)
library(ggplot2)
library(tmap)    

#Importando Shape
ce <- readOGR("Limites_municipais_IPECE_2019_utm_sirgas_2000.shp")
proj4string(ce)
names(ce)
plot(ce)

#Mapa ICMS

##Juntando Atributos com o mapa
map_ICMS<-merge(ce, df_ICMS_w,
                by.x="codigo_ibg",
                by.y="cod_mun",
                duplicate.Geoms=TRUE)#Juntando atributo e shape

##OBS: Definir valor maximo e minimo do atributo

###Selecionando as posicoes das colunas 

grep("txdif",colnames(df_ICMS_w))

###Criando lista de minimos 
minimos<-apply(df_ICMS_w[,
        c(9,17,25,33,41,49,57,65,73,81,89,97,105)],
        2,min)
###Valor minimo
min(minimos)

###Criando lista de  maximos
maximos<-apply(df_ICMS_w[,
                         c(9,17,25,33,41,49,57,65,73,81,89,97,105)],
               2,max)
###Valor maximos
max(maximos)

##Os valores da legenda serão distribuidos em 4 classes:-1 ate 0; 0 ate 1, 1 ate 2, acima de 2.
quebras<-c(-100,-50,0,50,100,150,200,250)

#Elaborando Mapas

##Definindo cores azuis para maiores que zero e vermelhas para menores
library(RColorBrewer)
brewer.pal(n=11, "RdBu") #nomes das cores
display.brewer.pal(n=11, "RdBu") #visualizar as cores
brewer.pal(n=9, "Blues") #nomes das cores azuis
display.brewer.pal(n=9, "Blues") #visualizar as cores azuis


cores<-c("#B2182B", "#F4A582",#cores vermelhas
         "#C6DBEF","#6BAED6","#2171B5","#08306B") #cores azuis

#MAPA
map_2009<-tm_shape(map_ICMS,
         unit ="km") +
  tm_fill( "txdif2009", 
           title = "Mudança Percentual na cota-parte com a Lei 14.023/07",
           style = "fixed",
           breaks = quebras,
           palette=cores,
           contrast=0.9,
           interval.closure="right",
           midpoint = NA,
           text.separator= "até",
           legend.hist=TRUE)+
  tm_layout(legend.outside = TRUE,
            inner.margins = c(0.06, 0.10, 0.10, 0.08),
            legend.outside.position = c("right","bottom"),
            title.size = 0.8,
            title.position = c("right","bottom"),
            legend.position =c("right","bottom"),
            legend.hist.width=0.8,
            legend.format =list(text.separator="até"))+
  tm_borders("grey40", lwd=2)
dev.off()

#Salvando o mapa
tmap_save(map_2009, filename = "map_2009.png",
          width = 12, height = 8, units = "in", dpi =  320)


##Lista com nomes de saída dos mapas
entrada = paste0("txdif", 2005:2017)
saida = paste0("tx_dif_", 2005:2017, ".png")

##Tentativa 1 (loop)
for(i in 1:13){
  
  atributo = entrada[i]
  output = saida[i]
  
  #MAPA
  atributo<-tm_shape(map_ICMS,
                     unit ="km") +
    tm_fill( atributo, 
             title = "Mudança Percentual na cota-parte com a Lei 14.023/07",
             style = "fixed",
             breaks = quebras,
             palette=cores,
             contrast=0.9,
             interval.closure="right",
             midpoint = NA,
             text.separator= "até",
             legend.hist=TRUE)+
    tm_layout(legend.outside = TRUE,
              inner.margins = c(0.06, 0.10, 0.10, 0.08),
              legend.outside.position = c("right","bottom"),
              title.size = 0.8,
              title.position = c("right","bottom"),
              legend.position =c("right","bottom"),
              legend.hist.width=0.8)+
    tm_borders("grey40", lwd=2)
 dev.off()
  
  #Salvando o mapa
  tmap_save(atributo, filename = "output",
            width = 12, height = 8, units = "in", dpi =  320)
}






