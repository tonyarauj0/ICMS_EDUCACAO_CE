setwd("C:/Users/Tony/Documents/Guilherme")

getwd()

library(readxl)

#ICMS

# Importando uma worksheet para um dataframe
df_ICMS_w <- read_excel("cotaparte_resultados_wide.xlsx", col_names=TRUE)
str(df_ICMS_w)
##Deixando variaveis em percentual
names(df_ICMS_w)

library(tidyverse)

x = paste0("txdif", 2005:2017)

for(i in 1:13){
  nova=x[i]
  df_ICMS_w %>% group_by(cod_mun) %>%
    mutate(nova=nova*100)
}




df_ICMS_w[grep("txdif",names(df_ICMS_w))]*100

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
map_ICMS<-merge(ce, df_ICMS_w,
                by.x="codigo_ibg",
                by.y="cod_mun",
                duplicate.Geoms=TRUE)#Juntando atributo e shape

?tm_shape
?tm_fill
x = paste0("txdif", 2005:2017)
saida = paste0("tx_dif_", 2005:2017, ".png")

#OBS: Definir valor maximo e minimo do atributo
##Selecionando as posicoes das colunas 

grep("txdif",colnames(df_ICMS_w))

##Criando lista de minimos 
minimos<-apply(df_ICMS_w[,
        c(9,17,25,33,41,49,57,65,73,81,89,97,105)],
        2,min)
##Valor minimo
min(minimos)

##Criando lista de  maximos
maximos<-apply(df_ICMS_w[,
                         c(9,17,25,33,41,49,57,65,73,81,89,97,105)],
               2,max)
##Valor maximos
max(maximos)

#Os valores da legenda serão distribuidos em 4 classes:-1 ate 0; 0 ate 1, 1 ate 2, acima de 2.
quebras<-c(-1,-0.5,0,0.5,1,1.5,2,2.5)

for(i in 1:13){
  
  atributo = x[13]
  output = saida[13]
  
  png(filename = output, w = 12, h = 8, units = "in", res = 320)
  tm_shape(map_ICMS,
           unit ="km") +
    tm_fill( atributo , 
             title = "Taxa da Diferenca",
             style = "fixed",
             breaks = quebras,
             n=5,
             palette="RdBu",
             midpoint = NA,
             contrast=0.9,
             #legend.reverse = T,
             #
             legend.format = list(fun = function(x) paste0("",
                                                           formatC(x, digits = 2, format = "f",)),
                                  text.separator="-"),
             border.col = "white", 
             border.alpha = 0.5) +
    tm_layout(inner.margins = c(0.06, 0.10, 0.10, 0.08))+
    tm_legend(legend.position = c("right", "bottom"), scale=1,
              legend.outside.size=T) +
    tm_compass(type = "4star", size = 2.5, text.size = 0.5,
               color.dark = "gray60", text.color = "gray60",
               position = c("right", "top")) +
    tm_borders()
  dev.off()
}

?png

 
png(filename = "ano_2017.png", w = 12, h = 8, units = "in", res = 320)
tm_shape(map_ICMS,
           unit ="km") +
    tm_fill( "txdif2017", 
             title = "Mudança % na cota-parte",
             style = "fixed",
             breaks = quebras,
             n=5,
             palette="RdYlBu",
             midpoint = NA,
             contrast=0.9,
             interval.closure="right",
             #legend.reverse = T,
             #
             legend.format = list(fun = function(x) paste0("",
                                                           formatC(x, digits = 2, format = "f",)),
                                  text.separator="até"),
             border.col = "white", 
             border.alpha = 0.5) +
    tm_layout(inner.margins = c(0.06, 0.10, 0.10, 0.08))+
    tm_legend(legend.position = c("right", "bottom"), scale=1,
              legend.outside.size=T) +
        tm_borders()
  dev.off()


