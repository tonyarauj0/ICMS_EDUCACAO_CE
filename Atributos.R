library(readxl)

#ICMS

# Importando uma worksheet para um dataframe
df_ICMS_w <- read_excel("cotaparte_resultados_wide.xlsx", col_names=TRUE)
str(df_ICMS_w)

##Deixando variaveis em percentual
names(df_ICMS_w)

library(tidyverse)

##Tentativa 1 (Erro no loop)
x = paste0("txdif", 2005:2017)

for(i in 1:13){
  nova=x[i]
  df_ICMS_w %>% group_by(cod_mun) %>%
    mutate(nova=nova*100)
}

##Tentativa 2 (Crio variaveis mais nao consigo juntar ao df)


df_ICMS_w[grep("txdif",names(df_ICMS_w))]*100