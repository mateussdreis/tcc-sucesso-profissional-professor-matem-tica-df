
#Limpando dados de idade etc

library(tidyverse)
library(dplyr)
library(tm)

db <- read_csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vQO3zDHq-lTwrlUmh-8FrJ3XsYUe1qQwXlYOQ18gdCPpqg1_q_6OKR1vr5GiXZnRxEdPapBfQfyE-72/pub?output=csv")

#o calculo da idade não está funcionando
# %>% 
#   select(-c(`Carimbo de data/hora`,Gênero)) %>% 
#   mutate(
#     idade = {(Sys.Date()-as.Date(`Data de nascimento`,"%d/%m/%Y"))/365.25}%>% 
#       as.numeric()#%>% 
#       #round(digits = 2)
#   )

#Limpando o nome dos cabeçalhos

col.db<-colnames(db)

col.db=col.db %>% 
  removePunctuation() %>% 
  gsub("Marque apenas um item por linha","",.) %>% 
  gsub("Escolha apenas um item","",.)
  
#view(col.db)

colnames(db)<-col.db

#Excluindo a coluna do lattes e a coluna de e-mail:
db<-db[,-c(14,53)]

db$`Atualmente considere sua maior carga horária`[2]<-"Trabalha na Rede Federal (Instituto Federal de Brasília)"

write_csv(db,"/home/mateus/Dropbox/TCC_mateus_reis/TCC/PROGRAMAÇÃO/tcc-sucesso-profissional-professor-matem-tica-df/base_tcc.csv")




