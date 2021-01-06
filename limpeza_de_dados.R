
#Limpando dados de idade etc

library(tidyverse)
library(dplyr)
library(tm)

db <- read_csv("/home/mateus/Dropbox/TCC_mateus_reis/TCC/PROGRAMAÇÃO/tcc-sucesso-profissional-professor-matem-tica-df/Formulário - Pesquisa Sucesso Profissional.csv") %>% 
  select(-c(`Carimbo de data/hora`,Gênero)) %>% 
  mutate(
    idade = {(Sys.Date() - `Data de nascimento`)/365.25}%>% 
      as.numeric()#%>% 
      #round(digits = 2)
  )

#Limpando o nome dos cabeçalhos

col.db<-colnames(db)

col.db=col.db %>% 
  removePunctuation() %>% 
  gsub("Marque apenas um item por linha","",.) %>% 
  gsub("Escolha apenas um item","",.)
  
#view(col.db)

colnames(db)<-col.db

#Excluindo a coluna do lattes e a coluna de e-mail:
db<-db[,-c(12,51)]


write_csv(db,"/home/mateus/Dropbox/TCC_mateus_reis/TCC/PROGRAMAÇÃO/tcc-sucesso-profissional-professor-matem-tica-df/base_tcc.csv")
