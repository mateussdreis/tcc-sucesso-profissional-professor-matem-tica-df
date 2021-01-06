
#Limpando dados de idade etc

library(tidyverse)
library(dplyr)

db <- read_csv("/home/mateus/Dropbox/TCC_mateus_reis/TCC/PROGRAMAÇÃO/tcc-sucesso-profissional-professor-matem-tica-df/Formulário - Pesquisa Sucesso Profissional.csv") %>% 
  select(-c(`Carimbo de data/hora`,Gênero)) %>% 
  mutate(
    idade = {(Sys.Date() - `Data de nascimento`)/365.25}%>% 
      as.numeric()#%>% 
      #round(digits = 2)
  )

#falta limpar o nome dos cabeçalhos
