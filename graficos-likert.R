library(tidyverse) 
library(likert)
library(kableExtra)

db<-read_csv("/home/mateus/Dropbox/TCC_mateus_reis/TCC/PROGRAMAÇÃO/tcc-sucesso-profissional-professor-matem-tica-df/base_tcc.csv")

Graf.likert<-function(lik){
  
  
  lik[ , 1:length(lik)] <- lapply(  lik[ , 1:length(lik)], function(x){ factor(x, 
                                                                                     levels = c('Discordo totalmente.', 
                                                                                                'Discordo parcialmente.', 
                                                                                                'Nem concordo e nem discordo.',
                                                                                                'Concordo parcialmente.', 
                                                                                                'Concordo totalmente.'), 
                                                                                     labels = c('Discordo totalmente.', 
                                                                                                'Discordo parcialmente.', 
                                                                                                'Nem concordo e nem discordo.',
                                                                                                'Concordo parcialmente.', 
                                                                                                'Concordo totalmente.'))})
  lik.1<-likert(as.data.frame(lik))
  
  return(plot(lik.1, wrap = 60, text.size=5) + theme(axis.text.y = element_text(size="10")))
  
}

# Aspectos de competência profissional

Comp.profissional<-db[,c(14:17)]
Graf.likert(Comp.profissional)

# Relações Interpessoais

Rel.Int<-db[,c(18:21)]
Graf.likert(Rel.Int)

# Riscos e iniciativas

Ris.Ini<-db[,c(22:25)]  
Graf.likert(Ris.Ini)
  
# Interação com alunos

Int.Alun<-db[,c(26:30)]  
Graf.likert(Int.Alun)

# Autossatisfação

Autossatisfacao<-db[,c(31:36)]  
Graf.likert(Autossatisfacao)

# Autoaceitação

Autoaceitacao<-db[,c(37:42)]
Graf.likert(Autoaceitacao)

# Estrutura e localização da instituição de trabalho

Local.Trab<-db[,c(43:46)]
Graf.likert(Local.Trab)

# Sucesso profissional

Sucesso.Prof<-db[,48]
Graf.likert(Sucesso.Prof)

#Fazer se orientadores concordarem, porém acredito que usaremos o bibplot para possíveis investigações.

## SEDF

# IFB
