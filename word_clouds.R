library(tm)
library(stringi)
library(dplyr)
library(RWeka)
library(lubridate)
library(wordcloud)
library(tidyverse) 
library(ggplot2)
library(reshape2)
  

options(mc.cores=1)

db<-read_csv("/home/mateus/Dropbox/TCC_mateus_reis/TCC/PROGRAMAÇÃO/tcc-sucesso-profissional-professor-matem-tica-df/base_tcc.csv")
  
#Função que gera um dataframe com a frequência das palavras

freq.palavras=function(vetor){
  
  #coloca tudo num só vetor:
  vetor=str_flatten(na.omit(vetor),collapse = " ")
  
  vetor=vetor %>% 
    #gsub("[^A-Za-z0-9]", " ", .) %>%  
 
    removeWords(., stopwords("portuguese"))   %>% 
    stri_trans_general(., "Latin-ASCII") %>% 
    iconv(., "UTF-8", "ASCII//TRANSLIT") %>% 
    removePunctuation() %>% 
    removeNumbers() %>%
    tolower() %>% 
    gsub("\n", " ", ., fixed = T) %>% 
    gsub(" ", " ", .) 
  #transforma numa lista detalhada: revisar todo esse código
  vetor<-Corpus(VectorSource(vetor)) #talvez seja melhor usar outro código. String split e fazer um table
  dtm <- TermDocumentMatrix(vetor) 
  
  matrix <- as.matrix(dtm) 
  palavras <- sort(rowSums(matrix),decreasing=TRUE) 
  df <- data.frame(Palavra = names(palavras),freq=palavras)
  
  return(df)
}


funcao_nuvem2<-function(df){
  
  wordcloud(words = df$Palavra, freq = df$freq, min.freq = 1,max.words=200, random.order=FALSE, rot.per=0.35,colors=brewer.pal(8, "Dark2"))
  
}


# Resultados: RESTA SALVAR TABELAS E WORDCLOUDS

## Me considero ou não me considero um professor de sucesso porque

set.seed(123)
funcao_nuvem2(freq.palavras(db$`Me considero ou não me considero um professor de sucesso porque`))

Mecons.fre<-freq.palavras(db$`Me considero ou não me considero um professor de sucesso porque`)


##Quais fatores você considera importante para ser bem sucedido
set.seed(123)
funcao_nuvem2(freq.palavras(db$`Quais fatores você considera importante para ser bem sucedido`))

Quais_fat.freq<-freq.palavras(db$`Quais fatores você considera importante para ser bem sucedido`)


## Comentários eou sugestões NÃO FUNCIONOU 
set.seed(123)
funcao_nuvem2(freq.palavras(db$`Comentários eou sugestões`))

Coment.freq<-freq.palavras(db$`Comentários eou sugestões`)


#Vou salvar a frequência das palavras caso seja necessário de acordo com as orientações após fechar o questionário

## ------ Função para gerar ngrans

Ngrans<-function(vetor,mini,maxi) {
#coloca tudo num só vetor:
vetor=str_flatten(na.omit(vetor),collapse = " ")

vetor=vetor %>% 
  #gsub("[^A-Za-z0-9]", " ", .) %>%  
  
  removeWords(., c(stopwords("portuguese"),"professor","professora","sucesso","porque","me","se","considero","e"))   %>% 
  stri_trans_general(., "Latin-ASCII") %>% 
  iconv(., "UTF-8", "ASCII//TRANSLIT") %>% 
  removePunctuation() %>% 
  removeNumbers() %>%
  tolower() %>% 
  gsub("\n", " ", ., fixed = T) %>% 
  gsub(" ", " ", .) 
  
  
  corpus.ng=VCorpus(VectorSource(vetor)) #Necessário para fazer a tokenização
  
  BigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = mini, max = maxi))
  tdm.bigram = TermDocumentMatrix(corpus.ng,control = list(tokenize = BigramTokenizer))
  
  freq = sort(rowSums(as.matrix(tdm.bigram)),decreasing = TRUE)
  freq.df = data.frame(Palvras=names(freq), freq=freq)
  return(freq.df)
  
  
  
  return(freq.df)
}

funcao_nuvem3<-function(freq.df){
  
  wordcloud(freq.df$Palvras,freq.df$freq, min.freq = 1,scale = c(2, 0.1),max.words=100, random.order=FALSE, rot.per=0.35,colors=brewer.pal(8, "Dark2"))
}    

funcao_nuvem3(Ngrans(db$`Me considero ou não me considero um professor de sucesso porque`,2,2))

## Me considero ou não me considero um professor de sucesso porque

set.seed(123)
funcao_nuvem3(Ngrans(db$`Me considero ou não me considero um professor de sucesso porque`,2,2))

Mecons.fre.2<-Ngrans(db$`Me considero ou não me considero um professor de sucesso porque`,2,2)


##Quais fatores você considera importante para ser bem sucedido
set.seed(123)
funcao_nuvem3(Ngrans(db$`Quais fatores você considera importante para ser bem sucedido`,2,2))

Quais_fat.freq.2<-Ngrans(db$`Quais fatores você considera importante para ser bem sucedido`,2,2)


## Comentários eou sugestões NÃO FUNCIONOU 
set.seed(123)
funcao_nuvem3(Ngrans(db$`Comentários eou sugestões`,2,2))

Coment.freq.2<-Ngrans(db$`Comentários eou sugestões`,2,2)


