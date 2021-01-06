library(tm)
library(stringi)
library(dplyr)
library(RWeka)
library(lubridate)
library(wordcloud)


#Função que gera um dataframe com a frequência das palavras

freq.palavras=function(vetor){
  
  #coloca tudo num só vetor:
  vetor=str_flatten(vetor,collapse = " ")
  
  vetor=vetor %>% 
    #gsub("[^A-Za-z0-9]", " ", .) %>%   
    stri_trans_general(., "Latin-ASCII") %>% 
    iconv(., "UTF-8", "ASCII//TRANSLIT") %>% 
    removePunctuation() %>% 
    removeNumbers() %>%
    tolower() %>% 
    gsub("\n", " ", ., fixed = T) %>% 
    gsub(" ", " ", .) %>% 
    removeWords(., stopwords("portuguese"))
  
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

funcao_nuvem2(freq.palavras(db$`Me considero ou não me considero um professor de sucesso porque`))

freq.palavras(db$`Me considero ou não me considero um professor de sucesso porque`)


##Quais fatores você considera importante para ser bem sucedido

funcao_nuvem2(freq.palavras(db$`Quais fatores você considera importante para ser bem sucedido`))

freq.palavras(db$`Quais fatores você considera importante para ser bem sucedido`)


## Comentários eou sugestões NÃO FUNCIONOU - VERIFICAR O OCORRIDO

funcao_nuvem2(freq.palavras(db$`Comentários eou sugestões`))

freq.palavras(db$`Comentários eou sugestões`)









  