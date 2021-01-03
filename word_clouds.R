library(tm)
library(stringi)
library(dplyr)
library(RWeka)
library(lubridate)
library(wordcloud)

funcao_nuvem = function(a){
    a %>% 
    gsub("[^A-Za-z0-9]", " ", .) %>%   
    stri_trans_general(., "Latin-ASCII") %>% 
    iconv(., "UTF-8", "ASCII//TRANSLIT") %>% 
    removePunctuation() %>% 
    removeNumbers() %>%
    tolower() %>% 
    gsub("\n", "", ., fixed = T) %>% 
    gsub(" ", " ", .) %>% 
    removeWords(., stopwords("portuguese")) %>% 
    gsub("\\s+", " ", .) %>% 
    paste(., collapse = " ") %>% 
    wordcloud()
  
}

#Dá pra melhorar essa worclous:   https://towardsdatascience.com/create-a-word-cloud-with-r-bde3e7422e8a ;  http://www.sthda.com/english/wiki/text-mining-and-word-cloud-fundamentals-in-r-5-simple-steps-you-should-know

funcao_nuvem(db$`Me considero, ou não me considero, um professor de sucesso porque...`)

funcao_nuvem(db$`Quais fatores você considera importante para ser bem sucedido?`)

funcao_nuvem(db$`Comentários e/ou sugestões.`)
