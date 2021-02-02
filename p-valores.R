
db<-read_csv("/home/mateus/Dropbox/TCC_mateus_reis/TCC/PROGRAMAÇÃO/tcc-sucesso-profissional-professor-matem-tica-df/base_tcc.csv")

names(db)
View(db)
colunas <- c(3:6, 8:13, 16:48, 50)
tmp <- db[, colunas] %>% 
  as.data.frame()


# rotina

nomes2a2 <- tmp %>% 
  names() %>% 
  combn(2) %>% 
  t()
head(nomes2a2)

pvalues <- numeric(nrow(nomes2a2))
for (i in 1:nrow(nomes2a2)) {
  x <- tmp[, nomes2a2[i, ]]
  y <- chisq.test(x = x[,1], y = x[,2], simulate.p.value = F)$p.value
  pvalues[i] <- y
}
nomes2a2 <- as_tibble(nomes2a2)
nomes2a2$pvalue <- pvalues

# nomes2a2 %>% 
#   arrange(V1, desc(pvalue)) %>% 
#   as.data.frame() %>% 
#   xlsx::write.xlsx2('pvalues-questionário.xlsx', row.names = F)



df<-nomes2a2[order(nomes2a2$pvalue),]

df %>%  
as.data.frame() %>%
xlsx::write.xlsx2('pvalues-questionário.xlsx', row.names = F)
