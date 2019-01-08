library(twitteR)
library(tm)
library(wordcloud)
library(RColorBrewer)
library(tm)
library(dplyr)
library(data.table)
library(WriteXLS)

rm(list = ls())
setwd("D:/TWITTER/Bancos_tweets")
#-----------------------------
#colocando as chaves

api_key             <- "xxxxxxxxxxxxxxxxxxxxxxxxxxx"
api_secret          <- "xxxxxxxxxxxxxxxxxxxxxxxxxxx"
access_token        <- "xxxxxxxxxxxxxxxxxxxxxxxxxxx"
access_token_secret <- "xxxxxxxxxxxxxxxxxxxxxxxxxxx"
setup_twitter_oauth(api_key,api_secret,access_token,access_token_secret)

#Retirar as primeiras 3200 com  retweets e replies

##----------------- MARINA
twsilva_marina<-userTimeline('silva_marina',n=3200,includeRts=T,excludeReplies=F)

silva_marina_df<-twListToDF(twsilva_marina)
silva_marina_df2<-sapply(twsilva_marina, function(x) x$getText())

saveRDS(silva_marina_df, file="twsilva_marina.Rds")
saveRDS(silva_marina_df2, file="twsilva_marina2.Rds")

##----------------- DILMA
twdilma<-userTimeline('dilmabr',n=3200,includeRts=T,excludeReplies=F)
dilma_df<-twListToDF(twdilma)
dilma_df2<-sapply(twdilma, function(x) x$getText())

saveRDS(dilma_df, file="twdilma.Rds")
saveRDS(dilma_df2, file="twdilma2.Rds")

##----------------- AECIO
twaecio<-userTimeline('AecioNeves',n=3200,includeRts=T,excludeReplies=F)
aecio_df<-twListToDF(twaecio)
aecio_df2<-sapply(twaecio, function(x) x$getText())

saveRDS(aecio_df, file="twaecio.Rds")
saveRDS(aecio_df2, file="twaecio2.Rds")

#retirar por datas específicas
#podemos verificar a data do último tweet exportado por userTimeline e pegar o restante por aqui
#twsilva_marina2<- searchTwitter('from:silva_marinaNeves', n =1500,lang = 'pt',
#                         since ='2014-07-06',until = '2014-10-11')

## extrair o texto de todos os tweets
#write.table(silva_marina_df,'D:/TWITTER/Bancos_tweets/twsilva_marina4.txt')

#remover emojis, é possível usar 'ASCII'