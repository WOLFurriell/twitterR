# Bibliotecas -------------------------------------------------------------
#library("twitteR")
#library("wordcloud")
#library(twitteR)
library(tm)
library(dplyr)
library(ggplot2)
library(lazyeval)

# Importação --------------------------------------------------------------
BD<-'D:/TWITTER/Bancos_tweets'
setwd(BD)

words<-as.character(unlist(read.table("D:/TWITTER/STOPWORDS.txt", fileEncoding="latin1")))

dados2<-read.table(file = "Candidatos.csv",header = T,sep = ",")
dados2$created<-as.character(dados2$created)
dados2$created<-substr(dados2$created,1,10)
dados2$created<-as.Date(dados2$created,format="%Y-%m-%d")

dados2<-dados2%>%mutate(periods = (created<"2014-12-31")+(created<"2015-12-31")+(created<"2016-12-31"))
dados2$periods<-as.factor(dados2$periods)
levels(dados2$periods)<-c("<2017","<2016","<2015")
dados2$periods = factor(dados2$periods,levels(dados2$periods)[3:1])

dados3<-dados2%>%group_by(periods,nome)%>%summarise(freq=n())

# Gráficos ----------------------------------------------------------------
p1<-dados2%>%ggplot(aes(nome,retweetCount))+geom_boxplot()+coord_cartesian(ylim=c(0,100))
p2<-dados2%>%ggplot(aes(nome,retweetCount))+geom_boxplot()+coord_flip()
p2<-p2 +facet_grid(~periods,scales="free",space="free")
p2

p3<-dados3%>%ggplot(aes(nome,freq))+geom_bar(stat="identity")+facet_grid(~periods)+coord_flip()
p3<-p3 + labs(y="Frequência de tweets",x="")
p3

p3<-dados3%>%ggplot(aes(col=nome,freq,x=periods, group=nome))+geom_point(size=3)+geom_line()
p3<-p3 + labs(y="Frequência de tweets",x="",fill="Candidato") 
p3

library(gridExtra)
grid.arrange(p2,p1)
