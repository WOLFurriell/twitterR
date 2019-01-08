# Libraries ---------------------------------------------------------------
library(tidytext)
library(dplyr)
library(ggplot2)
library(RColorBrewer)
library(png)
library(grid)

# Import ------------------------------------------------------------------
BD<-'D:/TWITTER/Bancos_tweets'
setwd(BD)
dados2<-read.table(file = "Candidatos.csv",header = T,sep = ",")
dados2$created<-as.character(dados2$created)
dados2$created<-as.Date(dados2$created,format="%Y-%m-%d")

dados2<-dados2%>%mutate(periods = (created<"2014-12-31")+(created<"2015-12-31")+(created<"2016-12-31"))
dados2$periods<-as.factor(dados2$periods)
levels(dados2$periods)<-c("<2017","<2016","<2015")
dados2$periods = factor(dados2$periods,levels(dados2$periods)[3:1])
dados2$text<-as.character(dados2$text)


# Removing words
words<-read.table("D:/TWITTER/STOPWORDS.txt",
                encoding = "UTF-8")

stop<-data.frame('word'=as.character(c(words)))
stop$word<-as.character(stop$word)

words2<-c("aécio","aecioneves","aecio","equipean","neves",
          "dilma","dilmabr","rousseff","eduardo","jorge","eduardojorge",
          "everaldo","levy","fidelix","levyfidelix","luciana",
          "genro","equipe","silva_marina","silva_marina","marina",
          "silva","silvamarina","aécio","aecioneves","aecio","equipean",
          "neves", "dilma","dilmabr","rousseff",'http',
          "eduardo","jorge","eduardojorge", "everaldo",
          "levy","fidelix","levyfidelix","luciana","genro","equipe",
          "silva_marina","silva_marina","marina","silva","silvamarina",
          "aécio45","é","rt","t.co","já","você","equipe50","https","à","p","já","q","u",
          "eduardojorge43","equipe40","soumarina40",'c','está','pra',"são",'sobre','everaldo_20')  

# Images
readPNG2<-function(x) {rasterGrob(readPNG(x), interpolate=TRUE)}
Dilma <- rasterGrob(Dilma, interpolate=TRUE)
Aecio  <- rasterGrob(Aecio , interpolate=TRUE)
Marina <- rasterGrob(Marina, interpolate=TRUE)
Everaldo <- rasterGrob(Everaldo , interpolate=TRUE)
Eduardo <- rasterGrob(Eduardo, interpolate=TRUE)
Levy  <- rasterGrob(Levy , interpolate=TRUE)
Luciana  <- rasterGrob(Luciana , interpolate=TRUE)

# Tidy --------------------------------------------------------------------

data <- dados2 %>% unnest_tokens(word, text)
data<- data%>%anti_join(stop,by="word")%>%filter(!(word %in% words2))

data10<-data %>% 
  group_by(nome)%>%
  count(word, sort = TRUE)%>%
  bind_tf_idf(word, nome, n) %>%
  arrange(tf_idf)%>%
  group_by(nome)%>%
  mutate(rank=rank(desc(n)) )%>%filter(rank  <=10)

data10

data10$nome <- as.factor(data10$nome)
data10$nome <- factor(data10$nome, levels = c("Dilma", "Aécio", "Marina Silva",
                                              "Luciana Genro", "Everaldo", "Eduardo Jorge","Levy"))
#data10%>%ggplot(aes(x=word,y=n))+geom_bar(stat="identity")+facet_grid(nome~periods)

data10[which(data10$nome=="Marina Silva" & data10$word=="agora"),"rank"]<-8
data10[which(data10$nome=="Marina Silva" & data10$word=="hoje"),"rank"]<-9

Dilma2<-rgb(red = 204, green = 32,blue =  44,maxColorValue = 255)
Aecio2<-rgb(0, 89, 171,maxColorValue = 255)
Marina2<-rgb(255,0,0,maxColorValue = 255)
Eduardo2<-rgb(36,150,56,maxColorValue = 255)
Everaldo2<-rgb(0,146,63,maxColorValue = 255)
Levy2<-rgb(21,74,148,maxColorValue = 255)
Luciana2<-rgb(253,252,1,maxColorValue = 255)


#p1<-data10%>%filter(nome=="Aécio"|nome=="Dilma")%>%ggplot(aes(x=nome,y=rank))
p1<-data10%>%ggplot(aes(x=nome,y= rank ))
p1<-p1+geom_label(aes(label=word,fill=nome), color='black', size=5)
#p1<-p1+ facet_grid(~periods)
p1<-p1+ theme_minimal()
p1<-p1+ labs(x="",y="",title="Most characteristic Words per presidential candidate")
p1<-p1+ theme(axis.text.x=element_blank(),axis.text.y=element_text(size=20))
p1<-p1+ scale_y_continuous(limits=c(1,14), breaks=c(1,6,10),
                           labels=c("#1","#5","#10"))
p1<-p1+ scale_fill_manual(values = c(Dilma2,Aecio2,Marina2,Luciana2,Everaldo2,Eduardo2,Levy2))
p1<-p1+ theme(legend.position="none")
p1<-p1+ theme(plot.title = element_text(size=22))
#Candidatos
p1<-p1+ annotation_custom(Dilma,    xmin=0.5, xmax=1.5, ymin=11, ymax=14)
p1<-p1+ annotation_custom(Aecio,    xmin=1.5, xmax=2.5, ymin=11, ymax=14)
p1<-p1+ annotation_custom(Marina,   xmin=2.5, xmax=3.5, ymin=11, ymax=14)
p1<-p1+ annotation_custom(Luciana,  xmin=3.5, xmax=4.5, ymin=11, ymax=14)
p1<-p1+ annotation_custom(Everaldo, xmin=4.5, xmax=5.5, ymin=11, ymax=14)
p1<-p1+ annotation_custom(Eduardo,  xmin=5.5, xmax=6.5, ymin=11, ymax=14)
p1<-p1+ annotation_custom(Levy,     xmin=6.5, xmax=7.5, ymin=11, ymax=14)
x11()
p1
ggsave("teste.png",plot = p1,width = 12,height = 6)
getwd()
