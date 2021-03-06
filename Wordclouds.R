library(tm)
library(wordcloud)
library(RColorBrewer)
library(dplyr)
library(data.table)

rm(list = ls())

BD<-'D:/TWITTER/Bancos_tweets'
setwd(BD)

candidatos<-read.table(file = "Candidatos.csv",header = T,sep = ",")
levels(candidatos$nome)
all_tw<-split(candidatos,candidatos$nome) 

#Atribuir o banco geral como �ltimo elemento da lista
all_tw[[8]]<-candidatos
names(all_tw)[8]<-"candidatos"

plotword<-list()

ordem<-c(2,1,7,6,4,3,5,8)
nomescand<-c("A�cio Neves","Dilma","Eduardo Jorge","Pastor Everaldo",
             "Levy Fidelix","Luciana Genro","Marina Silva",
             "Geral")

pdf("D:/TWITTER/graphs/wordcloud2.pdf",height = 6, width = 14)
par(mfrow=c(2,4))
for(i in ordem){
  # criando um corpus
  adapt <- Corpus(VectorSource(all_tw[[i]]))
  #removendo espa�os em branco, formatando as letras em min�sculas e removendo a pontua��o
  adapt <- tm_map(adapt, stripWhitespace)
  adapt <- tm_map(adapt, content_transformer(tolower))
  adapt <- tm_map(adapt, removeNumbers)
  adapt <- tm_map(adapt, removePunctuation)
  adapt <- tm_map(adapt, stripWhitespace)
  adapt <- tm_map(adapt, removeWords, stopwords("portuguese"))
  
  aux<-read.table("D:/TWITTER/STOPWORDS.txt",
                  encoding = "Latin-1")
  words1<-as.vector(aux$V1)
  
  if (i == 1){
    words2<-c("a�cio","aecioneves","aecio","equipean","neves","https",
              "�","p","j�","q","u","a�cio45","�","rt","t.co")
  }else if (i == 2){
    words2<-c("dilma","dilmabr","rousseff","https","�","rt","t.co",
              "querodilmatreze")
  }else if (i == 3){
    words2<-c("eduardo","jorge","eduardojorge","eduardojorge43","https",
              "�","p","j�","q","u","�","rt","t.co","httptcovyevurkvew")
  }else if (i == 4){
  words2<-c("everaldo","https","�","p","j�","q","u","�","rt","t.co")
  }else if (i == 5){
  words2<-c("levy","fidelix","levyfidelix","https","�","p","j�","q","u",
            "�","rt","t.co")
  }else if (i == 6){
    words2<-c("luciana","genro","equipe","https","�","p","j�","q","u",
              "�","rt","t.co","soumarina")
  }else if (i == 7){
    words2<-c("silva_marina","silva_marina","marina","silva","silvamarina",
              "soumarina40","�","p","j�","q","u","�","rt","t.co")
  }else 
    words2<-c("a�cio","aecioneves","aecio","equipean","neves",
              "dilma","dilmabr","rousseff","eduardo","jorge","eduardojorge",
              "everaldo","levy","fidelix","levyfidelix","luciana",
              "genro","equipe","silva_marina","silva_marina","marina",
              "silva","silvamarina","a�cio","aecioneves","aecio","equipean",
              "neves", "dilma","dilmabr","rousseff",'http',
              "eduardo","jorge","eduardojorge", "everaldo",
              "levy","fidelix","levyfidelix","luciana","genro","equipe",
              "silva_marina","silva_marina","marina","silva","silvamarina",
              "a�cio45","�","rt","t.co","j�","voc�","equipe50","https","�","p","j�","q","u",
              "eduardojorge43","equipe40","soumarina40",'c','est�','pra',"s�o",'sobre','everaldo_20',
            "httptcovyevurkvew","http...")  
  
  words<-c(words1,words2)
  words<-sort(words)
  
  adapt <- tm_map(adapt, removeWords,words) 
  
  dtm <- TermDocumentMatrix(adapt)
  m <- as.matrix(dtm)
  v <- sort(rowSums(m),decreasing=TRUE)
  d <- data.frame(word = names(v),freq=v)
  
  set.seed(1234)
  wordcloud(words = d$word, freq = d$freq, min.freq = 2,scale=c(5, .2),
            max.words=400, random.order=F,rot.per=0.30,
            colors=c("darkslategray3","lightslateblue","dodgerblue",
                     "mediumpurple4","darkmagenta",
                     "mediumpurple3","midnightblue"))
  mtext(nomescand[[i]], side = 2,line = 2)
  }
dev.off()

