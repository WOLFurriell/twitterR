# Função ------------------------------------------------------------------
get_info<-function(dados,conta){
  conta<-unlist(strsplit(conta, split=""))
  j<-k<-1
  day<-month<-year<-rt<-c()
  for(i in 1:length(dados)){
    teste1<-  unlist(strsplit(dados[i], split="")) 
    for(l in 1:length(teste1)){
      if((teste1[l]==conta[1])&(teste1[(l+1)]==conta[2])&(teste1[(l+2)]==conta[3])&(teste1[l+3]==conta[4])){
        n<-length(conta)
        N<-length(teste1)
        day[j]<- as.character(paste(teste1[(l+n+2):(l+n+3)],collapse = "")); ifelse(as.numeric(day[j])<10,init<-l+n+7,init<-l+n+8)
        month[j]<-  as.character(paste(teste1[init:(init+2)] ,collapse = "")) 
        year[j]<-  as.character(paste(teste1[(init+2+4) :(init+2+4+4)] ,collapse = "")) 
        j<-j+1    
      }   
    }  
    if(is.na(as.numeric(substr(dados[i],1,1)))==F){
      teste<-  unlist(strsplit(dados[i], split=""))
      for(u in 1:length(teste)){
        if( (teste[u]=="r") & (teste[u+1]=="e") & (teste[u+2]=="t") & (teste[u+3]=="w") ){
          rt[k]<-as.character(paste(teste[1:(u-1)],collapse = ""))
          k<-k+1
        }  
      }    
    }  
  }
resp<-data.frame(day,month,year,rt)
resp$rt<-as.numeric(gsub(pattern = "\\.",replacement = "",x = resp$rt))
return(resp)
}


# Luciana - .csv -------------------------------------------------------------------
setwd("C:\\Users\\Vinícius\\Google Drive\\Projetos\\Eventos\\Semana da Estatística - (SdEUEM)\\Sentimental Analysis - (SdEUEM2016)\\BD - Sentimental Analyses")


twitter_luciana<-read.table("luciana.csv",sep = ";", quote=NULL)
twitter_luciana$V1<-as.character(twitter_luciana$V1)


conta<-'?@lucianagenro'

dados_luciana<-get_info(dados = twitter_luciana$V1,conta = conta);head(dados_luciana,30)

# Dilma - .csv -------------------------------------------------------------------
twitter_dilma<-read.table("dilma.csv",sep = ";",quote = NULL)
twitter_dilma$V1<-as.character(twitter_dilma$V1)


conta<-'?@dilmabr'
dados_dilma<-get_info(dados = twitter_dilma$V1,conta = conta);head(dados_dilma,30)

  