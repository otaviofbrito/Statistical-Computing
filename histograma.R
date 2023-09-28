library(RcmdrMisc)

Dataset <- readXL("Documents/unifal/estatistica/r/rep/data/Dados_exemplo_implanta_ESF.xlsx",
                  rownames=FALSE, header=TRUE,
                  na="", sheet="Plan1", stringsAsFactors=TRUE)

dados <- Dataset$taxa_glicose #VARIAVEL ESTUDADA


tabela <- function(dados,casas_dec){
  A<-max(dados)-min(dados)
  n<-length(dados) 
  if(n<=100){
    k<-ceiling(sqrt(n))
  }else{
    k<-ceiling(5*log10(n))
  }
  C<-round(A/(k-1),casas_dec)
  LI<-c(rep(0,(k+1)))
  media<-c(NA)
  LI[1]<-round(min(dados)-(C)/2,casas_dec)
  for(i in 2:(k+1)){
    LI[i]<-round(LI[i-1]+C,casas_dec)
    media[i-1]<-mean(c(LI[i],LI[i-1]))
  }
  limites<-LI
  TDF<-hist(dados,breaks=limites,plot=FALSE,right=FALSE)
  tabela<-matrix(c(rep(6*k)),k,6)

  for(i in 1:k){
    tabela[i,1]<-round(LI[i],casas_dec)
    tabela[i,2]<-round(LI[i+1],casas_dec)
    tabela[i,3]<-round(media[i],casas_dec)
    tabela[i,4]<-(TDF$counts[i])
    tabela[i,5]<-round(((TDF$counts[i])/n),5)
    tabela[i,6]<-round((100*TDF$counts[i])/n,3)
  }

  colnames(tabela)<-c("LI","LS","X","Fa","Fr","Fp")

  return(tabela)
}

#Histograma

quebras<-function(dados,casas_dec){
  A<-max(dados)-min(dados)
  n<-length(dados) 
  if(n<=100){
    k<-ceiling(sqrt(n))
  }else{
    k<-ceiling(5*log10(n))
  }
  C<-round(A/(k-1),casas_dec)

  LI<-c(rep(0,(k+1)))
  LI[1]<-round(min(dados)-(C)/2,casas_dec)
  for(i in 2:(k+1)){
    LI[i]<-round(LI[i-1]+C,casas_dec)
  }
  return(LI)
}

#CONSTRUINDO TABELA DE DISTRIBUICAO DE FREQUENCIA
tabela(dados,2)
limites<-quebras(dados,3)


#Construindo as informacoes da Tabela de Distribuicao de Frequencias

TDF<-hist(dados,breaks=limites,plot=FALSE,right=FALSE)
print(TDF)

hist(dados,label=FALSE,main="",
     xlab=expression(paste("Concentracaoo (", mu*g/m^3 ,")")),  # ALTERAR DESC E UNIDADE
     ylab="Frequ�ncia absoluta", 
     ylim=c(0,(max(TDF$counts)+1)),
     breaks=limites,axes=FALSE,right=FALSE)
     axis(1,at=limites,pos=c(0,0))
     axis(2,at=c(seq(0:(max(TDF$counts)+1))-1))

