library(RcmdrMisc)

Dataset <- readXL("Documents/unifal/estatistica/r/rep/Dados_Aula_Pratica_Amostragem.xlsx",
                  rownames=FALSE, header=TRUE,
                  na="", sheet="Plan1", stringsAsFactors=TRUE)

aprendeu <- factor(Dataset$Aprendeu)
idade <- Dataset$Idade
id <- Dataset$ID
atuacao <- Dataset$Atuacao

summary(Dataset)

n <- 100 #Tamanho amostra
N <- length(aprendeu) #Tamanho populacao

#=============AMOSTRA CASUAL SIMPLES
amostraCS <- sample(idade, n)
mean(amostraCS) #MEDIA DA AMOSTRA CASUAL SIMPLES

#============AMOSTRA SISTEMATICA
jump <- round(N/n)
first_element <- sample(1:jump, 1)
amostraSist <- c()
index <- first_element 
for(i in 1:n){
  amostraSist[i] <- idade[index]
  index <- index + jump
}
mean(amostraSist) #MEDIA DA AMOSTRA SISTEMATICA

#===========AMOSTRA ESTRATIFICADA

subsetA <- subset(Dataset, Atuacao == 'A') #ESTRATO A
subsetP <- subset(Dataset, Atuacao == 'P') #ESTRATO P
subsetO <- subset(Dataset, Atuacao == 'O') #ESTRATO O

#Amostragem proporcional

estratos <- summary(atuacao)
amostraProporcional <- round((estratos/N)*n)

amostraA <- sample(subsetA$Idade, amostraProporcional[1])
amostraO <- sample(subsetO$Idade, amostraProporcional[2])
amostraP <- sample(subsetP$Idade, amostraProporcional[3])


amostraEstratificada <- c(amostraA, amostraP, amostraO)

mean(amostraEstratificada) #MEDIA DA AMOSTRA ESTRATIFICADA





                  