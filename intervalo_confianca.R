dados <- c(231,81,318,68,161,175,112,435,283,87,360,143,54,311,170,168,176,202,176,250)

#==============================================================
# construindo intervalo de Confianca para a Media (Mu) 
#==============================================================

My = 44    #mean(dados)   # Media amostral
DPy = 6  #sd(dados)    # Desvio padrao amostral
ny = 12  #length(dados) # Tamanho da amostra 
conf=0.95          # Nivel de confianca
alfa=1-conf;
alfa               # Nivel de significancia 
me_mi=qt((alfa/2),ny-1,lower.tail = FALSE)*(DPy/sqrt(ny));
me_mi              #Margem de erro
IC_mi=cbind(My-me_mi,My+me_mi);
IC_mi              #Intervalo de confianca

#==============================================================
# construindo intervalo de Confianca para a Proporcao (pi) 
#==============================================================

n=400       #tamanho da amostra#
np=240       # numero de sucessos em n#
pe=np/n;
pe     # proporcao amostral
conf=0.99  # nivel de confianca#
alfa=1-conf;
alfa  # nivel de significancia#
me_p = qnorm(1-alfa/2) * (sqrt(pe*(1-pe)) / sqrt(n))
me_p  #calculando a margem de erro#
IC_p=cbind(pe-me_p, pe +me_p);
IC_p



#==============================================================
# calculo do tamanho da amostra para margem de erro na proporcao(PI) #
#==============================================================

pap = 0.6      # proporcao na amostra piloto#
merrod = 0.02   #margem de erro desejada, deve sempre ser em DECIMAL#
alfa=0.05 # nivel de significancia#
n_novo_pi=((qnorm(alfa/2,lower.tail = FALSE)*(sqrt(pap*(1-pap))/merrod)))^2;
n_novo_pi


#==============================================================
# calculo do tamanho da amostra com margem de erro para Mu #
#==============================================================

DPap = 6 #sd(dados)      #Desvio padrao da amostra piloto
nap = 12 #length(dados)   #Tamanho da amostra piloto
merrod= 2             #Margem de erro desejada, mesma unidade da media
alfa=0.05
n_novo_mi=(qt((alfa/2),nap-1,lower.tail = FALSE)*DPap/merrod)^2;
n_novo_mi             #Novo tamanho amostral
