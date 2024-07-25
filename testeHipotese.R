############################################################
# fazendo o teste MANUALMENTE para MEDIA (Mi) populacional 
#############################################################

#===========================================
#  AFIRMACAO
#==========================================
#testando de Mu=5, teste BILATERAL#

dpa= 10    # desvio padrao da AMOSTRA#
mediaa= 134     #media da AMOSTRA#
n=40              #tamanho da AMOSTRA#

valorpar=130       # Valor testado Mu0#

tcalc=((mediaa-valorpar)/(dpa/(n^0.5)));
tcalc #  quantil T calculado#

#pvalor do teste, ESCOLHA PELA HIPOTESE H1

# A) Teste Bilateral
p_valor_t=2*pt(abs(tcalc),df=n-1, lower.tail = FALSE);p_valor_t

# B) Teste unilateral a esquerda 
p_valor_t=pt(tcalc,df=n-1, lower.tail =TRUE);p_valor_t

# C) Teste unilateral a direita
p_valor_t=pt(tcalc,df=n-1, lower.tail = FALSE);p_valor_t

#==========================================================
# USANDO  A Funcao t.test() para MEDIA populacional
#  Realiza o teste t-Student para uma ou duas amostras.
#==========================================================
amostra=c(6.0,7.3,6.8,7,6.9,7.3,7.2,7,6.7,6.6,7.8,7,6.3,7.3,7.1)
t.test(amostra,mu=7.3,alternative="less")

# usar alternative="greater", quando H1 tiver sinal de maior (>)
# usar alternative="less", quando H1 tiver sinal de menor (<) 
# usar alternative="two.side", quando H1 tiver sinal de diferente (!=)

################################################################################
#               Teste a PROPORCAO (PI) de uma populacao
#                Fazendo o teste MANUALMENTE 
################################################################################


n=50
ns=42
p0<-0.95 # Valor a ser testado Pi0#

pe=ns/n;pe # propor��o amostral
zc = (pe-p0)/sqrt((pe*(1-pe))/n);zc

#pvalor do teste, ESCOLHA PELA HIPOTESE H1

# A) Teste Bilateral
p_valor_z=2*pnorm(abs(zc), lower.tail = FALSE);p_valor_z

# B) Teste unilateral � esquerda
p_valor_z=pnorm((zc), lower.tail = TRUE);p_valor_z

# C) Teste unilateral � direita
p_valor_z=pnorm((zc), lower.tail = FALSE);p_valor_z


#==========================================================
#               USANDO A FUNCAO binom.test
#              Teste EXATO para PROPORCAO (PI)                            
#==========================================================
# entrada com numero de sucessos (ns) e tamanho da amostra (n)

n=80
ns=68
p0<-0.92 # Valor a ser testado (assumido) para Pi#


binom.test(ns, n, p = p0,alternative="less")

# usar alternative="greater", quando H1 tiver sinal de maior (>)
# usar alternative="less", quando H1 tiver sinal de menor (<) 
# usar alternative="two.side", quando H1 tiver sinal de diferente (!=)

#=====================================================================

