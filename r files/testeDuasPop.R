#==================================================================
# var.test()  Realiza o teste F para razao de variancias
#==================================================================

##Pode-se afirmar que A e B possuem variancias iguais? 

#H0: sig2A/sig2B =  1
#H1: sig2A/sig2B != 1

amostraA = c(120, 119, 89, 103, 122, 108, 105, 102, 122, 132)
amostraB = c(115, 110, 90, 110, 105, 113, 98, 104, 125, 111)
var(amostraA)  #calculando a variancia amostral
var(amostraB) 

var.test (amostraA, amostraB,alternative="two.side")

## Ficar atento com H1.
# usar alternative="two.side", quando H1 tiver sinal de diferente (!=)
# usar alternative="greater", quando H1 tiver sinal de maior (>)
# usar alternative="less", quando H1 tiver sinal de menor (<) 

###################################################################
#==================================================================
#              TESTE T PARA COMPARAR DUAS MEDIAS (Mu1 - Mu2)
#==================================================================        
# t.test()  Realiza o teste t-Student para  duas amostras INDEPENDENTES
##Teste t para comparacao de duas medias com variancias iguais
######################################################################

#  H0 : MuA - MuB = 0
#  H1 : MuA - MuB =\ 0

amostraA = c(3, 5, 7, 9, 8, 9, 7, 4, 9, 9)
amostraB = c(8, 5, 9, 3, 2, 6, 4, 5, 2, 5)

# como variancias sao iguais usar "var.equal = TRUE" ##

t.test(amostraA, amostraB, var.equal = TRUE, alternative="two.side")


#OBS: Quando as variancias das duas populacoes forem consideradas diferentes, 
#deve-se usar: var.equal = FALSE, como argumento na funcao t.test.

# t.test(amostraA, amostraB, var.equal = FALSE, alternative="two.side")

###################################################################
# t.test()  Realiza o teste t-Student para  duas amostras DEPENDENTES
#### Teste t EMPARELHADO (quando as observações são no mesmo elemento amostral)
######################################################################
# H0 : µDif =  0
# H1 : µDif =! 0

a = c(170, 154, 162, 188, 155, 145, 167, 154, 177, 148, 160, 150)
b = c(165, 152, 164, 167, 154, 142, 162, 152, 165, 151, 153, 152)

t.test(a,b,paired=TRUE,alternative="less" )  #paired=TRUE, indica que as amostras são dependentes.

# Ficar atento opções: alternative="greater", "less", "two.side"
#
#==================================================================
#  Teste para compara dua proporções (Pi1-Pi2)
#==================================================================
n1=160
ns1=150 # número de sucesso na amostra da pop_1

n2=140
ns2=118 # número de sucesso na amostra da pop_2

#Ha:Pip < Pim

prop.test(c(ns2,ns1),c(n2,n1),alternative="less")

#Ha:Pim > Pip
prop.test(c(ns1,ns2),c(n1,n2),alternative="greater")
## Ficar atento com H1.
# usar alternative="two.side", quando H1 tiver sinal de diferente (!=)
# usar alternative="greater", quando H1 tiver sinal de maior (>)
# usar alternative="less", quando H1 tiver sinal de menor (<) 