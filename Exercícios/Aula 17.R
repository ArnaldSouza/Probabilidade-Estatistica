##### Uma amostra : proporção#####
#Exercício 1: Suponha que um experimento testando a resistência de pavers feitos com materias 
#recicláveis. EM uma amostra de tamanho 15, teste se a média é igual a 21. 
set.seed(10);x=rnorm(15,22,3) #simular dados
x
mean(x)

t.test(x,alternative = 'two.side', mu = 21, conf.level = 0.95 ) #diferente
t.test(x,alternative = 'greater', mu = 21, conf.level = 0.95 ) #maior
t.test(x,alternative = 'less', mu = 21, conf.level = 0.95 ) #menor
#R:como p-value = 0.3114, não podemos concluir a nivel de 95% que a verdadeira média é 21


#Exercício 2: Suponha que em um experimento, 20 amostras de um novo tipo de drenagem foram testadas. Se o 
#sistema funcionasse recebia o valor 1, caso contrario, 0. Teste, ao nével de 5% de significancia
#se a verdadeira proporção do sistema de drenagem funciona é maior que 25%.
set.seed(10);p=rbinom(20,1,0.5)
p
sum 
sum(p)
sum(p)/20

prop.test(sum(p),n = 20, alternative = 'greater', p=0.25, conf.level = 0.95) #maior
prop.test(sum(p),n = 20, alternative = 'two.side', p=0.25, conf.level = 0.95) #diferente
prop.test(sum(p),n = 20, alternative = 'less', p=0.25, conf.level = 0.95) #menor
#como: p-value = 0.2193, não reijeitamos H0 ao nivel de 95% de confiança, logo não podemos
#afirmar que a verdadeira proporção de drenagem que gunciona é maior que 25%

##### Duas amostras independentes, media  #####
#Exercício : Suponha que estamos comparando a resistência de dois produtos, A e B. Foram 
#testados 20 vezes o produto A e 15 o B, sendo que as forças nos pontos medios de falha foram 
#anotadas. Teste se ambos possuem, em média, a mesma resistencia, considerando um nivel de 
#confiança de 95% e que as variâncias populacionais são iguais.
set.seed(10);a=rnorm(20,22,3) #simular dados
set.seed(10);b=rnorm(15,19,3) #simular dados

a
b

mean(a)
mean(b)

#variancias iguais
#muA - muB> < !=
t.test(a,b, var.equal = T, conf.level = 0.95, alternative = 'two.side') #diferentes
t.test(a,b, var.equal = T, conf.level = 0.95, alternative = 'greater') #maior
t.test(a,b, var.equal = T, conf.level = 0.95, alternative = 'less') #menor

#R: como p-value<5¢ rejeitamos H0 ao nível de 95% de confiança, logo podemos afirmar que a 
#resistência média de A é diferente de B.


#variancias diferentes
#muA-muB
t.test(a,b, var.equal = F, conf.level = 0.95, alternative = 'two.side') #diferentes
t.test(a,b, var.equal = F, conf.level = 0.95, alternative = 'greater') #maior
t.test(a,b, var.equal = F, conf.level = 0.95, alternative = 'less') #menor




##### Duas amostras dependentes #####
#Exercicio : Suponha que estamos comprarando a resistencia de produtos, antes e após uma 
#modificação (serão testados os mesmos produtos antes e após). Foram testados 20 vezes A(antes) e
#B(após), sendo que as forças no ponto de falha foram anotadas. Teste se a modificação teve efeito,
#considerando um nivel de confiança de 95%
set.seed(10);a=rnorm(20,22,3) #simular dados
set.seed(10);b=rnorm(20,19,4) #simular dados

#diferença=a-b  0
t.test(a,b, conf.level = 0.95, alternative = 'two.side', paired = T)
t.test(a,b, conf.level = 0.95, alternative = 'greater', paired = T)
t.test(a,b, conf.level = 0.95, alternative = 'less', paired = T)
#R: como p-value <5%, reheitamos H0 a nível de 95% de confiança, logo afirmamos que a modificação
#teve efeito