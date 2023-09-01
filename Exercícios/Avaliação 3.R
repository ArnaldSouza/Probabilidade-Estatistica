source('https://git.io/J3zW5')
avaliacao3(2271923)

#exercício 1 - Construa um IC para media considerando um nivel de confianca de de 96%
ex1
length(ex1)

t.test(ex1,conf.level = 0.96)

#exercício 2 - Construa um IC para proporcao de sucessos (p=1 sucesso) considerando um
#nivel de confianca de de 96%
ex2
length(ex2)
prop.test(12,40,conf.level = 0.96, correct =  F)

#exercicio 3 - Teste se a media eh igual ou diferente de 22 considerando um nivel de 
#confianca de 96%
ex3
t.test(ex3,alternative = 'two.side', mu = 22, conf.level = 0.96 )

#exercicio 4 - Teste se a proporcao de sucessos (p=1 sucesso) eh igual ou diferente de 45% 
#considerando um nivel de confianca de de 96%
ex4
prop.test(sum(ex4),n = 49, alternative = 'two.side', p=0.45, conf.level = 0.96) 

#exercicio 5 - Teste se a media da populacao X eh igual ou diferente da de Y considerando 
#um nivel de confianca de de 96%. Suponha duas amostras independentes e variancias 
#populacionais iguais
ex5x
ex5y
t.test(ex5x,ex5y, var.equal = T, conf.level = 0.96, alternative = 'two.side')

#exercicio 6 - Teste se a media da populacao X eh igual ou diferente da de Y considerando
#um nivel de confianca de de 96%. Suponha duas amostras independentes e variancias 
#populacionais diferentes
ex6x
ex6y
t.test(ex6x,ex6y, var.equal = F, conf.level = 0.96, alternative = 'two.side')

#exercicio 7 - Teste se a media de antes eh igual ou diferente da de depois, considerando 
#um nivel de confianca de de 96%. Suponha duas amostras dependentes.
ex7antes
ex7depois
t.test(ex7antes,ex7depois, conf.level = 0.96, alternative = 'two.side', paired = T)
