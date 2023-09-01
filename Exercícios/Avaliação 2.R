source('https://git.io/JYHHh')
avaliacao2(2271923)

##### exercício 1#####
#Dado que 56 % dos produtos de uma empresa nao apresentam
#falhas apos a producao, em uma amostra de 12 componentes, determine:
#a)	A probabilidade de que nenhum falhe
dbinom(x = 0, size=12, prob = 0.56)
dbinom(12,12,0.44)
#b) A probabilidade de que no minimo 1 falhe
1-dbinom(x = 0, size=12, prob = 0.56)
sum(dbinom(1:12,12,0.56))
#c) A probabilidade de que ao menos 3 falhem
sum(dbinom(0:3,12,0.56))
pbinom(3,12,0.56)
#d) Se a amostra for de 50 componentes, quantos irao falhar em media?
rbinom(0:50, 50, 0.44)
mean(rbinom(0:50, 50, 0.44))
#e) Construa um grafico com todas as probabilidades
xx=0:12
px=dbinom(xx,12,0.56)
dados=data.frame(xx,px)
dados 
ggplot(dados,aes(x=xx,y=px))+geom_col(fill= "pink")+geom_text(aes(label=round(px,3)))+
  labs(title = '', x = 'N° de Componentes', y = 'Probabilidade de Não Apresentar Falhas')
  
##### exercício 2#####  
#A capacidade de processamento de um componente eletronico eh,
#em media, de 3.2 Hz por segundo. Determine:
#a)	A probabilidade de processar  4 ciclos  por segundo?
dpois(x=4, lambda=3.2)
#b)A probabilidade de processar  3 ou mais ciclos  por segundo
1-sum(dpois(0:2,3.2))
1-ppois(2,3.2)
ppois(2,3.2,lower.tail = F)
#c)Apresente graficamente a maior parte das probabilidades
xx=0:10
px=dpois(xx,3.2)
dados=data.frame(xx,px)
ggplot(dados,aes(xx,px))+geom_col(fill = "light blue")+geom_text(aes(label=round(px,3)))+
  labs(title = '', x = 'Ciclos', y = 'Probabilidades')
#d)  A probabilidade de processar 300 ou menos ciclos em um minuto?
# Como 300 ciclos em um minuto é o mesmo que 5 ciclos em 1 segundo.
sum(dpois(0:5,3.2))
ppois(5,3.2)

##### exercício 3 #####
#O tempo de falha de um componente eletrico segue uma distribuicao 
#exponencial com media 3.17 anos. Calcule:
#a)A probabilidade da falha ocorrer apos 7 anos?
1-pexp(7,rate=1/3.17)
pexp(7,rate=1/3.17,lower.tail = F)
#b)A probabilidade da falha ocorrer antes de 3 anos? 
pexp(3,rate=1/3.17)
#c)A probabilidade da falha ocorrer entre 2 e 6 anos? 
x=pexp(6,rate=1/3.17) 
y=pexp(2,rate=1/3.17) 
x-y
#d)Qual a variancia do tempo de falha? (formula)  
lambda = 1/3.17
var = 1/ (lambda)^2
var
#e)Apresente graficamente a distribuicao de probabilidade. 
xx=rexp(30,1/3.17)
px=dexp(xx,1/3.17)
ax=pexp(xx,1/3.17)
dados=data.frame(xx,px,ax)
ggplot(dados,aes(xx))+  geom_line(aes(xx,px),col='red',lwd=2)+
  labs(title = '', x = 'Anos', y = 'Probabilidade de Falhar')

#####exercicio 4 ####
#Seja X uma variavel aleatoria que segue distribuicao normal 
#com media 93.9 e variancia 150. Determine:
v = sqrt(150)
v
#a)	P(X < 115)  
pnorm(115,93.9,v)
#b)	P(85<X<110)  
x = pnorm(110,93.9, v)
y = pnorm(85,93.9,v)
x-y
#c)	P(X> 65)  
1-pnorm(65,93.9,v)
pnorm(65,93.9,v,lower.tail = F)
#d)	O valor de k tal que  P(X<K)=0,45	
k=qnorm(0.45,93.9,v)
k
#e) Apresente graficamente a distribuicao normal 
xx=rnorm(500,93.9,v)
px=dnorm(xx,93.9,v)
dados=data.frame(xx,px)
ggplot(dados,aes(xx,px))+geom_line(col='purple',lwd = 1.5)+
  labs(title = '', x = 'x', y = 'Densidade')

#####exercicio 5 #####
#Uma amostra de tamanho 60 de uma variavel que segue uma distribuicao
#normal foi obtida e se encontra salva no objeto "amostra"(digite amostra para ver!).
#Determine a probabilidade de X<25 
amostra
m=mean(amostra)
m
v=var(amostra)
v
x=sqrt(v)
x
pnorm(25,m,x)
