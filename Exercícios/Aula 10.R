x=c(1.987,2.876,3.456,1.776)

round(x,1) #arredondar

x
ifelse(x<=2 & x<=3,'correto',0) #verifica se verdaderio ou falso

##### Distribuição Binomial #####
#Suponha um experimetno realizado 10 vezes com probabilidade de sucesso de 30%
#Determine a P(x=1)
#n = tentativas, p = prob de sucesso
dbinom(x = 1, size=10, prob = 0.3)



#Determine P(X<=5)
0:5 #sequencia de 0 a 5
sum(dbinom(0:5,10,0.3)) #por soma

pbinom(5,10,0.3) #função acumulada


#Determine P(X>=6)
sum(dbinom(6:10,10,0.3))

1-pbinom(5,10,0.3) #complemento
1-pbinom(5,10,0.3, lower.tail = F) 


#gere uma amostra de tamanho 100
rbinom(100,10,0.3)

#Determine as prob. de todos valores de C e salve em um obj.
xx=0:10
px=dbinom(xx,10,0.3)


#faça um grafico de barras com os valores obtidos
library(ggplot2)
dados=data.frame(xx,px)
dados 

ggplot(dados,aes(x=xx,y=px))+geom_col()+geom_text(aes(label=round(px,2)))


#faça uma area para x<3
ggplot(dados,aes(x=xx,y=px))+geom_col(fill=ifelse(dados$xx<=1 | dados$xx>=5 ,'blue','red'))+
  geom_text(aes(label=round(px,2)))


#faça um gráfico de barras pra a função acumulada
xx=0:10
px=dbinom(xx,10,0.3)
ax=pbinom(xx,10,0.3)
ax

dados=data.frame(xx,px,ax)
ggplot(dados,aes(xx,ax))+geom_col()+geom_text(aes(label=round(ax,2)))

##### Distribuição de Poisson #####

#suponha X~Pois, com media = 50 (lambda)

#determine P(X=45)
dpois(x=45, lambda=50)

#determine a P(X<=45)
sum(dpois(0:45,50))
ppois(45,50)

#determine a P(X>=50)
1-sum(dpois(0:49,50))
1-ppois(49,50)
ppois(49,50,lower.tail = F)


#gere uma amostra de tamanho 100
rpois(100,50)


#faça um gráfico das probabilidades
xx=25:100
px=dpois(xx,50)
dados=data.frame(xx,px)

#xlim(min,max)
ggplot(dados,aes(xx,px))+geom_col()+geom_text(aes(label=round(px,2)))


#marque a area para x<40
ggplot(dados,aes(xx,px))+geom_col(fill=ifelse(dados$xx<40,'blue','red'))+
  geom_text(aes(label=round(px,2)))

#marque a area para x>40 e x<60
ggplot(dados,aes(xx,px))+geom_col(fill=ifelse(dados$xx>40 & dados$xx<60,'blue','red'))+
  geom_text(aes(label=round(px,2)))

#grafico da acumulada
xx=25:100
px=dpois(xx,50)
ax=ppois(xx,50)
dados=data.frame(xx,px)

ggplot(dados,aes(xx,ax))+geom_col()+geom_text(aes(label=round(ax,2)))


#marque a area para x<50

ggplot(dados,aes(xx,ax))+geom_col(fill=ifelse(dados$xx<=50,'blue','red'))+
  geom_text(aes(label=round(ax,2)))
















##### Distribuição Exponencial #####

#suponha X~Exp, com media 50 (lambda=1/50)

#P(X<40)
pexp(40,rate=1/50)

#P(X>40)
1-pexp(40,rate=1/50)
pexp(40,rate=1/50,lower.tail = F)

#gere uma amostra do tamanho 5000 e apresente um histograma
xx=rexp(5000,1/50)
px=dexp(xx,1/50)
ax=pexp(xx,1/50)
dados=data.frame(xx,px,ax)

ggplot(dados,aes(xx))+geom_histogram() #contagem
ggplot(dados,aes(xx))+geom_histogram(aes(y=..density..)) #prob

#adicione a função de probabilidade exponencial
ggplot(dados,aes(xx))+geom_histogram(aes(y=..density..))+
  geom_line(aes(xx,px),col='red',lwd=2)

#apresente apenas a função de probabilidade
ggplot(dados,aes(xx))+
  geom_line(aes(xx,px),col='red',lwd=2)

#apresente a area de X<60
ggplot(dados,aes(xx))+geom_line(aes(xx,px),col='red',lwd=2)+
  geom_area(mapping = aes (x=ifelse(xx<60,xx,0),y=ifelse(xx<60,px,0)),fill='blue')

#apresente a acumulada
ggplot(dados,aes(xx,ax))+geom_line()
ggplot(dados,aes(xx))+geom_line(aes(xx,ax))


#determine a mediana de X e represente no grafico
pexp(20,1/50)
qexp(0.32968,1/50)
qexp(0.5,1/50)

md=qexp(0.5,1/50)

ggplot(dados,aes(xx))+geom_line(aes(xx,px),col='red',lwd=2)+
  geom_area(mapping = aes (x=ifelse(xx<md,xx,0),y=ifelse(xx<md,px,0)),fill='blue')

ggplot(dados,aes(xx))+geom_line(aes(xx,ax))+
  geom_area(mapping = aes (x=ifelse(xx<md,xx,0),y=ifelse(xx<md,ax,0)),fill='blue')+
  geom_hline(yintercept=0.5)

##### Distribuição Normal #####

#suponha que X~N(media=50,var=25)

#P(X<45)
pnorm(45,50,5)

#P(X>45)
1-pnorm(45,50,5)
pnorm(45,50,5,lower.tail = F)

#gere uma amostra de tamanho 5000 e apresente um histograma
xx=rnorm(5000,50,5)
px=dnorm(xx,50,5)
ax=rnorm(xx,50,5)
dados=data.frame(xx,px,ax)

ggplot(dados,aes(xx))+geom_histogram(aes(y=..density..))

#apresente a função de probabilidade
ggplot(dados,aes(xx,px))+geom_line()

#apresente a função acumulada
ggplot(dados,aes(xx,ax))+geom_line()

#determine o valor de a tal que P(X<a)=0.38
a=qnorm(0.38,50,5)

ggplot(dados,aes(xx,px))+geom_line()+
  geom_area(mapping = aes(x=ifelse(xx<a,xx,30),y=ifelse(xx<a,px,0)),fill='red')
