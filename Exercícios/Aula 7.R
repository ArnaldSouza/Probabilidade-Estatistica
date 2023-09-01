#comentarios
#rodar linha = crtl + enter
1+1


#funcoes basicas
sin(pi/2)
cos(pi)
tan(pi)

#vetor
(x=c(1,2,5,6))
(x=2:10)
y=3:11
x+y

#matrix
z=matrix(x,nrow = 2,byrow = T)
solve(z)
t(z)
determinant(z)
 
#derivadas
D(expression(2*tan(x)*sin(x)),'x')

#integrais
library(mosaicCalc)

antiD(sin(x)~x)
 

funcao=function(x){
  x+1
}

funcao(3)


funcao2=function(a,b,c){ #baskara
  delta=b^2-4*a*c
  x1=(-b-sqrt(delta))/(2*a)
  x2=(-b+sqrt(delta))/(2*a)
  xs=c(x1,x2)
  xs
}

funcao2(4,2,-6)

#integral definida
integrate(funcao,0,2)

#como ler dados no R
dados=read.csv('dados.csv',sep=';')
dados$Renda

#estatistica
mean(dados$Renda) #media
var(dados$Renda) #variancia
sd(dados$Renda)  #desvio padrao 

 







