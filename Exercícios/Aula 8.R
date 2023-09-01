####carregar dados####
dados=read.csv('dados_plot.csv',sep = ',')
dados
head(dados)
dados[,4] #dados[linha,coluna]

#arrumar nove variável 
names(dados)
names(dados)=c('linha','empresa','rendimento','tamanho','vendas','palavra','fre_palavra')
names(dados)

head(dados)
#Tabela
table(dados$empresa,dados$tamanho)


#medidas por variavel 
tapply(dados$rendimento,dados$empresa,mean)


library(ggplot2) #install

#####Grafico de coluna/barra de empresa####

#util para variaveis nao numericas
# quantidade de empresas ()

#ggplot base
ggplot(dados,aes(x =empresa)) 

#adiciona colunas +geom_bar()
ggplot(dados,aes(x =empresa)) +geom_bar()

#barra: +coord_flip()
ggplot(dados,aes(x =empresa)) +geom_bar()+coord_flip()

#legenda e cor: fill=
ggplot(dados,aes(x =empresa,fill=empresa)) +geom_bar()

#nomes eixos e titulo: labs
ggplot(dados,aes(x =empresa,fill=empresa)) +geom_bar()+
  labs(title = 'Titulo', x = 'Empresa', y = 'Total') 


#mudar de cor, +scale_fill_brewer(palette="Set3")
ggplot(dados,aes(x =empresa,fill=empresa)) +geom_bar()+scale_fill_brewer(palette="Set3")
#Dark2, PuOr, Greys, Paired, Set1, Set2, Set3

#tema de fundo, +theme_dark()
ggplot(dados,aes(x =empresa,fill=empresa)) +geom_bar()+
  scale_fill_brewer(palette="Set3")+theme_dark()
#theme_bw(), theme_linedraw(), theme_light(), theme_dark(), theme_minimal()


#colunas agrupadas (adicionar uma nova variavel em fill)
names(dados) #tamanho
ggplot(dados,aes(x =empresa,fill=tamanho)) +geom_bar()+
  scale_fill_brewer(palette="Set3")+theme_dark()


#dividir  por variavel facet_wrap(~tamanho)
ggplot(dados,aes(x =empresa,fill=empresa)) +geom_bar()+
  scale_fill_brewer(palette="Set3")+theme_dark()+
  facet_wrap(~tamanho)


#barra +coord_flip()
ggplot(dados,aes(x =empresa,fill=empresa)) +geom_bar()+
  scale_fill_brewer(palette="Set3")+theme_dark()+
  facet_wrap(~tamanho)+coord_flip()


######### Grafico de dispersao####
#util para duas variaveis numericas

#dispersao
ggplot(dados,aes(x=rendimento,vendas))

#pontos +geom_point()
ggplot(dados,aes(x=rendimento,vendas))+geom_point()

#tendencia
ggplot(dados,aes(x=rendimento,vendas))+geom_point()+  geom_smooth()


#cor (agora deve ser col=) #remover smooth
ggplot(dados,aes(x=rendimento,vendas,col=empresa))+geom_point()


#tendencia por empresa
ggplot(dados,aes(x=rendimento,vendas,col=empresa))+geom_point()+  geom_smooth()

#remover desvio padrao
ggplot(dados,aes(x=rendimento,vendas,col=empresa))+geom_point()+  geom_smooth(se=F)


#tendencia por empresa separados
ggplot(dados,aes(x=rendimento,vendas,col=empresa))+geom_point()+geom_smooth(se=F)+
  facet_wrap(~empresa)


#tendencia por empresa com tendencia e tema
ggplot(dados,aes(x=rendimento,vendas,col=empresa))+geom_point()+geom_smooth(se=F)+
  facet_wrap(~empresa) +theme_dark()



#####pizza ####
dados1=as.data.frame(table(dados$empresa))
names(dados1)[1]='empresa'
names(dados1)
dados2=dados1
dados2$Freq=round(dados2$Freq/sum(dados2$Freq),3)
dados2$cum=cumsum(dados2$Freq)

ggplot(dados2, aes(x = "", y = Freq, fill = empresa)) + 
  geom_bar(width = 1, stat="identity") +   coord_polar("y",start = 0) +
  scale_fill_brewer(palette="Set3")


#text
ggplot(dados2, aes(x = "", y = Freq, fill = empresa)) + 
  geom_bar(width = 1, stat="identity") +   coord_polar("y",start = 0) +
  scale_fill_brewer(palette="Set3")+
  geom_text(aes(y = cum-0.2, label = Freq), color = "black")


#tema
ggplot(dados2, aes(x = "", y = Freq, fill = empresa)) + 
  geom_bar(width = 1, stat="identity") +   coord_polar("y",start = 0) +
  scale_fill_brewer(palette="Set3")+
  geom_text(aes(y = cum-0.2, label = Freq), color = "black")+
  theme_void()



#dunnet plot
#x=2 e +xlim(0.5, 2.5)
ggplot(dados2, aes(x = 2, y = Freq, fill = empresa)) + 
  geom_bar(width = 1, stat="identity") +   coord_polar("y",start = 0) +
  scale_fill_brewer(palette="Set3")+
  geom_text(aes(y = cum-0.2, label = round(Freq,3)), color = "black")+
  theme_void()+xlim(0.5, 2.5)





######Histograma####
#parecido com grafico de barras, mas para variavel numerica continua


#geom_histogram() base + histograma
ggplot(dados, aes(rendimento))+ geom_histogram() 


#numero de grupos, bins 
ggplot(dados, aes(rendimento))+ geom_histogram(bins = 5)

#cor:
ggplot(dados, aes(rendimento))+ geom_histogram(bins = 5,fill='orange')


#aes(fill=empresa)
ggplot(dados, aes(rendimento))+ geom_histogram(aes(fill=empresa),bins = 5)




##### Densidade ####
#eh um histograma, porem com linha e probabilidade
ggplot(dados, aes(rendimento))+ geom_density()  

#por empresa aes(fill=factor(empresa))
ggplot(dados, aes(rendimento))+ geom_density(aes(fill=factor(empresa)))  


#transparencia alpha=
ggplot(dados, aes(rendimento))+ geom_density(aes(fill=factor(empresa)), alpha=0.5)  

#separar por empresas
ggplot(dados, aes(rendimento))+ geom_density(aes(fill=factor(empresa)))+
facet_wrap(~empresa)

#####Box plot ####
ggplot(dados, aes( y=rendimento))+geom_boxplot() 


#por empresa
ggplot(dados, aes(x=empresa, y=rendimento))+geom_boxplot() 

#cor
ggplot(dados, aes(x=empresa, y=rendimento,fill=empresa))+geom_boxplot() 


#comparar por tamanho
ggplot(dados, aes(x=empresa, y=rendimento,fill=empresa))+geom_boxplot() +
  facet_wrap(~tamanho)



#####violino####
ggplot(dados, aes(x=empresa, y=rendimento,fill=empresa))+geom_violin()



#####tree map####
library(treemapify)
ggplot(dados1,aes(area=Freq,fill=empresa))+geom_treemap()

#rendimento medio por tamanho x tipo empresa
#por empresa e tamanho, Ga, Gb... Pa, Pb
dados$interacao=interaction(dados$tamanho,dados$empresa)
dados3=as.data.frame(tapply(dados$rendimento,dados$interacao, mean))
dados3
dados3$nomes=labels(dados3)[[1]]
names(dados3)[1]='total'
dados3


ggplot(dados3,aes(area=total,fill=nomes))+geom_treemap()


#####nuvem de letras####
dados4= dados[,6:7]
dados4
library(worldcloud2) 
wordcloud2(data=dados4, size=1.6)






##### Animado dispersao####
library(gganimate) #animado
library(gapminder) #dados

head(gapminder)


#ralacao entre renda x vida
ggplot(gapminder, aes(gdpPercap, lifeExp)) +
  geom_point() +  theme_bw() 

#colorir por continente
ggplot(gapminder, aes(gdpPercap, lifeExp, col = continent)) +
  geom_point() +  theme_bw() 

#tamanho do ponto de acordo com a populacao
ggplot(gapminder, aes(gdpPercap, lifeExp, size = pop, color = continent)) +
  geom_point() +  theme_bw() 

#arrumar nomes eixos
ggplot(gapminder, aes(gdpPercap, lifeExp, size = pop, color = continent)) +
  geom_point() +  theme_bw() +
  labs(title = 'Ano: {frame_time}', x = 'Renda per capita', y = 'Expectativa de vida') 


#animar por ano
ggplot(gapminder, aes(gdpPercap, lifeExp, size = pop, color = continent)) +
  geom_point() +  theme_bw() +
  labs(title = 'Ano: {frame_time}', x = 'Renda per capita', y = 'Expectativa de vida') +
  transition_time(year) 


#instalar esses dois pacotes
install.packages('gifski')
install.packages('png')

#carregar
library(gifski)
library(png)

#salvar o grafico em um objeto
grafico=ggplot(gapminder, aes(gdpPercap, lifeExp, size = pop, color = continent)) +
  geom_point() +  theme_bw() +
  labs(title = 'Ano: {frame_time}', x = 'Renda per capita', y = 'Expectativa de vida') +
  transition_time(year) 

animate(grafico, renderer = gifski_renderer())
