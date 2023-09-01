source("https://git.io/Jq9pB ")

avaliacao1(2271923)

head(Arnald)
dados=Arnald
#exercicio a
library(ggplot2)
ggplot(Arnald,aes(x =empresa,fill=empresa)) +geom_bar() +scale_fill_brewer(palette="Accent")+theme_classic()+labs(x='Empresa',y='Frequência')

#exercício b
ggplot(Arnald,aes(x =empresa, y = resistencia,fill=empresa))+
  geom_boxplot()+
  theme_light()+scale_fill_brewer(palette="Accent")

#exercício c
m=as.data.frame(tapply(dados$resistencia,dados$empresa,mean))
m$empresa=labels(m) [[1]]

names(m) [1] = 'media_resistencia'

m

m2=m
m2$media_resistencia=round(m2$media_resistencia/sum(m2$media_resistencia),3)
m2$cum=cumsum(m2$media_resistencia)

ggplot(m2,aes(area=media_resistencia,fill=empresa))+geom_treemap()
ggplot(m2,aes(x=2, y=media_resistencia, fill=empresa))+
  geom_bar(width = 1, stat="identity") + coord_polar("y",start = 0) +
  scale_fill_brewer(palette="Accent")+
  theme_void()+xlim(0.5, 2.5)

#exercicio d
Arnald

x[x>15 & x<17]
ggplot(Arnald,aes(x=tempo ,y=resistencia, empresa, col=empresa))+ xlim(15,17) + geom_point()+geom_smooth()+theme_classic()   


#exercicio e
library (treemapify)
ggplot(m,aes(area=media_resistencia,fill=empresa)) + geom_treemap() + 
  scale_fill_brewer(palette="Accent") + theme_classic()


#exercicio f
library(Arnald)

ggplot(Arnald,aes(x=tempo, y=resistencia,col=empresa, size=resistencia))+
  geom_point()+theme_classic()+labs(x='Tempo',y='Resistência')+
  trasition_time(tempo)+shadow_mark(past=T,future=F,alpha=0.3)

grafico=ggplot(Arnald,aes(x=tempo, y=resistencia,col=empresa, size=resistencia))+
  geom_point()+theme_classic()+labs(x='Tempo',y='Resistência')+
  trasition_time(tempo)+shadow_mark(past=T,future=F,alpha=0.3)

animate(grafico,renderer = magick_renderer())
