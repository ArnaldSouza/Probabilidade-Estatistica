######intervalo de confiança para média####

#primeira questão: IC media com alpha  5% (95% de confiança)
x=c(19.8,10.1,14.9,7.5,15.4,15.4,15.4,18.5,7.9,12.7,11.9,11.4,11.4,14.1,17.6,16.7,15.8,
    19.5,8.8,13.6,11.9,11.4)
length(x)

t.test(x,conf.level = 0.95)


#segunda questão: xbarra=150 , 1% de significância, desvio = sqrt(40), n=15
(icI=150-qt(0.995,df =15-1)*sqrt(40/15))
(icI=150+qt(0.995,df =15-1)*sqrt(40/15))


#####intervalo de confiança para a proporção####
#terceira questão: #n = 35 , %defeitos= 0.46, alpha = 10% , (90% confiança)

#prop.test(quantidade de falhas, tamanho amostra )
prop.test(35*0.46,35,conf.level = 0.90, correct =  F)
