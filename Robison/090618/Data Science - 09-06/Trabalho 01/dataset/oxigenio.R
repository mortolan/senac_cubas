#Leitura do Arquivo oxigenio.txt
consumo = read.table('oxigenio.txt', header=T)
attach(consumo)

#Diagrama de Dispersao
plot(tempo, oxigenio, col=c('red', 'blue', 'green'))

#Coeficiente de Correlacao
cor(tempo,oxigenio)

#Teste de Hipoteses para o Coeficiente de Correlacao
cor.test(tempo,oxigenio)

#Ajuste do Modelo de Regressao Linear (lm = Linear Model)
ajuste.modeloLinear = lm(oxigenio ~ tempo)
ajuste.modeloLinear

# Modelo com ambiente Multiplo
#ajuste.modeloLinear = lm(oxigenio ~ tempo + sexo + score + ano)


#Teste de Significancia do Modelo
summary(ajuste.modeloLinear)

#Teste de Normalidade
shapiro.test(residuals(ajuste.modeloLinear))

# Estimativa de Consumo
predict(ajuste.modeloLinear, newdata=data.frame(tempo=12.5))

#Reta ajustada no Diagrama de Dispersao
plot(tempo, oxigenio, col=c('red', 'blue', 'green'))
abline(ajuste.modeloLinear)

#Coeficiente de Determinacao
summary(ajuste.modeloLinear)$r.squared

