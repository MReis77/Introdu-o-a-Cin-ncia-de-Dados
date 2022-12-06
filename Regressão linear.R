library(corrgram)
library(readxl)

cars   
dim(cars)

#Saber a correla��o
cor(cars)
#criando o modelo
modelo = lm(speed ~ dist , data = cars)
modelo
#plotando o gr�fico 
plot(speed ~ dist, data = cars, xlab = 'Dist�ncia',ylab = 'Velocidade', main = 'Carros')
abline(modelo)
#fazer a previs�o modo 1
modelo$coefficients
modelo$coefficients[1] + modelo$coefficients[2] * 22
#fazer a previs�o modo 2
predict(modelo, data.frame(dist = 20))

#para saber mais informa��es
summary(modelo)
modelo$coefficients
modelo$residuals
hist(modelo$residuals)
modelo$fitted.values
plot(modelo$fitted.values, cars$dist)

mtcars
dim(mtcars)
cor(mtcars)

plot(mtcars$mpg ~ mtcars$disp, ylab = 'Consumo', xlab = 'Cilindradas', main = 'Carros')
modelo <- lm(mpg ~ disp, data = mtcars)
modelo
summary(modelo)$r.squared
summary(modelo)$adj.r.squared
plot(mpg~disp, data = mtcars, xlab = 'Cilindradas', ylab = 'Consumo', main = 'Carros')
abline(modelo)

predict(modelo, data.frame(disp = 500))

#modelo com mais de uma vari�vel no modelo 
modelo <- lm(mpg ~ disp + hp + cyl, data = mtcars) 
modelo

summary(modelo)$r.squared
summary(modelo)$adj.r.squared             

predict(modelo, data.frame(disp = 200, hp = 100, cyl = 4))             

#atividade pr�tica             

df <- read_excel('C:/Users/Opencadd/Desktop/regre.xlsx')            
df  
dim(df)

plot(FrqAnual ~ CusInic, data = df)
plot(CusInic ~ FrqAnual, data = df)
modelo <- lm(FrqAnual ~ CusInic, data = df)
modelo             
modelo2
plot(FrqAnual ~CusInic, data = df, ylab = 'Fanquia Anual', xlab = 'Custo inicial', main = 'Previs�o de Franquia anual')
abline(modelo)              

predict(modelo, data.frame(CusInic = 1800))

