library(readxl)
excel_sheets(path = file.path("data_rls_uti.xlsx"))
data <- read_excel("data_rls_uti.xlsx",sheet = "Hoja1",col_names = TRUE, na="")

str(data)
View(data)

#Regresion lineal

reg <- lm(Utilidad~Ventas , data)
str(reg)
anova<-aov(reg)
summary(reg)
summary(anova)

#Valor percentil t de student, fisher
qt(0.975 , df =38)
qf(0.95 , df1=1,df2=38)

#B1 no es significativo, por lo que se debe centrar los datos
#------------------------------------------------------------

#intervalos de confianza
confint(reg , level=0.95)


names(reg)

res<-reg[["residuals"]]
pred<-reg[["fitted.values"]]

data2<-data.frame(data,Predicciones=pred,Residuos=res)

hist(res,15)
mean(res)


#prueba de normalidad
qqnorm(res)
qqline(res,col="red")

#Graficas

plot(data[,"Ventas"],data[,"Utilidad"])
plot(res,pred)
plot(res, data[,"Ventas"])



#--------------------------------------------------------------------------
#Datos centrados


utilidad_c<-data[,"Utilidad"]-mean(data[,"Utilidad"])
ventas_c<-data[,"Ventas"]-mean(data[,"Ventas"])
mean(utilidad_c)
mean(ventas_c)
data_c<-data.frame(Utilidad=utilidad_c,Ventas=ventas_c)

#Regresion lineal

reg_c <- lm(Utilidad~Ventas , data_c)
str(reg_c)
anova_c<-aov(reg_c)
summary(reg_c)
summary(anova_c)


#Valor percentil
qt(0.975 , df =38)
qf(0.95 , df1=1,df2=38)

#Intervalos de confianza

confint(reg_c,level=0.95)

names(reg_c)

res_c<-reg_c[["residuals"]]
pred_c<-reg_c[["fitted.values"]]

data_c2<-data.frame(data_c,Predicciones=pred_c,Residuos=res_c)

hist(res_c,15)
mean(res_c)

#prueba de normalidad

qqline(res_c,col="red")

#Graficas

plot(data_c[,"Ventas"],data_c[,"Utilidad"])
plot(res_c,pred_c)
plot(res_c, data_c[,"Ventas"])



