Pearson's product-moment correlation
data:  CityMPG and Weight
t = -13.612, df = 80, p-value < 2.2e-16
alternative hypothesis: true correlation is not equal to 0
95 percent confidence interval:
 -0.8911272 -0.7558007
sample estimates:
       cor 
-0.8357354 

library(fmsb)
attach(cars)
model1= lm(CityMPG~Weight)
summary(model1)
#graphing 
par(mfrow=c(2,2))
plot(CityMPG~Weight, main= 'Model1',pch=1)
abline(model1,lty=2)
plot(Weight, model1$residuals, main= 'Model1',xlab= 'Weight', ylab = 'Residuals')
abline(h=0,lty=2)
model2a= lm(CityMPG~Weight+I(Weight^2))
summary(model2a)
invweight= 1/Weight
model2b= lm(CityMPG~invweight)
summary(model2b)
plot(CityMPG~invweight, main= 'Model2b',pch=1)
abline(model2b,lty=2)
plot(invweight, model2b$residuals, main= 'Model2b',xlab= 'inverseweight', ylab = 'Residuals')
abline(h=0,lty=2)
#Scatterplot against other variables 
par(mfrow=c(1,3))
plot(Domestic, CityMPG, pch=1 )
plot(Horsepower, CityMPG, pch=1 )
plot(RPM, CityMPG, pch=1 )
#Type as additional variable 
x1grp=ifelse(Type!="Small",0,1)
x2grp=ifelse(Type!="Compact",0,1)
x3grp=ifelse(Type!="Midsize",0,1)
x4grp=ifelse(Type!="Sporty",0,1)
model3= lm(CityMPG~ invweight + x1grp +x2grp +x3grp + x4grp)
summary(model3)
model4a= lm(CityMPG~invweight + Domestic)
summary(model4a)
invhorsepower = 1/Horsepower
model4b= lm(CityMPG~invweight + invhorsepower )
summary(model4b)
model4c= lm(CityMPG~invweight + RPM)
summary(model4c)
#interpretation of results 
summary(model2b)
aov(CityMPG~invweight)
par(mfrow=c(1,3))
plot(CityMPG~invweight, main= 'Model2b',pch=1)
abline(model2b,lty=2)
plot(invweight, model2b$residuals, main= 'Model2b',xlab= 'inverseweight', ylab = 'Residuals')
abline(h=0,lty=2)
qqnorm(model2b$residuals,ylab='Residuals', xlab='Normal Quantiles')
qqline(model2b$residuals)
ks.test(model2b$residuals,pnorm,mean(model2b$residuals),sd(model2b$residuals))
#percentage decrease in CityMPG given mean(weight)
mean(Weight)
invmeanweight = 1/mean(Weight)
initialMPG= model2b$coefficients[1]+invmeanweight*model2b$coefficients[2]
invmeanweight1.1 = 1/ (mean(Weight)*1.1)
newMPG= model2b$coefficients[1]+invmeanweight1.1*model2b$coefficients[2]
percentchangeMPG = (newMPG-initialMPG)/initialMPG
percentchangeMPG