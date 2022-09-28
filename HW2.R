myName <- "Ruiyi Feng"

#11.5a
setwd("~/Documents/BU/2022 Fall/678/ROS-Examples-master/pyth")
pyth <- read.table('pyth.txt',header=T)
m1 <- lm(y~x1+x2,data=pyth[1:40,])
summary(m1)

#11.5b
plot(pyth$x1[1:40],pyth$y[1:40],pch=20,xlab='x1',ylab='y',cex=1)
abline(coef(m1)[1]+coef(m1)[3]*mean(pyth$x2[1:40]),coef(m1)[2])
plot(pyth$x2[1:40],pyth$y[1:40],pch=20,xlab='x2',ylab='y',cex=1)
abline(coef(m1)[1]+coef(m1)[2]*mean(pyth$x1[1:40]),coef(m1)[3])

#11.5c
plot(m1,which=1,cex=1)
plot(m1,which=2,cex=1)
## Independence is met, but equal variance and normality are not met.

#11.5d
a <- data.frame(x1=pyth$x1[41:60],x2=pyth$x2[41:60])
predict(m1,a)
## R square is large enough so very confident.

#12.5a
## 0.78 1.28

#12.5b
setwd("~/Documents/BU/2022 Fall/678/ROS-Examples-master/Earnings/data")
earnings <- read.csv('earnings.csv')
logh <- log(earnings$height)
logw <- log(earnings$weight)
m2 <- lm(logw~logh)
plot(logh,logw,pch=20,xlab='log(height)',ylab='log(weight)')
abline(coef(m2)[1],coef(m2)[2])

#12.6a
setwd("~/Documents/BU/2022 Fall/678/ROS-Examples-master/Pollution/data")
pollution <- read.csv('pollution.csv')
m3 <- lm(pollution$mort~pollution$nox)
plot(pollution$nox,pollution$mort,pch=20,cex=1)
abline(coef(m3)[1],coef(m3)[2])
plot(m3,which=1)
plot(m3,which=2)
## Linear regression does not fit the data well. It looks like that residuals are independent 
## normally distributed but the variance is not equal. 

#12.6b
lmo <- log(pollution$mort)
ln <- log(pollution$nox)
m4 <- lm(lmo~ln)
plot(pollution$nox,lmo,pch=20,cex=1)
abline(coef(m4)[1],coef(m4)[2])
plot(m4,which=1)
plot(m4,which=2)
m4

#12.6c
## For each 1% difference in nox, the predicted mort difference is 0.15%.

#12.6d
m5 <- lm(log(pollution$mort)~log(pollution$nox)+log(pollution$so2)+log(pollution$hc))
m_nox <- mean(log(pollution$nox))
m_so2 <- mean(log(pollution$so2))
m_hc <- mean(log(pollution$hc))
c <- coef(m5)
plot(log(pollution$nox),log(pollution$mort),pch=20,cex=1)
abline(c[1]+m_so2*c[3]+m_hc*c[4],c[2])
plot(log(pollution$so2),log(pollution$mort),pch=20,cex=1)
abline(c[1]+m_nox*c[2]+m_hc*c[4],c[3])
plot(log(pollution$hc),log(pollution$mort),pch=20,cex=1)
abline(c[1]+m_nox*c[2]+m_so2*c[3],c[4])
m5
## For each 1% difference in nox, the predicted mort difference is 0.06%.
## For each 1% difference in so2, the predicted mort difference is 0.01%.
## For each 1% difference in hc, the predicted mort difference is -0.06%.

#12.6e
m6 <- lm(log(pollution$mort[1:30])~log(pollution$nox[1:30])+log(pollution$so2[1:30])+log(pollution$hc[1:30]))
b <- data.frame(x1=log(pollution$nox[31:60]),x2=log(pollution$so2[31:60]),x3=log(pollution$hc[31:60]))
pre <- predict(m6,b)
plot(c(1:30),pre-log(pollution$mort[31:60]))

#12.7a
library(rstanarm)
setwd("~/Documents/BU/2022 Fall/678/ROS-Examples-master/Earnings/data")
earnings <- read.csv('earnings.csv')
m7 <- stan_glm(earnings$earnk~earnings$height+earnings$male)
d <- log(earnings$earnk[earnings$earnk!=0])
m8 <- stan_glm(d~earnings$height[earnings$earnk!=0]+earnings$male[earnings$earnk!=0])
loo(m7)
loo(m8)

#12.7b
## Compare models with other exercises. 

#12.8a
## Suppose that the height of the animal is x and the predicted weight is y. 
## A linear regression model with prediction is log(y) = c+(2%/1%)*log(x). 
## Substitute x with 50cm and y with 10kg, the equation turns into log(10)=c+2*log(50). 
## Then c=log(10)-2*log(50)= -5.5215
## The equation of the regression line is log(weight in kg)=-5.5215+2*log(height in cm).
## 95% of the values fall within a factor of 1.1 of predicted values.
## Then the residual standard deviation of the regression is 0.1*0.5/0.95 = 0.0526

#12.8b
## 1-(0.0526/0.2)^2=0.9308

#12.9a
## This transformation is easy to conduct and shows the relationship between vote share and value difference. 
## When value difference is equal to zero, the simple difference still makes sense.

#12.9b
## This transformation is easy to conduct and shows the relationship between vote share and value ratio.
## However, this does not make sense because ratio cannot be zero. 

#12.9c
## This transformation is easy to conduct and shows the relationship between vote share and log value difference. 
## When log value difference is equal to zero, the difference on log scale still makes sense.

#12.9d
## This transformation is easy to conduct and shows the relationship between vote share and value ratio.
## However, this does not make sense because ratio cannot be zero. 

#12.11
## We can have Q=(e^alpha)*(P^0.3). For each 1% difference in the average price, the predicted difference of quantity purchased is 0.3%. 

#12.13
setwd("~/Documents/BU/2022 Fall/678/ROS-Examples-master/Beauty/data")
beauty <- read.csv('beauty.csv')
m9 <- lm(eval~beauty+female+age+female*age,data=beauty)
summary(m9)
m10 <- lm(log(eval)~beauty+female+age+female*age,data=beauty)
summary(m10)
plot(m9,which=1)
plot(m9,which=2)
plot(m10,which=1)
plot(m10,which=2)
## I choose the model with log transformation because the residuals are more normally distributed and constant variance. 

#12.14
setwd("~/Documents/BU/2022 Fall/678/ROS-Examples-master/Mesquite/data")
mesquite <- read.table('mesquite.dat',header=T)
mesquite
e <- stan_glm(formula=weight~group+diam1+diam2+total_height+canopy_height+density,data=mesquite)
