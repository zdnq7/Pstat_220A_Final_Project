# Pstat_220A_Final_Project
Pstat_220A_Advanced_Statistical_Methods


data<-read.table("http://www.pstat.ucsb.edu/faculty/yuedong/classes/data/property.txt",
           header=T)
str(data)
cor(data)  # no strong correlation
library(MASS)
attach(data)
pairs(data) # pair linear relationship
summary(data)
par(mfrow=c(1,5))
#histogram of variables
hist(price)
hist(size)
hist(age)
hist(dc)
hist(dt)


residual.plot<-function(fit){
  par(mfrow=c(2,2))
  qqnorm(fitted(fit),ylab="Residuals",main="QQ-plot of residuals")
  qqline(fitted(fit))
  qqnorm(rstandard(fit), ylab="Residuals",main="QQ-plot of standarized residuals")
  qqline(rstandard(fit))
  plot(fitted(fit),resid(fit),xlab="Fitted",
       ylab="Residuals",main="Residuals vs. Fitted")
  abline(h=0)
  plot(fitted(fit), abs(residuals(fit)), xlab="Fitted",
       ylab="Absolute residuals",main="Absolute residuals vs fitted")
  abline(h=0)
}
# 1st fit
fit1<-lm(price~.,data=data)
summary(fit1)
residual.plot(fit1)
fit1.2<-lm(price~.-dc,data=data)
summary(fit1.2)
# 2nd fit
fit2<-lm(price~.-dc,data=data)
summary(fit2)

residual.plot(fit2)
boxcox(fit2,plotit=T,lambda=seq(-.5,1.5,len=100))

par(mfrow=c(1,2))
h<-hatvalues(fit2)
plot(h,ylab="leverage")
identify(h,n=1)

cd<-cooks.distance(fit2)
plot(h/(1-h),cd,ylab="Cook Statistic")
identify(h/(1-h),cd,n=1)

# test outliers
library(car)
outlierTest(fit2)

#AIC
#(1)forward 
step(lm(price~1,data=data),scope=list(upper=formula(fit1)),direction="forward") 
#(2)backward
step(fit1,direction="backward")
#(3)both
step(fit1,direction="both")

#Mallow's Cp & adjusted R-square
library(leaps) 
a<-regsubsets(formula(fit1),data=data,method="exhaustive")
(rs<-summary(a))
par(mfrow=c(1,2))
plot(x=2:5,rs$cp,xlab="p (Number of parameters)",ylab="Cp statistic", main="Cp statistic vs p")
abline(0,1)
plot(2:5,rs$adjr2,xlab="p (number of parameters)",ylab="Adjusted R-square",
     main="Adjusted R-square")
#partial residual plots
par(mfrow=c(1,3))
pr<-residuals(fit2)+coef(fit2)[1]*data[,1] 
plot(data[,1],pr,xlab=names(data)[1], ylab="Partial residuals",main="(a)")
abline(0,coef(fit2)[1]) 
lines(lowess(data[,1],pr),col="red",lty=2)
pr<-residuals(fit2)+coef(fit2)[2]*data[,2] 
plot(data[,2],pr,xlab=names(data)[2], ylab="Partial residuals",main="(b)")
abline(0,coef(fit2)[2])
lines(lowess(data[,2],pr),col="red",lty=2)
pr<-residuals(fit2)+coef(fit2)[4]*data[,4] 
plot(data[,4],pr,xlab=names(data)[4], ylab="Partial residuals",main="(c)") 
abline(0,coef(fit2)[4]) 
lines(lowess(data[,4],pr),col="red",lty=2)
#add higher order terms of age
fit2.1<-update(fit2,.~.+I(age^2))
summary(fit2.1)
#residual plots
residual.plot(fit2.1)
#influential points

#plot for leverages and Cook's statistic
par(mfrow=c(1,2))
h<-hatvalues(fit2.1)
plot(h,ylab="leverage")
identify(h,n=1)

cd<-cooks.distance(fit2.1)
plot(h/(1-h),cd,ylab="Cook Statistic")
identify(h/(1-h),cd,n=1)
#plot change in coef
par(mfrow=c(2,2))
fit2.1inf<-influence(fit2.1)
#plot change in size coef
plot(fit2.1inf$coef[,2],ylab="Change in size coef",main="(a)")
abline(h=0)
#plot change in dt coef
plot(fit2.1inf$coef[,4],ylab="Change in dt coef",main="(b)")
abline(h=0)
#plot change in age coef
plot(fit2.1inf$coef[,3],ylab="Change in age coef",main="(c)") 
identify(fit2.1inf$coef[,3],n=1)
abline(h=0)
#plot change in age^2 coef
plot(fit2.1inf$coef[,5],ylab="Change in age^2 coef",main="(d)") 
identify(fit2.1inf$coef[,5],n=1)
abline(h=0)

# 3rd fit
fit2.2<-lm(price~size+age+dt+I(age^2),data=data[-51,])
summary(fit2.2)
residual.plot(fit2.2)

par(mfrow=c(1,2))
h<-hatvalues(fit2.2)
plot(h,ylab="leverage")
identify(h,n=1)

cd<-cooks.distance(fit2.2)
plot(h/(1-h),cd,ylab="Cook Statistic")
identify(h/(1-h),cd,n=1)
