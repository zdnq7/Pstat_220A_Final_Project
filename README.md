# Pstat_220A_Final_Project
Pstat_220A_Advanced_Statistical_Methods


properties <- read.table("http://www.pstat.ucsb.edu/faculty/yuedong/classes/data/property.txt",header=TRUE)
properties
attach(properties)
par(mfrow=c(1,4))
#histogram of variables
hist(size)
hist(age)
hist(dc)
hist(dt)
#scatter plot 
pairs(properties)
#fit the model
fit1 <- lm(price ~ size+age+dc+dt)
summary(fit1)
fit2 <- glm(price ~ size+age+dt)
summary(fit2)
#diagnostics
par(mfrow=c(2,2))
library(boot)
glm.diag.plots(fit2)
#boxcox
library(MASS)
boxcox(fit2,plotit=T,lambda=seq(-.5,1.5,len=100))

#partial residual plots
par(mfrow=c(1,3))
pr1 <- residuals(fit2)+coef(fit2)[2]*size
plot(size,pr1,xlab="Size",ylab="Partial residuals")
abline(0,coef(fit2)[2])
lines(lowess(size,pr1),col="red",lty=2)
title("Partial residual plot for size")

pr2 <- residuals(fit2)+coef(fit2)[3]*age
plot(age,pr2,xlab="Age",ylab="Partial residuals")
abline(0,coef(fit2)[3])
lines(lowess(age,pr2),col="red",lty=2)
title("Partial residual plot for age")

pr3 <- residuals(fit2)+coef(fit2)[4]*dt
plot(dt,pr3,xlab="dt",ylab="Partial residuals")
abline(0,coef(fit2)[4])
lines(lowess(dt,pr3),col="red",lty=2)
title("Partial residual plot for dt")
#plot of leverages and cook's statistic
par(mfrow=c(1,2))
h <- hatvalues(fit2)
cd <- cooks.distance(fit2)
plot(h/(1-h),cd,ylab="Cook statistic")
identify(h/(1-h),cd,n=1)
plot(h,ylab="Leverage")
identify(h,n=1)
#confidence band
par(mfrow=c(1,2))
grid <- seq(min(size),max(size),len=100)
f <- lm(price~size)
p1 <- predict(f,newdata=data.frame(size=grid),se=T,interval="confidence")
p2 <- predict(f,newdata=data.frame(size=grid),se=T,interval="prediction")
matplot(grid,p1$fit,lty=c(1,2,2),col=c(1,2,2),type="l",xlab="Size",ylab="price",ylim=range(p1$fit,p2$fit,price))
points(size,price,cex=.5)
title("Prediction of mean response")
matplot(grid,p2$fit,lty=c(1,2,2),col=c(1,2,2),type="l",xlab="Size",ylab="price",ylim=range(p1$fit,p2$fit,price))
points(size,price,cex=.5)
title("Prediction of future observations")
#confidence region
library(car)
confidenceEllipse(fit2,c(3,4))
abline(v=confint(fit2)[3,],lty=2)
abline(h=confint(fit2)[4,],lty=2)
#confidence intervals
confint(fit2)
