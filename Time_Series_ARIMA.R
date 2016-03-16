data<-read.csv(file.choose(),header=F,col.names = c("Virtual Machines"))
data

data<-ts(data,frequency = 12)

plot(data, xlab="Time", ylab = "Virtual Machines")



##Difference data to make data stationary on mean

plot(diff(data),ylab="Differenced Virtual Mahines")

##log transform data to make data stationary on variance

plot(log10(data),ylab="Log (Virtual Mahines)")

##Difference log transform data to make data stationary on both mean and variance

plot(diff(log10(data)),ylab="Differenced Log (Virtual Mahines)")

###Plot ACF and PACF to identify potential AR and MA model

par(mfrow = c(1,2))
acf(ts(diff(log10(data))),main="ACF Diff log(Virtual Mahines)")
pacf(ts(diff(log10(data))),main="PACF Diff log(Virtual Mahines)")


##Augmented Dickey-Fuller Test
library(tseries)
adf.test(data, alternative = c("stationary", "explosive"),k = trunc((length(data)-1)^(1/3)))


##kpss Test
kpss.test(data, null = c("Level", "Trend"), lshort = TRUE)

###Fit a linear model with time series components
par(mfrow = c(1,1))
library(forecast)
fit.ex4 <- tslm(data ~ trend)
f <- forecast(fit.ex4, h=5,level=c(80,95))
plot(f, ylab="Virtaul Mahines",xlab="Time")
lines(fitted(fit.ex4),col="blue")
summary(fit.ex4)


###histogram and QQ plot and lag plot

hist(data, prob=TRUE, 12)      
lines(density(data))      
qqnorm(data)               
qqline(data) 

##lag plot
library(astsa)
v<-diff(log10(data))
lag1.plot(v, 9)

##Identification of best fit ARIMA model
library(forecast)
af<- auto.arima(log10(data),approximation=FALSE,trace=FALSE)
summary(af)
par(mfrow = c(1,1))
##forecasting from above data
fit <- arima(data, order=c(1,0,1), list(order=c(0,1,1), period=12))
fore <- predict(fit, n.ahead=30)
U <- fore$pred + 2*fore$se
L <- fore$pred - 2*fore$s
ts.plot(data, fore$pred, U, L, col=c(1,2,4,4), lty = c(1,1,2,2))
legend("topleft", c("Actual", "Forecast", "Error Bounds (95% Confidence)"), 
       col=c(1,2,4), lty=c(1,1,2))
v<-ts(fore$pred, frequency = 12)
v
accuracy(v,data,test=NULL,d=NULL,D=NULL)
fit7<-forecast(fit,h=1)
list(fit7)
c<-Box.test(fit7$residuals,type="Ljung-Box")
c

##Rechecking with acf and pacf
par(mfrow=c(1,2))
acf(ts(fit$residuals))
pacf(ts(fit$residuals))

####Since there are no spikes outside the insignificant zone for both ACF and PACF plots we can
##conclude that residuals are random with no information or juice in them. Hence our ARIMA
##model is working fine.

