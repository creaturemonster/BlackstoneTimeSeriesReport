#PART ONE
#Load the Quantmode package 
library(quantmod)

#select a stock that has significant autocorrelations
BX=getSymbols('BX',src='yahoo',from='2012-12-01',to='2015-12-01',auto.assign = FALSE)
head(BX)

# use adjusted closing price to compute the daily log return
BX.adjusted <- BX$BX.Adjusted
BX.rtn=dailyReturn(BX.adjusted,type='log')
head(BX.rtn)
BX.open <- BX$BX.Open
BX.close <- BX$BX.Close

#check the log return series using both acf plot and Box.test.
acf(BX.rtn)
Box.test(BX.rtn)

#1.divide the dataset into two parts
len<-length(BX.adjusted)
training <-BX.rtn[1:(len-125)]
testing <-BX.rtn[(len-125):len]

#2.auto.arima as Model1
Model1<-auto.arima(training)
Model1
cor1=0 # initiate the number of correctness equal to 0
for (i in 0:124) {
  model<-arima(BX.rtn[1:(len-125+i)],order=c(2,0,2))
  fo<-forecast(model,h=1)
  fo$mean
  if ((fo$mean<=0 && BX.rtn[len-125+i+1]<=0) || (fo$mean>=0 && BX.rtn[len-125+i+1]>=0)) {
    cor1 = cor1 + 1
  }
}
pro_cor1=cor1/125
pro_cor1


#3.fit MA(q) as Model2 and AR(p) as Model3
acf(training)
pacf(training)

#fit MA(4) as Model2 based on the acf plot
Model2 <-arima(training,order=c(0,0,4))
cor2=0 # initiate the number of correctness equal to 0
for (i in 0:124) {
  model<-arima(BX.rtn[1:(len-125+i)],order=c(0,0,4))
  fo<-forecast(model,h=1)
  fo$mean
  if ((fo$mean<=0 && BX.rtn[len-125+i+1]<=0) || (fo$mean>=0 && BX.rtn[len-125+i+1]>=0)) {
    cor2 = cor2 + 1
  }
}
pro_cor2=cor2/125
pro_cor2

#fit AR(4) as Model3 based on the pacf plot
Model3 <-arima(training,order=c(4,0,0))
cor3=0 # initiate the number of correctness equal to 0
for (i in 0:124) {
  model<-arima(BX.rtn[1:(len-125+i)],order=c(4,0,0))
  fo<-forecast(model,h=1)
  fo$mean
  if ((fo$mean<=0 && BX.rtn[len-125+i+1]<=0) || (fo$mean>=0 && BX.rtn[len-125+i+1]>=0)) {
    cor3 = cor3 + 1
  }
}
pro_cor3=cor3/125
pro_cor3

#4account investment
account1=100000 #for model1
for (i in 0:124) {
  model<-arima(BX.rtn[1:(len-125+i)],order=c(2,0,2))
  fo<-forecast(model,h=1)
  fo$mean
  if (fo$mean>=0) {
    account1=(account1/(as.numeric(BX.open[len-125+i+1])))*(as.numeric(BX.close[len-125+i+1]))*(1-0.00003)-account1*0.00003
  } else {
    account1=account1+(account1/(as.numeric(BX.open[len-125+i+1])))*(as.numeric(BX.open[len-125+i+1]-as.numeric(BX.close[len-125+i+1])))-account1*0.00003-(account1/(as.numeric(BX.open[len-125+i+1])))*(as.numeric(BX.close[len-125+i+1]))*0.00003
  }
}
account1

account2=100000 #for model2
for (i in 0:124) {
  model<-arima(BX.rtn[1:(len-125+i)],order=c(0,0,4))
  fo<-forecast(model,h=1)
  fo$mean
  if (fo$mean>=0) {
    account2=(account2/(as.numeric(BX.open[len-125+i+1])))*(as.numeric(BX.close[len-125+i+1]))*(1-0.00003)-account2*0.00003
  } else {
    account2=account2+(account2/(as.numeric(BX.open[len-125+i+1])))*(as.numeric(BX.open[len-125+i+1]-as.numeric(BX.close[len-125+i+1])))-account2*0.00003-(account2/(as.numeric(BX.open[len-125+i+1])))*(as.numeric(BX.close[len-125+i+1]))*0.00003
  }
}
account2

account3=100000 #for model3
for (i in 0:124) {
  model<-arima(BX.rtn[1:(len-125+i)],order=c(4,0,0))
  fo<-forecast(model,h=1)
  fo$mean
  if (fo$mean>=0) {
    account3=(account3/(as.numeric(BX.open[len-125+i+1])))*(as.numeric(BX.close[len-125+i+1]))*(1-0.00003)-account3*0.00003
  } else {
    account3=account3+(account3/(as.numeric(BX.open[len-125+i+1])))*(as.numeric(BX.open[len-125+i+1]-as.numeric(BX.close[len-125+i+1])))-account3*0.00003-(account3/(as.numeric(BX.open[len-125+i+1])))*(as.numeric(BX.close[len-125+i+1]))*0.00003
  }
}
account3

#5.ARMA+GARCH model
library(fGarch)
#Model1
acf(Model1$residuals)
acf(Model1$residuals^2)
Box.test(Model1$residuals)
Box.test(Model1$residuals^2)
Garch1 <-garchFit(formula=~arma(2,2)+garch(1,1), data=training, trace=F)
GARCH1 <-garchFit(formula=~garch(1,1), data=training, trace=F)
GARCH1
summary(GARCH1)

Garch1_rtn = function (k) {
  acct=100000
  for (i in 0:124) {
    model_rtn<-arima(BX.rtn[1:(len-125+i)],order=c(2,0,2))
    fo<-forecast(model_rtn,h=1)
    fo$mean
    model_vol<-garchFit(~garch(1,1),data=BX.rtn[1:(len-125+i)],trace=F)
    vol<-predict(model_vol,1)
    vol$meanForecast
    if (fo$mean>= mean(BX.rtn[(len-125+i-20):(len-125+i)])+k*vol$meanForecast) {
      acct=(acct/(as.numeric(BX.open[len-125+i+1])))*(as.numeric(BX.close[len-125+i+1]))*(1-0.00003)-acct*0.00003
    } else if (fo$mean<=mean(BX.rtn[(len-125+i-20):(len-125+i)])-k*vol$meanForecast){
      acct=acct+(acct/(as.numeric(BX.open[len-125+i+1])))*(as.numeric(BX.open[len-125+i+1]-as.numeric(BX.close[len-125+i+1])))-acct*0.00003-(acct/(as.numeric(BX.open[len-125+i+1])))*(as.numeric(BX.close[len-125+i+1]))*0.00003
    }
  }
  acct
}

Garch1_rtn(0.05)
Garch1_rtn(0.1)
Garch1_rtn(0.15)
Garch1_rtn(0.2)
Garch1_rtn(0.25)
Garch1_rtn(0.3)
Garch1_rtn(0.35)
Garch1_rtn(0.4)
Garch1_rtn(0.45)
Garch1_rtn(0.5)

#Model2
acf(Model2$residuals)
acf(Model2$residuals^2)
Box.test(Model2$residuals)
Box.test(Model2$residuals^2)
Garch2 <-garchFit(formula=~arma(0,4)+garch(1,1), data=training, trace=F)
Garch2
summary(Garch2)
summary(Model2)
#According to the summary output, ma1, ma2 and ma3 are insignificant
#But the porfolio will have a higher value if we don't fix them to 0

Garch2_rtn = function (k) {
  acct=100000
  for (i in 0:124) {
    model_rtn<-arima(BX.rtn[1:(len-125+i)],order=c(0,0,4))
    fo<-forecast(model_rtn,h=1)
    fo$mean
    model_vol<-garchFit(~garch(1,1),data=BX.rtn[1:(len-125+i)],trace=F)
    vol<-predict(model_vol,1)
    vol$meanForecast
    if (fo$mean>= mean(BX.rtn[(len-125+i-20):(len-125+i)])+k*vol$meanForecast) {
      acct=(acct/(as.numeric(BX.open[len-125+i+1])))*(as.numeric(BX.close[len-125+i+1]))*(1-0.00003)-acct*0.00003
    } else if (fo$mean<=mean(BX.rtn[(len-125+i-20):(len-125+i)])-k*vol$meanForecast){
      acct=acct+(acct/(as.numeric(BX.open[len-125+i+1])))*(as.numeric(BX.open[len-125+i+1]-as.numeric(BX.close[len-125+i+1])))-acct*0.00003-(acct/(as.numeric(BX.open[len-125+i+1])))*(as.numeric(BX.close[len-125+i+1]))*0.00003
    }
  }
  acct
}

Garch2_rtn(0.05)
Garch2_rtn(0.1)
Garch2_rtn(0.15)
Garch2_rtn(0.2)
Garch2_rtn(0.25)
Garch2_rtn(0.3)
Garch2_rtn(0.35)
Garch2_rtn(0.4)
Garch2_rtn(0.45)
Garch2_rtn(0.5)

#Model3
acf(Model3$residuals)
acf(Model3$residuals^2)
Box.test(Model3$residuals)
Box.test(Model3$residuals^2)
Garch3 <-garchFit(formula=~arma(4,0)+garch(1,1), data=training, trace=F)
Garch3
summary(Garch3)
summary(Model3)
#According to the summary output, ar1, ar2 and ar3 are insignificant
#But the porfolio will have a higher value if we don't fix them to 0

Garch3_rtn = function (k) {
  acct=100000
  for (i in 0:124) {
    model_rtn<-arima(BX.rtn[1:(len-125+i)],order=c(4,0,0))
    fo<-forecast(model_rtn,h=1)
    fo$mean
    model_vol<-garchFit(~garch(1,1),data=BX.rtn[1:(len-125+i)],trace=F)
    vol<-predict(model_vol,1)
    vol$meanForecast
    if (fo$mean>= mean(BX.rtn[(len-125+i-20):(len-125+i)])+k*vol$meanForecast) {
      acct=(acct/(as.numeric(BX.open[len-125+i+1])))*(as.numeric(BX.close[len-125+i+1]))*(1-0.00003)-acct*0.00003
    } else if (fo$mean<=mean(BX.rtn[(len-125+i-20):(len-125+i)])-k*vol$meanForecast){
      acct=acct+(acct/(as.numeric(BX.open[len-125+i+1])))*(as.numeric(BX.open[len-125+i+1]-as.numeric(BX.close[len-125+i+1])))-acct*0.00003-(acct/(as.numeric(BX.open[len-125+i+1])))*(as.numeric(BX.close[len-125+i+1]))*0.00003
    }
  }
  acct
}

Garch3_rtn(0.05)
Garch3_rtn(0.1)
Garch3_rtn(0.15)
Garch3_rtn(0.2)
Garch3_rtn(0.25)
Garch3_rtn(0.3)
Garch3_rtn(0.35)
Garch3_rtn(0.4)
Garch3_rtn(0.45)
Garch3_rtn(0.5)


#PART TWO
#### acct compare the predicted return to the average return for the past 20 days(1 months) plus parameter k times the predicted volatility
#### the larger the range, the more money we invest
acct=100000 
for (i in 0:124) {
  model_rtn<-arima(BX.rtn[1:(len-125+i)],order=c(2,0,2))
  fo<-forecast(model_rtn,h=1)
  fo$mean
  model_vol<-garchFit(~garch(1,1),data=BX.rtn[1:(len-125+i)],trace=F)
  vol<-predict(model_vol,1)
  vol$meanForecast
  if (fo$mean>= mean(BX.rtn[(len-125+i-20):(len-125+i)])+0.05*vol$meanForecast) {
    acct=0.5*acct+(0.5*acct/(as.numeric(BX.open[len-125+i+1])))*(as.numeric(BX.close[len-125+i+1]))*(1-0.00003)-acct*0.5*0.00003
  } else if (fo$mean>= mean(BX.rtn[(len-125+i-20):(len-125+i)])+0.1*vol$meanForecast) {
    acct=0.3*acct+(0.7*acct/(as.numeric(BX.open[len-125+i+1])))*(as.numeric(BX.close[len-125+i+1]))*(1-0.00003)-acct*0.7*0.00003
  } else if (fo$mean>= mean(BX.rtn[(len-125+i-20):(len-125+i)])+0.15*vol$meanForecast) {
    acct=0.1*acct+(0.9*acct/(as.numeric(BX.open[len-125+i+1])))*(as.numeric(BX.close[len-125+i+1]))*(1-0.00003)-acct*0.9*0.00003
  } else if (fo$mean>= mean(BX.rtn[(len-125+i-20):(len-125+i)])+0.2*vol$meanForecast) {
    acct=(acct/(as.numeric(BX.open[len-125+i+1])))*(as.numeric(BX.close[len-125+i+1]))*(1-0.00003)-acct*0.00003
  } else if (fo$mean<=mean(BX.rtn[(len-125+i-20):(len-125+i)])-0.05*vol$meanForecast){
    acct=acct+(0.5*acct/(as.numeric(BX.open[len-125+i+1])))*(as.numeric(BX.open[len-125+i+1]-as.numeric(BX.close[len-125+i+1])))-acct*0.5*0.00003-(acct*0.5/(as.numeric(BX.open[len-125+i+1])))*(as.numeric(BX.close[len-125+i+1]))*0.00003
  } else if (fo$mean<=mean(BX.rtn[(len-125+i-20):(len-125+i)])-0.1*vol$meanForecast){
    acct=acct+(0.7*acct/(as.numeric(BX.open[len-125+i+1])))*(as.numeric(BX.open[len-125+i+1]-as.numeric(BX.close[len-125+i+1])))-acct*0.7*0.00003-(acct*0.7/(as.numeric(BX.open[len-125+i+1])))*(as.numeric(BX.close[len-125+i+1]))*0.00003
  } else if (fo$mean<=mean(BX.rtn[(len-125+i-20):(len-125+i)])-0.15*vol$meanForecast){
    acct=acct+(0.9*acct/(as.numeric(BX.open[len-125+i+1])))*(as.numeric(BX.open[len-125+i+1]-as.numeric(BX.close[len-125+i+1])))-acct*0.9*0.00003-(acct*0.9/(as.numeric(BX.open[len-125+i+1])))*(as.numeric(BX.close[len-125+i+1]))*0.00003
  } else if (fo$mean<=mean(BX.rtn[(len-125+i-20):(len-125+i)])-0.2*vol$meanForecast){
    acct=acct+(acct/(as.numeric(BX.open[len-125+i+1])))*(as.numeric(BX.open[len-125+i+1]-as.numeric(BX.close[len-125+i+1])))-acct*0.00003-(acct/(as.numeric(BX.open[len-125+i+1])))*(as.numeric(BX.close[len-125+i+1]))*0.00003
  }
}
acct

