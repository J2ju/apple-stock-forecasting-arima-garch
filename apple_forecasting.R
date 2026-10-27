library(tseries)
library(FinTS)
library(lubridate)
library(forecast)
library(readxl)
library(PerformanceAnalytics)
library(rugarch)
library(fpp2)
library(seastests)
library(car)

#Importing data 
AAPL <- read.csv("data/AAPL.csv")  
AAPL

#Attaching and converting Open prices into time series 
attach(AAPL)
AAPL.ts =ts(Open, 
            freq=365.25/7, 
            start=decimal_date(ymd("2013-3-18"))
            )
AAPL.ts
summary(AAPL.ts)
autoplot(AAPL.ts,main = "Time Series Plot of Apple Inc Openning Stock Prices",
         ylab = "Prices in USD", xlab = "Weeks")

#Plotting the variances of segments of data
rolling_var <- rollapply(AAPL.ts, width = 52, FUN = var, align = "right", by = 1)
plot(rolling_var, type = "l",main="Variance Before Transformation")

#Splitting Into Test and Train Sets
AAPL.train <- AAPL.ts[1:503] %>% 
  ts(frequency = 365.25/7 , start=decimal_date(ymd("2013-01-01")))
autoplot(AAPL.train,main="Train Time Series Plot of Apple Inc Openning Stock Prices",
         xlab = "Weeks", ylab = "Prices in USD")

AAPL.test <- AAPL.ts[504:523] %>% 
  ts(frequency = 365.25/7 , start=decimal_date(ymd("2022-01-17")))
autoplot(AAPL.test,main="Test Time Series Plot of Apple Inc Openning Stock Prices",
         xlab = "Weeks", ylab = "Prices in USD")

#Transformation of train data
lambdaValue = BoxCox.lambda(AAPL.train)
trAAPL.train = BoxCox(AAPL.train,lambda = lambdaValue)

#Plotting the variances of segments of data after transformation
rolling_var <- rollapply(trAAPL.train, width = 52, FUN = var, align = "right", by = 1)
plot(rolling_var, type = "l", main="Variance After Transformation")

#Diagrammatic description of  the train data
Acf(trAAPL.train,lag = 100, main="Autocorrelation Function Plot")
Pacf(trAAPL.train,main="Partial Autocorrelation Function Plot")
hist(trAAPL.train)

#Unit Root Tests at Level
adf.test(trAAPL.train)
kpss.test(trAAPL.train)
PP.test(trAAPL.train)

#Test for seasonality
welch(trAAPL.train)

#Differencing 
FirstDiff = diff(trAAPL.train)
autoplot(FirstDiff,main="First Order Difference of Apple Inc Openning Stock Prices",
         xlab = "Weeks", ylab = "Prices in USD")
#Unit Root Tests of first order difference
adf.test(FirstDiff)
kpss.test(FirstDiff)
PP.test(FirstDiff)

#Model Selection By Graph
Acf(FirstDiff,main="Autocorrelation Function Plot of differenced data")
Pacf(FirstDiff,main="Partial Autocorrelation Function Plot of differenced data")


#Data is not stationary, number of differencing needed 
ndiffs(AAPL.train)


#Automatic Search for Suitable model
auto.arima(trAAPL.train,stepwise = FALSE,approximation = FALSE,trace = TRUE)

#Fitting the model obtained
bestModel = Arima(trAAPL.train, order =c(2,1,2),include.drift = TRUE)
bestModel


#Competing/Candidate Models
cModel1 =Arima(trAAPL.train,order = c(2,1,2),seasonal = c(0,0,1),include.drift = TRUE)
cModel2 =Arima(trAAPL.train,order = c(2,1,0),include.drift = TRUE)
cModel3 =Arima(trAAPL.train,order = c(1,1,1),include.drift = TRUE)
cModel4 =Arima(trAAPL.train,order = c(2,1,2),seasonal = c(1,0,0),include.drift = TRUE)
#auto.arima(AAPL.train,stepwise = FALSE, approximation = FALSE, trace = TRUE)
cModel1
cModel2
cModel3
cModel4



#Forecasting the test set
tForecast = forecast(bestModel, h=20)
trForecast=InvBoxCox(tForecast$mean,lambda = lambdaValue)
autoplot(tForecast)

#Forecast for Competing Models
cm1Forecast = forecast(cModel1,h=20)
cm2Forecast = forecast(cModel2,h=20)
cm3Forecast = forecast(cModel3,h=20)
cm4Forecast = forecast(cModel4,h=20)

#Inverse of Transformation
tcm1Forecast=InvBoxCox(cm1Forecast$mean,lambda = lambdaValue)
tcm2Forecast=InvBoxCox(cm2Forecast$mean,lambda = lambdaValue)
tcm3Forecast=InvBoxCox(cm3Forecast$mean,lambda = lambdaValue)
tcm4Forecast=InvBoxCox(cm4Forecast$mean,lambda = lambdaValue)

#Checking forecasting performance
accuracy(AAPL.test,trForecast)
accuracy(tcm1Forecast,AAPL.test)
accuracy(tcm2Forecast,AAPL.test)
accuracy(tcm3Forecast,AAPL.test)
accuracy(tcm4Forecast,AAPL.test)

#Model Diagnostics
checkresiduals(bestModel,lags =30)
checkresiduals(cModel1)
checkresiduals(cModel2)
checkresiduals(cModel3)
checkresiduals(cModel4)

#Extracting residual from the best model
Residual = bestModel$residuals

#Diagnostics on residuals extracted
t.test(Residual)
Acf(Residual,lag=30)
shapiro.test(Residual)

#Fitting model to the entire data
PriceModel = Arima(AAPL.ts, order = c(2,1,2),include.drift = TRUE)


#Future Forecasting
pForecast = forecast(PriceModel, h=20)
pForecast
autoplot(pForecast,ylab="Price (USD)",xlab="Weeks")


#VOLATILITY

#Test for Arch Effect
ArchTest(AAPL.ts)

#Calculating Returns
Returns=CalculateReturns(ts(AAPL.ts),method="log")
autoplot(Returns, col= "blue")
Returns = Returns[-1]
Returns = ts(Returns,frequency = 365.25/7 , start=decimal_date(ymd("2013-3-25")))

shapiro.test(Returns)

meanmodel1 = Arima(Returns,order =c(1,0,1))
meanmodel2 = Arima(Returns,order =c(1,0,2))
meanmodel3 = Arima(Returns,order =c(2,0,1))
meanmodel4 = Arima(Returns,order =c(2,0,2))
meanmodel5 = Arima(Returns,order =c(3,0,1))
meanmodel6 = Arima(Returns,order =c(3,0,2))

#According to literature
bestmeanmodel = Arima(Returns,order =c(0,0,0))

meanmodel1
meanmodel2
meanmodel3
meanmodel4
meanmodel5
meanmodel6


#Getting a mean model for the returns
meanModel = auto.arima(Returns, stepwise = FALSE, approximation = FALSE)

#Getting the Best Garch model (auto.arima of Garch)
garch(Returns,grad="numerical",trace=FALSE)

#Fitting Garch models

#SGARCH model specification and fitting
garchModel= ugarchspec(variance.model=list(model = "sGARCH",garchOrder=c(1,1)),
                       mean.model = list(armaOrder=c(0,0)),
                       distribution.model = "std"
                       )
gModel= ugarchfit(spec=garchModel, data=Returns, solver.control=list(trace = 1))
gModel

#EGARCH model specification and fitting
egarchModel= ugarchspec(variance.model=list(model = "eGARCH",garchOrder=c(1,1)),
                       mean.model = list(armaOrder=c(0,0)),
                       distribution.model = "std"
)
egModel= ugarchfit(spec=egarchModel, data=Returns, solver.control=list(trace = 1))
egModel

#GJR-GARCH model specification and fitting
gjrgarchModel= ugarchspec(variance.model=list(model = "gjrGARCH",garchOrder=c(1,1)),
                        mean.model = list(armaOrder=c(0,0)),
                        distribution.model = "std"
)
gjrgModel= ugarchfit(spec=gjrgarchModel, data=Returns, solver.control=list(trace = 1))
gjrgModel

#apGARCH model specification and fitting
aparchModel= ugarchspec(variance.model=list(model = "apARCH",garchOrder=c(1,1)),
                          mean.model = list(armaOrder=c(0,0)),
                          distribution.model = "std"
)
apModel= ugarchfit(spec=aparchModel, data=Returns, solver.control=list(trace = 1))
apModel

#Diagnostic plots and news impact curve
plot(gModel, which=2)
plot(gModel, which=3)
plot(gModel, which=8)
plot(gModel, which=9)
plot(gModel, which=10)
plot(gModel, which=12)

#Volatility Forecast
vForecast = ugarchforecast(gModel,n.ahead=20)
vForecast
plot(vForecast)

#Creating a data frame for predicted price and predicted volatility
combined = data.frame(pForecast$mean,sigma(vForecast))
colnames(combined) <- c("Forecasted Price","Volatility")
combined








