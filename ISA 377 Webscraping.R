library(quantmod)
library(xts)
install.packages("forecast")
library(forecast)
install.packages('imputeTS')
library(imputeTS)

# sp500
env <- new.env()
getSymbols("^GSPC", env = env, src = "yahoo", from = as.Date("1986-01-01"), to = as.Date("2017-03-31"))
gspc <- env$GSPC
sp500_close <- as.ts(gspc$GSPC.Close)


# dowJ
getSymbols("^DJI", env = env, src = "yahoo", from = as.Date("1986-01-01"), to = as.Date("2017-03-31"))
dowJ <- env$DJI
dowJ_close <- as.ts(dowJ$DJI.Close)

# NASDAQ
getSymbols("^IXIC", env = env, src = "yahoo", from = as.Date("1986-01-01"), to = as.Date("2017-03-31"))
nasdaq <- env$IXIC
nasdaq_close <- as.ts(nasdaq$IXIC.Close)

# Bond rates - daily
getSymbols("DGS1", src="FRED", from = as.Date("1986-01-01"), to = as.Date("2017-03-31"))
getSymbols("DGS10",src="FRED", from = as.Date("1986-01-01"), to = as.Date("2017-03-31"))
getSymbols("DGS30",src="FRED", from = as.Date("1986-01-01"), to = as.Date("2017-03-31"))

# Currency rates - daily
getSymbols('EUR=x',src='yahoo', from = as.Date("1986-01-01"), to = as.Date("2017-03-31"))
eur_close <- as.ts(`EUR=X`$`EUR=X.Close`)
getSymbols('GBP=x',src='yahoo', from = as.Date("1986-01-01"), to = as.Date("2017-03-31"))
gbp_close <- as.ts(`GBP=X`$`GBP=X.Close`)
getSymbols('JPY=x',src='yahoo', from = as.Date("1986-01-01"), to = as.Date("2017-03-31"))
jpy_close <- as.ts(`JPY=X`$`JPY=X.Close`)
getSymbols('CAD=x',src='yahoo', from = as.Date("1986-01-01"), to = as.Date("2017-03-31"))
cad_close <- as.ts(`CAD=X`$`CAD=X.Close`)
getSymbols('MXN=x',src='yahoo', from = as.Date("1986-01-01"), to = as.Date("2017-03-31"))
mxn_close <- as.ts(`MXN=X`$`MXN=X.Close`)

# housing starts - monthly
getSymbols('HOUST',src='FRED')

# unemployment rate - monthly
unemp <- getSymbols('UNRATE',src='FRED')

# AAA bonds yield
getSymbols('BAMLCC0A1AAATRIV',src='FRED')

# consumer price index
getSymbols('CPIAUCSL',src='FRED')

# real gross domestic product
getSymbols('A191RL1Q225SBEA',src='FRED')

# 30-year fixed mortgage rates
getSymbols('MORTGAGE30US',src='FRED')

# real median household income
getSymbols('MEHOINUSA672N',src='FRED')

#https://fred.stlouisfed.org/series/MEHOINUSA672N

getSymbols("AAPL", env = env, src = "yahoo", from = as.Date("1986-01-01"), to = as.Date("2017-03-31"))
aapl <- env$AAPL
aapl_close <- as.ts(aapl$AAPL.Close)

# complete data frame
dabber <- ts.union(aapl_close, sp500_close, dowJ_close, nasdaq_close, DGS1, DGS10, DGS30, 
                eur_close, gbp_close, jpy_close, cad_close, mxn_close, HOUST,
                UNRATE, BAMLCC0A1AAATRIV, CPIAUCSL, A191RL1Q225SBEA, MORTGAGE30US,
                MEHOINUSA672N)

# impute missing values for time series
dabber <- na.interpolation(dabber)

# time series model
cool <- tslm(aapl_close ~ sp500_close + dowJ_close + nasdaq_close + DGS1 +
               DGS10 + DGS30 + eur_close + gbp_close + jpy_close +
               cad_close + mxn_close + HOUST + UNRATE + BAMLCC0A1AAATRIV +
               CPIAUCSL + A191RL1Q225SBEA + MORTGAGE30US +
               MEHOINUSA672N, data=dabber)
summary(cool)

# plot apple stock over time
plot(aapl_close)

# take stock splits into consideration
getSplits('AAPL')

trial <- as.data.frame(dabber)
plot(trial$HOUST)

# http://www2.isye.gatech.edu/~sahmed/isye6669/notes/portfolio