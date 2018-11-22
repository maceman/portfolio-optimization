library(quantmod)
library(xts)
install.packages("forecast")
library(forecast)
install.packages('imputeTS')
library(imputeTS)
library(xts)
install.packages("RCurl")
library(RCurl)
install.packages("downloader")
library(downloader)
library(ggplot2)
library(reshape2)
install.packages("corrplot")
library(corrplot)
library(forecast)


# READ: http://www2.isye.gatech.edu/~sahmed/isye6669/notes/portfolio

# FRED: https://fred.stlouisfed.org/series/MEHOINUSA672N

# tech/fund analysis: http://www.investopedia.com/university/technical/techanalysis2.asp


today<-Sys.Date()
begin<- as.POSIXlt(today)
begin$year<-begin$year-15
begin<-as.Date(begin)

from.dat <- as.Date(begin, format="%y/%m/%yd")
to.dat <- as.Date(today, format="%y/%m/%d")
setInternet2(TRUE) 

macro<-c("CPIAUCL","A191RL1Q225SBEA","INDPRO","DGS10","DGS1","DGS3","UNRATE","FEDFUNDS","AWHAETP","M2SL","T10YIEM",
         "M1SL","PCE","T5YIFRM","BUSLOANS","EXUSEU","UMCSENT","HOUST","CSUSHPINSA","DSPIC96","RECPROUSM156N",
         "LFWA64TTUSM647S","EXCHUS","MPRIME","EXCAUS","MTSDS133FMS","EXUSUK","M0892AUSM156SNBR","TOTALSA",
         "EXUSAL","EXMXUS","RSXFS","MORTG","BOPGSTB","PI","LNS11300002","ISRATIO","TTLCONS","USEPUINDXM","LNS14027662",
         "TCDSL","SPDYNCBRTINUSA")  

for (i in 42) {
  getSymbols(macro[i], src = "FRED", from = from.dat, to  = to.dat,env=parent.frame())
}

getSymbols("CPIAUCL", src = "FRED", from = from.dat, to  = to.dat,env=parent.frame())
getSymbols(macro, src = "FRED", from = from.dat, to  = to.dat,env=parent.frame())
#macro<- macro[paste0(from.dat,"/",to.dat)]

#### environment
env <- new.env()

### 1.apple
getSymbols("AAPL", env = env, src = "yahoo", from = as.Date("1986-01-01"), to = as.Date("2017-03-31"))
aapl <- env$AAPL
aapl_close <- as.ts(aapl$AAPL.Close)

### 2.coca cola
getSymbols("KO", env = env, src = "yahoo", from = as.Date("1986-01-01"), to = as.Date("2017-03-31"))
ko <- env$KO
ko_close <- as.ts(ko$KO.Close)

### 3.general electric
getSymbols("GE", env = env, src = "yahoo", from = as.Date("1986-01-01"), to = as.Date("2017-03-31"))
ge <- env$GE
ge_close <- as.ts(ge$GE.Close)

### 4.mcdonalds
getSymbols("MCD", env = env, src = "yahoo", from = as.Date("1986-01-01"), to = as.Date("2017-03-31"))
mcd <- env$MCD
mcd_close <- as.ts(mcd$MCD.Close)

### 5.ford
getSymbols("F", env = env, src = "yahoo", from = as.Date("1986-01-01"), to = as.Date("2017-03-31"))
f <- env$F
f_close <- as.ts(f$F.Close)

### 6.ibm
getSymbols("IBM", env = env, src = "yahoo", from = as.Date("1986-01-01"), to = as.Date("2017-03-31"))
ibm <- env$IBM
ibm_close <- as.ts(ibm$IBM.Close)

### 7.kroger
getSymbols("KR", env = env, src = "yahoo", from = as.Date("1986-01-01"), to = as.Date("2017-03-31"))
kr <- env$KR
kr_close <- as.ts(kr$KR.Close)

### 8. cardinal health
getSymbols("CAH", env = env, src = "yahoo", from = as.Date("1986-01-01"), to = as.Date("2017-03-31"))
cah <- env$CAH
cah_close <- as.ts(cah$CAH.Close)

### 9.verizon
getSymbols("VZ", env = env, src = "yahoo", from = as.Date("1986-01-01"), to = as.Date("2017-03-31"))
vz <- env$VZ
vz_close <- as.ts(vz$VZ.Close)

### 10. exxon mobil
getSymbols("XOM", env = env, src = "yahoo", from = as.Date("1986-01-01"), to = as.Date("2017-03-31"))
xom <- env$XOM
xom_close <- as.ts(xom$XOM.Close)

# combine stock data
stocks <- ts.union(aapl_close,cah_close,f_close,ge_close,ibm_close,ko_close,kr_close,mcd_close,vz_close,xom_close)
stocks_df <- as.data.frame(stocks)
stock_cor <- cor(stocks_df)
stock_cor
stock_col_cor <- corrplot(stock_cor, method="color") # "circle", "square", "ellipse", "number", "shade", "color", "pie"
stock_col_cor

# ts model
ts_model <- tslm(aapl_close ~ cah_close + f_close + ge_close + 
                   ibm_close + ko_close + kr_close + mcd_close + 
                   vz_close + xom_close)
summary(ts_model)
plot(ts_model)
