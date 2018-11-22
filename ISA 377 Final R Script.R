###LOAD REQUIRED PACKAGES

library(quantmod)
library(xts)
library(RCurl)
library(downloader)
library("rpart")
library("rpart.plot")
library("DBI")
library("randomForest")
library(corrplot)
library(zoo)
library(forecast)


#SET THE DATE PARAMETERS FOR THE VARIABLES THAT WILL BE COLLECTED
today<-Sys.Date()
end<- as.POSIXlt(today)
end$mon<-end$mon-7
begin<- as.POSIXlt(today)
begin$year<-begin$year-15
begin<-as.Date(begin)
from.dat <- as.Date(begin, format="%y/%m/%yd")
to.dat <- as.Date(end, format="%y/%m/%d")


#LIST THE VARIABLES TO PULL FROM QUANTMOD
macro<-c("CPIAUCSL","INDPRO","DGS10","DGS1","DGS3","UNRATE","FEDFUNDS","M2SL",
         "M1SL","PCE","BUSLOANS","EXUSEU","UMCSENT","HOUST","CSUSHPINSA","DSPIC96","RECPROUSM156N",
         "EXCHUS","MPRIME","EXCAUS","MTSDS133FMS","EXUSUK","TOTALSA",
         "EXUSAL","EXMXUS","RSXFS","MORTG","BOPGSTB","PI","LNS11300002","ISRATIO","TTLCONS","USEPUINDXM","LNS14027662",
         "TCDSL")

#PULL ALL DESIRED VARIABLES FROM QUANTMOD
getSymbols(macro, src = "FRED")


########################################################

#PULL DOW JONES FROM QUANTMOD
env <- new.env()
DJI<-getSymbols.yahoo("^DJI",from="1900-01-01", env=env)
DJI <- env$DJI
DJI<-DJI[,6]
DJI<-apply.monthly(DJI,tail,1)

#PULL NASDAQ FROM QUANTMOD
IXIC<-getSymbols.yahoo("^IXIC",from="1900-01-01", env=env)
IXIC <- env$IXIC
IXIC<-IXIC[,6]
IXIC<-apply.monthly(IXIC,tail,1)

#PULL XLE FROM QUANTMOD
XLE <-getSymbols.yahoo("XLE",from="1900-01-01", env=env)
XLE <- env$XLE
XLE<-XLE[,6]
XLE<-apply.monthly(XLE,tail,1)

#PULL XLF FROM QUANTMOD
XLF <-getSymbols.yahoo("XLF",from="1900-01-01", env=env)
XLF <- env$XLF
XLF<-XLF[,6]
XLF<-apply.monthly(XLF,tail,1)

#PULL XLU FROM QUANTMOD
XLU <-getSymbols.yahoo("XLU",from="1900-01-01", env=env)
XLU <- env$XLU
XLU<-XLU[,6]
XLU<-apply.monthly(XLU,tail,1)

#PULL XLI FROM QUANTMOD
XLI <-getSymbols.yahoo("XLI",from="1900-01-01", env=env)
XLI <- env$XLI
XLI<-XLI[,6]
XLI<-apply.monthly(XLI,tail,1)

#PULL XLB FROM QUANTMOD
XLB <-getSymbols.yahoo("XLB",from="1900-01-01", env=env)
XLB <- env$XLB
XLB<-XLB[,6]
XLB<-apply.monthly(XLB,tail,1)

#PULL XLP FROM QUANTMOD
XLP <-getSymbols.yahoo("XLP",from="1900-01-01", env=env)
XLP <- env$XLP
XLP<-XLP[,6]
XLP<-apply.monthly(XLP,tail,1)

#################



#APPLY DATE PARAMETERS TO ALL COLLECTED VARIABLES
CPIAUCSL<- CPIAUCSL[paste0(from.dat,"/",to.dat)]
INDPRO<- INDPRO[paste0(from.dat,"/",to.dat)]
DGS10<- DGS10[paste0(from.dat,"/",to.dat)]
DGS1<- DGS1[paste0(from.dat,"/",to.dat)]
DGS3<- DGS3[paste0(from.dat,"/",to.dat)]
UNRATE<- UNRATE[paste0(from.dat,"/",to.dat)]
FEDFUNDS<- FEDFUNDS[paste0(from.dat,"/",to.dat)]
M2SL<- M2SL[paste0(from.dat,"/",to.dat)]
M1SL<- M1SL[paste0(from.dat,"/",to.dat)]
PCE<- PCE[paste0(from.dat,"/",to.dat)]
BUSLOANS<- BUSLOANS[paste0(from.dat,"/",to.dat)]
EXUSEU<- EXUSEU[paste0(from.dat,"/",to.dat)]
UMCSENT<- UMCSENT[paste0(from.dat,"/",to.dat)]
HOUST<- HOUST[paste0(from.dat,"/",to.dat)]
CSUSHPINSA<- CSUSHPINSA[paste0(from.dat,"/",to.dat)]
DSPIC96<- DSPIC96[paste0(from.dat,"/",to.dat)]
RECPROUSM156N<- RECPROUSM156N[paste0(from.dat,"/",to.dat)]
EXCHUS<- EXCHUS[paste0(from.dat,"/",to.dat)]
MPRIME<- MPRIME[paste0(from.dat,"/",to.dat)]
EXCAUS<- EXCAUS[paste0(from.dat,"/",to.dat)]
MTSDS133FMS<- MTSDS133FMS[paste0(from.dat,"/",to.dat)]
EXUSUK<- EXUSUK[paste0(from.dat,"/",to.dat)]
TOTALSA<- TOTALSA[paste0(from.dat,"/",to.dat)]
EXUSAL<- EXUSAL[paste0(from.dat,"/",to.dat)]
EXMXUS<- EXMXUS[paste0(from.dat,"/",to.dat)]
RSXFS<- RSXFS[paste0(from.dat,"/",to.dat)]
MORTG<- MORTG[paste0(from.dat,"/",to.dat)]
BOPGSTB<- BOPGSTB[paste0(from.dat,"/",to.dat)]
PI<- PI[paste0(from.dat,"/",to.dat)]
LNS11300002<- LNS11300002[paste0(from.dat,"/",to.dat)]
ISRATIO<- ISRATIO[paste0(from.dat,"/",to.dat)]
TTLCONS<- TTLCONS[paste0(from.dat,"/",to.dat)]
USEPUINDXM<- USEPUINDXM[paste0(from.dat,"/",to.dat)]
LNS14027662<- LNS14027662[paste0(from.dat,"/",to.dat)]
TCDSL<- TCDSL[paste0(from.dat,"/",to.dat)]
DJI<- DJI[paste0(from.dat,"/",to.dat)]
IXIC<- IXIC[paste0(from.dat,"/",to.dat)]
XLE <- XLE[paste0(from.dat,"/",to.dat)]
XLF <- XLF[paste0(from.dat,"/",to.dat)]
XLI <- XLI[paste0(from.dat,"/",to.dat)]
XLU <- XLU[paste0(from.dat,"/",to.dat)]
XLB <- XLB[paste0(from.dat,"/",to.dat)]
XLP <- XLP[paste0(from.dat,"/",to.dat)]

##########################


#OFFSET VARIABLES ONE MONTH
CPIAUCSL$CPIprev<-c(rep(NA, 1), head(CPIAUCSL$CPIAUCSL, -1))
INDPRO$Indproprev<-c(rep(NA, 1), head(INDPRO$INDPRO, -1))
DGS10$DG10prev<-c(rep(NA, 1), head(DGS10$DGS10, -1))
DGS1$DG1prev<-c(rep(NA, 1), head(DGS1$DGS1, -1))
DGS3$DG3prev<-c(rep(NA, 1), head(DGS3$DGS3, -1))
CSUSHPINSA$CSUSHprev<-c(rep(NA, 1), head(CSUSHPINSA$CSUSHPINSA, -1))
UNRATE$UNRATEprev<-c(rep(NA, 1), head(UNRATE$UNRATE, -1))
FEDFUNDS$FEDprev<-c(rep(NA, 1), head(FEDFUNDS$FEDFUNDS, -1))
M2SL$M2prev<-c(rep(NA, 1), head(M2SL$M2SL, -1))
M1SL$M1prev<-c(rep(NA, 1), head(M1SL$M1SL, -1))
PCE$PCEprev<-c(rep(NA, 1), head(PCE$PCE, -1))
BUSLOANS$BUSLOANprev<-c(rep(NA, 1), head(BUSLOANS$BUSLOANS, -1))
EXUSEU$EXUSEUprev<-c(rep(NA, 1), head(EXUSEU$EXUSEU, -1))
UMCSENT$UMCSENTprev<-c(rep(NA, 1), head(UMCSENT$UMCSENT, -1))
HOUST$HOUSTprev<-c(rep(NA, 1), head(HOUST$HOUST, -1))
DSPIC96$DSPIC96prev<-c(rep(NA, 1), head(DSPIC96$DSPIC96, -1))
RECPROUSM156N$prev<-c(rep(NA, 1), head(RECPROUSM156N$RECPROUSM156N, -1))
EXCHUS$EXCHUSprev<-c(rep(NA, 1), head(EXCHUS$EXCHUS, -1))
MPRIME$MPRIMEprev<-c(rep(NA, 1), head(MPRIME$MPRIME, -1))
EXCAUS$EXCAUSprev<-c(rep(NA, 1), head(EXCAUS$EXCAUS, -1))
MTSDS133FMS$MTSDprev<-c(rep(NA, 1), head(MTSDS133FMS$MTSDS133FMS, -1))
EXUSUK$EXUSUKprev<-c(rep(NA, 1), head(EXUSUK$EXUSUK, -1))
TOTALSA$TOTALSAprev<-c(rep(NA, 1), head(TOTALSA$TOTALSA, -1))
EXUSAL$EXUSALprev<-c(rep(NA, 1), head(EXUSAL$EXUSAL, -1))
EXMXUS$EXMXUSprev<-c(rep(NA, 1), head(EXMXUS$EXMXUS, -1))
RSXFS$RSXprev<-c(rep(NA, 1), head(RSXFS$RSXFS, -1))
MORTG$MORTGprev<-c(rep(NA, 1), head(MORTG$MORTG, -1))
BOPGSTB$BOPprev<-c(rep(NA, 1), head(BOPGSTB$BOPGSTB, -1))
PI$PIprev<-c(rep(NA, 1), head(PI$PI, -1))
LNS11300002$LNSprev<-c(rep(NA, 1), head(LNS11300002$LNS11300002, -1))
ISRATIO$ISRAIOprev<-c(rep(NA, 1), head(ISRATIO$ISRATIO, -1))
TTLCONS$TTLprev<-c(rep(NA, 1), head(TTLCONS$TTLCONS, -1))
USEPUINDXM$USEPUIprev<-c(rep(NA, 1), head(USEPUINDXM$USEPUINDXM, -1))
LNS14027662$LNS14prev<-c(rep(NA, 1), head(LNS14027662$LNS14027662, -1))
TCDSL$TCDSLprev<-c(rep(NA, 1), head(TCDSL$TCDSL, -1))
DJI$DJIprev<-c(rep(NA, 1), head(DJI$DJI.Adjusted, -1))
IXIC$IXICprev<-c(rep(NA, 1), head(IXIC$IXIC.Adjusted, -1))
XLE$XLEprev <-c(rep(NA, 1), head(XLE$XLE.Adjusted, -1))
XLF$XLFprev <-c(rep(NA, 1), head(XLF$XLF.Adjusted, -1))
XLI$XLIprev <-c(rep(NA, 1), head(XLI$XLI.Adjusted, -1))
XLU$XLUprev <-c(rep(NA, 1), head(XLU$XLU.Adjusted, -1))
XLB$XLBprev <-c(rep(NA, 1), head(XLB$XLB.Adjusted, -1))
XLP$XLPprev <-c(rep(NA, 1), head(XLP$XLP.Adjusted, -1))


#DELETE ORIGINAL VALUE, AS IT IS NOT AVAILABLE FOR PREDICTORS
CPIAUCSL$CPIAUCSL=NULL
INDPRO$INDPRO=NULL
DGS10$DGS10=NULL
DGS1$DGS1=NULL
DGS3$DGS3=NULL
UNRATE$UNRATE=NULL
FEDFUNDS$FEDFUNDS=NULL
M2SL$M2SL=NULL
M1SL$M1SL=NULL
PCE$PCE=NULL
BUSLOANS$BUSLOANS=NULL
EXUSEU$EXUSEU=NULL
UMCSENT$UMCSENT=NULL
HOUST$HOUST=NULL
DSPIC96$DSPIC96=NULL
CSUSHPINSA$CSUSHPINSA=NULL
RECPROUSM156N$RECPROUSM156N=NULL
EXCHUS$EXCHUS=NULL
MPRIME$MPRIME=NULL
EXCAUS$EXCAUS=NULL
MTSDS133FMS$MTSDS133FMS=NULL
EXUSUK$EXUSUK=NULL
TOTALSA$TOTALSA=NULL
EXUSAL$EXUSAL=NULL
EXMXUS$EXMXUS=NULL
RSXFS$RSXFS=NULL
MORTG$MORTG=NULL
BOPGSTB$BOPGSTB=NULL
PI$PI=NULL
LNS11300002$LNS11300002=NULL
ISRATIO$ISRATIO=NULL
TTLCONS$TTLCONS=NULL
USEPUINDXM$USEPUINDXM=NULL
LNS14027662$LNS14027662=NULL
TCDSL$TCDSL=NULL

######################

#TAKE OFF FIRST OBSERVATION, BECAUSE IT IS BLANK FOR MOST VARIABLES NOW
CSUSHPINSA<-CSUSHPINSA[-1,]
CPIAUCSL<-CPIAUCSL[-1,]
INDPRO<-INDPRO[-1,]
DGS10<-DGS10[-1,]
DGS1<-DGS1[-1,]
DGS3<-DGS3[-1,]
UNRATE<-UNRATE[-1,]
FEDFUNDS<-FEDFUNDS[-1,]
M2SL<-M2SL[-1,]
M1SL<-M1SL[-1,]
PCE<-PCE[-1,]
BUSLOANS<-BUSLOANS[-1,]
EXUSEU<-EXUSEU[-1,]
UMCSENT<-UMCSENT[-1,]
DSPIC96<-DSPIC96[-1,]
RECPROUSM156N<-RECPROUSM156N[-1,]
EXCHUS<-EXCHUS[-1,]
MPRIME<-MPRIME[-1,]
EXCAUS<-EXCAUS[-1,]
MTSDS133FMS<-MTSDS133FMS[-1,]
EXUSUK<-EXUSUK[-1,]
TOTALSA<-TOTALSA[-1,]
EXUSAL<-EXUSAL[-1,]
EXMXUS<-EXMXUS[-1,]
RSXFS<-RSXFS[-1,]
MORTG<-MORTG[-1,]
HOUST<-HOUST[-1,]
BOPGSTB<-BOPGSTB[-1,]
PI<-PI[-1,]
LNS11300002<-LNS11300002[-1,]
ISRATIO<-ISRATIO[-1,]
TTLCONS<-TTLCONS[-1,]
USEPUINDXM<-USEPUINDXM[-1,]
LNS14027662<-LNS14027662[-1,]
TCDSL<-TCDSL[-1,]
DJI<-DJI[-1,]
IXIC<-IXIC[-1,]
SPY = SPY[-1,]
XLE<-XLE[-1,]
XLF<-XLF[-1,]
XLV<-XLV[-1,]
XLV<-XLV[-1,]
XLI<-XLI[-1,]
KRX<-KRX[-1,]
XLU<-XLU[-1,]
XLB<-XLB[-1,]
XLP<-XLP[-1,]

##################

#CHANGE XTS OBJECTS TO DATA FRAMES
CSUSHPINSA<-as.data.frame(CSUSHPINSA)
CPIAUCSL<-as.data.frame(CPIAUCSL)
INDPRO<-as.data.frame(INDPRO)
UNRATE<-as.data.frame(UNRATE)
FEDFUNDS<-as.data.frame(FEDFUNDS)
M2SL<-as.data.frame(M2SL)
M1SL<-as.data.frame(M1SL)
PCE<-as.data.frame(PCE)
BUSLOANS<-as.data.frame(BUSLOANS)
EXUSEU<-as.data.frame(EXUSEU)
UMCSENT<-as.data.frame(UMCSENT)
DSPIC96<-as.data.frame(DSPIC96)
RECPROUSM156N<-as.data.frame(RECPROUSM156N)
RECPROUSM156N<-as.data.frame(RECPROUSM156N)
HOUST<-as.data.frame(HOUST)
EXCHUS<-as.data.frame(EXCHUS)
MPRIME<-as.data.frame(MPRIME)
EXCAUS<-as.data.frame(EXCAUS)
MTSDS133FMS<-as.data.frame(MTSDS133FMS)
EXUSUK<-as.data.frame(EXUSUK)
TOTALSA<-as.data.frame(TOTALSA)
EXUSAL<-as.data.frame(EXUSAL)
EXMXUS<-as.data.frame(EXMXUS)
RSXFS<-as.data.frame(RSXFS)
MORTG<-as.data.frame(MORTG)
BOPGSTB<-as.data.frame(BOPGSTB)
PI<-as.data.frame(PI)
LNS11300002<-as.data.frame(LNS11300002)
ISRATIO<-as.data.frame(ISRATIO)
TTLCONS<-as.data.frame(TTLCONS)
USEPUINDXM<-as.data.frame(USEPUINDXM)
LNS14027662<-as.data.frame(LNS14027662)
TCDSL<-as.data.frame(TCDSL)
DJI<-as.data.frame(DJI)
IXIC<-as.data.frame(IXIC)
SPY = as.data.frame(SPY)
XLE = as.data.frame(XLE)
XLF = as.data.frame(XLF)
XLV = as.data.frame(XLV)
XLI = as.data.frame(XLI)
KRX = as.data.frame(KRX)
XLU = as.data.frame(XLU)
XLB = as.data.frame(XLB)
XLP = as.data.frame(XLP)


library("TTR")

#HERE ARE THE SIMPLE MOVING AVERAGE PREDICTION MODELS. WE DIDN'T END UP USING THESE.
# ################################xle
# MrgXLE<-cbind(CPIAUCSL,INDPRO,UNRATE,FEDFUNDS,M2SL,M1SL,PCE,BUSLOANS,EXUSEU,UMCSENT,
#               DSPIC96,RECPROUSM156N,EXCHUS,MPRIME,EXCAUS,EXUSUK,TOTALSA,EXUSAL,EXMXUS,RSXFS,
#               MORTG,BOPGSTB,PI,LNS11300002,ISRATIO,TTLCONS,USEPUINDXM,LNS14027662,TCDSL,DJI,IXIC,
#               XLE)
# 
# XLEprev<-MrgXLE$XLEprev
# 
# XLESMOOTH<-SMA(XLEprev,n=5)
# 
# check_xle<-as.data.frame(cbind(XLESMOOTH, MrgXLE$XLE.Adjusted))
# 
# check_xle<-tail(check_xle, -9)
# 
# check_xle$ape<-(abs(check_xle$XLESMOOTH-check_xle$V2)/check_xle$V2)
# 
# MAPE_xle <- mean(check_xle$ape)
# MAPE_xle
# 
# ################################xlf
# MrgXLF<-cbind(CPIAUCSL,INDPRO,UNRATE,FEDFUNDS,M2SL,M1SL,PCE,BUSLOANS,EXUSEU,UMCSENT,
#               DSPIC96,RECPROUSM156N,EXCHUS,MPRIME,EXCAUS,EXUSUK,TOTALSA,EXUSAL,EXMXUS,RSXFS,
#               MORTG,BOPGSTB,PI,LNS11300002,ISRATIO,TTLCONS,USEPUINDXM,LNS14027662,TCDSL,DJI,IXIC,
#               XLF)
# 
# set.seed(598312)
# trainind_xlf = c(1:round(.75*nrow(MrgXLF)))
# MrgXLF$all = NULL
# train_xlf<-MrgXLF[trainind_xlf,]
# valid_xlf<-MrgXLF[-trainind_xlf,]
# 
# forest_xlf <- randomForest (XLF.Adjusted~ . ,  data = MrgXLF,subset=trainind_xlf ,mtry=32,importance=TRUE, ntree =500)
# regforest_xlf <- predict (forest_xlf, newdata=valid_xlf)
# 
# check_xlf<-as.data.frame(cbind(regforest_xlf,valid_xlf$XLF.Adjusted, valid_xlf$XLFprev))
# 
# check_xlf$ape<-(abs(check_xlf$regforest_xlf-check_xlf$V2)/check_xlf$V2)
# 
# MAPE_xlf <- mean(check_xlf$ape)
# MAPE_xlf
# 
# 
# ################################xli
# 
# # MAPE_xli
# MrgXLI<-cbind(CPIAUCSL,INDPRO,UNRATE,FEDFUNDS,M2SL,M1SL,PCE,BUSLOANS,EXUSEU,UMCSENT,
#               DSPIC96,RECPROUSM156N,EXCHUS,MPRIME,EXCAUS,EXUSUK,TOTALSA,EXUSAL,EXMXUS,RSXFS,
#               MORTG,BOPGSTB,PI,LNS11300002,ISRATIO,TTLCONS,USEPUINDXM,LNS14027662,TCDSL,DJI,IXIC,
#               XLI)
# XLIprev<-MrgXLI$XLIprev
# 
# XLISMOOTH<-SMA(XLIprev,n=5)
# 
# check_xli<-as.data.frame(cbind(XLISMOOTH, MrgXLI$XLI.Adjusted))
# 
# check_xli<-tail(check_xli, -9)
# 
# check_xli$ape<-(abs(check_xli$XLISMOOTH-check_xli$V2)/check_xli$V2)
# 
# MAPE_xli <- mean(check_xli$ape)
# MAPE_xli
# 
# ################################xlp
# MrgXLP<-cbind(CPIAUCSL,INDPRO,UNRATE,FEDFUNDS,M2SL,M1SL,PCE,BUSLOANS,EXUSEU,UMCSENT,
#               DSPIC96,RECPROUSM156N,EXCHUS,MPRIME,EXCAUS,EXUSUK,TOTALSA,EXUSAL,EXMXUS,RSXFS,
#               MORTG,BOPGSTB,PI,LNS11300002,ISRATIO,TTLCONS,USEPUINDXM,LNS14027662,TCDSL,DJI,IXIC,
#               XLP)
# 
# XLPprev<-MrgXLP$XLPprev
# 
# XLPSMOOTH<-SMA(XLPprev,n=5)
# 
# check_xlp<-as.data.frame(cbind(XLPSMOOTH, MrgXLP$XLP.Adjusted))
# 
# check_xlp<-tail(check_xlp, -9)
# 
# check_xlp$ape<-(abs(check_xlp$XLPSMOOTH-check_xlp$V2)/check_xlp$V2)
# 
# MAPE_xlp <- mean(check_xlp$ape)
# MAPE_xlp
# 
# ################################xlb
# 
# MrgXLB<-cbind(CPIAUCSL,INDPRO,UNRATE,FEDFUNDS,M2SL,M1SL,PCE,BUSLOANS,EXUSEU,UMCSENT,
#               DSPIC96,RECPROUSM156N,EXCHUS,MPRIME,EXCAUS,EXUSUK,TOTALSA,EXUSAL,EXMXUS,RSXFS,
#               MORTG,BOPGSTB,PI,LNS11300002,ISRATIO,TTLCONS,USEPUINDXM,LNS14027662,TCDSL,DJI,IXIC,
#               XLB)
# 
# XLBprev<-MrgXLB$XLBprev
# 
# XLBSMOOTH<-SMA(XLBprev,n=5)
# 
# check_xlb<-as.data.frame(cbind(XLBSMOOTH, MrgXLB$XLB.Adjusted))
# 
# check_xlb<-tail(check_xlb, -9)
# 
# check_xlb$ape<-(abs(check_xlb$XLBSMOOTH-check_xlb$V2)/check_xlb$V2)
# 
# MAPE_xlb <- mean(check_xlb$ape)
# MAPE_xlb
# 
# ################################xlu
# MrgXLU<-cbind(CPIAUCSL,INDPRO,UNRATE,FEDFUNDS,M2SL,M1SL,PCE,BUSLOANS,EXUSEU,UMCSENT,
#               DSPIC96,RECPROUSM156N,EXCHUS,MPRIME,EXCAUS,EXUSUK,TOTALSA,EXUSAL,EXMXUS,RSXFS,
#               MORTG,BOPGSTB,PI,LNS11300002,ISRATIO,TTLCONS,USEPUINDXM,LNS14027662,TCDSL,DJI,IXIC,
#               XLU)
# 
# 
# XLUprev<-MrgXLU$XLUprev
# 
# XLUSMOOTH<-SMA(XLUprev,n=5)
# 
# check_xlu<-as.data.frame(cbind(XLUSMOOTH, MrgXLU$XLU.Adjusted))
# 
# check_xlu<-tail(check_xlu, -9)
# 
# check_xlu$ape<-(abs(check_xlu$XLUSMOOTH-check_xlu$V2)/check_xlu$V2)
# 
# MAPE_xlu <- mean(check_xlu$ape)
# MAPE_xlu

######################

# COMBINE DATA SETS
cov_adj <- as.ts(cbind(XLB$XLB.Adjusted,
                       XLE$XLE.Adjusted,
                       XLF$XLF.Adjusted,
                       XLI$XLI.Adjusted,
                       XLP$XLP.Adjusted,
                       XLU$XLU.Adjusted))


colnames(cov_adj) <- c("XLB.Adjusted",
                       "XLE.Adjusted",
                       "XLF.Adjusted",
                       "XLI.Adjusted",
                       "XLP.Adjusted",
                       "XLU.Adjusted")


#CREATE 3 DATA FRAMES LIMITING PREDICTIONS TO USE ONLY PREVIOUS 5 MONTHS

cov_adj_aug <- as.ts(cov_adj[166:170,])
cov_adj_sept <- as.ts(cov_adj[167:171,])
cov_adj_oct <- as.ts(cov_adj[168:172,])


#PREDICT THE NEXT MONTHS (AUGUST, SEPTEMBER, OCTOBER)
aug_pred <- forecast(cov_adj_aug, 1)
sept_pred <- forecast(cov_adj_sept, 1)
oct_pred <- forecast(cov_adj_oct, 1)

summary(aug_pred)
summary(sept_pred)
summary(oct_pred)

plot(aug_pred)
plot(sept_pred)
plot(oct_pred)


#CREATE DATA FRAMES INCLUDING THE MONTHS UP UNTIL DESIGNATED MONTH
XLB_aug = as.data.frame(XLB[1:171,])
XLE_aug = as.data.frame(XLE[1:171,])
XLF_aug = as.data.frame(XLF[1:171,])
XLI_aug = as.data.frame(XLI[1:171,])
XLP_aug = as.data.frame(XLP[1:171,])
XLU_aug = as.data.frame(XLU[1:171,])

XLB_sept = as.data.frame(XLB[1:172,])
XLE_sept = as.data.frame(XLE[1:172,])
XLF_sept = as.data.frame(XLF[1:172,])
XLI_sept = as.data.frame(XLI[1:172,])
XLP_sept = as.data.frame(XLP[1:172,])
XLU_sept = as.data.frame(XLU[1:172,])

XLB_oct = as.data.frame(XLB)
XLE_oct = as.data.frame(XLE)
XLF_oct = as.data.frame(XLF)
XLI_oct = as.data.frame(XLI)
XLP_oct = as.data.frame(XLP)
XLU_oct = as.data.frame(XLU)


XLB_aug <- rbind(XLB_aug,"2016-08-31" = c(45.65626,47.97155))
XLE_aug <- rbind(XLE_aug,"2016-08-31" = c(67.0708,66.22571))
XLF_aug <- rbind(XLF_aug,"2016-08-31" = c(18.31284,18.945637))
XLI_aug <- rbind(XLI_aug,"2016-08-31" = c(53.96671,57.11350))
XLP_aug <- rbind(XLP_aug,"2016-08-31" = c(51.53557,53.67850))
XLU_aug <- rbind(XLU_aug,"2016-08-31" = c(47.57123,50.79776))

XLB_sept <- rbind(XLB_sept,"2016-09-30" = c(45.94995,47.84347))
XLE_sept <- rbind(XLE_sept,"2016-09-30" = c(65.02218,67.33635))
XLF_sept <- rbind(XLF_sept,"2016-09-30" = c(18.52341,19.674624))
XLI_sept <- rbind(XLI_sept,"2016-09-30" = c(57.1133,57.65472))
XLP_sept <- rbind(XLP_sept,"2016-09-30" = c(53.68773,53.36448))
XLU_sept <- rbind(XLU_sept,"2016-09-30" = c(50.7978,47.98108))

XLB_oct <- rbind(XLB_oct,"2016-10-31" = c(47.73387,47.28792))
XLE_oct <- rbind(XLE_oct,"2016-10-31" = c(66.36746,69.81985))
XLF_oct <- rbind(XLF_oct,"2016-10-31" = c(18.87711,19.144524))
XLI_oct <- rbind(XLI_oct,"2016-10-31" = c(57.65467,57.76205))
XLP_oct <- rbind(XLP_oct,"2016-10-31" = c(53.37079,52.53970))
XLU_oct <- rbind(XLU_oct,"2016-10-31" = c(48.84828,48.15433))


##################
#CREATE CALCULATIONS FOR RETURNS
roww_aug <- 171
for (i in 1:(roww_aug)) {
  XLB_aug$Return[i] <- ((XLB_aug[i,1]-XLB_aug[i,2])/XLB_aug[i,2])
}
for (i in 1:(roww_aug)) {
  XLE_aug$Return[i] <- ((XLE_aug[i,1]-XLE_aug[i,2])/XLE_aug[i,2])
}
for (i in 1:(roww_aug)) {
  XLF_aug$Return[i] <- ((XLF_aug[i,1]-XLF_aug[i,2])/XLF_aug[i,2])
}
for (i in 1:(roww_aug)) {
  XLI_aug$Return[i] <- ((XLI_aug[i,1]-XLI_aug[i,2])/XLI_aug[i,2])
}
for (i in 1:(roww_aug)) {
  XLP_aug$Return[i] <- ((XLP_aug[i,1]-XLP_aug[i,2])/XLP_aug[i,2])
}
for (i in 1:(roww_aug)) {
  XLU_aug$Return[i] <- ((XLU_aug[i,1]-XLU_aug[i,2])/XLU_aug[i,2])
}

###
roww_sept <- 172
for (i in 1:(roww_sept)) {
  XLB_sept$Return[i] <- ((XLB_sept[i,1]-XLB_sept[i,2])/XLB_sept[i,2])
}
for (i in 1:(roww_sept)) {
  XLE_sept$Return[i] <- ((XLE_sept[i,1]-XLE_sept[i,2])/XLE_sept[i,2])
}
for (i in 1:(roww_sept)) {
  XLF_sept$Return[i] <- ((XLF_sept[i,1]-XLF_sept[i,2])/XLF_sept[i,2])
}
for (i in 1:(roww_sept)) {
  XLI_sept$Return[i] <- ((XLI_sept[i,1]-XLI_sept[i,2])/XLI_sept[i,2])
}
for (i in 1:(roww_sept)) {
  XLP_sept$Return[i] <- ((XLP_sept[i,1]-XLP_sept[i,2])/XLP_sept[i,2])
}
for (i in 1:(roww_sept)) {
  XLU_sept$Return[i] <- ((XLU_sept[i,1]-XLU_sept[i,2])/XLU_sept[i,2])
}

###
roww_oct <- 173
for (i in 1:(roww_oct)) {
  XLB_oct$Return[i] <- ((XLB_oct[i,1]-XLB_oct[i,2])/XLB_oct[i,2])
}
for (i in 1:(roww_oct)) {
  XLE_oct$Return[i] <- ((XLE_oct[i,1]-XLE_oct[i,2])/XLE_oct[i,2])
}
for (i in 1:(roww_oct)) {
  XLF_oct$Return[i] <- ((XLF_oct[i,1]-XLF_oct[i,2])/XLF_oct[i,2])
}
for (i in 1:(roww_oct)) {
  XLI_oct$Return[i] <- ((XLI_oct[i,1]-XLI_oct[i,2])/XLI_oct[i,2])
}
for (i in 1:(roww_oct)) {
  XLP_oct$Return[i] <- ((XLP_oct[i,1]-XLP_oct[i,2])/XLP_oct[i,2])
}
for (i in 1:(roww_oct)) {
  XLU_oct$Return[i] <- ((XLU_oct[i,1]-XLU_oct[i,2])/XLU_oct[i,2])
}


##
cov_return_aug <- as.data.frame(cbind(XLB_aug$Return,
                                  XLE_aug$Return,
                                  XLF_aug$Return,
                                  XLI_aug$Return,
                                  XLP_aug$Return,
                                  XLU_aug$Return))
colnames(cov_return_aug) <- c("XLB_aug Return",
                          "XLE_aug Return",
                          "XLF_aug Return",
                          "XLI_aug Return",
                          "XLP_aug Return",
                          "XLU_aug Return")

##
cov_return_sept <- as.data.frame(cbind(XLB_sept$Return,
                                   XLE_sept$Return,
                                   XLF_sept$Return,
                                   XLI_sept$Return,
                                   XLP_sept$Return,
                                   XLU_sept$Return))
colnames(cov_return_sept) <- c("XLB_sept Return",
                           "XLE_sept Return",
                           "XLF_sept Return",
                           "XLI_sept Return",
                           "XLP_sept Return",
                           "XLU_sept Return")

##
cov_return_oct <- as.data.frame(cbind(XLB_oct$Return,
                                   XLE_oct$Return,
                                   XLF_oct$Return,
                                   XLI_oct$Return,
                                   XLP_oct$Return,
                                   XLU_oct$Return))
colnames(cov_return_oct) <- c("XLB_oct Return",
                           "XLE_oct Return",
                           "XLF_oct Return",
                           "XLI_oct Return",
                           "XLP_oct Return",
                           "XLU_oct Return")

# covariance matrices
c_aug <- as.data.frame(cov(cov_return_aug))
c_sept <- as.data.frame(cov(cov_return_sept))
c_oct <- as.data.frame(cov(cov_return_oct))
