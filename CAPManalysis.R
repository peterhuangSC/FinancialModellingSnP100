require(quantmod)
require(PerformanceAnalytics)

#dates, set up interval
startDate <- as.Date("2015-01-01")
endDate <- as.Date("2017-06-25")

sp500 <- new.env()
getSymbols("AMZN", env=sp500, src="yahoo",
           from = startDate, to = endDate)
getSymbols("^GSPC", env=sp500, src="yahoo",
           from = startDate, to = endDate)

rfEnv <- new.env()
getSymbols("DGS3MO", env=rfEnv, src="FRED",
           from = startDate, to = endDate)

AMZN <- sp500$AMZN
DGS3MO <- rfEnv$DGS3MO
GSPC <- sp500$GSPC

rfRate<-c(DGS3MO["2015"], DGS3MO["2016"], DGS3MO["2017"])

#curStockA<-tail(AMZN$AMZN.Close, 252)
#riskFreeA<-tail(rfRate, 252)
#curStockB<-head(tail(AMZN$AMZN.Close, 252*2), 252)
#riskFreeB<-head(tail(rfRate, 252*2), 252)

AMZN$diffYear <- AMZN$AMZN.Close - lag(AMZN$AMZN.Close, 252)
GSPC$diffYear <- GSPC$GSPC.Close - lag(GSPC$GSPC.Close, 252)
rfRate$diffYear <- rfRate$DGS3MO - lag(rfRate$DGS3MO, 252)
for (i in 2:(dim(rfRate$diffYear)[1])) {
  if(is.na(rfRate$diffYear[i])) {
    rfRate$diffYear[i] <- rfRate$diffYear[i-1]
    #rfRate$diffYear2[i] <- "bob"
  }
}

for (i in 2:(dim(rfRate$DGS3MO)[1])) {
  if(is.na(rfRate$DGS3MO[i])) {
    rfRate$DGS3MO[i] <- rfRate$DGS3MO[i-1]
    #rfRate$diffYear2[i] <- "bob"
  }
}

curStockCh <- tail(AMZN$diffYear, dim(AMZN)[1] - 252)
marketCh <- tail(GSPC$diffYear, dim(GSPC)[1] - 252)
riskFreeCh <- tail(rfRate$diffYear, dim(rfRate)[1] - 262) #starts jan4
riskFreeCh$price <- tail(rfRate$DGS3MO, dim(rfRate)[1] - 262)

R_curStock <- curStockCh$diffYear / AMZN$AMZN.Close
R_curStock$Daily <- ((AMZN$AMZN.Close - lag(AMZN$AMZN.Close, 1)) / lag(AMZN$AMZN.Close, 1)) * 100
R_market <- marketCh$diffYear / GSPC$GSPC.Close
R_market$Daily <- ((GSPC$GSPC.Close - lag(GSPC$GSPC.Close, 1)) / lag(GSPC$GSPC.Close, 1)) * 100

#class(riskFreeCh) <- "numeric"
#R_rf <- riskFreeCh["diffYear"] / riskFreeCh["price"]

#cov(R_curStock$diffYear, R_market$diffYear, use = "complete.obs")
betaStock <- cov(R_curStock$Daily, R_market$Daily, use = "complete.obs") / 
  var(R_market$Daily, use = "complete.obs")

curBeta <- betaStock[1]

CAPMstock <- rfRate$DGS3MO + curBeta * (R_curStock$diffYear * 100 - rfRate$DGS3MO)  

plot(CAPMstock)


#--------------------------------------------------------#

nextwed <- function(x) 7 * ceiling(as.numeric(x - 5 + 4)/7) + as.Date(5 - 4)

SP.wed <- xts(aggregate(AMZN, nextwed, tail, 1))

RF.wed <- xts(aggregate(DGS3MO, nextwed, tail, 1))

SPClose.wed <- Cl(SP.wed)
#RFClose.we <- Cl(RF.wed)

plot(SPClose.wed)

lr <- diff(log(SPClose.wed))
plot(lr)
abline(h=0, col="red")

#apply.weekly(lr, colMeans)
#mean(lr$AMZN.Close)

#------------------------------------------------------#
# Beta = Cov(R_i, R_m) / Variance(R_m)
# Beta = Corr(R_a, R_m) x stdev(sigma_i / sigma_m)
# Sharpe Ratio = R_p - R_f / stdev(p)

# CAPM

#CAPM = rf90Rate + beta * (SPClose.wed - rf90Rate)

sharpe <- (diff(SPClose.we) / SPClose.we) * 100
rf90rate <- (diff(RF.we) / RF.we) * 100
plot(sharpe,
     main = "Weekly Sharpe Ratios",
     xlab = "Date",
     ylab = "% Return")
abline(h=0, col="red")