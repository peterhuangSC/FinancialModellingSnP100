require(quantmod)
require(PerformanceAnalytics)

#dates, set up interval
startDate <- as.Date("2015-01-01")
endDate <- as.Date("2017-06-25")

sp500 <- new.env()
getSymbols("AMZN", env=sp500, src="yahoo",
           from = startDate, to = endDate)

rfRate <- new.env()
getSymbols("DGS3MO", env=rfRate, src="FRED",
           from = startDate, to = endDate)

AMZN <- sp500$AMZN
#AMZN1 <- get ("AMZN", envir=sp500)
#AMZN2 <- with(sp500, AMZN)
tBill90d <- rfRate$DGS3MO

#rm(AMZN1)
#rm(AMZN2)

class (AMZN)
dim (AMZN)
class (tBill90d)
dim (tBill90d)

head (AMZN$AMZN.Volume)
AMZN["2017-06"]

#--------------------------------------------------------#

nextfri <- function(x) 7 * ceiling(as.numeric(x - 5 + 4)/7) + as.Date(5 - 4)
nextwed <- function(x) 7 * ceiling(as.numeric(x - 3 + 4)/7) + as.Date(3 - 4)

#SP.we <- aggregate(AMZN, nextfri, tail, 1)
SP.we <- xts(aggregate(AMZN, nextfri, tail, 1))

RF.we <- xts(aggregate(tBill90d, nextfri, tail, 1))

SPClose.we <- Cl(SP.we)
#RFClose.we <- Cl(RF.we)

plot(SPClose.we)

lr <- diff(log(SPClose.we))
plot(lr)
abline(h=0, col="red")

#apply.weekly(lr, colMeans)
#mean(lr$AMZN.Close)

#------------------------------------------------------#
# Beta = Cov(R_i, R_m) / Variance(R_m)
# Beta = Corr(R_a, R_m) x stdev(sigma_i / sigma_m)
# Sharpe Ratio = R_p - R_f / stdev(p)

# CAPM

CAPM = rf90Rate + beta * (SPClose.we - rf90Rate)

sharpe <- (diff(SPClose.we) / SPClose.we) * 100
rf90rate <- (diff(RF.we) / RF.we) * 100
plot(sharpe,
     main = "Weekly Sharpe Ratios",
     xlab = "Date",
     ylab = "% Return")
abline(h=0, col="red")