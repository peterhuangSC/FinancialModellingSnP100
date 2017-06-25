require(quantmod)
sp500 <- new.env()
getSymbols("AMZN", env=sp500, src="yahoo",
           from = as.Date("2015-01-03"), 
           to = as.Date("2017-06-23"))

AMZN <- sp500$AMZN
AMZN1 <- get ("AMZN", envir=sp500)
AMZN2 <- with(sp500, AMZN)

#rm(AMZN1)
#rm(AMZN2)

class (AMZN)

dim (AMZN)

head (AMZN$AMZN.Volume)
AMZN["2017-06"]

#--------------------------------------------------------#

nextfri <- function(x) 7 * ceiling(as.numeric(x - 5 + 4)/7) + as.Date(5 - 4)

#SP.we <- aggregate(AMZN, nextfri, tail, 1)

SP.we <- xts(aggregate(AMZN, nextfri, tail, 1))

SPClose.we <- Cl(SP.we)

plot(SPClose.we)

lr <- diff(log(SPClose.we))
plot(lr)
abline(h=0, col="red")

#apply.weekly(lr, colMeans)
#mean(lr$AMZN.Close)
