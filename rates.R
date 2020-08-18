# download weekly national 30Y mortgage rate from FRED
# convert to monthly data

library(data.table)
library(quantmod)

# get national 30Y mortgage rates from FRED
mrates <- getSymbols("MORTGAGE30US",src="FRED",auto.assign=FALSE)
mrates <- na.omit(mrates)
str(mrates)
data <- data.table(Date=index(mrates),RT=mrates)
names(data) <- c("Date","RT")
z  <- rollapply(data$RT,4,mean)
data$ma4 <- c(NA,NA,NA,z)
z <- to.period(data,
               period='months',
               OHLC = FALSE)
rates <- data.table(yearmon = index(z),
                    rate    = z$ma4)
names(rates) <- c("yearmon","rate")
rates$yearmon <- as.integer(floor(as.numeric(as.character(format(rates$yearmon,"%Y%m%d")))/100))
setorderv(rates,c("yearmon"))

save(rates,file="mortgage_rates.rda")