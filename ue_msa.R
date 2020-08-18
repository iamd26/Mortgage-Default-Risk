# read unemployment rates and create MSA unemployment rate 

library(data.table)
library(zoo)

url1 <- "https://download.bls.gov/pub/time.series/la/la.data.0.CurrentU00-04"
url2 <- "https://download.bls.gov/pub/time.series/la/la.data.0.CurrentU05-09"
url3 <- "https://download.bls.gov/pub/time.series/la/la.data.0.CurrentU10-14"
url4 <- "https://download.bls.gov/pub/time.series/la/la.data.0.CurrentU15-19"
url5 <- "https://download.bls.gov/pub/time.series/la/la.data.0.CurrentU90-94"
url6 <- "https://download.bls.gov/pub/time.series/la/la.data.0.CurrentU95-99"

dat1 <- fread(url1)
dat2 <- fread(url2)
dat3 <- fread(url3)
dat4 <- fread(url4)
dat5 <- fread(url5)
dat6 <- fread(url6)

UE <- rbind(dat1,dat2,dat3,dat4,dat5,dat6)
UE[, footnote_codes := NULL]
UE[, yearmon := as.integer(year)*100 + as.integer(substr(period,2,3))]

UE[, c("year","period") := NULL]
names(UE) <- c("series_id","ue","yearmon")

MSA <- fread("UE_MSACode.csv")
ue_msa <- UE[series_id %in% MSA$series_id,]
ue_msa <- ue_msa[MSA, on = "series_id"]
ue_msa[, c("MSA_name") := NULL]
ue_msa[, ue := as.numeric(ue)]
ue_msa[, yearmon := as.integer(yearmon)]
ue_msa[, series_id := NULL]
setorderv(ue_msa,c("MSA","yearmon"))
