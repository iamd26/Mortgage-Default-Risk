# read Freddie monthly HPI
library(data.table)
library(zoo)

hpi_master <- fread("http://www.freddiemac.com/fmac-resources/research/docs/fmhpi_master_file.csv")
hpi_master[, yearmon := Year*100+Month]
hpi_msa   <- hpi_master[GEO_Type=="CBSA",]
setorderv(hpi_msa,c("GEO_Code","yearmon"))
hpi_msa[, c("GEO_Type","GEO_Name","Index_SA","Year","Month") := NULL]
names(hpi_msa) <- c("MSA","hpi","yearmon")
hpi_msa[, MSA := as.integer(MSA)]
hpi_msa[, difl := c(0,diff(log(hpi))),by="MSA"]
hpi_msa[, spi := sqrt(12)*c(rep(0,23),rollapply(difl,24,sd)), by="MSA"]
hpi_msa[, difl := NULL]

save(hpi_msa,file="D:/Data/Freddie/hpi_msa.rda")

