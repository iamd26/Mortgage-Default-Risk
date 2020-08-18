library(data.table)
library(quantmod)
#step 1
load("hpi_msa.rda")
load("mortgage_rates.rda")
load("ue_msa.rda")

# Step 2
load("TeamAssignment3_cdata_Q1.rda")
# Perform the following tasks on Data_C:
# - convert the MSA variable from character to integer.
Data_C$MSA <- as.integer(Data_C$MSA)
# - remove loans whose MSA is not in ue_msa$MSA
Data_C <- Data_C[MSA %in% ue_msa$MSA,]
# - remove loans whose MSA is not in hpi_msa$MSA
Data_C <- Data_C[MSA %in% hpi_msa$MSA,]
# - remove loans with no FICO score (CSCORE_B)
Data_C <- Data_C[!(is.na(Data_C$CSCORE_B)),]
# - remove loans with no original value (ORIG_VAL)
Data_C <- Data_C[!(is.na(Data_C$ORIG_VAL)),]
# - remove loans with no origination amount (ORIG_AMT)
Data_C <- Data_C[!(is.na(Data_C$ORIG_AMT)),]
# - remove loans with no origination interest rate (ORIG_RT)
Data_C <- Data_C[!(is.na(Data_C$ORIG_RT)),]
nrow(Data_C)
# This should leave you with 77,159 loans

# Set the ORIG_DTE variable to be integer in the form YYYYMM.
Data_C <- Data_C[, ORIG_DTE := as.integer(year(ORIG_DTE))*100 + as.integer(substr(ORIG_DTE,6,7))]
# Merge Data_C and rates[,c(1,2)] using by.x = "ORIG_DTE", by.y = "yearmon", all.x=TRUE
Data_C <- merge(Data_C, rates[,c(1,2)], by.x = 'ORIG_DTE', by.y = 'yearmon', all.x=TRUE)
# Merge Data_C and hpi_msa using by.x = c("ORIG_DTE","MSA")), by.y = c("yearmon","MSA"), all.x=TRUE
Data_C <- merge(Data_C, hpi_msa, by.x = c('ORIG_DTE','MSA'), by.y = c('yearmon','MSA'), all.x=TRUE)
# Then rename the "hpi" variable to be "hpi0"
names(Data_C)[names(Data_C) == 'hpi'] <- 'hpi0'
# Create the spread variable as ORIG_RT - rate
Data_C$spread <- Data_C$ORIG_RT - Data_C$rate
# Keep only the following variables:
Data_C <- Data_C[, c('LOAN_ID','OLTV','CSCORE_B','spread','ORIG_VAL','hpi0','MSA','ORIG_RT','NUM_BO','PURPOSE','PROP_TYP',
                     'OCC_STAT','DTI','FTHB_FLG')]

# Step 3
load("TeamAssignment3_pdata_Q1.RDA")
names(Data_P)[names(Data_P) == 'Monthly.Rpt.Prd'] <- 'yearmon'
Data_P <- Data_P[, yearmon := as.integer(year(yearmon))*100 + as.integer(substr(yearmon,6,7))]
Data_P <- Data_P[LOAN_ID %in% Data_C$LOAN_ID,]
nrow(Data_P)
# Should be 2,661,798 rows in Data_P
data1 <- merge(Data_P, Data_C, by='LOAN_ID', all.x=TRUE)
setorderv(data1,c('LOAN_ID',"yearmon"))
data1$status <- ifelse(data1$Zero.Bal.Code %in% c("02","03","09","15"),"default",
                       ifelse(data1$Zero.Bal.Code %in% c("01"),"prepaid","censored"))
# Step 4
data1 <- merge(data1, rates, by='yearmon')
data1$cvr <- data1$ORIG_RT/data1$rate

#step 5
data1 <- merge(data1, ue_msa, by=c('yearmon','MSA'))

#step 6
data1 <- merge(data1, hpi_msa, by=c('yearmon','MSA'))
data1$val <- data1$ORIG_VAL * data1$hpi / data1$hpi0
data1$pneq <- pnorm(log(data1$LAST_UPB/data1$val)/(100*data1$spi))

#step 7
data1$start <- data1$Loan.Age
data1$end <- data1$Loan.Age + 1


#Estimation
library(survival)
# cox.v1
cox.v1 <- coxph(formula = Surv(start,end,status=="default") ~
                  CSCORE_B+pneq+OLTV+spread+ue+cvr+NUM_BO+PURPOSE+PROP_TYP+OCC_STAT+DTI+FTHB_FLG,data=data1,ties="efron")

summary(cox.v1)
AIC(cox.v1)
#cox.v2
#elimnate any cov that is not statistically significant      
cox.v2 <- coxph(formula = Surv(start,end,status=="default") ~
                  CSCORE_B+pneq+OLTV+spread+cvr+DTI,data=data1,ties="efron")
summary(cox.v2)
AIC(cox.v2)
#16745.87
# cox.v3
#Evaluate DTI by eliminate it
cox.v3 <- coxph(formula = Surv(start,end,status=="default") ~
                  CSCORE_B+pneq+OLTV+spread+cvr,data=data1,ties="efron")
AIC(cox.v3)
#17461.05
# cox.v4
#Keep DTI, evaluate cvr
cox.v4 <- coxph(formula = Surv(start,end,status=="default") ~
                  CSCORE_B+pneq+OLTV+spread+DTI,data=data1,ties="efron")
AIC(cox.v4)
#16752.74
# cox.v5
#Keep cvr, evaluate spread
cox.v5 <- coxph(formula = Surv(start,end,status=="default") ~
                  CSCORE_B+pneq+OLTV+cvr+DTI,data=data1,ties="efron")
AIC(cox.v5)
#16774.02
# cox.v6
#Keep spread, evaluate OLTV
cox.v6 <- coxph(formula = Surv(start,end,status=="default") ~
                  CSCORE_B+pneq+spread+cvr+DTI,data=data1,ties="efron")
AIC(cox.v6)
#16745.24
#better than AIC(cox.v2)

#checking based on variables defined during class discussion
cox.v7 <- coxph(formula = Surv(start,end,status=="default") ~
                  CSCORE_B+pneq+spread+cvr+ue,data=data1,ties="efron")
AIC(cox.v7)


# Prepayment
coxp.v1 <- coxph(formula = Surv(start,end,status=="prepaid") ~
                   CSCORE_B+pneq+OLTV+spread+ue+cvr+NUM_BO+PURPOSE+PROP_TYP+OCC_STAT+DTI+FTHB_FLG,data=data1,ties="efron")
summary(coxp.v1)
AIC(coxp.v1)
#elimnate any cov that is not statistically significant      
coxp.v2 <- coxph(formula = Surv(start,end,status=="prepaid") ~
                   CSCORE_B+pneq+OLTV+ue+cvr+PROP_TYP+OCC_STAT+DTI,data=data1,ties="efron")
summary(coxp.v2)
AIC(coxp.v2)
#although this is not the lowest AIC, it has higher concordance than other options tested by trial & error
# coxp.v3
coxp.v3 <- coxph(formula = Surv(start,end,status=="prepaid") ~
                   CSCORE_B+pneq+OLTV+ue+cvr+DTI,data=data1,ties="efron")
summary(coxp.v3)
AIC(coxp.v3)



# Prediction
# set up variables
# For default prediction
n <- 60
CSCORE_B <- 720
pneq <- 0
spread <- median(data1$spread)
cvr <- median(data1$cvr)
status <- "default"
DTI <- median(data1$DTI, na.rm=TRUE)
data2 <- data.frame(rep(CSCORE_B, n), rep(pneq, n), rep(spread, n), rep(cvr, n), c(0:59), c(1:60), rep(status, n), rep(DTI, n) )
names(data2)<- c("CSCORE_B", "pneq", "spread", "cvr", "start", "end", "status", "DTI")
# Start predicting
pred2x <- predict(cox.v6, newdata = data2, type='expected')
pred2x <- 1-exp(-pred2x) # hazard rate each month given survival
s <- 1-pred2x    # survival rate each month given survival
cumsurv <- cumprod(s)   # cumulative survival prob each month
cumdef <- 1-cumsurv    # cumulative default prob each month
data2$Cumdef <- cumdef
library(ggplot2)
ggplot()+
  geom_line(data = data2, aes(data2$end, data2$Cumdef), color = "red")+
  ggtitle("Cumulative Default Probability")+
  xlab("Month")+
  ylab("Probability")

# For prepayment prediction
n <- 60
CSCORE_B <- 720
pneq <- 0
OLTV <- median(data1$OLTV)
ue <- median(data1$ue)
cvr <- median(data1$cvr)
DTI <- median(data1$DTI,  na.rm = TRUE)
status1 <- "prepaid"
# Prepaid data
data3 <- data.frame(rep(CSCORE_B, n), rep(pneq, n), rep(OLTV, n), rep(ue, n), rep(cvr, n), rep(DTI, n), c(0:59), c(1:60), rep(status1, n))
names(data3)<- c("CSCORE_B", "pneq", "OLTV", "ue", "cvr", "DTI", "start", "end", "status")
# Start predicting
pred3x <- predict(coxp.v3, newdata = data3, type='expected')
pred3x <- 1-exp(-pred3x) # hazard rate each month given survival
sp <- 1-pred3x    # survival rate each month given survival
cumsurvp <- cumprod(sp)   # cumulative survival prob each month
cumdefp <- 1-cumsurvp    # cumulative default prob each month
data3$Cumdef <- cumdefp

ggplot()+
  geom_line(data = data3, aes(data3$end, data3$Cumdef), color = "red")+
  ggtitle("Cumulative Prepayment Probability")+
  xlab("Month")+
  ylab("Probability")




