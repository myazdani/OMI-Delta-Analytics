## top-patient-analysis.R
##

library(ggplot2)
library(plotly)
setwd("~/Documents/OMI/Delta/")

claims = read.csv("./data/cleanData/claims.csv", header = TRUE, stringsAsFactors = FALSE)
RXclaims = read.csv("./data/cleanData/RXclaims.csv", header = TRUE, stringsAsFactors = FALSE)
patient.info = read.csv("./data/cleanData/patientInfo.csv", header = TRUE, stringsAsFactors = FALSE)


##
## Remove columns that have NA for all records (rows)
##
get.NA.cols = function(df, NA.thresh){
  na.cols = sapply(df, FUN = function(x) length(which(is.na(x))) == NA.thresh)
  return(which(na.cols))
}

claims.clean = claims[,-get.NA.cols(claims, nrow(claims))]
## RXclaims.clean = RXclaims[,-get.NA.cols(RXclaims, nrow(RXclaims))] RXclaims is all clean!
patient.clean = patient.info[,-get.NA.cols(patient.info, nrow(patient.info))]


## 
## patient.info notes:
## - GroupNumber and PlanNumber are identical 
## - Rx_bene_available is always Y
## - Rx_data_available is always N
##

claims.clean$From_date = as.Date(claims.clean$From_date,format="%m/%d/%Y")
claims.clean$Thru_date = as.Date(claims.clean$Thru_date,format="%m/%d/%Y")
claims.clean$num.days = as.numeric(claims.clean$Thru_date - claims.clean$From_date)

claims.clean$PlaceOfService = as.factor(claims.clean$PlaceOfService)


patient.freq = as.data.frame(table(claims.clean$Ee_NUM))
patient.freq = patient.freq[order(patient.freq$Freq, decreasing = TRUE),]

proc.freq = as.data.frame(table(claims.clean$Hcpcscpt4))
proc.freq = as.data.frame(proc.freq[order(proc.freq$Freq, decreasing = TRUE),])
ggplot(proc.freq, aes(x = c(1:nrow(proc.freq)), y = Freq, label = Var1)) + geom_text() -> p
ggplotly(p)

library(dplyr)

claims.clean %>%
  group_by(Hcpcscpt4) %>%
  summarise(num.records = n(), 
            total.Paid = sum(Paid_amount),
            num.patients = length(unique(Ee_NUM))) %>%
  as.data.frame() -> res

res$num.records.per.patient = res$num.records/res$num.patients
res = res[order(res$num.records.per.patient, decreasing = TRUE), ]

ggplot(res, aes(x = c(1:nrow(res)), y = num.records.per.patient, label = Hcpcscpt4)) + 
  geom_text() -> p


ggplot(res, aes(x = num.records/num.patients, y = total.Paid, size = num.patients, label = Hcpcscpt4)) + geom_text() -> p
