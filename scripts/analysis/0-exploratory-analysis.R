## exploratory-analysis.R
##

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


claims.clean$From_date = as.Date(claims.clean$From_date,format="%m/%d/%Y")
claims.clean$Thru_date = as.Date(claims.clean$Thru_date,format="%m/%d/%Y")
claims.clean$num.days = as.numeric(claims.clean$Thru_date - claims.clean$From_date)

ggplot(subset(claims.clean, num.days > 0 & num.days < 365), aes(x = log10(num.days), y = Paid_amount, size = log10(num.days)*Paid_amount)) + 
  geom_point(alpha = .1) -> p


## replace NA revenue with "1"
claims.clean$Revenue[is.na(claims.clean$Revenue)] = 1

claims.clean$PlaceOfService = as.factor(claims.clean$PlaceOfService)

ggplot(subset(claims.clean, num.days > 0 & num.days < 365), aes(x = num.days, y = Paid_amount/Revenue)) + 
  geom_point(alpha = .5) -> p


ggplot(subset(claims.clean, num.days > 0 & num.days < 365), aes(x = log10(num.days), y = Paid_amount, colour = PlaceOfService)) + 
  geom_point(alpha = .9, size = .8) -> p

claims.clean$from.month = as.factor(month(claims.clean$From_date))

ggplot(subset(claims.clean, num.days == 0 & Paid_amount > 0), aes(x = from.month, y = Paid_amount)) + 
  geom_boxplot() + facet_wrap(~PlaceOfService, scales = "free_y") -> p

patient.freq = as.data.frame(table(claims.clean$Ee_NUM))
patient.freq = patient.freq[order(patient.freq$Freq, decreasing = TRUE),]

top.1K.patients = as.character(patient.freq[c(1:1000), "Var1"])
top.100.patients = as.character(patient.freq[c(1:100), "Var1"])

ggplot(subset(claims.clean, num.days == 0 & Paid_amount > 0 & Ee_NUM %in% top.100.patients), aes(x = from.month, y = Paid_amount)) + 
  geom_boxplot() + facet_wrap(~PlaceOfService, scales = "free_y") -> p

ggplot(subset(claims.clean, num.days == 0 & Paid_amount > 0), aes(x = from.month, y = log10(Paid_amount), colour = top.1K)) + 
  geom_point(size = .2) + geom_boxplot() + facet_wrap(~PlaceOfService, scales = "free_y") -> p

ggplot(subset(claims.clean, num.days > 0 & num.days < 365), aes(x = Revenue, y = log(Paid_amount))) + 
  geom_point(alpha = .9, size = .8) -> p



p = ggplot(claims.clean, aes(x = num.days)) + geom_histogram(fill = "white", size=1) + geom_rug(aes(y=-2), position="jitter", sides="b", alpha = .1)

p = ggplot(claims.clean, aes(x = num.days, y = Paid_amount)) + geom_point(alpha = .5) +  geom_rug(alpha = .1)
