## per-code-analysis.R
##

library(ggplot2)
library(plotly)
library(dplyr)
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


claims.clean$PlaceOfService = as.factor(claims.clean$PlaceOfService)


generate.figs = function(df, dest.path, group.var){
  title.string = paste("For:", group.var, "Num Records:",nrow(df), 
                       "Num Unique Patients:", length(unique(df$Ee_NUM)))
  ggplot(df, aes(x = PlaceOfService, y = Paid_amount)) + geom_boxplot() + 
    ggtitle(title.string) -> p
  ggsave(filename = paste0(dest.path, group.var, ".png"), plot = p)
}


##------------------------------------------------------------------------------------------
##
## Procedure code
##
##------------------------------------------------------------------------------------------
proc.freq = as.data.frame(table(claims.clean$Hcpcscpt4))
proc.freq = as.data.frame(proc.freq[order(proc.freq$Freq, decreasing = TRUE),])
names(proc.freq)[1] = "Procedure.Code"
write.csv(proc.freq, file = "./processedData/procedure_code_ranks.csv", row.names = FALSE, quote = FALSE)

claims.clean %>%
  group_by(Hcpcscpt4) %>%
  do(generate.figs(., dest.path = "./figures/Paid_amount/procedure_code/", group.var = .$Hcpcscpt4))

##------------------------------------------------------------------------------------------
##
## Diag1 codes
##
##------------------------------------------------------------------------------------------
Diag1.freq = as.data.frame(table(claims.clean$Diag1))
Diag1.freq = Diag1.freq[order(Diag1.freq$Freq, decreasing = TRUE),]
names(Diag1.freq)[1] = "Diag1.Code"
write.csv(Diag1.freq, file = "./processedData/Diag1_code_ranks.csv", row.names = FALSE, quote = FALSE)

claims.clean %>%
  group_by(Diag1) %>%
  do(generate.figs(., dest.path = "./figures/Paid_amount/Diag1/", group.var = .$Diag1))


##------------------------------------------------------------------------------------------
##
## Patient ranks
##
##------------------------------------------------------------------------------------------

patient.freq = as.data.frame(table(claims.clean$Ee_NUM))
patient.freq = patient.freq[order(patient.freq$Freq, decreasing = TRUE),]
names(patient.freq)[1] = "Ee_NUM.code"
write.csv(patient.freq, file = "./processedData/patient_ranks.csv", row.names = FALSE, quote = FALSE)


generate.patient.figs = function(df, dest.path, group.var){
  title.string = paste("For Ee_NUM:", group.var, "Num Records:",nrow(df), 
                       "\n Num Unique Procedure Codes:", length(unique(df$Hcpcscpt4)),
                       "\n Num Unique Diag1:", length(unique(df$Diag1)))
  ggplot(df, aes(x = PlaceOfService, y = Paid_amount)) + geom_boxplot() + 
    ggtitle(title.string) -> p
  ggsave(filename = paste0(dest.path, group.var, ".png"), plot = p)
}


claims.clean %>%
  group_by(Ee_NUM) %>%
  do(generate.patient.figs(., dest.path = "./figures/Paid_amount/Ee_NUM/", group.var = .$Ee_NUM))
