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

##------------------------------------------------------------------------------
##
## Procedure code analysis 
##
##------------------------------------------------------------------------------


proc.freq = as.data.frame(table(claims.clean$Hcpcscpt4))
proc.freq = as.data.frame(proc.freq[order(proc.freq$Freq, decreasing = TRUE),])


claims.clean %>%
  group_by(Hcpcscpt4) %>%
  do(generate.figs(., dest.path = "./figures/Paid_amount/procedure_code/", group.var = .$Hcpcscpt4))

