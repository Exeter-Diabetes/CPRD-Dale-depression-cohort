library(aurum)
library(tidyverse)
rm(list = ls())


####Connection and code lists####
#Open connection
#Connect to the MySQL database
cprdenvname <- ""
yaml <- ""

#open connection and get codes sets
cprd = CPRDData$new(cprdEnv = cprdenvname, cprdConf = yaml)
codesets <- cprd$codesets()
codes <- codesets$getAllCodeSetVersion("18/05/2025")

analysis <- cprd$analysis("all_patid")

drugs <- names(codes)

for (i in drugs){
  
  if (length(codes[[i]]) > 0) {
    print(paste("making", i, "prodcode table"))
    
    raw_tablename <- paste0("raw_", i, "_prodcodes")
    
    data <- cprd$tables$drugIssue %>%
      inner_join(codes[[i]], by="prodcodeid") %>%
      analysis$cached(raw_tablename, indexes=c("patid", "issuedate"))
    
    
    assign(raw_tablename, data)
    
  }
}
