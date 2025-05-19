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

#Extract raw codes
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

analysis <- cprd$analysis("all_patid")
valid_dates <- valid_dates %>% analysis$cached("death_end_dat")

#raw_AD_SSRI_prodcodes %>% count() - 

#Clean codes
for(i in drugs){
  
  clean_tablename <- paste0("clean_", i, "_prodcodes")
  raw_tablename <- paste0("raw_", i, "_prodcodes")
  drug_class <- i
  
  #Names for dynamically saving table
  current_cat_sym <- sym(current_cat)
  index_cols <- c("patid", "date", "chem_name")
  
  print(paste0("Making ", drug_class, " clean prodcode table"))
  
  #Create clean table.
  data <- get(raw_tablename) %>% inner_join(valid_dates, by="patid") %>%
    filter(issuedate>=min_dob & issuedate<=gp_death_end_date) %>%
    mutate(drug_class = drug_class) %>% 
    select(patid, date=issuedate, dosageid, quantity, quantunitid, duration, chem_name = !!current_cat_sym, drug_class) %>%
    analysis$cached(clean_tablename, indexes=index_cols)
}



