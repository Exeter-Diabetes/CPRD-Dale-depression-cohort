library(tidyverse)
library(aurum)
rm(list=ls())

cprdenvname <- ""
yaml <- ""

#open connection and get codes sets
cprd = CPRDData$new(cprdEnv = cprdenvname, cprdConf = yaml)
codesets = cprd$codesets()
codes = codesets$getAllCodeSetVersion(v = "31/10/2021")


comorbids <- c("af", #atrial fibrillation
               "angina",
               "asthma",
               "bronchiectasis",
               "ckd5_code",
               "cld",
               "copd",
               "cysticfibrosis",
               "dementia",
               "diabeticnephropathy",
               "haem_cancer",
               "heartfailure",
               "hypertension",
               "ihd", #ischaemic heart disease
               "myocardialinfarction",
               "neuropathy",
               "otherneuroconditions",
               "pad", #peripheral arterial disease
               "pulmonaryfibrosis",
               "pulmonaryhypertension",
               "retinopathy",
               "revasc", #revascularisation procedure
               "rheumatoidarthritis",
               "solid_cancer",
               "solidorgantransplant",
               "stroke",
               "tia"  #transient ischaemic attack
)

analysis = cprd$analysis("all_patid")

for (i in comorbids) {
  
  if (length(codes[[i]]) > 0) {
    print(paste("making", i, "medcode table"))
    
    raw_tablename <- paste0("raw_", i, "_medcodes")
    
    data <- cprd$tables$observation %>%
      inner_join(codes[[i]], by="medcodeid") %>%
      analysis$cached(raw_tablename, indexes=c("patid", "obsdate"))
    
    assign(raw_tablename, data)
    
  }
  
  if (length(codes[[paste0("icd10_", i)]]) > 0 & i!="hypertension") {
    print(paste("making", i, "ICD10 code table"))
    
    raw_tablename <- paste0("raw_", i, "_icd10")
    
    data <- cprd$tables$hesDiagnosisEpi %>%
      inner_join(codes[[paste0("icd10_",i)]], sql_on="LHS.ICD LIKE CONCAT(icd10,'%')") %>%
      analysis$cached(raw_tablename, indexes=c("patid", "epistart"))
    
    assign(raw_tablename, data)
    
  }
  
  if (length(codes[[paste0("opcs4_", i)]]) > 0) {
    print(paste("making", i, "OPCS4 code table"))
    
    raw_tablename <- paste0("raw_", i, "_opcs4")
    
    data <- cprd$tables$hesProceduresEpi %>%
      inner_join(codes[[paste0("opcs4_",i)]],by=c("OPCS"="opcs4")) %>%
      analysis$cached(raw_tablename, indexes=c("patid", "evdate"))
    
    assign(raw_tablename, data)
    
  }
  
}
