library(dplyr)
library(data.table)
library(openxlsx)
library(aurum)
library(tidyverse)
library(stringdist)
rm(list = ls())
setwd("")


#Load in the code lists taken from UK Biobank.
read2 <- fread("antidepressants/read2_unique_drugnames_mapped.txt")
dmd <- fread("antidepressants/dmd_unique_drugnames_mapped.txt")
bnf <- fread("antidepressants/bnf_unique_drugnames_mapped.txt")
drug_db <- fread("CPRDAurumProduct.txt")

#Collect the drug names into one vector
drug_dict <- read2 %>% select(chem_name, functional_class) %>% 
  bind_rows(dmd %>% select(chem_name, functional_class)) %>% 
  bind_rows(bnf %>% select(chem_name, functional_class))

#Check functional classes
table(drug_dict$functional_class)

#Remove amino_acid_supplement as it can be difficult to know if it's for depression or not, 
#Especially if it's a formulation. 
drug_dict <- drug_dict %>% filter(!functional_class == "amino_acid_supplement")


#First, we try and get as many direct dmd codes as we can:
dmd_matches <- drug_db %>% 
  inner_join(dmd %>% select(dmd_code, functional_class) %>% filter(functional_class != "amino_acid_supplement"), by = c( "dmdid" = "dmd_code")) 
#298 matches - not that surprising as there are a lot of duplicate dmd codes

#next, we take the full list of distinct substance names, and create a simple dict of substance
drug_table <- drug_dict %>% distinct(chem_name, .keep_all = T)
drug_table <- drug_table %>% mutate(functional_class = ifelse(functional_class == "other", "atypical", functional_class))

#Clean up the groupings to make them a bit simpler: 
drug_table <- drug_table %>% mutate(functional_class = case_when(
  functional_class == "MAOI_typical_antipsychotic" ~ "MAOI_apsych",
  functional_class == "phenothiazine_antipsychotic" ~ "phenothiazine_apsych", 
  functional_class == "tetracyclic_antidepressant" ~ "tetracyclic", 
  functional_class == "tricyclic_antidepressant" ~ "tricyclic", 
  functional_class == "tricyclic_antidepressant_typical_antipsychotic" ~ "tricyclic_apsych", 
  TRUE ~ functional_class
))

chem_names <- c("phenelzine", "isocarboxazid", "tranylcypromine", 
                "tranylcypromine_trifluoperazine", "amitriptyline_perphenazine")

df <- data.frame(
  id = 1:4,
  text = c("Patient is on phenelzine and fluoxetine",
           "Started amitriptyline and perphenazine",
           "Taking tranylcypromine with trifluoperazine",
           "Given isocarboxazid"),
  stringsAsFactors = FALSE
)

# Function to find matching chem_name
match_chem_name <- function(text_row, chem_names) {
  matches <- c()
  for (drug in chem_names) {
    if (grepl("_", drug)) {
      parts <- unlist(strsplit(drug, "_"))
      if (all(sapply(parts, function(part) grepl(part, text_row, ignore.case = TRUE)))) {
        matches <- c(matches, drug)
      }
    } else {
      if (grepl(drug, text_row, ignore.case = TRUE)) {
        matches <- c(matches, drug)
      }
    }
  }
  if (length(matches) == 0) {
    return(NA)
  } else {
    return(paste(matches, collapse = "; "))
  }
}



drug_db$matched_drugs <- sapply(drug_db$DrugSubstanceName, match_chem_name, chem_names = drug_table$chem_name)
drug_db_matched <- drug_db %>% filter(!is.na(matched_drugs))

drug_db_matched$best_match <- sapply(drug_db_matched$DrugSubstanceName, function(row_text) {
  dists <- stringdist(row_text, drug_table$chem_name, method = "jw")  # Jaro-Winkler for better word match
  best_index <- which.min(dists)
  drug_table$chem_name[best_index]
})

#This should be the list - let's compare to the dm+d list.
#drug_db_matched %>% filter(dmdid %in% bnf_matches$dmdid) %>% count() # only 90 matches - mine fetches more drugs.

drug_db_merged <- drug_db_matched %>% rename("chem_name" = best_match) %>% 
  inner_join(drug_table) %>% 
  mutate(
    ProdCodeId = as.character(ProdCodeId), 
    chem_name = str_trim(chem_name))


#Save the names and the classes
drug_classes <- drug_db_merged %>% distinct(functional_class) %>% pull()

#Open up the connection
####Connection and code lists####
#Open connection
#Connect to the MySQL database
cprdenvname <- ""
yaml <- ""

#open connection and get codes sets
cprd = CPRDData$new(cprdEnv = cprdenvname, cprdConf = yaml)
codesets <- cprd$codesets()




#Setup a loop where I add all values to dictionary
for(drug_class in drug_classes){
  
  #Set vars
  working_df <- drug_db_merged %>% filter(functional_class == drug_class)
  current_name <- paste0("AD_", drug_class)
  
  codesets$deleteCodeSet(current_name)
  working_df %>% codesets$loadProdCodeSet(name = current_name, version = "18/05/2025", category = "chem_name", colname = "ProdCodeId")
  
}


#Write a list of the names:
fwrite(data.frame(drug_db_merged), "", row.names = F, quote = F, sep = "\t", col.names = F)
