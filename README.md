# CPRD depression cohort
WORK IN PROGRESS

To add: 
  - Depression and biomarker code lists
  -   - Upload latest scripts
  

## Cohort definition
This Github details the construction of the CPRD depression cohort using the February 2024 CPRD data extraction.
The cohort is defined using the following criteria:

**Inclusion:**
  - A Quality Outcome Framework code for depression
  - If the Quality Outcome Framework code is not diagnostic, another diagnostic "broad" depression code

**Exclusion:**
  - Patient is from one of the 44 practices that may have merged (unreliable patient data tracking)
  - Undetermined Gender
  - Non-diagnostic depression codes present at first evidence of depression (see broad_depression code list).
  - Diagnosed with a severe mental illness (excluding depression with psychosis) prior to depression.
  - Diagnosed with depression after introduction depression into the Quality and Outcomes Framework (01/04/2025)



## Index date definition
While all individuals were required to have a QOF depression medcode for inclusion in this study, we allowed index dates to reflect the earliest occurrence of depression identified using the 
"Broad depression" phenotype.


![image](https://github.com/user-attachments/assets/62484341-d2f9-4ded-bad3-4bfb15fdfacb)

# Further adjustments of index date

## 



# Depression severity/biotypes

## Public health Questionnaire 9 (PHQ-9) data processing

### Description
The PHQ-9 is a questionnaire that is used for both depression screening and severity testing.
The questionnaire is composed of 9 questions, each of which can have a maximum value of 3 points. For diagnosis, the following categories are sometimes used:

  - **Score 10+:** Cut off score used to denote a current episode of depression.
  - **Score 10 - 14:** Moderate depression
  - **Score 15 - 19:** Moderately severe depression
  - **Score 20+:** Severe depression

### QC process
While the most common SNOMED-CT codes for PHQ-9 relate to individuals
