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
Antidepressant use was considered as a primary indicator of depression diagnosis. Given that non-SSRIs (especially since the 1990s) have been the primary first-line antidepressant for depression treatment, we only considered SSRIs as an indicator. The follow criteria were used to define an antidepressant-informed index date: 

- The patient must receive an SSRI as their first antidepressant. 
- The patient must receive their first antidepressant prescription more than 90 days after their current GP practice registration. This approach prevents legacy prescriptions being treated as a new depressive episode.
- In line with our QOF guideline-based cohort design, individuals must receive their first antidepressant prescription after the introduction of depression to the QOF (01/04/2006). 

From this point, individuals had two index dates:
- A date at which they first received a diagnostic depression code
- A date at which, if ever, they first received an antidepressant.

For all individuals, their depression index date was defined as the earliest of these two dates. 

## Further QC considerations

# Clinical heterogeneity in depression
All scripts and codes related to these phenotypes are included in cohort_scripts/depression_phrenotypes section of this Repo.
Where possible, all phenotypes were defined as events, besides antidepressant adherence.

## Public Health Questionnaire-9 results
The PHQ-9 is a questionnaire that is used for both depression screening and severity testing.
The questionnaire is composed of 9 questions, each of which can have a maximum value of 3 points. For diagnosis, the following categories are sometimes used:

  - **Score 10+:** Cut off score used to denote a current episode of depression.
  - **Score 10 - 14:** Moderate depression
  - **Score 15 - 19:** Moderately severe depression
  - **Score 20+:** Severe depression

A PhQ-9 score at index was defined as having been recorded within 30 days before or 7 days after either receiving their first antidepressant or depression code. The 7 day excess window is allow for delayed entry time into EMIS. 

## Antidepressant use
Antidepressant use was defined as having a prescription of any antidepressant, as outlined in the sections above.

## Trying a different antidepressant of the same class (SSRI)
This was defined as the first instance at which an individual  received a prescription for another SSRI (as all individuals were required to try an SSRI first as part of the inclusion criteria). 

## Trying another antidepressant class
This was defined as the first instance at which an individual received a prescription for a non-SSRI antidepressant prescription. 

## Antidepressant coverage
Over a given time frame, antidepressant use was calculated using Percentage days covered, as described by https://pubmed.ncbi.nlm.nih.gov/40696379/. As it might be expected that individuals are not required all the time, PDC more accurately describes the amount of time covered by antidepressant prescriptions as a measure of ongoing depression treatment rather than a proxy for medication adherence.

## Treatment resistant depression
For treatment resistance, we used a previously published definition. Briefly, prescriptions were split into 6 month windows, where a new episode began when there was more than 6 months since the previous antidepressant prescription. Treatment resistance was defined as receiving a third antidepressant within given time frames, in line with clinical prescribing guidelines (https://pubmed.ncbi.nlm.nih.gov/33753889/). 

## Augmentation therapy with antipsychotics
Augmentation therapy was considered as receiving one of four widely used antipsychotics for augmentation therapy:
- Quetiapine
- Rispiradone
- Aripiprazole
- Olanzapine

We also considered two definitions:
- **Clinical definition**: Defined according to Fabbri et al 2021 (https://pubmed.ncbi.nlm.nih.gov/33753889/). Individuals were required to have a prescription overlap of at least 30 days with an antidepressant. This definition is very strict but is closest to approximating the guidelines for antipsychotic augmentation in depression.
- **Real-world definition**: In this case, we require a single prescription for one of these antidepressant to be considered as augmentation. 



# Public health Questionnaire 9 (PHQ-9) data processing

