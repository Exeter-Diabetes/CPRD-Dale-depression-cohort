# CPRD depression cohort
WORK IN PROGRESS

To add: 
  - Depression and biomarker code lists
  - Further QC steps to flow chart
  - Upload latest scripts
  

## Cohort definition
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


```mermaid
graph TD;
    A["<b>CPRD Aurum December 2023 release</b> with linked November 2023 HES APC, and patient IMD"] --> |"Unique patients with a depression-related medcode between 1988 - 30/12/2023"| B["<b>Our extract</b>: n=3,984,014"]
    B -->|"Patient from one of 44 practices which may have merged (recommended to remove in CPRD Aurum Data Specification v3.4)"|C["n=48,912"]
    B -->|"Patients with gender==3 (indeterminate)"|D["n=348"]
    B --> E["n=3,934,754"]
    E -->|"With a depression QOF code with a valid date"|F["n=3,376,433"]
```
