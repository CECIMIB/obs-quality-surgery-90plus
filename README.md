# Conceptual characteristics, methodological and reporting quality in observational studies of preoperative risk assessment and major non-cardiac surgery among nonagenarians and centenarians — Supplementary Data and Code Archive

## Overview

This repository contains the publication-level source matrix, variable dictionaries, complete descriptive and inferential outputs, display items (tables and figures), and analytical R scripts supporting the study: *"Conceptual characteristics, methodological and reporting quality in observational studies of preoperative risk assessment and major non-cardiac surgery among nonagenarians and centenarians"*.

In the accompanying manuscript, the datasets and scripts archived here are referenced as **Supplementary Data 1–7** (`.xlsx` workbooks) and **Supplementary Code 1–6** (`.R` scripts). These identifiers correspond directly to the repository inventory below and are independent of journal-specific "Additional file" numbering assigned during editorial production.

## Repository Inventory and Manuscript Linkage

| Repository Label | File Name | Manuscript Linkage |
|---|---|---|
| Supplementary Data 1 | `Supplementary_Data_01_Study_Characteristics_and_Domain_Denominators.xlsx` | Table 1; Tables S1–S2, S20 |
| Supplementary Data 2 | `Supplementary_Data_02_Full_Descriptive_Results_14_Blocks.xlsx` | Tables 1–2; Tables S1–S5, S20 |
| Supplementary Data 3 | `Supplementary_Data_03_Bivariate_Comparisons_3934.xlsx` | Tables S16–S18; Figure S6 |
| Supplementary Data 4 | `Supplementary_Data_04_Extended_Effect_Estimates.xlsx` | Table S19 |
| Supplementary Data 5 | `Supplementary_Data_05_STROBE_JBI_Multivariable_Models.xlsx` | Tables 2–3; Tables S3–S4, S7; Figure 1; Figures S1–S3, S5 |
| Supplementary Data 6 | `Supplementary_Data_06_Priority_Adjusted_Models.xlsx` | Table 3; Tables S6, S13–S15; Figures 2–3; Figures S4–S5 |
| Supplementary Data 7 | `Supplementary_Data_07_STROBE_JBI_Candidate_Associations.xlsx` | Table 3; Tables S8–S12; Figure 4; Figures S7–S8 |
| Supplementary Code 1 | `Supplementary_Code_01_Descriptive_14Blocks.R` | Generates Supplementary Data 2 |
| Supplementary Code 2 | `Supplementary_Code_02_Bivariate_Screening.R` | Generates Supplementary Data 3 |
| Supplementary Code 3 | `Supplementary_Code_03_Extended_Effect_Estimates.R` | Generates Supplementary Data 4 |
| Supplementary Code 4 | `Supplementary_Code_04_Ordinal_and_Fractional_Models.R` | Generates Supplementary Data 5 |
| Supplementary Code 5 | `Supplementary_Code_05_Priority_Models.R` | Generates Supplementary Data 6 |
| Supplementary Code 6 | `Supplementary_Code_06_STROBE_JBI_Candidate_Associations.R` | Generates Supplementary Data 7 |

## Directory Structure and Supporting Files

- **Main Tables:** `Tables/Manuscript_1_Main_Tables_V7.docx` (contains Main Tables 1–3).
- **Supplementary Tables:** `Tables/Manuscript_1_Supplementary_Tables_V7.docx` (contains numbered Supplementary Tables S1–S20; note that the document comprises 21 Word table objects because Table S19 is divided into two parts).
- **Figures:** `Figures/` contains the compiled vector PDFs for Main Figures 1–4 and Supplementary Figures S1–S8.
- **Crosswalk & Metadata:**
  - `Documentation/Crosswalk_Tables_Figures_Data_Code.csv`: Exact mapping between manuscript display items, datasets, and scripts.
  - `Documentation/Data_and_Code_Naming.csv`: Correspondence between legacy internal filenames and standardized repository identifiers.
  - `Documentation/File_Inventory_SHA256.csv`: Per-file SHA-256 checksums for data integrity verification.

## Source Matrix

The primary analytical input is `Input_Data/Source_Data_V7_128_Publications.xlsx` (129 rows including header × 375 columns). 
- **SHA-256 Checksum:** `fb08267557c71aafa76787e1ee0a12088149fcfa55e78a91da8530dfb3fba4dd`
- **Unit of Analysis:** This dataset is strictly publication-level ($N = 128$ published observational studies) and contains no individual patient-level records or protected health information.

## Execution Order and Command-Line Usage

To reproduce the analytical outputs from the root directory of the repository, execute the R scripts in sequential order by passing the explicit input, dictionary, and output paths as positional arguments:

```bash
# 1. Descriptive analyses across all 14 analytical blocks
Rscript Supplementary_Code/Supplementary_Code_01_Descriptive_14Blocks.R \
  Input_Data/Source_Data_V7_128_Publications.xlsx \
  Input_Data/Diccionario_370_variables_14_bloques.csv output/SC01_descriptive

# 2. Bivariate screening across candidate pairs
Rscript Supplementary_Code/Supplementary_Code_02_Bivariate_Screening.R \
  Input_Data/Source_Data_V7_128_Publications.xlsx \
  Input_Data/diccionario_variables_V7_ES.csv output/SC02_bivariate

# 3. Extended effect estimates (requires output/SC02_bivariate/Resultados_bivariados_P1_R.csv from Step 2)
Rscript Supplementary_Code/Supplementary_Code_03_Extended_Effect_Estimates.R \
  Input_Data/Source_Data_V7_128_Publications.xlsx \
  Input_Data/diccionario_variables_V7_ES.csv output/SC02_bivariate

# 4. Multivariable ordinal and fractional regression models
Rscript Supplementary_Code/Supplementary_Code_04_Ordinal_and_Fractional_Models.R \
  Input_Data/Source_Data_V7_128_Publications.xlsx output/SC04_models

# 5. Priority-adjusted regression models
Rscript Supplementary_Code/Supplementary_Code_05_Priority_Models.R \
  Input_Data/Source_Data_V7_128_Publications.xlsx output/SC05_priority

# 6. STROBE and JBI candidate association models
Rscript Supplementary_Code/Supplementary_Code_06_STROBE_JBI_Candidate_Associations.R \
  Input_Data/Source_Data_V7_128_Publications.xlsx output/SC06_candidates
