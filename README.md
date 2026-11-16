# Manuscript 1 V7 — Supplementary data and R code archive (DRAFT)

## Status and manuscript use

**NOT FINAL OR VALIDATED FOR ZENODO RELEASE.** This archive preserves the 13 uploaded spreadsheet/script files byte-for-byte under stable descriptive names. It also includes the V7 source matrix, two recovered dictionaries, a compatibility reference copy, the existing three main and 20 numbered supplementary tables, and the 4 main plus 8 supplementary figure PDFs. No published Zenodo DOI has been assigned. The R scripts have **not** been executed in this environment because Rscript is unavailable. Some were independently written to reproduce results originally calculated in Python; exact numerical agreement has **not** been established.

The manuscript may cite the labels **Supplementary Data 1–7** for XLSX evidence files, and **Supplementary Code 1–6** for R scripts, provided the labels are defined in the repository inventory. These labels are **Zenodo asset identifiers**, not Springer Nature's journal submission 'Additional file' numbering. The journal 'Additional file' numbers must be set separately in final order of citation. The file names are stable within this draft archive.

## Exact file names for manuscript and Zenodo

| Manuscript label | Exact file name | Main linkage |
|---|---|---|
| Supplementary Data 1 | `Supplementary_Data_01_Study_Characteristics_and_Domain_Denominators.xlsx` | T1; S1-S2; S20 |
| Supplementary Data 2 | `Supplementary_Data_02_Full_Descriptive_Results_14_Blocks.xlsx` | T1-T2; S1-S5; S20 |
| Supplementary Data 3 | `Supplementary_Data_03_Bivariate_Comparisons_3934.xlsx` | S16-S18; F S6 |
| Supplementary Data 4 | `Supplementary_Data_04_Extended_Effect_Estimates.xlsx` | S19 |
| Supplementary Data 5 | `Supplementary_Data_05_STROBE_JBI_Multivariable_Models.xlsx` | T2-T3; S3-S4; S7; F1, FS1-FS3, FS5 |
| Supplementary Data 6 | `Supplementary_Data_06_Priority_Adjusted_Models.xlsx` | T3; S6; S13-S15; F2-F3; FS4-FS5 |
| Supplementary Data 7 | `Supplementary_Data_07_STROBE_JBI_Candidate_Associations.xlsx` | T3; S8-S12; F4; FS7-FS8 |
| Supplementary Code 1 | `Supplementary_Code_01_Descriptive_14Blocks.R` | SD02 |
| Supplementary Code 2 | `Supplementary_Code_02_Bivariate_Screening.R` | SD03 |
| Supplementary Code 3 | `Supplementary_Code_03_Extended_Effect_Estimates.R` | SD04 |
| Supplementary Code 4 | `Supplementary_Code_04_Ordinal_and_Fractional_Models.R` | SD05 |
| Supplementary Code 5 | `Supplementary_Code_05_Priority_Models.R` | SD06 |
| Supplementary Code 6 | `Supplementary_Code_06_STROBE_JBI_Candidate_Associations.R` | SD07 |

## Relationship to the manuscript

- Three main tables: `Tables/Manuscript_1_Main_Tables_V7.docx`.
- Twenty **numbered** supplementary tables S1–S20: `Tables/Manuscript_1_Supplementary_Tables_V7.docx`. The Word file contains 21 table objects because S19 contains two; no S21 identifier exists.
- Four main and eight supplementary Figure PDFs under `Figures/`. The main-versus-supplementary selection is provisional, pending editorial agreement.
- Exact element-to-data/code correspondence: `Documentation/Crosswalk_Tables_Figures_Data_Code.csv`.
- Exact original-name to new-name correspondence: `Documentation/Data_and_Code_Naming.csv`.
- Per-file SHA-256: `Documentation/File_Inventory_SHA256.csv`.

## Source matrix identity

`Input_Data/Source_Data_V7_128_Publications.xlsx` is the unmodified source matrix recovered from the prior bivariate analysis bundle. SHA-256: **fb08267557c71aafa76787e1ee0a12088149fcfa55e78a91da8530dfb3fba4dd**. It matches the source fingerprint recorded in the priority-model workbook. A second V7 file has another binary SHA-256, but a cell-by-cell comparison of all 129 rows × 375 columns showed identical values; it is not copied into this archive to avoid ambiguity.

This matrix is **publication level** (128 publications). It is not a patient-level dataset. Source titles and bibliographic data need normal licensing/copyright review before public distribution; there are no individual-patient records implied by this description.

## Dependencies and execution order

The packaged scripts are **unchanged**. To run them, supply positional arguments with the actual paths rather than relying on their internal legacy default names. Example commands from the root of the extracted package, after installing R packages required by each script:

```bash
Rscript Supplementary_Code/Supplementary_Code_01_Descriptive_14Blocks.R \
  Input_Data/Source_Data_V7_128_Publications.xlsx \
  Input_Data/Diccionario_370_variables_14_bloques.csv output/SC01_descriptive

Rscript Supplementary_Code/Supplementary_Code_02_Bivariate_Screening.R \
  Input_Data/Source_Data_V7_128_Publications.xlsx \
  Input_Data/diccionario_variables_V7_ES.csv output/SC02_bivariate

# Requires `output/SC02_bivariate/Resultados_bivariados_P1_R.csv` from SC02.
Rscript Supplementary_Code/Supplementary_Code_03_Extended_Effect_Estimates.R \
  Input_Data/Source_Data_V7_128_Publications.xlsx \
  Input_Data/diccionario_variables_V7_ES.csv output/SC02_bivariate

Rscript Supplementary_Code/Supplementary_Code_04_Ordinal_and_Fractional_Models.R \
  Input_Data/Source_Data_V7_128_Publications.xlsx output/SC04_models

Rscript Supplementary_Code/Supplementary_Code_05_Priority_Models.R \
  Input_Data/Source_Data_V7_128_Publications.xlsx output/SC05_priority

Rscript Supplementary_Code/Supplementary_Code_06_STROBE_JBI_Candidate_Associations.R \
  Input_Data/Source_Data_V7_128_Publications.xlsx output/SC06_candidates
```

**Important R code specifics:**

1. `SC03` expects the 10,290 screening pairs from `SC02` and will not run independently; its 3,934 evaluable pair comparisons are a subset.
2. `SC04` is **not** an exact replication of the ordinal GEE model originally estimated in Python; the script uses `ordinal::clmm` for an ordinal conditional model, which changes the estimand. It may be described as an alternative model only after checking its execution and convergence.
3. `SC05` and `SC06` are independent R implementations of Python reference analyses, requiring numerical comparison before reproducibility claims.
4. `SC06` looks for the original reference workbook name in `dirname(input)` to produce its R-vs-Python check. The matching compatibility workbook has been included in `Input_Data/` with that exact name.
5. Figure-specific R code for F1–F4 and FS1–FS8 is **not among these 13 newly supplied files**, and this archive therefore does **not** currently constitute a complete figure-generation package. The PDFs are included as frozen display outputs, not as proof of regeneration.
6. The 14-block descriptive workbook (SD02) and the domain workbook (SD01) overlap and include some information relating to the companion thesis article. SD01 is a complete upstream domain dataset, not exclusively a P1 final supplementary table. Avoid conflating those domains in the narrative.
7. Global BH correction of 3,934 bivariate contrasts, the separate family of extended effect estimates, 24 priority model coefficients, and the 29/23 adjusted candidate comparison families must remain distinct. Do not interpret a nominal p value as globally adjusted.
8. JBI evaluations in cohort studies (n=55) and other observational designs use different checklists and should not be pooled into a common validated quality metric.

## Proposed manuscript wording (when DOI exists)

*Methods / Statistical analysis*: "The complete descriptive and inferential outputs, variable dictionaries, and R analysis scripts are archived as Supplementary Data 1–7 and Supplementary Code 1–6 in the study repository [Zenodo DOI]."

*Results, example*: "Complete criterion-level distributions are provided in Supplementary Data 5, with the underlying analyses available in Supplementary Code 4 [Zenodo DOI]." Do not call that script an exact reproduction of GEE until equivalence is resolved.

*Data availability*: "The publication-level dataset, supporting analysis tables, and analysis code will be deposited in Zenodo [persistent DOI to be inserted once published]." **This is future-tense draft wording; replace it only after upload and verification.** For an article already submitted after deposit, write "are available" and supply the actual DOI.

For Springer Nature submission, list the journal's **Additional files** with filename, format, title and short description, and cite them in order in the manuscript. The Zenodo DOI is a repository citation in the reference list and Data Availability statement. Neither the DOI nor final Additional file numbers have been assigned here.

## Release checklist

- [ ] Execute SC01–SC06 with a recorded R version and package versions.
- [ ] Numerically compare outputs to SD01–SD07 and explain any disparities.
- [ ] Reconcile the ordinal GEE / conditional clmm distinction.
- [ ] Verify every Figure PDF against its figure-specific R script, and add those scripts with frozen inputs.
- [ ] Verify all table captions, n/N denominators, p/q families, and any category labeling inconsistencies.
- [ ] Check source-data sharing rights, provenance, author information, licenses, and registry metadata.
- [ ] Assign repository version, license, DOI and release date only after final validation.
