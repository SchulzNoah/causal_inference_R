# Causal Analysis of Parental Education on Income
## Data: [ALLBUS 2021, ZA5280](https://search.gesis.org/research_data/ZA5280)

This repository contains an empirical causal inference analysis investigating the effect of parental education on individual income in Germany using the **ALLBUS 2021 (ZA5280)** survey dataset. 

The analytical pipeline addresses missing data via **Multiple Imputation by Chained Equations (MICE)**, handles confounding via **Propensity Score Matching (PSM)**, and estimates pooled outcome regressions using **HC3-robust standard errors** and **survey design weights**.

---

## 📌 Methodological Overview

1. **Data Preprocessing & Filtering**:
   - Sample restricted to the working-age population (ages 18–65).
   - Recoding of parental education (`edu_eltern`), parental migration background (`mig_eltern`), region (`west_de`), and age polynomials.
2. **Missing Data Analysis & Imputation**:
   - Missingness patterns evaluated via Little's MCAR test and `naniar` diagnostics.
   - $m = 15$ imputations performed using Predictive Mean Matching (`pmm`) for continuous income and Logistic Regression (`logreg`) for binary covariates.
3. **Causal Inference & Modeling**:
   - **Naive & Adjusted OLS**: Pooled (multiple) linear regressions without matching.
   - **Propensity Score Matching**: 1:1 Nearest-Neighbor PSM.
   - **Balance Diagnostics**: Evaluation of absolute Standardized Mean Differences ($\text{|SMD|} < 0.10$) across all 15 imputed datasets.
   - **Outcome Estimation**: Pooled OLS regressions on matched datasets.

---

## 📁 Repository Structure

```text
├── Outputs/                          
│   ├── Übersicht_Missing_Values.xlsx
│   ├── MCAR_Test.xlsx
│   ├── Naiver_Schätzer_Treatment.xlsx
│   ├── Adjustierter_Schätzer_Treatment.xlsx
│   ├── Breusch_Pagan_Tests.xlsx
│   ├── Balance-Check_Matching.xlsx
│   └── Propensity_Score_Matching.xlsx
├── Leistungsnachweis_Kausale Inferenz.R  
└── README.md                         
