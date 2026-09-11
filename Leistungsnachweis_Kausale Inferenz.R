# Leistungsnachweis: Grundlagen der kausalen Inferenz ---------------------



# Laden relevanter Packages -----------------------------------------------

# Einlesen von ALLBUS im dta-Format
if (!require("haven")) install.packages("haven"); library(haven)
# Multiple Imputation
if (!require("mice")) install.packages("mice"); library(mice) 
# Data Wrangling, Cleaning etc.
if (!require("tidyverse")) install.packages("tidyverse"); library(tidyverse)
# Propensity Score Matching
if (!require("MatchIt")) install.packages("MatchIt"); library(MatchIt) 
# Missing-Data-Analyse
if (!require("naniar")) install.packages("naniar"); library(naniar)
# Breusch-Pagan-Test
if (!require("lmtest")) install.packages("lmtest"); library(lmtest) 
# Berechnung robuste Standardfehler
if (!require("sandwich")) install.packages("sandwich"); library(sandwich) 
# Robuste Standardfehler gepoolte Regressionsmodelle
if (!require("parameters")) install.packages("parameters"); library(parameters)
# Export in Excel-Tabellen
if (!require("writexl")) install.packages("writexl"); library(writexl) 


# Einlesen des ALLBUS 2021 ------------------------------------------------


allbus = read_dta("Data/ZA5280_v2-1-0.dta")


# Data Cleaning -----------------------------------------------------------


df_edu = allbus |> 
  mutate(
    # Umkodieren von Bildung der Mutter
    # Wenn mind. Fachhochschule 1, ansonsten 0
    # NA bei keiner Angabe oder anderer Abschluss
    meduc = case_when(
      meduc %in% c(1:3) ~ 0,
      meduc %in% c(4:5) ~ 1,
      meduc < 1         ~ NA,
      meduc == 6        ~ NA),
    
    # Umkodieren von Bildung des Vaters
    # Wenn mind. Fachhochschule 1, ansonsten 0
    # NA bei keiner Angabe oder anderer Abschluss
    feduc = case_when(
      feduc %in% c(1:3) ~ 0,
      feduc %in% c(4:5) ~ 1,
      feduc < 1         ~ NA,
      feduc == 6        ~ NA),
    
    # neue Variable für höchsten Bildungsabschluss der Eltern
    # wenn mind. 1 Elternteil mind. Fachhochschulreife hat: 1,
    # ansonsten: 0, oder wenn zu beiden keine Angabe: NA
    edu_eltern = pmax(meduc, feduc, na.rm = TRUE),
    
    # Nicht-Angaben des Alters als NA kodieren
    alter = ifelse(age > 0, age, NA),
    
    # Umkodieren Herkunftsland Mutter
    # 0: in Deutschland oder früheren deutschen Ostgebieten geboren
    # 1: außerhalb Deutschlands geboren
    mdm01 = case_when(
      mdm01 == 0        ~ 0,
      mdm01 == 996      ~ 0,
      mdm01 > 0         ~ 1,
      mdm01 < 0         ~ NA),
    
    # Umkodieren Herkunftsland Vater
    # 0: in Deutschland oder früheren deutschen Ostgebieten geboren
    # 1: außerhalb Deutschlands geboren
    fdm01 = case_when(
      fdm01 == 0        ~ 0,
      fdm01 == 996      ~ 0,
      fdm01 > 0         ~ 1,
      fdm01 < 0         ~ NA),
    
    # neue Variable mig_eltern
    # 1: Wenn mindestens ein Elternteil im Ausland geboren wurde
    # 0: Wenn beide Elternteile in Deutschland geboren wurden
    mig_eltern = pmax(mdm01, fdm01, na.rm = T),
    
    # Dummy-Variable Leben in Westdeutschland
    # 1: Westdeutschland (alte Bundesländer)
    # 0: Ostdeutschland (neue Bundesländer)
    west_de = ifelse(eastwest == 1, 1, 0),
    
    # gruppierte Einkommensvariable wird verwendet
    # Missings auf NA gesetzt
    einkommen = ifelse(incc > 0, incc, NA)) |> 
  
  # Filterung des Datensatzes für erwerbsfähiges Alter (18-65)
  filter(alter < 66) |> 
  
  # Auswahl der Variablen + Gewichtungsvariable
  select(einkommen, edu_eltern, mig_eltern, alter, west_de, wghtpew)



# Missing Data Analyse ----------------------------------------------------


# Exkulsion der Gewichtungsvariable für Missing Data Analyse
df_analysis = df_edu |> select(-wghtpew)


# Übersicht der NAs (relativ und absolut) pro Variable
freq_nas = miss_var_summary(df_analysis)

# Export in Excel-Datei
freq_nas |> 
  mutate(pct_miss = paste0(round(as.numeric(pct_miss), 2), " %")) |> 
  write_xlsx("Outputs/Übersicht_Missing_Values.xlsx")


# Visuelle Kontrolle der Missing Patterns
gg_miss_upset(df_analysis)


# Littles MCAR-Test
# H0: Daten sind MCAR
# MAR/MNAR liegt im Datensatz vor
mcar_test(df_analysis) |> 
  select(-df) |> 
  write_xlsx("Outputs/MCAR_Test.xlsx")

# Durchführung einer multiplen Imputation ---------------------------------


init <- mice(df_analysis, maxit = 10, print = FALSE)
meth <- init$method

# Festlegen der Imputationsmethoden pro Variable
# einkommen: pmm (Predictive Mean Matching)
# edu_eltern/mig_eltern: logreg (Logistische Regression)
# west_de/alter enthalten keine NA

meth[c("einkommen", "edu_eltern", "mig_eltern")] <- c("pmm", "logreg", "logreg")


# Imputation mit dem 'method'-Argument ausführen
# 15 Imputationen: 15 = aufgerundeter Anteil der maximalen NAs in einer Variable
imp_data <- mice(
  df_analysis,
  m = 15,
  method = meth,
  seed = 42,
  printFlag = FALSE
)



# Konvergenzvisualisierung
# kein Trend sollte beobachtbar sein
plot(imp_data)



# Naiver und adjustierter Treatment-Effekt --------------------------------


# Gepoolte Regression mit allen imputierten Datensätzen
# Spezifikation von HC3-robusten Standardfehlern wegen Heteroskedastizität


# Naiver Schätzer
models_naive <- lapply(1:15, function(i) {
  d_i <- mice::complete(imp_data, action = i)
  d_i$wghtpew <- df_edu$wghtpew        
  lm(einkommen ~ edu_eltern, data = d_i, weights = d_i$wghtpew)
})

# Pooling mit HC3-robusten Standardfehlern
pooled_naive <- pool_parameters(models_naive, vcov = "HC3")
print(pooled_naive)

# Export in Excel-Datei
pooled_naive |> 
  select(-df_error, -Statistic) |> 
  rename(Variable = Parameter,
         "b" = Coefficient) |> 
  mutate(across(2:5, ~ round(as.numeric(.x), 2))) |> 
  mutate(
    `CI_95%` = paste0(
      "[", 
      formatC(CI_low, format = "f", digits = 2, decimal.mark = ","), 
      "; ", 
      formatC(CI_high, format = "f", digits = 2, decimal.mark = ","), 
      "]")) |> 
  select(-CI_low, -CI_high) |> 
  relocate(`CI_95%`, .after = "b") |> 
  write_xlsx("Outputs/Naiver_Schätzer_Treatment.xlsx")



# Adjustierter Schätzer
models_adj <- lapply(1:15, function(i) {
  d_i <- mice::complete(imp_data, action = i)
  d_i$wghtpew <- df_edu$wghtpew
  lm(einkommen ~ edu_eltern + mig_eltern + alter + I(alter^2) + west_de,
     data = d_i, weights = d_i$wghtpew)
})

pooled_adj <- pool_parameters(models_adj, vcov = "HC3")
print(pooled_adj)

# Export in Excel-Datei
pooled_adj |> 
  select(-df_error, -Statistic) |> 
  rename(Variable = Parameter,
         "b" = Coefficient) |> 
  mutate(across(2:5, ~ round(as.numeric(.x), 2))) |> 
  mutate(
    `CI_95%` = paste0(
      "[", 
      formatC(CI_low, format = "f", digits = 2, decimal.mark = ","), 
      "; ", 
      formatC(CI_high, format = "f", digits = 2, decimal.mark = ","), 
      "]")) |> 
  select(-CI_low, -CI_high) |> 
  relocate(`CI_95%`, .after = "b") |> 
  write_xlsx("Outputs/Adjustierter_Schätzer_Treatment.xlsx")



# Testung auf Homoskedastizität -------------------------------------------


# Breusch-Pagan_Test je Imputation: naives Modell
bp_naive       <- lapply(models_naive, bptest)
bp_naive_stat  <- sapply(bp_naive, function(x) x$statistic)
bp_naive_pval  <- sapply(bp_naive, function(x) x$p.value)

# Breusch-Pagan-Test je Imputation: adjustiertes Modell
bp_adj         <- lapply(models_adj, bptest)
bp_adj_stat    <- sapply(bp_adj, function(x) x$statistic)
bp_adj_pval    <- sapply(bp_adj, function(x) x$p.value)


# Median und maximaler p-Wert für Breusch-Pagan-Statistik der 15 imp. Datensätze
# Nullhypothese: Homoskedastizität
# Heteroskedastizität liegt vor --> Spezifikation robuster Standardfehler
bp_gepoolt <- data.frame(
  Modell   = c("Naiv", "Adjustiert"),
  Median_p = c(median(bp_naive_pval), median(bp_adj_pval)),
  Max_p    = c(max(bp_naive_pval), max(bp_adj_pval)))


bp_gepoolt

# Export Excel-Tabelle
bp_gepoolt |> write_xlsx("Outputs/Breusch_Pagan_Tests.xlsx")



# Propensity Score Matching -----------------------------------------------



psm_results <- lapply(1:15, function(i) {
  
  # i-ten imputierten Datensatz laden
  dat <- complete(imp_data, action = i)
  
  # Propensity Score Matching
  # Schätzung der Propensity Scores für Treatment (Bildung der Eltern)
  matchit(
    edu_eltern ~ mig_eltern + alter + I(alter^2) + west_de,
    data = dat,
    method = "nearest",
    distance = "glm",
    ratio = 1,
    s.weights = df_edu$wghtpew
  )
})


# Testung der Balance für alle 15 imputierten Datensätze
# Balance ist i.O., da alle Std. Mean Diff.-Werte betragsmäßig < 0,1 sind

smd_matrix <- map_dfc(1:15, function(i) {
  summary(psm_results[[i]], un = FALSE)$sum.matched |> 
    as.data.frame() %>%
    select(!!paste0("Imp_", i) := `Std. Mean Diff.`)
}) |> 
  rownames_to_column(var = "Variable") |> 
  mutate(
    Mean_SMD = rowMeans(across(starts_with("Imp_"))),
    Abs_Mean_SMD = abs(Mean_SMD),
    Max_Abs_SMD = do.call(pmax, abs(pick(starts_with("Imp_"))))) |> 
  select(Variable, Abs_Mean_SMD, Max_Abs_SMD, everything())

# Export Excel-Tabelle
smd_matrix |> 
  select(1:3) |> 
  mutate(across(where(is.numeric), ~ round(.x, 2))) |> 
  write_xlsx("Outputs/Balance-Check_Matching.xlsx")


# Gematchte Datensätze
matched_data <- lapply(psm_results, function(m) {
    
    match.data(
      m,
      include.s.weights = TRUE
    )
  })
  

# Spezifikation der OLS-Regressionen für gematchte Datensätze
models_psm <- lapply(matched_data, function(dat) {
    
    lm(
      einkommen ~ edu_eltern + mig_eltern + alter + I(alter^2) + west_de,
      data = dat,
      weights = weights
    )
  })
  


# Pooling der Ergebnisse
pooled_psm <- pool_parameters(
    models_psm,
    vcov = "HC3")

# Ausgabe der Koeffizienten
pooled_psm


# Export Excel-Datei
pooled_psm |> 
  remove_rownames() |> 
  select(-c(Statistic, df_error)) |> 
  rename(Variable = Parameter,
         "b" = Coefficient) |> 
  mutate(across(2:5, ~round(.x, 2))) |> 
  mutate(
    `CI_95%` = paste0(
      "[", 
      formatC(CI_low, format = "f", digits = 2, decimal.mark = ","), 
      "; ", 
      formatC(CI_high, format = "f", digits = 2, decimal.mark = ","), 
      "]")) |> 
  select(-CI_low, -CI_high) |> 
  relocate(`CI_95%`, .after = "b") |> 
  write_xlsx("Outputs/Propensity_Score_Matching.xlsx")

