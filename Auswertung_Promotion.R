# Run analyses for a corss-sectional study on perceived safety of people with Parkinson's Disease (PwPD): 
# Code developed by Florian Kuschel and David Pedrosa

# Version 2.1 # 2024-14-11, # Smaller adjustments to make the code readble for different collaborators

## Load required packages from external script
source("load_packages.R")

## Load data according to the username
username <- Sys.getenv("USERNAME")

if (username == "dpedrosac") {
  wdir <- "/media/storage/SAFEPD/"
  datadir <- file.path(wdir, "data")
} else {
  wdir <- "~/SAFEPD/"
  datadir <- file.path(wdir, "data") # you need to move the file to this folder. I have added folder to .gitignore
}

# Load the SAFEPD dataset with specific range and column types
df_safepd <- read_excel(
  file.path(datadir, "SAFEPD_imputeddata.xlsx"),
  range = "A1:DF209",
  col_types = rep("numeric", 110),  # Set 110 columns as numeric
  n_max = 210                       # Load only the first 210 rows
)

## Dichotomisierung / Imputation
source("SAFEPD_dich.R")
# write.xlsx(SAFEPD, "SAFEPD_imputeddata.xlsx", rowNames = FALSE)

## Normalvertilung
source("SAFEPD_dist.R")

## deskriptive Analyse
source("SAFEPD_descr.R")
source("SAFEPD_descr_diag.R") 

## Korrelationsanalyse
# source("SAFEPD_corr.R")

## Regressionsanalyse
source("SAFEPD_reg.R")

## Hypothesen Untersuchung
# source("SAFEPD_hypothesis.R")

## Datenabgleich / Diskussion
# source("SAFEPD_disc.R")
