## Pakete laden
source("loadpackages.R")

## Datei laden
SAFEPD <- read_excel("~/SAFEPD/SAFEPD_imputeddata.xlsx", 
                     range = "A1:DF209",
                     col_types = c(rep("numeric", 110)), n_max = 210)

## Dichotomisierung / Imputation
source("SAFEPD_dich.R")
# write.xlsx(SAFEPD, "SAFEPD_imputeddata.xlsx", rowNames = FALSE)

## Normalvertilung
source("SAFEPD_dist.R")

## deskriptive Analyse
source("SAFEPD_descr.R")
source("SAFEPD_descr_diag.R")

## Korrelationsanalyse
source("SAFEPD_corr.R")

## Regressionsanalyse
source("SAFEPD_reg.R")

## Datenabgleich / Diskussion
source("SAFEPD_disc.R")
