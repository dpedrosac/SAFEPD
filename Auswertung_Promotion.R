## Datei laden
library(readxl)
SAFEPD <- read_excel("~/Library/CloudStorage/OneDrive-Persönlich/Documents/Promotion/SAFEPD.xlsx", 
                     col_types = c("numeric", "numeric", "numeric", 
                                   "numeric", "numeric", "numeric", "numeric", "numeric", 
                                   "numeric", "numeric", "numeric", "numeric", "numeric", 
                                   "numeric", "numeric", "numeric", "numeric", "numeric", 
                                   "numeric", "numeric", "numeric", "numeric", "numeric", 
                                   "numeric", "numeric", "numeric", "numeric", "numeric", 
                                   "numeric", "numeric", "numeric", "numeric", "numeric", 
                                   "numeric", "numeric", "numeric", "numeric", "numeric", 
                                   "numeric", "numeric", "numeric", "numeric", "numeric", 
                                   "numeric", "numeric", "numeric", "numeric", "numeric", 
                                   "numeric", "numeric", "numeric", "numeric", "numeric", 
                                   "numeric", "numeric", "numeric", "numeric", "numeric", 
                                   "numeric", "numeric", "numeric", "numeric", 
                                   "numeric", "numeric", "numeric", "numeric", "numeric", 
                                   "numeric", "numeric", "numeric", "numeric", "numeric", 
                                   "numeric", "numeric", "numeric", "numeric", "numeric", 
                                   "numeric", "numeric", "numeric", "numeric", "numeric", 
                                   "numeric", "numeric", "numeric", "numeric", "numeric", 
                                   "numeric", "numeric", "numeric", "numeric", "numeric", 
                                   "numeric", "numeric", "numeric", "numeric", "numeric", 
                                   "numeric", "numeric", "numeric", "numeric", "numeric", 
                                   "numeric", "numeric", "numeric", "numeric", "numeric", 
                                   "numeric", "numeric", "numeric", "skip", 
                                   "skip"), n_max = 200)
View(SAFEPD)

## Pakete laden
library(psych)
library(tableone)
library(compareGroups)
library(lsr)
library(descr)



## Variablen einteilen
VarDemographic <- c("gender", "nationality", "martial_status", "years_since_diagnosis", "persons_houshold", "school_graduation", "professional_graduation", "employment_status")
VarSAFEPDQA <- c("lack_of_information", "uncertain_future",	"chaging_symptom_severity",	"gait_insecurity_fall",	"pain",	"gastrointestinal_symptoms",	"urinary_symptoms",	"mental_abilities",	"mental_symptoms",	"other_disease",	"nursing_care",	"side_effects_complications",	"access_healthcare",	"communication_with_me",	"communication_between_professionals",	"loneliness",	"everyday_problems",	"daily_routine",	"overload_among_people",	"pejorativ_looks_comments",	"family_role",	"conflicts_with_relatives",	"victim_to_crime",	"financial_worries",	"not_at_peace_with_myself",	"participation_in_road_traffic",	"overall_situation")
VarUPDRS <- c("UPDRS_1_7",	"UPDRS_1_8",	"UPDRS_1_9",	"UPDRS_1_10",	"UPDRS_1_11",	"UPDRS_1_12",	"UPDRS_1_13", "UPDRS_2_1", "UPDRS_2_2",	"UPDRS_2_3",	"UPDRS_2_4",	"UPDRS_2_5",	"UPDRS_2_6",	"UPDRS_2_7",	"UPDRS_2_8",	"UPDRS_2_9",	"UPDRS_2_10",	"UPDRS_2_11",	"UPDRS_2_12",	"UPDRS_2_13")
VarUPDRS_Score <- c("UPDRS_I_Score", "UPDRS_II_Score")
VarFIMA 
VarNVTest <- c("age","gender", "nationality", "martial_status", "years_since_diagnosis", "persons_houshold", "school_graduation", "professional_graduation", "employment_status", "lack_of_information", "uncertain_future",	"chaging_symptom_severity",	"gait_insecurity_fall",	"pain",	"gastrointestinal_symptoms",	"urinary_symptoms",	"mental_abilities",	"mental_symptoms",	"other_disease",	"nursing_care",	"side_effects_complications",	"access_healthcare",	"communication_with_me",	"communication_between_professionals",	"loneliness",	"everyday_problems",	"daily_routine",	"overload_among_people",	"pejorativ_looks_comments",	"family_role",	"conflicts_with_relatives",	"victim_to_crime",	"financial_worries",	"not_at_peace_with_myself",	"participation_in_road_traffic",	"overall_situation")
  
## deskriptive Statistik demographische Variablen
table_gender <- cbind(Absolut = table(factor(SAFEPD$gender, levels = c(0, 1), labels = c("weiblich", "männlich"))), 
                        Relativ = round(prop.table(table(factor(SAFEPD$gender, levels = c(0, 1), labels = c("weiblich", "männlich")))) * 100, 2))

table_nationality <- cbind(Absolut = table(factor(SAFEPD$nationality, levels = c(0, 1, 2), labels = c("deutsch", "andere", "dual"))), 
                           Relativ = round(prop.table(table(factor(SAFEPD$nationality, levels = c(0, 1, 2), labels = c("deutsch", "andere", "dual")))) * 100, 2))

table_martial_status <- cbind(Absolut = table(factor(SAFEPD$martial_status, levels = c(0, 1, 2, 3), labels = c("ledig", "verheiratet", "geschieden", "verwitwet"))), 
                              Relativ = round(prop.table(table(factor(SAFEPD$martial_status, levels = c(0, 1, 2, 3), labels = c("ledig", "verheiratet", "geschieden", "verwitwet")))) * 100, 2))

table_persons_houshold <- cbind(Absolut = table(SAFEPD$persons_houshold), Relativ = round(prop.table(table(SAFEPD$persons_houshold)) * 100, 2))

table_school_graduation <- cbind(Absolut = table(factor(SAFEPD$school_graduation, levels = c(0, 1, 2, 3, 4), labels = c("kein Abschluss", "Hauptschule", "Realschule", "Abitur", "sonstiger Abschluss"))), 
                                 Relativ = round(prop.table(table(factor(SAFEPD$school_graduation, levels = c(0, 1, 2, 3, 4), labels = c("kein Abschluss", "Hauptschule", "Realschule", "Abitur", "sonstiger Abschluss")))) * 100, 2))

table_professional_graduation <- cbind(Absolut = table(factor(SAFEPD$professional_graduation, levels = c(0, 1, 2, 3, 4, 5), labels = c("kein Abschluss", "Berufsschule", "Fachschule", "Ingenieurschule", "Hochschule", "sonstiger Abschluss"))), 
                                       Relativ = round(prop.table(table(factor(SAFEPD$professional_graduation, levels = c(0, 1, 2, 3, 4, 5), labels = c("kein Abschluss", "Berufsschule", "Fachschule", "Ingenieurschule", "Hochschule", "sonstiger Abschluss")))) * 100, 2))

table_employment_status <- cbind(Absolut = table(factor(SAFEPD$employment_status, levels = c(0, 1, 2, 3, 4, 5, 6), labels = c("Vollzeit", "Teilzeit", "Hausmann/Hausfrau", "arbeitslos", "berentet", "arbeitsunfähig", "sonstiges"))), 
                                 Relativ = round(prop.table(table(factor(SAFEPD$employment_status, levels = c(0, 1, 2, 3, 4, 5, 6), labels = c("Vollzeit", "Teilzeit", "Hausmann/Hausfrau", "arbeitslos", "berentet", "arbeitsunfähig", "sonstiges")))) * 100, 2))

describe(SAFEPD$age, IQR = TRUE)
hist_alter <- hist(SAFEPD$age, xlab="Alter", ylab="Häufigkeit", main="Histogramm Alter", col="steelblue", ylim=c(0,70), xlim=c(30,90), breaks=5) ##x-Grenzen?
text(hist_alter$mids, hist_alter$counts, labels = hist_alter$counts, adj=c(0.5, -0.5))

describe(SAFEPD$years_since_diagnosis, IQR = TRUE)
hist_years_since_diagnosis <- hist(SAFEPD$years_since_diagnosis, xlab="Krankheitsdauer", ylab="Häufigkeit", main="Histogramm Krankheitsdauer", col="steelblue", ylim=c(0,80))
text(hist_years_since_diagnosis$mids, hist_years_since_diagnosis$counts, labels = hist_years_since_diagnosis$counts, adj = c(0.5, -0.5))

Table_SozDem <- CreateCatTable( 
  vars = VarDemographic,
  strata = "gender", 
  data = SAFEPD,
  includeNA = TRUE,
  test = FALSE,
  testApprox = chisq.test,
  argsApprox = list(correct = TRUE),
  testExact = fisher.test,
  argsExact = list(workspace = 2 * 10^5),
  smd = FALSE,
  addOverall = TRUE
)

## deskriptive Statistik SAFEPDQA
Table_SAFEPDQA <- CreateCatTable(
  vars = VarSAFEPDQA,
  strata = "gender", 
  data = SAFEPD,
  includeNA = TRUE,
  test = FALSE,
  testApprox = chisq.test,
  argsApprox = list(correct = FALSE),
  testExact = fisher.test,
  argsExact = list(workspace = 2 * 10^5),
  smd = FALSE,
  addOverall = TRUE
)

## deskriptive Statistik UPDRS
CreateCatTable(
  vars = VarUPDRS,
  strata = "gender",
  data = SAFEPD,
  includeNA = TRUE,
  test = FALSE,
  testApprox = chisq.test,
  argsApprox = list(correct = FALSE),
  testExact = fisher.test,
  argsExact = list(workspace = 2 * 10^5),
  smd = FALSE,
  addOverall = TRUE
)

CreateCatTable( 
  vars = VarUPDRS_Score,
  strata = "overall",
  data = SAFEPD,
  includeNA = TRUE,
  test = FALSE,
  testApprox = chisq.test,
  argsApprox = list(correct = TRUE),
  testExact = fisher.test,
  argsExact = list(workspace = 2 * 10^5),
  smd = FALSE,
  addOverall = TRUE
) 

## Test auf Normalverteilung 
shapiro_tests <- lapply(VarNVTest, function(variable) {
  shapiro_test_result <- shapiro.test(SAFEPD[[variable]])
  return(data.frame(Variable = variable, p_value = shapiro_test_result$p.value, 
                    is_normal = shapiro_test_result$p.value > 0.05))
})
shapiro_results_table <- do.call(rbind, shapiro_tests)

qqnorm(SAFEPD$age)
qqline(SAFEPD$age)

## SAFEPDQA dichotomisieren
SAFEPD[paste0(VarSAFEPDQA, "_Group")] <- lapply(SAFEPD[, VarSAFEPDQA], function(x) ifelse(x %in% 1:3, "eher zutreffend", "eher nicht zutreffend"))

## Soziodemographische Daten dichotomisieren
SAFEPD$martial_status_Group <- ifelse(SAFEPD$martial_status == 1, "verheiratet", "nicht verheiratet")
SAFEPD$school_graduation_Group <- ifelse(SAFEPD$school_graduation == 3, "Abitur", "kein Abitur")
SAFEPD$professional_graduation_Group <- ifelse(SAFEPD$professional_graduation == 4, "Hochschulabschlss", "kein Hochschulabschluss")
SAFEPD$employment_status_Group <- ifelse(SAFEPD$employment_status %in% c(0, 1), "arbeitend", "nicht arbeitend")



