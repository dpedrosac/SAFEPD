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
VarFIMA <- 
  
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
  addOverall = TRUE,
) ##Fehler unused argument (alist())

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
  addOverall = TRUE,
) ## Fehler unused argument (alist())

## Test auf Normalverteilung 
shapiro_tests <- lapply(VarNVTest, function(variable) {
  shapiro_test_result <- shapiro.test(SAFEPD[[variable]])
  return(data.frame(Variable = variable, p_value = shapiro_test_result$p.value, 
                    is_normal = shapiro_test_result$p.value > 0.05))
})
shapiro_results_table <- do.call(rbind, shapiro_tests)

qqnorm(SAFEPD$age)
qqline(SAFEPD$age)

## SAFEPDQA umkodieren / dichotomisieren, Test
SAFEPD$lack_of_information_dich <- cut(SAFEPD$lack_of_information, breaks=c(3, Inf), right = TRUE, labels=c("x", "y"))
SAFEPD$uncertain_future_dich <- cut(SAFEPD$uncertain_future, breaks=c(3, Inf), right = TRUE, labels=c("x", "y"))
SAFEPD$chaging_symptom_severity_dich <- cut(SAFEPD$chaging_symptom_severity, breaks=c(3, Inf), right = TRUE, labels=c("x", "y"))
SAFEPD$gait_insecurity_fall_dich <- cut(SAFEPD$gait_insecurity_fall, breaks=c(3, Inf), right = TRUE, labels=c("x", "y"))
SAFEPD$pain_dich <- cut(SAFEPD$pain, breaks=c(3, Inf), right = TRUE, labels=c("x", "y"))
SAFEPD$gastrointestinal_symptoms_dich <- cut(SAFEPD$gastrointestinal_symptoms, breaks=c(3, Inf), right = TRUE, labels=c("x", "y"))
SAFEPD$urinary_symptoms_dich <- cut(SAFEPD$urinary_symptoms, breaks=c(3, Inf), right = TRUE, labels=c("x", "y"))
SAFEPD$mental_abilities_dich <- cut(SAFEPD$mental_abilities, breaks=c(3, Inf), right = TRUE, labels=c("x", "y"))
SAFEPD$mental_symptoms_dich <- cut(SAFEPD$mental_symptoms, breaks=c(3, Inf), right = TRUE, labels=c("x", "y"))
SAFEPD$other_disease_dich <- cut(SAFEPD$other_disease, breaks=c(3, Inf), right = TRUE, labels=c("x", "y"))
SAFEPD$nursing_care_dich <- cut(SAFEPD$nursing_care, breaks=c(3, Inf), right = TRUE, labels=c("x", "y"))
SAFEPD$side_effects_complications_dich <- cut(SAFEPD$side_effects_complications, breaks=c(3, Inf), right = TRUE, labels=c("x", "y"))
SAFEPD$access_healthcare_dich <- cut(SAFEPD$access_healthcare, breaks=c(3, Inf), right = TRUE, labels=c("x", "y"))
SAFEPD$communication_with_me_dich <- cut(SAFEPD$communication_with_me, breaks=c(3, Inf), right = TRUE, labels=c("x", "y"))
SAFEPD$communication_between_professionals_dich <- cut(SAFEPD$communication_between_professionals, breaks=c(3, Inf), right = TRUE, labels=c("x", "y"))
SAFEPD$loneliness_dich <- cut(SAFEPD$loneliness, breaks=c(3, Inf), right = TRUE, labels=c("x", "y"))
SAFEPD$everyday_problems_dich <- cut(SAFEPD$everyday_problems, breaks=c(3, Inf), right = TRUE, labels=c("x", "y"))
SAFEPD$daily_routine_dich <- cut(SAFEPD$daily_routine, breaks=c(3, Inf), right = TRUE, labels=c("x", "y"))
SAFEPD$overload_among_people_dich <- cut(SAFEPD$overload_among_people, breaks=c(3, Inf), right = TRUE, labels=c("x", "y"))
SAFEPD$pejorativ_looks_comments_dich <- cut(SAFEPD$pejorativ_looks_comments, breaks=c(3, Inf), right = TRUE, labels=c("x", "y"))
SAFEPD$family_role_dich <- cut(SAFEPD$family_role, breaks=c(3, Inf), right = TRUE, labels=c("x", "y"))
SAFEPD$conflicts_with_relatives_dich <- cut(SAFEPD$conflicts_with_relatives, breaks=c(3, Inf), right = TRUE, labels=c("x", "y"))
SAFEPD$victim_to_crime_dich <- cut(SAFEPD$victim_to_crime, breaks=c(3, Inf), right = TRUE, labels=c("x", "y"))
SAFEPD$financial_worries_dich <- cut(SAFEPD$financial_worries, breaks=c(3, Inf), right = TRUE, labels=c("x", "y"))
SAFEPD$not_at_peace_with_myself_dich <- cut(SAFEPD$not_at_peace_with_myself, breaks=c(3, Inf), right = TRUE, labels=c("x", "y"))
SAFEPD$participation_in_road_traffic_dich <- cut(SAFEPD$participation_in_road_traffic, breaks=c(3, Inf), right = TRUE, labels=c("x", "y"))
SAFEPD$overall_situation_dich <- cut(SAFEPD$overall_situation, breaks=c(3, Inf), right = TRUE, labels=c("x", "y"))



