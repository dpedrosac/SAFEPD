## Datei laden
library(readxl)
SAFEPD <- read_excel("Library/CloudStorage/OneDrive-Persönlich/Documents/Promotion/SAFEPD.xlsx", 
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
  
describe(SAFEPD$age, IQR = TRUE)
hist_alter <- hist(SAFEPD$age, xlab="Alter", ylab="Häufigkeit", main="Histogramm Alter", col="steelblue", ylim=c(0,50), xlim=c(30,90)) ##x-Grenzen?
text(hist_alter$mids, hist_alter$counts, labels = hist_alter$counts, adj=c(0.5, -0.5))

ah_gender <- table(SAFEPD$gender)
rh_gender <- round(prop.table(table(SAFEPD$gender))*100,2) ##NA einfügen?
table_gender <- cbind(ah_gender, rh_gender)

ah_nationality <- table(SAFEPD$nationality)
rh_nationality <- round(prop.table(table(SAFEPD$nationality))*100,2)
table_nationality <- cbind(ah_nationality, rh_nationality)

ah_martial_status <- table(SAFEPD$martial_status)
rh_martial_status <- round(prop.table(table(SAFEPD$martial_status))*100,2)
table_martial_status <- cbind(ah_martial_status, rh_martial_status)

describe(SAFEPD$years_since_diagnosis, IQR = TRUE)
hist_years_since_diagnosis <- hist(SAFEPD$years_since_diagnosis, xlab="Krankheitsdauer", ylab="Häufigkeit", main="Histogramm Krankheitsdauer", col="steelblue", ylim=c(0,80))
text(hist_years_since_diagnosis$mids, hist_years_since_diagnosis$counts, labels = hist_years_since_diagnosis$counts, adj = c(0.5, -0.5))

ah_persons_houshold <- table(SAFEPD$persons_houshold)
rh_persons_houshold <- round(prop.table(table(SAFEPD$persons_houshold))*100,2)
table_persons_houshold <- cbind(ah_persons_houshold, rh_persons_houshold)

ah_school_graduation <- table(SAFEPD$school_graduation)
rh_school_graduation <- round(prop.table(table(SAFEPD$school_graduation))*100,2)
table_school_graduation <- cbind(ah_school_graduation, rh_school_graduation
                                 )
ah_professional_graduation <- table(SAFEPD$professional_graduation)
rh_professional_graduation <- round(prop.table(table(SAFEPD$professional_graduation))*100,2)
table_professional_graduation <- cbind(ah_professional_graduation, rh_professional_graduation
                                       )
ah_employment_status <- table(SAFEPD$employment_status)
rh_employment_status <- round(prop.table(table(SAFEPD$employment_status))*100,2)
table_employment_status <- cbind(ah_employment_status, rh_employment_status)


Table_SozDem <- CreateCatTable( ##kann man das irgendwie in einem ansehlichen Format ausgeben?
  vars = VarDemographic,
  strata = "gender", ##keine Ahnung wie ich das nach irgendwas sinnvollem / oder gar nichts aufteile, auch für die übrigen Tabellen unten
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

shapiro.test(SAFEPD$age)
qqnorm(SAFEPD$age)
qqline(SAFEPD$age)
shapiro.test(SAFEPD$years_since_diagnosis)
shapiro.test(SAFEPD$lack_of_information)
shapiro.test(SAFEPD$uncertain_future)
shapiro.test(SAFEPD$chaging_symptom_severity)
shapiro.test(SAFEPD$gait_insecurity_fall)
shapiro.test(SAFEPD$pain)
shapiro.test(SAFEPD$gastrointestinal_symptoms)
shapiro.test(SAFEPD$urinary_symptoms)
shapiro.test(SAFEPD$mental_abilities)
shapiro.test(SAFEPD$mental_symptoms)
shapiro.test(SAFEPD$other_disease)
shapiro.test(SAFEPD$nursing_care)
shapiro.test(SAFEPD$side_effects_complications)
shapiro.test(SAFEPD$access_healthcare)
shapiro.test(SAFEPD$communication_with_me)
shapiro.test(SAFEPD$communication_between_professionals)
shapiro.test(SAFEPD$loneliness)
shapiro.test(SAFEPD$everyday_problems)
shapiro.test(SAFEPD$daily_routine)
shapiro.test(SAFEPD$overload_among_people)
shapiro.test(SAFEPD$pejorativ_looks_comments)
shapiro.test(SAFEPD$family_role)
shapiro.test(SAFEPD$conflicts_with_relatives)
shapiro.test(SAFEPD$victim_to_crime)
shapiro.test(SAFEPD$financial_worries)
shapiro.test(SAFEPD$not_at_peace_with_myself)
shapiro.test(SAFEPD$participation_in_road_traffic)
shapiro.test(SAFEPD$overall_situation)

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


## UPDRS_Score umkodieren, Test
## Cut-Off Werte vorhanden? -> Mit Anna besprechen, UPDRS I nicht alle Fragen abgefragt
SAFEPD$UPDRS_I_Score_dich <- cut(SAFEPD$UPDRS_I_Score, breaks=c(0, 1, 2, Inf), right=TRUE, labels=c("mild", "moderat", "schwer"))

SAFEPD$UPDRS_II_Score_dich <- cut(SAFEPD$UPDRS_II_Score, breaks=c(0, 1, 2, Inf), right = TRUE, labels = c("mild", "moderat", "schwer"))


