## Datei laden
library(readxl)
SAFEPD <- read_excel("~/Library/CloudStorage/OneDrive-Persönlich/Documents/Promotion/SAFEPD_aktuell.xlsx", 
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
                                   "numeric", "numeric", "numeric", "numeric", 
                                   "skip"), n_max = 210)
View(SAFEPD)

## Pakete laden
library(psych)
library(tableone)
library(compareGroups)
library(lsr)
library(descr)
library(ordinal)
library(MASS)
library(pROC)
library(lmtest)

## Variablen einteilen
VarDemographic <- c("gender", "nationality", "martial_status", "years_since_diagnosis", "persons_houshold", "school_graduation", "professional_graduation", "employment_status")
VarSAFEPDQA <- c("lack_of_information", "uncertain_future",	"chaging_symptom_severity",	"gait_insecurity_fall",	"pain",	"gastrointestinal_symptoms",	"urinary_symptoms",	"mental_abilities",	"mental_symptoms",	"other_disease",	"nursing_care",	"side_effects_complications",	"access_healthcare",	"communication_with_me",	"communication_between_professionals",	"loneliness",	"everyday_problems",	"daily_routine",	"overload_among_people",	"pejorativ_looks_comments",	"family_role",	"conflicts_with_relatives",	"victim_to_crime",	"financial_worries",	"not_at_peace_with_myself",	"participation_in_road_traffic",	"overall_situation")
VarUPDRS <- c("UPDRS_1_7",	"UPDRS_1_8",	"UPDRS_1_9",	"UPDRS_1_10",	"UPDRS_1_11",	"UPDRS_1_12",	"UPDRS_1_13", "UPDRS_2_1", "UPDRS_2_2",	"UPDRS_2_3",	"UPDRS_2_4",	"UPDRS_2_5",	"UPDRS_2_6",	"UPDRS_2_7",	"UPDRS_2_8",	"UPDRS_2_9",	"UPDRS_2_10",	"UPDRS_2_11",	"UPDRS_2_12",	"UPDRS_2_13")
VarUPDRS_Score <- c("UPDRS_I_Score", "UPDRS_II_Score")
VarArztbesuche <- c("FIMA_1_Hausarzt", "FIMA_1_Neurologe", "FIMA_1_Psychiater", "FIMA_1_Internist_FA", "FIMA_1_Gynaekologe", "FIMA_1_Urologe", "FIMA_1_Orthopaede", "FIMA_1_Notfall_KH", "FIMA_1_other")
VarTherapie <- c("FIMA_2_Krankengymnastik", "FIMA_2_Ergotherapie", "FIMA_2_Sprachtherapie", "FIMA_2_Heilpraktiker", "FIMA_2_Osteopath", "FIMA_2_Chiropraktiker", "FIMA_2_Psychotherapeut")
VarNVTest <- c("age","gender", "nationality", "martial_status", "years_since_diagnosis", "persons_houshold", "school_graduation", "professional_graduation", "employment_status", "lack_of_information", "uncertain_future",	"chaging_symptom_severity",	"gait_insecurity_fall",	"pain",	"gastrointestinal_symptoms",	"urinary_symptoms",	"mental_abilities",	"mental_symptoms",	"other_disease",	"nursing_care",	"side_effects_complications",	"access_healthcare",	"communication_with_me",	"communication_between_professionals",	"loneliness",	"everyday_problems",	"daily_routine",	"overload_among_people",	"pejorativ_looks_comments",	"family_role",	"conflicts_with_relatives",	"victim_to_crime",	"financial_worries",	"not_at_peace_with_myself",	"participation_in_road_traffic",	"overall_situation")

## SAFEPDQA dichotomisieren
SAFEPD[paste0(VarSAFEPDQA, "_Group")] <- lapply(SAFEPD[, VarSAFEPDQA], function(x) as.numeric(ifelse(x %in% 1:3, "1", "0"))) ## 1=eingeschränkt, 0=uneingeschränkt

## Soziodemographische Daten dichotomisieren
SAFEPD$gender_Group <- as.numeric(ifelse(SAFEPD$gender == 0, "0", "1")) ## 0 = weiblich
SAFEPD$nationality_Group <- as.numeric(ifelse(SAFEPD$nationality == 0, "0", "1")) ## 0 = deutsch
SAFEPD$martial_status_Group <- as.numeric(ifelse(SAFEPD$martial_status == 1, "1", "0")) ## 0 = nicht verheiratet
SAFEPD$persons_houshold_Group <- as.numeric(ifelse(SAFEPD$persons_houshold == 1, "0", "1")) ## == 0 = alleinlebend
SAFEPD$school_graduation_Group <- as.numeric(ifelse(SAFEPD$school_graduation == 3, "1", "0")) ## 0 = kein Abitur
SAFEPD$professional_graduation_Group <- as.numeric(ifelse(SAFEPD$professional_graduation == 4, "1", "0")) ## 0 = kein Hochschulabschluss
SAFEPD$employment_status_Group <- as.numeric(ifelse(SAFEPD$employment_status == 4 , "1", "0")) ## 0 = nicht berentet
SAFEPD$employment_status_Group2 <- as.numeric(ifelse(SAFEPD$employment_status %in% c(0,1) , "1", "0")) ## 0 = nicht arbeitend

## FIMA dichotomisieren / neue Variablen
SAFEPD$FIMA_16_Group <- as.numeric(ifelse(SAFEPD$FIMA_16 == 1 , "0", "1")) ## 0 = gesetzlich versichert
SAFEPD$FIMA_13_Mobilitaet <- ifelse(rowSums(SAFEPD[, c("FIMA_13_Gehstock", "FIMA_13_Anti_Freezing_Stock", "FIMA_13_Kamptokormie_Rucksack", "FIMA_13_Rollator", "FIMA_13_Rollstuhl", "FIMA_13_Treppenlift")]) > 0, 1, 0)
SAFEPD$FIMA_13_Hygiene <- ifelse(rowSums(SAFEPD[, c("FIMA_13_Badewannenlift", "FIMA_13_Badewanneneinstieghilfe", "FIMA_13_Badewannensitz", "FIMA_13_Toilettensitz", "FIMA_13_Toilettenstuhl", "FIMA_13_Urinflasche", "FIMA_13_Inkontinenzeinlagen")]) > 0, 1, 0)
SAFEPD$FIMA_13_Atmung <- ifelse(rowSums(SAFEPD[, c("FIMA_13_Sauerstoffgeraet", "FIMA_13_Schlafapnoe")]) > 0, 1, 0)
SAFEPD$FIMA_13_Tremor <- ifelse(rowSums(SAFEPD[, c("FIMA_13_Tremorbesteck", "FIMA_13_Greifzange")]) > 0, 1, 0)
SAFEPD$FIMA_13_Anzahl <- rowSums(SAFEPD[, c("FIMA_13_Gehstock", "FIMA_13_Anti_Freezing_Stock", "FIMA_13_Kamptokormie_Rucksack", "FIMA_13_Rollator", "FIMA_13_Rollstuhl", "FIMA_13_Treppenlift", "FIMA_13_Badewannenlift", "FIMA_13_Brille", "FIMA_13_Hoergeraet", "FIMA_13_Tremorbesteck", "FIMA_13_Greifzange", "FIMA_13_Sauerstoffgeraet", "FIMA_13_Schlafapnoe", "FIMA_13_Badewanneneinstieghilfe", "FIMA_13_Badewannensitz", "FIMA_13_Toilettensitz", "FIMA_13_Toilettenstuhl", "FIMA_13_Urinflasche", "FIMA_13_Inkontinenzeinlagen", "FIMA_13_Hausnotruf")])
SAFEPD$FIMA_1_Gesamt <- rowSums(SAFEPD[, c("FIMA_1_Hausarzt", "FIMA_1_Neurologe", "FIMA_1_Psychiater", "FIMA_1_Internist_FA", "FIMA_1_Gynaekologe", "FIMA_1_Urologe", "FIMA_1_Orthopaede", "FIMA_1_Notfall_KH", "FIMA_1_other")])

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

Table_SozDem <- CreateCatTable( ##kann man das irgendwie in einem ansehlichen Format ausgeben?
  vars = VarDemographic,
  strata = "gender", ##keine Ahnung wie ich das nach irgendwas sinnvollem / oder gar nichts aufteile, auch für die übrigen Tabellen unten
  data = SAFEPD,
  includeNA = TRUE,
  test = TRUE,
  testApprox = chisq.test,
  argsApprox = list(correct = TRUE),
  testExact = fisher.test,
  argsExact = list(workspace = 2 * 10^5),
  smd = TRUE,
  addOverall = TRUE
)

## deskriptive Statistik SAFEPDQA
Table_SAFEPDQA1 <- CreateCatTable(
  vars = "overall_situation_Group",
  strata = "FIMA_13_Mobilitaet", 
  data = SAFEPD,
  includeNA = FALSE,
  test = TRUE,
  testApprox = chisq.test,
  argsApprox = list(correct = TRUE),
  testExact = fisher.test,
  argsExact = list(workspace = 2 * 10^5),
  smd = FALSE,
  addOverall = TRUE
)

Table_SAFEPDQA2 <- CreateCatTable(
  vars = "overall_situation_Group",
  strata = "FIMA_13_Hygiene", 
  data = SAFEPD,
  includeNA = FALSE,
  test = TRUE,
  testApprox = chisq.test,
  argsApprox = list(correct = TRUE),
  testExact = fisher.test,
  argsExact = list(workspace = 2 * 10^5),
  smd = FALSE,
  addOverall = TRUE
)

Table_SAFEPDQA3 <- CreateCatTable(
  vars = "overall_situation_Group",
  strata = "FIMA_13_Tremor", 
  data = SAFEPD,
  includeNA = FALSE,
  test = TRUE,
  testApprox = chisq.test,
  argsApprox = list(correct = TRUE),
  testExact = fisher.test,
  argsExact = list(workspace = 2 * 10^5),
  smd = FALSE,
  addOverall = TRUE
)

Table_SAFEPDQA4 <- CreateCatTable(
  vars = "overall_situation_Group",
  strata = "FIMA_13_Atmung", 
  data = SAFEPD,
  includeNA = FALSE,
  test = TRUE,
  testApprox = chisq.test,
  argsApprox = list(correct = TRUE),
  testExact = fisher.test,
  argsExact = list(workspace = 2 * 10^5),
  smd = FALSE,
  addOverall = TRUE
)

## deskriptive Statistik UPDRS
describe(SAFEPD$UPDRS_I_Score, IQR = TRUE)
hist_UPDRS_I <- hist(SAFEPD$UPDRS_I_Score, xlab="UPDRS Score", ylab="Häufigkeit", main="Histogramm UPDRS I", col="steelblue", ylim=c(0,80), breaks=4)
text(hist_UPDRS_I$mids, hist_UPDRS_I$counts, labels = hist_UPDRS_I$counts, adj=c(0.5, -0.5))

describe(SAFEPD$UPDRS_II_Score, IQR = TRUE)
hist_UPDRS_II <- hist(SAFEPD$UPDRS_II_Score, xlab="UPDRS Score", ylab="Häufigkeit", main="Histogramm UPDRS II", col="steelblue", ylim=c(0,100), xlim=c(0,50), breaks=4)
text(hist_UPDRS_II$mids, hist_UPDRS_II$counts, labels = hist_UPDRS_II$counts, adj=c(0.5, -0.5))

Table_UPDRS_All <- CreateCatTable(
  vars = VarUPDRS,
  strata = "gender",
  data = SAFEPD,
  includeNA = TRUE,
  test = TRUE,
  testApprox = chisq.test,
  argsApprox = list(correct = TRUE),
  testExact = fisher.test,
  argsExact = list(workspace = 2 * 10^5),
  smd = TRUE,
  addOverall = TRUE
) 

Table_UPDRS_I <- CreateCatTable( 
  vars = "UPDRS_I_Score",
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

Table_UPDRS_II <- CreateCatTable( 
  vars = "UPDRS_II_Score",
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

## deskriptivce Statistik FIMA
describe(SAFEPD$FIMA_1_Hausarzt, IQR = TRUE)
hist_Hausarzt <- hist(SAFEPD$FIMA_1_Hausarzt, xlab="Hausarztbesuche", ylab="Häufigkeit", main="Histogramm Hausarztbesuche", col="steelblue", ylim=c(0,140))
text(hist_Hausarzt$mids, hist_Hausarzt$counts, labels = hist_Hausarzt$counts, adj = c(0.5, -0.5))
Hausarztbesuche <- table(SAFEPD$FIMA_1_Hausarzt)

describe(SAFEPD$FIMA_1_Neurologe, IQR = TRUE)
hist_Neurologe <- hist(SAFEPD$FIMA_1_Neurologe, xlab="Neurologenbesuche", ylab="Häufigkeit", main="Histogramm Neurologenbesuche", col="steelblue", ylim=c(0,160))
text(hist_Neurologe$mids, hist_Neurologe$counts, labels = hist_Neurologe$counts, adj = c(0.5, -0.5))
Hausarztbesuche <- table(SAFEPD$FIMA_1_Neurologe)

describe(SAFEPD$FIMA_1_Psychiater, IQR = TRUE)
hist_Psychiater <- hist(SAFEPD$FIMA_1_Psychiater, xlab="Psychiaterbesuche", ylab="Häufigkeit", main="Histogramm Psychiaterbesuche", col="steelblue", ylim=c(0,160))
text(hist_Psychiater$mids, hist_Psychiater$counts, labels = hist_Psychiater$counts, adj = c(0.5, -0.5))
Psychiaterbesuche <- table(SAFEPD$FIMA_1_Psychiater)

describe(SAFEPD$FIMA_1_Internist_FA, IQR = TRUE)
hist_Internist_FA <- hist(SAFEPD$FIMA_1_Internist_FA, xlab="Internist_FA-Besuche", ylab="Häufigkeit", main="Histogramm Internist_FA-Besuche", col="steelblue", ylim=c(0,170))
text(hist_Internist_FA$mids, hist_Internist_FA$counts, labels = hist_Internist_FA$counts, adj = c(0.5, -0.5))
Internist_FA_Besuche <- table(SAFEPD$FIMA_1_Internist_FA)

describe(SAFEPD$FIMA_1_Gynaekologe, IQR = TRUE)
hist_Gynaekologe <- hist(SAFEPD$FIMA_1_Gynaekologe, xlab="Gynaekologe-Besuche", ylab="Häufigkeit", main="Histogramm Gynaekologe-Besuche", col="steelblue", ylim=c(0,200))
text(hist_Gynaekologe$mids, hist_Gynaekologe$counts, labels = hist_Gynaekologe$counts, adj = c(0.5, -0.5))
Gynaekologe_Besuche <- table(SAFEPD$FIMA_1_Gynaekologe)

describe(SAFEPD$FIMA_1_Urologe, IQR = TRUE)
hist_Urologe <- hist(SAFEPD$FIMA_1_Urologe, xlab="Urologe-Besuche", ylab="Häufigkeit", main="Histogramm Urologe-Besuche", col="steelblue", ylim=c(0,170))
text(hist_Urologe$mids, hist_Urologe$counts, labels = hist_Urologe$counts, adj = c(0.5, -0.5))
Urologe_Besuche <- table(SAFEPD$FIMA_1_Urologe)

describe(SAFEPD$FIMA_1_Orthopaede, IQR = TRUE)
hist_Orthopaede <- hist(SAFEPD$FIMA_1_Orthopaede, xlab="Orthopaede-Besuche", ylab="Häufigkeit", main="Histogramm Orthopaede-Besuche", col="steelblue", ylim=c(0,200))
text(hist_Orthopaede$mids, hist_Orthopaede$counts, labels = hist_Orthopaede$counts, adj = c(0.5, -0.5))
Orthopaede_Besuche <- table(SAFEPD$FIMA_1_Orthopaede)

describe(SAFEPD$FIMA_1_Notfall_KH, IQR = TRUE)
hist_Notfall_KH <- hist(SAFEPD$FIMA_1_Notfall_KH, xlab="Notfall_KH-Besuche", ylab="Häufigkeit", main="Histogramm Notfall_KH-Besuche", col="steelblue", ylim=c(0,200))
text(hist_Notfall_KH$mids, hist_Notfall_KH$counts, labels = hist_Notfall_KH$counts, adj = c(0.5, -0.5))
Notfall_KH_Besuche <- table(SAFEPD$FIMA_1_Notfall_KH)

describe(SAFEPD$FIMA_1_other, IQR = TRUE)
hist_other <- hist(SAFEPD$FIMA_1_other, xlab="other-Besuche", ylab="Häufigkeit", main="Histogramm other-Besuche", col="steelblue", ylim=c(0,200))
text(hist_other$mids, hist_other$counts, labels = hist_other$counts, adj = c(0.5, -0.5))
other_Besuche <- table(SAFEPD$FIMA_1_other)

describe(SAFEPD$FIMA_1_Gesamt, IQR = TRUE)
hist_Gesamt <- hist(SAFEPD$FIMA_1_Gesamt, xlab="Gesamtesuche", ylab="Häufigkeit", main="Histogramm Gesamtesuche", col="steelblue", ylim=c(0,200))
text(hist_Gesamt$mids, hist_Gesamt$counts, labels = hist_Gesamt$counts, adj = c(0.5, -0.5))
Gesamtbesuche <- table(SAFEPD$FIMA_1_Gesamt)

Table_FIMA1 <- CreateCatTable( 
  vars = VarArztbesuche,
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

Table_FIMA2 <- CreateCatTable( 
  vars = VarTherapie,
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

## Test auf Normalverteilung 
shapiro_tests <- lapply(VarNVTest, function(variable) {
  shapiro_test_result <- shapiro.test(SAFEPD[[variable]])
  return(data.frame(Variable = variable, p_value = shapiro_test_result$p.value, 
                    is_normal = shapiro_test_result$p.value > 0.05))
})
shapiro_results_table <- do.call(rbind, shapiro_tests)

qqnorm(SAFEPD$age)
qqline(SAFEPD$age)

# Streudiagramm erstellen
plot(SAFEPD$age, SAFEPD$overall_situation_Group)

## Regressionsanalyse zu SAFEPD_overall_situation 
## (Haushalt dichotom, berentet)
modell1 <- glm(overall_situation_Group ~ age + years_since_diagnosis + gender_Group + nationality_Group + martial_status_Group + persons_houshold_Group + school_graduation_Group + professional_graduation_Group + employment_status_Group + UPDRS_I_Score + UPDRS_II_Score, data = SAFEPD, family = binomial)
summary(modell1)
anova(modell1, test = "Chisq")
print(modell1)
p_values1 <- summary(modell1)$coefficients[, "Pr(>|z|)"]
exp_coef1 <- exp(coef(modell1))
conf_int1 <- exp(confint(modell1))
results1 <- data.frame(OR = exp_coef1, ci.lb = conf_int1[, 1], ci.ub = conf_int1[, 2], pval = p_values1)
results1sig <- subset(results1, pval < 0.05)

observed_indices <- which(!is.na(SAFEPD$overall_situation_Group))
observed_outcome <- SAFEPD$overall_situation_Group[observed_indices]
predicted_probabilities1 <- predict(modell1, type = "response")[observed_indices]
roc_curve1 <- roc(response = observed_outcome, predictor = predicted_probabilities1)
auc1 <- auc(roc_curve1)
plot(roc_curve1, main = "ROC Curve", col = "blue", lwd = 2)
legend("bottomright", legend = paste("AUC =", round(auc(roc_curve1), 3)), col = "blue", lty = 1, cex = 0.8)


## Regressionsanalyse zu SAFEPD_overall_situation 
## (Haushalt dichotom, arbeitend)
modell2 <- glm(overall_situation_Group ~ age + years_since_diagnosis + gender_Group + nationality_Group + martial_status_Group + persons_houshold_Group + school_graduation_Group + professional_graduation_Group + employment_status_Group2 + UPDRS_I_Score + UPDRS_II_Score, data = SAFEPD, family = binomial)
summary(modell2)
anova(modell2, test ="Chisq")
print(modell2)
p_values2 <- summary(modell2)$coefficients[, "Pr(>|z|)"]
exp_coef2 <- exp(coef(modell2))
conf_int2 <- exp(confint(modell2))
results2 <- data.frame(OR = exp_coef2, ci.lb = conf_int2[, 1], ci.ub = conf_int2[, 2], pval = p_values2)
results2sig <- subset(results2, pval < 0.05)

predicted_probabilities2 <- predict(modell2, type = "response")[observed_indices]
roc_curve2 <- roc(response = observed_outcome, predictor = predicted_probabilities2)
auc2 <- auc(roc_curve2)
plot(roc_curve2, main = "ROC Curve", col = "blue", lwd = 2)
legend("bottomright", legend = paste("AUC =", round(auc(roc_curve2), 3)), col = "blue", lty = 1, cex = 0.8)


## Regressionsanalyse zu SAFEPD_overall_situation 
## (Haushalt kontinuierlich, berentet)
modell3 <- glm(overall_situation_Group ~ age + years_since_diagnosis + gender_Group + nationality_Group + martial_status_Group + persons_houshold + school_graduation_Group + professional_graduation_Group + employment_status_Group + UPDRS_I_Score + UPDRS_II_Score, data = SAFEPD, family = binomial)
summary(modell3)
anova(modell3, test = "Chisq")
print(modell3)
p_values3 <- summary(modell3)$coefficients[, "Pr(>|z|)"]
exp_coef3 <- exp(coef(modell3))
conf_int3 <- exp(confint(modell3))
results3 <- data.frame(OR = exp_coef3, ci.lb = conf_int3[, 1], ci.ub = conf_int3[, 2], pval = p_values3)
results3sig <- subset(results3, pval < 0.05)

predicted_probabilities3 <- predict(modell3, type = "response")[observed_indices]
roc_curve3 <- roc(response = observed_outcome, predictor = predicted_probabilities3)
auc3 <- auc(roc_curve3)
plot(roc_curve3, main = "ROC Curve", col = "blue", lwd = 2)
legend("bottomright", legend = paste("AUC =", round(auc(roc_curve3), 3)), col = "blue", lty = 1, cex = 0.8)


## Regressionsanalyse zu SAFEPD_overall_situation 
## (Haushalt kontinuierlich, arbeitend)
modell4 <- glm(overall_situation_Group ~ age + years_since_diagnosis + gender_Group + nationality_Group + martial_status_Group + persons_houshold + school_graduation_Group + professional_graduation_Group + employment_status_Group2 + UPDRS_I_Score + UPDRS_II_Score, data = SAFEPD, family = binomial)
summary(modell4)
anova(modell4, test = "Chisq")
print(modell4)
p_values4 <- summary(modell4)$coefficients[, "Pr(>|z|)"]
exp_coef4 <- exp(coef(modell4))
conf_int4 <- exp(confint(modell4))
results4 <- data.frame(OR = exp_coef4, ci.lb = conf_int4[, 1], ci.ub = conf_int4[, 2], pval = p_values4)
results4sig <- subset(results4, pval < 0.05)

predicted_probabilities4 <- predict(modell4, type = "response")[observed_indices]
roc_curve4 <- roc(response = observed_outcome, predictor = predicted_probabilities4)
auc4 <- auc(roc_curve4)
plot(roc_curve4, main = "ROC Curve", col = "blue", lwd = 2)
legend("bottomright", legend = paste("AUC =", round(auc(roc_curve4), 3)), col = "blue", lty = 1, cex = 0.8)


## Gruppenvergleich zu SAFEPD_overall_situatuion mit FIMA-Daten
modellKG <- glm(overall_situation_Group ~ FIMA_2_Krankengymnastik, data = SAFEPD, family = binomial)
summary(modellKG)
OR_KG <- exp(coef(summary(modellKG))[, "Estimate"])
CI_KG <- exp(confint(modellKG))
CI.lb_KG <- CI_KG[2, 1]
CI.ub_KG <- CI_KG[2, 2]
pval_KG <- chisq.test(SAFEPD$overall_situation_Group, SAFEPD$FIMA_2_Krankengymnastik)$p.value

modellERGO <- glm(overall_situation_Group ~ FIMA_2_Ergotherapie, data = SAFEPD, family = binomial)
summary(modellERGO)
OR_ERGO <- exp(coef(summary(modellERGO))[, "Estimate"])
CI_ERGO <- exp(confint(modellERGO))
CI.lb_ERGO <- CI_ERGO[2, 1]
CI.ub_ERGO <- CI_ERGO[2, 2]
pval_ERGO <- chisq.test(SAFEPD$overall_situation_Group, SAFEPD$FIMA_2_Ergotherapie)$p.value

modellLOGO <- glm(overall_situation_Group ~ FIMA_2_Sprachtherapie, data = SAFEPD, family = binomial)
summary(modellLOGO)
OR_LOGO <- exp(coef(summary(modellLOGO))[, "Estimate"])
CI_LOGO <- exp(confint(modellLOGO))
CI.lb_LOGO <- CI_LOGO[2, 1]
CI.ub_LOGO <- CI_LOGO[2, 2]
pval_LOGO <- chisq.test(SAFEPD$overall_situation_Group, SAFEPD$FIMA_2_Sprachtherapie)$p.value

modellHP <- glm(overall_situation_Group ~ FIMA_2_Heilpraktiker, data = SAFEPD, family = binomial)
summary(modellHP)
OR_HP <- exp(coef(summary(modellHP))[, "Estimate"])
CI_HP <- exp(confint(modellHP))
CI.lb_HP <- CI_HP[2, 1]
CI.ub_HP <- CI_HP[2, 2]
pval_HP <- chisq.test(SAFEPD$overall_situation_Group, SAFEPD$FIMA_2_Heilpraktiker)$p.value

modellOST <- glm(overall_situation_Group ~ FIMA_2_Osteopath, data = SAFEPD, family = binomial)
summary(modellOST)
OR_OST <- exp(coef(summary(modellOST))[, "Estimate"])
CI_OST <- exp(confint(modellOST))
CI.lb_OST <- CI_OST[2, 1]
CI.ub_OST <- CI_OST[2, 2]
pval_OST <- chisq.test(SAFEPD$overall_situation_Group, SAFEPD$FIMA_2_Osteopath)$p.value

modellCP <- glm(overall_situation_Group ~ FIMA_2_Chiropraktiker, data = SAFEPD, family = binomial)
summary(modellCP)
OR_CP <- exp(coef(summary(modellCP))[, "Estimate"])
CI_CP <- exp(confint(modellCP))
CI.lb_CP <- CI_CP[2, 1]
CI.ub_CP <- CI_CP[2, 2]
pval_CP <- chisq.test(SAFEPD$overall_situation_Group, SAFEPD$FIMA_2_Chiropraktiker)$p.value

modellPT <- glm(overall_situation_Group ~ FIMA_2_Psychotherapeut, data = SAFEPD, family = binomial)
summary(modellPT)
OR_PT <- exp(coef(summary(modellPT))[, "Estimate"])
CI_PT <- exp(confint(modellPT))
CI.lb_PT <- CI_PT[2, 1]
CI.ub_PT <- CI_PT[2, 2]
pval_PT <- chisq.test(SAFEPD$overall_situation_Group, SAFEPD$FIMA_2_Psychotherapeut)$p.value

modellKV <- glm(overall_situation_Group ~ FIMA_16_Group, data = SAFEPD, family = binomial)
summary(modellKV)
OR_KV <- exp(coef(summary(modellKV))[, "Estimate"])
CI_KV <- exp(confint(modellKV))
CI.lb_KV <- CI_KV[2, 1]
CI.ub_KV <- CI_KV[2, 2]
pval_KV <- chisq.test(SAFEPD$overall_situation_Group, SAFEPD$FIMA_16_Group)$p.value

modellF13Mob <- glm(overall_situation_Group ~ FIMA_13_Mobilitaet, data = SAFEPD, family = binomial)
summary(modellF13Mob)
OR_F13Mob <- exp(coef(summary(modellF13Mob))[, "Estimate"])
CI_F13Mob <- exp(confint(modellF13Mob))
CI.lb_F13Mob <- CI_F13Mob[2, 1]
CI.ub_F13Mob <- CI_F13Mob[2, 2]
pval_F13Mob <- chisq.test(SAFEPD$overall_situation_Group, SAFEPD$FIMA_13_Mobilitaet)$p.value

modellF13Hyg <- glm(overall_situation_Group ~ FIMA_13_Hygiene, data = SAFEPD, family = binomial)
summary(modellF13Hyg)
OR_F13Hyg <- exp(coef(summary(modellF13Hyg))[, "Estimate"])
CI_F13Hyg <- exp(confint(modellF13Hyg))
CI.lb_F13Hyg <- CI_F13Hyg[2, 1]
CI.ub_F13Hyg <- CI_F13Hyg[2, 2]
pval_F13Hyg<- chisq.test(SAFEPD$overall_situation_Group, SAFEPD$FIMA_13_Hygiene)$p.value

modellF13Tre <- glm(overall_situation_Group ~ FIMA_13_Tremor, data = SAFEPD, family = binomial)
summary(modellF13Tre)
OR_F13Tre <- exp(coef(summary(modellF13Tre))[, "Estimate"])
CI_F13Tre <- exp(confint(modellF13Tre))
CI.lb_F13Tre <- CI_F13Tre[2, 1]
CI.ub_F13Tre <- CI_F13Tre[2, 2]
pval_F13Tre <- chisq.test(SAFEPD$overall_situation_Group, SAFEPD$FIMA_13_Tremor)$p.value

modellF13At <- glm(overall_situation_Group ~ FIMA_13_Atmung, data = SAFEPD, family = binomial)
summary(modellF13At)
OR_F13At <- exp(coef(summary(modellF13At))[, "Estimate"])
CI_F13At <- exp(confint(modellF13At))
CI.lb_F13At <- CI_F13At[2, 1]
CI.ub_F13At <- CI_F13At[2, 2]
pval_F13At <- chisq.test(SAFEPD$overall_situation_Group, SAFEPD$FIMA_13_Atmung)$p.value

modellF13Not <- glm(overall_situation_Group ~ FIMA_13_Hausnotruf, data = SAFEPD, family = binomial)
summary(modellF13Not)
OR_F13Not <- exp(coef(summary(modellF13Not))[, "Estimate"])
CI_F13Not <- exp(confint(modellF13Not))
CI.lb_F13Not <- CI_F13Not[2, 1]
CI.ub_F13Not <- CI_F13Not[2, 2]
pval_F13Not <- chisq.test(SAFEPD$overall_situation_Group, SAFEPD$FIMA_13_Hausnotruf)$p.value

modellF13Sum <- glm(overall_situation_Group ~ FIMA_13_Anzahl, data = SAFEPD, family = binomial)
summary(modellF13Sum)
OR_F13Sum <- exp(coef(summary(modellF13Sum))[, "Estimate"])
CI_F13Sum <- exp(confint(modellF13Sum))
CI.lb_F13Sum <- CI_F13Sum[2, 1]
CI.ub_F13Sum <- CI_F13Sum[2, 2]
pval_F13Sum <- chisq.test(SAFEPD$overall_situation_Group, SAFEPD$FIMA_13_Anzahl)$p.value

## Zusammenführung des Gruppenvergleichs zu FIMA-Daten
Variablen_FIMA <- c("Mobilitätshilfsmittel", "Hygienehilftmittel", "Tremorhilfsmittel", "Atemhilfmittel", "Hausnotruf", "Hilfsmittelanzahl", "Krankengymnastik", "Ergotherapie", "Logopaedie", "Heilpraktiker", "Chiropraktiker", "Osteopath", "Psychtherapeut", "Versicherungsstatus")
OR_FIMA <- c(OR_F13Mob[2], OR_F13Hyg[2], OR_F13Tre[2], OR_F13At[2], OR_F13Not[2], OR_F13Sum[2], OR_KG[2], OR_ERGO[2], OR_LOGO[2], OR_HP[2], OR_CP[2], OR_OST[2], OR_PT[2], OR_KV[2])
CI.lb_FIMA <- c(CI.lb_F13Mob, CI.lb_F13Hyg, CI.lb_F13Tre, CI.lb_F13At, CI.lb_F13Not, CI.lb_F13Sum, CI.lb_KG, CI.lb_ERGO, CI.lb_LOGO, CI.lb_HP, CI.lb_CP, CI.lb_OST, CI.lb_PT, CI.lb_KV)
CI.ub_FIMA <- c(CI.ub_F13Mob, CI.ub_F13Hyg, CI.ub_F13Tre, CI.ub_F13At, CI.ub_F13Not, CI.ub_F13Sum, CI.ub_KG, CI.ub_ERGO, CI.ub_LOGO, CI.ub_HP, CI.ub_CP, CI.ub_OST, CI.ub_PT, CI.ub_KV)
pval_FIMA <- c(pval_F13Mob, pval_F13Hyg, pval_F13Tre, pval_F13At, pval_F13Not, pval_F13Sum, pval_KG, pval_ERGO, pval_LOGO, pval_HP, pval_CP, pval_OST, pval_PT, pval_KV)
data_FIMA <- data.frame(Variablen_FIMA, OR_FIMA, CI.lb_FIMA, CI.ub_FIMA, pval_FIMA)


