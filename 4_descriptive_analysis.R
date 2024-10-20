# Soziodemographische Daten
describe(SAFEPD$age, IQR = TRUE)
describe(subset(SAFEPD$age, SAFEPD$gender == 1))
describe(subset(SAFEPD$age, SAFEPD$gender == 0))

describe(SAFEPD$years_since_diagnosis, IQR = TRUE)
describe(subset(SAFEPD$years_since_diagnosis, SAFEPD$gender == 1))
describe(subset(SAFEPD$years_since_diagnosis, SAFEPD$gender == 0))
hist_years_since_diagnosis <- hist(SAFEPD$years_since_diagnosis, xlab="Krankheitsdauer", ylab="Häufigkeit", main="Histogramm Krankheitsdauer", col="steelblue", ylim=c(0,80))
text(hist_years_since_diagnosis$mids, hist_years_since_diagnosis$counts, labels = hist_years_since_diagnosis$counts, adj = c(0.5, -0.5))

VarDemographic <- c("age", "gender", "nationality", "martial_status", "years_since_diagnosis", "persons_houshold", "school_graduation", "professional_graduation", "employment_status")

Table_SozDem <- CreateCatTable(
  vars = VarDemographic,
  strata = "gender", 
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

VarDemographic_Group <- c("age", "gender_Group", "nationality_Group", "martial_status_Group", "years_since_diagnosis", "persons_houshold_Group", "school_graduation_Group", "professional_graduation_Group", "employment_status_Group")

Table_SozDem_Group <- CreateCatTable(
  vars = VarDemographic_Group,
  strata = "gender", 
  data = SAFEPD,
  includeNA = TRUE,
  test = TRUE,
  testApprox = chisq.test,
  argsApprox = list(correct = TRUE),
  testExact = fisher.test,
  argsExact = list(workspace = 2 * 10^5),
  smd = FALSE,
  addOverall = TRUE
)

# Sicherheitsfragebogen 
Table_SAFEPDQA1 <- CreateCatTable(
  vars = "overall_situation_Group",
  strata = "gender", 
  data = SAFEPD,
  includeNA = TRUE,
  test = TRUE,
  testApprox = chisq.test,
  argsApprox = list(correct = TRUE),
  testExact = fisher.test,
  argsExact = list(workspace = 2 * 10^5),
  smd = FALSE,
  addOverall = TRUE
)

# UPDRS
describe(SAFEPD$UPDRS_I_Score, IQR = TRUE)
describe(subset(SAFEPD$UPDRS_I_Score, SAFEPD$gender == 1))
describe(subset(SAFEPD$UPDRS_I_Score, SAFEPD$gender == 0))

describe(SAFEPD$UPDRS_II_Score, IQR = TRUE)
describe(subset(SAFEPD$UPDRS_II_Score, SAFEPD$gender == 1))
describe(subset(SAFEPD$UPDRS_II_Score, SAFEPD$gender == 0))

VarUPDRS <- c("UPDRS_1_7",	"UPDRS_1_8",	"UPDRS_1_9",	"UPDRS_1_10",	"UPDRS_1_11",	"UPDRS_1_12",	"UPDRS_1_13", "UPDRS_2_1", "UPDRS_2_2",	"UPDRS_2_3",	"UPDRS_2_4",	"UPDRS_2_5",	"UPDRS_2_6",	"UPDRS_2_7",	"UPDRS_2_8",	"UPDRS_2_9",	"UPDRS_2_10",	"UPDRS_2_11",	"UPDRS_2_12",	"UPDRS_2_13")
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
  test = TRUE,
  testApprox = chisq.test,
  argsApprox = list(correct = TRUE),
  testExact = fisher.test,
  argsExact = list(workspace = 2 * 10^5),
  smd = TRUE,
  addOverall = TRUE
)

Table_UPDRS_II <- CreateCatTable( 
  vars = "UPDRS_II_Score",
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

# FIMA
describe(SAFEPD$FIMA_1_Hausarzt, IQR = TRUE)
describe(SAFEPD$FIMA_1_Neurologe, IQR = TRUE)
describe(SAFEPD$FIMA_1_Psychiater, IQR = TRUE)
describe(SAFEPD$FIMA_1_Internist_FA, IQR = TRUE)
describe(subset(SAFEPD$FIMA_1_Gynaekologe, SAFEPD$gender == 0), IQR = TRUE)
describe(subset(SAFEPD$FIMA_1_Urologe, SAFEPD$gender == 1),IQR = TRUE)
describe(SAFEPD$FIMA_1_Orthopaede, IQR = TRUE)
describe(SAFEPD$FIMA_1_Notfall_KH, IQR = TRUE)
describe(SAFEPD$FIMA_1_other, IQR = TRUE)
describe(SAFEPD$FIMA_1_Gesamt, IQR = TRUE)

VarArztbesuche <- c("FIMA_1_Hausarzt", "FIMA_1_Neurologe", "FIMA_1_Psychiater", "FIMA_1_Internist_FA", "FIMA_1_Gynaekologe", "FIMA_1_Urologe", "FIMA_1_Orthopaede", "FIMA_1_Notfall_KH", "FIMA_1_other")
Table_FIMA1 <- CreateCatTable( 
  vars = VarArztbesuche,
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

VarTherapie <- c("FIMA_2_Krankengymnastik", "FIMA_2_Ergotherapie", "FIMA_2_Sprachtherapie", "FIMA_2_Heilpraktiker", "FIMA_2_Osteopath", "FIMA_2_Chiropraktiker", "FIMA_2_Psychotherapeut")
Table_FIMA2 <- CreateCatTable( 
  vars = VarTherapie,
  strata = "gender",
  data = SAFEPD,
  includeNA = TRUE,
  test = TRUE,
  testApprox = chisq.test,
  argsApprox = list(correct = TRUE),
  testExact = fisher.test,
  argsExact = list(workspace = 2 * 10^5),
  smd = FALSE,
  addOverall = TRUE
) 

VarPflege <- c("FIMA_3", "FIMA_4", "FIMA_5", "FIMA_6", "FIMA_7", "FIMA_8", "FIMA_8_Group")
Table_FIMA3 <- CreateCatTable(
  vars = VarPflege,
  strata = "gender",
  data = SAFEPD,
  includeNA = TRUE,
  test = TRUE, 
  testApprox = chisq.test,
  argsApprox = list(correct = TRUE),
  testExact = fisher.test,
  argsExact = list(workspace = 2 * 10^5),
  smd = FALSE,
  addOverall = TRUE
)
  
VarKlinik <- c("FIMA_9_Group", "FIMA_10_Group", "FIMA_11_Group", "FIMA_12_Group")
Table_FIMA4 <- CreateCatTable(
  vars = VarKlinik,
  strata = "gender",
  data = SAFEPD,
  includeNA = TRUE,
  test = TRUE, 
  testApprox = chisq.test,
  argsApprox = list(correct = TRUE),
  testExact = fisher.test,
  argsExact = list(workspace = 2 * 10^5),
  smd = FALSE,
  addOverall = TRUE
)
  
VarWohnung <- c("FIMA_15", "FIMA_14_Group")
Table_FIMA5 <- CreateCatTable(
  vars = VarWohnung,
  strata = "gender", 
  data = SAFEPD,
  includeNA = TRUE,
  test = TRUE,
  testApprox = chisq.test,
  argsApprox = list(correct = TRUE),
  testExact = fisher.test,
  argsExact = list(workspace = 2 * 10^5),
  smd = FALSE,
  addOverall = TRUE
)
  
Table_FIMA6 <- CreateCatTable(
  vars = "FIMA_16_Group",
  strata = "gender", 
  data = SAFEPD,
  includeNA = TRUE,
  test = TRUE,
  testApprox = chisq.test,
  argsApprox = list(correct = TRUE),
  testExact = fisher.test,
  argsExact = list(workspace = 2 * 10^5),
  smd = FALSE,
  addOverall = TRUE
)
