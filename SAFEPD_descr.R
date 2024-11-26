# TODO: I would add some sort of explanation here, so that everyone knows what the function does, ... 

# Code developed by Florian Kuschel, Anna and David Pedrosa

# Version 2.1 # 2024-26-11, # Added the comment to rather use something like the tableone package

#TODO: This one is certainly very meticulous but maybe a bit difficult to digest. Why don't you try the tableone package and stick to the most important categories/values? This should probably be the first result of your dissertation/manuscript. (cf. https://cran.r-project.org/web/packages/tableone/vignettes/introduction.html)

# Soziodemographische Daten
describe(df_safepd$age, IQR = TRUE)
describe(subset(df_safepd$age, df_safepd$gender == 1))
describe(subset(df_safepd$age, df_safepd$gender == 0))

describe(df_safepd$years_since_diagnosis, IQR = TRUE)
describe(subset(df_safepd$years_since_diagnosis, df_safepd$gender == 1))
describe(subset(df_safepd$years_since_diagnosis, df_safepd$gender == 0))

VarDemographic <- c("age", "gender", "nationality", "martial_status", "years_since_diagnosis", "persons_houshold", "school_graduation", "professional_graduation", "employment_status")

Table_SozDem <- CreateCatTable(
  vars = VarDemographic,
  strata = "gender", 
  data = df_safepd,
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
  data = df_safepd,
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
Table_df_safepdQA1 <- CreateCatTable(
  vars = "overall_situation_Group",
  strata = "UPDRS_I_Score", 
  data = df_safepd,
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
describe(df_safepd$UPDRS_I_Score, IQR = TRUE)
describe(subset(df_safepd$UPDRS_I_Score, df_safepd$gender == 1))
describe(subset(df_safepd$UPDRS_I_Score, df_safepd$gender == 0))

describe(df_safepd$UPDRS_II_Score, IQR = TRUE)
describe(subset(df_safepd$UPDRS_II_Score, df_safepd$gender == 1))
describe(subset(df_safepd$UPDRS_II_Score, df_safepd$gender == 0))

VarUPDRS <- c("UPDRS_1_7",	"UPDRS_1_8",	"UPDRS_1_9",	"UPDRS_1_10",	"UPDRS_1_11",	"UPDRS_1_12",	"UPDRS_1_13", "UPDRS_2_1", "UPDRS_2_2",	"UPDRS_2_3",	"UPDRS_2_4",	"UPDRS_2_5",	"UPDRS_2_6",	"UPDRS_2_7",	"UPDRS_2_8",	"UPDRS_2_9",	"UPDRS_2_10",	"UPDRS_2_11",	"UPDRS_2_12",	"UPDRS_2_13")
Table_UPDRS_All <- CreateCatTable(
  vars = VarUPDRS,
  strata = "gender",
  data = df_safepd,
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
  data = df_safepd,
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
  data = df_safepd,
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
describe(df_safepd$FIMA_1_Hausarzt, IQR = TRUE)
describe(df_safepd$FIMA_1_Neurologe, IQR = TRUE)
describe(df_safepd$FIMA_1_Psychiater, IQR = TRUE)
describe(df_safepd$FIMA_1_Internist_FA, IQR = TRUE)
describe(subset(df_safepd$FIMA_1_Gynaekologe, df_safepd$gender == 0), IQR = TRUE)
describe(subset(df_safepd$FIMA_1_Urologe, df_safepd$gender == 1),IQR = TRUE)
describe(df_safepd$FIMA_1_Orthopaede, IQR = TRUE)
describe(df_safepd$FIMA_1_Notfall_KH, IQR = TRUE)
describe(df_safepd$FIMA_1_other, IQR = TRUE)
describe(df_safepd$FIMA_1_Gesamt, IQR = TRUE)

VarArztbesuche <- c("FIMA_1_Hausarzt", "FIMA_1_Neurologe", "FIMA_1_Psychiater", "FIMA_1_Internist_FA", "FIMA_1_Gynaekologe", "FIMA_1_Urologe", "FIMA_1_Orthopaede", "FIMA_1_Notfall_KH", "FIMA_1_other")
Table_FIMA1 <- CreateCatTable( 
  vars = VarArztbesuche,
  strata = "gender",
  data = df_safepd,
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
  data = df_safepd,
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
  data = df_safepd,
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
  data = df_safepd,
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
  data = df_safepd,
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
  data = df_safepd,
  includeNA = TRUE,
  test = TRUE,
  testApprox = chisq.test,
  argsApprox = list(correct = TRUE),
  testExact = fisher.test,
  argsExact = list(workspace = 2 * 10^5),
  smd = FALSE,
  addOverall = TRUE
)


  
