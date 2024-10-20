# multiple Imputation für NA einmal durchgeführt
# missing_data <- SAFEPD[rowSums(is.na(SAFEPD)) > 0, , drop = FALSE]
# imputed_missing_data <- mice(missing_data)
# completed_missing_data <- complete(imputed_missing_data)
# missing_rows <- which(rowSums(is.na(SAFEPD)) > 0)
# SAFEPD[missing_rows, ] <- completed_missing_data

# Soziodemographische Daten
SAFEPD$gender_Group <- as.numeric(ifelse(SAFEPD$gender == 1, "1", "0")) # 0 = weiblich, 1 = männlich
SAFEPD$nationality_Group <- as.numeric(ifelse(SAFEPD$nationality == 0, "1", "0")) # 0 = andere, 1 = deutsch
SAFEPD$martial_status_Group <- as.numeric(ifelse(SAFEPD$martial_status == 1, "1", "0")) # 0 = nicht verheiratet, 1 = verheiratet
SAFEPD$persons_houshold[SAFEPD$persons_houshold == 0] <- 1
SAFEPD$persons_houshold_Group <- as.numeric(ifelse(SAFEPD$persons_houshold == 1, "1", "0")) # 0 = nicht alleinlebend, 1 =  alleinlebend
SAFEPD$school_graduation_Group <- as.numeric(ifelse(SAFEPD$school_graduation == 3, "1", "0")) # 0 = kein Abitur, 1 = Abitur
SAFEPD$professional_graduation_Group <- as.numeric(ifelse(SAFEPD$professional_graduation == 4, "1", "0")) # 0 = kein Hochschulabschluss, 1 = Hochschulabschluss
SAFEPD$employment_status_Group <- as.numeric(ifelse(SAFEPD$employment_status == 4, "1", "0")) # 0 = nicht berentet, 1 = berentet

# SAFEPDQA 
VarSAFEPDQA <- c("lack_of_information", "uncertain_future",	"chaging_symptom_severity",	"gait_insecurity_fall",	"pain",	"gastrointestinal_symptoms",	"urinary_symptoms",	"mental_abilities",	"mental_symptoms",	"other_disease",	"nursing_care",	"side_effects_complications",	"access_healthcare",	"communication_with_me",	"communication_between_professionals",	"loneliness",	"everyday_problems",	"daily_routine",	"overload_among_people",	"pejorativ_looks_comments",	"family_role",	"conflicts_with_relatives",	"victim_to_crime",	"financial_worries",	"not_at_peace_with_myself",	"participation_in_road_traffic",	"overall_situation")
SAFEPD[paste0(VarSAFEPDQA, "_Group")] <- lapply(SAFEPD[, VarSAFEPDQA], function(x) as.numeric(ifelse(x %in% 1:3, "1", "0"))) ## 1=eingeschränkt, 0=uneingeschränkt

# UPDRS
SAFEPD$UPDRS_Sum <- SAFEPD$UPDRS_I_Score + SAFEPD$UPDRS_II_Score

# FIMA
SAFEPD$FIMA_1_Gesamt <- rowSums(SAFEPD[, c("FIMA_1_Hausarzt", "FIMA_1_Neurologe", "FIMA_1_Psychiater", "FIMA_1_Internist_FA", "FIMA_1_Gynaekologe", "FIMA_1_Urologe", "FIMA_1_Orthopaede", "FIMA_1_Notfall_KH", "FIMA_1_other")])
SAFEPD$FIMA_13_Mobilitaet <- ifelse(rowSums(SAFEPD[, c("FIMA_13_Gehstock", "FIMA_13_Anti_Freezing_Stock", "FIMA_13_Kamptokormie_Rucksack", "FIMA_13_Rollator", "FIMA_13_Rollstuhl", "FIMA_13_Treppenlift")]) > 0, 1, 0)
SAFEPD$FIMA_13_Hygiene <- ifelse(rowSums(SAFEPD[, c("FIMA_13_Badewannenlift", "FIMA_13_Badewanneneinstieghilfe", "FIMA_13_Badewannensitz", "FIMA_13_Toilettensitz", "FIMA_13_Toilettenstuhl", "FIMA_13_Urinflasche", "FIMA_13_Inkontinenzeinlagen")]) > 0, 1, 0)
SAFEPD$FIMA_13_Atmung <- ifelse(rowSums(SAFEPD[, c("FIMA_13_Sauerstoffgeraet", "FIMA_13_Schlafapnoe")]) > 0, 1, 0)
SAFEPD$FIMA_13_Tremor <- ifelse(rowSums(SAFEPD[, c("FIMA_13_Tremorbesteck", "FIMA_13_Greifzange")]) > 0, 1, 0)
SAFEPD$FIMA_13_Anzahl <- rowSums(SAFEPD[, c("FIMA_13_Gehstock", "FIMA_13_Anti_Freezing_Stock", "FIMA_13_Kamptokormie_Rucksack", "FIMA_13_Rollator", "FIMA_13_Rollstuhl", "FIMA_13_Treppenlift", "FIMA_13_Badewannenlift", "FIMA_13_Brille", "FIMA_13_Hoergeraet", "FIMA_13_Tremorbesteck", "FIMA_13_Greifzange", "FIMA_13_Sauerstoffgeraet", "FIMA_13_Schlafapnoe", "FIMA_13_Badewanneneinstieghilfe", "FIMA_13_Badewannensitz", "FIMA_13_Toilettensitz", "FIMA_13_Toilettenstuhl", "FIMA_13_Urinflasche", "FIMA_13_Inkontinenzeinlagen", "FIMA_13_Hausnotruf")])
SAFEPD$FIMA_8_Group <- as.numeric(ifelse(SAFEPD$FIMA_8 == 0, "0", "1")) ## 0 = kein Pflegegrad
SAFEPD$FIMA_9_Group <- as.numeric(ifelse(SAFEPD$FIMA_9 == 0, "0", "1")) ## 0 = keine Reha
SAFEPD$FIMA_10_Group <- as.numeric(ifelse(SAFEPD$FIMA_10 == 0, "0", "1")) ## 0 = keine Tagesklinik
SAFEPD$FIMA_11_Group <- as.numeric(ifelse(SAFEPD$FIMA_11 == 0, "0", "1")) ## 0 = kein Krankenhaus
SAFEPD$FIMA_12_Group <- as.numeric(ifelse(SAFEPD$FIMA_12 == 0, "0", "1")) ## 0 = keine Psychiatrie
SAFEPD$FIMA_14_Group <- as.numeric(ifelse(SAFEPD$FIMA_14 == 1, "0", "1")) ## 0 = zuhause wohnhaft 
SAFEPD$FIMA_16_Group <- as.numeric(ifelse(SAFEPD$FIMA_16 == 1 , "0", "1")) ## 0 = gesetzlich versichert

