# TODO: I would add some sort of explanation here, so that everyone knows what the function does, e.g.,
# Imputation of data and recoding the variables in order tp prepare for subsequent analyses in the SAFEPD study 
# Code developed by Florian Kuschel, Anna and David Pedrosa

# Version 2.1 # 2024-26-11, # Added some additional options for enhancing readability in the code.


# TODO: Is this part actually done before, as the data is called data_imputed?! If so, I would not omit it
# but let it run here, as well.

# multiple Imputation für NA einmal durchgeführt
# missing_data <- df_safepd[rowSums(is.na(df_safepd)) > 0, , drop = FALSE]
# imputed_missing_data <- mice(missing_data)
# completed_missing_data <- complete(imputed_missing_data)
# missing_rows <- which(rowSums(is.na(df_safepd)) > 0)
# df_safepd[missing_rows, ] <- completed_missing_data


# ==== Dichotomisation 
# Define the categories as factors for later

# TODO: the next few lines do what you were doing but it is a bit tidier and it includes the attributed as metadata
# so that you can later always assess them when you need them. 

df_safepd <- df_safepd %>%
  mutate(
    gender_Group = factor(gender, 
                          levels = c(0, 1), 
                          labels = c("female", "male")),
    nationality_Group = factor(nationality, 
                               levels = c(0, 1), 
                               labels = c("other", "german")),
    martial_status_Group = factor(martial_status, 
                                  levels = c(0, 1), 
                                  labels = c("unmarried", "married")),
    persons_houshold = if_else(persons_houshold == 0, 1, persons_houshold),
    persons_houshold_Group = factor(persons_houshold, 
                                    levels = c(0, 1), 
                                    labels = c("not living alone", "living alone")),
    school_graduation_Group = factor(school_graduation, 
                                     levels = c(0, 1), 
                                     labels = c("other", "Abitur")),
    professional_graduation_Group = factor(professional_graduation, 
                                           levels = c(0, 1), 
                                           labels = c("without graduation", "graduation")),
    employment_status_Group = factor(employment_status, 
                                     levels = c(0, 1), 
                                     labels = c("not retired", "retired"))
  )

# TODO: You are aware that this "grouping" introduces "NA" in your dataset, when the values are not present, right?

# Your previous version
# Soziodemographische Daten
#df_safepd$gender_Group <- as.numeric(ifelse(df_safepd$gender == 1, "1", "0")) # 0 = weiblich, 1 = männlich
#df_safepd$nationality_Group <- as.numeric(ifelse(df_safepd$nationality == 0, "1", "0")) # 0 = andere, 1 = deutsch
#df_safepd$martial_status_Group <- as.numeric(ifelse(df_safepd$martial_status == 1, "1", "0")) # 0 = nicht verheiratet, 1 = verheiratet
#df_safepd$persons_houshold[df_safepd$persons_houshold == 0] <- 1
#df_safepd$persons_houshold_Group <- as.numeric(ifelse(df_safepd$persons_houshold == 1, "1", "0")) # 0 = nicht alleinlebend, 1 =  alleinlebend
#df_safepd$school_graduation_Group <- as.numeric(ifelse(df_safepd$school_graduation == 3, "1", "0")) # 0 = kein Abitur, 1 = Abitur
#df_safepd$professional_graduation_Group <- as.numeric(ifelse(df_safepd$professional_graduation == 4, "1", "0")) # 0 = kein Hochschulabschluss, 1 = Hochschulabschluss
#df_safepd$employment_status_Group <- as.numeric(ifelse(df_safepd$employment_status == 4, "1", "0")) # 0 = nicht berentet, 1 = berentet

# ==== Transform factors 
# Define variables for transformation
VarQA <- c(
  "lack_of_information", "uncertain_future", "chaging_symptom_severity", 
  "gait_insecurity_fall", "pain", "gastrointestinal_symptoms", 
  "urinary_symptoms", "mental_abilities", "mental_symptoms", 
  "other_disease", "nursing_care", "side_effects_complications", 
  "access_healthcare", "communication_with_me", 
  "communication_between_professionals", "loneliness", "everyday_problems", 
  "daily_routine", "overload_among_people", "pejorativ_looks_comments", 
  "family_role", "conflicts_with_relatives", "victim_to_crime", 
  "financial_worries", "not_at_peace_with_myself", 
  "participation_in_road_traffic", "overall_situation"
)

# Transform and create new grouped variables
df_safepd <- df_safepd %>%
  mutate(across(
    all_of(VarQA),
    ~ as.numeric(if_else(. %in% 1:3, 1, 0)), # 1 = eingeschränkt, 0 = uneingeschränkt
    .names = "{.col}_Group"                # Append "_Group" to each variable name
  ))

# Your previous version
# df_safepdQA 
#Vardf_safepdQA <- c("lack_of_information", "uncertain_future",	"chaging_symptom_severity",	"gait_insecurity_fall",	"pain",	"gastrointestinal_symptoms",	"urinary_symptoms",	"mental_abilities",	"mental_symptoms",	"other_disease",	"nursing_care",	"side_effects_complications",	"access_healthcare",	"communication_with_me",	"communication_between_professionals",	"loneliness",	"everyday_problems",	"daily_routine",	"overload_among_people",	"pejorativ_looks_comments",	"family_role",	"conflicts_with_relatives",	"victim_to_crime",	"financial_worries",	"not_at_peace_with_myself",	"participation_in_road_traffic",	"overall_situation")
#df_safepd[paste0(Vardf_safepdQA, "_Group")] <- lapply(df_safepd[, Vardf_safepdQA], function(x) as.numeric(ifelse(x %in% 1:3, "1", "0"))) ## 1=eingeschränkt, 0=uneingeschränkt

# ==== Create new sums 
# Add UPDRS_Sum with NA handling
df_safepd <- df_safepd %>%
  mutate(
    UPDRS_Sum = rowSums(across(c(UPDRS_I_Score, UPDRS_II_Score)), na.rm = TRUE) # Handles missing values
  )

# Previous version of the UPDRS I + II score
# df_safepd$UPDRS_Sum <- df_safepd$UPDRS_I_Score + df_safepd$UPDRS_II_Score


# ==== Create FIMA scores
# Add FIMA subscores

df_safepd <- df_safepd %>%
  mutate(FIMA_1_Gesamt = rowSums(across(c( # Row sums for FIMA_1_Gesamt
    "FIMA_1_Hausarzt", "FIMA_1_Neurologe", "FIMA_1_Psychiater", 
    "FIMA_1_Internist_FA", "FIMA_1_Gynaekologe", "FIMA_1_Urologe", 
    "FIMA_1_Orthopaede", "FIMA_1_Notfall_KH", "FIMA_1_other"
  )), na.rm = TRUE)) %>%

  mutate(   # Binary grouping for FIMA_13 categories
    FIMA_13_Mobilitaet = as.numeric(rowSums(across(c(
      "FIMA_13_Gehstock", "FIMA_13_Anti_Freezing_Stock", "FIMA_13_Kamptokormie_Rucksack", 
      "FIMA_13_Rollator", "FIMA_13_Rollstuhl", "FIMA_13_Treppenlift"
    )), na.rm = TRUE) > 0),
    
    FIMA_13_Hygiene = as.numeric(rowSums(across(c(
      "FIMA_13_Badewannenlift", "FIMA_13_Badewanneneinstieghilfe", 
      "FIMA_13_Badewannensitz", "FIMA_13_Toilettensitz", "FIMA_13_Toilettenstuhl", 
      "FIMA_13_Urinflasche", "FIMA_13_Inkontinenzeinlagen"
    )), na.rm = TRUE) > 0),

    FIMA_13_Atmung = as.numeric(rowSums(across(c(
      "FIMA_13_Sauerstoffgeraet", "FIMA_13_Schlafapnoe"
    )), na.rm = TRUE) > 0),

    FIMA_13_Tremor = as.numeric(rowSums(across(c(
      "FIMA_13_Tremorbesteck", "FIMA_13_Greifzange"
    )), na.rm = TRUE) > 0)
  ) %>%

  mutate(FIMA_13_Anzahl = rowSums(across(c(   # Total count for FIMA_13 related equipment
    "FIMA_13_Gehstock", "FIMA_13_Anti_Freezing_Stock", "FIMA_13_Kamptokormie_Rucksack", 
    "FIMA_13_Rollator", "FIMA_13_Rollstuhl", "FIMA_13_Treppenlift", 
    "FIMA_13_Badewannenlift", "FIMA_13_Brille", "FIMA_13_Hoergeraet", 
    "FIMA_13_Tremorbesteck", "FIMA_13_Greifzange", "FIMA_13_Sauerstoffgeraet", 
    "FIMA_13_Schlafapnoe", "FIMA_13_Badewanneneinstieghilfe", 
    "FIMA_13_Badewannensitz", "FIMA_13_Toilettensitz", "FIMA_13_Toilettenstuhl", 
    "FIMA_13_Urinflasche", "FIMA_13_Inkontinenzeinlagen", "FIMA_13_Hausnotruf"
  )), na.rm = TRUE)) %>%

  mutate(   # Binary groups for FIMA_8 to FIMA_16
    FIMA_8_Group = as.numeric(FIMA_8 != 0),
    FIMA_9_Group = as.numeric(FIMA_9 != 0),
    FIMA_10_Group = as.numeric(FIMA_10 != 0),
    FIMA_11_Group = as.numeric(FIMA_11 != 0),
    FIMA_12_Group = as.numeric(FIMA_12 != 0),
    FIMA_14_Group = as.numeric(FIMA_14 != 1),  # 0 = zuhause wohnhaft
    FIMA_16_Group = as.numeric(FIMA_16 != 1)   # 0 = gesetzlich versichert
  )

# Previous version of the FIMA scores
# FIMA
#df_safepd$FIMA_1_Gesamt <- rowSums(df_safepd[, c("FIMA_1_Hausarzt", "FIMA_1_Neurologe", "FIMA_1_Psychiater", "FIMA_1_Internist_FA", "FIMA_1_Gynaekologe", "FIMA_1_Urologe", "FIMA_1_Orthopaede", "FIMA_1_Notfall_KH", "FIMA_1_other")])
#df_safepd$FIMA_13_Mobilitaet <- ifelse(rowSums(df_safepd[, c("FIMA_13_Gehstock", "FIMA_13_Anti_Freezing_Stock", "FIMA_13_Kamptokormie_Rucksack", "FIMA_13_Rollator", "FIMA_13_Rollstuhl", "FIMA_13_Treppenlift")]) > 0, 1, 0)
#df_safepd$FIMA_13_Hygiene <- ifelse(rowSums(df_safepd[, c("FIMA_13_Badewannenlift", "FIMA_13_Badewanneneinstieghilfe", "FIMA_13_Badewannensitz", "FIMA_13_Toilettensitz", "FIMA_13_Toilettenstuhl", "FIMA_13_Urinflasche", "FIMA_13_Inkontinenzeinlagen")]) > 0, 1, 0)
#df_safepd$FIMA_13_Atmung <- ifelse(rowSums(df_safepd[, c("FIMA_13_Sauerstoffgeraet", "FIMA_13_Schlafapnoe")]) > 0, 1, 0)
#df_safepd$FIMA_13_Tremor <- ifelse(rowSums(df_safepd[, c("FIMA_13_Tremorbesteck", "FIMA_13_Greifzange")]) > 0, 1, 0)
#df_safepd$FIMA_13_Anzahl <- rowSums(df_safepd[, c("FIMA_13_Gehstock", "FIMA_13_Anti_Freezing_Stock", "FIMA_13_Kamptokormie_Rucksack", "FIMA_13_Rollator", "FIMA_13_Rollstuhl", "FIMA_13_Treppenlift", "FIMA_13_Badewannenlift", "FIMA_13_Brille", "FIMA_13_Hoergeraet", "FIMA_13_Tremorbesteck", "FIMA_13_Greifzange", "FIMA_13_Sauerstoffgeraet", "FIMA_13_Schlafapnoe", "FIMA_13_Badewanneneinstieghilfe", "FIMA_13_Badewannensitz", "FIMA_13_Toilettensitz", "FIMA_13_Toilettenstuhl", "FIMA_13_Urinflasche", "FIMA_13_Inkontinenzeinlagen", "FIMA_13_Hausnotruf")])
#df_safepd$FIMA_8_Group <- as.numeric(ifelse(df_safepd$FIMA_8 == 0, "0", "1")) ## 0 = kein Pflegegrad
#df_safepd$FIMA_9_Group <- as.numeric(ifelse(df_safepd$FIMA_9 == 0, "0", "1")) ## 0 = keine Reha
#df_safepd$FIMA_10_Group <- as.numeric(ifelse(df_safepd$FIMA_10 == 0, "0", "1")) ## 0 = keine Tagesklinik
#df_safepd$FIMA_11_Group <- as.numeric(ifelse(df_safepd$FIMA_11 == 0, "0", "1")) ## 0 = kein Krankenhaus
#df_safepd$FIMA_12_Group <- as.numeric(ifelse(df_safepd$FIMA_12 == 0, "0", "1")) ## 0 = keine Psychiatrie
#df_safepd$FIMA_14_Group <- as.numeric(ifelse(df_safepd$FIMA_14 == 1, "0", "1")) ## 0 = zuhause wohnhaft 
#df_safepd$FIMA_16_Group <- as.numeric(ifelse(df_safepd$FIMA_16 == 1 , "0", "1")) ## 0 = gesetzlich versichert


# TODO: For the time being, I'm not sure if this makes sense. I haven't seen the analyses but so far you are getting rid of a lot of variance as you are omitting parts of the questionnaire and creating very simple variables, which may hamper analyses later. But again, I'm not sure about this.
