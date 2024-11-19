## Test auf Normalverteilung
VarNVTest <- c("age","gender", "nationality", "martial_status", "years_since_diagnosis", "persons_houshold", "school_graduation", "professional_graduation", "employment_status", "UPDRS_I_Score", "UPDRS_II_Score", "lack_of_information", "uncertain_future",	"chaging_symptom_severity",	"gait_insecurity_fall",	"pain",	"gastrointestinal_symptoms",	"urinary_symptoms",	"mental_abilities",	"mental_symptoms",	"other_disease",	"nursing_care",	"side_effects_complications",	"access_healthcare",	"communication_with_me",	"communication_between_professionals",	"loneliness",	"everyday_problems",	"daily_routine",	"overload_among_people",	"pejorativ_looks_comments",	"family_role",	"conflicts_with_relatives",	"victim_to_crime",	"financial_worries",	"not_at_peace_with_myself",	"participation_in_road_traffic",	"overall_situation")

shapiro_tests <- lapply(VarNVTest, function(variable) {
  shapiro_test_result <- shapiro.test(SAFEPD[[variable]])
  return(data.frame(Variable = variable, p_value = shapiro_test_result$p.value, 
                    is_normal = shapiro_test_result$p.value > 0.05))})
shapiro_results_table <- do.call(rbind, shapiro_tests)

qqnorm(SAFEPD$age, main = "Q-Q-Plot Alter")
qqline(SAFEPD$age)

qqnorm(SAFEPD$UPDRS_I_Score, main = "Q-Q-Plot UPDRS 1")
qqline(SAFEPD$UPDRS_I_Score)

qqnorm(SAFEPD$UPDRS_II_Score, main = "Q-Q-Plot UPDRS 2")
qqline(SAFEPD$UPDRS_II_Score)
