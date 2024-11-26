# Hypothese: Mehr als Zwei Ärzte in Behandlung --> negative Auswirkung auf Pflege und Kommunikation (Desmedt et al.)
# Spalten für die Arztbesuche 
doctor_columns <- c("FIMA_1_Hausarzt", "FIMA_1_Neurologe", "FIMA_1_Psychiater", "FIMA_1_Internist_FA", "FIMA_1_Gynaekologe", "FIMA_1_Urologe", "FIMA_1_Orthopaede", "FIMA_1_Notfall_KH", "FIMA_1_other")  
# Überprüfen, ob der Proband bei mehr als einem Arzt war (d.h. mindestens zwei Spalten haben einen Wert größer als 0)
df_safepd$visited_two_or_more <- ifelse(rowSums(df_safepd[, doctor_columns] > 0, na.rm = TRUE) >= 2, 1, 0)
# Korrelation
cor.test(df_safepd$visited_two_or_more, df_safepd$nursing_care_Group, method = "pearson", use = "complete.obs")
chisq.test(df_safepd$visited_two_or_more, df_safepd$nursing_care_Group)
cor.test(df_safepd$visited_two_or_more, df_safepd$communication_with_me_Group, method = "pearson", use = "complete.obs")
chisq.test(df_safepd$visited_two_or_more, df_safepd$communication_with_me_Group)
cor.test(df_safepd$visited_two_or_more, df_safepd$communication_between_professionals_Group, method = "pearson", use = "complete.obs")
chisq.test(df_safepd$visited_two_or_more, df_safepd$communication_between_professionals_Group)

# Hypothese: positive Assoziation von Sicherheit & guter Pflege 
cor.test(df_safepd$overall_situation_Group, df_safepd$nursing_care_Group, method = "pearson", use = "complete.obs")
chisq.test(df_safepd$overall_situation_Group, df_safepd$nursing_care_Group)

# Hypothese: positive Assoziation von Sicherheit & Hilfe von Familienangehörigen (Milberg et al.)
cor.test(df_safepd$overall_situation_Group, df_safepd$FIMA_5, method = "pearson", use = "complete.obs")
chisq.test(df_safepd$overall_situation_Group, df_safepd$FIMA_5)

# Hypothese: negative Assoziation von Sicherheit & finanziellen Problemen
cor.test(df_safepd$overall_situation_Group, df_safepd$financial_worries_Group, method = "pearson", use = "complete.obs")
chisq.test(df_safepd$overall_situation_Group, df_safepd$financial_worries_Group)

# Hypothese: schlechteres psychosoziales Funktionsniveau bei Frauen (Perepezko et al.)
cor.test(df_safepd$loneliness_Group, df_safepd$gender_Group, method = "pearson", use = "complete.obs")
chisq.test(df_safepd$loneliness_Group, df_safepd$gender_Group)
chisq.test(df_safepd$loneliness_Group, df_safepd$persons_houshold_Group)
H1 <- glm(df_safepd$loneliness_Group ~ df_safepd$gender_Group * df_safepd$persons_houshold_Group, family = "binomial")
summary(H1)

cor.test(df_safepd$not_at_peace_with_myself_Group, df_safepd$gender_Group, method = "pearson", use = "complete.obs")
chisq.test(df_safepd$not_at_peace_with_myself_Group, df_safepd$gender_Group)
chisq.test(df_safepd$not_at_peace_with_myself_Group, df_safepd$persons_houshold_Group)
H2 <- glm(df_safepd$not_at_peace_with_myself_Group ~ df_safepd$gender_Group * df_safepd$persons_houshold_Group, family = "binomial")
summary(H2)

cor.test(df_safepd$persons_houshold_Group, df_safepd$gender_Group, method = "pearson", use = "complete.obs")
cor.test(df_safepd$persons_houshold_Group, df_safepd$loneliness_Group, method = "pearson", use = "complete.obs")

# Hypothese: schlechtere soziale Unterstützung bei Frauen (Perepezko et al.)
cor.test(df_safepd$FIMA_5, df_safepd$gender_Group, method = "pearson", use = "complete.obs")
chisq.test(df_safepd$FIMA_5, df_safepd$gender_Group)
H3 <- glm(df_safepd$FIMA_5 ~ df_safepd$gender_Group, family = "binomial")
summary(H3)

# Hypothese: bessere Paarbeziehung (= family role???) bei älteren Paaren (Perepezko et al.)
cor.test(df_safepd$family_role_Group, df_safepd$age, method = "pearson", use = "complete.obs")
wilcox.test(df_safepd$family_role_Group, df_safepd$age)
H4 <- glm(df_safepd$family_role_Group ~ df_safepd$age, family = "binomial")
summary(H4)

# Hypothese: Unsicherheit vor der Zukunft mit geringerer Beziehungsqualität (family role???) verbunden (ebd.)
cor.test(df_safepd$family_role_Group, df_safepd$uncertain_future_Group, method = "pearson", use = "complete.obs")
chisq.test(df_safepd$family_role_Group, df_safepd$uncertain_future_Group)
H5 <- glm(df_safepd$family_role_Group ~ df_safepd$uncertain_future_Group, family = "binomial")
summary(H5)

# Hypothese: höheres Alter, längere Krankheitsdauer und weibliches Geschlecht negativ mit Berufsleben assoziiert (ebd.)
H6 <- glm(df_safepd$employment_status_Group  ~ df_safepd$age + df_safepd$years_since_diagnosis + df_safepd$gender_Group)
summary(H6)

# Hypothese: schlechteres allgemeines soziales Funktionsniveau bei kognitiver Beeinträchtigung (ebd.)
cor.test(df_safepd$mental_abilities_Group, df_safepd$family_role_Group)
cor.test(df_safepd$mental_abilities_Group, df_safepd$overload_among_people_Group)

# Hypothese: jüngere Patienten fühlen sich bei stationärem Aufenthalt sicherer als ältere Patienten (Robinson)
H7 <- glm(df_safepd$overall_situation_Group ~ df_safepd$age * df_safepd$FIMA_11_Group, family = gaussian())
summary(H7)

# Hypothese: mehr als eine Hospitalisierung ist negativ mit allgemeiner Sicherheit assoziiert (meine)
df_safepd$multiple_clinic_stays <- ifelse(df_safepd$FIMA_11 + df_safepd$FIMA_12 > 1, 1, 0)
cor.test(df_safepd$overall_situation_Group, df_safepd$multiple_clinic_stays, method = "pearson", use = "compelte.obs")
H8 <- glm(df_safepd$overall_situation_Group ~ df_safepd$multiple_clinic_stays, family = binomial())
summary(H8)
