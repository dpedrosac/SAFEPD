# Hypothese: Mehr als Zwei Ärzte in Behandlung --> negative Auswirkung auf Pflege und Kommunikation (Desmedt et al.)
# Spalten für die Arztbesuche 
doctor_columns <- c("FIMA_1_Hausarzt", "FIMA_1_Neurologe", "FIMA_1_Psychiater", "FIMA_1_Internist_FA", "FIMA_1_Gynaekologe", "FIMA_1_Urologe", "FIMA_1_Orthopaede", "FIMA_1_Notfall_KH", "FIMA_1_other")  
# Überprüfen, ob der Proband bei mehr als einem Arzt war (d.h. mindestens zwei Spalten haben einen Wert größer als 0)
SAFEPD$visited_two_or_more <- ifelse(rowSums(SAFEPD[, doctor_columns] > 0, na.rm = TRUE) >= 2, 1, 0)
# Korrelation
cor.test(SAFEPD$visited_two_or_more, SAFEPD$nursing_care_Group, method = "pearson", use = "complete.obs")
cor.test(SAFEPD$visited_two_or_more, SAFEPD$communication_with_me_Group, method = "pearson", use = "complete.obs")
cor.test(SAFEPD$visited_two_or_more, SAFEPD$communication_between_professionals_Group, method = "pearson", use = "complete.obs")

# Hypothese: positive Assoziation von Sicherheit & guter Pflege 
cor.test(SAFEPD$overall_situation_Group, SAFEPD$nursing_care_Group, method = "pearson", use = "complete.obs")

# Hypothese: positive Assoziation von Sicherheit & Hilfe von Familienangehörigen (Milberg et al.)
cor.test(SAFEPD$overall_situation_Group, SAFEPD$FIMA_5, method = "pearson", use = "complete.obs")

# Hypothese: negative Assoziation von Sicherheit & finanziellen Problemen
cor.test(SAFEPD$overall_situation_Group, SAFEPD$financial_worries_Group, method = "pearson", use = "complete.obs")

# Hypothese: schlechteres psychosoziales Funktionsniveau bei Frauen (Perepezko et al.)
cor.test(SAFEPD$loneliness_Group, SAFEPD$gender_Group, method = "pearson", use = "complete.obs")
H1 <- glm(SAFEPD$loneliness_Group ~ SAFEPD$gender_Group, family = "binomial")
summary(H1)

cor.test(SAFEPD$not_at_peace_with_myself_Group, SAFEPD$gender_Group, method = "pearson", use = "complete.obs")
H2 <- glm(SAFEPD$not_at_peace_with_myself_Group ~ SAFEPD$gender_Group, family = "binomial")
summary(H2)

# Hypothese: schlechtere soziale Unterstützung bei Frauen (Perepezko et al.)
cor.test(SAFEPD$FIMA_5, SAFEPD$gender_Group, method = "pearson", use = "complete.obs")
H3 <- glm(SAFEPD$FIMA_5 ~ SAFEPD$gender_Group, family = "binomial")
summary(H3)

# Hypothese: bessere Paarbeziehung (= family role???) bei älteren Paaren (Perepezko et al.)
cor.test(SAFEPD$family_role_Group, SAFEPD$age, method = "pearson", use = "complete.obs")
H4 <- glm(SAFEPD$family_role_Group ~ SAFEPD$age, family = "binomial")
summary(H4)

# Hypothese: Unsicherheit vor der Zukunft mit geringerer Beziehungsqualität (family role???) verbunden (ebd.)
cor.test(SAFEPD$family_role_Group, SAFEPD$uncertain_future_Group, method = "pearson", use = "complete.obs")
H5 <- glm(SAFEPD$family_role_Group ~ SAFEPD$uncertain_future_Group, family = "binomial")
summary(H5)

# Hypothese: höheres Alter, längere Krankheitsdauer und weibliches Geschlecht negativ mit Berufsleben assoziiert (ebd.)
H6 <- glm(SAFEPD$employment_status_Group)

# Hypothese: schlechteres allgemeines soziales Funktionsniveau bei kognitiver Beeinträchtigung (ebd.)
cor.test(SAFEPD$mental_abilities_Group)

# Hypothese: jüngere Patienten fühlen sich bei stationärem Aufenthalt sicherer als ältere Patienten (Robinson et al.)
H7 <- glm(SAFEPD$overall_situation_Group ~ SAFEPD$age * SAFEPD$FIMA_11_Group, family = gaussian())
summary(H7)

# Hypothese: mehr als eine Hospitalisierung ist negativ mit allgemeiner Sicherheit assoziiert (meine)
SAFEPD$multiple_clinic_stays <- ifelse(SAFEPD$FIMA_11 + SAFEPD$FIMA_12 > 1, 1, 0)
cor.test(SAFEPD$overall_situation_Group, SAFEPD$multiple_clinic_stays, method = "pearson", use = "compelte.obs")
H8 <- glm(SAFEPD$overall_situation_Group ~ SAFEPD$multiple_clinic_stays, family = binomial())
summary(H8)
