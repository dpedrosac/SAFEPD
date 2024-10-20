# Diagram: Altersverteilung
SAFEPD$age_cat <- cut(SAFEPD$age, 
                      breaks = c(0, 45, seq(50, 85, by = 5), Inf), 
                      right = FALSE,  
                      labels = c("<45", "45-49", "50-54", "55-59", "60-64", "65-69", "70-74", "75-79", "80-84", ">84"), 
                      include.lowest = TRUE)  
hist_age <- table(SAFEPD$age_cat)
bar_positions <- barplot(hist_age, 
                         xlab = "Alter", 
                         ylab = "Häufigkeit", 
                         main = "Altersverteilung", 
                         col = "steelblue", 
                         ylim = c(0, max(hist_age) + 10),  
                         space = 0)  
text(x = bar_positions, 
     y = hist_age, 
     label = hist_age, 
     pos = 3,  
     cex = 0.8, 
     col = "black")

# Diagram: Verteilung der Krankheitsdauer 
SAFEPD$years_since_diagnosis_cat <- cut(SAFEPD$years_since_diagnosis, 
                                        breaks = c(0, 1, 6, 11, 16, 21, 26, Inf), 
                                        right = FALSE,   
                                        labels = c("<1", "1-5", "6-10", "11-15", "16-20", "21-25", ">25"), 
                                        include.lowest = TRUE)  
hist_years_since_diagnosis <- table(SAFEPD$years_since_diagnosis_cat)
bar_positions <- barplot(hist_years_since_diagnosis, 
                         xlab = "Krankheitsdauer (Jahre)", 
                         ylab = "Häufigkeit", 
                         main = "Verteilung der Krankheitsdauer", 
                         col = "steelblue", 
                         ylim = c(0, 80),
                         space = 0)  
text(x = bar_positions, 
     y = hist_years_since_diagnosis, 
     label = hist_years_since_diagnosis, 
     pos = 3,  # Über den Säulen
     cex = 0.8, 
     col = "black")

# Diagramme: Punkteverteilung UPDRS 
hist_UPDRS_I <- hist(SAFEPD$UPDRS_I_Score, xlab="UPDRS Score", ylab="Häufigkeit", main="UPDRS Teil 1", col="steelblue", ylim=c(0,80), breaks=4)
text(hist_UPDRS_I$mids, hist_UPDRS_I$counts, labels = hist_UPDRS_I$counts, adj=c(0.5, -0.5))
hist_UPDRS_II <- hist(SAFEPD$UPDRS_II_Score, xlab="UPDRS Score", ylab="Häufigkeit", main="UPDRS Teil 2", col="steelblue", ylim=c(0,100), xlim=c(0,50), breaks=4)
text(hist_UPDRS_II$mids, hist_UPDRS_II$counts, labels = hist_UPDRS_II$counts, adj=c(0.5, -0.5))

# Diagramme: FIMA
# Hausarzt & Neurologe
create_custom_histogram <- function(data, column, title) {
  data[[column]] <- ifelse(data[[column]] >= 5, 5, data[[column]])
  categories <- c(0, 1, 2, 3, 4, "5+")
  visit_counts <- table(factor(data[[column]], levels = 0:5))
  bar_positions <- barplot(visit_counts, 
                           names.arg = categories, 
                           xlab = "Anzahl der Besuche", 
                           ylab = "Häufigkeit", 
                           main = title, 
                           col = "steelblue", 
                           ylim = c(0, max(visit_counts) + 10),
                           space = 0)  
  text(x = bar_positions,  
       y = visit_counts, 
       label = visit_counts, 
       pos = 3,  
       cex = 0.8, 
       col = "black")
  return(visit_counts)
}
Hausarzt_histogram <- create_custom_histogram(SAFEPD, "FIMA_1_Hausarzt", "Anzahl der Konsultationen des Hausarztes")
Neurologe_histogram <- create_custom_histogram(SAFEPD, "FIMA_1_Neurologe", "Anzahl der Konsultationen des Neurologen")

# andere Fachärzte
absolute_häufigkeiten_Arzt <- data.frame(
  Facharzt = c("Internist", "Gynaekologe", "Urologe", "Orthopaede", "Psychiater", "anderer Facharzt", "Notfallambulanz"),
  Ja = c(
    sum(SAFEPD$FIMA_1_Internist_FA > 0),
    sum(SAFEPD$FIMA_1_Gynaekologe > 0),
    sum(SAFEPD$FIMA_1_Urologe > 0),
    sum(SAFEPD$FIMA_1_Orthopaede > 0),
    sum(SAFEPD$FIMA_1_Psychiater > 0),
    sum(SAFEPD$FIMA_1_other > 0),
    sum(SAFEPD$FIMA_1_Notfall_KH > 0)
  ),
  Nein = c(
    sum(SAFEPD$FIMA_1_Internist_FA == 0),
    sum(SAFEPD$FIMA_1_Gynaekologe == 0),
    sum(SAFEPD$FIMA_1_Urologe == 0),
    sum(SAFEPD$FIMA_1_Orthopaede == 0),
    sum(SAFEPD$FIMA_1_Psychiater == 0),
    sum(SAFEPD$FIMA_1_other == 0),
    sum(SAFEPD$FIMA_1_Notfall_KH == 0)
  )
)
long_data_Arzt <- pivot_longer(absolute_häufigkeiten_Arzt, 
                               cols = c("Ja", "Nein"), 
                               names_to = "Besuch", 
                               values_to = "Anzahl")
long_data_Arzt$Facharzt <- factor(long_data_Arzt$Facharzt, levels = c("Internist", "Gynaekologe", "Urologe", "Orthopaede", "Psychiater", "anderer Facharzt", "Notfallambulanz"))
ggplot(long_data_Arzt, aes(x = Facharzt, y = Anzahl, fill = Besuch)) +
  geom_bar(stat = "identity", position = "dodge") +  
  geom_text(aes(label = Anzahl), 
            position = position_dodge(width = 0.9),  
            vjust = -0.5, size = 3) + 
  labs(x = "Facharzt", y = "Anzahl der Probanden", fill = "Besuch") +  
  theme_minimal() +  # Minimalistisches Theme
  scale_fill_manual(values = c("Ja" = "lightblue", "Nein" = "steelblue")) +  
  ggtitle("Haben Sie innerhalb der letzten drei Monate den folgenden Arzt konsultiert?") +  
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 

# Therapieangebote
absolute_häufigkeiten_Therapien <- data.frame(
  Therapie = c("Krankengymnastik", "Ergotherapie", "Sprachtherapie", 
               "Heilpraktiker", "Chiropraktiker", "Osteopath", "Psychotherapeut"),
  Ja = c(
    sum(SAFEPD$FIMA_2_Krankengymnastik > 0),
    sum(SAFEPD$FIMA_2_Ergotherapie > 0),
    sum(SAFEPD$FIMA_2_Sprachtherapie > 0),
    sum(SAFEPD$FIMA_2_Heilpraktiker > 0),
    sum(SAFEPD$FIMA_2_Chiropraktiker > 0),
    sum(SAFEPD$FIMA_2_Osteopath > 0),
    sum(SAFEPD$FIMA_2_Psychotherapeut > 0)
  ),
  Nein = c(
    sum(SAFEPD$FIMA_2_Krankengymnastik == 0),
    sum(SAFEPD$FIMA_2_Ergotherapie == 0),
    sum(SAFEPD$FIMA_2_Sprachtherapie == 0),
    sum(SAFEPD$FIMA_2_Heilpraktiker == 0),
    sum(SAFEPD$FIMA_2_Chiropraktiker == 0),
    sum(SAFEPD$FIMA_2_Osteopath == 0),
    sum(SAFEPD$FIMA_2_Psychotherapeut == 0)
  )
)
long_data_Therapien <- pivot_longer(absolute_häufigkeiten_Therapien, 
                                    cols = c("Ja", "Nein"), 
                                    names_to = "Besuch", 
                                    values_to = "Anzahl")
long_data_Therapien$Therapie <- factor(long_data_Therapien$Therapie, 
                                       levels = c("Krankengymnastik", "Ergotherapie", "Sprachtherapie", 
                                                  "Heilpraktiker", "Chiropraktiker", "Osteopath", "Psychotherapeut"))
ggplot(long_data_Therapien, aes(x = Therapie, y = Anzahl, fill = Besuch)) +
  geom_bar(stat = "identity", position = "dodge") +  
  geom_text(aes(label = Anzahl), 
            position = position_dodge(width = 0.9),  
            vjust = -0.5, size = 3) + 
  labs(x = "Therapie", y = "Anzahl der Probanden", fill = "Besuch") +  
  theme_minimal() +  # Minimalistisches Theme
  scale_fill_manual(values = c("Ja" = "lightblue", "Nein" = "steelblue")) +  
  ggtitle("Haben Sie innerhalb der letzten drei Monate eine der folgenden Therapien in Anspruch genommen?") +  
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Pflegeangbote
absolute_häufigkeiten_Pflege <- data.frame(
  Variable = c("FIMA_3", "FIMA_4", "FIMA_5", "FIMA_6", "FIMA_7"),
  Ja = c(
    sum(SAFEPD$FIMA_3 > 0),
    sum(SAFEPD$FIMA_4 > 0),
    sum(SAFEPD$FIMA_5 > 0),
    sum(SAFEPD$FIMA_6 > 0),
    sum(SAFEPD$FIMA_7 > 0)
  ),
  Nein = c(
    sum(SAFEPD$FIMA_3 == 0),
    sum(SAFEPD$FIMA_4 == 0),
    sum(SAFEPD$FIMA_5 == 0),
    sum(SAFEPD$FIMA_6 == 0),
    sum(SAFEPD$FIMA_7 == 0)
  )
)
long_data_Pflege <- pivot_longer(absolute_häufigkeiten_Pflege, 
                                 cols = c("Ja", "Nein"), 
                                 names_to = "Besuch", 
                                 values_to = "Anzahl")
long_data_Pflege$Variable <- factor(long_data_Pflege$Variable, 
                                    levels = c("FIMA_3", "FIMA_4", "FIMA_5", "FIMA_6", "FIMA_7"),
                                    labels = c("ambulanter Pflegedienst", 
                                               "bezahlte Haushaltshilfe", 
                                               "Verwandte und Bekannte", 
                                               "teilstationäre Pflege", 
                                               "vollstationäre Pflege"))
ggplot(long_data_Pflege, aes(x = Variable, y = Anzahl, fill = Besuch)) +
  geom_bar(stat = "identity", position = "dodge") +  
  geom_text(aes(label = Anzahl), 
            position = position_dodge(width = 0.9),  
            vjust = -0.5, size = 3) + 
  labs(x = "Pflegeleistungen", y = "Anzahl der Probanden", fill = "Besuch") +  
  theme_minimal() +  
  scale_fill_manual(values = c("Ja" = "lightblue", "Nein" = "steelblue")) +  
  ggtitle("Haben Sie innerhalb der letzten drei Monate eine der folgenden Pflegeleistungen in Anspruch genommen?") +  
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Pflegegrade
hist_Pflegegrade <- hist(SAFEPD$FIMA_8,  
                         breaks = seq(-0.5, 5.5, by = 1),   
                         col = "steelblue",                 
                         main = "Häufigkeit der Pflegegrade",
                         xlab = "Ausprägung",               
                         ylab = "Häufigkeit",
                         plot = FALSE)                      
plot(hist_Pflegegrade, ylim = c(0, max(hist_Pflegegrade$counts) * 1.1), col = "steelblue", main = "Häufigkeit der Pflegegrade", xlab = "Ausprägung", ylab = "Häufigkeit",)
text(hist_Pflegegrade$mids, hist_Pflegegrade$counts, labels = hist_Pflegegrade$counts, pos = 3)

# Krankenhausaufenthalte
absolute_häufigkeiten_KH <- data.frame(
  Variable = c("FIMA_9", "FIMA_10", "FIMA_11", "FIMA_12"),
  Ja = c(
    sum(SAFEPD$FIMA_9 > 0),
    sum(SAFEPD$FIMA_10 > 0),
    sum(SAFEPD$FIMA_11 > 0),
    sum(SAFEPD$FIMA_12 > 0)
  ),
  Nein = c(
    sum(SAFEPD$FIMA_9 == 0),
    sum(SAFEPD$FIMA_10 == 0),
    sum(SAFEPD$FIMA_11 == 0),
    sum(SAFEPD$FIMA_12 == 0)
  )
)
long_data_KH <- pivot_longer(absolute_häufigkeiten_KH, 
                             cols = c("Ja", "Nein"), 
                             names_to = "Besuch", 
                             values_to = "Anzahl")
long_data_KH$Variable <- factor(long_data_KH$Variable, 
                                levels = c("FIMA_9", "FIMA_10", "FIMA_11", "FIMA_12"),
                                labels = c("Rehabilitation", 
                                           "Tagesklinik", 
                                           "somatische Klinik", 
                                           "psychiatrische Klinik"))
ggplot(long_data_KH, aes(x = Variable, y = Anzahl, fill = Besuch)) +
  geom_bar(stat = "identity", position = "dodge") +  
  geom_text(aes(label = Anzahl), 
            position = position_dodge(width = 0.9),  
            vjust = -0.5, size = 3) +  
  labs(x = "Krankenhausleistungen", y = "Anzahl der Probanden", fill = "Besuch") +  
  theme_minimal() +  
  scale_fill_manual(values = c("Ja" = "lightblue", "Nein" = "steelblue")) +  
  ggtitle("Haben Sie innerhalb der letzten drei Monate eine der folgenden Krankenhausleistungen in Anspruch genommen?") +  
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Sicherheitsfragebogen 
# Funktion zur Berechnung der Häufigkeit 
berechne_häufigkeit <- function(daten) {
  häufigkeiten <- table(daten)
  return(häufigkeiten)
}

# Funktion zur Berechnung der Häufigkeiten für mehrere Variablen
berechne_alle_häufigkeiten <- function(daten, spalten_namen) {
  alle_häufigkeiten <- lapply(spalten_namen, function(spalte) {
    return(berechne_häufigkeit(daten[[spalte]]))
  })
  names(alle_häufigkeiten) <- spalten_namen
  return(alle_häufigkeiten)
}

# Farbpaletten
zwei_palette <- c("lightblue","steelblue")

# Berechnung der Häufigkeiten für alle Fragen
alle_häufigkeiten <- berechne_alle_häufigkeiten(SAFEPD, VarSAFEPDQA)
VarSAFEPDQA_Group <- paste(VarSAFEPDQA, "_Group", sep = "")
alle_häufigkeiten_Group <- berechne_alle_häufigkeiten(SAFEPD, VarSAFEPDQA_Group)

# Gruppierung der Häufigkeiten in 1+2+3 und 4+5
gruppen_haeufigkeiten <- function(haeufigkeit) {
  gruppe_1_2_3 <- sum(haeufigkeit[c("1", "2", "3")], na.rm = TRUE)
  gruppe_4_5 <- sum(haeufigkeit[c("4", "5")], na.rm = TRUE)
  return(c(gruppe_1_2_3, gruppe_4_5))
}

# Anwendung der Gruppierung auf alle Häufigkeiten
alle_häufigkeiten_gruppen <- lapply(alle_häufigkeiten, gruppen_haeufigkeiten)

# Erstellung eines DataFrames für die Visualisierung
kategorien <- rep(VarSAFEPDQA, each = 2)
gruppen <- rep(c("restricted", "unrestricted"), length(VarSAFEPDQA))
häufigkeiten <- unlist(alle_häufigkeiten_gruppen)
häufigkeitstabelle <- data.frame(Kategorie = kategorien, Gruppe = gruppen, Häufigkeit = häufigkeiten)

# Filter für spezifische Krankheitsfaktoren 1
krankheitsfaktoren <- c("gait_insecurity_fall", "pain", "gastrointestinal_symptoms", "urinary_symptoms")
häufigkeitstabelle_phys <- subset(häufigkeitstabelle, Kategorie %in% krankheitsfaktoren)
# Namen der Kategorien ändern
häufigkeitstabelle_phys$Kategorie <- factor(häufigkeitstabelle_phys$Kategorie, levels = krankheitsfaktoren,
                                            labels = c("Gait Insecurity", "Pain", "Gastrointestinal Symptoms", "Urinary Symptoms"))
# Gestapeltes Säulendiagramm
ggplot(häufigkeitstabelle_phys, aes(x = Kategorie, y = Häufigkeit, fill = Gruppe)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
  geom_text(aes(label = Häufigkeit), vjust = -0.5, position = position_dodge(width = 0.9), size = 3) +
  labs(x = "", y = "frequency", title = "disease domain: physical symptoms") +
  scale_fill_manual(values = zwei_palette, name = "percieved safety", labels = c("restricted", "unrestricted")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
# Filter für spezifische Krankheitsfaktoren 2
krankheitsfaktoren2 <- c("side_effects_complications", "chaging_symptom_severity", "uncertain_future", "other_disease")
häufigkeitstabelle_dis2 <- subset(häufigkeitstabelle, Kategorie %in% krankheitsfaktoren2)
# Namen der Kategorien ändern
häufigkeitstabelle_dis2$Kategorie <- factor(häufigkeitstabelle_dis2$Kategorie, levels = krankheitsfaktoren2,
                                            labels = c("Side Effects", "Changing Symptom Severity", "Uncertain Future", "Other Diseases"))
# Gestapeltes Säulendiagramm
ggplot(häufigkeitstabelle_dis2, aes(x = Kategorie, y = Häufigkeit, fill = Gruppe)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
  geom_text(aes(label = Häufigkeit), vjust = -0.5, position = position_dodge(width = 0.9), size = 3) +
  labs(x = "", y = "frequency", title = "disease domain: therapy and progression") +
  scale_fill_manual(values = zwei_palette, name = "percieved safety", labels = c("restricted", "unrestricted")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Filter für spezifische Krankheitsfaktoren 3
krankheitsfaktoren3 <- c("mental_abilities", "mental_symptoms")
häufigkeitstabelle_dis3 <- subset(häufigkeitstabelle, Kategorie %in% krankheitsfaktoren3)
# Namen der Kategorien ändern
häufigkeitstabelle_dis3$Kategorie <- factor(häufigkeitstabelle_dis3$Kategorie, levels = krankheitsfaktoren3,
                                            labels = c("Mental Abilities", "Mental Symptoms"))
# Gestapeltes Säulendiagramm
ggplot(häufigkeitstabelle_dis3, aes(x = Kategorie, y = Häufigkeit, fill = Gruppe)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
  geom_text(aes(label = Häufigkeit), vjust = -0.5, position = position_dodge(width = 0.9), size = 3) +
  labs(x = "", y = "frequency", title = "disease domain: psychiatric symptoms") +
  scale_fill_manual(values = zwei_palette, name = "percieved safety", labels = c("restricted", "unrestricted")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Filter für spezifische Krankheitsfaktoren 4
krankheitsfaktoren4 <- c("loneliness", "not_at_peace_with_myself")
häufigkeitstabelle_dis4 <- subset(häufigkeitstabelle, Kategorie %in% krankheitsfaktoren4)
# Namen der Kategorien ändern
häufigkeitstabelle_dis4$Kategorie <- factor(häufigkeitstabelle_dis4$Kategorie, levels = krankheitsfaktoren4,
                                            labels = c("Loneliness", "Inner Peace"))
# Gestapeltes Säulendiagramm
ggplot(häufigkeitstabelle_dis4, aes(x = Kategorie, y = Häufigkeit, fill = Gruppe)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
  geom_text(aes(label = Häufigkeit), vjust = -0.5, position = position_dodge(width = 0.9), size = 3) +
  labs(x = "", y = "frequency", title = "emotional domain") +
  scale_fill_manual(values = zwei_palette, name = "percieved insecurity", labels = c("restricted", "unrestricted")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Filter für spezifische Krankheitsfaktoren 5
krankheitsfaktoren5 <- c("everyday_problems", "daily_routine", "participation_in_road_traffic")
häufigkeitstabelle_dis5 <- subset(häufigkeitstabelle, Kategorie %in% krankheitsfaktoren5)
# Namen der Kategorien ändern
häufigkeitstabelle_dis5$Kategorie <- factor(häufigkeitstabelle_dis5$Kategorie, levels = krankheitsfaktoren5,
                                            labels = c("Everyday Problems", "Disruption of Daily Routines", "Road Traffic"))
# Gestapeltes Säulendiagramm
ggplot(häufigkeitstabelle_dis5, aes(x = Kategorie, y = Häufigkeit, fill = Gruppe)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
  geom_text(aes(label = Häufigkeit), vjust = -0.5, position = position_dodge(width = 0.9), size = 3) +
  labs(x = "", y = "frequency", title = "social domain: daily life") +
  scale_fill_manual(values = zwei_palette, name = "percieved safety", labels = c("restricted", "unrestricted")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Filter für spezifische Krankheitsfaktoren 6
krankheitsfaktoren6 <- c("family_role", "conflicts_with_relatives", "overload_among_people", "pejorativ_looks_comments")
häufigkeitstabelle_dis6 <- subset(häufigkeitstabelle, Kategorie %in% krankheitsfaktoren6)
# Namen der Kategorien ändern
häufigkeitstabelle_dis6$Kategorie <- factor(häufigkeitstabelle_dis6$Kategorie, levels = krankheitsfaktoren6,
                                            labels = c("Family Role", "Conflicts with Relatives", "Overload Among People", "Stigmatisation"))
# Gestapeltes Säulendiagramm
ggplot(häufigkeitstabelle_dis6, aes(x = Kategorie, y = Häufigkeit, fill = Gruppe)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
  geom_text(aes(label = Häufigkeit), vjust = -0.5, position = position_dodge(width = 0.9), size = 3) +
  labs(x = "", y = "frequency", title = "social domain: roles and interactions") +
  scale_fill_manual(values = zwei_palette, name = "percieved safety", labels = c("restricted", "unrestricted")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Filter für spezifische Krankheitsfaktoren 7
krankheitsfaktoren7 <- c("victim_to_crime", "financial_worries")
häufigkeitstabelle_dis7 <- subset(häufigkeitstabelle, Kategorie %in% krankheitsfaktoren7)
# Namen der Kategorien ändern
häufigkeitstabelle_dis7$Kategorie <- factor(häufigkeitstabelle_dis7$Kategorie, levels = krankheitsfaktoren7,
                                            labels = c("Victim of Crime", "Financial Worries"))
# Gestapeltes Säulendiagramm
ggplot(häufigkeitstabelle_dis7, aes(x = Kategorie, y = Häufigkeit, fill = Gruppe)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
  geom_text(aes(label = Häufigkeit), vjust = -0.5, position = position_dodge(width = 0.9), size = 3) +
  labs(x = "", y = "frequency", title = "social domain: percieved risks") +
  scale_fill_manual(values = zwei_palette, name = "percieved safety", labels = c("restricted", "unrestricted")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Filter für spezifische Krankheitsfaktoren 8
krankheitsfaktoren8 <- c("access_healthcare", "nursing_care")
häufigkeitstabelle_dis8 <- subset(häufigkeitstabelle, Kategorie %in% krankheitsfaktoren8)
# Namen der Kategorien ändern
häufigkeitstabelle_dis8$Kategorie <- factor(häufigkeitstabelle_dis8$Kategorie, levels = krankheitsfaktoren8,
                                            labels = c("Access to Healthcare", "Nursing Care"))
# Gestapeltes Säulendiagramm
ggplot(häufigkeitstabelle_dis8, aes(x = Kategorie, y = Häufigkeit, fill = Gruppe)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
  geom_text(aes(label = Häufigkeit), vjust = -0.5, position = position_dodge(width = 0.9), size = 3) +
  labs(x = "", y = "frequency", title = "healthcare domain: access") +
  scale_fill_manual(values = zwei_palette, name = "percieved safety", labels = c("restricted", "unrestricted")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Filter für spezifische Krankheitsfaktoren 9
krankheitsfaktoren9 <- c("lack_of_information", "communication_with_me", "communication_between_professionals")
häufigkeitstabelle_dis9 <- subset(häufigkeitstabelle, Kategorie %in% krankheitsfaktoren9)
# Namen der Kategorien ändern
häufigkeitstabelle_dis9$Kategorie <- factor(häufigkeitstabelle_dis9$Kategorie, levels = krankheitsfaktoren9,
                                            labels = c("Lack of Information", "Communication with Patient", "Communication Between Professionals"))
# Gestapeltes Säulendiagramm
ggplot(häufigkeitstabelle_dis9, aes(x = Kategorie, y = Häufigkeit, fill = Gruppe)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
  geom_text(aes(label = Häufigkeit), vjust = -0.5, position = position_dodge(width = 0.9), size = 3) +
  labs(x = "", y = "frequency", title = "healthcare domain: communication") +
  scale_fill_manual(values = zwei_palette, name = "percieved safety", labels = c("restricted", "unrestricted")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Filter für Overall Situation
häufigkeitstabelle_overall <- subset(häufigkeitstabelle, Kategorie == "overall_situation")
# Gestapeltes Säulendiagramm
ggplot(häufigkeitstabelle_overall, aes(x = "Overall Situation", y = Häufigkeit, fill = Gruppe)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
  geom_text(aes(label = Häufigkeit), vjust = -0.5, position = position_dodge(width = 0.9), size = 3) +
  labs(x = "", y = "frequency", title = "Overall Situation") +
  scale_fill_manual(values = zwei_palette, name = "percieved safety", labels = c("restricted", "unrestricted")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))




