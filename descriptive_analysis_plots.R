# Zuerst installieren und ggplot2 laden
install.packages("ggplot2")
library(ggplot2)

# Blaue Farbpalette mit 5 Abstufungen erstellen
blau_palette <- c("#99ccff","#cce5ff","#6699ff","#3366cc","#0047b3")
zwei_palette <- c("#FFDAB9","#cce5ff")

# Funktion zur Berechnung der Häufigkeiten für mehrere Variablen
berechne_alle_häufigkeiten <- function(daten, spalten_namen) {
  alle_häufigkeiten <- lapply(spalten_namen, function(spalte) {
    return(berechne_häufigkeit(daten[[spalte]]))
  })
  names(alle_häufigkeiten) <- spalten_namen
  return(alle_häufigkeiten)
}
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
  theme_minimal() 
  

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
  theme_minimal()

# Filter für spezifische Krankheitsfaktoren3
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
  theme_minimal()

# Filter für spezifische Krankheitsfaktoren
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
  theme_minimal()

# Filter für spezifische Krankheitsfaktoren
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
  theme_minimal()

# Filter für spezifische Krankheitsfaktoren
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
  theme_minimal()

# Filter für spezifische Krankheitsfaktoren
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
  theme_minimal()

# Filter für spezifische Krankheitsfaktoren
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
  theme_minimal()

# Filter für spezifische Krankheitsfaktoren
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
  theme_minimal()

# Filter für Overall Situation
häufigkeitstabelle_overall <- subset(häufigkeitstabelle, Kategorie == "overall_situation")
# Gestapeltes Säulendiagramm
ggplot(häufigkeitstabelle_overall, aes(x = "Overall Situation", y = Häufigkeit, fill = Gruppe)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
  geom_text(aes(label = Häufigkeit), vjust = -0.5, position = position_dodge(width = 0.9), size = 3) +
  labs(x = "", y = "frequency", title = "Overall Situation") +
  scale_fill_manual(values = zwei_palette, name = "percieved safety", labels = c("restricted", "unrestricted")) +
  theme_minimal()


