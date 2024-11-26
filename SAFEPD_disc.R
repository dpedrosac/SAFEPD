# Funktion zur Berechnung der Verteilungstabelle
calculate_distribution <- function(data, variable, categories) {
  abs_häufigkeiten <- table(data[[variable]])
  prozentuale_verteilung <- prop.table(abs_häufigkeiten) 
  verteilungstabelle <- data.frame(categories = names(prozentuale_verteilung), Prozent = prozentuale_verteilung)
  return(verteilungstabelle)
}

# Funktion zur Skalierung der Vergleichsdaten auf 100%
scale_comparison_data <- function(comparison_data) {
  sum_percentage <- sum(as.numeric(comparison_data$percentage))
  if (sum_percentage != 1) {
    comparison_data$percentage <- comparison_data$percentage / sum_percentage
  }
  return(comparison_data)
}

# Funktion zur Zusammenfassung von Kategorien
summarize_categories <- function(data, categories_to_summarize) {
  data$categories[data$categories %in% categories_to_summarize] <- "summarized_category"
  data <- aggregate(Prozent.Freq ~ categories, data = data, sum)
  return(data)
}

# Anwendung für Schulabschluss
verteilungstabelle <- calculate_distribution(df_safepd, "school_graduation", "Schulabschluss")
Vergleich_Schule_no_na <- Vergleich_Schule %>% 
  filter(Schulabschluss != "NA")
Vergleich_Schule_no_na <- scale_comparison_data(Vergleich_Schule_no_na)
chisq.test(x = verteilungstabelle$Prozent.Freq, p = Vergleich_Schule_no_na$percentage)

# Anwendung für Berufsabschluss
verteilungstabelleB <- calculate_distribution(df_safepd, "professional_graduation", "Berufsabschluss")
verteilungstabelleB <- summarize_categories(verteilungstabelleB, c("3", "4"))
Vergleich_Beruf_no_na <- Vergleich_Beruf %>%
  filter(!is.na(Berufsabschluss) & !is.na(as.numeric(percentage)))
Vergleich_Beruf_no_na <- scale_comparison_data(Vergleich_Beruf_no_na)
Vergleich_Beruf_no_na$percentage <- as.numeric(Vergleich_Beruf_no_na$percentage)
chisq.test(x = verteilungstabelleB$Prozent.Freq, p = Vergleich_Beruf_no_na$percentage)

# Anwendung für Familienstand
verteilungstabelleM <- calculate_distribution(df_safepd, "martial_status", "Familienstand")
verteilungstabelleM <- summarize_categories(verteilungstabelleM, c("1", "4"))
Vergleich_Familie_no_na <- Vergleich_Familie %>%
  filter(Familienstand != "NA")
Vergleich_Familie_no_na <- scale_comparison_data(Vergleich_Familie_no_na)
Vergleich_Familie_no_na$Familienstand[Vergleich_Familie_no_na$Familienstand %in% c("2", "5")] <- "2-5"
Vergleich_Familie_no_na$percentage <- as.numeric(Vergleich_Familie_no_na$percentage)
Vergleich_Familie_no_na <- aggregate(percentage ~ Familienstand, data = Vergleich_Familie_no_na, sum)
Vergleich_Familie_no_na <- Vergleich_Familie_no_na[c(2, 3, 1, 4), ]
verteilungstabelleM <- verteilungstabelleM[c(1,4,2,3), ]
chisq.test(x = verteilungstabelleM$Prozent.Freq, p = Vergleich_Familie_no_na$percentage)

# Anwendung für Haushalt
verteilungstabelleH <- calculate_distribution(df_safepd, "persons_houshold", "Haushalt")
verteilungstabelleH$categories[verteilungstabelleH$categories >= 5] <- "5+"
verteilungstabelleH <- aggregate(Prozent.Freq ~ categories, data = verteilungstabelleH, sum)
Vergleich_Haushalt <- read_excel("~/df_safepd/df_safepd_imputeddata.xlsx", sheet = "Vergleichsdaten", range = "A26:B31")
Vergleich_Haushalt <- scale_comparison_data(Vergleich_Haushalt)
chisq.test(x = verteilungstabelleH$Prozent.Freq, p = Vergleich_Haushalt$percentage)

# Anwendung für Alter
verteilungstabelleA <- calculate_distribution(df_safepd, "age", "Alter")
verteilungstabelleA$age_group <- cut(as.numeric(verteilungstabelleA$categories), breaks = c(30, 40, 50, 60, 70, 80, 90, Inf), 
                                     labels = c("30-39", "40-49", "50-59", "60-69", "70-79", "80-89", ">89"),
                                     include.lowest = TRUE, right = FALSE)
verteilungstabelleA <- aggregate(Prozent.Freq ~ age_group, data = verteilungstabelleA, sum)
verteilungstabelleA <- rbind(verteilungstabelleA, data.frame(age_group = ">89", Prozent.Freq = 0))
Vergleich_Alter <- read_excel("~/df_safepd/df_safepd_imputeddata.xlsx", sheet = "Vergleichsdaten", range = "A33:B40")
Vergleich_Alter <- scale_comparison_data(Vergleich_Alter)
chisq.test(x = verteilungstabelleA$Prozent.Freq, p = Vergleich_Alter$percentage)
