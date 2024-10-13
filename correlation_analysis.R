# Korrelationsanalyse: SozDem ~ SozDem
correlation_Soz <- cor(na.omit(SAFEPD[, 2:11])) 
corrplot(correlation_Soz , method = "color", addrect = 2)

# Korrelationsanalyse: SAFEPDQA ~ overall_situation
correlation_matrix <- cor(na.omit(SAFEPD[, 12:38]))
corrplot(correlation_matrix, method = "color", addrect = 2)

highest_correlation_matrix <- correlation_matrix
highest_correlation_matrix[highest_correlation_matrix <= 0.7] <- NA
corrplot(highest_correlation_matrix, method = "color", addrect = 2)

high_correlation_matrix <- correlation_matrix
high_correlation_matrix[high_correlation_matrix <= 0.5 | high_correlation_matrix >= 0.7] <- NA
corrplot(high_correlation_matrix, method = "color", addrect = 2)

medium_correlation_matrix <- correlation_matrix
medium_correlation_matrix[medium_correlation_matrix <= 0.3 | medium_correlation_matrix >= 0.5] <- NA
corrplot(medium_correlation_matrix, method = "color", addrect = 2)

low_correlation_matrix <- correlation_matrix
low_correlation_matrix[low_correlation_matrix <= 0 | low_correlation_matrix >= 0.3] <- NA
corrplot(low_correlation_matrix, method = "color", addrect = 2)

# Korrelationsanalyse: FIMA-Daten ~ overall_situation
columns_FIMA <- c(62:77, 148:152, 38)
correlation_FIMA <- cor(na.omit(SAFEPD[, columns_FIMA]))
corrplot(correlation_FIMA, method = "color", addrect = 2)

# Gruppenvergleich: FIMA-Daten ~ overall_situation
# analyse_FIMA <- function(formel, daten, variablen, gruppe) {
#  modell <- glm(formel, data = daten, family = binomial)
#  OR <- exp(coef(summary(modell))[, "Estimate"])
#  CI <- exp(confint(modell))
#  CI.lb <- CI[2, 1]
#  CI.ub <- CI[2, 2]
#  pval <- round(chisq.test(daten[[gruppe]], daten[[variablen]])$p.value, 3)
#  result <- data.frame(Variable = variablen, Gruppenvariable = gruppe, OR = OR, CI.lb = CI.lb, CI.ub = CI.ub, pval = pval)
#  return(result)}

# FIMA_analysen <- list(
#  analyse_FIMA(overall_situation_Group ~ FIMA_1_Hausarzt, SAFEPD, "FIMA_1_Hausarzt", "overall_situation_Group"),
#  analyse_FIMA(overall_situation_Group ~ FIMA_1_Neurologe, SAFEPD, "FIMA_1_Neurologe", "overall_situation_Group"),
#  analyse_FIMA(overall_situation_Group ~ FIMA_1_Psychiater, SAFEPD, "FIMA_1_Psychiater", "overall_situation_Group"),
#  analyse_FIMA(overall_situation_Group ~ FIMA_1_Internist_FA, SAFEPD, "FIMA_1_Internist_FA", "overall_situation_Group"),
#  analyse_FIMA(overall_situation_Group ~ FIMA_1_Urologe, SAFEPD, "FIMA_1_Urologe", "overall_situation_Group"),
#  analyse_FIMA(overall_situation_Group ~ FIMA_1_Gynaekologe, SAFEPD, "FIMA_1_Gynaekologe", "overall_situation_Group"),
#  analyse_FIMA(overall_situation_Group ~ FIMA_1_Orthopaede, SAFEPD, "FIMA_1_Orthopaede", "overall_situation_Group"),
#  analyse_FIMA(overall_situation_Group ~ FIMA_1_Notfall_KH, SAFEPD, "FIMA_1_Notfall_KH", "overall_situation_Group"),
#  analyse_FIMA(overall_situation_Group ~ FIMA_1_other, SAFEPD, "FIMA_1_other", "overall_situation_Group"),
#  analyse_FIMA(overall_situation_Group ~ FIMA_1_Gesamt, SAFEPD, "FIMA_1_Gesamt", "overall_situation_Group"),
#  analyse_FIMA(overall_situation_Group ~ FIMA_2_Krankengymnastik, SAFEPD, "FIMA_2_Krankengymnastik", "overall_situation_Group"),
#  analyse_FIMA(overall_situation_Group ~ FIMA_2_Ergotherapie, SAFEPD, "FIMA_2_Ergotherapie", "overall_situation_Group"),
#  analyse_FIMA(overall_situation_Group ~ FIMA_2_Sprachtherapie, SAFEPD, "FIMA_2_Sprachtherapie", "overall_situation_Group"),
#  analyse_FIMA(overall_situation_Group ~ FIMA_2_Heilpraktiker, SAFEPD, "FIMA_2_Heilpraktiker", "overall_situation_Group"),
#  analyse_FIMA(overall_situation_Group ~ FIMA_2_Osteopath, SAFEPD, "FIMA_2_Osteopath", "overall_situation_Group"),
#  analyse_FIMA(overall_situation_Group ~ FIMA_2_Chiropraktiker, SAFEPD, "FIMA_2_Chiropraktiker", "overall_situation_Group"),
#  analyse_FIMA(overall_situation_Group ~ FIMA_2_Psychotherapeut, SAFEPD, "FIMA_2_Psychotherapeut", "overall_situation_Group"),
#  analyse_FIMA(overall_situation_Group ~ FIMA_16_Group, SAFEPD, "FIMA_16_Group", "overall_situation_Group"),
#  analyse_FIMA(overall_situation_Group ~ FIMA_13_Mobilitaet, SAFEPD, "FIMA_13_Mobilitaet", "overall_situation_Group"),
#  analyse_FIMA(overall_situation_Group ~ FIMA_13_Hygiene, SAFEPD, "FIMA_13_Hygiene", "overall_situation_Group"),
#  analyse_FIMA(overall_situation_Group ~ FIMA_13_Tremor, SAFEPD, "FIMA_13_Tremor", "overall_situation_Group"),
#  analyse_FIMA(overall_situation_Group ~ FIMA_13_Atmung, SAFEPD, "FIMA_13_Atmung", "overall_situation_Group"),
#  analyse_FIMA(overall_situation_Group ~ FIMA_13_Hausnotruf, SAFEPD, "FIMA_13_Hausnotruf", "overall_situation_Group"),
#  analyse_FIMA(overall_situation_Group ~ FIMA_13_Anzahl, SAFEPD, "FIMA_13_Anzahl", "overall_situation_Group"))
# ergebnisse_FIMA <- do.call(rbind, FIMA_analysen)
