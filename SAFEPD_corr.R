
# Korrelationsanalyse: SozDem ~ SozDem
correlation_Soz <- cor(na.omit(df_safepd[, 2:11])) 
corrplot(correlation_Soz , method = "color", addrect = 2)

# Korrelationsanalyse: df_safepdQA ~ overall_situation
correlation_matrix <- cor(na.omit(df_safepd[, 12:38]))
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
correlation_FIMA <- cor(na.omit(df_safepd[, columns_FIMA]))
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
#  analyse_FIMA(overall_situation_Group ~ FIMA_1_Hausarzt, df_safepd, "FIMA_1_Hausarzt", "overall_situation_Group"),
#  analyse_FIMA(overall_situation_Group ~ FIMA_1_Neurologe, df_safepd, "FIMA_1_Neurologe", "overall_situation_Group"),
#  analyse_FIMA(overall_situation_Group ~ FIMA_1_Psychiater, df_safepd, "FIMA_1_Psychiater", "overall_situation_Group"),
#  analyse_FIMA(overall_situation_Group ~ FIMA_1_Internist_FA, df_safepd, "FIMA_1_Internist_FA", "overall_situation_Group"),
#  analyse_FIMA(overall_situation_Group ~ FIMA_1_Urologe, df_safepd, "FIMA_1_Urologe", "overall_situation_Group"),
#  analyse_FIMA(overall_situation_Group ~ FIMA_1_Gynaekologe, df_safepd, "FIMA_1_Gynaekologe", "overall_situation_Group"),
#  analyse_FIMA(overall_situation_Group ~ FIMA_1_Orthopaede, df_safepd, "FIMA_1_Orthopaede", "overall_situation_Group"),
#  analyse_FIMA(overall_situation_Group ~ FIMA_1_Notfall_KH, df_safepd, "FIMA_1_Notfall_KH", "overall_situation_Group"),
#  analyse_FIMA(overall_situation_Group ~ FIMA_1_other, df_safepd, "FIMA_1_other", "overall_situation_Group"),
#  analyse_FIMA(overall_situation_Group ~ FIMA_1_Gesamt, df_safepd, "FIMA_1_Gesamt", "overall_situation_Group"),
#  analyse_FIMA(overall_situation_Group ~ FIMA_2_Krankengymnastik, df_safepd, "FIMA_2_Krankengymnastik", "overall_situation_Group"),
#  analyse_FIMA(overall_situation_Group ~ FIMA_2_Ergotherapie, df_safepd, "FIMA_2_Ergotherapie", "overall_situation_Group"),
#  analyse_FIMA(overall_situation_Group ~ FIMA_2_Sprachtherapie, df_safepd, "FIMA_2_Sprachtherapie", "overall_situation_Group"),
#  analyse_FIMA(overall_situation_Group ~ FIMA_2_Heilpraktiker, df_safepd, "FIMA_2_Heilpraktiker", "overall_situation_Group"),
#  analyse_FIMA(overall_situation_Group ~ FIMA_2_Osteopath, df_safepd, "FIMA_2_Osteopath", "overall_situation_Group"),
#  analyse_FIMA(overall_situation_Group ~ FIMA_2_Chiropraktiker, df_safepd, "FIMA_2_Chiropraktiker", "overall_situation_Group"),
#  analyse_FIMA(overall_situation_Group ~ FIMA_2_Psychotherapeut, df_safepd, "FIMA_2_Psychotherapeut", "overall_situation_Group"),
#  analyse_FIMA(overall_situation_Group ~ FIMA_16_Group, df_safepd, "FIMA_16_Group", "overall_situation_Group"),
#  analyse_FIMA(overall_situation_Group ~ FIMA_13_Mobilitaet, df_safepd, "FIMA_13_Mobilitaet", "overall_situation_Group"),
#  analyse_FIMA(overall_situation_Group ~ FIMA_13_Hygiene, df_safepd, "FIMA_13_Hygiene", "overall_situation_Group"),
#  analyse_FIMA(overall_situation_Group ~ FIMA_13_Tremor, df_safepd, "FIMA_13_Tremor", "overall_situation_Group"),
#  analyse_FIMA(overall_situation_Group ~ FIMA_13_Atmung, df_safepd, "FIMA_13_Atmung", "overall_situation_Group"),
#  analyse_FIMA(overall_situation_Group ~ FIMA_13_Hausnotruf, df_safepd, "FIMA_13_Hausnotruf", "overall_situation_Group"),
#  analyse_FIMA(overall_situation_Group ~ FIMA_13_Anzahl, df_safepd, "FIMA_13_Anzahl", "overall_situation_Group"))
# ergebnisse_FIMA <- do.call(rbind, FIMA_analysen)

cor(df_safepd$UPDRS_II_Score, df_safepd$UPDRS_I_Score, method = "spearman", use = "complete.obs")

ggplot(correlation_results, aes(x = reorder(Item, Correlation), y = Correlation)) +
  geom_bar(stat = "identity", fill = "blue") +
  coord_flip() +
  labs(title = "Korrelation der Items zur Overall Situation", x = "Items", y = "Korrelation") +
  theme_minimal()



