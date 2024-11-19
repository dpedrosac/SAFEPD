## Hypothese: größeres Sicherheitsgefühl bei Männern in Population mit fortgeschrittenem Krebs / Parkinson (Milberg et al. / Akiyama et al. / Pedrosa et al.)
## Hypothese: negative Assoziation von Sicherheit & starker Symptomausprägung (Milberg et al. / Pedrosa et al.)

# Funktion für Vollständige Analyse aus Regression und Gütmaßen
perform_regression_analysis <- function(data, formula, covariates, reference_group = NULL, dichotomous_household = TRUE, export_path = "regression_analysis_results.xlsx") {
  # Modell erstellen
  modell <- glm(formula, data = data, family = binomial())
  
  # Modellzusammenfassung
  model_summary <- summary(modell)
  
  # Berechnung der Modellkoeffizienten und des Konfidenzintervalls
  p_values <- model_summary$coefficients[, "Pr(>|z|)"]
  exp_coef <- exp(coef(modell))
  conf_int <- exp(confint(modell))
  results <- round(data.frame(OR = exp_coef, ci.lb = conf_int[, 1], ci.ub = conf_int[, 2], pval = p_values), 3)
  results_sig <- subset(results, pval < 0.05)
  
  # ROC-Kurve und AUC-Wert
  observed_indices <- which(!is.na(data$overall_situation_Group))
  observed_outcome <- data$overall_situation_Group[observed_indices]
  predicted_probabilities <- predict(modell, type = "response")[observed_indices]
  roc_curve <- roc(response = observed_outcome, predictor = predicted_probabilities)
  auc_val <- auc(roc_curve)
  
  # Plot ROC-Kurve
  plot(roc_curve, main = "ROC Curve", col = "blue", lwd = 2)
  legend("bottomright", legend = paste("AUC =", round(auc_val, 3)), col = "blue", lty = 1, cex = 0.8)
  
  # Variance Inflation Factors (VIF)
  vif_values <- round(car::vif(modell), 3)
  
  # Berechnung der Modellgüte
  deviance <- modell$deviance
  df_residual <- modell$df.residual
  n <- nrow(data)  # Anzahl der Beobachtungen
  R2cs <- 1 - exp((modell$deviance - modell$null.deviance) / n)
  R2n <- R2cs / (1 - exp(-modell$null.deviance / n))
  
  # Vergleich Varianzaufklärung zum Nullmodell
  modelchi <- modell$null.deviance - modell$deviance
  chidf <- modell$df.null - modell$df.residual
  chisqp <- 1 - pchisq(modelchi, chidf)
  
  # Ergebnisse in einer Liste zurückgeben
  result_list <- list(
    modell = modell,
    model_summary = model_summary,
    model_summary_table = results,
    significant_results_table = round(results_sig, 3),
    roc_auc = round(auc_val, 3),
    vif_values = round(vif_values, 3),
    deviance = round(deviance, 3),
    df_residual = round(df_residual, 3),
    omnibus_test = list(
      chi_square = round(modelchi, 3),
      df = chidf,
      p_value = round(chisqp, 3)
    ),
    goodness_of_fit = list(
      R2cs = round(R2cs, 3),
      R2n = round(R2n, 3)
    ),
    model_summary_df = as.data.frame(results),
    significant_results_df = as.data.frame(results_sig),
    roc_auc_df = data.frame(roc_auc = round(auc_val, 3)),
    vif_values_df = data.frame(vif_values = round(vif_values, 3))
  )
  
  # Exportiere die Ergebnisse in eine Excel-Datei
  #write_xlsx(
  #  list(
  #    "Model Summary" = cbind(Variable = rownames(model_summary$coefficients), as.data.frame(model_summary$coefficients)),  
  #    "Results" = cbind(Variable = rownames(result_list$model_summary_df), result_list$model_summary_df),                   
  #    "Significant Results" = cbind(Variable = rownames(result_list$significant_results_df), result_list$significant_results_df),  
  #    "VIF Values" = cbind(Variable = names(vif_values), result_list$vif_values_df),                              
  #    "ROC AUC" = result_list$roc_auc_df,                                                         
  #    "Deviance" = data.frame(Deviance = result_list$deviance),                            
  #    "Residual DF" = data.frame(DF_Residual = result_list$df_residual),                       
  #    "Omnibus Test" = data.frame(Chi_Square = result_list$omnibus_test$chi_square, 
  #                                DF = result_list$omnibus_test$df, 
  #                                P_Value = result_list$omnibus_test$p_value),                  
  #    "Goodness of Fit" = data.frame(R2cs = result_list$goodness_of_fit$R2cs, 
  #                                   R2n = result_list$goodness_of_fit$R2n)                 
  #  ),
  #  path = export_path  # Ziel-Dateiname
  #)
  
  return(result_list)
}


alle_variablen <- c("age", "years_since_diagnosis", "gender_Group", "nationality_Group", 
                    "martial_status_Group", "school_graduation_Group", "persons_houshold_Group", 
                    "professional_graduation_Group", "employment_status_Group", 
                    "UPDRS_I_Score", "UPDRS_II_Score", "FIMA_1_Hausarzt", "FIMA_1_Neurologe", 
                    "FIMA_2_Krankengymnastik", "FIMA_2_Ergotherapie", "FIMA_2_Sprachtherapie", 
                    "FIMA_3", "FIMA_4", "FIMA_5", "FIMA_6", "FIMA_7", "FIMA_8", "FIMA_9", 
                    "FIMA_10", "FIMA_11", "FIMA_12", "FIMA_13_Anzahl")

# Modell 1: Variablen wie zuerst besprochen
analysis_result <- perform_regression_analysis(
  data = SAFEPD,
  formula = overall_situation_Group ~
    age + 
    years_since_diagnosis + 
    gender_Group + 
    nationality_Group +
    martial_status_Group +
    school_graduation_Group +
    persons_houshold_Group +
    professional_graduation_Group +
    employment_status_Group +
    UPDRS_I_Score +
    UPDRS_II_Score +
    FIMA_1_Hausarzt +
    FIMA_1_Neurologe  
)


# Modell 2: manuelle Vortwärtsselektion aus allen Variablen
# Vorwärtsselektion: welche Variablen zeigen eine Korrelation zur Gesamtsituation (p < 0,2)
results <- data.frame(Variable = character(), Correlation = numeric(), P_Value = numeric(), stringsAsFactors = FALSE)
for (var in alle_variablen) {
  
  test_result <- cor.test(SAFEPD$overall_situation_Group, SAFEPD[[var]])
  
  alle_results <- rbind(alle_results, data.frame(
    Variable = var, 
    Correlation = test_result$estimate, 
    P_Value = test_result$p.value
  ))
}
cortest_significant_results <- subset(alle_results, P_Value < 0.25)
forward_analysis_result <- perform_regression_analysis(
  data = SAFEPD,
  formula = overall_situation_Group ~ 
    age +
    #years_since_diagnosis +
    gender_Group +
    #nationality_Group +
    martial_status_Group +
    #school_graduation_Group +
    persons_houshold_Group +
    #professional_graduation_Group +
    employment_status_Group +
    UPDRS_I_Score +
    UPDRS_II_Score +
    FIMA_1_Hausarzt +
    FIMA_1_Neurologe + 
    FIMA_2_Ergotherapie +
    FIMA_2_Sprachtherapie +
    FIMA_3 +
    FIMA_4 + 
    FIMA_5 + 
    FIMA_7 + 
    FIMA_8 +
    FIMA_11 +
    FIMA_13_Anzahl,
  covariates = NULL,  
  export_path = "regression_analysis_results.xlsx"  
)

# Modell 3: zusätzliche Rückwärtsselektion nach Vorwärtsselektion (cortest_significant_results)
# gesättigtes Modell - Welche Variablen sollen in die Regression miteingehen?
forward_model <- glm(SAFEPD$overall_situation_Group ~ 
                    #age + 
                    #years_since_diagnosis + 
                    gender_Group + 
                    #nationality_Group + 
                    martial_status_Group + 
                    #school_graduation_Group + 
                    persons_houshold_Group + 
                    #professional_graduation_Group + 
                    employment_status_Group + 
                    UPDRS_I_Score +
                    UPDRS_II_Score +
                    FIMA_1_Hausarzt + 
                    FIMA_1_Neurologe + 
                    #FIMA_2_Krankengymnastik + 
                    FIMA_2_Ergotherapie + 
                    #FIMA_2_Sprachtherapie +
                    FIMA_3 + 
                    FIMA_4 + 
                    FIMA_5 +
                    #FIMA_6 +
                    FIMA_7 +
                    FIMA_8 +
                    #FIMA_9 + 
                    #FIMA_10 + 
                    FIMA_11 + 
                    #FIMA_12 + 
                    FIMA_13_Anzahl,
                  data = SAFEPD, family = binomial, na.action = na.exclude)
for_back_final_model <- step(forward_model, direction = "backward")
for_back_analysis_result <- perform_regression_analysis(
  data = SAFEPD,
  formula = overall_situation_Group ~ 
    #age +
    #years_since_diagnosis +
    gender_Group +
    #nationality_Group +
    #martial_status_Group +
    #school_graduation_Group +
    #persons_houshold_Group +
    #professional_graduation_Group +
    #employment_status_Group +
    UPDRS_I_Score +
    #UPDRS_II_Score +
    FIMA_1_Hausarzt +
    FIMA_1_Neurologe + 
    #FIMA_2_Ergotherapie +
    #FIMA_2_Sprachtherapie +
    FIMA_3 +
    FIMA_13_Anzahl,
  covariates = NULL,  
  export_path = "regression_analysis_results.xlsx"  
)

# automatische Schrittweise Selektion und Analyse nach Funktion 
aut_full_model <- glm(overall_situation_Group ~ ., data = SAFEPD[, c("overall_situation_Group", alle_variablen)], family = binomial)
aut_step_model <- step(aut_full_model, direction = "both")
aut_analysis_result <- perform_regression_analysis(
  data = SAFEPD,
  formula = overall_situation_Group ~ 
    age +
    gender_Group +
    UPDRS_I_Score +
    FIMA_1_Hausarzt +
    FIMA_1_Neurologe + 
    FIMA_2_Ergotherapie +
    FIMA_2_Sprachtherapie +
    FIMA_3 + #nur 33 Personen, die einen ambulanten Pfelegediesnt hatten
    FIMA_6 + #nur 11 Personen, die in Tagespflege waren
    FIMA_11 + 
    FIMA_13_Anzahl,
  covariates = NULL,  
  export_path = "regression_analysis_results_alt3.xlsx"  
)



# Modellvergleich
compare_models <- function(model1, model2) {
  LRT <- abs(model1$deviance - model2$deviance)  
  df_LRT <- abs(model1$df.residual - model2$df.residual)  
  p_value <- pchisq(LRT, df_LRT, lower.tail = FALSE)
  result <- list(
    LRT = LRT,
    df = df_LRT,
    p_value = p_value
  )
  return(result)
}

# Anwendung Modellvergleich
comparison_result <- compare_models(analysis_result$modell, forward_analysis_result$modell) 
# Signifikant Bessere Erklärung der Daten durch Vorwärtsselektion

comparison_result2 <- compare_models(analysis_result$modell, for_back_analysis_result$modell)
# Keine Signifikant Bessere Erklärung der Daten durch kombinierte manuelle Vor- und Rückwärtsselektion

comparison_result3 <- compare_models(forward_analysis_result$modell, for_back_analysis_result$modell)
# Keine Signifikant Bessere Erklärung der Daten

comparison_result4 <- compare_models(analysis_result$modell, aut_analysis_result$modell)
# Signifikant Bessere Erklärung der Daten durch das Modell nach automatischer schrittweiser Selektion

comparison_result5 <- compare_models(forward_analysis_result$modell, aut_analysis_result$modell)
# Keine Signifikant Bessere Erklärung der Daten --> automatische schrittweise Selektion "besser", da einfacher




