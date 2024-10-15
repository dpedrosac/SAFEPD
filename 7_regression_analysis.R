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
  write_xlsx(
    list(
      "Model Summary" = cbind(Variable = rownames(model_summary$coefficients), as.data.frame(model_summary$coefficients)),  # Modellzusammenfassung
      "Results" = cbind(Variable = rownames(result_list$model_summary_df), result_list$model_summary_df),                   # Ergebnisse der Modellkoeffizienten
      "Significant Results" = cbind(Variable = rownames(result_list$significant_results_df), result_list$significant_results_df),  # Signifikante Ergebnisse
      "VIF Values" = cbind(Variable = names(vif_values), result_list$vif_values_df),                              # VIF Werte
      "ROC AUC" = result_list$roc_auc_df,                                                            # ROC AUC
      "Deviance" = data.frame(Deviance = result_list$deviance),                                 # Deviance
      "Residual DF" = data.frame(DF_Residual = result_list$df_residual),                         # Residual Degrees of Freedom
      "Omnibus Test" = data.frame(Chi_Square = result_list$omnibus_test$chi_square, 
                                  DF = result_list$omnibus_test$df, 
                                  P_Value = result_list$omnibus_test$p_value),                  # Omnibus-Test
      "Goodness of Fit" = data.frame(R2cs = result_list$goodness_of_fit$R2cs, 
                                     R2n = result_list$goodness_of_fit$R2n)                  # Güte der Anpassung
    ),
    path = export_path  # Ziel-Dateiname
  )
  
  return(result_list)
}


# Hypothese: größeres Sicherheitsgefühl bei Männern in Population mit fortgeschrittenem Krebs / Parkinson (Milberg et al. / Akiyama et al. / Pedrosa et al.)
# Hypothese: negative Assoziation von Sicherheit & starker Symptomausprägung (Milberg et al. / Pedrosa et al.)
# Analyse
analysis_result <- perform_regression_analysis(
  data = SAFEPD,
  formula = overall_situation_Group ~ age +
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
    FIMA_1_Neurologe,
  covariates = NULL,  
  export_path = "regression_analysis_results.xlsx"  
)


# Alternatives Modell
alternative_analysis <- perform_regression_analysis(
  data = SAFEPD,
  formula = overall_situation_Group ~ 
    age + 
    #years_since_diagnosis + 
    gender_Group + 
    #nationality_Group + 
    #martial_status_Group + 
    #school_graduation_Group + 
    #persons_houshold_Group + 
    #professional_graduation_Group + 
    #employment_status_Group + 
    UPDRS_I_Score +
    UPDRS_II_Score + 
    FIMA_1_Hausarzt + 
    FIMA_1_Neurologe,
  covariates = NULL,  
  export_path = "alternative_regression_analysis_results.xlsx"  
)



# Berechnung der Likelihood-Ratio-Statistik zwischen dem Modell und dem Nullmodell
calculate_lrt <- function(modell) {
  deviance_model <- modell$deviance
  deviance_null <- modell$null.deviance
  
  LRT <- deviance_null - deviance_model
  
  df_LRT <- modell$df.null - modell$df.residual
  
  p_value <- pchisq(LRT, df_LRT, lower.tail = FALSE)
  
  result <- list(
    LRT = LRT,
    df = df_LRT,
    p_value = p_value
  )
  
  return(result)
}

# Anwendung auf das Modell
lrt_result1 <- calculate_lrt(analysis_result$modell)
lrt_result2 <- calculate_lrt(alternative_analysis$modell)

# Vergleich zweier Modelle
compare_models <- function(model1, model2) {
  # Berechnung der Differenz der Deviances
  LRT <- abs(model1$deviance - model2$deviance)  # Verwende den absoluten Unterschied
  
  # Berechnung der Differenz der Freiheitsgrade
  df_LRT <- abs(model1$df.residual - model2$df.residual)  # Verwende den absoluten Unterschied
  
  # Berechnung des p-Werts
  p_value <- pchisq(LRT, df_LRT, lower.tail = FALSE)
  
  # Ergebnisse zurückgeben
  result <- list(
    LRT = LRT,
    df = df_LRT,
    p_value = p_value
  )
  
  return(result)
}

# Anwendung 
comparison_result <- compare_models(analysis_result$modell, alternative_analysis$modell)




