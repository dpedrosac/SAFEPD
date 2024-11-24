if (!dir.exists("results")) {
  dir.create("results")
}

## Hypothese: größeres Sicherheitsgefühl bei Männern in Population mit fortgeschrittenem Krebs / Parkinson (Milberg et al. / Akiyama et al. / Pedrosa et al.)
## Hypothese: negative Assoziation von Sicherheit & starker Symptomausprägung (Milberg et al. / Pedrosa et al.)

# Funktion für Vollständige Analyse aus Regression und Gütmaßen
perform_regression_analysis <- function(data, formula, covariates, reference_group = NULL, dichotomous_household = TRUE, export_path = "regression_analysis_results.xlsx") 
  {
  modell <- glm(formula, data = data, family = binomial())
  model_summary <- summary(modell)
  p_values <- model_summary$coefficients[, "Pr(>|z|)"]
  exp_coef <- exp(coef(modell))
  conf_int <- exp(confint(modell))
  results <- round(data.frame(OR = exp_coef, ci.lb = conf_int[, 1], ci.ub = conf_int[, 2], pval = p_values), 3)
  results_sig <- subset(results, pval < 0.05)
  observed_indices <- which(!is.na(data$overall_situation_Group))
  if (length(observed_indices) == 0) {
    stop("Keine gültigen Daten für die ROC-Kurve gefunden.")
  }
  observed_outcome <- data$overall_situation_Group[observed_indices]
  predicted_probabilities <- predict(modell, type = "response")[observed_indices]
  roc_curve <- pROC::roc(response = observed_outcome, predictor = predicted_probabilities)
  auc_val <- pROC::auc(roc_curve)
  plot(roc_curve, main = "ROC Curve", col = "blue", lwd = 2)
  legend("bottomright", legend = paste("AUC =", round(auc_val, 3)), col = "blue", lty = 1, cex = 0.8)
  vif_values <- round(car::vif(modell), 3)
  deviance <- modell$deviance
  df_residual <- modell$df.residual
  n <- nrow(data) 
  R2cs <- 1 - exp((modell$deviance - modell$null.deviance) / n)
  R2n <- R2cs / (1 - exp(-modell$null.deviance / n))
  result_list <- list(
    modell = modell,
    model_summary = model_summary,
    model_summary_table = results,
    significant_results_table = round(results_sig, 3),
    roc_auc = round(auc_val, 3),
    vif_values = round(vif_values, 3),
    deviance = round(deviance, 3),
    df_residual = round(df_residual, 3),
    goodness_of_fit = list(
      R2cs = round(R2cs, 3),
      R2n = round(R2n, 3)
    ),
    model_summary_df = as.data.frame(results),
    significant_results_df = as.data.frame(results_sig),
    roc_auc_df = data.frame(roc_auc = round(auc_val, 3)),
    vif_values_df = data.frame(vif_values = round(vif_values, 3))
  )
   write_xlsx(
    list(
      "Model Summary" = cbind(Variable = rownames(model_summary$coefficients), as.data.frame(model_summary$coefficients)),  
      "Results" = cbind(Variable = rownames(result_list$model_summary_df), result_list$model_summary_df),                   
      "Significant Results" = cbind(Variable = rownames(result_list$significant_results_df), result_list$significant_results_df),  
      "VIF Values" = cbind(Variable = names(vif_values), result_list$vif_values_df),                              
      "ROC AUC" = result_list$roc_auc_df,                                                         
      "Deviance" = data.frame(Deviance = result_list$deviance),                            
      "Residual DF" = data.frame(DF_Residual = result_list$df_residual),                       
      "Goodness of Fit" = data.frame(R2cs = result_list$goodness_of_fit$R2cs, 
                                     R2n = result_list$goodness_of_fit$R2n)                 
    ),
    path = export_path  # Ziel-Dateiname
)
  
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
  data = df_safepd,
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
    FIMA_1_Neurologe,
  covariates = NULL,  
  export_path = "results/regression_analysis_results.xlsx"
)


# Modell 2: manuelle Vortwärtsselektion aus allen Variablen
# Vorwärtsselektion: welche Variablen zeigen eine Korrelation zur Gesamtsituation (p < 0,2)
results <- data.frame(Variable = character(), Correlation = numeric(), P_Value = numeric(), stringsAsFactors = FALSE)
for (var in alle_variablen) {
  test_result <- cor.test(df_safepd$overall_situation_Group, df_safepd[[var]])
  results <- rbind(results, data.frame(
    Variable = var, 
    Correlation = test_result$estimate, 
    P_Value = test_result$p.value
  ))
}
significant_results <- results[results$P_Value < 0.2]
significant_vars <- significant_results$Variable[significant_results$P_Value < 0.2]
forward_analysis_result <- perform_regression_analysis(
  data = df_safepd,
  formula = paste("overall_situation_Group ~", paste(significant_vars, collapse = " + ")),
  covariates = NULL,  
  export_path = "results/forward_regression_analysis_results.xlsx"  
)

# Modell 3: zusätzliche Rückwärtsselektion nach Vorwärtsselektion (cortest_significant_results)
# gesättigtes Modell - Welche Variablen sollen in die Regression miteingehen?
forward_model <- glm(paste("overall_situation_Group ~", paste(significant_vars, collapse = " + ")),
    data = df_safepd,
    family = binomial, 
    na.action = na.exclude)
for_back_final_model <- step(forward_model, direction = "backward")
for_back_vars <- names(coef(for_back_final_model))[-1]
for_back_analysis_result <- perform_regression_analysis(
  data = df_safepd,
  formula = paste("overall_situation_Group ~", paste(for_back_vars, collapse = " + ")),
  covariates = NULL,  
  export_path = "results/for_back_regression_analysis_results.xlsx"  
)

# Modell 4: automatische Schrittweise Selektion 
aut_full_model <- glm(overall_situation_Group ~ ., data = df_safepd[, c("overall_situation_Group", alle_variablen)], family = binomial)
aut_step_model <- step(aut_full_model, direction = "both")
aut_step_vars <- names(coef(aut_step_model))[-1]
aut_analysis_result <- perform_regression_analysis(
  data = df_safepd,
  formula = paste("overall_situation_Group ~", paste(aut_step_vars, collapse = " + ")),
  covariates = NULL,  
  export_path = "results/aut_regression_analysis_results.xlsx"  
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





