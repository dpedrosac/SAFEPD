# Funktion
perform_regression_analysis <- function(data, formula, covariates, reference_group = NULL, dichotomous_household = TRUE) {
  modell <- glm(formula, data = data, family = binomial)
  summary(modell)
  # anova_result <- anova(modell, test = "Chisq")
  print(modell)
  p_values <- summary(modell)$coefficients[, "Pr(>|z|)"]
  exp_coef <- exp(coef(modell))
  conf_int <- exp(confint(modell))
  results <- data.frame(OR = exp_coef, ci.lb = conf_int[, 1], ci.ub = conf_int[, 2], pval = p_values)
  results_sig <- subset(results, pval < 0.05)
  
  observed_indices <- which(!is.na(data$overall_situation_Group))
  observed_outcome <- data$overall_situation_Group[observed_indices]
  predicted_probabilities <- predict(modell, type = "response")[observed_indices]
  roc_curve <- roc(response = observed_outcome, predictor = predicted_probabilities)
  auc_val <- auc(roc_curve)
  plot(roc_curve, main = "ROC Curve", col = "blue", lwd = 2)
  legend("bottomright", legend = paste("AUC =", round(auc_val, 3)), col = "blue", lty = 1, cex = 0.8)
  
  vif_values <- car::vif(modell)
  
  deviance <- modell$deviance
  df_residual <- modell$df.residual
  r_squared <- 1 - (modell$deviance / modell$null.deviance)
  n <- nrow(data)
  p <- length(coef(modell)) - 1  
  adjusted_r_squared <- 1 - (1 - r_squared) * ((n - 1) / (n - p - 1))
  
  result_list <- list(
    modell = modell,
    model_summary = summary(modell),
    # anova_result = anova_result,
    model_summary_table = results,
    significant_results_table = results_sig,
    roc_auc = auc_val,
    vif_values = vif_values,
    deviance = deviance,
    df_residual = df_residual,
    r_squared = r_squared,
    adjusted_r_squared = adjusted_r_squared
  )
  
  return(result_list)
}

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
    FIMA_1_Neurologe
)

analysis_result$model_summary <- round(analysis_result$model_summary_table, 3)
analysis_result$model_summary_table <- round(analysis_result$model_summary_table, 3)
analysis_result$significant_results_table <- round(analysis_result$significant_results_table, 3)
analysis_result$roc_auc <- round(analysis_result$roc_auc, 3)
analysis_result$vif_values <- round(analysis_result$vif_values, 3)

model_summary_df <- data.frame(analysis_result$model_summary_table)
significant_results_df <- data.frame(analysis_result$significant_results_table)
roc_auc_df <- data.frame(roc_auc = analysis_result$roc_auc)
vif_values_df <- data.frame(analysis_result$vif_values)
r_squared_df <- data.frame(analysis_result$r_squared)
adjusted_r_squared_df <- data.frame(analysis_result$adjusted_r_squared)

#write.xlsx(list(Model_Summary = model_summary_df, 
                #Significant_Results = significant_results_df,
                #ROC_AUC = roc_auc_df,
                #VIF_Values = vif_values_df, 
                #R = r_squared_df, 
                #adjR = adjusted_r_squared_df),
           #rowNames = TRUE, 
           #colNames = TRUE,
           #file = "analysis_results.xlsx")

