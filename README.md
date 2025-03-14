# Generate Subgroup Analysis Code and Execute
# Load necessary libraries
library(dplyr)

label_subgroup <- 'Subgroup: '
label_interaction_test <- 'Interaction test: '
label_pvalue <- 'p for interaction: '
label_effect_size <- 'Regression Coefficient:'
label_conf_int <- 'Confidence Interval:'

# Initialize lists to store results
results_list <- list()
interaction_results <- list()

# Analysis for subgroup: ragender=0
data_sub_ragender_0 <- data_frame %>% filter(ragender == '0')
model <- lm(frailty ~ One_cardiometabolic_disease, data = data_sub_ragender_0)
model_summary <- capture.output(summary(model))
effect_size <- coef(model)
conf_int <- confint.default(model)
results_list[['ragender=0']] <- list(
  summary = model_summary,
  effect_size = capture.output(effect_size),
  conf_int = capture.output(conf_int)
)

# Analysis for subgroup: ragender=1
data_sub_ragender_1 <- data_frame %>% filter(ragender == '1')
model <- lm(frailty ~ One_cardiometabolic_disease, data = data_sub_ragender_1)
model_summary <- capture.output(summary(model))
effect_size <- coef(model)
conf_int <- confint.default(model)
results_list[['ragender=1']] <- list(
  summary = model_summary,
  effect_size = capture.output(effect_size),
  conf_int = capture.output(conf_int)
)

# Interaction test for: ragender
interaction_pvalue <- NA
try({
  interaction_model <- lm(frailty ~ One_cardiometabolic_disease * ragender, data = data_frame)
  # 提取交互项名称
  interaction_terms <- grep(':', names(coef(interaction_model)), value = TRUE)
  num_interactions <- length(interaction_terms)
  if (num_interactions == 1) {
    summary_int <- summary(interaction_model)
    pv_int <- summary_int$coefficients[interaction_terms, 'Pr(>|t|)']
  } else if (num_interactions > 1) {
    anova_result <- anova(interaction_model)
    pv_int <- anova_result$`Pr(>F)`[length(anova_result$`Pr(>F)`)]
  }
  interaction_pvalue <- pv_int
}, silent = TRUE)

if(is.na(interaction_pvalue)) {
  interaction_results[['ragender']] <- 'Interaction test failed or p-value could not be calculated.'
} else {
  interaction_results[['ragender']] <- paste(label_pvalue, interaction_pvalue)
}

# Analysis for subgroup: agey=0
data_sub_agey_0 <- data_frame %>% filter(agey == '0')
model <- lm(frailty ~ One_cardiometabolic_disease, data = data_sub_agey_0)
model_summary <- capture.output(summary(model))
effect_size <- coef(model)
conf_int <- confint.default(model)
results_list[['agey=0']] <- list(
  summary = model_summary,
  effect_size = capture.output(effect_size),
  conf_int = capture.output(conf_int)
)

# Analysis for subgroup: agey=1
data_sub_agey_1 <- data_frame %>% filter(agey == '1')
model <- lm(frailty ~ One_cardiometabolic_disease, data = data_sub_agey_1)
model_summary <- capture.output(summary(model))
effect_size <- coef(model)
conf_int <- confint.default(model)
results_list[['agey=1']] <- list(
  summary = model_summary,
  effect_size = capture.output(effect_size),
  conf_int = capture.output(conf_int)
)

# Interaction test for: agey
interaction_pvalue <- NA
try({
  interaction_model <- lm(frailty ~ One_cardiometabolic_disease * agey, data = data_frame)
  # 提取交互项名称
  interaction_terms <- grep(':', names(coef(interaction_model)), value = TRUE)
  num_interactions <- length(interaction_terms)
  if (num_interactions == 1) {
    summary_int <- summary(interaction_model)
    pv_int <- summary_int$coefficients[interaction_terms, 'Pr(>|t|)']
  } else if (num_interactions > 1) {
    anova_result <- anova(interaction_model)
    pv_int <- anova_result$`Pr(>F)`[length(anova_result$`Pr(>F)`)]
  }
  interaction_pvalue <- pv_int
}, silent = TRUE)

if(is.na(interaction_pvalue)) {
  interaction_results[['agey']] <- 'Interaction test failed or p-value could not be calculated.'
} else {
  interaction_results[['agey']] <- paste(label_pvalue, interaction_pvalue)
}

# Analysis for subgroup: Current_drinking=0
data_sub_Current_drinking_0 <- data_frame %>% filter(Current_drinking == '0')
model <- lm(frailty ~ One_cardiometabolic_disease, data = data_sub_Current_drinking_0)
model_summary <- capture.output(summary(model))
effect_size <- coef(model)
conf_int <- confint.default(model)
results_list[['Current_drinking=0']] <- list(
  summary = model_summary,
  effect_size = capture.output(effect_size),
  conf_int = capture.output(conf_int)
)

# Analysis for subgroup: Current_drinking=1
data_sub_Current_drinking_1 <- data_frame %>% filter(Current_drinking == '1')
model <- lm(frailty ~ One_cardiometabolic_disease, data = data_sub_Current_drinking_1)
model_summary <- capture.output(summary(model))
effect_size <- coef(model)
conf_int <- confint.default(model)
results_list[['Current_drinking=1']] <- list(
  summary = model_summary,
  effect_size = capture.output(effect_size),
  conf_int = capture.output(conf_int)
)

# Interaction test for: Current_drinking
interaction_pvalue <- NA
try({
  interaction_model <- lm(frailty ~ One_cardiometabolic_disease * Current_drinking, data = data_frame)
  # 提取交互项名称
  interaction_terms <- grep(':', names(coef(interaction_model)), value = TRUE)
  num_interactions <- length(interaction_terms)
  if (num_interactions == 1) {
    summary_int <- summary(interaction_model)
    pv_int <- summary_int$coefficients[interaction_terms, 'Pr(>|t|)']
  } else if (num_interactions > 1) {
    anova_result <- anova(interaction_model)
    pv_int <- anova_result$`Pr(>F)`[length(anova_result$`Pr(>F)`)]
  }
  interaction_pvalue <- pv_int
}, silent = TRUE)

if(is.na(interaction_pvalue)) {
  interaction_results[['Current_drinking']] <- 'Interaction test failed or p-value could not be calculated.'
} else {
  interaction_results[['Current_drinking']] <- paste(label_pvalue, interaction_pvalue)
}

# Analysis for subgroup: Current_smoking=0
data_sub_Current_smoking_0 <- data_frame %>% filter(Current_smoking == '0')
model <- lm(frailty ~ One_cardiometabolic_disease, data = data_sub_Current_smoking_0)
model_summary <- capture.output(summary(model))
effect_size <- coef(model)
conf_int <- confint.default(model)
results_list[['Current_smoking=0']] <- list(
  summary = model_summary,
  effect_size = capture.output(effect_size),
  conf_int = capture.output(conf_int)
)

# Analysis for subgroup: Current_smoking=1
data_sub_Current_smoking_1 <- data_frame %>% filter(Current_smoking == '1')
model <- lm(frailty ~ One_cardiometabolic_disease, data = data_sub_Current_smoking_1)
model_summary <- capture.output(summary(model))
effect_size <- coef(model)
conf_int <- confint.default(model)
results_list[['Current_smoking=1']] <- list(
  summary = model_summary,
  effect_size = capture.output(effect_size),
  conf_int = capture.output(conf_int)
)

# Interaction test for: Current_smoking
interaction_pvalue <- NA
try({
  interaction_model <- lm(frailty ~ One_cardiometabolic_disease * Current_smoking, data = data_frame)
  # 提取交互项名称
  interaction_terms <- grep(':', names(coef(interaction_model)), value = TRUE)
  num_interactions <- length(interaction_terms)
  if (num_interactions == 1) {
    summary_int <- summary(interaction_model)
    pv_int <- summary_int$coefficients[interaction_terms, 'Pr(>|t|)']
  } else if (num_interactions > 1) {
    anova_result <- anova(interaction_model)
    pv_int <- anova_result$`Pr(>F)`[length(anova_result$`Pr(>F)`)]
  }
  interaction_pvalue <- pv_int
}, silent = TRUE)

if(is.na(interaction_pvalue)) {
  interaction_results[['Current_smoking']] <- 'Interaction test failed or p-value could not be calculated.'
} else {
  interaction_results[['Current_smoking']] <- paste(label_pvalue, interaction_pvalue)
}

# Analysis for subgroup: Physical_activity=1
data_sub_Physical_activity_1 <- data_frame %>% filter(Physical_activity == '1')
model <- lm(frailty ~ One_cardiometabolic_disease, data = data_sub_Physical_activity_1)
model_summary <- capture.output(summary(model))
effect_size <- coef(model)
conf_int <- confint.default(model)
results_list[['Physical_activity=1']] <- list(
  summary = model_summary,
  effect_size = capture.output(effect_size),
  conf_int = capture.output(conf_int)
)

# Analysis for subgroup: Physical_activity=0
data_sub_Physical_activity_0 <- data_frame %>% filter(Physical_activity == '0')
model <- lm(frailty ~ One_cardiometabolic_disease, data = data_sub_Physical_activity_0)
model_summary <- capture.output(summary(model))
effect_size <- coef(model)
conf_int <- confint.default(model)
results_list[['Physical_activity=0']] <- list(
  summary = model_summary,
  effect_size = capture.output(effect_size),
  conf_int = capture.output(conf_int)
)

# Interaction test for: Physical_activity
interaction_pvalue <- NA
try({
  interaction_model <- lm(frailty ~ One_cardiometabolic_disease * Physical_activity, data = data_frame)
  # 提取交互项名称
  interaction_terms <- grep(':', names(coef(interaction_model)), value = TRUE)
  num_interactions <- length(interaction_terms)
  if (num_interactions == 1) {
    summary_int <- summary(interaction_model)
    pv_int <- summary_int$coefficients[interaction_terms, 'Pr(>|t|)']
  } else if (num_interactions > 1) {
    anova_result <- anova(interaction_model)
    pv_int <- anova_result$`Pr(>F)`[length(anova_result$`Pr(>F)`)]
  }
  interaction_pvalue <- pv_int
}, silent = TRUE)

if(is.na(interaction_pvalue)) {
  interaction_results[['Physical_activity']] <- 'Interaction test failed or p-value could not be calculated.'
} else {
  interaction_results[['Physical_activity']] <- paste(label_pvalue, interaction_pvalue)
}

# Combine all subgroup summaries and interaction summaries
final_results <- ''
final_results <- paste(final_results, '---\n', label_subgroup, 'ragender=0\n')
final_results <- paste(final_results, paste(results_list[['ragender=0']]$summary, collapse = '\n'), '\n')
final_results <- paste(final_results, label_effect_size, '\n', paste(results_list[['ragender=0']]$effect_size, collapse = '\n'), '\n')
final_results <- paste(final_results, label_conf_int, '\n', paste(results_list[['ragender=0']]$conf_int, collapse = '\n'), '\n')
final_results <- paste(final_results, '---\n', label_subgroup, 'ragender=1\n')
final_results <- paste(final_results, paste(results_list[['ragender=1']]$summary, collapse = '\n'), '\n')
final_results <- paste(final_results, label_effect_size, '\n', paste(results_list[['ragender=1']]$effect_size, collapse = '\n'), '\n')
final_results <- paste(final_results, label_conf_int, '\n', paste(results_list[['ragender=1']]$conf_int, collapse = '\n'), '\n')
final_results <- paste(final_results, '---\n', label_interaction_test, 'ragender\n', interaction_results[['ragender']], '\n')
final_results <- paste(final_results, '---\n', label_subgroup, 'agey=0\n')
final_results <- paste(final_results, paste(results_list[['agey=0']]$summary, collapse = '\n'), '\n')
final_results <- paste(final_results, label_effect_size, '\n', paste(results_list[['agey=0']]$effect_size, collapse = '\n'), '\n')
final_results <- paste(final_results, label_conf_int, '\n', paste(results_list[['agey=0']]$conf_int, collapse = '\n'), '\n')
final_results <- paste(final_results, '---\n', label_subgroup, 'agey=1\n')
final_results <- paste(final_results, paste(results_list[['agey=1']]$summary, collapse = '\n'), '\n')
final_results <- paste(final_results, label_effect_size, '\n', paste(results_list[['agey=1']]$effect_size, collapse = '\n'), '\n')
final_results <- paste(final_results, label_conf_int, '\n', paste(results_list[['agey=1']]$conf_int, collapse = '\n'), '\n')
final_results <- paste(final_results, '---\n', label_interaction_test, 'agey\n', interaction_results[['agey']], '\n')
final_results <- paste(final_results, '---\n', label_subgroup, 'Current_drinking=0\n')
final_results <- paste(final_results, paste(results_list[['Current_drinking=0']]$summary, collapse = '\n'), '\n')
final_results <- paste(final_results, label_effect_size, '\n', paste(results_list[['Current_drinking=0']]$effect_size, collapse = '\n'), '\n')
final_results <- paste(final_results, label_conf_int, '\n', paste(results_list[['Current_drinking=0']]$conf_int, collapse = '\n'), '\n')
final_results <- paste(final_results, '---\n', label_subgroup, 'Current_drinking=1\n')
final_results <- paste(final_results, paste(results_list[['Current_drinking=1']]$summary, collapse = '\n'), '\n')
final_results <- paste(final_results, label_effect_size, '\n', paste(results_list[['Current_drinking=1']]$effect_size, collapse = '\n'), '\n')
final_results <- paste(final_results, label_conf_int, '\n', paste(results_list[['Current_drinking=1']]$conf_int, collapse = '\n'), '\n')
final_results <- paste(final_results, '---\n', label_interaction_test, 'Current_drinking\n', interaction_results[['Current_drinking']], '\n')
final_results <- paste(final_results, '---\n', label_subgroup, 'Current_smoking=0\n')
final_results <- paste(final_results, paste(results_list[['Current_smoking=0']]$summary, collapse = '\n'), '\n')
final_results <- paste(final_results, label_effect_size, '\n', paste(results_list[['Current_smoking=0']]$effect_size, collapse = '\n'), '\n')
final_results <- paste(final_results, label_conf_int, '\n', paste(results_list[['Current_smoking=0']]$conf_int, collapse = '\n'), '\n')
final_results <- paste(final_results, '---\n', label_subgroup, 'Current_smoking=1\n')
final_results <- paste(final_results, paste(results_list[['Current_smoking=1']]$summary, collapse = '\n'), '\n')
final_results <- paste(final_results, label_effect_size, '\n', paste(results_list[['Current_smoking=1']]$effect_size, collapse = '\n'), '\n')
final_results <- paste(final_results, label_conf_int, '\n', paste(results_list[['Current_smoking=1']]$conf_int, collapse = '\n'), '\n')
final_results <- paste(final_results, '---\n', label_interaction_test, 'Current_smoking\n', interaction_results[['Current_smoking']], '\n')
final_results <- paste(final_results, '---\n', label_subgroup, 'Physical_activity=1\n')
final_results <- paste(final_results, paste(results_list[['Physical_activity=1']]$summary, collapse = '\n'), '\n')
final_results <- paste(final_results, label_effect_size, '\n', paste(results_list[['Physical_activity=1']]$effect_size, collapse = '\n'), '\n')
final_results <- paste(final_results, label_conf_int, '\n', paste(results_list[['Physical_activity=1']]$conf_int, collapse = '\n'), '\n')
final_results <- paste(final_results, '---\n', label_subgroup, 'Physical_activity=0\n')
final_results <- paste(final_results, paste(results_list[['Physical_activity=0']]$summary, collapse = '\n'), '\n')
final_results <- paste(final_results, label_effect_size, '\n', paste(results_list[['Physical_activity=0']]$effect_size, collapse = '\n'), '\n')
final_results <- paste(final_results, label_conf_int, '\n', paste(results_list[['Physical_activity=0']]$conf_int, collapse = '\n'), '\n')
final_results <- paste(final_results, '---\n', label_interaction_test, 'Physical_activity\n', interaction_results[['Physical_activity']], '\n')
# Display the results
cat(final_results)
