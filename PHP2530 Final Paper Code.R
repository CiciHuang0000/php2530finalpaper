library("readxl")
library("rstan")
library(dplyr)
library(bayesplot)
library(pROC)

# Read in dataset
survey <- read_excel("survey.xlsx")

# Remove NA, ID and risk factor columns
survey <- survey[,c(-1,-2,-6)]

# Removing the single-valued columns 
survey <- survey[, !(names(survey) %in% c("know cancer", "alcohol"))]

# Factor columns
survey <- data.frame(lapply(survey, factor))

# Eliminate NA values
survey <- na.omit(survey)

# Create the model matrix 
# Add outcome variable as 1 or 0
outcome <- ifelse(survey$`heard.about.cancer.of.cervix` == "Yes", 1, 0)
encoded_df <- model.matrix(~ . -1, data = survey)  

# Convert the matrix to a data frame
encoded_df <- as.data.frame(encoded_df)

# Put  'outcome' as the first column instead of the last
encoded_df <- cbind(outcome = outcome, encoded_df)
encoded_df <- encoded_df[,-4]

# Load the Stan model
stan_file <- "2530 final stan model.stan"
sm <- stan_model(stan_file)

# Prepare data list for Stan
stan_data <- list(N = nrow(encoded_df), Y = encoded_df$outcome, K = ncol(encoded_df) - 1, X = as.matrix(encoded_df[, -1]))

# Run the model
samples <- sampling(sm, data = stan_data, chains = 4, iter = 4000, warmup = 2000, 
                    control = list(adapt_delta = 0.99, max_treedepth = 15))

# Extract from samples
posterior_samples <- extract(samples)

# Extract the predicted probabilities
y_pred <- posterior_samples$y_pred

# Calculate the mean predicted probability for each observation across all posterior samples
predicted_probabilities <- apply(y_pred, 2, mean)

# Actual binary outcome variable
actual_outcomes <- encoded_df$outcome

# Compute ROC curve and AUC
roc_curve <- roc(actual_outcomes, predicted_probabilities)
auc_value <- auc(roc_curve)

# Plot ROC curve with AUC value
plot(roc_curve, main = "ROC Curve")
text(0.8, 0.2, paste("AUC =", round(auc_value, 4)), pos = 4)

# Create summary statistics of R-hat values
summary_stats <- summary(samples)$summary
rhat_values <- summary_stats[, "Rhat"]
rhat_summary <- summary(rhat_values)
rhat_summary <- data.frame(Min=0.9996,"First Quantile"=0.9998, Median=1.0000, Mean=1.0001, "Third Quantile"=1.0002, Max="1.0096")
# Kable output
 kable(rhat_summary, caption = "Summary Statistics of r-hat values", booktabs=T) %>%
   kable_styling(latex_options = "HOLD_position")

# Extract the regression coefficients
beta_samples <- posterior_samples$beta
# Calculate the mean, median, and credible intervals for each coefficient
beta_summary <- apply(beta_samples, 2, function(x) {
  round(c(mean = mean(x), median = median(x), 
    sd = sd(x), 
    lower_95 = quantile(x, 0.025), upper_95 = quantile(x, 0.975)),4)
})

# Convert to a data frame 
beta_summary_df <- as.data.frame(t(beta_summary))

# To identify the most significant predictors, sort by the absolute value of the mean
beta_summary_df <- beta_summary_df[order(abs(beta_summary_df$mean), decreasing = TRUE), ]
print(beta_summary_df)

# Extract the column names from encoded_df
column_names <- colnames(encoded_df)

# Map the indices to column names (excluding the outcome column)
mapped_columns <- data.frame(
  index = 1:(ncol(encoded_df) - 1),
  column_name = column_names[-1]
)

# Display the mapping for the indices from the summary table
summary_indices <- c(9, 71, 52, 70, 21, 3, 8, 10, 84, 1)
mapped_columns[summary_indices, ]

# Extract the first 10 variables and assign corresponding column name
df_present <- beta_summary_df[1:10,]
rownames(df_present) <- c("marital.statusSingle", "health.facilities.in.your.areaPoor",
                               "smokingYes", "blood.in.vomit..urine..stoolYes",
                               "age.first.periodAbsence of menstrual cycle","age≥ 50","marital.statusSeparated", "marital.statusWidowed",
                          "think.cervical.cancer.preventable.diagnosed.early.Yes",
                          "cancer.can.affect.any.organNo"
                          )
# kable output
kable(df_present, caption = "Summary Statistics of r-hat values", booktabs=T) %>%
   kable_styling(latex_options = "HOLD_position")



