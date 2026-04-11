# Vitamin D Deficiency and Quality of Life in EDS Patients - Summary Statistics

# 0. Load Package
install.packages("ggplot2")
install.packages("rlang")
library(ggplot2)
library(tidyr)
library(dplyr)
library(gt)

# 1. DATA CONSTRUCTION (AGGREGATE COUNTS)


# 1.1 Define the counts for each Quality of Life category
deficiency <- c(53, 160, 139, 94)
no_deficiency <- c(74, 232, 265, 216)

# 1.2 Calculate total cases per QoL category
total_cases <- deficiency + no_deficiency

# 1.3 Create labels
qol_labels <- c("Poor", "Fair", "Good", "Very Good-Excellent")

# 1.4 Bind into a 2x4 contingency matrix
data_matrix <- rbind(deficiency, no_deficiency)
colnames(data_matrix) <- qol_labels


# 2. PROPORTION & ASSOCIATION TESTS


#  2.1 --- Cochran-Armitage Test for Trend ---
# Tests for a linear trend in deficiency proportions across the ordinal QoL levels
trend_test <- prop.trend.test(x = deficiency, n = total_cases)
cat("\n--- Cochran-Armitage Test for Trend ---\n")
print(trend_test)

#   2.2 --- Chi-Square Test of Independence ---
# Tests for general association, treating categories as nominal
chi_sq_test <- chisq.test(data_matrix)
cat("\n--- Chi-Square Test of Independence ---\n")
print(chi_sq_test)



# 3. EXPANDING DATA TO LONG FORMAT 
## Reconstruct the original row-level data for modeling and rank tests

# 3.1 Create ordered factor for QoL
qol_factor <- factor(rep(qol_labels, times = total_cases), 
                     levels = qol_labels, 
                     ordered = TRUE)

# 3.2 Create corresponding Vitamin D status factor
status_vector <- c(
  rep("Deficiency", deficiency[1]), rep("No Deficiency", no_deficiency[1]),
  rep("Deficiency", deficiency[2]), rep("No Deficiency", no_deficiency[2]),
  rep("Deficiency", deficiency[3]), rep("No Deficiency", no_deficiency[3]),
  rep("Deficiency", deficiency[4]), rep("No Deficiency", no_deficiency[4])
)

df <- data.frame(QoL = qol_factor, VitaminD = factor(status_vector))


# 4. RANK-BASED & REGRESSION TESTS


# 4.1 --- Mann-Whitney U Test (Wilcoxon Rank Sum) ---
# Tests if the median QoL rank differs between the two Vitamin D groups
df$QoL_Rank <- as.numeric(df$QoL)
mwu_test <- wilcox.test(QoL_Rank ~ VitaminD, data = df)
cat("\n--- Mann-Whitney U Test ---\n")
print(mwu_test)

# 4.2 --- Ordinal Logistic Regression ---
# Requires the MASS package
if (!requireNamespace("MASS", quietly = TRUE)) install.packages("MASS")
library(MASS)

# 4.3 Fit the proportional odds logistic regression model
ord_model <- polr(QoL ~ VitaminD, data = df, Hess = TRUE)
cat("\n--- Ordinal Logistic Regression Summary ---\n")
print(summary(ord_model))

# 4.4 Calculate Odds Ratios
cat("\n--- Odds Ratios ---\n")
print(exp(coef(ord_model)))

# 4.5. Calculate 95% CI for the Odds Ratios
exp(confint(ord_model))
























