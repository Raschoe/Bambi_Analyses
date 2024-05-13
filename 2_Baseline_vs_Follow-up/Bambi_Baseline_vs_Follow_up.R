###################################################
#Baseline vs. Follow-up
#Bambi
#Raphaela Schöpfer
###################################################


# Load libraries
library(tidyverse)
library(readxl)
library(ggplot2)
library(car)
library(stats)
library(effsize) 
library(dplyr)
library(forcats)

# Load the data
data <- read_excel("C:/Users/rapha/Documents/FPS/2_Baseline_vs_Follow-up/Both_Con_values_table.xlsx")

################################################################################

# Adjust condition names 

data$Condition <- fct_recode(data$Condition,
                             Easy = "Leicht",
                             Difficult = "Schwer",
                             `Whole Task` = "Schwer_Leicht_Combined",
                             Rest = "Rest")
################################################################################
#Outliers
################################################################################

# Function to detect and impute outliers within each group of each condition
impute_outliers <- function(data, variables) {
  for (condition in unique(data$Condition)) {
    for (variable in variables) {
      condition_indices <- which(data$Condition == condition)
      values <- data[[variable]][condition_indices]
      q1 <- quantile(values, 0.25)
      q3 <- quantile(values, 0.75)
      iqr <- q3 - q1
      outliers_logical <- (values < (q1 - 1.5 * iqr)) | (values > (q3 + 1.5 * iqr))
      
      # Calculate mean without outliers
      mean_without_outliers <- mean(values[!outliers_logical], na.rm = TRUE)
      
      # Replace outliers with mean without outliers
      data[[variable]][condition_indices[outliers_logical]] <- mean_without_outliers
    }
  }
  return(data)
}

# Apply outlier detection and imputation
variables <- c("A_MeanConValue", "B_MeanConValue")
data_cleaned <- impute_outliers(data, variables)

################################################################################
#Checking assumptions
#################################################################################

# Reshape the data for the assumption checks
data_long <- data_cleaned %>%
  pivot_longer(cols = variables, names_to = "Group", values_to = "Value")

# Check assumptions for t-test
check_assumptions <- function(data_long) {
  conditions <- unique(data_long$Condition)
  
  for (condition in conditions) {
    cat("Checking assumptions for", condition, "condition:\n")
    
    # Subset data for the condition
    subset_data <- filter(data_long, Condition == condition)
    
    # Shapiro-Wilk normality test for each group
    groups <- unique(subset_data$Group)
    for (group in groups) {
      shapiro_result <- shapiro.test(subset_data$Value[subset_data$Group == group])
      cat("Shapiro-Wilk test for", group, ":", shapiro_result$p.value, "\n")
    }
    
    # Levene's test for homogeneity of variances
    levene_result <- leveneTest(Value ~ Group, data = subset_data)
    cat("Levene's test for equality of variances:", levene_result$'Pr(>F)'[1], "\n\n")
  }
}

# Perform assumption checks
check_assumptions(data_long)


################################################################################
#Analysis
################################################################################

# Perform t-tests within each condition and calculate Cohen's d
perform_t_tests_within_conditions <- function(data_cleaned) {
  conditions <- unique(data_cleaned$Condition)
  results <- data.frame(Condition = character(), P_Value = numeric(), Cohens_D = numeric())
  
  for (condition in conditions) {
    a_values <- data_cleaned %>% filter(Condition == condition) %>% pull("A_MeanConValue")
    b_values <- data_cleaned %>% filter(Condition == condition) %>% pull("B_MeanConValue")
    test_result <- t.test(a_values, b_values, paired = TRUE)
    cohens_d <- cohen.d(a_values, b_values)
    
    # Extract the Cohen's d value
    cohens_d_value <- cohens_d$estimate
    
    # Collect results
    results <- rbind(results, data.frame(Condition = condition, P_Value = test_result$p.value, Cohens_D = cohens_d_value))
  }
  

  
  return(results)
}


# Perform t-tests, calculate Cohen's d, and collect results
results <- perform_t_tests_within_conditions(data_cleaned)

# Print results
print(results)

################################################################################
#Plots
################################################################################

#Boxplot

data_long_cleaned <- data_cleaned %>%
  pivot_longer(cols = variables, names_to = "Variable", values_to = "Value") %>%
  mutate(Variable = case_when(
    Variable == "A_MeanConValue" ~ "Baseline",
    Variable == "B_MeanConValue" ~ "Follow-up",
    TRUE ~ Variable # 
  ))


ggplot(data_long_cleaned, aes(x = Condition, y = Value, fill = Variable)) +
  geom_boxplot() +
  scale_fill_manual(values = c("Baseline" = "#fbb4ae", "Follow-up" = "#b3cde3"), name = "Time") + # Pastel colors
  labs(title = "Perirhinal Cortex Activity: Baseline vs. Follow-up", y = "Mean Contrast Value", x = "") +
  theme_minimal() +
  scale_x_discrete(limits = c("Rest", "Easy", "Difficult", "Whole Task")) 


########Violin


data_long_cleaned <- data_cleaned %>%
  pivot_longer(cols = variables, names_to = "Variable", values_to = "Value") %>%
  mutate(Variable = case_when(
    Variable == "A_MeanConValue" ~ "Baseline",
    Variable == "B_MeanConValue" ~ "Follow-up",
    TRUE ~ Variable 
  ))

ggplot(data_long_cleaned, aes(x = Condition, y = Value, fill = Variable)) +
  geom_violin(trim = FALSE, alpha = 0.4, position = position_dodge(width = 0.8)) +
  geom_boxplot(width = 0.1, position = position_dodge(width = 0.8), outlier.shape = NA, alpha = 0.5) +
  scale_fill_manual(values = c("Baseline" = "#fbb4ae", "Follow-up" = "#b3cde3"), name = "Time") +
  labs(title = "Perirhinal Cortex Activity: Baseline vs. Follow-up", y = "Mean Contrast Value", x = "") +
  theme_minimal() +
  scale_x_discrete(limits = c("Rest", "Easy", "Difficult", "Whole Task")) +
  theme(legend.position = "bottom")

################################################################################



