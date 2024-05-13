################################################################################

#Bambi
#Additional Analysis
#Raphaela Schöpfer

#################################################################################

# Loading packages, loading dataset, handling outliers and NA's

library(tidyverse)
library(readxl)
library(ggpubr)
library(lsr)
library(car)
library(ggplot2)
library(caret)


# Load dataset
data <- read_excel("C:/Users/rapha/Documents/FPS/Bambi_Psychological_Data_2.xlsx")

# Data Cleaning: Convert "NA" strings to actual NA values and convert to numeric
data <- data %>%
  mutate(across(c(Perirhinal_Activity_Task_Baseline, Perirhinal_Cortex_Activity_Task_FU, OOO_correct_visit1, OOO_correct_visit2),
                ~na_if(as.character(.x), "NA"))) %>%
  mutate(across(everything(), as.numeric))

# Replace outliers with the mean of non-outliers
replace_outliers_with_mean <- function(x) {
  qnt <- quantile(x, probs=c(.25, .75), na.rm = TRUE)
  H <- 1.5 * IQR(x, na.rm = TRUE)
  outlier_bounds <- c(qnt[1] - H, qnt[2] + H)
  x[!(x >= outlier_bounds[1] & x <= outlier_bounds[2])] <- mean(x[!(x < outlier_bounds[1] | x > outlier_bounds[2])], na.rm = TRUE)
  return(x)
}

# Apply the outlier function to Perirhinal Cortex Activity Task Baseline and FU
data <- data %>%
  mutate(Perirhinal_Activity_Task_Baseline = replace_outliers_with_mean(Perirhinal_Activity_Task_Baseline),
         Perirhinal_Cortex_Activity_Task_FU = replace_outliers_with_mean(Perirhinal_Cortex_Activity_Task_FU))


####################################################################################################

# High vs low Activity group

####################################################################################################

# Define groups for Baseline
baseline_cutoff <- mean(data$Perirhinal_Activity_Task_Baseline, na.rm = TRUE)
data <- data %>%
  mutate(Baseline_Group = if_else(Perirhinal_Activity_Task_Baseline > baseline_cutoff, "High Activity", "Low Activity"))

# Perform t-test for Baseline groups on OOO correct scores for visit 1
t_test_OOO_baseline <- t.test(OOO_correct_visit1 ~ Baseline_Group, data = data, var.equal = TRUE)

# Define groups for Follow-Up
followup_cutoff <- mean(data$Perirhinal_Cortex_Activity_Task_FU, na.rm = TRUE)
data <- data %>%
  mutate(Followup_Group = if_else(Perirhinal_Cortex_Activity_Task_FU > followup_cutoff, "High Activity", "Low Activity"))

# Perform t-test for Follow-Up groups on OOO correct scores for visit 2
t_test_OOO_followup <- t.test(OOO_correct_visit2 ~ Followup_Group, data = data, var.equal = TRUE)

# Print t-test results
print("T-Test for OOO Correct Visit 1 Scores - Baseline Groups:")
print(t_test_OOO_baseline)
print("T-Test for OOO Correct Visit 2 Scores - Follow-Up Groups:")
print(t_test_OOO_followup)

# Reordner for plotting

data_long <- data %>%
  select(Baseline_Group, Followup_Group, OOO_correct_visit1, OOO_correct_visit2) %>%
  pivot_longer(
    cols = starts_with("OOO_correct_visit"),
    names_to = "Visit",
    values_to = "Score"
  ) %>%
  mutate(Group = if_else(Visit == "OOO_correct_visit1", as.character(Baseline_Group), as.character(Followup_Group))) %>%
  mutate(Visit = case_when(
    Visit == "OOO_correct_visit1" ~ "Baseline",
    Visit == "OOO_correct_visit2" ~ "Follow-Up",
    TRUE ~ Visit  # Fallback, should not be needed but good for catching issues
  ))


# Create the boxplot for OOO Correct Scores by Visit and Group with improved color scheme
ooo_plot <- ggplot(data_long, aes(x = Visit, y = Score, fill = Group)) +
  geom_boxplot(position = position_dodge(0.8)) +  # Default outliers included
  scale_fill_manual(values = c("High Activity" = "#FBB4AE", "Low Activity" = "#B3CDE3")) +  # Adjusted pastel colors
  labs(title = "Odd-One-Out Test Performance and Perirhinal Cortex Activity",
       x = "Time",
       y = "Correct Odd-One-Out Scores") +
  theme_minimal() +
  theme(legend.position = "bottom") +
  guides(fill=guide_legend(title="Group"))  

# Print the OOO Correct boxplot
print(ooo_plot)


#Violin plot

# Create the violin plot with small boxplots for OOO Correct Scores by Visit and Group
# and make the violin plots pointed at the end
ooo_violin_box_plot <- ggplot(data_long, aes(x = Visit, y = Score, fill = Group)) +
  geom_violin(trim = FALSE, position = position_dodge(0.9)) + # Set trim to TRUE for pointed ends
  geom_boxplot(width = 0.1, position = position_dodge(0.9), outlier.shape = NA, alpha = 0.5) +
  scale_fill_manual(values = c("High Activity" = "#FBB4AE", "Low Activity" = "#B3CDE3")) + # Adjusted pastel colors
  labs(title = "Odd-One-Out Test Performance and Perirhinal Cortex Activity",
       x = "Time",
       y = "Correct Odd-One-Out Scores") +
  theme_minimal() +
  theme(legend.position = "bottom") +
  guides(fill = guide_legend(title = "Group")) # Legend for the fill color

# Print the OOO Correct violin plot with boxplots
print(ooo_violin_box_plot)


# Cohens d

# Define group1 (High Activity Group)
group1 <- data$OOO_correct_visit2[data$Followup_Group == "High Activity"]

# Define group2 (Low Activity Group)
group2 <- data$OOO_correct_visit2[data$Followup_Group == "Low Activity"]


# Calculate Cohen's d
d_value <- cohensD(group1, group2)

# Print the result
print(paste("Cohen's d for the comparison between High and Low Activity groups at follow-up is:", d_value))

##########################################################################################

#Correlation

#############################################################################################

#Test for normality

# Histogram for Perirhinal Cortex Activity
hist(data$Perirhinal_Cortex_Activity_Task_FU, main = "Histogram: Perirhinal Cortex Activity", xlab = "Activity")
shapiro.test(data$Perirhinal_Cortex_Activity_Task_FU)

# Histogram for OOO Correct Scores
hist(data$OOO_correct_visit2, main = "Histogram: OOO Correct Scores", xlab = "Correct Scores")
shapiro.test(data$OOO_correct_visit2)

#Homoscedasticity

model <- lm(Perirhinal_Cortex_Activity_Task_FU ~ OOO_correct_visit2, data = data)
plot(model, which = 1)  # Residuals vs Fitted plot

correlation <- cor.test(data$OOO_correct_visit2, data$Perirhinal_Cortex_Activity_Task_FU, method = "pearson")
print(correlation)


#############################################################################################################################

#Regression Analysis#

##############################################################################################################################



# Regression with Perirhinal Cortex Activity as the dependent variable
model_activity <- lm(Perirhinal_Cortex_Activity_Task_FU ~ OOO_correct_visit2, data = data)
summary(model_activity)


# Plots for assumptions 
plot(model_activity)


# Create a scatter plot with regression line and confidence interval
ggplot(data, aes(x = OOO_correct_visit2, y = Perirhinal_Cortex_Activity_Task_FU)) +
  geom_point(aes(color = Followup_Group), alpha = 0.6) +  # Scatter plot points with transparency and colored by group
  geom_smooth(method = "lm", se = TRUE, color = "black", fill = "grey80") +  # Black regression line with grey confidence interval
  scale_color_manual(values = c("High Activity" = "red", "Low Activity" = "blue")) +  # Custom colors for groups
  labs(title = 'Perirhinal Cortex Activity and Odd-One-Out Task Performance',
       x = 'Odd-One-Out Correct Responses at Follow-up',
       y = 'Perirhinal Cortex Activity at Follow-Up') +
  theme_minimal() +  # Minimal theme for a clean look
  theme(legend.title = element_blank())  # Hide the legend title


#################################################################################

#Chi-Square test

#################################################################################

# Ensure the 'Group' variable is a factor and has correct levels
data$Group <- factor(data$Group, levels = c("stabil", "verschlechtert"))

# Create a contingency table of Followup_Group vs Group
contingency_table <- table(data$Followup_Group, data$Group)

# Print the contingency table to check the data
print(contingency_table)

# Chi-squared test
chi_test_result <- chisq.test(contingency_table)

# Print the results of the chi-squared test
print(chi_test_result)

#Plot

# Ensure 'Group' variable is a factor and set the correct levels with correct names
data$Group <- factor(data$Group, levels = c("stabil", "verschlechtert"), labels = c("Stable", "Worsened"))

# Define custom color palette with new factor names
my_colors <- c("Worsened" = "#FBB4AE", "Stable" = "#B3CDE3")

# Create the bar plot with improved aesthetics
ggplot(data, aes(x = Followup_Group, fill = Group)) +
  geom_bar(position = "dodge") +
  scale_fill_manual(values = my_colors) +
  labs(title = "Perirhinal Cortex Activity at Follow-up and Course",
       x = "Activity",
       y = "Count",
       fill = "Course") +
  theme_minimal() +
  theme(legend.position = "bottom",
        legend.title = element_text(face = "bold"),  # Bold the legend title
        legend.text = element_text(size = 11),      # Adjust text size in legend
        plot.title = element_text(hjust = 0.5, face = "bold", size = 12),  # Center and bold the plot title
        axis.title = element_text(size = 11, face = "bold"))  # Bold axis titles



################################################################################

## Predicition of course by perirhinal cortex activity

#################################################################################


# Ensure the Group variable is correctly factored
data$Group <- factor(data$Group, levels = c("stabil", "verschlechtert"), labels = c("Stable", "Worsened"))

# Logistic Regression Model
logistic_model <- glm(Group ~ Perirhinal_Cortex_Activity_Task_FU, data = data, family = binomial())

# Summary of the model
summary(logistic_model)

# Predicted probabilities
data$Predicted_Probabilities <- predict(logistic_model, type = "response")

# Convert probabilities to predicted classes
data$Predicted_Groups <- ifelse(data$Predicted_Probabilities > 0.5, "Worsened", "Stable")
data$Predicted_Groups <- factor(data$Predicted_Groups, levels = c("Stable", "Worsened"))

# Create the confusion matrix
conf_mat <- confusionMatrix(data$Predicted_Groups, data$Group, positive = "Worsened")

# Print the confusion matrix
print(conf_mat)





