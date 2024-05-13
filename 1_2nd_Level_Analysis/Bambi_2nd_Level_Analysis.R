##############################################################################################
#2nd_Level_Analysis
#Bambi-Langzeit
#Raphaela Schöpfer

##############################################################################################

# Install packages
install.packages("readxl")
install.packages("ggplot2")
install.packages("car")
install.packages("effsize")
install.packages("rstatix")
install.packages("coin")

# Load libraries
library(readxl)
library(ggplot2)
library(car)
library(effsize)
library(rstatix)
library(coin)
library(ggplot2)
library(RColorBrewer)
library(ggpubr) 


# Load the data
data <- read_excel("C:\\Users\\rapha\\Documents\\FPS\\A_Con_values_table_2.xlsx")

##########################################################################################
#Outliers
##############################################################################################

# Function to calculate outliers
calculate_outliers <- function(x) {
  Q1 <- quantile(x, 0.25)
  Q3 <- quantile(x, 0.75)
  IQR <- Q3 - Q1
  lower_bound <- Q1 - 1.5 * IQR
  upper_bound <- Q3 + 1.5 * IQR
  return(x < lower_bound | x > upper_bound)
}

# Function to calculate and print outliers for each condition
calculate_and_print_outliers <- function(data, condition_name) {
  condition_data <- data$MeanConValue[data$Condition == condition_name]
  outliers_logical <- calculate_outliers(condition_data)
  outliers <- condition_data[outliers_logical]
  cat("Outliers for", condition_name, ":", outliers, "\n")
  return(which(data$Condition == condition_name & outliers_logical))
}

# Calculate and print outliers for each condition
outliers_schwer_indices <- calculate_and_print_outliers(data, "Schwer")
outliers_rest_indices <- calculate_and_print_outliers(data, "Rest")
outliers_leicht_indices <- calculate_and_print_outliers(data, "Leicht")
outliers_combined_indices <- calculate_and_print_outliers(data, "Schwer_Leicht_Combined")

##########--For Baseline

# Calculate outliers for the 'Rest' condition
rest_condition_indices <- which(data$Condition == "Rest")
rest_values <- data$MeanConValue[rest_condition_indices]
outliers_logical <- calculate_outliers(rest_values)
outliers_indices <- rest_condition_indices[outliers_logical]

# Calculate mean of 'Rest' condition without outliers
mean_without_outliers_rest <- mean(rest_values[!outliers_logical], na.rm = TRUE)

# Replace outliers in the original dataset with this mean
data$MeanConValue[outliers_indices] <- mean_without_outliers_rest

#New_data =Outliers in 'Rest' condition replaced with the mean of this group

############## --For Follow-up

# Calculate outliers for the 'Schwer' condition
#schwer_condition_indices <- which(data$Condition == "Schwer")
#schwer_values <- data$MeanConValue[schwer_condition_indices]
#outliers_logical_schwer <- calculate_outliers(schwer_values)
#outliers_indices_schwer <- schwer_condition_indices[outliers_logical_schwer]

# Calculate mean of 'Schwer' condition without outliers
#mean_without_outliers_schwer <- mean(schwer_values[!outliers_logical_schwer], na.rm = TRUE)

# Replace outliers in the original dataset for 'Schwer' with this mean
#data$MeanConValue[outliers_indices_schwer] <- mean_without_outliers_schwer

# Calculate outliers for the 'Schwer_Leicht_Combined' condition
#combined_condition_indices <- which(data$Condition == "Schwer_Leicht_Combined")
#combined_values <- data$MeanConValue[combined_condition_indices]
#outliers_logical_combined <- calculate_outliers(combined_values)
#outliers_indices_combined <- combined_condition_indices[outliers_logical_combined]

# Calculate mean of 'Schwer_Leicht_Combined' condition without outliers
#mean_without_outliers_combined <- mean(combined_values[!outliers_logical_combined], na.rm = TRUE)

# Replace outliers in the original dataset for 'Schwer_Leicht_Combined' with this mean
#data$MeanConValue[outliers_indices_combined] <- mean_without_outliers_combined

# Updated data with replaced outliers 
new_data <- data


##############################################################################################
#Check assumptions for tests
##############################################################################################


# Normality tests for each condition
shapiro_test_schwer <- shapiro.test(new_data$MeanConValue[data$Condition == "Schwer"])
shapiro_test_rest <- shapiro.test(new_data$MeanConValue[data$Condition == "Rest"])
shapiro_test_leicht <- shapiro.test(new_data$MeanConValue[data$Condition == "Leicht"])
shapiro_test_combined <- shapiro.test(new_data$MeanConValue[data$Condition == "Schwer_Leicht_Combined"])


# Print normality of test results
print(shapiro_test_schwer)
print(shapiro_test_rest)
print(shapiro_test_leicht)
print(shapiro_test_combined)


##############################################################################################
#Statistical tests
##############################################################################################


# Perform Paired t-tests
t_test_combine_vs_rest <- t.test(MeanConValue ~ Condition, data = subset(new_data, Condition %in% c("Schwer_Leicht_Combined", "Rest")), paired = TRUE)
t_test_schwer_vs_rest <- t.test(MeanConValue ~ Condition, data = subset(new_data, Condition %in% c("Schwer", "Rest")), paired = TRUE)
t_test_leicht_vs_rest <- t.test(MeanConValue ~ Condition, data = subset(new_data, Condition %in% c("Leicht", "Rest")), paired = TRUE)
t_test_leicht_vs_schwer <- t.test(MeanConValue ~ Condition, data = subset(new_data, Condition %in% c("Leicht", "Schwer")), paired = TRUE)

# Collect p-values
p_values <- c(t_test_combine_vs_rest$p.value,
              t_test_schwer_vs_rest$p.value,
              t_test_leicht_vs_rest$p.value,
              t_test_leicht_vs_schwer$p.value)



# Print original p-values

print(p_values)



# Calculate Cohen's d
cohen_d_combine_vs_rest <- cohen.d(new_data$MeanConValue[new_data$Condition == "Schwer_Leicht_Combined"], new_data$MeanConValue[new_data$Condition == "Rest"], paired = TRUE)
cohen_d_schwer_vs_rest <- cohen.d(new_data$MeanConValue[new_data$Condition == "Schwer"], new_data$MeanConValue[new_data$Condition == "Rest"], paired = TRUE)
cohen_d_leicht_vs_rest <- cohen.d(new_data$MeanConValue[new_data$Condition == "Leicht"], new_data$MeanConValue[new_data$Condition == "Rest"], paired = TRUE)
cohens_d_leicht_vs_schwer <- cohen.d(new_data$MeanConValue[new_data$Condition == "Leicht"], new_data$MeanConValue[new_data$Condition == "Schwer"], paired = TRUE)


# Print effect sizes (Cohen's d)
print(cohen_d_combine_vs_rest)
print(cohen_d_schwer_vs_rest)
print(cohen_d_leicht_vs_rest)
print(cohens_d_leicht_vs_schwer)




##############################################################################################
#Plots
##############################################################################################


#Boxplot

# Factor the 'Condition' column to set levels and labels
new_data$Condition <- factor(new_data$Condition, levels = c("Rest", "Leicht", "Schwer", "Schwer_Leicht_Combined"),
                             labels = c("Rest", "Easy", "Difficult", "Whole Task"))

# Reorder the factor levels 
new_data$Condition <- factor(new_data$Condition, levels = c("Rest","Easy", "Difficult", "Whole Task"))

# Now create the plot
p <- ggplot(new_data, aes(x = Condition, y = MeanConValue, fill = Condition)) +
  geom_boxplot(alpha = 0.7, outlier.shape = NA) +
  geom_jitter(width = 0.2, alpha = 0.5, color = "black") +
  scale_fill_brewer(palette = "Dark2") +
  theme_minimal(base_size = 14) +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Mean Perirhinal Cortex Activity",
       x = "Condition",
       y = "Activity (Mean Contrast Value)")

# Print 
print(p)

##########################################################################

#Violin plot

# Change factor levels from "schwer" to "Difficult" and "leicht" to "Easy"
new_data$Condition <- recode(new_data$Condition, 
                             'schwer' = 'Difficult', 
                             'leicht' = 'Easy')

# Set the order of factor levels to Easy, Difficult, Whole Task, Rest
new_data$Condition <- factor(new_data$Condition, levels = c("Rest", "Easy", "Difficult", "Whole Task"))

# Create the plot
p <- ggplot(new_data, aes(x = Condition, y = MeanConValue, fill = Condition)) +
  geom_violin(trim = FALSE, alpha = 0.4) + 
  geom_boxplot(width = 0.2, alpha = 0.6, outlier.shape = 21, outlier.fill = "white") +
  geom_jitter(width = 0.15, alpha = 0.5, color = "black") +
  scale_fill_brewer(palette = "Set3") +
  theme_minimal(base_size = 14) +
  theme(legend.position = "none") + 
  labs(title = "Mean Perirhinal Cortex Activity",
       x = "Condition",
       y = "Activity (Mean Contrast Value)")


# Print the plot
print(p)

##############################################################################################





