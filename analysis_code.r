# Load necessary libraries
library(readr)
library(dplyr)
library(ggplot2)
library(RColorBrewer)
library(psych)  # for point-biserial correlation

# Read data
accident_data <- read.csv("accident_data.csv")

# Convert factors
accident_data$Cause_category <- as.factor(accident_data$Cause_category)
accident_data$Outcome_of_incident <- as.factor(accident_data$Outcome_of_incident)
accident_data$Cities <- as.factor(accident_data$Cities)

# Handle missing values
cleaned_data <- na.omit(accident_data)

# Summary statistics
summary(cleaned_data)

# Distribution of cities
city_distribution <- table(cleaned_data$Cities)
print(city_distribution)

# Frequency of cause categories
cause_category_frequency <- table(cleaned_data$Cause_category)
print(cause_category_frequency)

# Frequency of cause subcategories
cause_subcategory_frequency <- table(cleaned_data$Cause_subcategory)
print(cause_subcategory_frequency)

# Distribution of outcomes
outcome_distribution <- table(cleaned_data$Outcome_of_incident)
print(outcome_distribution)

# Age distribution histogram
hist(cleaned_data$Age, main = "Age Distribution of Individuals Involved in Accidents", xlab = "Age")

# Logistic regression model
model <- glm(Outcome_of_incident ~ Cause_category + Age + Cities, data = cleaned_data, family = binomial)
summary(model)

# Predicted probabilities
cleaned_data$predicted_prob <- predict(model, type = "response")

# Aggregate data to calculate total count of accidents for each city
total_accidents_by_city <- cleaned_data %>%
  group_by(Cities) %>%
  summarise(Total_Count = sum(Count, na.rm = TRUE))

# Create the bar plot: Number of accidents by city
ggplot(total_accidents_by_city, aes(x = Cities, y = Total_Count)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(title = "Total Count of Accidents by City", x = "City", y = "Total Count of Accidents") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))

# Bar chart: Number of accidents by cause category
ggplot(cleaned_data, aes(x = Cause_category)) +
  geom_bar(fill = "green") +
  labs(title = "Number of Accidents by Cause Category", x = "Cause Category", y = "Number of Accidents")

# Pie chart: Distribution of outcomes for incidents
outcome_data <- cleaned_data %>%
  count(Outcome_of_incident) %>%
  mutate(percentage = n / sum(n) * 100)

outcome_data$percentage <- round(outcome_data$percentage, 1)
outcome_data$percentage[nrow(outcome_data)] <- 100 - sum(outcome_data$percentage[-nrow(outcome_data)])

ggplot(outcome_data, aes(x = "", y = percentage, fill = Outcome_of_incident)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y") +
  geom_text(aes(label = paste0(percentage, "%")), 
            position = position_stack(vjust = 0.5)) +
  labs(title = "Distribution of Outcomes for Incidents", x = NULL, y = NULL) +
  theme_void()

# Histogram: Age distribution
ggplot(cleaned_data, aes(x = Age)) +
  geom_histogram(binwidth = 5, fill = "skyblue", color = "black") +
  labs(title = "Age Distribution of Individuals Involved in Accidents", x = "Age", y = "Frequency")

# Scatter plot: Relationship between age and outcome of incidents
ggplot(cleaned_data, aes(x = Age, y = as.numeric(Outcome_of_incident))) +
  geom_point(alpha = 0.5) +
  labs(title = "Relationship between Age and Outcome of Incidents", x = "Age", y = "Outcome of Incident") +
  scale_y_continuous(breaks = 1:length(levels(cleaned_data$Outcome_of_incident)),
                     labels = levels(cleaned_data$Outcome_of_incident)) +
  theme_minimal()

# Calculate point-biserial correlation between Age and Outcome_of_incident
point_biserial_corr <- cor.test(cleaned_data$Age, as.numeric(cleaned_data$Outcome_of_incident))
print(point_biserial_corr)

# Correlation between age and outcome
age_outcome_correlation <- cor(cleaned_data$Age, as.numeric(cleaned_data$Outcome_of_incident))
print(age_outcome_correlation)

# Chi-square test
chisq_test <- chisq.test(table(cleaned_data$Cause_category, cleaned_data$Outcome_of_incident))
print(chisq_test)

# Logistic Regression for train and test data
set.seed(123)  # for reproducibility
train_index <- sample(nrow(cleaned_data), 0.7 * nrow(cleaned_data))  # 70% train, 30% test
train_data <- cleaned_data[train_index, ]
test_data <- cleaned_data[-train_index, ]

logistic_model <- glm(Outcome_of_incident ~ Cause_category + Age + Cities, data = train_data, family = binomial)
summary(logistic_model)

logistic_predictions <- predict(logistic_model, newdata = test_data, type = "response")
logistic_predictions <- ifelse(logistic_predictions > 0.5, 1, 0)

# Linear regression model
linear_model <- lm(Age ~ Cause_category + Cities, data = cleaned_data)
summary(linear_model)

# Predictions
cleaned_data$predicted_age <- predict(linear_model, cleaned_data)

# Scatter plot with regression line: Age vs. Cause_category
ggplot(cleaned_data, aes(x = Cause_category, y = Age)) +
  geom_boxplot(fill = "gray", color = "black", alpha = 0.7) +
  stat_summary(fun = mean, geom = "point", shape = 20, size = 4, color = "red", fill = "red") +
  geom_smooth(method = "lm", aes(group = 1), color = "red", se = FALSE) +
  labs(title = "Age Distribution by Cause Category", x = "Cause Category", y = "Age")

# Scatter plot with regression line: Age vs. Cities
ggplot(cleaned_data, aes(x = Cities, y = Age)) +
  geom_boxplot(fill = "gray", color = "black", alpha = 0.7) +
  stat_summary(fun = mean, geom = "point", shape = 20, size = 4, color = "red", fill = "red") +
  geom_smooth(method = "lm", aes(group = 1), color = "red", se = FALSE) +
  labs(title = "Age Distribution by City", x = "City", y = "Age") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 10), # Adjust the angle and size of x-axis text
        plot.title = element_text(hjust = 0.5)) # Center the title

# Scatter plot: Predicted vs. Actual Age
ggplot(cleaned_data, aes(x = predicted_age, y = Age)) +
  geom_point(alpha = 0.5, color = "blue") +
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") +
  labs(title = "Predicted vs. Actual Age", x = "Predicted Age", y = "Actual Age") +
  theme_minimal()
