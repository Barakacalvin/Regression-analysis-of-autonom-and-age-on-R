# Load required libraries
library(tidyverse)

# Function to check if required columns exist
check_columns <- function(data, columns) {
  missing_cols <- setdiff(columns, colnames(data))
  if (length(missing_cols) > 0) {
    stop(paste("Columns missing from the dataset:", paste(missing_cols, collapse = ", ")))
  }
}

# Load the dataset with error handling
file_path <- "C:/Users/cbaraka/OneDrive/Masters/Assignments/Quantitative Analysis/Week 12/job_survey_data.csv"
if (file.exists(file_path)) {
  data <- read.csv(file_path)
  check_columns(data, c("autonom1", "autonom2", "autonom3", "autonom4", "age"))
} else {
  stop("File not found.")
}

# Calculate the average autonomy score
data <- data %>%
  mutate(average_autonom = rowMeans(select(., autonom1:autonom4), na.rm = TRUE))

# Perform regression analysis using the average autonomy score
model_avg <- lm(average_autonom ~ age, data = data)

# Summary of the regression
summary(model_avg)

# Predicted value for age 54
predicted_value_avg <- predict(model_avg, newdata = data.frame(age = 54))
print(predicted_value_avg)

# Create a scatter plot with regression line
ggplot(data, aes(x = age, y = average_autonom)) +
  geom_point(color = "blue") +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(title = "Regression Analysis: Autonom vs Age",
       x = "Age",
       y = "Autonom") +
  theme_minimal()
