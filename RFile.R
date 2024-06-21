# Heart Disease Prediction using Linear Regression
# Authors: Bommidi Nagaraju, Saravan Kumar Cherukupally, Saurabh Rajkapoor Chaurasia
# Date: 2022-11-11

# Importing packages
library(tidyverse) # metapackage with lots of helpful functions

# You can access files from datasets you've added to this kernel in the "../input/" directory.
# You can see the files added to this kernel by running the code below.
list.files(path = "../input")

# Set plot size
options(repr.plot.width = 10, repr.plot.height = 8)

# Read dataset
# Replace file.choose() with the path to your dataset
df <- read_csv( file.choose(), show_col_types = FALSE)

# Print first few rows of data
head(df)

# Summary of data
summary(df)

# Checking for missing values
sum(is.na(df))

# Convert categorical variables to factors
df$HeartDisease <- as.factor(df$HeartDisease)
df$Smoking <- as.factor(df$Smoking)
df$AlcoholDrinking <- as.factor(df$AlcoholDrinking)
df$Stroke <- as.factor(df$Stroke)
df$DiffWalking <- as.factor(df$DiffWalking)
df$Sex <- as.factor(df$Sex)
df$AgeCategory <- as.factor(df$AgeCategory)
df$Race <- as.factor(df$Race)
df$Diabetic <- as.factor(df$Diabetic)
df$PhysicalActivity <- as.factor(df$PhysicalActivity)
df$GenHealth <- as.factor(df$GenHealth)
df$Asthma <- as.factor(df$Asthma)
df$KidneyDisease <- as.factor(df$KidneyDisease)
df$SkinCancer <- as.factor(df$SkinCancer)

# Exploratory Data Analysis (EDA)
ggplot(data = df, aes(x = HeartDisease, fill = HeartDisease)) +
  geom_bar() +
  ggtitle("Distribution of Heart Disease") +
  theme_minimal()

# Linear Regression Model

# Convert HeartDisease to numeric for regression
df$HeartDisease <- ifelse(df$HeartDisease == "Yes", 1, 0)

# Split data into training and testing sets
set.seed(123)
train_index <- sample(seq_len(nrow(df)), size = 0.7 * nrow(df))
train_data <- df[train_index, ]
test_data <- df[-train_index, ]

# Train the model
model <- lm(HeartDisease ~ ., data = train_data)

# Summary of the model
summary(model)

# Make predictions
predictions <- predict(model, newdata = test_data)

# Evaluate the model
mse <- mean((test_data$HeartDisease - predictions)^2)
print(paste("Mean Squared Error: ", mse))

# Plotting predictions vs actual
plot(test_data$HeartDisease, predictions,
     main = "Predictions vs Actual",
     xlab = "Actual HeartDisease",
     ylab = "Predicted HeartDisease")
abline(0, 1)
