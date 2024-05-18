# Assuming your data is stored in a dataframe named 'data'
# Replace 'data' with the actual name of your dataframe

# Remove the last column 'class' as it seems to be your target variable
data <- data[, -ncol(data)]

# Convert the last column to numeric
data$class <- as.numeric(data$class)

# Fit a linear regression model
model <- lm(class ~ ., data = data)

# Summarize the model
summary(model)
