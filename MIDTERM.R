# Load the dataset
data(airquality)
# Display the first few rows of the dataset
head(airquality)

# Linear Regression
# Remove rows with missing values
airquality <- na.omit(airquality)

# Fit the linear model
linear_model <- lm(Ozone ~ Temp, data=airquality)

# Display the summary of the model
summary(linear_model)

# Predict Ozone values
predicted_ozone <- predict(linear_model)

# Plot actual vs predicted values
plot(airquality$Temp, airquality$Ozone, col="blue", pch=20, main="Linear Regression: Ozone vs Temperature",
     xlab="Temperature", ylab="Ozone")
lines(airquality$Temp, predicted_ozone, col="red", lwd=2)
legend("topright", legend=c("Actual", "Predicted"), col=c("blue", "red"), pch=20, lty=1, lwd=2)

# Polynomial Regression
# Fit the polynomial model
poly_model <- lm(Ozone ~ poly(Temp, 2), data=airquality)

# Display the summary of the model
summary(poly_model)

# Predict Ozone values
predicted_ozone_poly <- predict(poly_model)

# Plot actual vs predicted values
plot(airquality$Temp, airquality$Ozone, col="blue", pch=20, main="Polynomial Regression: Ozone vs Temperature",
     xlab="Temperature", ylab="Ozone")
lines(airquality$Temp, predicted_ozone_poly, col="red", lwd=2)
legend("topright", legend=c("Actual", "Predicted"), col=c("blue", "red"), pch=20, lty=1, lwd=2)

# Logistic Regression
# Create a binary outcome variable
median_ozone <- median(airquality$Ozone, na.rm=TRUE)
airquality$HighOzone <- ifelse(airquality$Ozone > median_ozone, 1, 0)

# Fit the logistic model
logistic_model <- glm(HighOzone ~ Temp, family=binomial, data=airquality)

# Display the summary of the model
summary(logistic_model)

# Predict probabilities
predicted_probabilities <- predict(logistic_model, type="response")

# Plot actual vs predicted values
plot(airquality$Temp, airquality$HighOzone, col="blue", pch=20, main="Logistic Regression: High Ozone vs Temperature",
     xlab="Temperature", ylab="High Ozone (1=High, 0=Low)")
points(airquality$Temp, predicted_probabilities, col="red", pch=4)
legend("topright", legend=c("Actual", "Predicted"), col=c("blue", "red"), pch=c(20, 4))
