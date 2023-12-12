#Final Project Data Science
#firstly, we have to do an initial analysis on auto mpg
#cylinders= a chamber where fuel is combusted and power is generated.
#displacement=the combined volume of air moved 
#— or displaced — by the pistons in its cylinders
#horsepower=how quickly the force is produced from a vehicle's engine
file_path <- "~/Desktop/auto-mpg.csv"
autodata <- read.csv(file_path)
head(autodata)
#inital analysis to me is finding the mean, median etc of each of our variables
summary(autodata)
#this checks if there is any null values or missing and the answer is false, none.
any(is.na(autodata))


# Extract the first 300 samples
subset_data <- head(autodata, 300)

#Simple Linear Regression 
#(with horsepower as the independent variable)
model_simple <- lm(mpg ~ horsepower, data = subset_data)

# Multiple Linear Regression
#(with horsepower and weight as independent variables)
model_multiple <- lm(mpg ~ horsepower + weight, data = subset_data)

# Model 1
summary_simple <- summary(model_simple)
r_squared_simple <- summary_simple$r.squared
adjusted_r_squared_simple <- summary_simple$adj.r.squared
equation_simple <- as.character(round(coef(model_simple), 3))

#  Model 2 
summary_multiple <- summary(model_multiple)
r_squared_multiple <- summary_multiple$r.squared
adjusted_r_squared_multiple <- summary_multiple$adj.r.squared
equation_multiple <- as.character(round(coef(model_multiple), 3))

# Print the results Model 1 Multiple R-squared: 0.858 Adjusted R-squared: 0.8 
#Linear Regression Equation: mpg = 23 + -4.567 * horsepower 
cat("Multiple R-squared:", round(r_squared_simple, 3), "\n")
cat("Adjusted R-squared:", round(adjusted_r_squared_simple, 3), "\n")
cat("Linear Regression Equation:", paste("mpg =", equation_simple[1], "+", equation_simple[2], "* horsepower"), "\n\n")

#Model 2 Multiple R-squared: 0.895 Adjusted R-squared: 0.852 
#Linear Regression Equation: mpg = 32.191 + -1.7 * horsepower + -0.425 * weight 
cat("Multiple R-squared:", round(r_squared_multiple, 3), "\n")
cat("Adjusted R-squared:", round(adjusted_r_squared_multiple, 3), "\n")
cat("Linear Regression Equation:", paste("mpg =", equation_multiple[1], "+", equation_multiple[2], "* horsepower +", equation_multiple[3], "* weight"), "\n")

#We learned in stats two things I think are helpful for this project
#One is multivariate regression, which is when all the independent variables
#are shown as predictors for the dependent variable
# Model 3 multivariate regression
model_multivariate <- lm(mpg ~ ., data = subset_data)

# Model 4 Backward Selection, which is when each of the variables are tested for 
#their significance it takes away one independent variables and tests if results are different
model_backward <- step(model_multivariate, direction = "backward")

# get summary stats for multivariate
summary_multivariate <- summary(model_multivariate)
r_squared_multivariate <- summary_multivariate$r.squared
adjusted_r_squared_multivariate <- summary_multivariate$adj.r.squared
#this one i looked up its hard code 
equation_multivariate <- as.character(round(coef(model_multivariate), 3))

# get summary stats for backward selection
summary_backward <- summary(model_backward)
r_squared_backward <- summary_backward$r.squared
adjusted_r_squared_backward <- summary_backward$adj.r.squared
equation_backward <- as.character(round(coef(model_backward), 3))

# Print the results multivariate regression
#Multiple R-squared: 0.995, Adjusted R-squared: 0.928 
#equation=Linear Regression Equation: mpg = 32.399 +
#-2.285 * horsepower + 0.004 * weight + -4.178 * cylinders + -1.073 * acceleration 
cat("Multiple R-squared:", round(r_squared_multivariate, 3), "\n")
cat("Adjusted R-squared:", round(adjusted_r_squared_multivariate, 3), "\n")
cat("Linear Regression Equation:", paste("mpg =", equation_multivariate[1], "+", equation_multivariate[2], "* horsepower +", equation_multivariate[3], "* weight +", equation_multivariate[4], "* cylinders +", equation_multivariate[5], "* acceleration"), "\n\n")

#Backward selection Multiple R-squared: 0.995 Adjusted R-squared: 0.931 
cat("Multiple R-squared:", round(r_squared_backward, 3), "\n")
cat("Adjusted R-squared:", round(adjusted_r_squared_backward, 3), "\n")
#says this cant handle it cat("Linear Regression Equation:", formula(model_backward), "\n")
#kinda makes sense cuz if its erasing one everytime how could it have an equation
# We see according to the multivariate and backward r squared that they fit better
#the r is .995 which is strong
unique_levels_train <- levels(autodata$horsepower)
unique_levels_new <- levels(remaining_data$horsepower)

setdiff(unique_levels_new, unique_levels_train)
setdiff(unique_levels_train, unique_levels_new)
str(autodata$horsepower)
str(remaining_data$horsepower)

model_backward <- step(model_multivariate, direction = "backward")


# Now testing the 98 samples left using the tail to show what we want
# Use the best linear model to make predictions- to me the best was backwards selection 
#i had a million errors so i ended up
#using multivariate and it had the best fit on r squared, predict makes predictions on the data
# Now testing the 98 samples left using the tail to show what we want
remaining_data <- tail(autodata, 98)

# Use the multivariate linear model to make predictions
predictions <- predict(model_multivariate,remaining_data)


#residuals are very important, if residuals are regular it means your data is normal
# its very good. you calculate by subtracting predicted vals from real vals 
residuals <- remaining_data$mpg - predictions

#we need to plot it to see it  how well the model predicts 
#across the range of predicted values. The red dashed line represents zero residuals.
plot(predictions, residuals, main = "Residual Plot", xlab = "Predicted MPG", ylab = "Residuals")
abline(h = 0, col = "red", lty = 2)

# histogram of residuals
hist(residuals, main = "Histogram of Residuals", xlab = "Residuals", col = "lightblue", border = "black")

# Display summary statistics of residuals
summary(residuals)
