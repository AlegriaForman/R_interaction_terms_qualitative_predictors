# Loading mtcars data set from a mtcars.csv file
mtcars <- read.csv(file='mtcars.csv', header=TRUE, sep=",")

# Converting appropriate variables to factors  
mtcars2 <- within(mtcars, {
   vs <- factor(vs)
   am <- factor(am)
   cyl  <- factor(cyl)
   gear <- factor(gear)
   carb <- factor(carb)
})


# Print the first six rows
print("head")
head(mtcars2, 6)

myvars <- c("mpg","wt","drat")
mtcars_subset <- mtcars2[myvars]

# Print the first six rows
print("head")
head(mtcars_subset, 6)

# Print the correlation matrix
print("cor")
corr_matrix <- cor(mtcars_subset, method = "pearson")
round(corr_matrix, 4)

# Create the multiple regression model and print summary statistics. Note that this model includes the interaction term. 
model1 <- lm(mpg ~ wt + drat + wt:drat, data=mtcars_subset)
summary(model1)

# Subsetting data to only include the variables that are needed
myvars <- c("mpg","wt","drat","am")
mtcars_subset <- mtcars2[myvars]

# Create the model
model2 <- lm(mpg ~ wt + drat + wt:drat + am, data=mtcars_subset)
summary(model2)

# predicted values
print("fitted")
fitted_values <- fitted.values(model2) 
fitted_values

# residuals
print("residuals")
residuals <- residuals(model2)
residuals

plot(fitted_values, residuals, 
     main = "Residuals against Fitted Values",
     xlab = "Fitted Values", ylab = "Residuals",
     col="red", 
     pch = 19, frame = FALSE)

qqnorm(residuals, pch = 19, col="red", frame = FALSE)
qqline(residuals, col = "blue", lwd = 2)

# confidence intervals for model parameters
print("confint")
conf_90_int <- confint(model2, level=0.90) 
round(conf_90_int, 4)

newdata <- data.frame(wt=3.88, drat=3.05, am='1')

print("prediction interval")
prediction_pred_int <- predict(model2, newdata, interval="predict", level=0.90) 
round(prediction_pred_int, 4)

print("confidence interval")
prediction_conf_int <- predict(model2, newdata, interval="confidence", level=0.90) 
round(prediction_conf_int, 4)

# Correlation Analysis 
# Line that loads mtcars data set from mtcars.csv file
mtcarsmod <- read.csv(file='mtcars.csv', header=TRUE, sep=",")

# Line that converts variables to factors  
mtcarsmod <- within(mtcarsmod, {
   vs <- factor(vs)
   am <- factor(am)
   cyl  <- factor(cyl)
   gear <- factor(gear)
   carb <- factor(carb)
})

# Print the first 10 rows
print("head")
head(mtcarsmod, 10)

# Line that will subset data mpg, hp, qsec and drat
vars1 <- c("mpg","hp","qsec","drat")
mtcarsmod_subset <- mtcarsmod[vars1]

# Print the first 10 rows
print("head")
head(mtcarsmod_subset, 10)

# Line that will calculate Pearson correlation coefficients and print the correlation matrix
print("Correlation Matrix for MPG, HP, QSEC & DRAT")
corr_matrix <- cor(mtcarsmod_subset, method = "pearson")
round(corr_matrix, 4)

# Reporting Results
# Line that produces the regression model for mpg applying hp, qsec, and drat as predictors, hp:qsec; hp:drat
firstmodel <- lm(mpg ~ hp + qsec + drat + hp:qsec + hp:drat, data=mtcarsmod_subset)
summary(firstmodel)

# Line producing Predicted Values
print("Fitted Values")
fittedval <- fitted.values(firstmodel) 
fittedval

# Line producing Residual Values
print("Residual Values")
residualval <- residuals(firstmodel)
residualval

# Line showing Residuals against Fitted Values plot 
plot(fittedval, residualval, 
     main = "Residuals Against Fitted Values",
     xlab = "Fitted Values", ylab = "Residuals",
     col="purple", 
     pch = 19, frame = FALSE)

# Line showing Normal Q-Q plot 
qqnorm(residualval, pch = 19, col="purple", frame = FALSE)
qqline(residualval, col = "red", lwd = 2)

# Making Predictions Using the Model
# Line that will show predictions utilizing the regression model
carreg1 <- data.frame(hp = 175, qsec = 14.2, drat = 3.91)

print("Prediction Interval for MPG of the car with HP = 175, QSEC = 14.2, DRAT = 3.91")
prediction_pred_int <- predict(firstmodel, carreg1, interval="predict", level=0.95) 
round(prediction_pred_int, 4)
print("Confidence Interval for MPG of the car with HP = 175, QSEC = 14.2, DRAT = 3.91")
prediction_conf_int <- predict(firstmodel, carreg1, interval="confidence", level=0.95) 
round(prediction_conf_int, 4)

# 4. Model with Interaction Term and Qualitative Predictor
# Reporting Results
# Line that will subset the data to show mpg, hp, qsec, hp:qsec and cyl
vars2 <- c("mpg","hp", "qsec","cyl")
mtcarsmod_subset <- mtcarsmod[vars2]

# Print the first 10 rows
print("head")
head(mtcarsmod_subset, 10)

# Line that will show regression model for mpg, hp, qsec, hp:qsec, and number of cyl
model2 <- lm(mpg ~ hp + qsec + hp:qsec + cyl, data=mtcarsmod_subset)
summary(model2)

# Predicted values for Model 2
print("Fitted Values for Model 2")
fittedval2 <- fitted.values(model2) 
fittedval2

# Residual values for Model 2
print("Residuals Values for Model 2")
residualval2 <- residuals(model2)
residualval2

# Residuals against Fitted Values plot for Model 2
plot(fittedval2, residualval2, 
     main = "Residuals Against Fitted Values for Model 2",
     xlab = "Fitted Values", ylab = "Residuals",
     col="purple", 
     pch = 19, frame = FALSE)

# Normal Q-Q plot for Model 2
qqnorm(residualval2, pch = 19, col="purple", frame = FALSE)
qqline(residualval2, col = "red", lwd = 2)

# Line for predictions producing Model 2 predicting MPG of a car with 175 HP, .2 QSEC and 6 CYL
carpred <- data.frame(hp = 175, qsec = .2, cyl = '6')

print("Prediction Interval For Model 2 Values")
prediction_pred_int <- predict(model2, carpred, interval="predict", level=0.95) 
round(prediction_pred_int, 4)

print("Confidence Interval For Model 2 Values")
prediction_conf_int <- predict(model2, carpred, interval="confidence", level = 0.95)
round(prediction_conf_int, 4)
