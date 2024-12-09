setwd("C:/Users/yogesh/OneDrive/Documents/Capstone Project/Project")

data = read.csv("Automobile.csv")
head(data)
tail(data)

sapply(data, function(x) sum(is.na(x)))  #Check the missing values
   
summary(data)


#Box plot for five number summaries
windows()
par(mfrow = c(3,2))

boxplot(data$mpg, main = 'mpg')  
boxplot(data$cylinders, main = 'cylinders')
boxplot(data$displacement, main = 'displacement')
boxplot(data$horsepower, main = 'horsepower')
boxplot(data$weight, main = 'weight')
boxplot(data$acceleration, main = 'acceleration')

#Pearson product-moment correlation matrix

library(corrplot)
correlation_matrix <- cor(data[, c('mpg' , 'cylinders', 'displacement', 'horsepower', 'weight', 'acceleration')]) 
print(correlation_matrix)

# Load necessary library
#install.packages("car")
library(car)

# Calculate VIF
vif_values <- vif(mlr_model)
print(vif_values)



variables_of_interest <- c('mpg', 'cylinders', 'displacement', 'horsepower', 'weight', 'acceleration')

# Create a scatterplot matrix
pairs(data[, variables_of_interest], main="Scatterplot Matrix")


#Plot histograms
windows()
par(mfrow = c(3,2))

hist(data$mpg, col = 'blue', main = 'mpg') 
hist(data$cylinders, col = 'red', main = 'cylinders')
hist(data$displacement, col = 'green', main = 'displacement')
hist(data$horsepower, col = 'green', main = 'horsepower')
hist(data$weight, col = 'yellow', main = 'weight')
hist(data$acceleration, col = 'purple', main = 'acceleration')


#e.	Analysis of outliers or high-leverage data (if applicable)1
#i.	use both z-score and 1.5 X IQR
#ii.	resolution (remove or retain, and explain why)


# Assuming your data is in a vector or column 'data_column'
z_scores <- scale(data$mpg)
z_scorez_scores <- scale(data$cylinders)
s <- scale(data$displacement)
z_scores <- scale(data$horsepower)
z_scores <- scale(data$weight)
z_scores <- scale(data$acceleration)

# Define a threshold for z-scores
z_threshold <- 2  # You can adjust this threshold as needed

# Identify and analyze outliers based on the z-score
outliers_zscore <- data[abs(z_scores) > z_threshold]

# Print or analyze outliers
print(outliers_zscore)

### - Drop the columns or variables that will not be used for the calculations

data1 = subset(data, select = -c(name, model_year, origin))
head(data1)



#######################################################################################
# PREDICTIVE ANALYSIS - MULTIPLE LINEAR REGRESSION & REGRESSION TREE


##### -- SPLITTING THE DATA IN TO TRAINING - 80% AND TESTING 20%
# Setting a seed for reproducibility
set.seed(123)

# Splitting the data into training (80%) and testing (20%) sets
sample_size <- floor(0.80 * nrow(data))
train_indices <- sample(seq_len(nrow(data)), size = sample_size)

# Create training and testing sets
train_data <- data[train_indices, ]
test_data <- data[-train_indices, ]

########### - BUILDING MULTIPLE LINEAR REGRESSION MODEL 

# Multiple Linear Regression
mlr_model <- lm(mpg ~ cylinders + displacement + horsepower + weight + acceleration, data = train_data)


# Summary of the model
summary(mlr_model)

# Predictions for Training Data
mlr_train_predictions <- predict(mlr_model, newdata = train_data)

# Calculate Residuals
mlr_residuals <- train_data$mpg - mlr_train_predictions

# Residual Plot for MLR
plot(mlr_train_predictions, mlr_residuals, 
     xlab = "Predicted MPG", 
     ylab = "Residuals", 
     main = "Residual Plot for MLR")
abline(h = 0, col = "red")  # Adds a horizontal line at y = 0


### - BUILDING REGRESSION TREE MODEL

# Load the required library for regression trees
library(rpart)
library(rpart.plot)

# Fit a regression tree model
regression_tree <- rpart(mpg ~ cylinders + displacement + horsepower + weight + acceleration, data = train_data, method = "anova")


# Extract variable importance
variable_importance <- regression_tree$variable.importance

# Convert to a data frame for better visualization
importance_df <- data.frame(Variable = names(variable_importance), Importance = variable_importance)

# Create a bar plot for variable importance
library(ggplot2)

ggplot(importance_df, aes(x = reorder(Variable, Importance), y = Importance)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +  # Flip coordinates for better readability
  labs(title = "Variable Importance for Regression Tree Model",
       x = "Predictor Variables",
       y = "Importance") +
  theme_minimal()

  

# Visualize the tree
plot(regression_tree)
text(regression_tree, pretty = 1)

summary(regression_tree)

# Predictions for Training Data using Regression Tree
tree_train_predictions <- predict(regression_tree, newdata = train_data)

# Calculate Residuals for Regression Tree
tree_residuals <- train_data$mpg - tree_train_predictions

# Residual Plot for Regression Tree
plot(tree_train_predictions, tree_residuals, 
     xlab = "Predicted MPG (Regression Tree)", 
     ylab = "Residuals", 
     main = "Residual Plot for Regression Tree")
abline(h = 0, col = "red")  # Adds a horizontal line at y = 0




### - Model Evaluation

# Load the required library for calculating performance metrics
library(Metrics)

# Predictions for Linear Regression Model
mlr_predictions <- predict(mlr_model, newdata = test_data)

# Calculate MSE and R-squared for Linear Regression
mlr_mse <- mse(test_data$mpg, mlr_predictions)
mlr_r2 <- 1 - (sum((mlr_predictions - test_data$mpg)^2) / sum((test_data$mpg - mean(test_data$mpg))^2))

# Print MSE and R-squared for MLR
print(paste("Multiple Linear Regression Testing MSE:", mlr_mse))
print(paste("Multiple Linear Regression Testing R-squared:", mlr_r2))

# Predictions for Regression Tree
tree_predictions <- predict(regression_tree, newdata = test_data)

# Calculate MSE and R-squared for Regression Tree
tree_mse <- mse(test_data$mpg, tree_predictions)
tree_r2 <- 1 - (sum((tree_predictions - test_data$mpg)^2) / sum((test_data$mpg - mean(test_data$mpg))^2))

# Print MSE and R-squared for Regression Tree
print(paste("Regression Tree Testing MSE:", tree_mse))
print(paste("Regression Tree Testing R-squared:", tree_r2))

### - COMPARING TRAINING AND TESTING PERFORAMNCE FOR MLR TO SEE IF WE NEED FURTHER IMPROVEMENT TO OUT MODEL

# MSE and R-squared for Training Set
mlr_train_predictions <- predict(mlr_model, newdata = train_data)
mlr_train_mse <- mean((train_data$mpg - mlr_train_predictions)^2)
mlr_train_r2 <- 1 - (sum((mlr_train_predictions - train_data$mpg)^2) / sum((train_data$mpg - mean(train_data$mpg))^2))

# Print Training MSE and R-squared
print(paste("MLR Training MSE:", mlr_train_mse))
print(paste("MLR Training R-squared:", mlr_train_r2))


### - COMPARING TRAINING AND TESTING PERFORAMNCE FOR REGRESSION TREE TO SEE IF WE NEED FURTHER IMPROVEMENT TO OUT MODEL

# MSE and R-squared for Training Set
tree_train_predictions <- predict(regression_tree, newdata = train_data)
tree_train_mse <- mean((train_data$mpg - tree_train_predictions)^2)
tree_train_r2 <- 1 - (sum((tree_train_predictions - train_data$mpg)^2) / sum((train_data$mpg - mean(train_data$mpg))^2))

# Print Training MSE and R-squared
print(paste("Regression Tree Training MSE:", tree_train_mse))
print(paste("Regression Tree Training R-squared:", tree_train_r2))


################## VISUALIZATION TO VISUALLY SEE HOW OUR MODELS ARE PERFROMING  #######################

# A scatter plot showing actual vs. predicted values for both the models
#can help visualize the accuracy of the models.

# Actual vs. Predicted for MLR
plot(test_data$mpg, mlr_predictions, 
     xlab = "Actual MPG", 
     ylab = "Predicted MPG", 
     main = "Actual vs. Predicted for MLR")
abline(0, 1, col = "blue")  # 45-degree line

# Actual vs. Predicted for Regression Tree
plot(test_data$mpg, tree_predictions, 
     xlab = "Actual MPG", 
     ylab = "Predicted MPG", 
     main = "Actual vs. Predicted for Regression Tree")
abline(0, 1, col = "blue")  # 45-degree line


# Variable importance plot for regression tree 
#This plot shows the importance of each predictor variable in the regression tree model.

# Variable Importance for Regression Tree
library(rpart.plot)
rpart.plot(regression_tree, main = "Regression Tree", extra = 101)


# Boxplot of MPG by Number of Cylinders- Boxplots can help visualize how mpg varies across different categories of cylinders.
boxplot(mpg ~ cylinders, data = train_data,
        xlab = "Number of Cylinders", 
        ylab = "Miles per Gallon (MPG)", 
        main = "Boxplot of MPG by Number of Cylinders")

####Actual vs. Predicted Values: This plot uses points to represent actual 
#versus predicted values for both models. A dashed line represents the ideal prediction 
#scenario (where actual values equal predicted values).


# Load necessary library
library(ggplot2)
#install.packages("reshape2")
library(reshape2)

# Create a data frame with actual and predicted values
comparison_df <- data.frame(
  Actual = test_data$mpg,
  MLR_Predicted = mlr_predictions,
  Tree_Predicted = tree_predictions
)

# Reshape the data for plotting
comparison_df_melted <- melt(comparison_df, id.vars = "Actual", 
                             variable.name = "Model", 
                             value.name = "Predicted")

# Plot Actual vs. Predicted for both models
ggplot(comparison_df_melted, aes(x = Actual, y = Predicted, color = Model)) +
  geom_point(alpha = 0.6) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "black") +
  labs(title = "Actual vs. Predicted Values",
       x = "Actual MPG",
       y = "Predicted MPG") +
  theme_minimal() +
  theme(legend.title = element_blank())






