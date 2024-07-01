install.packages('caret')
install.packages('pROC')
install.packages('rpart')
install.packages('rpart.plot')

library(caret)
library(pROC)
library(rpart)
library(rpart.plot)

df=read.csv("D:/Bootcamp VCU datasets/car_data.csv")
head(df)

df$Purchased =as.factor(df$Purchased)

# Split the dataset into training and testing sets
set.seed(123)
trainIndex <- createDataPartition(df$Purchased, p = .8, list = FALSE, times = 1)
df_train <- df[trainIndex,]
df_test <- df[-trainIndex,]

# Logistic Regression
logistic_model <- train(Purchased ~ Age + AnnualSalary, data = df_train, method = 'glm', family = 'binomial')
logistic_pred <- predict(logistic_model, newdata = df_test)
logistic_prob <- predict(logistic_model, newdata = df_test, type = "prob")[,2]

# Confusion Matrix
logistic_cm <- confusionMatrix(logistic_pred, df_test$Purchased)
print(logistic_cm)

# ROC Curve and AUC
logistic_roc <- roc(df_test$Purchased, logistic_prob)
plot(logistic_roc, col = "blue", main = "ROC Curve (Logistic Regression)")
abline(a=0, b=1, col="red", lty=2)
logistic_auc <- auc(logistic_roc)
print(logistic_auc)

# Precision, Recall, F1-Score
logistic_metrics <- logistic_cm$byClass
logistic_accuracy <- logistic_cm$overall['Accuracy']

# Decision Tree
tree_model <- rpart(Purchased ~ Age + AnnualSalary, data = df_train, method = "class")
tree_pred <- predict(tree_model, newdata = df_test, type = "class")
tree_prob <- predict(tree_model, newdata = df_test, type = "prob")[,2]

# Confusion Matrix
tree_cm <- confusionMatrix(tree_pred, df_test$Purchased)
print(tree_cm)

# ROC Curve and AUC
tree_roc <- roc(df_test$Purchased, tree_prob)
plot(tree_roc, col = "blue", main = "ROC Curve (Decision Tree)")
abline(a=0, b=1, col="red", lty=2)
tree_auc <- auc(tree_roc)
print(tree_auc)

# Precision, Recall, F1-Score
tree_metrics <- tree_cm$byClass
tree_accuracy <- tree_cm$overall['Accuracy']


# Comparison DataFrame
comparison_df <- data.frame(
  Model = c('Logistic Regression', 'Logistic Regression', 'Decision Tree', 'Decision Tree'),
  Class = c('Class 0', 'Class 1', 'Class 0', 'Class 1'),
  Precision = c(logistic_metrics['Pos Pred Value'], logistic_metrics['Neg Pred Value'], 
                tree_metrics['Pos Pred Value'], tree_metrics['Neg Pred Value']),
  Recall = c(logistic_metrics['Sensitivity'], logistic_metrics['Specificity'], 
             tree_metrics['Sensitivity'], tree_metrics['Specificity']),
  F1_Score = c(2 * logistic_metrics['Pos Pred Value'] * logistic_metrics['Sensitivity'] / 
                 (logistic_metrics['Pos Pred Value'] + logistic_metrics['Sensitivity']),
               2 * logistic_metrics['Neg Pred Value'] * logistic_metrics['Specificity'] / 
                 (logistic_metrics['Neg Pred Value'] + logistic_metrics['Specificity']),
               2 * tree_metrics['Pos Pred Value'] * tree_metrics['Sensitivity'] / 
                 (tree_metrics['Pos Pred Value'] + tree_metrics['Sensitivity']),
               2 * tree_metrics['Neg Pred Value'] * tree_metrics['Specificity'] / 
                 (tree_metrics['Neg Pred Value'] + tree_metrics['Specificity'])),
  Accuracy = c(logistic_accuracy, logistic_accuracy, tree_accuracy, tree_accuracy),
  AUC = c(logistic_auc, logistic_auc, tree_auc, tree_auc)
)

# Reorder columns
comparison_df <- comparison_df[, c('Model', 'Class', 'Precision', 'Recall', 'F1_Score', 'Accuracy', 'AUC')]

# Print the comparison table
print(comparison_df)