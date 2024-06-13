dat3 <- read.csv("aca.csv")

dat3$Marital.status <- as.factor(dat3$Marital.status)
dat3$Application.mode <- as.factor(dat3$Application.mode)
dat3$Application.order <- as.factor(dat3$Application.order)
dat3$Course <- as.factor(dat3$Course)
dat3$Daytime.evening.attendance. <- as.factor(dat3$Daytime.evening.attendance.)
dat3$Previous.qualification <- as.factor(dat3$Previous.qualification)
dat3$Nacionality <- as.factor(dat3$Nacionality)
dat3$Mother.s.qualification <- as.factor(dat3$Mother.s.qualification)
dat3$Father.s.qualification <- as.factor(dat3$Father.s.qualification)
dat3$Mother.s.occupation <- as.factor(dat3$Mother.s.occupation)
dat3$Father.s.occupation <- as.factor(dat3$Father.s.occupation)
dat3$Father.s.occupation <- as.factor(dat3$Father.s.occupation)
dat3$Displaced <- as.factor(dat3$Displaced)
dat3$Displaced <- as.factor(dat3$Displaced)
dat3$Educational.special.needs <- as.factor(dat3$Educational.special.needs)
dat3$Debtor <- as.factor(dat3$Debtor)
dat3$Tuition.fees.up.to.date <- as.factor(dat3$Tuition.fees.up.to.date)
dat3$Gender <- as.factor(dat3$Gender)
dat3$Scholarship.holder <- as.factor(dat3$Scholarship.holder)
dat3$International <- as.factor(dat3$International)

str(dat3)
dat3$Target <- as.factor(dat3$Target)


install.packages('caTools') 
library(caTools) 


set.seed(123) 
split = sample.split(dat3$Target, SplitRatio = 0.75) 

training_set = subset(dat3, split == TRUE) 
test_set = subset(dat3, split == FALSE) 

#scale train
str(training_set)
training_set$Admission.grade = scale(training_set$Admission.grade) 
training_set$Previous.qualification..grade. = scale(training_set$Previous.qualification..grade.)
training_set$Age.at.enrollment = scale(training_set$Age.at.enrollment)
training_set[22:36] = scale(training_set[22:36])
str(training_set)

#scale test
test_set$Admission.grade = scale(test_set$Admission.grade) 
test_set$Previous.qualification..grade. = scale(test_set$Previous.qualification..grade.)
test_set$Age.at.enrollment = scale(test_set$Age.at.enrollment)
test_set[22:36] = scale(test_set[22:36])


#model fit linear
install.packages('e1071') 
library(e1071) 

classifier = svm(formula = Target ~ ., 
                 data = training_set, 
                 type = 'C-classification', 
                 kernel = 'linear') 
classifier


y_pred = predict(classifier, newdata = test_set[-37])


cm = table(test_set[, 37], y_pred)
cm

plot(classifier,training_set)

#78 accuracy
#21.3 percent false negative for dropout



#model fit non-linear (Radial)
classifier2 = svm(formula = Target ~ ., 
                 data = training_set, 
                 type = 'C-classification', 
                 kernel = 'radial') 
classifier2


y_pred2 = predict(classifier2, newdata = test_set[-37])


cm = table(test_set[, 37], y_pred2)
cm



#model fit non-linear (polynomial)
classifier3 = svm(formula = Target ~ ., 
                  data = training_set, 
                  type = 'C-classification', 
                  kernel = 'polynomial',
                  degree = 2)

classifier3


y_pred3 = predict(classifier3, newdata = test_set[-37])


cm = table(test_set[, 37], y_pred3)
cm



#knn


install.packages("caTools") 
install.packages("class") 
library(caTools) 
library(class)

classifier_knn <- knn(train = training_set, 
                      test = test_set, 
                      cl = training_set$Target, 
                      k = 1) 
classifier_knn 

library(ggplot2)
ggplot(data = dat3, aes(x = Father.s.occupation, fill = Target)) +
  geom_bar()

ggplot(data = dat3, aes(x = Father.s.qualification, fill = Target)) +
  geom_bar()

ggplot(data = dat3, aes(x = Mother.s.occupation, fill = Target)) +
  geom_bar()

ggplot(data = dat3, aes(x = Mother.s.qualification, fill = Target)) +
  geom_bar()

#####
#try removing parents qualifications
####
dat3_new <- dat3[, -which(names(dat3) == "Mother.s.qualification")]
dat3_new <- dat3_new[, -which(names(dat3_new) == "Father.s.qualification")]
dat3_new <- dat3_new[, -which(names(dat3_new) == "Mother.s.occupation")]
str(dat3_new)
dat3_new <- dat3_new[, -which(names(dat3_new) == "Father.s.occupation")]

str(dat3_new)
#split
set.seed(123) 
split_new = sample.split(dat3_new$Target, SplitRatio = 0.75) 

training_set_new = subset(dat3_new, split == TRUE) 
test_set_new = subset(dat3_new, split == FALSE) 

#still need to run
#scale train
str(training_set_new)
training_set_new$Admission.grade = scale(training_set_new$Admission.grade) 
training_set_new$Previous.qualification..grade. = scale(training_set_new$Previous.qualification..grade.)
training_set_new$Age.at.enrollment = scale(training_set_new$Age.at.enrollment)
training_set_new[18:32] = scale(training_set_new[18:32])
str(training_set_new)

#scale test
test_set_new$Admission.grade = scale(test_set_new$Admission.grade) 
test_set_new$Previous.qualification..grade. = scale(test_set_new$Previous.qualification..grade.)
test_set_new$Age.at.enrollment = scale(test_set_new$Age.at.enrollment)
test_set_new[18:32] = scale(test_set_new[18:32])


test_set_new
str(test_set_new)
str(training_set_new)
library(e1071)
#new run with no parents
classifier_no_parents = svm(formula = Target ~ ., 
                 data = training_set_new, 
                 type = 'C-classification', 
                 kernel = 'linear',
                 degree = 3) 
classifier_no_parents


y_pred_no_parents = predict(classifier_no_parents, newdata = test_set_new[-33])


cm = table(test_set_new[, 33], y_pred_no_parents)
cm

plot(classifier_no_parents,training_set_new)




#put them back in

course_test <- dat3 %>%
  groupby(Course) %>%
  summarize()

####################Final XgBoost######################
library(caret)
library(ROSE)
library(xgboost)

# Extract features and target variable
feature <- subset(df_set, select = -c(Target))
target <- df_set$Target

# Split data into training and testing sets
set.seed(1)
trainIndex <- createDataPartition(target, p = 0.8, list = FALSE)
X_train <- feature[trainIndex, ]
X_test <- feature[-trainIndex, ]
y_train <- target[trainIndex]
y_test <- target[-trainIndex]

# Perform oversampling using SMOTE + ENN
smenn <- ovun.smoteenn(Target ~ ., data = cbind(X_train, y_train), seed = 123)
X_train_smenn <- smenn$data[, -ncol(smenn$data)]
y_train_smenn <- as.factor(smenn$data[, ncol(smenn$data)])

# Fit the XGBoost classifier
xgb_classifier <- xgboost(data = as.matrix(X_train_smenn),
                          label = as.integer(y_train_smenn),
                          nrounds = 90,
                          max_depth = 4,
                          eta = 0.4,
                          objective = "binary:logistic",
                          verbose = FALSE)

# Make predictions using the model
y_pred <- predict(xgb_classifier, as.matrix(X_test))

# Calculate evaluation metrics
accuracy <- sum(as.integer(y_pred > 0.5) == as.integer(y_test)) / length(y_test)
kappa <- caret::cohenKappa(as.integer(y_pred > 0.5), as.integer(y_test))
f1 <- F1_Score(as.integer(y_test), as.integer(y_pred > 0.5))
recall <- Recall(as.integer(y_test), as.integer(y_pred > 0.5))
precision <- Precision(as.integer(y_test), as.integer(y_pred > 0.5))

# Print the evaluation metrics
cat("Results With Oversampling SMOTE + ENN\n")
cat("XGBoost Accuracy:", accuracy, "\n")
cat("Kappa Score:", kappa, "\n")
cat("F1 Score:", f1, "\n")
cat("Recall:", recall, "\n")
cat("Precision:", precision, "\n")

# Print confusion matrix
cm <- confusionMatrix(data = factor(y_pred > 0.5, levels = c(FALSE, TRUE)),
                      reference = factor(y_test, levels = c(FALSE, TRUE)))
print(cm)


library(xgboost)
library(caret)

names(df_set) <- c('Marital status', 'Application mode', 'Application order', 'Course', 'Daytime/evening attendance',
                   'Previous qualification', 'Nacionality', 'Mothers qualification', 'Fathers qualification',
                   'Mothers occupation', 'Fathers occupation', 'Displaced', 'Educational special needs', 'Debtor',
                   'Tuition fees up to date', 'Gender', 'Scholarship holder', 'International', 'Target')


cat_cols <- c('Marital status', 'Application mode', 'Application order', 'Course', 'Daytime/evening attendance',
              'Previous qualification', 'Nacionality', 'Mothers qualification', 'Fathers qualification',
              'Mothers occupation', 'Fathers occupation', 'Displaced', 'Educational special needs', 'Debtor',
              'Tuition fees up to date', 'Gender', 'Scholarship holder', 'International')
df_set[cat_cols] <- lapply(df_set[cat_cols], as.factor)


df_set <- subset(df_set, select = -c(GDP, 'Inflation rate', 'Unemployment rate', 'Application mode', 'Application order',
                                     'International', 'Educational special needs'))


df_Combine <- df_set
df_Enroll_Drop <- df_set
df_Grad_Drop <- df_set


df_Enroll_Drop <- df_Enroll_Drop[df_Enroll_Drop$Target != 'Enrolled', ]
df_Grad_Drop <- df_Grad_Drop[df_Grad_Drop$Target != 'Graduate', ]


target_mapping <- c('Graduate' = 2, 'Enrolled' = 1, 'Dropout' = 0)
target_mapping_Combine <- c('Graduate' = 1, 'Enrolled' = 1, 'Dropout' = 0)
df_set$Target <- as.integer(factor(df_set$Target, levels = names(target_mapping), labels = target_mapping))
df_Combine$Target <- as.integer(factor(df_Combine$Target, levels = names(target_mapping_Combine), labels = target_mapping_Combine))

feature_Enroll <- subset(df_Enroll_Drop, select = -c(Target))
feature_Graduate <- subset(df_Grad_Drop, select = -c(Target))


target_Enroll <- as.factor(ifelse(df_Enroll_Drop$Target == 'Enrolled', 1, 0))
target_Graduate <- as.factor(ifelse(df_Grad_Drop$Target == 'Graduate', 1, 0))

# Split the data into training and testing sets
set.seed(123) # for reproducibility
trainIndex <- createDataPartition(target_Enroll, p = .8, 
                                  list = FALSE,
                                  times = 1)
X_train_Enroll <- feature_Enroll[trainIndex, ]
X_test_Enroll <- feature_Enroll[-trainIndex, ]
y_train_Enroll <- target_Enroll[trainIndex]
y_test_Enroll <- target_Enroll[-trainIndex]

#had more values just included these
xgb_classifier_Enroll <- xgboost(data = as.matrix(X_train_Enroll), 
                                 label = as.integer(y_train_Enroll),
                                 nrounds = 75,
                                 max_depth = 6,
                                 eta = 0.1,
                                 gamma = 0.1,
                                 scale_pos_weight = 0.40,
                                 lambda = 1,
                                 alpha = 1,
                                 objective = 'binary:logistic')


y_pred_Enroll <- predict(xgb_classifier_Enroll, as.matrix(X_test_Enroll))


accuracy_E <- sum(as.integer(y_pred_Enroll > 0.5) == as.integer(y_test_Enroll)) / length(y_test_Enroll)
kappa_E <- caret::cohenKappa(as.integer(y_pred_Enroll > 0.5), as.integer(y_test_Enroll))
f1_E <- F1_Score(as.integer(y_test_Enroll), as.integer(y_pred_Enroll > 0.5))
recall_E <- Recall(as.integer(y_test_Enroll), as.integer(y_pred_Enroll > 0.5))
precision_E <- Precision(as.integer(y_test_Enroll), as.integer(y_pred_Enroll > 0.5))


print("Results With Oversampling Smote + ENN")
print("XGBoost Accuracy:", accuracy_E)
print("Kappa Score:", kappa_E)
print("F1 Score:", f1_E)
print("Recall:", recall_E
      
      
      
  
