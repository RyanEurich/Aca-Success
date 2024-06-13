library(nnet)

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

#
nnt2 <- nnet(Target ~ ., data = training_set, 
             size = 15, rang = 0.5, entropy = T,
             decay = 0.005, maxit = 5000,
             MaxNWts = 10000)


nnt3 <- nnet(Target ~ ., data = training_set, 
             size = 20, rang = 0.5, entropy = T,
             decay = 0.005, maxit = 5000,
             MaxNWts = 10000)

nnt4 <- nnet(Target ~ ., data = training_set, 
             size = 25, rang = 0.5, entropy = T,
             decay = 0.005, maxit = 5000,
             MaxNWts = 10000)

nnt5 <- nnet(Target ~ ., data = training_set, 
             size = 30, rang = 0.5, entropy = T,
             decay = 0.005, maxit = 10000,
             MaxNWts = 10000)

pred2 <- predict(nnt2, test_set)
pred3 <- predict(nnt3, test_set)
pred4 <- predict(nnt4, test_set)
pred5 <- predict(nnt5, test_set)
check = as.numeric(test_set$Target) == max.col(pred5)
accuracy = (sum(check)/nrow(test_set))*100
print(accuracy)

library(neuralnet)
library(nnet)
nn.out2 <- neuralnet(Target ~., data=training_set, 
                     hidden = c(10, 5),
                     stepmax = 10000,
                     linear.output = FALSE,
                     lifesign = "minimal")



#hyper parameter tuning
library(caret)


param_grid <- expand.grid(
  size = c(5, 10, 15,20,25, 30, 35,40,45),       # hidden layer sizes
  decay = c(0, 1e-5, 1e-4, 0.001, 0.005, 0.01, 0.05, 0.1, 0.2, 0.5)
)

ctrl <- trainControl(method = "cv", number = 10)
set.seed(123)
nnt5_cv_decay <- train(Target ~ ., 
                       data = training_set, 
                       method = "nnet",
                       trControl = ctrl,
                       tuneGrid = param_grid,
                       trace = TRUE,       # suppress output
                       linout = TRUE,       # linear output for regression
                       maxit = 15000,        # max iterations
                       MaxNWts = 50000,
                       lifesign = "minimal") 
# Print best parameters
print(nnt5_cv_expanded$bestTune)

# Plot results
plot(nnt5_cv_expanded)

xyplot(nnt5_cv_expanded, metric = "RMSE")  # or "Accuracy" for classification

