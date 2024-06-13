# Acaademic Success Indicators

## Intro
What are the chances that a student drops out, stays enrolled, or graduates? Can we use this information to predict student outcome rates in the future? There are many variables that could go into this and affect the outcomes of the prediction. Here we find relevant variables and significant variables to make an efficient model. 
With the ability to create predictions on how students might perform in school depending on certain variables, we could create certain programs to prevent dropping out from happening. 

### Modeling
We settled on XGBoost after running through many different models including SVM's, KNN, Logistic Regression (one-one, one-all), CNNs, RNNs, and random forests. The small size of our data set and many categorical features made XGBoost perform quite well compared to the others.

## Outcome and Objective
We are trying to predict 3 possible outcomes for students: Enrolled, Graduated, and Dropped Out.
<img width="932" alt="Screenshot 2024-06-12 at 8 01 50 PM" src="https://github.com/RyanEurich/Aca-Success/assets/97063139/631c6a48-9c73-4109-9d06-6e3840ed20cf">

### Gender and Grade
<img width="1125" alt="Screenshot 2024-06-12 at 8 05 20 PM" src="https://github.com/RyanEurich/Aca-Success/assets/97063139/bf19a429-92aa-4525-84cf-c4ca75c3f33e">

### Course
<img width="837" alt="Screenshot 2024-06-12 at 8 07 00 PM" src="https://github.com/RyanEurich/Aca-Success/assets/97063139/2c8ae6c7-056c-4162-9ec3-75c0a9f2bfc9">

### XGBoost and SMOTE CN
First pass with XGBoost yielded 80% accuracy but the 3 categories confused most models. Eventually we would take out the enrolled category to compare against dropout vs graduated to have more clear decision boundaries.
<img width="681" alt="Screenshot 2024-06-12 at 8 09 14 PM" src="https://github.com/RyanEurich/Aca-Success/assets/97063139/dc1a2ff8-5692-4926-a839-7b6c68ccc3e9">

As expected, GPA and admission scores had the biggest impact in predicting dropout and success.
<img width="1057" alt="Screenshot 2024-06-12 at 8 08 39 PM" src="https://github.com/RyanEurich/Aca-Success/assets/97063139/b9f9d085-b836-4a42-8bdd-08ff71f2e121">

## XGBOOST 2-Way Class
We did k=3 cross validation to tune our hyperparameters as I did not have a bunch of compute lying around. We kept the categorical variables and used XGBoost's new categorical feature, imposing no limit on the max category one-hot split. One concern was sparcity as many of the columns had 100+ categories with only a few occurences of each type.

Final model after taking out the enrolled class.
<img width="1164" alt="Screenshot 2024-06-12 at 8 22 38 PM" src="https://github.com/RyanEurich/Aca-Success/assets/97063139/1f5fc41e-ea8f-4dd2-9c6a-3f4a113b1005">
Feature Importance stayed relatively similar
<img width="1198" alt="Screenshot 2024-06-12 at 8 24 19 PM" src="https://github.com/RyanEurich/Aca-Success/assets/97063139/452a29f1-154b-4a11-a18d-2d115e14a58b">
We still had some trouble predicting the True Positive class but when changing the scale position weight it hurt overall model acuracy so we scaled back.
<img width="1038" alt="Screenshot 2024-06-12 at 8 25 03 PM" src="https://github.com/RyanEurich/Aca-Success/assets/97063139/032c1e6f-3a17-4de4-97af-6849f23f3abb">

## Other Models
SVM's were used and got 90% accuracy across all 3 classes but had less tuning options. CNN's and RNN's were also used but did not yield any better results. We tuned across 1,2, and 3 layers along with different activation functions. A lack of data was also an issue with only 5000 rows and probably why simplier models such as the SVM performed so well.
