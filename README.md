Education Model 

This repository contains the code for a project that predicts students' final mathematics grades (G3) using mulitple linear regression and feature selection techniques implemented in R. 


Code Overview 
The R implemented, specifically for the mathematics, performs the following tasks:

(1). Loads and preprocess the student performance dataset (mat.)

(2). Converts categorical variables into factors and removes G1 and G2 from the dataset 

(3). Splits the data inyo training and testing sets, then fits a multiple lienar regression model 

(4). Performs Best Subset, Foward Stepwise, and Backward Stepwise feature selection using leaps package

(5). Selects the models using Cp, BIC, and Adjsuted R^2

(6). Evaluates the selected models, included a model using all predicators, using Mean Squared Error on the test dataset 

Required R Packages: 
- leaps

Run the project 
1. Download the student performance dataset
2. Update the dataset path in mat-model.R if necessary
3. Install the required packages
4. Run mat-model.R

