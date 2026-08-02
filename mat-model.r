## Prediction G3 W/O G1 OR G2
## Tsithembile tembo
## CSP 578

# load data from desktop
math_student <- read.csv("../student-mat.csv", sep = ";")
str(math_student)

#clean dataset
sum(is.na(math_student))
sum(duplicated(math_student))

#factor for the ones that are not int but char
char_factor <- c("school", "sex", "address", "famsize",
"Pstatus", "Mjob", "Fjob",
"reason", "guardian", "schoolsup", "famsup", "paid", "activities",
"nursery", "higher", "internet", "romantic")

math_student[char_factor] <- lapply(math_student[char_factor], as.factor)

#removing G1 and G2
math_student$G1 <- NULL
math_student$G2 <- NULL

str(math_student)

#look at features and see which is most significant
all.fit <- lm(G3 ~ ., data = math_student)
summary(all.fit)
#many of the features are not significant and
#this model is weak, probably due to leaving out G1 and G3



#splitting the data up
set.seed(13)
train <- sample(1:nrow(math_student), nrow(math_student) * 0.6)
train_data <- math_student[train, ]
test_data <- math_student[-train, ]



#BEST SUBSET SELECTION PROCESS
library(leaps)
regfit_full <- regsubsets(G3 ~ ., data = train_data, nvmax = 39) ##lot more features becuase of sub categories. 
reg_summary <- summary(regfit_full)
#print(reg_summary)

#Cp, BIC, adhjusted R^2
which.min(reg_summary$cp)
which.min(reg_summary$bic)
which.max(reg_summary$adjr2)
#Variables -> Cp = 6, BIC = 1, ADJR = 15
coef(regfit_full, 6)
#sexM, age, Medu, guadrianother, failures, romaticyes
coef(regfit_full, 1)
#failures
coef(regfit_full, 15)
#sexM, age, addressU, PstatusT, Medu, reasonother,
#reasonreputation, guardianother, failures,
#schoolsupyes, paidyes, nurseryyes, higheryes, romanticyes, gout

#fit and predit model for Cp
CPbest_fit <- lm(G3 ~  sex + age + Medu + guardian + failures + romantic, data = train_data)
summary(CPbest_fit)
CPBest_pred <- predict(CPbest_fit, test_data)
mean((CPBest_pred - test_data$G3)^2) # 16.63068

#fit and predict model for BIC
BICbest_fit <- lm(G3 ~ failures, data = train_data)
summary(BICbest_fit)
BICBest_pred <- predict(BICbest_fit, test_data)
mean((BICBest_pred - test_data$G3)^2) #15.8463, the best model where the only feature was failures 

#fit and predict model for adjR^2
ADJbest_fit <- lm(G3 ~ sex + age + address + Pstatus + Medu + reason + guardian + failures +
                   schoolsup + paid + nursery + higher + romantic + goout, data = train_data)
summary(ADJbest_fit)
ADJBest_pred <- predict(ADJbest_fit, test_data)
mean((ADJBest_pred - test_data$G3)^2) #17.1042


#FOWARD SELECTION 
regfit_foward <- regsubsets(G3 ~., data = train_data, nvmax = 39, method = "forward")
reg_summary_forward <- summary(regfit_foward)

#Cp, BIC, adhjusted R^2
which.min(reg_summary_forward$cp) #6
which.min(reg_summary_forward$bic) #1
which.max(reg_summary_forward$adjr2) #15
#same as the best selection modethod.
#Variables -> Cp = 6, BIC = 1, ADJR = 15
coef(regfit_foward, 6)
#sexM, age, Medu, guadrianother, failures, romaticyes
coef(regfit_foward, 1)
#failures
coef(regfit_foward, 15)
#sexM, age, addressU, PstatusT, Medu, reasonother,
#reasonreputation, guardianother, failures,
#schoolsupyes, paidyes, nurseryyes, higheryes, romanticyes, gout
#no need to fit and predict with three more models

#BACKWARD SELECTION METHOD
regfit_backward <- regsubsets(G3 ~., data = train_data, nvmax = 39, method = "backward")
reg_summary_backward <- summary(regfit_backward)

#Cp, BIC, adhjusted R^2
which.min(reg_summary_backward$cp) #6 
which.min(reg_summary_backward$bic) #1
which.max(reg_summary_backward$adjr2) #15

#same as the best selection modethod and foward method
#Variables -> Cp = 6, BIC = 1, ADJR = 15
coef(regfit_backward, 6)
#sexM, age, Medu, guadrianother, failures, romaticyes
coef(regfit_backward, 1)
#failures
coef(regfit_backward, 15)
#sexM, age, addressU, PstatusT, Medu, reasonother,
#reasonreputation, guardianother, failures,
#schoolsupyes, paidyes, nurseryyes, higheryes, romanticyes, gout
#no need to fit and predict with three more models

##NOT SELECTED, SO ALL PREDICTORS
full_fit <- lm(G3 ~ ., data = train_data)
summary(full_fit)
full_pred <- predict(full_fit, test_data)
mean((full_pred - test_data$G3)^2) #15.82758


#Full Predictor Model: better prediction, more complex cause of num. of preds.
#BIC Model:almost the same prediction as BIC, jsut more simpler for num. of preds.
#Cp/AdjR^2: bigger model but worse result than full and bic


# Visualizations
# 1. why are failures the strognest predictors
pdf("mat-visualizations/Failures_vs_G3.pdf", width = 7, height = 5)
boxplot(G3 ~ failures,
        data = math_student,
        main = "Final Grade by Number of Previous Failures",
        xlab = "Number of Previous Failures",
        ylab = "Final Grade")
dev.off()

#2. Feature Selection 
pdf("mat-visualizations/Model_Selection.pdf", width = 7, height = 5)
par(mfrow = c(1, 3))
plot(reg_summary$cp,
     type = "l",
     xlab = "Number of Variables",
     ylab = "Cp",
     main = "CP")

plot(reg_summary$bic,
     type = "l",
     xlab = "Number of Variables",
     ylab = "BIC",
     main = "BIC")

plot(reg_summary$adjr2,
     type = "l",
     xlab = "Number of Variables",
     ylab = "Adjusted R²",
     main = "Adjusted R²")

dev.off()

#3. Test MSE Comparsion 
pdf("mat-visualizations/TestMSE_Comparison.pdf", width = 7, height = 5)
mse <- c(Full = 15.82758, 
        Cp = 16.63068, 
        BIC = 15.84630, 
        Adjr2 = 17.10420)
barplot(mse, ylab = "Test MSE", 
        main = "Model Comaprison")
dev.off()

#4. Predicted Values vs Actual Values
pdf("mat-visualizations/Predicted_VS_Actual.pdf", width = 7, height = 5)
plot(test_data$G3,
     full_pred,
     xlab = "Actual G3",
     ylab = "Predicted G3",
     main = "Predicted vs Actual")
abline(0, 1, col = "red")
dev.off()
