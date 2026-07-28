# Predict G3 (final scores) for Portugese subjects without G1 and G2 (previous scores)
# Load data 
por_student <- read.csv("student-por.csv", sep = ";")
str(por_student)

# Check for NA and duplicates
sum(is.na(por_student))
sum(duplicated(por_student))

# Factor char variables
char_factor <- c("school", "sex", "address", "famsize",
                 "Pstatus", "Mjob", "Fjob",
                 "reason", "guardian", "schoolsup", "famsup", "paid", "activities",
                 "nursery", "higher", "internet", "romantic")

por_student[char_factor] <- lapply(por_student[char_factor], as.factor)

# No G1 and G2
por_student$G1 <- NULL
por_student$G2 <- NULL

str(por_student)


library(leaps)
set.seed(1)
train <- sample(1:nrow(por_student), nrow(por_student) / 2)
train_data <- por_student[train, ]
test_data  <- por_student[-train, ]
# Best subset selection
regfit_full <- regsubsets(G3 ~ ., data = por_student, nvmax = 39)
reg_summary <- summary(regfit_full)


par(mfrow = c(2,2))
plot(reg_summary$rss, xlab = "Number of Variables", ylab = "RSS", type = "l")

plot(reg_summary$adjr2, xlab = "Number of Variables", ylab = "Adjusted RSq", type = "l")
adj_r2_max <- which.max(reg_summary$adjr2)
points(adj_r2_max, reg_summary$adjr2[adj_r2_max], col ="red", cex = 2, pch = 20)
coef(regfit_full, 24)
# schoolMS, sexM, age, addressU, famsizeLE3, Fedu, Mjobhealth, Mjobservices, Mjobteacher, 
# Fjobservices, Fjobteacher, reasonother, guardianmother, studytime, failures, schoolsupyes, 
# activitiesyes, higheryes, romanticyes, famrel, freetime, Dalc, health, absences

plot(reg_summary$cp, xlab = "Number of Variables", ylab = "Cp", type = "l")
cp_min <- which.min(reg_summary$cp) 
points(cp_min, reg_summary$cp[cp_min], col = "red", cex = 2, pch = 20)
coef(regfit_full, 15)
# schoolMS, sexM, age, Medu, Fjobteacher, reasonother, guardianmother, 
# studytime, failures, schoolsupyes, higheryes, romanticyes, 
# Dalc, health, absences

plot(reg_summary$bic, xlab = "Number of Variables", ylab = "BIC", type = "l")
bic_min <- which.min(reg_summary$bic) 
points(bic_min, reg_summary$bic[bic_min], col = "red", cex = 2, pch = 20)
coef(regfit_full, 8)
# schoolMS, Fedu, studytime, failures, schoolsupyes, higheryes, Dalc, health

# Forward selection
regfit_fwd <- regsubsets(G3 ~ ., data = por_student, nvmax = 39, method = "forward")
regfit_fwd_summary <- summary(regfit_fwd)

par(mfrow = c(2,2))
plot(regfit_fwd_summary$rss, xlab = "Number of Variables", ylab = "RSS", type = "l")

plot(regfit_fwd_summary$adjr2, xlab = "Number of Variables", ylab = "Adjusted RSq", type = "l")
adj_r2_max <- which.max(regfit_fwd_summary$adjr2)
points(adj_r2_max, regfit_fwd_summary$adjr2[adj_r2_max], col ="red", cex = 2, pch = 20)
coef(regfit_fwd, 26)

plot(regfit_fwd_summary$cp, xlab = "Number of Variables", ylab = "Cp", type = "l")
cp_min <- which.min(regfit_fwd_summary$cp) 
points(cp_min, regfit_fwd_summary$cp[cp_min], col = "red", cex = 2, pch = 20)
coef(regfit_fwd, 16) 

plot(regfit_fwd_summary$bic, xlab = "Number of Variables", ylab = "BIC", type = "l")
bic_min <- which.min(regfit_fwd_summary$bic) 
points(bic_min, regfit_fwd_summary$bic[bic_min], col = "red", cex = 2, pch = 20)
coef(regfit_fwd, 8)
# same as Best subset

# Backward selection
regfit_bwd <- regsubsets(G3 ~ ., data = por_student, nvmax = 39, method = "backward")
regfit_bwd_summary <- summary(regfit_bwd)

par(mfrow = c(2,2))
plot(regfit_bwd_summary$rss, xlab = "Number of Variables", ylab = "RSS", type = "l")

plot(regfit_bwd_summary$adjr2, xlab = "Number of Variables", ylab = "Adjusted RSq", type = "l")
adj_r2_max <- which.max(regfit_bwd_summary$adjr2) # 24
points(adj_r2_max, regfit_bwd_summary$adjr2[adj_r2_max], col ="red", cex = 2, pch = 20)
coef(regfit_bwd , 24)
# same as Best subset

plot(regfit_bwd_summary$cp, xlab = "Number of Variables", ylab = "Cp", type = "l")
cp_min <- which.min(regfit_bwd_summary$cp)
points(cp_min, regfit_bwd_summary$cp[cp_min], col = "red", cex = 2, pch = 20)
coef(regfit_bwd , 18)

plot(regfit_bwd_summary$bic, xlab = "Number of Variables", ylab = "BIC", type = "l")
bic_min <- which.min(regfit_bwd_summary$bic) 
points(bic_min, regfit_bwd_summary$bic[bic_min], col = "red", cex = 2, pch = 20)
coef(regfit_bwd , 8)
# schoolMS, Fjobteacher, studytime, failures, schoolsupyes, higheryes, Dalc, health
# The best eight-variable models identified by the best subset selection and forward stepwise selection are the same
# but are different from the backward stepwise selection.


# Validation-Set 
regfit_best_train <- regsubsets(G3 ~ ., data = train_data, nvmax = 39)
train_summary <- summary(regfit_best_train)
test_mat <- model.matrix(G3 ~ ., data = test_data)


val_errors <- rep(NA, 39)
# Iterates over each size i
for(i in 1:39){
  # Extract the vector of predictors in the best fit model on i predictors
  coefi <- coef(regfit_best_train, id = i)
  # Make predictions using matrix multiplication of the test matrix and the coefficients vector
  pred <- test_mat[,names(coefi)] %*% coefi
  # Calculate the MSE
  val_errors[i] <- mean((test_data$G3 - pred)^2)
}

min <- which.min(val_errors) # 16
coef(regfit_best_train, min)

# Plot the errors for each model size
plot(val_errors, type = 'b')
points(min, val_errors[min][1], col = "red", cex = 2, pch = 20)

predict.regsubsets <- function(object, newdata, id, ...) {
  form  <- as.formula(object$call[[2]])
  mat   <- model.matrix(form, newdata)
  coefi <- coef(object, id = id)
  mat[, names(coefi)] %*% coefi
}

# Cross-validation
k <- 10
n <- nrow(por_student)
set.seed(1)
folds <- sample(rep(1:k, length = n))
cv_errors <- matrix(NA, k, 39,
                    dimnames = list(NULL, paste(1:39)))
for (j in 1:k) {
  best_fit <- regsubsets(G3 ~ .,
                         data = por_student[folds != j, ],
                         nvmax = 39)
  for (i in 1:39) {
    pred <- predict(best_fit, por_student[folds == j, ], id = i)
    cv_errors[j, i] <-
      mean((por_student$G3[folds == j] - pred)^2)
  }
}

mean_cv_errors <- apply(cv_errors, 2, mean)
min_cv = which.min(mean_cv_errors) # 35

par(mfrow = c(1, 1))
plot(mean_cv_errors, type = 'b')
points(min_cv, mean_cv_errors[min_cv][1], col = "red", cex = 2, pch = 20)

# Full linear model
full_fit  <- lm(G3 ~ ., data = train_data)
summary(full_fit)
full_pred <- predict(full_fit, test_data)

regfit_fwd_train  <- regsubsets(G3 ~ ., data = train_data, nvmax = 39, method = "forward")
regfit_bwd_train  <- regsubsets(G3 ~ ., data = train_data, nvmax = 39, method = "backward")

fwd_train_summary <- summary(regfit_fwd_train)
bwd_train_summary <- summary(regfit_bwd_train)

cp_pred    <- predict(regfit_best_train, test_data, id = which.min(train_summary$cp))
bic_pred   <- predict(regfit_best_train, test_data, id = which.min(train_summary$bic))
adjr2_pred <- predict(regfit_best_train, test_data, id = which.max(train_summary$adjr2))
fwd_bic_pred <- predict(regfit_fwd_train, test_data, id = which.min(fwd_train_summary$bic))
bwd_bic_pred <- predict(regfit_bwd_train, test_data, id = which.min(bwd_train_summary$bic))

# Regression model comparison table
regression_results <- data.frame(
  Model = c("Full linear model", "Best subset (Cp)",
            "Best subset (BIC)", "Best subset (AdjR2)", "Forward (BIC)", "Backward (BIC)"),
  Test_MSE = c(
    mean((full_pred - test_data$G3)^2),
    mean((cp_pred - test_data$G3)^2),
    mean((bic_pred - test_data$G3)^2),
    mean((adjr2_pred - test_data$G3)^2),
    mean((fwd_bic_pred - test_data$G3)^2),
    mean((bwd_bic_pred - test_data$G3)^2)
  )
)
regression_results[order(regression_results$Test_MSE), ]

# Backward's BIC model (8.925) slightly outperforms forward/best-subset's BIC model (9.056)

