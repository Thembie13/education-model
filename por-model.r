
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

# REGRESSION (Predicting G3)
## Baseline
baseline_pred <- mean(por_student$G3)
baseline_mse  <- mean((baseline_pred - por_student$G3)^2)
baseline_mse

## Train/Test split
set.seed(13)
train <- sample(1:nrow(por_student), nrow(por_student) * 0.6)
train_data <- por_student[train, ]
test_data  <- por_student[-train, ]

## Full linear model
full_fit  <- lm(G3 ~ ., data = train_data)
summary(full_fit)
full_pred <- predict(full_fit, test_data)
mean((full_pred - test_data$G3)^2)

## Best/Forward/Backward subset selection (Cp, BIC, AdjR2)
library(leaps)

regfit_full <- regsubsets(G3 ~ ., data = train_data, nvmax = 30)
reg_summary <- summary(regfit_full)

which.min(reg_summary$cp)
which.min(reg_summary$bic)
which.max(reg_summary$adjr2)

coef(regfit_full, which.min(reg_summary$cp))
coef(regfit_full, which.min(reg_summary$bic))
coef(regfit_full, which.max(reg_summary$adjr2))

par(mfrow = c(1, 3))
plot(reg_summary$cp,    xlab = "Number of variables", ylab = "Cp",     type = "b")
plot(reg_summary$bic,   xlab = "Number of variables", ylab = "BIC",    type = "b")
plot(reg_summary$adjr2, xlab = "Number of variables", ylab = "Adj R2", type = "b")
par(mfrow = c(1, 1))

regfit_forward <- regsubsets(G3 ~ ., data = train_data, nvmax = 30, method = "forward")
reg_summary_forward <- summary(regfit_forward)
which.min(reg_summary_forward$cp)
which.min(reg_summary_forward$bic)
which.max(reg_summary_forward$adjr2)

regfit_backward <- regsubsets(G3 ~ ., data = train_data, nvmax = 30, method = "backward")
reg_summary_backward <- summary(regfit_backward)
which.min(reg_summary_backward$cp)
which.min(reg_summary_backward$bic)
which.max(reg_summary_backward$adjr2)

## predict.regsubsets helper (regsubsets has no built-in predict())
predict.regsubsets <- function(object, newdata, id, ...) {
  form  <- as.formula(object$call[[2]])
  mat   <- model.matrix(form, newdata)
  coefi <- coef(object, id = id)
  mat[, names(coefi)] %*% coefi
}

cp_pred    <- predict(regfit_full, test_data, id = which.min(reg_summary$cp))
bic_pred   <- predict(regfit_full, test_data, id = which.min(reg_summary$bic))
adjr2_pred <- predict(regfit_full, test_data, id = which.max(reg_summary$adjr2))

mean((cp_pred    - test_data$G3)^2)
mean((bic_pred   - test_data$G3)^2)
mean((adjr2_pred - test_data$G3)^2)

## Regression model comparison table
regression_results <- data.frame(
  Model = c("Baseline (mean)", "Full linear model", "Best subset (Cp)",
            "Best subset (BIC)", "Best subset (AdjR2)"),
  Test_MSE = c(
    baseline_mse,
    mean((full_pred - test_data$G3)^2),
    mean((cp_pred - test_data$G3)^2),
    mean((bic_pred - test_data$G3)^2),
    mean((adjr2_pred - test_data$G3)^2)
  )
)
regression_results[order(regression_results$Test_MSE), ]