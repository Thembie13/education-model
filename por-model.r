# Predict G3 (final scores) for Portuguese subjects without G1 and G2 (previous scores)
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

summary(por_student$G3)
sd(por_student$G3)
cat("Mean:", round(mean(por_student$G3), 2),
    "| Median:", median(por_student$G3),
    "| SD:", round(sd(por_student$G3), 2),
    "| Range:", paste(range(por_student$G3), collapse = "\u2013"), "\n")
cat("Number of students scoring 0:", sum(por_student$G3 == 0),
    sprintf("(%.1f%%)", 100 * mean(por_student$G3 == 0)), "\n")

# Histogram
hist(por_student$G3,
     breaks = seq(-0.5, 19.5, by = 1),
     col = "#3B6EA5",
     border = "white",
     main = paste0("Distribution of Final Grades (G3), n = ", nrow(por_student)),
     xlab = "Final Grade (G3)",
     ylab = "Number of Students")

library(leaps)
set.seed(1)
train <- sample(1:nrow(por_student), nrow(por_student) / 2)
train_data <- por_student[train, ]
test_data  <- por_student[-train, ]

# Best subset selection
regfit_full <- regsubsets(G3 ~ ., data = train_data, nvmax = 39)
reg_summary <- summary(regfit_full)

par(mfrow = c(2,2))
plot(reg_summary$rss, xlab = "Number of Variables", ylab = "RSS", type = "l")

plot(reg_summary$adjr2, xlab = "Number of Variables", ylab = "Adjusted RSq", type = "l")
adj_r2_max <- which.max(reg_summary$adjr2)
points(adj_r2_max, reg_summary$adjr2[adj_r2_max], col ="red", cex = 2, pch = 20)
coef(regfit_full, 24)
# schoolMS, sexM, age, famsizeLE3, Mjobhealth, Mjobother,Mjobservices, Mjobteacher,
# Fjobservices, Fjobteacher, reasonother, guardianmother, studytime, failures,
# schoolsupyes, paidyes, activitiesyes, higheryes, romanticyes, famrel,
# freetime, goout, health, absences

plot(reg_summary$cp, xlab = "Number of Variables", ylab = "Cp", type = "l")
cp_min <- which.min(reg_summary$cp) 
points(cp_min, reg_summary$cp[cp_min], col = "red", cex = 2, pch = 20)
coef(regfit_full, 15)
# schoolMS, sexM, age, Medu, Fjobteacher, reasonother, studytime, failures,
# schoolsupyes, activitiesyes, higheryes, romanticyes, freetime, Walc, absences

plot(reg_summary$bic, xlab = "Number of Variables", ylab = "BIC", type = "l")
bic_min <- which.min(reg_summary$bic) 
points(bic_min, reg_summary$bic[bic_min], col = "red", cex = 2, pch = 20)
coef(regfit_full, 8)
# schoolMS, age, Fjobteacher, studytime, failures, schoolsupyes, higheryes, romanticyes

# Forward selection
regfit_fwd <- regsubsets(G3 ~ ., data = train_data, nvmax = 39, method = "forward")
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

# Backward selection
regfit_bwd <- regsubsets(G3 ~ ., data = train_data, nvmax = 39, method = "backward")
regfit_bwd_summary <- summary(regfit_bwd)

par(mfrow = c(2,2))
plot(regfit_bwd_summary$rss, xlab = "Number of Variables", ylab = "RSS", type = "l")

plot(regfit_bwd_summary$adjr2, xlab = "Number of Variables", ylab = "Adjusted RSq", type = "l")
adj_r2_max <- which.max(regfit_bwd_summary$adjr2) # 24
points(adj_r2_max, regfit_bwd_summary$adjr2[adj_r2_max], col ="red", cex = 2, pch = 20)
coef(regfit_bwd , 24)

plot(regfit_bwd_summary$cp, xlab = "Number of Variables", ylab = "Cp", type = "l")
cp_min <- which.min(regfit_bwd_summary$cp)
points(cp_min, regfit_bwd_summary$cp[cp_min], col = "red", cex = 2, pch = 20)
coef(regfit_bwd , 18)

plot(regfit_bwd_summary$bic, xlab = "Number of Variables", ylab = "BIC", type = "l")
bic_min <- which.min(regfit_bwd_summary$bic) 
points(bic_min, regfit_bwd_summary$bic[bic_min], col = "red", cex = 2, pch = 20)
coef(regfit_bwd , 8)
# same as Best subset

# The best eight-variable models identified by the best subset selection and backward stepwise selection are the same
# but are different from the forward stepwise selection.

# Cross-validation
predict.regsubsets <- function(object, newdata, id, ...) {
  form  <- as.formula(object$call[[2]])
  mat   <- model.matrix(form, newdata)
  coefi <- coef(object, id = id)
  mat[, names(coefi)] %*% coefi
}

k <- 10
n <- nrow(por_student)
set.seed(1)
folds <- sample(rep(1:k, length = n))
cv_errors <- matrix(NA, k, 39, dimnames = list(NULL, paste(1:39)))

for (j in 1:k) {
  best_fit <- regsubsets(G3 ~ ., data = por_student[folds != j, ], nvmax = 39)
  for (i in 1:39) {
    pred <- predict(best_fit, por_student[folds == j, ], id = i)
    cv_errors[j, i] <- mean((por_student$G3[folds == j] - pred)^2)
  }
}

mean_cv_errors <- apply(cv_errors, 2, mean)
min_cv <- which.min(mean_cv_errors)

par(mfrow = c(1, 1))
plot(mean_cv_errors, type = 'b', xlab = "Number of Variables", ylab = "Mean CV MSE")
points(min_cv, mean_cv_errors[min_cv], col = "red", cex = 2, pch = 20)

se_cv_errors <- apply(cv_errors, 2, sd) / sqrt(k)

one_se_size <- min(which(mean_cv_errors <= mean_cv_errors[min_cv] + se_cv_errors[min_cv]))

points(one_se_size, mean_cv_errors[one_se_size], col = "blue", cex = 2, pch = 20)
legend("topright", legend = c("CV minimum", "1-SE (simpler) model"), col = c("red","blue"), pch = 20)

cat("CV-optimal size:", min_cv, " | 1-SE rule size:", one_se_size, "\n")

# Full linear model
full_fit  <- lm(G3 ~ ., data = train_data)
summary(full_fit)
full_pred <- predict(full_fit, test_data)

cp_pred      <- predict(regfit_full, test_data, id = which.min(reg_summary$cp))
bic_pred     <- predict(regfit_full, test_data, id = which.min(reg_summary$bic))
adjr2_pred   <- predict(regfit_full, test_data, id = which.max(reg_summary$adjr2))
fwd_bic_pred <- predict(regfit_fwd,  test_data, id = which.min(regfit_fwd_summary$bic))
bwd_bic_pred <- predict(regfit_bwd,  test_data, id = which.min(regfit_bwd_summary$bic))

# Regression model comparison table
regression_results <- data.frame(
  Model = c("Full linear model (OLS)", "Best subset (Cp)",
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
