mat-model
================
Tsithembile Tembo
2026-07-28

``` r
# load data from desktop
math_student <- read.csv("student-mat.csv", sep = ";")
str(math_student)
```

    ## 'data.frame':    395 obs. of  33 variables:
    ##  $ school    : chr  "GP" "GP" "GP" "GP" ...
    ##  $ sex       : chr  "F" "F" "F" "F" ...
    ##  $ age       : int  18 17 15 15 16 16 16 17 15 15 ...
    ##  $ address   : chr  "U" "U" "U" "U" ...
    ##  $ famsize   : chr  "GT3" "GT3" "LE3" "GT3" ...
    ##  $ Pstatus   : chr  "A" "T" "T" "T" ...
    ##  $ Medu      : int  4 1 1 4 3 4 2 4 3 3 ...
    ##  $ Fedu      : int  4 1 1 2 3 3 2 4 2 4 ...
    ##  $ Mjob      : chr  "at_home" "at_home" "at_home" "health" ...
    ##  $ Fjob      : chr  "teacher" "other" "other" "services" ...
    ##  $ reason    : chr  "course" "course" "other" "home" ...
    ##  $ guardian  : chr  "mother" "father" "mother" "mother" ...
    ##  $ traveltime: int  2 1 1 1 1 1 1 2 1 1 ...
    ##  $ studytime : int  2 2 2 3 2 2 2 2 2 2 ...
    ##  $ failures  : int  0 0 3 0 0 0 0 0 0 0 ...
    ##  $ schoolsup : chr  "yes" "no" "yes" "no" ...
    ##  $ famsup    : chr  "no" "yes" "no" "yes" ...
    ##  $ paid      : chr  "no" "no" "yes" "yes" ...
    ##  $ activities: chr  "no" "no" "no" "yes" ...
    ##  $ nursery   : chr  "yes" "no" "yes" "yes" ...
    ##  $ higher    : chr  "yes" "yes" "yes" "yes" ...
    ##  $ internet  : chr  "no" "yes" "yes" "yes" ...
    ##  $ romantic  : chr  "no" "no" "no" "yes" ...
    ##  $ famrel    : int  4 5 4 3 4 5 4 4 4 5 ...
    ##  $ freetime  : int  3 3 3 2 3 4 4 1 2 5 ...
    ##  $ goout     : int  4 3 2 2 2 2 4 4 2 1 ...
    ##  $ Dalc      : int  1 1 2 1 1 1 1 1 1 1 ...
    ##  $ Walc      : int  1 1 3 1 2 2 1 1 1 1 ...
    ##  $ health    : int  3 3 3 5 5 5 3 1 1 5 ...
    ##  $ absences  : int  6 4 10 2 4 10 0 6 0 0 ...
    ##  $ G1        : int  5 5 7 15 6 15 12 6 16 14 ...
    ##  $ G2        : int  6 5 8 14 10 15 12 5 18 15 ...
    ##  $ G3        : int  6 6 10 15 10 15 11 6 19 15 ...

``` r
#clean dataset
sum(is.na(math_student))
```

    ## [1] 0

``` r
sum(duplicated(math_student))
```

    ## [1] 0

``` r
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
```

    ## 'data.frame':    395 obs. of  31 variables:
    ##  $ school    : Factor w/ 2 levels "GP","MS": 1 1 1 1 1 1 1 1 1 1 ...
    ##  $ sex       : Factor w/ 2 levels "F","M": 1 1 1 1 1 2 2 1 2 2 ...
    ##  $ age       : int  18 17 15 15 16 16 16 17 15 15 ...
    ##  $ address   : Factor w/ 2 levels "R","U": 2 2 2 2 2 2 2 2 2 2 ...
    ##  $ famsize   : Factor w/ 2 levels "GT3","LE3": 1 1 2 1 1 2 2 1 2 1 ...
    ##  $ Pstatus   : Factor w/ 2 levels "A","T": 1 2 2 2 2 2 2 1 1 2 ...
    ##  $ Medu      : int  4 1 1 4 3 4 2 4 3 3 ...
    ##  $ Fedu      : int  4 1 1 2 3 3 2 4 2 4 ...
    ##  $ Mjob      : Factor w/ 5 levels "at_home","health",..: 1 1 1 2 3 4 3 3 4 3 ...
    ##  $ Fjob      : Factor w/ 5 levels "at_home","health",..: 5 3 3 4 3 3 3 5 3 3 ...
    ##  $ reason    : Factor w/ 4 levels "course","home",..: 1 1 3 2 2 4 2 2 2 2 ...
    ##  $ guardian  : Factor w/ 3 levels "father","mother",..: 2 1 2 2 1 2 2 2 2 2 ...
    ##  $ traveltime: int  2 1 1 1 1 1 1 2 1 1 ...
    ##  $ studytime : int  2 2 2 3 2 2 2 2 2 2 ...
    ##  $ failures  : int  0 0 3 0 0 0 0 0 0 0 ...
    ##  $ schoolsup : Factor w/ 2 levels "no","yes": 2 1 2 1 1 1 1 2 1 1 ...
    ##  $ famsup    : Factor w/ 2 levels "no","yes": 1 2 1 2 2 2 1 2 2 2 ...
    ##  $ paid      : Factor w/ 2 levels "no","yes": 1 1 2 2 2 2 1 1 2 2 ...
    ##  $ activities: Factor w/ 2 levels "no","yes": 1 1 1 2 1 2 1 1 1 2 ...
    ##  $ nursery   : Factor w/ 2 levels "no","yes": 2 1 2 2 2 2 2 2 2 2 ...
    ##  $ higher    : Factor w/ 2 levels "no","yes": 2 2 2 2 2 2 2 2 2 2 ...
    ##  $ internet  : Factor w/ 2 levels "no","yes": 1 2 2 2 1 2 2 1 2 2 ...
    ##  $ romantic  : Factor w/ 2 levels "no","yes": 1 1 1 2 1 1 1 1 1 1 ...
    ##  $ famrel    : int  4 5 4 3 4 5 4 4 4 5 ...
    ##  $ freetime  : int  3 3 3 2 3 4 4 1 2 5 ...
    ##  $ goout     : int  4 3 2 2 2 2 4 4 2 1 ...
    ##  $ Dalc      : int  1 1 2 1 1 1 1 1 1 1 ...
    ##  $ Walc      : int  1 1 3 1 2 2 1 1 1 1 ...
    ##  $ health    : int  3 3 3 5 5 5 3 1 1 5 ...
    ##  $ absences  : int  6 4 10 2 4 10 0 6 0 0 ...
    ##  $ G3        : int  6 6 10 15 10 15 11 6 19 15 ...

``` r
#look at features and see which is most significant
all.fit <- lm(G3 ~ ., data = math_student)
summary(all.fit)
```

    ## 
    ## Call:
    ## lm(formula = G3 ~ ., data = math_student)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -13.0442  -1.9028   0.4289   2.7570   8.8874 
    ## 
    ## Coefficients:
    ##                  Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)      14.07769    4.48089   3.142  0.00182 ** 
    ## schoolMS          0.72555    0.79157   0.917  0.35997    
    ## sexM              1.26236    0.50003   2.525  0.01202 *  
    ## age              -0.37516    0.21721  -1.727  0.08501 .  
    ## addressU          0.55135    0.58412   0.944  0.34586    
    ## famsizeLE3        0.70281    0.48824   1.439  0.15090    
    ## PstatusT         -0.32010    0.72390  -0.442  0.65862    
    ## Medu              0.45687    0.32317   1.414  0.15833    
    ## Fedu             -0.10458    0.27762  -0.377  0.70663    
    ## Mjobhealth        0.99808    1.11819   0.893  0.37268    
    ## Mjobother        -0.35900    0.71316  -0.503  0.61500    
    ## Mjobservices      0.65832    0.79784   0.825  0.40985    
    ## Mjobteacher      -1.24149    1.03821  -1.196  0.23257    
    ## Fjobhealth        0.34767    1.43796   0.242  0.80909    
    ## Fjobother        -0.61967    1.02304  -0.606  0.54509    
    ## Fjobservices     -0.46577    1.05697  -0.441  0.65972    
    ## Fjobteacher       1.32619    1.29654   1.023  0.30707    
    ## reasonhome        0.07851    0.55380   0.142  0.88735    
    ## reasonother       0.77707    0.81757   0.950  0.34252    
    ## reasonreputation  0.61304    0.57657   1.063  0.28839    
    ## guardianmother    0.06978    0.54560   0.128  0.89830    
    ## guardianother     0.75010    0.99946   0.751  0.45345    
    ## traveltime       -0.24027    0.33897  -0.709  0.47889    
    ## studytime         0.54952    0.28765   1.910  0.05690 .  
    ## failures         -1.72398    0.33291  -5.179 3.75e-07 ***
    ## schoolsupyes     -1.35058    0.66693  -2.025  0.04361 *  
    ## famsupyes        -0.86182    0.47869  -1.800  0.07265 .  
    ## paidyes           0.33975    0.47775   0.711  0.47746    
    ## activitiesyes    -0.32953    0.44494  -0.741  0.45942    
    ## nurseryyes       -0.17730    0.54931  -0.323  0.74706    
    ## higheryes         1.37045    1.07780   1.272  0.20437    
    ## internetyes       0.49813    0.61956   0.804  0.42192    
    ## romanticyes      -1.09449    0.46925  -2.332  0.02024 *  
    ## famrel            0.23155    0.24593   0.942  0.34706    
    ## freetime          0.30242    0.23735   1.274  0.20345    
    ## goout            -0.59367    0.22451  -2.644  0.00855 ** 
    ## Dalc             -0.27223    0.33087  -0.823  0.41120    
    ## Walc              0.26339    0.24801   1.062  0.28896    
    ## health           -0.17678    0.16101  -1.098  0.27297    
    ## absences          0.05629    0.02897   1.943  0.05277 .  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 4.108 on 355 degrees of freedom
    ## Multiple R-squared:  0.2756, Adjusted R-squared:  0.196 
    ## F-statistic: 3.463 on 39 and 355 DF,  p-value: 3.317e-10

``` r
#many of the features are not significant and
#this model is weak, probably due to leaving out G1 and G3
```

``` r
#splitting the data up
set.seed(13)
train <- sample(1:nrow(math_student), nrow(math_student) * 0.6)
train_data <- math_student[train, ]
test_data <- math_student[-train, ]
```

``` r
#BEST SUBSET SELECTION PROCESS
library(leaps)
regfit_full <- regsubsets(G3 ~ ., data = train_data, nvmax = 39) ##lot more features becuase of sub categories. 
reg_summary <- summary(regfit_full)
```

``` r
#Cp, BIC, adhjusted R^2
which.min(reg_summary$cp)
```

    ## [1] 6

``` r
which.min(reg_summary$bic)
```

    ## [1] 1

``` r
which.max(reg_summary$adjr2)
```

    ## [1] 15

``` r
coef(regfit_full, 6)
```

    ##   (Intercept)          sexM           age          Medu guardianother 
    ##    16.5747200     1.3255614    -0.4439743     0.5063067     2.9817604 
    ##      failures   romanticyes 
    ##    -1.8153289    -1.1434697

``` r
coef(regfit_full, 1)
```

    ## (Intercept)    failures 
    ##   11.027147   -1.930422

``` r
coef(regfit_full, 15)
```

    ##      (Intercept)             sexM              age         addressU 
    ##       15.8383037        1.6391840       -0.3907461        0.7424949 
    ##         PstatusT             Medu      reasonother reasonreputation 
    ##       -1.2180090        0.4020186        1.9289896        1.0065193 
    ##    guardianother         failures     schoolsupyes          paidyes 
    ##        2.3920263       -1.5301634       -1.0229192        0.6851495 
    ##       nurseryyes        higheryes      romanticyes            goout 
    ##       -1.1420211        1.7198184       -1.0246728       -0.3018561

``` r
#fit and predit model for Cp
CPbest_fit <- lm(G3 ~  sex + age + Medu + guardian + failures + romantic, data = train_data)
summary(CPbest_fit)
```

    ## 
    ## Call:
    ## lm(formula = G3 ~ sex + age + Medu + guardian + failures + romantic, 
    ##     data = train_data)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -13.1900  -1.7070   0.4347   2.8626   9.4650 
    ## 
    ## Coefficients:
    ##                Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)     16.6934     4.0912   4.080 6.21e-05 ***
    ## sexM             1.3143     0.5719   2.298   0.0225 *  
    ## age             -0.4352     0.2378  -1.830   0.0685 .  
    ## Medu             0.5152     0.2630   1.959   0.0513 .  
    ## guardianmother  -0.3555     0.7007  -0.507   0.6124    
    ## guardianother    2.7043     1.3467   2.008   0.0458 *  
    ## failures        -1.8198     0.3933  -4.627 6.21e-06 ***
    ## romanticyes     -1.1726     0.6066  -1.933   0.0544 .  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 4.345 on 229 degrees of freedom
    ## Multiple R-squared:  0.1753, Adjusted R-squared:  0.1501 
    ## F-statistic: 6.956 on 7 and 229 DF,  p-value: 1.621e-07

``` r
CPBest_pred <- predict(CPbest_fit, test_data)
mean((CPBest_pred - test_data$G3)^2) # 16.63068
```

    ## [1] 16.63068

``` r
#fit and predict model for BIC
BICbest_fit <- lm(G3 ~ failures, data = train_data)
summary(BICbest_fit)
```

    ## 
    ## Call:
    ## lm(formula = G3 ~ failures, data = train_data)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -11.0271  -2.0271  -0.0271   2.9729   7.9729 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)  11.0271     0.3171  34.775  < 2e-16 ***
    ## failures     -1.9304     0.3744  -5.156 5.36e-07 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 4.477 on 235 degrees of freedom
    ## Multiple R-squared:  0.1016, Adjusted R-squared:  0.0978 
    ## F-statistic: 26.58 on 1 and 235 DF,  p-value: 5.356e-07

``` r
BICBest_pred <- predict(BICbest_fit, test_data)
mean((BICBest_pred - test_data$G3)^2) #15.8463, the best model where the only feature was failures 
```

    ## [1] 15.8463

``` r
#fit and predict model for adjR^2
ADJbest_fit <- lm(G3 ~ sex + age + address + Pstatus + Medu + reason + guardian + failures +
                   schoolsup + paid + nursery + higher + romantic + goout, data = train_data)
summary(ADJbest_fit)
```

    ## 
    ## Call:
    ## lm(formula = G3 ~ sex + age + address + Pstatus + Medu + reason + 
    ##     guardian + failures + schoolsup + paid + nursery + higher + 
    ##     romantic + goout, data = train_data)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -13.027  -1.700   0.276   2.768   7.766 
    ## 
    ## Coefficients:
    ##                  Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)       16.0198     4.9864   3.213 0.001513 ** 
    ## sexM               1.6154     0.5971   2.705 0.007357 ** 
    ## age               -0.3840     0.2575  -1.491 0.137400    
    ## addressU           0.7001     0.6873   1.019 0.309497    
    ## PstatusT          -1.2635     0.9218  -1.371 0.171863    
    ## Medu               0.4112     0.2760   1.490 0.137605    
    ## reasonhome         0.1614     0.7294   0.221 0.825024    
    ## reasonother        1.9904     1.0728   1.855 0.064892 .  
    ## reasonreputation   1.0800     0.7394   1.461 0.145526    
    ## guardianmother    -0.3933     0.7006  -0.561 0.575092    
    ## guardianother      2.0646     1.3705   1.507 0.133370    
    ## failures          -1.5340     0.4180  -3.669 0.000305 ***
    ## schoolsupyes      -1.0283     0.9114  -1.128 0.260445    
    ## paidyes            0.6790     0.6006   1.131 0.259462    
    ## nurseryyes        -1.1548     0.7058  -1.636 0.103252    
    ## higheryes          1.7102     1.4570   1.174 0.241773    
    ## romanticyes       -1.0675     0.6130  -1.742 0.082995 .  
    ## goout             -0.2896     0.2610  -1.110 0.268361    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 4.306 on 219 degrees of freedom
    ## Multiple R-squared:  0.2255, Adjusted R-squared:  0.1653 
    ## F-statistic:  3.75 on 17 and 219 DF,  p-value: 2.399e-06

``` r
ADJBest_pred <- predict(ADJbest_fit, test_data)
mean((ADJBest_pred - test_data$G3)^2) #17.1042
```

    ## [1] 17.1042

``` r
#FOWARD SELECTION 
regfit_foward <- regsubsets(G3 ~., data = train_data, nvmax = 39, method = "forward")
reg_summary_forward <- summary(regfit_foward)
```

``` r
#Cp, BIC, adhjusted R^2
which.min(reg_summary_forward$cp) #6
```

    ## [1] 6

``` r
which.min(reg_summary_forward$bic) #1
```

    ## [1] 1

``` r
which.max(reg_summary_forward$adjr2) #15
```

    ## [1] 15

``` r
#same as the best selection modethod.
#Variables -> Cp = 6, BIC = 1, ADJR = 15
coef(regfit_foward, 6)
```

    ##   (Intercept)          sexM           age          Medu guardianother 
    ##    16.5747200     1.3255614    -0.4439743     0.5063067     2.9817604 
    ##      failures   romanticyes 
    ##    -1.8153289    -1.1434697

``` r
coef(regfit_foward, 1)
```

    ## (Intercept)    failures 
    ##   11.027147   -1.930422

``` r
coef(regfit_foward, 15)
```

    ##      (Intercept)             sexM              age         addressU 
    ##       15.8383037        1.6391840       -0.3907461        0.7424949 
    ##         PstatusT             Medu      reasonother reasonreputation 
    ##       -1.2180090        0.4020186        1.9289896        1.0065193 
    ##    guardianother         failures     schoolsupyes          paidyes 
    ##        2.3920263       -1.5301634       -1.0229192        0.6851495 
    ##       nurseryyes        higheryes      romanticyes            goout 
    ##       -1.1420211        1.7198184       -1.0246728       -0.3018561

``` r
#BACKWARD SELECTION METHOD
regfit_backward <- regsubsets(G3 ~., data = train_data, nvmax = 39, method = "backward")
reg_summary_backward <- summary(regfit_backward)
```

``` r
#Cp, BIC, adhjusted R^2
which.min(reg_summary_backward$cp) #6 
```

    ## [1] 6

``` r
which.min(reg_summary_backward$bic) #1
```

    ## [1] 1

``` r
which.max(reg_summary_backward$adjr2) #15
```

    ## [1] 15

``` r
#same as the best selection modethod and foward method
#Variables -> Cp = 6, BIC = 1, ADJR = 15
coef(regfit_backward, 6)
```

    ##   (Intercept)          sexM           age          Medu guardianother 
    ##    16.5747200     1.3255614    -0.4439743     0.5063067     2.9817604 
    ##      failures   romanticyes 
    ##    -1.8153289    -1.1434697

``` r
coef(regfit_backward, 1)
```

    ## (Intercept)    failures 
    ##   11.027147   -1.930422

``` r
coef(regfit_backward, 15)
```

    ##      (Intercept)             sexM              age         addressU 
    ##       15.9891751        1.7911587       -0.4120992        0.7072230 
    ##         PstatusT             Medu      reasonother reasonreputation 
    ##       -1.3639294        0.3747946        2.1768774        1.0942814 
    ##    guardianother         failures     schoolsupyes          paidyes 
    ##        2.5179318       -1.5605463       -1.0002618        0.7584236 
    ##       nurseryyes        higheryes      romanticyes             Dalc 
    ##       -1.2733728        1.6594170       -1.0064154       -0.3395154

``` r
##NOT SELECTED, SO ALL PREDICTORS
full_fit <- lm(G3 ~ ., data = train_data)
summary(full_fit)
```

    ## 
    ## Call:
    ## lm(formula = G3 ~ ., data = train_data)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -12.1012  -1.8869   0.1793   3.0080   7.7769 
    ## 
    ## Coefficients:
    ##                  Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)      17.99887    6.42473   2.802 0.005594 ** 
    ## schoolMS          0.71735    1.11582   0.643 0.521039    
    ## sexM              1.98571    0.72939   2.722 0.007062 ** 
    ## age              -0.50778    0.29848  -1.701 0.090483 .  
    ## addressU          0.63760    0.85354   0.747 0.455953    
    ## famsizeLE3        0.18640    0.72555   0.257 0.797514    
    ## PstatusT         -0.89482    1.03251  -0.867 0.387188    
    ## Medu              0.35917    0.46425   0.774 0.440064    
    ## Fedu             -0.02400    0.40643  -0.059 0.952969    
    ## Mjobhealth        0.83837    1.68625   0.497 0.619615    
    ## Mjobother         0.19075    1.04696   0.182 0.855619    
    ## Mjobservices      0.89210    1.16537   0.766 0.444884    
    ## Mjobteacher      -0.09709    1.48982  -0.065 0.948107    
    ## Fjobhealth        0.06991    1.97731   0.035 0.971832    
    ## Fjobother        -0.08258    1.33685  -0.062 0.950805    
    ## Fjobservices      0.33111    1.41962   0.233 0.815821    
    ## Fjobteacher       0.58008    1.74222   0.333 0.739520    
    ## reasonhome       -0.09603    0.79807  -0.120 0.904344    
    ## reasonother       1.65408    1.21842   1.358 0.176157    
    ## reasonreputation  0.92264    0.84452   1.093 0.275944    
    ## guardianmother   -0.31858    0.82394  -0.387 0.699430    
    ## guardianother     2.05943    1.52546   1.350 0.178553    
    ## traveltime       -0.12104    0.47191  -0.256 0.797843    
    ## studytime         0.28092    0.42254   0.665 0.506926    
    ## failures         -1.61847    0.46849  -3.455 0.000675 ***
    ## schoolsupyes     -0.83756    0.99157  -0.845 0.399318    
    ## famsupyes        -0.31794    0.69738  -0.456 0.648955    
    ## paidyes           0.76629    0.69790   1.098 0.273547    
    ## activitiesyes    -0.56746    0.66867  -0.849 0.397111    
    ## nurseryyes       -1.25729    0.77218  -1.628 0.105072    
    ## higheryes         1.70979    1.58853   1.076 0.283096    
    ## internetyes      -0.14579    0.87903  -0.166 0.868439    
    ## romanticyes      -1.12202    0.67269  -1.668 0.096914 .  
    ## famrel           -0.01911    0.34943  -0.055 0.956435    
    ## freetime          0.06997    0.35190   0.199 0.842600    
    ## goout            -0.26163    0.34261  -0.764 0.446009    
    ## Dalc             -0.45868    0.46001  -0.997 0.319934    
    ## Walc              0.17999    0.35668   0.505 0.614379    
    ## health           -0.16315    0.23555  -0.693 0.489341    
    ## absences          0.04307    0.04232   1.018 0.310047    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 4.477 on 197 degrees of freedom
    ## Multiple R-squared:  0.2471, Adjusted R-squared:  0.098 
    ## F-statistic: 1.657 on 39 and 197 DF,  p-value: 0.01383

``` r
full_pred <- predict(full_fit, test_data)
mean((full_pred - test_data$G3)^2) #15.82758
```

    ## [1] 15.82758
