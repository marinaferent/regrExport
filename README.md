# regrExport: Results and goodness of fit tables for lm, plm, and glmer models

Functions that ease the reporting stage of research projects that use
lm, plm, coeftest, or glmer models.

*resultsMatrix()* - Coefficient, significance stars, and (bootstrapped)
sandard error (t-statistic, or p-value) are reported according to social
sciences standards.

*goodnessFitMatrixMLM()* - Provides a comprehensive table of goodness of
fit measures for glmer mixed effects logit models, such as random
intercept, ICC, AIC, BIC, logLik, deviance, PCV, and R^2GLMM.

*diagTestMatrixPLM()* - Provides a table of diagnostic tests for panel
linear models - F-test, Hausman test, B-P/LM test of independence,
Pasaran CD test of independence, Breusch-Godfrey/Wooldridge test for
serial correlation in panel models, and Breusch-Pagan test for
heteroskedasticity. The table comprises null hypothesis, statistic
value, and also the decision of the test.

## Installation

``` r
library(devtools)
```

``` r
install_github("marinaferent/regrExport")
```

## Usage

Some examples are provided below. 
Ideal for export in .csv files for storage and future use. Suitable for R markdown generated
files.

**resultsMatrix()** - Exports lm, plm and glmer regression’s coefficient
and the statistical measure of preference in parenthesis.

*Example 1-* plm OLS model:

``` r
library(regrExport)
library(plm)
data("Grunfeld", package = "plm") ###use the models from plm package (Croissant Y. et al, 2022: pg.4):
pooledOLS=plm(inv ~ value + capital, data = Grunfeld, model="pooling")
resultsMatrix(pooledOLS) #returns the coefficient and standard error, 4 decimals
```

    ##             Coefficient (stderr)
    ## (Intercept) -42.7144*** (9.5117)
    ## value         0.1156*** (0.0058)
    ## capital       0.2307*** (0.0255)

*Example 2-* coeftest OLS model

``` r
library(lmtest)
```
``` r
ols_corrected=coeftest(pooledOLS, vcov = vcovSCC(pooledOLS, method="arellano", type="HC3", cluster = "group"))
resultsMatrix(ols_corrected, "pvalue", x.coeftest=TRUE) #returns the coefficient and pvalue, 4 decimals
```

    ##             Coefficient (pvalue)
    ## (Intercept) -42.7144*** (0.0073)
    ## value              0.1156*** (0)
    ## capital       0.2307*** (0.0076)

*Example 3-* glmer logit multilevel model:

``` r
library(lme4)
```

``` r
library(boot)
set.seed(404)
beta0=-1.4
beta1=0.1
age=sample(18:40, 100, replace=T)
gender=sample(0:1, 100, replace=T)
eduCat=sample(1:3, 100, replace=T)
groupId=sample(1:10, 100, replace=T)
prob=exp(beta0 + beta1 * age) / (1 + exp(beta0 + beta1 * age))
WLB=rbinom(n=100, size=1, prob=prob)
dataTest=as.data.frame(cbind(WLB, age, gender, eduCat, groupId))
regression.WLB=glmer(WLB ~ age + factor(gender) + I(eduCat==1) + I(eduCat==3) + (1 | groupId), data = dataTest, family = binomial, control=glmerControl(optimizer="bobyqa"), nAGQ = 0)
```

``` r
resultsMatrix(regression.WLB) #returns the coefficient and standard error, 4 decimals
```

    ##                    Coefficient (stderr)
    ## (Intercept)             -1.0783 (1.241)
    ## age                    0.0879** (0.042)
    ## factor(gender)1         0.0583 (0.5389)
    ## I(eduCat == 1)TRUE       0.291 (0.6283)
    ## I(eduCat == 3)TRUE     -0.1957 (0.6567)

``` r
resultsMatrix(regression.WLB, "pvalue", 2) #returns the coefficient and p-value, 2 decimals
```

    ##                    Coefficient (pvalue)
    ## (Intercept)                -1.08 (0.38)
    ## age                       0.09** (0.02)
    ## factor(gender)1             0.06 (0.91)
    ## I(eduCat == 1)TRUE          0.29 (0.65)
    ## I(eduCat == 3)TRUE          -0.2 (0.76)

To return (tstat/pvalues based on) *bootstrapped standard errors*:

``` r
FUN <- function(fit) {
  return(fixef(fit))
}
bootStdErr=bootMer(regression.WLB, FUN=FUN, nsim=10)
resultsMatrix(regression.WLB,bRes=bootStdErr) #returns the coefficient and bootstrapped standard error, 4 decimals
```

    ##                    Coefficient (bootstrapped stderr)
    ## (Intercept)                         -1.0783 (1.2991)
    ## age                                 0.0879* (0.0461)
    ## factor(gender)1                      0.0583 (0.3231)
    ## I(eduCat == 1)TRUE                    0.291 (0.9161)
    ## I(eduCat == 3)TRUE                  -0.1957 (0.7943)

``` r
resultsMatrix(regression.WLB,"tstat", 3, bootStdErr) #returns the coefficient and t stat based on bootstrapped standard error, 3 decimals
```

    ##                    Coefficient (bootstrapped tstat)
    ## (Intercept)                          -1.078 (-0.83)
    ## age                                  0.088* (1.913)
    ## factor(gender)1                        0.058 (0.18)
    ## I(eduCat == 1)TRUE                    0.291 (0.318)
    ## I(eduCat == 3)TRUE                  -0.196 (-0.247)

``` r
#***, **, * represent statistical significance at 1%, 5%, and 10%, respectively.
```

**goodnessFitMatrixMLM()** - Exports table of goodness of fit measures
for multilevel logit models.

``` r
library(lme4)
library(boot)
set.seed(404)
beta_0=-1.4
beta_1=0.1
beta=0.5
age=sample(18:40, 100, replace=T)
gender=sample(0:1, 100, replace=T)
eduCat=sample(1:3, 100, replace=T)
groupId=sample(1:10, 100, replace=T)
prob=exp(beta_0 + beta_1 * age + beta * groupId) / (1 + exp(beta_0 + beta_1 * age + beta * groupId))
WLB=rbinom(n=100, size=1, prob=prob)
dataTest=as.data.frame(cbind(WLB, age, gender, eduCat, groupId))
first.WLB=glmer(formula=WLB ~ age + factor(gender) + I(eduCat==1) + I(eduCat==3) + (1 | groupId), data = dataTest, family = binomial, control=glmerControl(optimizer="bobyqa"), nAGQ = 0)
goodnessFitMatrixMLM(first.WLB)
```

    ##                                    V1
    ## Random intercept variance      0.5074
    ## Interclass correlation (ICC)   0.1336
    ## AIC                           50.5492
    ## BIC                           66.1802
    ## logLik                       -19.2746
    ## deviance                      38.5492
    ## df.resid                           94
    ## No. observations                  100
    ## No. countries                      10

``` r
goodnessFitMatrixMLM(first.WLB, decim=2, decim_per=0)
```

    ##                                  V1
    ## Random intercept variance      0.51
    ## Interclass correlation (ICC)   0.13
    ## AIC                           50.55
    ## BIC                           66.18
    ## logLik                       -19.27
    ## deviance                      38.55
    ## df.resid                         94
    ## No. observations                100
    ## No. countries                    10

To return PCV and R^2GLMM, also provide the null model:

``` r
null.WLB=glmer(WLB ~ 1 + (1 | groupId), data = dataTest, family = binomial, control=glmerControl(optimizer="bobyqa"), nAGQ = 0)
goodnessFitMatrixMLM(first.WLB, null.WLB, 2, 0) #returns PCV, R^2GLMM in relation to null.WLB, 2 decimals for numbers, 0 decimals for percentage
```

    ##                                                       V1
    ## Random intercept variance                           0.51
    ## Interclass correlation (ICC)                        0.13
    ## AIC                                                50.55
    ## BIC                                                66.18
    ## logLik                                            -19.27
    ## deviance                                           38.55
    ## df.resid                                              94
    ## Proportion change in variance (PCV)                  20%
    ## Marginal R-squared (R^2_GLMM(m))- Theoretical         6%
    ## Conditional R-squared (R^2_GLMM(c)) - Theoretical    18%
    ## Marginal R-squared (R^2_GLMM(m))- Delta               1%
    ## Conditional R-squared (R^2_GLMM(c)) - Delta           4%
    ## No. observations                                     100
    ## No. countries                                         10

**diagTestMatrixPLM()** - Exports a table of diagnostic tests for panel
linear regression models: null hypothesis, value, and decision based on
user-defined significance threshold level.

``` r
library(plm)
data("Grunfeld", package = "plm") ###use the models from plm package (Croissant Y. et al, 2022: pg.4):
pooledOLS=plm(inv ~ value + capital, data = Grunfeld, model="pooling")
tests=diagTestMatrixPLM(pooledOLS)
#View(tests)
#write.csv(tests, "POLStests.csv")
FE=plm(inv ~ value + capital, data = Grunfeld, model="within")
RE=plm(inv ~ value + capital, data = Grunfeld, model="random")
tests=diagTestMatrixPLM(pooledOLS, 2, 0.01, FE)
#View(tests)
tests=diagTestMatrixPLM(pooledOLS,FEM=FE, REM=RE)
#View(tests)
#***, **, * represent statistical significance at 1%, 5%, and 10%, respectively.
```


## References
--- Barton, K. (2023). MuMIn: Multi-Model Inference (version 1.47.5). https://CRAN.R-project.org/package=MuMIn

--- Bates, D. et al. (2023), Package ‘lme4’: Linear Mixed-Effects Models using 'Eigen' and S4 (Version 1.1-32). https://github.com/lme4/lme4/

--- Croissant, Y. et al.(2022), Package ‘plm’:linear models for panel data (Version 2.6-2). https://cran.r-project.org/package=plm

--- Hothorn, T. et al. (2022), Package ‘lmtest’: Testing Linear Regression Models (Version 0.9-40). https://CRAN.R-project.org/package=lmtest

--- Nakagawa, S., and Schielzeth, H. (2013). A general and simple method for obtaining R2 from generalized linear mixed-effects models, Methods in Ecology and Evolution, Vol.4 No.2, 133-142.

--- Nakagawa, S., Johnson, P. C., and Schielzeth, H. (2017). The coefficient of determination R2 and intra-class correlation coefficient from generalized linear mixed-effects models revisited and expanded, Journal of the Royal Society Interface, Vol.14 No.134.

--- Sommet, N., and Morselli, D. (2017). Keep Calm and Learn Multilevel Logistic Modeling: A Simplified Three-Step Procedure Using Stata, R, Mplus, and SPSS. International Review of Social Psychology, Vol.30 No.1, pp.203-218.
