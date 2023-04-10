#' Table of diagnostic tests for panel linear regression models: null hypothesis, value, and decision based on user-defined significance threshold level.
#' @param x An object of class plm.
#' @param decim Specifies the number of decimals to display. The default is 4.
#' @param prob Specifies the threshold for rejecting the null hypothesis. The default is 0.05.
#' @param FEM An object of class plm. If x is POLS model, specifies the Fixed Effects model to be compared with for the Ftest.
#' @param REM An object of class plm. If FEM is specified, REM specifies the Random Effects model to be compared with for the Hausman test.
#' @returns A matrix with the null hypothesis, statistic, p-value, number of stars, and the interpretation of the tests' results. Significance stars follow the social sciences standards.
#' The rownames are the tests' names: F-test, Hausman test, B-P/LM test of independence, Pasaran CD test of independence, Breusch-Godfrey/Wooldridge test for serial correlation in panel models, and Breusch-Pagan test for heteroskedasticity.
#' Builds on plm and lmtest packages. Adds the interpretation of the tests' results.
#' The function does not decide which tests are appropriate to be used for the regression model provided. This remains at the researcher's discretion.
#' For e.g. the function compiles the Breusch-Pagan test for heteroskedasticity even if the model provided in argument x is a random effects one.
#' @references
#' Croissant, Y. et al.(2022), Package ‘plm’:linear models for panel data (Version 2.6-2). https://cran.r-project.org/package=plm
#' Hothorn, T. et al. (2022), Package ‘lmtest’: Testing Linear Regression Models (Version 0.9-40). https://CRAN.R-project.org/package=lmtest
#' @examples
#' library(plm)
#' data("Grunfeld", package = "plm") ###use the models from plm package (Croissant Y. et al, 2022: pg.4):
#' pooledOLS=plm(inv ~ value + capital, data = Grunfeld, model="pooling")
#' tests=diagTestMatrixPLM(pooledOLS)
#' #View(tests)
#' #write.csv(tests, "POLStests.csv")
#' FE=plm(inv ~ value + capital, data = Grunfeld, model="within")
#' RE=plm(inv ~ value + capital, data = Grunfeld, model="random")
#' tests=diagTestMatrixPLM(pooledOLS, 2, 0.01, FE)
#' #View(tests)
#' tests=diagTestMatrixPLM(pooledOLS,FEM=FE, REM=RE)
#' #View(tests)
#' #' #***, **, * represent statistical significance at 1%, 5%, and 10%, respectively.

diagTestMatrixPLM=function(x, decim=4, prob=0.05, FEM, REM)
{
  require(plm)
  require(lmtest)
  star=function(x)
  {
    if(x<0.01)
    {
      paste("***")
    } else
    {
      if(x<0.05)
      {
        paste("**")
      } else
      {
        if(x<0.1)
        {
          paste("*")
        }
      }
    }
  }
  test=as.data.frame(matrix(NA,ncol=3, nrow=7))
  colnames(test)=c("Null hypothesis", "Statistic", "Decision")
  if(missing(FEM)){
    T=1
  } else {
    T=1
    ##F-test: Redundant fixed effects##
    rownames(test)[T]=paste0("F-test")
    test[T,1]=paste0("Redundant fixed effects")
    pFtest(FEM, x) #redundant fixed effects F-test
    Fstat=round(pFtest(FEM, x)$statistic, decim)
    pvalue=round(pFtest(FEM, x)$p.value, decim)
    test[T,2]=paste0(Fstat, star(pvalue), " (", pvalue, ")")
    if(pvalue>prob)
    {
      test[T,3]=paste("cannot reject H0 => OLS is preferred")
    } else {
      test[T,3]=paste("reject HO => FEM is preferred")
    }
    T=T+1
  }
  if(missing(REM)){
    T=T
  } else {
    if(missing(FEM)){
      warning("Hausman test is not ran. Please provide Fixed Effects model (argument FEM) to run the Hausman test.")
    } else {
      ##Hausman test: REM is preferred##
      rownames(test)[T]=paste0("Hausman test")
      test[T,1]=paste0("Random effects model is preferred")
      phtest(FEM, REM) #hausman test - fixed vs random effects
      Fstat=round(phtest(FEM, REM)$statistic, decim)
      pvalue=round(phtest(FEM, REM)$p.value, decim)
      test[T,2]=paste0(Fstat, star(pvalue), " (", pvalue, ")")
      if(pvalue>prob)
      {
        test[T,3]=paste("cannot reject H0 => REM is preferred")
      } else {
        test[T,3]=paste("reject H0 => FEM is preferred")
      }
      T=T+1
    }
  }
  ##B-P/LM test of independence: Residuals across entities are not correlated##
  rownames(test)[T]=paste0("B-P/LM test of independence")
  test[T,1]=paste0("Residuals across entities are not correlated")
  pcdtest(x, test = c("lm")) #B-P/LM test of independence -H0 residuals across entities are not correlated
  Fstat=round(pcdtest(x, test = c("lm"))$statistic, decim)
  pvalue=round(pcdtest(x, test = c("lm"))$p.value, decim)
  test[T,2]=paste0(Fstat, star(pvalue), " (", pvalue, ")")
  alt=pcdtest(x, test = c("lm"))$alternative
  if(pvalue>prob)
  {
    test[T,3]=paste("cannot reject H0 => residuals across entities are not correlated")
  } else {
    test[T,3]=paste(paste0("reject H0=>", alt))
  }
  T=T+1

  ##Pesaran CD test of independence: Residuals across entities are not correlated##
  rownames(test)[T]=paste0("Pasaran CD test of independence")
  test[T,1]=paste0("Residuals across entities are not correlated")
  pcdtest(x, test = c("cd")) # Pasaran CD test of independence -H0 residuals across entities are not correlated
  Fstat=round(pcdtest(x, test = c("cd"))$statistic, decim)
  pvalue=round(pcdtest(x, test = c("cd"))$p.value, decim)
  test[T,2]=paste0(Fstat, star(pvalue), " (", pvalue, ")")
  alt=pcdtest(x, test = c("cd"))$alternative
  if(pvalue>prob)
  {
    test[T,3]=paste("cannot reject H0 => residuals across entities are not correlated")
  } else {
    test[T,3]=paste0("reject H0=>", alt)
  }
  T=T+1

  #_________________________________________________________________________________________________________________
  #*Info point: According to Baltagi (2005), cross-sectional dependence is a problem in macro panels with long time series.
  #**This is not much of a problem in micro panels (few years and large number of cases).
  #**The null hypothesis in the B-P/LM and Pasaran CD tests of independence is that residuals across
  #**entities are not correlated. B-P/LM and Pasaran CD (cross-sectional dependence) tests are used to test
  #*whether the residuals are correlated across entities*. Cross-sectional dependence can lead to bias in
  #**tests results (also called contemporaneous correlation).
  #_________________________________________________________________________________________________________________

  ##Breusch-Godfrey/Wooldridge test for serial correlation in panel models: No serial correlation##
  rownames(test)[T]=paste0("Breusch-Godfrey/Wooldridge test for serial correlation in panel models")
  test[T,1]=paste0("No serial correlation")
  pbgtest(x) #Breusch-Godfrey/Wooldridge test for serial correlation in panel models -H0 no serial correlation
  Fstat=round(pbgtest(x)$statistic, decim)
  pvalue=round(pbgtest(x)$p.value, decim)
  test[T,2]=paste0(Fstat, star(pvalue), " (", pvalue, ")")
  alt=pbgtest(x)$alternative
  if(pvalue>prob)
  {
    test[T,3]=paste("cannot reject H0 => No serial correlation.")
  } else {
    test[T,3]=paste0("reject H0=>", alt)
  }
  T=T+1

  #Breusch-Pagan test for heteroskedasticity: The residuals are homoskedstic##
  rownames(test)[T]=paste0("Breusch-Pagan test for heteroskedasticity")
  test[T,1]=paste0("The residuals are homoskedstic")
  bptest(x) #Breusch-Pagan test for heteroskedasticity
  Fstat=round(bptest(x)$statistic, decim)
  pvalue=round(bptest(x)$p.value, decim)
  test[T,2]=paste0(Fstat, star(pvalue), " (", pvalue, ")")
  alt=bptest(x)$alternative
  if(pvalue>prob)
  {
    test[T,3]=paste("cannot reject H0 => The residuals are homoskedstic")
  } else {
    test[T,3]=paste0("reject H0=> The residuals are heteroskedastic")
  }

  test[rowSums(is.na(test))!= ncol(test),]
}
