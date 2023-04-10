#' Table of goodness of fit measures for multilevel logit models.
#'
#' @param x An object of subclass glmerMod - result of glmer().
#' @param null.model Specifies the model to compare with when computing proportion change in variance (PCV) and R^2GLMM. If missing, PCV is not computed. An object of subclass glmerMod - result of glmer().
#' @param decim Specifies the number of decimals to display. The default is 4.
#' @param decim_per Specifies the number of decimals to display in case of percent results. The default is 2.
#' @returns An n x 1 matrix with the random intercept, ICC, AIC, BIC, logLik, deviance.
#' If argument null.model is supplied, it also returns PCV (based on Nakagawa and Schielzeth, 2012) and marginal and conditional R^2GLMM using the r.squaredGLMM from package MuMIn (based on Nakagawa et al.,2017)
#' The rownames are the variable names.
#' @references
#' Barton, K. (2023). MuMIn: Multi-Model Inference (version 1.47.5). https://CRAN.R-project.org/package=MuMIn
#' Sommet, N., and Morselli, D. (2017). Keep Calm and Learn Multilevel Logistic Modeling: A Simplified Three-Step Procedure Using Stata, R, Mplus, and SPSS. International Review of Social Psychology, Vol.30 No.1, pp.203-218.
#' Nakagawa, S., and Schielzeth, H. (2013). A general and simple method for obtaining R2 from generalized linear mixed-effects models, Methods in Ecology and Evolution, Vol.4 No.2, 133-142.
#' Nakagawa, S., Johnson, P. C., and Schielzeth, H. (2017). The coefficient of determination R2 and intra-class correlation coefficient from generalized linear mixed-effects models revisited and expanded, Journal of the Royal Society Interface, Vol.14 No.134.
#' @examples
#' library(lme4)
#' library(boot)
#' set.seed(404)
#' beta_0=-1.4
#' beta_1=0.1
#' beta=0.5
#' age=sample(18:40, 100, replace=T)
#' gender=sample(0:1, 100, replace=T)
#' eduCat=sample(1:3, 100, replace=T)
#' groupId=sample(1:10, 100, replace=T)
#' prob=exp(beta_0 + beta_1 * age + beta * groupId) / (1 + exp(beta_0 + beta_1 * age + beta * groupId))
#' WLB=rbinom(n=100, size=1, prob=prob)
#' dataTest=as.data.frame(cbind(WLB, age, gender, eduCat, groupId))
#' first.WLB=glmer(formula=WLB ~ age + factor(gender) + I(eduCat==1) + I(eduCat==3) + (1 | groupId), data = dataTest, family = binomial, control=glmerControl(optimizer="bobyqa"), nAGQ = 0)
#' goodnessFitMatrixMLM(first.WLB)
#' goodnessFitMatrixMLM(first.WLB, decim=2, decim_per=0)
#' null.WLB=glmer(WLB ~ 1 + (1 | groupId), data = dataTest, family = binomial, control=glmerControl(optimizer="bobyqa"), nAGQ = 0)
#' goodnessFitMatrixMLM(first.WLB, null.WLB) #returns PCV, R^2GLMM in relation to null.WLB
#' goodnessFitMatrixMLM(first.WLB, null.WLB, 2, 0)

goodnessFitMatrixMLM=function(x, null.model, decim=4, decim_per=2)
{
  require(MuMIn)
  goodnessFit=as.data.frame(matrix("",nrow=11,ncol=1))
  varianceRandomIntercept=summary(x)$varcor[[1]][1]
  ICC=varianceRandomIntercept/(varianceRandomIntercept+3.29)
  #_________________INFO POINT:
  ####Sommet and Morselli (2017) - https://www.rips-irsp.com/articles/10.5334/irsp.162/ pp. 10:
  ####the and (π2/3) ≈ 3.29 refers to the standard logistic distribution, that is, the assumed level-1 variance component:
  ####We take this assumed value, as the logistic regression model does not include level-1 residual
  tableGoodness=summary(x)$AICtab
  goodnessFit[1,1]=round(varianceRandomIntercept,decim)
  rownames(goodnessFit)[1]=paste("Random intercept variance")
  goodnessFit[2,1]=round(ICC,decim)
  rownames(goodnessFit)[2]=paste("Interclass correlation (ICC)")
  for (i in 3:(2+length(tableGoodness)))
  {
    goodnessFit[i,1]=round(tableGoodness[i-2],decim)
    rownames(goodnessFit)[i]=names(tableGoodness)[i-2]
  }
  #_________________INFO POINT:
  #related articles for R-squared and PCV: Nakagawa et al(2017) and Nakagawa and Schielzeth (2013)
  ##Related article for PCV: Merlo et al. 2005a,b
  #__________________________________________
  if(missing(null.model)){
    i=i+1
  } else {
    i=i+1

    varianceRandomInterceptY=summary(null.model)$varcor[[1]][1]
    goodnessFit[i,1]=paste0(round((varianceRandomInterceptY - varianceRandomIntercept)/varianceRandomInterceptY,decim_per+2)*100, "%")
    rownames(goodnessFit)[i]=paste("Proportion change in variance (PCV)")
    i=i+1

    goodnessFit[i,1]=paste0(round(r.squaredGLMM(x, null.model)[1,1],decim_per+2)*100,"%")
    rownames(goodnessFit)[i]=paste("Marginal R-squared (R^2_GLMM(m))- Theoretical") #uses the theoretical variance of 3.29 for individual level
    i=i+1

    goodnessFit[i,1]=paste0(round(r.squaredGLMM(x, null.model)[1,2],decim_per+2)*100,"%")
    rownames(goodnessFit)[i]=paste("Conditional R-squared (R^2_GLMM(c)) - Theoretical")#uses the theoretical variance of 3.29 for individual level
    i=i+1

    goodnessFit[i,1]=paste0(round(r.squaredGLMM(x, null.model)[2,1],decim_per+2)*100,"%")
    rownames(goodnessFit)[i]=paste("Marginal R-squared (R^2_GLMM(m))- Delta")
    i=i+1

    goodnessFit[i,1]=paste0(round(r.squaredGLMM(x, null.model)[2,2],decim_per+2)*100,"%")
    rownames(goodnessFit)[i]=paste("Conditional R-squared (R^2_GLMM(c)) - Delta")
    i=i+1
  }

  goodnessFit[i,1]=summary(x)$devcomp$dims[1]
  rownames(goodnessFit)[i]=paste("No. observations")
  i=i+1
  goodnessFit[i,1]=summary(x)$devcomp$dims[5]
  rownames(goodnessFit)[i]=paste("No. countries")

  goodnessFit
}
