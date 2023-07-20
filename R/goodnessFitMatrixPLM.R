#' Table of goodness of fit measures for (panel) linear models models.
#'
#' @param x An object of class lm, plm.
#' @param decim Specifies the number of decimals to display. The default is 4.
#' @param decim_per Specifies the number of decimals to display in case of percent results. The default is 2.
#' @returns An n x 1 matrix with the R-squared, Adjusted R-squared, and F-statistic (p-value).
#' Note that for Fixed-Effects Models, withih R-squareds are provided as given by the plm().
#' @references
#' Croissant, Y. et al.(2022), Package ‘plm’:linear models for panel data (Version 2.6-2).https://cran.r-project.org/package=plm
#' @examples
#' library(plm)
#' data("Grunfeld", package = "plm") ###use the models from plm package (Croissant Y. et al, 2022: pg.4):
#' pooledOLS=plm(inv ~ value + capital, data = Grunfeld, model="pooling")
#' goodnessFitMatrixPLM(pooledOLS)
#' FEM=plm(inv ~ value + capital, data = Grunfeld, model="within", effect="twoways")
#' goodnessFitMatrixPLM(FEM, 2, 2)
#' DVOLS=plm(inv ~ value + capital + factor(firm) + factor(year), data = Grunfeld, model="pooling")
#' goodnessFitMatrixPLM(DVOLS, 2, 2)

goodnessFitMatrixPLM=function(x, decim=4, decim_per=2)
{
  require(plm)
  goodnessFit=as.data.frame(matrix("",nrow=3,ncol=1))

  i=1

  r_squared=unname(summary(x)$r.squared[1])
  goodnessFit[i,1]=paste0(round(r_squared, decim_per+2)*100, "%")
  rownames(goodnessFit)[i]=paste("R-squared")
  i=i+1

  r_squared_adj=unname(summary(x)$r.squared[2])
  goodnessFit[i,1]=paste0(round(r_squared_adj, decim_per+2)*100, "%")
  rownames(goodnessFit)[i]=paste("Adj. R-squared")
  i=i+1

  F_statistic=unname(summary(x)$fstatistic$statistic)
  F_statistic_prob=unname(summary(x)$fstatistic$p.value)
  goodnessFit[i,1]=paste0(round(F_statistic, decim), " (", round(F_statistic_prob, decim),  ")")
  rownames(goodnessFit)[i]=paste("F statistic (p-value)")

  unname(goodnessFit)
}
