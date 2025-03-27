######### After cleaning data, you can use cleaned data ('PFAS_MYOPIA_DATA1.RData') to analysis.
rm(list=ls())
options(survey.lonely.psu = "adjust")
setwd('C:\\cabri\\myopia\\250325myopia\\GITHUB\\NHANESDATA')

load('PFAS_MYOPIA_DATA1.RData')

library(plotRCS)
data2 <- data1
data2$race3 <- as.factor(data2$race3)
data2$bmi3 <- as.factor(data2$bmi3)
data2$EDU2 <- as.factor(data2$EDU2)
data2$SMD410 <- as.factor(data2$SMD410)
data2$RIAGENDR <- as.factor(data2$RIAGENDR)
data2 <- data2[,c('myopia1','pfhs','pfna','pfoa','pfos','RIAGENDR','RIDAGEYR','race3','bmi3','EDU2','INDFMPIR','SMD410')]
#
rcsplot(
  data=data2,
  outcome = 'myopia1', exposure='pfhs',
  covariates = c('RIDAGEYR','RIAGENDR','race3','bmi3','EDU2','INDFMPIR','SMD410'),
  xlab="Ln-PFHxS (ng/ml)",ylab="OR (95% CI)",knots = knot(3),pvalue = TRUE,fontfamily = 'sans',linecolor = 'red',linesize = 0.5)
)

rcsplot(
  data=data2,
  outcome = 'myopia1', exposure='pfna',
  covariates = c('RIDAGEYR','RIAGENDR','race3','bmi3','EDU2','INDFMPIR','SMD410'),
  xlab="Ln-PFNA (ng/ml)",ylab="OR (95% CI)",knots = knot(3),pvalue = TRUE,fontfamily = 'sans',linecolor = 'red',linesize = 0.5)
)
rcsplot(
  data=data2,
  outcome = 'myopia1', exposure='pfoa',
  covariates = c('RIDAGEYR','RIAGENDR','race3','bmi3','EDU2','INDFMPIR','SMD410'),
  xlab="Ln-PFOA (ng/ml)",ylab="OR (95% CI)",knots = knot(3),pvalue = TRUE,fontfamily = 'sans',linecolor = 'red',linesize = 0.5)
)

rcsplot(
  data=data2,
  outcome = 'myopia1', exposure='pfos',
  covariates = c('RIDAGEYR','RIAGENDR','race3','bmi3','EDU2','INDFMPIR','SMD410'),
  xlab="Ln-PFOS (ng/ml)",ylab="OR (95% CI)",knots = knot(3),pvalue = TRUE,fontfamily = 'sans',linecolor = 'red',linesize = 0.5)
)
