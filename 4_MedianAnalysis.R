######### After cleaning data, you can use cleaned data ('PFAS_MYOPIA_DATA1.RData') to analysis.
rm(list=ls())
library(mediation)
library(survey)
options(survey.lonely.psu = "adjust")
setwd('C:\\cabri\\myopia\\250325myopia\\GITHUB\\NHANESDATA')

load('PFAS_MYOPIA_DATA1.RData')
data2 <- data1
### Exclude those missing serum ALB (n=2)
data2 <- data2[!is.na(data2[,'LBXSAL']),]
data2$class <- ifelse(data2$LBXSAL<4.2,1,
                      ifelse(data2$LBXSAL<4.4,2,
                             ifelse(data2$LBXSAL<4.6,3,4)))
nhs <- svydesign(id=~SDMVPSU,
                 strata = ~SDMVSTRA,
                 weights = ~wt, 
                 nest      = TRUE,
                 data      = data2)
svyquantile(~LBXSAL,nhs,na=TRUE,c(.25,.50,.75),ci=TRUE)


### Table 5 Association between PFAS and serum ALB
#####  Crude model
summary(glm_result <- svyglm(LBXSAL~pfhs,nhs
                             ,na.action=na.omit))
coef(glm_result <- svyglm(LBXSAL~pfhs,nhs
                          ,na.action=na.omit))
confint(glm_result <- svyglm(LBXSAL~pfhs,nhs
                             ,na.action=na.omit))

summary(glm_result <- svyglm(LBXSAL~pfna,nhs
                             ,na.action=na.omit))
coef(glm_result <- svyglm(LBXSAL~pfna,nhs
                          ,na.action=na.omit))
confint(glm_result <- svyglm(LBXSAL~pfna,nhs
                             ,na.action=na.omit))

summary(glm_result <- svyglm(LBXSAL~pfoa,nhs
                             ,na.action=na.omit))
coef(glm_result <- svyglm(LBXSAL~pfoa,nhs
                          ,na.action=na.omit))
confint(glm_result <- svyglm(LBXSAL~pfoa,nhs
                             ,na.action=na.omit))

summary(glm_result <- svyglm(LBXSAL~pfos,nhs
                             ,na.action=na.omit))
coef(glm_result <- svyglm(LBXSAL~pfos,nhs
                          ,na.action=na.omit))
confint(glm_result <- svyglm(LBXSAL~pfos,nhs
                             ,na.action=na.omit))
##### Model 1
summary(glm_result <- svyglm(LBXSAL~pfhs+RIDAGEYR+RIAGENDR+factor(race3)+factor(bmi3)+INDFMPIR+SMD410+factor(EDU2),nhs
                             ,na.action=na.omit))
coef(glm_result <- svyglm(LBXSAL~pfhs+RIDAGEYR+RIAGENDR+factor(race3)+factor(bmi3)+INDFMPIR+SMD410+factor(EDU2),nhs
                          ,na.action=na.omit))
confint(glm_result <- svyglm(LBXSAL~pfhs+RIDAGEYR+RIAGENDR+factor(race3)+factor(bmi3)+INDFMPIR+SMD410+factor(EDU2),nhs
                             ,na.action=na.omit))

summary(glm_result <- svyglm(LBXSAL~pfna+RIDAGEYR+RIAGENDR+factor(race3)+factor(bmi3)+INDFMPIR+SMD410+factor(EDU2),nhs
                             ,na.action=na.omit))
coef(glm_result <- svyglm(LBXSAL~pfna+RIDAGEYR+RIAGENDR+factor(race3)+factor(bmi3)+INDFMPIR+SMD410+factor(EDU2),nhs
                          ,na.action=na.omit))
confint(glm_result <- svyglm(LBXSAL~pfna+RIDAGEYR+RIAGENDR+factor(race3)+factor(bmi3)+INDFMPIR+SMD410+factor(EDU2),nhs
                             ,na.action=na.omit))

summary(glm_result <- svyglm(LBXSAL~pfoa+RIDAGEYR+RIAGENDR+factor(race3)+factor(bmi3)+INDFMPIR+SMD410+factor(EDU2),nhs
                             ,na.action=na.omit))
coef(glm_result <- svyglm(LBXSAL~pfoa+RIDAGEYR+RIAGENDR+factor(race3)+factor(bmi3)+INDFMPIR+SMD410+factor(EDU2),nhs
                          ,na.action=na.omit))
confint(glm_result <- svyglm(LBXSAL~pfoa+RIDAGEYR+RIAGENDR+factor(race3)+factor(bmi3)+INDFMPIR+SMD410+factor(EDU2),nhs
                             ,na.action=na.omit))

summary(glm_result <- svyglm(LBXSAL~pfos+RIDAGEYR+RIAGENDR+factor(race3)+factor(bmi3)+INDFMPIR+SMD410+factor(EDU2),nhs
                             ,na.action=na.omit))
coef(glm_result <- svyglm(LBXSAL~pfos+RIDAGEYR+RIAGENDR+factor(race3)+factor(bmi3)+INDFMPIR+SMD410+factor(EDU2),nhs
                          ,na.action=na.omit))
confint(glm_result <- svyglm(LBXSAL~pfos+RIDAGEYR+RIAGENDR+factor(race3)+factor(bmi3)+INDFMPIR+SMD410+factor(EDU2),nhs
                             ,na.action=na.omit))

###  Table 6 Association between serum ALB and mypia

summary(glm_result <- svyglm(myopia1~LBXSAL+RIDAGEYR+RIAGENDR+factor(race3)+factor(bmi3)+INDFMPIR+factor(EDU2)+SMD410,nhs
                             ,family='quasibinomial',na.action=na.omit))
exp(coef(glm_result <- svyglm(myopia1~LBXSAL+RIDAGEYR+RIAGENDR+factor(race3)+factor(bmi3)+INDFMPIR+factor(EDU2)+SMD410,nhs
                              ,family='quasibinomial',na.action=na.omit)))
exp(confint(glm_result <- svyglm(myopia1~LBXSAL+RIDAGEYR+RIAGENDR+factor(race3)+factor(bmi3)+INDFMPIR+factor(EDU2)+SMD410,nhs
                                 ,family='quasibinomial',na.action=na.omit)))

summary(glm_result <- svyglm(myopia1~factor(class)+RIDAGEYR+RIAGENDR+factor(race3)+factor(bmi3)+INDFMPIR+factor(EDU2)+SMD410,nhs
                             ,family='quasibinomial',na.action=na.omit))
exp(coef(glm_result <- svyglm(myopia1~factor(class)+RIDAGEYR+RIAGENDR+factor(race3)+factor(bmi3)+INDFMPIR+factor(EDU2)+SMD410,nhs
                              ,family='quasibinomial',na.action=na.omit)))
exp(confint(glm_result <- svyglm(myopia1~factor(class)+RIDAGEYR+RIAGENDR+factor(race3)+factor(bmi3)+INDFMPIR+factor(EDU2)+SMD410,nhs
                                 ,family='quasibinomial',na.action=na.omit)))


###  Table 7 Mediation analysis
pfhs1 =  svyglm(LBXSAL~pfhs+RIDAGEYR+RIAGENDR+BMXBMI+factor(race3)+INDFMPIR+factor(EDU2)+SMD410, nhs)
pfhs2 =  svyglm(myopia1~pfhs+LBXSAL+RIDAGEYR+RIAGENDR+factor(race3)+BMXBMI+INDFMPIR+factor(EDU2)+SMD410, nhs,family = 'quasibinomial' )
##1133
set.seed(3)
pfhs3 <- mediate(pfhs1,pfhs2,treat = 'pfhs',mediator = 'LBXSAL',sims=500)
summary(pfhs3)

pfna1 =  svyglm(LBXSAL~pfna+RIDAGEYR+RIAGENDR+BMXBMI+factor(race3)+INDFMPIR+factor(EDU2)+SMD410, nhs)
pfna2 =  svyglm(myopia1~pfna+LBXSAL+RIDAGEYR+RIAGENDR+factor(race3)+BMXBMI+INDFMPIR+factor(EDU2)+SMD410, nhs,family = 'quasibinomial' )
##1133
set.seed(3)
pfna3 <- mediate(pfna1,pfna2,treat = 'pfna',mediator = 'LBXSAL',sims=500)
summary(pfna3)

pfoa1 =  svyglm(LBXSAL~pfoa+RIDAGEYR+RIAGENDR+BMXBMI+factor(race3)+INDFMPIR+factor(EDU2)+SMD410, nhs)

pfoa2 =  svyglm(myopia1~pfoa+LBXSAL+RIDAGEYR+RIAGENDR+factor(race3)+BMXBMI+INDFMPIR+factor(EDU2)+SMD410, nhs,family = 'quasibinomial' )
##1133
set.seed(3)
pfoa3 <- mediate(pfoa1,pfoa2,treat = 'pfoa',mediator = 'LBXSAL',sims=500)
summary(pfoa3)

pfos1 =  svyglm(LBXSAL~pfos+RIDAGEYR+RIAGENDR+BMXBMI+factor(race3)+INDFMPIR+factor(EDU2)+SMD410, nhs)
pfos2 =  svyglm(myopia1~pfos+LBXSAL+RIDAGEYR+RIAGENDR+factor(race3)+BMXBMI+INDFMPIR+factor(EDU2)+SMD410, nhs,family = 'quasibinomial' )
##1133
set.seed(3)
pfos3 <- mediate(pfos1,pfos2,treat = 'pfos',mediator = 'LBXSAL',sims=500)
summary(pfos3)



trace(mediation:::print.summary.mediate, 
      at = 11,
      tracer = quote({
        printCoefmat <- function(x, digits) {
          p <- x[, 4] #p-values seem to be stored rounded
          x[, 1:3] <- sprintf("%.8f", x[, 1:3])
          x[, 4] <- sprintf("%.3f", p)
          print(x, quote = FALSE, right = TRUE)
        } 
      }),
      print = FALSE)

mediation:::print.summary.mediate(summary(pfhs3))
mediation:::print.summary.mediate(summary(pfna3))
mediation:::print.summary.mediate(summary(pfoa3))
mediation:::print.summary.mediate(summary(pfos3))