######### After cleaning data, you can use cleaned data ('PFAS_MYOPIA_DATA1.RData') to analysis.
rm(list=ls())
library(mediation)
options(survey.lonely.psu = "adjust")
setwd('C:\\cabri\\myopia\\250325myopia\\GITHUB\\NHANESDATA')

load('PFAS_MYOPIA_DATA1.RData')
nhs <- svydesign(id=~SDMVPSU,
                 strata = ~SDMVSTRA,
                 weights = ~wt, 
                 nest      = TRUE,
                 data      = data1)


### Table 2 Logistic regression analysis
#####  Crude model (continuous)
summary(glm_result <- svyglm(myopia1~pfhs ,nhs,family='quasibinomial',na.action=na.omit))
exp(coef(glm_result <- svyglm(myopia1~pfhs ,nhs,family='quasibinomial',na.action=na.omit)))
exp(confint(glm_result <- svyglm(myopia1~pfhs ,nhs,family='quasibinomial',na.action=na.omit)))

summary(glm_result <- svyglm(myopia1~pfna ,nhs,family='quasibinomial',na.action=na.omit))
exp(coef(glm_result <- svyglm(myopia1~pfna ,nhs,family='quasibinomial',na.action=na.omit)))
exp(confint(glm_result <- svyglm(myopia1~pfna ,nhs,family='quasibinomial',na.action=na.omit)))

summary(glm_result <- svyglm(myopia1~pfoa ,nhs,family='quasibinomial',na.action=na.omit))
exp(coef(glm_result <- svyglm(myopia1~pfoa ,nhs,family='quasibinomial',na.action=na.omit)))
exp(confint(glm_result <- svyglm(myopia1~pfoa ,nhs,family='quasibinomial',na.action=na.omit)))

summary(glm_result <- svyglm(myopia1~pfos ,nhs,family='quasibinomial',na.action=na.omit))
exp(coef(glm_result <- svyglm(myopia1~pfos ,nhs,family='quasibinomial',na.action=na.omit)))
exp(confint(glm_result <- svyglm(myopia1~pfos ,nhs,family='quasibinomial',na.action=na.omit)))
#####  Model 1 (continuous)
summary(glm_result <- svyglm(myopia1~pfhs+RIDAGEYR+RIAGENDR+factor(race3)+factor(bmi3)+INDFMPIR+factor(EDU2)+SMD410,nhs,family='quasibinomial',na.action=na.omit))
exp(coef(glm_result <- svyglm(myopia1~pfhs+RIDAGEYR+RIAGENDR+factor(race3)+factor(bmi3)+INDFMPIR+factor(EDU2)+SMD410,nhs,family='quasibinomial',na.action=na.omit)))
exp(confint(glm_result <- svyglm(myopia1~pfhs+RIDAGEYR+RIAGENDR+factor(race3)+factor(bmi3)+INDFMPIR+factor(EDU2)+SMD410,nhs,family='quasibinomial',na.action=na.omit)))

summary(glm_result <- svyglm(myopia1~pfna+RIDAGEYR+RIAGENDR+factor(race3)+factor(bmi3)+INDFMPIR+factor(EDU2)+SMD410,nhs,family='quasibinomial',na.action=na.omit))
exp(coef(glm_result <- svyglm(myopia1~pfna+RIDAGEYR+RIAGENDR+factor(race3)+factor(bmi3)+INDFMPIR+factor(EDU2)+SMD410,nhs,family='quasibinomial',na.action=na.omit)))
exp(confint(glm_result <- svyglm(myopia1~pfna+RIDAGEYR+RIAGENDR+factor(race3)+factor(bmi3)+INDFMPIR+factor(EDU2)+SMD410,nhs,family='quasibinomial',na.action=na.omit)))

summary(glm_result <- svyglm(myopia1~pfoa+RIDAGEYR+RIAGENDR+factor(race3)+factor(bmi3)+INDFMPIR+factor(EDU2)+SMD410,nhs,family='quasibinomial',na.action=na.omit))
exp(coef(glm_result <- svyglm(myopia1~pfoa+RIDAGEYR+RIAGENDR+factor(race3)+factor(bmi3)+INDFMPIR+factor(EDU2)+SMD410,nhs,family='quasibinomial',na.action=na.omit)))
exp(confint(glm_result <- svyglm(myopia1~pfoa+RIDAGEYR+RIAGENDR+factor(race3)+factor(bmi3)+INDFMPIR+factor(EDU2)+SMD410,nhs,family='quasibinomial',na.action=na.omit)))

summary(glm_result <- svyglm(myopia1~pfos+RIDAGEYR+RIAGENDR+factor(race3)+factor(bmi3)+INDFMPIR+factor(EDU2)+SMD410,nhs,family='quasibinomial',na.action=na.omit))
exp(coef(glm_result <- svyglm(myopia1~pfos+RIDAGEYR+RIAGENDR+factor(race3)+factor(bmi3)+INDFMPIR+factor(EDU2)+SMD410,nhs,family='quasibinomial',na.action=na.omit)))
exp(confint(glm_result <- svyglm(myopia1~pfos+RIDAGEYR+RIAGENDR+factor(race3)+factor(bmi3)+INDFMPIR+factor(EDU2)+SMD410,nhs,family='quasibinomial',na.action=na.omit)))
#####   Crude model (quantile)
summary(glm_result <- svyglm(myopia1~ factor(classhs) ,nhs,family='quasibinomial',na.action=na.omit))
exp(coef(glm_result <- svyglm(myopia1~ factor(classhs) ,nhs,family='quasibinomial',na.action=na.omit)))
exp(confint(glm_result <- svyglm(myopia1~ factor(classhs) ,nhs,family='quasibinomial',na.action=na.omit)))

summary(glm_result <- svyglm(myopia1~ factor(classna) ,nhs,family='quasibinomial',na.action=na.omit))
exp(coef(glm_result <- svyglm(myopia1~ factor(classna) ,nhs,family='quasibinomial',na.action=na.omit)))
exp(confint(glm_result <- svyglm(myopia1~ factor(classna) ,nhs,family='quasibinomial',na.action=na.omit)))

summary(glm_result <- svyglm(myopia1~ factor(classoa) ,nhs,family='quasibinomial',na.action=na.omit))
exp(coef(glm_result <- svyglm(myopia1~ factor(classoa) ,nhs,family='quasibinomial',na.action=na.omit)))
exp(confint(glm_result <- svyglm(myopia1~ factor(classoa) ,nhs,family='quasibinomial',na.action=na.omit)))

summary(glm_result <- svyglm(myopia1~ factor(classos) ,nhs,family='quasibinomial',na.action=na.omit))
exp(coef(glm_result <- svyglm(myopia1~ factor(classos) ,nhs,family='quasibinomial',na.action=na.omit)))
exp(confint(glm_result <- svyglm(myopia1~ factor(classos) ,nhs ,family='quasibinomial',na.action=na.omit)))
#####   Model 1 (quantile)
summary(glm_result <- svyglm(myopia1~ factor(classhs)+RIDAGEYR+RIAGENDR+factor(race3)+factor(bmi3)+INDFMPIR+factor(EDU2)+SMD410,nhs,family='quasibinomial',na.action=na.omit))
exp(coef(glm_result <- svyglm(myopia1~ factor(classhs)+RIDAGEYR+RIAGENDR+factor(race3)+factor(bmi3)+INDFMPIR+factor(EDU2)+SMD410,nhs,family='quasibinomial',na.action=na.omit)))
exp(confint(glm_result <- svyglm(myopia1~ factor(classhs)+RIDAGEYR+RIAGENDR+factor(race3)+factor(bmi3)+INDFMPIR+factor(EDU2)+SMD410,nhs,family='quasibinomial',na.action=na.omit)))

summary(glm_result <- svyglm(myopia1~ factor(classna)+RIDAGEYR+RIAGENDR+factor(race3)+factor(bmi3)+INDFMPIR+factor(EDU2)+SMD410,nhs ,family='quasibinomial',na.action=na.omit))
exp(coef(glm_result <- svyglm(myopia1~ factor(classna)+RIDAGEYR+RIAGENDR+factor(race3)+factor(bmi3)+INDFMPIR+factor(EDU2)+SMD410,nhs ,family='quasibinomial',na.action=na.omit)))
exp(confint(glm_result <- svyglm(myopia1~ factor(classna)+RIDAGEYR+RIAGENDR+factor(race3)+factor(bmi3)+INDFMPIR+factor(EDU2)+SMD410,nhs ,family='quasibinomial',na.action=na.omit)))

summary(glm_result <- svyglm(myopia1~ factor(classoa)+RIDAGEYR+RIAGENDR+factor(race3)+factor(bmi3)+INDFMPIR+factor(EDU2)+SMD410,nhs,family='quasibinomial',na.action=na.omit))
exp(coef(glm_result <- svyglm(myopia1~ factor(classoa)+RIDAGEYR+RIAGENDR+factor(race3)+factor(bmi3)+INDFMPIR+factor(EDU2)+SMD410,nhs,family='quasibinomial',na.action=na.omit)))
exp(confint(glm_result <- svyglm(myopia1~ factor(classoa)+RIDAGEYR+RIAGENDR+factor(race3)+factor(bmi3)+INDFMPIR+factor(EDU2)+SMD410,nhs,family='quasibinomial',na.action=na.omit)))

summary(glm_result <- svyglm(myopia1~ factor(classos)+RIDAGEYR+RIAGENDR+factor(race3)+factor(bmi3)+INDFMPIR+factor(EDU2)+SMD410,nhs,family='quasibinomial',na.action=na.omit))
exp(coef(glm_result <- svyglm(myopia1~ factor(classos)+RIDAGEYR+RIAGENDR+factor(race3)+factor(bmi3)+INDFMPIR+factor(EDU2)+SMD410,nhs,family='quasibinomial',na.action=na.omit)))
exp(confint(glm_result <- svyglm(myopia1~ factor(classos)+RIDAGEYR+RIAGENDR+factor(race3)+factor(bmi3)+INDFMPIR+factor(EDU2)+SMD410,nhs,family='quasibinomial',na.action=na.omit)))




### Table 3 Logistic regression analysis stratfied by myopia severity
##### Mild myopia
summary(glm_result <- svyglm(myopia1~pfhs+RIDAGEYR+RIAGENDR+factor(race3)+factor(bmi3)+INDFMPIR+factor(EDU2)+SMD410,nhs
                             ,family='quasibinomial',na.action=na.omit,subset=(re==1|re==2)))
exp(coef(glm_result <- svyglm(myopia1~pfhs+RIDAGEYR+RIAGENDR+factor(race3)+factor(bmi3)+INDFMPIR+factor(EDU2)+SMD410,nhs
                              ,family='quasibinomial',na.action=na.omit,subset=(re==1|re==2))))
exp(confint(glm_result <- svyglm(myopia1~pfhs+RIDAGEYR+RIAGENDR+factor(race3)+factor(bmi3)+INDFMPIR+factor(EDU2)+SMD410,nhs
                                 ,family='quasibinomial',na.action=na.omit,subset=(re==1|re==2))))

summary(glm_result <- svyglm(myopia1~pfna+RIDAGEYR+RIAGENDR+factor(race3)+factor(bmi3)+INDFMPIR+factor(EDU2)+SMD410,nhs
                             ,family='quasibinomial',na.action=na.omit,subset=(re==1|re==2)))
exp(coef(glm_result <- svyglm(myopia1~pfna+RIDAGEYR+RIAGENDR+factor(race3)+factor(bmi3)+INDFMPIR+factor(EDU2)+SMD410,nhs
                              ,family='quasibinomial',na.action=na.omit,subset=(re==1|re==2))))
exp(confint(glm_result <- svyglm(myopia1~pfna+RIDAGEYR+RIAGENDR+factor(race3)+factor(bmi3)+INDFMPIR+factor(EDU2)+SMD410,nhs
                                 ,family='quasibinomial',na.action=na.omit,subset=(re==1|re==2))))

summary(glm_result <- svyglm(myopia1~pfoa+RIDAGEYR+RIAGENDR+factor(race3)+factor(bmi3)+INDFMPIR+factor(EDU2)+SMD410,nhs
                             ,family='quasibinomial',na.action=na.omit,subset=(re==1|re==2)))
exp(coef(glm_result <- svyglm(myopia1~pfoa+RIDAGEYR+RIAGENDR+factor(race3)+factor(bmi3)+INDFMPIR+factor(EDU2)+SMD410,nhs
                              ,family='quasibinomial',na.action=na.omit,subset=(re==1|re==2))))
exp(confint(glm_result <- svyglm(myopia1~pfoa+RIDAGEYR+RIAGENDR+factor(race3)+factor(bmi3)+INDFMPIR+factor(EDU2)+SMD410,nhs
                                 ,family='quasibinomial',na.action=na.omit,subset=(re==1|re==2))))

summary(glm_result <- svyglm(myopia1~pfos+RIDAGEYR+RIAGENDR+factor(race3)+factor(bmi3)+INDFMPIR+factor(EDU2)+SMD410,nhs
                             ,family='quasibinomial',na.action=na.omit,subset=(re==1|re==2)))
exp(coef(glm_result <- svyglm(myopia1~pfos+RIDAGEYR+RIAGENDR+factor(race3)+factor(bmi3)+INDFMPIR+factor(EDU2)+SMD410,nhs
                              ,family='quasibinomial',na.action=na.omit,subset=(re==1|re==2))))
exp(confint(glm_result <- svyglm(myopia1~pfos+RIDAGEYR+RIAGENDR+factor(race3)+factor(bmi3)+INDFMPIR+factor(EDU2)+SMD410,nhs
                                 ,family='quasibinomial',na.action=na.omit,subset=(re==1|re==2))))
##### Moderate myopia
summary(glm_result <- svyglm(myopia1~pfhs+RIDAGEYR+RIAGENDR+factor(race3)+factor(bmi3)+INDFMPIR+factor(EDU2)+SMD410,nhs
                             ,family='quasibinomial',na.action=na.omit,subset=(re==1|re==3)))
exp(coef(glm_result <- svyglm(myopia1~pfhs+RIDAGEYR+RIAGENDR+factor(race3)+factor(bmi3)+INDFMPIR+factor(EDU2)+SMD410,nhs
                              ,family='quasibinomial',na.action=na.omit,subset=(re==1|re==3))))
exp(confint(glm_result <- svyglm(myopia1~pfhs+RIDAGEYR+RIAGENDR+factor(race3)+factor(bmi3)+INDFMPIR+factor(EDU2)+SMD410,nhs
                                 ,family='quasibinomial',na.action=na.omit,subset=(re==1|re==3))))

summary(glm_result <- svyglm(myopia1~pfna+RIDAGEYR+RIAGENDR+factor(race3)+factor(bmi3)+INDFMPIR+factor(EDU2)+SMD410,nhs
                             ,family='quasibinomial',na.action=na.omit,subset=(re==1|re==3)))
exp(coef(glm_result <- svyglm(myopia1~pfna+RIDAGEYR+RIAGENDR+factor(race3)+factor(bmi3)+INDFMPIR+factor(EDU2)+SMD410,nhs
                              ,family='quasibinomial',na.action=na.omit,subset=(re==1|re==3))))
exp(confint(glm_result <- svyglm(myopia1~pfna+RIDAGEYR+RIAGENDR+factor(race3)+factor(bmi3)+INDFMPIR+factor(EDU2)+SMD410,nhs
                                 ,family='quasibinomial',na.action=na.omit,subset=(re==1|re==3))))

summary(glm_result <- svyglm(myopia1~pfoa+RIDAGEYR+RIAGENDR+factor(race3)+factor(bmi3)+INDFMPIR+factor(EDU2)+SMD410,nhs
                             ,family='quasibinomial',na.action=na.omit,subset=(re==1|re==3)))
exp(coef(glm_result <- svyglm(myopia1~pfoa+RIDAGEYR+RIAGENDR+factor(race3)+factor(bmi3)+INDFMPIR+factor(EDU2)+SMD410,nhs
                              ,family='quasibinomial',na.action=na.omit,subset=(re==1|re==3))))
exp(confint(glm_result <- svyglm(myopia1~pfoa+RIDAGEYR+RIAGENDR+factor(race3)+factor(bmi3)+INDFMPIR+factor(EDU2)+SMD410,nhs
                                 ,family='quasibinomial',na.action=na.omit,subset=(re==1|re==3))))

summary(glm_result <- svyglm(myopia1~pfos+RIDAGEYR+RIAGENDR+factor(race3)+factor(bmi3)+INDFMPIR+factor(EDU2)+SMD410,nhs
                             ,family='quasibinomial',na.action=na.omit,subset=(re==1|re==3)))
exp(coef(glm_result <- svyglm(myopia1~pfos+RIDAGEYR+RIAGENDR+factor(race3)+factor(bmi3)+INDFMPIR+factor(EDU2)+SMD410,nhs
                              ,family='quasibinomial',na.action=na.omit,subset=(re==1|re==3))))
exp(confint(glm_result <- svyglm(myopia1~pfos+RIDAGEYR+RIAGENDR+factor(race3)+factor(bmi3)+INDFMPIR+factor(EDU2)+SMD410,nhs
                                 ,family='quasibinomial',na.action=na.omit,subset=(re==1|re==3))))
##### High myopia
summary(glm_result <- svyglm(myopia1~pfhs+RIDAGEYR+RIAGENDR+factor(race3)+factor(bmi3)+INDFMPIR+factor(EDU2)+SMD410,nhs
                             ,family='quasibinomial',na.action=na.omit,subset=(re==1|re==4)))
exp(coef(glm_result <- svyglm(myopia1~pfhs+RIDAGEYR+RIAGENDR+factor(race3)+factor(bmi3)+INDFMPIR+factor(EDU2)+SMD410,nhs
                              ,family='quasibinomial',na.action=na.omit,subset=(re==1|re==4))))
exp(confint(glm_result <- svyglm(myopia1~pfhs+RIDAGEYR+RIAGENDR+factor(race3)+factor(bmi3)+INDFMPIR+factor(EDU2)+SMD410,nhs
                                 ,family='quasibinomial',na.action=na.omit,subset=(re==1|re==4))))

summary(glm_result <- svyglm(myopia1~pfna+RIDAGEYR+RIAGENDR+factor(race3)+factor(bmi3)+INDFMPIR+factor(EDU2)+SMD410,nhs
                             ,family='quasibinomial',na.action=na.omit,subset=(re==1|re==4)))
exp(coef(glm_result <- svyglm(myopia1~pfna+RIDAGEYR+RIAGENDR+factor(race3)+factor(bmi3)+INDFMPIR+factor(EDU2)+SMD410,nhs
                              ,family='quasibinomial',na.action=na.omit,subset=(re==1|re==4))))
exp(confint(glm_result <- svyglm(myopia1~pfna+RIDAGEYR+RIAGENDR+factor(race3)+factor(bmi3)+INDFMPIR+factor(EDU2)+SMD410,nhs
                                 ,family='quasibinomial',na.action=na.omit,subset=(re==1|re==4))))

summary(glm_result <- svyglm(myopia1~pfoa+RIDAGEYR+RIAGENDR+factor(race3)+factor(bmi3)+INDFMPIR+factor(EDU2)+SMD410,nhs
                             ,family='quasibinomial',na.action=na.omit,subset=(re==1|re==4)))
exp(coef(glm_result <- svyglm(myopia1~pfoa+RIDAGEYR+RIAGENDR+factor(race3)+factor(bmi3)+INDFMPIR+factor(EDU2)+SMD410,nhs
                              ,family='quasibinomial',na.action=na.omit,subset=(re==1|re==4))))
exp(confint(glm_result <- svyglm(myopia1~pfoa+RIDAGEYR+RIAGENDR+factor(race3)+factor(bmi3)+INDFMPIR+factor(EDU2)+SMD410,nhs
                                 ,family='quasibinomial',na.action=na.omit,subset=(re==1|re==4))))

summary(glm_result <- svyglm(myopia1~pfos+RIDAGEYR+RIAGENDR+factor(race3)+factor(bmi3)+INDFMPIR+factor(EDU2)+SMD410,nhs
                             ,family='quasibinomial',na.action=na.omit,subset=(re==1|re==4)))
exp(coef(glm_result <- svyglm(myopia1~pfos+RIDAGEYR+RIAGENDR+factor(race3)+factor(bmi3)+INDFMPIR+factor(EDU2)+SMD410,nhs
                              ,family='quasibinomial',na.action=na.omit,subset=(re==1|re==4))))
exp(confint(glm_result <- svyglm(myopia1~pfos+RIDAGEYR+RIAGENDR+factor(race3)+factor(bmi3)+INDFMPIR+factor(EDU2)+SMD410,nhs
                                 ,family='quasibinomial',na.action=na.omit,subset=(re==1|re==4))))



### Table 4 Subgroup analysis
### SUBGROUP
#####  Age
####### <16
summary(glm_result <- svyglm(myopia1~ pfhs+RIAGENDR+factor(race3)+factor(bmi3)+INDFMPIR+factor(EDU2)+SMD410,nhs,subset=(RIDAGEYR<16)
                             ,family='quasibinomial',na.action=na.omit))
exp(coef(glm_result <- svyglm(myopia1~ pfhs+RIAGENDR+factor(race3)+factor(bmi3)+INDFMPIR+factor(EDU2)+SMD410,nhs,subset=(RIDAGEYR<16)
                              ,family='quasibinomial',na.action=na.omit)))
exp(confint(glm_result <- svyglm(myopia1~ pfhs+RIAGENDR+factor(race3)+factor(bmi3)+INDFMPIR+factor(EDU2)+SMD410,nhs,subset=(RIDAGEYR<16)
                                 ,family='quasibinomial',na.action=na.omit)))

summary(glm_result <- svyglm(myopia1~ pfna+RIAGENDR+factor(race3)+factor(bmi3)+INDFMPIR+factor(EDU2)+SMD410,nhs,subset=(RIDAGEYR<16)
                             ,family='quasibinomial',na.action=na.omit))
exp(coef(glm_result <- svyglm(myopia1~ pfna+RIAGENDR+factor(race3)+factor(bmi3)+INDFMPIR+factor(EDU2)+SMD410,nhs,subset=(RIDAGEYR<16)
                              ,family='quasibinomial',na.action=na.omit)))
exp(confint(glm_result <- svyglm(myopia1~ pfna+RIAGENDR+factor(race3)+factor(bmi3)+INDFMPIR+factor(EDU2)+SMD410,nhs,subset=(RIDAGEYR<16)
                                 ,family='quasibinomial',na.action=na.omit)))

summary(glm_result <- svyglm(myopia1~ pfoa+RIAGENDR+factor(race3)+factor(bmi3)+INDFMPIR+factor(EDU2)+SMD410,nhs,subset=(RIDAGEYR<16)
                             ,family='quasibinomial',na.action=na.omit))
exp(coef(glm_result <- svyglm(myopia1~ pfoa+RIAGENDR+factor(race3)+factor(bmi3)+INDFMPIR+factor(EDU2)+SMD410,nhs,subset=(RIDAGEYR<16)
                              ,family='quasibinomial',na.action=na.omit)))
exp(confint(glm_result <- svyglm(myopia1~ pfoa+RIAGENDR+factor(race3)+factor(bmi3)+INDFMPIR+factor(EDU2)+SMD410,nhs,subset=(RIDAGEYR<16)
                                 ,family='quasibinomial',na.action=na.omit)))

summary(glm_result <- svyglm(myopia1~ pfos+RIAGENDR+factor(race3)+factor(bmi3)+INDFMPIR+factor(EDU2)+SMD410,nhs,subset=(RIDAGEYR<16)
                             ,family='quasibinomial',na.action=na.omit))
exp(coef(glm_result <- svyglm(myopia1~ pfos+RIAGENDR+factor(race3)+factor(bmi3)+INDFMPIR+factor(EDU2)+SMD410,nhs,subset=(RIDAGEYR<16)
                              ,family='quasibinomial',na.action=na.omit)))
exp(confint(glm_result <- svyglm(myopia1~ pfos+RIAGENDR+factor(race3)+factor(bmi3)+INDFMPIR+factor(EDU2)+SMD410,nhs,subset=(RIDAGEYR<16)
                                 ,family='quasibinomial',na.action=na.omit)))
####### >=16
summary(glm_result <- svyglm(myopia1~ pfhs+RIAGENDR+factor(race3)+factor(bmi3)+INDFMPIR+factor(EDU2)+SMD410,nhs,subset=(RIDAGEYR>=16)
                             ,family='quasibinomial',na.action=na.omit))
exp(coef(glm_result <- svyglm(myopia1~ pfhs+RIAGENDR+factor(race3)+factor(bmi3)+INDFMPIR+factor(EDU2)+SMD410,nhs,subset=(RIDAGEYR>=16)
                              ,family='quasibinomial',na.action=na.omit)))
exp(confint(glm_result <- svyglm(myopia1~ pfhs+RIAGENDR+factor(race3)+factor(bmi3)+INDFMPIR+factor(EDU2)+SMD410,nhs,subset=(RIDAGEYR>=16)
                                 ,family='quasibinomial',na.action=na.omit)))

summary(glm_result <- svyglm(myopia1~ pfna+RIAGENDR+factor(race3)+factor(bmi3)+INDFMPIR+factor(EDU2)+SMD410,nhs,subset=(RIDAGEYR>=16)
                             ,family='quasibinomial',na.action=na.omit))
exp(coef(glm_result <- svyglm(myopia1~ pfna+RIAGENDR+factor(race3)+factor(bmi3)+INDFMPIR+factor(EDU2)+SMD410,nhs,subset=(RIDAGEYR>=16)
                              ,family='quasibinomial',na.action=na.omit)))
exp(confint(glm_result <- svyglm(myopia1~ pfna+RIAGENDR+factor(race3)+factor(bmi3)+INDFMPIR+factor(EDU2)+SMD410,nhs,subset=(RIDAGEYR>=16)
                                 ,family='quasibinomial',na.action=na.omit)))
    
summary(glm_result <- svyglm(myopia1~ pfoa+RIAGENDR+factor(race3)+factor(bmi3)+INDFMPIR+factor(EDU2)+SMD410,nhs,subset=(RIDAGEYR>=16)
                             ,family='quasibinomial',na.action=na.omit))
exp(coef(glm_result <- svyglm(myopia1~ pfoa+RIAGENDR+factor(race3)+factor(bmi3)+INDFMPIR+factor(EDU2)+SMD410,nhs,subset=(RIDAGEYR>=16)
                              ,family='quasibinomial',na.action=na.omit)))
exp(confint(glm_result <- svyglm(myopia1~ pfoa+RIAGENDR+factor(race3)+factor(bmi3)+INDFMPIR+factor(EDU2)+SMD410,nhs,subset=(RIDAGEYR>=16)
                                 ,family='quasibinomial',na.action=na.omit)))

summary(glm_result <- svyglm(myopia1~ pfos+RIAGENDR+factor(race3)+factor(bmi3)+INDFMPIR+factor(EDU2)+SMD410,nhs,subset=(RIDAGEYR>=16)
                             ,family='quasibinomial',na.action=na.omit))
exp(coef(glm_result <- svyglm(myopia1~ pfos+RIAGENDR+factor(race3)+factor(bmi3)+INDFMPIR+factor(EDU2)+SMD410,nhs,subset=(RIDAGEYR>=16)
                              ,family='quasibinomial',na.action=na.omit)))
exp(confint(glm_result <- svyglm(myopia1~ pfos+RIAGENDR+factor(race3)+factor(bmi3)+INDFMPIR+factor(EDU2)+SMD410,nhs,subset=(RIDAGEYR>=16)
                                 ,family='quasibinomial',na.action=na.omit)))


##### Sex
#######  Males
summary(glm_result <- svyglm(myopia1~ pfhs+RIDAGEYR+RIAGENDR+factor(race3)+factor(bmi3)+INDFMPIR+factor(EDU2)+SMD410,nhs,subset=(RIAGENDR==1)
                             ,family='quasibinomial',na.action=na.omit))
exp(coef(glm_result <- svyglm(myopia1~ pfhs+RIDAGEYR+RIAGENDR+factor(race3)+factor(bmi3)+INDFMPIR+factor(EDU2)+SMD410,nhs,subset=(RIAGENDR==1)
                              ,family='quasibinomial',na.action=na.omit)))
exp(confint(glm_result <- svyglm(myopia1~ pfhs+RIDAGEYR+RIAGENDR+factor(race3)+factor(bmi3)+INDFMPIR+factor(EDU2)+SMD410,nhs,subset=(RIAGENDR==1)
                                 ,family='quasibinomial',na.action=na.omit)))

summary(glm_result <- svyglm(myopia1~ pfna+RIDAGEYR+RIAGENDR+factor(race3)+factor(bmi3)+INDFMPIR+factor(EDU2)+SMD410,nhs,subset=(RIAGENDR==1)
                             ,family='quasibinomial',na.action=na.omit))
exp(coef(glm_result <- svyglm(myopia1~ pfna+RIDAGEYR+RIAGENDR+factor(race3)+factor(bmi3)+INDFMPIR+factor(EDU2)+SMD410,nhs,subset=(RIAGENDR==1)
                              ,family='quasibinomial',na.action=na.omit)))
exp(confint(glm_result <- svyglm(myopia1~ pfna+RIDAGEYR+RIAGENDR+factor(race3)+factor(bmi3)+INDFMPIR+factor(EDU2)+SMD410,nhs,subset=(RIAGENDR==1)
                                 ,family='quasibinomial',na.action=na.omit)))

summary(glm_result <- svyglm(myopia1~ pfoa+RIDAGEYR+RIAGENDR+factor(race3)+factor(bmi3)+INDFMPIR+factor(EDU2)+SMD410,nhs,subset=(RIAGENDR==1)
                             ,family='quasibinomial',na.action=na.omit))
exp(coef(glm_result <- svyglm(myopia1~ pfoa+RIDAGEYR+RIAGENDR+factor(race3)+factor(bmi3)+INDFMPIR+factor(EDU2)+SMD410,nhs,subset=(RIAGENDR==1)
                              ,family='quasibinomial',na.action=na.omit)))
exp(confint(glm_result <- svyglm(myopia1~ pfoa+RIDAGEYR+RIAGENDR+factor(race3)+factor(bmi3)+INDFMPIR+factor(EDU2)+SMD410,nhs,subset=(RIAGENDR==1)
                                 ,family='quasibinomial',na.action=na.omit)))

summary(glm_result <- svyglm(myopia1~ pfos+RIDAGEYR+RIAGENDR+factor(race3)+factor(bmi3)+INDFMPIR+factor(EDU2)+SMD410,nhs,subset=(RIAGENDR==1)
                             ,family='quasibinomial',na.action=na.omit))
exp(coef(glm_result <- svyglm(myopia1~ pfos+RIDAGEYR+RIAGENDR+factor(race3)+factor(bmi3)+INDFMPIR+factor(EDU2)+SMD410,nhs,subset=(RIAGENDR==1)
                              ,family='quasibinomial',na.action=na.omit)))
exp(confint(glm_result <- svyglm(myopia1~ pfos+RIDAGEYR+RIAGENDR+factor(race3)+factor(bmi3)+INDFMPIR+factor(EDU2)+SMD410,nhs,subset=(RIAGENDR==1)
                                 ,family='quasibinomial',na.action=na.omit)))

#######  Females
summary(glm_result <- svyglm(myopia1~ pfhs+RIDAGEYR+RIAGENDR+factor(race3)+factor(bmi3)+INDFMPIR+factor(EDU2)+SMD410,nhs,subset=(RIAGENDR==2)
                             ,family='quasibinomial',na.action=na.omit))
exp(coef(glm_result <- svyglm(myopia1~ pfhs+RIDAGEYR+RIAGENDR+factor(race3)+factor(bmi3)+INDFMPIR+factor(EDU2)+SMD410,nhs,subset=(RIAGENDR==2)
                              ,family='quasibinomial',na.action=na.omit)))
exp(confint(glm_result <- svyglm(myopia1~ pfhs+RIDAGEYR+RIAGENDR+factor(race3)+factor(bmi3)+INDFMPIR+factor(EDU2)+SMD410,nhs,subset=(RIAGENDR==2)
                                 ,family='quasibinomial',na.action=na.omit)))

summary(glm_result <- svyglm(myopia1~ pfna+RIDAGEYR+RIAGENDR+factor(race3)+factor(bmi3)+INDFMPIR+factor(EDU2)+SMD410,nhs,subset=(RIAGENDR==2)
                             ,family='quasibinomial',na.action=na.omit))
exp(coef(glm_result <- svyglm(myopia1~ pfna+RIDAGEYR+RIAGENDR+factor(race3)+factor(bmi3)+INDFMPIR+factor(EDU2)+SMD410,nhs,subset=(RIAGENDR==2)
                              ,family='quasibinomial',na.action=na.omit)))
exp(confint(glm_result <- svyglm(myopia1~ pfna+RIDAGEYR+RIAGENDR+factor(race3)+factor(bmi3)+INDFMPIR+factor(EDU2)+SMD410,nhs,subset=(RIAGENDR==2)
                                 ,family='quasibinomial',na.action=na.omit)))

summary(glm_result <- svyglm(myopia1~ pfoa+RIDAGEYR+RIAGENDR+factor(race3)+factor(bmi3)+INDFMPIR+factor(EDU2)+SMD410,nhs,subset=(RIAGENDR==2)
                             ,family='quasibinomial',na.action=na.omit))
exp(coef(glm_result <- svyglm(myopia1~ pfoa+RIDAGEYR+RIAGENDR+factor(race3)+factor(bmi3)+INDFMPIR+factor(EDU2)+SMD410,nhs,subset=(RIAGENDR==2)
                              ,family='quasibinomial',na.action=na.omit)))
exp(confint(glm_result <- svyglm(myopia1~ pfoa+RIDAGEYR+RIAGENDR+factor(race3)+factor(bmi3)+INDFMPIR+factor(EDU2)+SMD410,nhs,subset=(RIAGENDR==2)
                                 ,family='quasibinomial',na.action=na.omit)))

summary(glm_result <- svyglm(myopia1~ pfos+RIDAGEYR+RIAGENDR+factor(race3)+factor(bmi3)+INDFMPIR+factor(EDU2)+SMD410,nhs,subset=(RIAGENDR==2)
                             ,family='quasibinomial',na.action=na.omit))
exp(coef(glm_result <- svyglm(myopia1~ pfos+RIDAGEYR+RIAGENDR+factor(race3)+factor(bmi3)+INDFMPIR+factor(EDU2)+SMD410,nhs,subset=(RIAGENDR==2)
                              ,family='quasibinomial',na.action=na.omit)))
exp(confint(glm_result <- svyglm(myopia1~ pfos+RIDAGEYR+RIAGENDR+factor(race3)+factor(bmi3)+INDFMPIR+factor(EDU2)+SMD410,nhs,subset=(RIAGENDR==2)
                                 ,family='quasibinomial',na.action=na.omit)))

##### race
####### white
summary(glm_result <- svyglm(myopia1~ pfhs+RIDAGEYR+RIAGENDR+factor(bmi3)+INDFMPIR+factor(EDU2)+SMD410,nhs,subset=(race3==1)
                             ,family='quasibinomial',na.action=na.omit))
exp(coef(glm_result <- svyglm(myopia1~ pfhs+RIDAGEYR+RIAGENDR+factor(bmi3)+INDFMPIR+factor(EDU2)+SMD410,nhs,subset=(race3==1)
                              ,family='quasibinomial',na.action=na.omit)))
exp(confint(glm_result <- svyglm(myopia1~ pfhs+RIDAGEYR+RIAGENDR+factor(bmi3)+INDFMPIR+factor(EDU2)+SMD410,nhs,subset=(race3==1)
                                 ,family='quasibinomial',na.action=na.omit)))

summary(glm_result <- svyglm(myopia1~ pfna+RIDAGEYR+RIAGENDR+factor(bmi3)+INDFMPIR+factor(EDU2)+SMD410,nhs,subset=(race3==1)
                             ,family='quasibinomial',na.action=na.omit))
exp(coef(glm_result <- svyglm(myopia1~ pfna+RIDAGEYR+RIAGENDR+factor(bmi3)+INDFMPIR+factor(EDU2)+SMD410,nhs,subset=(race3==1)
                              ,family='quasibinomial',na.action=na.omit)))
exp(confint(glm_result <- svyglm(myopia1~ pfna+RIDAGEYR+RIAGENDR+factor(bmi3)+INDFMPIR+factor(EDU2)+SMD410,nhs,subset=(race3==1)
                                 ,family='quasibinomial',na.action=na.omit)))

summary(glm_result <- svyglm(myopia1~ pfoa+RIDAGEYR+RIAGENDR+factor(bmi3)+INDFMPIR+factor(EDU2)+SMD410,nhs,subset=(race3==1)
                             ,family='quasibinomial',na.action=na.omit))
exp(coef(glm_result <- svyglm(myopia1~ pfoa+RIDAGEYR+RIAGENDR+factor(bmi3)+INDFMPIR+factor(EDU2)+SMD410,nhs,subset=(race3==1)
                              ,family='quasibinomial',na.action=na.omit)))
exp(confint(glm_result <- svyglm(myopia1~ pfoa+RIDAGEYR+RIAGENDR+factor(bmi3)+INDFMPIR+factor(EDU2)+SMD410,nhs,subset=(race3==1)
                                 ,family='quasibinomial',na.action=na.omit)))

summary(glm_result <- svyglm(myopia1~ pfos+RIDAGEYR+RIAGENDR+factor(bmi3)+INDFMPIR+factor(EDU2)+SMD410,nhs,subset=(race3==1)
                             ,family='quasibinomial',na.action=na.omit))
exp(coef(glm_result <- svyglm(myopia1~ pfos+RIDAGEYR+RIAGENDR+factor(bmi3)+INDFMPIR+factor(EDU2)+SMD410,nhs,subset=(race3==1)
                              ,family='quasibinomial',na.action=na.omit)))
exp(confint(glm_result <- svyglm(myopia1~ pfos+RIDAGEYR+RIAGENDR+factor(bmi3)+INDFMPIR+factor(EDU2)+SMD410,nhs,subset=(race3==1)
                                 ,family='quasibinomial',na.action=na.omit)))
####### black
summary(glm_result <- svyglm(myopia1~ pfhs+RIDAGEYR+RIAGENDR+factor(bmi3)+INDFMPIR+factor(EDU2)+SMD410,nhs,subset=(race3==2)
                             ,family='quasibinomial',na.action=na.omit))
exp(coef(glm_result <- svyglm(myopia1~ pfhs+RIDAGEYR+RIAGENDR+factor(bmi3)+INDFMPIR+factor(EDU2)+SMD410,nhs,subset=(race3==2)
                              ,family='quasibinomial',na.action=na.omit)))
exp(confint(glm_result <- svyglm(myopia1~ pfhs+RIDAGEYR+RIAGENDR+factor(bmi3)+INDFMPIR+factor(EDU2)+SMD410,nhs,subset=(race3==2)
                                 ,family='quasibinomial',na.action=na.omit)))

summary(glm_result <- svyglm(myopia1~ pfna+RIDAGEYR+RIAGENDR+factor(bmi3)+INDFMPIR+factor(EDU2)+SMD410,nhs,subset=(race3==2)
                             ,family='quasibinomial',na.action=na.omit))
exp(coef(glm_result <- svyglm(myopia1~ pfna+RIDAGEYR+RIAGENDR+factor(bmi3)+INDFMPIR+factor(EDU2)+SMD410,nhs,subset=(race3==2)
                              ,family='quasibinomial',na.action=na.omit)))
exp(confint(glm_result <- svyglm(myopia1~ pfna+RIDAGEYR+RIAGENDR+factor(bmi3)+INDFMPIR+factor(EDU2)+SMD410,nhs,subset=(race3==2)
                                 ,family='quasibinomial',na.action=na.omit)))

summary(glm_result <- svyglm(myopia1~ pfoa+RIDAGEYR+RIAGENDR+factor(bmi3)+INDFMPIR+factor(EDU2)+SMD410,nhs,subset=(race3==2)
                             ,family='quasibinomial',na.action=na.omit))
exp(coef(glm_result <- svyglm(myopia1~ pfoa+RIDAGEYR+RIAGENDR+factor(bmi3)+INDFMPIR+factor(EDU2)+SMD410,nhs,subset=(race3==2)
                              ,family='quasibinomial',na.action=na.omit)))
exp(confint(glm_result <- svyglm(myopia1~ pfoa+RIDAGEYR+RIAGENDR+factor(bmi3)+INDFMPIR+factor(EDU2)+SMD410,nhs,subset=(race3==2)
                                 ,family='quasibinomial',na.action=na.omit)))

summary(glm_result <- svyglm(myopia1~ pfos+RIDAGEYR+RIAGENDR+factor(bmi3)+INDFMPIR+factor(EDU2)+SMD410,nhs,subset=(race3==2)
                             ,family='quasibinomial',na.action=na.omit))
exp(coef(glm_result <- svyglm(myopia1~ pfos+RIDAGEYR+RIAGENDR+factor(bmi3)+INDFMPIR+factor(EDU2)+SMD410,nhs,subset=(race3==2)
                              ,family='quasibinomial',na.action=na.omit)))
exp(confint(glm_result <- svyglm(myopia1~ pfos+RIDAGEYR+RIAGENDR+factor(bmi3)+INDFMPIR+factor(EDU2)+SMD410,nhs,subset=(race3==2)
                                 ,family='quasibinomial',na.action=na.omit)))
#######other
summary(glm_result <- svyglm(myopia1~ pfhs+RIDAGEYR+RIAGENDR+factor(bmi3)+INDFMPIR+factor(EDU2)+SMD410,nhs,subset=(race3==3)
                             ,family='quasibinomial',na.action=na.omit))
exp(coef(glm_result <- svyglm(myopia1~ pfhs+RIDAGEYR+RIAGENDR+factor(bmi3)+INDFMPIR+factor(EDU2)+SMD410,nhs,subset=(race3==3)
                              ,family='quasibinomial',na.action=na.omit)))
exp(confint(glm_result <- svyglm(myopia1~ pfhs+RIDAGEYR+RIAGENDR+factor(bmi3)+INDFMPIR+factor(EDU2)+SMD410,nhs,subset=(race3==3)
                                 ,family='quasibinomial',na.action=na.omit)))

summary(glm_result <- svyglm(myopia1~ pfna+RIDAGEYR+RIAGENDR+factor(bmi3)+INDFMPIR+factor(EDU2)+SMD410,nhs,subset=(race3==3)
                             ,family='quasibinomial',na.action=na.omit))
exp(coef(glm_result <- svyglm(myopia1~ pfna+RIDAGEYR+RIAGENDR+factor(bmi3)+INDFMPIR+factor(EDU2)+SMD410,nhs,subset=(race3==3)
                              ,family='quasibinomial',na.action=na.omit)))
exp(confint(glm_result <- svyglm(myopia1~ pfna+RIDAGEYR+RIAGENDR+factor(bmi3)+INDFMPIR+factor(EDU2)+SMD410,nhs,subset=(race3==3)
                                 ,family='quasibinomial',na.action=na.omit)))

summary(glm_result <- svyglm(myopia1~ pfoa+RIDAGEYR+RIAGENDR+factor(bmi3)+INDFMPIR+factor(EDU2)+SMD410,nhs,subset=(race3==3)
                             ,family='quasibinomial',na.action=na.omit))
exp(coef(glm_result <- svyglm(myopia1~ pfoa+RIDAGEYR+RIAGENDR+factor(bmi3)+INDFMPIR+factor(EDU2)+SMD410,nhs,subset=(race3==3)
                              ,family='quasibinomial',na.action=na.omit)))
exp(confint(glm_result <- svyglm(myopia1~ pfoa+RIDAGEYR+RIAGENDR+factor(bmi3)+INDFMPIR+factor(EDU2)+SMD410,nhs,subset=(race3==3)
                                 ,family='quasibinomial',na.action=na.omit)))

summary(glm_result <- svyglm(myopia1~ pfos+RIDAGEYR+RIAGENDR+factor(bmi3)+INDFMPIR+factor(EDU2)+SMD410,nhs,subset=(race3==3)
                             ,family='quasibinomial',na.action=na.omit))
exp(coef(glm_result <- svyglm(myopia1~ pfos+RIDAGEYR+RIAGENDR+factor(bmi3)+INDFMPIR+factor(EDU2)+SMD410,nhs,subset=(race3==3)
                              ,family='quasibinomial',na.action=na.omit)))
exp(confint(glm_result <- svyglm(myopia1~ pfos+RIDAGEYR+RIAGENDR+factor(bmi3)+INDFMPIR+factor(EDU2)+SMD410,nhs,subset=(race3==3)
                                 ,family='quasibinomial',na.action=na.omit)))

##### education
######## less than 9
summary(glm_result <- svyglm(myopia1~ pfhs+RIDAGEYR+RIAGENDR+factor(race3)+factor(bmi3)+INDFMPIR+SMD410,nhs,subset=(EDU2==1)
                             ,family='quasibinomial',na.action=na.omit))
exp(coef(glm_result <- svyglm(myopia1~ pfhs+RIDAGEYR+RIAGENDR+factor(race3)+factor(bmi3)+INDFMPIR+SMD410,nhs,subset=(EDU2==1)
                              ,family='quasibinomial',na.action=na.omit)))
exp(confint(glm_result <- svyglm(myopia1~ pfhs+RIDAGEYR+RIAGENDR+factor(race3)+factor(bmi3)+INDFMPIR+SMD410,nhs,subset=(EDU2==1)
                                 ,family='quasibinomial',na.action=na.omit)))

summary(glm_result <- svyglm(myopia1~ pfna+RIDAGEYR+RIAGENDR+factor(race3)+factor(bmi3)+INDFMPIR+SMD410,nhs,subset=(EDU2==1)
                             ,family='quasibinomial',na.action=na.omit))
exp(coef(glm_result <- svyglm(myopia1~ pfna+RIDAGEYR+RIAGENDR+factor(race3)+factor(bmi3)+INDFMPIR+SMD410,nhs,subset=(EDU2==1)
                              ,family='quasibinomial',na.action=na.omit)))
exp(confint(glm_result <- svyglm(myopia1~ pfna+RIDAGEYR+RIAGENDR+factor(race3)+factor(bmi3)+INDFMPIR+SMD410,nhs,subset=(EDU2==1)
                                 ,family='quasibinomial',na.action=na.omit)))

summary(glm_result <- svyglm(myopia1~ pfoa+RIDAGEYR+RIAGENDR+factor(race3)+factor(bmi3)+INDFMPIR+SMD410,nhs,subset=(EDU2==1)
                             ,family='quasibinomial',na.action=na.omit))
exp(coef(glm_result <- svyglm(myopia1~ pfoa+RIDAGEYR+RIAGENDR+factor(race3)+factor(bmi3)+INDFMPIR+SMD410,nhs,subset=(EDU2==1)
                              ,family='quasibinomial',na.action=na.omit)))
exp(confint(glm_result <- svyglm(myopia1~ pfoa+RIDAGEYR+RIAGENDR+factor(race3)+factor(bmi3)+INDFMPIR+SMD410,nhs,subset=(EDU2==1)
                                 ,family='quasibinomial',na.action=na.omit)))

summary(glm_result <- svyglm(myopia1~ pfos+RIDAGEYR+RIAGENDR+factor(race3)+factor(bmi3)+INDFMPIR+SMD410,nhs,subset=(EDU2==1)
                             ,family='quasibinomial',na.action=na.omit))
exp(coef(glm_result <- svyglm(myopia1~ pfos+RIDAGEYR+RIAGENDR+factor(race3)+factor(bmi3)+INDFMPIR+SMD410,nhs,subset=(EDU2==1)
                              ,family='quasibinomial',na.action=na.omit)))
exp(confint(glm_result <- svyglm(myopia1~ pfos+RIDAGEYR+RIAGENDR+factor(race3)+factor(bmi3)+INDFMPIR+SMD410,nhs,subset=(EDU2==1)
                                 ,family='quasibinomial',na.action=na.omit)))
########  ABOVE THAN 9
summary(glm_result <- svyglm(myopia1~ pfhs+RIDAGEYR+RIAGENDR+factor(race3)+factor(bmi3)+INDFMPIR+SMD410,nhs,subset=(EDU2==2)
                             ,family='quasibinomial',na.action=na.omit))
exp(coef(glm_result <- svyglm(myopia1~ pfhs+RIDAGEYR+RIAGENDR+factor(race3)+factor(bmi3)+INDFMPIR+SMD410,nhs,subset=(EDU2==2)
                              ,family='quasibinomial',na.action=na.omit)))
exp(confint(glm_result <- svyglm(myopia1~ pfhs+RIDAGEYR+RIAGENDR+factor(race3)+factor(bmi3)+INDFMPIR+SMD410,nhs,subset=(EDU2==2)
                                 ,family='quasibinomial',na.action=na.omit)))

summary(glm_result <- svyglm(myopia1~ pfna+RIDAGEYR+RIAGENDR+factor(race3)+factor(bmi3)+INDFMPIR+SMD410,nhs,subset=(EDU2==2)
                             ,family='quasibinomial',na.action=na.omit))
exp(coef(glm_result <- svyglm(myopia1~ pfna+RIDAGEYR+RIAGENDR+factor(race3)+factor(bmi3)+INDFMPIR+SMD410,nhs,subset=(EDU2==2)
                              ,family='quasibinomial',na.action=na.omit)))
exp(confint(glm_result <- svyglm(myopia1~ pfna+RIDAGEYR+RIAGENDR+factor(race3)+factor(bmi3)+INDFMPIR+SMD410,nhs,subset=(EDU2==2)
                                 ,family='quasibinomial',na.action=na.omit)))

summary(glm_result <- svyglm(myopia1~ pfoa+RIDAGEYR+RIAGENDR+factor(race3)+factor(bmi3)+INDFMPIR+SMD410,nhs,subset=(EDU2==2)
                             ,family='quasibinomial',na.action=na.omit))
exp(coef(glm_result <- svyglm(myopia1~ pfoa+RIDAGEYR+RIAGENDR+factor(race3)+factor(bmi3)+INDFMPIR+SMD410,nhs,subset=(EDU2==2)
                              ,family='quasibinomial',na.action=na.omit)))
exp(confint(glm_result <- svyglm(myopia1~ pfoa+RIDAGEYR+RIAGENDR+factor(race3)+factor(bmi3)+INDFMPIR+SMD410,nhs,subset=(EDU2==2)
                                 ,family='quasibinomial',na.action=na.omit)))

summary(glm_result <- svyglm(myopia1~ pfos+RIDAGEYR+RIAGENDR+factor(race3)+factor(bmi3)+INDFMPIR+SMD410,nhs,subset=(EDU2==2)
                             ,family='quasibinomial',na.action=na.omit))
exp(coef(glm_result <- svyglm(myopia1~ pfos+RIDAGEYR+RIAGENDR+factor(race3)+factor(bmi3)+INDFMPIR+SMD410,nhs,subset=(EDU2==2)
                              ,family='quasibinomial',na.action=na.omit)))
exp(confint(glm_result <- svyglm(myopia1~ pfos+RIDAGEYR+RIAGENDR+factor(race3)+factor(bmi3)+INDFMPIR+SMD410,nhs,subset=(EDU2==2)
                                 ,family='quasibinomial',na.action=na.omit)))

