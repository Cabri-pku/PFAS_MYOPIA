rm(list=ls())
library(survey)
options(survey.lonely.psu = "adjust")
setwd('C:\\cabri\\myopia\\250325myopia\\GITHUB\\NHANESDATA')
load('PFAS_MYOPIA_DATA1.RData')

### Exclude those wearing contact lens
data1 <- data1%>%filter(VIQ220<3)
data1$cl <- ifelse(data1$VIQ240==2|data1$VIQ240==3,1,0)
data1$cl[is.na(data1$cl)] = 0
table(data1$cl)
data1 <- data1%>%filter(cl==0)


data1$classhs <- ifelse(data1$LBXPFHS<1.2,1,
                        ifelse(data1$LBXPFHS<2.4,2,
                               ifelse(data1$LBXPFHS<4.6,3,4)))
data1$classna <- ifelse(data1$LBXPFNA<0.6,1,
                        ifelse(data1$LBXPFNA<0.902,2,
                               ifelse(data1$LBXPFNA<1.312,3,4)))

data1$classoa <- ifelse(data1$LBXPFOA<2.9,1,
                        ifelse(data1$LBXPFOA<4.0,2,
                               ifelse(data1$LBXPFOA<5.4,3,4)))
data1$classos <- ifelse(data1$LBXPFOS<10.6,1,
                        ifelse(data1$LBXPFOS<15.7,2,
                               ifelse(data1$LBXPFOS<24.1,3,4)))

nhs <- svydesign(id=~SDMVPSU,
                 strata = ~SDMVSTRA,
                 weights = ~wt, 
                 nest      = TRUE,
                 data      = data1)

svyquantile(~LBXPFHS+LBXPFNA+LBXPFOA+LBXPFOS,nhs,na=TRUE,c(.25,.50,.75),ci=TRUE)





####  Model 1 (continuous)
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


