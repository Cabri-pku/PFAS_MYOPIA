rm(list=ls())

#########  load package
library(foreign)
library(dplyr)
library(plyr)
library(survey)
library(mice)
options(survey.lonely.psu = "adjust")


#########  Before cleaning data, please download NHANES files in the NHANESDATA file folder, and then set your work directory.
setwd('C:\\cabri\\myopia\\250325myopia\\GITHUB\\NHANESDATA')
### NHANES 1999-2000 cycle file
demo_9900 <- read.xport('199900demo.XPT')
age_9900 <- demo_9900[,c('SEQN','RIDAGEYR','RIAGENDR','RIDRETH1','DMDEDUC3','INDFMPIR','DMDMARTL',
                         'WTMEC2YR','SDMVPSU','SDMVSTRA')]
BMI_9900 <- read.xport('199900body.XPT')
BMI_9900 <- BMI_9900[,c('SEQN','BMXBMI','BMXWAIST')]
sd_9900 <- read.xport('199900vision.XPT')
sd_9900 <- sd_9900[,c('SEQN','VIXORSM','VIXORCM','VIQ180','VIQ200','VIQ220','VIQ240')]
hs_9900 <- read.xport('199900housesmoke.XPT')
hs_9900 <- hs_9900[,c('SEQN','SMD410')]
pfas_9900 <- read.xport('199900pfas.XPT')
pfas_9900 <- pfas_9900[,c('SEQN','SPFHS','SPFHSLC','SPFNA','SPFNALC','SPFOA','SPFOALC','SPFOS','SPFOSLC')]
colnames(pfas_9900)=c('SEQN','LBXPFHS','LBDPFHSL','LBXPFNA','LBDPFNAL','LBXPFOA','LBDPFOAL','LBXPFOS','LBDPFOSL')
### NHANES 2003-2004 cycle file
demo_0304 <- read.xport('200304demo.XPT')
age_0304 <- demo_0304[,c('SEQN','RIDAGEYR','RIAGENDR','RIDRETH1','DMDEDUC3','INDFMPIR','DMDMARTL','SDMVPSU','SDMVSTRA')]
BMI_0304 <- read.xport('200304body.XPT')
BMI_0304 <- BMI_0304[,c('SEQN','BMXBMI','BMXWAIST')]
sd_0304 <- read.xport('200304vision.XPT')
sd_0304 <- sd_0304[,c('SEQN','VIXORSM','VIXORCM','VIQ180','VIQ200','VIQ220','VIQ240')]
pfas_0304 <- read.xport('200304pfas.XPT')
pfas_0304 <- pfas_0304[,c('SEQN','WTSA2YR','LBXPFHS','LBDPFHSL','LBXPFNA','LBDPFNAL','LBXPFOA','LBDPFOAL','LBXPFOS','LBDPFOSL')]
hs_0304 <- read.xport('200304housesmoke.XPT')
hs_0304 <- hs_0304[,c('SEQN','SMD410')]
### NHANES 2005-2006 cycle file
demo_0506 <- read.xport('200506demo.XPT')
age_0506 <- demo_0506[,c('SEQN','RIDAGEYR','RIAGENDR','RIDRETH1','DMDEDUC3','INDFMPIR','DMDMARTL','SDMVPSU','SDMVSTRA')]
BMI_0506 <- read.xport('200506body.XPT')
BMI_0506 <- BMI_0506[,c('SEQN','BMXBMI','BMXWAIST')]
sd_0506 <- read.xport('200506vision.XPT')
sd_0506 <- sd_0506[,c('SEQN','VIXORSM','VIXORCM','VIQ180','VIQ200','VIQ220','VIQ240')]
PFC_0506 <- read.xport('PFC_D.XPT')
pfas_0506 <- PFC_0506[,c('SEQN','WTSA2YR','LBXPFHS','LBDPFHSL','LBXPFNA','LBDPFNAL','LBXPFOA','LBDPFOAL','LBXPFOS','LBDPFOSL')]
hs_0506 <- read.xport('200506housesmoke.XPT')
hs_0506 <- hs_0506[,c('SEQN','SMD410')]
### NHANES 2007-2008 cycle file
demo_0708 <- read.xport('200708demo.XPT')
age_0708 <- demo_0708[,c('SEQN','RIDAGEYR','RIAGENDR','RIDRETH1','DMDEDUC3','INDFMPIR','DMDMARTL','SDMVPSU','SDMVSTRA')]
BMI_0708 <- read.xport('200708body.XPT')
BMI_0708 <- BMI_0708[,c('SEQN','BMXBMI','BMXWAIST')]
sd_0708 <- read.xport('200708vision.XPT')
sd_0708 <- sd_0708[,c('SEQN','VIXORSM','VIXORCM','VIQ180','VIQ200','VIQ220','VIQ240')]
PFC_0708 <- read.xport('PFC_E.XPT')
pfas_0708 <- PFC_0708[,c('SEQN','WTSC2YR','LBXPFHS','LBDPFHSL','LBXPFNA','LBDPFNAL','LBXPFOA','LBDPFOAL','LBXPFOS','LBDPFOSL')]
hs_0708 <- read.xport('200708housesmoke.XPT')
hs_0708 <- hs_0708[,c('SEQN','SMD410')]

### Serum ALB for mediation analysis.
bio_0304 <- read.xport('200304bio.XPT')
bio_0304 <- bio_0304[,c('SEQN','LBXSAL')]
bio_0506 <- read.xport('200506bio.XPT')
bio_0506 <- bio_0506[,c('SEQN','LBXSAL')]
bio_0708 <- read.xport('200708bio.XPT')
bio_0708 <- bio_0708[,c('SEQN','LBXSAL')]
bio_9900 <- read.xport('199900bio.XPT')
bio_9900 <- bio_9900[,c('SEQN','LBXSAL')]

### merge data
data_9900 <- join_all(list(age_9900,BMI_9900,sd_9900,pfas_9900,hs_9900,bio_9900),
                      by='SEQN',type='full')
data_0304 <- join_all(list(age_0304,BMI_0304,sd_0304,pfas_0304,hs_0304,bio_0304),
                      by='SEQN',type='full')
data_0506 <- join_all(list(age_0506,BMI_0506,sd_0506,pfas_0506,hs_0506,bio_0506),
                      by='SEQN',type='full')
data_0708 <- join_all(list(age_0708,BMI_0708,sd_0708,pfas_0708,hs_0708,bio_0708),
                      by='SEQN',type='full')

### According to the NHANES guideline for PFAS (https://wwwn.cdc.gov/Nchs/Data/Nhanes/Public/2007/DataFiles/PFC_E.htm), 
### those values below LOD were replaced by LOD/sqrt(2).

data_9900 <- data_9900[!is.na(data_9900[,'LBXPFHS']),]
data_9900 <- data_9900[!is.na(data_9900[,'LBXPFNA']),]
data_9900 <- data_9900[!is.na(data_9900[,'LBXPFOA']),]
data_9900 <- data_9900[!is.na(data_9900[,'LBXPFOS']),]
#data_9900 <- data_9900[!is.na(data_9900[,'LBXPFDE']),]
data_9900$LBXPFHS <- ifelse(data_9900$LBDPFHSL==0,data_9900$LBXPFHS,0.1/sqrt(2))
data_9900$LBXPFNA <- ifelse(data_9900$LBDPFNAL==0,data_9900$LBXPFNA,0.1/sqrt(2))
data_9900$LBXPFOA <- ifelse(data_9900$LBDPFOAL==0,data_9900$LBXPFOA,0.1/sqrt(2))
data_9900$LBXPFOS <- ifelse(data_9900$LBDPFOSL==0,data_9900$LBXPFOS,0.2/sqrt(2))

data_0304 <- data_0304[!is.na(data_0304[,'LBXPFHS']),]
data_0304 <- data_0304[!is.na(data_0304[,'LBXPFNA']),]
data_0304 <- data_0304[!is.na(data_0304[,'LBXPFOA']),]
data_0304 <- data_0304[!is.na(data_0304[,'LBXPFOS']),]
#data_0304 <- data_0304[!is.na(data_0304[,'LBXPFDE']),]
data_0304$LBXPFHS <- ifelse(data_0304$LBDPFHSL==0,data_0304$LBXPFHS,0.3/sqrt(2))
data_0304$LBXPFNA <- ifelse(data_0304$LBDPFNAL==0,data_0304$LBXPFNA,0.1/sqrt(2))
data_0304$LBXPFOA <- ifelse(data_0304$LBDPFOAL==0,data_0304$LBXPFOA,0.1/sqrt(2))
data_0304$LBXPFOS <- ifelse(data_0304$LBDPFOSL==0,data_0304$LBXPFOS,0.4/sqrt(2))

data_0506 <- data_0506[!is.na(data_0506[,'LBXPFHS']),]
data_0506 <- data_0506[!is.na(data_0506[,'LBXPFNA']),]
data_0506 <- data_0506[!is.na(data_0506[,'LBXPFOA']),]
data_0506 <- data_0506[!is.na(data_0506[,'LBXPFOS']),]
#data_0506 <- data_0506[!is.na(data_0506[,'LBXPFDE']),]
data_0506$LBXPFHS <- ifelse(data_0506$LBDPFHSL==0,data_0506$LBXPFHS,0.1/sqrt(2))
data_0506$LBXPFNA <- ifelse(data_0506$LBDPFNAL==0,data_0506$LBXPFNA,0.1/sqrt(2))
data_0506$LBXPFOA <- ifelse(data_0506$LBDPFOAL==0,data_0506$LBXPFOA,0.1/sqrt(2))
data_0506$LBXPFOS <- ifelse(data_0506$LBDPFOSL==0,data_0506$LBXPFOS,0.2/sqrt(2))
data_0708 <- data_0708[!is.na(data_0708[,'LBXPFHS']),]
data_0708 <- data_0708[!is.na(data_0708[,'LBXPFNA']),]
data_0708 <- data_0708[!is.na(data_0708[,'LBXPFOA']),]
data_0708 <- data_0708[!is.na(data_0708[,'LBXPFOS']),]
#data_0708 <- data_0708[!is.na(data_0708[,'LBXPFDE']),]
data_0708$LBXPFHS <- ifelse(data_0708$LBDPFHSL==0,data_0708$LBXPFHS,0.1/sqrt(2))
data_0708$LBXPFNA <- ifelse(data_0708$LBDPFNAL==0,data_0708$LBXPFNA,0.082/sqrt(2))
data_0708$LBXPFOA <- ifelse(data_0708$LBDPFOAL==0,data_0708$LBXPFOA,0.1/sqrt(2))
data_0708$LBXPFOS <- ifelse(data_0708$LBDPFOSL==0,data_0708$LBXPFOS,0.2/sqrt(2))
### Because the weight name of PFAS were different in some cycles, we renamed PFAS weight name for further analysis.
colnames(data_9900)[8]='wt'
colnames(data_0506)[18]='wt'
colnames(data_0304)[18]='wt'
colnames(data_0708)[18]='wt'

### Total n (1999-2000, 2003-2008): 9965 + 10122 + 10348 + 10149 = 40584
### Final data excluded those missing PFAS value, n = 7770
data <- rbind(data_9900,data_0304,data_0506,data_0708)

### Define Spherical Equivalent Refraction (SER)
data$sd <- data$VIXORSM + 0.5 * data$VIXORCM
### Exclude those missing SER
data <- data[!is.na(data[,'sd']),]
### Exclude those age 20 and older
data1 <- data%>%filter(RIDAGEYR<=19)

### Use multiple imputation to fill covariate BMI and FIR.
df <- data1[,c('BMXBMI','INDFMPIR')]
imp_data <- mice(df, #数据集
                 method = "rf", #采用随机森林插补
                 m=5, # 5次插补
                 printFlag = FALSE,seed = 2024 #不显示历史记录
)
dataimp <- complete(imp_data, action = 1)
df <- dataimp
data1$BMXBMI <- df$BMXBMI
data1$INDFMPIR <- df$INDFMPIR
### Exclude those missing houses moking data (n =15)
data1 <- data1 %>% filter(SMD410<3)
### Exclude those with refractive surgery or cataract surgery (n = 6)
data1 <- data1 %>% filter(VIQ180==2 & VIQ200==2)
### Exclude those were hyperopia (n = 67)
data1 <- data1 %>% filter(sd<=1)

### Define covariates
data1$EDU2 <- ifelse(data1$DMDEDUC3<=9|data1$DMDEDUC3==55|data1$DMDEDUC3==66,1,2)
data1$race3 <- ifelse(data1$RIDRETH1==3,1,
                      ifelse(data1$RIDRETH1==4,2,3))
data1$bmi3 <- ifelse(data1$BMXBMI<25,1,
                     ifelse(data1$BMXBMI<30,2,3))
### Set sample weights
data1$wt <- 1/4 * data1$wt

### Define myopia
data1$myopia1 <- ifelse(data1$sd<=(-1),1,0)
### Define myopia severity
data1$re <- ifelse(data1$sd<=(-5),4,
                   ifelse(data1$sd<=(-3),3,
                          ifelse(data1$sd<=(-1),2,1)))
### Ln-transform of PFAS
data1$pfhs <- log(data1$LBXPFHS)
data1$pfna <- log(data1$LBXPFNA)
data1$pfoa <- log(data1$LBXPFOA)
data1$pfos <- log(data1$LBXPFOS)
### Use NHANES sample weight
nhs <- svydesign(id=~SDMVPSU,
                 strata = ~SDMVSTRA,
                 weights = ~wt, 
                 nest      = TRUE,
                 data      = data1)
### Check quantile distribution and set 4 class.
svyquantile(~LBXPFHS+LBXPFNA+LBXPFOA+LBXPFOS,nhs,na=TRUE,c(.25,.50,.75),ci=TRUE)
data1$classhs <- ifelse(data1$LBXPFHS<1.2,1,
                        ifelse(data1$LBXPFHS<2.3,2,
                               ifelse(data1$LBXPFHS<4.6,3,4)))
data1$classna <- ifelse(data1$LBXPFNA<0.6,1,
                        ifelse(data1$LBXPFNA<0.902,2,
                               ifelse(data1$LBXPFNA<1.312,3,4)))

data1$classoa <- ifelse(data1$LBXPFOA<3.0,1,
                        ifelse(data1$LBXPFOA<4.0,2,
                               ifelse(data1$LBXPFOA<5.4,3,4)))
data1$classos <- ifelse(data1$LBXPFOS<10.5,1,
                        ifelse(data1$LBXPFOS<15.7,2,
                               ifelse(data1$LBXPFOS<24.1,3,4)))

### save data for further analysis n = 1971
save(data1,file='PFAS_MYOPIA_DATA1.RData')



### Table 1 characteristics
library(tableone)
### check the number of myopia patients
table(data1$myopia1)
### check characteristics of total included participants,please note that PFAS were reported as median and IQR, and others were mean and SD, or percentages.
tab <- svyCreateTableOne(vars=c('RIDAGEYR','RIAGENDR','race3','BMXBMI','EDU2','INDFMPIR','SMD410','sd','LBXPFHS','LBXPFNA','LBXPFOA','LBXPFOS'),
                         factorVars = c('RIAGENDR','race3','EDU2','SMD410'),strata = 'myopia1',
                         data=nhs)
print(tab,nonnormal=c('LBXPFHS','LBXPFNA','LBXPFOA','LBXPFOS'))
### check characteristics of total included participants stratified by myopia
taba <- svyCreateTableOne(vars=c('RIDAGEYR','RIAGENDR','race3','BMXBMI','EDU2','INDFMPIR','SMD410','sd','LBXPFHS','LBXPFNA','LBXPFOA','LBXPFOS'),
                          factorVars = c('RIAGENDR','race3','EDU2','SMD410'),
                          data=nhs)
taba
print(taba,nonnormal=c('LBXPFHS','LBXPFNA','LBXPFOA','LBXPFOS'))

### check unweighted number of characteristics of included participants
datayes <- data1%>%filter(myopia1==1)
datano <- data1%>%filter(myopia1==0)
table(data1$RIAGENDR)
table(data1$race3)
table(data1$EDU2)
table(data1$SMD410)
table(datano$RIAGENDR)
table(datano$race3)
table(datano$EDU2)
table(datano$SMD410)
table(datayes$RIAGENDR)
table(datayes$race3)
table(datayes$EDU2)
table(datayes$SMD410)

