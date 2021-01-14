#install.packages("tidyverse")
#install.packages("caret")
#install.packages("rpart.plot")
#install.packages("pROC") 
#install.packages("randomForest")
#install.packages("Metrics")


library(Hmisc)
library(plyr)


mydata <- sasxport.get("LLCP2019.xpt")


mydata_CA <- subset(mydata, mydata$x.state == 6)

#summary(mydata_CA$x.state)

#IV

#count(mydata_CA,'addepev3')


#Demographic

#count(mydata_CA$sexvar)
#count(mydata_CA$x.sex) same
#count(mydata_CA$x.age.g)
#count(mydata_CA$x.race.g1)
#count(mydata_CA$x.imprace)
#which(is.na(mydata_CA$x.bmi5))
#View(mydata_CA$x.bmi5)


#count(mydata_CA$x.bmi5cat)

#count(mydata_CA$x.educag) #combine1,2,3

#count(mydata_CA$x.incomg)

#count(mydata_CA$marital)


#health conditions

#count(mydata_CA$x.phys14d)

#count(mydata_CA$x.ment14d)

#count(mydata_CA$checkup1)

#count(mydata_CA$x.rfhlth)

#cognition
#count(mydata_CA$decide)


#hyptertension and medication
#count(mydata_CA$x.rfhype5)

#count(mydata_CA$bpmeds)


#exercise
#count(mydata_CA$exerany2)

#count(mydata_CA$pa2min.)

#count(mydata_CA$x.totinda)

#count(mydata_CA$x.pacat2)


# create use dataframe
df_BRFSS2019 <- mydata_CA %>% select(addepev3,sexvar,x.age.g,x.imprace,x.bmi5cat,x.educag,x.incomg,marital,x.phys14d,x.ment14d,checkup1,
                                     x.rfhlth,decide,x.rfhype5,bpmeds,pa2min.,x.totinda,x.pacat2,physhlth,menthlth,poorhlth,diffalon)





# as factor and add labels to levels


#df_BRFSS2019$addepev3 <- ifelse(df_BRFSS2019$addepev3 == "1", "2","1")   

#df_BRFSS2019$addepev3 <- factor(df_BRFSS2019$addepev3,levels = c(1,2),labels = c("No","Yes"))

df_BRFSS2019$addepev3 <- factor(df_BRFSS2019$addepev3,levels = c(1,2),labels = c("yes","no"))

df_BRFSS2019$sexvar <- factor(df_BRFSS2019$sexvar,levels = c(1,2),labels = c("male","female"))

df_BRFSS2019$x.age.g <- factor(df_BRFSS2019$x.age.g, levels = c(1,2,3,4,5,6),
                               labels = c("18-24yr","25-34yr","35-44yr","45-54yr","55-64yr","65 or above"),
                               ordered = is.ordered(df_BRFSS2019$x.age.g))


df_BRFSS2019$x.imprace <- factor(df_BRFSS2019$x.imprace,levels = c(1,2,3,4,5,6),
                                 labels = c("White","Black","Asian","American Indian/Alaska","Hispanic","Others"))



df_BRFSS2019$x.bmi5cat <- factor(df_BRFSS2019$x.bmi5cat,levels = c(1,2,3,4),
                                 labels = c("Underweight", "Normal weight", "Overweight","Obese"))

 
df_BRFSS2019$x.educag  <- factor(df_BRFSS2019$x.educag, levels = c(1,2,3,4),
                                 labels = c("Did not graduate high school ",
                                            "Graduated high school",
                                            "Attended college or technical school",
                                            "Graduated from college or technical school"))

df_BRFSS2019$x.incomg <- factor(df_BRFSS2019$x.incomg, levels = c(1,2,3,4,5),
                                labels = c("less than $15000",
                                           "$15000 to less than $25000",
                                           "$25000 to less than $35000",
                                           "$35000 to less than $50000",
                                           "$50000 or more"),
                                ordered = is.ordered(df_BRFSS2019$x.incomg))


df_BRFSS2019$marital <- factor(df_BRFSS2019$marital, levels = c(1,2,3,4,5,6),
                               labels = c("Married","Divorced","Widowed","Separated",
                                          "Never married","unmarried couple"))


df_BRFSS2019$x.phys14d <- factor(df_BRFSS2019$x.phys14d, levels = c(1,2,3),
                                 labels = c("0 days","1-13 days","14 or more days"),
                                 ordered = is.ordered(df_BRFSS2019$x.phys14d))



df_BRFSS2019$x.ment14d <- factor(df_BRFSS2019$x.ment14d, levels = c(1,2,3),
                                 labels = c("0 days","1-13 days","14 or more days"),
                                 ordered = is.ordered(df_BRFSS2019$x.ment14d))



df_BRFSS2019$checkup1 <- factor(df_BRFSS2019$checkup1,levels = c(1,2,3,4),
                               labels = c("Within past year",
                                          "Within past 2 years",
                                          "Within past 5 years",
                                          "5 years or more"), ordered = is.ordered(df_BRFSS2019$checkup))

df_BRFSS2019$x.rfhlth <- factor(df_BRFSS2019$x.rfhlth, levels = c(1,2),
                                labels = c("Excellent/good general health",
                                           "fair/poor general health"))




df_BRFSS2019$x.rfhype5 <- factor(df_BRFSS2019$x.rfhype5,levels = c(2,1),
                                 labels = c("Yes","No"))


df_BRFSS2019$decide <- factor(df_BRFSS2019$decide,levels = c(1,2),
                              labels = c("Yes","No"))




df_BRFSS2019$bpmeds <- factor(df_BRFSS2019$bpmeds,levels = c(1,2),
                              labels = c("Yes","No"))
  

  

df_BRFSS2019$x.totinda <- factor(df_BRFSS2019$x.totinda,levels = c(1,2),
                                 labels = c("Had physical activities","no physical activities"))




df_BRFSS2019$x.pacat2 <- factor(df_BRFSS2019$x.pacat2, levels = c(1,2,3,4),
                                labels = c("Highly active","Active","Insufficient active","Inactive"),
                                           ordered = is.ordered(df_BRFSS2019$checkup))





df_BRFSS2019$diffalon <- factor(df_BRFSS2019$diffalon,levels = c(1,2),
                                labels = c("Yes","No"))








df_BRFSS2019$menthlth[df_BRFSS2019$menthlth == 88] <-0 
df_BRFSS2019$menthlth<-na_if(df_BRFSS2019$menthlth,77)
df_BRFSS2019$menthlth<-na_if(df_BRFSS2019$menthlth,99)

df_BRFSS2019$physhlth[df_BRFSS2019$physhlth == 88] <-0 
df_BRFSS2019$physhlth<-na_if(df_BRFSS2019$physhlth,77)
df_BRFSS2019$physhlth<-na_if(df_BRFSS2019$physhlth,99)



df_BRFSS2019$poorhlth[df_BRFSS2019$poorhlth == 88] <-0 
df_BRFSS2019$poorhlth<-na_if(df_BRFSS2019$poorhlth,77)
df_BRFSS2019$poorhlth<-na_if(df_BRFSS2019$poorhlth,99)





save(df_BRFSS2019,file = "df_BRFSS2019")

load("df_BRFSS2019")
summary(df_BRFSS2019)


library(tidyverse)
###EDA###

########depression and non-depression group #########
df_EDA <- df_BRFSS2019[complete.cases(df_BRFSS2019$addepev3),]


###########Comparisons of prevalence of depression on demographic features############
#Sex and depression - women higher prevalence

tabsex1 <- table(df_BRFSS2019$addepev3,df_BRFSS2019$sexvar)


#summary(df_EDA)

 Sexplot <-df_EDA %>% group_by(sexvar,addepev3)%>% 
 tally() %>% 
 complete(addepev3, fill = list(n = 0)) %>% 
 mutate(percentage = n / sum(n) * 100)



  ggplot(Sexplot, aes(addepev3, percentage, fill = sexvar )) + 
    geom_bar(stat = 'identity', position = 'dodge') +
    xlab("Ever told had depression")+
    labs(fill = "Sex")+
    theme_bw()

## odds ratio and risk ratio

  tabsex2 <- matrix(c(1167,4560,669,5179),nrow=2,byrow=T)
  
  colnames(tabsex2) <- c("yes","no")
  rownames(tabsex2) <- c("female","male")
  
#install.packages("epiR")
  library('epiR')
# Tools for the analysis of epidemiological data. Contains functions for directly and indirectly adjusting measures of disease frequency, 
#quantifying measures of association on the basis of single or multiple strata of count data presented in a contingency table, 
# and computing confidence intervals around incidence risk and incidence rate estimates.
  
   
epi.2by2(tabsex2, method = "cohort.count")

# with NAs

tab1 <- table(df_BRFSS2019$sexvar,df_BRFSS2019$addepev3, dnn = c("Sex", "Depression"))
print(tab1)

epi.2by2(dat = tab1, method = "cohort.count",
         conf.level = 0.95, units = 100, outcome = "as.columns")


###############

df_risk1 <- df_BRFSS2019[complete.cases(df_BRFSS2019$addepev3,df_BRFSS2019$x.rfhype5,df_BRFSS2019$x.pacat2),]


tab2 <- table(df_risk1$x.rfhype5, df_risk1$addepev3,df_risk1$x.pacat2,dnn = c("Hypertension","Depression","Exercise"))
print(tab2)

rval <- epi.2by2(dat = tab2, method = "cohort.count",
                 conf.level = 0.95, units = 100, outcome = "as.columns")
print(rval)

#summary(df_EDA6)

library(ggplot2); library(scales)

nstrata <- 1:dim(tab2)[3]
strata.lab <- paste("Strata ", nstrata, sep = "")
y.at <- c(nstrata, max(nstrata) + 1)
y.lab <- c("M-H", strata.lab = c("Highly active","Active","Insufficient active",
                                 "Inactive"))
x.at <- c(0.5, 1, 2)
or.l <- c(rval$massoc$OR.mh$lower, rval$massoc$OR.strata.cfield$lower)
or.u <- c(rval$massoc$OR.mh$upper, rval$massoc$OR.strata.cfield$upper)
or.p <- c(rval$massoc$OR.mh$est, rval$massoc$OR.strata.cfield$est)
dat <- data.frame(y.at, y.lab, or.p, or.l, or.u)

ggplot(dat, aes(or.p, y.at)) +
  geom_point() +
  geom_errorbarh(aes(xmax = or.l, xmin = or.u, height = 0.2)) +
  labs(x = "Odds ratio", y = "Strata") +
  scale_x_continuous(trans = log2_trans(), breaks = x.at,
                     limits = c(0.5,2)) + scale_y_continuous(breaks = y.at, labels = y.lab) +
  geom_vline(xintercept = 1, lwd = 1) + coord_fixed(ratio = 0.75 / 1) +
  theme(axis.title.y = element_text(vjust = 0))



###############
rval$massoc$ARisk.mh.green
rval$massoc$ARisk.mh.wald

#dat2 <- data.frame(tab2)
#tab3 <- xtabs(Freq ~ Sex + Depression + age,data = dat2)
#print(tab3)

#rval <- epi.2by2(dat = tab3, method = "cohort.count",
                 conf.level = 0.95, units = 100, outcome = "as.columns")
#print(rval)
# Age group and depression - no visible differences
 
agetable1 <- table(df_EDA$addepev3,df_EDA$x.age.g)

# Chi-square test
chi_age <- chisq.test(agetable1)

#If you want to know the most contributing cells to the total Chi-square score, 
#just have to calculate the person residual for each cell 
library(corrplot)
corrplot(chi_age$residuals, is.cor = FALSE)

contrib <- 100*chi_age$residuals^2/chi_age$statistic
round(contrib, 3)

corrplot(contrib, is.cor = FALSE,tl.cex = 1)
######
 Ageplot <-df_EDA %>% group_by(x.age.g,addepev3)%>% 
    tally() %>% 
    complete(addepev3, fill = list(n = 0)) %>% 
    mutate(percentage = n / sum(n) * 100)
  
 Ageplot1 <- subset(Ageplot,Ageplot$addepev3 == "yes")
  
  ggplot(Ageplot1, aes(reorder(x.age.g, -percentage),percentage)) + 
    geom_bar(stat = 'identity', position = 'dodge',fill = "steelblue") +
    xlab("Ever told had depression(yes)")+
    labs(fill = "Age groups")+
    theme_bw()

#race and depression - Asian lower prevalence
  
  Raceplot <-df_EDA %>% group_by(x.imprace,addepev3)%>% 
    tally() %>% 
    complete(addepev3, fill = list(n = 0)) %>% 
    mutate(percentage = n / sum(n) * 100)
  
  Raceplot1 <- subset(Raceplot,Raceplot$addepev3 == "yes")
  
  ggplot(Raceplot1, aes(reorder(x.imprace, -percentage),percentage)) + 
    geom_bar(stat = 'identity', position = 'dodge',fill = "steelblue") +
    xlab("Ever told had depression(yes)")+
    labs(fill = "Race/Ethnicity")+
    theme_bw()
    
    
    racetable1 <- table(df_EDA$addepev3,df_EDA$x.imprace) 
    chi_race <- chisq.test(racetable1)
    library(corrplot)
    corrplot(chi_race$residuals, is.cor = FALSE)
    
    contrib <- 100*chi_race$residuals^2/chi_race$statistic
    round(contrib, 3)
    
    corrplot(contrib, is.cor = FALSE,tl.cex = 1)

#Marital status and depression - Married is lower than the divoced and the others
  df_EDA1 <- df_BRFSS2019[complete.cases(df_BRFSS2019$addepev3,df_BRFSS2019$marital),]
  
  #summary(df_EDA1)
  
  
  Mariplot <-df_EDA1 %>% group_by(marital,addepev3)%>% 
    tally() %>% 
    complete(addepev3, fill = list(n = 0)) %>% 
    mutate(percentage = n / sum(n) * 100)
  
  
  ggplot(Mariplot, aes(addepev3, percentage, fill = marital)) + 
    geom_bar(stat = 'identity', position = 'dodge') +
    xlab("Ever told had depression")+
    labs(fill = "Marital Status")+
    theme_bw()
  

#Educational level and depression - big higher prevalence among those attended college
  df_EDA2 <- df_BRFSS2019[complete.cases(df_BRFSS2019$addepev3,df_BRFSS2019$x.educag),]

  #summary(df_EDA2)
 
  edutable1 <- table(df_EDA2$addepev3,df_EDA2$x.educag) 
  chi_edu <- chisq.test(edutable1)
  library(corrplot)
  corrplot(chi_edu$residuals, is.cor = FALSE,tl.cex = 0.5)

  contrib <- 100*chi_edu$residuals^2/chi_edu$statistic
  round(contrib, 3)
  
  corrplot(contrib, is.cor = FALSE,tl.cex = 0.6)
   
  Eduplot <-df_EDA2 %>% group_by(x.educag,addepev3)%>% 
    tally() %>% 
    complete(addepev3, fill = list(n = 0)) %>% 
    mutate(percentage = n / sum(n) * 100)
  
  Eduplot1 <- subset(Eduplot,Eduplot$addepev3 == "yes")
  
  ggplot(Eduplot1, aes(reorder(x.educag, -percentage), percentage)) + 
    geom_bar(stat = 'identity', position = 'dodge',fill = "steelblue") +
    xlab("Ever told had depression(yes)")+
    labs(fill = "Education level")+
    theme_bw()
  
#Income and depression - linear relationship higher income lower prevalence

  df_EDA3 <- df_BRFSS2019[complete.cases(df_BRFSS2019$addepev3,df_BRFSS2019$x.incomg),]
  
  #summary(df_EDA3)
  
  Incplot <-df_EDA3 %>% group_by(x.incomg,addepev3)%>% 
    tally() %>% 
    complete(addepev3, fill = list(n = 0)) %>% 
    mutate(percentage = n / sum(n) * 100)
  
  
  
  ggplot(Incplot, aes(addepev3, percentage, fill = x.incomg)) + 
    geom_bar(stat = 'identity', position = 'dodge') +
    xlab("Ever told had depression")+
    labs(fill = "Income level")+
    theme_bw()
  
#BMI and depression - obesew with higher prevalence

  df_EDA4 <- df_BRFSS2019[complete.cases(df_BRFSS2019$addepev3,df_BRFSS2019$x.bmi5cat),]
  
  summary(df_EDA4)
  
  BMIplot <-df_EDA4 %>% group_by(x.bmi5cat,addepev3)%>% 
    tally() %>% 
    complete(addepev3, fill = list(n = 0)) %>% 
    mutate(percentage = n / sum(n) * 100)
  
  BMIplot1 <- subset(BMIplot,BMIplot$addepev3 == "yes")
  
  ggplot(BMIplot1, aes(reorder(x.bmi5cat, -percentage), percentage)) + 
    geom_bar(stat = 'identity', position = 'dodge', fill = "steelblue") +
    xlab("Ever told had depression(yes)")+
    labs(fill = "BMI groups")+
    theme_bw()

  
  BMItable1 <- table(df_EDA4$addepev3,df_EDA4$x.bmi5cat) 
  chi_BMI <- chisq.test(BMItable1)
  library(corrplot)
  corrplot(chi_BMI$residuals, is.cor = FALSE,tl.cex = 0.5)
  
  contrib <- 100*chi_BMI$residuals^2/chi_BMI$statistic
  round(contrib, 3)
  
  corrplot(contrib, is.cor = FALSE,tl.cex = 0.8)


#########Comparison of prevalence of depression between hypertension and non-hypertension group#########
######### Among those with hypertension, comparison between those taking or not taking hypertension medication############

  df_EDA5 <- df_BRFSS2019[complete.cases(df_BRFSS2019$addepev3,df_BRFSS2019$x.rfhype5),]
  
  summary(df_EDA5)
  
  #tab2 <- table(df_EDA5$x.rfhype5,df_EDA5$addepev3, dnn = c("Hypertension", "Depression"))
  #print(tab2)
  
  tabhyp2 <- matrix(c(722,2862,1109,6850),nrow=2,byrow=T)
  
  colnames(tabhyp2) <- c("yes","no")
  rownames(tabhyp2) <- c("hypertension","no-hypertension")
  
  epi.2by2(dat = tabhyp2, method = "cohort.count",
           conf.level = 0.95, units = 100, outcome = "as.columns")
  
  
  
    hptplot <-df_EDA5 %>% group_by(x.rfhype5,addepev3)%>% 
    tally() %>% 
    complete(addepev3, fill = list(n = 0)) %>% 
    mutate(percentage = n / sum(n) * 100)
  
  ggplot(hptplot, aes(addepev3, percentage, fill = x.rfhype5)) + 
    geom_bar(stat = 'identity', position = 'dodge') +
    xlab("Ever told had depression")+
    labs(fill = "Hypertension")+
    theme_bw()

## higher in hypertension group - bit higher for those do not taking medicine
  
df_EDA5 <- df_EDA5 [which(df_EDA5$x.rfhype5 == 'Yes'), ]
summary(df_EDA5)
table(df_EDA5$sexvar,df_EDA5$addepev3)

##################################################################
tab5 <- table(df_EDA5$bpmeds,df_EDA5$addepev3, dnn = c("Medicine-taking", "Depression"))
print(tab5)

epi.2by2(dat = tab5, method = "cohort.count",
         conf.level = 0.95, units = 100, outcome = "as.columns")

####
tabMed2 <- matrix(c(227,759,492,2113),nrow=2,byrow=T)

colnames(tabMed2) <- c("yes","no")
rownames(tabMed2) <- c("no","yes")


epi.2by2(tabMed2, method = "cohort.count")

###################################################################

df_EDA6 <- df_EDA5[complete.cases(df_EDA5$addepev3,df_EDA5$bpmeds),]

hpmedplot <-df_EDA6 %>% group_by(bpmeds,addepev3)%>% 
  tally() %>% 
  complete(addepev3, fill = list(n = 0)) %>% 
  mutate(percentage = n / sum(n) * 100)



ggplot(hpmedplot, aes(addepev3, percentage, fill = bpmeds)) + 
  geom_bar(stat = 'identity', position = 'dodge') +
  xlab("Ever told had depression")+
  labs(fill = "Taking medicine for hypertension")+
  theme_bw()

######### For person with hypertension, will high physical activity level reduce prevalence of the depression###########
## linear: more active, lower prevalence of depression

df_EDA7 <- df_EDA5[complete.cases(df_EDA5$addepev3,df_EDA5$x.pacat2),]


hpactplot <-df_EDA7 %>% group_by(x.pacat2,addepev3)%>% 
  tally() %>% 
  complete(addepev3, fill = list(n = 0)) %>% 
  mutate(percentage = n / sum(n) * 100)



ggplot(hpactplot, aes(addepev3, percentage, fill = x.pacat2)) + 
  geom_bar(stat = 'identity', position = 'dodge') +
  xlab("Ever told had depression")+
  labs(fill = "Physically Active level")+
  theme_bw()



df_EDA8 <-df_EDA5[complete.cases(df_EDA5$pa2min.),]

ExerminPlot <- df_EDA8 %>% group_by(addepev3)%>% 
  summarise(mins_exercise = mean(pa2min.))


ggplot(ExerminPlot, aes(addepev3, mins_exercise)) + 
  geom_bar(stat = 'identity', position = 'dodge') +
  xlab("Ever told had depression")+
  ylab("Minuts of total physical activities per week")
  theme_bw()


##### scatter plot #####
##### physactive mins, poor mental day, poor physical day in those with hypertension
  
plot(df_EDA5$x.pacat2,df_EDA5$menthlth)
  abline(lm(df_EDA5$menthlth ~ df_EDA5$x.pacat2),col = "red")


plot(df_EDA5$x.pacat2,df_EDA5$physhlth)
  abline(lm(df_EDA5$physhlth ~ df_EDA5$x.pacat2),col = "red") 
  

plot(df_EDA5$x.pacat2,df_EDA5$poorhlth)
  abline(lm(df_EDA5$physhlth ~ df_EDA5$x.pacat2),col = "red")
#### among those had both hypertension and depression####
df_EDA9 <- df_EDA5[which(df_EDA5$addepev3 == 'yes'), ]
  
summary(df_EDA9)
  
plot(df_EDA9$x.pacat2,df_EDA9$poorhlth)
abline(lm(df_EDA9$physhlth ~ df_EDA9$x.pacat2),col = "red")




############### 2012-2019 ################
table_sex <- read.csv("SEX_line.csv")


ggplot(data = table_sex, aes(x = Year, y = Percent, group = Sex,color = Sex)) + geom_line(size = 0.5) + 
  geom_point()+ylab("Prevalence of depression * 100")+
 theme_bw()+
  geom_ribbon(aes(ymin = lower, ymax = higher), alpha = .05,colour = NA)




table_total <- read.csv("TOTAL_line.csv")

ggplot(data = table_total, aes(x = ï..Year , y = Percentage)) + geom_line(size = 0.5) + 
  geom_point()+ylab("Prevalence of depression * 100")+
  theme_bw()+
  geom_ribbon(aes(ymin = Lower, ymax = Higher), alpha = .05,colour = NA)


#####################################
########Logistic regression##########
########whole sample########Model comparisons########

#summary(df_BRFSS2019)
df_bl <- df_BRFSS2019[complete.cases(df_BRFSS2019$addepev3,df_BRFSS2019$x.bmi5cat,
                                     df_BRFSS2019$x.phys14d,df_BRFSS2019$x.educag,df_BRFSS2019$marital,
                                     df_BRFSS2019$checkup1,df_BRFSS2019$x.rfhype5,df_BRFSS2019$x.pacat2),]



df_bl <- df_bl[ , colSums(is.na(df_bl)) == 0]

df_bl <- subset(df_bl, select = -c(x.physlth,x.totinda,checkup1))

df_bl <- subset(df_bl,x.bmi5cat != "Underweight" )

model1 <- glm(formula = addepev3 ~., data = df_bl, family = binomial(link = "logit")) 

summary(model1)

confint(model1)

model1_Null <- glm(formula = addepev3 ~ 1, data = df_bl, family = binomial(link = "logit"))

1-model1$deviance/model1_Null$deviance

# same as above 1- logLik(model1)/logLik(model1_Null)

exp(0.762659) 
exp(0.269651)

######## Machine Learning##########
library(caret)
library(rpart.plot)
library(caTools)
library(pROC) 
library(Metrics)
library(plyr)

# split train-test set #
set.seed(3033)
intrain <- createDataPartition(y = df_bl$addepev3, p= 0.7, list = FALSE) #70% split based on neg and pos at response variable
training <- df_bl[intrain,]
testing <- df_bl[-intrain,]
dim(intrain); dim(training); dim(testing)


#count(training$addepev3)
#count(testing$addepev3)

#logistic regression glm# main model
myctrl <- trainControl(method = "cv", number = 5)

model_glm <- train(addepev3 ~. , 
                   data = training, method = "glm",
                   family=binomial,
                   trControl = myctrl,
                   preProcess = c("center", "scale")) 
summary(model_glm)


library(pscl)
pR2(model_glm1)

test_predic <- predict(model_glm , newdata = testing)

test_predic
confusionMatrix(testing$addepev3,test_predic,positive="Yes")

########Logistic regression##########
########hypertension sample########

df_hypt <- df_BRFSS2019[complete.cases(df_BRFSS2019$addepev3,df_BRFSS2019$x.bmi5cat,
                                     df_BRFSS2019$x.phys14d,df_BRFSS2019$x.educag,df_BRFSS2019$marital,
                                     df_BRFSS2019$checkup1,df_BRFSS2019$x.rfhype5,df_BRFSS2019$x.pacat2,df_BRFSS2019$bpmeds),]



df_hypt <- df_hypt[ , colSums(is.na(df_hypt)) == 0]

df_hypt <- subset(df_hypt, select = -c(physhlth,x.totinda,checkup1,x.rfhlth,x.rfhype5))

df_hypt <- subset(df_hypt,x.bmi5cat != "Underweight" )

model2 <- glm(formula = addepev3 ~., data = df_hypt, family = binomial(link = "logit")) 

summary(model2)

confint(model2)

model2_Null <- glm(formula = addepev3 ~ 1, data = df_hypt, family = binomial(link = "logit"))

1-model2$deviance/model2_Null$deviance

exp( 0.26104)


barplot(table(df_hypt$x.age.g))
