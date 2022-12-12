
View(phthalates_NHANESv6)
attach(phthalates_NHANESv)
quantile(sat_fat)
summary(sat_fat)
hist(sat_fat)

quantile(MEHP)
summary(MEHP)
hist(MEHP)

normfat <-ifelse(sat_fat<0.3,0,1)
table(normfat)
normmehp <-ifelse(MEHP<4,0,1)
table(normmehp)
table(normfat,normmehp)
normfat<-ifelse(sat_fat<0.3,0,1)
table(race,normfat)
table(sex,normfat)
table(meal_home,normfat)
table(income,normfat)
hisp=table(race,normfat)
sex=table(sex,normfat)
homemeals=table(meal_home,normfat)
income=table(income,normfat)
rbind(hisp[,1:2],sex,homemeals,income)
prop.table(table(race,normfat),2)
prop.table(table(sex,normfat),2)
prop.table(table(meal_home,normfat),2)
prop.table(table(income,normfat),2)
chisq.test(race,normfat,correct = FALSE)
chisq.test(sex,normfat,correct = FALSE)
chisq.test(meal_home,normfat,correct = FALSE)
chisq.test(income,normfat,correct = FALSE) 
normfat<-ifelse(sat_fat<0.3,0,1)
normMEHP<-ifelse(MEHP<4,0,1)
table(normfat,normMEHP)
barplot(prop.table(table(normfat,normMEHP)),beside=TRUE,names=c("Below 0.3g","Above 0.3g"),col=c("blue","green"),xlab="Saturated Fat Levels",ylab="Frequency of MEHP level",main="MEHP Levels Based on Saturated Fat Levels")
legend(x=1.2,y=0.3,legend=c("Below 4","Above 4"),fill=c("blue","green"),title="MEHP level")
boxplot(MEHP~normfat,names=c("below 0.3 g","above 0.3 g"),main="Distribution of MEHP Levels by Saturated Fat Levels",ylab="MEHP Levels", xlab="Saturated Fat Levels")

normfat<-ifelse(sat_fat<0.3,0,1)
normMEHP<-ifelse(MEHP<4,0,1)
table(normMEHP,normfat)
prop.test(c(507,576),c(755,820),correct=FALSE)
prop.table(table(normMEHP,normfat))
Call:
lm(formula = MEHP ~ normfat + sex + meal_home + relevel(factor(race), 
    ref = "3"))
summary(glm(normMEHP~normfat + sex + meal_home + race+income,family=binomial (link=logit)))
 
Call:
glm(formula = normMEHP ~ normfat + sex + meal_home + race + income,
    family = binomial(link = logit))
log.out<-glm(normMEHP~normfat + sex + meal_home + race+income,family=binomial (link=logit))
summary(log.out)
 
Call:
glm(formula = normMEHP ~ normfat + sex + meal_home + race + income,
    family = binomial(link = logit))
exp(log.out$coefficients)
reg<-glm(highmehp~normfat, family=binomial (link=logit))
summary(reg)

Call:
glm(formula = highmehp ~ normfat, family = binomial(link = logit))
