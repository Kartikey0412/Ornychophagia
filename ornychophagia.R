library(rockchalk)
library(ggplot2)
library(glm)

#reading csv file
n <- read.csv("nail.csv")
#removing single patient with incomplete data 
n <- n[-103,]

#Mean age for groups where people have bitten nails for >1 month and people have not bitten nails for >1 month
meanage <- aggregate(n$How.old.are.you..the.patient.. ~ n$Do.you..the.patient..or.have.you.ever.bitten.your.fingernail.s..for.prolonged.periods.of.time..i.e.more.than.a.month.., n,mean)


#Function for chisq tests
chifun <- function(x,y){
  x <- as.factor(x)
  y <- as.factor(y)
  c <- chisq.test(x,y)
  return (c$p.value)
}

#Function for one-way analysis of variance with x as the factor independent varialbe and y as the continous dependent variable 
aovfunc <- function(x,y){
  x <- as.factor(x)
  a <- aov(y~x)
  t <- TukeyHSD(a)
  return (data.frame(summary(a)[[1]][[1,"Pr(>F)"]]))
  result<-data.frame(t$x)
  return (data.frame(result["p.adj"]))
}


n$when <- apply(n[36:40], 1, function(x) names(n[36:40])[which.max(x)])
n$how <- apply(n[2:4], 1, function(x) names(n)[which.max(x)+1])
n$how <- apply(n[42:45], 1, function(x) names(n[42:45])[which.max(x)])
n$Which.fingernails.do.you..the.patient..bite.most.often.<-  combineLevels(n$Which.fingernails.do.you..the.patient..bite.most.often., levs=c("","Fingernails and toenails"), newLabel = c("Fingernails and toenails"))
n$Which.fingernails.do.you..the.patient..bite.most.often.<-  combineLevels(n$Which.fingernails.do.you..the.patient..bite.most.often., levs=c("Fingernails and toenails", "Toenails"), newLabel = c("Fingernails and toenails"))
n$How.often.would.you..the.patient..bite.your.fingernails. <- combineLevels(n$How.often.would.you..the.patient..bite.your.fingernails., levs=c("Rarely","At least once every 2-3 days", "At least once a week", "At least once a month"), newLabel = c("Less than once a day"))

gender <- n$Are.you..the.patient..male.or.female.
how <- n$How.often.would.you..the.patient..bite.your.fingernails.
psych <- na$Have.you..the.patient..ever.been.diag0sed.with.a.psychiatric.disorder.
first <- na$How.old.were.you..the.patient..when.you.started.biting.your.fingernails.
left_right  <- na$Are.you..the.patient..left.handed.or.right.handed.
family <- na$Any.family.members.bite.their.fingernails.
treat <- na$Have.you..the.patient..sought.treatment.for.your.nail.biting.habit.
which <- n$Which.fingernails.do.you..the.patient..bite.most.often.
#when <- relevel(na3$when,ref = "When.are.you..the.patient..most.likely.to.bite.your.nails...choice.Other.")
feel <- n$How.do.you..the.patient..feel.about.biting.your.nails...choice
#na23b$threemon <- na21$Have.you..the.patient..bitten.your.nails.within.the.last.3.months.
#na23b$time <- na.omit(na3$Do.you..the.patient..or.have.you.ever.bitten.your.fingernail.s..for.prolonged.periods.of.time..i.e.more.than.a.month..)
age <- nasub1$How.old.are.you..the.patient..
n$prologned <- na1$Do.you..the.patient..or.have.you.ever.bitten.your.fingernail.s..for.prolonged.periods.of.time..i.e.more.than.a.month..




#converting pysch to factor variable
#na2$Have.you..the.patient..ever.been.diag0sed.with.a.psychiatric.disorder. <- as.factor(na2$Have.you..the.patient..ever.been.diag0sed.with.a.psychiatric.disorder.)

#gen1tbl <- tablefun(na1$Are.you..the.patient..male.or.female.,na1$Do.you..the.patient..or.have.you.ever.bitten.your.fingernail.s..for.prolonged.periods.of.time..i.e.more.than.a.month..)
#chisq(gen1tbl)

#psych1tbl <- tablefun(na1$Have.you..the.patient..ever.been.diag0sed.with.a.psychiatric.disorder., na1$Do.you..the.patient..or.have.you.ever.bitten.your.fingernail.s..for.prolonged.periods.of.time..i.e.more.than.a.month..)
#chisq.test(psych1tbl)

#QQ plot for nomrality testing of age across the groups
qqnorm(n$age[n$prolonged == 0], main = 'Prolonged no')
qqline(n$age[n$prolonged == 0])
qqnorm(n$age[n$prolonged == 1], main = 'Prolonged yes')
qqline(n$age[n$prolonged == 1])

#Mann Whitney Test for Age across the groups
wilcox.test(n$age ~ n$prolonged)


#Logistic Regression, with covariates age, gender and psych
nlog1 <- glm(na1$Do.you..the.patient..or.have.you.ever.bitten.your.fingernail.s..for.prolonged.periods.of.time..i.e.more.than.a.month..~na1$How.old.are.you..the.patient..+na1$Are.you..the.patient..male.or.female.+na1$Have.you..the.patient..ever.been.diag0sed.with.a.psychiatric.disorder., family ="binomial")
summary(nlog1)
ctable1 <- coef(summary(nlog1))
p1 <- pnorm(abs(ctable1[, "t value"]), lower.tail = FALSE) * 2
ctable1 <- cbind(ctable1, "p value" = p1)
ci1 <- confint.default(na23clog2)
exp(cbind(OR = coef(nlog1), ci1))



#plotting pysch vs gender 
gengg <- ggplot(n1, aes(x = factor(na1$Have.you..the.patient..ever.been.diag0sed.with.a.psychiatric.disorder.), y = mean,fill = factor(na1$Are.you..the.patient..male.or.female.)))






#o <- read.csv("orno.csv")
#subsetting high biters- change this to where more than once = 1
#na2 <- na1[178:281,]
n2 <- n1[n1$prolonged == 1]
#removing nulls

#na2 <- na.omit(na2)
n2 <- na.omit(n2)
#na2$Have.you..the.patient..ever.been.diag0sed.with.a.psychiatric.disorder. <- as.factor(na2$Have.you..the.patient..ever.been.diag0sed.with.a.psychiatric.disorder.)
#na2$How.often.would.you..the.patient..bite.your.fingernails.<-  combineLevels(na2$How.often.would.you..the.patient..bite.your.fingernails., levs=c("Rarely","At least once a month"), newLabel = c("Rarely"))
#na2$How.often.would.you..the.patient..bite.your.fingernails. <- ordered(na2$How.often.would.you..the.patient..bite.your.fingernails., levels=c("Less than once a day","At least once a day"))

#nasub <- na2

#combining fingernails and toenails, with toenails



na21 <- na.omit(na21)


#Logistic Regression



nahow <- glm(nasub1$how ~ age + gender + psych + first + treat + which + family + left_right + when + feel, data = , family = 'binomial')


ctable <- coef(summary(na23clog2))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint.default(na23clog2)
exp(cbind(OR = coef(na23clog2), ci))


#histogram of age when people start biting
hist(na1$How.old.were.you..the.patient..when.you.started.biting.your.fingernails., main="Histogram of Age when people start biting", xlab = "Age when people start biting their nails", ylab = "Number of people in that Age Group")


#for when
#nsub1$when <- names(nsub1[-1])[apply(nsub[-1] == 1, 1, which)]
#nsub$when <- apply(nsub[6:9], 1, function(x) names(nsub[6:9])[which.max(x)+1] )






#gender
#0.87
chisq.test(na2$Are.you..the.patient..male.or.female.,na2$How.often.would.you..the.patient..bite.your.fingernails.)

#0.56
chisq.test(na2$Are.you..the.patient..male.or.female., na2$Which.fingernails.do.you..the.patient..bite.most.often.)

#0.08
chisq.test(nasub1$Are.you..the.patient..male.or.female., nasub2$when)

#0.14
chisq.test(nasub1$Are.you..the.patient..male.or.female., nasub3$how)


#Psych
#1.0
chisq.test(na2$Have.you..the.patient..ever.been.diag0sed.with.a.psychiatric.disorder.,na2$How.often.would.you..the.patient..bite.your.fingernails.)

#0.07; 0.03
nasub1psychwhich <- chisq.test(na2$Have.you..the.patient..ever.been.diag0sed.with.a.psychiatric.disorder., na2$Which.fingernails.do.you..the.patient..bite.most.often.)

#0.35
chisq.test(nasub1$Have.you..the.patient..ever.been.diag0sed.with.a.psychiatric.disorder., nasub3$how)

#Age- 
#normal 0.01
#nasub1often <- nasub1[order(nasub1$How.often.would.you..the.patient..bite.your.fingernails.),]
t.test(nasub1often$How.old.are.you..the.patient..~nasub1often$How.often.would.you..the.patient..bite.your.fingernails.)


#non normal 0.19
wilcox.test(nasub1which$How.old.are.you..the.patient..~nasub1which$Which.fingernails.do.you..the.patient..bite.most.often.)

#cut off 38- no partiuclar reason, 81- bored, 85 to more- stresses
#normal, stressed and no partiuclar reason , 0.03


nasub2when$when <- as.factor(nasub2when$when)
nasub2aov <- aov(nasub2when$age~ nasub2when$when)
TukeyHSD(nasub2aov)
#HSD.test(nasub2aov, group=TRUE)
#tapply(nasub2when$age, nasub2when$when, mean)

nasub3$age <- nasub1$How.old.are.you..the.patient..
nasub3how <- nasub3[order(nasub3$how),]
#cut off 38- not so sure, 53- embarassesed, 54 or more- does not bother
#normal 0.65
nasub3how$how <- as.factor(nasub3how$how)
nasub3aov <- aov(nasub3how$age ~ nasub3how$how)

#Gender, sough treatment #0.09
chisq.test(na2$Are.you..the.patient..male.or.female.,na2$Have.you..the.patient..sought.treatment.for.your.nail.biting.habit.)
