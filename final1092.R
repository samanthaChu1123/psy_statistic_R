library(ggplot2)
library(corrplot)
library(DescTools)

resp<-read.csv("res156.csv",header=TRUE, sep=",")
dim(resp)
summary(resp)

resp$cond<-ifelse(resp$before=="home","A",
                  ifelse(resp$after=="dorm","C","B"))
resp$home<-ifelse(resp$where=="home","Yes","No")

## ?????????
summary(subset(resp,where=="home"))
summary(subset(resp,where=="dormORschool"))
summary(subset(resp,where=="cafe"))
summary(subset(resp,where=="others"))

summary(subset(resp,reason=="affection"))
summary(subset(resp,reason=="environment"))
summary(subset(resp,reason=="focus"))

##function: calculate effect size

omega <- function(aov){
  sum_stats <- summary(aov)[[1]]
  SSt <- sum_stats[["Sum Sq"]][1]
  SSe <- sum_stats[["Sum Sq"]][2]
  DF <- sum_stats[["Df"]][1]
  MSe <- sum_stats[["Mean Sq"]][2]
  W <- (SSt-DF*MSe)/(SSt+SSe+MSe)
  return(W)
}

##### home, gender, grade

## home correlation test

dtaH<-resp[,5:8]
cor(dtaH)
corrplot(cor(dtaH), method = "circle")

with(dtaH,cor.test(peopleH,acceptH))
with(dtaH,cor.test(peopleH,affectionH))
with(dtaH,cor.test(peopleH,comfortH))
with(dtaH,cor.test(affectionH,comfortH)) ## 0.0001
with(dtaH,cor.test(affectionH,acceptH)) ## <10e-4
with(dtaH,cor.test(comfortH,acceptH)) ## < 10e-11

summary(lm(acceptH~comfortH,dtaH)) # R-squared: 0.2541
summary(lm(acceptH~comfortH+affectionH,dtaH)) ### R-squared: 0.2848
summary(lm(acceptH~comfortH*affectionH,dtaH)) # R-squared: 0.2943, interaction not significant


## affectionH // different gender, grade

ggplot(resp,aes(x=gender,y=affectionH))+
  geom_boxplot()+xlab("gender")+ylab("affectionH")
ggplot(resp,aes(x=grade,y=affectionH))+
  geom_boxplot()+xlab("grade")+ylab("affectionH")

means1<-with(resp,tapply(affectionH, list(gender,grade), mean))
means1<-means1[c(0:5,7:8)]
n1<-with(resp,table(gender,grade))
n1<-n1[c(0:5,7:8)]
ses1<-with(resp,tapply(affectionH, list(gender,grade), sd))
ses1<-ses1[c(0:5,7:8)]
ses1[3]=0
ses1<-ses1/sqrt(n1)

dta1<-data.frame(means=c(means1),ses=c(ses1),
                 gender=c("F","M","O","F","M","F","M"),
                 grade=c(rep("fs",3),rep("js",2),rep("ms",2)))

ggplot(dta1,
       aes(grade,means,fill=gender))+
  geom_bar(stat="identity",position=position_dodge(0.9))+
  geom_errorbar(aes(ymin=means-ses,ymax=means+ses),width=0.2,
                position=position_dodge(0.9))+
  xlab("grade")+
  ylab("affectionH")

summary(aov(affectionH~gender,resp))
summary(aov(affectionH~gender,subset(resp,resp$gender!="O")))
summary(aov(affectionH~grade,resp))

## comfortH // different gender, grade

ggplot(resp,aes(x=gender,y=comfortH))+
  geom_boxplot()+xlab("gender")+ylab("comfortH")
ggplot(resp,aes(x=grade,y=comfortH))+
  geom_boxplot()+xlab("grade")+ylab("comfortH")

meansC<-with(resp,tapply(comfortH, list(gender,grade), mean))
meansC<-meansC[c(0:5,7:8)]
sesC<-with(resp,tapply(comfortH, list(gender,grade), sd))
sesC<-sesC[c(0:5,7:8)]
sesC[3]=0
sesC<-sesC/sqrt(n1)

dtaC<-data.frame(means=c(meansC),ses=c(sesC),
                 gender=c("F","M","O","F","M","F","M"),
                 grade=c(rep("fs",3),rep("js",2),rep("ms",2)))

ggplot(dtaC,
       aes(grade,means,fill=gender))+
  geom_bar(stat="identity",position=position_dodge(0.9))+
  geom_errorbar(aes(ymin=means-ses,ymax=means+ses),width=0.2,
                position=position_dodge(0.9))+
  xlab("grade")+
  ylab("comfortH")

summary(aov(comfortH~gender,subset(resp,gender!="O"))) # 0.07
PostHocTest(aov(comfortH~gender,subset(resp,gender!="O")),method="lsd")
omega(aov(comfortH~gender,subset(resp,gender!="O")))
summary(aov(comfortH~grade,resp)) ## 0.04
PostHocTest(aov(comfortH~grade,resp),method="lsd")
omega(aov(comfortH~grade,resp))


## acceptH // different gender, grade

ggplot(resp,aes(x=gender,y=acceptH))+
  geom_boxplot()+xlab("gender")+ylab("acceptH")
ggplot(resp,aes(x=grade,y=acceptH))+
  geom_boxplot()+xlab("grade")+ylab("acceptH")

means2<-with(resp,tapply(acceptH, list(gender,grade), mean))
means2<-means2[c(0:5,7:8)]
ses2<-with(resp,tapply(acceptH, list(gender,grade), sd))
ses2<-ses2[c(0:5,7:8)]
ses2[3]=0
ses2<-ses2/sqrt(n1)

dta2<-data.frame(means=c(means2),ses=c(ses2),
                 gender=c("F","M","O","F","M","F","M"),
                 grade=c(rep("fs",3),rep("js",2),rep("ms",2)))

ggplot(dta2,
       aes(grade,means,fill=gender))+
  geom_bar(stat="identity",position=position_dodge(0.9))+
  geom_errorbar(aes(ymin=means-ses,ymax=means+ses),width=0.2,
                position=position_dodge(0.9))+
  xlab("grade")+
  ylab("acceptH")

summary(aov(acceptH~gender,resp)) # 0.08
summary(aov(acceptH~gender,subset(resp,gender!="O")))
PostHocTest(aov(acceptH~gender,subset(resp,gender!="O")),method="lsd")
omega(aov(acceptH~gender,subset(resp,gender!="O")))
summary(aov(acceptH~grade,resp))


## dorm correlation test

dtaD<-resp[79:156,9:12]
dim(dtaD)
dtaD<-subset(dtaD,peopleD!=0)
dim(dtaD)
cor(dtaD)
corrplot(cor(dtaD), method = "circle")

with(dtaD,cor.test(peopleD,affectionD))
with(dtaD,cor.test(peopleD,comfortD))
with(dtaD,cor.test(peopleD,acceptD))
with(dtaD,cor.test(affectionD,comfortD))
with(dtaD,cor.test(affectionD,acceptD))
with(dtaD,cor.test(comfortD,acceptD)) ## 10e-15

summary(lm(acceptD~comfortD,dtaD)) ### R-squared: 0.6053
summary(lm(acceptD~comfortD+affectionD,dtaD)) # R-squared: 0.6059
summary(lm(acceptD~comfortD*affectionD,dtaD)) # R-squared: 0.6059


#### dorm
respD<-subset(resp,cond!="A")
respD<-subset(respD, affectionD!="NA")

## home diff //cond B & C

m1<-with(respD,tapply(peopleH, cond, mean))
m2<-with(respD,tapply(affectionH, cond, mean))
m3<-with(respD,tapply(comfortH, cond, mean))
m4<-with(respD,tapply(acceptH, cond, mean))
means3<-c(m1,m2,m3,m4)

dta3<-data.frame(means=means3,
                 cond=rep(c("B","C"),4),
                 type=gl(4,2,8,labels=c("peopleH","affectionH","comfortH","acceptH")))

ggplot(dta3,
       aes(type,means,fill=cond))+
  geom_bar(stat="identity",position=position_dodge(0.9))+
  xlab("type")+
  ylab("point")

  ## infer: no significant diff
t.test(subset(respD,cond=="B")$peopleH,subset(respD,cond=="C")$peopleH)
t.test(subset(respD,cond=="B")$affectionH,subset(respD,cond=="C")$affectionH)
t.test(subset(respD,cond=="B")$comfortH,subset(respD,cond=="C")$comfortH)
t.test(subset(respD,cond=="B")$acceptH,subset(respD,cond=="C")$acceptH)

#summary(aov(peopleH~cond,respD)) # 0.09
#summary(aov(affectionH~cond,respD))
#summary(aov(comfortH~cond,respD))
#summary(aov(acceptH~cond,respD))

## dorm diff //cond B & C

m5<-with(respD,tapply(peopleD, cond, mean))
m6<-with(respD,tapply(affectionD, cond, mean))
m7<-with(respD,tapply(comfortD, cond, mean))
m8<-with(respD,tapply(acceptD, cond, mean))
means4<-c(m5,m6,m7,m8)

dta4<-data.frame(means=means4,
                 cond=rep(c("B","C"),4),
                 type=gl(4,2,8,labels=c("peopleD","affectionD","comfortD","acceptD")))

ggplot(dta4,
       aes(type,means,fill=cond))+
  geom_bar(stat="identity",position=position_dodge(0.9))+
  xlab("type")+
  ylab("point")

t.test(subset(respD,cond=="B")$peopleD,subset(respD,cond=="C")$peopleD) ## 10e-5
t.test(subset(respD,cond=="B")$affectionD,subset(respD,cond=="C")$affectionD)
t.test(subset(respD,cond=="B")$comfortD,subset(respD,cond=="C")$comfortD) ## 0.01
t.test(subset(respD,cond=="B")$acceptD,subset(respD,cond=="C")$acceptD) ## 0.006



#summary(aov(peopleD~cond,respD)) ## 10e-6
#summary(aov(affectionD~cond,respD))
#summary(aov(comfortD~cond,respD)) ## 0.03
#summary(aov(acceptD~cond,respD)) ## 0.012

#summary(aov(affectionH~home,respD))
#summary(aov(affectionH~reason,respD))

#summary(aov(affectionD~home,respD))
#summary(aov(affectionD~reason,respD))

#summary(aov(comfortH~home,respD)) ## 0.01
#summary(aov(comfortH~reason,respD))

#summary(aov(comfortD~home,respD)) ## 0.0007
#summary(aov(comfortD~reason,respD))

#summary(aov(acceptH~home,respD)) ## 0.01
#summary(aov(acceptH~reason,respD)) # 0.03

#summary(aov(acceptD~home,respD)) ## 0.0001
#summary(aov(acceptD~reason,respD)) # 0.06


#### place and reason
dta_p_r<-data.frame(amount=c(17,10,54,15,5,52,1,2),
                    place=rep(c("Home","NOT Home"),4),
                    reason=gl(4,2,8,labels=c("affection","environment","focus","others")))

ggplot(dta_p_r,aes(place,amount,fill=reason))+
  geom_bar(stat="identity",position=position_dodge(0.9))+
  xlab("place")+
  ylab("amount(people)")

t.test(subset(resp,home=="Yes")$peopleH,subset(resp,home=="No")$peopleH)
t.test(subset(resp,home=="Yes")$comfortH,subset(resp,home=="No")$comfortH) ## 0.001
t.test(subset(resp,home=="Yes")$affectionH,subset(resp,home=="No")$affectionH) ## 0.02
t.test(subset(resp,home=="Yes")$acceptH,subset(resp,home=="No")$acceptH) ## 0.001

t.test(subset(resp,home=="Yes")$peopleD,subset(resp,home=="No")$peopleD)
t.test(subset(resp,home=="Yes")$comfortD,subset(resp,home=="No")$comfortD) ## 0.0008
t.test(subset(resp,home=="Yes")$affectionD,subset(resp,home=="No")$affectionD)
t.test(subset(resp,home=="Yes")$acceptD,subset(resp,home=="No")$acceptD) ## 0.0003


