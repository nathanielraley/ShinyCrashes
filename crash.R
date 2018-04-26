##Crash data for SSI

library(jsonlite)
library(dplyr)
library(ggplot2)

allegheny<-read.csv("~/Downloads/all-crashes2004-2016.csv",stringsAsFactors = F)
head(allegheny)

all1<-allegheny%>%mutate(FATAL=ifelse(FATAL_COUNT>0,1,0),YOUNG=ifelse(DRIVER_16YR==1|DRIVER_17YR==1|DRIVER_18YR==1|DRIVER_19YR==1|DRIVER_20YR==1,1,0),MIDDLE=ifelse(YOUNG!=1&DRIVER_50_64YR!=1&DRIVER_COUNT_65_74YR!=1&DRIVER_COUNT_75PLUS!=1,1,0),
                         SPEEDING_RELATED=ifelse(SPEEDING==1|SPEEDING_RELATED==1,1,0))


#rear end, head on, angle
all1<-all1%>%filter(!is.na(SPEED_LIMIT),SPEED_LIMIT>20,SPEED_LIMIT<70,TOTAL_UNITS>1,COLLISION_TYPE%in%c(1,2,4))
all1$COLLISION_TYPE<-factor(all1$COLLISION_TYPE,levels=c('1','2','4'),labels=c("Rear","Front","Side"))

all1<-mutate(CRASH_YEAR=as.factor(CRASH_YEAR))

fit1<-glm(data=all1,FATAL_OR_MAJ_INJ~SPEED_LIMIT+SPEEDING_RELATED+COLLISION_TYPE+DRINKING_DRIVER+UNBELTED+
            CRASH_YEAR+YOUNG+MIDDLE+DRIVER_50_64YR+DRIVER_65_74YR+DRIVER_75PLUS,family="binomial")
summary(fit1)

setwd("~/Desktop/CarCrash/CrashFatalities/")
save(fit1,file="fit1")


newdat<-data.frame(SPEED_LIMIT=seq(25,65,length.out = 1000),
                   #SPEEDING_RELATED=rep(1,1000),
                   COLLISION_TYPE=rep(c('Rear','Front','Side'),1000),
                   DRINKING_DRIVER=rep(0,1000),
                   UNBELTED=rep(1,1000))
yhat<-predict(fit1,newdata = newdat,type = "link",se=T)
newdat<-data.frame(newdat,yhat)

std <- qnorm(0.95 / 2 + 0.5)
newdat$ymin <- fit1$family$linkinv(newdat$fit - std * newdat$se)
newdat$ymax <- fit1$family$linkinv(newdat$fit + std * newdat$se)
newdat$fit <- fit1$family$linkinv(newdat$fit)

p <- ggplot(all1[sample(1:85483,5000),], aes(x=SPEED_LIMIT, FATAL_OR_MAJ_INJ)) 
p + geom_jitter(height=0,size=.1) + 
  geom_ribbon(data=newdat, aes(y=fit, ymin=ymin, ymax=ymax, 
                                 fill=as.factor(COLLISION_TYPE)), alpha=0.5) + 
  geom_line(data=newdat, aes(y=fit, colour=as.factor(COLLISION_TYPE))) + 
  labs(x="Speed Limit", y="Fatality") 


yhat<-unique(yhat)
all1%>%ggplot(aes(x=SPEED_LIMIT, y=FATAL)) + geom_point() + 
  stat_smooth(method="glm", method.args=list(family="binomial"), se=FALSE)

with(allegheny,table(COLLISION_TYPE,AUTOMOBILE_COUNT))




## CELL PHONE BAN


## UBER/LYFT BAN


