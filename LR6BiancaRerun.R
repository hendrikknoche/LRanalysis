options(install.lock = FALSE) 
# lib ---------------------------------------------------------------------
libloc= Sys.getenv("R_LIBS_USER")
.libPaths("C:/Users/kh/kh - Lokal/R/win-library/3.2")

library(ggplot2)
library(Rmisc)
library(ROCR)
library(dplyr)
library(readxl)
Handedness_Responses_Form_responses_1 <- read_excel("~/git/AAU/LRanalysis/data/Handedness (Responses) - Form responses 1.xlsx")
# , 
#                                                     +     col_types = c("date", "numeric", "text", 
#                                                                         +         "text", "text", "text", "text", "text", 
#                                                                         +         "text", "text", "text", "text", "text", 
#                                                                         +         "text", "numeric", "numeric", "numeric", 
#                                                                         +         "numeric", "numeric"))

#Set workspace directory

setwd("~/git/AAU/LRanalysis/data/LR6rerunBianca")
#setwd("/Users/hendrik/Dropbox/BNC/Analysis/LR studies/Target Size Test/Data")
#Load data from workspace directory

data<-data.frame()
touchData<-data.frame()

logID<-0
tLogID<-0

for(fileName in dir()){
  
  if(substr(fileName, 1, 1) == "T") {
    
    td<-read.csv(fileName,header = TRUE, sep = ";")
    td$FileID<-tLogID
    touchData<-rbind(touchData,td)
    tLogID<-tLogID+1
  }
  else {
    d<-read.csv(fileName,header = TRUE, sep = ";")
    d$FileID<-logID
    data<-rbind(data,d)
    logID<-logID+1
  }
}

LR6participants<-distinct(data[,c("UserID","Age","Gender","DominantEye","LongNails","DominantHand")])



MaxX<- 154 # after the rotation this is correct, the 154mm come from the landscape orientation of the device with 0,0 in the upper left corner 
MaxY<- 90  # after the rotation this is correct, the 90mm come from the landscape orientation of the device with 0,0 in the upper left corner 

data$tempTargetX<-data$TargetY
data$tempTargetY<-MaxX-data$TargetX
data$TargetX<-data$tempTargetX
data$TargetY<-data$tempTargetY
data$tempTargetX<-NULL
data$tempTargetY<-NULL

data$tempLiftX<-data$LiftY
data$tempLiftY<-MaxX-data$LiftX
data$LiftX<-data$tempTargetX
data$LiftY<-data$tempLiftY
data$tempLiftX<-NULL
data$tempLiftY<-NULL

touchData$tempTouchX<-data$TouchY
touchData$tempTouchY<-MaxX-touchData$TouchX
touchData$TouchX<-touchData$tempTouchX
touchData$TouchY<-touchData$tempTouchY
touchData$tempTouchX<-NULL
touchData$tempTouchY<-NULL

touchData$tempFirstTouchX<-touchData$FirstTouchY
touchData$tempFirstTouchY<-MaxX-touchData$FirstTouchX
touchData$FirstTouchX<-touchData$tempFirstTouchX
touchData$FirstTouchY<-touchData$tempFirstTouchY
touchData$tempFirstTouchX<-NULL
touchData$tempFirstTouchY<-NULL

data$tempTouchOffsetX<--data$TouchOffsetY
data$tempTouchOffsetY<-data$TouchOffsetX
data$TouchOffsetX<-data$tempTouchOffsetX
data$TouchOffsetY<-data$tempTouchOffsetY
data$tempTouchOffsetX<-NULL
data$tempTouchOffsetY<-NULL

data$tempLiftOffsetX<-data$LiftOffsetY
data$tempLiftOffsetY<-MaxX-data$LiftOffsetX
data$LiftOffsetX<-data$tempLiftOffsetX
data$LiftOffsetY<-data$tempLiftOffsetY
data$tempLiftOffsetX<-NULL
data$tempLiftffsetY<-NULL

touchData$tempTouchDeltaX<--touchData$TouchDeltaY
touchData$tempTouchDeltaY<-touchData$TouchDeltaX
touchData$TouchDeltaX<-touchData$tempTouchDeltaX
touchData$TouchDeltaY<-touchData$tempTouchDeltaY
touchData$tempTouchDeltaX<-NULL
touchData$tempTouchDeltaY<-NULL

data$tempOutset[data$Outset == "N"]<-"E"
data$tempOutset[data$Outset == "S"]<-"W"
data$tempOutset[data$Outset == "E"]<-"S"
data$tempOutset[data$Outset == "W"]<-"N"
data$Outset<-data$tempOutset
data$tempOutset<-NULL

data$tempGoal[data$Goal == "N"]<-"E"
data$tempGoal[data$Goal == "S"]<-"W"
data$tempGoal[data$Goal == "E"]<-"S"
data$tempGoal[data$Goal == "W"]<-"N"
data$Goal<-data$tempGoal
data$tempGoal<-NULL

data$TargetCombination<-paste(data$Outset,data$Goal,sep = "")


data$DominantHandCoded<-ifelse(data$DominantHand == "L",-1,1)
data$OutsetXcoded<-ifelse(data$Outset == "W",-1,ifelse(data$Outset == "E",1,0))
data$OutsetYcoded<-ifelse(data$Outset == "S",-1,ifelse(data$Outset == "N",1,0))
data$GoalXcoded<-ifelse(data$Goal == "W",-1,ifelse(data$Goal == "E",1,0))
data$GoalYcoded<-ifelse(data$Goal == "S",-1,ifelse(data$Goal == "N",1,0))
data$CrossTargetCoded<-ifelse(data$CrossTargets == "True",1,0)
data$fromIpsilateral<-ifelse(data$OutsetXcoded==data$DominantHandCoded,1,0)
data$toIpsilateral<-ifelse(data$GoalXcoded==data$DominantHandCoded,1,0)

data$slideX<-data$LiftOffsetX-data$TouchOffsetX
data$slideY<-data$LiftOffsetY-data$TouchOffsetY


mergedData<-merge(data, touchData, by=c("FileID","UserID","TouchID"))
mergedData<-mergedData[order(c(mergedData$Filedata$Time,mergedData$Time)),]



fit <- lm(TouchOffsetX ~ OutsetXcoded*GoalXcoded*CrossTargetCoded*TargetSize*DominantHandCoded, data=data[which(data$HitType == "Center" & data$MistakeOccured == "No"),])
summary(fit)
step(fit)

ggplot(data=data[which(data$HitType == "Center" & data$MistakeOccured == "No" ),],aes(x = TouchOffsetX, y = TouchOffsetY, color = Outset))+geom_point(alpha=.3)+facet_grid(.~UserID)

fit <- lm(TouchOffsetX ~ OutsetXcoded*fromIpsilateral*DominantHandCoded+DominantHandCoded*GoalXcoded*toIpsilateral+CrossTargetCoded+TargetSize+DominantHandCoded, data=data[which(data$HitType == "Center" & data$MistakeOccured == "No"),])
summary(fit)
step(fit)
summary(step(fit))


fit <- lm(TouchOffsetY ~ OutsetYcoded+GoalYcoded+CrossTargetCoded+TargetSize*DominantHandCoded, data=data[which(data$HitType == "Center" & & data$MistakeOccured == "No"),])
step(fit)

fit <- lm(slideX ~ OutsetXcoded+OutsetYcoded+GoalXcoded+GoalYcoded+CrossTargetCoded+TargetSize+DominantHandCoded+GoalXcoded*DominantHandCoded+OutsetXcoded*DominantHandCoded, data=data[which(data$HitType == "Center" & data$MistakeOccured == "No"),])
summary(fit)

fit <- lm(slideY ~ OutsetXcoded+OutsetYcoded+GoalXcoded+GoalYcoded+CrossTargetCoded+TargetSize+DominantHandCoded, data=data[which(data$HitType == "Center" & data$MistakeOccured == "No"),])
step(fit)


dataSummaryXtouch<-summarySE(data[which(data$HitType == "Center" & data$MistakeOccured == "No"),], c("TouchOffsetX"), c("OutsetXcoded","OutsetYcoded","GoalXcoded","GoalYcoded","DominantHandCoded"), conf.interval = 0.95)
dataSummaryYtouch<-summarySE(data[which(data$HitType == "Center" & data$MistakeOccured == "No"),], c("TouchOffsetY"), c("OutsetXcoded","OutsetYcoded","GoalXcoded","GoalYcoded","DominantHandCoded"), conf.interval = 0.95)

dataSummaryTouch<-merge(dataSummaryXtouch,dataSummaryYtouch, by=c("OutsetXcoded","OutsetYcoded","GoalXcoded","GoalYcoded","DominantHandCoded"))
dataSummaryTouch$orderID<-c(1:nrow(dataSummaryXtouch))
dataSummaryTouch$slideID<-c(1:nrow(dataSummaryXtouch))

dataSummaryXlift<-summarySE(data[which(data$HitType == "Center" & data$MistakeOccured == "No"),], c("LiftOffsetX"), c("OutsetXcoded","OutsetYcoded","GoalXcoded","GoalYcoded","DominantHandCoded"), conf.interval = 0.95)
dataSummaryYlift<-summarySE(data[which(data$HitType == "Center" & data$MistakeOccured == "No"),], c("LiftOffsetX"), c("OutsetXcoded","OutsetYcoded","GoalXcoded","GoalYcoded","DominantHandCoded"), conf.interval = 0.95)

dataSummaryLift<-merge(dataSummaryXlift,dataSummaryYlift, by=c("OutsetXcoded","OutsetYcoded","GoalXcoded","GoalYcoded","DominantHandCoded"))
dataSummaryLift$orderID<-c(1:nrow(dataSummaryXlift))+0.5
dataSummaryLift$slideID<-c(1:nrow(dataSummaryXlift))

names(dataSummaryLift)<-names(dataSummaryTouch)
dataSummary<-rbind(dataSummaryTouch,dataSummaryLift)
dataSummary<-dataSummary[order(dataSummary$orderID),]

dataSummary$OutsetYcoded<-factor(dataSummary$OutsetYcoded, levels = c(1,0,-1))
dataSummary$GoalYcoded<-factor(dataSummary$GoalYcoded, levels = c(1,0,-1))

ggplot(dataSummary, aes(x = TouchOffsetX, y = TouchOffsetY, color = factor(DominantHandCoded), group = factor(slideID)))+
  geom_path(arrow=arrow())+
  #geom_errorbar(aes(ymin=TouchOffsetY-ci.y, ymax=TouchOffsetY+ci.y), width=.5)+
  #geom_errorbarh(aes(xmin=TouchOffsetX-ci.x, xmax=TouchOffsetX+ci.x), height=.5)+
  facet_grid(OutsetYcoded+GoalYcoded~OutsetXcoded+GoalXcoded)+
  coord_fixed()


dataSummaryXslide<-summarySE(data[which(data$HitType == "Center" & data$MistakeOccured == "No"),], c("slideX"), c("OutsetXcoded","OutsetYcoded","GoalXcoded","GoalYcoded","DominantHandCoded"), conf.interval = 0.95)
dataSummaryYslide<-summarySE(data[which(data$HitType == "Center" & data$MistakeOccured == "No"),], c("slideY"), c("OutsetXcoded","OutsetYcoded","GoalXcoded","GoalYcoded","DominantHandCoded"), conf.interval = 0.95)

dataSummarySlide<-merge(dataSummaryXslide,dataSummaryYslide, by=c("OutsetXcoded","OutsetYcoded","GoalXcoded","GoalYcoded","DominantHandCoded"))
dataSummarySlide$orderID<-c(1:nrow(dataSummarySlide))
dataSummarySlide$slideID<-c(1:nrow(dataSummarySlide))

dataSummarySlide$OutsetYcoded<-factor(dataSummarySlide$OutsetYcoded, levels = c(1,0,-1))
dataSummarySlide$GoalYcoded<-factor(dataSummarySlide$GoalYcoded, levels = c(1,0,-1))

ggplot(dataSummarySlide, aes(x = slideX, y = slideY, color = factor(DominantHandCoded), group = factor(slideID)))+
  geom_point()+
  geom_errorbar(aes(ymin=slideY-ci.y, ymax=slideY+ci.y), width=.5)+
  geom_errorbarh(aes(xmin=slideX-ci.x, xmax=slideX+ci.x), height=.5)+
  facet_grid(OutsetYcoded+GoalYcoded~OutsetXcoded+GoalXcoded)+
  coord_fixed(xlim = c(-3,3), ylim = c(-3,3))



dataSummaryXslideOutset<-summarySE(data[which(data$HitType == "Center" & data$MistakeOccured == "No"),], c("slideX"), c("OutsetXcoded","OutsetYcoded","DominantHandCoded"), conf.interval = 0.95)
dataSummaryYslideOutset<-summarySE(data[which(data$HitType == "Center" & data$MistakeOccured == "No"),], c("slideY"), c("OutsetXcoded","OutsetYcoded","DominantHandCoded"), conf.interval = 0.95)

dataSummarySlideOutset<-merge(dataSummaryXslideOutset,dataSummaryYslideOutset, by=c("OutsetXcoded","OutsetYcoded","DominantHandCoded"))
dataSummarySlideOutset$orderID<-c(1:nrow(dataSummarySlideOutset))
dataSummarySlideOutset$slideID<-c(1:nrow(dataSummarySlideOutset))

dataSummarySlideOutset$OutsetYcoded<-factor(dataSummarySlideOutset$OutsetYcoded, levels = c(1,0,-1))

ggplot(dataSummarySlideOutset, aes(x = slideX, y = slideY, color = factor(DominantHandCoded)))+
  geom_point()+
  geom_hline(yintercept = 0)+
  geom_vline(xintercept = 0)+
  geom_errorbar(aes(ymin=slideY-ci.y, ymax=slideY+ci.y), width=.1)+
  geom_errorbarh(aes(xmin=slideX-ci.x, xmax=slideX+ci.x), height=.1)+
  coord_fixed(xlim = c(-.8,.8), ylim = c(-.8,.8))+
  facet_grid(OutsetYcoded~OutsetXcoded)



dataSummaryXslideGoal<-summarySE(data[which(data$HitType == "Center" & data$MistakeOccured == "No"),], c("slideX"), c("GoalXcoded","GoalYcoded","DominantHandCoded"), conf.interval = 0.95)
dataSummaryYslideGoal<-summarySE(data[which(data$HitType == "Center" & data$MistakeOccured == "No"),], c("slideY"), c("GoalXcoded","GoalYcoded","DominantHandCoded"), conf.interval = 0.95)

dataSummarySlideGoal<-merge(dataSummaryXslideGoal,dataSummaryYslideGoal, by=c("GoalXcoded","GoalYcoded","DominantHandCoded"))
dataSummarySlideGoal$orderID<-c(1:nrow(dataSummarySlideOutset))
dataSummarySlideGoal$slideID<-c(1:nrow(dataSummarySlideOutset))

dataSummarySlideGoal$GoalYcoded<-factor(dataSummarySlideOutset$OutsetYcoded, levels = c(1,0,-1))

ggplot(dataSummarySlideGoal, aes(x = slideX, y = slideY, color = factor(DominantHandCoded)))+
  geom_point()+
  geom_hline(yintercept = 0)+
  geom_vline(xintercept = 0)+
  geom_errorbar(aes(ymin=slideY-ci.y, ymax=slideY+ci.y), width=.1)+
  geom_errorbarh(aes(xmin=slideX-ci.x, xmax=slideX+ci.x), height=.1)+
  coord_fixed(xlim = c(-2,2), ylim = c(-2,2))+
  facet_grid(GoalYcoded~GoalXcoded)









dataSummaryXOffsetOutset<-summarySE(data[which(data$HitType == "Center" & data$MistakeOccured == "No"),], c("TouchOffsetX"), c("OutsetXcoded","OutsetYcoded","DominantHandCoded"), conf.interval = 0.95)
dataSummaryYOffsetOutset<-summarySE(data[which(data$HitType == "Center" & data$MistakeOccured == "No"),], c("TouchOffsetY"), c("OutsetXcoded","OutsetYcoded","DominantHandCoded"), conf.interval = 0.95)

dataSummaryOffsetOutset<-merge(dataSummaryXOffsetOutset,dataSummaryYOffsetOutset, by=c("OutsetXcoded","OutsetYcoded","DominantHandCoded"))
dataSummaryOffsetOutset$orderID<-c(1:nrow(dataSummaryOffsetOutset))
dataSummaryOffsetOutset$slideID<-c(1:nrow(dataSummaryOffsetOutset))

dataSummaryOffsetOutset$OutsetYcoded<-factor(dataSummaryOffsetOutset$OutsetYcoded, levels = c(1,0,-1))

ggplot(dataSummaryOffsetOutset, aes(x = TouchOffsetX, y = TouchOffsetY, color = factor(DominantHandCoded)))+
  geom_point()+
  geom_hline(yintercept = 0)+
  geom_vline(xintercept = 0)+
  geom_errorbar(aes(ymin=TouchOffsetY-ci.y, ymax=TouchOffsetY+ci.y), width=.1)+
  geom_errorbarh(aes(xmin=TouchOffsetX-ci.x, xmax=TouchOffsetX+ci.x), height=.1)+
  coord_fixed(xlim = c(-1.5,1.5), ylim = c(-1.5,1.5))+
  facet_grid(OutsetYcoded~OutsetXcoded)



dataSummaryXOffsetGoal<-summarySE(data[which(data$HitType == "Center" & data$MistakeOccured == "No"),], c("TouchOffsetX"), c("GoalXcoded","GoalYcoded","DominantHandCoded"), conf.interval = 0.95)
dataSummaryYOffsetGoal<-summarySE(data[which(data$HitType == "Center" & data$MistakeOccured == "No"),], c("TouchOffsetY"), c("GoalXcoded","GoalYcoded","DominantHandCoded"), conf.interval = 0.95)

dataSummaryOffsetGoal<-merge(dataSummaryXOffsetGoal,dataSummaryYOffsetGoal, by=c("GoalXcoded","GoalYcoded","DominantHandCoded"))
dataSummaryOffsetGoal$orderID<-c(1:nrow(dataSummaryOffsetGoal))
dataSummaryOffsetGoal$slideID<-c(1:nrow(dataSummaryOffsetGoal))

dataSummaryOffsetOutset$OutsetYcoded<-factor(dataSummaryOffsetOutset$OutsetYcoded, levels = c(1,0,-1))

ggplot(dataSummaryOffsetGoal, aes(x = TouchOffsetX, y = TouchOffsetY, color = factor(DominantHandCoded)))+
  geom_point()+
  geom_hline(yintercept = 0)+
  geom_vline(xintercept = 0)+
  geom_errorbar(aes(ymin=TouchOffsetY-ci.y, ymax=TouchOffsetY+ci.y), width=.1)+
  geom_errorbarh(aes(xmin=TouchOffsetX-ci.x, xmax=TouchOffsetX+ci.x), height=.1)+
  coord_fixed(xlim = c(-1.5,1.5), ylim = c(-1.5,1.5))+
  facet_grid(GoalYcoded~GoalXcoded)








dataSubset<-subset(data, HitType == "Center" & MistakeOccured == "No")

fit <- lm(DominantHandCoded ~ TouchOffsetX*slideX*GoalXcoded*OutsetXcoded+TouchOffsetY*slideY*GoalYcoded*OutsetYcoded, dataSubset)
summary(fit)
step(fit)




samples<-500



fit <- lm(DominantHandCoded ~ TouchOffsetX*slideX*TouchOffsetY*slideY, data=dataSubset)
summary(fit)
coeff<-summary(fit)$coefficients[1:5,1]

dataSubset$CurrentTouchPred<-coeff[1]+coeff[2]*dataSubset$TouchOffsetX+coeff[3]*dataSubset$slideX+coeff[4]*dataSubset$TouchOffsetY+coeff[5]*dataSubset$slideY

CurrentTouchROC<-data.frame(seq(from = min(dataSubset$CurrentTouchPred), to = max(dataSubset$CurrentTouchPred), by = (max(dataSubset$CurrentTouchPred)-min(dataSubset$CurrentTouchPred))/(samples-1)))
names(CurrentTouchROC)<-"Thresholds"

CurrentTouchROC$FPR<-0
CurrentTouchROC$TPR<-0

for(i in 1:nrow(CurrentTouchROC)){
  CurrentTouchROC[i,]$FPR<-nrow(subset(dataSubset, CurrentTouchPred > CurrentTouchROC[i,]$Thresholds & DominantHandCoded == 0))/nrow(subset(dataSubset, DominantHandCoded == 0))
  CurrentTouchROC[i,]$TPR<-nrow(subset(dataSubset, CurrentTouchPred > CurrentTouchROC[i,]$Thresholds & DominantHandCoded == 1))/nrow(subset(dataSubset, DominantHandCoded == 1))
}

CurrentTouchROC$Predictor<-"Touch"



fit <- lm(DominantHandCoded ~ TouchOffsetX*slideX*TouchOffsetY*slideY*OutsetXcoded*OutsetYcoded, data=dataSubset)
summary(fit)
coeff<-summary(fit)$coefficients[1:7,1]

dataSubset$TouchAndOutsetPred<-coeff[1]+coeff[2]*dataSubset$TouchOffsetX+coeff[3]*dataSubset$slideX+coeff[4]*dataSubset$TouchOffsetY+coeff[5]*dataSubset$slideY+coeff[6]*dataSubset$OutsetXcoded+coeff[7]*dataSubset$OutsetYcoded

TouchAndOutsetROC<-data.frame(seq(from = min(dataSubset$TouchAndOutsetPred), to = max(dataSubset$TouchAndOutsetPred), by = (max(dataSubset$TouchAndOutsetPred)-min(dataSubset$TouchAndOutsetPred))/(samples-1)))
names(TouchAndOutsetROC)<-"Thresholds"

TouchAndOutsetROC$FPR<-0
TouchAndOutsetROC$TPR<-0

for(i in 1:nrow(TouchAndOutsetROC)){
  TouchAndOutsetROC[i,]$FPR<-nrow(subset(dataSubset, TouchAndOutsetPred > TouchAndOutsetROC[i,]$Thresholds & DominantHandCoded == 0))/nrow(subset(dataSubset, DominantHandCoded == 0))
  TouchAndOutsetROC[i,]$TPR<-nrow(subset(dataSubset, TouchAndOutsetPred > TouchAndOutsetROC[i,]$Thresholds & DominantHandCoded == 1))/nrow(subset(dataSubset, DominantHandCoded == 1))
}

TouchAndOutsetROC$Predictor<-"Touch+Outset"






fit <- lm(DominantHandCoded ~ TouchOffsetX*slideX*TouchOffsetY*slideY*GoalXcoded*GoalYcoded, data=dataSubset)
summary(fit)
coeff<-summary(fit)$coefficients[1:7,1]

dataSubset$TouchAndGoalPred<-coeff[1]+coeff[2]*dataSubset$TouchOffsetX+coeff[3]*dataSubset$slideX+coeff[4]*dataSubset$TouchOffsetY+coeff[5]*dataSubset$slideY+coeff[6]*dataSubset$GoalXcoded+coeff[7]*dataSubset$GoalYcoded

TouchAndGoalROC<-data.frame(seq(from = min(dataSubset$TouchAndGoalPred), to = max(dataSubset$TouchAndGoalPred), by = (max(dataSubset$TouchAndGoalPred)-min(dataSubset$TouchAndGoalPred))/(samples-1)))
names(TouchAndGoalROC)<-"Thresholds"

TouchAndGoalROC$FPR<-0
TouchAndGoalROC$TPR<-0

for(i in 1:nrow(TouchAndGoalROC)){
  TouchAndGoalROC[i,]$FPR<-nrow(subset(dataSubset, TouchAndGoalPred > TouchAndGoalROC[i,]$Thresholds & DominantHandCoded == 0))/nrow(subset(dataSubset, DominantHandCoded == 0))
  TouchAndGoalROC[i,]$TPR<-nrow(subset(dataSubset, TouchAndGoalPred > TouchAndGoalROC[i,]$Thresholds & DominantHandCoded == 1))/nrow(subset(dataSubset, DominantHandCoded == 1))
}

TouchAndGoalROC$Predictor<-"Touch+Goal"






fit <- lm(DominantHandCoded ~ TouchOffsetX*slideX*TouchOffsetY*slideY*OutsetXcoded*OutsetYcoded*GoalXcoded*GoalYcoded, data=dataSubset)
summary(fit)
coeff<-summary(fit)$coefficients[1:9,1]

dataSubset$TouchOutsetGoalPred<-coeff[1]+coeff[2]*dataSubset$TouchOffsetX+coeff[3]*dataSubset$slideX+coeff[4]*dataSubset$TouchOffsetY+coeff[5]*dataSubset$slideY+coeff[6]*dataSubset$OutsetXcoded+coeff[7]*dataSubset$OutsetYcoded+coeff[8]*dataSubset$GoalXcoded+coeff[9]*dataSubset$GoalYcoded

TouchOutsetGoalROC<-data.frame(seq(from = min(dataSubset$TouchOutsetGoalPred), to = max(dataSubset$TouchOutsetGoalPred), by = (max(dataSubset$TouchOutsetGoalPred)-min(dataSubset$TouchOutsetGoalPred))/(samples-1)))
names(TouchOutsetGoalROC)<-"Thresholds"

TouchOutsetGoalROC$FPR<-0
TouchOutsetGoalROC$TPR<-0

for(i in 1:nrow(TouchOutsetGoalROC)){
  TouchOutsetGoalROC[i,]$FPR<-nrow(subset(dataSubset, TouchOutsetGoalPred > TouchOutsetGoalROC[i,]$Thresholds & DominantHandCoded == 0))/nrow(subset(dataSubset, DominantHandCoded == 0))
  TouchOutsetGoalROC[i,]$TPR<-nrow(subset(dataSubset, TouchOutsetGoalPred > TouchOutsetGoalROC[i,]$Thresholds & DominantHandCoded == 1))/nrow(subset(dataSubset, DominantHandCoded == 1))
}

TouchOutsetGoalROC$Predictor<-"Touch+Outset+Goal"




AllROC<-rbind(CurrentTouchROC,TouchAndOutsetROC,TouchAndGoalROC,TouchOutsetGoalROC)
AllROC$DataAggregation<-"Raw"

ggplot(AllROC, aes(x = FPR, y = TPR, color = Predictor))+
  geom_line()+
  geom_abline(intercept = 0)+
  coord_fixed()












SummaryXtouch<-summarySE(dataSubset, c("TouchOffsetX"), c("UserID","OutsetXcoded","OutsetYcoded","GoalXcoded","GoalYcoded","DominantHandCoded"), conf.interval = 0.95)
SummaryYtouch<-summarySE(dataSubset, c("TouchOffsetY"), c("UserID","OutsetXcoded","OutsetYcoded","GoalXcoded","GoalYcoded","DominantHandCoded"), conf.interval = 0.95)

SummaryXslide<-summarySE(dataSubset, c("slideX"), c("UserID","OutsetXcoded","OutsetYcoded","GoalXcoded","GoalYcoded","DominantHandCoded"), conf.interval = 0.95)
SummaryYslide<-summarySE(dataSubset, c("slideY"), c("UserID","OutsetXcoded","OutsetYcoded","GoalXcoded","GoalYcoded","DominantHandCoded"), conf.interval = 0.95)

MeanSummary<-merge(SummaryXtouch[c(1:6,8)],SummaryYtouch[c(1:6,8)], by=c("UserID","OutsetXcoded","OutsetYcoded","GoalXcoded","GoalYcoded","DominantHandCoded"))
MeanSummary<-merge(MeanSummary,SummaryXslide[c(1:6,8)], by=c("UserID","OutsetXcoded","OutsetYcoded","GoalXcoded","GoalYcoded","DominantHandCoded"))
MeanSummary<-merge(MeanSummary,SummaryYslide[c(1:6,8)], by=c("UserID","OutsetXcoded","OutsetYcoded","GoalXcoded","GoalYcoded","DominantHandCoded"))







fit <- lm(DominantHandCoded ~ TouchOffsetX*slideX*TouchOffsetY*slideY, data=MeanSummary)
coeff<-summary(fit)$coefficients[1:5,1]

MeanSummary$CurrentTouchPred<-coeff[1]+coeff[2]*MeanSummary$TouchOffsetX+coeff[3]*MeanSummary$slideX+coeff[4]*MeanSummary$TouchOffsetY+coeff[5]*MeanSummary$slideY

MeanCurrentTouchROC<-data.frame(MeanSummary$CurrentTouchPred)
names(MeanCurrentTouchROC)<-"Thresholds"

MeanCurrentTouchROC$FPR<-0
MeanCurrentTouchROC$TPR<-0

for(i in 1:nrow(MeanCurrentTouchROC)){
  MeanCurrentTouchROC[i,]$FPR<-nrow(subset(MeanSummary, CurrentTouchPred >= MeanCurrentTouchROC[i,]$Thresholds & DominantHandCoded == 0))/nrow(subset(MeanSummary, DominantHandCoded == 0))
  MeanCurrentTouchROC[i,]$TPR<-nrow(subset(MeanSummary, CurrentTouchPred >= MeanCurrentTouchROC[i,]$Thresholds & DominantHandCoded == 1))/nrow(subset(MeanSummary, DominantHandCoded == 1))
}

MeanCurrentTouchROC$Predictor<-"Touch"


fit <- lm(TouchOffsetX ~ DominantHandCoded*slideX*TouchOffsetY*slideY*OutsetXcoded*OutsetYcoded, data=MeanSummary)
coeff<-summary(fit)$coefficients[1:7,1]




fit <- lm(DominantHandCoded ~ TouchOffsetX*slideX*TouchOffsetY*slideY*OutsetXcoded*OutsetYcoded, data=MeanSummary)
coeff<-summary(fit)$coefficients[1:7,1]

MeanSummary$TouchAndOutsetPred<-coeff[1]+coeff[2]*MeanSummary$TouchOffsetX+coeff[3]*MeanSummary$slideX+coeff[4]*MeanSummary$TouchOffsetY+coeff[5]*MeanSummary$slideY+coeff[6]*MeanSummary$OutsetXcoded+coeff[7]*MeanSummary$OutsetYcoded

MeanTouchAndOutsetROC<-data.frame(MeanSummary$TouchAndOutsetPred)
names(MeanTouchAndOutsetROC)<-"Thresholds"

MeanTouchAndOutsetROC$FPR<-0
MeanTouchAndOutsetROC$TPR<-0

for(i in 1:nrow(MeanTouchAndOutsetROC)){
  MeanTouchAndOutsetROC[i,]$FPR<-nrow(subset(MeanSummary, TouchAndOutsetPred >= MeanTouchAndOutsetROC[i,]$Thresholds & DominantHandCoded == 0))/nrow(subset(MeanSummary, DominantHandCoded == 0))
  MeanTouchAndOutsetROC[i,]$TPR<-nrow(subset(MeanSummary, TouchAndOutsetPred >= MeanTouchAndOutsetROC[i,]$Thresholds & DominantHandCoded == 1))/nrow(subset(MeanSummary, DominantHandCoded == 1))
}

MeanTouchAndOutsetROC$Predictor<-"Touch+Outset"






fit <- lm(DominantHandCoded ~ TouchOffsetX*slideX*TouchOffsetY*slideY*GoalXcoded*GoalYcoded, data=MeanSummary)
coeff<-summary(fit)$coefficients[1:7,1]

MeanSummary$TouchAndGoalPred<-coeff[1]+coeff[2]*MeanSummary$TouchOffsetX+coeff[3]*MeanSummary$slideX+coeff[4]*MeanSummary$TouchOffsetY+coeff[5]*MeanSummary$slideY+coeff[6]*MeanSummary$GoalXcoded+coeff[7]*MeanSummary$GoalYcoded

MeanTouchAndGoalROC<-data.frame(MeanSummary$TouchAndGoalPred)
names(MeanTouchAndGoalROC)<-"Thresholds"

MeanTouchAndGoalROC$FPR<-0
MeanTouchAndGoalROC$TPR<-0

for(i in 1:nrow(MeanTouchAndGoalROC)){
  MeanTouchAndGoalROC[i,]$FPR<-nrow(subset(MeanSummary, TouchAndGoalPred >= MeanTouchAndGoalROC[i,]$Thresholds & DominantHandCoded == 0))/nrow(subset(MeanSummary, DominantHandCoded == 0))
  MeanTouchAndGoalROC[i,]$TPR<-nrow(subset(MeanSummary, TouchAndGoalPred >= MeanTouchAndGoalROC[i,]$Thresholds & DominantHandCoded == 1))/nrow(subset(MeanSummary, DominantHandCoded == 1))
}

MeanTouchAndGoalROC$Predictor<-"Touch+Goal"






fit <- lm(DominantHandCoded ~ TouchOffsetX*slideX*TouchOffsetY*slideY*OutsetXcoded*OutsetYcoded*GoalXcoded*GoalYcoded, data=MeanSummary)
coeff<-summary(fit)$coefficients[1:9,1]

MeanSummary$TouchOutsetGoalPred<-coeff[1]+coeff[2]*MeanSummary$TouchOffsetX+coeff[3]*MeanSummary$slideX+coeff[4]*MeanSummary$TouchOffsetY+coeff[5]*MeanSummary$slideY+coeff[6]*MeanSummary$OutsetXcoded+coeff[7]*MeanSummary$OutsetYcoded+coeff[8]*MeanSummary$GoalXcoded+coeff[9]*MeanSummary$GoalYcoded

MeanTouchOutsetGoalROC<-data.frame(MeanSummary$TouchOutsetGoalPred)
names(MeanTouchOutsetGoalROC)<-"Thresholds"

MeanTouchOutsetGoalROC$FPR<-0
MeanTouchOutsetGoalROC$TPR<-0

for(i in 1:nrow(MeanTouchOutsetGoalROC)){
  MeanTouchOutsetGoalROC[i,]$FPR<-nrow(subset(MeanSummary, TouchOutsetGoalPred >= MeanTouchOutsetGoalROC[i,]$Thresholds & DominantHandCoded == 0))/nrow(subset(MeanSummary, DominantHandCoded == 0))
  MeanTouchOutsetGoalROC[i,]$TPR<-nrow(subset(MeanSummary, TouchOutsetGoalPred >= MeanTouchOutsetGoalROC[i,]$Thresholds & DominantHandCoded == 1))/nrow(subset(MeanSummary, DominantHandCoded == 1))
}

MeanTouchOutsetGoalROC$Predictor<-"Touch+Outset+Goal"




AllMeanROC<-rbind(MeanCurrentTouchROC,MeanTouchAndOutsetROC,MeanTouchAndGoalROC,MeanTouchOutsetGoalROC)
AllMeanROC$DataAggregation<-"Mean"

ggplot(AllMeanROC, aes(x = FPR, y = TPR, color = Predictor))+
  geom_line()+
  geom_abline(intercept = 0)+
  coord_fixed()


summary(data)
sqldf('select outset, goal, dominanthand, targetsize, crosstargets, count(userid) from dataSubset group by outset, goal, dominanthand, targetsize, crosstargets')


attach(dataSubset)
MedianXtouch<-aggregate(TouchOffsetX, by = list(UserID,OutsetXcoded,OutsetYcoded,GoalXcoded,GoalYcoded,DominantHandCoded), FUN = median)
MedianYtouch<-aggregate(TouchOffsetY, by = list(UserID,OutsetXcoded,OutsetYcoded,GoalXcoded,GoalYcoded,DominantHandCoded), FUN = median)
MedianXSlide<-aggregate(slideX, by = list(UserID,OutsetXcoded,OutsetYcoded,GoalXcoded,GoalYcoded,DominantHandCoded), FUN = median)
MedianYSlide<-aggregate(slideY, by = list(UserID,OutsetXcoded,OutsetYcoded,GoalXcoded,GoalYcoded,DominantHandCoded), FUN = median)
detach(dataSubset)

names(MedianXtouch)<-c("UserID","OutsetXcoded","OutsetYcoded","GoalXcoded","GoalYcoded","DominantHandCoded","TouchOffsetX")
names(MedianYtouch)<-c("UserID","OutsetXcoded","OutsetYcoded","GoalXcoded","GoalYcoded","DominantHandCoded","TouchOffsetY")
names(MedianXSlide)<-c("UserID","OutsetXcoded","OutsetYcoded","GoalXcoded","GoalYcoded","DominantHandCoded","slideX")
names(MedianYSlide)<-c("UserID","OutsetXcoded","OutsetYcoded","GoalXcoded","GoalYcoded","DominantHandCoded","slideY")

MedianSummary<-merge(MedianXtouch,MedianYtouch, by=c("UserID","OutsetXcoded","OutsetYcoded","GoalXcoded","GoalYcoded","DominantHandCoded"))
MedianSummary<-merge(MedianSummary,MedianXSlide, by=c("UserID","OutsetXcoded","OutsetYcoded","GoalXcoded","GoalYcoded","DominantHandCoded"))
MedianSummary<-merge(MedianSummary,MedianYSlide, by=c("UserID","OutsetXcoded","OutsetYcoded","GoalXcoded","GoalYcoded","DominantHandCoded"))



fit <- lm(DominantHandCoded ~ TouchOffsetX*slideX*TouchOffsetY*slideY, data=MedianSummary)
coeff<-summary(fit)$coefficients[1:5,1]

MedianSummary$CurrentTouchPred<-coeff[1]+coeff[2]*MedianSummary$TouchOffsetX+coeff[3]*MedianSummary$slideX+coeff[4]*MedianSummary$TouchOffsetY+coeff[5]*MedianSummary$slideY

MedianCurrentTouchROC<-data.frame(MedianSummary$CurrentTouchPred)
names(MedianCurrentTouchROC)<-"Thresholds"

MedianCurrentTouchROC$FPR<-0
MedianCurrentTouchROC$TPR<-0

for(i in 1:nrow(MedianCurrentTouchROC)){
  MedianCurrentTouchROC[i,]$FPR<-nrow(subset(MedianSummary, CurrentTouchPred >= MedianCurrentTouchROC[i,]$Thresholds & DominantHandCoded == 0))/nrow(subset(MedianSummary, DominantHandCoded == 0))
  MedianCurrentTouchROC[i,]$TPR<-nrow(subset(MedianSummary, CurrentTouchPred >= MedianCurrentTouchROC[i,]$Thresholds & DominantHandCoded == 1))/nrow(subset(MedianSummary, DominantHandCoded == 1))
}

MedianCurrentTouchROC$Predictor<-"Touch"






fit <- lm(DominantHandCoded ~ TouchOffsetX*slideX*TouchOffsetY*slideY*OutsetXcoded*OutsetYcoded, data=MedianSummary)
coeff<-summary(fit)$coefficients[1:7,1]

MedianSummary$TouchAndOutsetPred<-coeff[1]+coeff[2]*MedianSummary$TouchOffsetX+coeff[3]*MedianSummary$slideX+coeff[4]*MedianSummary$TouchOffsetY+coeff[5]*MedianSummary$slideY+coeff[6]*MedianSummary$OutsetXcoded+coeff[7]*MedianSummary$OutsetYcoded

MedianTouchAndOutsetROC<-data.frame(MedianSummary$TouchAndOutsetPred)
names(MedianTouchAndOutsetROC)<-"Thresholds"

MedianTouchAndOutsetROC$FPR<-0
MedianTouchAndOutsetROC$TPR<-0

for(i in 1:nrow(MedianTouchAndOutsetROC)){
  MedianTouchAndOutsetROC[i,]$FPR<-nrow(subset(MedianSummary, TouchAndOutsetPred >= MedianTouchAndOutsetROC[i,]$Thresholds & DominantHandCoded == 0))/nrow(subset(MedianSummary, DominantHandCoded == 0))
  MedianTouchAndOutsetROC[i,]$TPR<-nrow(subset(MedianSummary, TouchAndOutsetPred >= MedianTouchAndOutsetROC[i,]$Thresholds & DominantHandCoded == 1))/nrow(subset(MedianSummary, DominantHandCoded == 1))
}

MedianTouchAndOutsetROC$Predictor<-"Touch+Outset"






fit <- lm(DominantHandCoded ~ TouchOffsetX*slideX*TouchOffsetY*slideY*GoalXcoded*GoalYcoded, data=MedianSummary)
coeff<-summary(fit)$coefficients[1:7,1]

MedianSummary$TouchAndGoalPred<-coeff[1]+coeff[2]*MedianSummary$TouchOffsetX+coeff[3]*MedianSummary$slideX+coeff[4]*MedianSummary$TouchOffsetY+coeff[5]*MedianSummary$slideY+coeff[6]*MedianSummary$GoalXcoded+coeff[7]*MedianSummary$GoalYcoded

MedianTouchAndGoalROC<-data.frame(MedianSummary$TouchAndGoalPred)
names(MedianTouchAndGoalROC)<-"Thresholds"

MedianTouchAndGoalROC$FPR<-0
MedianTouchAndGoalROC$TPR<-0

for(i in 1:nrow(MedianTouchAndGoalROC)){
  MedianTouchAndGoalROC[i,]$FPR<-nrow(subset(MedianSummary, TouchAndGoalPred >= MedianTouchAndGoalROC[i,]$Thresholds & DominantHandCoded == 0))/nrow(subset(MedianSummary, DominantHandCoded == 0))
  MedianTouchAndGoalROC[i,]$TPR<-nrow(subset(MedianSummary, TouchAndGoalPred >= MedianTouchAndGoalROC[i,]$Thresholds & DominantHandCoded == 1))/nrow(subset(MedianSummary, DominantHandCoded == 1))
}

MedianTouchAndGoalROC$Predictor<-"Touch+Goal"






fit <- lm(DominantHandCoded ~ TouchOffsetX*slideX*TouchOffsetY*slideY*OutsetXcoded*OutsetYcoded*GoalXcoded*GoalYcoded, data=MedianSummary)
coeff<-summary(fit)$coefficients[1:9,1]

MedianSummary$TouchOutsetGoalPred<-coeff[1]+coeff[2]*MedianSummary$TouchOffsetX+coeff[3]*MedianSummary$slideX+coeff[4]*MedianSummary$TouchOffsetY+coeff[5]*MedianSummary$slideY+coeff[6]*MedianSummary$OutsetXcoded+coeff[7]*MedianSummary$OutsetYcoded+coeff[8]*MedianSummary$GoalXcoded+coeff[9]*MedianSummary$GoalYcoded

MedianTouchOutsetGoalROC<-data.frame(MedianSummary$TouchOutsetGoalPred)
names(MedianTouchOutsetGoalROC)<-"Thresholds"

MedianTouchOutsetGoalROC$FPR<-0
MedianTouchOutsetGoalROC$TPR<-0

for(i in 1:nrow(MedianTouchOutsetGoalROC)){
  MedianTouchOutsetGoalROC[i,]$FPR<-nrow(subset(MedianSummary, TouchOutsetGoalPred >= MedianTouchOutsetGoalROC[i,]$Thresholds & DominantHandCoded == 0))/nrow(subset(MedianSummary, DominantHandCoded == 0))
  MedianTouchOutsetGoalROC[i,]$TPR<-nrow(subset(MedianSummary, TouchOutsetGoalPred >= MedianTouchOutsetGoalROC[i,]$Thresholds & DominantHandCoded == 1))/nrow(subset(MedianSummary, DominantHandCoded == 1))
}

MedianTouchOutsetGoalROC$Predictor<-"Touch+Outset+Goal"




AllMedianROC<-rbind(MedianCurrentTouchROC,MedianTouchAndOutsetROC,MedianTouchAndGoalROC,MedianTouchOutsetGoalROC)
AllMedianROC$DataAggregation<-"Median"

ggplot(AllMedianROC, aes(x = FPR, y = TPR, color = Predictor))+
  geom_line()+
  geom_abline(intercept = 0)+
  coord_fixed()



ROC<-rbind(AllROC,AllMeanROC,AllMedianROC)
ROC<-ROC[order(ROC$DataAggregation,ROC$Predictor,ROC$FPR,ROC$TPR),]




labelInterval<-5

ROC$labels<-rep(c(1,rep(0,times=labelInterval-1)),times = ceiling(nrow(ROC)/labelInterval))[1:nrow(ROC)]

ggplot(ROC, aes(x = FPR, y = TPR, color = Predictor))+
  geom_path()+
  geom_abline(intercept = 0)+
  geom_text(data = ROC[which(ROC$labels == 1),],aes(label = round(Thresholds,2)), hjust = -0.15, size = 3)+
  geom_point(data = ROC[which(ROC$labels == 1),])+
  coord_fixed()+
  facet_grid(.~DataAggregation)+
  theme(legend.position="bottom")








SummaryXtouchOnePerson<-summarySE(dataSubset, c("TouchOffsetX"), c("UserID","DominantHandCoded"), conf.interval = 0.95)
SummaryYtouchOnePerson<-summarySE(dataSubset, c("TouchOffsetY"), c("UserID","DominantHandCoded"), conf.interval = 0.95)

SummaryXslideOnePerson<-summarySE(dataSubset, c("slideX"), c("UserID","DominantHandCoded"), conf.interval = 0.95)
SummaryYslideOnePerson<-summarySE(dataSubset, c("slideY"), c("UserID","DominantHandCoded"), conf.interval = 0.95)

MeanSummaryOnePerson<-merge(SummaryXtouchOnePerson[c(1:2,4)],SummaryYtouchOnePerson[c(1:2,4)], by=c("UserID","DominantHandCoded"))
MeanSummaryOnePerson<-merge(MeanSummaryOnePerson,SummaryXslideOnePerson[c(1:2,4)], by=c("UserID","DominantHandCoded"))
MeanSummaryOnePerson<-merge(MeanSummaryOnePerson,SummaryYslideOnePerson[c(1:2,4)], by=c("UserID","DominantHandCoded"))

fit <- lm(DominantHandCoded ~ TouchOffsetX*slideX*TouchOffsetY*slideY, data=MeanSummaryOnePerson)
coeff<-summary(fit)$coefficients[1:5,1]

MeanSummaryOnePerson$CurrentTouchPred<-coeff[1]+coeff[2]*MeanSummaryOnePerson$TouchOffsetX+coeff[3]*MeanSummaryOnePerson$slideX+coeff[4]*MeanSummaryOnePerson$TouchOffsetY+coeff[5]*MeanSummaryOnePerson$slideY

MeanCurrentTouchOnePersonROC<-data.frame(MeanSummaryOnePerson$CurrentTouchPred)
names(MeanCurrentTouchOnePersonROC)<-"Thresholds"

MeanCurrentTouchOnePersonROC$FPR<-0
MeanCurrentTouchOnePersonROC$TPR<-0

for(i in 1:nrow(MeanCurrentTouchOnePersonROC)){
  MeanCurrentTouchOnePersonROC[i,]$FPR<-nrow(subset(MeanSummaryOnePerson, CurrentTouchPred >= MeanCurrentTouchOnePersonROC[i,]$Thresholds & DominantHandCoded == 0))/nrow(subset(MeanSummaryOnePerson, DominantHandCoded == 0))
  MeanCurrentTouchOnePersonROC[i,]$TPR<-nrow(subset(MeanSummaryOnePerson, CurrentTouchPred >= MeanCurrentTouchOnePersonROC[i,]$Thresholds & DominantHandCoded == 1))/nrow(subset(MeanSummaryOnePerson, DominantHandCoded == 1))
}

MeanCurrentTouchOnePersonROC$Predictor<-"Touch"

MeanCurrentTouchOnePersonROC<-MeanCurrentTouchOnePersonROC[order(MeanCurrentTouchOnePersonROC$Predictor,MeanCurrentTouchOnePersonROC$FPR,MeanCurrentTouchOnePersonROC$TPR),]

ggplot(MeanCurrentTouchOnePersonROC, aes(x = FPR, y = TPR, color = Predictor))+
  geom_line()+
  geom_abline(intercept = 0)+
  geom_text(aes(label = round(Thresholds,2)), hjust = -0.15, size = 3)+
  coord_fixed(xlim = c(0,1),ylim = c(0,1))








attach(dataSubset)
MedianXtouchOnePerson<-aggregate(TouchOffsetX, by = list(UserID,DominantHandCoded), FUN = median)
MedianYtouchOnePerson<-aggregate(TouchOffsetY, by = list(UserID,DominantHandCoded), FUN = median)
MedianXSlideOnePerson<-aggregate(slideX, by = list(UserID,DominantHandCoded), FUN = median)
MedianYSlideOnePerson<-aggregate(slideY, by = list(UserID,DominantHandCoded), FUN = median)
detach(dataSubset)

names(MedianXtouchOnePerson)<-c("UserID","DominantHandCoded","TouchOffsetX")
names(MedianYtouchOnePerson)<-c("UserID","DominantHandCoded","TouchOffsetY")
names(MedianXSlideOnePerson)<-c("UserID","DominantHandCoded","slideX")
names(MedianYSlideOnePerson)<-c("UserID","DominantHandCoded","slideY")

MedianSummaryOnePerson<-merge(MedianXtouchOnePerson,MedianYtouchOnePerson, by=c("UserID","DominantHandCoded"))
MedianSummaryOnePerson<-merge(MedianSummaryOnePerson,MedianXSlideOnePerson, by=c("UserID","DominantHandCoded"))
MedianSummaryOnePerson<-merge(MedianSummaryOnePerson,MedianYSlideOnePerson, by=c("UserID","DominantHandCoded"))

fit <- lm(DominantHandCoded ~ TouchOffsetX*slideX*TouchOffsetY*slideY, data=MedianSummaryOnePerson)
coeff<-summary(fit)$coefficients[1:5,1]

MedianSummaryOnePerson$CurrentTouchPred<-coeff[1]+coeff[2]*MedianSummaryOnePerson$TouchOffsetX+coeff[3]*MedianSummaryOnePerson$slideX+coeff[4]*MedianSummaryOnePerson$TouchOffsetY+coeff[5]*MedianSummaryOnePerson$slideY

MedianCurrentTouchOnePersonROC<-data.frame(MedianSummaryOnePerson$CurrentTouchPred)
names(MedianCurrentTouchOnePersonROC)<-"Thresholds"

MedianCurrentTouchOnePersonROC$FPR<-0
MedianCurrentTouchOnePersonROC$TPR<-0

for(i in 1:nrow(MedianCurrentTouchOnePersonROC)){
  MedianCurrentTouchOnePersonROC[i,]$FPR<-nrow(subset(MedianSummaryOnePerson, CurrentTouchPred >= MedianCurrentTouchOnePersonROC[i,]$Thresholds & DominantHandCoded == 0))/nrow(subset(MedianSummaryOnePerson, DominantHandCoded == 0))
  MedianCurrentTouchOnePersonROC[i,]$TPR<-nrow(subset(MedianSummaryOnePerson, CurrentTouchPred >= MedianCurrentTouchOnePersonROC[i,]$Thresholds & DominantHandCoded == 1))/nrow(subset(MedianSummaryOnePerson, DominantHandCoded == 1))
}

MedianCurrentTouchOnePersonROC$Predictor<-"Touch"

MedianCurrentTouchOnePersonROC<-MedianCurrentTouchOnePersonROC[order(MedianCurrentTouchOnePersonROC$Predictor,MedianCurrentTouchOnePersonROC$FPR,MedianCurrentTouchOnePersonROC$TPR),]

ggplot(MedianCurrentTouchOnePersonROC, aes(x = FPR, y = TPR, color = Predictor))+
  geom_line()+
  geom_abline(intercept = 0)+
  geom_text(aes(label = round(Thresholds,2)), hjust = -0.15, size = 3)+
  coord_fixed(xlim = c(0,1),ylim = c(0,1))

