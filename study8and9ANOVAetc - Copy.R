library(xlsx)
library(stringi)
library(sqldf)
library(cowplot)
library(gridExtra)
library(ggplot2)
library(grid)

g_legend <- function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}

isMADoutlier <- function(x) {
  abs(x - median(x,na.rm=TRUE)) > 4*mad(x,na.rm=TRUE)}



makeScatterWithHorVerDens<-function(df,xin,yin,colorVarin,groupVarin,xlimFrom,xlimTo,ylimFrom,ylimTo){ 
  arguments <- as.list(match.call())
  df$xi<-eval(arguments$xin,df)
  df$yi<-eval(arguments$yin,df)
  df$colorVari<-eval(arguments$colorVarin,df)
  df$groupVari<-eval(arguments$groupVarin,df)
  c1<-ggplot(df,aes(xi,yi,colour=colorVari,alpha=0.05))+theme_bw()+geom_point(shape=21) + geom_jitter()+ xlim(xlimFrom,xlimTo) + ylim(ylimFrom,ylimTo)+ coord_fixed(ratio = 1)#+ geom_density2d()
  c2<-c1+theme(legend.position = "none")
 
  c2p1 <- ggplot(df, aes(x=xi, group=groupVari, colour=colorVari)) +theme_bw() + geom_line(stat="density") +xlim(xlimFrom,xlimTo)+ theme(legend.position = "none", axis.title.x=element_blank(), axis.text.x=element_blank()) 
  c2p2 <- ggplot(df, aes(x=yi, group=groupVari, colour=colorVari)) +theme_bw() + geom_line(stat="density") +xlim(ylimFrom,ylimTo) +coord_flip() + theme(legend.position = "none", axis.title.y=element_blank(), axis.text.y=element_blank())

  
  legend <- g_legend(c1)
  FinalPlot<-grid.arrange(arrangeGrob(c2p1, legend, c2, c2p2, widths = unit.c(unit(0.8*(xlimTo-xlimFrom)/(ylimTo-ylimFrom), "npc"), unit(0.2*(xlimTo-xlimFrom)/(ylimTo-ylimFrom), "npc")), heights=unit.c(unit(0.2*(ylimTo-ylimFrom)/(xlimTo-xlimFrom), "npc"), unit(0.8*(ylimTo-ylimFrom)/(xlimTo-xlimFrom), "npc")), nrow=2))
  
  return(FinalPlot)}

#testF<-function(df,colN){ 
#    df$
#    max(df$colN) return(res)}

# mydata <- read.xlsx("~/SVN/BNC/Data/LeftRight Data studies/AllLRData.xlsx", 1,sheetName = "Sheet1")

df <- read.table("~/SVN/BNC/Data/LeftRight Data studies/AllLRData.csv", header=TRUE, sep=",")
df$isDomHand <-ifelse(stri_length(df$PlayingHand)>1,0,1)
df$PID<-df$UserID
# length(df$PlayingHand)
df$inputHand <- ifelse(stri_length(df$PlayingHand)<2,substr(df$PlayingHand,1,1),ifelse(substr(df$PlayingHand,1,1)=='L','R','L'))
parti<-sqldf("select distinct studyNumber, PID,inputHand as Handed, age, gender,  DominantEye from df where isDomHand=1 ")
df$DominantEyenew<-ifelse(stri_length(df$DominantEye)==1 & df$studyNumber %in% 8:9, ifelse(substr(df$DominantEye,1,1)=="L","LLL","RRR") ,substr(df$DominantEye,1,3))
df<-merge(df, parti[,1:3], by = c("studyNumber","PID"))
df$prevPID<-c(-1,df$PID[1:nrow(df)-1])
df$nextPID<-c(df$PID[2:nrow(df)],-1)
df$prevTouchX<-ifelse(df$prevPID==df$PID,1,NA)*c(-1,df$TouchX[1:nrow(df)-1])
df$prevTouchY<-ifelse(df$prevPID==df$PID,1,NA)*c(-1,df$TouchY[1:nrow(df)-1])
df$prevLiftTime<-ifelse(df$prevPID==df$PID,1,NA)*c(-1,df$LiftTime[1:nrow(df)-1])
df$moveTime<-ifelse(df$HitType!="Outset",1,NA)*(df$TouchTime-df$prevLiftTime)
df$nextTouchIsError<-c(ifelse(df$HitType[2:nrow(df)]=="Miss"|df$HitType[2:nrow(df)]=="WrongTarget"|!(df$nextPID[2:nrow(df)]==df$PID[2:nrow(df)]),TRUE,FALSE), NA)
df$nextXpos<-ifelse(df$nextPID==df$PID & !(df$nextTouchIsError),1,NA)*c(df$TargetX[2:nrow(df)],-1)
df$nextYpos<-ifelse(df$nextPID==df$PID & !(df$nextTouchIsError),1,NA)*c(df$TargetY[2:nrow(df)],-1)
df$inVectorX<- df$TouchX - df$prevTouchX
df$inVectorY<- df$TouchY - df$prevTouchY
df$outVectorX<- df$nextXpos - df$TouchX
df$outVectorY<- df$nextYpos - df$TouchY
df$prevTargDist<-sqrt(df$inVectorX^2+df$inVectorY^2)
df$nextTargDist<- sqrt(df$outVectorX^2+df$outVectorY^2)
df$moveSpeed<-df$prevTargDist/df$moveTime
df$inSlideDiffDeg<- (atan2(df$TouchDeltaY,df$TouchDeltaX)-atan2(df$inVectorY,df$inVectorX))*180/pi
df$slideOutDiffDeg<-  (atan2(df$outVectorY,df$outVectorX)-atan2(df$TouchDeltaY,df$TouchDeltaX))*180/pi
df$slideDist<-sqrt(df$TouchDeltaX^2+df$TouchDeltaY^2)  
df$inSlideDiffDegX<-cos(df$inSlideDiffDeg*pi/180) * sqrt(df$TouchDeltaX^2+df$TouchDeltaY^2)  
df$inSlideDiffDegY<-sin(df$inSlideDiffDeg*pi/180) * sqrt(df$TouchDeltaX^2+df$TouchDeltaY^2) 
df$slideOutDiffDegX<-cos(df$slideOutDiffDeg*pi/180) * sqrt(df$TouchDeltaX^2+df$TouchDeltaY^2) 
df$slideOutDiffDegY<-sin(df$slideOutDiffDeg*pi/180) * sqrt(df$TouchDeltaX^2+df$TouchDeltaY^2) 
df$inputHandBinary <- ifelse(df$inputHand=="R",1,-1)
df$ApproachFromLR <-ifelse(df$TargetDistance<0,-1,1)
df$ApproachFromLRsideFactor <-ifelse(df$TargetDistance<0,"from R" ,"from L")
df$headingIntoDir<-ifelse(df$outVectorX<0,-1,1)
df$headingIntoDirFactor <-ifelse(df$headingIntoDir<0,"to L" ,"to R")
df$GenderEyeOffSet<-ifelse(df$Gender=="M",32,31)
df$LongNailsBinary<-ifelse(df$LongNails=="Yes",1,0)
df$YfromBottom<-df$TargetY-min(df$TargetY)
df$offsetOutlier<-isMADoutlier(df$TouchOffsetX) |   isMADoutlier(df$TouchOffsetY|isMADoutlier(df$TouchDeltaX)|isMADoutlier(df$TouchDeltaY))
df<-df[!df$offsetOutlier,]


df$XoffCenter<-df$TargetX-(round((max(df$TargetX)-min(df$TargetX))/2,0)+min(df$TargetX))
df$YoffCenter<-df$TargetY-(round((max(df$TargetY)-min(df$TargetY))/2,0)+min(df$TargetY))

#df$Xparallax <- (df$XoffCenter-df$GenderEyeOffSet*df$EyeBinary)/55*54.83 - (df$XoffCenter-df$GenderEyeOffSet*df$EyeBinary)
df$Yparallax <-  df$YfromBottom/45.15*45 -(df$YfromBottom)

df9 <-df[df$studyNumber==9,]
df8 <-df[df$studyNumber==8,]
df56<-sqldf("select * from df where studyNumber in (5,6)")
df8G<-df8[df8$HitType=="Goal",]
df8O<-df8[df8$HitType=="Outset",]

# df8$XoffCenter<-df8$TargetX-(round((max(df8$TargetX)-min(df8$TargetX))/2,0)+min(df8$TargetX))
# df8$YoffCenter<-df8$TargetY-(round((max(df8$TargetY)-min(df8$TargetY))/2,0)+min(df8$TargetY))
# df8$YfromBottom<-df8$TargetY-min(df8$TargetY)
df8$EyeBinary <- ifelse(df8$DominantEye=="R",1,-1)
df8$inputHandBinary <- ifelse(df8$inputHand=="R",1,-1)
df8$eyeHelper <- ifelse(df8$XoffCenter < -37,1,ifelse(df8$XoffCenter < 38,2,3))
df8$EyeUsed <- substr(df8$DominantEyenew,df8$eyeHelper,df8$eyeHelper)
df8$Xparallax <- (df8$XoffCenter-df8$GenderEyeOffSet*df8$EyeBinary)/55*54.83 - (df8$XoffCenter-df8$GenderEyeOffSet*df8$EyeBinary)
df8$Yparallax <-  df8$YfromBottom/45.15*45 -(df8$YfromBottom)

# df9$XoffCenter<-df9$TargetX-(round((max(df9$TargetX)-min(df9$TargetX))/2,0)+min(df9$TargetX))
# df9$YoffCenter<-df9$TargetY-(round((max(df9$TargetY)-min(df9$TargetY))/2,0)+min(df9$TargetY))
df9$YfromBottom<-df9$TargetY-min(df9$TargetY)
df9$eyeHelper <- ifelse(df9$XoffCenter < -37,1,ifelse(df9$XoffCenter < 38,2,3))
df9$EyeUsed <- substr(df9$DominantEyenew,df9$eyeHelper,df9$eyeHelper)
df9$EyeBinary <- ifelse(df9$EyeUsed=="R",1,-1)
df9$inputHandBinary <- ifelse(df9$inputHand=="R",1,-1)
df9$GenderEyeOffSet<-ifelse(df9$Gender=="M",32,31)
df9$Xparallax <- (df9$XoffCenter-df9$GenderEyeOffSet*df9$EyeBinary)/55*54.83 - (df9$XoffCenter-df9$GenderEyeOffSet*df9$EyeBinary)
df9$Yparallax <-  df9$YfromBottom/45.15*45 -(df9$YfromBottom)



setwd("~/Dropbox/Apps/ShareLatex/CHI'17-LR Paper/CHI-LR Paper/figures")
s8slideOut<-ggplot(df8[which(df8$HitType %in% c("Goal","Outset")),],aes(x=slideOutDiffDegX,y=slideOutDiffDegY))+coord_equal()+theme_bw()+xlim(-5,15)+ylim(-5,5)+geom_point(aes(colour=inputHand,alpha=0.05))+ geom_hline(aes(yintercept = 0),color="gray") + geom_vline(aes(xintercept = 0),color="gray")+geom_density2d()+facet_grid(. ~ HitType);s8slideOut
ggsave("s8slideOut.png")
s8inSlide<-ggplot(df8[which(df8$HitType %in% c("Goal","Outset")),],aes(x=inSlideDiffDegX,y=inSlideDiffDegY))+coord_equal()+theme_bw()+xlim(-10,5)+ylim(-5,5)+ geom_hline(aes(yintercept = 0),color="gray") + geom_vline(aes(xintercept = 0),color="gray")+geom_point(aes(colour=inputHand,alpha=0.05))+ facet_grid(. ~ HitType);s8inSlide
ggsave("s8inslide.png")

s8TD<-ggplot(df8[which(df8$HitType %in% c("Goal","Outset")),],aes(x=TouchDeltaX,y=TouchDeltaY))+coord_equal()+theme_bw()+xlim(-5,5)+ylim(-5,5)+geom_point(aes(colour=inputHand,alpha=0.05))+ geom_hline(aes(yintercept = 0),color="gray") + geom_vline(aes(xintercept = 0),color="gray")+geom_jitter(aes(colour=inputHand,alpha=0.05))+facet_grid(. ~ HitType);s8TD
ggsave("s8TD.png",width = 4.71,s8TD)
s8TDo<-ggplot(df8[which(df8$HitType %in% c("Outset")),],aes(x=TouchDeltaX,y=TouchDeltaY))+coord_equal()+theme_bw()+xlim(-5,5)+ylim(-5,5)+geom_point(aes(colour=inputHand,alpha=0.05))+ geom_hline(aes(yintercept = 0),color="gray") + geom_vline(aes(xintercept = 0),color="gray")+geom_jitter(aes(colour=inputHand,alpha=0.05))+geom_density2d()+facet_grid(. ~ inputHand*headingIntoDirFactor);s8TDo
ggsave("s8TDo.png",width = 4.71,s8TDo)



s8biasX<-ggplot(df8[which(df8$HitType %in% c("Goal","Outset")),],aes(x=TouchOffsetX,y=TouchOffsetY))+coord_equal()+theme_bw()+xlim(-7,7)+ylim(-7,7)+geom_point(aes(colour=factor(ApproachFromLR),alpha=0.05),shape=21)+ geom_hline(aes(yintercept = 0),color="gray") + geom_vline(aes(xintercept = 0),color="gray")+geom_point(aes(colour=factor(ApproachFromLR),alpha=0.05))+facet_grid(. ~ inputHand);s8biasX
ggsave("s8biasX.png",width = 4.71,s8biasX)
s8biasY<-ggplot(df8[which(df8$HitType %in% c("Goal","Outset")),],aes(x=TouchOffsetX,y=TouchOffsetY))+coord_equal()+theme_bw()+xlim(-7,7)+ylim(-7,7)+geom_point(aes(colour=inputHand,alpha=0.05))+ geom_hline(aes(yintercept = 0),color="gray") + geom_vline(aes(xintercept = 0),color="gray")+geom_point(aes(colour=Yparallax,alpha=0.05))+facet_grid(. ~ inputHand);s8biasY
ggsave("s8biasY.png",width = 4.71,s8biasY)

slideGoalVectorP<- makeScatterWithHorVerDens(df8[which(df8$HitType %in% c("Goal") ),],TouchDeltaX,TouchDeltaY,inputHand,inputHand,-5,5,-5,5);slideGoalVectorP
slideOutsetVectorP<- makeScatterWithHorVerDens(df8[which(df8$HitType %in% c("Outset") ),],slideOutDiffDegX,slideOutDiffDegY,inputHand,inputHand,-2.5,15,-5,5);slideOutsetVectorP

biasFromApproachP<-makeScatterWithHorVerDens(df8[which(df8$HitType %in% c("Outset") ),],TouchOffsetX,TouchOffsetY,ApproachFromLRsideFactor,ApproachFromLRsideFactor,-10,10,-10,10);biasFromApproachP

s9slideOut<-ggplot(df9[which(df9$HitType %in% c("Target","Center")),],aes(x=slideOutDiffDegX,y=slideOutDiffDegY))+coord_equal()+theme_bw()+xlim(-5,15)+ylim(-5,5)+geom_point(aes(colour=inputHand,alpha=0.05))+ geom_hline(aes(yintercept = 0),color="gray") + geom_vline(aes(xintercept = 0),color="gray")+ geom_density2d()+facet_grid(Clockwise ~ HitType);s9slideOut
ggsave("s9slideOut.png",width = 4.71,s9slideOut)
s9InSlide<-ggplot(df9[which(df9$HitType %in% c("Center","Target")),],aes(x=inSlideDiffDegX,y=inSlideDiffDegY))+coord_equal()+theme_bw()+xlim(-15,5)+ylim(-5,5)+geom_point(aes(colour=inputHand,alpha=0.05))+ geom_hline(aes(yintercept = 0),color="gray") + geom_vline(aes(xintercept = 0),color="gray")+ geom_density2d()+facet_grid(Clockwise ~ HitType);s9InSlide
ggsave("s9InSlide.png",width = 4.71,s9InSlide)


sqldf("select   DominantEye, count(PID) from parti where studyNumber=9 group by DominantEye")



attach(df8G)
df8tAgg <-aggregate(df8G, by=list(XoffCenter, YfromBottom, TargetDistance,inputHandBinary,PID,LongNails,Age,Gender,TargetSize,Handed), FUN=mean, na.rm=TRUE)
detach(df8G)
names(df8tAgg)[1:10]<-c("XoffCenter", "YfromBottom", "TargetDistance","inputHandBinary","PID","LongNails","Age","Gender","TargetSize","Handed")
df8tAgg <- df8tAgg[, !duplicated(colnames(df8tAgg))]

# assumptions for parallax, eye distance is 6.4male/6.2cm women 
df8tAgg$GenderEyeOffSet<-ifelse(df8tAgg$Gender=="M",32,31)
#assumption here is that the display glass surface is 1.5mm above the from 
df8tAgg$Xparallax <- (df8tAgg$XoffCenter-df8tAgg$GenderEyeOffSet*df8tAgg$EyeBinary)/55*54.83 - (df8tAgg$XoffCenter-df8tAgg$GenderEyeOffSet*df8tAgg$EyeBinary)
df8tAgg$Yparallax <-   df8tAgg$YfromBottom/45.15*45 - (df8tAgg$YfromBottom)

df8tAgg$ApproachFromLR <-ifelse(df8tAgg$TargetDistance<0,-1,1)


attach(df8G)
slideM<-lm(slideDist~DeltaHitTime+nextTargDist+prevTargDist+XoffCenter+YoffCenter+moveSpeed)
slideM<-lm(slideDist~
                          #DeltaHitTime+
                          #nextTargDist+
                          prevTargDist+
                          #XoffCenter+
                          #YoffCenter+
                          moveSpeed)
summary(slideM)

plmbx<-lm(TouchOffsetX ~    #XoffCenter +
                          #YfromBottom +
                          #TargetDistance + 
                          inputHandBinary +
                           ApproachFromLR  +
                          # inputHandBinary:ApproachFromLR
                          #+ Xparallax 
                          #+ Xparallax 
                          #+ Handed 
                          #+ Age 
                          #+ Gender 
                          #+ EyeBinary:ApproachFromLR 
                          #+ EyeBinary:inputHandBinary
                          + LongNailsBinary:inputHandBinary 
                          #+ LongNailsBinary
      )


detach(df8G)

summary(plmbx)
anova(plmbx)

attach(df8tAgg)
mby<-lm(TouchOffsetY ~      #XoffCenter +
          #YfromBottom +
          #TargetDistance + 
          #ApproachFromLR + 
          inputHandBinary + 
          Yparallax #+
          #Handed  
          #Age + 
          #Gender + 
         # +EyeBinary  #+ inputHandBinary:EyeBinary
          #+ EyeBinary:ApproachFromLR + inputHandBinary:ApproachFromLR
          + LongNailsBinary 
)
detach(df8tAgg)
summary(mby)
anova(mby)


detach(df8G)

summary(plmbx)
anova(plmbx)

attach(df8O)
slideXlm<-lm(TouchDeltaX ~      
               #XoffCenter +
          #YfromBottom +
          #TargetDistance +
          # moveSpeed +
          #ApproachFromLR + 
          inputHandBinary + 
          headingIntoDir
          #Xparallax +
        #factor(Handed) + 
        # + 
        #factor(Gender)
        # +EyeBinary  #+ inputHandBinary:EyeBinary
        #+ EyeBinary:ApproachFromLR 
        #  + 
        #  inputHandBinary:ApproachFromLR+
        #factor(LongNailsBinary) 
)
detach(df8O)
summary(slideXlm)
anova(slideXlm)



attach(df8tAgg)
slideYlm<-lm(TouchDeltaY ~      
               #XoffCenter +
               YfromBottom +
               #TargetDistance + 
               #ApproachFromLR + 
               #inputHandBinary + 
               #Yparallax +
               #Handed + 
                + 
               #Gender
             +EyeBinary  + inputHandBinary:EyeBinary
             #+ EyeBinary:ApproachFromLR 
             + inputHandBinary:ApproachFromLR
             + LongNailsBinary 
)
detach(df8tAgg)
summary(slideYlm)
anova(slideYlm)




attach(df)
diffDegXlm<-lm(inSlideDiffDegX ~      
               XoffCenter +
                 #inVectorY + 
                 inVectorX +
                 outVectorX + 
                 outVectorY +
               #YfromBottom +
               TargetDistance + 
               ApproachFromLR + 
               #inputHandBinary + 
               #Yparallax +
               Handed + 
               + 
               Gender
               #+EyeBinary  + inputHandBinary:EyeBinary
             #+ EyeBinary:ApproachFromLR 
             + inputHandBinary:ApproachFromLR
             #+ LongNailsBinary 
)
detach(df)
summary(diffDegXlm)
anova(diffDegXlm)



attach(df)
diffDegYlm<-lm(inSlideDiffDegY ~      
                 #XoffCenter +
                 inVectorY + 
                 #inVectorX +
                 #outVectorX + 
                 outVectorY +
                 #YfromBottom +
                 #TargetDistance + 
                 ApproachFromLR + 
                 inputHandBinary #+ 
                 #Yparallax +
                 #Handed
                #+ 
                 #Gender
               #+EyeBinary  + inputHandBinary:EyeBinary
               #+ EyeBinary:ApproachFromLR 
               #+ inputHandBinary:ApproachFromLR
               #+ LongNailsBinary 
)
detach(df)
summary(diffDegYlm)
anova(diffDegYlm)



attach(df)
OutdiffDegXlm<-lm(slideOutDiffDegX ~      
                 #XoffCenter +
                 #inVectorY + 
                 inVectorX +
                 #outVectorX + 
                 #outVectorY +
                 #YfromBottom +
                 TargetDistance + 
                 ApproachFromLR + 
                 #inputHandBinary + 
               #Yparallax +
               Handed+ 
               Gender
              #inputHandBinary:ApproachFromLR+
               #LongNailsBinary 
)
detach(df)
summary(OutdiffDegXlm)
anova(OutdiffDegXlm)



attach(df)
OutdiffDegYlm<-lm(slideOutDiffDegY ~      
                    inputHandBinary + 
                    inputHandBinary:Yparallax
)
detach(df)
summary(OutdiffDegYlm)
anova(OutdiffDegYlm)

step(m)
# study 8 -----------------------------------------------------------------
sqldf("select  ClockWise, isDomHand, count(PID) from df8 where (HitType ='Outset' or HitType='Goal') and studyNumber=8 and PID=0 group by Clockwise, isDomHand ")


# study9 ------------------------------------------------------------------


sqldf("select  ClockWise, isDomHand, count(PID) from df9 where (HitType ='Center' or HitType='Target') and studyNumber=9 and PID=0 group by Clockwise, isDomHand ")

 #get unstandardized predicted and residual values
  unstandardizedPredicted <- predict(mbx)
 unstandardizedResiduals <- resid(mbx)
 #get standardized values
 standardizedPredicted <- (unstandardizedPredicted - mean(unstandardizedPredicted)) / sd(unstandardizedPredicted)
 standardizedResiduals <- (unstandardizedResiduals - mean(unstandardizedResiduals)) / sd(unstandardizedResiduals)
 #create standardized residuals plot
 plot(standardizedPredicted, standardizedResiduals, main = "Standardized Residuals Plot", xlab = "Standardized Predicted Values", ylab = "Standardized Residuals")
 #add horizontal line
  abline(0,0)
  hist(standardizedResiduals, freq = FALSE)
  curve(dnorm, add = TRUE)
  probDist <- pnorm(standardizedResiduals)
  plot(ppoints(length(standardizedResiduals)), sort(probDist), main = "PP Plot", xlab = "Observed Probability", ylab = "Expected Probability")
  abline(0,1)
  