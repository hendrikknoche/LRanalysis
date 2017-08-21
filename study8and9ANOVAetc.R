library(xlsx)
library(stringi)
library(sqldf)
library(cowplot)
library(plyr)
library(dplyr)
library(gridExtra)
library(ggplot2)
library(grid)

g_legend <- function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}
source("marginal_plot.R")

isMADoutlier <- function(x) {
  abs(x - median(x,na.rm=TRUE)) > 4*mad(x,na.rm=TRUE)}

makeScatterWithHorVerDens<-function(df,xin,yin,colorVarin,groupVarin,xlimFrom,xlimTo,ylimFrom,ylimTo){ 
  arguments <- as.list(match.call())
  df$xi<-eval(arguments$xin,df)
  df$yi<-eval(arguments$yin,df)
  df$colorVari<-eval(arguments$colorVarin,df)
  df$groupVari<-eval(arguments$groupVarin,df)
  c1<-ggplot(df,aes(xi,yi,colour=colorVari,alpha=0.05))+theme_bw()+geom_point(shape=21) + geom_jitter() + 
    xlim(xlimFrom,xlimTo) + ylim(ylimFrom,ylimTo)+ coord_fixed(ratio = 1)#+ geom_density2d()
  c2<-c1+theme(legend.position = "none")
 
  c2p1 <- ggplot(df, aes(x=xi, group=groupVari, colour=colorVari)) +theme_bw() + geom_line(stat="density") + 
    xlim(xlimFrom,xlimTo)+ theme(legend.position = "none", axis.title.x=element_blank(), axis.text.x=element_blank()) 
  c2p2 <- ggplot(df, aes(x=yi, group=groupVari, colour=colorVari)) +theme_bw() + geom_line(stat="density") + 
    xlim(ylimFrom,ylimTo) +coord_flip() + theme(legend.position = "none", axis.title.y=element_blank(), axis.text.y=element_blank())

  
  legend <- g_legend(c1)
  FinalPlot<-grid.arrange(arrangeGrob(c2p1, legend, c2, c2p2, 
                                      widths = unit.c(unit(0.8*(xlimTo-xlimFrom)/(ylimTo-ylimFrom), "npc"),
                                                      unit(0.2*(xlimTo-xlimFrom)/(ylimTo-ylimFrom), "npc")), 
                                      heights=unit.c(unit(0.2*(ylimTo-ylimFrom)/(xlimTo-xlimFrom), "npc"), 
                                                     unit(0.8*(ylimTo-ylimFrom)/(xlimTo-xlimFrom), "npc")), nrow=2))
  return(FinalPlot)}

#testF<-function(df,colN){ 
#    df$
#    max(df$colN) return(res)}

# mydata <- read.xlsx("~/SVN/BNC/Data/LeftRight Data studies/AllLRData.xlsx", 1,sheetName = "Sheet1")
# prepare the data --------------------------------------------------------


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
df$ApproachFromLR <-ifelse(df$studyNumber==9,ifelse(df$inVectorX<0,-1,1),df$ApproachFromLR)
df$ApproachFromLRsideFactor <-ifelse(df$ApproachFromLR<0,"from R" ,"from L")
df$headingIntoDir<-ifelse(df$outVectorX<0,-1,1)
df$headingIntoDirFactor <-ifelse(df$headingIntoDir<0,"Heading Left" ,"Heading Right")
df$GenderEyeOffSet<-ifelse(df$Gender=="M",32,31)
df$LongNailsBinary<-ifelse(df$LongNails=="Yes",1,0)
df$YfromBottom<-df$TargetY-min(df$TargetY)
df$offsetOutlier<-isMADoutlier(df$TouchOffsetX) |   isMADoutlier(df$TouchOffsetY|isMADoutlier(df$TouchDeltaX)|isMADoutlier(df$TouchDeltaY))
df<-df[!df$offsetOutlier,]

df$slideDeg<-atan2(df$TouchDeltaY,df$TouchDeltaX)*(180/pi)
df$touchDeg<-atan2(df$TouchY,df$TouchX)*(180/pi)
df$inDeg<-atan2(df$inVectorY,df$inVectorX)*(180/pi)
df$outDeg<-atan2(df$outVectorY,df$outVectorX)*(180/pi)


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

df9C<-df9[df9$HitType=="Center",]
df9T<-df9[df9$HitType=="Target",]


#### PLOTTING ####
marginal_plot(x = TouchOffsetX, y = TouchOffsetY, group = inputHand, data = df8[df8$HitType=='Goal',], bw = "nrd", 
              lm_formula = NULL, xlab = "Xbias in mm", ylab = "Ybias in mm", pch = 15, cex = 0.5)
marginal_plot(x = TouchOffsetX, y = TouchOffsetY, group = ApproachFromLRsideFactor, data = df8[df8$HitType=='Goal',], bw = "nrd", 
              lm_formula = NULL, xlab = "Xbias in mm", ylab = "Ybias in mm", pch = 15, cex = 0.5)

marginal_plot(x = TouchOffsetX, y = TouchOffsetY, group = inputHand, data = df9[df9$HitType=='Center',], bw = "nrd", 
              lm_formula = NULL, xlab = "Xbias in mm", ylab = "Ybias in mm", pch = 15, cex = 0.5)


biasHandScatterdf8Goal<-makeScatterWithHorVerDens(df8[which(df8$HitType %in% c("Goal") ),],TouchOffsetX,TouchOffsetY,inputHand,inputHandBinary,-10,10,-10,10);biasHandScatterdf8Goal
biasHandScatterdf8Goal<-makeScatterWithHorVerDens(df8[which(df8$HitType %in% c("Goal") ),],TouchOffsetX,TouchOffsetY,ApproachFromLRsideFactor,ApproachFromLRsideFactor,-10,10,-10,10);biasHandScatterdf8Goal

biasHandScatterdf9Center<-makeScatterWithHorVerDens(df8[which(df8$HitType %in% c("Center") ),],TouchOffsetX,TouchOffsetY,inputHand,inputHandBinary,-10,10,-10,10);biasHandScatterdf9Center

# basic comparison of averages for L/R hand and approach
ddply(df8[which(df8$HitType %in% c("Center") ),c('inputHand','ApproachFromLRsideFactor','TouchOffsetX','TouchOffsetY')], by=c('inputHand','ApproachFromLRsideFactor'))

df8[df8$HitType %in% c("Goal"),]%>%group_by(inputHand, ApproachFromLRsideFactor)%>% summarise(avgTouchOffsetX=mean(TouchOffsetX),avgTouchOffsetY=mean(TouchOffsetY))
df9SlidePlot<-ggplot(df9[which(df9$HitType %in% c("Target","Center") & df9$inputHand %in% c("L")),],aes(x=TouchDeltaX,y=TouchDeltaY, color = factor(Clockwise), shape = inputHand))+
  coord_equal()+theme_bw()+xlim(-6,6)+ylim(-6,6)+
  geom_point(alpha=0.05)+
  geom_density2d()+
  facet_grid(TargetY ~ TargetX)

biasApproachdf8<-sqldf("select inputHand, ApproachFromLRsideFactor, avg(TouchOffsetX) as avgXbias, avg(TouchOffsetY) as avgYbias from df8 where hittype='Goal' group by inputHand, ApproachFromLRsideFactor")
biasApproachdf9<-sqldf("select inputHand, ApproachFromLRsideFactor, avg(TouchOffsetX) as avgXbias, avg(TouchOffsetY) as avgYbias from df9 where hittype='Center' and ApproachFromLRsideFactor<>'NA' group by inputHand, ApproachFromLRsideFactor")
ggplot(biasApproachdf8,aes(avgXbias,avgYbias,colour=inputHand, shape=ApproachFromLRsideFactor, group=interaction(inputHand, ApproachFromLRsideFactor)))+
  geom_point()+coord_fixed(ratio=1)+xlim(-2,2)+ylim(-2,2)
ggplot(biasApproachdf9,aes(avgXbias,avgYbias,colour=inputHand, group=interaction(inputHand, ApproachFromLRsideFactor)))+
  geom_point()+coord_fixed(ratio=1)+xlim(-3,3)+ylim(-3,3)+facet_grid(.~ApproachFromLRsideFactor)
ggplot(df9,aes(TouchOffsetX,TouchOffsetY,colour=inputHand))+
  geom_point()+coord_fixed(ratio=1)+xlim(-10,10)+ylim(-10,10)+facet_grid(.~ApproachFromLRsideFactor)

setwd("~/Dropbox/Apps/ShareLatex/CHI'17-LR Paper/CHI-LR Paper/figures")
s8slideOut<-ggplot(df8[which(df8$HitType %in% c("Goal","Outset")),],aes(x=slideOutDiffDegX,y=slideOutDiffDegY))+coord_equal()+theme_bw()+xlim(-5,15)+ylim(-5,5)+geom_point(aes(colour=inputHand,alpha=0.05))+ geom_hline(aes(yintercept = 0),color="gray") + geom_vline(aes(xintercept = 0),color="gray")+geom_density2d(aes(color = inputHand))+facet_grid( ~ HitType);s8slideOut
ggsave("s8slideOut.png")
s8inSlide<-ggplot(df8[which(df8$HitType %in% c("Goal","Outset")),],aes(x=inSlideDiffDegX,y=inSlideDiffDegY))+coord_equal()+theme_bw()+xlim(-10,5)+ylim(-5,5)+ geom_hline(aes(yintercept = 0),color="gray") + geom_vline(aes(xintercept = 0),color="gray")+geom_point(aes(colour=inputHand,alpha=0.05))+ facet_grid(. ~ HitType);s8inSlide
ggsave("s8inslide.png")


s8slideOut<-ggplot(df8[which(df8$HitType %in% c("Outset") & !is.na(df8$headingIntoDirFactor)),],aes(x=TouchDeltaX,y=TouchDeltaY))+coord_equal()+theme_bw()+xlim(-5,15)+ylim(-5,5)+geom_point(aes(colour=inputHand,alpha=0.05))+ geom_hline(aes(yintercept = 0),color="gray") + geom_vline(aes(xintercept = 0),color="gray")+geom_density2d(aes(color = inputHand))+facet_grid(. ~ headingIntoDirFactor);s8slideOut


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

s9slideOut<-ggplot(df9[which(df9$HitType %in% c("Target","Center")),],aes(x=slideOutDiffDegX,y=slideOutDiffDegY))+
            coord_equal()+theme_bw()+xlim(-5,15)+ylim(-5,5)+geom_point(aes(colour=inputHand,alpha=0.05))+ 
            geom_hline(aes(yintercept = 0),color="gray") + geom_vline(aes(xintercept = 0),color="gray")+ 
            geom_density2d()+facet_grid(Clockwise ~ HitType);s9slideOut
ggsave("s9slideOut.png",width = 4.71,s9slideOut)
s9InSlide<-ggplot(df9[which(df9$HitType %in% c("Center","Target")),],aes(x=inSlideDiffDegX,y=inSlideDiffDegY)) +
          coord_equal()+theme_bw()+xlim(-15,5)+ylim(-5,5)+geom_point(aes(colour=inputHand,alpha=0.05)) +
          geom_hline(aes(yintercept = 0),color="gray") + geom_vline(aes(xintercept = 0),color="gray")+
          geom_density2d()+facet_grid(Clockwise ~ HitType);s9InSlide
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
detach(df8G)
attach(df9C)
plmbx<-lm(TouchOffsetX ~  
            #XoffCenter
            #+ YfromBottom
            #+ TargetDistance 
            + inputHandBinary
            + ApproachFromLR
            #+ inputHandBinary:ApproachFromLR
            + Xparallax
            #+ EyeBinary:ApproachFromLR
            #+ EyeBinary:inputHandBinary
            + LongNailsBinary:inputHandBinary
            #+ LongNailsBinary
      )


summary(plmbx)
detach(df9C)


anova(plmbx)

attach(df8G)
mby<-lm(TouchOffsetY ~  #XoffCenter
        #+ YfromBottom
        + TargetDistance 
        + inputHandBinary
        #+ ApproachFromLR
        + inputHandBinary:ApproachFromLR
        + Xparallax
        + EyeBinary:ApproachFromLR
        #+ EyeBinary:inputHandBinary
        + LongNailsBinary:inputHandBinary
        + LongNailsBinary
)
detach(df8G)
summary(mby)
anova(mby)


attach(df8O)
plmbx<-lm(TouchOffsetX ~  
          #XoffCenter
          #+ YfromBottom
          #+ TargetDistance 
          + inputHandBinary
          + headingIntoDir
          #+ inputHandBinary:headingIntoDir
          #+ Yparallax
          #+ Xparallax
          #+ EyeBinary:headingIntoDir
          #+ EyeBinary:inputHandBinary
          + LongNailsBinary:inputHandBinary
          #+ LongNailsBinary
)

detach(df8O)

summary(plmbx)
anova(plmbx)


attach(df8O)
plmbx<-lm(TouchOffsetY ~  
            #XoffCenter
            #+ YfromBottom
            #+ TargetDistance 
            #+ inputHandBinary
          #+ headingIntoDir
          #+ inputHandBinary:headingIntoDir
          + Yparallax
          + Xparallax
          #+ EyeBinary:headingIntoDir
          #+ EyeBinary:inputHandBinary
          #+ LongNailsBinary:inputHandBinary
          + LongNailsBinary
)

detach(df8O)

summary(plmbx)
anova(plmbx)


detach(df8G)

summary(plmbx)
anova(plmbx)

attach(df8O)
slideXlm<-lm(TouchDeltaX ~  
               #TargetDistance 
               + inputHandBinary
               + headingIntoDir
               #+ inputHandBinary:headingIntoDir
               #+ Yparallax
               #+ Xparallax
               #+ LongNailsBinary:inputHandBinary
               + LongNailsBinary
)
detach(df8O)
summary(slideXlm)
anova(slideXlm)



attach(df8O)
slideYlm<-lm(TouchDeltaY  ~  #TargetDistance 
               #+ inputHandBinary
             #+ headingIntoDir
             #+ inputHandBinary:headingIntoDir
             #+ Yparallax
             #+ Xparallax
             #+ LongNailsBinary:inputHandBinary
             + LongNailsBinary )
detach(df8O)
summary(slideYlm)
anova(slideYlm)




attach(df8G)
slideXlm<-lm(TouchDeltaX ~  
               #TargetDistance 
               + inputHandBinaryattach(df9C)
             #+ ApproachFromLR
             #+ inputHandBinary:ApproachFromLR
             #+ Yparallax
             + Xparallax
             + LongNailsBinary:inputHandBinary
             + LongNailsBinary
)
detach(df8G)
summary(slideXlm)
anova(slideXlm)



attach(df8G)
slideYlm<-lm(TouchDeltaY  ~  
               #TargetDistance 
               #+ inputHandBinary
               #+ ApproachFromLR
               #+ inputHandBinary:ApproachFromLR
               + Yparallax
               + Xparallax
               #+ LongNailsBinary:inputHandBinary
               + LongNailsBinary
)
detach(df8G)
summary(slideYlm)
anova(slideYlm)

df9aggByPerson<-sqldf('select PID,inputHand,ApproachFromLR, avg(TouchOffsetX) as Xb, avg(TouchOffsetY) as Yb from df9C where ApproachFromLR is not null group by PID, ApproachFromLR, inputHand')


attach(df8G)
diffDegXlm<-lm(inSlideDiffDegX ~ #XoffCenter
               #+ outVectorX
               #+ outVectorY
               #+ YfromBottom
               #+ TargetDistance
               #+ ApproachFromLR
               #+ inputHandBinary 
               #+ Xparallax
               #+ Yparallax
               #+ EyeBinary
               #+ inputHandBinary:EyeBinary
               #+ EyeBinary:ApproachFromLR
               + inputHandBinary:ApproachFromLR
               + LongNailsBinary 
)
detach(df8G)
summary(diffDegXlm)
anova(diffDegXlm)



attach(df8G)
diffDegYlm<-lm(inSlideDiffDegY ~ #XoffCenter
               #+ outVectorX
               #+ outVectorY
               #+ YfromBottom
               #+ TargetDistance
               + ApproachFromLR
               + inputHandBinary 
               #+ Yparallax
               #+ EyeBinary
               #+ inputHandBinary:EyeBinary
               #+ EyeBinary:ApproachFromLR
               #+ inputHandBinary:ApproachFromLR
               #+ LongNailsBinary 
)
detach(df8G)
summary(diffDegYlm)
anova(diffDegYlm)



attach(df8O)
OutdiffDegXlm<-lm(slideOutDiffDegX ~ #XoffCenter
                  #+ YfromBottom
                  #+ TargetDistance
                  #+ headingIntoDir
                  #+ inputHandBinary 
                  #+ Xparallax
                  + Yparallax
                  #+ EyeBinary
                  #+ inputHandBinary:EyeBinary
                  #+ EyeBinary:headingIntoDir
                  + inputHandBinary:headingIntoDir
                  #+ LongNailsBinary 
)
detach(df8O)
summary(OutdiffDegXlm)
anova(OutdiffDegXlm)



attach(df8O)
OutdiffDegYlm<-lm(slideOutDiffDegY ~ #XoffCenter
                    #+ inVectorY
                    #+ inVectorX
                    #+ YfromBottom
                  #+ TargetDistance
                  #+ headingIntoDir
                  + inputHandBinary 
                  #+ Xparallax
                  #+ Yparallax
                  #+ EyeBinary
                  #+ inputHandBinary:EyeBinary
                  #+ EyeBinary:ApproachFromLR
                  #+ inputHandBinary:headingIntoDir
                  #+ LongNailsBinary 
)
detach(df8O)
summary(OutdiffDegYlm)
anova(OutdiffDegYlm)



attach(df)
OutdiffDegYlm<-lm(slideOutDiffDegY ~      
                    inputHandBinary + 
                    inputHandBinary:Yparallax
)
detach(df)
summary(OutdiffDegYlm)
anova(OutdiffDegYlm)




attach(df9T)

plmbx<-lm(slideDeg ~   
          #+ inputHandBinary
          #+ Xparallax
          #+ Yparallax
          #+ inVectorX
          #+ inVectorY
          #+ outVectorX
          #+ outVectorY
          #+ inDeg
          + outDeg
          #+ LongNailsBinary:inputHandBinary
          #+ LongNailsBinary
)

detach(df9qT)

summary(plmbx)
anova(plmbx)




ggplot(df8[which(df8$HitType %in% c("Goal","Outset")),],aes(x=TouchOffsetX,y=TouchOffsetY, color = factor(inputHand)))+
  geom_vline(xintercept = 0)+
  geom_line(stat = "density")+
  theme_bw()+
  theme(legend.position = "none",
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        text = element_text(size=20))+
  labs(y = "Density")

df8BiasPoints <- ggplot(df8[which(df8$HitType %in% c("Goal","Outset")),],aes(x=TouchOffsetX,y=TouchOffsetY, color = factor(inputHand)))+
  coord_equal()+
  scale_y_continuous(breaks = c(-5, 0, 5, 5),
                     labels = c(-5, 0, "5.00", 5))+
  geom_hline(yintercept = 0)+
  geom_vline(xintercept = 0)+
  theme_bw()+
  theme(legend.position = "bottom",
        text = element_text(size=20),
        axis.text.y = element_text(colour=c("black","black","white","black")))+
  labs(x = "Touch Bias X",
       y = "Touch Bias Y",
       color = "Input Hand") +
  scale_color_discrete(name  ="Input Hand",
                       breaks=c("L", "R"),
                       labels=c("Left", "Right")) +
  geom_point(alpha=0.2)

grid.arrange(df8BiasDensity, df8BiasPoints, heights = c(1,4), widths = 1)


pointDummy <- df8[1:2,]
pointDummy$TouchDeltaX <- c(-5,5)
pointDummy$TouchDeltaY <- 0
pointDummy$headingIntoDirFactor <- c("Heading Left","Heading Right")

ggplot(df8[which(df8$HitType %in% c("Outset")) & !is.na(df8$headingIntoDirFactor),],aes(x=TouchDeltaX,y=TouchDeltaY, color = factor(inputHand)))+
  coord_equal()+
  geom_hline(yintercept = 0)+
  geom_vline(xintercept = 0)+
  geom_point(data = pointDummy, color = "red", size = 10)+
  theme_bw()+
  theme(legend.position = "bottom",
        text = element_text(size=20),
        strip.background = element_rect(fill="white"))+
  labs(x = "Slide X",
       y = "Slide Y",
       color = "Input Hand") +
  scale_color_discrete(name  ="Input Hand",
                       breaks=c("L", "R"),
                       labels=c("Left", "Right")) +
  xlim(-5,5)+ylim(-2.5,2.5)+
  geom_point(alpha=0.2)+
  geom_density2d()+
  facet_grid(headingIntoDirFactor ~ .)




df9BiasDensity <- ggplot(df9[which(df9$HitType %in% c("Center","Target")),],aes(x=TouchOffsetX, color = factor(inputHand)))+
  geom_vline(xintercept = 0)+
  geom_line(stat = "density")+
  theme_bw()+
  theme(legend.position = "none",
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        text = element_text(size=20))+
  geom_density2d()+
  labs(y = "Density")

df9BiasPoints <- ggplot(df9[which(df9$HitType %in% c("Center","Target")),],aes(x=TouchOffsetX,y=TouchOffsetY, color = factor(inputHand)))+
  coord_equal()+ 
  scale_y_continuous(breaks = c(-5, 0, 5, 5),
                     labels = c(-5, 0, "5.00", 5))+
  geom_hline(yintercept = 0)+
  geom_vline(xintercept = 0)+
  theme_bw()+
  theme(legend.position = "bottom",
        text = element_text(size=20),
        axis.text.y = element_text(colour=c("black","black","white","black")))+
  labs(x = "Touch Bias X",
       y = "Touch Bias Y",
       color = "Input Hand") +
  scale_color_discrete(name  ="Input Hand",
                       breaks=c("L", "R"),
                       labels=c("Left", "Right")) +
  geom_point(alpha=0.15)

grid.arrange(df9BiasDensity, df9BiasPoints, heights = c(1,4))




ggplot(df9[which(df9$HitType %in% c("Center","Target")),],aes(x=slideOutDiffDegY,y=slideOutDiffDegX), color = black)+
  coord_equal()+
  geom_hline(yintercept = 0)+
  geom_vline(xintercept = 0)+
  geom_point(x = 0, y = 5, color = "green", size = 20)+
  theme_bw()+
  theme(legend.position = "bottom",
        text = element_text(size=20))+
  labs(x = "Normalized Slide X",
       y = "Normalized Slide Y") +
  ylim(-1.75,5)+xlim(-2,2)+
  geom_point(alpha=0.08,position = position_jitter(width = 0.1, height = 0.1))+
  geom_density2d(color = "black", size = 1.05)




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
  