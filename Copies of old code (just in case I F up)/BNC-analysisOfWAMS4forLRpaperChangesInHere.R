library(RODBC)
library("Rmisc", lib.loc="\\\\Mac/Home/Documents/R/win-library/3.2")
library("Rmisc", lib.loc="C:/Users/Hendrik/AppData/Local/R")
library("grid", lib.loc="C:/Program Files/R/R-3.2.2/library")
library(grid) 
library("RODBCext", lib.loc="C:/Users/Hendrik/AppData/Local/R")
library("plyr", lib.loc="\\\\Mac/Home/Documents/R/win-library/3.2")
library("ggplot2", lib.loc="\\\\Mac/Home/Documents/R/win-library/3.2")
library("grid", lib.loc="C:/Program Files/R/R-3.2.2/library")
library(ggplot2)

### === data import from either accesss or mysql ======================
libloc= Sys.getenv("R_LIBS_USER")
### === data import from mysql - make sure the config.R file exists and has all information user/pass/dbname/serverIP =======================
source(paste(libloc,"//config.R",sep=''))

myconn <-odbcConnect("CreateServer", uid=ODBCUID, pwd=pass)

WAMS42cent  <-sqlQuery(myconn, "select * from ana_WAM_log where studyNumber in (2,4) and hitOutcome>0 and hitTypeID in (4)")
WAMS42targ  <- sqlQuery(myconn, "select * from ana_WAM_log where studyNumber in (2,4) and hitOutcome>0 and hitTypeID in (0,1,2)")
WAMS42targL <- sqlQuery(myconn, "select * from ana_WAM_log where studyNumber in (2,4) and dominantHand='l' and hitOutcome>0 and hitTypeID in (0,1,2)")
WAMS42targR <- sqlQuery(myconn, "select * from ana_WAM_log where studyNumber in (2,4) and dominantHand='r' and hitOutcome>0 and hitTypeID in (0,1,2)")
close(myconn) 

WAMS42targ[,"PIDSN"]<- paste(WAMS42targ$PID,WAMS42targ$studyNumber,sep="_")
WAMS42targ[,"xposNmir"]<- ifelse(WAMS42targ$DominantHand=="r",ifelse(WAMS42targ$targPosXvp>.5,"il",ifelse(WAMS42targ$targPosXvp<.5,"cl","M")),ifelse(WAMS42targ$targPosXvp>.5,"cl",ifelse(WAMS42targ$targPosXvp<.5,"il","M")))
WAMS42targ[,"xpos"]<- ifelse(WAMS42targ$targPosXvp>.5,"R",ifelse(WAMS42targ$targPosXvp<.5,"L","M"))
WAMS42targ[,"ypos"]<- ifelse(WAMS42targ$targPosYvp>.5,"T",ifelse(WAMS42targ$targPosYvp<.5,"B","M"))
WAMS42targ[,"hitOffsetXmmmir"]<-ifelse(WAMS42targ$DominantHand=="r",WAMS42targ$hitOffsetXmm,-1*WAMS42targ$hitOffsetXmm)
WAMS42targc<-ddply(WAMS42targ[WAMS42targ$ypos!="M" & WAMS42targ$xpos!="M",],c("xpos", "ypos","DominantHand","PIDSN"),summarise,Xb=mean(hitOffsetXmm),Xbsd=sd(hitOffsetXmm),Yb=mean(hitOffsetYmm),Ybsd=sd(hitOffsetYmm),N=length(hitOffsetXmm))
WAMS42targcMir<-ddply(WAMS42targ[WAMS42targ$ypos!="M" & WAMS42targ$xpos!="M",],c("xposNmir", "ypos","DominantHand","PIDSN"),summarise,Xb=mean(hitOffsetXmmmir),Xbsd=sd(hitOffsetXmmmir),Yb=mean(hitOffsetYmm),Ybsd=sd(hitOffsetYmm),N=length(hitOffsetXmmmir))

WAMS42cent[,"PIDSN"]<- paste(WAMS42cent$PID,WAMS42cent$studyNumber,sep="_")
WAMS42cent[,"xposNmir"]<- ifelse(WAMS42cent$DominantHand=="r",ifelse(WAMS42cent$prevTargXvp>.5,"tocl",ifelse(WAMS42cent$prevTargXvp<.5,"toil","M")),ifelse(WAMS42cent$prevTargXvp>.5,"toil",ifelse(WAMS42cent$prevTargXvp<.5,"tocl","M")))
WAMS42cent[,"xpos"]<- ifelse(WAMS42cent$prevTargXvp>.5,"R",ifelse(WAMS42cent$prevTargXvp<.5,"L","M"))
WAMS42cent[,"ypos"]<- ifelse(WAMS42cent$prevTargYvp>.5,"T",ifelse(WAMS42cent$prevTargYvp<.5,"B","M"))
WAMS42cent[,"hitOffsetXmmmir"]<-ifelse(WAMS42cent$DominantHand=="r",WAMS42cent$hitOffsetXmm,-1*WAMS42cent$hitOffsetXmm)
WAMS42centc<-ddply(WAMS42cent[WAMS42cent$ypos!="M" & WAMS42cent$xpos!="M",],c("xpos", "ypos","DominantHand","PIDSN"),summarise,Xb=mean(hitOffsetXmm),Xbsd=sd(hitOffsetXmm),Yb=mean(hitOffsetYmm),Ybsd=sd(hitOffsetYmm),N=length(hitOffsetXmm))
WAMS42centcMir<-ddply(WAMS42cent[WAMS42cent$ypos!="M" & WAMS42cent$xpos!="M",],c("xposNmir", "ypos","DominantHand","PIDSN"),summarise,Xb=mean(hitOffsetXmmmir),Xbsd=sd(hitOffsetXmmmir),Yb=mean(hitOffsetYmm),Ybsd=sd(hitOffsetYmm),N=length(hitOffsetXmmmir))


attach(WAMS42targcMir)
  aovTXb.out = aov(Xb~xposNmir*ypos*DominantHand+Error(PIDSN/(xposNmir*ypos))+DominantHand)
  summary(aovTXb.out)
  shapiro.test(aovTXb.out$PIDSN$residuals)
  
  aovTYb.out = aov(Yb~xposNmir*ypos*DominantHand+Error(PIDSN/(xposNmir*ypos))+DominantHand)
  summary(aovTYb.out)
  shapiro.test(aovTYb.out$PIDSN$residuals)
detach(WAMS42targcMir)
  
attach(WAMS42centcMir)
  aovCXbmir.out = aov(Xb~xposNmir*ypos*DominantHand+Error(PIDSN/(xposNmir*ypos))+DominantHand)
  summary(aovCXbmir.out)
  shapiro.test(aovCXbmir.out$PIDSN$residuals)
  aovCYbmir.out = aov(Yb~xposNmir*ypos*DominantHand+Error(PIDSN/(xposNmir*ypos))+DominantHand)
  summary(aovCYbmir.out)
  shapiro.test(aovCYbmir.out$PIDSN$residuals)
  hist(aovCYbmir.out$PIDSN$residuals)
  qqnorm(aovCYbmir.out$PIDSN$residuals)
  qqline(aovCYbmir.out$PIDSN$residuals)
  ddply(WAMS42centcMir,~xposNmir,summarise,Xbmean=mean(Xb,na.rm = TRUE),Xbsd=sd(Xb,na.rm = TRUE),Ybmean=mean(Yb,na.rm = TRUE),Ybsd=sd(Yb,na.rm = TRUE))
detach(WAMS42centcMir)

######## here the analysis non mirrored  
  attach(WAMS42centc)
    aovCXb.out = aov(Xb~xpos*ypos*DominantHand+Error(PIDSN/(xpos*ypos))+DominantHand)
  summary(aovCXb.out)
  shapiro.test(aovCXb.out$PIDSN$residuals)
  
  aovCYb.out = aov(Yb~xpos*ypos*DominantHand+Error(PIDSN/(xpos*ypos))+DominantHand)
  summary(aovCYb.out)
  shapiro.test(aovCYb.out$PIDSN$residuals)
  qqnorm(aovCYb.out$PIDSN$residuals)
  qqline(aovCYb.out$PIDSN$residuals)
  
  ddply(WAMS42centc,~xpos,summarise,Xbmean=mean(Xb,na.rm = TRUE),Xbsd=sd(Xb,na.rm = TRUE),Ybmean=mean(Yb,na.rm = TRUE),Ybsd=sd(Yb,na.rm = TRUE) )
  
detach(WAMS42centc)
  
  ######## end of the analysis non mirrored   

ybias3wayX<-ddply(WAMS42targcMir,.(DominantHand,xposNmir,ypos),summarise,Xbmean=mean(Xb),Xbsd=sd(Xb),Ybmean=mean(Yb),Ybsd=sd(Yb))
ybias2wayX<-ddply(WAMS42targcMir,.(xposNmir,ypos),summarise,Xbmean=mean(Xb),Xbsd=sd(Xb),Ybmean=mean(Yb),Ybsd=sd(Yb))


ggplot(data = ybias3wayX,aes(x = Xarr,y = Yarr)) + 
  coord_fixed(ratio = 1,xlim = c(-3, 3),ylim=c(-3,3))+
  #geom_point() + 
  #geom_errorbar(aes(ymin = Y-Yci,ymax = Y+Yci)) + 
  #geom_errorbarh(aes(xmin = X-Xci,xmax = X+Xci))+geom_hline(aes(yintercept=0),colour="black",linetype=2)+geom_vline(aes(yintercept=0),linetype=2)+
  geom_segment(aes(x = Xarr, y = Yarr, xend = Xarr, yend = Ybmean+Yarr), arrow = arrow(length = unit(0.65, "cm")), color=c("red","blue")[factor(ybias3wayX$DominantHand)] )+ theme_bw()+
  #geom_point(data=cLR12[cLR12$TargetSize==6 & cLR12$hitType=="Target",],aes(x=Xb,y=Yb),shape=1)
  ggtitle( "Y bias on small target on top/bottom, contra- and ipsilateral side")     ;


yp<-c("T","T","B","B");xpNmir<-c("cl","il","cl","il");Xarr<-2*c(-1,1,-1,1);Yarr<-2*c(1,1,-1,-1)
xybarArrows<-data.frame(yp,xpNmir,Xarr,Yarr);
ybias3wayX<-join(ybias3wayX,xybarArrows,by=c("xposNmir","ypos"));  #####TODO rename columns to il cl,

plot2<-ggplot(data = subset(xybar,TargetSize==6  ),aes(x = X,y = Y)) + 
  coord_fixed(ratio = 1,xlim = c(-2.5, 5),ylim=c(-5,2.5))+
  geom_point() + 
  geom_errorbar(aes(ymin = Y-Yci,ymax = Y+Yci)) + 
  geom_errorbarh(aes(xmin = X-Xci,xmax = X+Xci))+geom_hline(aes(yintercept=0),colour="black",linetype=2)+geom_vline(aes(yintercept=0),linetype=2)+
  geom_segment(aes(x = X+Xarr, y = Y+Yarr, xend = X, yend = Y), arrow = arrow(length = unit(0.65, "cm")), color=c("red","blue")[factor(fromLR)] )+
  geom_point(data=cLR12[cLR12$TargetSize==6 & cLR12$hitType=="Target",],aes(x=Xb,y=Yb),shape=1)+ theme_bw()+
  ggtitle( "Touch bias on small target with approach vector")     ;






attach(WAMS42targc)
aov.out = aov(Xb~xpos*ypos*DominantHand+Error(PIDSN/(xpos*ypos))+DominantHand)
summary(aov.out)
aov.out = aov(Yb~xpos*ypos*DominantHand+Error(PIDSN/(xpos*ypos))+DominantHand)
summary(aov.out)
detach(WAMS42targc)
ddply(WAMS42targc,~DominantHand,summarise,Xbmean=mean(Xb),Xbsd=sd(Xb),Ybmean=mean(Yb),Ybsd=sd(Yb))

#fix these three below
#test.pr<-proj(aov.out)
#residualsC[1]<-data.frame(test.pr[[3]][,"Residuals"])
#WAMS42targ[,"resXb"]<-residualsC[1]

shapiro.test(aov.out$PIDSN$residuals)
shapiro.test(aov.out$`PIDSN:xpos`$residuals)
shapiro.test(aov.out$`PIDSN:ypos`$residuals)
shapiro.test(aov.out$`PIDSN:xpos:ypos`$residuals)


## === 
WAMS42$PID <-factor(WAM4$PID)
WAM4 <- subset(WAM4,WAM4$hitOutcome>0)
lmer(WAM4$hitOffsetXmm ~ (1|WAM4$PID)+WAM4$handUsedForInput+WAM4$RL)
boxplot(WAM4$hitOffsetXmm~WAM4$+WAM4$handUsedForInput)
politeness.model = lmer(frequency ~ attitude +
                  gender + (1+attitude|subject) +
                  (1+attitude|scenario),data=politeness,REML=FALSE)

t<-data.frame(aggdata$hOffX,aggdata$hOffY,aggdata$hOffXr,aggdata$hOffXl,aggdata$hOffYr,aggdata$hOffYr,aggdata$hOffXmmcentre,aggdata$hOffXmmfLcent,aggdata$hOffXmmfRcent,aggdata$HandTypeID)
colnames(t)<-c("hOffX", "hOffY", "hOffXr", "hOffXl","hOffYr","hOffYr","hOffCentX","hOffCentXfL","hOffCentXfR","handType" )

aggdata <-sqlQuery(myconn, "select * from ana_SessionBased_WAMallButS3 where studyNumber=4")


#TODO subset to non-fM "M", stitch x,y together with  for four sector


### ANOVA for x/y bias ,TB,LRh as factors from for center and for mole


################################### over view of material through plots
#target arrow plot of bias L/R handed participants

zoomf<-3
WAMS42targ[,"xbins"]<-floor(WAMS42targ$targPosXvp*zoomf)/zoomf+1/zoomf/2
WAMS42targ[,"ybins"]<-floor(WAMS42targ$targPosYvp*zoomf)/zoomf+1/zoomf/2
#WAMS42targR[,"ybins"]<-floor(WAMS42targR$targPosYvp*zoomf)/zoomf
#WAMS42targL[,"ybins"]<-floor(WAMS42targL$targPosYvp*zoomf)/zoomf
W42tRcx<-summarySE(WAMS42targ, measurevar="hitOffsetXvp",groupvars=c("xbins", "ybins","DominantHand"),na.rm=TRUE)
W42tRcy<-summarySE(WAMS42targ, measurevar="hitOffsetYvp",groupvars=c("xbins", "ybins","DominantHand"),na.rm=TRUE)
W42Arr<-join(W42tRcx,W42tRcy,by=c("xbins","ybins","DominantHand"))
W42Arr<-W42Arr[W42Arr$xbins>=0 & W42Arr$xbins<=1,]

# uncomment the next two lines if you want the mirrored version in which Lh people have their x pos (1-Xvp) and x-offset (*-1) mirrored
W42Arr$xbins[W42Arr$DominantHand=="l"]<- 1-W42Arr$xbins[W42Arr$DominantHand=="l"]
W42Arr$hitOffsetXvp[W42Arr$DominantHand=="l"]<- -1*W42Arr$hitOffsetXvp[W42Arr$DominantHand=="l"]

ggplot(data=W42Arr, aes(x=xbins, y=ybins)) + geom_segment(aes(xend=xbins+30*hitOffsetXvp, yend=ybins+30*hitOffsetYvp), arrow = arrow(length = unit(0.3,"cm")), colour=c("blue","red")[factor(W42Arr$DominantHand)])+theme_bw()

######## targets - plots of bias with approachvectors
WAMS42targ[,"fromLR"]<- ifelse(WAMS42targ$targPosXvp<.5,"fR",ifelse(WAMS42targ$targPosXvp>.5,"fL","fM"))
WAMS42targ[,"fromTB"]<- ifelse(WAMS42targ$targPosYvp<.5,"fT",ifelse(WAMS42targ$targPosYvp>.5,"fB","fM"))
WAMS42targcX<-summarySE(WAMS42targ, measurevar="hitOffsetXmm",groupvars=c("fromTB", "fromLR","DominantHand"),na.rm=TRUE)
WAMS42targcY<-summarySE(WAMS42targ, measurevar="hitOffsetYmm",groupvars=c("fromTB", "fromLR","DominantHand"),na.rm=TRUE)

Txybar<-join(WAMS42targcX,WAMS42targcY,by=c("fromTB","fromLR","DominantHand"))
Txybar<-subset(Txybar,fromTB!="M" & fromLR!="M")
names(Txybar)[5:8]<-c("X","Xsd","Xse","Xci")
names(Txybar)[10:13]<-c("Y","Ysd","Yse","Yci")
Txybar<-subset(Txybar,fromTB!="fM" & fromLR!="fM")
fromTB<-c("fT","fT","fB","fB");fromLR<-c("fL","fR","fL","fR");Xarr<-.5*c(-1,1,-1,1);Yarr<-.5*c(1,1,-1,-1)

ggplot(data=Txybar ,aes(colour=DominantHand,x = X,y = Y)) + 
  coord_fixed(ratio = 1,xlim = c(-1, 1),ylim=c(-1,.6))+
  geom_hline(aes(yintercept=0),colour="black",linetype=2)+
  geom_vline(aes(yintercept=0),linetype=2)+
  geom_point() + 
  geom_errorbar(aes(ymin = Y-Yci,ymax = Y+Yci),width=0.1) + 
  geom_errorbarh(aes(xmin = X-Xci,xmax = X+Xci),width=0.1)+
  theme_bw()+theme(legend.title=element_blank())+
  geom_segment(aes(x = X+Xarr, y = Y+Yarr, xend = X, yend = Y), arrow = arrow(length = unit(0.45, "cm")), color=c("blue","red")[factor(Txybar$fromLR)]) ;

#### targets - plots of bias with approachvectors - mirrored i.e. ipsi/contralateral 

#ggplot(data=Txybar, aes(x=X, y=Y)) + 
#  geom_errorbar(aes(ymin = Y-Yci,ymax = Y+Yci),width=0.1) + 
#  geom_errorbarh(aes(xmin = X-Xci,xmax = X+Xci),width=0.1) +
#  geom_segment(aes(xend=xbins+30*hitOffsetXvp, yend=ybins+30*hitOffsetYvp), arrow = arrow(length = unit(0.3,"cm")), colour=c("blue","red")[factor(W42Arr$DominantHand)]+ 
#  theme_bw() 

#################### now for the center button###############################
# create approach for centers
WAMS42cent[,"fromLR"]<- ifelse(WAMS42cent$prevTargXvp>.5,"fR",ifelse(WAMS42cent$prevTargXvp<.5,"fL","fM"))
WAMS42cent[,"fromTB"]<- ifelse(WAMS42cent$prevTargYvp>.5,"fT",ifelse(WAMS42cent$prevTargYvp<.5,"fB","fM"))

WAMS42CentcX<-summarySE(WAMS42cent, measurevar="hitOffsetXmm",groupvars=c("fromTB", "fromLR","DominantHand"),na.rm=TRUE)
WAMS42CentcY<-summarySE(WAMS42cent, measurevar="hitOffsetYmm",groupvars=c("fromTB", "fromLR","DominantHand"),na.rm=TRUE)
xybar<-join(WAMS42CentcX,WAMS42CentcY,by=c("fromTB","fromLR","DominantHand"))

names(xybar)[5]<-"X";names(xybar)[5]<-"X";
names(xybar)[10]<-"Y";names(xybar)[11]<-"Ysd";names(xybar)[12]<-"Yse"; names(xybar)[13]<-"Yci"
names(xybar)[5:8]<-c("X","Xsd","Xse","Xci")

fromTB<-c("fT","fT","fB","fB");fromLR<-c("fL","fR","fL","fR");Xarr<-.5*c(-1,1,-1,1);Yarr<-.5*c(1,1,-1,-1)
xybarArrows<-data.frame(fromTB,fromLR,Xarr,Yarr);
xybar<-join(xybar,xybarArrows,by=c("fromTB","fromLR"));
xybar<- subset(xybar,fromLR!="fM" & fromTB!="fM" )

png(file="Z://Dropbox//BNC_trial//Analysis//WAMS42//notitle.png",width=2000, height=2000,res=300)
par(mar=c(5,3,2,2)+0.1)

ggplot(data=xybar ,aes(colour=DominantHand,x = X,y = Y)) + 
  coord_fixed(ratio = 1,xlim = c(-1, 1),ylim=c(-1.5,.6))+
  geom_hline(aes(yintercept=0,xintercept=0),colour="black",linetype=2)+
  geom_vline(aes(yintercept=0,xintercept=0),linetype=2)+
  geom_point() + 
  geom_errorbar(aes(ymin = Y-Yci,ymax = Y+Yci),width=0.1) + 
  geom_errorbarh(aes(xmin = X-Xci,xmax = X+Xci),width=0.1)+
  theme_bw()+theme(legend.title=element_blank())+
  geom_segment(aes(x = X+Xarr, y = Y+Yarr, xend = X, yend = Y), arrow = arrow(length = unit(0.45, "cm")), color=c("blue","red")[factor(xybar$fromLR)]) ;
dev.off()

+ ggtitle( "Touch bias on center with approach vector by hand - left (red) and right (blue) ") ;


geom_point(data=cLR12[cLR12$TargetSize==6 & cLR12$hitType=="Center",],aes(x=Xb,y=Yb),shape=1)+

