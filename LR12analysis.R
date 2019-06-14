# loading libraries
library(Matrix)
library(lme4)
library(RODBC)
library(mvoutlier)
library(grid)
library(gridExtra)
library(plyr)
library(Rmisc)
library(ggplot2)

# import data LR12
### === data import from either accesss or mysql ======================
libloc= Sys.getenv("R_LIBS_USER")
### === data import from mysql - make sure the config.R file exists and has all information user/pass/dbname/serverIP =======================
source(paste(libloc,"//config.R",sep=''))

myconn <-odbcConnect("CreateServer", uid=ODBCUID, pwd=pass)

LR12 <-sqlQuery(myconn, "select * from ana_LR12_data_byTBLR  ")
LR12cent<-LR12[LR12$HitType=="Center",]
LR12targ<-LR12[LR12$HitType=="Target",]
LR12cent <-sqlQuery(myconn, "SELECT * FROM ana_LR12_data_byTBLR where hitType='Center' and fromTB<>'M' and fromLR<>'M'");
LR12targ <- sqlQuery(myconn,"SELECT * FROM ana_LR12_data_byTBLR where hitType='Target' and LMR <>'M' and UMD<>'M'");

LR12Base <-sqlQuery(myconn, "SELECT * FROM ana_LR12_data ");
LR12BaseC <- sqlQuery(myconn,"SELECT * FROM ana_LR12_data where hitType='Center' and fromTB<>'M' and fromLR<>'M'and dominanthand='r' ") ;
LR12BaseT <- sqlQuery(myconn,"SELECT * FROM ana_LR12_data where hitType='Target' and LMR <>'M' and UMD<>'M'");

#LR12cent <-sqlQuery(myconn, "SELECT * FROM ana_LR12_data_byTBLR where hitType='Center' and fromTB<>'M' and fromLR<>'M'");
#LR12BaseT<-LR12Base[LR12Base$HitType=="Target" & LR12Base$LMR!="M" & LR12Base$UMD!="M" ,];
# hitType='Center' and fromTB<>'M' and fromLR<>'M'
#ddply(dt,~grou,psummarise,mean=mean(age),sd=sd(age))


read.excel <- function(header=TRUE,...) {
  read.table("clipboard",sep="\t",header=header,...)
}
LR12targ=read.excel()

#basic averageges, ci, sd, err, etc.
LR12centcX<-summarySE(LR12cent, measurevar="AvgOfTouchOffsetX",groupvars=c("fromTB", "fromLR","TargetSize"))
LR12centcY<-summarySE(LR12cent, measurevar="AvgOfTouchOffsetY",groupvars=c("fromTB", "fromLR","TargetSize"))
LR12centcXd<-summarySE(LR12cent, measurevar="AvgOfTouchDeltaX",groupvars=c("fromTB", "fromLR","TargetSize"))
LR12centcYd<-summarySE(LR12cent, measurevar="AvgOfTouchDeltaY",groupvars=c("fromTB", "fromLR","TargetSize"))

LR12targcX<-summarySE(LR12targ, measurevar="AvgOfTouchOffsetX",groupvars=c("fromTB", "fromLR","TargetSize"))
LR12targcY<-summarySE(LR12targ, measurevar="AvgOfTouchOffsetY",groupvars=c("fromTB", "fromLR","TargetSize"))
LR12targcXd<-summarySE(LR12targ, measurevar="AvgOfTouchDeltaX",groupvars=c("fromTB", "fromLR","TargetSize"))
LR12targcYd<-summarySE(LR12targ, measurevar="AvgOfTouchDeltaY",groupvars=c("fromTB", "fromLR","TargetSize"))



pd <- position_dodge(0.1)
ggplot(LR12centc,aes(x=FromLR,y=AvgOfTouchOffsetX,colour=fromTB))
+geom_errorbar(aes(ymin=AvgOfTouchOffsetX-se,ymax=AvgOfTouchOffsetX+se), width=.2, position=pd)
+geom_line(position=pd) 
+geom_point(position=pd)

cx<-summarySEwithin(LR12cent,y="AvgOfTouchOffsetX", withinvars = c("fromTB", "FromLR", "TargetSize"),idvar = "PID_SN_PID")
ggplot(data=cx, aes(x=FromLR, y=AvgOfTouchOffsetX, group=fromTB, group=TargetSize)) + 
  geom_line()+geom_point() 
cx$TargetSize<-factor(cx$TargetSize)
cx$ts<-as.numeric(as.character(cx$TargetSize))

dodge <- position_dodge(width=0.2)
ggplot(cx, aes(x=FromLR, y=AvgOfTouchOffsetX, shape = interaction(TargetSize, fromTB))) +coord_flip() + geom_point(aes(size=4),position=dodge,) + geom_line(aes(group=interaction(TargetSize, fromTB)),position=dodge)+geom_errorbar(aes(ymax = AvgOfTouchOffsetX + se, ymin=AvgOfTouchOffsetX - se), position=dodge, width=0.25)+ scale_shape(solid = FALSE)

#setup residuals structure
resCCount<-153
residualsC <-data.frame(resCXbias = numeric(resCCount),resCYbias=numeric(resCCount),resCXdelta= numeric(resCCount),resCYdelta= numeric(resCCount))
colnames(residualsC) <- c("resCXbias","resCYbias", "resCXdelta", "resCYdelta")
# analysis ANOVA

as.numeric(levels(cx.TargetSize))[f]

attach(LR12cent)
aov.out = aov(LR12cent$AvgOfTouchOffsetX~LR12cent$TargetSize*LR12cent$fromTB*LR12cent$fromLR+Error(LR12cent$PID_SN_PID/(LR12cent$fromTB*LR12cent$fromLR)))
summary(aov.out)
test.pr<-proj(aov.out)
residualsC[1]<-data.frame(test.pr[[3]][,"Residuals"])
LR12cent[,"resXb"]<-residualsC[1]

aov.out = aov(AvgOfTouchOffsetY~TargetSize*fromTB*fromLR+Error(PID_SN_PID/(fromTB*fromLR)))
#summary(aov.out)
test.pr<-proj(aov.out)
residualsC[2]<-data.frame(test.pr[[3]][,"Residuals"])
LR12cent[,"resYb"]<-residualsC[2]

aov.out = aov(AvgOfTouchDeltaX~TargetSize*fromTB*fromLR+Error(PID_SN_PID/(fromTB*fromLR)))
#summary(aov.out)
test.pr<-proj(aov.out)
residualsC[3]<-data.frame(test.pr[[3]][,"Residuals"])
LR12cent[,"resXd"]<-residualsC[3]

aov.out = aov(AvgOfTouchDeltaY~TargetSize*fromTB*fromLR+Error(PID_SN_PID/(fromTB*fromLR)))
#summary(aov.out)
test.pr<-proj(aov.out)
residualsC[4]<-data.frame(test.pr[[3]][,"Residuals"])
LR12cent[,"resYd"]<-residualsC[4]

detach(LR12cent)

residualsC[5]<-LR12cent$fromTB
residualsC[6]<-LR12cent$FromLR
residualsC[7]<-LR12cent$TargetSize


#plot QQs of the base data
par( mfrow = c( 2, 2 ))
qqnorm(LR12BaseC$TouchOffsetX)
qqline(LR12BaseC$TouchOffsetX)
hist(LR12BaseC$TouchOffsetX)
qqnorm(LR12BaseC$TouchOffsetY)
#;m<-mean(LR12BaseC$TouchOffsetY); std<-sqrt(var(LR12BaseC$TouchOffsetY))
qqline(LR12BaseC$TouchOffsetY)
hist(LR12BaseC$TouchOffsetY)

qqnorm(LR12BaseC$TouchDeltaX)
qqline(LR12BaseC$TouchDeltaX)
hist(LR12BaseC$TouchDeltaX)
qqnorm(LR12BaseC$TouchDeltaY)
#;m<-mean(LR12BaseC$TouchDeltaY); std<-sqrt(var(LR12BaseC$TouchDeltaY))
qqline(LR12BaseC$TouchDeltaY)
hist(LR12BaseC$TouchDeltaY);

require("car")
require("MASS")
par( mfrow = c( 2, 2 ))
 qqp(LR12BaseC$TouchOffsetX,"norm")
 qqp(LR12BaseC$TouchOffsetY,"norm")
 qqp(LR12BaseT$TouchOffsetX,"norm")
 qqp(LR12BaseT$TouchOffsetY,"norm")


#kurtosis
kurtosis(LR12BaseC$TouchOffsetX)
kurtosis(LR12BaseT$TouchOffsetX)
kurtosis(LR12BaseC$TouchOffsetY)
kurtosis(LR12BaseT$TouchOffsetY)
kurtosis(LR12BaseC$TouchDeltaX)
kurtosis(LR12BaseT$TouchDeltaX)
kurtosis(LR12BaseC$TouchDeltaY)
kurtosis(LR12BaseT$TouchDeltaY)

#plot QQs of the base data
par( mfrow = c( 2, 2 ))
qqnorm(LR12BaseT$TouchOffsetX)
qqline(LR12BaseT$TouchOffsetX)
hist(LR12BaseT$TouchOffsetX)
qqnorm(LR12BaseT$TouchOffsetY)
#;m<-mean(LR12BaseC$TouchOffsetY); std<-sqrt(var(LR12BaseC$TouchOffsetY))
qqline(LR12BaseT$TouchOffsetY)
hist(LR12BaseT$TouchOffsetY)

qqnorm(LR12BaseT$TouchDeltaX)
qqline(LR12BaseT$TouchDeltaX)
hist(LR12BaseT$TouchDeltaX)
qqnorm(LR12BaseT$TouchDeltaY)
#;m<-mean(LR12BaseC$TouchDeltaY); std<-sqrt(var(LR12BaseC$TouchDeltaY))
qqline(LR12BaseT$TouchDeltaY)
hist(LR12BaseT$TouchDeltaY);

#kurtosis
kurtosis(LR12BaseT$TouchOffsetX)
kurtosis(LR12BaseT$TouchOffsetY)
kurtosis(LR12BaseT$TouchDeltaX)
kurtosis(LR12BaseT$TouchDeltaY)


#plot QQs of the residuals
par( mfrow = c( 2, 4 ) )
qqnorm(residualsC$resCXbias)
qqline(residualsC$resCXbias)
hist(residualsC$resCXbias)
qqnorm(residualsC$resCYbias)
qqline(residualsC$resCYbias)
hist(residualsC$resCYbias)
qqnorm(residualsC$resCXdelta)
qqline(residualsC$resCXdelta)
hist(residualsC$resCXdelta)
qqnorm(residualsC$resCYdelta)
qqline(residualsC$resCYdelta)
hist(residualsC$resCYdelta)


par( mfrow = c( 2, 2 ) )
qqp(LR12BaseC$AvgOfTouchOffsetX,"norm")
qqp(LR12cent$AvgOfTouchOffsetY,"norm")
qqp(LR12cent$AvgOfTouchDeltaX,"lnorm")
qqp(LR12cent$AvgOfTouchDeltaY,"norm")

shapiro.test(residualsC$resCXbias)
shapiro.test(residualsC$resCYbias)
shapiro.test(residualsC$resCXdelta)
shapiro.test(residualsC$resCYdelta)
resTCount<-204
residualsT <-data.frame(resCXbias = numeric(resTCount),resCYbias=numeric(resTCount),resCXdelta= numeric(resTCount),resCYdelta= numeric(resTCount))
colnames(residualsT) <- c("resTXbias","resTYbias", "resTXdelta", "resTYdelta")


attach(LR12targ)

aov.out = aov(LR12targ$AvgOfTouchOffsetX~LR12targ$TargetSize*LR12targ$UMD*LR12targ$LMR+Error(LR12targ$PID_SN_PID/(LR12targ$UMD*LR12targ$LMR)))
#summary(aov.out)de
test.pr<-proj(aov.out)
residualsT[1]<-data.frame(test.pr[[3]][,"Residuals"])
summary(aov.out)

aov.out = aov(AvgOfTouchOffsetY~TargetSize*UMD*LMR+Error(PID_SN_PID/(UMD*LMR)))
#summary(aov.out)
test.pr<-proj(aov.out)
residualsT[2]<-data.frame(test.pr[[3]][,"Residuals"])

aov.out = aov(AvgOfTouchDeltaX~TargetSize*UMD*LMR+Error(PID_SN_PID/(UMD*LMR)))
#summary(aov.out)
test.pr<-proj(aov.out)
residualsT[3]<-data.frame(test.pr[[3]][,"Residuals"])

aov.out = aov(AvgOfTouchDeltaY~TargetSize*UMD*LMR+Error(PID_SN_PID/(UMD*LMR)))
#summary(aov.out)
test.pr<-proj(aov.out)
residualsT[4]<-data.frame(test.pr[[3]][,"Residuals"])


detach(LR12targ)

par( mfrow = c( 2, 2 ) )
qqnorm(residualsT$resTXbias)
qqline(residualsT$resTXbias)
qqnorm(residualsT$resTYbias)
qqline(residualsT$resTYbias)
qqnorm(residualsT$resTXdelta)
qqline(residualsT$resTXdelta)
qqnorm(residualsT$resTYdelta)
qqline(residualsT$resTYdelta)


shapiro.test(residualsT$resTXbias)
shapiro.test(residualsT$resTYbias)
shapiro.test(residualsT$resTXdelta)
shapiro.test(residualsT$resTYdelta)
par( mfrow = c( 1, 1 ) )

# turns out they are all not normally distributed

# Plotting either targets or centers Further down 
xybar<-join(LR12targcX,LR12targcY,by=c("fromTB","fromLR","TargetSize"))

xybar<-subset(xybar,fromTB!="M" & fromLR!="M")
names(xybar)[5]<-"X";names(xybar)[5]<-"X";
names(xybar)[10]<-"Y";names(xybar)[11]<-"Ysd";names(xybar)[12]<-"Yse"; names(xybar)[13]<-"Yci"
names(xybar)[5:8]<-c("X","Xsd","Xse","Xci")

fromLR<-c("fL","fR","fL","fR");fromTB<-c("fT","fT","fB","fB");Xarr<-(2)*c(-1,1,-1,1);Yarr<-(2)*c(1,1,-1,-1);
xybarArrows<-data.frame(fromLR,fromTB,Xarr,Yarr);
xybar<-join(xybar,xybarArrows,by=c("fromTB","fromLR"));


plot1<-ggplot(data = subset(xybar,TargetSize==20),aes(x = X,y = Y)) + 
  coord_fixed(ratio = 1,xlim = c(-2.5, 5),ylim=c(-5,2.5))+
  geom_point() + 
  geom_errorbar(aes(ymin = Y-Yci,ymax = Y+Yci)) + 
  geom_errorbarh(aes(xmin = X-Xci,xmax = X+Xci))+geom_hline(aes(yintercept=0),colour="black",linetype=2)+geom_vline(aes(yintercept=0),linetype=2)+
  geom_segment(aes(x = X+Xarr, y = Y+Yarr, xend = X, yend = Y), arrow = arrow(length = unit(0.65, "cm")) , color=c("red","blue")[factor(fromLR)] )+
  geom_point(data=cLR12[cLR12$TargetSize==20 & cLR12$hitType=="Target",],aes(x=Xb,y=Yb),shape=1)+ theme_bw()+
  ggtitle( "Touch bias on large target with approach vector")  
;
plot2<-ggplot(data = subset(xybar,TargetSize==6  ),aes(x = X,y = Y)) + 
  coord_fixed(ratio = 1,xlim = c(-2.5, 5),ylim=c(-5,2.5))+
  geom_point() + 
  geom_errorbar(aes(ymin = Y-Yci,ymax = Y+Yci)) + 
  geom_errorbarh(aes(xmin = X-Xci,xmax = X+Xci))+geom_hline(aes(yintercept=0),colour="black",linetype=2)+geom_vline(aes(yintercept=0),linetype=2)+
  geom_segment(aes(x = X+Xarr, y = Y+Yarr, xend = X, yend = Y), arrow = arrow(length = unit(0.65, "cm")), color=c("red","blue")[factor(fromLR)] )+
  geom_point(data=cLR12[cLR12$TargetSize==6 & cLR12$hitType=="Target",],aes(x=Xb,y=Yb),shape=1)+ theme_bw()+
  ggtitle( "Touch bias on small target with approach vector")     ;

grid.arrange(plot1, plot2, nrow=1, ncol=2)

#plot the center part
xybar<-join(LR12centcX,LR12centcY,by=c("fromTB","fromLR","TargetSize"))
xybar<-subset(xybar,fromTB!="M" & fromLR!="M")
names(xybar)[5]<-"X";names(xybar)[5]<-"X";
names(xybar)[10]<-"Y";names(xybar)[11]<-"Ysd";names(xybar)[12]<-"Yse"; names(xybar)[13]<-"Yci"
names(xybar)[5:8]<-c("X","Xsd","Xse","Xci")

fromTB<-c("T","T","B","B");fromLR<-c("L","R","L","R");Xarr<-2*c(-1,1,-1,1);Yarr<-2*c(1,1,-1,-1)
xybarArrows<-data.frame(fromTB,fromLR,Xarr,Yarr);
xybar<-join(xybar,xybarArrows,by=c("fromTB","fromLR"));

plot3<-ggplot(data = subset(xybar,TargetSize==20),aes(x = X,y = Y)) + 
  coord_fixed(ratio = 1,xlim = c(-2.5, 5),ylim=c(-5,2.5))+
  geom_point() + 
  geom_errorbar(aes(ymin = Y-Yci,ymax = Y+Yci)) + 
  geom_errorbarh(aes(xmin = X-Xci,xmax = X+Xci))+
  geom_point(data=cLR12[cLR12$TargetSize==20 & cLR12$hitType=="Center",],aes(x=Xb,y=Yb),shape=1)+ theme_bw() +geom_hline(aes(yintercept=0),colour="black",linetype=2)+geom_vline(aes(yintercept=0),linetype=2)+
  geom_segment(aes(x = X+Xarr, y = Y+Yarr, xend = X, yend = Y), arrow = arrow(length = unit(0.45, "cm")), color=c("red","blue")[factor(fromLR)] )+
  ggtitle( "Touch bias on large center with approach vector")   ;

plot4<-ggplot(data = subset(xybar,TargetSize==6),aes(x = X,y = Y)) + 
  coord_fixed(ratio = 1,xlim = c(-2.5, 5),ylim=c(-5,2.5))+
  geom_point() + 
  geom_errorbar(aes(ymin = Y-Yci,ymax = Y+Yci)) + 
  geom_errorbarh(aes(xmin = X-Xci,xmax = X+Xci))+theme_bw()+ geom_hline(aes(yintercept=0),colour="black",linetype=2)+geom_vline(aes(yintercept=0),linetype=2)+
  geom_point(data=cLR12[cLR12$TargetSize==6 & cLR12$hitType=="Center",],aes(x=Xb,y=Yb),shape=1)+
  geom_segment(aes(x = X+Xarr, y = Y+Yarr, xend = X, yend = Y), arrow = arrow(length = unit(0.45, "cm")), color=c("red","blue")[factor(fromLR)])+
ggtitle( "Touch bias on small center with approach vector") ;
grid.arrange(plot3, plot4, nrow=1, ncol=2)

cLR12<-ddply(LR12Base[LR12Base$DominantHand=="r",],c("hitType", "TargetSize","DominantHand"),summarise,Xb=mean(TouchOffsetX),Xbsd=sd(TouchOffsetX),Yb=mean(TouchOffsetY),Ybsd=sd(TouchOffsetY),N=length(TouchOffsetX))

ggplot(cLR12,aes(x=Xb,y=Yb))+geom_point() + coord_fixed(ratio = 1,xlim = c(-5, 5),ylim=c(-5,5))+ geom_errorbar(aes(ymin = Yb-Ybsd,ymax = Yb+Ybsd)) + geom_errorbarh(aes(xmin = Xb-Xbsd,xmax = Xb+Xbsd))


# analysis on Touchduration's effect on accuracy abd touchDelta_ll
plot(LR12Base$touchDuration,abs(LR12Base$TouchOffsetX))
reglx<-lm(abs(LR12Base$TouchOffsetX)~LR12Base$touchDuration)
par(cex=.8)
plot(LR12Base$touchDuration,abs(LR12Base$TouchOffsetX))
abline(reglx)

regly<-lm(abs(LR12Base$TouchOffsetY)~LR12Base$touchDuration)
par(cex=.8)
plot(LR12Base$touchDuration,abs(LR12Base$TouchOffsetY))
abline(regly)

regldx<-lm(abs(LR12Base$TouchDeltaX)~LR12Base$touchDuration)
par(cex=.8)
plot(LR12Base$touchDuration,abs(LR12Base$TouchDeltaX))
abline(regldx)

regldy<-lm(abs(LR12Base$TouchDeltaY)~LR12Base$touchDuration)
par(cex=.8)
plot(LR12Base$touchDuration,abs(LR12Base$TouchDeltaY))
abline(regldy)

