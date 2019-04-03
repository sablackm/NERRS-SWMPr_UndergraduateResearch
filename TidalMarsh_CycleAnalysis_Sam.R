rm(list=ls(all=TRUE))
library(ggplot2)
library(gridExtra)
library(SWMPr)
library(tidyverse)
library(naniar)

#######################
##### Load Data #######
#######################

path <- "/Users/samuelblackman/Desktop/Research/NERRS/SouthAtlantic"
sitename = 'sapdcwq'
data_collected <- import_local(path, sitename, trace = FALSE)
bh <- qaqc(data_collected)

TidesAll <- bh
#-------------------------------------------------------------------------------------------------------------------------------------------------------------

#Only Max - WORKS!
#Will not have to worry about lower bound, if using LoadingData.R
s2 <- TidesAll[c(3114:385728), c(1,7)]
s2 <- na.omit(s2)

m<-10
f<-rep(1/(2*m+1),(2*m+1))
smoo2<-stats::filter(s2$depth,f,sides=2)

#max<-(which(diff(sign(diff(smoo2)))==-2)+1)

sign_diff <- sign(diff(smoo2))
sdf <- data.frame(sign_diff[1:length(sign_diff)])
sdf[sdf == 0] <- NA
sdf <- na.locf(sdf$sign_diff.1.length.sign_diff.., na.rm = FALSE)

#sign_diff2 <- diff(sign(diff(smoo2)))
sign_diff2 <- diff(sdf)
max <- which(sign_diff2==-2)+1

pt<-sort(max)
# Cycles<-rep(NA,length(s2[,1]))
# TideChange<-rep(NA,length(s2[,1]))
# Tide.cy<-rep(NA,length(s2[,1]))
# Tide.dS<-rep(NA,length(s2[,1]))
# s2<-data.frame(s2,TideChange,Cycles,Tide.cy,Tide.dS)
s2<-data.frame(s2[1:length(sign_diff2),],Cycles[1:length(sign_diff2)], sign_diff[1:length(sign_diff2)], sign_diff2)
s2$Cycles[1:pt[1]]<-1
# z<-length(pt)
# s2<-s2[1:pt[z],]

for (k in 2:length(pt)) {
  s2$Cycles[pt[k-1]:pt[k]]<-k
}

#Clean up End of Dataset:
toRemove <- c()
for (i in (length(s2$Cycles)-100):length(s2$Cycles)) {
  if(is.na(s2$Cycles[i])){
    toRemove <- c(toRemove, i)
  }
}

s2 <- s2[-toRemove,]


#Viewing Depths Versus Filtered Depths

plot(smoo2, type= 'l')
lines(s2$depth, col = 'green')
plot(s2$depth, type = 'l', col = 'green')
lines(smoo2)



y2017 <- TidesAll[TidesAll$datetimestamp>='2017-01-01 00:00:00' & TidesAll$datetimestamp<='2017-12-31 23:45:00',]
y2017 <- y2017[c(1:length(y2017$datetimestamp)), c(1,7)]
y2017 <- na.omit(y2017)

















### Ratios between flood stages
## note: Tide.dS is the change in stage for each ebb or flood (pos on flood, neg on ebb)
# u<-unique(s$Tide.dS)
# u.flood<-u[seq(from=1,to=length(u),by=2)]
# library(zoo)
# u.flood<-data.frame(u.flood,lag(zoo(u.flood),1,na.pad=T))
# rownames(u.flood)<-seq(from=1,to=length(u),by=2)
# fl.ratio<-u.flood[,2]/u.flood[,1]
# fl.ratio<-data.frame(ratio=fl.ratio[1:(length(fl.ratio)-1)],cy1=seq(from=1,to=(length(u)-2),by=2),cy2=seq(from=3,to=length(u),by=2))

### Plot of cycles with smooth and cycle IDs. Dotted line is approximate of marsh platform.
plot(s$Stage,type='l',ylab="Stage (m)",xlab="",main="Jul 24 - Aug 10, 2015")
lines(smoo,col='green')
abline(v=(which(diff(sign(diff(smoo)))==-2)+1),col="gray")
text((which(diff(sign(diff(smoo)))==-2)+1),1.3,labels=as.character(seq(from=1,to=length(u),by=2)))
abline(v=(which(diff(sign(diff(smoo)))==2)+1),col="gray")
abline(h=b,lty='dotted')

### Densities of ebb and flood flows from entire period
# h<-ggplot(s,aes(abs(Flow),fill=Tide))+geom_density(alpha=0.2)+labs(title="Jul 24 - Aug 10",x=bquote('Flow ('*m^3*'/s)'))+theme_bw()+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())	
# h


### Density comparison between different cycles
# r.un<-c(9:12)
r.un<-c(5:12)
# r.ev<-c(39:40)
r.ev<-c(45:46)
# r.ev<-c(27:28,35:36,45:46)
even<-s[!is.na(match(s$Cycles,r.ev)),]
uneven<-s[!is.na(match(s$Cycles,r.un)),]

h<-ggplot(even,aes(abs(Flow),fill=Tide))+geom_density(alpha=0.3)+labs(title="Even Tides",x=bquote('Flow ('*m^3*'/s)'))+theme_bw()+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+scale_fill_manual(values=c("deepskyblue3","gray60"))
h1<-ggplot(uneven,aes(abs(Flow),fill=Tide))+geom_density(alpha=0.3)+labs(title="Uneven Tides",x=bquote('Flow ('*m^3*'/s)'))+theme_bw()+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+scale_fill_manual(values=c("orangered","gray60"))
rect<-even
rect1<-uneven
ts<-ggplot(s,aes(x=DateTime,y=Stage))+geom_line()+theme_bw()+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+geom_rect(data=rect,inherit.aes=F,color="lightgray",fill=NA,aes(xmin=DateTime[1],xmax=DateTime[length(DateTime)],ymin=-Inf,ymax=Inf))+geom_rect(data=rect1,inherit.aes=F,color="lightgray",fill=NA,aes(xmin=DateTime[1],xmax=DateTime[length(DateTime)],ymin=-Inf,ymax=Inf))+labs(x="",y="Stage (m)")+geom_hline(yintercept=b,linetype='dotted')
pushViewport(viewport(layout=grid.layout(3,2)))
print(h1,vp=viewport(layout.pos.row=1:2,layout.pos.col=1))
print(h,vp=viewport(layout.pos.row=1:2,layout.pos.col=2))
print(ts,vp=viewport(layout.pos.row=3,layout.pos.col=1:2))

# h<-ggplot(even,aes(abs(Flow),fill=Tide))+geom_density(alpha=0.2)+labs(title="Even Tides",x=bquote('Flow ('*m^3*'/s)'))	
# # h
# h1<-ggplot(uneven,aes(abs(Flow),fill=Tide))+geom_density(alpha=0.2)+labs(title="Uneven Tides",x=bquote('Flow ('*m^3*'/s)'))
# grid.arrange(h,h1,ncol=2)

