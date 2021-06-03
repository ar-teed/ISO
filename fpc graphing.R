

##########################################
##### Compute group trajectories  ########
##########################################

#Mean standardized observed values or FPC scores by dose and group
y1.hc.placebo = mean(data$y1[data$Group=="Com" & data$Dose=="Placebo"])
fpc11.hc.placebo = mean(data$fpc11[data$Group=="Com" & data$Dose=="Placebo"])
fpc12.hc.placebo = mean(data$fpc12[data$Group=="Com" & data$Dose=="Placebo"])
y1.hc.point5 = mean(data$y1[data$Group=="Com" & data$Dose=="Point5"])
fpc11.hc.point5 = mean(data$fpc11[data$Group=="Com" & data$Dose=="Point5"])
fpc12.hc.point5 = mean(data$fpc12[data$Group=="Com" & data$Dose=="Point5"])
y1.hc.two = mean(data$y1[data$Group=="Com" & data$Dose=="Two"])
fpc11.hc.two = mean(data$fpc11[data$Group=="Com" & data$Dose=="Two"])
fpc12.hc.two = mean(data$fpc12[data$Group=="Com" & data$Dose=="Two"])
y1.gad.placebo = mean(data$y1[data$Group=="GAD" & data$Dose=="Placebo"])
fpc11.gad.placebo = mean(data$fpc11[data$Group=="GAD" & data$Dose=="Placebo"])
fpc12.gad.placebo = mean(data$fpc12[data$Group=="GAD" & data$Dose=="Placebo"])
y1.gad.point5 = mean(data$y1[data$Group=="GAD" & data$Dose=="Point5"])
fpc11.gad.point5 = mean(data$fpc11[data$Group=="GAD" & data$Dose=="Point5"])
fpc12.gad.point5 = mean(data$fpc12[data$Group=="GAD" & data$Dose=="Point5"])
y1.gad.two = mean(data$y1[data$Group=="GAD" & data$Dose=="Two"])
fpc11.gad.two = mean(data$fpc11[data$Group=="GAD" & data$Dose=="Two"])
fpc12.gad.two = mean(data$fpc12[data$Group=="GAD" & data$Dose=="Two"])

#FPC score by dependant variable and dose across groups
fpc11.fit.placebo = mean(data$fpc11[data$Dose=="Placebo"])
fpc11.fit.point5 = mean(data$fpc11[data$Dose=="Point5"])
fpc11.fit.two = mean(data$fpc11[data$Dose=="Two"])
fpc12.fit.placebo = mean(data$fpc12[data$Dose=="Placebo"])
fpc12.fit.point5 = mean(data$fpc12[data$Dose=="Point5"])
fpc12.fit.two = mean(data$fpc12[data$Dose=="Two"])
fpc21.fit.placebo = mean(data$fpc21[data$Dose=="Placebo"])
fpc21.fit.point5 = mean(data$fpc21[data$Dose=="Point5"])
fpc21.fit.two = mean(data$fpc21[data$Dose=="Two"])
fpc22.fit.placebo = mean(data$fpc22[data$Dose=="Placebo"])
fpc22.fit.point5 = mean(data$fpc22[data$Dose=="Point5"])
fpc22.fit.two = mean(data$fpc22[data$Dose=="Two"])
fpc31.fit.placebo = mean(data$fpc31[data$Dose=="Placebo"])
fpc31.fit.point5 = mean(data$fpc31[data$Dose=="Point5"])
fpc31.fit.two = mean(data$fpc31[data$Dose=="Two"])
fpc32.fit.placebo = mean(data$fpc32[data$Dose=="Placebo"])
fpc32.fit.point5 = mean(data$fpc32[data$Dose=="Point5"])
fpc32.fit.two = mean(data$fpc32[data$Dose=="Two"])

#FPC score for each dependant variable by dose by groups
fpc11.hc.placebo = mean(data$fpc11[data$Group=="Com" & data$Dose=="Placebo"])
fpc12.hc.placebo = mean(data$fpc12[data$Group=="Com" & data$Dose=="Placebo"])
fpc11.hc.point5 = mean(data$fpc11[data$Group=="Com" & data$Dose=="Point5"])
fpc12.hc.point5 = mean(data$fpc12[data$Group=="Com" & data$Dose=="Point5"])
fpc11.hc.two = mean(data$fpc11[data$Group=="Com" & data$Dose=="Two"])
fpc12.hc.two = mean(data$fpc12[data$Group=="Com" & data$Dose=="Two"])
fpc11.gad.placebo = mean(data$fpc11[data$Group=="GAD" & data$Dose=="Placebo"])
fpc12.gad.placebo = mean(data$fpc12[data$Group=="GAD" & data$Dose=="Placebo"])
fpc11.gad.point5 = mean(data$fpc11[data$Group=="GAD" & data$Dose=="Point5"])
fpc12.gad.point5 = mean(data$fpc12[data$Group=="GAD" & data$Dose=="Point5"])
fpc11.gad.two = mean(data$fpc11[data$Group=="GAD" & data$Dose=="Two"])
fpc12.gad.two = mean(data$fpc12[data$Group=="GAD" & data$Dose=="Two"])

fpc21.hc.placebo = mean(data$fpc21[data$Group=="Com" & data$Dose=="Placebo"])
fpc22.hc.placebo = mean(data$fpc22[data$Group=="Com" & data$Dose=="Placebo"])
fpc21.hc.point5 = mean(data$fpc21[data$Group=="Com" & data$Dose=="Point5"])
fpc22.hc.point5 = mean(data$fpc22[data$Group=="Com" & data$Dose=="Point5"])
fpc21.hc.two = mean(data$fpc21[data$Group=="Com" & data$Dose=="Two"])
fpc22.hc.two = mean(data$fpc22[data$Group=="Com" & data$Dose=="Two"])
fpc21.gad.placebo = mean(data$fpc21[data$Group=="GAD" & data$Dose=="Placebo"])
fpc22.gad.placebo = mean(data$fpc22[data$Group=="GAD" & data$Dose=="Placebo"])
fpc21.gad.point5 = mean(data$fpc21[data$Group=="GAD" & data$Dose=="Point5"])
fpc22.gad.point5 = mean(data$fpc22[data$Group=="GAD" & data$Dose=="Point5"])
fpc21.gad.two = mean(data$fpc21[data$Group=="GAD" & data$Dose=="Two"])
fpc22.gad.two = mean(data$fpc22[data$Group=="GAD" & data$Dose=="Two"])

fpc31.hc.placebo = mean(data$fpc31[data$Group=="Com" & data$Dose=="Placebo"])
fpc32.hc.placebo = mean(data$fpc32[data$Group=="Com" & data$Dose=="Placebo"])
fpc31.hc.point5 = mean(data$fpc31[data$Group=="Com" & data$Dose=="Point5"])
fpc32.hc.point5 = mean(data$fpc32[data$Group=="Com" & data$Dose=="Point5"])
fpc31.hc.two = mean(data$fpc31[data$Group=="Com" & data$Dose=="Two"])
fpc32.hc.two = mean(data$fpc32[data$Group=="Com" & data$Dose=="Two"])
fpc31.gad.placebo = mean(data$fpc31[data$Group=="GAD" & data$Dose=="Placebo"])
fpc32.gad.placebo = mean(data$fpc32[data$Group=="GAD" & data$Dose=="Placebo"])
fpc31.gad.point5 = mean(data$fpc31[data$Group=="GAD" & data$Dose=="Point5"])
fpc32.gad.point5 = mean(data$fpc32[data$Group=="GAD" & data$Dose=="Point5"])
fpc31.gad.two = mean(data$fpc31[data$Group=="GAD" & data$Dose=="Two"])
fpc32.gad.two = mean(data$fpc32[data$Group=="GAD" & data$Dose=="Two"])

#Overall projected fits for each dependant variable by dose
y3.fit.all.placebo.Mu = Mu[[3]]+fpc31.fit.placebo*FPC_mean[[3]][,1]++fpc32.fit.placebo*FPC_mean[[3]][,2]
y3.fit.all.point5.Mu = Mu[[3]]+fpc31.fit.point5*FPC_mean[[3]][,1]++fpc32.fit.point5*FPC_mean[[3]][,2]
y3.fit.all.two.Mu = Mu[[3]]+fpc31.fit.two*FPC_mean[[3]][,1]++fpc32.fit.two*FPC_mean[[3]][,2]

y2.fit.all.placebo.Mu = Mu[[2]]+fpc21.fit.placebo*FPC_mean[[2]][,1]++fpc22.fit.placebo*FPC_mean[[2]][,2]
y2.fit.all.point5.Mu = Mu[[2]]+fpc21.fit.point5*FPC_mean[[2]][,1]++fpc22.fit.point5*FPC_mean[[2]][,2]
y2.fit.all.two.Mu = Mu[[2]]+fpc21.fit.two*FPC_mean[[2]][,1]++fpc22.fit.two*FPC_mean[[2]][,2]

y1.fit.all.placebo.Mu = Mu[[1]]+fpc11.fit.placebo*FPC_mean[[1]][,1]++fpc12.fit.placebo*FPC_mean[[1]][,2]
y1.fit.all.point5.Mu = Mu[[1]]+fpc11.fit.point5*FPC_mean[[1]][,1]++fpc12.fit.point5*FPC_mean[[1]][,2]
y1.fit.all.two.Mu = Mu[[1]]+fpc11.fit.two*FPC_mean[[1]][,1]++fpc12.fit.two*FPC_mean[[1]][,2]

#Overall projected fits for each dependant variable by dose and group
y1.fit.hc.placebo.Mu = Mu[[1]]+fpc11.hc.placebo*FPC_mean[[1]][,1]++fpc12.hc.placebo*FPC_mean[[1]][,2]
y1.fit.hc.point5.Mu = Mu[[1]]+fpc11.hc.point5*FPC_mean[[1]][,1]++fpc12.hc.point5*FPC_mean[[1]][,2]
y1.fit.hc.two.Mu = Mu[[1]]+fpc11.hc.two*FPC_mean[[1]][,1]++fpc12.hc.two*FPC_mean[[1]][,2]
y1.fit.gad.placebo.Mu = Mu[[1]]+fpc11.gad.placebo*FPC_mean[[1]][,1]++fpc12.gad.placebo*FPC_mean[[1]][,2]
y1.fit.gad.point5.Mu = Mu[[1]]+fpc11.gad.point5*FPC_mean[[1]][,1]++fpc12.gad.point5*FPC_mean[[1]][,2]
y1.fit.gad.two.Mu = Mu[[1]]+fpc11.gad.two*FPC_mean[[1]][,1]++fpc12.gad.two*FPC_mean[[1]][,2]

y2.fit.hc.placebo.Mu = Mu[[2]]+fpc21.hc.placebo*FPC_mean[[2]][,1]++fpc22.hc.placebo*FPC_mean[[2]][,2]
y2.fit.hc.point5.Mu = Mu[[2]]+fpc21.hc.point5*FPC_mean[[2]][,1]++fpc22.hc.point5*FPC_mean[[2]][,2]
y2.fit.hc.two.Mu = Mu[[2]]+fpc21.hc.two*FPC_mean[[2]][,1]++fpc22.hc.two*FPC_mean[[2]][,2]
y2.fit.gad.placebo.Mu = Mu[[2]]+fpc21.gad.placebo*FPC_mean[[2]][,1]++fpc22.gad.placebo*FPC_mean[[2]][,2]
y2.fit.gad.point5.Mu = Mu[[2]]+fpc21.gad.point5*FPC_mean[[2]][,1]++fpc22.gad.point5*FPC_mean[[2]][,2]
y2.fit.gad.two.Mu = Mu[[2]]+fpc21.gad.two*FPC_mean[[2]][,1]++fpc22.gad.two*FPC_mean[[2]][,2]

y3.fit.hc.placebo.Mu = Mu[[3]]+fpc31.hc.placebo*FPC_mean[[3]][,1]++fpc32.hc.placebo*FPC_mean[[3]][,2]
y3.fit.hc.point5.Mu = Mu[[3]]+fpc31.hc.point5*FPC_mean[[3]][,1]++fpc32.hc.point5*FPC_mean[[3]][,2]
y3.fit.hc.two.Mu = Mu[[3]]+fpc31.hc.two*FPC_mean[[3]][,1]++fpc32.hc.two*FPC_mean[[3]][,2]
y3.fit.gad.placebo.Mu = Mu[[3]]+fpc31.gad.placebo*FPC_mean[[3]][,1]++fpc32.gad.placebo*FPC_mean[[3]][,2]
y3.fit.gad.point5.Mu = Mu[[3]]+fpc31.gad.point5*FPC_mean[[3]][,1]++fpc32.gad.point5*FPC_mean[[3]][,2]
y3.fit.gad.two.Mu = Mu[[3]]+fpc31.gad.two*FPC_mean[[3]][,1]++fpc32.gad.two*FPC_mean[[3]][,2]

#Projected fits for each BOLD % signal change FPC by dose
y1.fpc1.all.placebo.Mu = Mu[[1]]+fpc11.fit.placebo*FPC_mean[[1]][,1]
y1.fpc1.all.point5.Mu = Mu[[1]]+fpc11.fit.point5*FPC_mean[[1]][,1]
y1.fpc1.all.two.Mu = Mu[[1]]+fpc11.fit.two*FPC_mean[[1]][,1]
y1.fpc2.all.placebo.Mu = Mu[[1]]+fpc12.fit.placebo*FPC_mean[[1]][,2]
y1.fpc2.all.point5.Mu = Mu[[1]]+fpc12.fit.point5*FPC_mean[[1]][,2]
y1.fpc2.all.two.Mu = Mu[[1]]+fpc12.fit.two*FPC_mean[[1]][,2]

#Projected fits for each dependant variable and FPC by dose
y1.fpc1.hc.placebo.Mu = Mu[[1]]+fpc11.hc.placebo*FPC_mean[[1]][,1]
y1.fpc1.hc.point5.Mu = Mu[[1]]+fpc11.hc.point5*FPC_mean[[1]][,1]
y1.fpc1.hc.two.Mu = Mu[[1]]+fpc11.hc.two*FPC_mean[[1]][,1]
y1.fpc1.gad.placebo.Mu = Mu[[1]]+fpc11.gad.placebo*FPC_mean[[1]][,1]
y1.fpc1.gad.point5.Mu = Mu[[1]]+fpc11.gad.point5*FPC_mean[[1]][,1]
y1.fpc1.gad.two.Mu = Mu[[1]]+fpc11.gad.two*FPC_mean[[1]][,1]

y2.fpc1.hc.placebo.Mu = Mu[[2]]+fpc21.hc.placebo*FPC_mean[[2]][,1]
y2.fpc1.hc.point5.Mu = Mu[[2]]+fpc21.hc.point5*FPC_mean[[2]][,1]
y2.fpc1.hc.two.Mu = Mu[[2]]+fpc21.hc.two*FPC_mean[[2]][,1]
y2.fpc1.gad.placebo.Mu = Mu[[2]]+fpc21.gad.placebo*FPC_mean[[2]][,1]
y2.fpc1.gad.point5.Mu = Mu[[2]]+fpc21.gad.point5*FPC_mean[[2]][,1]
y2.fpc1.gad.two.Mu = Mu[[2]]+fpc21.gad.two*FPC_mean[[2]][,1]

y3.fpc1.hc.placebo.Mu = Mu[[3]]+fpc31.hc.placebo*FPC_mean[[3]][,1]
y3.fpc1.hc.point5.Mu = Mu[[3]]+fpc31.hc.point5*FPC_mean[[3]][,1]
y3.fpc1.hc.two.Mu = Mu[[3]]+fpc31.hc.two*FPC_mean[[3]][,1]
y3.fpc1.gad.placebo.Mu = Mu[[3]]+fpc31.gad.placebo*FPC_mean[[3]][,1]
y3.fpc1.gad.point5.Mu = Mu[[3]]+fpc31.gad.point5*FPC_mean[[3]][,1]
y3.fpc1.gad.two.Mu = Mu[[3]]+fpc31.gad.two*FPC_mean[[3]][,1]


y1.fpc2.hc.placebo.Mu = Mu[[1]]+fpc12.hc.placebo*FPC_mean[[1]][,2]
y1.fpc2.hc.point5.Mu = Mu[[1]]+fpc12.hc.point5*FPC_mean[[1]][,2]
y1.fpc2.hc.two.Mu = Mu[[1]]+fpc12.hc.two*FPC_mean[[1]][,2]
y1.fpc2.gad.placebo.Mu = Mu[[1]]+fpc12.gad.placebo*FPC_mean[[1]][,2]
y1.fpc2.gad.point5.Mu = Mu[[1]]+fpc12.gad.point5*FPC_mean[[1]][,2]
y1.fpc2.gad.two.Mu = Mu[[1]]+fpc12.gad.two*FPC_mean[[1]][,2]

y2.fpc2.hc.placebo.Mu = Mu[[2]]+fpc22.hc.placebo*FPC_mean[[2]][,2]
y2.fpc2.hc.point5.Mu = Mu[[2]]+fpc22.hc.point5*FPC_mean[[2]][,2]
y2.fpc2.hc.two.Mu = Mu[[2]]+fpc22.hc.two*FPC_mean[[2]][,2]
y2.fpc2.gad.placebo.Mu = Mu[[2]]+fpc22.gad.placebo*FPC_mean[[2]][,2]
y2.fpc2.gad.point5.Mu = Mu[[2]]+fpc22.gad.point5*FPC_mean[[2]][,2]
y2.fpc2.gad.two.Mu = Mu[[2]]+fpc22.gad.two*FPC_mean[[2]][,2]

y3.fpc2.hc.placebo.Mu = Mu[[3]]+fpc32.hc.placebo*FPC_mean[[3]][,2]
y3.fpc2.hc.point5.Mu = Mu[[3]]+fpc32.hc.point5*FPC_mean[[3]][,2]
y3.fpc2.hc.two.Mu = Mu[[3]]+fpc32.hc.two*FPC_mean[[3]][,2]
y3.fpc2.gad.placebo.Mu = Mu[[3]]+fpc32.gad.placebo*FPC_mean[[3]][,2]
y3.fpc2.gad.point5.Mu = Mu[[3]]+fpc32.gad.point5*FPC_mean[[3]][,2]
y3.fpc2.gad.two.Mu = Mu[[3]]+fpc32.gad.two*FPC_mean[[3]][,2]

#Projection for each dependant variable and FPC by dose but without projecting on mean standardized observed values
y1.fpc1.all.placebo = fpc11.fit.placebo*FPC_mean[[1]][,1]
y1.fpc1.all.point5 = fpc11.fit.point5*FPC_mean[[1]][,1]
y1.fpc1.all.two = fpc11.fit.two*FPC_mean[[1]][,1]
y1.fpc2.all.placebo = fpc12.fit.placebo*FPC_mean[[1]][,2]
y1.fpc2.all.point5 = fpc12.fit.point5*FPC_mean[[1]][,2]
y1.fpc2.all.two = fpc12.fit.two*FPC_mean[[1]][,2]

y1.fpc1.hc.placebo = fpc11.hc.placebo*FPC_mean[[1]][,1]
y1.fpc1.hc.point5 = fpc11.hc.point5*FPC_mean[[1]][,1]
y1.fpc1.hc.two = fpc11.hc.two*FPC_mean[[1]][,1]
y1.fpc1.gad.placebo = fpc11.gad.placebo*FPC_mean[[1]][,1]
y1.fpc1.gad.point5 = fpc11.gad.point5*FPC_mean[[1]][,1]
y1.fpc1.gad.two = fpc11.gad.two*FPC_mean[[1]][,1]

y2.fpc1.hc.placebo = fpc21.hc.placebo*FPC_mean[[2]][,1]
y2.fpc1.hc.point5 = fpc21.hc.point5*FPC_mean[[2]][,1]
y2.fpc1.hc.two = fpc21.hc.two*FPC_mean[[2]][,1]
y2.fpc1.gad.placebo = fpc21.gad.placebo*FPC_mean[[2]][,1]
y2.fpc1.gad.point5 = fpc21.gad.point5*FPC_mean[[2]][,1]
y2.fpc1.gad.two = fpc21.gad.two*FPC_mean[[2]][,1]

y3.fpc1.hc.placebo = fpc31.hc.placebo*FPC_mean[[3]][,1]
y3.fpc1.hc.point5 = fpc31.hc.point5*FPC_mean[[3]][,1]
y3.fpc1.hc.two = fpc31.hc.two*FPC_mean[[3]][,1]
y3.fpc1.gad.placebo = fpc31.gad.placebo*FPC_mean[[3]][,1]
y3.fpc1.gad.point5 = fpc31.gad.point5*FPC_mean[[3]][,1]
y3.fpc1.gad.two = fpc31.gad.two*FPC_mean[[3]][,1]


y1.fpc2.hc.placebo = fpc12.hc.placebo*FPC_mean[[1]][,2]
y1.fpc2.hc.point5 = fpc12.hc.point5*FPC_mean[[1]][,2]
y1.fpc2.hc.two = fpc12.hc.two*FPC_mean[[1]][,2]
y1.fpc2.gad.placebo = fpc12.gad.placebo*FPC_mean[[1]][,2]
y1.fpc2.gad.point5 = fpc12.gad.point5*FPC_mean[[1]][,2]
y1.fpc2.gad.two = fpc12.gad.two*FPC_mean[[1]][,2]

y2.fpc2.hc.placebo = fpc22.hc.placebo*FPC_mean[[2]][,2]
y2.fpc2.hc.point5 = fpc22.hc.point5*FPC_mean[[2]][,2]
y2.fpc2.hc.two = fpc22.hc.two*FPC_mean[[2]][,2]
y2.fpc2.gad.placebo = fpc22.gad.placebo*FPC_mean[[2]][,2]
y2.fpc2.gad.point5 = fpc22.gad.point5*FPC_mean[[2]][,2]
y2.fpc2.gad.two = fpc22.gad.two*FPC_mean[[2]][,2]

y3.fpc2.hc.placebo = fpc32.hc.placebo*FPC_mean[[3]][,2]
y3.fpc2.hc.point5 = fpc32.hc.point5*FPC_mean[[3]][,2]
y3.fpc2.hc.two = fpc32.hc.two*FPC_mean[[3]][,2]
y3.fpc2.gad.placebo = fpc32.gad.placebo*FPC_mean[[3]][,2]
y3.fpc2.gad.point5 = fpc32.gad.point5*FPC_mean[[3]][,2]
y3.fpc2.gad.two = fpc32.gad.two*FPC_mean[[3]][,2]


##########################################
##### Plot grp and dose effects   ########
##########################################

#Function for graphing projected fpc fits on participants standardized measurements 
spaghetti <- function(y,times,ids,ylab="y",xlab="times",
			xlim=c(min(times,na.rm=T),max(times,na.rm=T)),
			ylim=c(min(y,na.rm=T),max(y,na.rm=T)),
			cols=rep(1,length(unique(ids))),lwd=1){
	plot(times,y,type="n",las=1,cex.axis=1.75,cex.lab = 1.3,xlim=xlim,ylim=ylim,xlab=xlab,ylab=ylab)
	for(i in 1:length(unique(ids))){
		if(length(y[ids==unique(ids)[i]])==1) lines(times[ids==unique(ids)[i]],
			y[ids==unique(ids)[i]],type="p",col=cols[i],pch=15,cex=.25)
		if(length(y[ids==unique(ids)[i]])>1) lines(times[ids==unique(ids)[i]],
			y[ids==unique(ids)[i]],type="l",col=cols[i],lwd=lwd)
	}
}


#Plot for the 1st principal component by group of the Self ROI with one panel for each ISO dose
tiff("Self_Group_fpc1_fig.tiff", units="mm", width=250, height=100, res=400)
#Defining plot size parameters
par(mfrow=c(1,3),mgp=c(4.5,1,0),mar=c(4,4.9,1.8,0.15))
plot(time_cont,y1.fpc1.hc.placebo.Mu, type="l", col="#339900FF", las=1, ann=FALSE, lty=1, lwd=3, xlab="", ylab="", cex.axis=1.65, cex.lab=1.75, ylim=c(-1.5,1.5))
mtext(side = 1, text = "Time (Standardized)", cex = 1.25, line = 2.8)
mtext(side = 2, text = "% Signal Change", cex = 1.25, line = 3.2)
lines(time_cont,y1.fpc1.gad.placebo.Mu, type="l", col="#6666FFFF", lty=1, lwd=2)
#legend("top",legend=c("HC_FPC1","GAD_FPC1"), col=c(4,3),cex=1.25 ,lty=c(1))
title(main="Group Fits: Saline",cex.main=1.7)

plot(time_cont,y1.fpc1.hc.point5.Mu, type="l", col="#339900FF", las=1, lty=1, lwd=3, xlab="", ylab="", cex.axis=1.65, cex.lab=1.75, ylim=c(-1.5,1.5))
mtext(side = 1, text = "Time (Standardized)", cex = 1.25, line = 2.8)
mtext(side = 2, text = "% Signal Change", cex = 1.25, line = 3.2)
lines(time_cont,y1.fpc1.gad.point5.Mu, type="l", col="#6666FFFF", lty=1, lwd=2)
legend("top",legend=c("HC_FPC1","GAD_FPC1"), col=c("#339900FF", "#6666FFFF"),cex=1.25, lwd=3, lty=c(1))
title(main="Group Fits: 0.5 μg",cex.main=1.7)

plot(time_cont,y1.fpc1.hc.two.Mu, type="l", col="#339900FF", las=1, lty=1, lwd=3, xlab="", ylab="", cex.axis=1.65,  cex.lab=1.75, ylim=c(-1.5,1.5))
mtext(side = 1, text = "Time (Standardized)", cex = 1.25, line = 2.8)
mtext(side = 2, text = "% Signal Change", cex = 1.25, line = 3.2)
lines(time_cont,y1.fpc1.gad.two.Mu, type="l", col="#6666FFFF", lty=1, lwd=2)
#legend("top",legend=c("HC_FPC1","GAD_FPC1"), col=c(4,3),cex=1.25 ,lty=c(1))
title(main="Group Fits: 2.0 μg",cex.main=1.7)
dev.off()

#Plot projecting the overall Self ROI FPC fit across groups onto the standardized with one panel for each ISO dose
tiff("Self_Spaghetti_fig.tiff", units="mm", width=250, height=100, res=400)
par(mfrow=c(1,3),mar=c(2,2.3,1.9,0.3))
spaghetti(y = data$y1[data$Dose=="Placebo"], 
	time = data$time[data$Dose=="Placebo"],
	ids = data$id[data$Dose=="Placebo"], xlab=NULL, ylab="% Signal Change", ylim=c(-5,5))
lines(time_cont,y1.fit.all.placebo, type="l", lwd=3, col=7)
title(main = "BOLD fpc Fit: Saline",cex.main=1.6)
spaghetti(y = data$y1[data$Dose=="Point5"], 
	time = data$time[data$Dose=="Point5"],
	ids = data$id[data$Dose=="Point5"], xlab=NULL, ylim=c(-5,5))
lines(time_cont,y1.fit.all.point5, type="l", lwd=3, col=7)
legend("top",legend=c("FPC Fit"), col=c(7),cex=1.25 ,lty=c(1))
title(main = "BOLD fpc Fit: 0.5 μg",cex.main=1.6)
spaghetti(y = data$y1[data$Dose=="Two"], 
	time = data$time[data$Dose=="Two"],
	ids = data$id[data$Dose=="Two"], xlab=NULL, ylim=c(-5,5))
lines(time_cont,y1.fit.all.two, type="l", lwd=3, col=7)
title(main = "BOLD fpc Fit: 2.0 μg",cex.main=1.6)
dev.off()


tiff("Self_Group_fpc1_fig.tiff", units="mm", width=250, height=100, res=400)
par(mfrow=c(1,3),mgp=c(4.5,1,0),mar=c(2,3.3,1.8,0.2))
plot(time_cont,y1.fpc1.hc.placebo.Mu, type="l", col="#339900FF", las=1, lty=1, lwd=3, xlab="Time (Standardized)", ylab="% Signal Change", cex.axis=1.65, cex.lab=1.75, ylim=c(-1.5,1.5))
lines(time_cont,y1.fpc1.gad.placebo.Mu, type="l", col="#6666FFFF", lty=1, lwd=2)
#legend("top",legend=c("HC_FPC1","GAD_FPC1"), col=c(4,3),cex=1.25 ,lty=c(1))
title(main="Group Fits: Saline",cex.main=1.65)

plot(time_cont,y1.fpc1.hc.point5.Mu, type="l", col="#339900FF", las=1, lty=1, lwd=3, xlab="Time (Standardized)", ylab="", cex.axis=1.65, cex.lab=1.75, ylim=c(-1.5,1.5))
lines(time_cont,y1.fpc1.gad.point5.Mu, type="l", col="#6666FFFF", lty=1, lwd=2)
legend("top",legend=c("HC_FPC1","GAD_FPC1"), col=c("#339900FF", "#6666FFFF"),cex=1.25, lwd=3, lty=c(1))
title(main="Group Fits: 0.5 μg",cex.main=1.65)

plot(time_cont,y1.fpc1.hc.two.Mu, type="l", col="#339900FF", las=1, lty=1, lwd=3, xlab="Time (Standardized)", ylab="", cex.axis=1.65,  cex.lab=1.75, ylim=c(-1.5,1.5))
lines(time_cont,y1.fpc1.gad.two.Mu, type="l", col="#6666FFFF", lty=1, lwd=2)
#legend("top",legend=c("HC_FPC1","GAD_FPC1"), col=c(4,3),cex=1.25 ,lty=c(1))
title(main="Group Fits: 2.0 μg",cex.main=1.65)
dev.off()


tiff("Self_Group_noMu_fig.tiff", units="mm", width=180, height=100, res=400)
par(mfrow=c(1,3),mgp=c(4.5,1,0),mar=c(2,3.3,1.8,0.2))
plot(time_cont,y1.fpc1.hc.placebo, type="l", col="#339900FF", las=1, lty=1, lwd=3, xlab="Time (Standardized)", ylab="% Signal Change", cex.axis=1.4, cex.lab=1.75, ylim=c(-1.5,1.5))
lines(time_cont,y1.fpc1.gad.placebo, type="l", col="#6666FFFF", lty=1, lwd=2)
#legend("top",legend=c("HC_FPC1","GAD_FPC1"), col=c(4,3),cex=1.25 ,lty=c(1))
title(main="BOLD fpc1: Saline",cex.main=1.65)

plot(time_cont,y1.fpc1.hc.point5, type="l", col="#339900FF", las=1, lty=1, lwd=3, xlab="Time (Standardized)", ylab="", cex.axis=1.4, cex.lab=1.75, ylim=c(-1.5,1.5))
lines(time_cont,y1.fpc1.gad.point5, type="l", col="#6666FFFF", lty=1, lwd=2)
legend("top",legend=c("HC_FPC1","GAD_FPC1"), col=c("#339900FF", "#6666FFFF"),cex=1.25, lwd=3, lty=c(1))
title(main="BOLD fpc1: 0.5 μg",cex.main=1.65)

plot(time_cont,y1.fpc1.hc.two, type="l", col="#339900FF", las=1, lty=1, lwd=3, xlab="Time (Standardized)", ylab="", cex.axis=1.4,  cex.lab=1.75, ylim=c(-1.5,1.5))
lines(time_cont,y1.fpc1.gad.two, type="l", col="#6666FFFF", lty=1, lwd=2)
#legend("top",legend=c("HC_FPC1","GAD_FPC1"), col=c(4,3),cex=1.25 ,lty=c(1))
title(main="BOLD fpc1: 2.0 μg",cex.main=1.65)
dev.off()

tiff("HR1_Group_noMu_fig.tiff", units="mm", width=180, height=100, res=400)
par(mfrow=c(1,3),mgp=c(4.5,1,0),mar=c(2,3.3,1.8,0.2))
plot(time_cont,y2.fpc1.hc.placebo, type="l", col="#339900FF", las=1, lty=1, lwd=3, xlab="Time (Standardized)", ylab="% Signal Change", cex.axis=1.4, cex.lab=1.75, ylim=c(-1,1.5))
lines(time_cont,y2.fpc1.gad.placebo, type="l", col="#6666FFFF", lty=1, lwd=2)
#legend("top",legend=c("HC_FPC1","GAD_FPC1"), col=c(4,3),cex=1.25 ,lty=c(1))
title(main="HR fpc1: Saline",cex.main=1.65)

plot(time_cont,y2.fpc1.hc.point5, type="l", col="#339900FF", las=1, lty=1, lwd=3, xlab="Time (Standardized)", ylab="", cex.axis=1.4, cex.lab=1.75, ylim=c(-1,1.5))
lines(time_cont,y2.fpc1.gad.point5, type="l", col="#6666FFFF", lty=1, lwd=2)
legend("top",legend=c("HC_FPC1","GAD_FPC1"), col=c("#339900FF", "#6666FFFF"),cex=1.25, lwd=3, lty=c(1))
title(main="HR fpc1: 0.5 μg",cex.main=1.65)

plot(time_cont,y2.fpc1.hc.two, type="l", col="#339900FF", las=1, lty=1, lwd=3, xlab="Time (Standardized)", ylab="", cex.axis=1.4,  cex.lab=1.75, ylim=c(-1,1.5))
lines(time_cont,y2.fpc1.gad.two, type="l", col="#6666FFFF", lty=1, lwd=2)
#legend("top",legend=c("HC_FPC1","GAD_FPC1"), col=c(4,3),cex=1.25 ,lty=c(1))
title(main="HR fpc1: 2.0 μg",cex.main=1.65)
dev.off()

tiff("HR2_Group_noMu_fig.tiff", units="mm", width=180, height=100, res=400)
par(mfrow=c(1,3),mgp=c(4.5,1,0),mar=c(2,3.3,1.8,0.2))
plot(time_cont,y2.fpc2.hc.placebo, type="l", col="#339900FF", las=1, lty=1, lwd=3, xlab="Time (Standardized)", ylab="% Signal Change", cex.axis=1.4, cex.lab=1.75, ylim=c(-1,1.5))
lines(time_cont,y2.fpc2.gad.placebo, type="l", col="#6666FFFF", lty=1, lwd=2)
#legend("top",legend=c("HC_FPC1","GAD_FPC1"), col=c(4,3),cex=1.25 ,lty=c(1))
title(main="HR fpc2: Saline",cex.main=1.65)

plot(time_cont,y2.fpc2.hc.point5, type="l", col="#339900FF", las=1, lty=1, lwd=3, xlab="Time (Standardized)", ylab="", cex.axis=1.4, cex.lab=1.75, ylim=c(-1,1.5))
lines(time_cont,y2.fpc2.gad.point5, type="l", col="#6666FFFF", lty=1, lwd=2)
legend("top",legend=c("HC_FPC1","GAD_FPC1"), col=c("#339900FF", "#6666FFFF"),cex=1.25, lwd=3, lty=c(1))
title(main="HR fpc2: 0.5 μg",cex.main=1.65)

plot(time_cont,y2.fpc2.hc.two, type="l", col="#339900FF", las=1, lty=1, lwd=3, xlab="Time (Standardized)", ylab="", cex.axis=1.4,  cex.lab=1.75, ylim=c(-1,1.5))
lines(time_cont,y2.fpc2.gad.two, type="l", col="#6666FFFF", lty=1, lwd=2)
#legend("top",legend=c("HC_FPC1","GAD_FPC1"), col=c(4,3),cex=1.25 ,lty=c(1))
title(main="HR fpc2: 2.0 μg",cex.main=1.65)
dev.off()

tiff("Dial_Group_noMu_fig.tiff", units="mm", width=180, height=100, res=400)
par(mfrow=c(1,3),mgp=c(4.5,1,0),mar=c(2,3.3,1.8,0.2))
plot(time_cont,y3.fpc1.hc.placebo, type="l", col="#339900FF", las=1, lty=1, lwd=3, xlab="Time (Standardized)", ylab="% Signal Change", cex.axis=1.4, cex.lab=1.75, ylim=c(-1,1.5))
lines(time_cont,y3.fpc1.gad.placebo, type="l", col="#6666FFFF", lty=1, lwd=2)
#legend("top",legend=c("HC_FPC1","GAD_FPC1"), col=c(4,3),cex=1.25 ,lty=c(1))
title(main="Dial fpc1: Saline",cex.main=1.65)

plot(time_cont,y3.fpc1.hc.point5, type="l", col="#339900FF", las=1, lty=1, lwd=3, xlab="Time (Standardized)", ylab="", cex.axis=1.4, cex.lab=1.75, ylim=c(-1,1.5))
lines(time_cont,y3.fpc1.gad.point5, type="l", col="#6666FFFF", lty=1, lwd=2)
legend("top",legend=c("HC_FPC1","GAD_FPC1"), col=c("#339900FF", "#6666FFFF"),cex=1.25, lwd=3, lty=c(1))
title(main="Dial fpc1: 0.5 μg",cex.main=1.65)

plot(time_cont,y3.fpc1.hc.two, type="l", col="#339900FF", las=1, lty=1, lwd=3, xlab="Time (Standardized)", ylab="", cex.axis=1.4,  cex.lab=1.75, ylim=c(-1,1.5))
lines(time_cont,y3.fpc1.gad.two, type="l", col="#6666FFFF", lty=1, lwd=2)
#legend("top",legend=c("HC_FPC1","GAD_FPC1"), col=c(4,3),cex=1.25 ,lty=c(1))
title(main="Dial fpc1: 2.0 μg",cex.main=1.65)
dev.off()

tiff("Dial_Group_fig.tiff", units="mm", width=180, height=100, res=400)
par(mfrow=c(1,3),mgp=c(4.5,1,0),mar=c(2,3.3,1.8,0.2))
plot(time_cont,y3.fpc1.hc.placebo, type="l", col="#339900FF", las=1, lty=1, lwd=3, xlab="Time (Standardized)", ylab="% Signal Change", cex.axis=1.4, cex.lab=1.75, ylim=c(-1.5,1.5))
lines(time_cont,y3.fpc1.gad.placebo, type="l", col="#6666FFFF", lty=1, lwd=2)
#legend("top",legend=c("HC_FPC1","GAD_FPC1"), col=c(4,3),cex=1.25 ,lty=c(1))
title(main="Dial fpc1: Saline",cex.main=1.65)

plot(time_cont,y3.fpc1.hc.point5, type="l", col="#339900FF", las=1, lty=1, lwd=3, xlab="Time (Standardized)", ylab="", cex.axis=1.4, cex.lab=1.75, ylim=c(-1.5,1.5))
lines(time_cont,y3.fpc1.gad.point5, type="l", col="#6666FFFF", lty=1, lwd=2)
legend("top",legend=c("HC_FPC1","GAD_FPC1"), col=c("#339900FF", "#6666FFFF"),cex=1.25, lwd=3, lty=c(1))
title(main="Dial fpc1: 0.5 μg",cex.main=1.65)

plot(time_cont,y3.fpc1.hc.two, type="l", col="#339900FF", las=1, lty=1, lwd=3, xlab="Time (Standardized)", ylab="", cex.axis=1.4,  cex.lab=1.75, ylim=c(-1.5,1.5))
lines(time_cont,y3.fpc1.gad.two, type="l", col="#6666FFFF", lty=1, lwd=2)
#legend("top",legend=c("HC_FPC1","GAD_FPC1"), col=c(4,3),cex=1.25 ,lty=c(1))
title(main="Dial fpc1: 2.0 μg",cex.main=1.65)
dev.off()

data$Group=="Com" & data$Dose=="Placebo"

tiff("HR_fpc2_explanation.tiff", units="mm", width=180, height=100, res=400)
par(mfrow=c(1,3),mar=c(2,2.3,1.9,0.5))
spaghetti(y = data$y2[data$Group=="Com" & data$Dose=="Placebo"],
	time = data$time[data$Group=="Com" & data$Dose=="Placebo"],
	ids = data$id[data$Group=="Com" & data$Dose=="Placebo"], xlab=NULL, ylab="% Signal Change", ylim=c(-5,5))
lines(time_cont,y2.fit.hc.placebo, type="l", lwd=3, col=7)
#legend("top",legend=c("FPC Fit"), col=c(7),cex=1.3 ,lty=c(1))
title(main = "HR FPC2 HC: Saline",cex.main=1.6)
spaghetti(y = data$y2[data$Dose=="Placebo"], 
          time = data$time[data$Dose=="Placebo"],
          ids = data$id[data$Dose=="Placebo"], xlab=NULL, ylim=c(-5,5))
lines(time_cont,Mu[[2]], type="l", lwd=3, col=7)
#legend("top",legend=c("FPC Fit"), col=c(7),cex=1.3 ,lty=c(1))
title(main = "HR Mu: 2.0 μg",cex.main=1.6)
spaghetti(y = data$y2[data$Dose=="Placebo"], 
	time = data$time[data$Dose=="Placebo"],
	ids = data$id[data$Dose=="Placebo"], xlab=NULL, ylim=c(-5,5))
lines(time_cont,FPC_mean[[2]][,2], type="l", lwd=3, col=7)
#legend("top",legend=c("FPC Fit"), col=c(7),cex=1.3 ,lty=c(1))
title(main = "HR FPC2 Mean: 2.0 μg",cex.main=1.6)
dev.off()

tiff("HR_Spaghetti_fig.tiff", units="mm", width=180, height=100, res=400)
par(mfrow=c(1,3),mar=c(2,2.3,1.9,0.5))
spaghetti(y = data$y2[data$Dose=="Placebo"], 
	time = data$time[data$Dose=="Placebo"],
	ids = data$id[data$Dose=="Placebo"], xlab=NULL, ylab="% Signal Change", ylim=c(-5,5))
lines(time_cont,y2.fit.all.placebo, type="l", lwd=3, col=7)
#legend("top",legend=c("FPC Fit"), col=c(7),cex=1.3 ,lty=c(1))
title(main = "HR FPC Fits: Saline",cex.main=1.6)
spaghetti(y = data$y2[data$Dose=="Point5"], 
	time = data$time[data$Dose=="Point5"],
	ids = data$id[data$Dose=="Point5"], xlab=NULL, ylim=c(-5,5))
lines(time_cont,y2.fit.all.point5, type="l", lwd=3, col=7)
legend("top",legend=c("FPC Fit"), col=c(7),cex=1.3 ,lty=c(1))
title(main = "HR FPC Fits: 0.5 μg",cex.main=1.6)
spaghetti(y = data$y2[data$Dose=="Two"], 
	time = data$time[data$Dose=="Two"],
	ids = data$id[data$Dose=="Two"], xlab=NULL, ylim=c(-5,5))
lines(time_cont,y2.fit.all.two, type="l", lwd=3, col=7)
#legend("top",legend=c("FPC Fit"), col=c(7),cex=1.3 ,lty=c(1))
title(main = "HR FPC Fits: 2.0 μg",cex.main=1.6)
dev.off()


tiff("Dial_Spaghetti_fig.tiff", units="mm", width=180, height=100, res=400)
par(mfrow=c(1,3),mar=c(2,2.3,1.9,0.3))
spaghetti(y = data$y3[data$Dose=="Placebo"], 
	time = data$time[data$Dose=="Placebo"],
	ids = data$id[data$Dose=="Placebo"], xlab=NULL, ylab="% Signal Change", ylim=c(-1,6))
lines(time_cont,y3.fit.all.placebo, type="l", lwd=3, col=7)
#legend("top",legend=c("FPC Fit"), col=c(7),cex=1.3 ,lty=c(1))
title(main = "Dial FPC Fits: Saline",cex.main=1.6)
spaghetti(y = data$y3[data$Dose=="Point5"], 
	time = data$time[data$Dose=="Point5"],
	ids = data$id[data$Dose=="Point5"], xlab=NULL, ylim=c(-1,6))
lines(time_cont,y3.fit.all.point5, type="l", lwd=3, col=7)
legend("top",legend=c("FPC Fit"), col=c(7),cex=1.3 ,lty=c(1))
title(main = "Dial FPC Fits: 0.5 μg",cex.main=1.6)
spaghetti(y = data$y3[data$Dose=="Two"], 
	time = data$time[data$Dose=="Two"],
	ids = data$id[data$Dose=="Two"], xlab=NULL, ylim=c(-1,6))
lines(time_cont,y3.fit.all.two, type="l", lwd=3, col=7)
#legend("top",legend=c("FPC Fit"), col=c(7),cex=1.3 ,lty=c(1))
title(main = "Dial FPC Fits: 2.0 μg",cex.main=1.6)
dev.off()




par(mfrow=c(1,3),mar=c(2,2.5,2,0.5))
plot(time_cont,y1.fpc1.hc.placebo, type="l", col=4, las=1, lty=1, lwd=2, xlab="Time (Scaled)", ylab="% Signal Change", cex.axis=1.75, cex.lab=1.5, ylim=c(-1.5,1.5))
lines(time_cont,y1.fpc1.gad.placebo, type="l", col=3, lty=1, lwd=2)
#legend("top",legend=c("HC_FPC1","GAD_FPC1"), col=c(4,3),cex=1.3 ,lty=c(1))
title(main="Group Fits: Saline",cex.main=2)

plot(time_cont,y1.fpc1.hc.point5, type="l", col=4, las=1, lty=1, lwd=2, xlab="Time (Scaled)", ylab=NULL, cex.axis=1.75, cex.lab=1.5, ylim=c(-1.5,1.5))
lines(time_cont,y1.fpc1.gad.point5, type="l", col=3, lty=1, lwd=2)
legend("top",legend=c("HC_FPC1","GAD_FPC1"), col=c(4,3),cex=1.3 ,lty=c(1))
title(main="Group Fits: 0.5 mcg",cex.main=2)

plot(time_cont,y1.fpc1.hc.two, type="l", col=4, las=1, lty=1, lwd=2, xlab="Time (Scaled)", ylab=NULL, cex.axis=1.75,  cex.lab=1.5, ylim=c(-1.5,1.5))
lines(time_cont,y1.fpc1.gad.two, type="l", col=3, lty=1, lwd=2)
#legend("top",legend=c("HC_FPC1","GAD_FPC1"), col=c(4,3),cex=1.3 ,lty=c(1))
title(main="Group Fits: 2.0 mcg",cex.main=2)


par(mfrow=c(1,3),mar=c(2,2,1,0.5))
plot(time_cont,y1.fit.hc.placebo, type="l", col=1, lty=1, ylim=c(-2,2))
lines(time_cont,y1.fit.hc.point5, type="l", col=2, lty=1)
lines(time_cont,y1.fit.hc.two, type="l", col=3, lty=1)
lines(time_cont,y1.fit.gad.placebo, type="l", col=1, lty=2)
lines(time_cont,y1.fit.gad.point5, type="l", col=2, lty=2)
lines(time_cont,y1.fit.gad.two, type="l", col=3, lty=2)
legend(0.2,1.6,legend=c("HC/Saline","HC/Point5","HC/Two","GAD/Saline","GAD/Point5","GAD/Two"), col=c(1:3,1:3),lty=c(1,1,1,2,2,2))
title(main="Valence vmPFC ROI: FPC Fits")

plot(time_cont,y2.fit.hc.placebo, type="l", col=1, lty=1, ylim=c(-2,2))
lines(time_cont,y2.fit.hc.point5, type="l", col=2, lty=1)
lines(time_cont,y2.fit.hc.two, type="l", col=3, lty=1)
lines(time_cont,y2.fit.gad.placebo, type="l", col=1, lty=2)
lines(time_cont,y2.fit.gad.point5, type="l", col=2, lty=2)
lines(time_cont,y2.fit.gad.two, type="l", col=3, lty=2)
legend("bottomright",legend=c("HC/Saline","HC/Point5","HC/Two","GAD/Saline","GAD/Point5","GAD/Two"), col=c(1:3,1:3),lty=c(1,1,1,2,2,2))
title(main="HR FPC Fits")

plot(time_cont,y3.fit.hc.placebo, type="l", col=1, lty=1, ylim=c(-2,2))
lines(time_cont,y3.fit.hc.point5, type="l", col=2, lty=1)
lines(time_cont,y3.fit.hc.two, type="l", col=3, lty=1)
lines(time_cont,y3.fit.gad.placebo, type="l", col=1, lty=2)
lines(time_cont,y3.fit.gad.point5, type="l", col=2, lty=2)
lines(time_cont,y3.fit.gad.two, type="l", col=3, lty=2)
legend("bottomright",legend=c("HC/Saline","HC/Point5","HC/Two","GAD/Saline","GAD/Point5","GAD/Two"), col=c(1:3,1:3),lty=c(1,1,1,2,2,2))
title(main="Dial FPC Fits")

par(mfrow=c(2,3),mar=c(2,2,2,0.5))
spaghetti(y = data$y2[data$Dose=="Placebo"], 
	time = data$time[data$Dose=="Placebo"],
	ids = data$id[data$Dose=="Placebo"], xlab=NULL, ylab="% Signal Change", ylim=c(-5,5))
lines(time_cont,y2.fit.all.placebo, type="l", lwd=3, col=7)
legend("top",legend=c("FPC Fit"), col=c(7),lty=c(1))
title(main = "A)     Heart Rate Fit: Saline")

spaghetti(y = data$y2[data$Dose=="Point5"], 
	time = data$time[data$Dose=="Point5"],
	ids = data$id[data$Dose=="Point5"], xlab=NULL, ylim=c(-5,5))
lines(time_cont,y2.fit.all.point5, type="l", lwd=3, col=7)
legend("top",legend=c("FPC Fit"), col=c(7),lty=c(1))
title(main = "B)     Heart Rate Fit: 0.5 mcg")
spaghetti(y = data$y2[data$Dose=="Two"], 
	time = data$time[data$Dose=="Two"],
	ids = data$id[data$Dose=="Two"], xlab=NULL, ylim=c(-5,5))
lines(time_cont,y2.fit.all.two, type="l", lwd=3, col=7)
legend("top",legend=c("FPC Fit"), col=c(7),lty=c(1))
title(main = "C)     Heart Rate Fit: 2.0 mcg")

plot(time_cont,y2.fpc1.hc.placebo, type="l", col=4, lty=1, lwd=2, xlab="Time (Scaled)", ylab="% Signal Change", cex.lab=1.5, ylim=c(-1.5,1.5))
lines(time_cont,y2.fpc1.gad.placebo, type="l", col=3, lty=1, lwd=2)
lines(time_cont,y2.fpc2.hc.placebo, type="l", col=4, lty=2, lwd=2)
lines(time_cont,y2.fpc2.gad.placebo, type="l", col=3, lty=2, lwd=2)
legend("bottom",legend=c("HC_FPC1","GAD_FPC1","HC_FPC2","GAD_FPC2"), col=c(4,3,4,3),lty=c(1,1,2,2), cex=0.85)
title(main="D)     Heart Rate FPC: Saline")

plot(time_cont,y2.fpc1.hc.point5, type="l", col=4, lty=1, lwd=2, xlab="Time (Scaled)", ylab=NULL, cex.lab=1.5, ylim=c(-1.5,1.5))
lines(time_cont,y2.fpc1.gad.point5, type="l", col=3, lty=1, lwd=2)
lines(time_cont,y2.fpc2.hc.point5, type="l", col=4, lty=2, lwd=2)
lines(time_cont,y2.fpc2.gad.point5, type="l", col=3, lty=2, lwd=2)
legend("bottom",legend=c("HC_FPC1","GAD_FPC1","HC_FPC2","GAD_FPC2"), col=c(4,3,4,3),lty=c(1,1,2,2), cex=0.85)
title(main="E)     Heart Rate FPC: 0.5 mcg")

plot(time_cont,y2.fpc1.hc.two, type="l", col=4, lty=1, lwd=2, xlab="Time (Scaled)", ylab=NULL, cex.lab=1.5, ylim=c(-1.5,1.5))
lines(time_cont,y2.fpc1.gad.two, type="l", col=3, lty=1, lwd=2)
lines(time_cont,y2.fpc2.hc.two, type="l", col=4, lty=2, lwd=2)
lines(time_cont,y2.fpc2.gad.two, type="l", col=3, lty=2, lwd=2)
legend("bottom",legend=c("HC_FPC1","GAD_FPC1","HC_FPC2","GAD_FPC2"), col=c(4,3,4,3),lty=c(1,1,2,2), cex=0.85)
title(main="F)     Heart Rate FPC: 2.0 mcg")

par(mfrow=c(1,3),mar=c(2,2,2,0.5))
plot(time_cont,y2.fpc1.hc.placebo, type="l", col=4, lty=1, lwd=2, xlab="Time (Scaled)", ylab="% Signal Change", cex.axis=1.75, cex.lab=1.5, ylim=c(-1.5,1.5))
lines(time_cont,y2.fpc1.gad.placebo, type="l", col=3, lty=1, lwd=2)
lines(time_cont,y2.fpc2.hc.placebo, type="l", col=4, lty=2, lwd=2)
lines(time_cont,y2.fpc2.gad.placebo, type="l", col=3, lty=2, lwd=2)
title(main="Saline",cex.main=2)

plot(time_cont,y2.fpc1.hc.point5, type="l", col=4, lty=1, lwd=2, xlab="Time (Scaled)", ylab=NULL, cex.axis=1.75, cex.lab=1.5, ylim=c(-1.5,1.5))
lines(time_cont,y2.fpc1.gad.point5, type="l", col=3, lty=1, lwd=2)
lines(time_cont,y2.fpc2.hc.point5, type="l", col=4, lty=2, lwd=2)
lines(time_cont,y2.fpc2.gad.point5, type="l", col=3, lty=2, lwd=2)
title(main="0.5 mcg",cex.main=2)

plot(time_cont,y2.fpc1.hc.two, type="l", col=4, lty=1, lwd=2, xlab="Time (Scaled)", ylab=NULL, cex.axis=1.75, cex.lab=1.5, ylim=c(-1.5,1.5))
lines(time_cont,y2.fpc1.gad.two, type="l", col=3, lty=1, lwd=2)
lines(time_cont,y2.fpc2.hc.two, type="l", col=4, lty=2, lwd=2)
lines(time_cont,y2.fpc2.gad.two, type="l", col=3, lty=2, lwd=2)
title(main="2.0 mcg",cex.main=2)


par(mfrow=c(2,3),mar=c(2,2,2,0.5))
spaghetti(y = data$y3[data$Dose=="Placebo"], 
	time = data$time[data$Dose=="Placebo"],
	ids = data$id[data$Dose=="Placebo"], xlab=NULL, ylab="% Signal Change", ylim=c(-3,6))
lines(time_cont,y3.fit.all.placebo, type="l", lwd=3, col=7)
legend("bottom",legend=c("FPC Fit"), col=c(7),lty=c(1))
title(main = "A)    Dial Rating Fit: Saline")

spaghetti(y = data$y3[data$Dose=="Point5"], 
	time = data$time[data$Dose=="Point5"],
	ids = data$id[data$Dose=="Point5"], xlab=NULL, ylim=c(-3,6))
lines(time_cont,y3.fit.all.point5, type="l", lwd=3, col=7)
legend("bottom",legend=c("FPC Fit"), col=c(7),lty=c(1))
title(main = "B)    Dial Rating Fit: 0.5 mcg")
spaghetti(y = data$y3[data$Dose=="Two"], 
	time = data$time[data$Dose=="Two"],
	ids = data$id[data$Dose=="Two"], xlab=NULL, ylim=c(-3,6))
lines(time_cont,y3.fit.all.two, type="l", lwd=3, col=7)
legend("bottom",legend=c("FPC Fit"), col=c(7),lty=c(1))
title(main = "C)    Dial Rating Fit: 2.0 mcg")

plot(time_cont,y3.fpc1.hc.placebo, type="l", col=4, lty=1, lwd=2, xlab="Time (Scaled)", ylab="% Signal Change", cex.lab=1.5, ylim=c(-2,2))
lines(time_cont,y3.fpc1.gad.placebo, type="l", col=3, lty=1, lwd=2)
lines(time_cont,y3.fpc2.hc.placebo, type="l", col=4, lty=2, lwd=2)
lines(time_cont,y3.fpc2.gad.placebo, type="l", col=3, lty=2, lwd=2)
legend("bottom",legend=c("HC_FPC1","GAD_FPC1","HC_FPC2","GAD_FPC2"), col=c(4,3,4,3),lty=c(1,1,2,2), cex=0.85)
title(main="D)    Dial Rating FPC: Saline")

plot(time_cont,y3.fpc1.hc.point5, type="l", col=4, lty=1, lwd=2, xlab="Time (Scaled)", ylab=NULL, cex.lab=1.5, ylim=c(-2,2))
lines(time_cont,y3.fpc1.gad.point5, type="l", col=3, lty=1, lwd=2)
lines(time_cont,y3.fpc2.hc.point5, type="l", col=4, lty=2, lwd=2)
lines(time_cont,y3.fpc2.gad.point5, type="l", col=3, lty=2, lwd=2)
legend("bottom",legend=c("HC_FPC1","GAD_FPC1","HC_FPC2","GAD_FPC2"), col=c(4,3,4,3),lty=c(1,1,2,2), cex=0.85)
title(main="E)    Dial Rating FPC: 0.5 mcg")

plot(time_cont,y3.fpc1.hc.two, type="l", col=4, lty=1, lwd=2, xlab="Time (Scaled)", ylab=NULL, cex.lab=1.5, ylim=c(-2,2))
lines(time_cont,y3.fpc1.gad.two, type="l", col=3, lty=1, lwd=2)
lines(time_cont,y3.fpc2.hc.two, type="l", col=4, lty=2, lwd=2)
lines(time_cont,y3.fpc2.gad.two, type="l", col=3, lty=2, lwd=2)
legend("bottom",legend=c("HC_FPC1","GAD_FPC1","HC_FPC2","GAD_FPC2"), col=c(4,3,4,3),lty=c(1,1,2,2), cex=0.85)
title(main="F)    Dial Rating FPC: 2.0 mcg")

par(mfrow=c(1,3),mar=c(2,2,2,0.5))
plot(time_cont,y3.fpc1.hc.placebo, type="l", col=4, lty=1, lwd=2, xlab="Time (Scaled)", ylab="% Signal Change", cex.axis=1.75, cex.lab=1.5, ylim=c(-1.5,2))
lines(time_cont,y3.fpc1.gad.placebo, type="l", col=3, lty=1, lwd=2)
lines(time_cont,y3.fpc2.hc.placebo, type="l", col=4, lty=2, lwd=2)
lines(time_cont,y3.fpc2.gad.placebo, type="l", col=3, lty=2, lwd=2)
title(main="Saline",cex.main=2)

plot(time_cont,y3.fpc1.hc.point5, type="l", col=4, lty=1, lwd=2, xlab="Time (Scaled)", ylab=NULL, cex.axis=1.75, cex.lab=1.5, ylim=c(-1.5,2))
lines(time_cont,y3.fpc1.gad.point5, type="l", col=3, lty=1, lwd=2)
lines(time_cont,y3.fpc2.hc.point5, type="l", col=4, lty=2, lwd=2)
lines(time_cont,y3.fpc2.gad.point5, type="l", col=3, lty=2, lwd=2)
title(main="0.5 mcg",cex.main=2)

plot(time_cont,y3.fpc1.hc.two, type="l", col=4, lty=1, lwd=2, xlab="Time (Scaled)", ylab=NULL, cex.axis=1.75, cex.lab=1.5, ylim=c(-1.5,2))
lines(time_cont,y3.fpc1.gad.two, type="l", col=3, lty=1, lwd=2)
lines(time_cont,y3.fpc2.hc.two, type="l", col=4, lty=2, lwd=2)
lines(time_cont,y3.fpc2.gad.two, type="l", col=3, lty=2, lwd=2)
title(main="2.0 mcg",cex.main=2)

par(mfrow=c(1,3),mar=c(2,2,2,0.5))
spaghetti(y = data$y1[data$Dose=="Placebo"], 
	time = data$time[data$Dose=="Placebo"],
	ids = data$id[data$Dose=="Placebo"], xlab=NULL, ylab="% Signal Change", ylim=c(-5,5))
lines(time_cont,y1.fit.all.placebo, type="l", lwd=3, col=7)
legend("top",legend=c("FPC Fit"), col=c(7),lty=c(1))
title(main = "A)           Self vmPFC ROI: Saline")

spaghetti(y = data$y1[data$Dose=="Point5"], 
	time = data$time[data$Dose=="Point5"],
	ids = data$id[data$Dose=="Point5"], xlab=NULL, ylim=c(-5,5))
lines(time_cont,y1.fit.all.point5, type="l", lwd=3, col=7)
legend("top",legend=c("FPC Fit"), col=c(7),lty=c(1))
title(main = "B)            Self vmPFC ROI: 0.5 mcg")
spaghetti(y = data$y1[data$Dose=="Two"], 
	time = data$time[data$Dose=="Two"],
	ids = data$id[data$Dose=="Two"], xlab=NULL, ylim=c(-5,5))
lines(time_cont,y1.fit.all.two, type="l", lwd=3, col=7)
legend("top",legend=c("FPC Fit"), col=c(7),lty=c(1))
title(main = "C)           Self vmPFC ROI: 2.0 mcg")

plot(time_cont,y1.fpc1.hc.placebo, type="l", col=4, lty=1, lwd=2, xlab="Time (Scaled)", ylab="% Signal Change", cex.lab=1.5, ylim=c(-1.5,1.5))
lines(time_cont,y1.fpc1.gad.placebo, type="l", col=3, lty=1, lwd=2)
lines(time_cont,y1.fpc2.hc.placebo, type="l", col=4, lty=2, lwd=2)
lines(time_cont,y1.fpc2.gad.placebo, type="l", col=3, lty=2, lwd=2)
legend(0.2,1.6,legend=c("HC_FPC1","GAD_FPC1","HC_FPC2","GAD_FPC2"), col=c(4,3,4,3),lty=c(1,1,2,2), cex=0.85)
title(main="D)           Self vmPFC ROI: Saline")

plot(time_cont,y1.fpc1.hc.point5, type="l", col=4, lty=1, lwd=2, xlab="Time (Scaled)", ylab=NULL, cex.lab=1.5, ylim=c(-1.5,1.5))
lines(time_cont,y1.fpc1.gad.point5, type="l", col=3, lty=1, lwd=2)
lines(time_cont,y1.fpc2.hc.point5, type="l", col=4, lty=2, lwd=2)
lines(time_cont,y1.fpc2.gad.point5, type="l", col=3, lty=2, lwd=2)
legend(0.2,1.6,legend=c("HC_FPC1","GAD_FPC1","HC_FPC2","GAD_FPC2"), col=c(4,3,4,3),lty=c(1,1,2,2), cex=0.85)
title(main="E)           Self vmPFC ROI: 0.5 mcg")

plot(time_cont,y1.fpc1.hc.two, type="l", col=4, lty=1, lwd=2, xlab="Time (Scaled)", ylab=NULL, cex.lab=1.5, ylim=c(-1.5,1.5))
lines(time_cont,y1.fpc1.gad.two, type="l", col=3, lty=1, lwd=2)
lines(time_cont,y1.fpc2.hc.two, type="l", col=4, lty=2, lwd=2)
lines(time_cont,y1.fpc2.gad.two, type="l", col=3, lty=2, lwd=2)
legend(0.2,1.6,legend=c("HC_FPC1","GAD_FPC1","HC_FPC2","GAD_FPC2"), col=c(4,3,4,3),lty=c(1,1,2,2), cex=0.85)
title(main="F)           Self vmPFC ROI: 2.0 mcg")

par(mfrow=c(1,3),mar=c(2,2,2,0.5))
plot(time_cont,y1.fpc1.hc.placebo, type="l", col=4, lty=1, lwd=2, xlab="Time (Scaled)", ylab="% Signal Change", cex.axis=1.75, cex.lab=1.5, ylim=c(-1.5,1.5))
lines(time_cont,y1.fpc1.gad.placebo, type="l", col=3, lty=1, lwd=2)
lines(time_cont,y1.fpc2.hc.placebo, type="l", col=4, lty=2, lwd=2)
lines(time_cont,y1.fpc2.gad.placebo, type="l", col=3, lty=2, lwd=2)
title(main="Saline",cex.main=2)

plot(time_cont,y1.fpc1.hc.point5, type="l", col=4, lty=1, lwd=2, xlab="Time (Scaled)", ylab=NULL, cex.axis=1.75, cex.lab=1.5, ylim=c(-1.5,1.5))
lines(time_cont,y1.fpc1.gad.point5, type="l", col=3, lty=1, lwd=2)
lines(time_cont,y1.fpc2.hc.point5, type="l", col=4, lty=2, lwd=2)
lines(time_cont,y1.fpc2.gad.point5, type="l", col=3, lty=2, lwd=2)
title(main="0.5 mcg",cex.main=2)

plot(time_cont,y1.fpc1.hc.two, type="l", col=4, lty=1, lwd=2, xlab="Time (Scaled)", ylab=NULL, cex.axis=1.75,  cex.lab=1.5, ylim=c(-1.5,1.5))
lines(time_cont,y1.fpc1.gad.two, type="l", col=3, lty=1, lwd=2)
lines(time_cont,y1.fpc2.hc.two, type="l", col=4, lty=2, lwd=2)
lines(time_cont,y1.fpc2.gad.two, type="l", col=3, lty=2, lwd=2)
title(main="2.0 mcg",cex.main=2)


par(mfrow=c(1,3),mar=c(2,2.5,2,0.5))
plot(time_cont,y1.fpc1.hc.placebo, type="l", col=4, lty=1, lwd=2, xlab="Time (Scaled)", ylab="% Signal Change", cex.axis=1.75, cex.lab=1.5, ylim=c(-1.5,1.5))
lines(time_cont,y1.fpc1.gad.placebo, type="l", col=3, lty=1, lwd=2)
title(main="Saline",cex.main=2)

plot(time_cont,y1.fpc1.hc.point5, type="l", col=4, lty=1, lwd=2, xlab="Time (Scaled)", ylab=NULL, cex.axis=1.75, cex.lab=1.5, ylim=c(-1.5,1.5))
lines(time_cont,y1.fpc1.gad.point5, type="l", col=3, lty=1, lwd=2)
title(main="0.5 mcg",cex.main=2)

plot(time_cont,y1.fpc1.hc.two, type="l", col=4, lty=1, lwd=2, xlab="Time (Scaled)", ylab=NULL, cex.axis=1.75,  cex.lab=1.5, ylim=c(-1.5,1.5))
lines(time_cont,y1.fpc1.gad.two, type="l", col=3, lty=1, lwd=2)
title(main="2.0 mcg",cex.main=2)


par(mfrow=c(1,3),mar=c(2,2.5,2,0.5))
plot(time_cont,y2.fpc1.hc.placebo, type="l", col=4, lty=1, lwd=2, xlab="Time (Scaled)", ylab="% Signal Change", cex.axis=1.75, cex.lab=1.5, ylim=c(-1.5,1.5))
lines(time_cont,y2.fpc1.gad.placebo, type="l", col=3, lty=1, lwd=2)
title(main="Saline",cex.main=2)

plot(time_cont,y2.fpc1.hc.point5, type="l", col=4, lty=1, lwd=2, xlab="Time (Scaled)", ylab=NULL, cex.axis=1.75, cex.lab=1.5, ylim=c(-1.5,1.5))
lines(time_cont,y2.fpc1.gad.point5, type="l", col=3, lty=1, lwd=2)
title(main="0.5 mcg",cex.main=2)

plot(time_cont,y2.fpc1.hc.two, type="l", col=4, lty=1, lwd=2, xlab="Time (Scaled)", ylab=NULL, cex.axis=1.75, cex.lab=1.5, ylim=c(-1.5,1.5))
lines(time_cont,y2.fpc1.gad.two, type="l", col=3, lty=1, lwd=2)
title(main="2.0 mcg",cex.main=2)

par(mfrow=c(1,3),mar=c(2,2.5,2,0.5))
plot(time_cont,y3.fpc1.hc.placebo, type="l", col=4, lty=1, lwd=2, xlab="Time (Scaled)", ylab="% Signal Change", cex.axis=1.75, cex.lab=1.5, ylim=c(-1.5,2))
lines(time_cont,y3.fpc1.gad.placebo, type="l", col=3, lty=1, lwd=2)
title(main="Saline",cex.main=2)

plot(time_cont,y3.fpc1.hc.point5, type="l", col=4, lty=1, lwd=2, xlab="Time (Scaled)", ylab=NULL, cex.axis=1.75, cex.lab=1.5, ylim=c(-1.5,2))
lines(time_cont,y3.fpc1.gad.point5, type="l", col=3, lty=1, lwd=2)
title(main="0.5 mcg",cex.main=2)

plot(time_cont,y3.fpc1.hc.two, type="l", col=4, lty=1, lwd=2, xlab="Time (Scaled)", ylab=NULL, cex.axis=1.75, cex.lab=1.5, ylim=c(-1.5,2))
lines(time_cont,y3.fpc1.gad.two, type="l", col=3, lty=1, lwd=2)
title(main="2.0 mcg",cex.main=2)



par(mfrow=c(1,3),mar=c(2,2,2,0.5))
plot(time_cont,y1.fpc1.hc.placebo, type="l", col=4, lty=1, lwd=2, xlab="Time (Scaled)", ylab="% Signal Change", cex.lab=1.5, ylim=c(-1.5,1.5))
lines(time_cont,y1.fpc1.gad.placebo, type="l", col=3, lty=1, lwd=2)
lines(time_cont,y1.fpc2.hc.placebo, type="l", col=4, lty=2, lwd=2)
lines(time_cont,y1.fpc2.gad.placebo, type="l", col=3, lty=2, lwd=2)
legend(0.2,1.6,legend=c("HC_FPC1","GAD_FPC1","HC_FPC2","GAD_FPC2"), col=c(4,3,4,3),lty=c(1,1,2,2))
title(main="A)   Valence vmPFC ROI: Saline")

plot(time_cont,y1.fpc1.hc.point5, type="l", col=4, lty=1, lwd=2, xlab="Time (Scaled)", ylab=NULL, cex.lab=1.5, ylim=c(-1.5,1.5))
lines(time_cont,y1.fpc1.gad.point5, type="l", col=3, lty=1, lwd=2)
lines(time_cont,y1.fpc2.hc.point5, type="l", col=4, lty=2, lwd=2)
lines(time_cont,y1.fpc2.gad.point5, type="l", col=3, lty=2, lwd=2)
legend(0.2,1.6,legend=c("HC_FPC1","GAD_FPC1","HC_FPC2","GAD_FPC2"), col=c(4,3,4,3),lty=c(1,1,2,2))
title(main="B)   Valence vmPFC ROI: 0.5 mcg")

plot(time_cont,y1.fpc1.hc.two, type="l", col=4, lty=1, lwd=2, xlab="Time (Scaled)", ylab=NULL, cex.lab=1.5, ylim=c(-1.5,1.5))
lines(time_cont,y1.fpc1.gad.two, type="l", col=3, lty=1, lwd=2)
lines(time_cont,y1.fpc2.hc.two, type="l", col=4, lty=2, lwd=2)
lines(time_cont,y1.fpc2.gad.two, type="l", col=3, lty=2, lwd=2)
legend(0.2,1.6,legend=c("HC_FPC1","GAD_FPC1","HC_FPC2","GAD_FPC2"), col=c(4,3,4,3),lty=c(1,1,2,2))
title(main="C)   Valence vmPFC ROI: 2.0 mcg")




par(mfrow=c(2,3))
spaghetti(y = data$y1[data$Dose=="Placebo"], 
          time = data$time[data$Dose=="Placebo"],
          ids = data$id[data$Dose=="Placebo"], ylim=c(-5,5))
lines(time_cont,y1.fit.all.placebo, type="l", lwd=3, col=7)
legend("top",legend=c("Fpc Fit"), col=c(7),lty=c(1))
title(main = "A)       Valence vmPFC ROI: Fit - Saline", ylab="% Signal Change")

spaghetti(y = data$y1[data$Dose=="Point5"], 
          time = data$time[data$Dose=="Point5"],
          ids = data$id[data$Dose=="Point5"], ylim=c(-5,5))
lines(time_cont,y1.fit.all.point5, type="l", lwd=3, col=7)
legend("top",legend=c("Fpc Fit"), col=c(7),lty=c(1))
title(main = "B)       Valence vmPFC ROI: Fit - 0.5 mcg")
spaghetti(y = data$y1[data$Dose=="Two"], 
          time = data$time[data$Dose=="Two"],
          ids = data$id[data$Dose=="Two"], ylim=c(-5,5))
lines(time_cont,y1.fit.all.two, type="l", lwd=3, col=7)
legend("top",legend=c("Fpc Fit"), col=c(7),lty=c(1))
title(main = "C)       Valence vmPFC ROI: Fit - 2.0 mcg")

plot(time_cont,y1.fpc1.hc.placebo, type="l", col=4, lty=1, lwd=2, ylim=c(-1.5,1.5))
lines(time_cont,y1.fpc2.hc.placebo, type="l", col=4, lty=2, lwd=2)
lines(time_cont,y1.fpc1.gad.placebo, type="l", col=3, lty=1, lwd=2)
lines(time_cont,y1.fpc2.gad.placebo, type="l", col=3, lty=2, lwd=2)
legend(0.2,1.6,legend=c("HC_Fpc1","HC_Fpc2","GAD_Fpc1","GAD_Fpc2"), col=c(4,4,3,3),lty=c(1,2,1,2))
title(main="D)       Valence vmPFC ROI: FPC Saline", xlab="Time (Scaled)", ylab="% Signal Change")

plot(time_cont,y1.fpc1.hc.point5, type="l", col=4, lty=1, lwd=2, ylim=c(-1.5,1.5))
lines(time_cont,y1.fpc2.hc.point5, type="l", col=4, lty=2, lwd=2)
lines(time_cont,y1.fpc1.gad.point5, type="l", col=3, lty=1, lwd=2)
lines(time_cont,y1.fpc2.gad.point5, type="l", col=3, lty=2, lwd=2)
legend(0.2,1.6,legend=c("HC_Fpc1","HC_Fpc2","GAD_Fpc1","GAD_Fpc2"), col=c(4,4,3,3),lty=c(1,2,1,2))
title(main="E)       Valence vmPFC ROI: FPC 0.5 mcg", xlab="Time (Scaled)")

plot(time_cont,y1.fpc1.hc.two, type="l", col=4, lty=1, lwd=2, ylim=c(-1.5,1.5))
lines(time_cont,y1.fpc2.hc.two, type="l", col=4, lty=2, lwd=2)
lines(time_cont,y1.fpc1.gad.two, type="l", col=3, lty=1, lwd=2)
lines(time_cont,y1.fpc2.gad.two, type="l", col=3, lty=2, lwd=2)
legend(0.2,1.6,legend=c("HC_Fpc1","HC_Fpc2","GAD_Fpc1","GAD_Fpc2"), col=c(4,4,3,3),lty=c(1,2,1,2))
title(main="F)       Valence vmPFC ROI: FPC 2.0 mcg", xlab="Time (Scaled)")

par(mfrow=c(2,3))
spaghetti(y = data$y1[data$Group=="Com" & data$Dose=="Placebo"], 
	time = data$time[data$Group=="Com" & data$Dose=="Placebo"],
	ids = data$id[data$Group=="Com" & data$Dose=="Placebo"], ylim=c(-5,5))
lines(time_cont,y1.fit.hc.placebo, type="l", lwd=3, col=2)
title(main = "Valence, HC, Saline")
spaghetti(y = data$y1[data$Group=="Com" & data$Dose=="Point5"], 
	time = data$time[data$Group=="Com" & data$Dose=="Point5"],
	ids = data$id[data$Group=="Com" & data$Dose=="Point5"], ylim=c(-5,5))
lines(time_cont,y1.fit.hc.point5, type="l", lwd=3, col=2)
title(main = "Valence, HC, Point5")
spaghetti(y = data$y1[data$Group=="Com" & data$Dose=="Two"], 
	time = data$time[data$Group=="Com" & data$Dose=="Two"],
	ids = data$id[data$Group=="Com" & data$Dose=="Two"], ylim=c(-5,5))
lines(time_cont,y1.fit.hc.two, type="l", lwd=3, col=2)
title(main = "Valence, HC, Two")
spaghetti(y = data$y1[data$Group=="GAD" & data$Dose=="Placebo"], 
	time = data$time[data$Group=="GAD" & data$Dose=="Placebo"],
	ids = data$id[data$Group=="GAD" & data$Dose=="Placebo"], ylim=c(-5,5))
lines(time_cont,y1.fit.gad.placebo, type="l", lwd=3, col=2)
title(main = "Valence, GAD, Saline")
spaghetti(y = data$y1[data$Group=="GAD" & data$Dose=="Point5"], 
	time = data$time[data$Group=="GAD" & data$Dose=="Point5"],
	ids = data$id[data$Group=="GAD" & data$Dose=="Point5"], ylim=c(-5,5))
lines(time_cont,y1.fit.gad.point5, type="l", lwd=3, col=2)
title(main = "Valence, GAD, Point5")
spaghetti(y = data$y1[data$Group=="GAD" & data$Dose=="Two"], 
	time = data$time[data$Group=="GAD" & data$Dose=="Two"],
	ids = data$id[data$Group=="GAD" & data$Dose=="Two"], ylim=c(-5,5))
lines(time_cont,y1.fit.gad.two, type="l", lwd=3, col=2)
title(main = "Valence, GAD, Two")



par(mfrow=c(1,3),mar=c(2,2,1,0.5))
plot(time_cont,y1.fpc1.hc.placebo, type="l", col=1, lty=1, ylim=c(-1.5,1.5))
lines(time_cont,y1.fpc1.hc.point5, type="l", col=2, lty=1)
lines(time_cont,y1.fpc1.hc.two, type="l", col=3, lty=1)
lines(time_cont,y1.fpc2.hc.placebo, type="l", col=1, lty=2)
lines(time_cont,y1.fpc2.hc.point5, type="l", col=2, lty=2)
lines(time_cont,y1.fpc2.hc.two, type="l", col=3, lty=2)
legend(0.2,1.6,legend=c("fpc1/Saline","fpc1/point5","fpc1/two","fpc2/Saline","fpc2/point5","fpc2/two"), col=c(1:3,1:3),lty=c(1,1,1,2,2,2))
title(main="Valence HC fpc curves")

plot(time_cont,y2.fpc1.hc.placebo, type="l", col=1, lty=1, ylim=c(-1.5,1.5))
lines(time_cont,y2.fpc1.hc.point5, type="l", col=2, lty=1)
lines(time_cont,y2.fpc1.hc.two, type="l", col=3, lty=1)
lines(time_cont,y2.fpc2.hc.placebo, type="l", col=1, lty=2)
lines(time_cont,y2.fpc2.hc.point5, type="l", col=2, lty=2)
lines(time_cont,y2.fpc2.hc.two, type="l", col=3, lty=2)
legend("bottomright",legend=c("fpc1/Saline","fpc1/point5","fpc1/two","fpc2/Saline","fpc2/point5","fpc2/two"), col=c(1:3,1:3),lty=c(1,1,1,2,2,2))
title(main="HR HC fpc curves")

plot(time_cont,y3.fpc1.hc.placebo, type="l", col=1, lty=1, ylim=c(-1.5,1.5))
lines(time_cont,y3.fpc1.hc.point5, type="l", col=2, lty=1)
lines(time_cont,y3.fpc1.hc.two, type="l", col=3, lty=1)
lines(time_cont,y3.fpc2.hc.placebo, type="l", col=1, lty=2)
lines(time_cont,y3.fpc2.hc.point5, type="l", col=2, lty=2)
lines(time_cont,y3.fpc2.hc.two, type="l", col=3, lty=2)
legend("bottomright",legend=c("fpc1/Saline","fpc1/point5","fpc1/two","fpc2/Saline","fpc2/point5","fpc2/two"), col=c(1:3,1:3),lty=c(1,1,1,2,2,2))
title(main="Dial HC fpc curves")

par(mfrow=c(1,3),mar=c(2,2,1,0.5))
plot(time_cont,y1.fpc1.gad.placebo, type="l", col=1, lty=1, ylim=c(-1.5,1.5))
lines(time_cont,y1.fpc1.gad.point5, type="l", col=2, lty=1)
lines(time_cont,y1.fpc1.gad.two, type="l", col=3, lty=1)
lines(time_cont,y1.fpc2.gad.placebo, type="l", col=1, lty=2)
lines(time_cont,y1.fpc2.gad.point5, type="l", col=2, lty=2)
lines(time_cont,y1.fpc2.gad.two, type="l", col=3, lty=2)
legend(0.2,1.6,legend=c("fpc1/Saline","fpc1/point5","fpc1/two","fpc2/Saline","fpc2/point5","fpc2/two"), col=c(1:3,1:3),lty=c(1,1,1,2,2,2))
title(main="Valence GAD fpc curves")

plot(time_cont,y2.fpc1.gad.placebo, type="l", col=1, lty=1, ylim=c(-1.5,1.5))
lines(time_cont,y2.fpc1.gad.point5, type="l", col=2, lty=1)
lines(time_cont,y2.fpc1.gad.two, type="l", col=3, lty=1)
lines(time_cont,y2.fpc2.gad.placebo, type="l", col=1, lty=2)
lines(time_cont,y2.fpc2.gad.point5, type="l", col=2, lty=2)
lines(time_cont,y2.fpc2.gad.two, type="l", col=3, lty=2)
legend("bottomright",legend=c("fpc1/Saline","fpc1/point5","fpc1/two","fpc2/Saline","fpc2/point5","fpc2/two"), col=c(1:3,1:3),lty=c(1,1,1,2,2,2))
title(main="HR GAD fpc curves")

plot(time_cont,y3.fpc1.gad.placebo, type="l", col=1, lty=1, ylim=c(-1.5,1.5))
lines(time_cont,y3.fpc1.gad.point5, type="l", col=2, lty=1)
lines(time_cont,y3.fpc1.gad.two, type="l", col=3, lty=1)
lines(time_cont,y3.fpc2.gad.placebo, type="l", col=1, lty=2)
lines(time_cont,y3.fpc2.gad.point5, type="l", col=2, lty=2)
lines(time_cont,y3.fpc2.gad.two, type="l", col=3, lty=2)
legend("bottomright",legend=c("fpc1/Saline","fpc1/point5","fpc1/two","fpc2/Saline","fpc2/point5","fpc2/two"), col=c(1:3,1:3),lty=c(1,1,1,2,2,2))
title(main="Dial GAD fpc curves")


par(mfrow=c(1,3),mar=c(2,2,1,0.5))
plot(time_cont,y1.fit.hc.placebo, type="l", col=1, lty=1, ylim=c(-1.5,1.5))
lines(time_cont,y1.fit.hc.point5, type="l", col=2, lty=1)
lines(time_cont,y1.fit.hc.two, type="l", col=3, lty=1)
lines(time_cont,y1.fit.gad.placebo, type="l", col=1, lty=2)
lines(time_cont,y1.fit.gad.point5, type="l", col=2, lty=2)
lines(time_cont,y1.fit.gad.two, type="l", col=3, lty=2)
legend(0.2,1.6,legend=c("HC/Saline","HC/Point5","HC/Two","GAD/Saline","GAD/Point5","GAD/Two"), col=c(1:3,1:3),lty=c(1,1,1,2,2,2))
title(main="Valence vmPFC ROI: FPC Fits")

plot(time_cont,y1.fpc1.hc.placebo, type="l", col=1, lty=1, ylim=c(-1.5,1.5))
lines(time_cont,y1.fpc1.hc.point5, type="l", col=2, lty=1)
lines(time_cont,y1.fpc1.hc.two, type="l", col=3, lty=1)
lines(time_cont,y1.fpc2.hc.placebo, type="l", col=1, lty=2)
lines(time_cont,y1.fpc2.hc.point5, type="l", col=2, lty=2)
lines(time_cont,y1.fpc2.hc.two, type="l", col=3, lty=2)
legend(0.2,1.6,legend=c("fpc1/Saline","fpc1/point5","fpc1/two","fpc2/Saline","fpc2/point5","fpc2/two"), col=c(1:3,1:3),lty=c(1,1,1,2,2,2))
title(main="Valence HC fpc curves")

plot(time_cont,y1.fpc1.gad.placebo, type="l", col=1, lty=1, ylim=c(-1.5,1.5))
lines(time_cont,y1.fpc1.gad.point5, type="l", col=2, lty=1)
lines(time_cont,y1.fpc1.gad.two, type="l", col=3, lty=1)
lines(time_cont,y1.fpc2.gad.placebo, type="l", col=1, lty=2)
lines(time_cont,y1.fpc2.gad.point5, type="l", col=2, lty=2)
lines(time_cont,y1.fpc2.gad.two, type="l", col=3, lty=2)
legend(0.2,1.6,legend=c("fpc1/Saline","fpc1/point5","fpc1/two","fpc2/Saline","fpc2/point5","fpc2/two"), col=c(1:3,1:3),lty=c(1,1,1,2,2,2))
title(main="Valence GAD fpc curves")



par(mfrow=c(1,3),mar=c(2,2,1,0.5))
plot(time_cont,y1.fit.hc.placebo, type="l", col=1, lty=1, ylim=c(-1.5,1.5))
lines(time_cont,y1.fit.hc.point5, type="l", col=2, lty=1)
lines(time_cont,y1.fit.hc.two, type="l", col=3, lty=1)
lines(time_cont,y1.fit.gad.placebo, type="l", col=1, lty=2)
lines(time_cont,y1.fit.gad.point5, type="l", col=2, lty=2)
lines(time_cont,y1.fit.gad.two, type="l", col=3, lty=2)
legend(0.2,1.6,legend=c("HC/Saline","HC/Point5","HC/Two","GAD/Saline","GAD/Point5","GAD/Two"), col=c(1:3,1:3),lty=c(1,1,1,2,2,2))
title(main="Caudate FPC Fitted curves")

plot(time_cont,y1.fpc1.hc.placebo, type="l", col=1, lty=1, ylim=c(-1.5,1.5))
lines(time_cont,y1.fpc1.hc.point5, type="l", col=2, lty=1)
lines(time_cont,y1.fpc1.hc.two, type="l", col=3, lty=1)
lines(time_cont,y1.fpc2.hc.placebo, type="l", col=1, lty=2)
lines(time_cont,y1.fpc2.hc.point5, type="l", col=2, lty=2)
lines(time_cont,y1.fpc2.hc.two, type="l", col=3, lty=2)
legend("bottomright",legend=c("fpc1/Saline","fpc1/point5","fpc1/two","fpc2/Saline","fpc2/point5","fpc2/two"), col=c(1:3,1:3),lty=c(1,1,1,2,2,2))
title(main="Caudate HC fpc curves")

plot(time_cont,y1.fpc1.gad.placebo, type="l", col=1, lty=1, ylim=c(-1.5,1.5))
lines(time_cont,y1.fpc1.gad.point5, type="l", col=2, lty=1)
lines(time_cont,y1.fpc1.gad.two, type="l", col=3, lty=1)
lines(time_cont,y1.fpc2.gad.placebo, type="l", col=1, lty=2)
lines(time_cont,y1.fpc2.gad.point5, type="l", col=2, lty=2)
lines(time_cont,y1.fpc2.gad.two, type="l", col=3, lty=2)
legend("bottomright",legend=c("fpc1/Saline","fpc1/point5","fpc1/two","fpc2/Saline","fpc2/point5","fpc2/two"), col=c(1:3,1:3),lty=c(1,1,1,2,2,2))
title(main="Caudate GAD fpc curves")
