
library(lme4)
library(lmerTest)
library(ggplot2)


summarySE <- function(data=NULL, measurevar, groupvars=NULL, na.rm=FALSE,
                  conf.interval=.95, .drop=TRUE) {
library(plyr)

# New version of length which can handle NA's: if na.rm==T, don't count them
length2 <- function (x, na.rm=FALSE) {
    if (na.rm) sum(!is.na(x))
    else       length(x)
}

# This does the summary. For each group's data frame, return a vector with
# N, mean, and sd
datac <- ddply(data, groupvars, .drop=.drop,
               .fun = function(xx, col) {
                   c(N    = length2(xx[[col]], na.rm=na.rm),
                     mean = mean   (xx[[col]], na.rm=na.rm),
                     sd   = sd     (xx[[col]], na.rm=na.rm)
                   )
               },
               measurevar
)

# Rename the "mean" column    
datac <- rename(datac, c("mean" = measurevar))

datac$se <- datac$sd / sqrt(datac$N)  # Calculate standard error of the mean

# Confidence interval multiplier for standard error
# Calculate t-statistic for confidence interval: 
# e.g., if conf.interval is .95, use .975 (above/below), and use df=N-1
ciMult <- qt(conf.interval/2 + .5, datac$N-1)
datac$ci <- datac$se * ciMult

return(datac)
}



ROIs <- read.csv("ISO_GAD_Paper/ROI_extract_means2.csv", header=TRUE)
Group <- factor(ROIs$Group,levels=c("Com","GAD"))
relevel(Group, HC)
Dose <- factor(ROIs$Dose,levels=c("Placebo","Point5","Two"))
Period <- factor(ROIs$Period,levels=c("Baseline","Ticipate","Peak","Early","Late"))
vmPFC <- lmer(ROIs$vmPFC ~ Period * Dose * Group + Age + BMI + Group:Age + Group:BMI + (1 | ROIs$Subject), data=ROIs)
#vmPFChr <- lmer(ROIs$vmPFC ~ Period * Dose * Group + Age + BMI + Group:Age + Group:BMI * HR + (1 | ROIs$Subject), data=ROIs)
dmPFC <- lmer(ROIs$dmPFC ~ Period * Dose * Group + Age + BMI + Group:Age + Group:BMI + (1 | ROIs$Subject), data=ROIs)
#dmPFChr <- lmer(ROIs$dmPFC ~ Period * Dose * Group + Age + BMI + Group:Age + Group:BMI * HR + (1 | ROIs$Subject), data=ROIs)
dACC <- lmer(ROIs$dACC ~ Period * Dose * Group + Age + BMI + Group:Age + Group:BMI + (1 | ROIs$Subject), data=ROIs)
#dACChr <- lmer(ROIs$dACC ~ Period * Dose * Group + Age + BMI + Group:Age + Group:BMI * HR + (1 | ROIs$Subject), data=ROIs)
Amy <- lmer(ROIs$Amygdala ~ Period * Dose * Group + Age + BMI + Group:Age + Group:BMI + (1 | ROIs$Subject), data=ROIs)
#AmyHR <- lmer(ROIs$Amygdala ~ Period * Dose * Group + Age + BMI + Group:Age + Group:BMI * HR + (1 | ROIs$Subject), data=ROIs)
Caudate <- lmer(ROIs$Caudate ~ Period * Dose * Group + Age + BMI + Group:Age + Group:BMI + (1 | ROIs$Subject), data=ROIs)
#CaudateHR <- lmer(ROIs$Caudate ~ Period * Dose * Group + Age + BMI + Group:Age + Group:BMI * HR + (1 | ROIs$Subject), data=ROIs)
A_Insula <- lmer(ROIs$A_Insula ~ Period * Dose * Group + Age + BMI + Group:Age + Group:BMI + (1 | ROIs$Subject), data=ROIs)
#A_InsulaHR <- lmer(ROIs$A_Insula ~ Period * Dose * Group + Age + BMI + Group:Age + Group:BMI * HR + (1 | ROIs$Subject), data=ROIs)
P_Insula <- lmer(ROIs$P_Insula ~ Period * Dose * Group + Age + BMI + Group:Age + Group:BMI + (1 | ROIs$Subject), data=ROIs)
#P_InsulaHR <- lmer(ROIs$P_Insula ~ Period * Dose * Group + Age + BMI + Group:Age + Group:BMI * HR + (1 | ROIs$Subject), data=ROIs)
Self <- lmer(ROIs$Self ~ Period * Dose * Group + Age + BMI + Group:Age + Group:BMI + (1 | ROIs$Subject), data=ROIs)
#SelfHR <- lmer(ROIs$Self ~ Period * Dose * Group + Age + BMI + Group:Age + Group:BMI * HR + (1 | ROIs$Subject), data=ROIs)
Valence <- lmer(ROIs$Valence ~ Period * Dose * Group + Age + BMI + Group:Age + Group:BMI + (1 | ROIs$Subject), data=ROIs)
#ValenceHR <- lmer(ROIs$Valence ~ Period * Dose * Group + Age + BMI + Group:Age + Group:BMI * HR + (1 | ROIs$Subject), data=ROIs)
Autonomic <- lmer(ROIs$Autonomic ~ Period * Dose * Group + Age + BMI + Group:Age + Group:BMI + (1 | ROIs$Subject), data=ROIs)
#AutonomicHR <- lmer(ROIs$Autonomic ~ Period * Dose * Group + Age + BMI + Group:Age + Group:BMI * HR + (1 | ROIs$Subject), data=ROIs)
L_Auto <- lmer(ROIs$L_Auto ~ Period * Dose * Group + Age + BMI + Group:Age + Group:BMI + (1 | ROIs$Subject), data=ROIs)
#L_AutoHR <- lmer(ROIs$L_Auto ~ Period * Dose * Group + Age + BMI + Group:Age + Group:BMI * HR + (1 | ROIs$Subject), data=ROIs)
R_Auto <- lmer(ROIs$R_Auto ~ Period * Dose * Group + Age + BMI + Group:Age + Group:BMI + (1 | ROIs$Subject), data=ROIs)
#R_AutoHR <- lmer(ROIs$R_Auto ~ Period * Dose * Group + Age + BMI + Group:Age + Group:BMI * HR + (1 | ROIs$Subject), data=ROIs)

i.1 <- lmer(fpc11~Group*Dose + (1|Name),data=data[data$time==min(data$time),])
i.2 <- lmer(fpc12~Group*Dose + (1|Name),data=data[data$time==min(data$time),])
i.3 <- lmer(fpc21~Group*Dose + (1|Name),data=data[data$time==min(data$time),])
i.4 <- lmer(fpc22~Group*Dose + (1|Name),data=data[data$time==min(data$time),])
i.5 <- lmer(fpc31~Group*Dose + (1|Name),data=data[data$time==min(data$time),])
i.6 <- lmer(fpc32~Group*Dose + (1|Name),data=data[data$time==min(data$time),])

ii.1 <- lmer(fpc11~Group*Dose*fpc21 + (1|Name),data=data[data$time==min(data$time),])
ii.2 <- lmer(fpc12~Group*Dose*fpc21 + (1|Name),data=data[data$time==min(data$time),])
ii.3 <- lmer(fpc11~Group*Dose*fpc22 + (1|Name),data=data[data$time==min(data$time),])
ii.4 <- lmer(fpc12~Group*Dose*fpc22 + (1|Name),data=data[data$time==min(data$time),])

summary(i.1)
summary(i.2)
summary(i.3)
summary(i.4)
summary(i.5)
summary(i.6)

summary(ii.1)
summary(ii.2)
summary(ii.3)
summary(ii.4)

report(i.1)
report(i.2)
report(i.3)
report(i.4)
report(i.5)
report(i.6)

report(ii.1)
report(ii.2)
report(ii.3)
report(ii.4)

confint(i.1)
confint(i.2)
confint(i.3)
confint(i.4)
confint(i.5)
confint(i.6)

confint(ii.1)
confint(ii.2)
confint(ii.3)
confint(ii.4)

vmPFChr <- lmer(ROIs$vmPFC ~ Dose * Group * Period + HR + Group:HR + Dose:HR + (1 | ROIs$Subject), data=ROIs)
dmPFChr <- lmer(ROIs$dmPFC ~ Dose * Group * Period + HR + Group:HR + Dose:HR + (1 | ROIs$Subject), data=ROIs)
dACChr <- lmer(ROIs$dACC ~ Dose * Group * Period + HR + Group:HR + Dose:HR + (1 | ROIs$Subject), data=ROIs)
AmyHR <- lmer(ROIs$Amygdala ~ Dose * Group * Period + HR + Group:HR + Dose:HR + (1 | ROIs$Subject), data=ROIs)
CaudateHR <- lmer(ROIs$Caudate ~ Dose * Group * Period + HR + Group:HR + Dose:HR + (1 | ROIs$Subject), data=ROIs)
A_InsulaHR <- lmer(ROIs$A_Insula ~ Dose * Group * Period + HR + Group:HR + Dose:HR + (1 | ROIs$Subject), data=ROIs)
P_InsulaHR <- lmer(ROIs$P_Insula ~ Dose * Group * Period + HR + Group:HR + Dose:HR + (1 | ROIs$Subject), data=ROIs)
SelfHR <- lmer(ROIs$Self ~ Dose * Group * Period + HR + Group:HR + Dose:HR + (1 | ROIs$Subject), data=ROIs)
ValenceHR <- lmer(ROIs$Valence ~ Dose * Group * Period + HR + Group:HR + Dose:HR + (1 | ROIs$Subject), data=ROIs)
AutonomicHR <- lmer(ROIs$Autonomic ~ Dose * Group * Period + HR + Group:HR + Dose:HR + (1 | ROIs$Subject), data=ROIs)
L_AutoHR <- lmer(ROIs$L_Auto ~ Dose * Group * Period + HR + Group:HR + Dose:HR + (1 | ROIs$Subject), data=ROIs)
R_AutoHR <- lmer(ROIs$R_Auto ~ Dose * Group * Period + HR + Group:HR + Dose:HR + (1 | ROIs$Subject), data=ROIs)

vmPFCd <- lmer(ROIs$vmPFC ~ Dose * Group * Period + Dial + Group:Dial + Dose:Dial + (1 | ROIs$Subject), data=ROIs)
dmPFCd <- lmer(ROIs$dmPFC ~ Dose * Group * Period + Dial + Group:Dial + Dose:Dial + (1 | ROIs$Subject), data=ROIs)
dACCd <- lmer(ROIs$dACC ~ Dose * Group * Period + Dial + Group:Dial + Dose:Dial + (1 | ROIs$Subject), data=ROIs)
AmyD <- lmer(ROIs$Amygdala ~ Dose * Group * Period + Dial + Group:Dial + Dose:Dial + (1 | ROIs$Subject), data=ROIs)
CaudateD <- lmer(ROIs$Caudate ~ Dose * Group * Period + Dial + Group:Dial + Dose:Dial + (1 | ROIs$Subject), data=ROIs)
A_InsulaD <- lmer(ROIs$A_Insula ~ Dose * Group * Period + Dial + Group:Dial + Dose:Dial + (1 | ROIs$Subject), data=ROIs)
P_InsulaD <- lmer(ROIs$P_Insula ~ Dose * Group * Period + Dial + Group:Dial + Dose:Dial + (1 | ROIs$Subject), data=ROIs)
SelfD <- lmer(ROIs$Self ~ Dose * Group * Period + Dial + Group:Dial + Dose:Dial + (1 | ROIs$Subject), data=ROIs)
ValenceD <- lmer(ROIs$Valence ~ Dose * Group * Period + Dial + Group:Dial + Dose:Dial + (1 | ROIs$Subject), data=ROIs)
AutonomicD <- lmer(ROIs$Autonomic ~ Dose * Group * Period + Dial + Group:Dial + Dose:Dial + (1 | ROIs$Subject), data=ROIs)
L_AutoD <- lmer(ROIs$L_Auto ~ Dose * Group * Period + Dial + Group:Dial + Dose:Dial + (1 | ROIs$Subject), data=ROIs)
R_AutoD <- lmer(ROIs$R_Auto ~ Dose * Group * Period + Dial + Group:Dial + Dose:Dial + (1 | ROIs$Subject), data=ROIs)

summary(vmPFC)
summary(vmPFChr)
summary(dmPFC)
summary(dmPFChr)
summary(dACC)
summary(dACChr)
summary(Amy)
summary(AmyHR)
summary(Caudate)
summary(CaudateHR)
summary(A_Insula)
summary(A_InsulaHR)
summary(P_Insula)
summary(P_InsulaHR)
summary(Self)
summary(SelfHR)
summary(Valence)
summary(ValenceHR)
summary(Autonomic)
summary(AutonomicHR)
summary(L_Auto)
summary(L_AutoHR)
summary(R_Auto)
summary(R_AutoHR)

summary(vmPFCd)
summary(dmPFCd)
summary(dACCd)
summary(AmyD)
summary(CaudateD)
summary(A_InsulaD)
summary(P_InsulaD)
summary(SelfD)
summary(ValenceD)
summary(AutonomicD)
summary(L_AutoD)
summary(R_AutoD)

Self <- lmer(data$Self ~ Dose * Group * Period + (1 | data$Subject), data=data[data$time==min(data$time),]))
SelfHR <- lmer(data$Self ~ Dose * Group * Period * HR + (1 | data$Subject), data=data)
Valence <- lmer(data$Valence ~ Dose * Group * Period + (1 | data$Subject), data=data)
ValenceHR <- lmer(data$Valence ~ Dose * Group * Period * HR + (1 | data$Subject), data=data)
Autonomic <- lmer(data$Autonomic ~ Dose * Group * Period + (1 | data$Subject), data=data)
AutonomicHR <- lmer(data$Autonomic ~ Dose * Group * Period * HR + (1 | data$Subject), data=data)

vmPFCci <- confint(vmPFC)
vmPFChrCI <- confint(vmPFChr)
dmPFCci <- confint(dmPFC)
dmPFChrCI <- confint(dmPFChr)
dACCci <- confint(dACC)
dACChrCI <- confint(dACChr)
AmyCI <- confint(Amy)
AmyHRci <- confint(AmyHR)
CaudateCI <- confint(Caudate)
CaudateHRci <- confint(CaudateHR)
A_InsulaCI <- confint(A_Insula)
A_IhrCI <- confint(A_InsulaHR)
P_InsulaCI <- confint(P_Insula)
P_IhrCI <- confint(P_InsulaHR)
SelfCI <- confint(Self)
SelfHRci <- confint(SelfHR)
ValCI <- confint(Valence)
ValHRci <- confint(ValenceHR)
AutoCI <- confint(Autonomic)
AutoHRci <- confint(AutonomicHR)
L_AutoCI <- confint(L_Auto)
L_AutoHRci <- confint(L_AutoHR)
R_AutoCI <- confint(R_Auto)
R_AutoHRci <- confint(R_AutoHR)

vmPFCci
vmPFChrCI
dmPFCci
dmPFChrCI
dACCci
dACChrCI
AmyCI
AmyHRci
CaudateCI
CaudateHRci
A_InsulaCI
A_IhrCI
P_InsulaCI
P_IhrCI
SelfCI
SelfHRci
ValCI
ValHRci
AutoCI
AutoHRci
L_AutoCI
L_AutoHRci
R_AutoCI
R_AutoHRci

valence <- cbind(data$fpc11, data$fpc12)
VALvar <- apply(valence, 2, var)
VALvar/sum(VALvar)


SelfHRfx <- FEsim(SelfHR, 1000)
cbind(SelfHRfx[,1] , round(SelfHRfx[,2:4],3))
plotFEsim(SelfHRfx)






A_Insula_SE <- summarySE(data=ROIs, measurevar=c("A_Insula"), c("Group","Dose","Period"))


Group <- factor(ROIs$Group,levels=c("HC","MA"))
relevel(Group, HC)
Dose <- factor(ROIs$Dose,levels=c("Placebo","Point5","Two"))
Period <- factor(ROIs$Period,levels=c("Base","Pate","Peak","RecEarly","RecLate"))
vmPFC <- lmer(ROIs$vmPFC ~ Dose * Group * Period + (1 | ROIs$Subject), data=ROIs)

grp <- factor(RIomit$Group,levels=c("COM","GAD"))
Group <- factor(Retro$Group,levels=c("HC","GAD"))

Group <- factor(Retro$Group,levels=c("HC","GAD"))
dose <- factor(CC$Dose,levels=c("Sal","pFive","Two"))
MaxHR <- lmer(CC$Max_HRcc ~ dose * grp + grp:CC$Age + grp:CC$BMI + (1 | CC$Subject), data=CC)

Detect = read.csv("Detection_counts.csv", header=TRUE)
Retro = read.csv("SOBP_29_HR_base_peak_av.csv", header=TRUE)
HRdial = read.csv("SOBP_29_HR_Dial_PeriodxRun_BC223.csv", header=TRUE)
CD25 = read.csv("SOBP_demo_clinical.csv", header=TRUE)
CD25 = read.csv("CD25_2out.csv", header=TRUE)
Cluster = read.csv("ISO_GAD_Paper/Cluster001Extract.csv", header=TRUE)
Peak001 <- subset(Cluster,Cluster$Mask=="Peak")
Rec001 <- subset(Cluster,Cluster$Mask=="Rec1")
AN_HC_dial = read.csv("Anorexia/AN_HC_28_phys_dial.csv", header=TRUE)
AN_cc = read.csv("Anorexia/AN_HC_cc.csv", header=TRUE)
InsClust = read.csv("ALL_PeakER_noBASEcon.csv", header=TRUE)

RXphysNA <- RXsub[,1:13]
RXdialNA <- subset(RXsub, select = -c(Av_HR,Max_HR,Av_RVV,Max_RVV,Av_RVT,Max_RVT))
RXphys <- na.omit(RXphysNA)
RXdial <- na.omit(RXdialNA)



InsDoses <- subset(InsClust,InsClust$Dose!="ISO")
Ins_G_SE <- summarySE(data=InsDoses, measurevar=c("NZ_Ven_Ins_comp"), c("Group","Dose"))
tiff("Insula_Group_Bar.tiff", units="mm", width=185, height=100, res=400)
ggplot(data=Ins_G_SE, aes(x=Dose, y=NZ_Ven_Ins_comp, fill=Group)) +
    labs(y = "Beta Estimates for Ventral Insula", x = NULL)+
    theme_bw()+
    theme(text=element_text(family="Arial", size=13))+
    theme(plot.title = element_text(hjust = 0.5), text = element_text(size=14))+
    theme(panel.border = element_blank())+
    geom_bar(stat="identity", position=position_dodge()) +
    geom_errorbar(position=position_dodge(.9), width=.25, aes(ymin=Ins_G_SE$NZ_Ven_Ins_comp-se, ymax=Ins_G_SE$NZ_Ven_Ins_comp+se))+
    scale_fill_manual(values=c("#6666FFFF", "#339900FF"),name = "Group", labels = c("HC", "GAD"))+
    scale_color_manual(values=c("#6666FFFF", "#339900FF"),name = "Group", labels = c("HC", "GAD"))+
    theme(legend.title=element_text(size=14),legend.text=element_text(size=13))+
    scale_x_discrete(limit=c("Sal", "pFive", "Two"), labels=c("Saline", "0.5 µg", "2.0 µg"))
dev.off()

Ins_SE <- summarySE(data=InsDoses, measurevar=c("NZ_Ven_Ins_comp"), c("Dose"))
tiff("Insula_Doses_Bar.tiff", units="mm", width=185, height=100, res=400)
ggplot(data=Ins_SE, aes(x=Dose, y=NZ_Ven_Ins_comp, fill=Dose)) +
    labs(y = "Beta Estimates for Ventral Insula", x = NULL)+
    theme_bw()+
    theme(text=element_text(family="Arial", size=13))+
    theme(plot.title = element_text(hjust = 0.5), text = element_text(size=14))+
    theme(panel.border = element_blank())+
    geom_bar(stat="identity", position=position_dodge()) +
    geom_errorbar(position=position_dodge(.9), width=.25, aes(ymin=Ins_SE$NZ_Ven_Ins_comp-se, ymax=Ins_SE$NZ_Ven_Ins_comp+se))+
	scale_color_jama()+
	scale_fill_jama()+
	theme(legend.position="none")+
    scale_x_discrete(limit=c("Sal", "pFive", "Two"), labels=c("Saline", "0.5 µg", "2.0 µg"))
dev.off()
	
	

plotDetect <- ggplot(data=Detect_raw, aes(x=Dose_label, y=Detect15, fill=Group)) +
labs(y = "# Detecting Heart Beat", x = "")+
theme_bw()+
theme(text=element_text(family="Arial", size=11))+
theme(plot.title = element_text(hjust = 0.5), text = element_text(size=14))+
theme(panel.border = element_blank())+
theme(axis.line = element_line(colour = "black"))+geom_errorbar(size=1, width=.1, aes(ymin=Anxious-se, ymax=Anxious+se)) +

geom_bar(stat="identity", position=position_dodge()) +
geom_errorbar(position=position_dodge(.9), width=.25, aes(ymin=Det_SE$Detect15-se, ymax=Det_SE$Detect15+se)) +

Det_SE <- summarySE(data=Detect_raw, measurevar=c("Detect15"), c("Group","Dose"))
plotDetect <- ggplot(data=Det_SE, aes(x=Dose_label, y=Detect15, fill=Group)) +
labs(y = "% Detecting Heartbeat", x = "")+
theme_bw()+
theme(text=element_text(family="Arial", size=11))+
theme(plot.title = element_text(hjust = 0.5), text = element_text(size=14))+
theme(panel.border = element_blank())+
geom_bar(stat="identity", position=position_dodge()) +
#geom_errorbar(position=position_dodge(.9), width=.25, aes(ymin=Det_SE$Detect15-se, ymax=Det_SE$Detect15+se))+
scale_fill_manual(values=c("#6666FFFF", "#339900FF"),name = "Group", labels = c("HC", "GAD"))+
scale_color_manual(values=c("#6666FFFF", "#339900FF"),name = "Group", labels = c("HC", "GAD"))+
#theme(legend.position=c(0.27,0.8))+
scale_x_discrete(limit=c("Saline", "0.5 mcg", "2 mcg"), labels=c("Saline", "0.5 µg", "2.0 µg"))

plotDetect <- ggplot(data=Detect_raw, aes(x=Dose_label, y=Detect15, fill=Group)) +
labs(y = "# Detecting Heart Beat", x = "")+
theme_bw()+
theme(text=element_text(family="Arial", size=11))+
theme(plot.title = element_text(hjust = 0.5), text = element_text(size=14))+
theme(panel.border = element_blank())+
theme(axis.line = element_line(colour = "black"))+
geom_sina(aes(color=Group), alpha=0.3) +
geom_boxplot(position=position_dodge(0.9), alpha=0.2, outlier.alpha = 0, width=0.2)+
scale_fill_manual(values=c("#6666FFFF", "#339900FF"),name = "Group", labels = c("HC", "GAD"))+
scale_color_manual(values=c("#6666FFFF", "#339900FF"),name = "Group", labels = c("HC", "GAD"))+
#theme(legend.position=c(0.27,0.8))+
scale_x_discrete(limit=c("Placebo", "Point5", "Two"), labels=c("Saline", "0.5 µg", "2.0 µg"))

ggplot(data=Peak001, aes(x=Peak001$Dose, y=Peak001$vmPFCnz, fill=Group)) +
   labs(y = "% Signal Δ from Base", x = NULL)+
   theme_bw()+
   theme(text=element_text(family="Arial", size=10))+
   theme(plot.title = element_text(hjust = 0.5), text = element_text(size=16))+
   geom_sina(aes(color=Group), alpha=0.3, size=2) +
   geom_boxplot(position=position_dodge(0.9), alpha=0.2, outlier.alpha = 0, lwd=0.75, width=0.3)+
   scale_fill_manual(values=c("#6666FFFF", "#339900FF"),name = "Group", labels = c("HC", "GAD"))+
   scale_color_manual(values=c("#6666FFFF", "#339900FF"),name = "Group", labels = c("HC", "GAD"))+
   theme(legend.position=c(0.2,0.2),legend.text=element_text(size=10),legend.title=element_text(size=14))+
   scale_x_discrete(limit=c("Sal", "pFive", "Two"), labels=c("Saline", "0.5 µg", "2.0 µg"))

ggsave("Cluser_Peak.tiff", units="mm", width=80, height=120, dpi=400)

Rec1Box <- ggplot(data=Rec001, aes(x=Rec001$Dose, y=Rec001$vmPFCnz, fill=Group)) +
    labs(y = "% Signal Δ from Base", x = "Dose (µg)")+
    theme_bw()+
    theme(text=element_text(family="Arial", size=12))+
	theme(plot.title = element_text(hjust = 0.5), text = element_text(size=20))+
    geom_sina(aes(color=Group), alpha=0.3) +
	geom_boxplot(position=position_dodge(0.9), alpha=0.2, outlier.alpha = 0, width=0.2)+
	scale_fill_manual(values=c("#6666FFFF", "#339900FF"),name = "Group", labels = c("HC", "GAD"))+
    scale_color_manual(values=c("#6666FFFF", "#339900FF"),name = "Group", labels = c("HC", "GAD"))+
	#theme(legend.position=c(0.27,0.8))+
	scale_x_discrete(limit=c("Sal", "pFive", "Two"), labels=c("Saline", "0.5 µg", "2.0 µg"))
	
CD25 <- subset(CD25,CD25$Group!="AN")
RetroCols <- c("Subject","Group","Dose","Dose_label","HBI","RI","Anxious","Happy","Age","BMI")
HBIomit <- Retro[RetroCols]
HBIomit <- na.omit(HBIomit)
HAPomit <- Retro[RetroCols]
HAPomit <- na.omit(HAPomit)
RIomit <- Retro[RetroCols]
RIomit <- na.omit(RIomit)
ANXomit <- Retro[RetroCols]
ANXomit <- na.omit(ANXomit)

AN_Retro_full = read.csv("AN_HC_Post_ratings.csv", header=TRUE)
AN_Retro <- subset(AN_Retro_full, AN_Retro_full$Dose!="Rest")

AN_HBIcols <- c("Subject","Group","Med_Stat","Med_num","Dose","Dose_label","HBI","Age","BMI")
AN_RIcols <- c("Subject","Group","Med_Stat","Med_num","Dose","Dose_label","RI","Age","BMI")
AN_HAPcols <- c("Subject","Group","Med_Stat","Med_num","Dose","Dose_label","Happy","Age","BMI")
AN_ANXcols <- c("Subject","Group","Med_Stat","Med_num","Dose","Dose_label","Anxious","Age","BMI")
AN_HBIomit <- AN_Retro[AN_HBIcols]
AN_HBIomit <- na.omit(AN_HBIomit)
AN_HAPomit <- AN_Retro[AN_HAPcols]
AN_HAPomit <- na.omit(AN_HAPomit)
AN_RIomit <- AN_Retro[AN_RIcols]
AN_RIomit <- na.omit(AN_RIomit)
AN_ANXomit <- AN_Retro[AN_ANXcols]
AN_ANXomit <- na.omit(AN_ANXomit)

AN_HBI_SE <- summarySE(data=AN_HBIomit, measurevar=c("HBI"), c("Group","Dose"))
AN_HAP_SE <- summarySE(data=AN_HAPomit, measurevar=c("Happy"), c("Group","Dose"))

grp <- factor(AN_HAPomit$Group,levels=c("HC","GAD"))
dose <- factor(AN_HAPomit$Dose_labels,levels=c("0","0.5","2"))
grp <- factor(AN_HBIomit$Group,levels=c("HC","GAD"))
dose <- factor(AN_HBIomit$Dose_labels,levels=c("0","0.5","2"))
grp <- factor(AN_RIomit$Group,levels=c("HC","GAD"))
dose <- factor(AN_RIomit$Dose_labels,levels=c("0","0.5","2"))
grp <- factor(AN_ANXomit$Group,levels=c("HC","GAD"))
#dose <- factor(AN_ANXomit$Dose,levels=c("Sal","pFive","Two"))
dose <- factor(AN_ANXomit$Dose_labels,levels=c("0","0.5","2"))

HRsub <- subset(HRdial, HRdial$Period!="Prep")
RXsub <- subset(AN_HC_dial, AN_HC_dial$Period!="Prep")
Group <- factor(RXphys$Group,levels=c("HC","RX"))
Dose <- factor(RXphys$Dose,levels=c("Placebo","Point5","Two"))
Period <- factor(RXphys$Period,levels=c("Baseline","Pate","Peak","RecEarly","RecLate"))
Group <- factor(RXdial$Group,levels=c("HC","RX"))
Dose <- factor(RXdial$Dose,levels=c("Placebo","Point5","Two"))
Period <- factor(RXdial$Period,levels=c("Baseline","Pate","Peak","RecEarly","RecLate"))

HR <- lmer(RXsub$Av_HR ~ Period * Dose * Group + RXsub$Age + RXsub$BMI + Group:RXsub$Age + Group:RXsub$BMI + (1 | RXsub$Subject), data=RXsub)
Dial <- lmer(RXdial$Mean_Dial ~ Period * Dose * Group + RXdial$Age + RXdial$BMI + Group:RXdial$Age + Group:RXdial$BMI + (1 | RXdial$Subject), data=RXdial)
RVV <- lmer(RXsub$Av_RVV ~ Period * Dose * Group + RXsub$Age + RXsub$BMI + Group:RXsub$Age + Group:RXsub$BMI + (1 | RXsub$Subject), data=RXsub)

HRm <- lmer(RXphys$Av_HR ~ Period * Dose * Group + Med_num + Age + BMI + Group:Med_num + Group:Age + Group:BMI + (1 | Subject), data=RXphys)
RVVm <- lmer(RXphys$Av_RVV ~ Period * Dose * Group + Med_num + Age + BMI + Group:Med_num + Group:Age + Group:BMI + (1 | Subject), data=RXphys)
Dialm <- lmer(RXdial$Av_Dial ~ Period * Dose * Group + Med_num + Age + BMI + Group:Med_num + Group:Age + Group:BMI + (1 | Subject), data=RXdial)

summary(HR)
summary(Dial)
summary(RVV)
confint(HR)
confint(Dial)
confint(RVV)
summary(HRm)
summary(Dialm)
summary(RVVm)
confint(HRm)
confint(Dialm)
confint(RVVm)

RI <- lmer(RIomit$RI ~ dose * grp + grp:RIomit$Age + grp:RIomit$BMI + (1 | RIomit$Subject), data=RIomit)

HBI <- lmer(HBIomit$HBI ~ dose * grp + grp:HBIomit$Age + grp:HBIomit$BMI + (1 | HBIomit$Subject), data=HBIomit)

HAP <- lmer(HAPomit$HAP ~ dose * grp + grp:HAPomit$Age + grp:HAPomit$BMI + (1 | HAPomit$Subject), data=HAPomit)

Anx <- lmer(ANXomit$Anxious ~ dose * grp + grp:ANXomit$Age + grp:ANXomit$BMI + (1 | ANXomit$Subject), data=ANXomit)


AN_RI <- lmer(AN_RIomit$RI ~ Dose * Group + Age + BMI + Group:Age + Group:BMI + (1 | Subject), data=AN_RIomit)

AN_HBI <- lmer(AN_HBIomit$HBI ~ Dose * Group + Age + BMI + Group:Age + Group:BMI + (1 | Subject), data=AN_HBIomit)

AN_HAP <- lmer(AN_HAPomit$Happy ~ Dose * Group + Age + BMI + Group:Age + Group:BMI + (1 | Subject), data=AN_HAPomit)

AN_Anx <- lmer(AN_ANXomit$Anxious ~ Dose * Group + Age + BMI + Group:Age + Group:BMI + (1 | Subject), data=AN_ANXomit)


AN_RIm <- lmer(AN_RIomit$RI ~ Dose * Group + Med_num + Age + BMI + Group:Age + Group:BMI + (1 | Subject), data=AN_RIomit)

AN_HBIm <- lmer(AN_HBIomit$HBI ~ Dose * Group + Med_num + Age + BMI + Group:Age + Group:BMI + (1 | Subject), data=AN_HBIomit)

AN_HAPm <- lmer(AN_HAPomit$Happy ~ Dose * Group + Med_num + Age + BMI + Group:Age + Group:BMI + (1 | Subject), data=AN_HAPomit)

AN_Anxm <- lmer(AN_ANXomit$Anxious ~ Dose * Group + Med_num + Age + BMI + Group:Age + Group:BMI + (1 | Subject), data=AN_ANXomit)


summary(AN_RI)
summary(AN_HBI)
summary(AN_HAP)
summary(AN_Anx)
confint(AN_Anx)
confint(AN_RI)
confint(AN_HBI)
confint(AN_HAP)
summary(AN_RIm)
summary(AN_HBIm)
summary(AN_HAPm)
summary(AN_Anxm)
confint(AN_Anxm)
confint(AN_RIm)
confint(AN_HBIm)
confint(AN_HAPm)


AN_LagHRcols <- c("Subject","Group","Med_Stat","Med_num","Dose","Run","Lag_At_Max_HRcc","Age","BMI")
AN_LagRVVcols <- c("Subject","Group","Med_Stat","Med_num","Dose","Run","Lag_At_Max_RVVcc","Age","BMI")
AN_LagRVTcols <- c("Subject","Group","Med_Stat","Med_num","Dose","Run","Lag_At_Max_RVTcc","Age","BMI")
HRccLag <- AN_cc[AN_LagHRcols]
HRccLag <- na.omit(HRccLag)
RVVccLag <- AN_cc[AN_LagRVVcols]
RVVccLag <- na.omit(RVVccLag)
RVTccLag <- AN_cc[AN_LagRVTcols]
RVTccLag <- na.omit(RVTccLag)

lmeHRcc <- lmer(AN_cc$HRcc_at_Zero_Lag ~ Dose * Group + Age + BMI + Group:Age + Group:BMI + (1 | AN_cc$Subject), data=AN_cc)

lmeHRccMax <- lmer(AN_cc$Max_HRcc ~ Dose * Group + Age + BMI + Group:Age + Group:BMI + (1 | AN_cc$Subject), data=AN_cc)

lmeHRccLag <- lmer(HRccLag$Lag_At_Max_HRcc ~ Dose * Group + Age + BMI + Group:Age + Group:BMI + (1 | HRccLag$Subject), data=HRccLag)

lmeRVVcc <- lmer(AN_cc$RVVcc_at_Zero_Lag ~ Dose * Group + Age + BMI + Group:Age + Group:BMI + (1 | AN_cc$Subject), data=AN_cc)

lmeRVVccMax <- lmer(AN_cc$Max_RVVcc ~ Dose * Group + Age + BMI + Group:Age + Group:BMI + (1 | AN_cc$Subject), data=AN_cc)

lmeRVVccLag <- lmer(RVVccLag$Lag_At_Max_RVVcc ~ Dose * Group + Age + BMI + Group:Age + Group:BMI + (1 | RVVccLag$Subject), data=RVVccLag)

lmeRVTcc <- lmer(AN_cc$RVTcc_at_Zero_Lag ~ Dose * Group + Age + BMI + Group:Age + Group:BMI + (1 | AN_cc$Subject), data=AN_cc)

lmeRVTccMax <- lmer(AN_cc$Max_RVTcc ~ Dose * Group + Age + BMI + Group:Age + Group:BMI + (1 | AN_cc$Subject), data=AN_cc)

lmeRVTccLag <- lmer(RVTccLag$Lag_At_Max_RVTcc ~ Dose * Group + Age + BMI + Group:Age + Group:BMI + (1 | RVTccLag$Subject), data=RVTccLag)


HRccM <- lmer(AN_cc$HRcc_at_Zero_Lag ~ Dose * Group + Med_num + Age + BMI + Group:Age + Group:BMI + (1 | AN_cc$Subject), data=AN_cc)

HRccMaxM <- lmer(AN_cc$Max_HRcc ~ Dose * Group + Med_num + Age + BMI + Group:Age + Group:BMI + (1 | AN_cc$Subject), data=AN_cc)

HRccLagM <- lmer(HRccLag$Lag_At_Max_HRcc ~ Dose * Group + Med_num + Age + BMI + Group:Age + Group:BMI + (1 | HRccLag$Subject), data=HRccLag)

RVVccM <- lmer(AN_cc$RVVcc_at_Zero_Lag ~ Dose * Group + Med_num + Age + BMI + Group:Age + Group:BMI + (1 | AN_cc$Subject), data=AN_cc)

RVVccMaxM <- lmer(AN_cc$Max_RVVcc ~ Dose * Group + Med_num + Age + BMI + Group:Age + Group:BMI + (1 | AN_cc$Subject), data=AN_cc)

RVVccLagM <- lmer(RVVccLag$Lag_At_Max_RVVcc ~ Dose * Group + Med_num + Age + BMI + Group:Age + Group:BMI + (1 | RVVccLag$Subject), data=RVVccLag)

RVTccM <- lmer(AN_cc$RVTcc_at_Zero_Lag ~ Dose * Group + Med_num + Age + BMI + Group:Age + Group:BMI + (1 | AN_cc$Subject), data=AN_cc)

RVTccMaxM <- lmer(AN_cc$Max_RVTcc ~ Dose * Group + Med_num + Age + BMI + Group:Age + Group:BMI + (1 | AN_cc$Subject), data=AN_cc)

RVTccLagM <- lmer(RVTccLag$Lag_At_Max_RVTcc ~ Dose * Group + Med_num + Age + BMI + Group:Age + Group:BMI + (1 | RVTccLag$Subject), data=RVTccLag)

summary(HRccM)
summary(HRccMaxM)
summary(HRccLagM)
summary(RVVccM)
summary(RVVccMaxM)
summary(RVVccLagM)
summary(RVTccM)
summary(RVTccMaxM)
summary(RVTccLagM)
confint(HRccM)
confint(HRccMaxM)
confint(HRccLagM)
confint(RVVccM)
confint(RVVccMaxM)
confint(RVVccLagM)
confint(RVTccM)
confint(RVTccMaxM)
confint(RVTccLagM)

summary(lmeHRcc)
summary(lmeHRccMax)
summary(lmeHRccLag)
summary(lmeRVVcc)
summary(lmeRVVccMax)
summary(lmeRVVccLag)
summary(lmeRVTcc)
summary(lmeRVTccMax)
summary(lmeRVTccLag)
confint(lmeHRcc)
confint(lmeHRccMax)
confint(lmeHRccLag)
confint(lmeRVVcc)
confint(lmeRVVccMax)
confint(lmeRVVccLag)
confint(lmeRVTcc)
confint(lmeRVTccMax)
confint(lmeRVTccLag)

3dMVM -prefix MVM_output -jobs 6 -bsVars group*study.site -SS_type 2 -num_glt 1 -gltLabel 1 disease_vs_no.disease -gltCode 1 'group : 1*disease - 1*no.disease' -dataTable @datatable.nifti.txt

3dMVM -prefix AN_MVM -jobs 6												\
-bsVars Group -wsvars Dose -SS_type 3 -num_glt							\
-gltLabel 1 GAD_vs_HC -gltCode 1 'Group : 1*GAD - 1*HC'						\
-gltLabel 2 Dose -gltCode 2 'Dose: 1*pFive - 1*Saline & 1*Two - 1*Saline'	\

 -dataTable @datatable.nifti.txt

3dMVM -prefix AN_MVM -jobs 6												\
-bsVars Med -wsvars Dose -SS_type 3 -num_glt							\
-gltLabel 1 Med_stat -gltCode 1 'Med : 1*Med - 1*HC'					\
-gltLabel 2 Dose -gltCode 2 'Dose: 1*pFive - 1*Saline & 1*Two - 1*Saline'	\

 -dataTable @datatable.nifti.txt

3dMVM -prefix AN_MVM -jobs 6												\
-bsVars Group -wsvars Dose*Period -SS_type 3 -num_glt							\
-gltLabel 1 GAD_vs_HC -gltCode 1 'Group : 1*GAD - 1*HC'						\
-gltLabel 2 Dose -gltCode 2 'Dose: 1*pFive - 1*Saline & 1*Two - 1*Saline'	\
-gltLabel 3 Dose -gltCode 3 'Period: 1*Ant - 1* & 1*Two - 1*Saline'	\

 -dataTable @datatable.nifti.txt

3dMVM -prefix AN_MVM -jobs 6												\
-bsVars Med -wsvars Dose -SS_type 3 -num_glt							\
-gltLabel 1 Med_stat -gltCode 1 'Med : 1*Med - 1*HC'					\
-gltLabel 2 Dose -gltCode 2 'Dose: 1*pFive - 1*Saline & 1*Two - 1*Saline'	\

 -dataTable @datatable.nifti.txt
 

ANX_SE <- summarySE(data=ANXomit, measurevar=c("Anxious"), c("Group","Dose_label"))
grp <- factor(ANX_SE$Group,level=c("HC","GAD"))
#dose <- factor(ANX_SE$Dose,level=c("Sal","pFive","Two"))
dose <- factor(ANX_SE$Dose_label,levels=c("0","0.5","2"))

plotANXold <- ggplot(data=ANX_SE, aes(x=dose, y=Anxious, group=Group, colour=Group)) +
geom_errorbar(size=1, width=.1, aes(ymin=Anxious-se, ymax=Anxious+se)) +
geom_line(size=1)+
geom_point(size=2)+
ylim(0,10)+
labs(y = "Mean Rating", x = "Dose")+
ggtitle("How Anxious/Tense/Nervous")+
theme_bw()+
theme(plot.title = element_text(hjust = 0.5), text = element_text(size=20))


library(ggplot2)
library(ggforce)


Group <- AN_ANXomit$Group
AN_plotANX <- ggplot(data=AN_ANXomit, aes(x=AN_ANXomit$Dose, y=AN_ANXomit$Anxious, fill=Group)) +
    labs(y = "Anxiety")+
    theme_bw()+
    theme(text=element_text(family="Arial", size=11))+
	theme(plot.title = element_text(hjust = 0.5), text = element_text(size=11))+
    geom_sina(aes(color=Group), alpha=0.3) +
	geom_boxplot(position=position_dodge(0.9), alpha=0.2, outlier.alpha = 0, width=0.2)+
	scale_fill_manual(values=c("#6666FFFF", "#339900FF"),name = "Group", labels = c("HC", "GAD"))+
    scale_color_manual(values=c("#6666FFFF", "#339900FF"),name = "Group", labels = c("HC", "GAD"))+
	theme(legend.position="none")+
	theme(axis.title.x = element_blank())+
	scale_x_discrete(limit=c("Placebo", "Point5", "Two"), labels=c("Saline", "0.5 µg", "2.0 µg"))
	
Group <- AN_HBIomit$Group
AN_plotHBI <- ggplot(data=AN_HBIomit, aes(x=AN_HBIomit$Dose, y=AN_HBIomit$HBI, fill=Group)) +
    labs(y = "Heart Beat Intensity")+
    theme_bw()+
    theme(text=element_text(family="Arial", size=11))+
	theme(plot.title = element_text(hjust = 0.5), text = element_text(size=11))+
    geom_sina(aes(color=Group), alpha=0.3) +
	geom_boxplot(position=position_dodge(0.9), alpha=0.2, outlier.alpha = 0, width=0.2)+
	scale_fill_manual(values=c("#6666FFFF", "#339900FF"),name = "Group", labels = c("HC", "GAD"))+
    scale_color_manual(values=c("#6666FFFF", "#339900FF"),name = "Group", labels = c("HC", "GAD"))+
	theme(legend.position="none")+
	theme(axis.title.x = element_blank())+
	scale_x_discrete(limit=c("Placebo", "Point5", "Two"), labels=c("Saline", "0.5 µg", "2.0 µg"))

Group <- AN_HAPomit$Group
AN_plotHAP <- ggplot(data=AN_HAPomit, aes(x=AN_HAPomit$Dose, y=AN_HAPomit$Happy, fill=Group)) +
    labs(y = "Happy/Excited")+
    theme_bw()+
    theme(text=element_text(family="Arial", size=11))+
	theme(plot.title = element_text(hjust = 0.5), text = element_text(size=11))+
    geom_sina(aes(color=Group), alpha=0.3) +
	geom_boxplot(position=position_dodge(0.9), alpha=0.2, outlier.alpha = 0, width=0.2)+
	scale_fill_manual(values=c("#6666FFFF", "#339900FF"),name = "Group", labels = c("HC", "GAD"))+
    scale_color_manual(values=c("#6666FFFF", "#339900FF"),name = "Group", labels = c("HC", "GAD"))+
	theme(legend.position="none")+
	theme(axis.title.x = element_blank())+
	scale_x_discrete(limit=c("Placebo", "Point5", "Two"), labels=c("Saline", "0.5 µg", "2.0 µg"))

Group <- AN_RIomit$Group
AN_plotRI <- ggplot(data=AN_RIomit, aes(x=AN_RIomit$Dose, y=AN_RIomit$RI, fill=Group)) +
    labs(y = "Respiratory Intensity")+
    theme_bw()+
    theme(text=element_text(family="Arial", size=11))+
    theme(plot.title = element_text(hjust = 0.5), text = element_text(size=11))+
    geom_sina(aes(color=Group), alpha=0.3) +
    geom_boxplot(position=position_dodge(0.9), alpha=0.2, outlier.alpha = 0, width=0.2)+
    scale_fill_manual(values=c("#6666FFFF", "#339900FF"),name = "Group", labels = c("HC", "GAD"))+
    scale_color_manual(values=c("#6666FFFF", "#339900FF"),name = "Group", labels = c("HC", "GAD"))+
    theme(legend.position=c(0.125,0.73), legend.title=element_text(size=9),legend.text=element_text(size=8))+
    theme(axis.title.x = element_blank())+
    scale_x_discrete(limit=c("Placebo", "Point5", "Two"), labels=c("Saline", "0.5 µg", "2.0 µg"))

tiff("Retro&CD25.tiff", units="mm", width=180, height=100, res=400)
(plotHBI + plotRI)/(plotCD25 + plotANX)
dev.off()

Group <- CD25$Group
plotCD25 <- ggplot(data=CD25, aes(x=Group, y=CD25$CD_rest_win, fill=Group)) +
    labs(y = "CD25")+
    theme_bw()+
    theme(text=element_text(family="Arial", size=10))+
	theme(plot.title = element_text(hjust = 0.5), text = element_text(size=10)) +
    geom_violin(aes(color=Group), alpha=0.2) +
    geom_sina(aes(color=Group),size=2, alpha=0.15) +
	geom_boxplot(position=position_dodge(0.9), alpha=0.2, outlier.alpha = 0, width=0.2)+
	scale_fill_manual(values=c("#6666FFFF", "#339900FF"),name = "Group", labels = c("HC", "GAD"))+
    scale_color_manual(values=c("#6666FFFF", "#339900FF"),name = "Group", labels = c("HC", "GAD"))+
	theme(legend.position="none")+
	theme(axis.title.x = element_blank())+
	scale_x_discrete(limit=c("HC", "MA"), labels=c("HC", "GAD"))




Group <- ANXomit$Group
plotANX <- ggplot(data=ANXomit, aes(x=ANXomit$Dose, y=ANXomit$Anxious, fill=Group)) +
    labs(y = "Anxiety")+
    theme_bw()+
    theme(text=element_text(family="Arial", size=11))+
	theme(plot.title = element_text(hjust = 0.5), text = element_text(size=11))+
    geom_sina(aes(color=Group), alpha=0.3) +
	geom_boxplot(position=position_dodge(0.9), alpha=0.2, outlier.alpha = 0, width=0.2)+
	scale_fill_manual(values=c("#6666FFFF", "#339900FF"),name = "Group", labels = c("HC", "GAD"))+
    scale_color_manual(values=c("#6666FFFF", "#339900FF"),name = "Group", labels = c("HC", "GAD"))+
	theme(legend.position="none")+
	theme(axis.title.x = element_blank())+
	scale_x_discrete(limit=c("Placebo", "Point5", "Two"), labels=c("Saline", "0.5 µg", "2.0 µg"))
	
Group <- HBIomit$Group
plotHBI <- ggplot(data=HBIomit, aes(x=HBIomit$Dose, y=HBIomit$HBI, fill=Group)) +
    labs(y = "Heart Beat Intensity")+
    theme_bw()+
    theme(text=element_text(family="Arial", size=11))+
	theme(plot.title = element_text(hjust = 0.5), text = element_text(size=11))+
    geom_sina(aes(color=Group), alpha=0.3) +
	geom_boxplot(position=position_dodge(0.9), alpha=0.2, outlier.alpha = 0, width=0.2)+
	scale_fill_manual(values=c("#6666FFFF", "#339900FF"),name = "Group", labels = c("HC", "GAD"))+
    scale_color_manual(values=c("#6666FFFF", "#339900FF"),name = "Group", labels = c("HC", "GAD"))+
	theme(legend.position="none")+
	theme(axis.title.x = element_blank())+
	scale_x_discrete(limit=c("Placebo", "Point5", "Two"), labels=c("Saline", "0.5 µg", "2.0 µg"))
	
Group <- RIomit$Group
plotRI <- ggplot(data=RIomit, aes(x=RIomit$Dose, y=RIomit$RI, fill=Group)) +
    labs(y = "Respiratory Intensity")+
    theme_bw()+
    theme(text=element_text(family="Arial", size=11))+
	theme(plot.title = element_text(hjust = 0.5), text = element_text(size=11))+
    geom_sina(aes(color=Group), alpha=0.3) +
	geom_boxplot(position=position_dodge(0.9), alpha=0.2, outlier.alpha = 0, width=0.2)+
	scale_fill_manual(values=c("#6666FFFF", "#339900FF"),name = "Group", labels = c("HC", "GAD"))+
    scale_color_manual(values=c("#6666FFFF", "#339900FF"),name = "Group", labels = c("HC", "GAD"))+
    theme(legend.position=c(0.125,0.73), legend.title=element_text(size=9),legend.text=element_text(size=8))+
	theme(axis.title.x = element_blank())+
	scale_x_discrete(limit=c("Placebo", "Point5", "Two"), labels=c("Saline", "0.5 µg", "2.0 µg"))


Group <- CD25$Group
plotCD25 <- ggplot(data=CD25, aes(x=Group, y=CD25$CD_rest_win, fill=Group)) +
    labs(y = "CD25")+
    theme_bw()+
    theme(text=element_text(family="Arial", size=1))+
	theme(plot.title = element_text(hjust = 0.5), text = element_text(size=11)) +
    geom_violin(aes(color=Group), alpha=0.2) +
    geom_sina(aes(color=Group),size=2, alpha=0.15) +
	geom_boxplot(position=position_dodge(0.9), alpha=0.2, outlier.alpha = 0, width=0.2)+
	scale_fill_manual(values=c("#6666FFFF", "#339900FF"),name = "Group", labels = c("HC", "GAD"))+
    scale_color_manual(values=c("#6666FFFF", "#339900FF"),name = "Group", labels = c("HC", "GAD"))+
	theme(legend.position="none")+
	theme(axis.title.x = element_blank())+
	scale_x_discrete(limit=c("HC", "MA"), labels=c("HC", "GAD"))


geom_errorbar(size=1, width=.1, aes(ymin=RI-se, ymax=RI+se)) +
geom_line(size=1)+
geom_point(size=2)+
ylim(0,10)+
labs(y = "Mean Rating", x = "Dose")+
ggtitle("Respiratory Intensity")+
theme_bw()+
theme(plot.title = element_text(hjust = 0.5), text = element_text(size=20))


plotHBI <- ggplot(data=HBI_SE, aes(x=dose, y=HBI, group=Group, colour=Group)) +
geom_errorbar(size=1, width=.1, aes(ymin=HBI-se, ymax=HBI+se)) +
geom_line(size=1)+
geom_point(size=2)+
ylim(0,10)+
labs(y = "Mean Rating", x = "Dose")+
ggtitle("Heart Beat Intensity")+
theme_bw()+
theme(plot.title = element_text(hjust = 0.5), text = element_text(size=20))



RI_SE <- summarySE(data=RIomit, measurevar=c("RI"), c("Group","Dose_label"))
grp <- factor(RI_SE$Group,level=c("HC","GAD"))
#dose <- factor(RI_SE$Dose,level=c("Sal","pFive","Two"))
dose <- factor(RI_SE$Dose_label,levels=c("0","0.5","2"))

plotRI <- ggplot(data=RI_SE, aes(x=dose, y=RI, group=Group, colour=Group)) +
geom_errorbar(size=1, width=.1, aes(ymin=RI-se, ymax=RI+se)) +
geom_line(size=1)+
geom_point(size=2)+
ylim(0,10)+
labs(y = "Mean Rating", x = "Dose")+
ggtitle("Respiratory Intensity")+
theme_bw()+
theme(plot.title = element_text(hjust = 0.5), text = element_text(size=20))

HCMA <- t.test(GAD$Age,HC$Age)

HBI_SE <- summarySE(data=HBIomit, measurevar=c("HBI"), c("Group","Dose_label"))
grp <- factor(HBI_SE$Group,level=c("HC","GAD"))
#dose <- factor(HBI_SE$Dose,level=c("Sal","pFive","Two"))
dose <- factor(HBI_SE$Dose_label,levels=c("0","0.5","2"))

plotHBI <- ggplot(data=HBI_SE, aes(x=dose, y=HBI, group=Group, colour=Group)) +
geom_errorbar(size=1, width=.1, aes(ymin=HBI-se, ymax=HBI+se)) +
geom_line(size=1)+
geom_point(size=2)+
ylim(0,10)+
labs(y = "Mean Rating", x = "Dose")+
ggtitle("Heart Beat Intensity")+
theme_bw()+
theme(plot.title = element_text(hjust = 0.5), text = element_text(size=20))



ISO_vmPFC1_SE <- summarySE(data=data, measurevar=c("vmPFC1"), groupvars=c("Group","Dose"))
plotvmPFC1 <- ggplot(data=ISO_vmPFC1_SE, aes(x=Dose, y=vmPFC1, fill=Group)) +
geom_bar(stat="identity", position=position_dodge()) +
geom_errorbar(position=position_dodge(.9), width=.25, aes(ymin=ISO_vmPFC1_SE$vmPFC1-se, ymax=ISO_vmPFC1_SE$vmPFC1+se)) +
labs(y = "vmPFC FPC1")+
theme_minimal()

ISO_vmPFC2_SE <- summarySE(data=data, measurevar=c("vmPFC2"), groupvars=c("Group","Dose"))
plotvmPFC2 <- ggplot(data=ISO_vmPFC2_SE, aes(x=Dose, y=vmPFC2, fill=Group)) +
geom_bar(stat="identity", position=position_dodge()) +
geom_errorbar(position=position_dodge(.9), width=.25, aes(ymin=ISO_vmPFC2_SE$vmPFC2-se, ymax=ISO_vmPFC2_SE$vmPFC2+se)) +
labs(y = "vmPFC FPC2")+
theme_minimal()
plotvmPFC1
plotvmPFC2

ISO_dACC1_SE <- summarySE(data=data, measurevar=c("dACC1"), groupvars=c("Group","Dose"))
plotdACC1 <- ggplot(data=ISO_dACC1_SE, aes(x=Dose, y=dACC1, fill=Group)) +
geom_bar(stat="identity", position=position_dodge()) +
geom_errorbar(position=position_dodge(.9), width=.25, aes(ymin=ISO_dACC1_SE$dACC1-se, ymax=ISO_dACC1_SE$dACC1+se)) +
labs(y = "dACC FPC1")+
theme_minimal()
#dev.off()

ISO_dACC2_SE <- summarySE(data=data, measurevar=c("dACC2"), groupvars=c("Group","Dose"))
plotdACC2 <- ggplot(data=ISO_dACC2_SE, aes(x=Dose, y=dACC2, fill=Group)) +
geom_bar(stat="identity", position=position_dodge()) +
geom_errorbar(position=position_dodge(.9), width=.25, aes(ymin=ISO_dACC2_SE$dACC2-se, ymax=ISO_dACC2_SE$dACC2+se)) +
labs(y = "dACC FPC2")+
theme_minimal()
#dev.off()


ISO_L_Pins1_SE <- summarySE(data=data, measurevar=c("L_Pins1"), groupvars=c("Group","Dose"))
plotL_Pins1 <- ggplot(data=ISO_L_Pins1_SE, aes(x=Dose, y=L_Pins1, fill=Group)) +
geom_bar(stat="identity", position=position_dodge()) +
geom_errorbar(position=position_dodge(.9), width=.25, aes(ymin=ISO_L_Pins1_SE$L_Pins1-se, ymax=ISO_L_Pins1_SE$L_Pins1+se)) +
labs(y = "Left Posterior Insula FPC1")+
theme_minimal()
#dev.off()

ISO_L_Pins2_SE <- summarySE(data=data, measurevar=c("L_Pins2"), groupvars=c("Group","Dose"))
plotR_Pins2 <- ggplot(data=ISO_L_Pins2_SE, aes(x=Dose, y=L_Pins2, fill=Group)) +
geom_bar(stat="identity", position=position_dodge()) +
geom_errorbar(position=position_dodge(.9), width=.25, aes(ymin=ISO_L_Pins2_SE$L_Pins2-se, ymax=ISO_L_Pins2_SE$L_Pins2+se)) +
labs(y = "Left Posterior Insula FPC2")+
theme_minimal()
#dev.off()


ISO_R_Pins1_SE <- summarySE(data=data, measurevar=c("R_Pins1"), groupvars=c("Group","Dose"))
plotR_Pins1 <- ggplot(data=ISO_R_Pins1_SE, aes(x=Dose, y=R_Pins1, fill=Group)) +
geom_bar(stat="identity", position=position_dodge()) +
geom_errorbar(position=position_dodge(.9), width=.25, aes(ymin=ISO_R_Pins1_SE$R_Pins1-se, ymax=ISO_R_Pins1_SE$R_Pins1+se)) +
labs(y = "Right Posterior Insula FPC1")+
theme_minimal()
#dev.off()

ISO_R_Pins2_SE <- summarySE(data=data, measurevar=c("R_Pins2"), groupvars=c("Group","Dose"))
plotR_Pins2 <- ggplot(data=ISO_R_Pins2_SE, aes(x=Dose, y=R_Pins2, fill=Group)) +
geom_bar(stat="identity", position=position_dodge()) +
geom_errorbar(position=position_dodge(.9), width=.25, aes(ymin=ISO_R_Pins2_SE$R_Pins2-se, ymax=ISO_R_Pins2_SE$R_Pins2+se)) +
labs(y = "Right Posterior Insula FPC2")+
theme_minimal()
#dev.off()


ISO_L_Ains1_SE <- summarySE(data=data, measurevar=c("L_Ains1"), groupvars=c("Group","Dose"))
plotL_Ains1 <- ggplot(data=ISO_L_Ains1_SE, aes(x=Dose, y=L_Ains1, fill=Group)) +
geom_bar(stat="identity", position=position_dodge()) +
geom_errorbar(position=position_dodge(.9), width=.25, aes(ymin=ISO_L_Ains1_SE$L_Ains1-se, ymax=ISO_L_Ains1_SE$L_Ains1+se)) +
labs(y = "Left Anterior Insula FPC1")+
theme_minimal()
#dev.off()

ISO_L_Ains2_SE <- summarySE(data=data, measurevar=c("L_Ains2"), groupvars=c("Group","Dose"))
plotL_Ains2 <- ggplot(data=ISO_L_Ains2_SE, aes(x=Dose, y=L_Ains2, fill=Group)) +
geom_bar(stat="identity", position=position_dodge()) +
geom_errorbar(position=position_dodge(.9), width=.25, aes(ymin=ISO_L_Ains2_SE$L_Ains2-se, ymax=ISO_L_Ains2_SE$L_Ains2+se)) +
labs(y = "Left Anterior Insula FPC2")+
theme_minimal()
#dev.off()


ISO_R_Ains1_SE <- summarySE(data=data, measurevar=c("R_Ains1"), groupvars=c("Group","Dose"))
plotR_Ains1 <- ggplot(data=ISO_R_Ains1_SE, aes(x=Dose, y=R_Ains1, fill=Group)) +
geom_bar(stat="identity", position=position_dodge()) +
geom_errorbar(position=position_dodge(.9), width=.25, aes(ymin=ISO_R_Ains1_SE$R_Ains1-se, ymax=ISO_R_Ains1_SE$R_Ains1+se)) +
labs(y = "Right Anterior Insula FPC1")+
theme_minimal()
#dev.off()

ISO_R_Ains2_SE <- summarySE(data=data, measurevar=c("R_Ains2"), groupvars=c("Group","Dose"))
plotR_Ains2 <- ggplot(data=ISO_R_Ains2_SE, aes(x=Dose, y=R_Ains2, fill=Group)) +
geom_bar(stat="identity", position=position_dodge()) +
geom_errorbar(position=position_dodge(.9), width=.25, aes(ymin=ISO_R_Ains2_SE$R_Ains2-se, ymax=ISO_R_Ains2_SE$R_Ains2+se)) +
labs(y = "Right Anterior Insula FPC2")+
theme_minimal()
#dev.off()

ISO_Amy1_SE <- summarySE(data=data, measurevar=c("Amy1"), groupvars=c("Group","Dose"))
plotAmy1 <- ggplot(data=ISO_Amy1_SE, aes(x=Dose, y=Amy1, fill=Group)) +
geom_bar(stat="identity", position=position_dodge()) +
geom_errorbar(position=position_dodge(.9), width=.25, aes(ymin=ISO_Amy1_SE$Amy1-se, ymax=ISO_Amy1_SE$Amy1+se)) +
labs(y = "Amygdala FPC1")+
theme_minimal()
#dev.off()

ISO_Amy2_SE <- summarySE(data=data, measurevar=c("Amy2"), groupvars=c("Group","Dose"))
plotAmy2 <- ggplot(data=ISO_Amy2_SE, aes(x=Dose, y=Amy2, fill=Group)) +
geom_bar(stat="identity", position=position_dodge()) +
geom_errorbar(position=position_dodge(.9), width=.25, aes(ymin=ISO_Amy2_SE$Amy2-se, ymax=ISO_Amy2_SE$Amy2+se)) +
labs(y = "Amygdala FPC2")+
theme_minimal()
#dev.off()


ISO_Caudate1_SE <- summarySE(data=data, measurevar=c("Caudate1"), groupvars=c("Group","Dose"))
plotCaudate1 <- ggplot(data=ISO_Caudate1_SE, aes(x=Dose, y=Caudate1, fill=Group)) +
geom_bar(stat="identity", position=position_dodge()) +
geom_errorbar(position=position_dodge(.9), width=.25, aes(ymin=ISO_Caudate1_SE$Caudate1-se, ymax=ISO_Caudate1_SE$Caudate1+se)) +
labs(y = "Caudate FPC1")+
theme_minimal()
#dev.off()

ISO_Caudate2_SE <- summarySE(data=data, measurevar=c("Caudate2"), groupvars=c("Group","Dose"))
plotCaudate2 <- ggplot(data=ISO_Caudate2_SE, aes(x=Dose, y=Caudate2, fill=Group)) +
geom_bar(stat="identity", position=position_dodge()) +
geom_errorbar(position=position_dodge(.9), width=.25, aes(ymin=ISO_Caudate2_SE$Caudate2-se, ymax=ISO_Caudate2_SE$Caudate2+se)) +
labs(y = "Caudate FPC2")+
theme_minimal()
#dev.off()


ISO_dmPFC1_SE <- summarySE(data=data, measurevar=c("dmPFC1"), groupvars=c("Group","Dose"))
plotdmPFC1 <- ggplot(data=ISO_dmPFC1_SE, aes(x=Dose, y=dmPFC1, fill=Group)) +
geom_bar(stat="identity", position=position_dodge()) +
geom_errorbar(position=position_dodge(.9), width=.25, aes(ymin=ISO_dmPFC1_SE$dmPFC1-se, ymax=ISO_dmPFC1_SE$dmPFC1+se)) +
labs(y = "dmPFC FPC1")+
theme_minimal()
#dev.off()

ISO_dmPFC2_SE <- summarySE(data=data, measurevar=c("dmPFC2"), groupvars=c("Group","Dose"))
plotdmPFC2 <- ggplot(data=ISO_dmPFC2_SE, aes(x=Dose, y=dmPFC2, fill=Group)) +
geom_bar(stat="identity", position=position_dodge()) +
geom_errorbar(position=position_dodge(.9), width=.25, aes(ymin=ISO_dmPFC2_SE$dmPFC2-se, ymax=ISO_dmPFC2_SE$dmPFC2+se)) +
labs(y = "dmPFC FPC2")+
theme_minimal()
#dev.off()


ISO_PCC1_SE <- summarySE(data=data, measurevar=c("PCC1"), groupvars=c("Group","Dose"))
plotPCC1 <- ggplot(data=ISO_PCC1_SE, aes(x=Dose, y=PCC1, fill=Group)) +
geom_bar(stat="identity", position=position_dodge()) +
geom_errorbar(position=position_dodge(.9), width=.25, aes(ymin=ISO_PCC1_SE$PCC1-se, ymax=ISO_PCC1_SE$PCC1+se)) +
labs(y = "PCC/Precuneus FPC1")+
theme_minimal()
#dev.off()

ISO_PCC2_SE <- summarySE(data=data, measurevar=c("PCC2"), groupvars=c("Group","Dose"))
plotPCC2 <- ggplot(data=ISO_PCC2_SE, aes(x=Dose, y=PCC2, fill=Group)) +
geom_bar(stat="identity", position=position_dodge()) +
geom_errorbar(position=position_dodge(.9), width=.25, aes(ymin=ISO_PCC2_SE$PCC2-se, ymax=ISO_PCC2_SE$PCC2+se)) +
labs(y = "PCC/Precuneus FPC2")+
theme_minimal()
#dev.off()