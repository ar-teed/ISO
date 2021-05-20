message("Begin script")
#  mFPCA MCMC.bash
#
#  Modified by Adam Teed on 07/21/2020.
#
rm(list=ls())

#############################################
#############################################
##	          PACKAGES                     ##
#############################################
#############################################

library(Matrix)
library(nlme)

#############################################
#############################################
##	         FUNCTIONS                     ##
#############################################
#############################################

source("basis_sparse_location_spec.R")
source("mfpca_knot_located.R")

spaghetti <- function(y,times,ids,ylab="y",xlab="times",
			xlim=c(min(times,na.rm=T),max(times,na.rm=T)),
			ylim=c(min(y,na.rm=T),max(y,na.rm=T)),
			cols=rep(1,length(unique(ids))),lwd=1){
	plot(times,y,type="n",xlim=xlim,ylim=ylim,xlab=xlab,ylab=ylab)
	for(i in 1:length(unique(ids))){
		if(length(y[ids==unique(ids)[i]])==1) lines(times[ids==unique(ids)[i]],
			y[ids==unique(ids)[i]],type="p",col=cols[i],pch=15,cex=.25)
		if(length(y[ids==unique(ids)[i]])>1) lines(times[ids==unique(ids)[i]],
			y[ids==unique(ids)[i]],type="l",col=cols[i],lwd=lwd)
	}
}

######################################################
######################################################
##	     READ IN, MANIPULATE, AND PLOT DATA         ##
######################################################
######################################################

##################
# Read in data   #
##################

#list.files("data/")

ISO = read.csv("data/ISO_2auto_phys_dial.csv", header=TRUE)
#ISO = ISO[complete.cases(ISO),]

group <- factor(ISO$Group,levels=c("HC","GAD"))
dose <- factor(ISO$Dose,levels=c("Sal","pFive","Two"))
#run <- factor(data$Run,levels=c("1st","2nd"))
time = 5:120
n = length(unique(ISO[,1]))

ISO$Self = scale(ISO$Self)
ISO$HR = scale(ISO$HR)
ISO$Dial = scale(ISO$Dial)
message("Self,HR,Dial") 

data = ISO

#3 is number of doses. We have 1 timeseries for each dose per person.
cnd = n*3
id = rep(1:cnd, each = length(time))
data$id=id
names(data)
dim(data)

data$time = (data$TR - min(data$TR)+1)/(1+max(data$TR)- min(data$TR))

#######################
#######################
## Select variables  ##
#######################
#######################

data$y1 = data$Self
data$y2 = data$HR
data$y3 = data$Dial
ind_y = c(which(names(data)=="y1"),which(names(data)=="y2"),which(names(data)=="y3"))

Y_sparse=list()
time_sparse=list()
N = max(data$id)
for(i in 1:N){
	Y_sparse[[i]]=list()
	Y_sparse[[i]][[1]]=data$y1[data$id==i & !is.na(data$y1)]
	Y_sparse[[i]][[2]]=data$y2[data$id==i & !is.na(data$y2)]
	Y_sparse[[i]][[3]]=data$y3[data$id==i & !is.na(data$y3)]
	time_sparse[[i]]=list()
	time_sparse[[i]][[1]]=data$time[data$id==i & !is.na(data$y1)]
	time_sparse[[i]][[2]]=data$time[data$id==i & !is.na(data$y2)]
	time_sparse[[i]][[3]]=data$time[data$id==i & !is.na(data$y3)]
}
N=length(Y_sparse)
group=rep(1,N)
save.image(file="Self_HR_Dial_455_mfpca.Rdata")

###################
#   Run model     #
###################

#rm(list=ls())

source("basis_sparse_location_spec.R")
source("mfpca_knot_located.R")
load("Self_HR_Dial_455_mfpca.Rdata")

Q=c(2,2,2) #number of PCs per variable
nknots=c(4,5,5) # number of basis functions, 1 knot = 5 basis functions (wiki) 
nloop=1100 #number of draws/iterations (samdata randomly in posterior distribution), will show in Y_sparse
burnin=100 # throwing away first burnin draws
thin=1
orth=TRUE
sim=FALSE

mfpca_mcmc_outputs=mfpca_knot_located(Y_sparse=Y_sparse,time_sparse=time_sparse,
	group=group,Q=Q,nknots=nknots,nloop=nloop,burnin=burnin,
	thin=thin,orth=orth,sim=sim)
save.image(file="mcmc_455_Self_HR_Dial.Rdata")

######################
######################
##  CHECK RESULTS   ##
######################
######################

#rm(list=ls())
load("mcmc_455_Self_HR_Dial.Rdata")

# extract posterior draws from mfpca run
ALPHA_array=mfpca_mcmc_outputs[[1]] #factor scores for individuals, grouped by # of draws
MU_array=mfpca_mcmc_outputs[[2]] # mean curve for the entire sample of each samdata session
D_array=mfpca_mcmc_outputs[[3]] #
R_array=mfpca_mcmc_outputs[[4]] #correlation matrix of each samdata 
SIGMA_OMEGA_array=mfpca_mcmc_outputs[[5]] #error term; variance of the fit
THETA_array=mfpca_mcmc_outputs[[6]] #each factor's loading on each basis function
basis_stuff=mfpca_mcmc_outputs[[7]]
knots=basis_stuff[[1]]
time_sparse=basis_stuff[[2]]
phi_t=basis_stuff[[3]]
time_cont=basis_stuff[[4]]
phi_t_cont=basis_stuff[[5]]

nloop=length(ALPHA_array)
first=1
last=nloop

# setting the x- and y-axis limit for plotting
P = length(Y_sparse[[1]])

Y_min = list()
Y_max = list()
for(p in 1:P){
	Y_min[[p]]=min(Y_sparse[[1]][[p]])	
	Y_max[[p]]=max(Y_sparse[[1]][[p]])	
	for(i in 2:N){
		Y_min[[p]]=min(Y_min[[p]],min(Y_sparse[[i]][[p]]))
		Y_max[[p]]=max(Y_max[[p]],max(Y_sparse[[i]][[p]]))
	}
}

#plot mean functions
MU_mean=MU_array[[1]] #mean function across samdata sessions
R_mean=R_array[[1]] # mean correlation matrix across samples
R_q=array(NA,dim=c(R_mean[1],R_mean[2],length(R_array)))

ALPHA_mean=ALPHA_array[[1]] #mean factor scores
D_mean=D_array[[1]] 
THETA_mean=THETA_array[[first]] #mean factor loading
for(iter in 2:nloop){
	MU_mean=MU_mean+MU_array[[iter]]
	R_mean=R_mean+R_array[[iter]]
	for(r1 in 1:dim(R_mean)[1]){
		for(r2 in 1:dim(R_mean)[2]){
			
		}	
	}
	D_mean=D_mean+D_array[[iter]]
	ALPHA_mean=ALPHA_mean+ALPHA_array[[iter]]
	THETA_mean=THETA_mean+THETA_array[[iter]]
}
MU_mean=cbind(MU_mean/(last-first+1))

tmp = bdiag(cbind(phi_t_cont[[1]]), cbind(phi_t_cont[[2]]))
if(P>2){
	for(p in 3:P){
		tmp = bdiag(tmp,cbind(phi_t_cont[[p]]))
	}
}
Mu_functions=t(tmp)%*%MU_mean

Mu = list()
for(p in 1:P){
	Mu[[p]] = Mu_functions[((p-1)*length(time_cont)+1):(p*length(time_cont))]
}

R_mean=cbind(R_mean/(last-first+1))
ALPHA_mean=cbind(ALPHA_mean/(last-first+1))
D_mean=cbind(D_mean/(last-first+1))
THETA_mean=cbind(THETA_mean/(last-first+1))
SIGMA_ALPHA_mean=D_mean%*%R_mean%*%D_mean

FPC_mean = list()
ind1 = 1
ind2 = 1
for(p in 1:P){
	FPC_mean[[p]] = t(phi_t_cont[[p]])%*%THETA_mean[ind1:(ind1+nknots[p]+3),ind2:(ind2+Q[p]-1)]
	ind1 = ind1 + nknots[p]+4
	ind2 = ind2 + Q[p]
}

par(mfrow=c(1,P)) #allocating plotting space, 2 rows and 3 columns
for(p in 1:P){
	plot(time_cont,Mu[[p]],type="l",ylim=c(Y_min[[p]],Y_max[[p]]),xlim=c(0,1),lwd=5,col=4)
	for(i in 1:30){
		lines(time_sparse[[i]][[p]],Y_sparse[[i]][[p]],type="l",lwd=.25)
	}
	lines(time_cont,Mu[[p]],type="l",ylim=c(Y_min[[p]],Y_max[[p]]),lwd=5,col=4)
	title(main=names(data)[ind_y[p]])
}
#dev.off()

par(mfrow=c(1,P)) #allocating plotting space, 2 rows and 3 columns
for(p in 1:P){
	plot(time_cont,FPC_mean[[p]][,1],type="l",lwd=2,ylim=c(min(FPC_mean[[p]]),max(FPC_mean[[p]])))
	if(Q[p]==2) lines(time_cont,FPC_mean[[p]][,2],type="l",lwd=1)
	title(main=paste("FPCs for",names(data)[ind_y[p]]))
}
#dev.off()

# plotting fitted function for each individual
Fits_sparse=list()
for(i in 1:N){
	Fits_sparse[[i]]=list()
	ind1 = 1
	ind2 = 1
	for(p in 1:P){
		Fits_sparse[[i]][[p]]=t(phi_t[[p]][[i]])%*%cbind(MU_mean[ind1:(ind1+nknots[p]+3)])+
			t(phi_t[[p]][[i]])%*%cbind(THETA_mean[ind1:(ind1+nknots[p]+3),ind2:(ind2+Q[p]-1)])%*%cbind(ALPHA_mean[ind2:(ind2+Q[p]-1),i])
		ind1 = ind1 + nknots[p]+4
		ind2 = ind2 + Q[p]
	}
}

par(mfrow=c(2,P))
for(p in 1:P){
	plot(time_cont,Mu[[p]],type="l",ylim=c(Y_min[[p]],Y_max[[p]]),xlim=c(0,1),lwd=2,col=4)
	for(i in 1:30){
		lines(time_sparse[[i]][[p]],Y_sparse[[i]][[p]],type="l",lwd=.25)
	}
	title(main=paste("Raw",names(data)[ind_y[p]]))
}
for(p in 1:P){
	plot(time_cont,Mu[[p]],type="l",ylim=c(Y_min[[p]],Y_max[[p]]),xlim=c(0,1),lwd=2,col=4)
	for(i in 1:30){
		lines(time_sparse[[i]][[p]],Fits_sparse[[i]][[p]],type="l",lwd=.25)
	}
	title(main=paste("Raw",names(data)[ind_y[p]]))
}
#dev.off()

# Plot random subject's data and fits
ind=sample(1:N,1)
par(mfrow=c(1,P))
for(i in ind){
	ind1 = 1
	ind2 = 1
	Y_i=Y_sparse[[i]]
	times_i=time_sparse[[i]]
	Fitted_i=list()
	for(p in 1:P){
		Fitted_i[[p]]=t(phi_t[[p]][[i]])%*%cbind(MU_mean[ind1:(ind1+nknots[p]+3)])+t(phi_t[[p]][[i]])%*%
			cbind(THETA_mean[ind1:(ind1+nknots[p]+3),ind2:(ind2+Q[p]-1)])%*%cbind(ALPHA_mean[ind2:(ind2+Q[p]-1),i])
		ind1 = ind1 + nknots[p]+4
		ind2 = ind2 + Q[p]
		plot(time_cont,Mu[[p]],type="l",ylim=c(-2,3),xlim=c(0,1),lwd=2,col=4,xlab="st. time",ylab="st. score")
		lines(times_i[[p]],Y_i[[p]],type="l",col=4,lty=2)
		lines(times_i[[p]],Fitted_i[[p]],type="l",col=4)
		#points(times_i[[1]],Y_i[[1]],col=4)
		title(main=paste("Fitted",names(data)[ind_y[p]],"for subj",i))
		#dev.off()
	}
}

Fit_FPC_low = list()
Fit_FPC_hi = list()
ind1 = 1
ind2 = 1
ind3 = 1
for(p in 1:P){
	Fit_FPC_low[[p]] = list()
	Fit_FPC_hi[[p]] = list()
	for(q in 1:Q[p]){
		Fit_FPC_low[[p]][[q]]=t(phi_t_cont[[p]])%*%cbind(MU_mean[ind1:(ind1+nknots[p]+3)])+
				t(phi_t_cont[[p]])%*%cbind(THETA_mean[ind1:(ind1+nknots[p]+3),ind3])%*%
				(cbind(mean(ALPHA_mean[ind3,])+quantile(ALPHA_mean[ind3,],.05)))
		Fit_FPC_hi[[p]][[q]]=t(phi_t_cont[[p]])%*%cbind(MU_mean[ind1:(ind1+nknots[p]+3)])+
				t(phi_t_cont[[p]])%*%cbind(THETA_mean[ind1:(ind1+nknots[p]+3),ind3])%*%
				(cbind(mean(ALPHA_mean[ind3,])+quantile(ALPHA_mean[ind3,],.95)))
		ind3 = ind3 + 1
	}
	ind1 = ind1 + nknots[p]+4
	ind2 = ind2 + Q[p]		
}
par(mfrow=c(sum(Q),2))

par(mar=c(1,1,1,1)) #To prevent figure margins too wide error
for(p in 1:P){
	for(q in 1:Q[p]){
		plot(time_cont,FPC_mean[[p]][,q],type="l",lwd=2,ylim=c(min(FPC_mean[[p]][,q]),max(FPC_mean[[p]][,q])))
		title(main=paste("FPC", q, "for", names(data)[ind_y[p]]))
		plot(time_cont,Mu[[p]],type="l",ylim=c(-1,1),xlim=c(0,1),lwd=2,col=1)
		lines(time_cont,Fit_FPC_low[[p]][[q]],type="l",lwd=.5,lty=2,col=2)
		lines(time_cont,Fit_FPC_hi[[p]][[q]],type="l",lwd=.5,lty=2,col=3)
		title(main=paste("Effect of FPC",q, "for",names(data)[ind_y[p]]))
	}
}

## Correlation of PCs
R_mean 

