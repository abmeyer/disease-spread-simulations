#This is a simulation of disease transmission in a small community where a subset of the population regularly engages in a risky behavior.

#install.packages("scales")
library(scales)

#this is the probability that a sick person stays sick each day
staySickPerc<-.95

#this is the probability that a sick person transmits the disease if he encounters and uninfected person
transPerc<-.125

#this is the number of periods that the simulation runs
periods=100

#this is the number of people in the town
n=32^2

#this sets the base that determines how far each person each person travels every day
travelProb<-.1

#this si the number of people engaging in a particularly risky activity
nJiu=round(n*.05)

#this is the number of people who start out sick in period 1
nSick=1

#this is the number of worlds to simulate
sims=5000

#these are holders for simulation results
regJiuPercSick3<-matrix(NA,ncol=periods,nrow=sims)
regJiuPercRecovered3<-matrix(NA,ncol=periods,nrow=sims)

#this block calculates distance (L2 norm) between each person in the town
distX<-abs(matrix(1:n^(1/2),nrow=n,ncol=n,byrow=T)-(1:n^(1/2)) )
distY<-abs(matrix(rep(1:n^(1/2),each=n^(1/2)),nrow=n,ncol=n,byrow=T)-rep(1:n^(1/2),each=n^(1/2)))
dist<-(distX^2+distY^2)^.5

#this turns distances into probabilities of encounter in each period
weights<-travelProb^dist
diag(weights)<-0
weights<-apply(weights,2,function(x)x/sum(x))

par(las=1)
plot(main="Percentage of the population currently infected",x=1,y=1,ylim=c(0,1),xlim=c(0,periods),type="n",bty="n",pch=19,xlab="days since first case", ylab="",yaxt="n")
axis(side=2, at=seq(0,1,.2),labels=paste(seq(0,100,20),"%",sep=""))
#axis(side=1, at=c(1,100,200,300,365))

for (s in 1:sims){

	sick<-sample(rep(c(1,0),c(nSick,n-nSick)),replace=F)
	jius<-sample(size=50,1:n,replace=F)
	recovered<-rep(0,n)

	for (i in 1:periods){

		sicks<-which(sick==1)

		for (j in sicks){

			weight<-weights[,j]
			oth<-sample(size=1,(1:n),prob=weight)
			othRec<-recovered[oth]
			othSick<-sick[oth]

			if (othRec==0&othSick==0){
				sick[oth]=rbinom(1,1,transPerc)
			} 

			if (j %in% jius){
				sick[jius]<-1-recovered[jius]
			}


			if (rbinom(1,1,staySickPerc)==0){
				recovered[j]<-1
				sick[j]<-0
			}


		}

		regJiuPercSick3[s,i]<-mean(sick)
		regJiuPercRecovered3[s,i]<-mean(recovered)

	}

	print(s)
	points(x=1:periods,y=regJiuPercSick3[s,],pch=19,type="l",col=alpha(rgb(0,0,0),.01),lty=1)

}






