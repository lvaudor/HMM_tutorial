m=3
mu0=c(1,30,60)
theta0=c(1,2,3)
p=0.01
gamma0=matrix(rep(p,m^2),nrow=m)
diag(gamma0)=1-p*(m-1)

data= read.csv("C:/Documents and Settings/lvaudor/Bureau/tutoRial/TUTOS/HMM/tuto_HMM_dev/chapitres/data_original/donnees_herve_arrangees.csv", sep=";", dec=",")
pk=data$PK
x=data$nbre.embacle

HMM.fitted=NB.HMM.mle(x,m,mu0,theta0,gamma0)




xsim=NB.HMM.generate_sample(n=length(x),m,mu=HMM.fitted$mu,HMM.fitted$theta,gamma=HMM.fitted$gamma)

states0=NB.HMM.viterbi(x,m,mu0,theta0,gamma0)
states=NB.HMM.viterbi(x,m,HMM.fitted$mu,HMM.fitted$theta,HMM.fitted$gamma)


par(mar=c(4,4,1,1))

plot(pk,x,type="l", ylim=c(-5,230), ylab="nb of wood rafts")


plot(pk,x,type="l", ylim=c(-5,230), ylab="nb of wood rafts", lty=2)
points(pk,HMM.fitted$mu[states], type="l", lwd=2, col="red")
limitesinf=qnbinom(c(0.025),mu=HMM.fitted$mu, size=1/HMM.fitted$theta)
limitessup=qnbinom(c(0.975),mu=HMM.fitted$mu, size=1/HMM.fitted$theta)
points(pk,limitesinf[states], type="l", col="red", lty=3, lwd=2)
points(pk,limitessup[states], type="l", col="red", lty=3, lwd=2)

