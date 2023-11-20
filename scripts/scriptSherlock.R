setwd("C:/Documents and Settings/lvaudor/Bureau/tutoRial/TUTOS/HMM/tuto_HMM_dev/chapitres")
text=scan("sherlock4.txt", what="raw", sep=" ")


print(length(text))
print(text[1:30])
quotemarks=rep(0,length(text))
quotemarks[which(text=="qm")]=1
quotemarks=cumsum(quotemarks)

type=rep("unquoted",length(text))
type[round(quotemarks/2)!=quotemarks/2]="quoted"

listHe=c("He","he","him","his","Sherlock","Holmes","himself")
listI=c("I","me","my","myself")
observations=rep("blah",length(text))
observations[type=="quoted"]="quoted"
for (k in 1:length(text)){
  if(any(text[k]==listI) ){observations[k]="I"}
  if(any(text[k]==listHe)){observations[k]="he"} 
}
datatext=data.frame(text,observations)
print(datatext[1:3000,])


plot_cat_series(observations,x=1:length(observations), col=c("grey","red","blue","bisque1"))

myprop=table(observations)
myprop=myprop/sum(myprop)
print(myprop)
myprop=as.vector(myprop)

M_E=matrix(rep(NA,3*4),nrow=3)
rownames(M_E)=c("narrHolmes","narrWatson","speech")
colnames(M_E)=c("blah","he","I","quoted")

M_E[1,]=c(0.80,0.10,0.05,0.05)
M_E[2,]=c(0.80,0.05,0.10,0.05)
M_E[3,]=c(0.08,0.01,0.01,0.90)

M_T=matrix(rep(0.05,3*3),nrow=3)
rownames(M_T)=c("narrHolmes","narrWatson","speech")
colnames(M_T)=c("narrHolmes","narrWatson","speech")
diag(M_T)=0.90

initial_HMM=initHMM(States=c("narrHolmes","narrWatson","speech"),
                    Symbols=c("blah","he","I","quoted"),
                    transProbs=M_T,
                    emissionProbs=M_E)


hmm_fit= baumWelch(initial_HMM,observations,maxIterations=100)$hmm

coloutcomes=c("grey","red","blue","bisque1")
colstates=c("pink2","skyblue3","orange")

print(round(hmm_fit$emissionProbs,2))
print(round(hmm_fit$transProbs,2))


layout(matrix(1:2,nrow=2))
par(mar=c(2,1,2,1),oma=c(1,1,1,1))
plot_cat_series(observations,col=coloutcomes,
                main="words outcome") 
plot_cat_series(states_speech,col=colstates,
                main="states of speech")