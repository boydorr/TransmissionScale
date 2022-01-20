#PARAMETER VALUES FOR RUNNING TREE

#1) SERIAL INTERVAL
#shape and rate parameters for gamma distributions describing incubation & infectious periods
incubation=c(1.08549138, 0.04919551) #~22.0 days mean
infectious=c(2.831788, 0.9193612) #~3.1 days mean

#Estimate serial interval parameters (incubation period + interval from start of infectious period to biting)
incx=rgamma(10000, incubation[1], incubation[2])
infx=rgamma(10000, infectious[1], infectious[2]); infd=runif(10000, min=0, max=infx)
SI=incx+infd

#Max Likelihood function for estimating the shape and rate parameters for a gamma distribution
NLLgamma1 = function(data, param){-sum(dgamma(data, shape=param[1], rate=param[2], log=TRUE))}

b=1; c=0.1 #starting parameters
SIest=optim(fn=NLLgamma1, par=c(shape=b,scale=c), data=SI, method="L-BFGS-B", lower=0.0002) #Optimization
SIshape=SIest$par[1]; SIrate=SIest$par[2] #1.43920570 #0.05894706

#2) SPATIAL INFECTION KERNEL
distfit=c(0.2149769451, 0.0002451336)
#meandist=distfit[1]/distfit[2]/1000  #check mean distance in km
distshape=0.2151918
distrate=0.0002457546

#3) PER TRANSMISSION SUBSTITUTION RATE
shape1 =   0.894028
shape2 =3514.271884
perTrans=rbeta(10000, shape1=shape1, shape2=shape2)
