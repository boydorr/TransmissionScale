# Katie Hampson 2021-01
# This file is to be run after Data_Cleaning.R
##### THE DEIDENTIFIED DATA HAVE JITTERED GPS SO SOME ESTIMATES WILL BE INCORRECT IF RUN DIRECTLY FROM THIS CODE!
# THESE ARE CLEARLY INDICATED AND INSTEAD THE CORRECT OUTPUTS ARE PROVIDED

library(MASS)
library(fitdistrplus)
library(bbmle)
source("R/MLEfunctions.R")
source("R/format_param_est.R")
source("R/compileSteps.R")
source("R/fitWeibull.R")
source("R/NLLweibullLC.R")

set.seed(0)

###################################################################
# NB: steps are not excluded in compileSteps if length is NA
###################################################################

# Make step lengths. To do this, we need to use the augmented biting_animals dataset, with the "bite footprints" per animal.
rabid_carnivores <- readRDS(file = "output/clean_bite_data_deid.rda")
nrow(rabid_carnivores) # 3295

# Subset on location information being available
rabid_carnivores_location <- subset(rabid_carnivores, !is.na(UTM.Easting.jitter))
nrow(rabid_carnivores_location) # 3251 (note that there are some that don't have UTMs)

# Estimate step length parameters given a Weibull distribution, for all carnivores with "footprints"
carnivores_with_footprints <- subset(rabid_carnivores_location, Animals.bitten > 0); nrow(carnivores_with_footprints) # 1569
steps.m <- compileSteps(animals.with.footprints = carnivores_with_footprints, tot.steps = sum(carnivores_with_footprints$Animals.bitten)) # 5360
weibull.params <- fitWeibull(values = steps.m$step.length, distr.params = c(threshold=100)) # truncated at 100m - low precision below this
# [1] "Fitting to left-censored data with threshold = 100"
# [1] "Initial values: shape (k0) = 0.305 and scale (lam0) = 100.000"
# [1] "CONVERGENCE: REL_REDUCTION_OF_F <= FACTR*EPSMCH"
censdata <- data.frame("left"=ifelse(steps.m$step.length>100,steps.m$step.length,0),
                       "right"=ifelse(steps.m$step.length>100,steps.m$step.length,100))
weibull <- fitdistcens(censdata,"weibull") # fitdistrplus gives pretty much the same estimates for weibull - will stick with this for simplicity
lnorm <- fitdistcens(censdata,"lnorm")
gamma <- fitdistcens(censdata,"gamma",control = list(ndeps=c(1e-4,1e-4), parscale=c(1,1e-5)))
weibull$aic; lnorm$aic; gamma$aic # weibull is best

# Visual check of estimated parameter values ...
nreps <- 1000000
sim.steps <- rweibull(nreps,shape=weibull$estimate["shape"],scale=weibull$estimate["scale"])
hist(steps.m$step.length, breaks = seq(0, max(c(steps.m$step.length,sim.steps), na.rm=T)+100, by=100),xlim=c(0,max(steps.m$step.length)), freq=F, col="grey") # data
hist(sim.steps, breaks = seq(0, max(c(steps.m$step.length,sim.steps), na.rm=T)+100, by=100), border="red", add=T, freq=F) # simulated

# Write out parameter estimates
weibull.params <- c(weibull$estimate,loglik=weibull$loglik, mean=mean(sim.steps), sd=sd(sim.steps),quantile(sim.steps, c(0.025, 0.975)))
print("Movement parameters using a Weibull: "); print(weibull.params)

# Write out summary statistics for supplementary materials
print(paste0("Total number of steps used in parameter estimation = ", nrow(steps.m))) # 5576
print(paste0("Mean step length = ", mean(steps.m$step.length))) # 197.62
print(paste0("Standard deviation of step lengths = ", sd(steps.m$step.length))) # 660.51
write.csv(steps.m, "output/steps_m.csv", row.names = FALSE)
write.csv(as.data.frame(t(weibull.params)), "output/steps.distribution.csv", row.names = FALSE)

steps_table = rbind(
  dist_fit_info(gamma, "steps"),
  dist_fit_info(lnorm, "steps"),
  dist_fit_info(weibull, "steps"))
write.csv(steps_table, "output/steps_params_table1.csv", row.names=FALSE)

###################################################################
## SPATIAL INFECTION KERNEL FOR TRANSMISSION TREES
# Look at distances to both contacts and cases!

#### JITTERED GPS MEAN THE RESULTING OUTPUTS WILL NOT WORK! 
# ct <- readRDS(file = "output/clean_bite_data_no_densities.rda"); dim(ct) # 9876 (rabid and bitten)
ct <- readRDS(file = "output/clean_bite_data_no_densities_deid.rda"); dim(ct) # 9876 (rabid and bitten)

## calculate max distance moved by biter (locations of bitten & origin)
max_moved=function(biter, bitten){
  gps=rbind(cbind(biter$UTM.East, biter$UTM.North),
            cbind(bitten$UTM.East, bitten$UTM.North))
  max(as.matrix(dist(gps)),na.rm=T)
}

## all distances moved by biter
all_moved=function(biter, bitten){
  gps=rbind(cbind(biter$UTM.East, biter$UTM.North),
            cbind(bitten$UTM.East, bitten$UTM.North))
  distances <- as.numeric(as.matrix(dist(gps))[1,2:(nrow(bitten)+1)])
  distances[which(!is.na(distances))]
}

## calculate case-by-case
rabid_carnivores$max_dist_case = rabid_carnivores$max_dist_contact = NA
rabid_carnivores$max_dist_case_censored = rabid_carnivores$max_dist_contact_censored = NA
dist_case_all <- dist_contact_all <- data.frame("dist"=c(),"right_censored"=c(),"ID"=c())
for(i in 1:nrow(rabid_carnivores)){
  cases = which(rabid_carnivores$Biter.ID == rabid_carnivores$ID[i])
  contacts = which(ct$Biter.ID == rabid_carnivores$ID[i])
  rabid_carnivores$max_dist_case[i] = ifelse(length(cases)>0, max_moved(rabid_carnivores[i,], rabid_carnivores[cases,]), NA)
  rabid_carnivores$max_dist_contact[i] = ifelse(length(contacts)>0, max_moved(rabid_carnivores[i,], ct[contacts,]), NA)
  if(rabid_carnivores$Owner[i]!="Known" & !is.na(rabid_carnivores$max_dist_case[i])){rabid_carnivores$max_dist_case_censored[i]<-T
  }else if(rabid_carnivores$Owner[i]=="Known" & !is.na(rabid_carnivores$max_dist_case[i])){rabid_carnivores$max_dist_case_censored[i]<-F}
  if(rabid_carnivores$Owner[i]!="Known" & !is.na(rabid_carnivores$max_dist_contact[i])){rabid_carnivores$max_dist_contact_censored[i]<-T
  }else if(rabid_carnivores$Owner[i]=="Known" & !is.na(rabid_carnivores$max_dist_contact[i])){rabid_carnivores$max_dist_contact_censored[i]<-F}
  if(length(cases)>0){
    all_case_moves <- all_moved(rabid_carnivores[i,], rabid_carnivores[cases,])
    if(rabid_carnivores$Owner[i]!="Known"){right_censored <- rep(T,length(all_case_moves))
    }else{right_censored <- rep(F,length(all_case_moves))}
    dist_case_all <- rbind(dist_case_all,cbind(all_case_moves,right_censored,rep(rabid_carnivores$ID[i],length(all_case_moves))))
  }
  if(length(contacts)>0){
    all_contact_moves <- all_moved(rabid_carnivores[i,], ct[contacts,])
    if(rabid_carnivores$Owner[i]!="Known"){right_censored <- rep(T,length(all_contact_moves))
    }else{right_censored <- rep(F,length(all_contact_moves))}
    dist_contact_all <- rbind(dist_contact_all,cbind(all_contact_moves,right_censored,rep(rabid_carnivores$ID[i],length(all_contact_moves))))
  }
  # print(i)
}
names(dist_contact_all)<-names(dist_case_all)<-c("distance","right_censored","ID")
write.csv(dist_contact_all,"output/dist_contact_all.csv",row.names = F)

# Compare distances between cases and contacts
par(mfrow=c(1,2)) 
hist(rabid_carnivores$max_dist_case, breaks=seq(0, 25000, 100), xlim=c(0,5000),freq=F)
hist(rabid_carnivores$max_dist_contact, breaks=seq(0, 25000, 100), xlim=c(0,5000),freq=F) 
mean(rabid_carnivores$max_dist_case, na.rm = TRUE); median(rabid_carnivores$max_dist_case, na.rm = TRUE) 
mean(rabid_carnivores$max_dist_contact, na.rm = TRUE); median(rabid_carnivores$max_dist_contact, na.rm = TRUE) 
hist(dist_case_all$distance, breaks=seq(0, 25000, 100), xlim=c(0,5000),freq=F)
hist(dist_contact_all$distance, breaks=seq(0, 25000, 100), xlim=c(0,5000),freq=F) 
mean(dist_case_all$distance, na.rm = TRUE); median(dist_case_all$distance, na.rm = TRUE) 
mean(dist_contact_all$distance, na.rm = TRUE);  median(dist_contact_all$distance, na.rm = TRUE) 

# Compare distances between contacts for unknown and known biters
mean(rabid_carnivores$max_dist_contact, na.rm = TRUE); quantile(rabid_carnivores$max_dist_contact,c(0.5,0.95,0.975), na.rm = TRUE) 
mean(rabid_carnivores$max_dist_contact[which(rabid_carnivores$max_dist_contact_censored==F)], na.rm = TRUE);  quantile(rabid_carnivores$max_dist_contact[which(rabid_carnivores$max_dist_contact_censored==F)],c(0.5,0.95,0.975), na.rm = TRUE) 
mean(rabid_carnivores$max_dist_contact[which(rabid_carnivores$max_dist_contact_censored==F|rabid_carnivores$max_dist_contact>100)], na.rm = TRUE);  quantile(rabid_carnivores$max_dist_contact[which(rabid_carnivores$max_dist_contact_censored==F|rabid_carnivores$max_dist_contact>100)],c(0.5,0.95,0.975), na.rm = TRUE) 
mean(ifelse(rabid_carnivores$Owner=="Known",rabid_carnivores$max_dist_contact,rabid_carnivores$max_dist_contact+100), na.rm = TRUE);  quantile(ifelse(rabid_carnivores$Owner=="Known",rabid_carnivores$max_dist_contact,rabid_carnivores$max_dist_contact+100),c(0.5,0.95,0.975), na.rm = TRUE) 

mean(dist_contact_all$distance, na.rm = TRUE); quantile(dist_contact_all$distance,c(0.5,0.95,0.975), na.rm = TRUE) 
mean(dist_contact_all$distance[which(dist_contact_all$right_censored==F)], na.rm = TRUE);  quantile(dist_contact_all$distance[which(dist_contact_all$right_censored==F)],c(0.5,0.95,0.975), na.rm = TRUE) 
mean(dist_contact_all$distance[which(dist_contact_all$right_censored==F|dist_contact_all$distance>100)], na.rm = TRUE);  quantile(dist_contact_all$distance[which(dist_contact_all$right_censored==F|dist_contact_all$distance>100)],c(0.5,0.95,0.975), na.rm = TRUE) 
mean(ifelse(dist_contact_all$right_censored==T,dist_contact_all$distance,dist_contact_all$distance+100), na.rm = TRUE);  quantile(ifelse(dist_contact_all$right_censored==T,dist_contact_all$distance,dist_contact_all$distance+100),c(0.5,0.95,0.975), na.rm = TRUE) 
mean(dist_contact_all$distance[which(dist_contact_all$right_censored==F|(dist_contact_all$ID %in% unique(dist_contact_all$ID[which(dist_contact_all$distance>100)])))],na.rm=T); quantile(dist_contact_all$distance[which(dist_contact_all$right_censored==F|(dist_contact_all$ID %in% unique(dist_contact_all$ID[which(dist_contact_all$distance>100)])))],c(0.5,0.95,0.975),na.rm=T)

# Fit distributions to maximum distances
idx<-which(!is.na(rabid_carnivores$max_dist_contact))
censdata <- data.frame("left"=ifelse(rabid_carnivores$max_dist_contact[idx]>100,rabid_carnivores$max_dist_contact[idx],0),
                       "right"=ifelse(rabid_carnivores$max_dist_contact[idx]>100,rabid_carnivores$max_dist_contact[idx],100))
censdata$left[which(rabid_carnivores$max_dist_contact_censored[idx]==T)] <- ifelse(censdata$left[which(rabid_carnivores$max_dist_contact_censored[idx]==T)]<100,100,censdata$left[which(rabid_carnivores$max_dist_contact_censored[idx]==T)])
censdata$right[which(rabid_carnivores$max_dist_contact_censored[idx]==T)] <- NA
max_dist_gamma <- fitdistcens(censdata, "gamma",control = list(ndeps=c(1e-4,1e-4), parscale=c(1,1e-5))) 
max_dist_lnorm <- fitdistcens(censdata, "lnorm") 
max_dist_weibull <- fitdistcens(censdata, "weibull") 
max_dist_gamma$aic; max_dist_lnorm$aic; max_dist_weibull$aic # toss up between gamma and weibull
qgamma(c(0.95,0.975),shape=max_dist_gamma$estimate["shape"],rate=max_dist_gamma$estimate["rate"])


# Fit distributions to all distances
censdata <- data.frame("left"=ifelse(dist_contact_all$distance>100,dist_contact_all$distance,0),
                       "right"=ifelse(dist_contact_all$distance>100,dist_contact_all$distance,100))
censdata$left[which(dist_contact_all$right_censored==T)] <- ifelse(censdata$left[which(dist_contact_all$right_censored==T)]<100,100,censdata$left[which(dist_contact_all$right_censored==T)])
censdata$right[which(dist_contact_all$right_censored==T)] <- NA
all_dist_gamma <- fitdistcens(censdata, "gamma",control = list(ndeps=c(1e-4,1e-4), parscale=c(1,1e-5))) 
all_dist_lnorm <- fitdistcens(censdata, "lnorm")
all_dist_weibull <- fitdistcens(censdata, "weibull")
all_dist_gamma$aic; all_dist_lnorm$aic; all_dist_weibull$aic # weibull best
qweibull(c(0.95,0.975),shape=all_dist_weibull$estimate["shape"],scale=all_dist_weibull$estimate["scale"])

DK_table = rbind(
  dist_fit_info(all_dist_gamma, "dist_contact"),
  dist_fit_info(all_dist_lnorm, "dist_contact"),
  dist_fit_info(all_dist_weibull, "dist_contact"))
write.csv(DK_table, "output/DK_params_table1.csv", row.names=FALSE)
length(which(is.na((all_dist_weibull$censdata)[,2]))) # RIGHT CENSORED!


# Simulate two draws from weibull distance kernel with a random change in direction in between
conv_with_direction <- function(n=1000000,dist,par1,par2){

  angle1 <- runif(n = n, min = 0, max = 2*pi)  
  angle2 <- runif(n = n, min = 0, max = 2*pi)  
  dist_1 <- get(paste0("r",dist))(n,par1,par2)
  dist_2 <- get(paste0("r",dist))(n,par1,par2)
  sim_2_dists <- rep(NA,n)
  
  for(i in 1:n){
    
    # Coords after first distance
    x1 <- (sin(angle1[i]) * dist_1[i]) + 0  
    y1 <- (cos(angle1[i]) * dist_1[i]) + 0
    
    # Coords after second distance
    x2 <- (sin(angle2[i]) * dist_2[i]) + x1  
    y2 <- (cos(angle2[i]) * dist_2[i]) + y1
    
    # Total distance travelled from (0,0)
    sim_2_dists[i] <- sqrt(x2^2 + y2^2)
    
  }
  
  return(sim_2_dists)
}
sim_2_dists_weibull <- conv_with_direction(dist="weibull",par1=coef(all_dist_weibull)["shape"],par2=coef(all_dist_weibull)["scale"])
summary(sim_2_dists_weibull)
gamma_2_weibull_dists <- fitdist(sim_2_dists_weibull,"gamma",control = list(ndeps=c(1e-4,1e-4), parscale=c(1,1e-5)))
lnorm_2_weibull_dists <- fitdist(sim_2_dists_weibull,"lnorm")
weibull_2_weibull_dists <- fitdist(sim_2_dists_weibull,"weibull")
gamma_2_weibull_dists$aic; lnorm_2_weibull_dists$aic; weibull_2_weibull_dists$aic # weibull best

# Simulate two draws from gamma distance kernel with a random change in direction in between
sim_2_dists_gamma <- conv_with_direction(dist="gamma",par1=coef(all_dist_gamma)["shape"],par2=coef(all_dist_gamma)["rate"])
summary(sim_2_dists_gamma)
gamma_2_gamma_dists <- fitdist(sim_2_dists_gamma,"gamma",control = list(ndeps=c(1e-4,1e-4), parscale=c(1,1e-5)))
lnorm_2_gamma_dists <- fitdist(sim_2_dists_gamma,"lnorm")
weibull_2_gamma_dists <- fitdist(sim_2_dists_gamma,"weibull")
gamma_2_gamma_dists$aic; lnorm_2_gamma_dists$aic; weibull_2_gamma_dists$aic # gamma best

# Simulate two draws from lnorm distance kernel with a random change in direction in between
sim_2_dists_lnorm <- conv_with_direction(dist="lnorm",par1=coef(all_dist_lnorm)["meanlog"],par2=coef(all_dist_lnorm)["sdlog"])
summary(sim_2_dists_lnorm)
gamma_2_lnorm_dists <- fitdist(sim_2_dists_lnorm,"gamma",control = list(ndeps=c(1e-4,1e-4), parscale=c(1,1e-5)))
lnorm_2_lnorm_dists <- fitdist(sim_2_dists_lnorm,"lnorm")
weibull_2_lnorm_dists <- fitdist(sim_2_dists_lnorm,"weibull")
gamma_2_lnorm_dists$aic; lnorm_2_lnorm_dists$aic; weibull_2_lnorm_dists$aic # lnorm best


# # Write parameters for transmission tree inference
# DK_params = data.frame(
#   DK_shape = coef(all_dist_weibull)["shape"], DK_scale = coef(all_dist_weibull)["scale"],
#   DK2_shape = coef(weibull_2_dists)["shape"], DK2_scale = coef(weibull_2_dists)["scale"]
# )
# DK_params
# write.csv(DK_params, "output/DK_params.csv", row.names=FALSE)

# Write parameters for transmission tree inference
summarise_dist <- function(dist_fit){

    c(dist = dist_fit$distname,
    AIC = round(dist_fit$aic),
    v1name = names(dist_fit$estimate)[1], 
    var1 = dist_fit$estimate[1],
    v2name = names(dist_fit$estimate)[2],
    var2 = dist_fit$estimate[2])
    
}
dists <- list(all_dist_gamma, gamma_2_gamma_dists, 
              all_dist_lnorm, lnorm_2_lnorm_dists,
              all_dist_weibull, weibull_2_weibull_dists)
DK_params = data.frame(
  kernel_type=rep(c("DK","DK2"),3),
  dist=NA,
  AIC=NA,
  par1name=NA,
  par1est=NA,
  par2name=NA,
  par2est=NA
)

for(i in 1:length(dists)){
  DK_params[i, 2:ncol(DK_params)] <- summarise_dist(dists[[i]])
}

DK_params$AIC[which(DK_params$kernel_type=="DK2")]<-NA
write.csv(DK_params, "output/DK_params.csv", row.names=FALSE)


## Compare distributions - to cases and to contacts (with fits)
par(mfrow=c(1,2))
int = 250
ms = seq(0, 30000, int)
rdist = hist(dist_case_all$distance, breaks=ms, main="",xlim=c(0,20000),freq = F) # dists travelled to cases
ctdist = hist(dist_contact_all$distance, breaks=ms, main="",xlim=c(0,20000),freq = F) # dists travelled to contacts
lines(ms, dgamma(ms, shape = all_dist_gamma$estimate["shape"], rate = all_dist_gamma$estimate["rate"]), col="green") 
lines(ms, dlnorm(ms, meanlog = all_dist_lnorm$estimate["meanlog"], sdlog = all_dist_lnorm$estimate["sdlog"]), col="purple") 
lines(ms, dweibull(ms, shape = all_dist_weibull$estimate["shape"], scale = all_dist_weibull$estimate["scale"]), col="blue") # exclude unknowns
lines(rep(qlnorm(.975, meanlog = all_dist_lnorm$estimate["meanlog"], sdlog = all_dist_lnorm$estimate["sdlog"]), 100), seq(0,1,length=100), col="purple") 
lines(rep(qgamma(.975, shape = all_dist_gamma$estimate["shape"], rate = all_dist_gamma$estimate["rate"]), 100), seq(0,1,length=100), col="green") 
lines(rep(qweibull(.975, shape = all_dist_weibull$estimate["shape"], scale = all_dist_weibull$estimate["scale"]), 100), seq(0,1,length=100), col="blue") 
lines(rep(quantile(dist_contact_all$distance,0.975), 100), seq(0,1,length=100), col="grey",lty=2) 


