dist_fit_info <- function(dist_fit,variable){
  # Function takes a fitted distribution, then formats the estimates (values,
  # parameters, CIs) and returns them with the aic and n
  var1name <- names(dist_fit$estimate)[1]
  var2name <- names(dist_fit$estimate)[2]
  CIs <- confint(dist_fit)
  var1 <- round(c(dist_fit$estimate[1], CIs[1,1], CIs[1,2]), 3)
  var2 <- round(c(dist_fit$estimate[2], CIs[2,1], CIs[2,2]), 3)
  
  distquant <- get(paste0("q",dist_fit$distname))
  distq = round(distquant(c(0.5, 0.025, 0.975), dist_fit$estimate[1], dist_fit$estimate[2]),3)
  
  data.frame(
    v = variable,
    dist = dist_fit$distname,
    var = paste0(distq[1], " (", distq[2], "-", distq[3], ")"),
    var1 = paste0(var1[1], " (", var1[2], "-", var1[3], ")"),
    var2 = paste0(var2[1], " (", var2[2], "-", var2[3], ")"),
    v1name = var1name, v2name = var2name,
    aic = dist_fit$aic,
    n = dist_fit$n)
}

dist_func_move <- function(x, distribution, variable){
  # Function takes data, a parameter distribution and fits the estimate for that variable
  # then formats the estimates (values, parameters, CIs) and returns them with the aic and n
  
  if(distribution == "gamma"){
    distfit = mle2(minuslogl = NLLhybrid,
                   start = list(shape=0.285, rate=0.00015),
                   data = list(data=x), # distances between cases
                   fixed = list(threshold = 100),
                   control = list(ndeps=c(1e-4,1e-4), parscale=c(1,1e-5)))
  }
  if(distribution == "lnorm"){
    distfit = mle2(minuslogl = NLLhybrid_lnorm,
                   start = list(ml=6, sdl=2),
                   fixed = list(threshold = 100),
                   data = list(data=x))
  }
  
  var1name <- names(coef(distfit))[1]
  var2name <- names(coef(distfit))[2]
  
  CIs <- confint(distfit)
  
  var1 <- round(c(coef(distfit)[1], CIs[1,1], CIs[1,2]), 4)
  var2 <- round(c(coef(distfit)[2], CIs[2,1], CIs[2,2]), 4)

  if(distribution == "gamma"){distquant = qgamma} 
  if(distribution == "lnorm"){distquant = qlnorm} 
  distq = round(distquant(c(0.5, 0.025, 0.975), coef(distfit)[1], coef(distfit)[2]),4)
  
  data.frame(
    v = variable,
    var = paste0(distq[1], " (", distq[2], "-", distq[3], ")"),
    var1 = paste0(var1[1], " (", var1[2], "-", var1[3], ")"),
    var2 = paste0(var2[1], " (", var2[2], "-", var2[3], ")"),
    v1name = var1name, v2name = var2name,
    aic = 2 - 2*logLik(distfit),
    n = length(x))
}

