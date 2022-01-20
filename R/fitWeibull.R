#' @title Conducts parameter estimation for Weibull distribution
#' @description Conducts parameter estimation, including for left-censored data
#' @param values Vector of values to fit to
#' @param distr.params List of named parameters. Includes "threshold" for left-censored data
#' @export
fitWeibull <- function(values, distr.params) {
  # Conduct fitting to estimate parameters
  if (distr.params["threshold"] > 0) { # Threshold for left-censored data
    threshold <- as.numeric(distr.params["threshold"])
    print(paste0("Fitting to left-censored data with threshold = ", as.numeric(threshold)))
  } else {
    threshold <- 0
  }
  # Compute parameter estimates directly from data for starting values for optim
  ws <- sort(values)
  ws <- ws[ws>0]
  Fh <- ppoints(ws)
  k0 <- as.numeric(lm(log(-log(1-Fh))~log(ws))$coefficients[2]) # shape
  lam0 <- as.numeric(quantile(c(values[values>=threshold], rep(threshold, sum(values<threshold))), p=.632)) # scale (should be relatively robust to left-censoring)
  # print(lam0); print(k0)
  print(paste0("Initial values: shape (k0) = ", sprintf("%1.3f", k0), " and scale (lam0) = ", sprintf("%1.3f",lam0)))
  
  # Conduct fitting
  optim.fit <- optim(fn = NLLweibullLC, par = c(shape=k0, scale=lam0), data = values, method = "L-BFGS-B", lower=0.00012, threshold=threshold)
  print(optim.fit$message)
  fitted.info <- c(shape = as.numeric(optim.fit$par[1]), scale = as.numeric(optim.fit$par[2]), loglik=-optim.fit$value) # returns mean, sd, log-likelihood
  
  # Simulate to obtain mean and sd of fitted distribution
  sim.fitted <- rweibull(n = 100000, shape = fitted.info["shape"], scale = fitted.info["scale"])
  fitted.mean <- mean(sim.fitted, na.rm = T)
  fitted.sd <- sd(sim.fitted, na.rm = T)
  fitted.CIs <- quantile(sim.fitted, c(0.025, 0.975), na.rm = T)
  fitted.info <- c(fitted.info,
                   fitted.mean = as.numeric(fitted.mean), 
                   fitted.sd = as.numeric(fitted.sd),
                   fitted.CIs = as.numeric(fitted.CIs))
  return(fitted.info)
}


