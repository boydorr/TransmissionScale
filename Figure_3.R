# Produce the simulation time series
rm(list=ls()); 
options(stringsAsFactors = F)

library(ggplot2)
library(cowplot)
library(dplyr)
library(tidyr)
library(lubridate)

source("Style_Sheet.R")

ff <- "java_output/"
ff_figs <- "figs/"
start_date <- as.Date("2002-01-01")
#end.date <- as.Date("2015-12-31")

ff_NORMAL <- "java_output/Round3_1000/postprocessing/monthlyTally_001_1000.csv"
ff_LOWER_VAX <- "java_output/Round3_LOWER_VAX_1000/postprocessing/monthlyTally_001_1000.csv"
ff_WO_DOG_VARIATION <- "java_output/Round3_WO_DOG_VARIATION_1000/postprocessing/monthlyTally_001_1000.csv"
ff_STOP_INCURSIONS <- "java_output/Round3_STOP_INCURSIONS_1000/postprocessing/monthlyTally_001_1000.csv"
# 

make_ts_plot <- function(mt, y_max) {
  # Gather mt into long format for ggplot
  mt_long <- gather(mt,"days","count", -Param)
  mt_long$days <- as.numeric(mt_long$days)
  mt_long$month <- start_date + mt_long$days
  head(mt_long)
  tail(mt_long)
  
  # Compute ranges, etc., aggregating over values
  mt_mutate <- filter(mt_long, Param!="data")  %>% 
    group_by(days, month) %>%
    summarise(min = min(y_max, min(count)), 
              max = min(y_max, max(count)), 
              lq = min(y_max, quantile(count, probs = 0.025)), 
              uq = min(y_max, quantile(count, probs = 0.975)), 
              ave = min(y_max, mean(count)))
  head(mt_mutate)
  dim(mt_mutate)
  mt_mutate$data <- filter(mt_long, Param=="data")$count
  
  g <- ggplot(mt_mutate, aes(month, y=ave), margin(t = 0, r = 0, b = 0, l = 0, unit = "pt")) + 
    geom_ribbon(aes(ymin = min, ymax = max), alpha = 0.2, fill = sc.colours$blue) +
    geom_ribbon(aes(ymin = lq, ymax = uq), alpha = 0.3, fill = sc.colours$blue) +
    geom_line(data=filter(mt_long, Param %in% c("001","002","003")), aes(month, y=count, group=Param), colour = "grey20", size = 0.3) +
    geom_line(aes(month, y=data), colour = "red") +
    scale_x_date("") +
    scale_y_continuous("Monthly cases", limits = c(0, y_max)) + 
    ss + theme(plot.margin = margin(1, 0, 0, 1), 
               axis.text = element_text(size = 12), 
               axis.title.y = element_text(size = 12))
  g
  
  return(g)
}

# Make plots
y_max <- 300

# Normal time series with vaccination as observed
mt_NORMAL <- read.csv(ff_NORMAL, check.names = F)
head(mt_NORMAL)
runs <- mt_NORMAL[-1,1:168]
data <- as.numeric(mt_NORMAL[1,1:168])

t_cases <- apply(runs, 1, sum); max_cases <- apply(runs, 1, max)
t_case_hist <- hist(t_cases, breaks = seq(0,10000, 100))
hist(max_cases, breaks = seq(0,300, 10))
length(which(runs[,168]==0)) # 3 runs had to be curtailed because exploded!
t_case_hist$mids[which.max(t_case_hist$counts)]

g_NORMAL <- make_ts_plot(mt_NORMAL, y_max); g_NORMAL
ggsave(plot = g_NORMAL, path = ff_figs, 
       filename = "NORMAL_ts_ribbon.pdf", width = 21, height = 15, units="cm")

# with minimal vaccination
mt_LOWER_VAX <- read.csv(ff_LOWER_VAX, check.names = F)
runs_low <- mt_LOWER_VAX[-1,1:168]
t_cases_low <- as.vector(apply(runs_low, 1, sum)); 
max_cases_low <- as.vector(apply(runs_low, 1, max))
hist(t_cases_low, breaks = seq(0,51000, 100))
t_case_low_hist <- hist(t_cases_low, breaks = seq(0,50100, 100))
t_case_low_hist$mids[which.max(t_case_low_hist$counts)]

hist(max_cases_low, breaks = seq(0,1200, 10))
length(which(runs_low[,168]==0)) # 10 runs had to be curtailed because exploded!
length(which(max_cases_low>400))

mean(t_cases_low[which(max_cases_low < 400)]); 
median(t_cases_low[which(max_cases_low < 400)])
mean(t_cases); median(t_cases)

mean(t_cases_low[which(max_cases_low < 400)])/mean(t_cases)
median(t_cases_low[which(max_cases_low < 400)])/median(t_cases)
median(t_cases_low)/median(t_cases)
t_case_low_hist$mids[which.max(t_case_low_hist$counts)]/t_case_hist$mids[which.max(t_case_hist$counts)]

cases_averted <- t_case_low_hist$mids[which.max(t_case_low_hist$counts)] - t_case_hist$mids[which.max(t_case_hist$counts)]
cases_averted
n = sum(mt_NORMAL[1,1:168])
cases_averted2 <- t_case_low_hist$mids[which.max(t_case_low_hist$counts)] - n 
cases_averted2
exp_case <- 1462/n
death_case <- 44/n

cases_averted * exp_case; cases_averted2 * exp_case
cases_averted * death_case; cases_averted2 * death_case

g_LOWER_VAX <- make_ts_plot(mt_LOWER_VAX, y_max); g_LOWER_VAX

ggsave(plot = g_LOWER_VAX, path = ff_figs, 
       filename = "LOWER_VAX_ts_ribbon.pdf", width = 21, height = 15, units="cm")

# without individual dog variation
mt_WO_DOG_VARIATION <- read.csv(ff_WO_DOG_VARIATION, check.names = F)
runs_wo_var <- mt_WO_DOG_VARIATION[-1,1:168]
t_cases_wo_var <- as.vector(apply(runs_wo_var, 1, sum))
max_cases_wo_var <- as.vector(apply(runs_wo_var, 1, max)); max_cases_wo_var; 
mean(max_cases_wo_var); var(max_cases_wo_var)

mean_mthly_cases_wo_var <- as.vector(apply(runs_wo_var, 1, mean)); mean_mthly_cases_wo_var; 
hist(mean_mthly_cases_wo_var)

t_case_wo_var_hist <- hist(t_cases_wo_var, breaks = seq(0,3000, 100))
t_case_wo_var_hist$mids[which.max(t_case_wo_var_hist$counts)]

t_case_hist$mids[which.max(t_case_hist$counts)]/2
t_case_wo_var_hist$mids[which.max(t_case_wo_var_hist$counts)]

g_WO_DOG_VARIATION <- make_ts_plot(mt_WO_DOG_VARIATION, y_max); g_WO_DOG_VARIATION
ggsave(plot = g_WO_DOG_VARIATION, path = ff_figs, 
       filename = "WO_DOG_VARIATION_ts_ribbon.pdf", width = 21, height = 15, units="cm")


# without incursions
mt_STOP_INCURSIONS <- read.csv(ff_STOP_INCURSIONS, check.names = F)
runs_wo_inc <- mt_STOP_INCURSIONS[-1,1:168]
t_cases_wo_inc <- as.vector(apply(runs_wo_inc, 1, sum))
hist(t_cases_wo_inc, breaks=seq(0,5000,100))

nruns <- 1000
extinction <- rep(NA, nruns)
for (i in 1:nruns){
  cuminc <- cumsum(as.numeric(runs_wo_inc[i,]))
  extinction[i] <- which.max(cuminc)
}
hist(extinction); max(extinction); 
mean(extinction); median(extinction)
sort(extinction)[900]

g_STOP_INCURSIONS <- make_ts_plot(mt_STOP_INCURSIONS, y_max); g_STOP_INCURSIONS
ggsave(plot = g_STOP_INCURSIONS, path = ff_figs, 
       filename = "STOP_INCURSIONS_ts_ribbon.pdf", width = 21, height = 15, units="cm")

###################################################################################
#                       Combine plots
###################################################################################
g_all <- cowplot::plot_grid(g_NORMAL, g_LOWER_VAX, g_WO_DOG_VARIATION, g_STOP_INCURSIONS, 
                            nrow = 2, rel_heights = c(0.5, 0.5), align = "v", 
                            labels = c('A', 'B', 'C','D'), label_size = 12)

ggsave(plot = g_all, path = ff_figs, filename = "fig3_timeseries.pdf", width = 19, height = 14, units="cm")
ggsave(plot = g_all, path = ff_figs, filename = "fig3_timeseries.pdf", width = 19, height = 14, units="cm")


