# library path
.libPaths('C:\\Users\\mbeck\\R\\library')

# startup message
cat('Sim city...\n')

# packages to use
library(knitr)
library(reshape2) 
library(plyr)
library(ggplot2)
library(scales)
library(doParallel)
library(foreach)
library(Metrics)
library(GGally)

setwd('M:/docs/SWMP/detiding_sim/')

# functions to use
source('sim_funs.r')

# grid of simulation conditions to evaluate
tide_cat <- c('Diurnal', 'Semidiurnal', 'Mixed Semidiurnal')
tide_cat <- factor(tide_cat, levels = tide_cat)
bio_rng <- round(seq(0, 2, length = 3),2)
tide_assoc <- round(seq(0, 2, length = 3), 2)
err_rng_pro <- round(seq(0, 2, length = 3), 2)
err_rng_obs <- round(seq(0, 2, length = 3), 2)

eval_grd <- expand.grid(tide_cat, bio_rng, tide_assoc, err_rng_pro, 
  err_rng_obs)
names(eval_grd) <- c('tide_cat', 'bio_rng', 'tide_assoc', 'err_rng_pro', 
  'err_rng_obs')
save(eval_grd, file = 'eval_grd.RData')

# grid of regression windows to evaluate
dy_wins <- c(1, 3, 8)
hr_wins <- c(3, 9, 24)
td_wins <- c(0.25, 1, 2)
wins_grd <- expand.grid(dy_wins, hr_wins, td_wins)
names(wins_grd) <- c('dec_time', 'hour', 'Tide')
save(wins_grd, file = 'wins_grd.RData')

# grid of both eval and window combinations
comb_grd <- expand.grid(tide_cat, bio_rng, tide_assoc, err_rng_pro, err_rng_obs,
  dy_wins,  hr_wins, td_wins)
names(comb_grd) <- c(names(eval_grd), names(wins_grd))
save(comb_grd, file = 'comb_grd.RData')

# time vector
vec <- c('2014-05-01 00:00:00', '2014-05-31 00:00:00')
vec <- as.POSIXct(vec, format = '%Y-%m-%d %H:%M:%S')
vec <- seq(vec[1], vec[2], by = 60*30)

# setup parallel 
cl <- makeCluster(8)
registerDoParallel(cl)

# iterate through evaluation grid to create sim series
# saved as separate files in /prdnrm/
strt <- Sys.time()
foreach(row = 1:nrow(comb_grd)) %dopar% {
 
  # progress
  sink('log.txt')
  cat('Log entry time', as.character(Sys.time()), '\n')
  cat(row, ' of ', nrow(comb_grd), '\n')
  print(Sys.time() - strt)
  sink()
    
  # eval grid to evaluate
  to_eval <- comb_grd[row, ]
  
  # create simulated time series of DO, tide, etc.
  DO_sim <- with(to_eval, 
    ts_create(
      vec, 
      do.amp = bio_rng, 
      tide_cat = as.character(tide_cat), 
      tide_assoc = tide_assoc,
      err_rng_obs = err_rng_obs,
      err_rng_pro = err_rng_pro
      )  
    )

  win_in <- with(to_eval, list(dec_time, hour, Tide))
  
  # get results
  res_tmp <- wtreg_fun(DO_sim, wins = win_in, parallel = F)
  
  # save and delete from workspace
  nm <- paste0('prdrm_', row)
  assign(nm, res_tmp)
  
  save(list = nm, file = paste0('prdnrm/', nm, '.RData'))
  
  rm(list = nm)
  
  }
stopCluster(cl)
