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

#eval grd, same as before but no tidal category since using actual tide
load('eval_grd.RData')
eval_grd_act <- unique(eval_grd[, !names(eval_grd) %in% 'tide_cat'])

# tide to simulate
site <- 'SFBFM'
load(paste0('M:/wq_models/SWMP/raw/rproc/tide_preds/', site, '.RData'))
tide <- get(site)

# time vector 
nobs <- 48 * 31 * 3 # 3 months
vec <- c(as.character(min(tide$DateTimeStamp)), 
  as.character(tide$DateTimeStamp[nobs]))
vec <- as.POSIXct(vec, format = '%Y-%m-%d %H:%M:%S')
vec <- seq(vec[1], vec[2], by = 60*30)

# setup parallel, for ddply in interpgrd 
cl <- makeCluster(8)
registerDoParallel(cl)

# iterate through evaluation grid to create sim series
strt <- Sys.time()
int_grds_tidact <- vector('list', length = nrow(eval_grd_act))
for(row in 1:nrow(eval_grd_act)){
 
  # progress
  sink('log.txt')
  cat('Log entry time', as.character(Sys.time()), '\n')
  cat(row, ' of ', nrow(eval_grd_act), '\n')
  print(Sys.time() - strt)
  sink()
    
  # eval grid to evaluate
  to_eval <- eval_grd_act[row, ]
  
  # create simulated time series of DO, tide, etc.
  DO_sim <- with(to_eval, 
    ts_create(
      vec, 
      do.amp = bio_rng, 
      tide_cat = tide[1:length(vec),], 
      tide_assoc = tide_assoc,
      err_rng_pro = err_rng_pro,
      err_rng_obs = err_rng_obs
      )  
    )

  # get interp grid, done in parallel
  int_grd <- interp_grd(DO_sim, wins = list(2, 0.5, 0.2), parallel = T)
  
  # append to results 
  int_grds_tidact[[row]] <- list(to_eval, DO_sim, int_grd)
  
  # save res as results are appended
  save(int_grds_tidact, file = 'int_grds_tidact.RData')

  }
stopCluster(cl)
Sys.time() - strt