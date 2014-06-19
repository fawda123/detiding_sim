# # rm(list = ls())
# # 
# # source('M:/r_code/SWMP/NEM_fun.r')
# # 
# # # metabolism bias by category and correction factors
# # 
# # load(file = 'M:/wq_models/SWMP/raw/rproc/dat_bi.RData')
# # load(file = 'M:/wq_models/SWMP/raw/rproc/dat_unbi.RData')
# # 
# # pdf('C:/Users/mbeck/Desktop/metab_corrs.pdf',height=14,
# #   width=18,family='serif')
# #  
# # for(site in unique(dat_bi$site)){
# #   
# #   stat.name<-stat.name.fun(site)
# #   stat.name<-paste0(stat.name,' (',site,')')
# #   
# #   cat(stat.name, '\n')
# #   
# #   to_plo <- dat_bi[dat_bi$site == site,]
# #   ylab<-expression(paste('Mean ',O[2],' flux (',mu,'mol ', m^-2,' ', hr^-1,')'))
# #   pd<-position_dodge(0.8) 
# #   
# #   p1 <- ggplot(to_plo, aes(x=flag, y=DOF, colour=fort,group=fort, fill = fort)) + 
# #     geom_errorbar(aes(ymin=DOF-ci, ymax=DOF+ci), width=.5, position=pd) +
# #   #   geom_line(position=pd) +
# #     scale_y_continuous(ylab) + 
# #     geom_bar(stat = 'identity', position = pd, alpha = 0.5,
# #       colour = NA, width  = 0.8) +
# #     xlab('Tide and solar cycle synchrony') + 
# #     theme_bw() +
# #     facet_wrap(seas~solar,ncol = 2) + 
# #     theme(legend.title=element_blank()) +
# #     theme(legend.position = 'bottom') + 
# #     ggtitle(paste(stat.name,'- mean DOF'))
# #   
# #   to_plo2 <- dat_unbi[dat_unbi$site == site,]
# #   ylab<-expression(paste('Correction as inst. ',O[2],' flux (',mu,'mol ', m^-2,' ', hr^-1,')'))
# #   
# #   p2 <- ggplot(to_plo2, aes(x=flag, y=unbi, colour=fort,
# #     group=fort, fill = fort)) + 
# #     scale_y_continuous(ylab) + 
# #     geom_bar(stat = 'identity', position = pd, alpha = 0.5,
# #       colour = NA, width  = 0.8) +
# #     xlab('Tide and solar cycle synchrony') + 
# #     theme_bw() +
# #     facet_wrap(seas~solar,ncol = 2) + 
# #     theme(legend.title=element_blank()) +
# #     theme(legend.position = 'bottom') +
# #     ggtitle(paste(stat.name,'- DOF correction'))
# # 
# #   grid.arrange(p1, p2, ncol = 2)
# #   
# # }
# # dev.off()
# #   
# # #####
# # # metabolism corrected and observed
# # 
# # rm(list = ls())
# # 
# # library(reshape2)
# # library(ggplot2)
# # 
# # source('M:/r_code/SWMP/nem_fun.r')
# # 
# # load(file='M:/wq_models/SWMP/raw/rproc/dat_nem.RData')
# # load(file='M:/wq_models/SWMP/raw/rproc/dat_nem_unbi.RData')
# # 
# # ylabs<-expression(paste('mmol ', O[2],' ', m^-2,' ', d^-1))
# # 
# # pdf('C:/Users/mbeck/Desktop/metab_unbi.pdf',height=8,width=22,family='serif')
# #  
# # for(site in names(dat.nem)){
# #   
# #   cat(site, which(site == names(dat.nem)), 'of', 
# #     length(dat.nem), '\n')
# #   
# #   stat.name<-stat.name.fun(site)
# #   stat.name<-paste0(stat.name,' (',site,')')
# #   
# #   # subset data by site
# #   met_unbi <- dat.nem.unbi[[site]]
# #   met_bi <- dat.nem[[site]]
# #   
# #   # merge corrected/observed data
# #   to_plo <- melt(list(met_unbi, met_bi), 
# #     id.var = c('Date','Station'), 
# #     measure.var = c('Pg', 'Rt', 'NEM'))
# #   to_plo$L1 <- factor(to_plo$L1, 
# #     labels = c('Corrected', 'Observed'))
# #   
# #   # x limits for plotting
# #   lims<-c(to_plo[1,1],to_plo[nrow(to_plo),1])
# #   
# #   p1<-ggplot(to_plo,aes(x=Date,y=value,group=variable,
# #       colour=variable)) + 
# #     geom_line() + 
# #     geom_point() +
# #     scale_y_continuous(name=ylabs) +
# #     theme_bw() +
# #     scale_x_date(limits=lims) + 
# #     facet_wrap(~L1) +
# #     ggtitle(paste(stat.name,'- daily'))
# # 
# #   to_plo2 <- to_plo
# #   to_plo2$Date<-as.Date(
# #     paste0(strftime(to_plo2$Date,format='%Y-%m'),'-01')
# #     )
# #   to_plo2<-aggregate(value ~ Date + variable + L1, 
# #     data = to_plo2, function(x) mean(x,na.rm=T))
# # 
# #   to.mrg<-seq.Date(min(to_plo2$Date),max(to_plo2$Date),by='mo')
# #   to.mrg<-data.frame(expand.grid(to.mrg,c('Pg','Rt','NEM'),
# #     c('Corrected', 'Observed')))
# #   names(to.mrg) <- c('Date', 'variable', 'L1')
# #   to_plo2 <- merge(to.mrg, to_plo2, 
# #     by = c('Date', 'variable', 'L1'), all.x = T)
# #     
# #   p2<-ggplot(to_plo2,aes(x=Date,y=value,group=variable,colour=variable)) + 
# #     geom_line() + 
# #     geom_point() +
# #     scale_y_continuous(name=ylabs) +
# #     theme_bw() +
# #     scale_x_date(limits=lims) + 
# #     facet_wrap(~L1) +
# #     ggtitle(paste(stat.name,'- monthly average'))
# # 
# #   grid.arrange(p1,p2, ncol=1)
# #   
# # }
# # 
# # dev.off()
# # 
# #####
# # is the bias/noise removed by site/category??
# 
# # change in % anoms
# 
# library(ggplot2)
# library(reshape2)
# 
# setwd('M:/wq_models/SWMP/raw/rproc/')
# 
# source('M:/r_code/SWMP/NEM_fun.r') 
# 
# load(file='M:/wq_models/SWMP/raw/rproc/dat_nem.RData')
# load(file='M:/wq_models/SWMP/raw/rproc/dat_nem_unbi.RData')
# load(file='M:/docs/SWMP/cases/groups.RData')
# 
# # merge 7 with 6
# groups[groups == 7] <- 6
# 
# #anomolous data for observed
# anoms <- lapply(dat.nem, anoms.fun)
# anoms <- melt(anoms, measure.vars = c('Pg', 'Rt'))
# anoms$L1 <- factor(anoms$L1)
# anoms <- merge(anoms,
#   data.frame(L1 = names(groups), clust = groups), 
#   by = 'L1')
# 
# to_plo1 <- anoms
# p1<-ggplot(na.omit(to_plo1), aes(x = L1, y = value, 
#     group = variable, fill = variable)) +
#   geom_bar(stat='identity', position = 'dodge') + 
#   facet_wrap(clust ~ variable, ncol=2, scales = 'free_x') +
#   theme_bw() + 
#   theme(
#     axis.text.x=element_text(angle=90,vjust=0.5,hjust=1,size=7), 
#     legend.position = 'none'
#     ) + 
#   scale_x_discrete('Station') +
#   ylab('% anomalous by tidal group')
# p1
# 
# # pdf('C:/Users/mbeck/Desktop/per_anom.pdf', family = 'serif', 
# #   height = 8.5, width = 6.5)
# # print(p1)
# # dev.off()
# 
# #anomolous data for corrected
# anoms_unbi <- lapply(dat.nem.unbi, anoms.fun)
# anoms_unbi <- melt(anoms_unbi, measure.vars = c('Pg', 'Rt'))
# 
# to_plo2 <- merge(anoms_unbi, anoms, by = c('variable', 'L1'))
# to_plo2$per_ch <- with(to_plo2, 100*(value.x - value.y)/value.y)
# to_plo2$L1 <- factor(to_plo2$L1)
# to_plo2 <- merge(to_plo2,
#   data.frame(L1 = names(groups), clust = groups), 
#   by = 'L1')
# to_plo2 <- to_plo2[, !names(to_plo2) %in% 
#     c('value.x', 'value.y')]
# 
# p2<-ggplot(na.omit(to_plo2), aes(x = L1, y = per_ch, 
#     group = variable, fill = variable)) +
#   geom_bar(stat='identity', position = 'dodge') + 
#   facet_wrap(clust.x ~ variable, ncol=2, scales = 'free') +
#   theme_bw() + 
#   theme(
#     axis.text.x=element_text(angle=90,vjust=0.5,hjust=1,size=7),
#     legend.position = 'none'
#     ) + 
#   scale_x_discrete('Station') +
#   ylab('% change in anomalous')
# 
# p2
# 
# # pdf('C:/Users/mbeck/Desktop/per_chng.pdf', family = 'serif', 
# #   height = 8.5, width = 6.5)
# # print(p2)
# # dev.off()
# # 
# # anoms_com <- merge(to_plo1, to_plo2[,c('L1', 'variable', 'per_ch')], 
# #   by = c('L1', 'variable'))
# # names(anoms_com)[grep('value|per_ch',names(anoms_com))] <- c('Initial', 'Change')
# # 
# # ggplot(anoms_com, aes(x = Initial, y = Change, group = factor(clust), 
# #   colour = factor(clust))) + 
# #   geom_point(size =4) + 
# #   facet_wrap(~variable) + 
# #   theme_bw() +
# #   theme(legend.position = 'none')
# 
# # ####
# # # correction factors
# # # are inst. flux values by category different after bias corr?
# # # yes....
# # 
# # library(ggplot2)
# # library(reshape2)
# # library(gridExtra)
# # 
# # source('M:/r_code/SWMP/NEM_fun.r')
# # 
# # site <- 'SFBSM'
# # 
# # # wq
# # load(paste0('M:/wq_models/SWMP/raw/rproc/inst_flux/',
# #   site,'.RData'))
# # dat_wq <- get(site)
# # 
# # # DOF before correction
# # bef <- na.omit(
# #   summarySE(
# #     dat_wq, 'DOF', c('flag', 'fort', 'seas', 'solar'),
# #     narm = T
# #     ))
# # bef <- bef[bef$flag != 0,]
# # 
# # # DOF after correction
# # aft <- unbi_fun(dat_wq, site, extra_cols = T)
# # aft <- na.omit(summarySE(
# #     aft, 'DOF_unbi', c('flag', 'fort', 'seas', 'solar'),
# #     narm = T
# #     ))
# # aft <- aft[aft$flag != 0,]
# # names(aft)[names(aft) %in% 'DOF_unbi'] <- 'DOF'
# # 
# # stat.name<-stat.name.fun(site)
# # stat.name<-paste0(stat.name,' (',site,')')
# # 
# # cat(stat.name, '\n')
# # ylab<-expression(paste('Mean ',O[2],' flux (',mu,'mol ', m^-2,' ', hr^-1,')'))
# # pd<-position_dodge(0.8) 
# # 
# # to_plo1 <- bef
# # p1 <- ggplot(to_plo1, aes(x=factor(flag), y=DOF, colour=fort,group=fort, fill = fort)) + 
# #   geom_errorbar(aes(ymin=DOF-ci, ymax=DOF+ci), width=.5, position=pd) +
# # #   geom_line(position=pd) +
# #   scale_y_continuous(ylab) + 
# #   geom_bar(stat = 'identity', position = pd, alpha = 0.5,
# #     colour = NA, width  = 0.8) +
# #   xlab('Tide and solar cycle synchrony') + 
# #   theme_bw() +
# #   facet_wrap(seas~solar,ncol = 2) + 
# #   theme(legend.title=element_blank()) +
# #   theme(legend.position = 'bottom') + 
# #   ggtitle(paste(stat.name,'- mean DOF'))
# # 
# # to_plo2 <- aft
# # p2 <- ggplot(to_plo2, aes(x=factor(flag), y=DOF, colour=fort,group=fort, fill = fort)) + 
# #   geom_errorbar(aes(ymin=DOF-ci, ymax=DOF+ci), width=.5, position=pd) +
# # #   geom_line(position=pd) +
# #   scale_y_continuous(ylab) + 
# #   geom_bar(stat = 'identity', position = pd, alpha = 0.5,
# #     colour = NA, width  = 0.8) +
# #   xlab('Tide and solar cycle synchrony') + 
# #   theme_bw() +
# #   facet_wrap(seas~solar,ncol = 2) + 
# #   theme(legend.title=element_blank()) +
# #   theme(legend.position = 'bottom') + 
# #   ggtitle(paste(stat.name,'- mean DOF'))
# # 
# # grid.arrange(p1, p2, ncol = 2)
# 
# ######
# # correction examples
# 
# rm(list = ls())
# 
# library(ggplot2)
# library(plyr)
# library(reshape2)
# library(gridExtra)
# 
# source('M:/r_code/SWMP/NEM_fun.r')
# 
# site <- 'GNDBL'
# 
# # wq 
# load(paste0('M:/wq_models/SWMP/raw/rproc/inst_flux/',
#   site,'.RData'))
# dat_wq <- get(site)
# dat_wq$Date <- as.Date(dat_wq$DateTimeStamp)
# 
# # tides
# load(paste0('M:/wq_models/SWMP/raw/rproc/tide_preds/',
#   site,'.RData'))
# dat_tides <- get(site)
# 
# # bias corrected
# unbi_wq <- unbi_fun(dat_wq, site, extra_cols = T)
# 
# # merge w/ tides
# unbi_wq$Tide <- dat_tides$Tide[-1]
# 
# # metab data, obs and unbiased
# load(file='M:/wq_models/SWMP/raw/rproc/dat_nem.RData')
# load(file='M:/wq_models/SWMP/raw/rproc/dat_nem_unbi.RData')
# nem <- dat.nem[[site]]
# nem <- nem[, c('Date', 'DOF_d', 'DOF_n')]
# names(nem) <- c('met.date', 'sunrise', 'sunset')
# nem$sunrise<- with(nem, sunrise - sunset)
# nem <- melt(nem, id.var = 'met.date')
# names(nem) <- c('met.date', 'solar', 'DOF_mean_obs')
# 
# nem_unbi <- dat.nem.unbi[[site]]
# nem_unbi <- nem_unbi[, c('Date', 'DOF_d', 'DOF_n')]
# names(nem_unbi) <- c('met.date', 'sunrise', 'sunset')
# nem_unbi$sunrise<- with(nem_unbi, sunrise - sunset)
# nem_unbi <- melt(nem_unbi, id.var = 'met.date')
# names(nem_unbi) <- c('met.date', 'solar', 'DOF_mean_unbi')
# 
# unbi_wq <- merge(unbi_wq, nem, by = c('met.date', 'solar'), 
#   all.x = T)
# unbi_wq <- merge(unbi_wq, nem_unbi, by = c('met.date', 'solar'), 
#   all.x = T)
# 
# # for y axis lims
# rng.fun<-function(vec.in){
#   rngs<-range(vec.in,na.rm=T)
#   buffs<-0.07*abs(diff(rngs))
#   c(rngs[1]-buffs,rngs[2]+buffs)
#   }
# 
# ##
# #plot data
# dat.rng<-as.Date(c('2012-01-08','2012-02-25')) 
# date.rng<-dat_wq$Date<=dat.rng[2] & dat_wq$Date>=dat.rng[1]
# to_plo<-unbi_wq[date.rng,]
# 
# # currently, cumsum doesnt work with NA values
# to_plo$DO_unbi <- with(to_plo, 
#     (cumsum(DOF_unbi*0.5/H/1000*32) + 
#       1+mean(DO_mgl, na.rm = T))
#   )
# 
# xlims <- range(to_plo$DateTimeStamp)
# #first
# y.lab<-expression(paste('Observed DO (mg',L^-1,')'))
# polys<-poly.fun(to_plo$solar, dat = to_plo)
# rng.vals<-rng.fun(to_plo$DO_mgl)
# p1<-ggplot(to_plo,aes(x=DateTimeStamp,y=DO_mgl)) + 
#   polys +
#   geom_line() +
#   geom_line(aes(y = DO_unbi), colour = 'red') + 
#   theme_bw() + 
#   ylab(y.lab) + 
#   coord_cartesian(xlim = xlims, ylim=c(3,12)) +
#   scale_fill_manual(values='yellow',labels='Day') +
# #   scale_x_datetime(lims = xlims) +
#   theme(legend.title=element_blank(),legend.position=c(1,0),
#     legend.justification = c(1, 0),
#     legend.background = element_rect(colour = "black")) +
#   xlab('')
# 
# #second
# y.lab<-expression(paste('Predicted tidal elevation (m)'))
# rng.vals<-rng.fun(to_plo$Tide)
# polys<-poly.fun(to_plo$flag,to_plo, 'green')
# p2<-ggplot(to_plo,aes(x=DateTimeStamp,y=Tide)) + 
#   polys +
#   geom_point(aes(colour = fort)) +
#   theme_bw() + 
#   ylab(y.lab) + 
#   coord_cartesian(xlim = xlims, ylim=rng.vals) +
#   scale_fill_manual(values='lightgreen',labels='Antagonistic') +
#   theme(legend.title=element_blank(),legend.position=c(1,0),
#     legend.justification = c(1,0),
#     legend.background = element_rect(colour = "black")) +
#   xlab('')
# 
# #third
# y.lab<-expression(paste('DO flux (',Delta,'DO ',hr^-1, ')'))
# polys<-poly.fun(to_plo$solar,to_plo)
# rng.vals<-rng.fun(to_plo$DOF)
# p3<-ggplot(to_plo,aes(x=DateTimeStamp,y=DOF)) + 
#   polys +
#   geom_line() +
#   geom_line(aes(y = DOF_unbi), colour = 'red') +
#   theme_bw() + 
#   ylab(y.lab) + 
#   coord_cartesian(xlim = xlims, ylim=rng.vals) +
#   scale_fill_manual(values='yellow',labels='Day') +
#   theme(legend.position='none') +
#   geom_hline(yintercept=0,linetype='dashed',size=1) +
#   xlab('')
# 
# #fourth
# y.lab<-expression(paste('Daily/Nightly hourly flux'))
# polys<-poly.fun(to_plo$solar, to_plo)
# rng.vals<-rng.fun(to_plo$DOF_mean_obs)
# p4<-ggplot(to_plo,aes(x=DateTimeStamp,y=DOF_mean_obs)) + 
#   polys +
#   geom_line() +
#   geom_line(aes(y = DOF_mean_unbi), colour = 'red') +
#   theme_bw() + 
#   ylab(y.lab) + 
#   coord_cartesian(xlim = xlims, ylim=rng.vals) +
#   scale_fill_manual(values='yellow',labels='Day') +
#   theme(legend.title=element_blank(),legend.position=c(1,0),
#     legend.justification = c(1,0),
#     legend.background = element_rect(colour = "black")) +
#   geom_hline(yintercept=0,linetype='dashed',size=1) +
#   xlab('')
# 
# grid.arrange(p1,p2,p3,p4,ncol=1)
# #   
# #   
# # tmp <- with(to_plo, 
# #     (cumsum(DOF*0.5/H/1000*32) + 
# #       1+mean(DO_mgl, na.rm = T))
# #   )
# # 
# # plot(to_plo$DO_mgl, tmp)#, xlim = c(200,400), ylim = c(200,400))
# # 
# # abline(0,1)
# #   
# #   

#######
# wrtds detiding

rm(list = ls())

library(foreach)
library(doParallel)

setwd('M:/wq_models/SWMP/raw/rproc/proc5')

# raw wq
site <- 'SFBSM'
load(paste0(site,'.RData'))
dat_wq <- get(site)

# tide
load(paste0('M:/wq_models/SWMP/raw/rproc/tide_preds/', site, '.RData'))
tide <- get(site)

# merge wq and tide
dat_wq <- merge(dat_wq, tide, by = 'DateTimeStamp')

##
# create dec time using jday on 24 hour scale
dec_fun <- function(dat_in, var_nm = 'DateTimeStamp'){

  # get time vec from dat_in
  posix_in  <- dat_in[, var_nm]
  
  #dec time on 24 hr scale
  jday <- as.numeric(format(posix_in, '%j')) 
  hour <- as.numeric(format(posix_in, '%H'))/24
  minu<- as.numeric(format(posix_in, '%M'))/60/24
  
  # add separate vecs on same scale
  dec_time <- jday + hour + minu
  
  out <- data.frame(dat_in, jday, hour, dec_time)
  return(out)
  
  }

##
#function for getting regression weights
#'wt_vars' is name of three variables to weight
#'ref_in' is row of dat.in that is used as reference
#'dat_in' is data to get weights from
#'wins' are the windows for the three wt.vars, values represent halves
#'all' will return all weights, rather than the product of all three
wt_fun<-function(ref_in, dat_in,
  wt_vars = c('jday', 'hour', 'Tide'),
  wins = list(5, 0.25, 2.5),
  all = F){
  
#   # for subroutine
#   dat_in <- dat_wq
#   ref_in <- dat_in[1,]
#   wt_vars = c('jday', 'hour', 'Tide')
#   wins = list(15, 0.5, NULL)
  
  # sanity check
  if(sum(wt_vars %in% names(dat_in)) != length(wt_vars))
    stop('Weighting variables must be named in "dat_in"')
  
  # windows for each of three variables
  wins_1<-wins[[1]]
  wins_2<-wins[[2]]
  wins_3<-wins[[3]]
  
  # default window width for third variable is half its range
  if(is.null(wins[[3]])) wins_3 <- diff(range(dat_in[, wt_vars[3]]))/2
  
  # weighting tri-cube function
  # mirror extends weighting function if vector repeats, e.g. monthly
  # 'dat_cal' is observation for weight assignment
  # 'ref' is reference observation for fitting the model
  # 'win' is window width from above (divided by two)
  # 'mirr' is logical indicating if distance accounts for repeating variables (e.g., month)
  # 'scl_val' is range for the ref vector of obs, used to get correct distance for mirrored obs
  wt_fun_sub <- function(dat_cal, ref, win, mirr = F, scl_val = 1){
    
    # dist_val is distance of value from the ref
    dist_val <- abs(dat_cal - ref)
    
    if(mirr){
      dist_val <- pmin(
        scl_val * (ref/scl_val + 1 - dat_cal/scl_val),
        scl_val * (dat_cal/scl_val + 1 - ref/scl_val),
        dist_val
        )
      }
    
    # get wts if within window
    if(dist_val <= win) return((1 - (dist_val/win)^3)^3)
    
    # otherwise... zero value
    else return(0)
      
    }

  #reference (starting) data
  ref_1 <- as.numeric(ref_in[, wt_vars[1]])
  ref_2 <- as.numeric(ref_in[, wt_vars[2]])
  ref_3 <- as.numeric(ref_in[, wt_vars[3]])

  ##
  # weights for each observation in relation to reference
  # see comments for 'wt_fun_sub' for 'scl_val' argument
  
  # jday
  wts_1 <- sapply(as.numeric(dat_in[, wt_vars[1]]), wt_fun_sub, 
    ref = ref_1, win = wins_1, mirr = T, scl_val = 365) 
  # hour
  wts_2 <- sapply(as.numeric(dat_in[, wt_vars[2]]), wt_fun_sub, 
    ref = ref_2, win = wins_2, mirr = T, scl_val = 1)
  # tide
  wts_3 <- sapply(as.numeric(dat_in[, wt_vars[3]]), wt_fun_sub, 
    ref = ref_3, win = wins_3)
  # all as product 
  out <- wts_1 * wts_2 * wts_3
  
  gr_zero <- sum(out > 0)
  #cat('   Number of weights greater than zero =',gr.zero,'\n')
  
  # extend window widths of weight vector is less than 100
  while(gr_zero < 100){
    
    wins_1 <- 0.1*wins_1 + wins_1
    wins_2 <- 0.1*wins_2 + wins_2
    wins_3 <- 0.1*wins_3 + wins_3
    
    #weights for each observation in relation to reference
    wts_1 <- sapply(as.numeric(dat_in[, wt_vars[1]]), wt_fun_sub, 
      ref = ref_1, win = wins_1, mirr = T)
    wts_2 <- sapply(as.numeric(dat_in[, wt_vars[2]]), wt_fun_sub, 
      ref = ref_2, win = wins_2)
    wts_3 <- sapply(as.numeric(dat_in[, wt_vars[3]]), wt_fun_sub, 
      ref = ref_3, win = wins_3)
    
    out <- wts_1 * wts_2 * wts_3
    
    gr_zero <- sum(out>0)
    
    #cat('   Number of weights greater than zero',gr.zero,'\n')
    
    }
  
  #return all weights if T
  if(all){
    out <- data.frame(wts_1, wts_2, wts_3)
    names(out) <- wt_vars
    return(out)
    }
  
  #final weights are product of all three
  out
  
  }

# add dec.time vector
dat_wq <- dec_fun(dat_wq)

# subset data for now
subs <- 500
dat_wq <- dat_wq[1:subs,]

# window widths to eval
grd_len <-10
jday_wins <- seq(3,15, length = grd_len)
hour_wins <- seq(0.1, 1, length = grd_len)
tide_wins <- seq(0.1, 5, length = grd_len)

# combined widths to eval 
grd_wins <- expand.grid(jday_wins, hour_wins, tide_wins)

cl<-makeCluster(8)
registerDoParallel(cl)

strt <- Sys.time()
res <- foreach(val = 1:nrow(grd_wins)) %dopar% {
# res <- vector('list', length = nrow(grd_wins))
# for(val in 1:nrow(grd_wins)){
  # progress log
  sink('C:/Users/mbeck/Desktop/log1.txt')
  cat(val, ' of ', nrow(grd_wins), '\n')
  print(Sys.time()-strt)
  sink()
  
  cat(val, '\n')
  
  # windows to check
  grd_win <- list(
    jday = grd_wins[val, 1],
    hour = grd_wins[val, 2],
    tide = grd_wins[val, 3]
    )

  # get preds for each row 
  row_out <- matrix(nrow  = nrow(dat_wq), ncol = 2)
  for(row in 1:nrow(dat_wq)){  
    
    ref_in <- dat_wq[row, ]
    
    cat(nrow(dat_wq) - row,'\n')
    
    ref_wts <- wt_fun(ref_in, dat_wq, wins = grd_win)
      
    #OLS wtd model
    mod_md <- lm(
      DO_mgl ~ dec_time + Tide + sin(2*pi*dec_time) + cos(2*pi*dec_time),
      weights = ref_wts,
      data = dat_wq
      )
      
    fit_md <- predict(
      mod_md,
      newdata = data.frame(dec_time = ref_in$dec_time, Tide = ref_in$Tide)
      )
      
    beta_md <- mod_md$coefficients['Tide']
  
    #append to row out for each unique sal
    row_out[row, ] <- c(fit_md, beta_md)
      
    }
  
  row_out
#     res[[val]]  <- row_out
  }
stopCluster(cl)

tmp <- res
save(tmp, file = 'C:/Users/mbeck/Desktop/tmp.RData')

load(file = 'C:/Users/mbeck/Desktop/tmp.RData')
tmp <- lapply(
  tmp, 
  function(x) data.frame(DateTimeStamp = dat_wq$DateTimeStamp, 
    pred = x[,1], parm = x[,2])
  )

tmp <- melt(tmp, id.var = names(tmp[[1]]))

tmp <- merge(tmp, dat_wq[,c('DateTimeStamp', 'DO_mgl')], 
  by = 'DateTimeStamp')

ggplot(tmp, aes(x = DateTimeStamp, y = pred, group = L1, colour = L1)) + 
  geom_line() +
  geom_point(aes(y = DO_mgl), colour = 'black') +
  theme_bw()

# get rmse of pred

library(Metrics)
tmp <- lapply(split(tmp, tmp$L1), function(x){
  
  x <- na.omit(x[, c('pred', 'DO_mgl')])
  
  pred <- x$pred
  obs <- x$DO_mgl
  rmse(obs, pred)
 
  })

err_grd <- data.frame(grd_wins, err = unlist(tmp))
names(err_grd) <- c('jday', 'hour', 'Tide', 'err')
ggplot(err_grd, aes(x = jday , y = hour, z = err, fill = err)) +
  geom_tile() +
  facet_wrap(~ Tide)

######
# tidal grid

rm(list = ls())

library(doParallel)

setwd('M:/wq_models/SWMP/raw/rproc/proc5')

# raw wq
site <- 'SFBSM'
load(paste0(site,'.RData'))
dat_wq <- get(site)

# tide
load(paste0('M:/wq_models/SWMP/raw/rproc/tide_preds/', site, '.RData'))
tide <- get(site)

# merge wq and tide
dat_wq <- merge(dat_wq, tide, by = 'DateTimeStamp')

##
# create dec time using jday on 24 hour scale
dec_fun <- function(dat_in, var_nm = 'DateTimeStamp'){

  # get time vec from dat_in
  posix_in  <- dat_in[, var_nm]
  
  #dec time on 24 hr scale
  jday <- as.numeric(format(posix_in, '%j')) 
  hour <- as.numeric(format(posix_in, '%H'))/24
  minu<- as.numeric(format(posix_in, '%M'))/60/24
  
  # add separate vecs on same scale
  dec_time <- jday + hour + minu
  
  out <- data.frame(dat_in, jday, hour, dec_time)
  return(out)
  
  }

##
#function for getting regression weights
#'wt_vars' is name of three variables to weight
#'ref_in' is row of dat.in that is used as reference
#'dat_in' is data to get weights from
#'wins' are the windows for the three wt.vars, values represent halves
#'all' will return all weights, rather than the product of all three
wt_fun<-function(ref_in, dat_in,
  wt_vars = c('jday', 'hour', 'Tide'),
  wins = list(5, 0.25, 2.5),
  all = F){
  
#   # for subroutine
#   dat_in <- dat_wq
#   ref_in <- dat_in[1,]
#   wt_vars = c('jday', 'hour', 'Tide')
#   wins = list(15, 0.5, NULL)
  
  # sanity check
  if(sum(wt_vars %in% names(dat_in)) != length(wt_vars))
    stop('Weighting variables must be named in "dat_in"')
  
  # windows for each of three variables
  wins_1<-wins[[1]]
  wins_2<-wins[[2]]
  wins_3<-wins[[3]]
  
  # default window width for third variable is half its range
  if(is.null(wins[[3]])) wins_3 <- diff(range(dat_in[, wt_vars[3]]))/2
  
  # weighting tri-cube function
  # mirror extends weighting function if vector repeats, e.g. monthly
  # 'dat_cal' is observation for weight assignment
  # 'ref' is reference observation for fitting the model
  # 'win' is window width from above (divided by two)
  # 'mirr' is logical indicating if distance accounts for repeating variables (e.g., month)
  # 'scl_val' is range for the ref vector of obs, used to get correct distance for mirrored obs
  wt_fun_sub <- function(dat_cal, ref, win, mirr = F, scl_val = 1){
    
    # dist_val is distance of value from the ref
    dist_val <- abs(dat_cal - ref)
    
    if(mirr){
      dist_val <- pmin(
        scl_val * (ref/scl_val + 1 - dat_cal/scl_val),
        scl_val * (dat_cal/scl_val + 1 - ref/scl_val),
        dist_val
        )
      }
    
    # get wts if within window
    if(dist_val <= win) return((1 - (dist_val/win)^3)^3)
    
    # otherwise... zero value
    else return(0)
      
    }

  #reference (starting) data
  ref_1 <- as.numeric(ref_in[, wt_vars[1]])
  ref_2 <- as.numeric(ref_in[, wt_vars[2]])
  ref_3 <- as.numeric(ref_in[, wt_vars[3]])

  ##
  # weights for each observation in relation to reference
  # see comments for 'wt_fun_sub' for 'scl_val' argument
  
  # jday
  wts_1 <- sapply(as.numeric(dat_in[, wt_vars[1]]), wt_fun_sub, 
    ref = ref_1, win = wins_1, mirr = T, scl_val = 365) 
  # hour
  wts_2 <- sapply(as.numeric(dat_in[, wt_vars[2]]), wt_fun_sub, 
    ref = ref_2, win = wins_2, mirr = T, scl_val = 1)
  # tide
  wts_3 <- sapply(as.numeric(dat_in[, wt_vars[3]]), wt_fun_sub, 
    ref = ref_3, win = wins_3)
  # all as product 
  out <- wts_1 * wts_2 * wts_3
  
  gr_zero <- sum(out > 0)
  #cat('   Number of weights greater than zero =',gr.zero,'\n')
  
  # extend window widths of weight vector is less than 100
  while(gr_zero < 100){
    
    wins_1 <- 0.1*wins_1 + wins_1
    wins_2 <- 0.1*wins_2 + wins_2
    wins_3 <- 0.1*wins_3 + wins_3
    
    #weights for each observation in relation to reference
    wts_1 <- sapply(as.numeric(dat_in[, wt_vars[1]]), wt_fun_sub, 
      ref = ref_1, win = wins_1, mirr = T)
    wts_2 <- sapply(as.numeric(dat_in[, wt_vars[2]]), wt_fun_sub, 
      ref = ref_2, win = wins_2)
    wts_3 <- sapply(as.numeric(dat_in[, wt_vars[3]]), wt_fun_sub, 
      ref = ref_3, win = wins_3)
    
    out <- wts_1 * wts_2 * wts_3
    
    gr_zero <- sum(out>0)
    
    #cat('   Number of weights greater than zero',gr.zero,'\n')
    
    }
  
  #return all weights if T
  if(all){
    out <- data.frame(wts_1, wts_2, wts_3)
    names(out) <- wt_vars
    return(out)
    }
  
  #final weights are product of all three
  out
  
  }

# add dec.time vector
dat_wq <- dec_fun(dat_wq)

# subset data for now
subs <- 500
dat_wq <- dat_wq[1:subs,]

# cl<-makeCluster(8)
# registerDoParallel(cl)

tide.div <- 20 #no. of divisions for tidal range

tide.grid<-seq(min(dat_wq$Tide), max(dat_wq$Tide), length=tide.div)

strt <- Sys.time()
# res <- foreach(row = 1:nrow(dat_wq), .combine = 'rbind') %dopar% {
all_out <- NULL
for(row in 1:nrow(dat_wq)){
  
  # progress log
  sink('C:/Users/mbeck/Desktop/log.txt')
  cat(row, ' of ', nrow(dat_wq), '\n')
  print(Sys.time()-strt)
  sink()

  ref_in <- dat_wq[row, ]
  
  row_out <- NULL
  
  for(tide in tide.grid){
    
    cat(row, tide, '\n')
    
    ref_in$Tide <- tide  
    ref_wts <- wt_fun(ref_in, dat_wq)
      
    #OLS wtd model
    mod_md <- lm(
      DO_mgl ~ dec_time + Tide + sin(2*pi*dec_time) + cos(2*pi*dec_time),
      weights = ref_wts,
      data = dat_wq
      )
      
    fit_md <- predict(
      mod_md,
      newdata = data.frame(dec_time = ref_in$dec_time, Tide = ref_in$Tide)
      )
      
    beta_md <- mod_md$coefficients['Tide']
  
    #append to row out for each unique sal
    row_out<- rbind(row_out, c(fit_md, beta_md, tide, row))
      
    }

#   row_out
  all_out <- rbind(all_out, row_out)
  }
# stopCluster(cl)

save(all_out, file = 'C:/Users/mbeck/Desktop/all_out.RData')

###

rm(list = ls())

# get orig wq file
setwd('M:/wq_models/SWMP/raw/rproc/proc5')

site <- 'SFBSM'
load(paste0(site,'.RData'))
dat_wq <- get(site)

# get pred tide
load(paste0('M:/wq_models/SWMP/raw/rproc/tide_preds/', site, '.RData'))
tide <- get(site)

# merge wq and tide
dat_wq <- merge(dat_wq, tide, by = 'DateTimeStamp')

# subset data for now
subs <- 5000
dat_wq <- dat_wq[1:subs,]

# load tide interp mat
int_grd <- paste0(site, '_intgrd')
load(file = paste0('C:/Users/mbeck/Desktop/', int_grd,'.RData'))

int_grd <- get(int_grd)
int_grd <- data.frame(int_grd, row.names = 1:nrow(int_grd))

int_grd$DateTimeStamp <- rep(dat_wq$DateTimeStamp, each = 20)

# pred lines by tidal value
ggplot(int_grd, aes(x = DateTimeStamp, y = DO_pred, group = Tide, 
  colour = Tide)) + 
  geom_point() + 
  geom_line()

# contour of interp grid
cont_plo <- dcast(int_grd[1000:15000,], DateTimeStamp ~ Tide, value.var = 'DO_pred')

x.val <- cont_plo[,1]
y.val <- as.numeric(names(cont_plo)[-1])
z.val <- as.matrix(cont_plo[,-1])

filled.contour(x.val, y.val, z.val, nlevels = 100, ylab = 'Tide')

##
# predicted from int grid using data.table

library(plyr)

pred <- merge(int_grd, dat_wq[, c('DateTimeStamp', 'DO_mgl', 'Tide')],
  by = 'DateTimeStamp')

pred <- ddply(pred,
  .variables = 'DateTimeStamp',
  .fun = function(x){
    ind <- which.min(abs(with(x, Tide.x - Tide.y)))
    x[ind,]
  })
    
ggplot(pred, aes(x = DateTimeStamp, y = DO_mgl)) + 
  geom_point(colour = 'lightgreen') + 
  geom_line(aes(y = DO_pred), colour = 'blue')

## normalized
# uses predicted
# and int grid

norm <- pred
norm$hr <- as.numeric(format(norm$DateTimeStamp, '%H')) + as.numeric(format(norm$DateTimeStamp, '%M'))/60 

norm$DO_nrm <- NA
for(val in 1:nrow(norm)){
  
  cat(val, '\n')
  
  # row to normalize
  to_nrm <- norm[val, ]
  
  # corresponding values for datetimestamp on int_grd
  day_int <- int_grd[int_grd$DateTimeStamp == to_nrm$DateTimeStamp,]
  
  # find all obs tidal values at the same time as to_nrm
  tide_nrm <- norm[norm$hr == to_nrm$hr, 'Tide.x']
  
  # DO values to normalize
  # matches all tides occuring at the hour to those in day_int
  DO_vals <- merge(
    data.frame(Tide = tide_nrm), 
    day_int[, c('DO_pred', 'Tide')], 
    by = 'Tide', all.x = T
    )
  
  # take ave
  DO_nrm <- mean(DO_vals$DO_pred)
  
  # append to data
  norm[val, 'DO_nrm'] <- DO_nrm
  
  }

ggplot(norm[2500:3000,], aes(x = DateTimeStamp, y = DO_mgl)) + 
  geom_point() + 
  geom_line(aes(y = DO_pred), colour = 'red') +
  geom_line(aes(y = DO_nrm), colour = 'blue') +
  theme_bw()

##
# get metab, obs, pred, and nrm

to_met <- merge(dat_wq, norm[, c('DateTimeStamp', 'DO_pred', 'DO_nrm')])

source('M:/r_code/SWMP/NEM_fun.r')

# cols to remove, probably not necessary
rm_col <- c('DO_pred', 'DO_nrm', 'Tide', 'jday', 'hour', 'dec_time')

# obs
met_in <- to_met
met_in <- met_in[, !names(met_in) %in% rm_col]
obs_met <- nem.fun(met_in, 'SFBSM')

# pred
met_in <- to_met
met_in$DO_mgl <- met_in$DO_pred
met_in <- met_in[, !names(met_in) %in% rm_col]
pred_met <- nem.fun(met_in, 'SFBSM')

# norm
met_in <- to_met
met_in$DO_mgl <- met_in$DO_nrm
met_in <- met_in[, !names(met_in) %in% rm_col]
nrm_met <- nem.fun(met_in, 'SFBSM')

# yessssssssssssssssssssssss
anoms.fun(obs_met)
anoms.fun(pred_met)
anoms.fun(nrm_met)

rm(list=ls())

library(reshape)
library(ggplot2)
library(gridExtra)

to.plos<-list(obs_met, pred_met, nrm_met)
names(to.plos) <- c('observed', 'predicted', 'detided')

ylabs<-expression(paste('mmol ', O[2],' ', m^-2,' ', d^-1))
  
for(out in 1:length(to.plos)){
  
  cat(out,'of',length(to.plos),'\n')
  
  to.plo<-to.plos[[out]]
  stat<-as.character(unique(to.plo$Station))
  stat.name<-stat.name.fun(stat)
  stat.name<-paste0(stat.name,' (',stat,')')
  stat.name <- paste0(stat.name, ' - ', names(to.plos)[out])
  
  to.plo<-melt(to.plo,id.var='Date', measure.var=c('Pg','Rt','NEM'))
#   to.plo<-to.plo[which(!is.na(to.plo$value))[1]:nrow(to.plo),]
  lims<-c(to.plo[1,1],to.plo[nrow(to.plo),1])
  
  p<-ggplot(to.plo,aes(x=Date,y=value,group=variable,colour=variable)) + 
    geom_line() + 
    geom_point() +
    scale_y_continuous(name=ylabs) +
    scale_x_date(limits=lims) + 
    theme_bw() +
    ggtitle(stat.name)
  
  plo_nm1 <- paste0('p', out)

  assign(plo_nm1, p)

  }

pdf('C:/Users/mbeck/Desktop/metab_detide.pdf',height=14,width=10,family='serif')
grid.arrange(p1,p2,p3, ncol=1)
dev.off()

#####
# check pred/norms for stations from each of six clusters

rm(list = ls())

sites <- c('GRBLR', 'PDBJL', 'CBMIP', 'WELSM', 'HUDSC', 'SFBSM')

# get orig wq file
setwd('M:/wq_models/SWMP/raw/rproc/proc5')

site <- sites[3]
load(paste0(site,'.RData'))
dat_wq <- get(site)

# subset data for now
frst <- which(as.character(dat_wq$DateTimeStamp) == '2001-07-07 12:00:00')
last <- which(as.character(dat_wq$DateTimeStamp) == '2001-07-28 07:30:00')
dat_wq <- dat_wq[frst:last,]

# get pred tide
load(paste0('M:/wq_models/SWMP/raw/rproc/tide_preds/', site, '.RData'))
tide <- get(site)

# merge wq and tide
dat_wq <- merge(dat_wq, tide, by = 'DateTimeStamp')

# # subset data
# subs <- 10000
# dat_wq <- dat_wq[1:subs,]

# load tide interp mat
int_grd <- paste0(site, '_intgrd')
load(file = paste0('C:/Users/mbeck/Desktop/', int_grd,'.RData'))

int_grd <- get(int_grd)
int_grd <- data.frame(int_grd, row.names = 1:nrow(int_grd))

int_grd$DateTimeStamp <- rep(dat_wq$DateTimeStamp, each = 20)

# pred lines by tidal value
ggplot(int_grd, aes(x = DateTimeStamp, y = DO_pred, group = Tide, 
  colour = Tide)) + 
  geom_point() + 
  geom_line() + 
  theme_bw()

# contour of interp grid
cont_plo <- dcast(int_grd, DateTimeStamp ~ Tide, value.var = 'DO_pred')

x.val <- cont_plo[,1]
y.val <- as.numeric(names(cont_plo)[-1])
z.val <- as.matrix(cont_plo[,-1])

filled.contour(x.val, y.val, z.val, nlevels = 100, ylab = 'Tide')

##
# predicted from int grid using data.table

library(plyr)

pred <- merge(int_grd, dat_wq[, c('DateTimeStamp', 'DO_mgl', 'Tide')],
  by = 'DateTimeStamp')

pred <- ddply(pred,
  .variables = 'DateTimeStamp',
  .fun = function(x){
    ind <- which.min(abs(with(x, Tide.x - Tide.y)))
    x[ind,]
  })
    
ggplot(pred, aes(x = DateTimeStamp, y = DO_mgl)) + 
  geom_point(colour = 'lightgreen') + 
  geom_line(aes(y = DO_pred), colour = 'blue') + 
  ylim(c(0,10))

summary(lm(DO_pred ~ DO_mgl, data = pred)
## normalized
# uses predicted
# and int grid

norm <- pred
norm$hr <- as.numeric(format(norm$DateTimeStamp, '%H')) + as.numeric(format(norm$DateTimeStamp, '%M'))/60 

DO_nrm <- ddply(
  int_grd, 
  .variables = 'DateTimeStamp',
  .fun = function(nrm_dt){
    
    cat(unique(nrm_dt$row), '\n')
    
    to_nrm <- norm[norm$DateTimeStamp == nrm_dt$DateTimeStamp, ]
    
    # find all obs tidal values at the same time as to_nrm
    tide_nrm <- norm[norm$hr == to_nrm$hr, 'Tide.x']
    
    # DO values to normalize
    # matches all tides occuring at the hour to those in day_int
    DO_vals <- merge(
      data.frame(Tide = tide_nrm), 
      nrm_dt[, c('DO_pred', 'Tide')], 
      by = 'Tide', all.x = T
      )
  
    # take ave
    DO_nrm <- mean(DO_vals$DO_pred)
    names(DO_nrm) <- 'DO_nrm'
    
    return(DO_nrm)
    }
  )

norm <- merge(norm, DO_nrm, by = 'DateTimeStamp')
  
ggplot(norm, aes(x = DateTimeStamp, y = DO_mgl)) + 
  geom_point() + 
  geom_line(aes(y = DO_pred), colour = 'red') +
  geom_line(aes(y = DO_nrm), colour = 'blue') +
  theme_bw()

##
# get metab, obs, pred, and nrm

to_met <- merge(dat_wq, norm[, c('DateTimeStamp', 'DO_pred', 'DO_nrm')])

source('M:/r_code/SWMP/NEM_fun.r')

# cols to remove, probably not necessary
rm_col <- c('DO_pred', 'DO_nrm', 'Tide', 'jday', 'hour', 'dec_time')

# obs
met_in <- to_met
met_in <- met_in[, !names(met_in) %in% rm_col]
obs_met <- nem.fun(met_in, site)

# pred
met_in <- to_met
met_in$DO_mgl <- met_in$DO_pred
met_in <- met_in[, !names(met_in) %in% rm_col]
pred_met <- nem.fun(met_in, site)

# norm
met_in <- to_met
met_in$DO_mgl <- met_in$DO_nrm
met_in <- met_in[, !names(met_in) %in% rm_col]
nrm_met <- nem.fun(met_in, site)

# check perc anom chng
anoms.fun(obs_met)
anoms.fun(pred_met)
anoms.fun(nrm_met)

######
#

