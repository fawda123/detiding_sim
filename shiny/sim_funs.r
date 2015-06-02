######
#gets station name, first site then station, separated by comma
#input is five character string for site uppe case, e.g., 'ACEBB'
stat.name.fun<-function(stat){
  
  if(!exists('dat.meta')) 
    dat.meta<-read.csv('M:/wq_models/SWMP/sampling_stations.csv',header=T,
      strip.white=T)
  
  out<-grep(tolower(stat),dat.meta$Station.Code)
  out<-unique(dat.meta[out,names(dat.meta) %in% c('Station.Name','Reserve.Name')])
  out<-paste(out[,'Reserve.Name'],out[,'Station.Name'],sep=', ')
  
  return(out)
  
  }

######
#creates simulated DO curve from sine wave
#input is vector of POSIX date objects at 30 min interals
#'do.mean' is used to change intercept
#'do.amp' changes amplitude
#'freq' changes period, this value is set to the number of observations within a period
DO.bio.fun<-function(time.in,sunrise='06:30:00',do.mean=8,do.amp=1,freq=48){
 
  # time.step per hour
  time.step <- 60/unique(diff(time.in))
  
  #time diff between strt of vector and sunrise
  sunrise <- paste(as.Date(min(time.in)), sunrise)
  sunrise <- as.POSIXct(sunrise, tz = attr(time.in, 'tzone'))
  cor.phs <- time.step * as.numeric(difftime(sunrise, min(time.in)))
  cor.phs <- 2 * pi * (1/freq) * cor.phs
  cor.phs <- cor.phs - pi
	
  # get input values to cos func
	in.vals <- seq(0,length(time.in), length = length(time.in))
  in.vals <- in.vals * 2 * pi * (1/freq)

	#amplitude change
	amp<-do.amp
	
	#intercept
	int<-do.mean
  
  # DO signal
	DO<-int+amp*cos(in.vals - cor.phs)
	
	return(DO)
	
	}

######
# creates simulated tidal signal as additive combination of sine waves
# resulting tide is scaled from 0 to 1 and added to 'scl.up'
# uses DO.bio.fun above
# 'time.in' is posix time vector for simulating tides
# 'waves.in' is list of each tidal component with two elements, period and amp
# 'scl.up' value to add to additive tidal vector
# 'all' is logical if each component should be returned
tide.fun <- function(time.in, waves.in, scl.up = 4, all = F){
  
  require(scales)
  
  # simulate each component from DO.bio.fun
  tide <- sapply(
    waves.in, 
    function(x){
      x[1] <- 2*x[1] # for 30 min obs
      DO.bio.fun(time.in, do.mean = 0, 
        do.amp = x[[2]], freq = x[[1]])
      }
    ) 
  
  # return all components if T
  if(all) return(tide)
  
  # add each component, center/scale at zero, 
  # change range to 1m, shift up to some arbitrary mean height
  tide <- scl.up + scales::rescale(scale(rowSums(tide)))
  
  # combine with data frame and add dtide/dt
  dTide <- c(diff(tide)[1], diff(tide))        

  # output
  out <- data.frame(DateTimeStamp = time.in, Tide = tide, dTide = dTide)

  return(out)
  
  }

######
#calculate number of anomolous estimates from nem output
#'nem.in' is output from 'nem.fun'
anoms.fun<-function(nem.in){
  Pg<-nem.in$Pg
  Pg<-sum(Pg<=0,na.rm=T)/length(na.omit(Pg))
  Rt<-nem.in$Rt
  Rt<-sum(Rt>=0,na.rm=T)/length(na.omit(Rt))
  return(data.frame(Pg,Rt))
  }

######
#r.squared function
#created for data from weighted regression, but works for all models
#residuals are observed - predicted, but taken from model objects (see 'epc_mods.R')
rsq.fun<-function(resid,obs){
  
  require(Metrics)
  
  ssr<-sum(resid^2,na.rm=T)
  sst<-sum(se(obs,mean(obs,na.rm=T)),na.rm=T)
  
  return(1 - (ssr/sst))
  
}

######
#variant of rmse fun in Metrics package but handles na values
#resid is obs - predicted
rmse.fun<-function(resid){
  
  out<-sqrt(mean(resid^2,na.rm=T))
    
  return(out) 
  
  }

######
# found on SO
# creates summary data frame from a larger data frame
# 'data' is input df
# 'measurevar' is variable to summarize
# 'groupvars' is chr vector of variables to condition summary
# similar to aggregate function
summarySE <- function(data=NULL, measurevar, groupvars=NULL, narm=FALSE,
                      conf.interval=.95, .drop=TRUE) {
    require(plyr)

    # New version of length which can handle NA's: if na.rm==T, don't count them
    length2 <- function (x, na.rm=narm) {
        if (narm) sum(!is.na(x))
        else       length(x)
    }

    # This does the summary. For each group's data frame, return a vector with
    # N, mean, and sd
    datac <- ddply(data, groupvars, .drop=.drop,
      .fun = function(xx, col) {
        c(N    = length2(xx[[col]], na.rm=narm),
          mean = mean   (xx[[col]], na.rm=narm),
          sd   = sd     (xx[[col]], na.rm=narm)
        )
      },
      measurevar
    )

    # Rename the "mean" column    
    datac <- rename(datac, c("mean" = measurevar))

    datac$se <- datac$sd / sqrt(datac$N)  # Calculate standard error of the mean

    # Confidence interval multiplier for standard error
    # Calculate t-statistic for confidence interval: 
    # e.g., if conf.interval is .95, use .975 (above/below), and use df=N-1
    ciMult <- qt(conf.interval/2 + .5, datac$N-1)
    datac$ci <- datac$se * ciMult

    return(datac)
}

######
# function for plotting repeating polygons in ggplot
# 'flag.in' is vector indicating binomial variable for polys
# 'flag.in' can be factor or numeric (w/ two values)
# 'dat' is data.frame with 'flag.in'
# 'fill.val' is fill colour of polygons
# output is geom object
poly.fun<-function(flag.in,dat, fill.val='yellow1'){

  require(reshape2)
  require(ggplot2)
  
  #for flag bias
  if(class(flag.in) == 'numeric'){ 
    neg.dates<-with(
      dat,
      DateTimeStamp[which(flag.in < 0)]
      )
    tz<-attr(neg.dates,'tzone')
    diffs<-c(0,diff(as.numeric(neg.dates)))
    strt.ind<-c(1,which(diffs>1800))
    end.ind<-c(strt.ind-1,length(flag.in))
    comb<-paste(neg.dates[strt.ind],neg.dates[end.ind],sep="\t")
    
    if(grepl('NA',comb[length(comb)])) 
      comb[length(comb)]<-gsub('NA',neg.dates[length(neg.dates)],comb[length(comb)])
  
    comb<-do.call('rbind',strsplit(comb,'\t'))
    comb<-cbind(comb,comb[,2],comb[,1])
  
    x.vals<-suppressMessages(melt(sapply(1:nrow(comb), 
      function(x) comb[x,],simplify=F))$value)
    x.vals<-as.POSIXct(as.character(x.vals),tz,
      format='%Y-%m-%d %H:%M:%S')
    y.vals<-rep(c(-1000,-1000,1000,1000),length=length(x.vals)) 
    Antagonistic<-rep(1:(length(x.vals)/4),each=4)
    polys<-data.frame(x.vals,y.vals,grp=Antagonistic)
    
    }

  #for sunset/rise
  if(class(flag.in) == 'factor'){
    
    plo.dates<-unique(dat[,c('solar','value')])
    
    if(plo.dates$solar[1] == 'sunset') 
      plo.dates<-plo.dates[-1,]
    if(plo.dates$solar[nrow(plo.dates)] == 'sunrise')
      plo.dates<-rbind(
        plo.dates,
        data.frame(solar='sunset',value=max(dat$DateTimeStamp))
        )
    
    plo.dates$inds<-rep(1:(nrow(plo.dates)/2),each=2)
    tz<-attr(plo.dates$value,'tzone')
    plo.dates$value <- as.character(plo.dates$value)
    plo.dates<-dcast(plo.dates,inds~solar,value.var='value')
    plo.dates<-with(plo.dates,
      data.frame(sunrise,sunset,sunset,sunrise)
      )
   
    x.vals <- sapply(1:nrow(plo.dates), 
           function(x) plo.dates[x,],simplify=F)
    x.vals<-suppressMessages(
      melt(x.vals, measure.vars = names(x.vals[[1]]))$value
      )
    x.vals<-as.POSIXct(x.vals, tz, origin = '1970-01-01')
    y.vals<-rep(c(-1000,-1000,1000,1000),nrow(plo.dates))
    Day<-as.character(trunc(x.vals,'days'))
    polys<-data.frame(x.vals,y.vals,grp=Day)

    }
  
  out<-geom_polygon(data=polys,aes(x.vals,y.vals,group=grp,fill='grp'),alpha=0.6)
  
  return(out)
  
  }

######
# create dec time using jday on 24 hour scale
# 'dat_in' is data frame input with time vecot as 'var_nm'
# output is same data frame including 'dec_time' column
dec_fun <- function(dat_in, var_nm = 'DateTimeStamp'){

  # get time vec from dat_in
  posix_in  <- dat_in[, var_nm]
  
  #dec time on 24 hr scale
  jday <- as.numeric(format(posix_in, '%j')) 
  hour <- as.numeric(format(posix_in, '%H'))
  minu<- as.numeric(format(posix_in, '%M'))
  
  # add separate vecs on same scale
  dec_time <- jday + (hour/24) + (minu/60/24)
  
  # convert hour back to 24 hour scale, add minutes
  hour <- hour + minu/60
  
  out <- data.frame(dat_in, jday, hour, dec_time)
  return(out)
  
  }

######
#function for getting regression weights
# note that this subsets the input data frame for faster wt selection
# subset is by limiting window for product of weights (dec_time)
# subsetted weights are recombined to equal vector of length = nrow(dat_in)
#'wt_vars' is name of three variables to weight
#'ref_in' is row of dat.in that is used as reference
#'dat_in' is data to get weights from
#'wins' are the windows for the three wt.vars, values represent halves
#'all' will return all weights, rather than the product of all three
#'slice' is logical for subsetting 'dat_in' for faster wt selection
#'subs_only' is logical for returning only wt vectors that are non-zero
wt_fun <- function(ref_in, dat_in,
  wt_vars = c('dec_time', 'hour', 'Tide'),
  wins = list(4, 12, NULL),
  all = F, 
  slice = T, 
  subs_only = F){
  
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
    dist_val <- sapply(ref, function(x) abs(dat_cal - x))
    
    # repeat if distance is checked on non-continuous number line
    if(mirr){
      
        dist_val <- pmin(
          sapply(ref, function(x)
            abs(x + scl_val - dat_cal)),
          sapply(ref, function(x) abs(dat_cal + scl_val - x)),
          dist_val
          )
      
      }
    
    # get wts within window, otherwise zero
    win_out <- dist_val > win
    dist_val <- (1 - (dist_val/win)^3)^3
    dist_val[win_out] <- 0
      
    return(dist_val)
      
    }

  #reference (starting) data
  ref_1 <- as.numeric(ref_in[, wt_vars[1]])
  ref_2 <- as.numeric(ref_in[, wt_vars[2]])
  ref_3 <- as.numeric(ref_in[, wt_vars[3]])

  ##
  # subset 'dat_in' by max window size for faster calc
  # this is repeated if min number of wts > 0 is not met
  # subset vector is all T if not using subset
  dec_rng <- range(dat_in$dec_time)
  ref_time <- unique(ref_in$dec_time)
  dec_sub <- with(dat_in, 
    dec_time > 
      ref_time - wins_1 * 5 & dec_time < ref_time + wins_1 * 5
    )
  if(!slice) dec_sub <- rep(T, length = nrow(dat_in))
  dat_sub <- dat_in[dec_sub, ]

  ##
  # weights for each observation in relation to reference
  # see comments for 'wt_fun_sub' for 'scl_val' argument
  
  # jday
  wts_1 <- wt_fun_sub(as.numeric(dat_sub[, wt_vars[1]]), 
    ref = ref_1, win = wins_1, mirr = F) 
  # hour
  wts_2 <- wt_fun_sub(as.numeric(dat_sub[, wt_vars[2]]), 
    ref = ref_2, win = wins_2, mirr = T, scl_val = 24)
  # tide
  wts_3 <- wt_fun_sub(as.numeric(dat_sub[, wt_vars[3]]), 
    ref = ref_3, win = wins_3, mirr = F)
  # all as product 
  out <- sapply(1:nrow(ref_in), function(x) wts_1[, x] * wts_2[, x] * wts_3[, x])
  
  gr_zero <- colSums(out > 0)
  #cat('   Number of weights greater than zero =',gr.zero,'\n')
  
  # extend window widths of weight vector is less than 100
  while(any(gr_zero < 100)){
    
    # increase window size by 10%
    wins_1 <- 1.1 * wins_1
    wins_2 <- 1.1 * wins_2
    wins_3 <- 1.1 * wins_3 
    
    # subset again
    dec_sub <- with(dat_in, 
      dec_time > ref_time - wins_1 * 5 & dec_time < ref_time + wins_1 * 5
      )
    if(!slice) dec_sub <- rep(T, length = nrow(dat_in))
    dat_sub <- dat_in[dec_sub, ]
    
    #weights for each observation in relation to reference
    wts_1 <- wt_fun_sub(as.numeric(dat_sub[, wt_vars[1]]), 
      ref = ref_1, win = wins_1, mirr = F)
    wts_2 <- wt_fun_sub(as.numeric(dat_sub[, wt_vars[2]]), 
      ref = ref_2, win = wins_2, mirr = T, scl_val = 24)
    wts_3 <- wt_fun_sub(as.numeric(dat_sub[, wt_vars[3]]), 
      ref = ref_3, win = wins_3, mirr = F)
    
    out <- sapply(1:nrow(ref_in), 
      function(x) wts_1[, x] * wts_2[, x] * wts_3[, x])
    
    gr_zero <- colSums(out > 0)
    
    }
  
  if(subs_only){
    
    nms <- which(dec_sub)
    out <- alply(out, 2, function(x) {
    
      to_sel <- x > 0
      tmp <- x[to_sel]
      names(tmp) <- which(dec_sub)[to_sel]
      tmp
    
      })
    
    return(out)
    
    }
  
  # extend weight vectors to length of dat_in
  empty_mat <- matrix(0, ncol = nrow(ref_in), nrow = nrow(dat_in))
  empty_fill <- function(wts_in) {
    out <- empty_mat
    out[dec_sub,] <- wts_in
    out
    }
  wts_1 <- empty_fill(wts_1)
  wts_2 <- empty_fill(wts_2)
  wts_3 <- empty_fill(wts_3)  
  out <- empty_fill(out)

  #return all weights if T
  if(all){
    out <- data.frame(dat_in$DateTimeStamp, 
      wts_1, wts_2, wts_3, out)
    names(out) <- c('DateTimeStamp', wt_vars, 'final')
    return(out)
    }
  
  #final weights are product of all three
  out
  
  }

######
# creates data for DO simulation, uses functions above
# 'time_in' is POSIX vector of continuous time series to estimate, currently as 30 minute obs
# 'do.amp' is amplitude of biological DO signal
# 'tide_cat' is either a list of tidal components, 
# a character string as 'Diurnal', 'Semidiurnal', or 'Mixed Semidiurnal', 
# or a data.frame of tidal values
# 'tide_assoc' is value +- range of DO_adv when converting Tide (i.e., amp)
# 'err_rng_obs' is sd of obs uncertainty for DO signal
# 'err_rng_pro' is bounded range +/- or proc unc
# 'seeded' is logical indicating if a random seed is used for obs/pro error
# output is data drame with DateTimeStamp, jday, hour, dec_time,
# sunrise, Tide, DO_bio, DO_adv, DO_tid, DO_noi, DO_obs
ts_create <- function(time_in, do.amp, tide_cat, tide_assoc, err_rng_obs, 
  err_rng_pro, seeded = T){

  require(scales)
  
  # start data.frame for plotting
  DO_sim <- data.frame(
    DateTimeStamp = time_in
    )
  
  # add decimal time on julian day
  DO_sim <- dec_fun(DO_sim)

  # add column for dec_time starting from one (for plotting)
  DO_sim$Day <- with(DO_sim, (dec_time + 1) - jday[1])
  
  # add vector for sunrise, -1 is daylight, 1 is nighttime
  DO_sim$sunrise <- 1
  day_vec <- with(DO_sim, 24*(dec_time - jday))
  DO_sim$sunrise[day_vec > 6 & day_vec <= 18] <- -1
  
  # move dec_time to correspond to meteorological day
  # note this is lazy, only works if sunrise is 630
  DO_sim$dec_time  <- DO_sim$dec_time - 6.5/24
  
  # get tide if character
  if(grepl('character|factor', class(tide_cat))){
    if(tide_cat == 'Semidiurnal')
      waves.in <- list(c(12.42, 1)) # M2, principal lunar semidiurnal
    if(tide_cat == 'Diurnal') 
      waves.in <- list(c(25.82, 1)) #O1, principal lunar diurnal
    if(tide_cat == 'Mixed Semidiurnal')
      waves.in <- list(c(12.42, 1), c(25.82, 1)) 
    
    tide <- tide.fun(time_in, waves.in)
    }

  # get tide if list
  if(class(tide_cat) == 'list')
    tide <- tide.fun(time_in, waves.in)
  
  # get tide if supplied as data.frame, first col is posix, second is tide
  if(class(tide_cat) == 'data.frame'){
    tide <- tide_cat
    tide$Tide <- scales::rescale(tide$Tide, to = c(4, 5))
    }

  # combine with data frame and add dtide/dt
  DO_sim$Tide <- tide$Tide
  
  ##
  # create biological DO
  
  # obs and proc uncertainty
  if(seeded) set.seed(1234)
  DO_sim$e_obs <- rnorm(nrow(DO_sim), 0, err_rng_obs)
  if(seeded) set.seed(4321)
  DO_sim$e_pro <- scales::rescale(cumsum(rnorm(nrow(DO_sim), 0, err_rng_pro)), 
    to = c(-1 * err_rng_pro, err_rng_pro))
  DO_sim$e_tot <- with(DO_sim, e_obs + e_pro)

  # add uncertainty to clean DO signal
  DO_sim$DO_bio <- DO.bio.fun(time_in, do.amp = do.amp)
  DO_sim$DO_bio <- pmax(0, with(DO_sim, DO_bio + e_tot))

  # add tidal effect
  DO_sim$DO_adv <- scales::rescale(DO_sim$Tide, to = c(-1*tide_assoc, tide_assoc))
  DO_sim$DO_obs <- with(DO_sim, DO_bio + DO_adv)

  # floor at zero
  DO_sim$DO_obs <- with(DO_sim, pmax(0, DO_obs))
  
  # 
  
  return(DO_sim)
  
  }

######
# get predicted, normalized values not using interp grid, tide as predictor
# 'dat_in' is raw data used to create 'grd_in' and used to get predictions
# 'DO_obs' is string indicating name of col for observed DO values from 'dat_in'
# output is data frame same as 'dat_in' but includes predicted and norm columns
wtreg_fun <- function(dat_in, DO_obs = 'DO_obs', wins = list(4, 12, NULL),
  parallel = F, progress = F){

  # get mean tidal height from empirical data
  mean_tide <- mean(dat_in$Tide)

  #for counter
  strt <- Sys.time()
  
  out <- ddply(dat_in, 
    .variable = 'DateTimeStamp',
    .parallel = parallel, 
    .fun = function(row){
      
      # row for prediction
      ref_in <- row
      ref_in <- ref_in[rep(1, 2),]
      ref_in$Tide <- c(unique(ref_in$Tide), mean_tide)
      
      # progress
      if(progress){
        prog <- which(row$DateTimeStamp == dat_in$DateTimeStamp)
        sink('log.txt')
        cat('Log entry time', as.character(Sys.time()), '\n')
        cat(prog, ' of ', nrow(dat_in), '\n')
        print(Sys.time() - strt)
        sink()
        }
      
      # get wts
      ref_wts <- wt_fun(ref_in, dat_in, wins = wins, slice = T, 
        subs_only = T, wt_vars = c('dec_time', 'hour', 'Tide'))
  
      #OLS wtd model
      out <- lapply(1:length(ref_wts),
        function(x){
          
          # subset data for weights > 0
          dat_proc <- dat_in[as.numeric(names(ref_wts[[x]])),]
          
          # if no DO values after subset, return NA
          # or if observed DO for the row is NA, return NA
          if(sum(is.na(dat_proc$DO_obs)) == nrow(dat_proc)|
              any(is.na((ref_in$DO_obs)))){
            
            DO_pred <- NA
            beta <- NA
            Tide <- ref_in$Tide[x]
            
            } else {
            
              # subset weigths > 0, rescale weights average
              ref_wts <- ref_wts[[x]]/mean(ref_wts[[x]])
            
              # get model
              mod_md <- lm(
                DO_obs ~ dec_time + Tide, # + sin(2*pi*dec_time) + cos(2*pi*dec_time),
                weights = ref_wts,
                data = dat_proc
                )
            
              # get prediction from model
              Tide <- ref_in$Tide[x]
              DO_pred <- predict(
                mod_md, 
                newdata = data.frame(dec_time = ref_in$dec_time[x], Tide = Tide)
                )
            
              # get beta from model
              beta <- mod_md$coefficients['Tide']
            
            }
          
          # output
          DO_pred
          
          }
        
        )

      out <- unlist(out)
      names(out) <- c('DO_prd', 'DO_nrm')
      out
      
      })
  
  out$DateTimeStamp <- NULL
  out <- cbind(dat_in, out)

  return(out)
  
  }

######
# using expressions in facet_wrap labels
# http://stackoverflow.com/questions/19282897/how-to-add-expressions-to-labels-in-facet-wrap
facet_wrap_labeller <- function(gg.plot,labels=NULL) {
  #works with R 3.0.1 and ggplot2 0.9.3.1
  require(gridExtra)

  g <- ggplotGrob(gg.plot)
  gg <- g$grobs      
  strips <- grep("strip_t", names(gg))

  for(ii in seq_along(labels))  {
    modgrob <- getGrob(gg[[strips[ii]]], "strip.text", 
                       grep=TRUE, global=TRUE)
    gg[[strips[ii]]]$children[[modgrob$name]] <- editGrob(modgrob,label=labels[ii])
  }

  g$grobs <- gg
  class(g) = c("arrange", "ggplot",class(g)) 
  g
}

######
# gets station location data for ggmap plotting
# 'val_in' is character string of first chrs of reserve, or five chrs for a site
# if only reserve is given, all sites are returned
# input is not case-sensitive
# returns appropriate row(s) from sampling_stations.csv
get_map_meta <- function(val_in){
  
  #removing trailing whit space in chr strings
  trim.trailing <- function(x) sub('^\\s+|\\s+$', '', x)

  # station metadata
  stats<-read.csv('M:/wq_models/SWMP/sampling_stations.csv',header=T)
  stats<-stats[grep('Active*',stats$Status),]
  
  # subset by reserve
  stats <- stats[grep(paste0(tolower(val_in),'.*wq'), stats[,'Station.Code']),]
  stats$Longitude <- -1*stats$Longitude
  stats$Station <- trim.trailing(as.character(stats$Station.Name))
  stats$Station.Code <- toupper(substr(trim.trailing(as.character(stats$Station.Code)),1,5))
  
  return(stats)
  
  }
  

