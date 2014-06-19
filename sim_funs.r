#functions for NEM processing of NERRS data
#created Dec. 2013 by M. Beck, adapted from 'spam_NEM_fun.r' and M. Murrell

#funcion that splits dataset into 24hr days based on sunrise
#merge with original data
met.day.fun<-function(dat.in, stat.in, 
  meta.path = 'M:/wq_models/SWMP/sampling_stations.csv'
  ){

  require(StreamMetabolism)  #for sunrise.set function
  
  if(!exists('dat.meta')) 
    dat.meta<-read.csv(meta.path,header=T)
  
  stat.meta<-toupper(paste0(stat.in,'WQ'))
  stat.meta<-dat.meta[grep(stat.meta,toupper(dat.meta$Station.Code)),]
  
  # all times are standard - no DST!
  gmt.tab<-data.frame(
    gmt.off=c(-4,-5,-6,-8,-9),
    tz=c('America/Virgin', 'America/Jamaica', 'America/Regina',
      'Pacific/Pitcairn', 'Pacific/Gambier'),
    stringsAsFactors=F
    )
  
  #get sunrise/sunset times using sunrise.set function from StreamMetabolism
  lat<-stat.meta$Latitude
  long<--1*stat.meta$Longitude
  GMT.Offset<-stat.meta$GMT.Offset
  tz<-gmt.tab[gmt.tab$gmt.off==GMT.Offset,'tz']
  start.day<-format(dat.in$DateTimeStamp[which.min(dat.in$DateTimeStamp)]-(60*60*24),format='%Y/%m/%d')
  tot.days<-1+length(unique(as.Date(dat.in$DateTimeStamp)))
  
  #ss.dat is matrix of sunrise/set times for each days  within period of obs
  ss.dat<-suppressWarnings(sunrise.set(lat,long,start.day,tz,tot.days))
  
  #remove duplicates, sometimes sunrise.set screws up
  ss.dat<-ss.dat[!duplicated(strftime(ss.dat[,1],format='%Y-%m_%d')),]
  ss.dat<-data.frame(
    ss.dat,
    met.date=as.Date(ss.dat$sunrise,tz=tz)
    )
  ss.dat<-melt(ss.dat,id.vars='met.date')
  if(!"POSIXct" %in% class(ss.dat$value))
    ss.dat$value<-as.POSIXct(ss.dat$value, origin='1970-01-01',tz=tz)
  ss.dat<-ss.dat[order(ss.dat$value),]
  ss.dat$day.hrs<-unlist(lapply(
    split(ss.dat,ss.dat$met.date),
    function(x) rep(as.numeric(x[2,'value']-x[1,'value']),2) 
    ))
  
  #matches is vector of row numbers indicating starting value that each
  #unique DateTimeStamp is within in ss.dat
  #output is meteorological day matches appended to dat.in
  matches<-findInterval(dat.in$DateTimeStamp,ss.dat$value)
  data.frame(dat.in,ss.dat[matches,])
      
  }

#calculates oxygen mass transfer coefficient, from Thiebault et al. 2008
#output from this can be used  to get volumetric rearation coefficient
#input is water temp, salinity, air temp, wind speed, barometric press, height of anemometer
f_calcKL<-function(Temp,Sal,ATemp,WSpd,BP,Height=10){

  require(oce) #for swSigmaT
  
  #celsius to kelvin conversion
  CtoK<-function(val) val+273.15 
  sig.fun<-Vectorize(swSigmaT)
  
  to.vect<-function(Temp,Sal,ATemp,WSpd,BP,Height=10){
    
    Patm<-BP*100; # convert from millibars to Pascals
    zo<-1e-5; # assumed surface roughness length (m) for smooth water surface
    U10<-WSpd*log(10/zo)/log(Height/zo)
    TempK<-CtoK(Temp)
    ATempK<-CtoK(ATemp)
    sigT<-sig.fun(Sal,Temp,10) # set for 10 decibars = 1000mbar = 1 bar = 1atm
    rho_w<-1000+sigT #density of SW (kg m-3)
    Upw<-1.002e-3*10^((1.1709*(20-Temp)-(1.827*10^-3*(Temp-20)^2))/(Temp+89.93)) #dynamic viscosity of pure water (Sal=0);
    Uw<-Upw*(1+(5.185e-5*Temp+1.0675e-4)*(rho_w*Sal/1806.55)^0.5+(3.3e-5*Temp+2.591e-3)*(rho_w*Sal/1806.55))  # dynamic viscosity of SW
    Vw<-Uw/rho_w  #kinematic viscosity
    Ew<-6.112*exp(17.65*ATemp/(243.12+ATemp))  # water vapor pressure (hectoPascals)
    Pv<-Ew*100 # Water vapor pressure in Pascals
    Rd<-287.05  # gas constant for dry air ( kg-1 K-1)
    Rv<-461.495  # gas constant for water vapor ( kg-1 K-1)
    rho_a<-(Patm-Pv)/(Rd*ATempK) +Pv/(Rv*TempK)
    kB<-1.3806503e-23 # Boltzman constant (m2 kg s-2 K-1)
    Ro<-1.72e-10     #radius of the O2 molecule (m)
    Dw<-kB*TempK/(4*pi*Uw*Ro)  #diffusivity of O2 in water 
    KL<-0.24*170.6*(Dw/Vw)^0.5*(rho_a/rho_w)^0.5*U10^1.81  #mass xfer coef (m d-1)
   
    return(KL)
    
    }
  
  out.fun<-Vectorize(to.vect)
  
  out.fun(Temp,Sal,ATemp,WSpd,BP,Height=10)
  
  }

######
# NEM function, uses all functions above
# estimates daily integrated rates, gross production, total respiraiton
# 'dat.in' input is station data frame 
# 'stat' is character string for station, five letters
# 'depth.val' is value for station depth if needed to add manually
# 'meta.path' is path of file with lat/long and GMT offset for station to evaluate, passed to 'met.day.fun'
# 'bott.stat' is logical indicating if station is below pycnocline, default to surface (T) accounts for air-sea exchange
nem.fun<-function(dat.in, stat, depth.val = NULL, 
  meta.path = NULL, bott.stat = F){
  
  ##dependent packages
  require(reshape) #data reshape
  require(wq) #for oxySol function
  require(oce) #for swSigma function

  ##begin calculations

  cat(stat,'\n')
  flush.console()
  strt<-Sys.time()
  
  #columns to be removed prior to processing
  to.rem<-c('flag')
  dat.in<-dat.in[,!names(dat.in) %in% to.rem]
  
  #convert DO from mg/L to mmol/m3
  dat.in$DO<-dat.in$DO_mgl/32*1000
  
  # get change in DO per hour, as mmol m^-3 hr^-1
  # scaled to time interval to equal hourly rates
  # otherwise, mmol m^-3 0.5hr^-1
  dDO_scl <- as.numeric(diff(dat.in$DateTimeStamp)/60)
  dDO<-diff(dat.in$DO)/dDO_scl
  
  #take diff of each column, divide by 2, add original value
  DateTimeStamp<-diff(dat.in$DateTimeStamp)/2 + dat.in$DateTimeStamp[-c(nrow(dat.in))]
  dat.in<-apply(
    dat.in[,2:ncol(dat.in)],
    2,
    function(x) diff(x)/2 + x[1:(length(x) -1)]
    )
  dat.in<-data.frame(DateTimeStamp,dat.in)
  DO <- dat.in$DO
  
  ##
  # replace missing wx values with climatological means
  # only ATemp, WSpd, and BP
  
  # monthly and hourly averages
  months <- format(dat.in$DateTimeStamp, '%m')
  hours <- format(dat.in$DateTimeStamp, '%H')
  clim_means <- ddply(data.frame(dat.in, months, hours),
    .variables=c('months', 'hours'),
    .fun = function(x){
      data.frame(
        ATemp = mean(x$ATemp, na.rm = T),
        WSpd = mean(x$WSpd, na.rm = T), 
        BP = mean(x$BP, na.rm = T)
      )   
    }
  )
  clim_means <- merge(
    data.frame(DateTimeStamp = dat.in$DateTimeStamp, months,hours),
    clim_means, by = c('months','hours'),
    all.x = T
  )
  clim_means <- clim_means[order(clim_means$DateTimeStamp),]

  # DateTimeStamp order in dat.in must be ascending to match
  if(is.unsorted(dat.in$DateTimeStamp))
    stop('DateTimeStamp is unsorted')
  
  # reassign empty values to means, objects are removed later
  ATemp_mix <- dat.in$ATemp
  WSpd_mix <- dat.in$WSpd
  BP_mix <- dat.in$BP
  ATemp_mix[is.na(ATemp_mix)] <- clim_means$ATemp[is.na(ATemp_mix)]
  WSpd_mix[is.na(WSpd_mix)] <- clim_means$WSpd[is.na(WSpd_mix)]
  BP_mix[is.na(BP_mix)] <- clim_means$BP[is.na(BP_mix)]

  ##
  # get sigma_t estimates
  SigT<-with(dat.in,swSigmaT(Sal,Temp,mean(dat.in$BP/100,na.rm=T)))
  
  #DOsat is DO at saturation given temp (C), salinity (st. unit), and press (atm)
  #DOsat converted to mmol/m3
  #used to get loss of O2 from diffusion
  DOsat<-with(dat.in,DO_mgl/(oxySol(Temp*(1000+SigT)/1000,Sal)))
  
  #station depth, defaults to mean depth value plus 0.5 in case not on bottom
  #uses 'depth.val' if provided
  if(is.null(depth.val))
    H<-rep(0.5+mean(pmax(1,dat.in$Depth),na.rm=T),nrow(dat.in))
  else H<-rep(depth.val,nrow(dat.in))
  
  #use met.day.fun to add columns indicating light/day, date, and hours of sunlight
  if(is.null(meta.path)) dat.in <- met.day.fun(dat.in, stat)
  else dat.in <- met.day.fun(dat.in, stat, meta.path)
  
  #get air sea gas-exchange using wx data with climate means
  KL<-with(dat.in,f_calcKL(Temp,Sal,ATemp_mix,WSpd_mix,BP_mix))
  rm(list = c('ATemp_mix', 'WSpd_mix', 'BP_mix'))
  
  #get volumetric reaeration coefficient from KL
  Ka<-KL/24/H
  
  #get exchange at air water interface
  D=Ka*(DO/DOsat-DO)
  
  #combine all data for processing
  proc.dat<-dat.in[,!names(dat.in) %in% c('DateTimeStamp','cDepth','Wdir',
    'SDWDir','ChlFluor','Turb','pH','RH','DO_mgl','DO_pct','SpCond','TotPrcp',
    'CumPrcp','TotSoRad','Depth')]
  proc.dat<-data.frame(proc.dat,DOsat,dDO,SigT,H,D)

  #get daily/nightly flux estimates for Pg, Rt, NEM estimates
  out<-lapply(
    split(proc.dat,proc.dat$met.date),
    function(x){
      
      #filter for minimum no. of records 
      if(length(with(x[x$variable=='sunrise',],na.omit(dDO))) < 3 |
         length(with(x[x$variable=='sunset',],na.omit(dDO))) < 3 ){
        DOF_d<-NA; D_d<-NA; DOF_n<-NA; D_n<-NA
        }
      
      else{
        #day
        DOF_d<-mean(with(x[x$variable=='sunrise',],dDO*H),na.rm=T)
        D_d<-mean(with(x[x$variable=='sunrise',],D),na.rm=T)
        
        #night
        DOF_n<-mean(with(x[x$variable=='sunset',],dDO*H),na.rm=T)
        D_n<-mean(with(x[x$variable=='sunset',],D),na.rm=T)
        }
      
      #metabolism
      #account for air-sea exchange if surface station
      #else do not
      if(!bott.stat){
        Pg<-((DOF_d-D_d) - (DOF_n-D_n))*unique(x$day.hrs)
        Rt<-(DOF_n-D_n)*24
      } else {
        Pg<-(DOF_d - DOF_n)*unique(x$day.hrs)
        Rt<-DOF_n*24
        }
      NEM<-Pg+Rt
      Pg_vol<-Pg/mean(x$H,na.rm=T)
      Rt_vol<-Rt/mean(x$H,na.rm=T)
      
      #dep vars to take mean
      var.out<-x[!names(x) %in% c('variable','value','met.date',
        'day.hrs')] 
      var.out<-data.frame(rbind(apply(var.out,2,function(x) mean(x,na.rm=T))))
      data.frame(Station=stat,Date=unique(x$met.date),var.out,DOF_d,D_d,DOF_n,D_n,Pg,Rt,NEM,
        Pg_vol,Rt_vol,numrecs=length(na.omit(x$dDO)))
      
      }
    )
  out<-do.call('rbind',out)
  
  return(out)
  
  }

######
# instantaneous DO flux function, uses all functions above
# output is hourly net production (GPP - R)
# 'dat.in' is station data frame 
# 'stat' is character string for station
# 'depth.val' is optional numeric vector for station depth
# 'meta.path' is path for SWMP metadata file
inst.flux.fun <- function(dat.in, stat, depth.val = NULL, 
  meta.path = NULL){
  
  ##dependent packages
  require(reshape) #data reshape
  require(wq) #for oxySol function
  require(oce) #for swSigma function

  ##begin calculations
  
  cat(stat,'\n')
  flush.console()
  strt<-Sys.time()
  
  # columns to be removed prior to processing
  # 'flag' retained for inst.flux calcs
  flag <- dat.in$flag[-1] # first is NA since based on diff
  to.rem <- c('flag')
  dat.in <- dat.in[, !names(dat.in) %in% to.rem]
  
  #convert DO from mg/L to mmol/m3
  dat.in$DO<-dat.in$DO_mgl/32*1000
  
  # get change in DO per hour, as mmol m^-3 hr^-1
  # scaled to time interval to equal hourly rates
  # otherwise, mmol m^-3 0.5hr^-1
  dDO_scl <- as.numeric(diff(dat.in$DateTimeStamp)/60)
  dDO<-diff(dat.in$DO)/dDO_scl
  
  #take diff of each column, divide by 2, add original value
  DateTimeStamp<-diff(dat.in$DateTimeStamp)/2 + dat.in$DateTimeStamp[-c(nrow(dat.in))]
  dat.in<-apply(
    dat.in[,2:ncol(dat.in)],
    2,
    function(x) diff(x)/2 + x[1:(length(x) -1)]
    )
  dat.in<-data.frame(DateTimeStamp,dat.in)
  DO <- dat.in$DO
  
  ##
  # replace missing wx values with climatological means
  # only ATemp, WSpd, and BP
  
  # monthly and hourly averages
  months <- format(dat.in$DateTimeStamp, '%m')
  hours <- format(dat.in$DateTimeStamp, '%H')
  clim_means <- ddply(data.frame(dat.in, months, hours),
    .variables=c('months', 'hours'),
    .fun = function(x){
      data.frame(
        ATemp = mean(x$ATemp, na.rm = T),
        WSpd = mean(x$WSpd, na.rm = T), 
        BP = mean(x$BP, na.rm = T)
      )   
    }
  )
  clim_means <- merge(
    data.frame(DateTimeStamp = dat.in$DateTimeStamp, months,hours),
    clim_means, by = c('months','hours'),
    all.x = T
  )
  clim_means <- clim_means[order(clim_means$DateTimeStamp),]

  # DateTimeStamp order in dat.in must be ascending to match
  if(is.unsorted(dat.in$DateTimeStamp))
    stop('DateTimeStamp is unsorted')
  
  # reassign empty values to means, objects are removed later
  ATemp_mix <- dat.in$ATemp
  WSpd_mix <- dat.in$WSpd
  BP_mix <- dat.in$BP
  ATemp_mix[is.na(ATemp_mix)] <- clim_means$ATemp[is.na(ATemp_mix)]
  WSpd_mix[is.na(WSpd_mix)] <- clim_means$WSpd[is.na(WSpd_mix)]
  BP_mix[is.na(BP_mix)] <- clim_means$BP[is.na(BP_mix)]

  ##
  # get sigma_t estimates
  SigT<-with(dat.in,swSigmaT(Sal,Temp,mean(dat.in$BP/100,na.rm=T)))
  
  #DOsat is DO at saturation given temp (C), salinity (st. unit), and press (atm)
  #DOsat converted to mmol/m3
  #used to get loss of O2 from diffusion
  DOsat<-with(dat.in,DO_mgl/(oxySol(Temp*(1000+SigT)/1000,Sal)))
  
  #station depth, defaults to mean depth value plus 0.5 in case not on bottom
  #uses 'depth.val' if provided
  if(is.null(depth.val))
    H<-rep(0.5+mean(pmax(1,dat.in$Depth),na.rm=T),nrow(dat.in))
  else H<-rep(depth.val,nrow(dat.in))
  
  #use met.day.fun to add columns indicating light/day, date, and hours of sunlight
  if(is.null(meta.path)) dat.in <- met.day.fun(dat.in, stat)
  else dat.in <- met.day.fun(dat.in, stat, meta.path)
  
  #get air sea gas-exchange using wx data with climate means
  KL<-with(dat.in,f_calcKL(Temp,Sal,ATemp_mix,WSpd_mix,BP_mix))
  rm(list = c('ATemp_mix', 'WSpd_mix', 'BP_mix'))
  
  #get volumetric reaeration coefficient from KL
  Ka<-KL/24/H
  
  #get exchange at air water interface
  D=Ka*(DO/DOsat-DO)  
  
  #combine all data for processing
  proc.dat<-dat.in[,!names(dat.in) %in% c('cDepth','Wdir',
    'SDWDir','ChlFluor','Turb','pH','RH','SpCond','TotPrcp',
    'CumPrcp','TotSoRad','PO4H','NH4F','NO2F','NO3F','NO23F','CHLA_N')]
  proc.dat<-data.frame(proc.dat,DOsat,dDO,SigT,H,D,flag)
  
  # change name for sunrise/sunset variable to solar
  names(proc.dat)[names(proc.dat) %in% 'variable'] <- 'solar'
    
  # get net instantaneous flux, 
  # not corrected for air/sea exchange
  # units are mmol m^-2 hr^-1 (this is why multiplied by H)
  proc.dat$DOF<-with(proc.dat,dDO*H)
  
  return(proc.dat)
  
  }

######
# NEM function w/ noise correction, uses all functions above
# estimates daily integrated rates, gross production, total respiraiton
# 'dat.in' input is station data frame 
# 'stat' is character string for station, five letters
# 'depth.val' is value for station depth if needed to add manually
# 'meta.path' is path of file with lat/long and GMT offset for station to evaluate, passed to 'met.day.fun'
# 'bott.stat' is logical indicating if station is below pycnocline, default to surface (T) accounts for air-sea exchange
nem.unbi.fun<-function(dat.in, stat, depth.val = NULL, 
  meta.path = NULL, bott.stat = F){
  
  ##dependent packages
  require(reshape) #data reshape
  require(wq) #for oxySol function
  require(oce) #for swSigma function

  ##begin calculations

  cat(stat,'\n')
  flush.console()
  strt<-Sys.time()
  
  #columns to be removed prior to processing
  flag <- dat.in$flag[-1] 
  to.rem<-c('flag')
  dat.in<-dat.in[,!names(dat.in) %in% to.rem]
  
  #convert DO from mg/L to mmol/m3
  dat.in$DO<-dat.in$DO_mgl/32*1000
  
  # get change in DO per hour, as mmol m^-3 hr^-1
  # scaled to time interval to equal hourly rates
  # otherwise, mmol m^-3 0.5hr^-1
  dDO_scl <- as.numeric(diff(dat.in$DateTimeStamp)/60)
  dDO<-diff(dat.in$DO)/dDO_scl
  
  #take diff of each column, divide by 2, add original value
  DateTimeStamp<-diff(dat.in$DateTimeStamp)/2 + dat.in$DateTimeStamp[-c(nrow(dat.in))]
  dat.in<-apply(
    dat.in[,2:ncol(dat.in)],
    2,
    function(x) diff(x)/2 + x[1:(length(x) -1)]
    )
  dat.in<-data.frame(DateTimeStamp,dat.in)
  DO <- dat.in$DO
  
  ##
  # replace missing wx values with climatological means
  # only ATemp, WSpd, and BP
  
  # monthly and hourly averages
  months <- format(dat.in$DateTimeStamp, '%m')
  hours <- format(dat.in$DateTimeStamp, '%H')
  clim_means <- ddply(data.frame(dat.in, months, hours),
    .variables=c('months', 'hours'),
    .fun = function(x){
      data.frame(
        ATemp = mean(x$ATemp, na.rm = T),
        WSpd = mean(x$WSpd, na.rm = T), 
        BP = mean(x$BP, na.rm = T)
      )   
    }
  )
  clim_means <- merge(
    data.frame(DateTimeStamp = dat.in$DateTimeStamp, months,hours),
    clim_means, by = c('months','hours'),
    all.x = T
  )
  clim_means <- clim_means[order(clim_means$DateTimeStamp),]

  # DateTimeStamp order in dat.in must be ascending to match
  if(is.unsorted(dat.in$DateTimeStamp))
    stop('DateTimeStamp is unsorted')
  
  # reassign empty values to means, objects are removed later
  ATemp_mix <- dat.in$ATemp
  WSpd_mix <- dat.in$WSpd
  BP_mix <- dat.in$BP
  ATemp_mix[is.na(ATemp_mix)] <- clim_means$ATemp[is.na(ATemp_mix)]
  WSpd_mix[is.na(WSpd_mix)] <- clim_means$WSpd[is.na(WSpd_mix)]
  BP_mix[is.na(BP_mix)] <- clim_means$BP[is.na(BP_mix)]

  ##
  # get sigma_t estimates
  SigT<-with(dat.in,swSigmaT(Sal,Temp,mean(dat.in$BP/100,na.rm=T)))
  
  #DOsat is DO at saturation given temp (C), salinity (st. unit), and press (atm)
  #DOsat converted to mmol/m3
  #used to get loss of O2 from diffusion
  DOsat<-with(dat.in,DO_mgl/(oxySol(Temp*(1000+SigT)/1000,Sal)))
  
  #station depth, defaults to mean depth value plus 0.5 in case not on bottom
  #uses 'depth.val' if provided
  if(is.null(depth.val))
    H<-rep(0.5+mean(pmax(1,dat.in$Depth),na.rm=T),nrow(dat.in))
  else H<-rep(depth.val,nrow(dat.in))
  
  #use met.day.fun to add columns indicating light/day, date, and hours of sunlight
  if(is.null(meta.path)) dat.in <- met.day.fun(dat.in, stat)
  else dat.in <- met.day.fun(dat.in, stat, meta.path)
  
  #get air sea gas-exchange using wx data with climate means
  KL<-with(dat.in,f_calcKL(Temp,Sal,ATemp_mix,WSpd_mix,BP_mix))
  rm(list = c('ATemp_mix', 'WSpd_mix', 'BP_mix'))
  
  #get volumetric reaeration coefficient from KL
  Ka<-KL/24/H
  
  #get exchange at air water interface
  D=Ka*(DO/DOsat-DO)
  
#  ###
#   # apply correction to dDO by seas/fort cats
#   
#   # change names 'variable' to 'solar' 
#   names(dat.in)[names(dat.in) %in% 'variable'] <- 'solar'
#   
#   # create fort and seas columns for correction
#   dat.in <- cats_fun2(dat.in, stat)
#   
#   # add syn/ant flag column saved from above
#   dat.in$flag  <- flag
#   
#   # DO flux must be in areal (m2) rates, i.e., 
#   # correction vals are based on area rates
#   dat.in$DOF <- dDO*H
#   
#   # add H for reconversion back to volumetric (w/in function)
#   dat.in$H <- H
#   
#   # apply correction with 'unbi_fun'
#   dat.in <- unbi_fun(dat.in, stat)
#   
#   # reconvert DOF back to volumetric rates for consistency with metab calcs
#   # this is redundant with below script but done for posterity
#   dat.in$dDO <- dat.in$DOF_unbi/H
#   
#   # change 'solar' back to 'variable'
#   names(dat.in)[names(dat.in) %in% 'solar'] <- 'variable'
  ###
  # apply correction to dDO via moving regression corrs
  
  # add syn/ant flag column saved from above
  dat.in$flag  <- flag
  
  # DO flux must be in areal (m2) rates, i.e., 
  # correction vals are based on area rates
  dat.in$DOF <- dDO*H
  
  # add H for reconversion back to volumetric (w/in function)
  dat.in$H <- H
  
  # apply correction with 'unbi_fun'
  dat.in <- unbi_mw_fun(dat.in, 
    path_in = 'C:/Users/mbeck/Desktop/NOCZB_win.RData')
 
  ###
  
  #combine all data for processing
  proc.dat<-dat.in[,!names(dat.in) %in% c('DateTimeStamp','cDepth','Wdir',
    'SDWDir','ChlFluor','Turb','pH','RH','DO_mgl','DO_pct','SpCond','TotPrcp',
    'CumPrcp','TotSoRad','Depth','flag','seas','fort','DOF_unbi','value')]
  proc.dat<-data.frame(proc.dat,DOsat,SigT,D)
  
  # melt by window if windows present
  # reassign names to melted variables
  if(any(grepl('win', names(proc.dat)))){
    
    # change variable name to prevent dups
    names(proc.dat)[grep('variable', names(proc.dat))] <- c('solar')
    
    # melt
    proc.dat <- melt(proc.dat, measure.var = grep('win',names(proc.dat)))
    
    # reassign var name
    names(proc.dat)[grep('variable|value', names(proc.dat))] <- c('win', 'dDO')
  
    # reassign var for solar back to varialbe
    names(proc.dat)[grep('solar', names(proc.dat))] <- 'variable'
  
    # split for processing
    proc.dat <- split(proc.dat, proc.dat$win)
    
  }
  
  # if not using windows, convert to list
  if(class(proc.dat) == 'data.frame') proc.dat <- list(proc.dat)

  #get daily/nightly flux estimates for Pg, Rt, NEM estimates
  out<-lapply(
    
    proc.dat,
    function(by_win){
      
      lapply(
        split(by_win,by_win$met.date),
        
        function(x){
          
          #filter for minimum no. of records 
          if(length(with(x[x$variable=='sunrise',],na.omit(dDO))) < 3 |
             length(with(x[x$variable=='sunset',],na.omit(dDO))) < 3 ){
            DOF_d<-NA; D_d<-NA; DOF_n<-NA; D_n<-NA
            }
          
          else{
            #day
            DOF_d<-mean(with(x[x$variable=='sunrise',],dDO*H),na.rm=T)
            D_d<- mean(with(x[x$variable=='sunrise',],D),na.rm=T)
            
            #night
            DOF_n<-mean(with(x[x$variable=='sunset',],dDO*H),na.rm=T)
            D_n<-mean(with(x[x$variable=='sunset',],D),na.rm=T)
            }
          
          #metabolism
          #account for air-sea exchange if surface station
          #else do not
          if(!bott.stat){
            Pg<-(DOF_d-D_d) - (DOF_n-D_n)*unique(x$day.hrs)
            Rt<-(DOF_n-D_n)*24
          } else {
            Pg<-DOF_d - DOF_n*unique(x$day.hrs)
            Rt<-DOF_n*24
            }
          NEM<-Pg+Rt
          Pg_vol<-Pg/mean(x$H,na.rm=T)
          Rt_vol<-Rt/mean(x$H,na.rm=T)
          
          #dep vars to take mean
          var.out<-x[!names(x) %in% c('variable','value','met.date',
            'day.hrs', 'win')] 
          var.out<-data.frame(rbind(apply(var.out,2,function(x) mean(x,na.rm=T))))
          data.frame(Station=stat,Date=unique(x$met.date),var.out,DOF_d,D_d,DOF_n,D_n,Pg,Rt,NEM,
            Pg_vol,Rt_vol,numrecs=length(na.omit(x$dDO)))
          
          }
        )

      }
    )
    
  out <- lapply(out, function(x) do.call('rbind', x))
  
  return(out)
  
  }

######
# corrects bias/noise in instantaneous flux estimates
# used with seasonal/fort correction factors
# can work with data from /inst_flux/ files, also used in 'nem_unbi_fun'
# 'dat_in' input is data frame, required inst DOF (areal rates), flag, solar, spring, fort
# 'stat_in' is text string of station name
# required data is correction data frame calle 'dat_unbi'
# 'dat_unbi' is created in 'swmp_anoms.r'
# 'extra_cols' is logical for extra output columsn - orig DOF and correction value
unbi_fun <- function(dat_in, stat_in, extra_cols = F){

  if(!exists('dat_unbi'))
    load(file = 'M:/wq_models/SWMP/raw/rproc/dat_unbi.RData')
  stat_unbi <- dat_unbi[dat_unbi$site == stat_in,]
  
  # reassign factor levels in unbias data to input data frame
  levels(stat_unbi$solar) <- c('sunrise', 'sunset')
  levels(stat_unbi$flag) <- c('-1','1')
  stat_unbi$flag <- as.numeric(as.character(stat_unbi$flag))
  
  # merge data
  out <- merge(dat_in, stat_unbi, 
    by = c('solar', 'flag', 'fort', 'seas'),
    all.x = T)
  
  # apply bias correction, make sure sign is correct
  out$DOF_unbi <- out$DOF - out$unbi
  
  # subsitute unbiased null values with actual DOF
  out$DOF_unbi[grep('0',out$flag)] <- out$DOF[grep('0',out$flag)] 
  
  # reorder by DateTimeStamp
  out <- out[order(out$DateTimeStamp),]
  
  # remove extra columns
  if(extra_cols) return(out)
  
  out <- out[, !names(out) %in% c('flag', 'fort', 
    'seas', 'site', 'unbi', 'DOF')]
  
  return(out)
}

######
# corrects bias/noise in instantaneous flux estimates
# used with moving window correction factors
# can work with data from /inst_flux/ files, also used in 'nem_unbi_fun'
# 'dat_in' input is data frame, required inst DOF (areal rates), flag, solar, spring, fort
# 'path_in' is full path of RData with correction factors as list for each window
# 'extra_cols' is logical for extra output columsn - orig DOF and correction value
unbi_mw_fun <- function(dat_in, path_in, extra_cols = F){

  require(reshape2)
  
  # will have to change path to make generic
  # load correction data from path
  stat_in <- gsub('.RData', '', basename(path_in))
  if(!exists(stat_in)) load(file = path_in)
  
  # merge list, then pivot
  stat_in <- get(stat_in)
  stat_in <- melt(stat_in, id.var = c('DateTimeStamp', 'corr'))
  names(stat_in)[names(stat_in) %in% 'L1'] <- 'window'
  stat_in <- dcast(stat_in, DateTimeStamp ~ window, value.var = 'corr')
    
  # convert flag to numeric if not
  if(class(dat_in$flag) != 'numeric'){
    levels(dat_in$flag) <- c('-1','1')
    dat_in$flag <- as.numeric(as.character(dat_in$flag))
  }
  
  # merge wq data with correction data
  out <- merge(dat_in[, c('DateTimeStamp', 'flag', 'DOF')], stat_in, 
    by = 'DateTimeStamp',
    all.x = T)
  out <- melt(out, id.var = c('DateTimeStamp', 'flag', 'DOF'))
  
  # apply bias correction, make sure sign is correct
  out$DOF_unbi <- with(out, DOF - (value*flag))
  
  # reconvert DOF back to volumetric rates 
  out$DOF_unbi <- with(out, DOF_unbi/H)

  # convert back to short table
  out <- dcast(out, DateTimeStamp ~ variable, value.var = 'DOF_unbi')
  
  # merge w/ wq data
  out <- merge(dat_in, out, by = 'DateTimeStamp')
  
  # reorder by DateTimeStamp as a precaution
  out <- out[order(out$DateTimeStamp),]
  
  # return full output, otherwise remove extra cols
  if(extra_cols) return(out)
  
  out <- out[, !names(out) %in% c('flag', 'DOF')]
  
  return(out)
  
}

######
# function for getting spring/neap and seasonal vector
# default for season end points are halfway between solstice and equinox
# this related to periodicity of tidal variation on annual scale
# 'dat_in' is data frame created within NEM function for a single station
# this version gets tidal predictions from existing data, see 'swmp_anoms.r'
cats_fun2 <- function(dat_in, stat_in){

  require(oce)
  require(plyr)
  
  ##
  # id spring/neap obs
  
  # get predicted tidal comps from existing data
  tide_path <- 'M:/wq_models/SWMP/raw/rproc/tide_preds/'
  load(file = paste0(tide_path, '/', stat_in, '.RData'))
  mod.all <- get(stat_in)
#   mod.all <- data.frame(DateTimeStamp = dat_in[,'DateTimeStamp'], 
#     Tide = mod.all)

  # daily range from mod.all, then smoothed
  mod.rng <- mod.all
  mod.rng$Date <- as.Date(mod.rng$DateTimeStamp)
  mod.rng <- ddply(mod.rng,
    .variables = c('Date'),
    .fun = function(x) diff(range(x$Tide))
    )
  names(mod.rng)[names(mod.rng) %in% 'V1'] <- 'Tide'
  mod.rng$Tide <- filter(mod.rng$Tide, filter = rep(1,4)/4)
  
  # get second deriv of tide based on daily tidal range
  # this is an expression of concavity
  # roughly approximat to spring/neap cycles
  mod.dff <- mod.rng
  mod.dff$tide.secder<- c(NA,diff(c(NA,diff(mod.dff$Tide))))
  mod.dff$fort <- 'Neap'
  mod.dff$fort[mod.dff$tide.secder < 0]  <- 'Spring'
  mod.dff$fort[is.na(mod.dff$tide.secder)] <- NA
  mod.dff$fort <- factor(mod.dff$fort)
  
  # date column for dat_in for merging, to be removed
  dat_in$Date <- as.Date(dat_in$DateTimeStamp, 
    tz = attr(dat_in$DateTimeStamp, 'tzone'))

  # merge
  dat_in<- merge(dat_in, mod.dff[,c('Date', 'fort')],
    by = 'Date', all.x = T)
  
#   ##
#   # create seasonal categories around equinox/solstic
#    
#   # create julian day vector
#   dat_in$seas <- as.numeric(as.character(
#     format(dat_in$Date, '%j')))
#   
#   # cut julian vector by seasons
#   # Feb. 5th 36, May 5th 126, Aug 5th 218, Nov. 5th 310
#   dat_in$seas <- cut(dat_in$seas, 
#     c(-Inf, 36, 126, 218, 310, Inf),
#     c('W', 'Sp', 'Su', 'Fa', 'W2'))
#   levels(dat_in$seas) <- c('W', 'Sp', 'Su', 'Fa', 'W')
  
  ##
  # create monthly cats
  dat_in$seas <- factor(as.character(
    format(dat_in$Date, '%m')),
    levels = c('01', '02', '03', '04', '05', '06', '07', '08',
      '09', '10', '11', '12'),
    labels = c('Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 
      'Sep', 'Oct', 'Nov', 'Dec')
  )
  
  ##
  # remove Date from dat.wq
  dat_out <- dat_in[,!names(dat_in) %in% 'Date']
  
  return(dat_out)
  
}

######
# function for getting spring/neap and seasonal vector
# default for season end points are halfway between solstice and equinox
# this related to periodicity of tidal variation on annual scale
# 'dat_in' is data frame created within NEM function for a single station
# this version predicts tide for calculations
cats_fun <- function(dat_in){

  require(oce)
  require(plyr)

  ##
  # id spring/neap obs
  
  # tidal components to est, used for tidal model
  tide.comp <- read.csv('M:/wq_models/SWMP/tide_comp.csv',
    header = T)
  tide.comp <- tide.comp$comp
 
  #tidal mod and predictions using all comps
  mod.all <- tidem(dat_in[,'Depth'], dat_in[,'DateTimeStamp'], 
    constituents = tide.comp)
  mod.all <- predict(mod.all, newdata = dat_in[,'DateTimeStamp'])
  mod.all <- data.frame(DateTimeStamp = dat_in[,'DateTimeStamp'], 
    Tide = mod.all)
  
  # daily range from mod.all, then smoothed
  mod.rng <- mod.all
  mod.rng$Date <- as.Date(mod.rng$DateTimeStamp)
  mod.rng <- ddply(mod.rng,
    .variables = c('Date'),
    .fun = function(x) diff(range(x$Tide))
    )
  names(mod.rng)[names(mod.rng) %in% 'V1'] <- 'Tide'
  mod.rng$Tide <- filter(mod.rng$Tide, filter = rep(1,4)/4)
  
  # get second deriv of tide based on daily tidal range
  # this is an expression of concavity
  # roughly approximat to spring/neap cycles
  mod.dff <- mod.rng
  mod.dff$tide.der<- c(NA,diff(c(NA,diff(mod.dff$Tide))))
  mod.dff$fort <- 'Neap'
  mod.dff$fort[mod.dff$tide.der < 0]  <- 'Spring'
  mod.dff$fort[is.na(mod.dff$tide.der)] <- NA
  mod.dff$fort <- factor(mod.dff$fort)
  
  # date column for dat_in for merging, to be removed
  dat_in$Date <- as.Date(dat_in$DateTimeStamp, 
    tz = attr(dat_in$DateTimeStamp, 'tzone'))

  # merge
  dat_in<- merge(dat_in, mod.dff[,c('Date', 'fort')],
    by = 'Date', all.x = T)
  
#   ##
#   # create seasonal categories around equinox/solstic
#    
#   # create julian day vector
#   dat_in$seas <- as.numeric(as.character(
#     format(dat_in$Date, '%j')))
#   
#   # cut julian vector by seasons
#   # Feb. 5th 36, May 5th 126, Aug 5th 218, Nov. 5th 310
#   dat_in$seas <- cut(dat_in$seas, 
#     c(-Inf, 36, 126, 218, 310, Inf),
#     c('W', 'Sp', 'Su', 'Fa', 'W2'))
#   levels(dat_in$seas) <- c('W', 'Sp', 'Su', 'Fa', 'W')
  
  ##
  # create monthly cats
  dat_in$seas <- factor(as.character(
    format(dat_in$Date, '%m')),
    levels = c('01', '02', '03', '04', '05', '06', '07', '08',
      '09', '10', '11', '12'),
    labels = c('Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 
      'Sep', 'Oct', 'Nov', 'Dec')
  )
  
  ##
  # remove Date from dat.wq
  dat_out <- dat_in[,!names(dat_in) %in% 'Date']
   
  return(dat_out)
  
}

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
  tide <- scl.up + rescale(scale(rowSums(tide)))
  
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
#gets first derivative of predicted values of tidal model
#'mod.in' is model of class 'tidem' from tidem function
#'constituents' is tidal component for estimating derivative
#note this is currently setup for obs at 30 minute intervals - continuous and no breaks
tidem.deriv.fun<-function(mod.in,constituents){
  
  if(length(constituents)>1) stop('Only one tidal component allowed')
  
  atts<-attr(mod.in,'data')
  ind<-which(atts$name==constituents)
    
  a.val<-atts[['amplitude']][ind]  
  f.val<-1/atts[['freq']][ind]/24
  
  int.val<-atts$model$coefficients['(Intercept)']

  time.in<-eval(atts$call$t)
  tot.days<-as.numeric(difftime(time.in[length(time.in)],time.in[1],units='days',
    tz=attr(time.in,'tzone')))
  in.vals<-seq(0,tot.days*2*pi*(1/f.val),length=length(time.in))

  out.nop<-int.val+a.val*sin(in.vals)

  #phase shift determined from lag with max positive corr
  #couldn't figure out phase value from 'tidem' object....
  p.shift<-ccf(predict(tide.mod),out.nop,plot=F,na.action=na.pass,lag=600)
  p.shift<-p.shift$lag[which.max(p.shift$acf)]
  p.shift<-(1/f.val)*2*pi*p.shift*30/60/24
  
  out<-a.val*cos(in.vals - p.shift)
  
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
    plo.dates<-dcast(plo.dates,inds~solar,value.var='value')
    plo.dates<-with(plo.dates,
      data.frame(sunrise,sunset,sunset,sunrise)
      )
   
    x.vals<-suppressMessages(melt(sapply(1:nrow(plo.dates), 
      function(x) plo.dates[x,],simplify=F))$value)
    x.vals<-as.POSIXct(x.vals,tz,'1970-01-01')
    y.vals<-rep(c(-1000,-1000,1000,1000),nrow(plo.dates))
    Day<-as.character(trunc(x.vals,'days'))
    polys<-data.frame(x.vals,y.vals,grp=Day)

    }
  
  out<-geom_polygon(data=polys,aes(x.vals,y.vals,group=grp,fill='grp'),alpha=0.6)
  
  return(out)
  
  }

##
# create dec time using jday on 24 hour scale
# 'dat_in' is data frame input with time vecot as 'var_nm'
# output is same data frame including 'dec_time' column
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

######
#function for getting regression weights
#'wt_vars' is name of three variables to weight
#'ref_in' is row of dat.in that is used as reference
#'dat_in' is data to get weights from
#'wins' are the windows for the three wt.vars, values represent halves
#'all' will return all weights, rather than the product of all three
wt_fun<-function(ref_in, dat_in,
  wt_vars = c('jday', 'hour', 'dTide'),
  wins = list(2, 0.5, 0.2),
  all = F){
  
#   # for subroutine
#   dat_in <- DO_sim
#   ref_in <- dat_in[1,]
#   wt_vars = c('jday', 'hour', 'dTide')
#   wins = list(5, 0.23, 2.651631) #list(15, 0.5, NULL)
  
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
    if(mirr)
      dist_val <- pmin(
        dist_val, 
        sapply(ref, function(x) abs(dat_cal - x + scl_val)),
        sapply(ref, function(x) abs(dat_cal - scl_val + x))
        )
    
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
  # weights for each observation in relation to reference
  # see comments for 'wt_fun_sub' for 'scl_val' argument
  
  # jday
  wts_1 <- wt_fun_sub(as.numeric(dat_in[, wt_vars[1]]), 
    ref = ref_1, win = wins_1, mirr = T, scl_val = 365) 
  # hour
  wts_2 <- wt_fun_sub(as.numeric(dat_in[, wt_vars[2]]), 
    ref = ref_2, win = wins_2, mirr = T, scl_val = 1)
  # tide
  wts_3 <- wt_fun_sub(as.numeric(dat_in[, wt_vars[3]]), 
    ref = ref_3, win = wins_3)
  # all as product 
  out <- sapply(1:nrow(ref_in), function(x) wts_1[, x] * wts_2[, x] * wts_3[, x])
  
  gr_zero <- colSums(out > 0)
  #cat('   Number of weights greater than zero =',gr.zero,'\n')
  
  # extend window widths of weight vector is less than 100
  while(any(gr_zero < 100)){
    
    wins_1 <- 1.1 * wins_1
    wins_2 <- 1.1 * wins_2
    wins_3 <- 1.1 * wins_3 
    
    #weights for each observation in relation to reference
    wts_1 <- wt_fun_sub(as.numeric(dat_in[, wt_vars[1]]), 
      ref = ref_1, win = wins_1, mirr = T, scl_val = 365)
    wts_2 <- wt_fun_sub(as.numeric(dat_in[, wt_vars[2]]), 
      ref = ref_2, win = wins_2, mirr = T, scl_val = 1)
    wts_3 <- wt_fun_sub(as.numeric(dat_in[, wt_vars[3]]), 
      ref = ref_3, win = wins_3)
    
    out <- sapply(1:nrow(ref_in), 
      function(x) wts_1[, x] * wts_2[, x] * wts_3[, x])
    
    gr_zero <- colSums(out > 0)
    
    }
  
  #return all weights if T
  if(all){
    out <- list(wts_1, wts_2, wts_3)
    names(out) <- wt_vars
    return(out)
    }
  
  #final weights are product of all three
  out
  
  }

######
# this is a repliate of filled.contour that removes category borders in the legend
# http://stackoverflow.com/questions/8068366/removing-lines-within-filled-contour-legend
filled.contour.hack <- function (x = seq(0, 1, length.out = nrow(z)), y = seq(0, 1, 
    length.out = ncol(z)), z, xlim = range(x, finite = TRUE), 
    ylim = range(y, finite = TRUE), zlim = range(z, finite = TRUE), 
    levels = pretty(zlim, nlevels), nlevels = 20, color.palette = cm.colors, 
    col = color.palette(length(levels) - 1), plot.title, plot.axes, 
    key.title, key.axes, asp = NA, xaxs = "i", yaxs = "i", las = 1, 
    axes = TRUE, frame.plot = axes, ...) 
{
    if (missing(z)) {
        if (!missing(x)) {
            if (is.list(x)) {
                z <- x$z
                y <- x$y
                x <- x$x
            }
            else {
                z <- x
                x <- seq.int(0, 1, length.out = nrow(z))
            }
        }
        else stop("no 'z' matrix specified")
    }
    else if (is.list(x)) {
        y <- x$y
        x <- x$x
    }
    if (any(diff(x) <= 0) || any(diff(y) <= 0)) 
        stop("increasing 'x' and 'y' values expected")
    mar.orig <- (par.orig <- par(c("mar", "las", "mfrow")))$mar
    on.exit(par(par.orig))
    w <- (3 + mar.orig[2L]) * par("csi") * 2.54
    layout(matrix(c(2, 1), ncol = 2L), widths = c(1, lcm(w)))
    par(las = las)
    mar <- mar.orig
    mar[4L] <- mar[2L]
    mar[2L] <- 1
    par(mar = mar)
    plot.new()
    plot.window(xlim = c(0, 1), ylim = range(levels), xaxs = "i", 
        yaxs = "i")
    rect(0, levels[-length(levels)], 1, levels[-1L], col = col, border = NA)
    if (missing(key.axes)) {
        if (axes) 
            axis(4)
    }
    else key.axes
    box()
    if (!missing(key.title)) 
        key.title
    mar <- mar.orig
    mar[4L] <- 1
    par(mar = mar)
    plot.new()
    plot.window(xlim, ylim, "", xaxs = xaxs, yaxs = yaxs, asp = asp)
    .filled.contour(x, y, z, levels, col)
    if (missing(plot.axes)) {
        if (axes) {
            title(main = "", xlab = "", ylab = "")
            Axis(x, side = 1)
            Axis(y, side = 2)
        }
    }
    else plot.axes
    if (frame.plot) 
        box()
    if (missing(plot.title)) 
        title(...)
    else plot.title
    invisible()
  }

######
# interpolation grid for weighted regression
# 'dat_in' is data.frame to interpolate, must contain dTide, dec_time, DO_obs, DateTimeStamp
# 'dtide_div' is number of values to interp for grid
# 'wins' is list of values for windows to determine weights
interp_grd <- function(dat_in, dtide_div = 10,
  wins = list(2, 0.5, 0.2), parallel = F){
  
  require(plyr)
  
  # assign to local env for ddply
  dtide_div <- dtide_div
  
  # setup range of tidal vals to predict for grid
  dtide.grid<-seq(min(dat_in$dTide), max(dat_in$dTide), length = dtide_div)

  interp_out <- vector('list', length = nrow(dat_in))

  out <- ddply(dat_in, 
    .variable = 'DateTimeStamp',
    .parallel = parallel,
#     .paropts = list(.export = c('dtide_div', 'dtide.grid', 'dat_in', 
#         'wins')),
    .fun = function(row){
    
      # row for prediction
      ref_in <- row
      ref_in <- ref_in[rep(1, dtide_div),]
      ref_in$dTide <- dtide.grid
      
      # get wts
      ref_wts <- wt_fun(ref_in, dat_in, wins = wins)
  
      #OLS wtd model
      out <- sapply(1:ncol(ref_wts),
        function(x){
          
          # subset data for weights > 0
          grzero <- ref_wts[, x] > 0 
          dat_in <- dat_in[grzero, ]
          
          # subset weigths > 0, rescale weights average
          ref_wts <- ref_wts[grzero, x]
          ref_wts <- ref_wts/mean(ref_wts)
          
          # get model
          mod_md <- lm(
            DO_obs ~ dec_time + dTide + sin(2*pi*dec_time) + cos(2*pi*dec_time),
            weights = ref_wts,
            data = dat_in
            )
          
          # get prediction from model
          dTide <- ref_in$dTide[x]
          DO_pred <- predict(
            mod_md, 
            newdata = data.frame(dec_time = ref_in$dec_time[x], dTide = dTide)
            )
          
          # get beta from model
          beta <- mod_md$coefficients['dTide']
          
          # output
          data.frame(DO_pred, beta, dTide)
          
          }
        
        )
      
      out <- t(out)
      out <- data.frame(out) #, row = row)
      
      out
    
      })
  
  # convert list vectors to numeic
  out$DO_pred <- as.numeric(out$DO_pred)
  out$beta <- as.numeric(out$beta)
  out$dTide <- as.numeric(out$dTide)
  
  return(out)
  
  }

######
# creates data for DO simulation, uses functions above
# 'time_in' is POSIX vector of continuous time series to estimate, currently as 30 minute obs
# 'do.amp' is amplitude of biological DO signal
# 'tide_cat' is either a list of tidal components, 
# a character string as 'Diurnal', 'Semidiurnal', or 'Mixed Semidiurnal', 
# or a data.frame of tidal values
# 'tide_assoc' is value +- range of DO_adv when converting dTide (i.e., amp)
# 'err_rng_obs' is sd of obs uncertainty for DO signal
# 'err_rng_pro' is bounded range +/- or proc unc
# 'seeded' is logical indicating if a random seed is used for obs/pro error
# output is data drame with DateTimeStamp, DO_bio, jday, hour, dec_time,
# sunrise, Tide, dTide, DO_adv, DO_tid, DO_noi, DO_obs
ts_create <- function(time_in, do.amp, tide_cat, tide_assoc, err_rng_obs, 
  err_rng_pro, seeded = T){

  require(scales)
  
  # create biological DO time series
  DO_sim <- DO.bio.fun(time_in, do.amp = do.amp)

  # start data.frame for plotting
  DO_sim <- data.frame(
    DateTimeStamp = time_in, 
    DO_bio = DO_sim
    )
  
  # add decimal time on julian day
  DO_sim <- dec_fun(DO_sim)

  # add column for dec_time starting from one (for plotting)
  DO_sim$Day <- with(DO_sim, (dec_time + 1) - jday[1])
  
  # add vector for sunrise, -1 is daylight, 1 is nighttime
  DO_sim$sunrise <- 1
  day_vec <- with(DO_sim, 24*(dec_time - jday))
  DO_sim$sunrise[day_vec > 6 & day_vec <= 18] <- -1
  
  # get tide if character
  if(class(tide_cat) == 'character'){
    if(tide_cat == 'Semidiurnal')
      waves.in <- list(c(12.42, 1)) # M2, principal lunar semidiurnal
    if(tide_cat == 'Diurnal') 
      waves.in <- list(c(25.82, 1)) #O1, principal lunar diurnal
    if(tide_cat == 'Mixed Semidiurnal')
      waves.in <- list(c(12.42, 1), c(25.82, 1)) 
    
    tide <- tide.fun(vec, waves.in)
    }

  # get tide if list
  if(class(tide_cat) == 'list')
    tide <- tide.fun(vec, waves.in)
  
  # get tide if supplied as data.frame, first col is posix, second is tide
  if(class(tide_cat) == 'data.frame'){
    tide <- tide_cat
    tide$Tide <- rescale(tide$Tide, to = c(4, 5))
    tide$dTide <- with(tide, c(diff(Tide)[1], diff(Tide)))
    }
  
  # combine with data frame and add dtide/dt
  DO_sim$Tide <- tide$Tide
  DO_sim$dTide <- tide$dTide

  # add tide
  DO_sim$DO_adv <- rescale(DO_sim$dTide, to = c(-1*tide_assoc, tide_assoc))
  DO_sim$DO_tid <- with(DO_sim, DO_bio + DO_adv)
  
  # add error
  if(seeded) set.seed(1234)
  DO_sim$e_obs <- rnorm(nrow(DO_sim), 0, err_rng_obs)
  if(seeded) set.seed(4321)
  DO_sim$e_pro <- rescale(cumsum(rnorm(nrow(DO_sim), 0, err_rng_pro)), 
    to = c(-1 * err_rng_pro, err_rng_pro))
  
  DO_sim$e_tot <- with(DO_sim, e_obs + e_pro)
  DO_sim$DO_obs <- with(DO_sim, DO_tid + e_tot)
  
  # floor at zero
  DO_sim$DO_obs <- with(DO_sim, pmax(0, DO_obs))
  
  return(DO_sim)
  
  }
  
######
# get predicted, normalized values from interp grid and obs data
# 'grd_in' is interpolation grid in from 'interp_grd' function
# 'dat_in' is raw data used to create 'grd_in' and used to get predictions
# 'DO_obs' is string indicating name of col for observed DO values from 'dat_in'
# output is data frame same as 'dat_in' but includes predicted and norm columns
prdnrm_fun <- function(grd_in, dat_in, DO_obs = 'DO_obs'){
  
  require(reshape)
  
  # merge int grd with obs data
  DO_pred <- merge(grd_in, dat_in[, c('DateTimeStamp', DO_obs, 'dTide')],
    by = 'DateTimeStamp')
   
  # get predicted values from grid using interped tide values closest to actual
  DO_pred <- ddply(DO_pred,
   .variables = 'DateTimeStamp',
   .fun = function(x){
     ind <- which.min(abs(with(x, dTide.x - dTide.y)))
     x[ind,]
   })$DO_pred
  
  # get normalized values by averaging int grd
  # note that this differs from hirsch method for interp
  # assumes all dtide values are equally likely for a given obs
  DO_nrm <-ddply(grd_in, 
   .variables = c('DateTimeStamp'), 
   .fun = function(x) mean(x$DO_pred)
   )$V1
  
  # add predicted to 'dat_in'
  dat_in$DO_pred <- DO_pred
   
  # add normalized to 'dat_in'
  dat_in$DO_nrm <- DO_nrm  
  
  return(dat_in)
  
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
  
