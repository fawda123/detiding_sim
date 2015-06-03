# devtools::install_github('fawda123/WtRegDO')
library(WtRegDO)
library(dplyr)
library(foreach)
library(doParallel)
library(ggplot2)
library(gridExtra)
source('shiny/sim_funs.R')

# create simulated data
dts <- as.POSIXct(c('01-01-2015', '12-31-2015'), format = '%m-%d-%Y', tz = 'America/Regina')
dts <- seq(dts[1], dts[2], by = 1800)
vec <- ts_create(dts, do.amp = 2, tide_cat = 'Semidiurnal', tide_assoc = 2, err_rng_obs = 0, 
  err_rng_pro = 0, seeded = T)
tomod <- select(vec, DateTimeStamp, DO_obs, Tide) %>% 
  mutate(
    Depth = Tide, 
    Sal = 30, 
    ATemp = 15, 
    Temp = 15, 
    BP = 1022, 
    WSpd = 0
  )

tz <- 'America/Regina'
lat <- 30.338
long <- -87.157529

mod <- tomod
mod$DO_bio <- vec$DO_bio

met_obs <- ecometab(mod, DO_var = 'DO_obs', tz = tz, lat = lat, long = long)
met_bio <- ecometab(mod, DO_var = 'DO_bio', tz = tz, lat = lat, long = long)

met_obs <- select(met_obs, Date, Pg, Rt)
names(met_obs) <- c('Date', 'Pg_obs', 'Rt_obs')
met_bio <- select(met_bio, Date, Pg, Rt)
names(met_bio) <- c('Date', 'Pg_bio', 'Rt_bio')

out <- merge(met_obs, met_bio, by = 'Date')
write.csv(out, 'C:/Users/mbeck/Desktop/sim_metab.csv', quote = F, row.names = F)

met_obs <- reshape2::melt(met_obs, id.vars = 'Date', measure.vars = c('Pg', 'Rt'))
met_bio <- reshape2::melt(met_bio, id.vars = 'Date', measure.vars = c('Pg', 'Rt'))

pdf('C:/Users/mbeck/Desktop/sim_metab.pdf', height = 9, width = 12, family = 'serif')

p1 <- ggplot(met_obs, aes(x = Date, y = value, group = variable, colour = variable)) + 
  geom_line() +
  theme_bw()
p2 <- ggplot(met_bio, aes(x = Date, y = value, group = variable, colour = variable)) + 
  geom_line() + 
  theme_bw()

grid.arrange(p1, p2)

aggregate(value ~ variable, met_obs, FUN = var, na.rm = T)
aggregate(value ~ variable, met_bio, FUN = var, na.rm = T)

do_mlt <- reshape2::melt(mod[1:2000, ], id.var = 'DateTimeStamp', measure.var = c('DO_obs', 'Tide', 'DO_prd', 'DO_nrm', 'DO_bio'))

p <- ggplot(do_mlt, aes(x = DateTimeStamp, y = value, group = variable, colour = variable)) + 
  geom_line() + 
  facet_wrap(~variable, ncol = 1, scales = 'free_y') + 
  theme_bw()

p

dev.off()