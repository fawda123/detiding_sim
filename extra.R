# # ts example
# 
# # create time vector
# vec <- c('2014-05-01 00:00:00', '2014-05-31 00:00:00')
# vec <- as.POSIXct(vec, format = '%Y-%m-%d %H:%M:%S')
# vec <- seq(vec[1], vec[2], by = 60*30)
# 
# # create simulated time series of DO, tide, etc.
# DO_sim <- ts_create(
#   vec, 
#   do.amp = 2, 
#   tide_cat = 'Diurnal', 
#   tide_assoc = 4,
#   err_rng_obs = 2, 
#   err_rng_pro = 2, 
#   seeded = T
#   )  
# 
# to_plo <- DO_sim
# 
# levs <- c('e_obs', 'e_pro', 'e_tot', 'DO_bio', 'DO_adv', 'DO_obs')
# to.plo <- melt(to_plo, id.var = c('Day', 'sunrise'),
#   measure.var = levs
#   )
# to.plo$variable <- factor(to.plo$variable, levels = levs)
# 
# p <- ggplot(to.plo, aes(x = Day, y = value, col = sunrise)) +
#   geom_line() +
#   facet_wrap(~ variable, scales = 'free_y', ncol = 1) + 
#   theme_bw() +
#   scale_colour_gradientn(colours = c('orange', 'black')) +
#   theme(legend.position = 'none')
#  
# facet_wrap_labeller(p, labels = c(
#   expression(italic(epsilon [obs])),
#   expression(italic(epsilon [ pro])),
#   expression(italic(epsilon [ obs] + epsilon [ pro])),
#   expression(italic(DO [Bio])), 
#   expression(italic(DO [adv])),
#   expression(italic(paste(DO [obs], '=', DO [bio] + DO [adv])))
#   ))
# 
# 
# cl <- makeCluster(8)
# registerDoParallel(cl)
# int_grd <- wtreg_fun(DO_sim, wins = list(4, 0.5, NULL), parallel = T)
# stopCluster(cl)
# 
# to_plo <- prdnrm
# 
# # results
# ggplot(to_plo, aes(x = DateTimeStamp, y = DO_obs, colour = 'DO_obs')) + 
#   geom_line() +
#   geom_line(aes(y = DO_prd, colour = 'DO_prd'), size = 1) +
#   geom_line(aes(y = DO_nrm, colour = factor(sunrise), group = 1), size = 1) +
#   scale_y_continuous(labels = function(x) format(x, digits = 2)) +
#   theme_bw() + 
#   theme(legend.title = element_blank(), legend.position = 'top') 
# 
# # ggpairs
# prdnrm$DO_est <- with(prdnrm, DO_nrm + DO_obs - DO_prd)
# to_plo <- prdnrm[, c('Tide', 'DO_adv', 'DO_bio', 'DO_obs', 'DO_prd', 'DO_nrm', 'DO_est')]
# 
# ggpairs(to_plo)

######
# ts example from comb_grd

load('comb_grd.RData')

to_sim <- comb_grd[9,]
# create simulated time series of DO, tide, etc., representatives
vec <- c('2014-05-01 00:00:00', '2014-05-31 00:00:00')
vec <- as.POSIXct(vec, format = '%Y-%m-%d %H:%M:%S')
vec <- seq(vec[1], vec[2], by = 60*30)
DO_sim <- ts_create(vec, 
  do.amp = to_sim$bio_rng, 
  tide_cat = to_sim$tide_cat, 
  tide_assoc = to_sim$tide_assoc, 
  err_rng_obs = to_sim$err_rng_obs, 
  err_rng_pro = to_sim$err_rng_pro, 
  seeded = T)

wins_in <- as.numeric(to_sim[, c('jday', 'hour', 'Tide')])
wins_in <- list(wins_in[1], wins_in[2], wins_in[3])
int_grd <- wtreg_fun(DO_sim, wins = wins_in, parallel = F)

to_plo <- int_grd
with(na.omit(to_plo), rmse(DO_obs, DO_prd))
with(na.omit(to_plo), cor(DO_obs, DO_prd))

# results
ggplot(to_plo, aes(x = DateTimeStamp, y = DO_obs, colour = 'DO_obs')) + 
  geom_line() +
  geom_line(aes(y = DO_prd, colour = 'DO_prd'), size = 1) +
  geom_line(aes(y = DO_nrm, colour = factor(sunrise), group = 1), size = 1) +
  scale_y_continuous(labels = function(x) format(x, digits = 2)) +
  theme_bw() + 
  theme(legend.title = element_blank(), legend.position = 'top') 

# ggpairs
to_plo_prs <- to_plo[, c('Tide', 'DO_bio', 'DO_obs', 'DO_prd', 'DO_nrm', 'DO_dtd')]

ggpairs(to_plo_prs)

######
to_sim2 <- comb_grd[27,]
# create simulated time series of DO, tide, etc., representatives

DO_sim2 <- ts_create(vec, 
  do.amp = to_sim2$bio_rng, 
  tide_cat = to_sim2$tide_cat, 
  tide_assoc = to_sim2$tide_assoc, 
  err_rng_obs = to_sim2$err_rng_obs, 
  err_rng_pro = to_sim2$err_rng_pro, 
  seeded = T)

wins_in <- as.numeric(to_sim2[, c('jday', 'hour', 'Tide')])
wins_in <- list(wins_in[1], wins_in[2], wins_in[3])
int_grd2 <- wtreg_fun(DO_sim2, wins = wins_in, parallel = F)

to_plo2 <- int_grd2
with(na.omit(to_plo2), rmse(DO_obs, DO_prd))
with(na.omit(to_plo2), cor(DO_obs, DO_prd))

# results
ggplot(to_plo2, aes(x = DateTimeStamp, y = DO_obs, colour = 'DO_obs')) + 
  geom_line() +
  geom_line(aes(y = DO_prd, colour = 'DO_prd'), size = 1) +
  geom_line(aes(y = DO_nrm, colour = factor(sunrise), group = 1), size = 1) +
  scale_y_continuous(labels = function(x) format(x, digits = 2)) +
  theme_bw() + 
  theme(legend.title = element_blank(), legend.position = 'top') 

# ggpairs
to_plo_prs2 <- to_plo2[, c('Tide', 'DO_bio', 'DO_obs', 'DO_prd', 'DO_nrm', 'DO_dtd')]

ggpairs(to_plo_prs2)




