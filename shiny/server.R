load('comb_grd.RData')
source('sim_funs.r')

require(ggplot2)
require(scales)
require(gridExtra)
require(reshape2)
require(RColorBrewer)

# Define server logic required to generate and plot data
shinyServer(function(input, output) {
  
  output$simplot <- renderPlot({
    
    # add plotting code here
    
#     # for debugging only
#     input <- list(
#       assoc_in = 2,
#       bio_in = 2,
#       epro_in = 0,
#       eobs_in = 0,
#       dectm_in = 1,
#       hr_in = 6,
#       Td_in = 0.25
#       )
    
    # input values from ui
    assoc_in <- input$assoc_in
    bio_in <- input$bio_in
    epro_in <- input$epro_in
    eobs_in <- input$eobs_in
    dectm <- input$dectm_in
    hr <- input$hr_in
    Td <- input$Td_in
    show <- input$show_corr
    
    # get vector of file to load from input
    sel_vec <- with(comb_grd,
                      bio_rng %in% bio_in &
                      tide_assoc %in% assoc_in &
                      err_rng_obs %in% eobs_in &
                      err_rng_pro %in% epro_in &
                      dec_time %in% dectm &
                      hour %in% hr &
                      Tide %in% Td
    )
    sel_vec <- which(sel_vec)
    
    # load file, assign to 'prd_tmp', remove orig
    load(paste0('prdnrm/prdnrm_', sel_vec, '.RData'))
    prd_tmp <- get(paste0('prdnrm_', sel_vec))
    rm(list = paste0('prdnrm_', sel_vec))
    
    # make long form for facetting, reassign factor labels for order
    levs <- c('DO_bio', 'DO_adv', 'DO_obs', 'DO_prd',
              'DO_nrm')
    to.plo <- melt(prd_tmp, id.var = c('Day'),
                   measure.var = levs
    )
    to.plo$variable <- factor(to.plo$variable, levels = levs)
    
    # create plots
    ylab<-expression(paste('DO (mg ',L^-1,')'))
    
    p1 <- ggplot(to.plo, aes(x = Day, y = value)) +
      geom_line(size = 1.1, alpha =0.8, colour = 'darkgreen') +
      facet_wrap(~ variable, scales = 'free_y', ncol = 1) + 
      theme_bw() +
      ylab(ylab)
    
    
    p1 <- facet_wrap_labeller(p1, labels = c(
      expression(italic(DO [bio])),
      expression(italic(DO [adv])),
      expression(italic(DO [obs])),
      expression(italic(DO [prd])),
      expression(italic(DO [nrm]))
    ))
    
    if(!show){
      print(p1)
    } else {
      
      to_plo2 <- prd_tmp
      title_val <- round(with(prd_tmp, cor(DO_bio, DO_nrm)), 3)
      p2 <- ggplot(to_plo2, aes(x = DO_bio, y = DO_nrm)) +
        geom_point() + 
        stat_smooth(method = 'lm') +
        ggtitle(paste('Corr', title_val)) +
        theme_bw()
      
      print(p2)
    }
    
  },height = 700, width = 700)
  
})