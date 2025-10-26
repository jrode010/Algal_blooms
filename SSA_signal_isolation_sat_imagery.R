# Ecosystem Dynamics and Causality
# Singular spectrum analysis
#Algal blooms
#

library(Rssa)
library(tidyverse)
library(stringr)
library(lubridate)
library(zoo)

#### Load data#####
  dat <- read.csv(file = 'Data/Clean/sat_dat_usf.csv')
  
  #Change character column to date
  dat <- dat %>%
    mutate(
      year = str_extract(source_file, "^\\d{4}"),
      month = str_extract(source_file, "(?<=_)\\d{2}(?=_)"),
      date = ymd(paste(year, month, "01", sep = "-"))
    )
  
  str(dat)

  dat1 <- dat[-c(97:112), ]
  dat_int <- dat1 %>% dplyr::select(-c(month, year, n_patches, source_file, X)) %>% mutate(logarea = log(total_area_m2))
  
  str(dat_int)

  dat_inc <- dat_int %>% mutate(logeast = log(easting), lognorth = log(northing))
  dat_inc <- dat_inc %>% mutate(areakm = total_area_m2/1000000)
  
ggplot()+
  geom_line(data = dat_inc, aes(x = date, y = areakm), color = 'blue4', size = 1.5)+
  ylab(expression(Area~(km^2)))+
  ggtitle('Algal Bloom size')+
  xlab('Year')+
  scale_x_date(date_breaks = "1 year", date_labels = "%Y")+
  theme_classic()+
  theme(axis.title   = element_text(face = "bold"),   # both x and y labels bold
        axis.text    = element_text(face = "bold"),
        plot.title = element_text(hjust = 0.5))

 ggsave(filename = 'Algal_bloom_size.png', width = 4.77, height = 5) 
  #### Plot frequencies####
  # select dat column
    colnames( dat_inc )
    var <- 'lognorth'
    x <- dat_inc[,var]
  # Fourier transform
    spec <- spectrum( x, method = 'pgram', plot = FALSE )
    df <- data.frame( power = spec$spec, period = 1/spec$freq )
    df <- df[ order( df$period ), ]
  # plot time series
   par(mfrow=c(2,1))
    plot( x, type = 'l', col = rgb(0,0,0,0.5), lwd = 3,
          las = 1, xlab = 'time', ylab = 'x',
          main = paste0( "Time series (",var,")" ),
          cex.lab = 1.3, cex.axis = 1.3, cex.main = 1.3 )
    points( x, col = rgb(0,0,0,1), pch = 16, cex = 0.8 )
    abline( v = axTicks(1), col = rgb(0,0,0,0.1))
    abline( h = axTicks(2), col = rgb(0,0,0,0.1))  
  # plot periodogram
    plot( power ~ period, data = df,
          type = 'l', lwd = 3, col = rgb(0,0,0,0.5),
          main = 'Periodogram', las = 1, bty = "L",
          cex.lab = 1.3, cex.axis = 1.3, cex.main = 1.3 )
    points( power ~ period, data = df, pch = 16, cex = 0.8 )
    abline( v = axTicks(1), col = rgb(0,0,0,0.1))
    abline( h = axTicks(2), col = rgb(0,0,0,0.1))
    df <- df[ order( df$power, decreasing = TRUE ), ]
    text( x = df$period[1:5], y = df$power[1:5],
          labels = round(df$period[1:5],2), pos = 4, font = 2 )
  # print spectrum
    df |> head(10)


# SSA decomposition
##
  # Set window length - window length has to be less than half of the time series and a multiple of the main component
    length( x )  
    win <- 32
  # Decompose
    obj <- ssa( x, L = win, neig = win,
                kind = 'toeplitz-ssa' )
  # Eigentriple plots
    # Singular values
    par(mfrow=c(1,1))
    obj$sigma |> plot( main = "Singular values",
                       xlab = 'eigentriple', ylab = 'singular value' )
    obj$sigma |> lines()
    abline( v = axTicks(1), col = rgb(0,0,0,0.1))
    abline( h = axTicks(2), col = rgb(0,0,0,0.1))
      # Eigenvectors
    obj |> plot( type = 'vectors', numvectors = 16 )
    obj |> plot( type = 'paired', numvectors = 16 )
    # W-correlation matrix
    #obj |> plot( type = 'wcor' )
    wcor(obj,groups = 1:30) |> plot()

    
# SSA grouping
##
  # Specify signal component groups
    #grp <- list( c(1,2), c(3,4), c(5,6), c(7,8) )  # xn
    #grp <- list(   c(1,4,5,8,9)  # STA34_Cin - first group is all of the "trend" components
    #             , c(2,3)
    #              , c(6,7)
    #              , c(10,11)
    #                 )
    grp <- list(c(1,2, 3, 4), c(5,6), c(7,8), c(9,12,13), c(10,11)) #area
    grp <- list( c(1,2,3,4, 9, 13), c(5,6,7,8, 10,11), c(12,16), c(14,15)) #easting
    grp <- list(c(1,2,5,6), c(3,4), c(7,8,9,10,11,12,13,14)) #northing
    grp <- list( c(1,2,6,7), c(3,4,5,6,7,10), c(8,9), c(11,14,15), c(12,13,23), c(16,19,20), c(17,18,21,22,26,27,28,33)) #gDO
    grp <- list(c(1,2,7,8,9,10), c(3,4,5,6), c(11,12,13,14,15,16,17,18,19,20,23), c(21,22)) #gsal
    grp <- list(c(1,2), c(3,4,5,6,7,8,9,10,11)) #gtemp
    grp <- list(c(1,2), c(3, 4), c(5,6,7,8,9,10,11), c(12,13,14,15,16,17,21,27)) #gmeanstage
    grp <- list(c(1,2), c(3, 4,5,7), c(6,8), c(9,10), c(11,12), c(13,14), c(15,16,17,18,19,20,28)) #acflow
    grp <- list(c(1,2), c(3,4,5,6,7,8,11), c(9,10), c(12,13,14,15,16,17,21,25)) #acmaxstage
    grp <- list(c(1,2), c(3,4), c(5,6,7,8,9,10,11), c(12,13), c(14,15,16,17,22,25), c(18,21), c(19,20,26,27)) #acminstage
    # grp <- list(c(1,3), c(2,4), c(5,6,7,8), c(9,10)) #gsal
    # grp <- list(c(1,2,3,11), c(4,5,10,13,16), c(6,8), c(7,9,12)) #gNH4
    # grp <- list(c(1,2), c(3,4), c(5,8,9,10,11,17), c(6,7)) #gpH
    # grp <- list(c(1,2), c(3,4,5,6), c(7,9), c(8,10,13,15), c(11,12), c(14)) #gTN
    # grp <- list(c(1,2,3,4,5,6), c(7,8), c(14)) #rTOC
    # grp <- list(c(1,2), c(3,4,6,10), c(5,7), c(8,9)) #rTP
    # grp <- list(c(1,2), c(3,4,5,6,7,8), c(9,10)) #rpH
    # grp <- list(c(1,2), c(3,4,5,7,8,9), c(6,10), c(11,12)) #rTN
    # grp <- list(c(1,2,4,5,6, 11, 14), c(7,9,14), c(8,10,12,13), c(15,16,17,18)) #rchl
    # grp <- list(c(1,2), c(3,4)) #gsrainfall
    # grp <- list(c(1,2)) #gstemperature
    # grp <- list(c(1,2), c(3,4)) #gsmeanstage
    # grp <- list(c(1,2), c(3,4), c(5,6,7,8,11), c(9,10)) #gschlorophyll


# SSA reconstruction
##
  # Add a residuals (noise) group
    grp[[ length(grp)+1 ]] <- which( !(1:win %in% unlist(grp)) )
  # Reconstruct grouped components
    recon <- obj |> reconstruct( groups = grp )
  # W-correlation matrix
    wcor.recon <- wcor( obj, groups = grp )
    wcor.recon |> plot()
    wcor.recon
  # Compute variance explained by each component
    eigenvals <- obj$sigma^2
    varexp <- lapply( grp, function(x) sum( eigenvals[x] ) * 100 / sum(eigenvals) )
    
  # Plot reconstructed components
    par(mfrow=c(3,3))
    ylims <- range(unlist(recon))
    for( i in 1:length(recon) ){
      if( i < length(grp) ){
        # Plot signal components
        plot( recon[[i]], type = 'l', ylim = ylims, las = 1,
              main = paste0("Group ",paste(grp[[i]],collapse=", "),
                           " (",round(varexp[[i]],2),"%)"),
              xlab = '', ylab = ''
              )
        abline( v = axTicks(1), col = rgb(0,0,0,0.1))
        abline( h = axTicks(2), col = rgb(0,0,0,0.1))
      } else {
        # Plot noise
        plot( recon[[i]], type = 'l', col = rgb(0.1,0.2,1,0.8),
              ylim = ylims, las = 1,
              main = paste0("Noise"," (",round(varexp[[i]],2),"%)"),
              xlab = '', ylab = ''
        )
        abline( v = axTicks(1), col = rgb(0,0,0,0.1))
        abline( h = axTicks(2), col = rgb(0,0,0,0.1))
        }
    }  # // end i 

  # Reconstruct signal and noise
    signal <- do.call( cbind, recon[ 1:(length(recon)-1) ] ) |> rowSums()
    sigstrength <- varexp[1:(length(varexp)-1)] |> unlist() |> sum()
    noise <- recon[[ length(recon) ]]

  # Plot signal with noise
    par(mfrow=c(1,1))
    ylims2 <- range( x, signal, noise )
    plot( x, type = 'l', ylim = ylims2, las = 1,
          main = paste0("Signal (",round(sigstrength,2),"%)"),
          xlab = "",
          lwd = 2, col = rgb(0,0,0,0.6) )
    lines( signal, lwd = 4, col = rgb(1,0.2,0.1,0.8) )
    abline( v = axTicks(1), col = rgb(0,0,0,0.1))
    abline( h = axTicks(2), col = rgb(0,0,0,0.1))
    plot( noise, type = 'l', ylim = ylims2, las = 1,
          main = "Noise", xlab = "time",
          col = rgb(0.1,0.2,1,0.8) )
    abline( v = axTicks(1), col = rgb(0,0,0,0.1))
    abline( h = axTicks(2), col = rgb(0,0,0,0.1))

 ####   
    signal
north<- data.frame(signal) %>% setNames('northing')

allssa <- cbind(marea, east, north)

write.csv(allssa, file = 'SSA_sat.csv')

ssadates <- cbind(allssa, dat1$date) %>% rename(date = `dat1$date`)
write.csv(ssadates, file = 'SSA_sat_dates.csv')
