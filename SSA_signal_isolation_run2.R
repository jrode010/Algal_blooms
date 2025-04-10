# Ecosystem Dynamics and Causality
# Singular spectrum analysis
#Algal blooms
#

library(Rssa)



#### Load data#####
  dat <- read.csv(file = 'coastal_data_month.csv')
  
  dat <- dat %>% dplyr::select(-c(oflow, omaxstage, omeanstage, ominstage, wflow, wmeanstage, gNN, gNO3, gNO2, gAP, gOP, rNN, rNO3, rNO2, rAP, rOP, gchlb, rchlb))

  str(dat)
  
  dat <- dat %>% dplyr::filter(date > ymd('2015-09-01'))
  dat <- dat %>% dplyr::filter(date < ymd('2023-10-01'))
  
  dat <- dat %>% dplyr::select(-c(mcnn, acnn, mcno3, acno3, mcno2, acno2, mcsrp, acsrp))
  
  dat_int <- dat %>% dplyr::select(-date) %>% 
    mutate(across(everything(), ~ na.approx(., na.rm = FALSE)))
  
  str(dat_int)
  
  dat_int <- dat_int %>% mutate(actn = (if_else(actn > 300, 300, actn)))
  
  dat_int$acnh4[nrow(dat_int)] <- 3
  
  dat_int$acdoc[nrow(dat_int)] <- 1900
  
  dat_inc <- dat_int %>% mutate(across(everything(), ~. - mean(., na.rm = F)))
  
  dat_inc <- dat_inc %>% mutate(gsrainfall = (if_else(gsrainfall > 1, 0.5, gsrainfall)))
  
#### Plot frequencies####
  # select dat column
    colnames( dat_inc )
    var <- 'gschlorophyll'
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
    win <- 48
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
    obj |> plot( type = 'wcor' )
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
    grp <- list( c(1,2), c(3,4,7,8,12,16), c(5,6,14), c(9,10,11), c(13,15)) #gchl
    grp <- list( c(1,2), c(3,4,5), c(6,11,13), c(7,8,9,10,12)) #actn
    grp <- list(c(1,2,3,4), c(5,6,7,8), c(9,10,11,12,13)) #actp
    grp <- list(c(1,2,5,6,7), c(3,4), c(8,9,10,13,15)) #acnh4
    grp <- list(c(1,2,3,4), c(5,6), c(7,8), c(9,10,11,12)) #acdoc
    grp <- list(c(1,2), c(3, 4), c(5,6,7,8)) #aflow
    grp <- list(c(1,2), c(3, 4,5,6), c(7,8,9), c(10,11)) #amaxstage
    grp <- list(c(1,2), c(3, 4,5,6)) #aminstage
    grp <- list(c(1,2), c(3,4,5,6,7,8,9), c(10,11)) #gTOC
    grp <- list(c(1,2), c(3,4), c(5,7), c(6,12), c(8,9)) #gTP
    grp <- list(c(1,3), c(2,4), c(5,6,7,8), c(9,10)) #gsal
    grp <- list(c(1,2,3,11), c(4,5,10,13,16), c(6,8), c(7,9,12)) #gNH4
    grp <- list(c(1,2), c(3,4), c(5,8,9,10,11,17), c(6,7)) #gpH
    grp <- list(c(1,2), c(3,4,5,6), c(7,9), c(8,10,13,15), c(11,12), c(14)) #gTN
    grp <- list(c(1,2,3,4,5,6), c(7,8), c(14)) #rTOC
    grp <- list(c(1,2), c(3,4,6,10), c(5,7), c(8,9)) #rTP
    grp <- list(c(1,2), c(3,4,5,6,7,8), c(9,10)) #rpH
    grp <- list(c(1,2), c(3,4,5,7,8,9), c(6,10), c(11,12)) #rTN
    grp <- list(c(1,2,4,5,6, 11, 14), c(7,9,14), c(8,10,12,13), c(15,16,17,18)) #rchl
    grp <- list(c(1,2), c(3,4)) #gsrainfall
    grp <- list(c(1,2)) #gstemperature
    grp <- list(c(1,2), c(3,4)) #gsmeanstage
    grp <- list(c(1,2), c(3,4), c(5,6,7,8,11), c(9,10)) #gschlorophyll

  
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
    par(mfrow=c(2,1))
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
gschl<- data.frame(signal) %>% setNames('gschl')

allssa <- cbind(gchl, actn, actp, acnh4, acdoc, aflow, amaxstage, aminstage, gTOC, gTP, gsal, gNH4, gpH, gTN, rTOC, rTP, rpH, gsrainfall, gsmeanstage, gstemp, gschl)

write.csv(allssa, file = 'SSA_run2.csv')

ssadates <- cbind(allssa, dat$date) %>% rename(date = `dat$date`)
write.csv(ssadates, file = 'SSA_run2_dates.csv')
