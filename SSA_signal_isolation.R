# Ecosystem Dynamics and Causality
# Singular spectrum analysis
#Algal blooms
#

library(Rssa)



# Load data
  dat <- read.csv(file = 'coastal_data_month.csv')
  
  dat <- dat %>% dplyr::select(-c(oflow, omaxstage, omeanstage, ominstage, wflow, wmeanstage, gNN, gNO3, gNO2, gAP, gOP, rNN, rNO3, rNO2, rAP, rOP, gchlb, rchlb))

  str(dat)
  
  dat <- dat %>% dplyr::filter(date > ymd('2010-10-01'))
  dat <- dat %>% dplyr::filter(date < ymd('2024-06-01'))
  
  dat <- dat %>% dplyr::select(-c(mcnn, acnn, mcno3, acno3, mcno2, acno2, mcsrp, acsrp))
  
  dat_int <- dat %>%
    mutate(across(everything(), ~ na.approx(., na.rm = FALSE)))
  
  str(dat_int)
  
  dat_int <- dat_int %>% mutate(actn = (if_else(actn > 300, 300, actn)))
  
  dat_inc <- dat_int %>% mutate(across(everything(), ~. - mean(., na.rm = F))) %>% dplyr::select(-date)
  
  ?if_else
  
# Plot frequencies
  # select dat column
    colnames( dat_inc )
    var <- 'amaxstage'
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
    win <- 72
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
    grp <- list( c(1), c(2,3,4,5), c(7,8,9)) #gchl
    grp <- list( c(1,2,3), c(4,5,6), c(7,8,9)) #mctn
    grp <- list( c(1,2), c(3,4,5,6, 7, 8,9,10)) #actn
    grp <- list(c(1,4), c(2, 3), c(5,6), c(7,9), c(8,10), c(11,12,13)) #mctp
    grp <- list(c(1,2), c(3,4), c(5, 6,9), c(7,8,10,11,12,13,14,15)) #actp
    grp <- list(c(1,2), c(3, 4), c(5,6), c(7,8,9,10), c(11,12)) #mcsal
    grp <- list(c(1,2), c(3, 4,5, 6,7,8), c(9,10,11)) #acsal
    grp <- list(c(1,4), c(2, 3,5)) #mcnh4
    grp <- list(c(1,2,3,4), c(5), c(6,7), c(8,9), c(10,11)) #acnh4
    grp <- list(c(1,2), c(3, 4,5, 6), c(7,8), c(9,10)) #acdoc
    grp <- list(c(1,2), c(3, 4)) #aflow
    grp <- list(c(1,2), c(3, 4), c(5,6), c(7,8), c(9,10)) #amaxstage
    grp <- list(c(1,2), c(3, 4,5, 6,7,8), c(9,10,11)) #acsal

  
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
amaxstage<- data.frame(signal) %>% setNames('amaxstage')

allssa <- cbind(gbchl, acs, mcs, gbtn, rbtn, gbphos, rbphos)

write.csv(allssa, file = 'ssa_FB_ac.csv')
