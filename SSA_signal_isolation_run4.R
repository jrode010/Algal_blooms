# Ecosystem Dynamics and Causality
# Singular spectrum analysis
#Algal blooms
#

library(Rssa)



#### Load data#####
  dat <- read.csv(file = 'coastal_data_month.csv')
  
  dat <- dat %>% dplyr::select(-c(oflow, omaxstage, omeanstage, ominstage, wflow, wmeanstage, gNN, gNO3, gNO2, gAP, gOP, rNN, rNO3, rNO2, rAP, rOP, gchlb, rchlb))

  str(dat)
  
  dat <- dat %>% dplyr::filter(date > ymd('2016-01-01'))
  dat <- dat %>% dplyr::filter(date < ymd('2024-04-01'))
  ccdat <- ccdat %>%  dplyr::filter(date > ymd('2016-01-01'))
  ccdat <- ccdat %>% dplyr::filter(date < ymd('2024-04-01'))
  ecdat <- ecdat %>%  dplyr::filter(date > ymd('2016-01-01'))
  ecdat <- ecdat %>% dplyr::filter(date < ymd('2024-04-01'))
  
  dat <- merge(dat, ecdat, by = 'date', all = T)
  
  dat <- dat %>% dplyr::select(-c(mcnn, acnn, mcno3, acno3, mcno2, acno2, mcsrp, acsrp))
  
  dat_int <- dat %>% dplyr::select(-date) %>% 
    mutate(across(everything(), ~ na.approx(., na.rm = FALSE)))
  
  str(dat_int)
  
  dat_int <- dat_int %>% mutate(actn = (if_else(actn > 300, 300, actn)))
  
  dat_int$acnh4[nrow(dat_int)] <- 3
  
  dat_int$acdoc[nrow(dat_int)] <- 1900
  
  write.csv(dat_int, file = 'field_dat_sat_timeline.csv')
  
  dat_inc <- dat_int %>% mutate(across(everything(), ~. - mean(., na.rm = F)))
  
  dat_inc <- dat_inc %>% mutate(gsrainfall = (if_else(gsrainfall > 1, 0.5, gsrainfall))) %>% slice(2:(n()-1))
  
  dat_inc <- dat_inc %>% dplyr::select(-c(mctn, actn, mctp, actp, mcsal, acsal))
  
  dat_inc <- dat_inc %>% slice(2:(n()-1))
  
  dat1 <- dat %>% slice(2:(n()-1))
  
##CC data
  head(dat_int)
  ecdat_int <- dat_int %>% dplyr::select(eNH4, eTOC, eDO, eTP, esal, eturb, etemp, eNO2, eTN, epH, echla)
ecdat_int <- ecdat_int %>% slice(2:(n()-1))
ecdat_inc <- ecdat_int %>% mutate(across(everything(), ~. - mean(., na.rm = F)))
  
#### Plot frequencies####
  # select dat column
    colnames( ecdat_inc )
    var <- 'eTP'
    x <- ecdat_inc[,var]
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
    win <- 38
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
    obj |> plot( type = 'vectors', numvectors = 24 )
    obj |> plot( type = 'paired', numvectors = 16 )
    # W-correlation matrix
    obj |> plot( type = 'wcor' )
    wcor(obj,groups = 1:25) |> plot()

    
# SSA grouping
##
  # Specify signal component groups
    #grp <- list( c(1,2), c(3,4), c(5,6), c(7,8) )  # xn
    #grp <- list(   c(1,4,5,8,9)  # STA34_Cin - first group is all of the "trend" components
    #             , c(2,3)
    #              , c(6,7)
    #              , c(10,11)
    #                 )
    grp <- list( c(1,2), c(3,4), c(5,6), c(7,8), c(9,10,18)) #mctnm
    grp <- list( c(1,2), c(3,4), c(5,6,7,11), c(8,9,10,12,17)) #actnm
    grp <- list(c(1,2), c(3,4), c(5,6,9,10,11), c(7,8)) #actpm
    grp <- list(c(1,2), c(3,4, 5, 6, 7, 8,9,10)) #mctpm
    grp <- list(c(1,2,3,4), c(5,6, 7, 8, 11, 12), c(9,10), c(13,14,15,18)) #mcnh4
    grp <- list(c(1,2,3,4,8), c(5,6), c(7,9,12,17)) #acnh4
    grp <- list(c(1,2), c(3, 4), c(5,6), c(7,8,9,10,11,13,14,15), c(12)) #mcdoc
    grp <- list(c(1,2,3,4), c(5,6), c(7,8,9,10)) #acdoc
    grp <- list(c(1,2), c(3,4), c(5,6), c(7,8), c(9,10,15), c(11,12), c(13,14)) #mcsalm
    grp <- list(c(1,2), c(3,4), c(5,6), c(7,8), c(9,10)) #acsalm
    grp <- list(c(1,2), c(3,4,5,8,9,10), c(6,7,11,12,13,14), c(15,16)) #aflow
    grp <- list(c(1,2), c(3,4), c(5,6,7,8,9)) #amaxstage
    grp <- list(c(1,2), c(3,4,5,6,7,8,9,10)) #mflow
    grp <- list(c(1,2), c(3,4), c(5,6,7,10,11), c(8,9), c(12,13,14)) #mmaxstage
    grp <- list(c(1,2), c(3,4,5,6,7,8,9), c(10,11), c(12,17)) #gTOC
    grp <- list(c(1,2), c(3,4), c(5,6,7,8,9,10,13), c(11,12)) #gTP
    grp <- list(c(1,2,3,4,5,6), c(7,8), c(9,10,11,12)) #gsal
    grp <- list(c(1,2), c(3,4), c(5,6,7,10), c(8,9), c(12,18), c(13,14), c(15, 16, 17, 21)) #gpH
    grp <- list(c(1,2), c(3,9), c(4,5,6,7,8,10,11,12,13)) #gchl
    grp <- list(c(1,2), c(3,4), c(5,6,7,9), c(8), c(10,11,12,14), c(13,15)) #gTN
    grp <- list(c(1,2,3,4), c(5,9,10,15,16), c(6,7), c(8,11,12,13), c(14)) #rTOC
    grp <- list(c(1,2), c(3,4,7,8), c(5,6,9,11,13,14)) #rTP
    grp <- list(c(1,2), c(3,4), c(5,6,7,8), c(9,10,11,12), c(13,14), c(15,16)) #rsal
    grp <- list(c(1,2), c(3,4,5,6), c(7,8,11,12,13), c(9,10)) #rpH
    grp <- list(c(1,2,3,4,5,6,7,15), c(8,9,10,11,13,14, 17,18), c(12,16)) #rchl
    grp <- list(c(1,6), c(2,3,4,5), c(7,8), c(9,10,11,12,13,14,15,18,19,20), c(16,17,21)) #rTN
    grp <- list(c(1,2), c(3,4)) #marshmeanrain
    grp <- list(c(1,2), c(3,4)) #gsrainfall
    grp <- list(c(1,2), c(3,4), c(5,6,7,10,13,14), c(8,9), c(11,12)) #gsmeanstage
    grp <- list(c(1,2), c(3,4,5,6,7,8), c(9,10)) #cNH4
    grp <- list(c(1,2), c(3,4), c(5,6,8,9,10,12,13,14), c(7,11), c(15,17), c(16,18)) #cTOC
    grp <- list(c(1,2), c(3,4,5,6,11,13), c(7,8)) #cDO
    grp <- list(c(1,2), c(3,6), c(4,5), c(7,8,9), c(10,11,12,14), c(13,15)) #cTP
    grp <- list(c(1,2), c(3,4,5,6), c(7,10,11,12,13,14,15), c(8,9)) #csal
    grp <- list(c(1,2), c(3,4,5,6,7), c(8,9), c(10,11,12,13,14)) #cturb
    grp <- list(c(1,2), c(3,4)) #ctemp
    grp <- list(c(1,2,3,4,5,6,9), c(7,8,10,11,12,13,14,15,16)) #cTN
    grp <- list(c(1,2), c(3,4), c(5,6,7,8), c(9,10,11,14)) #cpH
    grp <- list(c(1,2,3,4), c(5,6), c(7,8), c(9,10,11,12)) #cchl
    grp <- list(c(1,12), c(2,4), c(3,5,6,7,8,11), c(9,10), c(13,14), c(15,16,17,18)) #gNH4
    grp <- list(c(1,2,3,4,5,6), c(7,8,9,10), c(11,12)) #eNH4
    grp <- list(c(1,2), c(3,4), c(5,6,7,8), c(9,10,11,14), c(12,13,15,16), c(17)) #eTOC
    grp <- list(c(1,2), c(3,4,5,6,7,8,11,12), c(9,10), c(13,14,16,17), c(15,18)) #eDO
    grp <- list(c(1,2,3,6,7), c(4,5,8), c(9,10,11,12,13), c(14,15,16), c(17,18)) #eTP
    grp <- list(c(1,2), c(3,4,7,8), c(5,6,9,10), c(11,12)) #esal
    grp <- list(c(1,2,4,5), c(3,6), c(7,9,10,11), c(8,12), c(13,14)) #eturb
    grp <- list(c(1,2), c(3,4)) #etemp
    grp <- list(c(1,2,7), c(3,4,5,6), c(8,9), c(10,11,12)) #eTN
    grp <- list(c(1,2), c(3,4), c(5,6), c(7,8), c(9,10), c(11,12), c(13,14), c(15,16), c(17,18)) #epH
    grp <- list(c(1,2,3,8,9), c(4,6,10), c(5,7,13,14), c(11,12)) #echl
    


  
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
eTP <- data.frame(signal) %>% setNames('eTP')

allssa <- cbind(eNH4, eTOC, eDO, eTP, esal, eturb, etemp, eTN, epH, echl)

write.csv(allssa, file = 'SSA_ec.csv')

ssadates <- cbind(allssa, dat1$date) %>% rename(date = `dat1$date`)
write.csv(ssadates, file = 'SSA_cc_dates.csv')
