# Ecosystem Dynamics and Causality
# Phase space reconstruction
# Rodemann, algal blooms
#


library(tidyverse)
library(tseriesChaos)
library(zoo)
library(plotly)

# Load data and select column
dat <- allssa
dat <- read.csv("SSA_run1.csv")
  colnames( dat )
  var <- 'gsmeanstage'
  x <- dat[,var] |> na.omit()
  x <- scale( x )

# Plot the time series
  par(mfrow=c(2,1))
  plot( x, type = 'l', xlab = 'time', main = "Time series x" )
  abline( v = axTicks(1), col = rgb(0,0,0,0.2) )
  abline( h = axTicks(2), col = rgb(0,0,0,0.2) )

# Find embedding parameters
  par(mfrow=c(3,1))
  # Set embedding delay (d) using AMI function
  ami <- mutual( x, lag.max = 20 )  # average mutual information function
  local.d.min <- rollapply( ami, 3, function (x) which.min(x)==2 )  # local minima
  d <- as.numeric( which(local.d.min==TRUE)[1] )  # first local min
  points( x = d, y = ami[d+1], cex = 2, col = rgb(1,0,0,0.7) )
  d

  # Set Theiler window (tw) using ACF
  ac <- acf( x, lag.max = 20 )  # autocorrelation function
  ac$acf_abs <- ac$acf |> abs()  # absolute values of acf
  local.tw.min <- rollapply(ac$acf_abs, 3, function (x) which.min(x)==2 )  # abs local minima
  tw <- as.numeric( which(local.tw.min==TRUE)[1] )  # first abs local min
  points( x = tw, y = ac$acf[tw+1], cex = 2, col = rgb(1,0,0,0.7) )
  tw = 3
 #tw = 14 #For phosphorus into STA3

  # Set embedding dimension (m) using false nearest neighbors test
  fnn <- false.nearest( series=x, m=10, d=d, t=tw, eps=sd(x), rt=10 )
  threshold <- 0.15
  plot( fnn[1,], type = 'b', main = "FNN", pch = 16, cex = 2,
        xlab = 'dim', ylab = 'proportion of false neighbors' )
    abline( h = threshold, lty = 2, col = rgb(0,0,0,0.6) )
  m <- as.numeric( which( fnn[1,] <= threshold )[1] )
  points( x = m, y = fnn[1,m], cex = 3, col = rgb(1,0,0,0.7) )
  m = 3
  
# Time-delay embedding
  Mx <- embedd( x, m = m, d = d ) |> as.data.frame()

# Plot phase space reconstruction
  # Rename columns
  for(i in 1:m){
    if(i==1){ names(Mx)[i]<-'x(t)'
    } else {
      names(Mx)[i] <- paste0('x(t+',d*(i-1),')')
    }
  }  # // end i
  # Plotly
  print.htmlwidget <- function(widget){
    temp_file <- paste(tempfile('widget'), 'html', sep = '.')
    htmlwidgets::saveWidget(widget, temp_file, selfcontained = FALSE)
    shell(sprintf("start chrome -app=file://%s", temp_file))
  }
  plot_ly( Mx, x = ~Mx[,1], y = ~Mx[,2], z = ~Mx[,3],
           type = 'scatter3d', mode = 'lines',
           opacity = 0.75, line = list(width = 6, reverscale = FALSE) ) |> 
  layout( title = 'Reconstructed attractor',
          scene = list( xaxis = list(title=names(Mx)[1]),
                        yaxis = list(title=names(Mx)[2]),
                        zaxis = list(title=names(Mx)[3])
          ) )
  

# Test for nonlinear stationarity with space-time separation plots
  par(mfrow=c(1,1))
  stp <- stplot( series = x, m = m, d = d, mdt = length(x) )
  

  