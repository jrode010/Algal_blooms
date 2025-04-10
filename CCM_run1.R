# Ecosystem Dynamics and Causality
# Rodemann - algal blooms CCM
# Load libraries
library(rEDM)


# Load data
dat <- read.csv("SSA_run1_dates.csv")
colnames( dat )

# Select variables for CCM test
y <- "gchl"  # effect
x <- "gsrainfall"  # cause
df1 <- dat[,c("date",x,y)] |> na.omit()
df1$date <- df1$date |> ymd() # format dates
df1[,c(2,3)] <- apply( df1[,c(2,3)], 2, scale )  # scale signals to mean=0, sd=1
dim( df1 )

# Run CCM and plot results
ccm <- CCM( dataFrame = df1,
            E = 2,   # embedding dimension
            tau = -4,   # embedding delay
            exclusionRadius = 17,   # Theiler window
            target = x,   # prediction target (cause)
            columns = y,   # library (effect) 
            libSizes = "10 140 5",  # string for sequence 'from, to, by'
            sample = 100,   # number of replicate tests at each libSize
            showPlot = TRUE,
            parameterList = TRUE,
            includeData = TRUE
            )

# Output: CCM summary table
ccm$LibMeans

# Output: Results for each 'y xmap x' test
ccm$CCM1_PredictStat |> tail()


# A nicer plot for 'y xmap x' tests
plot( x = ccm$LibMeans$LibSize,
      y = ccm$LibMeans[,2],
      main = paste( y, 'xmap', x),
      ylab = "Prediction skill", xlab = "Library size",
      ylim = range( 0, range(ccm$LibMeans[,2]), 1 ),
      type = 'l', col = 1, lwd = 1 )
  # grid lines
  abline( h = axTicks(2), col = rgb(0,0,0,0.2) )
  abline( v = axTicks(1), col = rgb(0,0,0,0.2) )
  abline( h = 0 )
  # results of individual tests
  points( x = ccm$CCM1_PredictStat$LibSize,
          y = ccm$CCM1_PredictStat$rho, 
          pch = 16, col = rgb(1,0,0,0.1)
          )
  # Redraw mean prediction skill curve
  lines( x = ccm$LibMeans$LibSize,
         y = ccm$LibMeans[,2],
         lwd = 3, col = 1 )

png("gchlxgsrainfall.png", width = 6, height = 4, units = "in", res = 300)
