# Ecosystem Dynamics and Causality
# Rodemann - algal blooms CCM
# Load libraries
library(rEDM)


# Load data
dat <- read.csv("SSA_run3_dates.csv")
colnames( dat )

ggplot(dat, aes(x = ymd(date), y = gchl))+
  geom_line()

# Select variables for CCM test
y <- "gmeanstage"  # effect
x <- "acminstage"  # cause
df1 <- dat[,c("date",x,y)] |> na.omit()
df1$date <- df1$date |> ymd() # format dates
df1[,c(2,3)] <- apply( df1[,c(2,3)], 2, scale )  # scale signals to mean=0, sd=1
dim( df1 )

# Run CCM and plot results
ccm <- CCM( dataFrame = df1,
            E = 5,   # embedding dimension
            tau = -21,   # embedding delay
            exclusionRadius = 29,   # Theiler window
            target = x,   # prediction target (cause)
            columns = y,   # library (effect) 
            libSizes = "21 771 50",  # string for sequence 'from, to, by'
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


  loopccmlag3day <- function(dat, x, y, z){ # x = cause (string), y = effect (string)
    
    df1 <- dat %>%
      dplyr::select(date, !!sym(x), !!sym(y)) %>%
      dplyr::rename(xvar = !!sym(x), yvar = !!sym(y))  # consistent column names for lag and ccm
    
    df1$date <- ymd(df1$date)
    df1[, c("xvar", "yvar")] <- scale(df1[, c("xvar", "yvar")])
    
    df <- data.frame('lag' = NA, 'pred' = NA, 'sdpred' = NA)
    
    for(i in 0:z){
      df2 <- df1 %>%
        mutate(ll = lag(xvar, i)) %>%
        dplyr::select(date, yvar, ll) %>%
        drop_na()
      libsize_str <- paste("10", nrow(df2)-30, "10")
      ccm <- CCM(dataFrame = df2,
                 E = 2, # embedding dimension
                 tau = -15, # embedding delay
                 exclusionRadius = 63,  # Theiler window
                 target = "ll",       
                 libSizes = libsize_str,
                 columns = 'yvar',
                 sample = 100,
                 showPlot = FALSE,
                 parameterList = TRUE,
                 includeData = TRUE)
      
      df_ccm <- data.frame('lag' = i,
                           'pred' = mean(ccm$LibMeans[,2]),
                           'sdpred' = sd(ccm$LibMeans[,2]))
      
      df <- rbind(df, df_ccm)
    }
    
    return(df)
  }

  actn3gchl <- loopccmlag3day(dat, 'actn', 'gchl', 30)
  actp3gchl <- loopccmlag3day(dat, 'actp', 'gchl', 30)
  gstage3gchl <- loopccmlag3day(dat, 'gmeanstage', 'gchl', 30)  
  acflow3gchl <- loopccmlag3day(dat, 'acflow', 'gchl', 30)  
  acmaxstage3gchl <- loopccmlag3day(dat, 'acmaxstage', 'gchl', 30)  
  