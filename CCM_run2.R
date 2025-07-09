# Ecosystem Dynamics and Causality
# Rodemann - algal blooms CCM
# Load libraries
library(rEDM)


# Load data
data2 <- read.csv("SSA_run2_dates.csv")
colnames( dat )

# Select variables for CCM test
y <- "gsmeanstage"  # effect
x <- "actn"  # cause
df1 <- dat[,c("date",x,y)] |> na.omit()
df1$date <- df1$date |> ymd() # format dates
df1[,c(2,3)] <- apply( df1[,c(2,3)], 2, scale )  # scale signals to mean=0, sd=1
dim( df1 )

# Run CCM and plot results
ccm <- CCM( dataFrame = df1,
            E = 2,   # embedding dimension
            tau = -2,   # embedding delay
            exclusionRadius = 3,   # Theiler window
            target = x,   # prediction target (cause)
            columns = y,   # library (effect) 
            libSizes = "4 94 5",  # string for sequence 'from, to, by'
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

  ##Let's try to run a loop to get lagged effects:
 
  
  loopccmlag <- function(dat, x, y, z){ # x = cause (string), y = effect (string)
    
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
      libsize_str <- paste("5", nrow(df2)-10, "5")
      ccm <- CCM(dataFrame = df2,
                 E = 3, # embedding dimension
                 tau = -3, # embedding delay
                 exclusionRadius = 9,  # Theiler window
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
  
  #Function works! Let's run some CCMs
  actngchl <- loopccmlag(dat, 'actn', 'gchl', 12)
  actpgchl <- loopccmlag(dat, 'actp', 'gchl', 12)
  acnh4gchl <- loopccmlag(dat, 'acnh4', 'gchl', 12)
  aflowgchl <- loopccmlag(dat, 'aflow', 'gchl', 12)
  astagechl <- loopccmlag(dat, 'amaxstage', 'gchl', 12)
  gTOCgchl <- loopccmlag(dat, 'gTOC', 'gchl', 12)
  gTPgchl <- loopccmlag(dat, 'gTP', 'gchl', 12)
  gTNgchl <- loopccmlag(dat, 'gTN', 'gchl', 12)
  gpHgchl <- loopccmlag(dat, 'gpH', 'gchl', 12)
  gsrainfallgchl <- loopccmlag(dat, 'gsrainfall', 'gchl', 12)
  gsstagegchl <- loopccmlag(dat, 'gsmeanstage', 'gchl', 12)
  gstempgchl <- loopccmlag(dat, 'gstemp', 'gchl', 12)
  
  #gTP is highest value. Let's try to predict that
  #Change function for TP
  loopccmlag <- function(dat, x, y, z){ # x = cause (string), y = effect (string)
    
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
      libsize_str <- paste("5", nrow(df2)-15, "5")
      ccm <- CCM(dataFrame = df2,
                 E = 3, # embedding dimension
                 tau = -6, # embedding delay
                 exclusionRadius = 10,  # Theiler window
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
  
  #Function works! Let's run some CCMs
  actngTP <- loopccmlag(data2, 'actn', 'gTP', 12)
  actpgTP <- loopccmlag(data2, 'actp', 'gTP', 12)
  acnh4gTP <- loopccmlag(data2, 'acnh4', 'gTP', 12)
  aflowgTP <- loopccmlag(data2, 'aflow', 'gTP', 12)
  astagechl <- loopccmlag(data2, 'amaxstage', 'gTP', 12)
  gTOCgTP <- loopccmlag(data2, 'gTOC', 'gTP', 12)
  gTNgTP <- loopccmlag(data2, 'gTN', 'gTP', 12)
  gpHgTP <- loopccmlag(data2, 'gpH', 'gTP', 12)
  gsrainfallgTP <- loopccmlag(data2, 'gsrainfall', 'gTP', 12)
  gsstagegTP <- loopccmlag(data2, 'gsmeanstage', 'gTP', 12)
  gstempgTP <- loopccmlag(data2, 'gstemp', 'gTP', 12)
 
  #Try other way around for actp 
  loopccmlag <- function(dat, x, y, z){ # x = cause (string), y = effect (string)
    
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
      libsize_str <- paste("5", nrow(df2)-10, "5")
      ccm <- CCM(dataFrame = df2,
                 E = 2, # embedding dimension
                 tau = -2, # embedding delay
                 exclusionRadius = 3,  # Theiler window
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
  gTPactp <- loopccmlag(data2, 'gTP', 'actp', 12)
  
  #Let's try to predict gTN
  #Change function for TP
  loopccmlag <- function(dat, x, y, z){ # x = cause (string), y = effect (string)
    
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
      libsize_str <- paste("5", nrow(df2)-15, "5")
      ccm <- CCM(dataFrame = df2,
                 E = 3, # embedding dimension
                 tau = -4, # embedding delay
                 exclusionRadius = 7,  # Theiler window
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
  
  #Function works! Let's run some CCMs
  actngTN <- loopccmlag(data2, 'actn', 'gTN', 12)
  actpgTN <- loopccmlag(data2, 'actp', 'gTN', 12)
  acnh4gTN <- loopccmlag(data2, 'acnh4', 'gTN', 12)
  aflowgTN <- loopccmlag(data2, 'aflow', 'gTN', 12)
  astagechl <- loopccmlag(data2, 'amaxstage', 'gTN', 12)
  gTOCgTN <- loopccmlag(data2, 'gTOC', 'gTN', 12)
  gTPgTN <- loopccmlag(data2, 'gTP', 'gTN', 12)
  gpHgTN <- loopccmlag(data2, 'gpH', 'gTN', 12)
  gsrainfallgTN <- loopccmlag(data2, 'gsrainfall', 'gTN', 12)
  gsstagegTN <- loopccmlag(data2, 'gsmeanstage', 'gTN', 12)
  gstempgTN <- loopccmlag(data2, 'gstemp', 'gTN', 12)
  