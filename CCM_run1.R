# Ecosystem Dynamics and Causality
# Rodemann - algal blooms CCM
# Load libraries
library(rEDM)


# Load data
data <- read.csv("SSA_run1_dates.csv")
colnames( dat )

# Select variables for CCM test
y <- "gchl"  # effect
x <- "gsrainfall"  # cause
df1 <- dat[,c("date",x,y)] |> na.omit()
df1$date <- df1$date |> ymd() # format dates
df1[,c(2,3)] <- apply( df1[,c(2,3)], 2, scale )  # scale signals to mean=0, sd=1
dim( df1 )
df1 <- df1 %>% mutate(rain2 = lag(gsrainfall, 0))
df <- data.frame('lag' = NA, 'pred' = NA, 'sdpred' = NA)

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

##Let's try to run a loop to get lagged effects:
loopccmlag <- function(dat, x, y, z){ #x is cause, y is effect
  df1 <- dat %>% dplyr::select(date, {{x}}, {{y}})
  df1$date <- df1$date |> ymd() # format dates
  df1[,c(2,3)] <- apply( df1[,c(2,3)], 2, scale )  # scale signals to mean=0, sd=1
  df <- data.frame('lag' = NA, 'pred' = NA, 'sdpred' = NA)
  for(i in 0:z){
    df2 <- df1 %>% mutate(ll = lag({{x}}, i)) %>% dplyr::select(date, {{y}}, ll) %>% drop_na()
    ccm <- CCM( dataFrame = df2,
                E = 2,   # embedding dimension
                tau = -4,   # embedding delay
                exclusionRadius = 17,   # Theiler window
                target = {{x}},   # prediction target (cause)
                #columns = ll,   # library (effect) 
                libSizes = "10 140 5",  # string for sequence 'from, to, by'
                sample = 100,   # number of replicate tests at each libSize
                showPlot = F,
                parameterList = TRUE,
                includeData = TRUE
    )
    df_ccm <- data.frame('lag' = i, 'pred' = mean(ccm$LibMeans[,2]), 'sdpred' = sd(ccm$LibMeans[,2]))
    df <- rbind(df, df_ccm)
  }
  return(df)
}

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
    libsize_str <- paste("10", nrow(df2)-4, "5")
    ccm <- CCM(dataFrame = df2,
               E = 2,
               tau = -4,
               exclusionRadius = 17,
               target = "ll",       # fixed name
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
test <- loopccmlag(data, 'gchl', 'gsrainfall', 6)


