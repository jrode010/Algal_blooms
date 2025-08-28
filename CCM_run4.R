# Ecosystem Dynamics and Causality
# Rodemann - algal blooms CCM
# Load libraries
library(rEDM)
library(tidyverse)
library(future.apply)
library(rlang)

# Load data
dat <- read.csv("SSA_run4_dates.csv")
datsat <- read.csv('SSA_sat_dates.csv')
datwind <- read.csv('SSA_wind_dates.csv')
colnames( dat )

datsat <- datsat %>% dplyr::select(-date)
datwind <- datwind %>% dplyr::select(-date)

dat <- cbind(dat, datsat)
dat <- cbind(dat, datwind)

names(dat)

# Select variables for CCM test
y <- "mean_area"  # effect
x <- "gsmeanstage"  # cause
df1 <- dat[,c("date",x,y)] |> na.omit()
df1$date <- df1$date |> ymd() # format dates
df1[,c(2,3)] <- apply( df1[,c(2,3)], 2, scale )  # scale signals to mean=0, sd=1
dim( df1 )

# Run CCM and plot results
ccm <- CCM( dataFrame = df1,
            E = 3,   # embedding dimension
            tau = -4,   # embedding delay
            exclusionRadius = 5,   # Theiler window
            target = x,   # prediction target (cause)
            columns = y,   # library (effect) 
            libSizes = "6 84 6",  # string for sequence 'from, to, by'
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
png("E:/FIU/PostDoc/FB_sediment_algal_blooms/Project/Data/Figures_EDM/area_gsmeanstage.png", width = 800, height = 600, res = 100)
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

  
  dev.off()
  
  #Loops for area
  loopccmlaglead <- function(dat, x, y, z, e, t, er){ # x = cause (string), y = effect (string)
    
    df1 <- dat %>%
      dplyr::select(date, !!sym(x), !!sym(y)) %>%
      dplyr::rename(xvar = !!sym(x), yvar = !!sym(y))  # consistent column names for lag and ccm
    
    df1$date <- ymd(df1$date)
    df1[, c("xvar", "yvar")] <- scale(df1[, c("xvar", "yvar")])
    
    df <- data.frame('laglead' = NA, 'pred' = NA, 'sdpred' = NA)
    
    for(i in 0:z){
      df2 <- df1 %>%
        mutate(ll = lag(xvar, i)) %>%
        dplyr::select(date, yvar, ll) %>%
        drop_na()
      libsize_str <- paste("6", nrow(df2)-12, "6")
      ccm <- CCM(dataFrame = df2,
                 E = e, # embedding dimension
                 tau = -t, # embedding delay
                 exclusionRadius = er,  # Theiler window
                 target = "ll",       
                 libSizes = libsize_str,
                 columns = 'yvar',
                 sample = 100,
                 showPlot = FALSE,
                 parameterList = TRUE,
                 includeData = TRUE)
      vals <- ccm$LibMeans[ccm$LibMeans[,1] >= 60, 2]
      df_ccm <- data.frame('laglead' = -i,
                           'pred' = ccm$LibMeans[nrow(ccm$LibMeans),2],
                           'sdpred' = sd(vals))
      
      df <- rbind(df, df_ccm)
    }
    for(i in 0:z){
      df2 <- df1 %>%
        mutate(ll = lead(xvar, i)) %>%
        dplyr::select(date, yvar, ll) %>%
        drop_na()
      libsize_str <- paste("6", nrow(df2)-12, "6")
      ccm <- CCM(dataFrame = df2,
                 E = e, # embedding dimension
                 tau = -t, # embedding delay
                 exclusionRadius = er,  # Theiler window
                 target = "ll",       
                 libSizes = libsize_str,
                 columns = 'yvar',
                 sample = 100,
                 showPlot = FALSE,
                 parameterList = TRUE,
                 includeData = TRUE)
      vals <- ccm$LibMeans[ccm$LibMeans[,1] >= 60, 2]
      df_ccm <- data.frame('laglead' = i,
                           'pred' = ccm$LibMeans[nrow(ccm$LibMeans),2],
                           'sdpred' = sd(vals))
      
      df <- rbind(df, df_ccm)
    }
    df <- df %>% distinct(laglead, .keep_all = T) %>% drop_na()
    return(df)
  }
  

  #Function works! Let's run some CCMs - area first. e = 3, t = 4, er = 5
  actparea <- loopccmlaglead(dat, 'actp', 'mean_area', 12,3,4,5)
  speedarea <- loopccmlaglead(dat, 'speed', 'mean_area', 12,3,4,5)
  dirarea <- loopccmlaglead(dat, 'dir', 'mean_area', 12,3,4,5)
  aflowarea <- loopccmlaglead(dat, 'aflow', 'mean_area', 12,3,4,5)
  mflowarea <- loopccmlaglead(dat, 'mflow', 'mean_area', 12,3,4,5)
  mcdocarea <- loopccmlaglead(dat, 'mcdoc', 'mean_area', 12,3,4,5)
  acdocarea <- loopccmlaglead(dat, 'acdoc', 'mean_area', 12,3,4,5)
  amaxstagearea <- loopccmlaglead(dat, 'amaxstage', 'mean_area', 12,3,4,5)
  mmaxstagearea <- loopccmlaglead(dat, 'mmaxstage', 'mean_area', 12,3,4,5)
  rchlarea <- loopccmlaglead(dat, 'rchl', 'mean_area', 12,3,4,5)
  rTNarea <- loopccmlaglead(dat, 'rTN', 'mean_area', 12,3,4,5)
  marshmeanrainarea <- loopccmlaglead(dat, 'marshmeanrain', 'mean_area', 12,3,4,5)
  gsmeanstagearea <- loopccmlaglead(dat, 'gsmeanstage', 'mean_area', 12,3,4,5)
  mctnarea <- loopccmlaglead(dat, 'mctn', 'mean_area', 12,3,4,5)
  actnarea <- loopccmlaglead(dat, 'actn', 'mean_area', 12,3,4,5)
  gTParea <- loopccmlaglead(dat, 'gTP', 'mean_area', 12,3,4,5)
  gTNarea <- loopccmlaglead(dat, 'gTN', 'mean_area', 12,3,4,5)
  rTParea <- loopccmlaglead(dat, 'rTP', 'mean_area', 12,3,4,5)
  northingarea <- loopccmlaglead(dat, 'northing', 'mean_area', 12,3,4,5)
  gchlarea <- loopccmlaglead(dat, 'gchl', 'mean_area', 12,3,4,5)
  gsalarea <- loopccmlaglead(dat, 'gsal', 'mean_area', 12,3,4,5)
  rsalarea <- loopccmlaglead(dat, 'rsal', 'mean_area', 12,3,4,5)
  gTOCarea <- loopccmlaglead(dat, 'gTOC', 'mean_area', 12,3,4,5)
  rTOCarea <- loopccmlaglead(dat, 'rTOC', 'mean_area', 12,3,4,5)
  rpHarea <- loopccmlaglead(dat, 'rpH', 'mean_area', 12,3,4,5)
  gpHarea <- loopccmlaglead(dat, 'gpH', 'mean_area', 12,3,4,5)
  gsrainfallarea <- loopccmlaglead(dat, 'gsrainfall', 'mean_area', 12,3,4,5)
  acnh4area <- loopccmlaglead(dat, 'acnh4', 'mean_area', 12,3,4,5)
  
#Northing. e = 3, t = 2, er = 4
  actpnorthing <- loopccmlaglead(dat, 'actp', 'northing', 12,3,2,4)
  speednorthing <- loopccmlaglead(dat, 'speed', 'northing', 12,3,2,4)
  dirnorthing <- loopccmlaglead(dat, 'dir', 'northing', 12,3,2,4)
  aflownorthing <- loopccmlaglead(dat, 'aflow', 'northing', 12,3,2,4)
  mflownorthing <- loopccmlaglead(dat, 'mflow', 'northing', 12,3,2,4)
  mcdocnorthing <- loopccmlaglead(dat, 'mcdoc', 'northing', 12,3,2,4)
  acdocnorthing <- loopccmlaglead(dat, 'acdoc', 'northing', 12,3,2,4)
  amaxstagenorthing <- loopccmlaglead(dat, 'amaxstage', 'northing', 12,3,2,4)
  mmaxstagenorthing <- loopccmlaglead(dat, 'mmaxstage', 'northing', 12,3,2,4)
  rchlnorthing <- loopccmlaglead(dat, 'rchl', 'northing', 12,3,2,4)
  rTNnorthing <- loopccmlaglead(dat, 'rTN', 'northing', 12,3,2,4)
  marshmeanrainnorthing <- loopccmlaglead(dat, 'marshmeanrain', 'northing', 12,3,2,4)
  gsmeanstagenorthing <- loopccmlaglead(dat, 'gsmeanstage', 'northing', 12,3,2,4)
  areanorthing <- loopccmlaglead(dat, 'mean_area', 'northing', 12,3,2,4)
  mctnnorthing <- loopccmlaglead(dat, 'mctn', 'northing', 12,3,2,4)
  actnnorthing<- loopccmlaglead(dat, 'actn', 'northing', 12,3,2,4)
  gTPnorthing <- loopccmlaglead(dat, 'gTP', 'northing', 12,3,2,4)
  gTNnorthing <- loopccmlaglead(dat, 'gTN', 'northing', 12,3,2,4)
  rTPnorthing <- loopccmlaglead(dat, 'rTP', 'northing', 12,3,2,4)
  gTOCnorthing <- loopccmlaglead(dat, 'gTOC', 'northing', 12,3,2,4)
  rsalnorthing <- loopccmlaglead(dat, 'rsal', 'northing', 12,3,2,4)
  rTOCnorthing <- loopccmlaglead(dat, 'rTOC', 'northing', 12,3,2,4)

  #Loops for rchl: e = 3, t = 3, er = 2
  actprchl <- loopccmlaglead(dat, 'actp', 'rchl', 12,3,3,2)
  speedrchl <- loopccmlaglead(dat, 'speed', 'rchl', 12,3,3,2)
  dirrchl <- loopccmlaglead(dat, 'dir', 'rchl', 12,3,3,2)
  aflowrchl <- loopccmlaglead(dat, 'aflow', 'rchl', 12,3,3,2)
  mflowrchl <- loopccmlaglead(dat, 'mflow', 'rchl', 12,3,3,2)
  mcdocrchl <- loopccmlaglead(dat, 'mcdoc', 'rchl', 12,3,3,2)
  acdocrchl <- loopccmlaglead(dat, 'acdoc', 'rchl', 12,3,3,2)
  amaxstagerchl <- loopccmlaglead(dat, 'amaxstage', 'rchl', 12,3,3,2)
  mmaxstagerchl <- loopccmlaglead(dat, 'mmaxstage', 'rchl', 12,3,3,2)
  northingrchl <- loopccmlaglead(dat, 'northing', 'rchl', 12,3,3,2)
  rTNrchl <- loopccmlaglead(dat, 'rTN', 'rchl', 12,3,3,2)
  marshmeanrainrchl <- loopccmlaglead(dat, 'marshmeanrain', 'rchl', 12,3,3,2)
  gsmeanstagerchl <- loopccmlaglead(dat, 'gsmeanstage', 'rchl', 12,3,3,2)
  arearchl <- loopccmlaglead(dat, 'mean_area', 'rchl', 12,3,3,2)
  mctnrchl <- loopccmlaglead(dat, 'mctn', 'rchl', 12,3,3,2)
  actnrchl<- loopccmlaglead(dat, 'actn', 'rchl', 12,3,3,2)
  gTPrchl <- loopccmlaglead(dat, 'gTP', 'rchl', 12,3,3,2)
  gTNrchl <- loopccmlaglead(dat, 'gTN', 'rchl', 12,3,3,2)
  rTPrchl <- loopccmlaglead(dat, 'rTP', 'rchl', 12,3,3,2)
  gchlrchl <- loopccmlaglead(dat, 'gchl', 'rchl', 12,3,3,2)
  gsalrchl <- loopccmlaglead(dat, 'gsal', 'rchl', 12,3,3,2)
  rsalrchl <- loopccmlaglead(dat, 'rsal', 'rchl', 12,3,3,2)
  gTOCrchl <- loopccmlaglead(dat, 'gTOC', 'rchl', 12,3,3,2)
  rTOCrchl <- loopccmlaglead(dat, 'rTOC', 'rchl', 12,3,3,2)
  rpHrchl <- loopccmlaglead(dat, 'rpH', 'rchl', 12,3,3,2)
  gpHrchl <- loopccmlaglead(dat, 'gpH', 'rchl', 12,3,3,2)
  gsrainfallrchl <- loopccmlaglead(dat, 'gsrainfall', 'rchl', 12,3,3,2)
  acnh4rchl <- loopccmlaglead(dat, 'acnh4', 'rchl', 12,3,3,2)
 
  #Loops for gchl: e = 4, t = 3, er = 4
  actpgchl <- loopccmlaglead(dat, 'actp', 'gchl', 12,4,3,4)
  speedgchl <- loopccmlaglead(dat, 'speed', 'gchl', 12,4,3,4)
  dirgchl <- loopccmlaglead(dat, 'dir', 'gchl', 12,4,3,4)
  aflowgchl <- loopccmlaglead(dat, 'aflow', 'gchl', 12,4,3,4)
  mflowgchl <- loopccmlaglead(dat, 'mflow', 'gchl', 12,4,3,4)
  mcdocgchl <- loopccmlaglead(dat, 'mcdoc', 'gchl', 12,4,3,4)
  acdocgchl <- loopccmlaglead(dat, 'acdoc', 'gchl', 12,4,3,4)
  amaxstagegchl <- loopccmlaglead(dat, 'amaxstage', 'gchl', 12,4,3,4)
  mmaxstagegchl <- loopccmlaglead(dat, 'mmaxstage', 'gchl', 12,4,3,4)
  northinggchl <- loopccmlaglead(dat, 'northing', 'gchl', 12,4,3,4)
  rTNgchl <- loopccmlaglead(dat, 'rTN', 'gchl', 12,4,3,4)
  marshmeanraingchl <- loopccmlaglead(dat, 'marshmeanrain', 'gchl', 12,4,3,4)
  gsmeanstagegchl <- loopccmlaglead(dat, 'gsmeanstage', 'gchl', 12,4,3,4)
  areagchl <- loopccmlaglead(dat, 'mean_area', 'gchl', 12,4,3,4)
  mctngchl <- loopccmlaglead(dat, 'mctn', 'gchl', 12,4,3,4)
  actngchl<- loopccmlaglead(dat, 'actn', 'gchl', 12,4,3,4)
  gTPgchl <- loopccmlaglead(dat, 'gTP', 'gchl', 12,4,3,4)
  gTNgchl <- loopccmlaglead(dat, 'gTN', 'gchl', 12,4,3,4)
  rTPgchl <- loopccmlaglead(dat, 'rTP', 'gchl', 12,4,3,4)
  rchlgchl <- loopccmlaglead(dat, 'rchl', 'gchl', 12,4,3,4)
  gsalgchl <- loopccmlaglead(dat, 'gsal', 'gchl', 12,4,3,4)
  rsalgchl <- loopccmlaglead(dat, 'rsal', 'gchl', 12,4,3,4)
  gTOCgchl <- loopccmlaglead(dat, 'gTOC', 'gchl', 12,4,3,4)
  rTOCgchl <- loopccmlaglead(dat, 'rTOC', 'gchl', 12,4,3,4)
  rpHgchl <- loopccmlaglead(dat, 'rpH', 'gchl', 12,4,3,4)
  gpHgchl <- loopccmlaglead(dat, 'gpH', 'gchl', 12,4,3,4)
  gsrainfallgchl <- loopccmlaglead(dat, 'gsrainfall', 'gchl', 12,4,3,4)
  acnh4gchl <- loopccmlaglead(dat, 'acnh4', 'gchl', 12,4,3,4)
 
  
  graphlag <- function(x){
  x1 <- x %>% slice(2:n())
  
  ggplot(x1, aes(x = lag, y = pred)) +
    geom_point(color = "blue", size = 2) +  # Points for pred
    geom_errorbar(aes(ymin = pred - sdpred, ymax = pred + sdpred), width = 0.2, color = "darkgray") +  # SD bars
    geom_line(color = "black", linewidth = 1) +  # Connecting curve
    geom_hline(aes(yintercept = 0))+
    scale_x_continuous(breaks = seq(min(gTPgchl$lag), max(gTPgchl$lag), by = 2))+
    labs(
      x = "Lag (1 month)",
      y = "Prediction skill"
    ) +
    theme(
      axis.title = element_text(face = "bold")
    )+
    theme_classic()}
  
  graphlag(speedarea)
  
  ggsave(filename = "E:/FIU/PostDoc/FB_sediment_algal_blooms/Project/Data/Figures_EDM/area_speed_lag.png")
  
##Surrogate tests for seasonality on calculated effects



##Laglead rho values for seasonality
rho_lagleadfun_parallel <- function(dat, x, y, E, tau, er, NSURR, z){
  df1 <- dat %>%
    dplyr::select(!!sym(x), !!sym(y)) %>%
    dplyr::rename(xvar = !!sym(x), yvar = !!sym(y))  # consistent column names for lag and ccm
  
  df1[, c("xvar", "yvar")] <- scale(df1[, c("xvar", "yvar")])
  
  df_pval <- data.frame('laglead' = NA, 'p_value' = NA)
  
  
  for(j in 0:z){
    df_sur <- data.frame('pred' = NA)
    df2 <- df1 %>%
      mutate(ll = lag(xvar, j)) %>%
      dplyr::select(yvar, ll) %>%
      drop_na()
  surdat <-  as.data.frame(rEDM::SurrogateData(df2[,1], method = 'seasonal', T_period = 12, num_surr = NSURR))
  surdat1 <- as.data.frame(cbind(as.data.frame(df2[,2]), surdat))
  colnames(surdat1)[1] <- 'cause'
  libsize_str <- paste("6", nrow(df2)-12, "6")
  idx_cols <- 2:ncol(surdat1)                         # columns 2..n are libraries
  target_col <- colnames(surdat1)[1]                  # e.g., "cause"
  
  rho_surr <- future.apply::future_sapply(
    idx_cols,
    FUN = function(i_col) {
      lib_col <- colnames(surdat1)[i_col]
      
      ccm_s <- rEDM::CCM(
        dataFrame       = surdat1,
        E               = E,
        tau             = -tau,
        exclusionRadius = er,
        target          = colnames(surdat1)[1],                    
        columns         = lib_col,        
        libSizes        = libsize_str,                   
        sample          = 100,
        random          = TRUE,
        noTime          = TRUE,
        includeData     = TRUE,
        parameterList   = TRUE
      )
      
      return(ccm_s$LibMeans[nrow(ccm_s$LibMeans),2])
    }        # independent RNG per worker (reproducible)
    # you can also set: future.scheduling = 1 to split work evenly across workers
  )
  

   ccm_real <- CCM( dataFrame = df2,
                   E = E,   # embedding dimension
                   tau = -tau,   # embedding delay
                   exclusionRadius = er,   # Theiler window
                   target = colnames(df2)[2],   # prediction target (cause)
                   columns = colnames(df2)[1],   # library (effect) 
                   libSizes = libsize_str,  # string for sequence 'from, to, by'
                   sample = 100,   # number of replicate tests at each libSize
                   parameterList = TRUE,
                   includeData = TRUE,
                   noTime = TRUE
  )
  
  true_rho <- ccm_real$LibMeans[nrow(ccm_real$LibMeans),2]
  # Compute p-value
  k <- which(rho_surr >= true_rho ) %>% length()  # number of surrogate rho values exceeding the library's rho
  dp <- data.frame('laglead' = -j, 'p_value' = (k+1)/(NSURR+1))  # p-value: (k+1)/(n+1) where k is number of 'successes' and NSURR is total number of surrogates
  df_pval <- rbind(df_pval, dp)
  } 
  
  for(j in 0:z){
    df_sur <- data.frame('pred' = NA)
    df2 <- df1 %>%
      mutate(ll = lead(xvar, j)) %>%
      dplyr::select(yvar, ll) %>%
      drop_na()
    surdat <-  as.data.frame(rEDM::SurrogateData(df2[,1], method = 'seasonal', T_period = 12, num_surr = NSURR))
    surdat1 <- as.data.frame(cbind(as.data.frame(df2[,2]), surdat))
    colnames(surdat1)[1] <- 'cause'
    libsize_str <- paste("6", nrow(df2)-12, "6")
    idx_cols <- 2:ncol(surdat1)                         # columns 2..n are libraries
    target_col <- colnames(surdat1)[1]                  # e.g., "cause"
    
    rho_surr <- future.apply::future_sapply(
      idx_cols,
      FUN = function(i_col) {
        lib_col <- colnames(surdat1)[i_col]
        
        ccm_s <- rEDM::CCM(
          dataFrame       = surdat1,
          E               = E,
          tau             = -tau,
          exclusionRadius = er,
          target          = colnames(surdat1)[1],                    
          columns         = lib_col,        
          libSizes        = libsize_str,                   
          sample          = 100,
          random          = TRUE,
          noTime          = TRUE,
          includeData     = TRUE,
          parameterList   = TRUE
        )
        
        return(ccm_s$LibMeans[nrow(ccm_s$LibMeans),2])
      }        # independent RNG per worker (reproducible)
      # you can also set: future.scheduling = 1 to split work evenly across workers
    )
    
    ccm_real <- CCM( dataFrame = df2,
                     E = E,   # embedding dimension
                     tau = -tau,   # embedding delay
                     exclusionRadius = er,   # Theiler window
                     target = colnames(df2)[2],   # prediction target (cause)
                     columns = colnames(df2)[1],   # library (effect) 
                     libSizes = libsize_str,  # string for sequence 'from, to, by'
                     sample = 100,   # number of replicate tests at each libSize
                     parameterList = TRUE,
                     includeData = TRUE,
                     noTime = TRUE
    )
    true_rho <- ccm_real$LibMeans[nrow(ccm_real$LibMeans),2]
    # Compute p-value
    k <- which(rho_surr >= true_rho ) %>% length()  # number of surrogate rho values exceeding the library's rho
    dp <- data.frame('laglead' = j, 'p_value' = (k+1)/(NSURR+1))  # p-value: (k+1)/(n+1) where k is number of 'successes' and NSURR is total number of surrogates
    df_pval <- rbind(df_pval, dp)
 
  } 
  df_pval <- df_pval %>% distinct(laglead, .keep_all = T) %>% drop_na()
  return(df_pval) }

#Run these in parallel!!! Got it working! Let's start with 100 surrogates for all of the significant ccms
future::plan(multisession, workers = max(1, parallel::detectCores() - 1))
future::plan(sequential)
#rchl
rhorchlrTN <- rho_lagleadfun_parallel(dat, 'rTN', 'rchl', 3,3,2,100,12)
rhorchlrTOC <- rho_lagleadfun_parallel(dat, 'rTOC', 'rchl', 3,3,2,100,12)
rhorchlrTP <- rho_lagleadfun_parallel(dat, 'rTP', 'rchl', 3,3,2,100,12)
rhorchlgchl <- rho_lagleadfun_parallel(dat, 'gchl', 'rchl', 3,3,2,100,12)
rhorchlgTN <- rho_lagleadfun_parallel(dat, 'gTN', 'rchl', 3,3,2,100,12)
rhorchlgTOC <- rho_lagleadfun_parallel(dat, 'gTOC', 'rchl', 3,3,2,100,12)
rhorchlgTP <- rho_lagleadfun_parallel(dat, 'gTP', 'rchl', 3,3,2,100,12)
rhorchlgsal <- rho_lagleadfun_parallel(dat, 'gsal', 'rchl', 3,3,2,100,12)
rhorchlarea <- rho_lagleadfun_parallel(dat, 'mean_area', 'rchl', 3,3,2,100,12)
rhorchlgchl <- rho_lagleadfun_parallel(dat, 'gchl', 'rchl', 3,3,2,100,12)
#gchl
rhogchlaflow <- rho_lagleadfun_parallel(dat, 'aflow', 'gchl', 4,3,4,100,12)
rhogchlarea <- rho_lagleadfun_parallel(dat, 'mean_area', 'gchl', 4,3,4,100,12)
rhogchlgpH <- rho_lagleadfun_parallel(dat, 'gpH', 'gchl', 4,3,4,100,12)
rhogchlgsal <- rho_lagleadfun_parallel(dat, 'gsal', 'gchl', 4,3,4,100,12)
rhogchlgTN <- rho_lagleadfun_parallel(dat, 'gTN', 'gchl', 4,3,4,100,12)
rhogchlgTOC <- rho_lagleadfun_parallel(dat, 'gTOC', 'gchl', 4,3,4,100,12)
rhogchlgTP <- rho_lagleadfun_parallel(dat, 'gTP', 'gchl', 4,3,4,100,12)
rhogchlmctn <- rho_lagleadfun_parallel(dat, 'mctn', 'gchl', 4,3,4,100,12)
rhogchlnorthing <- rho_lagleadfun_parallel(dat, 'northing', 'gchl', 4,3,4,100,12)
rhogchlrsal <- rho_lagleadfun_parallel(dat, 'rsal', 'gchl', 4,3,4,100,12)
rhogchlrchl <- rho_lagleadfun_parallel(dat, 'rchl', 'gchl', 4,3,4,100,12)
#northing
rhonorthinggTOC <- rho_lagleadfun_parallel(dat, 'gTOC', 'northing', 3,2,4,100,12)
rhonorthinggTP <- rho_lagleadfun_parallel(dat, 'gTP', 'northing', 3,2,4,100,12)
rhonorthinggTN <- rho_lagleadfun_parallel(dat, 'gTN', 'northing', 3,2,4,100,12)
rhonorthingacdoc <- rho_lagleadfun_parallel(dat, 'acdoc', 'northing', 3,2,4,100,12)
rhonorthingactn <- rho_lagleadfun_parallel(dat, 'actn', 'northing', 3,2,4,100,12)
rhonorthingrsal <- rho_lagleadfun_parallel(dat, 'rsal', 'northing', 3,2,4,100,12)
#area
rhoareaaflow <- rho_lagleadfun_parallel(dat, 'aflow', 'mean_area', 3,4,5,100,12)
rhoareaamaxstage <- rho_lagleadfun_parallel(dat, 'amaxstage', 'mean_area', 3,4,5,100,12)
rhoareagsmeanstage <- rho_lagleadfun_parallel(dat, 'gsmeanstage', 'mean_area', 3,4,5,100,12)
rhoarearchl <- rho_lagleadfun_parallel(dat, 'rchl', 'mean_area', 3,4,5,100,12)
rhoarearTN <- rho_lagleadfun_parallel(dat, 'rTN', 'mean_area', 3,4,5,100,12)
rhoareaacdoc <- rho_lagleadfun_parallel(dat, 'acdoc', 'mean_area', 3,4,5,100,12)
rhoareadir <- rho_lagleadfun_parallel(dat, 'dir', 'mean_area', 3,4,5,100,12)
rhoareagpH <- rho_lagleadfun_parallel(dat, 'gpH', 'mean_area', 3,4,5,100,12)
rhoareagTOC <- rho_lagleadfun_parallel(dat, 'gTOC', 'mean_area', 3,4,5,100,12)
rhoareamarshmeanrain <- rho_lagleadfun_parallel(dat, 'marshmeanrain', 'mean_area', 3,4,5,100,12)
rhoareamcdoc <- rho_lagleadfun_parallel(dat, 'mcdoc', 'mean_area', 3,4,5,100,12)
rhoareamflow <- rho_lagleadfun_parallel(dat, 'mflow', 'mean_area', 3,4,5,100,12)
rhoareammaxstage <- rho_lagleadfun_parallel(dat, 'mmaxstage', 'mean_area', 3,4,5,100,12)
rhoarearTOC <- rho_lagleadfun_parallel(dat, 'rTOC', 'mean_area', 3,4,5,100,12)
rhoarearTP <- rho_lagleadfun_parallel(dat, 'rTP', 'mean_area', 3,4,5,100,12)
rhoareaspeed <- rho_lagleadfun_parallel(dat, 'speed', 'mean_area', 3,4,5,100,12)
rhoarearpH <- rho_lagleadfun_parallel(dat, 'rpH', 'mean_area', 3,4,5,100,12)

future::plan(sequential)




# x: data with cols laglead, pred, sdpred
# p_df: data with cols laglead and p (p-values)
graphlag <- function(x, p_df = NULL) {
  #Title from the symbol name used for `x`
  plot_title <- deparse(substitute(x))
  
  #Merge in p-values and make significance labels (if provided)
  dat <- x
  if (!is.null(p_df)) {
    dat <- dat %>%
      left_join(p_df, by = "laglead") %>%
      mutate(sig_lbl = case_when(
        !is.na(p_value) & p_value <= 0.05  ~ "*",
        TRUE                   ~ ""
      ))
  } else {
    dat <- dat %>% mutate(sig_lbl = "")
  }
  dat <- dat %>% filter(laglead <= 2)
  # vertical offset so the asterisks clear the error bars
  y_pad <- diff(range(dat$pred, na.rm = TRUE)) * 0.04
  dat <- dat %>% mutate(y_aster = pred + sdpred + y_pad)
  
  ggplot(dat, aes(x = laglead, y = pred)) +
    geom_point(color = "blue", size = 1.5) +
    geom_errorbar(aes(ymin = pred - sdpred, ymax = pred + sdpred),
                  width = 0.5, color = "darkgray") +
    geom_line(color = "black", linewidth = 1) +
    geom_hline(yintercept = 0) +
    # asterisks (only where non-empty)
    geom_text(aes(y = y_aster, label = sig_lbl),
              vjust = -0.2, size = 4, na.rm = TRUE) +
    scale_x_continuous(breaks = seq(-12, 12, by = 2)) +
    labs(
      title = plot_title,
      x = "Lag (1 month)",
      y = "Prediction skill"
    ) +
    coord_cartesian(clip = "off") + # avoid clipping asterisks at top
    theme_classic() +
    theme(
      axis.title = element_text(face = "bold"),
      plot.margin = margin(10, 20, 10, 10)
    )
}

gacdocarea <- graphlag(acdocarea, rhoareaacdoc) #yes
gacdocarea
ggtocarea <- graphlag(gTOCarea, rhoareagTOC) #yes
ggtocarea
gmcdocarea <- graphlag(mcdocarea, rhoareamcdoc) #yes
gmcdocarea
gmmaxstagearea <- graphlag(mmaxstagearea, rhoareammaxstage) #yes
gmmaxstagearea
gamaxstagearea <- graphlag(amaxstagearea, rhoareaamaxstage) #yes
gamaxstagearea
ggsmeanstagearea <- graphlag(gsmeanstagearea, rhoareagsmeanstage) #yesish
ggsmeanstagearea
grchlarea <- graphlag(rchlarea, rhoarearchl) #yes
grchlarea
grtnarea <- graphlag(rTNarea, rhoarearTN) #yes
grtnarea
grtocarea <- graphlag(rTOCarea, rhoarearTOC) #yes
grtocarea
grtparea <- graphlag(rTParea, rhoarearTP) #yes
grtparea
gareagchl <- graphlag(areagchl, rhogchlarea) #yes
gareagchl
ggphgchl <- graphlag(gpHgchl, rhogchlgpH) #yes
ggphgchl
ggsalgchl <- graphlag(gsalgchl, rhogchlgsal) #ish
ggsalgchl
ggtngchl <- graphlag(gTNgchl, rhogchlgTN) #yes
ggtngchl
ggtocgchl <- graphlag(gTOCgchl, rhogchlgTOC) #yes
ggtocgchl
ggtpgchl <- graphlag(gTPgchl, rhogchlgTP) #yes
ggtpgchl
grsalgchl <- graphlag(rsalgchl, rhogchlrsal) #no
grsalgchl
gacdocnorthing <- graphlag(acdocnorthing, rhonorthingacdoc) #yes
gacdocnorthing
gactnnorthing <- graphlag(actnnorthing, rhonorthingactn) #no
gactnnorthing
grsalnorthing <- graphlag(rsalnorthing, rhonorthingrsal) #yes
grsalnorthing
garearchl <- graphlag(arearchl, rhorchlarea) #yes
garearchl
ggsalrchl <- graphlag(gsalrchl, rhorchlgsal) #no
ggsalrchl
ggtnrchl <- graphlag(gTNrchl, rhorchlgTN) #yes
ggtnrchl
ggtocrchl <- graphlag(gTOCrchl, rhorchlgTOC) #yes
ggtocrchl
ggtprchl <- graphlag(gTPrchl, rhorchlgTP) #yes
ggtprchl
ggchlrchl <- graphlag(gchlrchl, rhorchlgchl) #yes
ggchlrchl
grtnrchl <- graphlag(rTNrchl, rhorchlrTN) #yes
grtnrchl
grtocrchl <- graphlag(rTOCrchl, rhorchlrTOC) #yes
grtocrchl
grtprchl <- graphlag(rTPrchl, rhorchlrTP) #yes
grtprchl

ggsave(filename = 'plots/acdocarea_lag.png', plot = gacdocarea)
ggsave(filename = 'plots/gtocarea_lag.png', plot = ggtocarea)
ggsave(filename = 'plots/mcdocarea_lag.png', plot = gmcdocarea)
ggsave(filename = 'plots/mmaxstagearea_lag.png', plot = gmmaxstagearea)
ggsave(filename = 'plots/amaxstagearea_lag.png', plot = gamaxstagearea)
ggsave(filename = 'plots/gsmeanstagearea_lag.png', plot = ggsmeanstagearea)
ggsave(filename = 'plots/rchlarea_lag.png', plot = grchlarea)
ggsave(filename = 'plots/rtnarea_lag.png', plot = grtnarea)
ggsave(filename = 'plots/rtocarea_lag.png', plot = grtocarea)
ggsave(filename = 'plots/rtparea_lag.png', plot = grtparea)
ggsave(filename = 'plots/areagchl_lag.png', plot = gareagchl)
ggsave(filename = 'plots/gphgchl_lag.png', plot = ggphgchl)
ggsave(filename = 'plots/gsalgchl_lag.png', plot = ggsalgchl)
ggsave(filename = 'plots/gtngchl_lag.png', plot = ggtngchl)
ggsave(filename = 'plots/gtocgchl_lag.png', plot = ggtocgchl)
ggsave(filename = 'plots/gtpgchl_lag.png', plot = ggtpgchl)
ggsave(filename = 'plots/rsalgchl_lag.png', plot = grsalgchl)
ggsave(filename = 'plots/acdocnorthing_lag.png', plot = gacdocnorthing)
ggsave(filename = 'plots/actnnorthing_lag.png', plot = gactnnorthing)
ggsave(filename = 'plots/rsalnorthing_lag.png', plot = grsalnorthing)
ggsave(filename = 'plots/arearchl_lag.png', plot = garearchl)
ggsave(filename = 'plots/gsalrchl_lag.png', plot = ggsalrchl)
ggsave(filename = 'plots/gtnrchl_lag.png', plot = ggtnrchl)
ggsave(filename = 'plots/gtocrchl_lag.png', plot = ggtocrchl)
ggsave(filename = 'plots/gtprchl_lag.png', plot = ggtprchl)
ggsave(filename = 'plots/gchlrchl_lag.png', plot = ggchlrchl)
ggsave(filename = 'plots/rtnrchl_lag.png', plot = grtnrchl)
ggsave(filename = 'plots/rtocrchl_lag.png', plot = grtocrchl)
ggsave(filename = 'plots/rtprchl_lag.png', plot = grtprchl)


