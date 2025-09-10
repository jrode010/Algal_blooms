# Ecosystem Dynamics and Causality
# Phase space reconstruction
# Rodemann, algal blooms
#


library(tidyverse)
library(tseriesChaos)
library(zoo)
library(plotly)

# Load data and select column
dat <- read.csv("SSA_run4.csv")
dat2 <- read.csv('SSA_flowcum.csv')
dat <- cbind(dat, dat2)
write.csv(dat, file = 'SSA_run4_wcumflow.csv') 

colnames( dat )
  var <- 'mflow_cum'
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
  ami <- mutual( x, lag.max = 30 )  # average mutual information function
  local.d.min <- rollapply( ami, 3, function (x) which.min(x)==2 )  # local minima
  d <- as.numeric( which(local.d.min==TRUE)[1] )  # first local min
  points( x = d, y = ami[d+1], cex = 2, col = rgb(1,0,0,0.7) )
  d

  # Set Theiler window (tw) using ACF
  ac <- acf( x, lag.max = 75 )  # autocorrelation function
  ac$acf_abs <- ac$acf |> abs()  # absolute values of acf
  local.tw.min <- rollapply(ac$acf_abs, 3, function (x) which.min(x)==2 )  # abs local minima
  tw <- as.numeric( which(local.tw.min==TRUE)[1] )  # first abs local min
  points( x = tw, y = ac$acf[tw+1], cex = 2, col = rgb(1,0,0,0.7) )
  tw
 # tw = 4
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
  stp <- stplot( series = x, m = 2, d = d, mdt = length(x) )

  #stationarity using NCP
  
library(infotheo)
library(nonlinearTseries)  
  library(tseriesChaos)
  
  # ------------------------------------------------------------
  #Helpers (no external packages required)
  # ------------------------------------------------------------
  estimate_tau_acf <- function(x, max_lag = 24) {
    ac <- acf(x, lag.max = max_lag, plot = FALSE, na.action = na.omit)$acf[-1]
    ecut <- which(ac < exp(-1)); tau_e <- if (length(ecut)) ecut[1] else NA_integer_
    zc   <- which(diff(sign(ac)) != 0); tau_z <- if (length(zc)) zc[1] else NA_integer_
    tau <- suppressWarnings(min(c(tau_e, tau_z), na.rm = TRUE))
    if (!is.finite(tau)) tau <- which.min(ac)
    as.integer(max(1, tau))
  }
  
  estimate_m_variance_gain <- function(x, tau, max_m = 10, gain_tol = 0.05) {
    prev_v <- 0; pick <- 2
    for (m in 2:max_m) {
      idx_end <- length(x) - (m - 1) * tau
      if (idx_end <= 5) break
      idx <- 1:idx_end
      Xv  <- vapply(0:(m - 1), function(k) var(x[idx + k * tau], na.rm = TRUE), 0.0)
      vtot <- sum(Xv); gain <- if (vtot == 0) 0 else (vtot - prev_v) / vtot
      if (m > 2 && abs(gain) < gain_tol) { pick <- m; break }
      prev_v <- vtot; pick <- m
    }
    as.integer(max(2, pick))
  }
  
  ## ---------- Your original nlp (self-prediction), kept for reference ----------
  # Uses neighbors within the same segment; returns corr.
  nlp_self <- function(dat, delay, tw, dim) {
    if (!requireNamespace("tseriesChaos", quietly = TRUE)) stop("install.packages('tseriesChaos')")
    dat <- as.vector(dat)
    Mx  <- tseriesChaos::embedd(x = dat, m = dim, d = delay)
    if (nrow(Mx) < 3) return(list(cor = NA_real_, dat = data.frame(prd = numeric(0), obs = numeric(0))))
    dists <- as.matrix(dist(Mx))
    out <- data.frame(prd = rep(NA_real_, nrow(Mx)), obs = Mx[, 1])
    for (i in 1:(nrow(Mx) - 1)) {
      dis <- data.frame(dis = dists[i, ], idx = seq_len(nrow(Mx)))
      # Theiler window around i
      if (tw > 0) {
        tw.idx <- (i - tw):(i + tw); tw.idx <- tw.idx[tw.idx %in% seq_len(nrow(Mx))]
        dis <- dis[!(dis$idx %in% tw.idx), ]
      }
      # Need neighbors with a valid +1
      dis <- dis[dis$idx < nrow(Mx), , drop = FALSE]
      dis <- dis[order(dis$dis), ]
      k   <- min(dim + 1L, nrow(dis))
      if (k < 1) next
      nbor.idx <- dis$idx[seq_len(k)]; nbor.dis <- dis$dis[seq_len(k)]
      mind <- max(min(nbor.dis), .Machine$double.eps)
      wt   <- exp(-nbor.dis / mind)
      if (any(!is.finite(wt))) wt[!is.finite(wt)] <- 1
      wt <- wt / sum(wt)
      lib <- Mx[nbor.idx + 1L, 1]  # advance one step
      out$prd[i + 1L] <- as.numeric(sum(lib * wt))
    }
    out <- out[-1, , drop = FALSE]
    list(cor = suppressWarnings(cor(out$obs, out$prd)), dat = out)
  }
  
  ## ---------- Cross-segment version for non-stationarity ----------
  # learn_vec predicts test_vec one step ahead via neighbors in learn.
  # tw applies *within the learning set* only (default 0).
  # k = dim+1 neighbors, exp(-d/min_d) weights, h-step ahead
  nlp_cross_h <- function(learn_vec, test_vec, delay, dim, tw = 0L, h = 1L) {
    if (!requireNamespace("tseriesChaos", quietly = TRUE)) stop("install.packages('tseriesChaos')")
    learn_em <- tseriesChaos::embedd(learn_vec, m = dim, d = delay)
    test_em  <- tseriesChaos::embedd(test_vec,  m = dim, d = delay)
    
    nL <- nrow(learn_em); nT <- nrow(test_em)
    if (nL < (h + 1) || nT < (h + 1)) return(list(cor = NA_real_, pred = numeric(0), truth = numeric(0)))
    
    pool_idx <- 1:(nL - h)  # neighbors must have a valid +h step
    
    pred <- numeric(0); truth <- numeric(0)
    for (i in 1:(nT - h)) {
      xref <- test_em[i, , drop = FALSE]
      dvec <- sqrt(rowSums((learn_em[pool_idx, , drop = FALSE] -
                              matrix(xref, nrow = length(pool_idx), ncol = ncol(learn_em), byrow = TRUE))^2))
      dis <- data.frame(dis = dvec, idx = pool_idx)
      
      # Optional Theiler window within learning set (usually unnecessary cross-segment)
      if (tw > 0) {
        # left as no-op by default
      }
      
      dis <- dis[order(dis$dis), , drop = FALSE]
      k   <- min(dim + 1L, nrow(dis))
      if (k < 1) next
      nbor.idx <- dis$idx[seq_len(k)]
      nbor.dis <- dis$dis[seq_len(k)]
      mind <- max(min(nbor.dis), .Machine$double.eps)
      wt   <- exp(-nbor.dis / mind); wt <- wt / sum(wt)
      
      yhat  <- sum(learn_em[nbor.idx + h, 1] * wt)  # advance neighbors by h
      ytrue <- test_em[i + h, 1]
      
      pred  <- c(pred,  yhat)
      truth <- c(truth, ytrue)
    }
    
    list(cor = suppressWarnings(cor(truth, pred)), pred = pred, truth = truth)
  }
  # --- 1) Strict Nash–Sutcliffe Efficiency (NSE) ---
  nse_strict <- function(pred, y_true) {
    stopifnot(length(pred) == length(y_true))
    keep <- is.finite(pred) & is.finite(y_true)
    pred <- pred[keep]; y_true <- y_true[keep]
    if (length(y_true) < 2) return(NA_real_)
    sse <- sum((y_true - pred)^2)
    sst <- sum((y_true - mean(y_true))^2)
    if (sst <= .Machine$double.eps) return(NA_real_)
    nse <- 1 - sse / sst
    if (nse > 1 && nse <= 1 + 1e-12) nse <- 1
    nse
  }
  
  ## ---------- Runner: 4 or 8 segments, global (d,m), corr matrix ----------
  run_cross_prediction_nlp_nseg <- function(
    x,
    nseg = 2,                 # <-- now supports 2 (also fine for 4 or 8)
    max_lag = 24,
    max_m  = 10,
    min_rows_frac = 0.25,
    gain_tol = 0.05,
    theiler = 0L,
    horizon = 1L,             # 1-step by default; keep if you want 2-step later
    plot_result = TRUE
  ) {
    stopifnot(is.numeric(nseg), nseg >= 2L)   # <-- was c(4,8); now any >=2
    x <- as.numeric(x); x <- x[is.finite(x)]
    if (length(x) < nseg) stop("Time series shorter than number of segments.")
    
    # Equal non-overlapping segments
    seg.length <- floor(length(x) / nseg)
    x_trim <- x[1:(seg.length * nseg)]
    segs <- split(x_trim, cut(seq_along(x_trim), nseg, labels = FALSE))
    seg.matrix <- matrix(0, seg.length, nseg)
    for (i1 in seq_len(nseg)) seg.matrix[, i1] <- unlist(segs[i1])
    
    # Global (d,m) from full series
    d_global <- estimate_tau_acf(x_trim, max_lag = max_lag)
    min_rows <- max(6L, floor(min_rows_frac * seg.length))
    # ensure enough rows for the chosen horizon
    need_rows <- max(min_rows, horizon + 1L)
    
    m_max_allowed <- max(2L, floor((seg.length - need_rows) / d_global) + 1L)
    m_cap <- min(max_m, m_max_allowed)
    if (m_cap < 2L) stop(sprintf("Segments too short for d=%d; reduce nseg or d.", d_global))
    
    m_global <- estimate_m_variance_gain(x_trim, tau = d_global, max_m = m_cap, gain_tol = gain_tol)
    usable_rows <- seg.length - (m_global - 1L) * d_global
    if (usable_rows < need_rows) {
      m_global <- max(2L, floor((seg.length - need_rows) / d_global) + 1L)
      usable_rows <- seg.length - (m_global - 1L) * d_global
      if (usable_rows < need_rows) stop("Too few embedded rows after capping m; reduce nseg or horizon.")
    }
    
    if (!requireNamespace("tseriesChaos", quietly = TRUE)) stop("install.packages('tseriesChaos')")
    
    # Skill matrices
    corr_matrix <- matrix(NA_real_, nrow = nseg, ncol = nseg)
    nse_matrix  <- matrix(NA_real_, nrow = nseg, ncol = nseg)
    
    for (i2 in 1:nseg) {         # learning segment (columns)
      learn <- seg.matrix[, i2]
      for (i3 in 1:nseg) {       # test segment (rows)
        test <- seg.matrix[, i3]
        res  <- nlp_cross_h(learn, test, delay = d_global, dim = m_global,
                            tw = theiler, h = horizon)
        corr_matrix[i3, i2] <- res$cor
        nse_matrix[i3,  i2] <- nse_strict(res$pred, res$truth)
      }
    }
    
    # ggplot for NSE (colored by learning segment)
    if (plot_result) {
      library(ggplot2)
      df_nse <- data.frame(
        test_segment  = rep(seq_len(nseg), times = nseg),
        learn_segment = factor(rep(seq_len(nseg), each = nseg), levels = seq_len(nseg)),
        nse           = as.vector(nse_matrix)
      )
      df_nse <- df_nse[is.finite(df_nse$nse), ]
      
      p <- ggplot(df_nse, aes(x = test_segment, y = nse,
                              color = learn_segment, group = learn_segment)) +
        geom_line(linewidth = 1) +
        geom_point(size = 2) +
        scale_x_continuous(breaks = seq_len(nseg)) +
        labs(x = "Test segment", y = "NSE",
             color = "Learning segment",
             title = paste0("Local predictor NSE (", nseg, "-segment, h = ", horizon, ")")) +
        theme_classic(base_size = 14)
      
      print(p)
    }
    
    invisible(list(
      nseg = nseg,
      seg_length = seg.length,
      d = d_global,
      m = m_global,
      usable_rows = usable_rows,
      horizon = horizon,
      corr_matrix = corr_matrix,
      nse_matrix  = nse_matrix
    ))
  }
  ## ---------- run, starting with one that has stationarity ----------
  colnames(dat)
  x <- dat$gsmeanstage
  gsmeanstage2 <- run_cross_prediction_nlp_nseg(x, nseg = 2, horizon = 1)
  gsmeanstage8 <- run_cross_prediction_nlp_nseg(x, nseg = 8, theiler = 0)
  
  # matrices of corr(pred, obs) by (test row, learn col):
  gsmeanstage2$nse_matrix
  gsmeanstage8$corr_matrix

  #good! Let's continue! Going to put this info in spreadsheet
  
  #mctn
  x <- dat$mctn
  mctn2 <- run_cross_prediction_nlp_nseg(x, nseg = 2, horizon = 1)
  mctn8 <- run_cross_prediction_nlp_nseg(x, nseg = 8, theiler = 0)
  
  ?geom_hline
  