##Breakpoint analysis for chlorophyll

#load packages
library(tidyverse)
library(lubridate)
library(zoo)
library(strucchange)
library(changepoint)
library(purrr)

#Load in data - full time series
dat <- read.csv('coastal_data_month.csv')
#Taylor Slough flows and stage
tsflow <- read.csv('Data/Taylor_bridge_flow.csv')
tsstage <- read.csv('Data/Taylor_bridge_stage.csv')
marsh <- read.csv('Data/clean/Marsh_CHP_rain.csv')
head(marsh)

#set up dataset
#Time column - monthly
dat$date <- ymd(dat$date)
str(dat)

tsflow$date <- mdy(tsflow$Daily.Date)
tsstage$date <- mdy(tsstage$Daily.Date)
str(tsflow)

marshstage <- marsh %>% dplyr::filter(DBKEY == 'meanstage') %>% mutate(date = mdy(Daily.Date))

#reduce columns, remove NAs at beginning, fill in NAs
dat2 <- dat %>% dplyr::select(date, gchl, rchl)
head(dat2)
#chlorophyll data is not viable before 2011. subset
dat2 <- dat2 %>% dplyr::filter(date > ymd('2011-01-01')) %>% dplyr::filter(date < ymd('2025-01-01'))

dat_int <- dat2 %>% dplyr::select(-date) %>% 
  mutate(across(everything(), ~ na.approx(., na.rm = FALSE))) %>% drop_na()

dat_date <- dat2 %>% dplyr::select(date)

dat3 <- cbind(dat_date, dat_int)
str(dat3)

ggplot()+
  geom_line(data = dat3, aes(x = date, y = rchl))

ggplot()+
  geom_line(data = tsflow, aes(x = date, y = Data.Value))
ggplot()+
  geom_line(data = tsstage, aes(x = date, y = Data.Value))

# --- Helper: manual AIC/BIC for piecewise-constant means (k = 0..K) ---
# BIC_k = n*log(RSS_k/n) + (k+1)*log n ; AIC_k = n*log(RSS_k/n) + 2*(k+1)
segmented_rss <- function(y, break_idx) {
  y <- as.numeric(y)
  n <- length(y)
  if (n == 0) return(NA_real_)
  cuts <- c(0L, sort(as.integer(break_idx)), n)
  rss <- 0
  for (j in seq_len(length(cuts) - 1L)) {
    idx <- (cuts[j] + 1L):cuts[j + 1L]
    mu  <- mean(y[idx])
    rss <- rss + sum((y[idx] - mu)^2)
  }
  rss
}

bic_aic_manual <- function(y, h = 12, K = 6) {
  y <- as.numeric(y)
  y <- y[is.finite(y)]
  n <- length(y)
  if (n < 2*h) {
    K_cap <- 0L
  } else {
    K_cap <- min(K, max(0L, floor(n / h) - 1L))
  }
  
  # k = 0
  rss0 <- sum((y - mean(y))^2)
  out <- tibble::tibble(
    k = 0L,
    rss = rss0,
    p   = 1L,
    AIC = n*log(rss0/n) + 2*1L,
    BIC = n*log(rss0/n) + log(n)*1L
  )
  
  if (K_cap == 0L) return(out)
  
  fits <- purrr::map(1:K_cap, function(k) {
    bp <- tryCatch(strucchange::breakpoints(y ~ 1, h = h, breaks = k), error = function(e) NULL)
    if (is.null(bp) || any(is.na(bp$breakpoints))) {
      return(tibble::tibble(k = k, rss = NA_real_, p = k+1L, AIC = NA_real_, BIC = NA_real_))
    }
    rssk <- segmented_rss(y, bp$breakpoints)
    tibble::tibble(
      k = k,
      rss = rssk,
      p   = k + 1L,
      AIC = n*log(rssk/n) + 2*(k+1L),
      BIC = n*log(rssk/n) + log(n)*(k+1L)
    )
  })
  
  dplyr::bind_rows(out, dplyr::bind_rows(fits))
}
# ---- Breakpoint analysis (with optional STL deseasoning) ----
breakpoint_analysis_chl <- function(
    data,
    date_col = "date",
    value_col = "chl",
    min_seg_months = 12,
    max_breaks = 5,
    deseason_stl = TRUE,
    ic = c("BIC","AIC")      # choose which IC selects the # of breaks (for mean model)
){
  ic <- match.arg(ic)
  
  library(tidyverse); library(lubridate); library(zoo)
  library(strucchange); library(changepoint)
  
  # ---- Monthly regularization ----
  dat <- data %>%
    mutate(.date = as.Date(.data[[date_col]]),
           .value = as.numeric(.data[[value_col]])) %>%
    filter(!is.na(.date)) %>%
    mutate(.ym = as.yearmon(.date)) %>%
    group_by(.ym) %>%
    summarize(chl = mean(.value, na.rm = TRUE), .groups = "drop") %>%
    arrange(.ym)
  
  full_ym <- seq(min(dat$.ym), max(dat$.ym), by = 1/12)
  monthly <- tibble(.ym = full_ym) %>%
    left_join(dat, by = ".ym") %>%
    mutate(date = as.Date(.ym))
  
  df_clean <- monthly %>% drop_na(chl) %>% mutate(t_idx = row_number())
  n <- nrow(df_clean)
  max_feasible <- max(0L, floor(n / min_seg_months) - 1L)
  cap_b <- min(max_breaks, max_feasible)
  
  # ---- Series for mean-shift model (optional STL deseason) ----
  y_for_mean <- df_clean$chl
  if (deseason_stl) {
    ts_chl <- ts(df_clean$chl, frequency = 12)
    stl_fit <- stl(ts_chl, s.window = "periodic", robust = TRUE)
    y_for_mean <- as.numeric(ts_chl - stl_fit$time.series[, "seasonal"])
  }
  
  # If no breaks are feasible, return early (cleanly)
  if (cap_b == 0L) {
    base <- ggplot(monthly, aes(date, chl)) + geom_line(linewidth = 0.7) +
      theme_classic() + labs(x = NULL, y = "Chlorophyll")
    return(list(
      monthly_data = monthly,
      results_mean = tibble(method="strucchange_mean", break_id=integer(), index=integer(),
                            date=as.Date(character()), ci_lower=as.Date(character()),
                            ci_upper=as.Date(character()), n_breaks=0),
      results_trend = tibble(method="strucchange_trend", break_id=integer(), index=integer(),
                             date=as.Date(character()), ci_lower=as.Date(character()),
                             ci_upper=as.Date(character()), n_breaks=0),
      results_changepoint = tibble(method="changepoint_PELT_mean",
                                   break_id=integer(), index=integer(), date=as.Date(character())),
      ic_curves_mean  = tibble(n_breaks = 0L, BIC = NA_real_, AIC = NA_real_, model = "mean"),
      ic_curves_trend = tibble(n_breaks = 0L, BIC = NA_real_, AIC = NA_real_, model = "trend"),
      test_global_mean  = tibble(test="supF", statistic=NA_real_, p_value=NA_real_),
      test_global_trend = tibble(test="supF", statistic=NA_real_, p_value=NA_real_),
      plot_mean  = base + labs(title = "No feasible breaks given h"),
      plot_trend = base + labs(title = "No feasible breaks given h")
    ))
  }
  
  # ---- MEAN MODEL: manual ICs over k = 0..cap_b ----
  ics_mean <- bic_aic_manual(y_for_mean, h = min_seg_months, K = cap_b)
  k_sel_mean <- with(ics_mean, if (ic == "BIC") k[which.min(BIC)] else k[which.min(AIC)])
  
  # Global supF (H0: no break) on the *path* up to cap_b (for a p-value)
  bp_mean_path <- breakpoints(y_for_mean ~ 1, h = min_seg_months, breaks = cap_b)
  test_global_mean <- {
    out <- tryCatch(sctest(bp_mean_path, type = "supF"), error = function(e) NULL)
    if (is.null(out)) tibble(test="supF", statistic=NA_real_, p_value=NA_real_)
    else tibble(test="supF", statistic=unname(out$statistic), p_value=unname(out$p.value))
  }
  
  # Fit at selected k (if > 0) and extract CI table
  bp_mean <- if (k_sel_mean > 0L)
    breakpoints(y_for_mean ~ 1, h = min_seg_months, breaks = k_sel_mean) else NULL
  
  mean_tbl <- if (is.null(bp_mean)) {
    tibble(method = "strucchange_mean",
           break_id = integer(), index = integer(),
           date = as.Date(character()), ci_lower = as.Date(character()),
           ci_upper = as.Date(character()), n_breaks = 0L)
  } else {
    ci <- confint(bp_mean)$confint
    tibble(
      method   = "strucchange_mean",
      break_id = seq_along(bp_mean$breakpoints),
      index    = bp_mean$breakpoints,
      date     = df_clean$date[bp_mean$breakpoints],
      ci_lower = df_clean$date[pmax(ci[,1], 1)],
      ci_upper = df_clean$date[pmin(ci[,2], nrow(df_clean))],
      n_breaks = k_sel_mean
    )
  }
  
  # ---- TREND (slope) MODEL: keep original IC selection (quick) ----
  bp_trend_all <- breakpoints(df_clean$chl ~ df_clean$t_idx, h = min_seg_months, breaks = cap_b)
  bic_trnd <- BIC(bp_trend_all); aic_trnd <- AIC(bp_trend_all)
  k_sel_trend <- if (ic == "BIC") which.min(bic_trnd) - 1L else which.min(aic_trnd) - 1L
  
  test_global_trend <- {
    out <- tryCatch(sctest(bp_trend_all, type = "supF"), error = function(e) NULL)
    if (is.null(out)) tibble(test="supF", statistic=NA_real_, p_value=NA_real_)
    else tibble(test="supF", statistic=unname(out$statistic), p_value=unname(out$p.value))
  }
  
  bp_trend <- if (k_sel_trend > 0L)
    breakpoints(df_clean$chl ~ df_clean$t_idx, h = min_seg_months, breaks = k_sel_trend) else NULL
  
  trend_tbl <- if (is.null(bp_trend)) {
    tibble(method = "strucchange_trend",
           break_id = integer(), index = integer(),
           date = as.Date(character()), ci_lower = as.Date(character()),
           ci_upper = as.Date(character()), n_breaks = 0L)
  } else {
    ci <- confint(bp_trend)$confint
    tibble(
      method   = "strucchange_trend",
      break_id = seq_along(bp_trend$breakpoints),
      index    = bp_trend$breakpoints,
      date     = df_clean$date[bp_trend$breakpoints],
      ci_lower = df_clean$date[pmax(ci[,1], 1)],
      ci_upper = df_clean$date[pmin(ci[,2], nrow(df_clean))],
      n_breaks = k_sel_trend
    )
  }
  
  # ---- PELT cross-check (mean, raw series) ----
  cp <- tryCatch(changepoint::cpt.mean(df_clean$chl, method = "PELT",
                                       penalty = "MBIC", minseglen = min_seg_months, class = TRUE),
                 error = function(e) NULL)
  cpt_tbl <- tibble(
    method = "changepoint_PELT_mean",
    break_id = if (is.null(cp)) integer() else seq_along(cpts(cp)),
    index    = if (is.null(cp)) integer() else cpts(cp),
    date     = if (is.null(cp)) as.Date(character()) else df_clean$date[cpts(cp)]
  )
  
  # ---- IC curves for output ----
  ic_curves_mean <- ics_mean %>%
    transmute(n_breaks = k, BIC, AIC, model = "mean")
  
  ic_curve_trend <- tibble(
    n_breaks = 0:cap_b,
    BIC = bic_trnd,
    AIC = aic_trnd,
    model = "trend"
  )
  
  # ---- Plots ----
  base <- ggplot(monthly, aes(date, chl)) + geom_line(linewidth = 0.7) +
    theme_classic() + labs(x = NULL, y = "Flow (CFS)")
  
  p_mean <- base +
    { if (nrow(mean_tbl)) geom_rect(data = mean_tbl, aes(xmin = ci_lower, xmax = ci_upper, ymin = -Inf, ymax = Inf),
                                    inherit.aes = FALSE, alpha = 0.12) } +
    { if (nrow(mean_tbl)) geom_vline(data = mean_tbl, aes(xintercept = as.numeric(date)), linetype = 2, color = 'red') } +
    labs(title = paste0("Mean-shift breaks (", if (deseason_stl) "STL-deseasoned" else "raw",
                        ", IC=", ic, ", h=", min_seg_months, ")"))
  
  p_trend <- base +
    { if (nrow(trend_tbl)) geom_rect(data = trend_tbl, aes(xmin = ci_lower, xmax = ci_upper, ymin = -Inf, ymax = Inf),
                                     inherit.aes = FALSE, alpha = 0.12) } +
    { if (nrow(trend_tbl)) geom_vline(data = trend_tbl, aes(xintercept = as.numeric(date)), linetype = 2, color = 'red') } +
    labs(title = paste0("Trend (slope) breaks (raw, IC=", ic, ", h=", min_seg_months, ")"))
  
  # ---- Return ----
  list(
    monthly_data = monthly,
    results_mean = mean_tbl,
    results_trend = trend_tbl,
    results_changepoint = cpt_tbl,
    ic_curves_mean  = ic_curves_mean,
    ic_curves_trend = ic_curve_trend,
    test_global_mean  = test_global_mean,
    test_global_trend = test_global_trend,
    plot_mean = p_mean,
    plot_trend = p_trend
  )
}

#run the breakpoint analysis - garfield
out_bic <- breakpoint_analysis_chl(
  dat3, date_col="date", value_col="gchl",
  min_seg_months=12, max_breaks=6,
  deseason_stl=TRUE, ic="BIC"
)

#run the breakpoint analysis - tsflow

#monthly
tsflow$month <- format(as.Date(tsflow$date, format = '%Y-%m-%d'), format = '%Y-%m-01')
str(tsflow)
tsflowmonth <- tsflow %>% group_by(month) %>% summarize(mean = mean(Data.Value), max = max(Data.Value), sum = sum(Data.Value)) %>% mutate(month = ymd(month))



out_bic <- breakpoint_analysis_chl(
  tsflowmonth, date_col="month", value_col="mean",
  min_seg_months=12, max_breaks=6,
  deseason_stl=TRUE, ic="BIC"
)

out_bic$ic_curves_mean   
out_bic$results_mean
out_bic$plot_mean

#6 breakpoints

#monthly max
out_bic <- breakpoint_analysis_chl(
  tsflowmonth, date_col="month", value_col="max",
  min_seg_months=12, max_breaks=8,
  deseason_stl=TRUE, ic="BIC"
)

out_bic$ic_curves_mean   
out_bic$results_mean
out_bic$plot_mean

#4 breakpoints

#monthly sum
out_bic <- breakpoint_analysis_chl(
  tsflowmonth, date_col="month", value_col="sum",
  min_seg_months=12, max_breaks=8,
  deseason_stl=TRUE, ic="BIC"
)

out_bic$ic_curves_mean   
out_bic$results_mean
out_bic$plot_mean


#run the breakpoint analysis - tsstage
out_bic <- breakpoint_analysis_chl(
  tsstage, date_col="date", value_col="Data.Value",
  min_seg_months=12, max_breaks=6,
  deseason_stl=TRUE, ic="BIC"
)

out_bic$ic_curves_mean   
out_bic$results_mean
out_bic$test_global_mean

#No breakpoints

#run the breakpoint analysis - marsh stage
out_bic <- breakpoint_analysis_chl(
  marshstage, date_col="date", value_col="Data.Value",
  min_seg_months=12, max_breaks=6,
  deseason_stl=TRUE, ic="BIC"
)

out_bic$ic_curves_mean   
out_bic$results_mean
out_bic$test_global_mean

#No breakpoints

# --- inputs (match what you used) ---
h  <- 12                     # min_seg_months
df <- dat3 %>% select(date, gchl)

# 1) Monthly regularization (same as function)
m_dat <- df %>%
  mutate(date = as.Date(date),
         ym   = as.yearmon(date)) %>%
  group_by(ym) %>%
  summarise(chl = mean(gchl, na.rm = TRUE), .groups = "drop") %>%
  arrange(ym) %>%
  mutate(date = as.Date(ym))

y_raw <- m_dat$chl
ok    <- is.finite(y_raw)
y_raw <- y_raw[ok]
dates <- m_dat$date[ok]
n     <- length(y_raw)

# deseason to match your run (set FALSE if you ran raw)
ts_chl <- ts(y_raw, frequency = 12)
stl_fit <- stl(ts_chl, s.window = "periodic", robust = TRUE)
y_ds <- as.numeric(ts_chl - stl_fit$time.series[, "seasonal"])

# 2) Global supF test for ≥1 break (respect h via trimming)
fs <- Fstats(y_ds ~ 1,
             from = h / n,             # trim so each side has at least h points
             to   = 1 - h / n)
supF <- sctest(fs, type = "supF")
supF
# supF$statistic and supF$p.value are your test results

# 3) Chow tests at YOUR detected breakpoints (k = 5 from your results)
n <- length(y_ds)

# keep only admissible break points (enough data on both sides)
valid_idx <- idx_breaks[idx_breaks > h & idx_breaks < (n - h)]

# Chow tests at those points — use the *formula* interface
chow_tbl <- tibble(
  index = valid_idx,
  date  = dates[valid_idx]
) %>%
  mutate(
    test      = map(index, ~ sctest(y_ds ~ 1, type = "Chow", point = .x)),
    statistic = map_dbl(test, ~ unname(.x$statistic)),
    p_value   = map_dbl(test, ~ unname(.x$p.value)),
    p_adj_BH  = p.adjust(p_value, method = "BH")  # optional multiple-testing adjust
  ) %>%
  select(-test)

chow_tbl

# Build segment boundaries from breaks
cuts <- c(0, sort(out_bic$results_mean$index), n)
seg_summary <- purrr::map_dfr(seq_len(length(cuts)-1), function(j){
  idx <- (cuts[j]+1):cuts[j+1]
  tibble(
    segment = j,
    start   = dates[min(idx)],
    end     = dates[max(idx)],
    n       = length(idx),
    mean_ds = mean(y_ds[idx]),         # deseasoned mean
    sd_ds   = sd(y_ds[idx]),
    se_ds   = sd_ds / sqrt(n)
  )
}) %>% 
  mutate(delta_from_prev = mean_ds - dplyr::lag(mean_ds),
         pct_change     = 100 * delta_from_prev / dplyr::lag(mean_ds))

seg_summary

ggplot() +
  geom_line(data = dat3, aes(x = date, y = gchl), color = "darkgreen") +
  # horizontal means
  geom_segment(aes(x = ymd("2011-02-01"), xend = ymd("2016-05-01"),
                   y = 1.49, yend = 1.49)) +
  geom_segment(aes(x = ymd("2016-06-01"), xend = ymd("2024-12-01"),
                   y = 9.04, yend = 9.04)) +
  # CIs as rectangles (was ribbon)
  annotate("rect", xmin = ymd("2011-02-01"), xmax = ymd("2016-05-01"),
           ymin = 1.302, ymax = 1.678, alpha = 0.3) +
  annotate("rect", xmin = ymd("2016-06-01"), xmax = ymd("2024-12-01"),
           ymin = 8.0685, ymax = 10.01, alpha = 0.3) +
  # breakpoint and its CI window
  geom_vline(xintercept = as.Date("2016-05-01"), color = "darkblue") +
  annotate("rect", xmin = ymd("2015-10-01"), xmax = ymd("2016-05-01"),
           ymin = -Inf, ymax = Inf, alpha = 0.1) +
  labs(x = "Date", y = "Garfield Chlorophyll") +
  theme_classic()

?geom_hline
#garfield time series has 5 breakpoints, first one in 2016 most significant
#only highly significant breakpoint for rankin is 2016

##Correlation between rchl and area
#### Load data#####

dat <- read.csv(file = 'Data/Clean/sat_dat_usf.csv')

#Change character column to date
dat <- dat %>%
  mutate(
    year = str_extract(source_file, "^\\d{4}"),
    month = str_extract(source_file, "(?<=_)\\d{2}(?=_)"),
    date = ymd(paste(year, month, "01", sep = "-"))
  )

str(dat)

dat1 <- dat[-c(97:112), ]
dat_sat <- dat1 %>% mutate(logarea = log(total_area_m2))
ggplot()+
  geom_line(data = dat_sat, aes(x = date, y = total_area_m2))+
  theme_classic()

dat <- read.csv(file = 'coastal_data_month.csv')

dat <- dat %>% dplyr::select(-c(oflow, omaxstage, omeanstage, ominstage, wflow, wmeanstage, gNN, gNO3, gNO2, gAP, gOP, rNN, rNO3, rNO2, rAP, rOP, gchlb, rchlb))

str(dat)

str(dat_sat)
dat$date <- ymd(dat$date)

dat_all <- merge(dat_sat, dat, by = 'date')
head(dat_all)
model <- lm(total_area_m2 ~ rchl, data = dat_all)
summary(model)
model1 <- lm(total_area_m2 ~ gchl, data = dat_all)
summary(model1)
model2 <- lm(rchl ~ gchl, data = dat_all)
summary(model2)

ggplot()+
  geom_smooth(data = dat_all, aes(x = rchl, y = total_area_m2), method = 'lm')+
  geom_point(data = dat_all, aes(x = rchl, y = total_area_m2))+
  theme_classic()

ggplot()+
  geom_smooth(data = dat_all, aes(x = gchl, y = total_area_m2), method = 'lm')+
  geom_point(data = dat_all, aes(x = gchl, y = total_area_m2))+
  theme_classic()

ggplot()+
  geom_smooth(data = dat_all, aes(x = gchl, y = rchl), method = 'lm')+
  geom_point(data = dat_all, aes(x = gchl, y = rchl))+
  theme_classic()
#plot
dat <- read.csv('Data/GB_grab_chl_91-06.csv')
head(dat)
dat <- dat %>% dplyr::filter(Value > 0)

dat$Collection_Date <- ymd(dat$Collection_Date)
ggplot(data = dat)+
  geom_line(aes(x = Collection_Date, y = Value), color = 'red')+
  theme_classic()
