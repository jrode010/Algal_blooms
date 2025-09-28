#Field correlations
library(dplyr)
library(stringr)
library(purrr)
library(readr)        # for parse_number()
library(lme4)
library(lmerTest)     # p-values for fixed effects in lmer()
library(performance) 
library(tidyverse)
library(broom)
library(rnaturalearth)
library(rnaturalearthdata)
library(sf)
library(rnaturalearthhires)

#Use Baywide data since it is a longer time series. 
BW_WQ <- read.csv('Data/BW_WQ.csv')
BW_wat_nut <- read.csv('Data/BW_wat_nut.csv')
head(BW_WQ)
head(BW_wat_nut)

BW_all <- merge(BW_WQ, BW_wat_nut, by = c('Site_Name', 'Collection_Date'))



names(BW_all)
str(BW_all)
BW_all$N.N_Rep_A <- as.numeric(BW_all$N.N_Rep_A)
BW_all$N.N_Rep_B <- as.numeric(BW_all$N.N_Rep_B)
BW_all$NO2_Rep_A <- as.numeric(BW_all$NO2_Rep_A)
BW_all$NO2_Rep_B <- as.numeric(BW_all$NO2_Rep_B)
BW_all$NO3_Rep_A <- as.numeric(BW_all$NO3_Rep_A)
BW_all$NO3_Rep_B <- as.numeric(BW_all$NO3_Rep_B)
BW_all$NH4_Rep_A <- as.numeric(BW_all$NH4_Rep_A)
BW_all$NH4_Rep_B <- as.numeric(BW_all$NH4_Rep_B)
BW_all$SRP_Rep_A <- as.numeric(BW_all$SRP_Rep_A)
BW_all$SRP_Rep_B <- as.numeric(BW_all$SRP_Rep_B)

#Let's look at relationships!
#NH4 and pH
ggplot(data = BW_all)+
  geom_point(aes(x = Surface_pH, y = NH4_Rep_A))

df <- BW_all %>%
  mutate(
    Site_Name  = factor(Site_Name),
    Surface_pH = as.numeric(Surface_pH)
  )
str(BW_all)
write.csv(BW_all, file = 'Data/Baywide/cleaned.csv')
# Identify predictor columns from "N.N_Rep_A" through "Si_Rep_B" (inclusive)
nm <- names(df)
start_i <- which(tolower(nm) == tolower("N.N_Rep_A"))
end_i   <- which(tolower(nm) == tolower("Si_Rep_B"))
if (length(start_i) != 1 || length(end_i) != 1) {
  stop("Could not locate the start/end predictor columns. Check names 'N.N_Rep_A' and 'Si_Rep_B'.")
}
predictor_cols <- nm[seq(from = min(start_i, end_i), to = max(start_i, end_i))]

# --- Fit one model per predictor and collect stats ---
fit_one <- function(pred_name) {
  # Build small data frame; parse character numerics like "ND" -> NA
  dat <- df %>%
    select(Site_Name, Turbidity, !!sym(pred_name)) %>%
    mutate(x_raw = .data[[pred_name]],
           x = if (is.character(x_raw)) parse_number(x_raw) else as.numeric(x_raw)) %>%
    select(-x_raw) %>%
    filter(is.finite(Turbidity), is.finite(x))
  
  n_obs   <- nrow(dat)
  n_sites <- dplyr::n_distinct(dat$Site_Name)
  if (n_obs < 10 || n_sites < 2) {
    return(tibble(
      predictor = pred_name, n = n_obs, n_sites = n_sites,
      beta = NA_real_, se = NA_real_, t_or_z = NA_real_, p_value = NA_real_,
      R2_marginal = NA_real_, R2_conditional = NA_real_
    ))
  }
  
  # LMM: Turbidity ~ predictor + (1|Site_Name)
  mod <- tryCatch(
    lmer(Turbidity ~ x + (1 | Site_Name), data = dat, REML = FALSE),
    error = function(e) NULL
  )
  if (is.null(mod)) {
    return(tibble(
      predictor = pred_name, n = n_obs, n_sites = n_sites,
      beta = NA_real_, se = NA_real_, t_or_z = NA_real_, p_value = NA_real_,
      R2_marginal = NA_real_, R2_conditional = NA_real_
    ))
  }
  
  coefs <- coef(summary(mod))
  if (!"x" %in% rownames(coefs)) {
    return(tibble(
      predictor = pred_name, n = n_obs, n_sites = n_sites,
      beta = NA_real_, se = NA_real_, t_or_z = NA_real_, p_value = NA_real_,
      R2_marginal = NA_real_, R2_conditional = NA_real_
    ))
  }
  
  beta <- unname(coefs["x", "Estimate"])
  se   <- unname(coefs["x", "Std. Error"])
  tz   <- unname(coefs["x", "t value"])
  pval <- suppressWarnings(unname(coefs["x", "Pr(>|t|)"]))
  
  r2s <- tryCatch(performance::r2(mod), error = function(e) NULL)
  R2m <- if (!is.null(r2s) && "R2_marginal" %in% names(r2s)) as.numeric(r2s$R2_marginal) else NA_real_
  R2c <- if (!is.null(r2s) && "R2_conditional" %in% names(r2s)) as.numeric(r2s$R2_conditional) else NA_real_
  
  tibble(
    predictor = pred_name, n = n_obs, n_sites = n_sites,
    beta = beta, se = se, t_or_z = tz, p_value = pval,
    R2_marginal = R2m, R2_conditional = R2c
  )
}

results_ph_as_response <- map_dfr(predictor_cols, fit_one) %>%
  mutate(p_adj_BH = p.adjust(p_value, method = "BH")) %>%  # optional multiple-testing control
  arrange(p_value)

results_ph_as_response
# 0) choose which predictors to plot (here: top two by p-value)
preds_to_plot <- results_ph_as_response %>%
  arrange(p_value) %>%
  slice_head(n = 2) %>%
  pull(predictor)

# 1) Prep a clean, long dataset for those predictors
prep_pred <- function(df, pred) {
  df %>%
    transmute(
      Site_Name   = factor(Site_Name),
      Turbidity  = as.numeric(Turbidity),
      x_raw       = .data[[pred]]
    ) %>%
    mutate(
      x = if (is.character(x_raw)) readr::parse_number(x_raw) else as.numeric(x_raw),
      predictor = pred
    ) %>%
    filter(is.finite(Turbidity), is.finite(x)) %>%
    select(-x_raw)
}

plot_dat <- bind_rows(map(preds_to_plot, ~ prep_pred(BW_all, .x)))

# 2) Fit one LMM per predictor: Turbidity ~ x + (1|Site_Name)
mods <- map(preds_to_plot, ~ lmer(Turbidity ~ x + (1 | Site_Name),
                                  data = filter(plot_dat, predictor == .x),
                                  REML = FALSE))
names(mods) <- preds_to_plot

# 3) Fixed-effect (population) prediction lines
pred_lines <- map_dfr(preds_to_plot, function(pr) {
  dsub <- filter(plot_dat, predictor == pr)
  beta <- fixef(mods[[pr]])  # (Intercept), x
  xg   <- seq(min(dsub$x, na.rm = TRUE), max(dsub$x, na.rm = TRUE), length.out = 100)
  tibble(
    predictor = pr,
    x = xg,
    yhat = beta[1] + beta[2] * xg
  )
})

# 4) Site-specific (conditional) lines from random intercepts
site_lines <- map_dfr(preds_to_plot, function(pr) {
  dsub <- filter(plot_dat, predictor == pr)
  beta <- fixef(mods[[pr]])
  ran  <- ranef(mods[[pr]])$Site_Name %>%
    rownames_to_column("Site_Name") %>%
    rename(b0 = `(Intercept)`)
  
  xg <- seq(min(dsub$x, na.rm = TRUE), max(dsub$x, na.rm = TRUE), length.out = 50)
  
  expand.grid(x = xg, Site_Name = ran$Site_Name, KEEP.OUT.ATTRS = FALSE) %>%
    as_tibble() %>%
    left_join(ran, by = "Site_Name") %>%
    mutate(
      yhat_site = (beta[1] + b0) + beta[2] * x,
      predictor = pr
    )
})

# 5) (optional) add slope & p-value labels per facet
lab_tbl <- results_ph_as_response %>%
  filter(predictor %in% preds_to_plot) %>%
  transmute(
    predictor,
    label = sprintf("slope = %.3g, p = %.2g", beta, p_value)
  )

# 6) Plot: points, faint site-specific lines, bold fixed-effect line
p <- ggplot(plot_dat, aes(x = x, y = Turbidity)) +
  geom_point(alpha = 0.5, size = 1) +
  geom_line(data = pred_lines, aes(y = yhat), linewidth = 1.1) +
  facet_wrap(~ predictor, scales = "free_x", ncol = 2) +
  theme_classic() +
  labs(
    x = "Total Phosphorus",
    y = "Turbidity",
    title = "Turbidity vs Total Phosphorus"
  )

# (optional) add slope/p labels in each facet
p + geom_text(
  data = lab_tbl,
  inherit.aes = FALSE,
  aes(x = -Inf, y = Inf, label = label),
  hjust = -0.05, vjust = 1.1, size = 3
)

ggsave(filename = 'plots/turbvstp.png', width = 6, height = 6)

#Add in all other data to perform relationships
#coastal
coast <- read.csv('coastal_data_month.csv')
#current day
sites <- read.csv('Data/Current_day/site_metadata.csv')
wqsonde <- read.csv('Data/Current_day/WQ_sonde1.csv')
wqnut <- read.csv('Data/Current_day/WQ_nut.csv')
wqchl <- read.csv('Data/Current_day/chl.csv')
head(wqsonde)
head(sites)
head(wqnut)
head(wqchl)
str(wqnut$Sampling_event)

for (i in 1:nrow(wqnut)) {
  if (wqnut$Sampling_event[i] == '45681'){
    wqnut$Sampling_event[i] <- '1-24'
  } else if (wqnut$Sampling_event[i] == '45985'){
    wqnut$Sampling_event[i] <- '11-24'
  } else if (wqnut$Sampling_event[i] == '45771'){
    wqnut$Sampling_event[i] <- '4-24'
  } else if (wqnut$Sampling_event[i] == '45893'){
    wqnut$Sampling_event[i] <- '8-24'
  } else if (wqnut$Sampling_event[i] == '45740'){
    wqnut$Sampling_event[i] <- '4-24'
  } else if (wqnut$Sampling_event[i] == '45862'){
    wqnut$Sampling_event[i] <- '8-24'
  } else if (wqnut$Sampling_event[i] == '45000'){
    wqnut$Sampling_event[i] <- '9-23'
  }
}
head(wqsonde)
wqsonde <- wqsonde %>% dplyr::select(Site_code, Sampling_event, Sample_code, Temp, DO, Salinity, pH, NTU, FDOM_RFU)
str(wqsonde)
unique(wqsonde$Sampling_event)
for (i in 1:nrow(wqsonde)) {
  if (wqsonde$Sampling_event[i] == '24-Jan'){
    wqsonde$Sampling_event[i] <- '1-24'
  } else if (wqsonde$Sampling_event[i] == '24-Nov'){
    wqsonde$Sampling_event[i] <- '11-24'
  } else if (wqsonde$Sampling_event[i] == '24-Apr'){
    wqsonde$Sampling_event[i] <- '4-24'
  } else if (wqsonde$Sampling_event[i] == '24-Aug'){
    wqsonde$Sampling_event[i] <- '8-24'
  } else if (wqsonde$Sampling_event[i] == '24-Mar'){
    wqsonde$Sampling_event[i] <- '4-24'
  } else if (wqsonde$Sampling_event[i] == '24-Jul'){
    wqsonde$Sampling_event[i] <- '8-24'
  } else if (wqsonde$Sampling_event[i] == '23-Sep'){
    wqsonde$Sampling_event[i] <- '9-23'
  } else if (wqsonde$Sampling_event[i] == '11_23'){
    wqsonde$Sampling_event[i] <- '1123E'
}}
head(wqnut)
wqnut_dis <- wqnut %>% dplyr::filter(is.na(NO2.N_um.L) == F) %>% dplyr::select(Sample_code, Site_code, Sampling_event, N.N_um.L, NO2.N_um.L, NO3.N_um.L, NH3.NH4.N_um.L, SRP_um.L) %>% distinct()
wqnut_tot <- wqnut %>% dplyr::filter(is.na(TN_um.L) == F) %>% dplyr::select(Sample_code, Site_code, Sampling_event, TN_um.L, TP_um.L, TOC_um.L) %>% distinct()

wqsonde1 <- merge(wqsonde, sites, by = 'Site_code')
str(wqsonde1)
str(wqnut_dis)
wq1 <- merge(wqsonde1, wqnut_dis, by = c('Site_code', 'Sampling_event'))
wq2 <- merge(wq1, wqnut_tot, by = c('Site_code', 'Sampling_event'))
wq3 <- merge(wq2, wqchl, by = c('Site_code', 'Sampling_event')) %>% distinct() 

write.csv(wq3, file = 'Data/Current_day/combined.csv')

cor_tot <- read.csv('Correlations/combined.csv')
cor_tot <- cor_tot %>% distinct()

head(cor_tot)

# Define responses and predictors
responses <- c("chlorophyll", "pH", "turbidity")
predictors <- c("TP", "TOC", "TN", "temp", "salinity", "NO2", "NH4")

# Function to find best model for one response
find_best_model <- function(response, predictors, data) {
  results <- map_dfr(predictors, function(pred) {
    formula <- as.formula(paste(response, "~", pred))
    model <- lm(formula, data = data)
    glance(model) %>%
      mutate(predictor = pred, model = list(model))
  })
  best <- results %>% arrange(desc(r.squared)) %>% slice(1)
  return(best)
}

# Loop through responses and make plots
for (resp in responses) {
  best <- find_best_model(resp, predictors, df)
  pred <- best$predictor
  mod <- best$model[[1]]
  r2 <- round(best$r.squared, 3)
  pval <- signif(summary(mod)$coefficients[2,4], 3)
  
  p <- ggplot(cor_tot, aes_string(x = pred, y = resp)) +
    geom_point(alpha = 0.6) +
    geom_smooth(method = "lm", se = TRUE, color = "blue") +
    labs(
      title = paste("Best predictor of", resp, ":", pred),
      subtitle = paste0("R² = ", r2, ", p = ", pval),
      x = pred, y = resp
    ) +
    theme_minimal(base_size = 14)
  
  print(p)
}

# Select only the variables you need
vars <- c("chlorophyll", "pH", "turbidity",
          "TP", "TOC", "TN", "temp", "salinity", "NO2", "NH4")

# Scale variables (mean 0, sd 1) and keep as data frame
scaled_df <- df %>%
  select(all_of(vars)) %>%
  mutate(across(everything(), scale)) %>%
  as.data.frame()

# Define responses and predictors
responses <- c("chlorophyll", "pH", "turbidity")
predictors <- c("TP", "TOC", "TN", "temp", "salinity", "NO2", "NH4")

# Function to find best model for one response
find_best_model <- function(response, predictors, data) {
  results <- map_dfr(predictors, function(pred) {
    if (!pred %in% names(data)) return(NULL)  # skip if missing
    formula <- as.formula(paste(response, "~", pred))
    model <- lm(formula, data = data)
    glance(model) %>%
      mutate(predictor = pred, model = list(model))
  })
  best <- results %>% arrange(desc(r.squared)) %>% slice(1)
  return(best)
}

# Loop through responses and make plots
for (resp in responses) {
  best <- find_best_model(resp, predictors, scaled_df)
  pred <- best$predictor
  mod <- best$model[[1]]
  r2 <- round(best$r.squared, 3)
  pval <- signif(summary(mod)$coefficients[2,4], 3)
  
  p <- ggplot(scaled_df, aes(x = !!sym(pred), y = !!sym(resp))) +
    geom_point(alpha = 0.6) +
    geom_smooth(method = "lm", se = TRUE, color = "blue") +
    labs(
      title = paste("Best predictor of", resp, ":", pred),
      subtitle = paste0("R² = ", r2, ", p = ", pval),
      x = paste0(pred, " (scaled)"), 
      y = paste0(resp, " (scaled)")
    ) +
    theme_minimal(base_size = 14)
  
  print(p)
}


df <- wq3 %>%
  rename(
    temp = Temp,
    turbidity = NTU,
    salinity = Salinity,
    NO2 = NO2.N_um.L,
    NH4 = NH3.NH4.N_um.L,
    TOC = TOC_um.L,
    TP = TP_um.L,
    TN = TN_um.L,
    chlorophyll = Chl
  )
for (i in (1:nrow(df))){
  if (df$Sampling_event[i] == '1123E'){
    df$Sampling_event[i] <-  '11-23'
  }
}
df <- df %>% mutate(Sampling_event = factor(Sampling_event, levels = c('9-23', '11-23', '1-24', '4-24', '8-24', '11-24')))
head(df)
df <- df %>% dplyr::rename(Temperature = temp, Turbidity = turbidity, SRP = SRP_um.L)
df <- df %>% dplyr::rename(Chlorophyll = chlorophyll, Salinity = salinity)
str(df)
df$SRP <- as.numeric(df$SRP)

df$Salinity <- ifelse(df$Salinity > 100, 16.1, df$Salinity)

df1 <- df %>% group_by(Site_code, Sampling_event) %>% mutate(Chlorophyll = mean(Chlorophyll)) %>% dplyr::select(-c(Sample_code.y.1, Rep, Sample_code.x, Sample_code.x.1, Water_amount, Collected, Processed)) %>%  distinct()

# Variables to map
vars_to_map <- c("Temperature", "pH", "Turbidity", 
                 "NO2", "NH4", "TOC", "TP", "TN", 
                 "Salinity", "Chlorophyll", 'SRP', 'DO')

# Pivot to long format
df_long <- df %>%
  pivot_longer(cols = all_of(vars_to_map),
               names_to = "variable",
               values_to = "value")

# --- Load shapefile (Florida shoreline) ---
shoreline <- st_read("south_florida_detailed.shp") %>%
  st_transform(crs = 4326)


# plot(shoreline)

# --- Compute bounding box of your data & zoom out a little ---
lon_range <- range(df_long$Longitude, na.rm = TRUE)
lat_range <- range(df_long$Latitude, na.rm = TRUE)

lon_pad <- diff(lon_range) * 0.15  # more zoom-out padding
lat_pad <- diff(lat_range) * 0.15

xlim <- c(lon_range[1] - lon_pad, lon_range[2] + lon_pad)
ylim <- c(lat_range[1] - lat_pad, lat_range[2] + lat_pad)



# --- Save multipage PDF ---
library(forcats)

pdf("current_day_values_map_final.pdf", width = 10, height = 8)

for (v in vars_to_map) {
  
  # 1) Start from the variable subset
  df_var <- df_long %>%
    filter(variable == v) %>%
    # keep only points that will actually plot
    filter(!is.na(Longitude), !is.na(Latitude), !is.na(value)) %>%
    # (optional but robust) only count points inside the map window
    mutate(in_extent = Longitude >= xlim[1] & Longitude <= xlim[2] &
             Latitude  >= ylim[1]  & Latitude  <= ylim[2]) %>%
    filter(in_extent)
  
  # 2) Find events with >= 5 plottable points
  valid_events <- df_var %>%
    count(Sampling_event, name = "n") %>%
    filter(n >= 5) %>%
    pull(Sampling_event)
  
  # Skip this variable if nothing qualifies
  if (length(valid_events) == 0) next
  event_levels <- c('9-23', '11-23', '1-24', '4-24', '8-24', '11-24')
  # 3) Keep only valid events + drop unused levels so facets disappear
  df_plot <- df_var %>%
    filter(Sampling_event %in% valid_events) %>%
    droplevels() %>%
    # (optional) lock a custom order; otherwise it uses the present levels
    mutate(Sampling_event = factor(Sampling_event, levels = event_levels[event_levels %in% Sampling_event]))
  
  p <- ggplot() +
    geom_sf(data = shoreline, fill = "gray85", color = "black") +
    geom_point(data = df_plot,
               aes(x = Longitude, y = Latitude, size = value, color = value),
               alpha = 0.7) +
    scale_size_continuous(range = c(1, 8)) +
    scale_color_viridis_c(option = "plasma") +
    coord_sf(xlim = xlim, ylim = ylim, expand = FALSE) +
    facet_wrap(~Sampling_event, drop = TRUE) +  # <- drop unused facet levels
    labs(
      title = paste(v, "across Sampling Events"),
      size = "Value", color = "Value"
    ) +
    theme_void(base_size = 12) +
    theme(legend.position = "right",
          strip.text = element_text(face = "bold"))
  
  print(p)
}

dev.off()

#Zoomed in map
# --- Compute bounding box of your data & zoom in ---
lon_range <- range(df_long$Longitude, na.rm = TRUE)
lat_range <- range(df_long$Latitude, na.rm = TRUE)

lon_pad <- diff(lon_range) * 0.3  # more zoom-out padding
lat_pad <- diff(lat_range) * 0.3

xlim <- c(lon_range[1] + lon_pad*1.3, lon_range[2] - lon_pad*0.66)
ylim <- c(lat_range[1] + lat_pad*0.8, lat_range[2] - lat_pad*1.4)



# --- Save multipage PDF ---
library(forcats)

pdf("current_day_values_zoomed_final.pdf", width = 10, height = 8)

for (v in vars_to_map) {
  
  # 1) Start from the variable subset
  df_var <- df_long %>%
    filter(variable == v) %>%
    # keep only points that will actually plot
    filter(!is.na(Longitude), !is.na(Latitude), !is.na(value)) %>%
    # (optional but robust) only count points inside the map window
    mutate(in_extent = Longitude >= xlim[1] & Longitude <= xlim[2] &
             Latitude  >= ylim[1]  & Latitude  <= ylim[2]) %>%
    filter(in_extent)
  
  # 2) Find events with >= 5 plottable points
  valid_events <- df_var %>%
    count(Sampling_event, name = "n") %>%
    filter(n >= 5) %>%
    pull(Sampling_event)
  
  # Skip this variable if nothing qualifies
  if (length(valid_events) == 0) next
  event_levels <- c('9-23', '11-23', '1-24', '4-24', '8-24', '11-24')
  # 3) Keep only valid events + drop unused levels so facets disappear
  df_plot <- df_var %>%
    filter(Sampling_event %in% valid_events) %>%
    droplevels() %>%
    # (optional) lock a custom order; otherwise it uses the present levels
    mutate(Sampling_event = factor(Sampling_event, levels = event_levels[event_levels %in% Sampling_event]))
  
  p <- ggplot() +
    geom_sf(data = shoreline, fill = "gray85", color = "black") +
    geom_point(data = df_plot,
               aes(x = Longitude, y = Latitude, size = value, color = value),
               alpha = 0.7) +
    scale_size_continuous(range = c(1, 8)) +
    scale_color_viridis_c(option = "plasma") +
    coord_sf(xlim = xlim, ylim = ylim, expand = FALSE) +
    facet_wrap(~Sampling_event, drop = TRUE) +  # <- drop unused facet levels
    labs(
      title = paste(v, "across Sampling Events"),
      size = "Value", color = "Value"
    ) +
    theme_void(base_size = 12) +
    theme(legend.position = "right",
          strip.text = element_text(face = "bold"))
  
  print(p)
}

dev.off()

#Bar graphs
unique(df_long$Sampling_event)
unique(df_long$variable)
df_long1 <- df_long %>% dplyr::filter(Sampling_event != '11-23')
pdf("variable_barplots.pdf", width = 10, height = 8)

for (v in vars_to_map) {
  message("Processing variable: ", v)
  
  # Summarize across ALL sites: mean + SE per event
  df_summary <- df_long1 %>%
    filter(variable == v) %>%
    group_by(Sampling_event) %>%
    summarise(
      mean_value = mean(value, na.rm = TRUE),
      n = n(),
      se = sd(value, na.rm = TRUE) / sqrt(n),   # Standard Error
      .groups = "drop"
    ) %>%
    filter(n > 0) %>%
    droplevels()

  
  if (nrow(df_summary) == 0) next
  
  # Plot: bar chart with SE error bars (no n labels)
  p <- ggplot(df_summary, aes(x = Sampling_event, y = mean_value, fill = Sampling_event)) +
    geom_col(show.legend = TRUE, width = 0.7) +
    geom_errorbar(
      aes(ymin = mean_value - se, ymax = mean_value + se),
      width = 0.2,
      linewidth = 0.7
    ) +
    scale_fill_manual(
      values = c(
        "9-23" = "darkblue",
        "1-24" = "pink",
        "4-24" = "darkred",
        "8-24" = "lightblue"
      )
    ) +
    labs(
      title = paste("Average", v, "across Sampling Events"),
      x = "Sampling Event",
      y = v,
      fill = "Sampling Event"
    ) +
    theme_minimal(base_size = 14) +
    theme(
      strip.text = element_text(face = "bold"),
      plot.title = element_text(face = "bold", hjust = 0.5)
    )
  
  print(p)
}

dev.off()
unique(df_long$variable)
#time series graphs
names(BW_WQ)
#conche channel
# --- Prep: compute Chlorophyll mean, filter sites, reshape long ---
sites_keep <- c("CON02", "SNB02", "RANKL03")

plot_dat <- BW_WQ %>% mutate(
  # try ISO first, then MM/DD/YYYY if present
  Collection_Date_chr = as.character(Collection_Date),
  d_iso = ymd(Collection_Date_chr, quiet = TRUE),
  d_mdy = mdy(Collection_Date_chr, quiet = TRUE),
  Collection_Date = coalesce(d_iso, d_mdy) %>% as.Date()
) %>%
  filter(!is.na(Collection_Date)) %>%                     # drop bad dates
  select(-Collection_Date_chr, -d_iso, -d_mdy)

# --- Compute Chlorophyll mean, filter sites, reshape
sites_keep <- c("CON02", "SNB02", "RANKL03")

BW_WQ_clean <- BW_WQ %>%
  mutate(
    Collection_Date_chr = as.character(Collection_Date),
    d_iso = suppressWarnings(ymd(Collection_Date_chr)),
    d_mdy = suppressWarnings(mdy(Collection_Date_chr)),
    d_xls = suppressWarnings(as.Date(as.numeric(Collection_Date_chr), origin = "1899-12-30")),
    Collection_Date = coalesce(d_iso, d_mdy, d_xls)
  ) %>%
  select(-Collection_Date_chr, -d_iso, -d_mdy, -d_xls) %>%
  filter(!is.na(Collection_Date)) %>%
  mutate(
    Surface_pH        = suppressWarnings(as.numeric(Surface_pH)),
    Chlorophyll_Rep_A = suppressWarnings(as.numeric(Chlorophyll_Rep_A)),
    Chlorophyll_Rep_B = suppressWarnings(as.numeric(Chlorophyll_Rep_B))
  ) %>% 
  mutate(Chlorophyll_Rep_A = if_else(Chlorophyll_Rep_A > 200, Chlorophyll_Rep_A-200, Chlorophyll_Rep_A), Chlorophyll_Rep_B = if_else(Chlorophyll_Rep_B > 200, Chlorophyll_Rep_B-200, Chlorophyll_Rep_B))

# 2) Compute chlorophyll mean, filter sites, reshape
plot_dat <- BW_WQ_clean %>%
  filter(Site_Name %in% sites_keep) %>%
  mutate(Chlorophyll = rowMeans(cbind(Chlorophyll_Rep_A, Chlorophyll_Rep_B), na.rm = TRUE)) %>%
  select(Site_Name, Collection_Date, Surface_pH, Chlorophyll) %>%
  pivot_longer(c(Surface_pH, Chlorophyll),
               names_to = "variable", values_to = "value") %>%
  mutate(variable = recode(variable,
                           "Surface_pH" = "Surface pH",
                           "Chlorophyll" = "Chlorophyll"))

# Quick sanity check (optional)
stopifnot(inherits(plot_dat$Collection_Date, "Date"))

# 3) Plot
library(patchwork)   # install.packages("patchwork") if needed

plot_one_site <- function(site_id) {
  ggplot(filter(plot_dat, Site_Name == site_id),
         aes(x = Collection_Date, y = value)) +
    geom_line(na.rm = TRUE) +
    labs(title = site_id, x = "Year", y = NULL) +
    scale_x_date(date_breaks = "1 year", date_labels = "%Y", expand = expansion(mult = c(0.01, 0.01))) +
    facet_wrap(~ variable, ncol = 1, scales = "free_y") +
    theme_classic() +
    theme(
      axis.title = element_text(face = "bold"),
      axis.text  = element_text(face = "bold"),
      plot.title = element_text(face = "bold", hjust = 0.5)
    )
}

p_con   <- plot_one_site("CON02")
p_snb   <- plot_one_site("SNB02")
p_rankl <- plot_one_site("RANKL03")
p_rankl

ggsave(plot = p_con, filename = 'plots/CON02_chlph.png', width = 8, height = 3)
ggsave(plot = p_snb, filename = 'plots/SNB02_chlph.png', width = 8, height = 3)
ggsave(plot = p_rankl, filename = 'plots/RANKL03_chlph.png', width = 8, height = 3)

# stack vertically; use | for side-by-side
p_con / p_snb / p_rankl

###Map for visualization
pkgs <- c("readxl","dplyr","sf","maptiles","terra","tidyterra","ggplot2","ggspatial")
to_install <- setdiff(pkgs, rownames(installed.packages()))
if (length(to_install)) install.packages(to_install, dependencies = TRUE)

library(readxl)
library(dplyr)
library(sf)
library(maptiles)
library(terra)
library(tidyterra)   # for geom_spatraster_rgb
library(ggplot2)
library(ggspatial)

sites <- read.csv('Data/BW_Loc.csv')
sites_sf <- sites %>%
  filter(Site_Name %in% c("CON02","SNB02","RANKL03")) %>%
  st_as_sf(coords = c("Longitude","Latitude"), crs = 4326, remove = FALSE)

# ---- 2) Build a padded bounding box for Florida Bay extent ----
pad <- 0.20  # ~0.2 degrees (~20 km); tweak if you want a wider frame
bb <- sf::st_bbox(sites_sf)       # named numeric vector with class "bbox"
bb["xmin"] <- bb["xmin"] - pad
bb["ymin"] <- bb["ymin"] - pad
bb["xmax"] <- bb["xmax"] + pad
bb["ymax"] <- bb["ymax"] + pad

bb_sfc <- sf::st_as_sfc(bb)       # now works; class is intact

# ---- 3) Fetch satellite tiles (Esri World Imagery) ----
# zoom can be adjusted: 10–13 gives bay-scale context. Try 11 or 12 if you want more detail.
sat <- maptiles::get_tiles(
  x = bb_sfc,
  provider = "Esri.WorldImagery",
  zoom = 11,
  crop = TRUE
)
# 'sat' is a SpatRaster with RGB bands

# ---- 4) Plot: satellite background + points + labels + north arrow + scale bar ----
p <- ggplot() +
  tidyterra::geom_spatraster_rgb(data = sat) +
  geom_sf(data = sites_sf, color = "black", size = 2) +
  geom_sf_text(
    data = sites_sf,
    aes(label = Site_Name),
    nudge_y = -0.02,
    vjust = 0.3,              # pushes text slightly below the anchor
    fontface = "bold",
    color = "black"
  ) +
  annotation_scale(location = "bl", width_hint = 0.25, text_cex = 0.8) +
  annotation_north_arrow(location = "tl", which_north = "true",
                         style = north_arrow_fancy_orienteering) +
  coord_sf(xlim = c(bb["xmin"], bb["xmax"]),
           ylim = c(bb["ymin"], bb["ymax"]),
           expand = FALSE) +
  theme_void()

print(p)

ggsave(filename = 'plots/map_BW3sites.png', height = 5, width = 5)
