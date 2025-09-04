#Field correlations
library(dplyr)
library(stringr)
library(purrr)
library(readr)        # for parse_number()
library(lme4)
library(lmerTest)     # p-values for fixed effects in lmer()
library(performance) 


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
    select(Site_Name, Surface_pH, !!sym(pred_name)) %>%
    mutate(x_raw = .data[[pred_name]],
           x = if (is.character(x_raw)) parse_number(x_raw) else as.numeric(x_raw)) %>%
    select(-x_raw) %>%
    filter(is.finite(Surface_pH), is.finite(x))
  
  n_obs   <- nrow(dat)
  n_sites <- dplyr::n_distinct(dat$Site_Name)
  if (n_obs < 10 || n_sites < 2) {
    return(tibble(
      predictor = pred_name, n = n_obs, n_sites = n_sites,
      beta = NA_real_, se = NA_real_, t_or_z = NA_real_, p_value = NA_real_,
      R2_marginal = NA_real_, R2_conditional = NA_real_
    ))
  }
  
  # LMM: Surface_pH ~ predictor + (1|Site_Name)
  mod <- tryCatch(
    lmer(Surface_pH ~ x + (1 | Site_Name), data = dat, REML = FALSE),
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
      Surface_pH  = as.numeric(Surface_pH),
      x_raw       = .data[[pred]]
    ) %>%
    mutate(
      x = if (is.character(x_raw)) readr::parse_number(x_raw) else as.numeric(x_raw),
      predictor = pred
    ) %>%
    filter(is.finite(Surface_pH), is.finite(x)) %>%
    select(-x_raw)
}

plot_dat <- bind_rows(map(preds_to_plot, ~ prep_pred(BW_all, .x)))

# 2) Fit one LMM per predictor: Surface_pH ~ x + (1|Site_Name)
mods <- map(preds_to_plot, ~ lmer(Surface_pH ~ x + (1 | Site_Name),
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
p <- ggplot(plot_dat, aes(x = x, y = Surface_pH)) +
  geom_point(alpha = 0.5, size = 1) +
  geom_line(data = pred_lines, aes(y = yhat), linewidth = 1.1) +
  facet_wrap(~ predictor, scales = "free_x", ncol = 2) +
  theme_classic() +
  labs(
    x = "NH4",
    y = "Surface pH",
    title = "Surface pH vs NH4"
  )

# (optional) add slope/p labels in each facet
p + geom_text(
  data = lab_tbl,
  inherit.aes = FALSE,
  aes(x = -Inf, y = Inf, label = label),
  hjust = -0.05, vjust = 1.1, size = 3
)
