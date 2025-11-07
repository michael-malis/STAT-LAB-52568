# ---- Libraries ----
library(tidyverse)
library(readr)
library(cowplot)   # for save_plot()

# ---- Global theme ----
theme_set(theme_minimal(base_size = 12))

# ---- Helper function ----
# Save ggplot as JPEG
save_plot_jpg <- function(p, name, width = 8, height = 5, dpi = 300) {
  dir.create("outputs", showWarnings = FALSE)
  path <- file.path("outputs", paste0(name, ".jpeg"))
  cowplot::save_plot(filename = path, plot = p, base_width = width, base_height = height, dpi = dpi)
  message("Saved: ", path)
}

# Confidence interval for binomial proportion (Wald ±2*SE)
accept_p <- function(p, n) {
  se <- sqrt(p * (1 - p) / n)
  tibble(Lower_CI = p - 2 * se, Upper_CI = p + 2 * se)
}

# Confidence interval for binomial proportion (Wald ±2*SE)
accept_p <- function(p, n) {
  se <- sqrt(p * (1 - p) / n)
  tibble(Lower_CI = p - 2 * se, Upper_CI = p + 2 * se)
}

# ===============================
# Step 1: Load data
# ===============================
raw_data <- read_csv("D:/_HUJI/_Year 3/STATISTICS LAB/rawdata.csv", show_col_types = FALSE)

# ===============================
# Step 2: Basic EDA on gestational weeks
# ===============================
eda_weeks <- raw_data %>%
  mutate(week_floor = floor(pregweek)) %>%
  count(week_floor, name = "n") %>%
  mutate(prop_pct = round(100 * n / sum(n), 3))

p_week_count <- ggplot(eda_weeks, aes(x = factor(week_floor), y = n)) +
  geom_col(fill = "steelblue") +
  labs(title = "Number of Observations by Gestational Week", x = "Gestational Week", y = "Count") +
  theme_minimal()

save_plot_jpg(p_week_count, "01_week_count")

p_week_prop <- ggplot(eda_weeks, aes(x = factor(week_floor), y = prop_pct)) +
  geom_col(fill = "steelblue") +
  labs(title = "Percentage of Observations by Gestational Week", x = "Gestational Week", y = "Percentage (%)") +
  theme_minimal()

save_plot_jpg(p_week_prop, "02_week_percentage")

# ===============================
# Step 3: Unique IDs and pregnancies
# ===============================
data40 <- raw_data %>% filter(pregweek < 41)
ids_tbl <- data40 %>% mutate(preg_id = snumber + expected / 100000)
n_unique_women <- ids_tbl %>% summarise(n = n_distinct(snumber)) %>% pull(n)
n_unique_preg  <- ids_tbl %>% summarise(n = n_distinct(preg_id)) %>% pull(n)
cat("Number of unique women:", n_unique_women, "\n")
cat("Number of unique pregnancies:", n_unique_preg, "\n")

# ===============================
# Step 4: Missingness in ab_cir
# ===============================
box_df <- data40 %>% mutate(ab_cir_missing = if_else(is.na(ab_cir), "Missing", "Observed"))
p_box <- ggplot(box_df, aes(x = ab_cir_missing, y = pregweek)) +
  geom_boxplot(outlier.alpha = 0.4, fill = "gray80") +
  labs(title = "Pregnancy Week by Missingness in ab_cir", x = "ab_cir Missing?", y = "Pregnancy Week (weeks)") +
  theme_minimal()

save_plot_jpg(p_box, "03_pregweek_by_ab_cir_missing")

week_summary <- data40 %>%
  mutate(week_floor = floor(pregweek)) %>%
  group_by(week_floor) %>%
  summarise(total = n(), non_missing = sum(!is.na(ab_cir)), missing = sum(is.na(ab_cir)), .groups = "drop") %>%
  mutate(missing_pct = round(100 * missing / total, 2))

p_missing_count <- ggplot(week_summary, aes(x = factor(week_floor), y = missing)) +
  geom_col(fill = "coral3") +
  labs(title = "Number of Missing ab_cir by Week", x = "Week", y = "Missing Count") +
  theme_minimal()

save_plot_jpg(p_missing_count, "04_missing_count_by_week")

p_missing_pct <- ggplot(week_summary, aes(x = factor(week_floor), y = missing_pct)) +
  geom_col(fill = "darkorange3") +
  labs(title = "Percentage of Missing ab_cir by Week", x = "Week", y = "Missing (%)") +
  theme_minimal()

save_plot_jpg(p_missing_pct, "05_missing_percent_by_week")

# ===============================
# Step 5: Clean AC values and convert units
# ===============================
clean_df <- data40 %>%
  filter(pregweek >= 19) %>%
  mutate(ab_cir_clean = case_when(ab_cir > 1000 ~ NA_real_, ab_cir > 0 & ab_cir <= 50 ~ ab_cir * 10, TRUE ~ ab_cir)) %>%
  filter(!is.na(ab_cir_clean)) %>%
  rename(ga_week = pregweek, ac_mm = ab_cir_clean)

p_ac_all <- ggplot(clean_df, aes(x = ga_week, y = ac_mm)) +
  geom_point(alpha = 0.5, size = 0.7, color = "darkgreen") +
  labs(title = "Abdominal Circumference vs Gestational Week", x = "Gestational Week", y = "AC (mm)") +
  theme_minimal()

save_plot_jpg(p_ac_all, "06_ac_vs_ga_clean")

# ===============================
# Step 6: Hadlock model (mean ± 3SD)
# ===============================

hadlock_df <- clean_df %>%
  mutate(
    mean_h = 10 * (-13.3 + 1.61 * ga_week - 0.00998 * ga_week^2),
    sd_h   = 13.4,
    z_h    = (ac_mm - mean_h) / sd_h
  )

x_grid <- tibble(ga_week = seq(min(clean_df$ga_week), max(clean_df$ga_week), length.out = 500)) %>%
  mutate(
    mean_h = 10 * (-13.3 + 1.61 * ga_week - 0.00998 * ga_week^2),
    sd_h   = 13.4,
    low_h  = mean_h - 3 * sd_h,
    up_h   = mean_h + 3 * sd_h
  )

p_hadlock <- ggplot(clean_df, aes(x = ga_week, y = ac_mm)) +
  geom_point(alpha = 0.5, size = 0.7, color = "gray50") +
  geom_line(data = x_grid, aes(y = mean_h), color = "tomato2", linewidth = 0.7) +
  geom_line(data = x_grid, aes(y = low_h), color = "tomato2", linetype = "dashed", linewidth = 0.7) +
  geom_line(data = x_grid, aes(y = up_h), color = "tomato2", linetype = "dashed", linewidth = 0.7) +
  labs(
    title = "Observed AC (mm) vs Hadlock Mean ± 3 SD",
    x     = "Gestational Age (weeks)",
    y     = "AC (mm)"
  ) +
  theme_minimal()

save_plot_jpg(p_hadlock, "07_hadlock_pm3sd")

# ===============================
# Step 7: K model (mean & SD)
# ===============================

k_df <- clean_df %>%
  mutate(
    mean_k = -89.39 + 12.03 * ga_week - 0.000863 * (ga_week^3),
    sd_k   = 1.179 + 0.4753 * ga_week,
    z_k    = (ac_mm - mean_k) / sd_k
  )


# ===============================
# Step 8: Tail probabilities tables (Hadlock & K)
# ===============================

q_vec <- c(0.03, 0.05, 0.10, 0.95, 0.975)
z_cut <- qnorm(q_vec)
n_h   <- nrow(hadlock_df)
n_k   <- nrow(k_df)


# ===============================
# Step 9: Standardized residual plots with percentile lines (Hadlock & K)
# ===============================

percentiles_df <- tibble(
  label = c("z = 0", "3%", "5%", "10%", "95%", "97.5%"),
  q     = c(0.5, 0.03, 0.05, 0.10, 0.95, 0.975),
  z     = qnorm(q),
  color = c("red", "darkorange", "gold", "chartreuse3", "dodgerblue3", "blueviolet")
)

p_z_h <- ggplot(hadlock_df, aes(x = ga_week, y = z_h)) +
  geom_point(alpha = 0.3, size = 0.6, color = "gray20") +
  geom_hline(
    data = percentiles_df,
    aes(yintercept = z, color = label, linetype = label),
    linewidth = 0.9
  ) +
  scale_color_manual(values = setNames(percentiles_df$color, percentiles_df$label)) +
  scale_linetype_manual(values = c(
    "z = 0"   = "solid",
    "3%"      = "longdash",
    "5%"      = "longdash",
    "10%"     = "longdash",
    "95%"     = "longdash",
    "97.5%"   = "longdash"
  )) +
  coord_cartesian(ylim = c(-4, 4)) +
  labs(
    title  = "Standardized Residuals (Hadlock z-scores) vs Gestational Age",
    x      = "Gestational Age (weeks)",
    y      = "z-score (Hadlock)",
    color  = "Percentiles",
    linetype = "Percentiles"
  ) +
  theme_minimal()

save_plot_jpg(p_z_h, "08_z_scores_hadlock")


p_z_k <- ggplot(k_df, aes(x = ga_week, y = z_k)) +
  geom_point(alpha = 0.3, size = 0.6, color = "gray20") +
  geom_hline(
    data = percentiles_df,
    aes(yintercept = z, color = label, linetype = label),
    linewidth = 0.9
  ) +
  scale_color_manual(values = setNames(percentiles_df$color, percentiles_df$label)) +
  scale_linetype_manual(values = c(
    "z = 0"   = "solid",
    "3%"      = "longdash",
    "5%"      = "longdash",
    "10%"     = "longdash",
    "95%"     = "longdash",
    "97.5%"   = "longdash"
  )) +
  coord_cartesian(ylim = c(-4, 4)) +
  labs(
    title  = "Standardized Residuals (K-model z-scores) vs Gestational Age",
    x      = "Gestational Age (weeks)",
    y      = "z-score (K model)",
    color  = "Percentiles",
    linetype = "Percentiles"
  ) +
  theme_minimal()

save_plot_jpg(p_z_k, "09_z_scores_k_model")
