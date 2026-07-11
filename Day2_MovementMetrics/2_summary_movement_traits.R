## ----setup, include=FALSE-------------------------------------------------------------------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE, message = FALSE, warning = FALSE)


## ----packages-------------------------------------------------------------------------------------------------------------------------------
# install.packages(c("amt", "sf", "tidyverse", "lubridate", "suncalc"))

library(amt)         # Tracks, step/window metrics
library(sf)          # Spatial vector data
library(tidyverse)   # Data wrangling and visualization
library(lubridate)   # Timestamps, floor_date()
library(suncalc)     # Sunrise/sunset times, for diurnality


## ----load-----------------------------------------------------------------------------------------------------------------------------------
load("../Day1_IntroMovementData/Data/WB_clean.rdata")   # loads WB.mv2

latlong_crs <- "EPSG:4326"
utm_crs     <- "EPSG:32737"

WB_df <- WB.mv2 %>%
  mutate(x = sf::st_coordinates(.)[, 1],
         y = sf::st_coordinates(.)[, 2]) %>%
  as_tibble() %>%
  transmute(id = individual_local_identifier, x, y, t = timestamp) %>%
  arrange(id, t)

trk_all <- make_track(WB_df, x, y, t, id = id, crs = latlong_crs, lon = x, lat = y)
trk_all <- transform_coords(trk_all, utm_crs)

head(trk_all)


## ----nest-----------------------------------------------------------------------------------------------------------------------------------
trk_nested <- trk_all %>% nest(data = -id)
trk_nested


## ----calc_disp_fn---------------------------------------------------------------------------------------------------------------------------
calc_disp <- function(track_df, time_unit = c("day", "week")) {
  time_unit <- match.arg(time_unit)

  track_df %>%
    mutate(period = floor_date(t_, unit = time_unit)) %>%
    arrange(t_) %>%
    group_by(period) %>%
    summarise(x_start = first(x_), y_start = first(y_),
              x_end   = last(x_),  y_end   = last(y_), .groups = "drop") %>%
    mutate(displacement = sqrt((x_end - x_start)^2 + (y_end - y_start)^2))
}


## ----daily_disp-----------------------------------------------------------------------------------------------------------------------------
wb_daily_disp_raw <- trk_nested %>%
  mutate(daily = map(data, ~ calc_disp(.x, time_unit = "day"))) %>%
  select(id, daily) %>%
  unnest(daily)

wb_daily_disp <- wb_daily_disp_raw %>%
  group_by(id) %>%
  summarise(mean_daily_disp = mean(displacement, na.rm = TRUE),
            sd_daily_disp   = sd(displacement, na.rm = TRUE))

wb_daily_disp


## ----plot_daily_disp------------------------------------------------------------------------------------------------------------------------
wb_daily_disp_raw %>%
  ggplot(aes(x = id, y = displacement, fill = id)) +
  geom_boxplot(show.legend = FALSE) +
  theme_minimal() +
  labs(title = "Daily displacement by individual",
       x = NULL, y = "Daily displacement (m)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


## ----calc_max_disp_fn-----------------------------------------------------------------------------------------------------------------------
calc_max_disp <- function(track_df, time_unit = c("month", "year")) {
  time_unit <- match.arg(time_unit)

  if (time_unit == "year") {
    coords <- track_df %>% select(x_, y_) %>% as.matrix()
    tibble(max_disp = max(dist(coords), na.rm = TRUE))
  } else {
    df_list <- track_df %>%
      mutate(period = floor_date(t_, unit = time_unit)) %>%
      group_by(period) %>%
      group_split()

    map_dfr(df_list, function(df_period) {
      if (nrow(df_period) < 2) {
        return(tibble(period = df_period$period[1], max_disp = NA_real_))
      }
      coords <- df_period %>% select(x_, y_) %>% as.matrix()
      tibble(period = df_period$period[1], max_disp = max(dist(coords), na.rm = TRUE))
    })
  }
}


## ----monthly_yearly_max_disp----------------------------------------------------------------------------------------------------------------
wb_monthly_max_disp_raw <- trk_nested %>%
  mutate(monthly = map(data, ~ calc_max_disp(.x, time_unit = "month"))) %>%
  select(id, monthly) %>%
  unnest(monthly)

wb_monthly_max_disp <- wb_monthly_max_disp_raw %>%
  group_by(id) %>%
  summarise(mean_max_monthly_disp = mean(max_disp, na.rm = TRUE),
            sd_max_monthly_disp   = sd(max_disp, na.rm = TRUE))

wb_yearly_max_disp <- trk_nested %>%
  mutate(yearly = map(data, ~ calc_max_disp(.x, time_unit = "year"))) %>%
  select(id, yearly) %>%
  unnest(yearly) %>%
  rename(yearly_max_disp = max_disp)

wb_monthly_max_disp
wb_yearly_max_disp


## ----plot_max_disp--------------------------------------------------------------------------------------------------------------------------
wb_monthly_max_disp_raw %>%
  mutate(max_disp_km = max_disp / 1000) %>%
  ggplot(aes(x = period, y = max_disp_km, color = id)) +
  geom_line() +
  geom_point(size = 1.5) +
  scale_x_datetime(date_labels = "%b %Y") +
  theme_minimal() +
  labs(title = "Monthly maximum displacement by individual",
       x = "Month", y = "Maximum displacement within month (km)", color = "Individual") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


## ----one_individual_sun---------------------------------------------------------------------------------------------------------------------
one_trk <- trk_all %>% filter(id == "Sotua")
one_resampled <- track_resample(one_trk, rate = hours(3), tolerance = minutes(30))

sun_times <- suncalc::getSunlightTimes(
  data = data.frame(date = as.Date(one_resampled$t_),
                     lat  = one_resampled$lat,
                     lon  = one_resampled$lon),
  keep = c("sunrise", "sunset"),
  tz   = "UTC"
)

one_fixes <- one_resampled %>%
  mutate(sunrise = sun_times$sunrise,
         sunset  = sun_times$sunset,
         daytime = if_else(t_ >= sunrise & t_ <= sunset, "day", "night"))

one_fixes %>% select(t_, lon, lat, sunrise, sunset, daytime) %>% head()


## ----one_individual_di----------------------------------------------------------------------------------------------------------------------
one_steps <- steps_by_burst(one_resampled)

one_di <- one_steps %>%
  left_join(one_fixes %>% select(t_, daytime), by = c("t1_" = "t_")) %>%
  group_by(daytime) %>%
  summarise(dist_sum = sum(sl_, na.rm = TRUE), n_fix = n(), .groups = "drop") %>%
  pivot_wider(names_from = daytime, values_from = c(dist_sum, n_fix)) %>%
  mutate(diurnality = ((dist_sum_day / n_fix_day) - (dist_sum_night / n_fix_night)) /
                       ((dist_sum_day / n_fix_day) + (dist_sum_night / n_fix_night)))

one_di


## ----calc_diurnality_fn---------------------------------------------------------------------------------------------------------------------
calc_diurnality_one <- function(trk_one) {
  trk_rs   <- track_resample(trk_one, rate = hours(3), tolerance = minutes(30))
  steps_rs <- steps_by_burst(trk_rs)

  sun <- suncalc::getSunlightTimes(
    data = data.frame(date = as.Date(trk_rs$t_), lat = trk_rs$lat, lon = trk_rs$lon),
    keep = c("sunrise", "sunset"), tz = "UTC"
  )

  fixes <- trk_rs %>%
    mutate(sunrise = sun$sunrise, sunset = sun$sunset,
           daytime = if_else(t_ >= sunrise & t_ <= sunset, "day", "night"))

  steps_rs %>%
    left_join(fixes %>% select(t_, daytime), by = c("t1_" = "t_")) %>%
    group_by(daytime) %>%
    summarise(dist_sum = sum(sl_, na.rm = TRUE), n_fix = n(), .groups = "drop") %>%
    pivot_wider(names_from = daytime, values_from = c(dist_sum, n_fix)) %>%
    mutate(diurnality = ((dist_sum_day / n_fix_day) - (dist_sum_night / n_fix_night)) /
                         ((dist_sum_day / n_fix_day) + (dist_sum_night / n_fix_night)))
}

wb_diurnality <- trk_nested %>%
  mutate(di = map(data, calc_diurnality_one)) %>%
  select(id, di) %>%
  unnest(di)

wb_diurnality


## ----plot_diurnality------------------------------------------------------------------------------------------------------------------------
wb_diurnality %>%
  mutate(id = fct_reorder(id, diurnality)) %>%
  ggplot(aes(x = id, y = diurnality, fill = diurnality > 0)) +
  geom_col(show.legend = FALSE) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey40") +
  coord_flip() +
  theme_minimal() +
  labs(title = "Diurnality index by individual",
       x = NULL, y = "Diurnality index (DI)")


## ----build_metrics_df-----------------------------------------------------------------------------------------------------------------------
metrics_list <- list(
  wb_daily_disp,
  wb_monthly_max_disp,
  wb_yearly_max_disp,
  wb_diurnality %>% select(id, diurnality)
)

metrics_df <- reduce(metrics_list, ~ left_join(.x, .y, by = "id"))
metrics_df


## ----scale_metrics--------------------------------------------------------------------------------------------------------------------------
# Standardize to mean 0, unit variance -- without this, yearly_max_disp (in
# meters, often in the tens of thousands) would dominate the distance
# calculation below purely because of its larger numeric scale, not because
# it's ecologically more important than diurnality (bounded -1 to 1).
metrics_mat <- scale(as.matrix(metrics_df %>% select(-id)))
rownames(metrics_mat) <- metrics_df$id


## ----hclust---------------------------------------------------------------------------------------------------------------------------------
dist_mat <- dist(metrics_mat)
hc <- hclust(dist_mat, method = "ward.D2")  # ward.D2 favors compact, similarly-sized clusters

plot(hc, main = "Hierarchical clustering of wildebeest movement traits")


## ----kmeans---------------------------------------------------------------------------------------------------------------------------------
set.seed(123)  # reproducibility
kmeans_result <- kmeans(metrics_mat, centers = 3, nstart = 25)
metrics_df$cluster <- as.factor(kmeans_result$cluster)

# Visualize via PCA projection onto the first two principal components
pca <- prcomp(metrics_mat)
pca_df <- data.frame(pca$x[, 1:2], id = rownames(metrics_mat), cluster = metrics_df$cluster)

ggplot(pca_df, aes(PC1, PC2, color = cluster, label = id)) +
  geom_point(size = 3) +
  geom_text(vjust = 1.5, hjust = 1.2, size = 3) +
  theme_minimal() +
  labs(title = "K-means clustering of wildebeest movement traits (PCA projection)", color = "Cluster")


## ----cluster_means--------------------------------------------------------------------------------------------------------------------------
metrics_df %>%
  group_by(cluster) %>%
  summarise(across(c(mean_daily_disp, sd_daily_disp, mean_max_monthly_disp,
                      sd_max_monthly_disp, yearly_max_disp, diurnality),
                    ~ mean(.x, na.rm = TRUE)),
            n_individuals = n(),
            .groups = "drop")

