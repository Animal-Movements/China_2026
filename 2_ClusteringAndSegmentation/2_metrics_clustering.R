library(tidyverse)
library(sf)
library(amt)
library(ggplot2)

load("./Data/wildebeest_data.rdata")

WB <- WB %>%
  as_tibble() %>%
  filter(
    fixType == "3D" & as.numeric(DOP) < 10 | fixType == "2D" & as.numeric(DOP)  < 5)

# summarize at individual level, tracking length and temporal interval 
WB_summary <- WB %>%
  arrange(animal_id, timestamp) %>%
  group_by(animal_id) %>%
  summarize(
    start_time = min(timestamp, na.rm = TRUE),
    end_time = max(timestamp, na.rm = TRUE),
    total_duration_days = as.numeric(difftime(end_time, start_time, units = "days")),
    interval_h = median(as.numeric(diff(timestamp), units = "hours"), na.rm = TRUE),
    n_fixes = n(),
    .groups = "drop"
  )
WB_summary

# some individuals have less than one year amount of data
# to make sure the movement metrics are comparable, we subset the dataset 
# to contain only individuals with at least one year amount of data 
# and subset the data to contain only data between Nov 1st 2010 and Oct 31 2011. 

long_term_ids <- WB_summary %>%
  filter(total_duration_days >= 365) %>%
  pull(animal_id)

WB.one.year <- WB %>%
  filter(animal_id %in% long_term_ids) %>%
  filter(timestamp < mdy("10-31-2011") & timestamp > mdy("11-01-2010") )

# then calculate metrics for all individuals in the one year dataset

wb.trk <- make_track(WB.one.year, 
                     # tell the function which column contains the coordinates
                     .x = longitude, .y = latitude, 
                     # tell the function which CRS the coordinates are based on. 
                     crs = 4326, 
                     .t = timestamp,
                     # any additional information you want to attach to the track
                     # such as animal ID and month
                     id = animal_id, sex = sex, temp = temp) 

wb.trk.nested <- wb.trk %>%
  transform_coords(., 32737) %>% 
  nest(data = -"id")

## calculate all movement metrics 

# daily displacement 
# monthly and annual maximum displacement 
# monthly and annual straightness
# monthly and annual use intensity 
# monthly 95% MCP and annual 95% MCP

wb_daily_disp <- wb.trk.nested %>%
  mutate(daily_disp = map(data, ~ calc_disp_amt(.x, time_unit = "day"))) %>%
  select(id, daily_disp) %>%
  tidyr::unnest(daily_disp) %>%
  group_by(id) %>%
  summarise(ave_daily_disp = mean(displacement, na.rm = T),
            sd_daily_distp = sd(displacement, na.rm = T))

# For monthly max displacement
wb_monthly_max_disp <- wb.trk.nested %>%
  mutate(monthly_disp = map(data, ~calc_max_disp(.x, time_unit = "month"))) %>%
  select(id, monthly_disp) %>%
  unnest(monthly_disp) %>%
  group_by(id) %>%
  summarise(ave_max_monthly_disp = mean(max_disp, na.rm = T),
            sd_max_monthly_disp = sd(max_disp, na.rm = T))

# For yearly max displacement
wb_yearly_max_disp <- wb.trk.nested %>%
  mutate(yearly_disp = map(data, ~calc_max_disp(.x, time_unit = "year"))) %>%
  select(id, yearly_disp) %>%
  unnest(yearly_disp) %>%
  rename(yearly_max_disp = max_disp)

# For monthly straightness
wb_monthly_straightness <- wb.trk.nested %>%
  mutate(monthly_straightness = map(data, ~ calc_straightness(.x, time_unit = "month"))) %>%
  select(id, monthly_straightness) %>%
  tidyr::unnest(monthly_straightness) %>%
  group_by(id) %>%
  summarise(ave_monthly_str = mean(straightness, na.rm = T),
            sd_monthly_str = sd(straightness, na.rm = T))

wb_yearly_straightness <- wb.trk.nested %>%
  mutate(yearly_straightness = map(data, ~ calc_straightness(.x, time_unit = "year"))) %>%
  select(id, yearly_straightness) %>%
  tidyr::unnest(yearly_straightness) %>%
  select(-period) %>%
  rename(yearly_straightness = straightness)

# monthly intensity of use per animal
wb_monthly_intensity <- wb.trk.nested %>%
  mutate(monthly_intensity = map(data, ~ calc_intensity_use(.x, time_unit = "month"))) %>%
  select(id, monthly_intensity) %>%
  tidyr::unnest(monthly_intensity) %>%
  group_by(id) %>%
  summarise(ave_monthly_intensity = mean(intensity_use, na.rm = T),
            sd_monthly_intensity = sd(intensity_use, na.rm = T))

wb_yearly_intensity <- wb.trk.nested %>%
  mutate(annual_intensity = map(data, ~ calc_intensity_use(.x, time_unit = "year"))) %>%
  select(id, annual_intensity) %>%
  tidyr::unnest(annual_intensity) %>%
  select(-period) %>%
  rename(yearly_intensity = intensity_use)

# monthly MCP of use per animal
wb_monthly_mcp <- wb.trk.nested %>%
  mutate(monthly_mcp = map(data, ~ calc_mcp(.x, time_unit = "month"))) %>%
  select(id, monthly_mcp) %>%
  tidyr::unnest(monthly_mcp) %>%
  group_by(id) %>%
  summarise(ave_monthly_mcp = mean(mcp95, na.rm = T),
            sd_monthly_mcp = sd(mcp95, na.rm = T))

wb_yearly_mcp <- wb.trk.nested %>%
  mutate(annual_mcp = map(data, ~ calc_mcp(.x, time_unit = "year"))) %>%
  select(id, annual_mcp) %>%
  tidyr::unnest(annual_mcp)%>%
  select(-period) %>%
  rename(yearly_mcp = mcp95)

## now combine all metrics 

metrics.list <- list(wb_daily_disp,
                     wb_monthly_max_disp,
                     wb_yearly_max_disp,
                     wb_monthly_straightness,
                     wb_yearly_straightness,
                     wb_monthly_intensity,
                     wb_yearly_intensity,
                     wb_monthly_mcp,
                     wb_yearly_mcp) 
metrics.df <- reduce(metrics.list, ~ left_join(.x, .y, by = "id")) %>%
  mutate(across(where(units::is_units), ~ drop_units(.)))   # Remove units (if any) so matrix conversion works cleanly
# rownames(metrics.df) <- as.character(metrics.df$id)

metrics.mat <- scale(as.matrix(metrics.df %>% select(-id)))
rownames(metrics.mat) <- as.character(metrics.df$id)

## ---- Hierarchical Clustering --- ###

# Compute distance and clustering
dist_mat <- dist(metrics.mat , method = "euclidean")   # or use "manhattan", etc.
hc <- hclust(dist_mat, method = "ward.D2")            # clustering method

# Plot the dendrogram
plot(hc, main = "Hierarchical Clustering of Movement Metrics", xlab = "", sub = "")

### --- k-means ----- ###
set.seed(123)  # for reproducibility
kmeans_result <- kmeans(metrics.mat, centers = 3, nstart = 25)  # example with 3 clusters
metrics.df$cluster <- as.factor(kmeans_result$cluster)

# Reduce to 2D with PCA
pca <- prcomp(metrics.mat)
pca_df <- as.data.frame(pca$x[, 1:2])
pca_df$id <- rownames(metrics.mat)
pca_df$cluster <- metrics.df$cluster

ggplot(pca_df, aes(PC1, PC2, color = cluster, label = id)) +
  geom_point(size = 3) +
  geom_text(vjust = 1.5, hjust = 1.2, size = 3) +
  theme_minimal() +
  labs(title = "K-means clustering on movement metrics", color = "Cluster")






# ----- FUNCTIONS --------- #
calc_disp <- function(track_df, time_unit = c("day", "week")) {
  time_unit <- match.arg(time_unit)
  
  track_df %>%
    mutate(period = floor_date(t_, unit = time_unit)) %>%
    arrange(t_) %>%
    group_by(period) %>%
    summarise(
      x_start = first(x_),
      y_start = first(y_),
      x_end   = last(x_),
      y_end   = last(y_),
      .groups = "drop"
    ) %>%
    mutate(
      displacement = sqrt((x_end - x_start)^2 + (y_end - y_start)^2)
    )
}

# calc maximum displacement monthly and annually 
calc_max_disp <- function(track_df, time_unit = c("month", "year")) {

  if (time_unit == "year") {
    coords <- track_df %>% select(x_, y_) %>% as.matrix()
    # Get all pairwise distances efficiently
    dists <- dist(coords)
    max_dist <- max(dists, na.rm = TRUE)
    
    tibble(
      max_disp = max_dist
    )
  } else {
    
    df <- track_df %>%
      mutate(period = floor_date(t_, unit = time_unit)) %>%
      group_by(period) %>%
      group_split()
    
    map_dfr(df, function(df_period) {
      n <- nrow(df_period)
      if (n < 2) return(tibble(period = df_period$period[1], max_disp = NA_real_))
      
      coords <- df_period %>% select(x_, y_) %>% as.matrix()
      
      # Get all pairwise distances efficiently
      dists <- dist(coords)
      max_dist <- max(dists, na.rm = TRUE)
      
      tibble(
        max_disp = max_dist
      )
    })
  }
}

# calculate monthly or yearly straightness
calc_straightness <- function(track_df, time_unit = c("month", "year")) {
  time_unit <- match.arg(time_unit)
  
  if (time_unit == "year") {
    # Calculate straightness once over all points
    straight <- straightness(track_df)
    return(tibble(period = "all_year", straightness = straight))
  } else {
    track_df <- track_df %>%
      mutate(period = floor_date(t_, unit = time_unit)) %>%
      arrange(t_)
    
    periods <- unique(track_df$period)
    
    map_dfr(periods, function(p) {
      track_period <- filter(track_df, period == p)
      
      straight <- straightness(track_period)
      
      tibble(period = p, straightness = straight)
    })
  } 
  
}

# calculate monthly or yearly intensity of use 
calc_intensity_use <- function(track_df, time_unit = c("month", "year")) {
  time_unit <- match.arg(time_unit)
  
  if (time_unit == "year") {
    # Calculate intensity_use once over all points
    intensity <- intensity_use(track_df)
    return(tibble(period = "all_year", intensity_use = intensity))
  } else {
    track_df <- track_df %>%
      mutate(period = floor_date(t_, unit = time_unit)) %>%
      arrange(t_)
    
    periods <- unique(track_df$period)
    
    map_dfr(periods, function(p) {
      track_period <- filter(track_df, period == p)
      
      intensity <- intensity_use(track_period)
      
      tibble(period = p, intensity_use = intensity)
    })
  } 
}


# calculate monthly intensity of use 
calc_mcp <- function(track_df, time_unit = c("month", "year")) {
  time_unit <- match.arg(time_unit)
  
  if (time_unit == "year") {
    # Calculate intensity_use once over all points
    mcp95 <- hr_mcp(track_df, levels = 0.95)$mcp$area
    return(tibble(period = "all_year", mcp95 = mcp95))
  } else {
    track_df <- track_df %>%
      mutate(period = floor_date(t_, unit = time_unit)) %>%
      arrange(t_)
    
    periods <- unique(track_df$period)
    
    map_dfr(periods, function(p) {
      track_period <- filter(track_df, period == p)
      
      if (nrow(track_period) < 2) {
        return(tibble(period = p, intensity_use = NA_real_))
      }
      
      mcp95 <- hr_mcp(track_period, levels = 0.95)$mcp$area
      
      tibble(period = p, mcp95 = mcp95)
    })
  }
}
