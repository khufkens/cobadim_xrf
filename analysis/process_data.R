# Process data for dplr analysis by Tom de Mil

# read libraries
library(tidyverse)
library(patchwork)

# read in stitched and centered
# time series of Ca XRF returns
df <- readRDS("data/corrected_xrf/corrected_xrf_timeseries.rds")

# convert XRF positions to pixels (pixel scan has higher resolution)
df <- df %>%
  group_by(sample) %>%
  mutate(
    position_bark_pith = position_bark_pith * 47.244094,
    stitchline_bark_pith = stitchline_bark_pith * 47.244094
  )

# read in ring widths as marked on the images
# as pixel locations
zpos <- list.files("data/flatbed_scans/","*zpos.txt", full.names = TRUE)

rw <- lapply(zpos, function(file){

  # read in position data
  pos <- read.table(
    file,
    header = FALSE,
    sep = ",",
    col.names = "zpos")

  # read sample name from filename
  sample <- tolower(substr(basename(file),1,7))

  return(
    data.frame(
    'sample' = sample,
    'zpos' = pos
  ))
})

# bind rows
rw <- bind_rows(rw)

# convert zpos into bark to pith order
# flipping
rw <- rw %>%
  filter( zpos > 0 ) %>%
  group_by(sample) %>%
  mutate(
    zpos = max(zpos) - zpos
  ) %>%
  arrange(sample, zpos)

# set start end locations
# of sections
rw <- rw %>%
  mutate(
    start_distance = zpos,
    end_distance = c(zpos[-1], NA),
    year = seq(2015, 2015 - length(zpos[-1]), by = -1)
  ) %>%
  na.omit()

# slice by year
rw_stats <- rw %>%
  group_by(sample, year) %>%
  do({

    x <- .

    ss <- df %>%
      filter(
        sample == x$sample,
        position_bark_pith >= x$start_distance,
        position_bark_pith < x$end_distance
      )

    if(nrow(ss) == 0){
      data.frame(
        cps_mean = NA,
        cps_max = NA,
        cps_min = NA
      )
    } else {
      ss %>%
        summarize(
          cps_mean = mean(cps),
          cps_max = max(cps),
          cps_min = min(cps)
        )
    }
  }) %>%
  na.omit() %>%
  filter(
    year > 1980
  )

rw_stats_wide <- rw_stats %>%
  pivot_longer(
    names_to = "statistic",
    cols = starts_with("cps")
  ) %>%
  group_by(statistic) %>%
  do({
    x <- .
    x <- x %>%
    pivot_wider(
      names_from = sample,
      values_from = value
    )
  })

p1 <- ggplot() +
  geom_line(
    data = df,
    aes(
      position_bark_pith,
      cps
    )
  ) +
  geom_vline(
    data = rw,
    aes(
      xintercept = start_distance
    )
  ) +
  geom_vline(
    data = df,
    aes(
      xintercept = stitchline_bark_pith
    ),
    colour =  "red"
  ) +
  facet_grid(sample~.)

plot(p1)

# save data to rds file
saveRDS(rw_stats_wide, "data/dplr_data.rds")

