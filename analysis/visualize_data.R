# co-plot image data

# read libraries
library(tidyverse)
library(patchwork)
library(raster)

# read in stitched and centered
# time series of Ca XRF returns
df <- readRDS("data/corrected_xrf/corrected_xrf_timeseries.rds")

# convert XRF positions to pixels (pixel scan has higher resolution)
df <- df %>%
  group_by(sample) %>%
  mutate(
    position_bark_pith = position_bark_pith * 47.244094,
    position_pith_bark = position_pith_bark * 47.244094
  )

tifs <- list.files("data/flatbed_scans/shifted/","*.tif", full.names = TRUE)
zpos <- list.files("data/flatbed_scans/","*zpos.txt", full.names = TRUE)

df %>%
  filter(sample == "tw76114") %>%
  do({

    x <- .

    sample <- unique(x$sample)
    tif <- tifs[grep(sample, tolower(basename(tifs)))]
    z <- zpos[grep(sample, tolower(basename(zpos)))]

    # read in position data
    pos <- read.table(
      z,
      header = FALSE,
      sep = ",",
      col.names = "zpos")

    offset <- max(pos$zpos) - max(x$position_pith_bark)

    r <- brick(tif)
    plotRGB(r)
    x$cps <- zoo::rollmean(x$cps, 3, na.pad = TRUE)
    lines(x$position_pith_bark + offset, x$cps * 2000, col = "black")
    #abline(v=pos$zpos, col = "blue")
    abline(h=350, col = "white")

  })

