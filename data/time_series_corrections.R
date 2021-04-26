# preprocessing all data, combining time series
library(tidyverse)
library(patchwork)
options(scipen = 9)
source("R/combine_sections.R")

# settings including the stitch offsets
# between sections and the stitch thresholds to
# set NA values for low values caused by the gaps
# between sections (which will then be removed)
# general thresholds remove low values at the
# start and end of the time series
# stitch location start and end values allow for
# more fine grained control over setting values
# to NA along the stitch line (i.e. higher values)
settings <- data.frame(
  sample = c("tw76114","tw76112","tw76109","tw76107","tw76104"),
  stitch_offset = c(153.4,153.2,225.2,244.2, 248.8),
  stitch_threshold = c(0.0015,0.007, 0.0012,0.01, 0.002),
  general_threshold = c(0.0005, 0.005, 0.001,0.001, 0.002),
  stitch_location_start = c(300,300,300, 1, 1),
  stitch_location_end = c(400,400,400, 600, 600)
)

# read in diffabs data only select Pericopsis elata
# and ca time series (settings above are element
# specific with respect to the thresholds)
df <- readRDS("data/diffabs_time_series.rds") %>%
  filter(element == "ca",
         species == "Pericopsis elata")

# join data with settings
df <- left_join(df, settings)

# process the data
values <- df %>%
   group_by(sample) %>%
   do({
     combine_sections(.)
   })

# save values
saveRDS(values,"data/corrected_xrf/corrected_xrf_timeseries.rds")
