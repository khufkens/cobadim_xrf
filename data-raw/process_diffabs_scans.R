# download library from bluegreen-labs repo
# devtools::install_github("bluegreen-labs/diffabsr")
library(diffabsr)

# diffabs settings
rois <- list(
  ca = c(360, 380),
  fe = c(620, 660),
  cu = c(775, 825),
  sr = c(1390, 1430),
  ti = c(440, 460)
)

# channel position data settings
channel_positions <- list(
  channel_0 = data.frame(
    ICR = 39,
    OCR = 43,
    s = 27
  ),
  channel_1 = data.frame(
    ICR = 40,
    OCR = 44,
    s = 28
  ),
  channel_2 = data.frame(
    ICR = 41,
    OCR = 45,
    s = 29
  ),
  channel_3 = data.frame(
    ICR = 42,
    OCR = 46,
    s = 30
  )
)

# read in samples to process
samples <- read.table("data/soleil_samples_meta-data.csv",
                      header = TRUE,
                      sep = ",")

# loop over all samples and process the
# diffabs scan data
series <- apply(samples, 1, function(x){

  # read in data, and process
  spectra <- diffabs_spectra(
    file = file.path("data-raw/scans/", sprintf("scan_%04d_0001.nxs",
                                                as.numeric(x['file_number']))),
    channels = channel_positions)

  # get elemental densities for all ROIs (i.e. elements) specified
  elements <- diffabs_elements(rois = rois,
                               spectra = spectra)

  # convert the nested list data to tidy data
  tidy_elements <- diffabs_tidy(elements)

  # add meta-data
  tidy_elements$species <- x['species']
  tidy_elements$section <- x['section']
  tidy_elements$sample <- x['sample']
  tidy_elements$file_number <- x['file_number']

  return(tidy_elements)
})

# bind list into tidy dataframe
series <- do.call("rbind", series)

# write to disk as clean data
saveRDS(series, "data/diffabs_time_series.rds")


