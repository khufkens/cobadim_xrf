# function which deals with combining
# two sections as scanned on diffabs
# adjusts for offsets and cuts out
# dips in the signal due to gaps
# between the series
#
# This analysis uses visually derived
# fits / parameters
#
# returned values are centered on 0.5
# with generally a range between 0 and 1

combine_sections <- function(
  df,
  plot = FALSE
){

  df1 <- df %>%
    ungroup() %>%
    group_by(section) %>%
    mutate(
      position = ifelse(section == 2,
                        stitch_offset + position, position),
    )

  # fill in the stitch gap
  df2 <- df1 %>%
    mutate(
      cps = ifelse(cps < df$stitch_threshold[1] &
                  position > df$stitch_location_start[1] &
                  position < df$stitch_location_end[1], NA, cps),
      cps = ifelse(cps < df$general_threshold[1], NA, cps)
    ) %>%
    na.omit()

  df2 <- df2 %>%
    ungroup() %>%
    group_by(position, element, species, sample) %>%
    summarize(
      cps = mean(cps, na.rm = TRUE),
      section = max(section, na.rm = TRUE)
    ) %>%
    arrange(element, position) %>%
    ungroup()

  df2 <- df2 %>%
    mutate(
      cps = scales::rescale(cps, to = c(0,1)), # rescale to ~same dynamic range
      cps = cps + (0.5 - median(cps, na.rm = TRUE)), # center on 0.5
    )

  df2 <- df2 %>%
    ungroup() %>%
    arrange(position) %>%
    mutate(
      position = 0:(length(position)-1) * 0.2
    )

  df2 <- df2 %>%
    rename(
      `position_pith_bark` = `position`
    ) %>%
    mutate(
      position_bark_pith = rev(position_pith_bark),
      stitchline_bark_pith = min(position_bark_pith[section == 1]),
      stitchline_pith_bark = max(position_pith_bark[section == 1])
    )

  if(plot){
    # some visual feedback
    p1 <- ggplot(df1) +
      geom_line(
        aes(
          position,
          cps,
          colour = section
        )
      ) +
      labs(
        x = "mm",
        title = sprintf("%s - %s", df$species[1],df$sample[1])
      ) +
      theme_minimal()

    print(head(df2))
    p2 <- ggplot(df2) +
      geom_line(
        aes(
          position_bark_pith,
          cps,
          colour = section
        )
      ) +
      labs(
        x = "mm"
      ) +
      #xlim(c(300,400)) +
      geom_vline(
        data = df2,
        aes(
          xintercept = stitchline_bark_pith)
      ) +
      theme_minimal()

    print(p1/p2)
  }

  return(df2)
}

