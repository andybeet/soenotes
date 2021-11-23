#' creates publicly available data set from cfdbs for NEUS wide landings of
#' menhaden very abundant in NAFO US landings
#'
#'
library(magrittr)
comlandData <- dplyr::as_tibble(readRDS(here::here("data",paste0("comland_raw_US_meatwt.Rds"))))

comlandData <- comlandData %>%
  dplyr::mutate(AREA = levels(AREA)[AREA]) %>%
  dplyr::mutate(AREA = dplyr::case_when(AREA %in% c("OFF","OFR") ~ "000",
                                        TRUE ~ AREA)) %>%
  dplyr::mutate(AREA = as.numeric(AREA))

comlandData %>%
  dplyr::filter(!(AREA %in% c(comlandr::EPUs$GB$statAreas,comlandr::EPUs$GOM$statAreas,comlandr::EPUs$MAB$statAreas)) ) %>%
  dplyr::distinct(AREA) %>%
  dplyr::arrange(AREA) %>%
  dplyr::pull(AREA)

convToMT <- 0.00045359237
menhaden <- comlandData %>%
  dplyr::filter(NESPP3 == 221) %>%
  dplyr::mutate(EPU = dplyr::case_when(AREA %in% comlandr::EPUs$GB$statAreas ~ "GB",
                                       AREA %in% comlandr::EPUs$MAB$statAreas ~ "MAB",
                                       AREA %in% comlandr::EPUs$GOM$statAreas ~ "GOM",
                                       AREA > 632 ~ "SMAB",
                                       AREA < 500 ~ "NGOM",
                                       TRUE ~ "other")) %>%
  dplyr::group_by(YEAR,EPU) %>%
  dplyr::summarise(Landings = sum(SPPLIVLB)*convToMT, .groups="drop") %>%
  dplyr::mutate(YEAR = as.numeric(YEAR)) %>%
  dplyr::mutate(Year = dplyr::case_when(YEAR<100 ~ YEAR+1900,
                                        TRUE ~ YEAR)) %>%
  dplyr::select(-YEAR)

usethis::use_data(menhaden,overwrite = T)
