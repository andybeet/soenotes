## Creates comland data in vignette since raw data is private

library(magrittr)

comlandData <- dplyr::as_tibble(readRDS(here::here("data",paste0("comland_meatwt_deflated_EPU_2020.Rds"))))

comlandSep <- comlandData %>%
  dplyr::filter(EPU %in% c("GB","GOM","MAB"))  %>% # US and NAFO landings
  dplyr::rename(Year = YEAR) %>%
  dplyr::group_by(Year,US) %>%
  dplyr::summarise(Landings = sum(SPPLIVMT), .groups="drop") %>%
  dplyr::rename(Source = US) %>%
  dplyr::mutate(Source = dplyr::case_when(Source==T ~ "USA",TRUE ~ "Foreign"))

usethis::use_data(comlandSep)
