#' creates publicly available data set from cfdbs for NEUS wide landings of
#' species very abundant in NAFO US landings
library(magrittr)
comlandData <- dplyr::as_tibble(readRDS(here::here("data",paste0("comland_raw_US_meatwt.Rds"))))

comlandData <- comlandData %>%
  dplyr::mutate(AREA = levels(AREA)[AREA]) %>%
  dplyr::mutate(AREA = dplyr::case_when(AREA %in% c("OFF","OFR") ~ "000",
                                        TRUE ~ AREA)) %>%
  dplyr::mutate(AREA = as.numeric(AREA))

convToMT <- 0.00045359237
species <- data.frame(speciesNames=c("Atlantic Menhaden","American Cupped Oyster","Blue Crab",
                                     "Hard Clam","Ocean Quahog","Sea Scallop","Surf Clam"),
                      NESPP3 = c(221,789,700,748,754,800,769))
## other species from NAFO

otherSpecies <- comlandData %>%
  dplyr::filter(NESPP3 %in% species$NESPP3) %>%
  dplyr::filter(AREA %in% c(comlandr::EPUs$GB$statAreas,
                            comlandr::EPUs$MAB$statAreas,
                            comlandr::EPUs$GOM$statAreas)) %>%
  dplyr::left_join(.,species,by="NESPP3") %>%
  dplyr::group_by(YEAR,speciesNames) %>%
  dplyr::summarise(Landings = sum(SPPLIVLB)*convToMT,.groups="drop")  %>%
  dplyr::mutate(YEAR = as.numeric(YEAR)) %>%
  dplyr::mutate(Year = dplyr::case_when(YEAR<100 ~ YEAR+1900,
                                        TRUE ~ YEAR)) %>%
  dplyr::select(-YEAR)

usethis::use_data(otherSpecies,overwrite=T)

# * Atlantic Menhaden (221)
# * American Cupped Oyster (CRASSOSTREA VIRGINICA, Eatern Oyster, 789)
# * Blue Crab (700)
# * Hard Clam (MERCENARIA MERCENARIA, Northern Clam, 748)
# * Ocean Quahog (754)
# * Sea Scallop (800)
# * Surf clam (769)
