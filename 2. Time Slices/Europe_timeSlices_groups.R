library(tidyverse)
library(kableExtra)
library(ggplot2)
library(scatterpie)
library(rnaturalearth)
library(ggrepel) 

# =============================================================================
# Functions
# =============================================================================
# function for computing the allele frequencies
compute.allele.freq <- function(genotypes)
{
  return(1-(sum(genotypes)/(2*length(genotypes))))
}

# =============================================================================
# Input Data
# =============================================================================
individuals.data  <- tibble::as_tibble(read.csv(file = "0. Input Data/individuals_data.csv")) %>% dplyr::select(-1)
# time slice 1: 0 - 3000 BP
europe.ts.1 <- dplyr::filter(individuals.data, individuals.data$date > 0 & individuals.data$date <= 3000) %>% filter(continent == "Europe") %>% select(c(1:11))
europe.ts.1.pnpla3 <- dplyr::filter(europe.ts.1, europe.ts.1$pnpla3 != 9)
# time slice 2: 2000 - 5000 BP
europe.ts.2 <- dplyr::filter(individuals.data, individuals.data$date > 3000 & individuals.data$date <= 7000) %>% filter(continent == "Europe") %>% select(c(1:11))
europe.ts.2.pnpla3 <- dplyr::filter(europe.ts.2, europe.ts.2$pnpla3 != 9)
# time slice 3: 5000 - 15000 BP
europe.ts.3 <- dplyr::filter(individuals.data, individuals.data$date > 7000 & individuals.data$date <= 15000) %>% filter(continent == "Europe") %>% select(c(1:11))
europe.ts.3.pnpla3 <- dplyr::filter(europe.ts.3, europe.ts.3$pnpla3 != 9)

# =============================================================================
# Time Slice 1
# =============================================================================
countries.group.pnpla3 <- c("Bulgaria, Serbia",
                            "Croatia, Czech Republic, Germany, Hungary, Poland, Slovakia, Switzerland",
                            "Estonia, Finland, Latvia",
                            "Canary Islands, France, Spain",
                            "Icealand",
                            "Italy",
                            "Ireland, United Kingdom",
                            "Denmark, Norway, Sweden",
                            "Moldova Ukraine")
# ind -> country summarization
europe.ts.1.pnpla3$subregion <- as.character(europe.ts.1.pnpla3$subregion)
europe.ts.1.pnpla3.country   <- dplyr::group_by(europe.ts.1.pnpla3, country) %>% summarise(individuals = n(),
                                                                                           subregion   = max(subregion),
                                                                                           mean.ycoord = mean(ycoord),
                                                                                           mean.xcoord = mean(xcoord),
                                                                                           date        = round(mean(date)),
                                                                                           group       = mean(group),
                                                                                           pnpla3      = compute.allele.freq(pnpla3))
# weighting for group allele frequencies
country.pnpla3.tmp        <- europe.ts.1.pnpla3.country
country.pnpla3.tmp$pnpla3 <- europe.ts.1.pnpla3.country$pnpla3 * europe.ts.1.pnpla3.country$individuals
# group summarization
europe.ts.1.pnpla3.group <- dplyr::group_by(country.pnpla3.tmp, group) %>% summarise(subregion   = max(subregion),
                                                                                     individuals = sum(individuals),
                                                                                     mean.ycoord = mean(mean.ycoord),
                                                                                     mean.xcoord = mean(mean.xcoord),
                                                                                     date        = round(mean(date)),
                                                                                     pnpla3      = sum(pnpla3)/individuals)
europe.ts.1.pnpla3.group <- tibble::add_column(europe.ts.1.pnpla3.group, countries = countries.group.pnpla3, .before = "subregion")
# create group kable table
group.table.data.ts1.pnpla3        <- dplyr::ungroup(europe.ts.1.pnpla3.group)
group.table.data.ts1.pnpla3        <- data.table::setorder(group.table.data.ts1.pnpla3, subregion)
group.table.data.ts1.pnpla3$pnpla3 <- round(group.table.data.ts1.pnpla3$pnpla3, digits = 4)
group.table.data.ts1.pnpla3 <- dplyr::select(group.table.data.ts1.pnpla3, -c(3,5,6))

# =============================================================================
# Time Slice 2
# =============================================================================
countries.group.pnpla3 <- c("Bulgaria, Greece, Romania, Serbia",
                            "Austria, Croatia, Czech Republic, Germany, Hungary, Netherlands, Poland, Switzerland",
                            "Estonia, Latvia, Lithuania",
                            "France, Gibraltar, Portugal, Spain",
                            "Faroes, Iceland",
                            "Italy",
                            "Ireland, United Kingdom",
                            "Denmark, Norway, Sweden",
                            "Ukraine")
# ind -> country summarization
europe.ts.2.pnpla3$subregion <- as.character(europe.ts.2.pnpla3$subregion)
europe.ts.2.pnpla3.country   <- dplyr::group_by(europe.ts.2.pnpla3, country) %>% summarise(individuals = n(),
                                                                                           subregion   = max(subregion),
                                                                                           mean.ycoord = mean(ycoord),
                                                                                           mean.xcoord = mean(xcoord),
                                                                                           date        = round(mean(date)),
                                                                                           group       = mean(group),
                                                                                           pnpla3      = compute.allele.freq(pnpla3))
# weighting for group allele frequencies
country.pnpla3.tmp        <- europe.ts.2.pnpla3.country
country.pnpla3.tmp$pnpla3 <- europe.ts.2.pnpla3.country$pnpla3 * europe.ts.2.pnpla3.country$individuals
# group summarization
europe.ts.2.pnpla3.group <- dplyr::group_by(country.pnpla3.tmp, group) %>% summarise(subregion   = max(subregion),
                                                                                     individuals = sum(individuals),
                                                                                     mean.ycoord = mean(mean.ycoord),
                                                                                     mean.xcoord = mean(mean.xcoord),
                                                                                     date        = round(mean(date)),
                                                                                     pnpla3      = sum(pnpla3)/individuals)
europe.ts.2.pnpla3.group <- tibble::add_column(europe.ts.2.pnpla3.group, countries = countries.group.pnpla3, .before = "subregion")
# create group kable table
group.table.data.ts2.pnpla3        <- dplyr::ungroup(europe.ts.2.pnpla3.group)
group.table.data.ts2.pnpla3        <- data.table::setorder(group.table.data.ts2.pnpla3, subregion)
group.table.data.ts2.pnpla3$pnpla3 <- round(group.table.data.ts2.pnpla3$pnpla3, digits = 4)
group.table.data.ts2.pnpla3 <- dplyr::select(group.table.data.ts2.pnpla3, -c(3,5,6))

# =============================================================================
# Time Slice 3
# =============================================================================
countries.group.pnpla3 <- c("Bulgaria, Greece, North Macedonia, Romania, Serbia",
                            "Austria, Croatia, Germany, Hungary, Luxembourg, Switzerland",
                            "Latvia, Lithuania",
                            "France, Spain",
                            "Italy",
                            "United Kingdom",
                            "Norway, Sweden",
                            "Ukraine")
# ind -> country summarization
europe.ts.3.pnpla3$subregion <- as.character(europe.ts.3.pnpla3$subregion)
europe.ts.3.pnpla3.country   <- dplyr::group_by(europe.ts.3.pnpla3, country) %>% summarise(individuals = n(),
                                                                                           subregion   = max(subregion),
                                                                                           mean.ycoord = mean(ycoord),
                                                                                           mean.xcoord = mean(xcoord),
                                                                                           date        = round(mean(date)),
                                                                                           group       = mean(group),
                                                                                           pnpla3      = compute.allele.freq(pnpla3))
# weighting for group allele frequencies
country.pnpla3.tmp        <- europe.ts.3.pnpla3.country
country.pnpla3.tmp$pnpla3 <- europe.ts.3.pnpla3.country$pnpla3 * europe.ts.3.pnpla3.country$individuals
# group summarization
europe.ts.3.pnpla3.group <- dplyr::group_by(country.pnpla3.tmp, group) %>% summarise(subregion   = max(subregion),
                                                                                     individuals = sum(individuals),
                                                                                     mean.ycoord = mean(mean.ycoord),
                                                                                     mean.xcoord = mean(mean.xcoord),
                                                                                     date        = round(mean(date)),
                                                                                     pnpla3      = sum(pnpla3)/individuals)
europe.ts.3.pnpla3.group <- tibble::add_column(europe.ts.3.pnpla3.group, countries = countries.group.pnpla3, .before = "subregion")
# create group kable table
group.table.data.ts3.pnpla3        <- dplyr::ungroup(europe.ts.3.pnpla3.group)
group.table.data.ts3.pnpla3        <- data.table::setorder(group.table.data.ts3.pnpla3, subregion)
group.table.data.ts3.pnpla3$pnpla3 <- round(group.table.data.ts3.pnpla3$pnpla3, digits = 4)
group.table.data.ts3.pnpla3 <- dplyr::select(group.table.data.ts3.pnpla3, -c(3,5,6))

# =============================================================================
# Export
# =============================================================================
# time slice 1
write.csv(group.table.data.ts1.pnpla3, "2. Time Slices/1. Data/europe_timeSlice1_group_table_data_pnpla3.csv")
# time slice 2
write.csv(group.table.data.ts2.pnpla3, "2. Time Slices/1. Data/europe_timeSlice2_group_table_data_pnpla3.csv")
# time slice 3
write.csv(group.table.data.ts3.pnpla3, "2. Time Slices/1. Data/europe_timeSlice3_group_table_data_pnpla3.csv")