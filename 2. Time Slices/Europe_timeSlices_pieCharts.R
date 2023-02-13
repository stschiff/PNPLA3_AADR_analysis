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

pie.chart.preparation <- function(europe.group.genotype, gene)
{
  # preparing for pie charts
  europe.group.genotype["reference"] <- 1 - europe.group.genotype[,7]
  data.table::setnames(europe.group.genotype, gene, "minor")
  # scaling factor
  europe.group.genotype <- tibble::add_column(europe.group.genotype, scale = rep(c(0), times = nrow(europe.group.genotype)), .after = "reference")
  europe.group.genotype$scale[europe.group.genotype$individuals < 10]                                            <- 2
  europe.group.genotype$scale[europe.group.genotype$individuals >= 10 & europe.group.genotype$individuals < 35]  <- 3
  europe.group.genotype$scale[europe.group.genotype$individuals > 35 & europe.group.genotype$individuals < 100]  <- 4
  europe.group.genotype$scale[europe.group.genotype$individuals > 100]                                           <- 5
  return(europe.group.genotype)
}

# =============================================================================
# Input Data
# =============================================================================
individuals.data  <- tibble::as_tibble(read.csv(file = "0. Input Data/individuals_data.csv")) %>% dplyr::select(-1)
# time slice 1: 0 - 3000 BP
europe.ts.1 <- dplyr::filter(individuals.data, individuals.data$date > 0 & individuals.data$date <= 3000) %>% filter(continent == "Europe") %>% select(c(1:11))
# time slice 2: 2000 - 5000 BP
europe.ts.2 <- dplyr::filter(individuals.data, individuals.data$date > 3000 & individuals.data$date <= 7000) %>% filter(continent == "Europe") %>% select(c(1:11))
# time slice 3: 5000 - 15000 BP
europe.ts.3 <- dplyr::filter(individuals.data, individuals.data$date > 7000 & individuals.data$date <= 15000) %>% filter(continent == "Europe") %>% select(c(1:11))
# exclude individuals with no allele information
europe.ts.1.pnpla3 <- dplyr::filter(europe.ts.1, europe.ts.1$pnpla3 != 9)
europe.ts.2.pnpla3 <- dplyr::filter(europe.ts.2, europe.ts.2$pnpla3 != 9)
europe.ts.3.pnpla3 <- dplyr::filter(europe.ts.3, europe.ts.3$pnpla3 != 9)

# =============================================================================
# Time Slice 1
# =============================================================================
# ind -> country summarization
europe.ts.1.pnpla3$subregion <- as.character(europe.ts.1.pnpla3$subregion)
europe.ts.1.pnpla3.country   <- dplyr::group_by(europe.ts.1.pnpla3, country) %>% summarise(individuals = n(),
                                                                                           subregion   = max(subregion),
                                                                                           mean.ycoord = mean(ycoord),
                                                                                           mean.xcoord = mean(xcoord),
                                                                                           date        = mean(date),
                                                                                           group       = mean(group),
                                                                                           pnpla3      = compute.allele.freq(pnpla3))
# weighting for group allele frequencies
europe.ts.1.pnpla3.country$pnpla3 <- europe.ts.1.pnpla3.country$pnpla3 * europe.ts.1.pnpla3.country$individuals
# group summarization
europe.ts.1.pnpla3.group <- dplyr::group_by(europe.ts.1.pnpla3.country, group) %>% summarise(subregion   = max(subregion),
                                                                                             individuals = sum(individuals),
                                                                                             mean.ycoord = mean(mean.ycoord),
                                                                                             mean.xcoord = mean(mean.xcoord),
                                                                                             date        = mean(date),
                                                                                             pnpla3      = sum(pnpla3)/individuals)
# preparing for pie charts
europe.ts.1.pnpla3.group.pc <- pie.chart.preparation(europe.ts.1.pnpla3.group, "pnpla3")

# =============================================================================
# Time Slice 2
# =============================================================================
# ind -> country summarization
europe.ts.2.pnpla3$subregion <- as.character(europe.ts.2.pnpla3$subregion)
europe.ts.2.pnpla3.country   <- dplyr::group_by(europe.ts.2.pnpla3, country) %>% summarise(individuals = n(),
                                                                                           subregion   = max(subregion),
                                                                                           mean.ycoord = mean(ycoord),
                                                                                           mean.xcoord = mean(xcoord),
                                                                                           date        = mean(date),
                                                                                           group       = mean(group),
                                                                                           pnpla3      = compute.allele.freq(pnpla3))
# weighting for group allele frequencies
europe.ts.2.pnpla3.country$pnpla3 <- europe.ts.2.pnpla3.country$pnpla3 * europe.ts.2.pnpla3.country$individuals
# group summarization
europe.ts.2.pnpla3.group <- dplyr::group_by(europe.ts.2.pnpla3.country, group) %>% summarise(subregion   = max(subregion),
                                                                                             individuals = sum(individuals),
                                                                                             mean.ycoord = mean(mean.ycoord),
                                                                                             mean.xcoord = mean(mean.xcoord),
                                                                                             date        = mean(date),
                                                                                             pnpla3      = sum(pnpla3)/individuals)
# preparing for pie charts
europe.ts.2.pnpla3.group.pc <- pie.chart.preparation(europe.ts.2.pnpla3.group, "pnpla3")

# =============================================================================
# Time Slice 3
# =============================================================================
# ind -> country summarization
europe.ts.3.pnpla3$subregion <- as.character(europe.ts.3.pnpla3$subregion)
europe.ts.3.pnpla3.country   <- dplyr::group_by(europe.ts.3.pnpla3, country) %>% summarise(individuals = n(),
                                                                                           subregion   = max(subregion),
                                                                                           mean.ycoord = mean(ycoord),
                                                                                           mean.xcoord = mean(xcoord),
                                                                                           date        = mean(date),
                                                                                           group       = mean(group),
                                                                                           pnpla3      = compute.allele.freq(pnpla3))
# weighting for group allele frequencies
europe.ts.3.pnpla3.country$pnpla3 <- europe.ts.3.pnpla3.country$pnpla3 * europe.ts.3.pnpla3.country$individuals
# group summarization
europe.ts.3.pnpla3.group <- dplyr::group_by(europe.ts.3.pnpla3.country, group) %>% summarise(subregion   = max(subregion),
                                                                                             individuals = sum(individuals),
                                                                                             mean.ycoord = mean(mean.ycoord),
                                                                                             mean.xcoord = mean(mean.xcoord),
                                                                                             date        = mean(date),
                                                                                             pnpla3      = sum(pnpla3)/individuals)
# preparing for pie charts
europe.ts.3.pnpla3.group.pc <- pie.chart.preparation(europe.ts.3.pnpla3.group, "pnpla3")

# =============================================================================
# Export
# =============================================================================
# time slice 1
data.table::setnames(europe.ts.1.pnpla3.group.pc, "minor", "pnpla3")
write.csv(europe.ts.1.pnpla3.group.pc, "2. Time Slices/1. Data/europe_timeSlice1_pnpla3.csv")
# time slice 2
data.table::setnames(europe.ts.2.pnpla3.group.pc, "minor", "pnpla3")
write.csv(europe.ts.2.pnpla3.group.pc, "2. Time Slices/1. Data/europe_timeSlice2_pnpla3.csv")
# time slice 3
data.table::setnames(europe.ts.3.pnpla3.group.pc, "minor", "pnpla3")
write.csv(europe.ts.3.pnpla3.group.pc, "2. Time Slices/1. Data/europe_timeSlice3_pnpla3.csv")