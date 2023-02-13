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

pie.chart.preparation <- function(asia.group.genotype, gene)
{
  # preparing for pie charts
  asia.group.genotype["reference"] <- 1 - asia.group.genotype[,7]
  data.table::setnames(asia.group.genotype, gene, "minor")
  # scaling factor
  asia.group.genotype <- tibble::add_column(asia.group.genotype, scale = rep(c(0), times = nrow(asia.group.genotype)), .after = "reference")
  asia.group.genotype$scale[asia.group.genotype$individuals < 10]                                          <- 2
  asia.group.genotype$scale[asia.group.genotype$individuals >= 10 & asia.group.genotype$individuals < 35]  <- 3
  asia.group.genotype$scale[asia.group.genotype$individuals > 35 & asia.group.genotype$individuals < 100]  <- 4
  asia.group.genotype$scale[asia.group.genotype$individuals > 100]                                         <- 5
  return(asia.group.genotype)
}

# =============================================================================
# Input Data
# =============================================================================
individuals.data  <- tibble::as_tibble(read.csv(file = "0. Input Data/individuals_data.csv")) %>% dplyr::select(-1)
# time slice 1: 0 - 3000 BP
asia.ts.1 <- dplyr::filter(individuals.data, individuals.data$date > 0 & individuals.data$date <= 3000) %>% filter(continent == "Asia") %>% select(c(1:11))
asia.ts.1 <- dplyr::mutate(asia.ts.1, group  = ifelse(country == "Japan", 2, group))
asia.ts.1 <- dplyr::mutate(asia.ts.1, xcoord = ifelse(group == 4, abs(xcoord), xcoord))
# time slice 2: 2000 - 5000 BP
asia.ts.2 <- dplyr::filter(individuals.data, individuals.data$date > 3000 & individuals.data$date <= 7000) %>% filter(continent == "Asia") %>% select(c(1:11))
asia.ts.2 <- dplyr::mutate(asia.ts.2, group = ifelse(country == "Japan", 2, group))
# time slice 3: 5000 - 15000 BP
asia.ts.3 <- dplyr::filter(individuals.data, individuals.data$date > 7000 & individuals.data$date <= 15000) %>% filter(continent == "Asia") %>% select(c(1:11))
# exclude individuals with no allele information
asia.ts.1.pnpla3 <- dplyr::filter(asia.ts.1, asia.ts.1$pnpla3 != 9)
asia.ts.2.pnpla3 <- dplyr::filter(asia.ts.2, asia.ts.2$pnpla3 != 9)
asia.ts.3.pnpla3 <- dplyr::filter(asia.ts.3, asia.ts.3$pnpla3 != 9)

# =============================================================================
# Time Slice 1
# =============================================================================
# ind -> country summarization
asia.ts.1.pnpla3$subregion <- as.character(asia.ts.1.pnpla3$subregion)
asia.ts.1.pnpla3.country   <- dplyr::group_by(asia.ts.1.pnpla3, country) %>% summarise(individuals = n(),
                                                                                       subregion   = max(subregion),
                                                                                       mean.ycoord = mean(ycoord),
                                                                                       mean.xcoord = mean(xcoord),
                                                                                       date        = mean(date),
                                                                                       group       = mean(group),
                                                                                       pnpla3      = compute.allele.freq(pnpla3))
# weighting for group allele frequencies
asia.ts.1.pnpla3.country$pnpla3 <- asia.ts.1.pnpla3.country$pnpla3 * asia.ts.1.pnpla3.country$individuals
# group summarization
asia.ts.1.pnpla3.group <- dplyr::group_by(asia.ts.1.pnpla3.country, group) %>% summarise(subregion   = max(subregion),
                                                                                         individuals = sum(individuals),
                                                                                         mean.ycoord = mean(mean.ycoord),
                                                                                         mean.xcoord = mean(mean.xcoord),
                                                                                         date        = mean(date),
                                                                                         pnpla3      = sum(pnpla3)/individuals)
# preparing for pie charts
asia.ts.1.pnpla3.group.pc <- pie.chart.preparation(asia.ts.1.pnpla3.group, "pnpla3")

# =============================================================================
# Time Slice 2
# =============================================================================
# ind -> country summarization
asia.ts.2.pnpla3$subregion <- as.character(asia.ts.2.pnpla3$subregion)
asia.ts.2.pnpla3.country   <- dplyr::group_by(asia.ts.2.pnpla3, country) %>% summarise(individuals = n(),
                                                                                       subregion   = max(subregion),
                                                                                       mean.ycoord = mean(ycoord),
                                                                                       mean.xcoord = mean(xcoord),
                                                                                       date        = mean(date),
                                                                                       group       = mean(group),
                                                                                       pnpla3      = compute.allele.freq(pnpla3))
# weighting for group allele frequencies
asia.ts.2.pnpla3.country$pnpla3 <- asia.ts.2.pnpla3.country$pnpla3 * asia.ts.2.pnpla3.country$individuals
# group summarization
asia.ts.2.pnpla3.group <- dplyr::group_by(asia.ts.2.pnpla3.country, group) %>% summarise(subregion   = max(subregion),
                                                                                         individuals = sum(individuals),
                                                                                         mean.ycoord = mean(mean.ycoord),
                                                                                         mean.xcoord = mean(mean.xcoord),
                                                                                         date        = mean(date),
                                                                                         pnpla3      = sum(pnpla3)/individuals)
# preparing for pie charts
asia.ts.2.pnpla3.group.pc <- pie.chart.preparation(asia.ts.2.pnpla3.group, "pnpla3")

# =============================================================================
# Time Slice 3
# =============================================================================
# ind -> country summarization
asia.ts.3.pnpla3$subregion <- as.character(asia.ts.3.pnpla3$subregion)
asia.ts.3.pnpla3.country   <- dplyr::group_by(asia.ts.3.pnpla3, country) %>% summarise(individuals = n(),
                                                                                       subregion   = max(subregion),
                                                                                       mean.ycoord = mean(ycoord),
                                                                                       mean.xcoord = mean(xcoord),
                                                                                       date        = mean(date),
                                                                                       group       = mean(group),
                                                                                       pnpla3      = compute.allele.freq(pnpla3))
# weighting for group allele frequencies
asia.ts.3.pnpla3.country$pnpla3 <- asia.ts.3.pnpla3.country$pnpla3 * asia.ts.3.pnpla3.country$individuals
# group summarization
asia.ts.3.pnpla3.group <- dplyr::group_by(asia.ts.3.pnpla3.country, group) %>% summarise(subregion   = max(subregion),
                                                                                         individuals = sum(individuals),
                                                                                         mean.ycoord = mean(mean.ycoord),
                                                                                         mean.xcoord = mean(mean.xcoord),
                                                                                         date        = mean(date),
                                                                                         pnpla3      = sum(pnpla3)/individuals)
# preparing for pie charts
asia.ts.3.pnpla3.group.pc <- pie.chart.preparation(asia.ts.3.pnpla3.group, "pnpla3")

# =============================================================================
# Export
# =============================================================================
# time slice 1
data.table::setnames(asia.ts.1.pnpla3.group.pc, "minor", "pnpla3")
write.csv(asia.ts.1.pnpla3.group.pc, "2. Time Slices/1. Data/asia_timeSlice1_pnpla3.csv")
# time slice 2
data.table::setnames(asia.ts.2.pnpla3.group.pc, "minor", "pnpla3")
write.csv(asia.ts.2.pnpla3.group.pc, "2. Time Slices/1. Data/asia_timeSlice2_pnpla3.csv")
# time slice 3
data.table::setnames(asia.ts.3.pnpla3.group.pc, "minor", "pnpla3")
write.csv(asia.ts.3.pnpla3.group.pc, "2. Time Slices/1. Data/asia_timeSlice3_pnpla3.csv")