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

pie.chart.preparation <- function(africa.group.genotype, gene)
{
  # preparing for pie charts
  africa.group.genotype["reference"] <- 1 - africa.group.genotype[,7]
  data.table::setnames(africa.group.genotype, gene, "minor")
  # scaling factor
  africa.group.genotype <- tibble::add_column(africa.group.genotype, scale = rep(c(0), times = nrow(africa.group.genotype)), .after = "reference")
  africa.group.genotype$scale[africa.group.genotype$individuals < 10]                                           <- 2
  africa.group.genotype$scale[africa.group.genotype$individuals >= 10 & africa.group.genotype$individuals < 35] <- 3
  africa.group.genotype$scale[africa.group.genotype$individuals > 35 & africa.group.genotype$individuals < 100] <- 4
  africa.group.genotype$scale[africa.group.genotype$individuals > 100]                                          <- 5
  return(africa.group.genotype)
}

# =============================================================================
# Input Data
# =============================================================================
individuals.data  <- tibble::as_tibble(read.csv(file = "0. Input Data/individuals_data.csv")) %>% dplyr::select(-1)
# time slice 1: 0 - 3000 BP
africa.ts.1 <- dplyr::filter(individuals.data, individuals.data$date > 0 & individuals.data$date <= 3000) %>% filter(continent == "Africa") %>% select(c(1:11))
# time slice 2: 2000 - 7000 BP
africa.ts.2 <- dplyr::filter(individuals.data, individuals.data$date > 3000 & individuals.data$date <= 7000) %>% filter(continent == "Africa") %>% select(c(1:11))
# time slice 3: 7000 - 15000 BP
africa.ts.3 <- dplyr::filter(individuals.data, individuals.data$date > 7000 & individuals.data$date <= 15000) %>% filter(continent == "Africa") %>% select(c(1:11))
# exclude individuals with no allele information
africa.ts.1.pnpla3 <- dplyr::filter(africa.ts.1, africa.ts.1$pnpla3 != 9)
africa.ts.2.pnpla3 <- dplyr::filter(africa.ts.2, africa.ts.2$pnpla3 != 9)
africa.ts.3.pnpla3 <- dplyr::filter(africa.ts.3, africa.ts.3$pnpla3 != 9)

# =============================================================================
# Time Slice 1
# =============================================================================
# ind -> country summarization
africa.ts.1.pnpla3$subregion <- as.character(africa.ts.1.pnpla3$subregion)
africa.ts.1.pnpla3.country   <- dplyr::group_by(africa.ts.1.pnpla3, country) %>% summarise(individuals = n(),
                                                                                           subregion   = max(subregion),
                                                                                           mean.ycoord = mean(ycoord),
                                                                                           mean.xcoord = mean(xcoord),
                                                                                           date        = mean(date),
                                                                                           group       = mean(group),
                                                                                           pnpla3      = compute.allele.freq(pnpla3))
# weighting for group allele frequencies
africa.ts.1.pnpla3.country$pnpla3 <- africa.ts.1.pnpla3.country$pnpla3 * africa.ts.1.pnpla3.country$individuals
# group summarization
africa.ts.1.pnpla3.group <- dplyr::group_by(africa.ts.1.pnpla3.country, group) %>% summarise(subregion   = max(subregion),
                                                                                             individuals = sum(individuals),
                                                                                             mean.ycoord = mean(mean.ycoord),
                                                                                             mean.xcoord = mean(mean.xcoord),
                                                                                             date        = mean(date),
                                                                                             pnpla3      = sum(pnpla3)/individuals)
# preparing for pie charts
africa.ts.1.pnpla3.group.pc <- pie.chart.preparation(africa.ts.1.pnpla3.group, "pnpla3")

# =============================================================================
# Time Slice 2
# =============================================================================
# ind -> country summarization
africa.ts.2.pnpla3$subregion <- as.character(africa.ts.2.pnpla3$subregion)
africa.ts.2.pnpla3.country   <- dplyr::group_by(africa.ts.2.pnpla3, country) %>% summarise(individuals = n(),
                                                                                           subregion   = max(subregion),
                                                                                           mean.ycoord = mean(ycoord),
                                                                                           mean.xcoord = mean(xcoord),
                                                                                           date        = mean(date),
                                                                                           group       = mean(group),
                                                                                           pnpla3      = compute.allele.freq(pnpla3))
# weighting for group allele frequencies
africa.ts.2.pnpla3.country$pnpla3 <- africa.ts.2.pnpla3.country$pnpla3 * africa.ts.2.pnpla3.country$individuals
# group summarization
africa.ts.2.pnpla3.group <- dplyr::group_by(africa.ts.2.pnpla3.country, group) %>% summarise(subregion   = max(subregion),
                                                                                             individuals = sum(individuals),
                                                                                             mean.ycoord = mean(mean.ycoord),
                                                                                             mean.xcoord = mean(mean.xcoord),
                                                                                             date        = mean(date),
                                                                                             pnpla3      = sum(pnpla3)/individuals)
# preparing for pie charts
africa.ts.2.pnpla3.group.pc <- pie.chart.preparation(africa.ts.2.pnpla3.group, "pnpla3")

# =============================================================================
# Time Slice 3
# =============================================================================
# ind -> country summarization
africa.ts.3.pnpla3$subregion <- as.character(africa.ts.3.pnpla3$subregion)
africa.ts.3.pnpla3.country   <- dplyr::group_by(africa.ts.3.pnpla3, country) %>% summarise(individuals = n(),
                                                                                           subregion   = max(subregion),
                                                                                           mean.ycoord = mean(ycoord),
                                                                                           mean.xcoord = mean(xcoord),
                                                                                           date        = round(mean(date)),
                                                                                           group       = mean(group),
                                                                                           pnpla3      = compute.allele.freq(pnpla3))
# weighting for group allele frequencies
africa.ts.3.pnpla3.country$pnpla3 <- africa.ts.3.pnpla3.country$pnpla3 * africa.ts.3.pnpla3.country$individuals
# group summarization
africa.ts.3.pnpla3.group <- dplyr::group_by(africa.ts.3.pnpla3.country, group) %>% summarise(subregion   = max(subregion),
                                                                                             individuals = sum(individuals),
                                                                                             mean.ycoord = mean(mean.ycoord),
                                                                                             mean.xcoord = mean(mean.xcoord),
                                                                                             date        = round(mean(date)),
                                                                                             pnpla3      = sum(pnpla3)/individuals)
# preparing for pie charts
africa.ts.3.pnpla3.group.pc <- pie.chart.preparation(africa.ts.3.pnpla3.group, "pnpla3")

# =============================================================================
# Export
# =============================================================================
# time slice 1
data.table::setnames(africa.ts.1.pnpla3.group.pc, "minor", "pnpla3")
write.csv(africa.ts.1.pnpla3.group.pc, "2. Time Slices/1. Data/africa_timeSlice1_pnpla3.csv")
# time slice 2
data.table::setnames(africa.ts.2.pnpla3.group.pc, "minor", "pnpla3")
write.csv(africa.ts.2.pnpla3.group.pc, "2. Time Slices/1. Data/africa_timeSlice2_pnpla3.csv")
# time slice 3
data.table::setnames(africa.ts.3.pnpla3.group.pc, "minor", "pnpla3")
write.csv(africa.ts.3.pnpla3.group.pc, "2. Time Slices/1. Data/africa_timeSlice3_pnpla3.csv")