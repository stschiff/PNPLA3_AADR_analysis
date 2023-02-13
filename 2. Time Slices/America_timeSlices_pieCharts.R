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

pie.chart.preparation <- function(america.group.genotype, gene)
{
  # preparing for pie charts
  america.group.genotype["reference"] <- 1 - america.group.genotype[,7]
  data.table::setnames(america.group.genotype, gene, "minor")
  # scaling factor
  america.group.genotype <- tibble::add_column(america.group.genotype, scale = rep(c(0), times = nrow(america.group.genotype)), .after = "reference")
  america.group.genotype$scale[america.group.genotype$individuals < 10]                                             <- 2
  america.group.genotype$scale[america.group.genotype$individuals >= 10 & america.group.genotype$individuals < 35]  <- 3
  america.group.genotype$scale[america.group.genotype$individuals > 35 & america.group.genotype$individuals < 100]  <- 4
  america.group.genotype$scale[america.group.genotype$individuals > 100]                                            <- 5
  return(america.group.genotype)
}

# =============================================================================
# Input Data
# =============================================================================
individuals.data  <- tibble::as_tibble(read.csv(file = "0. Input Data/individuals_data.csv")) %>% dplyr::select(-1)
# time slice 1: 0 - 3000 BP
america.ts.1 <- dplyr::filter(individuals.data, individuals.data$date > 0 & individuals.data$date <= 3000) %>% filter(continent == "North America" | continent == "South America") %>% select(c(1:11))
# time slice 2: 2000 - 7000 BP
america.ts.2 <- dplyr::filter(individuals.data, individuals.data$date > 3000 & individuals.data$date <= 7000) %>% filter(continent == "North America" | continent == "South America") %>% select(c(1:11))
# time slice 3: 7000 - 15000 BP
america.ts.3 <- dplyr::filter(individuals.data, individuals.data$date > 7000 & individuals.data$date <= 15000) %>% filter(continent == "North America" | continent == "South America") %>% select(c(1:11))
# exclude individuals with no allele information
america.ts.1.pnpla3 <- dplyr::filter(america.ts.1, america.ts.1$pnpla3 != 9)
america.ts.2.pnpla3 <- dplyr::filter(america.ts.2, america.ts.2$pnpla3 != 9)
america.ts.3.pnpla3 <- dplyr::filter(america.ts.3, america.ts.3$pnpla3 != 9)

# =============================================================================
# Time Slice 1
# =============================================================================
# ind -> country summarization
america.ts.1.pnpla3$subregion <- as.character(america.ts.1.pnpla3$subregion)
america.ts.1.pnpla3.country   <- dplyr::group_by(america.ts.1.pnpla3, country) %>% summarise(individuals = n(),
                                                                                             subregion   = max(subregion),
                                                                                             mean.ycoord = mean(ycoord),
                                                                                             mean.xcoord = mean(xcoord),
                                                                                             date        = mean(date),
                                                                                             group       = mean(group),
                                                                                             pnpla3      = compute.allele.freq(pnpla3))
# weighting for group allele frequencies
america.ts.1.pnpla3.country$pnpla3 <- america.ts.1.pnpla3.country$pnpla3 * america.ts.1.pnpla3.country$individuals
# group summarization
america.ts.1.pnpla3.group <- dplyr::group_by(america.ts.1.pnpla3.country, group) %>% summarise(subregion   = max(subregion),
                                                                                               individuals = sum(individuals),
                                                                                               mean.ycoord = mean(mean.ycoord),
                                                                                               mean.xcoord = mean(mean.xcoord),
                                                                                               date        = mean(date),
                                                                                               pnpla3      = sum(pnpla3)/individuals)
# preparing for pie charts
america.ts.1.pnpla3.group.pc <- pie.chart.preparation(america.ts.1.pnpla3.group, "pnpla3")

# =============================================================================
# Time Slice 2
# =============================================================================
# ind -> country summarization
america.ts.2.pnpla3$subregion <- as.character(america.ts.2.pnpla3$subregion)
america.ts.2.pnpla3.country   <- dplyr::group_by(america.ts.2.pnpla3, country) %>% summarise(individuals = n(),
                                                                                             subregion   = max(subregion),
                                                                                             mean.ycoord = mean(ycoord),
                                                                                             mean.xcoord = mean(xcoord),
                                                                                             date        = mean(date),
                                                                                             group       = mean(group),
                                                                                             pnpla3      = compute.allele.freq(pnpla3))
# weighting for group allele frequencies
country.pnpla3.tmp        <- america.ts.2.pnpla3.country
country.pnpla3.tmp$pnpla3 <- america.ts.2.pnpla3.country$pnpla3 * america.ts.2.pnpla3.country$individuals
# group summarization
america.ts.2.pnpla3.group <- dplyr::group_by(country.pnpla3.tmp, group) %>% summarise(subregion   = max(subregion),
                                                                                      individuals = sum(individuals),
                                                                                      mean.ycoord = mean(mean.ycoord),
                                                                                      mean.xcoord = mean(mean.xcoord),
                                                                                      date        = round(mean(date)),
                                                                                      pnpla3      = sum(pnpla3)/individuals)
# preparing for pie charts
america.ts.2.pnpla3.group.pc <- pie.chart.preparation(america.ts.2.pnpla3.group, "pnpla3")

# =============================================================================
# Time Slice 3
# =============================================================================
# ind -> country summarization
america.ts.3.pnpla3$subregion <- as.character(america.ts.3.pnpla3$subregion)
america.ts.3.pnpla3.country   <- dplyr::group_by(america.ts.3.pnpla3, country) %>% summarise(individuals = n(),
                                                                                             subregion   = max(subregion),
                                                                                             mean.ycoord = mean(ycoord),
                                                                                             mean.xcoord = mean(xcoord),
                                                                                             date        = round(mean(date)),
                                                                                             group       = mean(group),
                                                                                             pnpla3      = compute.allele.freq(pnpla3))
# weighting for group allele frequencies
country.pnpla3.tmp        <- america.ts.3.pnpla3.country
country.pnpla3.tmp$pnpla3 <- america.ts.3.pnpla3.country$pnpla3 * america.ts.3.pnpla3.country$individuals
# group summarization
america.ts.3.pnpla3.group <- dplyr::group_by(country.pnpla3.tmp, group) %>% summarise(subregion   = max(subregion),
                                                                                      individuals = sum(individuals),
                                                                                      mean.ycoord = mean(mean.ycoord),
                                                                                      mean.xcoord = mean(mean.xcoord),
                                                                                      date        = round(mean(date)),
                                                                                      pnpla3      = sum(pnpla3)/individuals)
# preparing for pie charts
america.ts.3.pnpla3.group.pc <- pie.chart.preparation(america.ts.3.pnpla3.group, "pnpla3")

# =============================================================================
# Export
# =============================================================================
# time slice 1 
data.table::setnames(america.ts.1.pnpla3.group.pc, "minor", "pnpla3")
write.csv(america.ts.1.pnpla3.group.pc, "2. Time Slices/1. Data/america_timeSlice1_pnpla3.csv")
# time slice 2
data.table::setnames(america.ts.2.pnpla3.group.pc, "minor", "pnpla3")
write.csv(america.ts.2.pnpla3.group.pc, "2. Time Slices/1. Data/america_timeSlice2_pnpla3.csv")
# time slice 3
data.table::setnames(america.ts.3.pnpla3.group.pc, "minor", "pnpla3")
write.csv(america.ts.3.pnpla3.group.pc, "2. Time Slices/1. Data/america_timeSlice3_pnpla3.csv")