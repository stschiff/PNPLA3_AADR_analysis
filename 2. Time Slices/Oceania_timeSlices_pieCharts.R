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

pie.chart.preparation <- function(oceania.group.genotype, gene)
{
  # preparing for pie charts
  oceania.group.genotype["reference"] <- 1 - oceania.group.genotype[,7]
  data.table::setnames(oceania.group.genotype, gene, "minor")
  # scaling factor
  oceania.group.genotype <- tibble::add_column(oceania.group.genotype, scale = rep(c(0), times = nrow(oceania.group.genotype)), .after = "reference")
  oceania.group.genotype$scale[oceania.group.genotype$individuals < 10]                                            <- 2
  oceania.group.genotype$scale[oceania.group.genotype$individuals >= 10 & oceania.group.genotype$individuals < 35] <- 3
  oceania.group.genotype$scale[oceania.group.genotype$individuals > 35 & oceania.group.genotype$individuals < 100] <- 4
  oceania.group.genotype$scale[oceania.group.genotype$individuals > 100]                                           <- 5
  return(oceania.group.genotype)
}

# =============================================================================
# Input Data
# =============================================================================
individuals.data  <- tibble::as_tibble(read.csv(file = "0. Input Data/individuals_data.csv")) %>% dplyr::select(-1)
# time slice 1: 0 - 3000 BP
oceania.ts.1 <- dplyr::filter(individuals.data, individuals.data$date > 0 & individuals.data$date <= 3000) %>% filter(continent == "Oceania") %>% select(c(1:11))
oceania.ts.1$xcoord <- abs(oceania.ts.1$xcoord)
# exclude individuals with no allele information
oceania.ts.1.pnpla3 <- dplyr::filter(oceania.ts.1, oceania.ts.1$pnpla3 != 9)

# =============================================================================
# Time Slice 1
# =============================================================================
# ind -> country summarization
oceania.ts.1.pnpla3$subregion <- as.character(oceania.ts.1.pnpla3$subregion)
oceania.ts.1.pnpla3.country   <- dplyr::group_by(oceania.ts.1.pnpla3, country) %>% summarise(individuals = n(),
                                                                                             subregion   = max(subregion),
                                                                                             mean.ycoord = mean(ycoord),
                                                                                             mean.xcoord = mean(xcoord),
                                                                                             date        = mean(date),
                                                                                             group       = mean(group),
                                                                                             pnpla3      = compute.allele.freq(pnpla3))
# weighting for group allele frequencies
oceania.ts.1.pnpla3.country$pnpla3 <- oceania.ts.1.pnpla3.country$pnpla3 * oceania.ts.1.pnpla3.country$individuals
# group summarization
oceania.ts.1.pnpla3.group <- dplyr::group_by(oceania.ts.1.pnpla3.country, group) %>% summarise(subregion   = max(subregion),
                                                                                               individuals = sum(individuals),
                                                                                               mean.ycoord = mean(mean.ycoord),
                                                                                               mean.xcoord = mean(mean.xcoord),
                                                                                               date        = mean(date),
                                                                                               pnpla3      = sum(pnpla3)/individuals)
# preparing for pie charts
oceania.ts.1.pnpla3.group.pc <- pie.chart.preparation(oceania.ts.1.pnpla3.group, "pnpla3")

# =============================================================================
# Export
# =============================================================================
# time slice 1
data.table::setnames(oceania.ts.1.pnpla3.group.pc, "minor", "pnpla3")
write.csv(oceania.ts.1.pnpla3.group.pc, "2. Time Slices/1. Data/oceania_timeSlice1_pnpla3.csv")