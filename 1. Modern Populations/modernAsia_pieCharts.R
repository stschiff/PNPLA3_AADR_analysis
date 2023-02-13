library(tidyverse)
library(rnaturalearth)
library(ggplot2)
library(scatterpie)
library(ggrepel)
options(digits = 5)

world.card <- ne_countries(scale = "medium", returnclass = "sf")
PROJ       <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"

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
  asia.group.genotype["reference"] <- 1 - asia.group.genotype[,6]
  data.table::setnames(asia.group.genotype, gene, "minor")
  # scaling factor
  asia.group.genotype <- tibble::add_column(asia.group.genotype, scale = rep(c(0), times = nrow(asia.group.genotype)), .after = "reference")
  asia.group.genotype$scale[asia.group.genotype$individuals < 10]                                         <- 2
  asia.group.genotype$scale[asia.group.genotype$individuals >= 10 & asia.group.genotype$individuals < 35] <- 3
  asia.group.genotype$scale[asia.group.genotype$individuals > 35 & asia.group.genotype$individuals < 100] <- 4
  asia.group.genotype$scale[asia.group.genotype$individuals > 100]                                        <- 5
  return(asia.group.genotype)
}

radius.labels <- c("< 10 individuals",
                   "10 < individuals < 35",
                   "> 100 individuals")

custom_geom_scatterpie_legend <- function (radius, x, y, n = 3, labeller, textsize=1) 
{
  if (length(radius) > n) {
    radius <- unique(sapply(seq(min(radius), max(radius), 
                                length.out = n), scatterpie:::round_digit))
  }
  label <- FALSE
  if (!missing(labeller)) {
    if (!inherits(labeller, "function")) {
      stop("labeller should be a function for converting radius")
    }
    label <- TRUE
  }
  dd <- data.frame(r = radius, start = 0, end = 2 * pi, x = x, 
                   y = y + radius - max(radius), maxr = max(radius))
  if (label) {
    dd$label <- labeller(dd$r)
  }
  else {
    dd$label <- dd$r
  }
  list(ggforce:::geom_arc_bar(aes_(x0 = ~x, y0 = ~y, r0 = ~r, r = ~r, 
                                   start = ~start, end = ~end), data = dd, inherit.aes = FALSE), 
       geom_segment(aes_(x = ~x, xend = ~x + maxr * 1.5, y = ~y + 
                           r, yend = ~y + r), data = dd, inherit.aes = FALSE), 
       geom_text(aes_(x = ~x + maxr * 1.6, y = ~y + r, label = ~label), 
                 data = dd, hjust = "left", inherit.aes = FALSE,size=textsize))
}

# =============================================================================
# Input Data
# =============================================================================
individuals.data  <- tibble::as_tibble(read.csv(file = "0. Input Data/individuals_data.csv")) %>% dplyr::select(-1)
asia.ind.modern <- dplyr::filter(individuals.data, individuals.data$date == 0) %>% filter(continent == "Asia") %>% select(c(1:11))

# =============================================================================
# PNPLA3
# =============================================================================
# exclude individuals with no allele information
asia.pnpla3 <- dplyr::filter(asia.ind.modern, asia.ind.modern$pnpla3 != 9)
asia.pnpla3$subregion <- as.character(asia.pnpla3$subregion)
# ind -> country summarization
asia.pnpla3.country <- dplyr::group_by(asia.pnpla3, country) %>% summarise(individuals = n(),
                                                                           subregion   = max(subregion),
                                                                           mean.ycoord = mean(ycoord),
                                                                           mean.xcoord = mean(xcoord),
                                                                           group       = mean(group),
                                                                           pnpla3      = compute.allele.freq(pnpla3))
# weighting for group allele frequencies
asia.pnpla3.country.tmp <- asia.pnpla3.country
asia.pnpla3.country.tmp$pnpla3 <- asia.pnpla3.country$pnpla3 * asia.pnpla3.country$individuals
# group summarization
asia.pnpla3.group <- dplyr::group_by(asia.pnpla3.country.tmp, group) %>% summarise(subregion   = max(subregion),
                                                                                   individuals = sum(individuals),
                                                                                   mean.ycoord = mean(mean.ycoord),
                                                                                   mean.xcoord = mean(mean.xcoord),
                                                                                   pnpla3      = sum(pnpla3)/individuals)
# preparing for pie charts
asia.pnpla3.group.pc <- pie.chart.preparation(asia.pnpla3.group, "pnpla3")
# plot
asia.pnpla3.group.pc$mean.xcoord[7] <- asia.pnpla3.group.pc$mean.xcoord[7] + 2
asia.pnpla3.group.pc$mean.xcoord[3] <- asia.pnpla3.group.pc$mean.xcoord[3] + 1
x.offset <- c(5,13,10,7,-18,10,13,-12, 0)
y.offset <- c(5,-8, 0,7, -5, 0, 0,  0,10)
pnpla3.plot <- ggplot(data = world.card) +
                geom_sf(fill = "antiquewhite") +
                # just mapping asia
                coord_sf(xlim = c(0, 180), ylim = c(-20,90), expand = FALSE) +
                # data 
                geom_point(data = asia.pnpla3.group.pc,
                           aes(x = mean.xcoord, y = mean.ycoord),
                           size = 1.5) +
                geom_segment(data = asia.pnpla3.group.pc, aes(x=mean.xcoord, y=mean.ycoord, xend = mean.xcoord+x.offset, yend = mean.ycoord+y.offset),
                             size = 0.5) +
                geom_scatterpie(data = asia.pnpla3.group.pc, aes(x=mean.xcoord+x.offset, y=mean.ycoord+y.offset, r=scale),
                                cols = c("minor", "reference"), color = NA) +
                geom_label_repel(data = asia.pnpla3.group.pc,
                                 aes(x = mean.xcoord, y = mean.ycoord, label = group),
                                 color = "black",
                                 label.padding = 0.15,
                                 size          = 3.5) +
                # scatterpie legend
                custom_geom_scatterpie_legend(asia.pnpla3.group.pc$scale, x = 60, y = -10, n = 3, labeller = function(x) x = radius.labels, textsize = 3.5) +
                # modify the continent.map
                xlab("Longitude") + ylab("Latitude") +
                ggtitle("PNPLA3: allele frequencies Asia") +
                theme(panel.grid.major = element_line(color = gray(.5), linetype = "dashed", size = 0.5),
                      panel.background = element_rect(fill = "aliceblue"),
                      panel.border     = element_rect(fill = NA))

# =============================================================================
# TM6SF2
# =============================================================================
# exclude individuals with no allele information
asia.tm6sf2 <- dplyr::filter(asia.ind.modern, asia.ind.modern$tm6sf2 != 9)
asia.tm6sf2$subregion <- as.character(asia.tm6sf2$subregion)
# ind -> country summarization
asia.tm6sf2.country <- dplyr::group_by(asia.tm6sf2, country) %>% summarise(individuals = n(),
                                                                           subregion   = max(subregion),
                                                                           mean.ycoord = mean(ycoord),
                                                                           mean.xcoord = mean(xcoord),
                                                                           group       = mean(group),
                                                                           tm6sf2      = compute.allele.freq(tm6sf2))
# weighting for group allele frequencies
asia.tm6sf2.country.tmp <- asia.tm6sf2.country
asia.tm6sf2.country.tmp$tm6sf2 <- asia.tm6sf2.country$tm6sf2 * asia.tm6sf2.country$individuals
# group summarization
asia.tm6sf2.group <- dplyr::group_by(asia.tm6sf2.country.tmp, group) %>% summarise(subregion   = max(subregion),
                                                                                   individuals = sum(individuals),
                                                                                   mean.ycoord = mean(mean.ycoord),
                                                                                   mean.xcoord = mean(mean.xcoord),
                                                                                   tm6sf2      = sum(tm6sf2)/individuals)
# preparing for pie charts
asia.tm6sf2.group.pc <- pie.chart.preparation(asia.tm6sf2.group, "tm6sf2")
# plot
asia.pnpla3.group.pc$mean.xcoord[7] <- asia.pnpla3.group.pc$mean.xcoord[7] + 2
asia.pnpla3.group.pc$mean.xcoord[3] <- asia.pnpla3.group.pc$mean.xcoord[3] + 1
tm6sf2.plot <- ggplot(data = world.card) +
                geom_sf(fill = "antiquewhite") +
                # just mapping asia
                coord_sf(xlim = c(0, 180), ylim = c(-20,90), expand = FALSE) +
                # data 
                geom_point(data = asia.tm6sf2.group.pc,
                           aes(x = mean.xcoord, y = mean.ycoord),
                           size = 1.5) +
                geom_segment(data = asia.tm6sf2.group.pc, aes(x=mean.xcoord, y=mean.ycoord, xend = mean.xcoord+x.offset, yend = mean.ycoord+y.offset),
                             size = 0.5) +
                geom_scatterpie(data = asia.tm6sf2.group.pc, aes(x=mean.xcoord+x.offset, y=mean.ycoord+y.offset, r=scale),
                                cols = c("minor", "reference"), color = NA) +
                geom_label_repel(data = asia.tm6sf2.group.pc,
                                 aes(x = mean.xcoord, y = mean.ycoord, label = group),
                                 color = "black",
                                 label.padding = 0.15,
                                 size          = 3.5) +
                # scatterpie legend
                custom_geom_scatterpie_legend(asia.tm6sf2.group.pc$scale, x = 60, y = -10, n = 3, labeller = function(x) x = radius.labels, textsize = 3.5) +
                # modify the continent.map
                xlab("Longitude") + ylab("Latitude") +
                ggtitle("TM6SF2: allele frequencies Asia") +
                theme(panel.grid.major = element_line(color = gray(.5), linetype = "dashed", size = 0.5),
                      panel.background = element_rect(fill = "aliceblue"),
                      panel.border     = element_rect(fill = NA))

# =============================================================================
# MBOAT7
# =============================================================================
# exclude individuals with no allele information
asia.mboat7 <- dplyr::filter(asia.ind.modern, asia.ind.modern$mboat7 != 9)
asia.mboat7$subregion <- as.character(asia.mboat7$subregion)
# ind -> country summarization
asia.mboat7.country <- dplyr::group_by(asia.mboat7, country) %>% summarise(individuals = n(),
                                                                           subregion   = max(subregion),
                                                                           mean.ycoord = mean(ycoord),
                                                                           mean.xcoord = mean(xcoord),
                                                                           group       = mean(group),
                                                                           mboat7      = compute.allele.freq(mboat7))
# weighting for group allele frequencies
asia.mboat7.country.tmp <- asia.mboat7.country
asia.mboat7.country.tmp$mboat7 <- asia.mboat7.country$mboat7 * asia.mboat7.country$individuals
# group summarization
asia.mboat7.group <- dplyr::group_by(asia.mboat7.country.tmp, group) %>% summarise(subregion   = max(subregion),
                                                                                   individuals = sum(individuals),
                                                                                   mean.ycoord = mean(mean.ycoord),
                                                                                   mean.xcoord = mean(mean.xcoord),
                                                                                   mboat7      = sum(mboat7)/individuals)
# preparing for pie charts
asia.mboat7.group.pc <- pie.chart.preparation(asia.mboat7.group, "mboat7")
# plot
asia.pnpla3.group.pc$mean.xcoord[7] <- asia.pnpla3.group.pc$mean.xcoord[7] + 2
asia.pnpla3.group.pc$mean.xcoord[3] <- asia.pnpla3.group.pc$mean.xcoord[3] + 1
mboat7.plot <- ggplot(data = world.card) +
                geom_sf(fill = "antiquewhite") +
                # just mapping asia
                coord_sf(xlim = c(0, 180), ylim = c(-20,90), expand = FALSE) +
                # data 
                geom_point(data = asia.mboat7.group.pc,
                           aes(x = mean.xcoord, y = mean.ycoord),
                           size = 1.5) +
                geom_segment(data = asia.mboat7.group.pc, aes(x=mean.xcoord, y=mean.ycoord, xend = mean.xcoord+x.offset, yend = mean.ycoord+y.offset),
                             size = 0.5) +
                geom_scatterpie(data = asia.mboat7.group.pc, aes(x=mean.xcoord+x.offset, y=mean.ycoord+y.offset, r=scale),
                                cols = c("minor", "reference"), color = NA) +
                geom_label_repel(data = asia.mboat7.group.pc,
                                 aes(x = mean.xcoord, y = mean.ycoord, label = group),
                                 color = "black",
                                 label.padding = 0.15,
                                 size          = 3.5) +
                # scatterpie legend
                custom_geom_scatterpie_legend(asia.mboat7.group.pc$scale, x = 60, y = -10, n = 3, labeller = function(x) x = radius.labels, textsize = 3.5) +
                # modify the continent.map
                xlab("Longitude") + ylab("Latitude") +
                ggtitle("mboat7: allele frequencies Asia") +
                theme(panel.grid.major = element_line(color = gray(.5), linetype = "dashed", size = 0.5),
                      panel.background = element_rect(fill = "aliceblue"),
                      panel.border     = element_rect(fill = NA))

# =============================================================================
# Export
# =============================================================================
# tables
data.table::setnames(asia.pnpla3.group.pc, "minor", "pnpla3")
write.csv(asia.pnpla3.group.pc, "1. Modern Populations/1. Data/asia_modern_pnpla3.csv")