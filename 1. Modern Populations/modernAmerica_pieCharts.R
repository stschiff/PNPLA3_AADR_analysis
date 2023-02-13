library(tidyverse)
library(rnaturalearth)
library(ggplot2)
library(scatterpie)
library(ggrepel)

options(digits = 5)

world.card <- ne_countries(scale = "medium", returnclass = "sf")
PROJ       <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"

# offsets
x.offset <- c(5,10,-5,-15,-12, 0,7)
y.offset <- c(5, 0, 5,  0, -5,10,7)

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
  america.group.genotype["reference"] <- 1 - america.group.genotype[,6]
  data.table::setnames(america.group.genotype, gene, "minor")
  # scaling factor
  america.group.genotype <- tibble::add_column(america.group.genotype, scale = rep(c(0), times = nrow(america.group.genotype)), .after = "reference")
  america.group.genotype$scale[america.group.genotype$individuals < 10]                                            <- 2
  america.group.genotype$scale[america.group.genotype$individuals > 10 & america.group.genotype$individuals < 35]  <- 3
  america.group.genotype$scale[america.group.genotype$individuals > 35 & america.group.genotype$individuals < 100] <- 4
  america.group.genotype$scale[america.group.genotype$individuals > 100]                                           <- 5
  return(america.group.genotype)
}

radius.labels <- c("< 10 individuals",
                   "10 < individuals < 35",
                   "35 < individuals < 100",
                   "> 100 individuals")

custom_geom_scatterpie_legend <- function (radius, x, y, n = 4, labeller, textsize=1) 
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
individuals.data   <- tibble::as_tibble(read.csv(file = "0. Input Data/individuals_data.csv")) %>% dplyr::select(-1)
america.ind.modern <- dplyr::filter(individuals.data, individuals.data$date == 0) %>% filter(continent == "North America" | continent == "South America") %>% select(c(1:11))

# =============================================================================
# PNPLA3
# =============================================================================
# exclude individuals with no allele information
america.pnpla3 <- dplyr::filter(america.ind.modern, america.ind.modern$pnpla3 != 9)
america.pnpla3$subregion <- as.character(america.pnpla3$subregion)
# ind -> country summarization
america.pnpla3.country <- dplyr::group_by(america.pnpla3, country) %>% summarise(individuals = n(),
                                                                                 subregion   = max(subregion),
                                                                                 mean.ycoord = mean(ycoord),
                                                                                 mean.xcoord = mean(xcoord),
                                                                                 group       = mean(group),
                                                                                 pnpla3      = compute.allele.freq(pnpla3))
# weighting for group allele frequencies
america.pnpla3.country.tmp <- america.pnpla3.country
america.pnpla3.country.tmp$pnpla3 <- america.pnpla3.country$pnpla3 * america.pnpla3.country$individuals
# group summarization
america.pnpla3.group <- dplyr::group_by(america.pnpla3.country.tmp, group) %>% summarise(subregion = max(subregion),
                                                                                         individuals = sum(individuals),
                                                                                         mean.ycoord = mean(mean.ycoord),
                                                                                         mean.xcoord = mean(mean.xcoord),
                                                                                         pnpla3      = sum(pnpla3)/individuals)
# preparing for pie charts
america.pnpla3.group.pc <- pie.chart.preparation(america.pnpla3.group, "pnpla3")
# plot
pnpla3.plot <- ggplot(data = world.card) +
                geom_sf(fill = "antiquewhite") +
                # just mapping america
                coord_sf(xlim = c(-180, -10), expand = FALSE) +
                # data 
                geom_point(data = america.pnpla3.group.pc,
                           aes(x = mean.xcoord, y = mean.ycoord),
                           size = 1.5) +
                geom_segment(data = america.pnpla3.group.pc, aes(x=mean.xcoord, y=mean.ycoord, xend = mean.xcoord+x.offset, yend = mean.ycoord+y.offset),
                             size = 0.5) +
                geom_scatterpie(data = america.pnpla3.group.pc, aes(x=mean.xcoord+x.offset, y=mean.ycoord+y.offset, r=scale),
                                cols = c("minor", "reference"), color = NA) +
                geom_label_repel(data = america.pnpla3.group.pc,
                                 aes(x = mean.xcoord, y = mean.ycoord, label = group),
                                 color = "black",
                                 label.padding = 0.15,
                                 size          = 3.5) +
                # scatterpie legend
                custom_geom_scatterpie_legend(america.pnpla3.group.pc$scale, x = -150, y = -60, n = 4, labeller = function(x) x = radius.labels, textsize = 3.5) +
                # modify the continent.map
                xlab("Longitude") + ylab("Latitude") +
                ggtitle("PNPLA3: allele frequencies America") +
                theme(panel.grid.major = element_line(color = gray(.5), linetype = "dashed", size = 0.5),
                      panel.background = element_rect(fill = "aliceblue"),
                      panel.border     = element_rect(fill = NA))


# =============================================================================
# Export
# =============================================================================
# tables
data.table::setnames(america.pnpla3.group.pc, "minor", "pnpla3")
write.csv(america.pnpla3.group.pc, "1. Modern Populations/1. Data/america_modern_pnpla3.csv")
