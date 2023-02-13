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

pie.chart.preparation <- function(oceania.group.genotype, gene)
{
  # preparing for pie charts
  oceania.group.genotype["reference"] <- 1 - oceania.group.genotype[,6]
  data.table::setnames(oceania.group.genotype, gene, "minor")
  # scaling factor
  oceania.group.genotype <- tibble::add_column(oceania.group.genotype, scale = rep(c(0), times = nrow(oceania.group.genotype)), .after = "reference")
  oceania.group.genotype$scale[oceania.group.genotype$individuals < 10]                                            <- 2
  oceania.group.genotype$scale[oceania.group.genotype$individuals > 10 & oceania.group.genotype$individuals < 35]  <- 3
  oceania.group.genotype$scale[oceania.group.genotype$individuals > 35 & oceania.group.genotype$individuals < 100] <- 4
  oceania.group.genotype$scale[oceania.group.genotype$individuals > 100]                                           <- 5
  return(oceania.group.genotype)
}

radius.labels <- c("< 10 individuals",
                   "35 < individuals < 100")

custom_geom_scatterpie_legend <- function (radius, x, y, n = 2, labeller, textsize=1) 
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
oceania.ind.modern <- dplyr::filter(individuals.data, individuals.data$date == 0) %>% filter(continent == "Oceania") %>% select(c(1:11))

# =============================================================================
# PNPLA3
# =============================================================================
# exclude individuals with no allele information
oceania.pnpla3 <- dplyr::filter(oceania.ind.modern, oceania.ind.modern$pnpla3 != 9)
oceania.pnpla3$subregion <- as.character(oceania.pnpla3$subregion)
# ind -> country summarization
oceania.pnpla3.country <- dplyr::group_by(oceania.pnpla3, country) %>% summarise(individuals = n(),
                                                                                 subregion   = max(subregion),
                                                                                 mean.ycoord = mean(ycoord),
                                                                                 mean.xcoord = mean(xcoord),
                                                                                 group       = mean(group),
                                                                                 pnpla3      = compute.allele.freq(pnpla3))
# weighting for group allele frequencies
oceania.pnpla3.country.tmp <- oceania.pnpla3.country
oceania.pnpla3.country.tmp$pnpla3 <- oceania.pnpla3.country$pnpla3 * oceania.pnpla3.country$individuals
# group summarization
oceania.pnpla3.group <- dplyr::group_by(oceania.pnpla3.country.tmp, group) %>% summarise(subregion = max(subregion),
                                                                                         individuals = sum(individuals),
                                                                                         mean.ycoord = mean(mean.ycoord),
                                                                                         mean.xcoord = mean(mean.xcoord),
                                                                                         pnpla3      = sum(pnpla3)/individuals)
# preparing for pie charts
oceania.pnpla3.group.pc <- pie.chart.preparation(oceania.pnpla3.group, "pnpla3")
# plot
# offsets
x.offset <- c(10,-10, 0)
y.offset <- c( 0,  0,10)
pnpla3.plot <- ggplot(data = world.card) +
                geom_sf(fill = "antiquewhite") +
                # just mapping oceania
                coord_sf(xlim = c(60, 180), ylim = c(-50, 20), expand = FALSE) +
                # data 
                geom_point(data = oceania.pnpla3.group.pc,
                           aes(x = mean.xcoord, y = mean.ycoord),
                           size = 1.5) +
                geom_segment(data = oceania.pnpla3.group.pc, aes(x=mean.xcoord, y=mean.ycoord, xend = mean.xcoord+x.offset, yend = mean.ycoord+y.offset),
                             size = 0.5) +
                geom_scatterpie(data = oceania.pnpla3.group.pc, aes(x=mean.xcoord+x.offset, y=mean.ycoord+y.offset, r=scale),
                                cols = c("minor", "reference"), color = NA) +
                geom_label_repel(data = oceania.pnpla3.group.pc,
                                 aes(x = mean.xcoord, y = mean.ycoord, label = group),
                                 color = "black",
                                 label.padding = 0.15,
                                 size          = 3.5) +
                # scatterpie legend
                custom_geom_scatterpie_legend(oceania.pnpla3.group.pc$scale, x = 80, y = -40, n = 2, labeller = function(x) x = radius.labels, textsize = 3.5) +
                # modify the continent.map
                xlab("Longitude") + ylab("Latitude") +
                ggtitle("PNPLA3: allele frequencies oceania") +
                theme(panel.grid.major = element_line(color = gray(.5), linetype = "dashed", size = 0.5),
                      panel.background = element_rect(fill = "aliceblue"),
                      panel.border     = element_rect(fill = NA))


# =============================================================================
# Export
# =============================================================================
# tables
data.table::setnames(oceania.pnpla3.group.pc, "minor", "pnpla3")
write.csv(oceania.pnpla3.group.pc, "1. Modern Populations/1. Data/oceania_modern_pnpla3.csv")