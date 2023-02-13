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

pie.chart.preparation <- function(europe.group.genotype, gene)
{
  # preparing for pie charts
  europe.group.genotype["reference"] <- 1 - europe.group.genotype[,6]
  data.table::setnames(europe.group.genotype, gene, "minor")
  # scaling factor
  europe.group.genotype <- tibble::add_column(europe.group.genotype, scale = rep(c(0), times = nrow(europe.group.genotype)), .after = "reference")
  europe.group.genotype$scale[europe.group.genotype$individuals < 10]                                           <- 2
  europe.group.genotype$scale[europe.group.genotype$individuals > 10 & europe.group.genotype$individuals < 35]  <- 3
  europe.group.genotype$scale[europe.group.genotype$individuals > 35 & europe.group.genotype$individuals < 100] <- 4
  europe.group.genotype$scale[europe.group.genotype$individuals > 100]                                          <- 5
  return(europe.group.genotype)
}

radius.labels <- c("< 10 individuals",
                   "> 100 individuals")

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
  europe.ind.modern <- dplyr::filter(individuals.data, individuals.data$date == 0) %>% filter(continent == "Europe") %>% select(c(1:11))
  europe.ind.modern <- dplyr::mutate(europe.ind.modern, group = ifelse(country == "Norway", 3, group))
  
  # =============================================================================
  # PNPLA3
  # =============================================================================
  # exclude individuals with no allele information
  europe.pnpla3 <- dplyr::filter(europe.ind.modern, europe.ind.modern$pnpla3 != 9)
  europe.pnpla3$subregion <- as.character(europe.pnpla3$subregion)
  # ind -> country summarization
  europe.pnpla3.country <- dplyr::group_by(europe.pnpla3, country) %>% summarise(individuals = n(),
                                                                                 subregion   = max(subregion),
                                                                                 mean.ycoord = mean(ycoord),
                                                                                 mean.xcoord = mean(xcoord),
                                                                                 group       = mean(group),
                                                                                 pnpla3      = compute.allele.freq(pnpla3))
  # weighting for group allele frequencies
  europe.pnpla3.country.tmp <- europe.pnpla3.country
  europe.pnpla3.country.tmp$pnpla3 <- europe.pnpla3.country$pnpla3 * europe.pnpla3.country$individuals
  # group summarization
  europe.pnpla3.group <- dplyr::group_by(europe.pnpla3.country.tmp, group) %>% summarise(subregion   = max(subregion),
                                                                                         individuals = sum(individuals),
                                                                                         mean.ycoord = mean(mean.ycoord),
                                                                                         mean.xcoord = mean(mean.xcoord),
                                                                                         pnpla3      = sum(pnpla3)/individuals)
  # preparing for pie charts
  europe.pnpla3.group.pc <- pie.chart.preparation(europe.pnpla3.group, "pnpla3")
  # plot
  # offsets
  europe.pnpla3.group.pc$mean.xcoord[3] <- europe.pnpla3.group.pc$mean.xcoord[3] + 3
  x.offset <- c(-5,5,15,-15,-4, -6, 0)
  y.offset <- c(-5,0,-5,  0,-4,-10,12)
  pnpla3.plot <- ggplot(data = world.card) +
                  geom_sf(fill = "antiquewhite") +
                  # just mapping europe
                  coord_sf(xlim = c(-40, 60), ylim = c(-40,80), expand = FALSE) +
                  # data 
                  geom_point(data = europe.pnpla3.group.pc,
                             aes(x = mean.xcoord, y = mean.ycoord),
                             size = 1.5) +
                  geom_segment(data = europe.pnpla3.group.pc, aes(x=mean.xcoord, y=mean.ycoord, xend = mean.xcoord+x.offset, yend = mean.ycoord+y.offset),
                               size = 0.5) +
                  geom_scatterpie(data = europe.pnpla3.group.pc, aes(x=mean.xcoord+x.offset, y=mean.ycoord+y.offset, r=scale),
                                  cols = c("minor", "reference"), color = NA) +
                  geom_label_repel(data = europe.pnpla3.group.pc,
                                   aes(x = mean.xcoord, y = mean.ycoord, label = group),
                                   color = "black",
                                   label.padding = 0.15,
                                   size          = 3.5) +
                  # scatterpie legend
                  custom_geom_scatterpie_legend(europe.pnpla3.group.pc$scale, x = -20, y = -20, n = 2, labeller = function(x) x = radius.labels, textsize = 3.5) +
                  # modify the continent.map
                  xlab("Longitude") + ylab("Latitude") +
                  ggtitle("PNPLA3: allele frequencies Europe") +
                  theme(panel.grid.major = element_line(color = gray(.5), linetype = "dashed", size = 0.5),
                        panel.background = element_rect(fill = "aliceblue"),
                        panel.border     = element_rect(fill = NA))
  
  
  # =============================================================================
  # Export
  # =============================================================================
  # tables
  data.table::setnames(europe.pnpla3.group.pc, "minor", "pnpla3")
  write.csv(europe.pnpla3.group.pc, "1. Modern Populations/1. Data/europe_modern_pnpla3.csv")