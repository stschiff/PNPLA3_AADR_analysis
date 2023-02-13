library(tidyverse)
library(ggplot2)
library(scatterpie)
library(grid)
library(rnaturalearth)
library(ggrepel) 

scaling.factor <- 2

world.card <- ne_countries(scale = "medium", returnclass = "sf")
PROJ       <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"

# =============================================================================
# Functions
# =============================================================================
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
# pnpla3
africa.modern.pnpla3  <- tibble::as_tibble(read.csv(file = "1. Modern Populations/1. Data/africa_modern_pnpla3.csv")) %>% dplyr::select(-1)
america.modern.pnpla3 <- tibble::as_tibble(read.csv(file = "1. Modern Populations/1. Data/america_modern_pnpla3.csv")) %>% dplyr::select(-1)
asia.modern.pnpla3    <- tibble::as_tibble(read.csv(file = "1. Modern Populations/1. Data/asia_modern_pnpla3.csv")) %>% dplyr::select(-1)
europe.modern.pnpla3  <- tibble::as_tibble(read.csv(file = "1. Modern Populations/1. Data/europe_modern_pnpla3.csv")) %>% dplyr::select(-1)
oceania.modern.pnpla3 <- tibble::as_tibble(read.csv(file = "1. Modern Populations/1. Data/oceania_modern_pnpla3.csv")) %>% dplyr::select(-1)
# scaling
africa.modern.pnpla3$scale  <- africa.modern.pnpla3$scale * scaling.factor
america.modern.pnpla3$scale <- america.modern.pnpla3$scale * scaling.factor
asia.modern.pnpla3$scale    <- asia.modern.pnpla3$scale * scaling.factor
europe.modern.pnpla3$scale  <- europe.modern.pnpla3$scale * scaling.factor
oceania.modern.pnpla3$scale <- oceania.modern.pnpla3$scale * scaling.factor
# =============================================================================
# Offsets
# =============================================================================
x.offset.africa  <- c(10,18,-15,15,18,-10)
y.offset.africa  <- c( 0,-8,-10, 5,-8,-10)
x.offset.america <- c(-15,10,-8,-20,-20, 0,-15)
y.offset.america <- c(-10, 0, 8, -5, -5,15,  0)
x.offset.asia    <- c(5,13,10,7,-18,10,13,-12, 0)
y.offset.asia    <- c(5,-8, 0,7, -5, 0, 0,  0,10)
x.offset.europe  <- c(-5,5,15,-15,-4,-5, 0)
y.offset.europe  <- c(-5,0,-5,  0,-4, 5,12)
x.offset.oceania <- c(15,-15,15)
y.offset.oceania <- c(-5,  0, 5)

# =============================================================================
# PNPLA3 World Plot
# =============================================================================
pnpla3.plot <- ggplot(data = world.card) +
                geom_sf(fill = "antiquewhite") +
                coord_sf(xlim = c(-145,180), ylim = c(-60,80), expand = FALSE) +
                # Africa
                geom_point(data = africa.modern.pnpla3,
                           aes(x = mean.xcoord, y = mean.ycoord),
                           size = 1.5) +
                geom_segment(data = africa.modern.pnpla3, aes(x=mean.xcoord, y=mean.ycoord, xend = mean.xcoord+x.offset.africa, yend = mean.ycoord+y.offset.africa),
                             size = 0.5) +
                geom_scatterpie(data = africa.modern.pnpla3, aes(x=mean.xcoord+x.offset.africa, y=mean.ycoord+y.offset.africa, r=scale),
                                cols = c("pnpla3", "reference"), color = NA, legend_name = "allele") +
                # America
                geom_point(data = america.modern.pnpla3,
                           aes(x = mean.xcoord, y = mean.ycoord),
                           size = 1.5) +
                geom_segment(data = america.modern.pnpla3, aes(x=mean.xcoord, y=mean.ycoord, xend = mean.xcoord+x.offset.america, yend = mean.ycoord+y.offset.america),
                             size = 0.5) +
                geom_scatterpie(data = america.modern.pnpla3, aes(x=mean.xcoord+x.offset.america, y=mean.ycoord+y.offset.america, r=scale),
                                cols = c("pnpla3", "reference"), color = NA) +
                # Asia
                geom_point(data = asia.modern.pnpla3,
                           aes(x = mean.xcoord, y = mean.ycoord),
                           size = 1.5) +
                geom_segment(data = asia.modern.pnpla3, aes(x=mean.xcoord, y=mean.ycoord, xend = mean.xcoord+x.offset.asia, yend = mean.ycoord+y.offset.asia),
                             size = 0.5) +
                geom_scatterpie(data = asia.modern.pnpla3, aes(x=mean.xcoord+x.offset.asia, y=mean.ycoord+y.offset.asia, r=scale),
                                cols = c("pnpla3", "reference"), color = NA, legend_name = "allele") +
                # Europe
                geom_point(data = europe.modern.pnpla3,
                           aes(x = mean.xcoord, y = mean.ycoord),
                           size = 1.5) +
                geom_segment(data = europe.modern.pnpla3, aes(x=mean.xcoord, y=mean.ycoord, xend = mean.xcoord+x.offset.europe, yend = mean.ycoord+y.offset.europe),
                             size = 0.5) +
                geom_scatterpie(data = europe.modern.pnpla3, aes(x=mean.xcoord+x.offset.europe, y=mean.ycoord+y.offset.europe, r=scale),
                                cols = c("pnpla3", "reference"), color = NA, legend_name = "allele") +
                # Oceania
                geom_point(data = oceania.modern.pnpla3,
                           aes(x = mean.xcoord, y = mean.ycoord),
                           size = 1.5) +
                geom_segment(data = oceania.modern.pnpla3, aes(x=mean.xcoord, y=mean.ycoord, xend = mean.xcoord+x.offset.oceania, yend = mean.ycoord+y.offset.oceania),
                             size = 0.5) +
                geom_scatterpie(data = oceania.modern.pnpla3, aes(x=mean.xcoord+x.offset.oceania, y=mean.ycoord+y.offset.oceania, r=scale),
                                cols = c("pnpla3", "reference"), color = NA) +
                # scatterpie legend
                custom_geom_scatterpie_legend(asia.modern.pnpla3$scale, x = -40, y = -46, n = 4, labeller = function(x) x = radius.labels, textsize = 3.5) +
                # Labels
                xlab("Longitude") + ylab("Latitude") +
                ggtitle("pnpla3: allele frequencies world distribution") +
                theme(panel.grid.major = element_line(color = gray(.5), linetype = "dashed", size = 0.5),
                      panel.background = element_rect(fill = "aliceblue"),
                      panel.border     = element_rect(fill = NA))
