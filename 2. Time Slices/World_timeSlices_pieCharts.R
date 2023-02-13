library(tidyverse)
library(ggplot2)
library(scatterpie)
library(rnaturalearth)

world.card <- ne_countries(scale = "medium", returnclass = "sf")
PROJ       <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"

scaling.factor  <- 2
point.size      <- 0.75
line.size       <- 0.2
axis.text.size  <- 5
axis.title.size <- 6
plot.title.size <- 8

# =============================================================================
# Functions
# =============================================================================
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
                                   start = ~start, end = ~end), data = dd, inherit.aes = FALSE, size = 0.3), 
       geom_segment(aes_(x = ~x, xend = ~x + maxr * 1.5, y = ~y + 
                           r, yend = ~y + r), data = dd, inherit.aes = FALSE, size = 0.3), 
       geom_text(aes_(x = ~x + maxr * 1.6, y = ~y + r, label = ~label), 
                 data = dd, hjust = "left", inherit.aes = FALSE,size=textsize))
}

# =============================================================================
# Input Data
# =============================================================================
# Time Slice 1
africa.ts.1.pnpla3  <- tibble::as_tibble(read.csv(file = "2. Time Slices/1. Data/africa_timeSlice1_pnpla3.csv")) %>% dplyr::select(-1)
america.ts.1.pnpla3 <- tibble::as_tibble(read.csv(file = "2. Time Slices/1. Data/america_timeSlice1_pnpla3.csv")) %>% dplyr::select(-1)
asia.ts.1.pnpla3    <- tibble::as_tibble(read.csv(file = "2. Time Slices/1. Data/asia_timeSlice1_pnpla3.csv")) %>% dplyr::select(-1)
europe.ts.1.pnpla3  <- tibble::as_tibble(read.csv(file = "2. Time Slices/1. Data/europe_timeSlice1_pnpla3.csv")) %>% dplyr::select(-1)
oceania.ts.1.pnpla3 <- tibble::as_tibble(read.csv(file = "2. Time Slices/1. Data/oceania_timeSlice1_pnpla3.csv")) %>% dplyr::select(-1)
# scaling
africa.ts.1.pnpla3$scale  <- africa.ts.1.pnpla3$scale  * scaling.factor
america.ts.1.pnpla3$scale <- america.ts.1.pnpla3$scale * scaling.factor
asia.ts.1.pnpla3$scale    <- asia.ts.1.pnpla3$scale    * scaling.factor
europe.ts.1.pnpla3$scale  <- europe.ts.1.pnpla3$scale  * scaling.factor
oceania.ts.1.pnpla3$scale <- oceania.ts.1.pnpla3$scale * scaling.factor
# Time Slice 2
africa.ts.2.pnpla3  <- tibble::as_tibble(read.csv(file = "2. Time Slices/1. Data/africa_timeSlice2_pnpla3.csv")) %>% dplyr::select(-1)
america.ts.2.pnpla3 <- tibble::as_tibble(read.csv(file = "2. Time Slices/1. Data/america_timeSlice2_pnpla3.csv")) %>% dplyr::select(-1)
asia.ts.2.pnpla3    <- tibble::as_tibble(read.csv(file = "2. Time Slices/1. Data/asia_timeSlice2_pnpla3.csv")) %>% dplyr::select(-1)
europe.ts.2.pnpla3  <- tibble::as_tibble(read.csv(file = "2. Time Slices/1. Data/europe_timeSlice2_pnpla3.csv")) %>% dplyr::select(-1)
oceania.ts.2.pnpla3 <- tibble::as_tibble(read.csv(file = "2. Time Slices/1. Data/oceania_timeSlice2_pnpla3.csv")) %>% dplyr::select(-1)
# scaling
africa.ts.2.pnpla3$scale  <- africa.ts.2.pnpla3$scale  * scaling.factor
america.ts.2.pnpla3$scale <- america.ts.2.pnpla3$scale * scaling.factor
asia.ts.2.pnpla3$scale    <- asia.ts.2.pnpla3$scale    * scaling.factor
europe.ts.2.pnpla3$scale  <- europe.ts.2.pnpla3$scale  * scaling.factor
oceania.ts.2.pnpla3$scale <- oceania.ts.2.pnpla3$scale * scaling.factor
# Time Slice 3
africa.ts.3.pnpla3  <- tibble::as_tibble(read.csv(file = "2. Time Slices/1. Data/africa_timeSlice3_pnpla3.csv")) %>% dplyr::select(-1)
america.ts.3.pnpla3 <- tibble::as_tibble(read.csv(file = "2. Time Slices/1. Data/america_timeSlice3_pnpla3.csv")) %>% dplyr::select(-1)
asia.ts.3.pnpla3    <- tibble::as_tibble(read.csv(file = "2. Time Slices/1. Data/asia_timeSlice3_pnpla3.csv")) %>% dplyr::select(-1)
europe.ts.3.pnpla3  <- tibble::as_tibble(read.csv(file = "2. Time Slices/1. Data/europe_timeSlice3_pnpla3.csv")) %>% dplyr::select(-1)
# scaling
africa.ts.3.pnpla3$scale  <- africa.ts.3.pnpla3$scale  * scaling.factor
america.ts.3.pnpla3$scale <- america.ts.3.pnpla3$scale * scaling.factor
asia.ts.3.pnpla3$scale    <- asia.ts.3.pnpla3$scale    * scaling.factor
europe.ts.3.pnpla3$scale  <- europe.ts.3.pnpla3$scale  * scaling.factor

# =============================================================================
# Offsets
# =============================================================================
# time slice 1
x1.offset.africa.pnpla3  <- c( 10,20)
y1.offset.africa.pnpla3  <- c(-15, 0)

x1.offset.america.pnpla3 <- c(-15,-20,-15,15,10, 0,-10,10, 0)
y1.offset.america.pnpla3 <- c(  0,  0,  0, 0,10,10,  5,10,-7)

x1.offset.asia.pnpla3    <- c( 0, 20,10,10,  0,12,10)
y1.offset.asia.pnpla3    <- c(20,-10,10,10,-15, 0, 0)

x1.offset.europe.pnpla3  <- c(  2,-30,15,  0,-10,  2,-15,-15,10)
y1.offset.europe.pnpla3  <- c(-10,-10, 4,-15,  5,-15,  2, 10, 0)

x1.offset.oceania.pnpla3 <- c( 0)
y1.offset.oceania.pnpla3 <- c(15)
# time slice 2
x2.offset.africa.pnpla3  <- c(  0,15,-8,15)
y2.offset.africa.pnpla3  <- c(-10, 0,-8, 0)
x2.offset.america.pnpla3 <- c(-10,15,-5,10,-10,10,-15, 0,-10,10,  0)
y2.offset.america.pnpla3 <- c(  5,-5, 5, 0, -5, 0, -5,10,  0,10,-10)
x2.offset.asia.pnpla3    <- c(15,15,10,  0,  0,7,  0)
y2.offset.asia.pnpla3    <- c( 0, 0,10,-10,-10,0,-15)
x2.offset.europe.pnpla3  <- c(  1,13,20,-20,0, -5,-20,-10,15)
y2.offset.europe.pnpla3  <- c(-10,20, 5, -5,8,-10,  5, 10, 0)
x2.offset.oceania.pnpla3 <- c( 0)
y2.offset.oceania.pnpla3 <- c(15)
# time slice 3
x3.offset.africa.pnpla3  <- c(  0,-10,-10)
y3.offset.africa.pnpla3  <- c(-10,  0,-10)
x3.offset.america.pnpla3 <- c( 0,-10,-7,-10)
y3.offset.america.pnpla3 <- c(10,  0,-7,  0)
x3.offset.asia.pnpla3    <- c(-10,10,10,20)
y3.offset.asia.pnpla3    <- c(  0, 0,-5, 5)
x3.offset.europe.pnpla3  <- c(  5,10,13,-10, -5,-10,-10,10)
y3.offset.europe.pnpla3  <- c(-10,20, 5,  0,-10,  5, 15, 3)

# =============================================================================
# PNPLA3 World Plot Time Slice 1 
# =============================================================================
radius.labels <- c("< 10 individuals",
                   "10 < individuals < 35",
                   "35 < individuals < 100",
                   "> 100 individuals")
data.table::setnames(africa.ts.1.pnpla3,  "pnpla3", "minor")
data.table::setnames(america.ts.1.pnpla3, "pnpla3", "minor")
data.table::setnames(asia.ts.1.pnpla3,    "pnpla3", "minor")
data.table::setnames(europe.ts.1.pnpla3,  "pnpla3", "minor")
data.table::setnames(oceania.ts.1.pnpla3, "pnpla3", "minor")
pnpla3.plot.ts1 <- ggplot(data = world.card) +
                    geom_sf(fill = "antiquewhite") +
                    coord_sf(xlim = c(-145,180), ylim = c(-60,80), expand = FALSE) +
                    # Africa
                    geom_point(data = africa.ts.1.pnpla3,
                               aes(x = mean.xcoord, y = mean.ycoord), size = point.size) +
                    geom_segment(data = africa.ts.1.pnpla3, aes(x=mean.xcoord, y=mean.ycoord, xend = mean.xcoord+x1.offset.africa.pnpla3, yend = mean.ycoord+y1.offset.africa.pnpla3),
                                 size = line.size) +
                    geom_scatterpie(data = africa.ts.1.pnpla3, aes(x=mean.xcoord+x1.offset.africa.pnpla3, y=mean.ycoord+y1.offset.africa.pnpla3, r=scale),
                                    cols = c("minor", "reference"), color = NA, legend_name = "allele") +
                    # America
                    geom_point(data = america.ts.1.pnpla3,
                               aes(x = mean.xcoord, y = mean.ycoord), size = point.size) +
                    geom_segment(data = america.ts.1.pnpla3, aes(x=mean.xcoord, y=mean.ycoord, xend = mean.xcoord+x1.offset.america.pnpla3, yend = mean.ycoord+y1.offset.america.pnpla3),
                                 size = line.size) +
                    geom_scatterpie(data = america.ts.1.pnpla3, aes(x=mean.xcoord+x1.offset.america.pnpla3, y=mean.ycoord+y1.offset.america.pnpla3, r=scale),
                                    cols = c("minor", "reference"), color = NA, legend_name = "allele") +
                    # Asia
                    geom_point(data = asia.ts.1.pnpla3,
                               aes(x = mean.xcoord, y = mean.ycoord), size = point.size) +
                    geom_segment(data = asia.ts.1.pnpla3, aes(x=mean.xcoord, y=mean.ycoord, xend = mean.xcoord+x1.offset.asia.pnpla3, yend = mean.ycoord+y1.offset.asia.pnpla3),
                                 size = line.size) +
                    geom_scatterpie(data = asia.ts.1.pnpla3, aes(x=mean.xcoord+x1.offset.asia.pnpla3, y=mean.ycoord+y1.offset.asia.pnpla3, r=scale),
                                    cols = c("minor", "reference"), color = NA, legend_name = "allele") +
                    # Europe
                    geom_point(data = europe.ts.1.pnpla3,
                               aes(x = mean.xcoord, y = mean.ycoord), size = point.size) +
                    geom_segment(data = europe.ts.1.pnpla3, aes(x=mean.xcoord, y=mean.ycoord, xend = mean.xcoord+x1.offset.europe.pnpla3, yend = mean.ycoord+y1.offset.europe.pnpla3),
                                 size = line.size) +
                    geom_scatterpie(data = europe.ts.1.pnpla3, aes(x=mean.xcoord+x1.offset.europe.pnpla3, y=mean.ycoord+y1.offset.europe.pnpla3, r=scale),
                                    cols = c("minor", "reference"), color = NA, legend_name = "allele") +
                    # Oceania
                    geom_point(data = oceania.ts.1.pnpla3,
                               aes(x = mean.xcoord, y = mean.ycoord), size = point.size) +
                    geom_segment(data = oceania.ts.1.pnpla3, aes(x=mean.xcoord, y=mean.ycoord, xend = mean.xcoord+x1.offset.oceania.pnpla3, yend = mean.ycoord+y1.offset.oceania.pnpla3),
                                 size = line.size) +
                    geom_scatterpie(data = oceania.ts.1.pnpla3, aes(x=mean.xcoord+x1.offset.oceania.pnpla3, y=mean.ycoord+y1.offset.oceania.pnpla3, r=scale),
                                    cols = c("minor", "reference"), color = NA, legend_name = "allele") + 
                    custom_geom_scatterpie_legend(asia.ts.1.pnpla3$scale, x = 55, y = -46, n = 4, labeller = function(x) x = radius.labels, textsize = 2) +
                    # Labels
                    xlab("Longitude") + ylab("Latitude") +
                    labs(title = "PNPLA3: allele frequencies world distribution - Time Slice 1 (0 - 3000)") +
                    theme(panel.grid.major = element_line(color = gray(.5), linetype = "dashed", linewidth = 0.05),
                          panel.background = element_rect(fill = "aliceblue"),
                          panel.border     = element_rect(fill = NA),
                          axis.title       = element_text(size = axis.title.size),
                          axis.text        = element_text(size = axis.text.size),
                          plot.title       = element_text(size = plot.title.size))

# =============================================================================
# PNPLA3 World Plot Time Slice 2
# =============================================================================
radius.labels <- c("< 10 individuals",
                   "10 < individuals < 35",
                   "35 < individuals < 100",
                   "> 100 individuals")
scale <- c(2,3,4,5) * scaling.factor
data.table::setnames(africa.ts.2.pnpla3,  "pnpla3", "minor")
data.table::setnames(america.ts.2.pnpla3, "pnpla3", "minor")
data.table::setnames(asia.ts.2.pnpla3,    "pnpla3", "minor")
data.table::setnames(europe.ts.2.pnpla3,  "pnpla3", "minor")
data.table::setnames(oceania.ts.2.pnpla3, "pnpla3", "minor")
pnpla3.plot.ts2 <- ggplot(data = world.card) +
                    geom_sf(fill = "antiquewhite") +
                    coord_sf(xlim = c(-145,180), ylim = c(-60,80), expand = FALSE) +
                    # Africa
                    geom_point(data = africa.ts.2.pnpla3,
                               aes(x = mean.xcoord, y = mean.ycoord), size = point.size) +
                    geom_segment(data = africa.ts.2.pnpla3, aes(x=mean.xcoord, y=mean.ycoord, xend = mean.xcoord+x2.offset.africa.pnpla3, yend = mean.ycoord+y2.offset.africa.pnpla3),
                                 size = line.size) +
                    geom_scatterpie(data = africa.ts.2.pnpla3, aes(x=mean.xcoord+x2.offset.africa.pnpla3, y=mean.ycoord+y2.offset.africa.pnpla3, r=scale),
                                    cols = c("minor", "reference"), color = NA, legend_name = "allele") +
                    # America
                    geom_point(data = america.ts.2.pnpla3,
                               aes(x = mean.xcoord, y = mean.ycoord), size = point.size) +
                    geom_segment(data = america.ts.2.pnpla3, aes(x=mean.xcoord, y=mean.ycoord, xend = mean.xcoord+x2.offset.america.pnpla3, yend = mean.ycoord+y2.offset.america.pnpla3),
                                 size = line.size) +
                    geom_scatterpie(data = america.ts.2.pnpla3, aes(x=mean.xcoord+x2.offset.america.pnpla3, y=mean.ycoord+y2.offset.america.pnpla3, r=scale),
                                    cols = c("minor", "reference"), color = NA) +
                    # Asia
                    geom_point(data = asia.ts.2.pnpla3,
                               aes(x = mean.xcoord, y = mean.ycoord), size = point.size) +
                    geom_segment(data = asia.ts.2.pnpla3, aes(x=mean.xcoord, y=mean.ycoord, xend = mean.xcoord+x2.offset.asia.pnpla3, yend = mean.ycoord+y2.offset.asia.pnpla3),
                                 size = line.size) +
                    geom_scatterpie(data = asia.ts.2.pnpla3, aes(x=mean.xcoord+x2.offset.asia.pnpla3, y=mean.ycoord+y2.offset.asia.pnpla3, r=scale),
                                    cols = c("minor", "reference"), color = NA, legend_name = "allele") +
                    # Europe
                    geom_point(data = europe.ts.2.pnpla3,
                               aes(x = mean.xcoord, y = mean.ycoord), size = point.size) +
                    geom_segment(data = europe.ts.2.pnpla3, aes(x=mean.xcoord, y=mean.ycoord, xend = mean.xcoord+x2.offset.europe.pnpla3, yend = mean.ycoord+y2.offset.europe.pnpla3),
                                 size = line.size) +
                    geom_scatterpie(data = europe.ts.2.pnpla3, aes(x=mean.xcoord+x2.offset.europe.pnpla3, y=mean.ycoord+y2.offset.europe.pnpla3, r=scale),
                                    cols = c("minor", "reference"), color = NA, legend_name = "allele") +
                    # Oceania
                    geom_point(data = oceania.ts.2.pnpla3,
                               aes(x = mean.xcoord, y = mean.ycoord), size = point.size) +
                    geom_segment(data = oceania.ts.2.pnpla3, aes(x=mean.xcoord, y=mean.ycoord, xend = mean.xcoord+x2.offset.oceania.pnpla3, yend = mean.ycoord+y2.offset.oceania.pnpla3),
                                 size = line.size) +
                    geom_scatterpie(data = oceania.ts.2.pnpla3, aes(x=mean.xcoord+x2.offset.oceania.pnpla3, y=mean.ycoord+y2.offset.oceania.pnpla3, r=scale),
                                    cols = c("minor", "reference"), color = NA) + 
                    custom_geom_scatterpie_legend(scale, x = 55, y = -46, n = 4, labeller = function(x) x = radius.labels, textsize = 2) +
                    # Labels
                    xlab("Longitude") + ylab("Latitude") +
                    labs(title = "PNPLA3: allele frequencies world distribution - Time Slice 2 (3000 - 7000)") +
                    theme(panel.grid.major = element_line(color = gray(.5), linetype = "dashed", linewidth = 0.05),
                          panel.background = element_rect(fill = "aliceblue"),
                          panel.border     = element_rect(fill = NA),
                          axis.title       = element_text(size = axis.title.size),
                          axis.text        = element_text(size = axis.text.size),
                          plot.title       = element_text(size = plot.title.size))

# =============================================================================
# PNPLA3 World Plot Time Slice 3
# =============================================================================
radius.labels <- c("< 10 individuals",
                   "10 < individuals < 35",
                   "35 < individuals < 100")
scale <- c(2,3,4) * scaling.factor
data.table::setnames(africa.ts.3.pnpla3,  "pnpla3", "minor")
data.table::setnames(america.ts.3.pnpla3, "pnpla3", "minor")
data.table::setnames(asia.ts.3.pnpla3,    "pnpla3", "minor")
data.table::setnames(europe.ts.3.pnpla3,  "pnpla3", "minor")
pnpla3.plot.ts3 <- ggplot(data = world.card) +
                    geom_sf(fill = "antiquewhite") +
                    coord_sf(xlim = c(-145,180), ylim = c(-60,80), expand = FALSE) +
                    # Africa
                    geom_point(data = africa.ts.3.pnpla3,
                               aes(x = mean.xcoord, y = mean.ycoord), size = point.size) +
                    geom_segment(data = africa.ts.3.pnpla3, aes(x=mean.xcoord, y=mean.ycoord, xend = mean.xcoord+x3.offset.africa.pnpla3, yend = mean.ycoord+y3.offset.africa.pnpla3),
                                 size = line.size) +
                    geom_scatterpie(data = africa.ts.3.pnpla3, aes(x=mean.xcoord+x3.offset.africa.pnpla3, y=mean.ycoord+y3.offset.africa.pnpla3, r=scale),
                                    cols = c("minor", "reference"), color = NA, legend_name = "allele") +
                    # America
                    geom_point(data = america.ts.3.pnpla3,
                               aes(x = mean.xcoord, y = mean.ycoord), size = point.size) +
                    geom_segment(data = america.ts.3.pnpla3, aes(x=mean.xcoord, y=mean.ycoord, xend = mean.xcoord+x3.offset.america.pnpla3, yend = mean.ycoord+y3.offset.america.pnpla3),
                                 size = line.size) +
                    geom_scatterpie(data = america.ts.3.pnpla3, aes(x=mean.xcoord+x3.offset.america.pnpla3, y=mean.ycoord+y3.offset.america.pnpla3, r=scale),
                                    cols = c("minor", "reference"), color = NA) +
                    # Asia
                    geom_point(data = asia.ts.3.pnpla3,
                               aes(x = mean.xcoord, y = mean.ycoord), size = point.size) +
                    geom_segment(data = asia.ts.3.pnpla3, aes(x=mean.xcoord, y=mean.ycoord, xend = mean.xcoord+x3.offset.asia.pnpla3, yend = mean.ycoord+y3.offset.asia.pnpla3),
                                 size = line.size) +
                    geom_scatterpie(data = asia.ts.3.pnpla3, aes(x=mean.xcoord+x3.offset.asia.pnpla3, y=mean.ycoord+y3.offset.asia.pnpla3, r=scale),
                                    cols = c("minor", "reference"), color = NA, legend_name = "allele") +
                    # Europe
                    geom_point(data = europe.ts.3.pnpla3,
                               aes(x = mean.xcoord, y = mean.ycoord), size = point.size) +
                    geom_segment(data = europe.ts.3.pnpla3, aes(x=mean.xcoord, y=mean.ycoord, xend = mean.xcoord+x3.offset.europe.pnpla3, yend = mean.ycoord+y3.offset.europe.pnpla3),
                                 size = line.size) +
                    geom_scatterpie(data = europe.ts.3.pnpla3, aes(x=mean.xcoord+x3.offset.europe.pnpla3, y=mean.ycoord+y3.offset.europe.pnpla3, r=scale),
                                    cols = c("minor", "reference"), color = NA, legend_name = "allele") +
                    custom_geom_scatterpie_legend(scale, x = 55, y = -46, n = 4, labeller = function(x) x = radius.labels, textsize = 2) +
                    # Labels
                    xlab("Longitude") + ylab("Latitude") +
                    labs(title = "PNPLA3: allele frequencies world distribution - Time Slice 3 (7000 - 15000)") +
                    theme(panel.grid.major = element_line(color = gray(.5), linetype = "dashed", linewidth = 0.05),
                          panel.background = element_rect(fill = "aliceblue"),
                          panel.border     = element_rect(fill = NA),
                          axis.title       = element_text(size = axis.title.size),
                          axis.text        = element_text(size = axis.text.size),
                          plot.title       = element_text(size = plot.title.size))

# =============================================================================
# PNPLA3 World Plot combined
# =============================================================================
pnpla3.plot <- cowplot::plot_grid(pnpla3.plot.ts1, pnpla3.plot.ts2, pnpla3.plot.ts3, ncol = 1, nrow = 3, labels = "AUTO")
ggsave(filename = "TimeSlices.pdf", plot = pnpla3.plot, device = "pdf", path = "5. Plots", width = 18, height = 26, units = "cm")
ggsave(filename = "TimeSlices.png", plot = pnpla3.plot, device = "png", path = "5. Plots", width = 18, height = 26, units = "cm")
