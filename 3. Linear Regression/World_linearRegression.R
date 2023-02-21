library(tidyverse)
library(flextable)
library(ggplot2)
library(ggpubr)
library(ggpmisc)
library(grid)
library(ggrepel)
library(officer)

# =============================================================================
# Input Data
# =============================================================================
# Africa
africa.modern.pnpla3 <- tibble::as_tibble(read.csv(file = "1. Modern Populations/1. Data/africa_modern_pnpla3.csv")) %>% dplyr::select(-1)
africa.ts.1.pnpla3   <- tibble::as_tibble(read.csv(file = "2. Time Slices/1. Data/africa_timeSlice1_pnpla3.csv")) %>% dplyr::select(-1)
africa.ts.2.pnpla3   <- tibble::as_tibble(read.csv(file = "2. Time Slices/1. Data/africa_timeSlice2_pnpla3.csv")) %>% dplyr::select(-1)
africa.ts.3.pnpla3   <- tibble::as_tibble(read.csv(file = "2. Time Slices/1. Data/africa_timeSlice3_pnpla3.csv")) %>% dplyr::select(-1)
# America
america.modern.pnpla3 <- tibble::as_tibble(read.csv(file = "1. Modern Populations/1. Data/america_modern_pnpla3.csv")) %>% dplyr::select(-1)
america.ts.1.pnpla3   <- tibble::as_tibble(read.csv(file = "2. Time Slices/1. Data/america_timeSlice1_pnpla3.csv")) %>% dplyr::select(-1)
america.ts.2.pnpla3   <- tibble::as_tibble(read.csv(file = "2. Time Slices/1. Data/america_timeSlice2_pnpla3.csv")) %>% dplyr::select(-1)
america.ts.3.pnpla3   <- tibble::as_tibble(read.csv(file = "2. Time Slices/1. Data/america_timeSlice3_pnpla3.csv")) %>% dplyr::select(-1)
# Asia
asia.modern.pnpla3 <- tibble::as_tibble(read.csv(file = "1. Modern Populations/1. Data/asia_modern_pnpla3.csv")) %>% dplyr::select(-1)
asia.ts.1.pnpla3   <- tibble::as_tibble(read.csv(file = "2. Time Slices/1. Data/asia_timeSlice1_pnpla3.csv")) %>% dplyr::select(-1)
asia.ts.2.pnpla3   <- tibble::as_tibble(read.csv(file = "2. Time Slices/1. Data/asia_timeSlice2_pnpla3.csv")) %>% dplyr::select(-1)
asia.ts.3.pnpla3   <- tibble::as_tibble(read.csv(file = "2. Time Slices/1. Data/asia_timeSlice3_pnpla3.csv")) %>% dplyr::select(-1)
# Europe
europe.modern.pnpla3 <- tibble::as_tibble(read.csv(file = "1. Modern Populations/1. Data/europe_modern_pnpla3.csv")) %>% dplyr::select(-1)
europe.ts.1.pnpla3   <- tibble::as_tibble(read.csv(file = "2. Time Slices/1. Data/europe_timeSlice1_pnpla3.csv")) %>% dplyr::select(-1)
europe.ts.2.pnpla3   <- tibble::as_tibble(read.csv(file = "2. Time Slices/1. Data/europe_timeSlice2_pnpla3.csv")) %>% dplyr::select(-1)
europe.ts.3.pnpla3   <- tibble::as_tibble(read.csv(file = "2. Time Slices/1. Data/europe_timeSlice3_pnpla3.csv")) %>% dplyr::select(-1)

# =============================================================================
# Modern Populations
# =============================================================================
# Africa
africa.modern.pnpla3$subregion <- as.character(africa.modern.pnpla3$subregion)
modern.dp.pnpla3.africa <- data.table::data.table(group       = 0,
                                                  subregion   = "Africa",
                                                  individuals = sum(africa.modern.pnpla3$individuals),
                                                  mean.ycoord = mean(africa.modern.pnpla3$mean.ycoord),
                                                  mean.xcoord = mean(africa.modern.pnpla3$mean.xcoord),
                                                  date        = 0,
                                                  pnpla3      = sum(africa.modern.pnpla3$pnpla3*africa.modern.pnpla3$individuals)/sum(africa.modern.pnpla3$individuals),
                                                  reference   = 1 - sum(africa.modern.pnpla3$pnpla3*africa.modern.pnpla3$individuals)/sum(africa.modern.pnpla3$individuals),
                                                  scale       = 5)
# America
america.modern.pnpla3$subregion <- as.character(america.modern.pnpla3$subregion)
modern.dp.pnpla3.america <- data.table::data.table(group       = 0,
                                                   subregion   = "America",
                                                   individuals = sum(america.modern.pnpla3$individuals),
                                                   mean.ycoord = mean(america.modern.pnpla3$mean.ycoord),
                                                   mean.xcoord = mean(america.modern.pnpla3$mean.xcoord),
                                                   date        = 0,
                                                   pnpla3      = sum(america.modern.pnpla3$pnpla3*america.modern.pnpla3$individuals)/sum(america.modern.pnpla3$individuals),
                                                   reference   = 1 - sum(america.modern.pnpla3$pnpla3*america.modern.pnpla3$individuals)/sum(america.modern.pnpla3$individuals),
                                                   scale       = 5)
# Asia
asia.modern.pnpla3$subregion <- as.character(asia.modern.pnpla3$subregion)
modern.dp.pnpla3.asia <- data.table::data.table(group       = 0,
                                                subregion   = "Asia",
                                                individuals = sum(asia.modern.pnpla3$individuals),
                                                mean.ycoord = mean(asia.modern.pnpla3$mean.ycoord),
                                                mean.xcoord = mean(asia.modern.pnpla3$mean.xcoord),
                                                date        = 0,
                                                pnpla3      = sum(asia.modern.pnpla3$pnpla3*asia.modern.pnpla3$individuals)/sum(asia.modern.pnpla3$individuals),
                                                reference   = 1 - sum(asia.modern.pnpla3$pnpla3*asia.modern.pnpla3$individuals)/sum(asia.modern.pnpla3$individuals),
                                                scale       = 5)
# Europe
europe.modern.pnpla3$subregion <- as.character(europe.modern.pnpla3$subregion)
modern.dp.pnpla3.europe <- data.table::data.table(group       = 0,
                                                  subregion   = "Europe",
                                                  individuals = sum(europe.modern.pnpla3$individuals),
                                                  mean.ycoord = mean(europe.modern.pnpla3$mean.ycoord),
                                                  mean.xcoord = mean(europe.modern.pnpla3$mean.xcoord),
                                                  date        = 0,
                                                  pnpla3      = sum(europe.modern.pnpla3$pnpla3*europe.modern.pnpla3$individuals)/sum(europe.modern.pnpla3$individuals),
                                                  reference   = 1 - sum(europe.modern.pnpla3$pnpla3*europe.modern.pnpla3$individuals)/sum(europe.modern.pnpla3$individuals),
                                                  scale       = 5)

# =============================================================================
# Merging ancient and modern populations
# =============================================================================
# Africa
africa.pnpla3 <- rbind(modern.dp.pnpla3.africa, africa.ts.1.pnpla3) %>% rbind(africa.ts.2.pnpla3) %>% rbind(africa.ts.3.pnpla3)
africa.pnpla3$date  <- africa.pnpla3$date * (-1)
africa.pnpla3$group <- c(0,1,2,4,5,3,8,7,6)
africa.pnpla3$scale <- africa.pnpla3$scale / 5
# America
america.pnpla3 <- rbind(modern.dp.pnpla3.america, america.ts.1.pnpla3) %>% rbind(america.ts.2.pnpla3) %>% rbind(america.ts.3.pnpla3)
america.pnpla3$date  <- america.pnpla3$date * (-1)
america.pnpla3$group <- c(0,1,3,2,9,11,5,7,4,10,6,8,18,16,15,17,13,12,14,21,19,20,22)
america.pnpla3$scale <- america.pnpla3$scale / 5
# Asia
asia.pnpla3 <- rbind(modern.dp.pnpla3.asia, asia.ts.1.pnpla3) %>% rbind(asia.ts.2.pnpla3) %>% rbind(asia.ts.3.pnpla3)
asia.pnpla3$date  <- asia.pnpla3$date * (-1)
asia.pnpla3$group <- c(0,7,4,2,1,6,5,3,9,8,12,10,13,11,14,15,17,16)
asia.pnpla3$scale <- asia.pnpla3$scale / 5
# Europe
europe.pnpla3 <- rbind(modern.dp.pnpla3.europe, europe.ts.1.pnpla3) %>% rbind(europe.ts.2.pnpla3) %>% rbind(europe.ts.3.pnpla3)
europe.pnpla3$date  <- europe.pnpla3$date * (-1)
europe.pnpla3$group <- c(0,8,6,7,5,1,4,3,2,9,14,13,17,15,10,12,11,16,21,22,19,18,24,25,23,20)
europe.pnpla3$scale <- europe.pnpla3$scale / 5

# =============================================================================
# Table Data
# =============================================================================
# Africa
africa.ts.1.group.table.data.pnpla3 <- tibble::as_tibble(read.csv(file = "2. Time Slices/1. Data/africa_timeSlice1_group_table_data_pnpla3.csv")) %>% dplyr::select(-1)
africa.ts.2.group.table.data.pnpla3 <- tibble::as_tibble(read.csv(file = "2. Time Slices/1. Data/africa_timeSlice2_group_table_data_pnpla3.csv")) %>% dplyr::select(-1)
africa.ts.3.group.table.data.pnpla3 <- tibble::as_tibble(read.csv(file = "2. Time Slices/1. Data/africa_timeSlice3_group_table_data_pnpla3.csv")) %>% dplyr::select(-1)
# America
america.ts.1.group.table.data.pnpla3 <- tibble::as_tibble(read.csv(file = "2. Time Slices/1. Data/america_timeSlice1_group_table_data_pnpla3.csv")) %>% dplyr::select(-1)
america.ts.2.group.table.data.pnpla3 <- tibble::as_tibble(read.csv(file = "2. Time Slices/1. Data/america_timeSlice2_group_table_data_pnpla3.csv")) %>% dplyr::select(-1)
america.ts.3.group.table.data.pnpla3 <- tibble::as_tibble(read.csv(file = "2. Time Slices/1. Data/america_timeSlice3_group_table_data_pnpla3.csv")) %>% dplyr::select(-1)
# Asia
asia.ts.1.group.table.data.pnpla3 <- tibble::as_tibble(read.csv(file = "2. Time Slices/1. Data/asia_timeSlice1_group_table_data_pnpla3.csv")) %>% dplyr::select(-1)
asia.ts.2.group.table.data.pnpla3 <- tibble::as_tibble(read.csv(file = "2. Time Slices/1. Data/asia_timeSlice2_group_table_data_pnpla3.csv")) %>% dplyr::select(-1)
asia.ts.3.group.table.data.pnpla3 <- tibble::as_tibble(read.csv(file = "2. Time Slices/1. Data/asia_timeSlice3_group_table_data_pnpla3.csv")) %>% dplyr::select(-1)
# Europe
europe.ts.1.group.table.data.pnpla3 <- tibble::as_tibble(read.csv(file = "2. Time Slices/1. Data/europe_timeSlice1_group_table_data_pnpla3.csv")) %>% dplyr::select(-1)
europe.ts.2.group.table.data.pnpla3 <- tibble::as_tibble(read.csv(file = "2. Time Slices/1. Data/europe_timeSlice2_group_table_data_pnpla3.csv")) %>% dplyr::select(-1)
europe.ts.3.group.table.data.pnpla3 <- tibble::as_tibble(read.csv(file = "2. Time Slices/1. Data/europe_timeSlice3_group_table_data_pnpla3.csv")) %>% dplyr::select(-1)

# =============================================================================
# Group Table
# =============================================================================
std_border = officer::fp_border(color="black", width = 1)
# Africa
africa.group.table.data.pnpla3 <- rbind(africa.ts.1.group.table.data.pnpla3, africa.ts.2.group.table.data.pnpla3) %>% rbind(africa.ts.3.group.table.data.pnpla3) %>%
                                  data.table::setorder(date) 
africa.group.table.data.pnpla3$group <- c(1:nrow(africa.group.table.data.pnpla3))
ft_raster_group.africa <- flextable::flextable(africa.group.table.data.pnpla3) 
ft_raster_group.africa <- flextable::width(ft_raster_group.africa, j = c(2), width = 4, unit = "cm")
ft_raster_group.africa <- hline(ft_raster_group.africa, i = c(2,5), border = std_border) %>% as_raster()
group.table.africa     <- ggplot() + 
                          theme_void() + 
                          annotation_custom(rasterGrob(ft_raster_group.africa), xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf)
# America
america.group.table.data.pnpla3 <- rbind(america.ts.1.group.table.data.pnpla3, america.ts.2.group.table.data.pnpla3) %>% rbind(america.ts.3.group.table.data.pnpla3) %>%
                                   data.table::setorder(date) 
america.group.table.data.pnpla3$group <- c(1:nrow(america.group.table.data.pnpla3))
ft_raster_group.america <- flextable::flextable(america.group.table.data.pnpla3)
ft_raster_group.america <- flextable::width(ft_raster_group.america, j = c(2), width = 4, unit = "cm")
ft_raster_group.america <- hline(ft_raster_group.america, i = c(11,18), border = std_border) %>% as_raster()
group.table.america     <- ggplot() + 
                           theme_void() + 
                           annotation_custom(rasterGrob(ft_raster_group.america), xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf)
# Asia
asia.group.table.data.pnpla3 <- rbind(asia.ts.1.group.table.data.pnpla3, asia.ts.2.group.table.data.pnpla3) %>% rbind(asia.ts.3.group.table.data.pnpla3) %>%
                                data.table::setorder(date) 
asia.group.table.data.pnpla3$group <- c(1:nrow(asia.group.table.data.pnpla3))
ft_raster_group.asia <- flextable::flextable(asia.group.table.data.pnpla3)
ft_raster_group.asia <- flextable::width(ft_raster_group.asia, j = c(2), width = 4, unit = "cm")
ft_raster_group.asia <- hline(ft_raster_group.asia, i = c(7,13), border = std_border) %>% as_raster()
group.table.asia     <- ggplot() + 
                        theme_void() + 
                        annotation_custom(rasterGrob(ft_raster_group.asia), xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf)
# Europe
europe.group.table.data.pnpla3 <- rbind(europe.ts.1.group.table.data.pnpla3, europe.ts.2.group.table.data.pnpla3) %>% rbind(europe.ts.3.group.table.data.pnpla3) %>%
                                  data.table::setorder(date) 
europe.group.table.data.pnpla3$group <- c(1:nrow(europe.group.table.data.pnpla3))
ft_raster_group.europe <- flextable::flextable(europe.group.table.data.pnpla3)
ft_raster_group.europe <- flextable::width(ft_raster_group.europe, j = c(2), width = 4, unit = "cm")
ft_raster_group.europe <- hline(ft_raster_group.europe, i = c(9,17), border = std_border) %>% as_raster()
group.table.europe     <- ggplot() + 
                          theme_void() + 
                          annotation_custom(rasterGrob(ft_raster_group.europe), xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf)

# =============================================================================
# Linear Regression
# =============================================================================
axis.text.size       <- 5
axis.title.size      <- 5
axis.ticks.size      <- 0.3
plot.title.size      <- 8
plot.subtitle.size   <- 7
grid.linewidth.major <- 0.3
grid.linewidth.minor <- 0.2
border.linewidth     <- 0.5
grid.colour          <- "grey"
lr.linewidth         <- 0.4
ts.linewidth         <- 0.2
label.text.size      <- 2
# Africa
lm.africa.pnpla3 <- ggplot(data = africa.pnpla3, aes(date, pnpla3, label = group)) +
  # vertical lines for time slices
  geom_vline(xintercept = -3000, size = ts.linewidth) +
  geom_vline(xintercept = -7000, size = ts.linewidth) +
  # horizontal line for mean allele frequency in modern populations
  geom_hline(yintercept = sum(africa.modern.pnpla3$pnpla3*africa.modern.pnpla3$individuals)/sum(africa.modern.pnpla3$individuals), linetype = "dashed") +
  # regression line
  geom_abline(intercept = stats::coefficients(stats::lm(formula = africa.pnpla3$pnpla3 ~ africa.pnpla3$date, weights = africa.pnpla3$individuals))[1],
              slope     = stats::coefficients(stats::lm(formula = africa.pnpla3$pnpla3 ~ africa.pnpla3$date, weights = africa.pnpla3$individuals))[2],
              color = "red", size = lr.linewidth) +
  # points
  geom_point(size = africa.pnpla3$scale) +
  geom_label_repel(
    data          = subset(africa.pnpla3,  date > -3000),
    nudge_x       = -1000 - subset(africa.pnpla3, date > -3000)$date,
    segment.size  = 0.2,
    segment.color = "black",
    direction     = "y",
    hjust         = 0,
    size          = label.text.size,
    box.padding   = 0,
    label.padding = 0.1,
    min.segment.length = 0
  ) +
  geom_label_repel(
    data          = subset(africa.pnpla3,  date < -3000 & date > -7000),
    nudge_x       = -5000 - subset(africa.pnpla3, date < -3000 & date > -7000)$date,
    segment.size  = 0.2,
    segment.color = "black",
    direction     = "y",
    hjust         = 0,
    size          = label.text.size,
    box.padding   = 0,
    label.padding = 0.1,
    min.segment.length = 0
  ) +
  geom_label_repel(
    data          = subset(africa.pnpla3,  date < -7000 & date > -15000),
    nudge_x       = -11500 - subset(africa.pnpla3, date < -7000 & date > -15000)$date,
    segment.size  = 0.2,
    segment.color = "black",
    direction     = "y",
    hjust         = 0,
    size          = label.text.size,
    box.padding   = 0,
    label.padding = 0.1,
    min.segment.length = 0
  ) +
  # adjust coordinate system
  labs(title    = "PNPLA3 - Regression Line Africa",
       subtitle = paste("Slope: ", format(round(stats::coefficients(stats::lm(formula = africa.pnpla3$pnpla3 ~ africa.pnpla3$date, weights = africa.pnpla3$individuals))[2]*10000, digits = 4), digits = 4, scientific = FALSE)),
       x = "Date (BP)", y = "Allele Frequency") +
  scale_y_continuous(breaks = c(0.0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0), limits = c(0,1)) +
  scale_x_continuous(limits = c(-15000,0)) +
  theme(axis.text        = element_text(size = axis.text.size),
        axis.title       = element_text(size = axis.title.size),
        axis.ticks       = element_line(linewidth = axis.ticks.size),
        plot.title       = element_text(size = plot.title.size),
        plot.subtitle    = element_text(size = plot.subtitle.size),
        panel.background = element_rect(fill = "white", colour = NA),
        panel.border     = element_rect(fill = NA, colour = "black", linewidth = border.linewidth),
        panel.grid.major = element_line(colour = grid.colour, linewidth = grid.linewidth.major),
        panel.grid.minor = element_line(colour = grid.colour, linewidth = grid.linewidth.minor),
        plot.background  = element_rect(fill = "white", colour = NA))
# America
lm.america.pnpla3 <- ggplot(data = america.pnpla3, aes(date, pnpla3, label = group)) +
  # vertical lines for time slices
  geom_vline(xintercept = -3000, size = ts.linewidth) +
  geom_vline(xintercept = -7000, size = ts.linewidth) +
  # horizontal line for mean allele frequency in modern populations
  geom_hline(yintercept = sum(america.modern.pnpla3$pnpla3*america.modern.pnpla3$individuals)/sum(america.modern.pnpla3$individuals), linetype = "dashed") +
  # regression line
  geom_abline(intercept = stats::coefficients(stats::lm(formula = america.pnpla3$pnpla3 ~ america.pnpla3$date, weights = america.pnpla3$individuals))[1],
              slope     = stats::coefficients(stats::lm(formula = america.pnpla3$pnpla3 ~ america.pnpla3$date, weights = america.pnpla3$individuals))[2],
              color = "red", size = lr.linewidth) +
  # points
  geom_point(size = america.pnpla3$scale) +
  geom_label_repel(
    data          = subset(america.pnpla3,  date > -3000),
    nudge_x       = -2500 - subset(america.pnpla3, date > -3000)$date,
    segment.size  = 0.2,
    segment.color = "black",
    direction     = "y",
    hjust         = 0,
    size          = label.text.size,
    box.padding   = 0,
    label.padding = 0.1,
    min.segment.length = 0
  ) +
  geom_label_repel(
    data          = subset(america.pnpla3,  date < -3000 & date > -7000),
    nudge_x       = -6500 - subset(america.pnpla3, date < -3000 & date > -7000)$date,
    segment.size  = 0.2,
    segment.color = "black",
    direction     = "y",
    hjust         = 0,
    size          = label.text.size,
    box.padding   = 0,
    label.padding = 0.1,
    min.segment.length = 0
  ) +
  geom_label_repel(
    data          = subset(america.pnpla3,  date < -7000 & date > -15000),
    nudge_x       = -12500 - subset(america.pnpla3, date < -7000 & date > -15000)$date,
    segment.size  = 0.2,
    segment.color = "black",
    direction     = "y",
    hjust         = 0,
    size          = label.text.size,
    box.padding   = 0,
    label.padding = 0.1,
    min.segment.length = 0
  ) +
  # adjust coordinate system
  labs(title    = "PNPLA3 - Regression Line America",
       subtitle = paste("Slope: ", format(round(stats::coefficients(stats::lm(formula = america.pnpla3$pnpla3 ~ america.pnpla3$date, weights = america.pnpla3$individuals))[2]*10000, digits = 4), digits = 4, scientific = FALSE)),
       x = "Date (BP)", y = "Allele Frequency") +
  scale_y_continuous(breaks = c(0.0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0), limits = c(0,1)) +
  scale_x_continuous(limits = c(-15000,0)) +
  theme(axis.text        = element_text(size = axis.text.size),
        axis.title       = element_text(size = axis.title.size),
        axis.ticks       = element_line(linewidth = axis.ticks.size),
        plot.title       = element_text(size = plot.title.size),
        plot.subtitle    = element_text(size = plot.subtitle.size),
        panel.background = element_rect(fill = "white", colour = NA),
        panel.border     = element_rect(fill = NA, colour = "black", linewidth = border.linewidth),
        panel.grid.major = element_line(colour = grid.colour, linewidth = grid.linewidth.major),
        panel.grid.minor = element_line(colour = grid.colour, linewidth = grid.linewidth.minor),
        plot.background  = element_rect(fill = "white", colour = NA)) 
# Asia
lm.asia.pnpla3 <- ggplot(data = asia.pnpla3, aes(date, pnpla3, label = group)) +
  # vertical lines for time slices
  geom_vline(xintercept = -3000, size = ts.linewidth) +
  geom_vline(xintercept = -7000, size = ts.linewidth) +
  # horizontal line for mean allele frequency in modern populations
  geom_hline(yintercept = sum(asia.modern.pnpla3$pnpla3*asia.modern.pnpla3$individuals)/sum(asia.modern.pnpla3$individuals), linetype = "dashed") +
  # regression line
  geom_abline(intercept = stats::coefficients(stats::lm(formula = asia.pnpla3$pnpla3 ~ asia.pnpla3$date, weights = asia.pnpla3$individuals))[1],
              slope     = stats::coefficients(stats::lm(formula = asia.pnpla3$pnpla3 ~ asia.pnpla3$date, weights = asia.pnpla3$individuals))[2],
              color = "red", size = lr.linewidth) +
  # points
  geom_point(size = asia.pnpla3$scale) +
  geom_label_repel(
    data          = subset(asia.pnpla3,  date > -3000),
    nudge_x       = -2500 - subset(asia.pnpla3, date > -3000)$date,
    segment.size  = 0.2,
    segment.color = "black",
    direction     = "y",
    hjust         = 0,
    size          = label.text.size,
    box.padding   = 0,
    label.padding = 0.1,
    min.segment.length = 0
  ) +
  geom_label_repel(
    data          = subset(asia.pnpla3,  date < -3000 & date > -7000),
    nudge_x       = -6500 - subset(asia.pnpla3, date < -3000 & date > -7000)$date,
    segment.size  = 0.2,
    segment.color = "black",
    direction     = "y",
    hjust         = 0,
    size          = label.text.size,
    box.padding   = 0,
    label.padding = 0.1,
    min.segment.length = 0
  ) +
  geom_label_repel(
    data          = subset(asia.pnpla3,  date < -7000 & date > -15000),
    nudge_x       = -11000 - subset(asia.pnpla3, date < -7000 & date > -15000)$date,
    segment.size  = 0.2,
    segment.color = "black",
    direction     = "y",
    hjust         = 0,
    size          = label.text.size,
    box.padding   = 0,
    label.padding = 0.1,
    min.segment.length = 0
  ) +
  # adjust coordinate system
  labs(title    = "PNPLA3 - Regression Line Asia",
       subtitle = paste("Slope: ", format(round(stats::coefficients(stats::lm(formula = asia.pnpla3$pnpla3 ~ asia.pnpla3$date, weights = asia.pnpla3$individuals))[2]*10000, digits = 4), digits = 4, scientific = FALSE)),
       x = "Date (BP)", y = "Allele Frequency") +
  scale_y_continuous(breaks = c(0.0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0), limits = c(0,1)) +
  scale_x_continuous(limits = c(-15000,0)) +
  theme(axis.text        = element_text(size = axis.text.size),
        axis.title       = element_text(size = axis.title.size),
        axis.ticks       = element_line(linewidth = axis.ticks.size),
        plot.title       = element_text(size = plot.title.size),
        plot.subtitle    = element_text(size = plot.subtitle.size),
        panel.background = element_rect(fill = "white", colour = NA),
        panel.border     = element_rect(fill = NA, colour = "black", linewidth = border.linewidth),
        panel.grid.major = element_line(colour = grid.colour, linewidth = grid.linewidth.major),
        panel.grid.minor = element_line(colour = grid.colour, linewidth = grid.linewidth.minor),
        plot.background  = element_rect(fill = "white", colour = NA)) 
# Europe
lm.europe.pnpla3 <- ggplot(data = europe.pnpla3, aes(date, pnpla3, label = group)) +
  # vertical lines for time slices
  geom_vline(xintercept = -3000, size = ts.linewidth) +
  geom_vline(xintercept = -7000, size = ts.linewidth) +
  # horizontal line for mean allele frequency in modern populations
  geom_hline(yintercept = sum(europe.modern.pnpla3$pnpla3*europe.modern.pnpla3$individuals)/sum(europe.modern.pnpla3$individuals), linetype = "dashed") +
  # regression line
  geom_abline(intercept = stats::coefficients(stats::lm(formula = europe.pnpla3$pnpla3 ~ europe.pnpla3$date, weights = europe.pnpla3$individuals))[1],
              slope     = stats::coefficients(stats::lm(formula = europe.pnpla3$pnpla3 ~ europe.pnpla3$date, weights = europe.pnpla3$individuals))[2],
              color = "red", size = lr.linewidth) +
  # points
  geom_point(size = europe.pnpla3$scale) +
  geom_label_repel(
    data          = subset(europe.pnpla3,  date > -3000),
    nudge_x       = -2500 - subset(europe.pnpla3, date > -3000)$date,
    segment.size  = 0.2,
    segment.color = "black",
    direction     = "y",
    hjust         = 0,
    size          = label.text.size,
    box.padding   = 0,
    label.padding = 0.1,
    min.segment.length = 0
  ) +
  geom_label_repel(
    data          = subset(europe.pnpla3,  date < -3000 & date > -7000),
    nudge_x       = -6500 - subset(europe.pnpla3, date < -3000 & date > -7000)$date,
    segment.size  = 0.2,
    segment.color = "black",
    direction     = "y",
    hjust         = 0,
    size          = label.text.size,
    box.padding   = 0,
    label.padding = 0.1,
    min.segment.length = 0
  ) +
  geom_label_repel(
    data          = subset(europe.pnpla3,  date < -7000 & date > -15000),
    nudge_x       = -11000 - subset(europe.pnpla3, date < -7000 & date > -15000)$date,
    segment.size  = 0.2,
    segment.color = "black",
    direction     = "y",
    hjust         = 0,
    size          = label.text.size,
    box.padding   = 0,
    label.padding = 0.1,
    min.segment.length = 0
  ) +
  # adjust coordinate system
  labs(title    = "PNPLA3 - Regression Line Europe",
       subtitle = paste("Slope: ", format(round(stats::coefficients(stats::lm(formula = europe.pnpla3$pnpla3 ~ europe.pnpla3$date, weights = europe.pnpla3$individuals))[2]*10000, digits = 4), digits = 4, scientific = FALSE)),
       x = "Date (BP)", y = "Allele Frequency") +
  scale_y_continuous(breaks = c(0.0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0), limits = c(0,1)) +
  scale_x_continuous(limits = c(-15000,0)) +
  theme(axis.text        = element_text(size = axis.text.size),
        axis.title       = element_text(size = axis.title.size),
        axis.ticks       = element_line(linewidth = axis.ticks.size),
        plot.title       = element_text(size = plot.title.size),
        plot.subtitle    = element_text(size = plot.subtitle.size),
        panel.background = element_rect(fill = "white", colour = NA),
        panel.border     = element_rect(fill = NA, colour = "black", linewidth = border.linewidth),
        panel.grid.major = element_line(colour = grid.colour, linewidth = grid.linewidth.major),
        panel.grid.minor = element_line(colour = grid.colour, linewidth = grid.linewidth.minor),
        plot.background  = element_rect(fill = "white", colour = NA))

# =============================================================================
# World - PNPLA3 - linear Regression + Table
# =============================================================================
# Africa
plot.pnpla3.africa  <- cowplot::plot_grid(lm.africa.pnpla3,  group.table.africa,  nrow = 2, ncol = 1, rel_heights = c(1:4), scale = c(1,0.8))
# America
plot.pnpla3.america <- cowplot::plot_grid(lm.america.pnpla3, group.table.america, nrow = 2, ncol = 1, rel_heights = c(1:4))
# Asia
plot.pnpla3.asia    <- cowplot::plot_grid(lm.asia.pnpla3,    group.table.asia,    nrow = 2, ncol = 1, rel_heights = c(1:4), scale = c(1,0.8))
# Europe
plot.pnpla3.europe  <- cowplot::plot_grid(lm.europe.pnpla3,  group.table.europe,  nrow = 2, ncol = 1, rel_heights = c(1:4))
# World
pnpla3.plot.1 <- cowplot::plot_grid(plot.pnpla3.africa, plot.pnpla3.america, nrow = 1, ncol = 2, scale = c(1:1), labels = "AUTO") +
                 theme(plot.background = element_rect(fill = "white", colour = NA))
pnpla3.plot.2 <- cowplot::plot_grid(plot.pnpla3.asia, plot.pnpla3.europe, nrow = 1, ncol = 2, scale = c(1:1), labels = c("C", "D")) +
                 theme(plot.background = element_rect(fill = "white", colour = NA))
ggsave(filename = "LinearRegression_part1.pdf", plot = pnpla3.plot.1, device = "pdf", path = "5. Plots", width = 18, height = 26, units = "cm")
ggsave(filename = "LinearRegression_part1.png", plot = pnpla3.plot.1, device = "png", path = "5. Plots", width = 18, height = 26, units = "cm")
ggsave(filename = "LinearRegression_part2.pdf", plot = pnpla3.plot.2, device = "pdf", path = "5. Plots", width = 18, height = 26, units = "cm")
ggsave(filename = "LinearRegression_part2.png", plot = pnpla3.plot.2, device = "png", path = "5. Plots", width = 18, height = 26, units = "cm")

