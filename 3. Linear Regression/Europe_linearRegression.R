library(tidyverse)
library(flextable)
library(ggplot2)
library(ggpubr)
library(ggpmisc)
library(grid)
library(ggrepel)

# =============================================================================
# Input Data
# =============================================================================
# pnpla3
europe.modern.pnpla3 <- tibble::as_tibble(read.csv(file = "1. Modern Populations/1. Data/europe_modern_pnpla3.csv")) %>% dplyr::select(-1)
europe.ts.1.pnpla3   <- tibble::as_tibble(read.csv(file = "2. Time Slices/1. Data/europe_timeSlice1_pnpla3.csv")) %>% dplyr::select(-1)
europe.ts.2.pnpla3   <- tibble::as_tibble(read.csv(file = "2. Time Slices/1. Data/europe_timeSlice2_pnpla3.csv")) %>% dplyr::select(-1)
europe.ts.3.pnpla3   <- tibble::as_tibble(read.csv(file = "2. Time Slices/1. Data/europe_timeSlice3_pnpla3.csv")) %>% dplyr::select(-1)

# modern population data points
europe.modern.pnpla3$subregion <- as.character(europe.modern.pnpla3$subregion)
modern.dp.pnpla3 <- data.table::data.table(group       = 0,
                                           subregion   = "Europe",
                                           individuals = sum(europe.modern.pnpla3$individuals),
                                           mean.ycoord = mean(europe.modern.pnpla3$mean.ycoord),
                                           mean.xcoord = mean(europe.modern.pnpla3$mean.xcoord),
                                           date        = 0,
                                           pnpla3      = sum(europe.modern.pnpla3$pnpla3*europe.modern.pnpla3$individuals)/sum(europe.modern.pnpla3$individuals),
                                           reference   = 1 - sum(europe.modern.pnpla3$pnpla3*europe.modern.pnpla3$individuals)/sum(europe.modern.pnpla3$individuals),
                                           scale       = 5)
# merge data for genes
europe.pnpla3 <- rbind(modern.dp.pnpla3, europe.ts.1.pnpla3) %>% rbind(europe.ts.2.pnpla3) %>% rbind(europe.ts.3.pnpla3)
europe.pnpla3$date  <- europe.pnpla3$date * (-1)
europe.pnpla3$group <- c(0,4,2,6,3,9,1,7,8,5,17,14,18,16,10,11,13,12,15,22,23,20,19,25,26,24,21)
europe.pnpla3$scale <- europe.pnpla3$scale / 5

# =============================================================================
# Table Data
# =============================================================================
# individuals
europe.ts.1.ind.table.data.pnpla3 <- tibble::as_tibble(read.csv(file = "2. Time Slices/1. Data/europe_timeSlice1_ind_table_data_pnpla3.csv")) %>% dplyr::select(-1) %>%
                                   data.table::setorder(group)
europe.ts.2.ind.table.data.pnpla3 <- tibble::as_tibble(read.csv(file = "2. Time Slices/1. Data/europe_timeSlice2_ind_table_data_pnpla3.csv")) %>% dplyr::select(-1) %>%
                                   data.table::setorder(group)
europe.ts.3.ind.table.data.pnpla3 <- tibble::as_tibble(read.csv(file = "2. Time Slices/1. Data/europe_timeSlice3_ind_table_data_pnpla3.csv")) %>% dplyr::select(-1) %>%
                                   data.table::setorder(group)
# groups
europe.ts.1.group.table.data.pnpla3 <- tibble::as_tibble(read.csv(file = "2. Time Slices/1. Data/europe_timeSlice1_group_table_data_pnpla3.csv")) %>% dplyr::select(-1)
europe.ts.2.group.table.data.pnpla3 <- tibble::as_tibble(read.csv(file = "2. Time Slices/1. Data/europe_timeSlice2_group_table_data_pnpla3.csv")) %>% dplyr::select(-1)
europe.ts.3.group.table.data.pnpla3 <- tibble::as_tibble(read.csv(file = "2. Time Slices/1. Data/europe_timeSlice3_group_table_data_pnpla3.csv")) %>% dplyr::select(-1)

# =============================================================================
# Individuals Table
# =============================================================================
# time slice 1
europe.ts.1.ind.table.data.pnpla3 <- dplyr::mutate(europe.ts.1.ind.table.data.pnpla3, group =ifelse(group == 1, 4, group))
europe.ts.1.ind.table.data.pnpla3 <- dplyr::mutate(europe.ts.1.ind.table.data.pnpla3, group =ifelse(group == 3, 6, group))
europe.ts.1.ind.table.data.pnpla3 <- dplyr::mutate(europe.ts.1.ind.table.data.pnpla3, group =ifelse(group == 4, 3, group))
europe.ts.1.ind.table.data.pnpla3 <- dplyr::mutate(europe.ts.1.ind.table.data.pnpla3, group =ifelse(group == 5, 9, group))
europe.ts.1.ind.table.data.pnpla3 <- dplyr::mutate(europe.ts.1.ind.table.data.pnpla3, group =ifelse(group == 6, 1, group))
europe.ts.1.ind.table.data.pnpla3 <- dplyr::mutate(europe.ts.1.ind.table.data.pnpla3, group =ifelse(group == 9, 5, group))
# time slice 2
europe.ts.2.ind.table.data.pnpla3 <- dplyr::mutate(europe.ts.2.ind.table.data.pnpla3, group =ifelse(group == 1, 17, group))
europe.ts.2.ind.table.data.pnpla3 <- dplyr::mutate(europe.ts.2.ind.table.data.pnpla3, group =ifelse(group == 2, 14, group))
europe.ts.2.ind.table.data.pnpla3 <- dplyr::mutate(europe.ts.2.ind.table.data.pnpla3, group =ifelse(group == 3, 18, group))
europe.ts.2.ind.table.data.pnpla3 <- dplyr::mutate(europe.ts.2.ind.table.data.pnpla3, group =ifelse(group == 4, 16, group))
europe.ts.2.ind.table.data.pnpla3 <- dplyr::mutate(europe.ts.2.ind.table.data.pnpla3, group =ifelse(group == 5, 10, group))
europe.ts.2.ind.table.data.pnpla3 <- dplyr::mutate(europe.ts.2.ind.table.data.pnpla3, group =ifelse(group == 6, 11, group))
europe.ts.2.ind.table.data.pnpla3 <- dplyr::mutate(europe.ts.2.ind.table.data.pnpla3, group =ifelse(group == 7, 13, group))
europe.ts.2.ind.table.data.pnpla3 <- dplyr::mutate(europe.ts.2.ind.table.data.pnpla3, group =ifelse(group == 8, 12, group))
europe.ts.2.ind.table.data.pnpla3 <- dplyr::mutate(europe.ts.2.ind.table.data.pnpla3, group =ifelse(group == 9, 15, group))
# time slice 3
europe.ts.3.ind.table.data.pnpla3 <- dplyr::mutate(europe.ts.3.ind.table.data.pnpla3, group =ifelse(group == 1, 22, group))
europe.ts.3.ind.table.data.pnpla3 <- dplyr::mutate(europe.ts.3.ind.table.data.pnpla3, group =ifelse(group == 2, 23, group))
europe.ts.3.ind.table.data.pnpla3 <- dplyr::mutate(europe.ts.3.ind.table.data.pnpla3, group =ifelse(group == 3, 20, group))
europe.ts.3.ind.table.data.pnpla3 <- dplyr::mutate(europe.ts.3.ind.table.data.pnpla3, group =ifelse(group == 4, 19, group))
europe.ts.3.ind.table.data.pnpla3 <- dplyr::mutate(europe.ts.3.ind.table.data.pnpla3, group =ifelse(group == 6, 25, group))
europe.ts.3.ind.table.data.pnpla3 <- dplyr::mutate(europe.ts.3.ind.table.data.pnpla3, group =ifelse(group == 7, 26, group))
europe.ts.3.ind.table.data.pnpla3 <- dplyr::mutate(europe.ts.3.ind.table.data.pnpla3, group =ifelse(group == 8, 24, group))
europe.ts.3.ind.table.data.pnpla3 <- dplyr::mutate(europe.ts.3.ind.table.data.pnpla3, group =ifelse(group == 9, 21, group))
# merge data table
europe.ind.table.data.pnpla3 <- rbind(europe.ts.1.ind.table.data.pnpla3, europe.ts.2.ind.table.data.pnpla3) %>% rbind(europe.ts.3.ind.table.data.pnpla3) %>%
                                data.table::setorder(group)
# create individual table
ft_raster_ind <- europe.ind.table.data.pnpla3 %>% flextable::flextable() %>% as_raster()
ind.table     <- ggplot() + 
                  theme_void() + 
                  annotation_custom(rasterGrob(ft_raster_ind), xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf)

# =============================================================================
# Group Table
# =============================================================================
# merge table data
europe.group.table.data.pnpla3 <- rbind(europe.ts.1.group.table.data.pnpla3, europe.ts.2.group.table.data.pnpla3) %>% rbind(europe.ts.3.group.table.data.pnpla3) %>%
                                  data.table::setorder(date) 
europe.group.table.data.pnpla3$group <- c(1:nrow(europe.group.table.data.pnpla3))
# create group table
std_border = officer::fp_border(color="black", width = 1)
ft_raster_group <- flextable::flextable(europe.group.table.data.pnpla3) 
ft_raster_group <- hline(ft_raster_group, i = c(9,18), border = std_border)
ft_raster_group <- as_raster(ft_raster_group)
group.table     <- ggplot() + 
                    theme_void() + 
                    annotation_custom(rasterGrob(ft_raster_group), xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf)
# linear regression
lm.europe.pnpla3 <- ggplot(data = europe.pnpla3, aes(date, pnpla3, label = group)) +
                    # vertical lines for time slices
                    geom_vline(xintercept = -3000) +
                    geom_vline(xintercept = -7000) +
                    # horizontal line for mean allele frequency in modern populations
                    geom_hline(yintercept = sum(europe.modern.pnpla3$pnpla3*europe.modern.pnpla3$individuals)/sum(europe.modern.pnpla3$individuals), linetype = "dashed") +
                    # regression line
                    geom_abline(intercept = stats::coefficients(stats::lm(formula = europe.pnpla3$pnpla3 ~ europe.pnpla3$date, weights = europe.pnpla3$individuals))[1],
                                slope     = stats::coefficients(stats::lm(formula = europe.pnpla3$pnpla3 ~ europe.pnpla3$date, weights = europe.pnpla3$individuals))[2],
                                color = "red", size = 0.75) +
                    # points
                    geom_point(size = europe.pnpla3$scale) +
                    geom_label_repel(
                      data          = subset(europe.pnpla3,  date > -3000),
                      nudge_x       = -1000 - subset(europe.pnpla3, date > -3000)$date,
                      segment.size  = 0.2,
                      segment.color = "black",
                      direction     = "y",
                      hjust         = 0,
                      size          = 1.5,
                      box.padding   = 0,
                      label.padding = 0.1,
                      min.segment.length = 0
                    ) +
                    geom_label_repel(
                      data          = subset(europe.pnpla3,  date < -3000 & date > -7000),
                      nudge_x       = -6000 - subset(europe.pnpla3, date < -3000 & date > -7000)$date,
                      segment.size  = 0.2,
                      segment.color = "black",
                      direction     = "y",
                      hjust         = 0,
                      size          = 1.5,
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
                      size          = 1.5,
                      box.padding   = 0,
                      label.padding = 0.1,
                      min.segment.length = 0
                    ) +
                    # adjust coordinate system
                    labs(title    = "D \nPNPLA3 - Regression Line Europe",
                         subtitle = paste("Slope (corrected): ", format(round(stats::coefficients(stats::lm(formula = europe.pnpla3$pnpla3 ~ europe.pnpla3$date, weights = europe.pnpla3$individuals))[2]*10000, digits = 4), digits = 4, scientific = FALSE)),
                         x = "Date (BP)", y = "Allele Frequency") +
                    scale_y_continuous(breaks = c(0.0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0), limits = c(0,1)) +
                    scale_x_continuous(limits = c(-15000,0)) +
                    theme_bw()

# =============================================================================
# Europe - PNPLA3 - linear Regression + Table
# =============================================================================
plot.pnpla3 <- cowplot::plot_grid(lm.europe.pnpla3, group.table, nrow = 2, ncol = 1)
ggsave(filename = "LinearRegression_Europe.pdf", plot = plot.pnpla3, device = "pdf", path = "5. Plots", width = 18, height = 13, units = "cm")
ggsave(filename = "LinearRegression_Europe.png", plot = plot.pnpla3, device = "png", path = "5. Plots", width = 18, height = 13, units = "cm")
