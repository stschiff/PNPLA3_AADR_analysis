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
asia.modern.pnpla3 <- tibble::as_tibble(read.csv(file = "1. Modern Populations/1. Data/asia_modern_pnpla3.csv")) %>% dplyr::select(-1)
asia.ts.1.pnpla3   <- tibble::as_tibble(read.csv(file = "2. Time Slices/1. Data/asia_timeSlice1_pnpla3.csv")) %>% dplyr::select(-1)
asia.ts.2.pnpla3   <- tibble::as_tibble(read.csv(file = "2. Time Slices/1. Data/asia_timeSlice2_pnpla3.csv")) %>% dplyr::select(-1)
asia.ts.3.pnpla3   <- tibble::as_tibble(read.csv(file = "2. Time Slices/1. Data/asia_timeSlice3_pnpla3.csv")) %>% dplyr::select(-1)

# modern population data points
asia.modern.pnpla3$subregion <- as.character(asia.modern.pnpla3$subregion)
modern.dp.pnpla3 <- data.table::data.table(group       = 0,
                                           subregion   = "Asia",
                                           individuals = sum(asia.modern.pnpla3$individuals),
                                           mean.ycoord = mean(asia.modern.pnpla3$mean.ycoord),
                                           mean.xcoord = mean(asia.modern.pnpla3$mean.xcoord),
                                           date        = 0,
                                           pnpla3      = sum(asia.modern.pnpla3$pnpla3*asia.modern.pnpla3$individuals)/sum(asia.modern.pnpla3$individuals),
                                           reference   = 1 - sum(asia.modern.pnpla3$pnpla3*asia.modern.pnpla3$individuals)/sum(asia.modern.pnpla3$individuals),
                                           scale       = 5)
# merge data for genes
asia.pnpla3 <- rbind(modern.dp.pnpla3, asia.ts.1.pnpla3) %>% rbind(asia.ts.2.pnpla3) %>% rbind(asia.ts.3.pnpla3)
asia.pnpla3$date  <- asia.pnpla3$date * (-1)
asia.pnpla3$group <- c(0,3,1,4,6,7,2,5,10,9,14,8,11,13,12,15,16,18,17)
asia.pnpla3$scale <- asia.pnpla3$scale / 5

# =============================================================================
# Table Data
# =============================================================================
# individuals
asia.ts.1.ind.table.data.pnpla3 <- tibble::as_tibble(read.csv(file = "2. Time Slices/Data/asia_timeSlice1_ind_table_data_pnpla3.csv")) %>% dplyr::select(-1) %>%
                                     data.table::setorder(group)
asia.ts.2.ind.table.data.pnpla3 <- tibble::as_tibble(read.csv(file = "2. Time Slices/Data/asia_timeSlice2_ind_table_data_pnpla3.csv")) %>% dplyr::select(-1) %>%
                                     data.table::setorder(group)
asia.ts.3.ind.table.data.pnpla3 <- tibble::as_tibble(read.csv(file = "2. Time Slices/Data/asia_timeSlice3_ind_table_data_pnpla3.csv")) %>% dplyr::select(-1) %>%
                                     data.table::setorder(group)
# groups
asia.ts.1.group.table.data.pnpla3 <- tibble::as_tibble(read.csv(file = "2. Time Slices/1. Data/asia_timeSlice1_group_table_data_pnpla3.csv")) %>% dplyr::select(-1)
asia.ts.2.group.table.data.pnpla3 <- tibble::as_tibble(read.csv(file = "2. Time Slices/1. Data/asia_timeSlice2_group_table_data_pnpla3.csv")) %>% dplyr::select(-1)
asia.ts.3.group.table.data.pnpla3 <- tibble::as_tibble(read.csv(file = "2. Time Slices/1. Data/asia_timeSlice3_group_table_data_pnpla3.csv")) %>% dplyr::select(-1)

# =============================================================================
# Individuals Table
# =============================================================================
# time slice 1
asia.ts.1.ind.table.data.pnpla3 <- dplyr::mutate(asia.ts.1.ind.table.data.pnpla3, group =ifelse(group == 1, 3, group))
asia.ts.1.ind.table.data.pnpla3 <- dplyr::mutate(asia.ts.1.ind.table.data.pnpla3, group =ifelse(group == 2, 1, group))
asia.ts.1.ind.table.data.pnpla3 <- dplyr::mutate(asia.ts.1.ind.table.data.pnpla3, group =ifelse(group == 5, 6, group))
asia.ts.1.ind.table.data.pnpla3 <- dplyr::mutate(asia.ts.1.ind.table.data.pnpla3, group =ifelse(group == 6, 7, group))
asia.ts.1.ind.table.data.pnpla3 <- dplyr::mutate(asia.ts.1.ind.table.data.pnpla3, group =ifelse(group == 8, 2, group))
asia.ts.1.ind.table.data.pnpla3 <- dplyr::mutate(asia.ts.1.ind.table.data.pnpla3, group =ifelse(group == 9, 5, group))
# time slice 2
asia.ts.2.ind.table.data.pnpla3 <- dplyr::mutate(asia.ts.2.ind.table.data.pnpla3, group =ifelse(group == 1, 10, group))
asia.ts.2.ind.table.data.pnpla3 <- dplyr::mutate(asia.ts.2.ind.table.data.pnpla3, group =ifelse(group == 2, 9, group))
asia.ts.2.ind.table.data.pnpla3 <- dplyr::mutate(asia.ts.2.ind.table.data.pnpla3, group =ifelse(group == 4, 14, group))
asia.ts.2.ind.table.data.pnpla3 <- dplyr::mutate(asia.ts.2.ind.table.data.pnpla3, group =ifelse(group == 5, 8, group))
asia.ts.2.ind.table.data.pnpla3 <- dplyr::mutate(asia.ts.2.ind.table.data.pnpla3, group =ifelse(group == 6, 11, group))
asia.ts.2.ind.table.data.pnpla3 <- dplyr::mutate(asia.ts.2.ind.table.data.pnpla3, group =ifelse(group == 8, 13, group))
asia.ts.2.ind.table.data.pnpla3 <- dplyr::mutate(asia.ts.2.ind.table.data.pnpla3, group =ifelse(group == 9, 12, group))
# time slice 3
asia.ts.3.ind.table.data.pnpla3 <- dplyr::mutate(asia.ts.3.ind.table.data.pnpla3, group =ifelse(group == 2, 15, group))
asia.ts.3.ind.table.data.pnpla3 <- dplyr::mutate(asia.ts.3.ind.table.data.pnpla3, group =ifelse(group == 4, 16, group))
asia.ts.3.ind.table.data.pnpla3 <- dplyr::mutate(asia.ts.3.ind.table.data.pnpla3, group =ifelse(group == 6, 18, group))
asia.ts.3.ind.table.data.pnpla3 <- dplyr::mutate(asia.ts.3.ind.table.data.pnpla3, group =ifelse(group == 9, 17, group))
# merge data table
asia.ind.table.data.pnpla3 <- rbind(asia.ts.1.ind.table.data.pnpla3, asia.ts.2.ind.table.data.pnpla3) %>% rbind(asia.ts.3.ind.table.data.pnpla3) %>%
                                data.table::setorder(group)
# create individual table
ft_raster_ind <- asia.ind.table.data.pnpla3 %>% flextable::flextable() %>% as_raster()
ind.table     <- ggplot() + 
                  theme_void() + 
                  annotation_custom(rasterGrob(ft_raster_ind), xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf)

# =============================================================================
# Group Table
# =============================================================================
# merge table data
asia.group.table.data.pnpla3 <- rbind(asia.ts.1.group.table.data.pnpla3, asia.ts.2.group.table.data.pnpla3) %>% rbind(asia.ts.3.group.table.data.pnpla3) %>%
                                data.table::setorder(date) 
asia.group.table.data.pnpla3$group <- c(1:nrow(asia.group.table.data.pnpla3))
# create group table
std_border = officer::fp_border(color="black", width = 1)
ft_raster_group.1 <- flextable::flextable(asia.group.table.data.pnpla3[1:9,]) 
ft_raster_group.1 <- hline(ft_raster_group.1, i = 7, border = std_border)
ft_raster_group.2 <- flextable::flextable(asia.group.table.data.pnpla3[10:18,]) 
ft_raster_group.2 <- hline(ft_raster_group.2, i = 5, border = std_border)
ft_raster_group.1 <- as_raster(ft_raster_group.1)
ft_raster_group.2 <- as_raster(ft_raster_group.2)
group.table.1 <- ggplot() + 
                  theme_void() + 
                  annotation_custom(rasterGrob(ft_raster_group.1), xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf) 
group.table.2 <- ggplot() + 
                  theme_void() + 
                  annotation_custom(rasterGrob(ft_raster_group.2), xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf) 
# linear regression
lm.asia.pnpla3 <- ggplot(data = asia.pnpla3, aes(date, pnpla3, label = group)) +
                      # vertical lines for time slices
                      geom_vline(xintercept = -3000) +
                      geom_vline(xintercept = -7000) +
                      # horizontal line for mean allele frequency in modern populations
                      geom_hline(yintercept = sum(asia.modern.pnpla3$pnpla3*asia.modern.pnpla3$individuals)/sum(asia.modern.pnpla3$individuals), linetype = "dashed") +
                      # regression line
                      geom_abline(intercept = stats::coefficients(stats::lm(formula = asia.pnpla3$pnpla3 ~ asia.pnpla3$date, weights = asia.pnpla3$individuals))[1],
                                  slope     = stats::coefficients(stats::lm(formula = asia.pnpla3$pnpla3 ~ asia.pnpla3$date, weights = asia.pnpla3$individuals))[2],
                                  color = "red", size = 0.75) +
                      # points
                      geom_point(size = asia.pnpla3$scale) +
                      geom_label_repel(
                        data          = subset(asia.pnpla3,  date > -3000),
                        nudge_x       = -1000 - subset(asia.pnpla3, date > -3000)$date,
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
                        data          = subset(asia.pnpla3,  date < -3000 & date > -7000),
                        nudge_x       = -5000 - subset(asia.pnpla3, date < -3000 & date > -7000)$date,
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
                        data          = subset(asia.pnpla3,  date < -7000 & date > -15000),
                        nudge_x       = -10000 - subset(asia.pnpla3, date < -7000 & date > -15000)$date,
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
                      labs(title    = "C \nPNPLA3 - Regression Line Asia",
                           subtitle = paste("Slope (corrected): ", format(round(stats::coefficients(stats::lm(formula = asia.pnpla3$pnpla3 ~ asia.pnpla3$date, weights = asia.pnpla3$individuals))[2]*10000, digits = 4), digits = 4, scientific = FALSE)),
                           x = "Date (BP)", y = "Allele Frequency") +
                      scale_y_continuous(breaks = c(0.0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0), limits = c(0,1)) +
                      scale_x_continuous(limits = c(-15000,0)) +
                      theme_bw()

# =============================================================================
# Asia - PNPLA3 - linear Regression + Table
# =============================================================================
group.table <- cowplot::plot_grid(group.table.1, group.table.2, nrow = 1, ncol = 2, scale = c(1,1))
plot.pnpla3 <- cowplot::plot_grid(lm.america.pnpla3, group.table, nrow = 2, ncol = 1)
ggsave(filename = "LinearRegression_Asia.pdf", plot = plot.pnpla3, device = "pdf", path = "5. Plots", width = 18, height = 13, units = "cm")
ggsave(filename = "LinearRegression_Asia.png", plot = plot.pnpla3, device = "png", path = "5. Plots", width = 18, height = 13, units = "cm")
