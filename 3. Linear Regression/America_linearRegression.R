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
america.modern.pnpla3 <- tibble::as_tibble(read.csv(file = "1. Modern Populations/1. Data/america_modern_pnpla3.csv")) %>% dplyr::select(-1)
america.ts.1.pnpla3   <- tibble::as_tibble(read.csv(file = "2. Time Slices/1. Data/america_timeSlice1_pnpla3.csv")) %>% dplyr::select(-1)
america.ts.2.pnpla3   <- tibble::as_tibble(read.csv(file = "2. Time Slices/1. Data/america_timeSlice2_pnpla3.csv")) %>% dplyr::select(-1)
america.ts.3.pnpla3   <- tibble::as_tibble(read.csv(file = "2. Time Slices/1. Data/america_timeSlice3_pnpla3.csv")) %>% dplyr::select(-1)

# modern population data points
america.modern.pnpla3$subregion <- as.character(america.modern.pnpla3$subregion)
modern.dp.pnpla3 <- data.table::data.table(group       = 0,
                                           subregion   = "America",
                                           individuals = sum(america.modern.pnpla3$individuals),
                                           mean.ycoord = mean(america.modern.pnpla3$mean.ycoord),
                                           mean.xcoord = mean(america.modern.pnpla3$mean.xcoord),
                                           date        = 0,
                                           pnpla3      = sum(america.modern.pnpla3$pnpla3*america.modern.pnpla3$individuals)/sum(america.modern.pnpla3$individuals),
                                           reference   = 1 - sum(america.modern.pnpla3$pnpla3*america.modern.pnpla3$individuals)/sum(america.modern.pnpla3$individuals),
                                           scale       = 5)
# merge data for genes
america.pnpla3 <- rbind(modern.dp.pnpla3, america.ts.1.pnpla3) %>% rbind(america.ts.2.pnpla3) %>% rbind(america.ts.3.pnpla3)
america.pnpla3$date  <- america.pnpla3$date * (-1)
america.pnpla3$group <- c(0,2,4,5,8,3,7,1,6,9,19,10,20,16,15,11,17,12,14,13,18,23,21,22,24)
america.pnpla3$scale <- america.pnpla3$scale / 5

# =============================================================================
# Table Data
# =============================================================================
# individuals
america.ts.1.ind.table.data.pnpla3 <- tibble::as_tibble(read.csv(file = "2. Time Slices/1. Data/america_timeSlice1_ind_table_data_pnpla3.csv")) %>% dplyr::select(-1) %>%
                                     data.table::setorder(group)
america.ts.2.ind.table.data.pnpla3 <- tibble::as_tibble(read.csv(file = "2. Time Slices/1. Data/america_timeSlice2_ind_table_data_pnpla3.csv")) %>% dplyr::select(-1) %>%
                                     data.table::setorder(group)
america.ts.3.ind.table.data.pnpla3 <- tibble::as_tibble(read.csv(file = "2. Time Slices/1. Data/america_timeSlice3_ind_table_data_pnpla3.csv")) %>% dplyr::select(-1) %>%
                                     data.table::setorder(group)
# groups
america.ts.1.group.table.data.pnpla3 <- tibble::as_tibble(read.csv(file = "2. Time Slices/1. Data/america_timeSlice1_group_table_data_pnpla3.csv")) %>% dplyr::select(-1)
america.ts.2.group.table.data.pnpla3 <- tibble::as_tibble(read.csv(file = "2. Time Slices/1. Data/america_timeSlice2_group_table_data_pnpla3.csv")) %>% dplyr::select(-1)
america.ts.3.group.table.data.pnpla3 <- tibble::as_tibble(read.csv(file = "2. Time Slices/1. Data/america_timeSlice3_group_table_data_pnpla3.csv")) %>% dplyr::select(-1)

# =============================================================================
# Individuals Table
# =============================================================================
# time slice 1
america.ts.1.ind.table.data.pnpla3 <- dplyr::mutate(america.ts.1.ind.table.data.pnpla3, group =ifelse(group == 1, 2, group))
america.ts.1.ind.table.data.pnpla3 <- dplyr::mutate(america.ts.1.ind.table.data.pnpla3, group =ifelse(group == 6, 8, group))
america.ts.1.ind.table.data.pnpla3 <- dplyr::mutate(america.ts.1.ind.table.data.pnpla3, group =ifelse(group == 7, 3, group))
america.ts.1.ind.table.data.pnpla3 <- dplyr::mutate(america.ts.1.ind.table.data.pnpla3, group =ifelse(group == 8, 7, group))
america.ts.1.ind.table.data.pnpla3 <- dplyr::mutate(america.ts.1.ind.table.data.pnpla3, group =ifelse(group == 9, 1, group))
america.ts.1.ind.table.data.pnpla3 <- dplyr::mutate(america.ts.1.ind.table.data.pnpla3, group =ifelse(group == 10, 6, group))
america.ts.1.ind.table.data.pnpla3 <- dplyr::mutate(america.ts.1.ind.table.data.pnpla3, group =ifelse(group == 11, 9, group))
# time slice 2
america.ts.2.ind.table.data.pnpla3 <- dplyr::mutate(america.ts.2.ind.table.data.pnpla3, group =ifelse(group == 1, 19, group))
america.ts.2.ind.table.data.pnpla3 <- dplyr::mutate(america.ts.2.ind.table.data.pnpla3, group =ifelse(group == 2, 10, group))
america.ts.2.ind.table.data.pnpla3 <- dplyr::mutate(america.ts.2.ind.table.data.pnpla3, group =ifelse(group == 3, 20, group))
america.ts.2.ind.table.data.pnpla3 <- dplyr::mutate(america.ts.2.ind.table.data.pnpla3, group =ifelse(group == 4, 16, group))
america.ts.2.ind.table.data.pnpla3 <- dplyr::mutate(america.ts.2.ind.table.data.pnpla3, group =ifelse(group == 5, 15, group))
america.ts.2.ind.table.data.pnpla3 <- dplyr::mutate(america.ts.2.ind.table.data.pnpla3, group =ifelse(group == 6, 11, group))
america.ts.2.ind.table.data.pnpla3 <- dplyr::mutate(america.ts.2.ind.table.data.pnpla3, group =ifelse(group == 7, 17, group))
america.ts.2.ind.table.data.pnpla3 <- dplyr::mutate(america.ts.2.ind.table.data.pnpla3, group =ifelse(group == 8, 12, group))
america.ts.2.ind.table.data.pnpla3 <- dplyr::mutate(america.ts.2.ind.table.data.pnpla3, group =ifelse(group == 9, 14, group))
america.ts.2.ind.table.data.pnpla3 <- dplyr::mutate(america.ts.2.ind.table.data.pnpla3, group =ifelse(group == 10, 13, group))
america.ts.2.ind.table.data.pnpla3 <- dplyr::mutate(america.ts.2.ind.table.data.pnpla3, group =ifelse(group == 11, 18, group))
# time slice 3
america.ts.3.ind.table.data.pnpla3 <- dplyr::mutate(america.ts.3.ind.table.data.pnpla3, group =ifelse(group == 1, 23, group))
america.ts.3.ind.table.data.pnpla3 <- dplyr::mutate(america.ts.3.ind.table.data.pnpla3, group =ifelse(group == 4, 21, group))
america.ts.3.ind.table.data.pnpla3 <- dplyr::mutate(america.ts.3.ind.table.data.pnpla3, group =ifelse(group == 5, 22, group))
america.ts.3.ind.table.data.pnpla3 <- dplyr::mutate(america.ts.3.ind.table.data.pnpla3, group =ifelse(group == 7, 24, group))
# merge data table
america.ind.table.data.pnpla3 <- rbind(america.ts.1.ind.table.data.pnpla3, america.ts.2.ind.table.data.pnpla3) %>% rbind(america.ts.3.ind.table.data.pnpla3) %>%
                                 data.table::setorder(group)
# create individual table
ft_raster_ind <- america.ind.table.data.pnpla3 %>% flextable::flextable() %>% as_raster()
ind.table     <- ggplot() + 
                  theme_void() + 
                  annotation_custom(rasterGrob(ft_raster_ind), xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf)

# =============================================================================
# Group Table
# =============================================================================
# merge table data
america.group.table.data.pnpla3 <- rbind(america.ts.1.group.table.data.pnpla3, america.ts.2.group.table.data.pnpla3) %>% rbind(america.ts.3.group.table.data.pnpla3) %>%
                                   data.table::setorder(date) 
america.group.table.data.pnpla3$group <- c(1:nrow(america.group.table.data.pnpla3))
# create group table
std_border = officer::fp_border(color="black", width = 1)
ft_raster_group.1 <- flextable::flextable(america.group.table.data.pnpla3[1:9,]) 
ft_raster_group.2 <- flextable::flextable(america.group.table.data.pnpla3[10:20,]) 
ft_raster_group.3 <- flextable::flextable(america.group.table.data.pnpla3[21:24,])
ft_raster_group.1 <- as_raster(ft_raster_group.1)
ft_raster_group.2 <- as_raster(ft_raster_group.2)
ft_raster_group.3 <- as_raster(ft_raster_group.3)
group.table.1 <- ggplot() + 
                  theme_void() + 
                  annotation_custom(rasterGrob(ft_raster_group.1), xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf) 
group.table.2 <- ggplot() + 
                  theme_void() + 
                  annotation_custom(rasterGrob(ft_raster_group.2), xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf) 
group.table.3 <- ggplot() + 
                  theme_void() + 
                  annotation_custom(rasterGrob(ft_raster_group.3), xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf)
                    
# linear regression
lm.america.pnpla3 <- ggplot(data = america.pnpla3, aes(date, pnpla3, label = group)) +
                      # vertical lines for time slices
                      geom_vline(xintercept = -3000) +
                      geom_vline(xintercept = -7000) +
                      # horizontal line for mean allele frequency in modern populations
                      geom_hline(yintercept = sum(america.modern.pnpla3$pnpla3*america.modern.pnpla3$individuals)/sum(america.modern.pnpla3$individuals), linetype = "dashed") +
                      # regression line
                      geom_abline(intercept = stats::coefficients(stats::lm(formula = america.pnpla3$pnpla3 ~ america.pnpla3$date, weights = america.pnpla3$individuals))[1],
                                  slope     = stats::coefficients(stats::lm(formula = america.pnpla3$pnpla3 ~ america.pnpla3$date, weights = america.pnpla3$individuals))[2],
                                  color = "red", size = 0.75) +
                      # points
                      geom_point(size = america.pnpla3$scale) +
                      geom_label_repel(
                        data          = subset(america.pnpla3,  date > -3000),
                        nudge_x       = -1000 - subset(america.pnpla3, date > -3000)$date,
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
                        data          = subset(america.pnpla3,  date < -3000 & date > -7000),
                        nudge_x       = -5500 - subset(america.pnpla3, date < -3000 & date > -7000)$date,
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
                        data          = subset(america.pnpla3,  date < -7000 & date > -15000),
                        nudge_x       = -12500 - subset(america.pnpla3, date < -7000 & date > -15000)$date,
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
                      labs(title    = "B \nPNPLA3 - Regression Line America",
                           subtitle = paste("Slope: ", format(round(stats::coefficients(stats::lm(formula = america.pnpla3$pnpla3 ~ america.pnpla3$date, weights = america.pnpla3$individuals))[2]*10000, digits = 4), digits = 4, scientific = FALSE)),
                           x = "Date (BP)", y = "Allele Frequency") +
                      scale_y_continuous(breaks = c(0.0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0), limits = c(0,1)) +
                      scale_x_continuous(limits = c(-15000,0)) +
                      theme_bw()

# =============================================================================
# America - PNPLA3 - linear Regression + Table
# =============================================================================
group.table <- cowplot::plot_grid(group.table.1, group.table.2, group.table.3, nrow = 1, ncol = 3, scale = c(1,0.97,1), axis = "ttt")
plot.pnpla3 <- cowplot::plot_grid(lm.america.pnpla3, group.table, nrow = 2, ncol = 1)
ggsave(filename = "LinearRegression_America.pdf", plot = plot.pnpla3, device = "pdf", path = "5. Plots", width = 18, height = 13, units = "cm")
ggsave(filename = "LinearRegression_America.png", plot = plot.pnpla3, device = "png", path = "5. Plots", width = 18, height = 13, units = "cm")

