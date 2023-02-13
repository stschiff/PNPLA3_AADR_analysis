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
africa.modern.pnpla3 <- tibble::as_tibble(read.csv(file = "1. Modern Populations/1. Data/africa_modern_pnpla3.csv")) %>% dplyr::select(-1)
africa.ts.1.pnpla3   <- tibble::as_tibble(read.csv(file = "2. Time Slices/1. Data/africa_timeSlice1_pnpla3.csv")) %>% dplyr::select(-1)
africa.ts.2.pnpla3   <- tibble::as_tibble(read.csv(file = "2. Time Slices/1. Data/africa_timeSlice2_pnpla3.csv")) %>% dplyr::select(-1)
africa.ts.3.pnpla3   <- tibble::as_tibble(read.csv(file = "2. Time Slices/1. Data/africa_timeSlice3_pnpla3.csv")) %>% dplyr::select(-1)

# modern population data points
africa.modern.pnpla3$subregion <- as.character(africa.modern.pnpla3$subregion)
modern.dp.pnpla3 <- data.table::data.table(group       = 0,
                                           subregion   = "Africa",
                                           individuals = sum(africa.modern.pnpla3$individuals),
                                           mean.ycoord = mean(africa.modern.pnpla3$mean.ycoord),
                                           mean.xcoord = mean(africa.modern.pnpla3$mean.xcoord),
                                           date        = 0,
                                           pnpla3      = sum(africa.modern.pnpla3$pnpla3*africa.modern.pnpla3$individuals)/sum(africa.modern.pnpla3$individuals),
                                           reference   = 1 - sum(africa.modern.pnpla3$pnpla3*africa.modern.pnpla3$individuals)/sum(africa.modern.pnpla3$individuals),
                                           scale       = 5)
# merge data for genes
africa.pnpla3 <- rbind(modern.dp.pnpla3, africa.ts.1.pnpla3) %>% rbind(africa.ts.2.pnpla3) %>% rbind(africa.ts.3.pnpla3)
africa.pnpla3$date  <- africa.pnpla3$date * (-1)
africa.pnpla3$group <- c(0,2,1,6,4,3,5,9,8,7)
africa.pnpla3$scale <- africa.pnpla3$scale / 5

# =============================================================================
# Table Data
# =============================================================================
# individuals
africa.ts.1.ind.table.data.pnpla3 <- tibble::as_tibble(read.csv(file = "2. Time Slices/1. Data/africa_timeSlice1_ind_table_data_pnpla3.csv")) %>% dplyr::select(-1) %>%
                                     data.table::setorder(group)
africa.ts.2.ind.table.data.pnpla3 <- tibble::as_tibble(read.csv(file = "2. Time Slices/1. Data/africa_timeSlice2_ind_table_data_pnpla3.csv")) %>% dplyr::select(-1) %>%
                                     data.table::setorder(group)
africa.ts.3.ind.table.data.pnpla3 <- tibble::as_tibble(read.csv(file = "2. Time Slices/1. Data/africa_timeSlice3_ind_table_data_pnpla3.csv")) %>% dplyr::select(-1) %>%
                                     data.table::setorder(group)
# groups
africa.ts.1.group.table.data.pnpla3 <- tibble::as_tibble(read.csv(file = "2. Time Slices/1. Data/africa_timeSlice1_group_table_data_pnpla3.csv")) %>% dplyr::select(-1)
africa.ts.2.group.table.data.pnpla3 <- tibble::as_tibble(read.csv(file = "2. Time Slices/1. Data/africa_timeSlice2_group_table_data_pnpla3.csv")) %>% dplyr::select(-1)
africa.ts.3.group.table.data.pnpla3 <- tibble::as_tibble(read.csv(file = "2. Time Slices/1. Data/africa_timeSlice3_group_table_data_pnpla3.csv")) %>% dplyr::select(-1)

# =============================================================================
# Individuals Table
# =============================================================================
# time slice 1
africa.ts.1.ind.table.data.pnpla3 <- dplyr::mutate(africa.ts.1.ind.table.data.pnpla3, group =ifelse(group == 5, 1, group))
# time slice 2
africa.ts.2.ind.table.data.pnpla3 <- dplyr::mutate(africa.ts.2.ind.table.data.pnpla3, group =ifelse(group == 1, 6, group))
africa.ts.2.ind.table.data.pnpla3 <- dplyr::mutate(africa.ts.2.ind.table.data.pnpla3, group =ifelse(group == 2, 4, group))
# time slice 3
africa.ts.3.ind.table.data.pnpla3 <- dplyr::mutate(africa.ts.3.ind.table.data.pnpla3, group =ifelse(group == 1, 9, group))
africa.ts.3.ind.table.data.pnpla3 <- dplyr::mutate(africa.ts.3.ind.table.data.pnpla3, group =ifelse(group == 2, 8, group))
africa.ts.3.ind.table.data.pnpla3 <- dplyr::mutate(africa.ts.3.ind.table.data.pnpla3, group =ifelse(group == 3, 7, group))
# merge data table
africa.ind.table.data.pnpla3 <- rbind(africa.ts.1.ind.table.data.pnpla3, africa.ts.2.ind.table.data.pnpla3) %>% rbind(africa.ts.3.ind.table.data.pnpla3) %>%
                                data.table::setorder(group)
# create individual table
ft_raster_ind <- africa.ind.table.data.pnpla3 %>% flextable::flextable() %>% as_raster()
ind.table     <- ggplot() + 
                  theme_void() + 
                  annotation_custom(rasterGrob(ft_raster_ind), xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf)

# =============================================================================
# Group Table
# =============================================================================
# merge table data
africa.group.table.data.pnpla3      <- rbind(africa.ts.1.group.table.data.pnpla3, africa.ts.2.group.table.data.pnpla3) %>% rbind(africa.ts.3.group.table.data.pnpla3) %>%
                                       data.table::setorder(date) 
africa.group.table.data.pnpla3$group <- c(1:nrow(africa.group.table.data.pnpla3))
# create group table
std_border = officer::fp_border(color="black", width = 1)
ft_raster_group <- flextable::flextable(africa.group.table.data.pnpla3) 
ft_raster_group <- hline(ft_raster_group, i = c(2,6), border = std_border)
ft_raster_group <- as_raster(ft_raster_group)
group.table     <- ggplot() + 
                    theme_void() + 
                    annotation_custom(rasterGrob(ft_raster_group), xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf)
# linear regression
lm.africa.pnpla3 <- ggplot(data = africa.pnpla3, aes(date, pnpla3, label = group)) +
                      # vertical lines for time slices
                      geom_vline(xintercept = -3000) +
                      geom_vline(xintercept = -7000) +
                      # horizontal line for mean allele frequency in modern populations
                      geom_hline(yintercept = sum(africa.modern.pnpla3$pnpla3*africa.modern.pnpla3$individuals)/sum(africa.modern.pnpla3$individuals), linetype = "dashed") +
                      # regression line
                      geom_abline(intercept = stats::coefficients(stats::lm(formula = africa.pnpla3$pnpla3 ~ africa.pnpla3$date, weights = africa.pnpla3$individuals))[1],
                                  slope     = stats::coefficients(stats::lm(formula = africa.pnpla3$pnpla3 ~ africa.pnpla3$date, weights = africa.pnpla3$individuals))[2],
                                  color = "red") +
                      # points
                      geom_point(size = africa.pnpla3$scale) +
                      geom_label_repel(
                        data          = subset(africa.pnpla3,  date > -3000),
                        nudge_x       = -1000 - subset(africa.pnpla3, date > -3000)$date,
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
                        data          = subset(africa.pnpla3,  date < -3000 & date > -7000),
                        nudge_x       = -6800 - subset(africa.pnpla3, date < -3000 & date > -7000)$date,
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
                        data          = subset(africa.pnpla3,  date < -7000 & date > -15000),
                        nudge_x       = -12500 - subset(africa.pnpla3, date < -7000 & date > -15000)$date,
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
                      labs(title    = "A \nPNPLA3 - Regression Line Africa",
                           subtitle = paste("Slope: ", format(round(stats::coefficients(stats::lm(formula = africa.pnpla3$pnpla3 ~ africa.pnpla3$date, weights = africa.pnpla3$individuals))[2]*10000, digits = 4), digits = 4, scientific = FALSE)),
                           x = "Date (BP)", y = "Allele Frequency") +
                      scale_y_continuous(breaks = c(0.0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0), limits = c(0,1)) +
                      scale_x_continuous(limits = c(-15000,0)) +
                      theme_bw()

# =============================================================================
# Africa - PNPLA3 - linear Regression + Table
# =============================================================================
plot.pnpla3 <- cowplot::plot_grid(lm.africa.pnpla3, group.table, nrow = 2, ncol = 1, labels = "AUTO")
ggsave(filename = "LinearRegression_Africa.pdf", plot = plot.pnpla3, device = "pdf", path = "5. Plots", width = 18, height = 13, units = "cm")
ggsave(filename = "LinearRegression_Africa.png", plot = plot.pnpla3, device = "png", path = "5. Plots", width = 18, height = 13, units = "cm")
