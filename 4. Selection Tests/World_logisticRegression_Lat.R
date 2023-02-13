library(tidyverse)
library(ggplot2)
library(glm.predict)
library(ggrepel)
library(ggpp)

very.ancient <- TRUE

# =============================================================================
# Functions
# =============================================================================
log.reg.model <- function(cur.snp, latitude, cur.snp.string, group.label)
{
  cur.snp[cur.snp == 2] <- -2
  cur.snp[cur.snp == 1] <- -1
  cur.snp[cur.snp == 0] <- -3
  cur.data <- as.data.frame(cbind(cur.snp, latitude))
  colnames(cur.data) <- c("string.tmp", "latitude")
  cur.data <- cur.data[complete.cases(cur.data),]
  
  cur.data <- rbind(cur.data, cur.data %>% filter(string.tmp == -2) %>% mutate(string.tmp = 0))
  cur.data <- rbind(cur.data, cur.data %>% filter(string.tmp == -1) %>% mutate(string.tmp = 1))
  cur.data <- rbind(cur.data, cur.data %>% filter(string.tmp == -3) %>% mutate(string.tmp = 1))
  cur.data[cur.data == -2]  <- 0
  cur.data[cur.data == -1]  <- 0
  cur.data[cur.data == -3]  <- 1
  colnames(cur.data) <- c(cur.snp.string, "latitude")
  cur.data <- cbind(cur.data, group.label)
  
  return(cur.data)
}

# =============================================================================
# Input Data
# =============================================================================
individuals.data <- tibble::as_tibble(read.csv(file = "0. Input Data/individuals_data.csv")) %>% dplyr::select(-1) %>%
  dplyr::mutate_if(is.factor, as.character)
# exclude individuals with no allele information
ind.pnpla3 <- dplyr::filter(individuals.data, individuals.data$pnpla3 != 9)
# use very ancient individuals too
if(!very.ancient) ind.pnpla3 <- dplyr::filter(ind.pnpla3, date <= 15000)

# =============================================================================
# Data preparation
# =============================================================================
# absolute value of ycoord = distance from equator
ind.pnpla3$ycoord <- abs(ind.pnpla3$ycoord)
ind.pnpla3        <- data.table::setorder(ind.pnpla3, continent)
ind.pnpla3$date   <- ind.pnpla3$date * (-1)
# Africa
africa.pnpla3 <- log.reg.model(cur.snp     = dplyr::filter(ind.pnpla3, continent == "Africa")$pnpla3, 
                               latitude    = dplyr::filter(ind.pnpla3, continent == "Africa")$ycoord, "pnpla3", 
                               group.label = dplyr::filter(ind.pnpla3, continent == "Africa")$group.label)
# America
america.pnpla3 <- log.reg.model(cur.snp     = dplyr::filter(ind.pnpla3, subregion == "North America" | subregion == "Middle America" | subregion == "South America")$pnpla3, 
                                latitude    = dplyr::filter(ind.pnpla3, subregion == "North America" | subregion == "Middle America" | subregion == "South America")$ycoord, "pnpla3", 
                                group.label = dplyr::filter(ind.pnpla3, subregion == "North America" | subregion == "Middle America" | subregion == "South America")$group.label)
# Asia
asia.pnpla3 <- log.reg.model(cur.snp     = dplyr::filter(ind.pnpla3, continent == "Asia")$pnpla3, 
                             latitude    = dplyr::filter(ind.pnpla3, continent == "Asia")$ycoord, "pnpla3", 
                             group.label = dplyr::filter(ind.pnpla3, continent == "Asia")$group.label)
# Europe
europe.pnpla3 <- log.reg.model(cur.snp     = dplyr::filter(ind.pnpla3, continent == "Europe")$pnpla3, 
                               latitude    = dplyr::filter(ind.pnpla3, continent == "Europe")$ycoord, "pnpla3", 
                               group.label = dplyr::filter(ind.pnpla3, continent == "Europe")$group.label)

# =============================================================================
# Plots
# =============================================================================
axis.text.size       <- 7
axis.title.size      <- 7
axis.ticks.size      <- 0.3
plot.title.size      <- 9
plot.subtitle.size   <- 8
grid.linewidth.major <- 0.3
grid.linewidth.minor <- 0.2
border.linewidth     <- 0.5
grid.colour          <- "grey"
lr.linewidth         <- 0.4
ts.linewidth         <- 0.2
label.text.size      <- 1.8
point.size           <- 0.2
# Africa
jitter       <- position_jitter(height = 0.1, seed = 1)
jitter_nudge <- position_jitternudge(height = 0.1, seed = 1, y = 0.2, x = 500,
                                     direction   = "split",
                                     nudge.from  = "jittered",
                                     kept.origin = "jittered")
glm.africa.pnpla3 <- ggplot(data = africa.pnpla3, aes(x = latitude, y = pnpla3)) +
                      # logistice regression curve
                      stat_smooth(method = "glm", method.args = list(family = "binomial"), se = FALSE, color = "red", linewidth = 0.5) +
                      # jittered points and labels
                      geom_point(position = jitter,
                                 size     = point.size) +
                      labs(title = "PNPLA3 - Logistic Regression Africa") +
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
jitter       <- position_jitter(height = 0.1, seed = 2)
jitter_nudge <- position_jitternudge(height = 0.1, seed = 2, y = 0.2, x = 500,
                                     direction   = "split",
                                     nudge.from  = "jittered",
                                     kept.origin = "jittered")
glm.america.pnpla3 <- ggplot(data = america.pnpla3, aes(x = latitude, y = pnpla3)) +
                        # logistic regression curve
                        stat_smooth(method = "glm", method.args = list(family = "binomial"), se = FALSE, color = "red", linewidth = 0.5) +
                        # jittered points and labels
                        geom_point(position = jitter,
                                   size     = point.size) +
                        labs(title = "PNPLA3 - Logistic Regression America") +
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
jitter       <- position_jitter(height = 0.1, seed = 123)
jitter_nudge <- position_jitternudge(height = 0.1, seed = 123, y = 0.2, x = 500,
                                     direction   = "split",
                                     nudge.from  = "jittered",
                                     kept.origin = "jittered")
glm.asia.pnpla3 <- ggplot(data = asia.pnpla3, aes(x = latitude, y = pnpla3)) +
                    # logistice regression curve
                    stat_smooth(method = "glm", method.args = list(family = "binomial"), se = FALSE, color = "red", linewidth = 0.5) +
                    geom_point(position = jitter,
                               size     = point.size) +
                    labs(title = "PNPLA3 - Logistic Regression Asia") +
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
jitter       <- position_jitter(height = 0.1, seed = 4)
jitter_nudge <- position_jitternudge(height = 0.1, seed = 4, y = 0.2, x = 500,
                                     direction   = "split",
                                     nudge.from  = "jittered",
                                     kept.origin = "jittered")
glm.europe.pnpla3 <- ggplot(data = europe.pnpla3, aes(x = latitude, y = pnpla3)) +
                      # logistic regression curve
                      stat_smooth(method = "glm", method.args = list(family = "binomial"), se = FALSE, color = "red", linewidth = 0.5) +
                      # jittered points and labels
                      geom_point(position = jitter,
                                 size     = point.size) +
                      labs(title = "PNPLA3 - Logistic Regression Europe") +
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
# logistic Regression World
# =============================================================================
log.reg.world <- cowplot::plot_grid(glm.africa.pnpla3, glm.america.pnpla3, glm.asia.pnpla3, glm.europe.pnpla3, nrow = 4, ncol = 1, labels = "AUTO")
ggsave(filename = "LogisticRegression_Lat.pdf", plot = log.reg.world, device = "pdf", path = "5. Plots", width = 18, height = 26, units = "cm")
ggsave(filename = "LogisticRegression_Lat.png", plot = log.reg.world, device = "png", path = "5. Plots", width = 18, height = 26, units = "cm")
