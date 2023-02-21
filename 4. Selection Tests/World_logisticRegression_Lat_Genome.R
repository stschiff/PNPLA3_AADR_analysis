library(tidyverse)
library(ggplot2)

abs.value <- TRUE

# =============================================================================
# Functions
# =============================================================================
log.reg.model <- function(cur.snp, latitude)
{
  tryCatch(
    {
      cur.snp[cur.snp == 9] <- NA
      cur.snp[cur.snp == 2] <- -2
      cur.snp[cur.snp == 1] <- -1
      cur.snp[cur.snp == 0] <- -3
      cur.data <- as.data.frame(cbind(cur.snp, latitude))
      colnames(cur.data) <- c("snp", "latitude")
      cur.data <- cur.data[complete.cases(cur.data),]
      
      cur.data <- rbind(cur.data, cur.data %>% filter(snp == -2) %>% mutate(snp = 0))
      cur.data <- rbind(cur.data, cur.data %>% filter(snp == -1) %>% mutate(snp = 1))
      cur.data <- rbind(cur.data, cur.data %>% filter(snp == -3) %>% mutate(snp = 1))
      cur.data[cur.data == -2]  <- 0
      cur.data[cur.data == -1]  <- 0
      cur.data[cur.data == -3]  <- 1
      
      cur.log.model <- stats::glm(snp ~ latitude, data = cur.data, family = "binomial")
      if(abs.value) return(abs(cur.log.model$coefficients[2]))
      else          return(cur.log.model$coefficients[2])
    },
    warning = function(warning.message) {
      return(NA)
    }
  )
}

# =============================================================================
# Input Data
# =============================================================================
individuals.data <- tibble::as_tibble(read.csv(file = "0. Input Data/individuals_data.csv")) %>% dplyr::select(-1) %>%
  dplyr::mutate_if(is.factor, as.character)
# exclude individuals with no allele information
ind.pnpla3 <- dplyr::filter(individuals.data, individuals.data$pnpla3 != 9)
ind.pnpla3$ycoord <- abs(ind.pnpla3$ycoord)
# Africa
africa.ind.pnpla3 <- dplyr::filter(ind.pnpla3, continent == "Africa")
# America
america.ind.pnpla3 <- dplyr::filter(ind.pnpla3, continent == "North America" | continent == "South America")
# Asia
asia.ind.pnpla3 <- dplyr::filter(ind.pnpla3, continent == "Asia")
# Europe
europe.ind.pnpla3 <- dplyr::filter(ind.pnpla3, continent == "Europe")

# =============================================================================
# Logistic Regression Models
# =============================================================================
# Africa 
log.reg.africa.pnpla3 <- as.data.frame(lapply(africa.ind.pnpla3[c(9:length(individuals.data))], log.reg.model, africa.ind.pnpla3$ycoord))
log.reg.africa.pnpla3 <- log.reg.africa.pnpla3[colSums(!is.na(log.reg.africa.pnpla3)) > 0]
# America 
log.reg.america.pnpla3 <- as.data.frame(lapply(america.ind.pnpla3[c(9:length(individuals.data))], log.reg.model, america.ind.pnpla3$ycoord))
log.reg.america.pnpla3 <- log.reg.america.pnpla3[colSums(!is.na(log.reg.america.pnpla3)) > 0]
# Asia 
log.reg.asia.pnpla3 <- as.data.frame(lapply(asia.ind.pnpla3[c(9:length(individuals.data))], log.reg.model, asia.ind.pnpla3$ycoord))
log.reg.asia.pnpla3 <- log.reg.asia.pnpla3[colSums(!is.na(log.reg.asia.pnpla3)) > 0]
# Europe 
log.reg.europe.pnpla3 <- as.data.frame(lapply(europe.ind.pnpla3[c(9:length(individuals.data))], log.reg.model, europe.ind.pnpla3$ycoord))
log.reg.europe.pnpla3 <- log.reg.europe.pnpla3[colSums(!is.na(log.reg.europe.pnpla3)) > 0]

# =============================================================================
# Plots
# =============================================================================
hist.colour    <- "gray0"
hist.size      <- 1
geom.dens.size <- 1
signal.size    <- 1.5
stats.size     <- 1
# Africa
africa.pnpla3      <- log.reg.africa.pnpla3[1,1]
data.africa.pnpla3 <- as.data.frame(lapply(dplyr::select(log.reg.africa.pnpla3, -c(1)), `[[`, 1)) %>% t() %>% na.omit()
data.africa.pnpla3 <- as.data.frame(data.africa.pnpla3)
colnames(data.africa.pnpla3) <- c("beta1")
selection.africa.pnpla3 <- ggplot(data = data.africa.pnpla3, aes(x = beta1)) +
  geom_histogram(aes(y = ..density..), bins = 100, color = hist.colour, fill = "white", size = hist.size) +
  geom_density(color = "tomato1", fill = "tomato1", alpha = 0.5, linewidth = geom.dens.size) +
  geom_vline(xintercept=africa.pnpla3, color = "black",   linetype = "solid",  size = signal.size) +
  geom_vline(xintercept=mean(data.africa.pnpla3$beta1),                                color = "red", linetype = "solid",  size = stats.size) +
  geom_vline(xintercept=mean(data.africa.pnpla3$beta1)+sd(data.africa.pnpla3$beta1),   color = "red", linetype = "dashed", size = stats.size) +
  geom_vline(xintercept=mean(data.africa.pnpla3$beta1)+2*sd(data.africa.pnpla3$beta1), color = "red", linetype = "dashed", size = stats.size) +
  labs(title = "PNPLA3 - Genome-wide Analysis Africa - Latitude", x = "beta1", y = "Density") +
  theme_bw()
# America
america.pnpla3      <- log.reg.america.pnpla3[1,1]
data.america.pnpla3 <- as.data.frame(lapply(dplyr::select(log.reg.america.pnpla3, -c(1)), `[[`, 1)) %>% t() %>% na.omit()
data.america.pnpla3 <- as.data.frame(data.america.pnpla3)
colnames(data.america.pnpla3) <- c("beta1")
selection.america.pnpla3 <- ggplot(data = data.america.pnpla3, aes(x = beta1)) +
  geom_histogram(aes(y = ..density..), bins = 100, color = hist.colour, fill = "white", size = hist.size) +
  geom_density(color = "tomato1", fill = "tomato1", alpha = 0.5, linewidth = geom.dens.size) +
  geom_vline(xintercept=america.pnpla3, color = "black",   linetype = "solid",  size = signal.size) +
  geom_vline(xintercept=mean(data.america.pnpla3$beta1),                                 color = "red", linetype = "solid",  size = stats.size) +
  geom_vline(xintercept=mean(data.america.pnpla3$beta1)+sd(data.america.pnpla3$beta1),   color = "red", linetype = "dashed", size = stats.size) +
  geom_vline(xintercept=mean(data.america.pnpla3$beta1)+2*sd(data.america.pnpla3$beta1), color = "red", linetype = "dashed", size = stats.size) +
  labs(title = "PNPLA3 - Genome-wide Analysis America - Latitude", x = "beta1", y = "Density") +
  theme_bw()
# Asia
asia.pnpla3      <- log.reg.asia.pnpla3[1,1]
data.asia.pnpla3 <- as.data.frame(lapply(dplyr::select(log.reg.asia.pnpla3, -c(1)), `[[`, 1)) %>% t() %>% na.omit()
data.asia.pnpla3 <- as.data.frame(data.asia.pnpla3)
colnames(data.asia.pnpla3) <- c("beta1")
selection.asia.pnpla3 <- ggplot(data = data.asia.pnpla3, aes(x = beta1)) +
  geom_histogram(aes(y = ..density..), bins = 100, color = hist.colour, fill = "white", size = hist.size) +
  geom_density(color = "tomato1", fill = "tomato1", alpha = 0.5, linewidth = geom.dens.size) +
  geom_vline(xintercept=asia.pnpla3, color = "black",   linetype = "solid",  size = signal.size) +
  geom_vline(xintercept=mean(data.asia.pnpla3$beta1),                                color = "red", linetype = "solid",  size = stats.size) +
  geom_vline(xintercept=mean(data.asia.pnpla3$beta1)+sd(data.asia.pnpla3$beta1),   color = "red", linetype = "dashed", size = stats.size) +
  geom_vline(xintercept=mean(data.asia.pnpla3$beta1)+2*sd(data.asia.pnpla3$beta1), color = "red", linetype = "dashed", size = stats.size) +
  labs(title = "PNPLA3 - Genome-wide Analysis Asia - Latitude", x = "beta1", y = "Density") +
  theme_bw()
# Europe
europe.pnpla3      <- log.reg.europe.pnpla3[1,1]
data.europe.pnpla3 <- as.data.frame(lapply(dplyr::select(log.reg.europe.pnpla3, -c(1)), `[[`, 1)) %>% t() %>% na.omit()
data.europe.pnpla3 <- as.data.frame(data.europe.pnpla3)
colnames(data.europe.pnpla3) <- c("beta1")
selection.europe.pnpla3 <- ggplot(data = data.europe.pnpla3, aes(x = beta1)) +
  geom_histogram(aes(y = ..density..), bins = 100, color = hist.colour, fill = "white", size = hist.size) +
  geom_density(color = "tomato1", fill = "tomato1", alpha = 0.5, linewidth = geom.dens.size) +
  geom_vline(xintercept=europe.pnpla3, color = "black",   linetype = "solid",  size = signal.size) +
  geom_vline(xintercept=mean(data.europe.pnpla3$beta1),                                color = "red", linetype = "solid",  size = stats.size) +
  geom_vline(xintercept=mean(data.europe.pnpla3$beta1)+sd(data.europe.pnpla3$beta1),   color = "red", linetype = "dashed", size = stats.size) +
  geom_vline(xintercept=mean(data.europe.pnpla3$beta1)+2*sd(data.europe.pnpla3$beta1), color = "red", linetype = "dashed", size = stats.size) +
  labs(title = "PNPLA3 - Genome-wide Analysis Europe - Latitude", x = "beta1", y = "Density") +
  theme_bw()

# =============================================================================
# logistic Regression World
# =============================================================================
log.reg.gen.world <- cowplot::plot_grid(selection.africa.pnpla3, selection.america.pnpla3, 
                                        selection.asia.pnpla3,   selection.europe.pnpla3,  nrow = 4, ncol = 1, labels = "AUTO")
ggsave(filename = "LogisticRegression_Lat_Genome.pdf", plot = log.reg.gen.world, device = "pdf", path = "5. Plots", width = 18, height = 26, units = "cm")
ggsave(filename = "LogisticRegression_Lat_Genome.png", plot = log.reg.gen.world, device = "png", path = "5. Plots", width = 18, height = 26, units = "cm")
