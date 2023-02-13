
# =============================================================================
# Data Preparation
# =============================================================================
source("0. Input Data/dataPreparation.R")
rm(list = ls(all.names = TRUE))

# create table with ancient individuals
source("0. Input Data/ancientTable.R")
rm(list = ls(all.names = TRUE))

# =============================================================================
# Modern Populations
# =============================================================================
# Africa
source("1. Modern Populations/modernAfrica_pieCharts.R")
rm(list = ls(all.names = TRUE))
# America
source("1. Modern Populations/modernAmerica_pieCharts.R")
rm(list = ls(all.names = TRUE))
# Asia
source("1. Modern Populations/modernAsia_pieCharts.R")
rm(list = ls(all.names = TRUE))
# Europe
source("1. Modern Populations/modernEurope_pieCharts.R")
rm(list = ls(all.names = TRUE))
# Oceania
source("1. Modern Populations/modernOceania_pieCharts.R")
rm(list = ls(all.names = TRUE))

# =============================================================================
# Time Slices
# =============================================================================
# Africa
source("2. Time Slices/Africa_timeSlices_pieCharts.R")
source("2. Time Slices/Africa_timeSlices_groups.R")
rm(list = ls(all.names = TRUE))
# America
source("2. Time Slices/America_timeSlices_pieCharts.R")
source("2. Time Slices/America_timeSlices_groups.R")
# Asia
rm(list = ls(all.names = TRUE))
source("2. Time Slices/Asia_timeSlices_pieCharts.R")
source("2. Time Slices/Asia_timeSlices_groups.R")
# Europe
rm(list = ls(all.names = TRUE))
source("2. Time Slices/Europe_timeSlices_pieCharts.R")
source("2. Time Slices/Europe_timeSlices_groups.R")
rm(list = ls(all.names = TRUE))
# Oceania
source("2. Time Slices/Oceania_timeSlices_pieCharts.R")
rm(list = ls(all.names = TRUE))
# World
source("2. Time Slices/World_timeSlices_pieCharts.R")
rm(list = ls(all.names = TRUE))

# =============================================================================
# Linear Regression
# =============================================================================
source("3. Linear Regression/World_linearRegression.R")
rm(list = ls(all.names = TRUE))

# =============================================================================
# Logistic Regression Date + Genomewide Analysis
# =============================================================================
# logistic Regression
source("4. Selection Tests/World_logisticRegression_Date.R")
rm(list = ls(all.names = TRUE))
# logistic Regression Genome
source("4. Selection Tests/World_logisticRegression_Date_Genome.R")
rm(list = ls(all.names = TRUE))
# =============================================================================
# Logistic Regression Latitude + Genomwide Analysis
# =============================================================================
# logistic Regression
source("4. Selection Tests/World_logisticRegression_Lat.R")
rm(list = ls(all.names = TRUE))
# logistic Regression Genome
source("4. Selection Tests/World_logisticRegression_Lat_Genome.R")
rm(list = ls(all.names = TRUE))
