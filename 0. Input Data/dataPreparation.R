library(tidyverse)
library(ggplot2)

ancient.table <- TRUE

# =============================================================================
# Auxiliary Files
# =============================================================================
country.continents <- as_tibble(readxl::read_excel("0. Input Data/Auxiliary Files/country_and_continents.xlsx")) %>% 
                      data.table::setDT() %>% data.table::setorder(col = Country)
missing.coords     <- as_tibble(readxl::read_excel("0. Input Data/Auxiliary Files/missingCoords.xlsx")) %>% 
                      data.table::setDT()
missing.coords$mean.ycoord <- readr::parse_double(missing.coords$mean.ycoord, na = "..")
missing.coords$mean.xcoord <- readr::parse_double(missing.coords$mean.xcoord, na = "..")

# =============================================================================
# Input Data
# =============================================================================
# data set
individuals.data <- tibble::as_tibble(read.csv("0. Input Data/dat_AADR_v50_FattyLiverSNPs_1000random.tsv", sep = "\t"))
if(ancient.table)
{
  # delete .REF indidvuals
  individuals.data <- subset(individuals.data, !(individuals.data$Individual_ID == "Ancestor.REF" | individuals.data$Individual_ID == "Chimp.REF" | 
                                                 individuals.data$Individual_ID == "Gorilla.REF"  | individuals.data$Individual_ID == "Href.REF"))
}

# rename columns
data.table::setnames(individuals.data, "PNPLA3.rs738409.",   "pnpla3")
data.table::setnames(individuals.data, "TM6SF2.rs58542926.", "tm6sf2")
data.table::setnames(individuals.data, "MBOAT7.rs641738.",   "mboat7")
data.table::setnames(individuals.data, "Country",            "country")
data.table::setnames(individuals.data, "Individual_ID",      "group.label")
data.table::setnames(individuals.data, "Longitude",          "xcoord")
data.table::setnames(individuals.data, "Latitude",           "ycoord")
individuals.data$country   <- as.character(individuals.data$country)
# calculate BC-date column and set modern individuals to 0
individuals.data <- tibble::add_column(individuals.data, date = abs((individuals.data$Date_BC_AD_Start+individuals.data$Date_BC_AD_Stop)/2)+1950, .after = "Date_BC_AD_Stop")
# mark neanderthal without date information
individuals.data$date[individuals.data$group.label == "VindijaG1_final_provisional.SG"] <- -1
individuals.data$date[is.na(individuals.data$date)] <- 0
individuals.data <- dplyr::select(individuals.data, -5, -6, -8)

# =============================================================================
# Set Continents
# =============================================================================
individuals.data <- tibble::add_column(individuals.data, continent = "", .after = "country")
for(ind in 1:nrow(individuals.data)) {
  individuals.data$continent[ind] <- country.continents$Continent[match(individuals.data$country[ind], country.continents$Country)]
  if(individuals.data$country[ind] == "Azerbaijan") individuals.data$continent[ind] <- "Asia"
}

# =============================================================================
# Subregions Oceania
# =============================================================================
individuals.data <- tibble::add_column(individuals.data, subregion = "", .after = "continent")
# set subregions in Oceania
individuals.data <- dplyr::mutate(individuals.data, subregion = ifelse(continent == "Oceania" | continent == "Oceanien", "Oceania", subregion))
individuals.data <- dplyr::mutate(individuals.data, continent = ifelse(continent == "Oceanien", "Oceania", continent))
# =============================================================================
# Subregions America
# =============================================================================
individuals.data <- dplyr::mutate(individuals.data, subregion = ifelse(continent == "North America", "North America", subregion))
individuals.data <- dplyr::mutate(individuals.data, subregion = ifelse(continent == "South America", "South America", continent))
individuals.data <- dplyr::mutate(individuals.data, subregion = ifelse(country == "Barbados"  | country == "Mexico"             | country == "Puerto Rico" | country == "Bahamas" | country == "Guam"   |
                                                                       country == "Cuba"      | country == "Dominican Republic" | country == "Guadeloupe"  | country == "Haiti" | country == "Panama" |
                                                                       country == "St. Lucia" | country == "Belize", "Middle America", subregion))

# =============================================================================
# Subregions Asia
# =============================================================================
# Central Asia
individuals.data <- dplyr::mutate(individuals.data, subregion = ifelse(country == "Kazakhstan" | country == "Kyrgyzstan" | country == "Tajikistan" | country == "Turkmenistan" | country == "Uzbekistan", 
                                                                       "Central Asia", subregion))
# East Asia
individuals.data <- dplyr::mutate(individuals.data, subregion = ifelse(country == "China" | country == "Japan" | country == "South Korea" | country == "Taiwan" | country == "Mongolia", 
                                                                       "East Asia", subregion))
# North Asia
individuals.data <- dplyr::mutate(individuals.data, subregion = ifelse(country == "Russia", "North Asia", subregion))
# South Asia
individuals.data <- dplyr::mutate(individuals.data, subregion = ifelse(country == "Bangladesh" | country == "India" | country == "Iran" | country == "Nepal" | country == "Afghanistan" |
                                                                       country == "Pakistan"   | country == "Sri Lanka", 
                                                                       "South Asia", subregion))
# South East Asia
individuals.data <- dplyr::mutate(individuals.data, subregion = ifelse(country == "Brunei" | country == "Cambodia" | country == "Malaysia" | country == "Myanmar" | country == "Philippines" | country == "Indonesia" |
                                                                       country == "Thailand" | country == "Vietnam"  | country == "Laos", 
                                                                       "South East Asia", subregion))
# West Asia
individuals.data <- dplyr::mutate(individuals.data, subregion = ifelse(country == "Armenia" | country == "Georgia" | country == "Iraq"   | country == "Israel" | country == "Jordan" | country == "Syria" |
                                                                       country == "Turkey"  | country == "Yemen"   |country == "Lebanon" | country == "Azerbaijan", 
                                                                       "West Asia", subregion))

# =============================================================================
# Subregions Europe
# =============================================================================
# East Europe
individuals.data <- dplyr::mutate(individuals.data, subregion = ifelse(country == "Ukraine" | country == "Moldova" | country == "Crimea", "East Europe", subregion))
# Middle Europe
individuals.data <- dplyr::mutate(individuals.data, subregion = ifelse(country == "Croatia" | country == "Czech Republic" | country == "Hungary"     | country == "Poland"      | country == "Slovakia" | country == "Belgium" |
                                                                       country == "Germany" | country == "Austria"        | country == "Netherlands" | country == "Switzerland" | country == "Luxembourg", 
                                                                       "Middle Europe", subregion))
# North Europe
individuals.data <- dplyr::mutate(individuals.data, subregion = ifelse(country == "Estonia" | country == "Finland" | country == "Iceland" | country == "Norway" | country == "United Kingdom" | country == "Isle of Man" |
                                                                       country == "Denmark" | country == "Latvia"  | country == "Sweden"  | country == "Faroes" | country == "Ireland"        | country == "Lithuania", 
                                                                       "North Europe", subregion))
# South Eruope
individuals.data <- dplyr::mutate(individuals.data, subregion = ifelse(country == "Italy", "South Europe", subregion))
# South East Europe
individuals.data <- dplyr::mutate(individuals.data, subregion = ifelse(country == "Albania" | country == "Bulgaria" | country == "Greece" | country == "Romania" | country == "Montenegro" |
                                                                       country == "Serbia"  | country == "North Macedonia", 
                                                                       "South East Europe", subregion))
# West Europe
individuals.data <- dplyr::mutate(individuals.data, subregion = ifelse(country == "France" | country == "Spain" | country == "Canary Islands" | country == "Portugal" | country == "Gibraltar", "West Europe", subregion))

# =============================================================================
# Missing Coordinates
# =============================================================================
for(ind in 1:nrow(individuals.data)) 
{
  if(is.na(individuals.data$ycoord[ind]) & is.na(individuals.data$xcoord[ind])) {
    individuals.data$ycoord[ind] <- missing.coords$mean.ycoord[match(individuals.data$country[ind], missing.coords$country)]
    individuals.data$xcoord[ind] <- missing.coords$mean.xcoord[match(individuals.data$country[ind], missing.coords$country)]
  }
}

individuals.data <- tibble::add_column(individuals.data, group = -1, .after = "subregion")
# =============================================================================
# Set Groups in Africa
# =============================================================================
# North Africa
individuals.data <- dplyr::mutate(individuals.data, group =     ifelse(country == "Algeria" | country == "Western Sahara (Morocco)" | country == "Morocco",  1, group))
individuals.data <- dplyr::mutate(individuals.data, subregion = ifelse(country == "Algeria" | country == "Western Sahara (Morocco)" | country == "Morocco", "North Africa", subregion))
# South Africa
individuals.data <- dplyr::mutate(individuals.data, group =     ifelse(country == "Angola"  | country == "BotswanaOrNamibia" | country == "Botswana" |country == "Lesotho" | 
                                                                       country == "Namibia" | country == "South Africa"      | country == "Malawi", 2, group))
individuals.data <- dplyr::mutate(individuals.data, subregion = ifelse(country == "Angola"  | country == "BotswanaOrNamibia" | country == "Botswana" |country == "Lesotho" | 
                                                                       country == "Namibia" | country == "South Africa"      | country == "Malawi", "South Africa", subregion))
# Central Africa
individuals.data <- dplyr::mutate(individuals.data, group     = ifelse(country == "Cameroon" | country == "Central African Republic" | country == "Congo" | country == "DR Congo", 3, group))
individuals.data <- dplyr::mutate(individuals.data, subregion = ifelse(country == "Cameroon" | country == "Central African Republic" | country == "Congo" | country == "DR Congo", "Central Africa", subregion))
# West Africa
individuals.data <- dplyr::mutate(individuals.data, group =     ifelse(country == "Chad"     | country == "Nigeria", 4, group))
individuals.data <- dplyr::mutate(individuals.data, subregion = ifelse(country == "Chad" | country == "Nigeria", "West Africa", subregion))
# East Africa
individuals.data <- dplyr::mutate(individuals.data, group =     ifelse(country == "Ethiopia" | country == "Kenya" | country == "Sudan" | country == "Tanzania" | country == "Egypt" | country == "Uganda", 5, group))
individuals.data <- dplyr::mutate(individuals.data, subregion = ifelse(country == "Ethiopia" | country == "Kenya" | country == "Sudan" | country == "Tanzania" | country == "Egypt" | country == "Uganda", "East Africa", subregion))
# West Africa
individuals.data <- dplyr::mutate(individuals.data, group =     ifelse(country == "Gambia" | country == "Senegal" | country == "Sierra Leone", 6, group))
individuals.data <- dplyr::mutate(individuals.data, subregion = ifelse(country == "Gambia" | country == "Senegal" | country == "Sierra Leone", "West Africa", subregion))

# =============================================================================
# Set Groups in America
# =============================================================================
# South America
individuals.data <- dplyr::mutate(individuals.data, group = ifelse(country == "Argentina" | country == "Brazil" | country == "Chile", 1, group))
individuals.data <- dplyr::mutate(individuals.data, group = ifelse(country == "Colombia" | country == "Peru" | country == "Venezuela" | country == "Bolivia" | country == "Curacao", 4, group))
# Middle America
individuals.data <- dplyr::mutate(individuals.data, group = ifelse(country == "Bahamas",             8, group))
individuals.data <- dplyr::mutate(individuals.data, group = ifelse(country == "Cuba",                9, group))
individuals.data <- dplyr::mutate(individuals.data, group = ifelse(country == "Dominican Republic" | country == "Haiti",             10, group))
individuals.data <- dplyr::mutate(individuals.data, group = ifelse(country == "Puerto Rico"        | country == "Guadeloupe",         6, group))
individuals.data <- dplyr::mutate(individuals.data, group = ifelse(country == "Barbados" | country == "St. Lucia",                    2, group))
individuals.data <- dplyr::mutate(individuals.data, group = ifelse(country == "Mexico"   | country == "Belize" | country == "Panama", 5, group))
# North America 
individuals.data <- dplyr::mutate(individuals.data, group = ifelse(country == "Canada",     3, group))
individuals.data <- dplyr::mutate(individuals.data, group = ifelse(country == "USA",        7, group))
individuals.data <- dplyr::mutate(individuals.data, group = ifelse(country == "Greenland", 11, group))

# =============================================================================
# Set Groups in Europe
# =============================================================================
# Middle Europe
individuals.data <- dplyr::mutate(individuals.data, group = ifelse(country == "Croatia"     | country == "Czech Republic" | country == "Hungary" | country == "Poland"      | country == "Germany" |
                                                                   country == "Switzerland" | country == "Slovakia"       | country == "Austria" | country == "Netherlands" | country == "Luxembourg" |
                                                                   country == "Belgium", 2, group))
# North Eruope
individuals.data <- dplyr::mutate(individuals.data, group = ifelse(country == "Estonia" | country == "Finland" | country == "Latvia" | country == "Lithuania", 3, group))
individuals.data <- dplyr::mutate(individuals.data, group = ifelse(country == "Norway"  | country == "Sweden"  | country == "Denmark", 8, group))
individuals.data <- dplyr::mutate(individuals.data, group = ifelse(country == "Iceland" | country == "Faroes",        5, group))
individuals.data <- dplyr::mutate(individuals.data, group = ifelse(country == "United Kingdom" | country == "Ireland" | country == "Isle of Man", 7, group))
# East Europe
individuals.data <- dplyr::mutate(individuals.data, group = ifelse(country == "Ukraine" | country == "Moldova" | country == "Crimea", 9, group))
# South East Europe
individuals.data <- dplyr::mutate(individuals.data, group = ifelse(country == "Albania" | country == "Bulgaria" | country == "Greece" | country == "Serbia" | country == "Romania" | 
                                                                   country == "North Macedonia" | country == "Montenegro", 1, group))
# South Europe
individuals.data <- dplyr::mutate(individuals.data, group = ifelse(country == "Italy", 6, group))
# West Europe
individuals.data <- dplyr::mutate(individuals.data, group = ifelse(country == "France" | country == "Spain" | country == "Canary Islands" | country == "Gibraltar" | country == "Portugal", 4, group))

# =============================================================================
# Set Groups in Asia
# =============================================================================
# Central Asia
individuals.data <- dplyr::mutate(individuals.data, group = ifelse(country == "Kazakhstan" | country == "Kyrgyzstan" | country == "Turkmenistan" | country == "Uzbekistan" | country == "Tajikistan", 1, group))
# East Asia
individuals.data <- dplyr::mutate(individuals.data, group = ifelse(country == "China" | country == "South Korea" | country == "Taiwan" | country == "Mongolia", 2, group))
individuals.data <- dplyr::mutate(individuals.data, group = ifelse(country == "Japan", 3, group))
# North Asia
individuals.data <- dplyr::mutate(individuals.data, group = ifelse(country == "Russia", 4, group))
# South Asia
individuals.data <- dplyr::mutate(individuals.data, group = ifelse(country == "Bangladesh" | country == "India" | country == "Nepal" | country == "Sri Lanka", 5, group))
# South Europe
individuals.data <- dplyr::mutate(individuals.data, group = ifelse(country == "Iran" | country == "Pakistan" | country == "Afghanistan", 6, group))
# South East Asia
individuals.data <- dplyr::mutate(individuals.data, group = ifelse(country == "Brunei"   | country == "Philippines" | country == "Indonesia" | country == "Guam", 7, group))
individuals.data <- dplyr::mutate(individuals.data, group = ifelse(country == "Cambodia" | country == "Malaysia" | country == "Myanmar" | country == "Thailand" | country == "Vietnam" | country == "Laos", 8, group))
# West Asia
individuals.data <- dplyr::mutate(individuals.data, group = ifelse(country == "Armenia" | country == "Georgia" | country == "Iraq" | country == "Turkey" | country == "Yemen" | country == "Azerbaijan" | country == "Syria", 9, group))
individuals.data <- dplyr::mutate(individuals.data, group = ifelse(country == "Israel" | country == "Jordan" | country == "Lebanon", 9, group))

# =============================================================================
# Set Groups in Oceania
# =============================================================================
# Australia
individuals.data <- dplyr::mutate(individuals.data, group     = ifelse(country == "Australia", 1,           group))
individuals.data <- dplyr::mutate(individuals.data, subregion = ifelse(country == "Australia", "Australia", subregion))
# New Zealand
individuals.data <- dplyr::mutate(individuals.data, group     = ifelse(country == "New Zealand",  2,            group))
individuals.data <- dplyr::mutate(individuals.data, subregion = ifelse(country == "New Zealand", "New Zealand", subregion))
# Islands
individuals.data <- dplyr::mutate(individuals.data, group = ifelse(country == "Papua New Guinea", 3, group))
individuals.data <- dplyr::mutate(individuals.data, group = ifelse(country == "Vanuatu" | country == "Solomon Islands" | country == "Tonga", 4, group))
individuals.data <- dplyr::mutate(individuals.data, subregion = ifelse(country == "Papua New Guinea" | country == "Vanuatu" | country == "Tonga" | country == "Solomon Islands", "Islands", subregion))

individuals.data$date <- round(individuals.data$date)

# =============================================================================
# Export
# =============================================================================
individuals.data <- data.table::setorder(individuals.data, continent)
if(!ancient.table) write.csv(individuals.data, "0. Input Data/individuals_data.csv")
if(ancient.table)  write.csv(individuals.data, "0. Input Data/ancient_table_data.csv")




