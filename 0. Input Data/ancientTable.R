library(tidyverse)
library(writexl)
library(DT)
library(data.table)

# =============================================================================
# Input Data
# =============================================================================
individuals.data <- tibble::as_tibble(read.csv(file = "0. Input Data/ancient_table_data.csv")) %>% dplyr::select(-1) %>% dplyr::select(-c(12:1011))

# filter ancient individuals
ancient.ind <- dplyr::filter(individuals.data, date < 0 | date > 15000)
ancient.ind <- dplyr::filter(ancient.ind, pnpla3 != 9)

# =============================================================================
# Set Phylogeny
# =============================================================================
ancient.ind <- tibble::add_column(ancient.ind, Phylogeny = "Modern Humans", .after = "group.label")
# Great Apes
orang.utan <- tibble::tibble(Individual = "Orang-Utan (Reference)", Phylogeny = "Great Apes", Region = "n/a", Continent = "n/a", pnpla3 = NA, Genotype = "C")
gorilla    <- tibble::tibble(Individual = "Gorilla (Reference)",    Phylogeny = "Great Apes", Region = "n/a", Continent = "n/a", pnpla3 = NA, Genotype = "C")
chimpanzee <- tibble::tibble(Individual = "Chimpanzee (Reference)", Phylogeny = "Great Apes", Region = "n/a", Continent = "n/a", pnpla3 = NA, Genotype = "C")
bonobo     <- tibble::tibble(Individual = "Bonobo (Reference)",     Phylogeny = "Great Apes", Region = "n/a", Continent = "n/a", pnpla3 = NA, Genotype = "C")
# Neanderthals
ancient.ind <- dplyr::mutate(ancient.ind, Phylogeny = ifelse(group.label == "Altai_published.DG" |
                                                             group.label == "Altai_snpAD.DG" |
                                                             group.label == "Chagyrskaya.SG" |
                                                             group.label == "Les_Cottes_final_provisional.SG" |
                                                             group.label == "Mezmaiskaya1_final_provisional.SG" |
                                                             group.label == "Mezmaiskaya2_final_provisional.SG" | 
                                                             group.label == "Spy_final_provisional.SG" |
                                                             group.label == "VindijaG1_final_provisional.SG", 
                                                             "Neanderthal", Phylogeny))
# Denisovan
ancient.ind <- dplyr::mutate(ancient.ind, Phylogeny = ifelse(group.label == "Denisova_published.DG" |
                                                             group.label == "Denisova_snpAD.DG" |
                                                             group.label == "Denisova11.SG",
                                                             "Denisovan", Phylogeny))

# =============================================================================
# Set Genotype
# =============================================================================
ancient.ind <- tibble::add_column(ancient.ind, Genotype  = "", .after = "pnpla3")
ancient.ind <- dplyr::mutate(ancient.ind, Genotype = ifelse(pnpla3 == 2, 
                                                            "C", "G"))

# rename columns
data.table::setnames(ancient.ind, "group.label", "Individual")
data.table::setnames(ancient.ind, "continent",   "Continent")
data.table::setnames(ancient.ind, "country",     "Region")

ancient.ind <- dplyr::select(ancient.ind, -c(5:9, 12:13))
ancient.ind <- data.table::setorder(ancient.ind, Phylogeny)

ancient.table <- rbind(orang.utan, gorilla) %>% rbind(chimpanzee) %>% rbind(bonobo) %>% rbind(ancient.ind)
# ancient.table <- ancient.table[c(1:4,29:38,5:7,8:28),]
great.apes    <- dplyr::filter(ancient.table, Phylogeny == "Great Apes")    %>% data.table::setorder(-Genotype)
neanderthal   <- dplyr::filter(ancient.table, Phylogeny == "Neanderthal")   %>% data.table::setorder(-Genotype)
denisovan     <- dplyr::filter(ancient.table, Phylogeny == "Denisovan")     %>% data.table::setorder(-Genotype)
modern.humans <- dplyr::filter(ancient.table, Phylogeny == "Modern Humans") %>% data.table::setorder(-Genotype)

ancient.table <- rbind(great.apes, neanderthal) %>% rbind(denisovan) %>% rbind(modern.humans)
ancient.table <- dplyr::select(ancient.table, -5)


writexl::write_xlsx(ancient.table, "6. Tables/ancient.table.xlsx")
