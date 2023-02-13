library(magrittr)

janno_obj <- poseidonR::read_janno("data/aadr_extracted.janno", validate = FALSE)

snp_obj <- readr::read_tsv("data/aadr_extracted.snp",
                           col_types = "cidicc",
                           col_names = c("rsid", "chrom", "gen_pos", "pos", "ref", "alt"))

nr_inds <- nrow(janno_obj)

v50_dat <- readr::read_tsv("data/v50.0_1240K_public.anno.txt", show_col_types = FALSE) %>%
  dplyr::select(`Version ID`, Country) %>%
  dplyr::rename(Individual_ID = `Version ID`)

# Character Vector
genotypes_raw <- scan("data/aadr_extracted.geno", what = "character")

# List of vectors
genotypes <- strsplit(genotypes_raw, "") %>% purrr::map(as.numeric)
names(genotypes) <- snp_obj$rsid

# Tibble
genotypes_dat <- tibble::as_tibble(genotypes) %>% dplyr::relocate(
  tidyselect::any_of(c("rs738409", "rs58542926", "rs641738"))
) %>%
  dplyr::rename("PNPLA3(rs738409)" = "rs738409",
                "TM6SF2(rs58542926)" = "rs58542926",
                "MBOAT7(rs641738)" = "rs641738")

dat <- janno_obj %>% dplyr::select(
  Individual_ID, Latitude, Longitude, Date_BC_AD_Start, Date_BC_AD_Stop,
  Group_Name
) %>% dplyr::left_join(v50_dat) %>% dplyr::bind_cols(genotypes_dat) %>%
  dplyr::relocate(Country, .before = Latitude)

readr::write_tsv(dat, "dat_AADR_v50_FattyLiverSNPs_1000random.tsv")
