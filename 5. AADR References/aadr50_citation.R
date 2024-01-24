library(magrittr)

#### prepare a list of all required papers by aadr citation keys ####

# download aadr reference tables
aadr50_1240K <- readr::read_tsv("https://reichdata.hms.harvard.edu/pub/datasets/amh_repo/curated_releases/V50/V50.0/SHARE/public.dir/v50.0_1240K_public.anno")
aadr50_HO <- readr::read_tsv("https://reichdata.hms.harvard.edu/pub/datasets/amh_repo/curated_releases/V50/V50.0/SHARE/public.dir/v50.0_HO_public.anno")

# compile a simple list of publication keys
keys_as_reported <- c(aadr50_1240K$Publication, aadr50_HO$Publication) %>% unique()
keys_sanitized <- keys_as_reported %>%
  stringr::str_extract_all(pattern = "[a-zA-Z]{5,}[0-9]{4}|1KGPhase3") %>%
  purrr::map(function(x) {
    if (all(is.na(x))) { NULL } else { x }
  }) %>%
  unlist %>%
  unique()

# replace bad keys (e.g. doi duplicates as identified below)
key_replacement <- tibble::tribble(
  ~bad, ~good,
  "RaghavanNature2013", "RaghavanNature2014",
  "Olalde2014", "OlaldeNature2014",
  "Gamba2014", "GambaNatureCommunications2014",
  "SiskaScienceAdvances2017", "SikoraScience2017",
  "VyasDryadDigitalRepository2017", "VyasAJPA2017"
)
keys_final <- keys_sanitized %>%
  purrr::map_chr(function(k) {
    if (k %in% key_replacement$bad) {
      key_replacement$good[key_replacement$bad == k]
    } else {
      k
    }
  }) %>% unique()

# write keys to the file system
writeLines(keys_final, "aadr_citation_keys.txt")

#### compile list of DOIs ###

# find dois for keys using aadr2doi: https://github.com/nevrome/aadr2doi
file.remove("aadr2doi_result.txt") # aadr2doi appends
system(paste(
  "aadr2doi",
  "--inFile aadr_citation_keys.txt",
  "--aadrVersion 50.0",
  "--doiShape Short",
  "--printKey",
  "-o aadr2doi_result.txt"
))

aadr2doi_result <- readr::read_tsv(
  "aadr2doi_result.txt",
  col_names = c("key", "doi")
)

# add missing DOIs that can not be recovered from the website
additional_dois <- tibble::tribble(
  ~key, ~doi,
  "BraceDiekmannNatureEcologyEvolution2019", "10.1038/s41559-019-0871-9",
  "KanzawaKiriyamaJHG2016", "10.1038/jhg.2016.110",
  "FanGenomeBiology2019", "10.1186/s13059-019-1679-2",
  "LazaridisNature2016", "10.1038/nature19310",
  "HaakLazaridis2015", "10.1038/nature14317",
  "OrlandoScience2014", "10.1126/science.aaa0114",
  "JonesCurrentBiology2017", "10.1016/j.cub.2016.12.060",
  "ColonMolecularBiologyandEvolution2020", "10.1093/molbev/msz267",  
  "VyasAJPA2017", "10.1002/ajpa.23312"
)

# combine automatically retrieved and manually added dois
combined_doi_table <- dplyr::bind_rows(aadr2doi_result, additional_dois)

# clean out .Paperpile suffix in dois
combined_doi_table$doi <- combined_doi_table$doi %>% gsub(".Paperpile", "", .)

# identify doi duplicates
combined_doi_table %>%
  dplyr::group_by(doi) %>%
  dplyr::summarize(keys = list(key)) %>%
  dplyr::filter(purrr::map_lgl(keys, \(x) length(x) > 1))

# write dois to the file system
writeLines(combined_doi_table$doi, "DOIs.txt")

#### resolve DOIs to BibTeX entries ####

# find bibtex for dois using doi2bib: https://github.com/bibcure/doi2bib
system(paste(
  "doi2bib",
  "-i DOIs.txt",
  "-o references_raw.bib"
))

#### clean resulting .bib file ####

references <- bibtex::read.bib("references_raw.bib")

# manual step: add field "journal" to entries 'Wohns_2021' '_egarac_2020': all "journal = {bioRxiv}"

references <- bibtex::read.bib("references_raw.bib")

# check which doi's are actually there
dois_actually_in_bibtex <- references %>% purrr::map_chr(\(x) x$doi)
setdiff(combined_doi_table$doi, dois_actually_in_bibtex)

#### render .bib file to bibliography ####

write(
"---
bibliography: references_raw.bib
nocite: '@*'
lang: en
---
",
file = "doc.md"
)

rmarkdown::render("doc.md", rmarkdown::word_document())

