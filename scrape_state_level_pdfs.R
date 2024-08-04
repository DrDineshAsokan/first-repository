######################################################
# Download PDFs from the NFHS Website and scape them #
######################################################

# Load Packages
library(xml2)
library(rvest)
library(here)
library(tidyverse)
library(fs)
library(pdftools)
library(downloader)


library(tictoc)
tic()
# Create Project Folders
source("project_setup_template.R")

# View the Folder Tree
fs::dir_tree(path = here::here(), recurse = FALSE)

# URL of the District Level Fact Sheets
URL <- "http://rchiips.org/nfhs/factsheet_NFHS-5.shtml"


# URLs at the State Level 
state_URLs <- URL %>% 
  read_html() %>% 
  html_nodes("option") %>% 
  html_attr("value") %>% 
  paste("http://rchiips.org/nfhs/", ., sep = "") %>% 
  tail(-2) %>% 
  head(-3)

state_folders <- str_split(state_URLs, "/") %>% 
  map_chr(pluck,6) %>% 
  str_split("\\.") %>% 
  map_chr(pluck,1) 

# Create Statewise folders 
dir_create(here::here("data", "raw_data", "state_reports", "nfhs5", state_folders))

# Destination for Download
file_dest <- state_folders %>% 
  epitrix::clean_labels() %>% 
  paste(., ".pdf", sep="") %>% 
  here::here("data", "raw_data",  "state_reports", "nfhs5", state_folders,.)

# Download Files
state_URLs  %>% map2(., file_dest, ~download.file(..1, destfile = ..2, mode = "wb"))


file_paths <- dir_info(here::here("data", "raw_data", "state_reports", "nfhs5"), recurse = 1, type = "file") %>% 
  mutate(across(.cols = everything(), as.character)) %>% 
  pull(path)

folder_names <- dir_ls(here::here("data", "raw_data",  "state_reports", "nfhs5")) %>% basename()

# Create Statewise folders 
dir_create(here::here("data", "subset_pdfs", "state_reports", "nfhs5",  folder_names))

new_file_paths <- file_paths %>% str_replace_all("raw_data", "subset_pdfs")

map2(file_paths, new_file_paths, ~pdftools::pdf_subset(..1, pages = 5, output = ..2))

# Remove everything from environment except new_file_paths
gdata::keep("new_file_paths", sure = T)
gc()

# Extract Tables
clean_df_fn <- function(x){
  
  state_name <- x %>% 
    pdf_text() %>% 
    read_delim(delim = "\n", col_names = FALSE) %>% 
    slice(1) %>% 
    str_trim() %>% 
    str_split(",") %>% 
    unlist() %>% 
    last() %>%
    str_split("- | – ") %>% 
    unlist() %>%
    first() %>% 
    str_trim()
  
  df <- x %>% 
    tabulizer::extract_tables() %>%
    as.data.frame() %>%
    as_tibble() %>% 
    na_if("") %>% 
    janitor::remove_empty(which = "cols") %>%
    separate(., X2, into = c("a", "b", "c"), sep = "[^\\S\\r\\n]{2,}", fill = "right") %>%
    janitor::remove_empty(which = "cols") %>%
    add_column(state = state_name) %>%
    setNames(c("indicator", "nfhs5_u", "nfhs5_r", "nfhs5", "nfhs4", "state"))
  
  from <- df %>% 
    pull(indicator) %>%
    str_detect('Children under age 3 years breastfed within') %>% 
    which()
  
  to <- df %>% 
    pull(indicator) %>%
    str_detect('Children under 5 years who are overweight') %>% 
    which()
  
  
  df <- df %>% 
    slice(from:to) %>% 
    mutate_all(list(~na_if(.,""))) %>% 
    janitor::remove_empty(which = "cols") %>% 
    mutate(indicator = stringr::str_sub(indicator, 5))
  df
}

df <- new_file_paths %>%
  map_df(clean_df_fn)

df <- df %>% mutate(indicator_new = case_when(str_detect(indicator, "stunted") ~ "stunting",
                                              str_detect(indicator, "overweight") ~ "overweight",
                                              str_detect(indicator, "underweight") ~"underweight",
                                              str_detect(indicator, "severely wasted") ~"wasting",
                                              str_detect(indicator, '6 months exclusively') ~ 'exclusive_breastfed',
                                              str_detect(indicator, 'under age 3 years breastfed') ~ 'breastfed_within_one_hr'))

df <- df %>% filter(indicator_new %in% c("stunting","overweight","underweight","wasting"))

df <- df %>% dplyr::select(-indicator)

df <- df %>% rename(indicator = indicator_new)

df %>% write_rds(here::here("data","clean_data", paste0("state_level_nfhs5_cleaned_df_", epitrix::clean_labels(Sys.Date()), ".rds")))
