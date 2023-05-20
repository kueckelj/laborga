

library(devtools)
library(tidyverse)
library(magrittr)
library(confuns)
load_all()



df_tissue_donor <- function(){ load_storage_df(1) }
df_tissue_sample <- function(){ load_storage_df(2) }
df_tissue_portion <- function(){ load_storage_df(3) }
df_raw_data <- function(){ load_storage_df(4) }

complete_df_tissue_donor <- function(){ get_storage_df(1) }
complete_df_tissue_sample <- function(){ get_storage_df(2) }
complete_df_tissue_portion <- function(){ get_storage_df(3) }
complete_df_raw_data <- function(){ get_storage_df(4) }

selected_tissue_donor <- function(){ load_storage_df(1)[1, "id_tissue_donor_num" ] %>% as.character()}
selected_tissue_sample <- function(){ load_storage_df(2)[1, "id_tissue_sample_num"] %>% as.character()}
selected_tissue_portion <- function(){ load_storage_df(3)[1, "id_tissue_portion_num"] %>% as.character()}
selected_raw_data <- function(){ load_storage_df(4)[1, "id_raw_data_num"] %>% as.character()}


project_dir <- function(){ return("C:\\Data\\Projects\\New Project") }
folder_organization <- function(){ c("organ", "histo_class", "assay_trademark")}
