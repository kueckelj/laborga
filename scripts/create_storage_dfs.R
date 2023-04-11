


workgroups <-
  list(
    "University Clinic Freiburg" = c("AG Bengsch", rep("AG Heiland", 3), "AG Ravi"),
    "University Clinic Aachen" = c("AG Delev"),
    "University Clinic Hamburg" = c("AG Ricklefs"),
    "Massachussets General Hospital" = c("AG Suva")
  )

n_donors <- c(20, 5, 5, 10)

dlist <- list(
  donor_species = c("Human"),
  organs = list(
    "AG Heiland" = c("Cerebrum", "Cerebellum", "Brainstem", "Pituitary Gland"),
    "AG Ravi" = c("Cerebrum", "Cerebellum", "Brainstem", "Pituitary Gland"),
    "AG Bengsch" = c("Liver"),
    "AG Delev" = c("Cerebrum", "Nerve"),
    "AG Ricklefs" = c("Cerebrum", "Cerebellum", "Brainstem", "Pituitary Gland"),
    "AG Suva" = c("Cerebrum")
    ),
  histo_class = list(
    "Cerebrum" = c("Glioblastoma", "Cortex"),
    "Cerebellum" = c("Cerebellum"),
    "Brainstem" = c("Cavernoma", "Stroke"),
    "Pituitary Gland" = c("Anterior Lobe", "Posterior Lobe", "Both Lobes"),
    "Liver" = c("Hepatocellular Carcinoma"),
    "Nerve" = c("Nerve", "Schwannoma")
    ) %>% map(.f = ~ c(., "Healthy")),
  mutations = list(
    "Cerebrum" = c("BRAF1", "EGFR", "IDH")
  ),
  preparateur = c("JG", "JK", "DHH", "VMR", "KJ"),
  assay_tademarks = c("Visium Spatial Gene Expression", "Visium Spatial Proteomics", "splitSeq", "dropSeq"),
  journals = c("Nature", "Nature Medicine", "Cell", "Cancer Cell", "Neurooncology"),
  authors = c("Ravi", "Kueckelhaus", "Will", "Benotmane", "Joseph"),
  pub_years = c(2015:2022)
)

possible_donor_tags <- as.character(100000000:999999999)

df_tissue_donor <-
  purrr::map_df(
    .x = 1:4,
    .f = function(i){

      tidyr::expand_grid(
        institution = names(workgroups)[i],
        workgroup = sample(x = workgroups[[i]], size = n_donors[i], replace = T)
      )

    }

  ) %>%
  mutate(donor_species = sample(x = dlist$donor_species, size = nrow(.), replace = T)) %>%
  mutate(
    institution = factor(institution, levels = names(workgroups)),
    workgroup = as.factor(workgroup),
    donor_species = as.factor(donor_species)
  )


# df_tissue_donor

df_tissue_donor <-
  group_by_all(df_tissue_donor) %>%
  mutate(
    #donor_tag = sample(possible_donor_tags, size = n(), replace = FALSE) %>% as.factor()
    donor_tag = row_number() %>% as.factor()
    ) %>%
  ungroup()

birthdays <- seq(as.Date('1930/01/01'), as.Date('2020/01/01'), by="day")

df_tissue_donor <-
  mutate(
    .data = df_tissue_donor,
    date_of_birth = sample(birthdays, size = nrow(df_tissue_donor)),
    sex = sample(c("female", "male"), replace = T, size = nrow(df_tissue_donor)),
    organ = "", # actually part of tissue_sample level!
    organ_part = "",
    side = "",
    mutations = map(.x = 1:nrow(df_tissue_donor), .f = function(x){"none"})
  ) %>%
  make_tissue_donor_ids()


for(i in 1:nrow(df_tissue_donor)){

  group <-
    as.matrix(df_tissue_donor[i, "workgroup"]) %>%
    as.character()

  df_tissue_donor[i, "organ"] <- sample(x = dlist$organs[[group]], size = 1)

  organ <-
    as.matrix(df_tissue_donor[i, "organ"]) %>%
    as.character()

  if(organ %in% names(organs_parts)){

    df_tissue_donor[i, "organ_part"] <- sample(x = organs_parts[[organ]], size = 1)

  }

  if(organ %in% organs_pairwise){

    df_tissue_donor[i, "side"] <- sample(x = c("unkown", "left", "right"), size = 1)

  }

  histo_class <- sample(x = dlist$histo_class[[organ]], size = 1)

  df_tissue_donor[i, "histo_class"] <- histo_class

  if(histo_class == "Glioblastoma"){

    mut <- sample(x = dlist$mutations[[organ]], size = sample(0:3, size = 1))

    if(length(mut) >= 1){

      df_tissue_donor[i, "mutations"][[1]][[1]] <- mut
    }

  }

}

# continue to work with df_tissue_donor due to tissue_sample meta data
df_tissue_donor_final <-
  select(df_tissue_donor, any_of(names(data_tables$tissue_donor))) %>%
  select(id_tissue_donor_num, all_of(get_id_vars("tissue_donor")), everything())

# df_tissue_sample

operation_dates <- seq(as.Date('2010/01/01'), as.Date('2020/01/01'), by="day")

df_tissue_sample <-
  purrr::map_df(
    .x = df_tissue_donor$id_tissue_donor_num,
    .f = function(id){

      n_samples <- sample(1:4, size = 1, prob = c(0.2, 0.5, 0.2, 0.1))

      tidyr::expand_grid(
        id_tissue_donor_num = id,
        date_of_extraction = sample(
          x = sample(
            x = sample(operation_dates, size = sample(c(1:3), size = 1), replace = T),
            size = n_samples, replace = T)
          ) %>% base::sort()
      ) %>%
        group_by(id_tissue_donor_num, date_of_extraction) %>%
        mutate(sample_index = 1:n()) %>%
        ungroup()

    }
  ) %>%
  left_join(x = ., y = df_tissue_donor, by = "id_tissue_donor_num") %>%
  arrange(id_tissue_donor_num, date_of_extraction)


df_tissue_sample_final <- make_tissue_sample_ids(df_tissue_sample)


df_tissue_portion_final <-
  map_df(
    .x = df_tissue_sample_final$id_tissue_sample_num,
    .f = function(id){

      n_portions <- sample(1:4, size = 1)
      storage_modes <- sample(storage_modes, size = n_portions, replace = T)
      preparateur_tissue_portion <- sample(dlist$preparateur, size = 1)

      tidyr::expand_grid(
        id_tissue_sample_num = id,
        portion_index = 1:n_portions
      ) %>%
        mutate(storage_mode = {{storage_modes}}, preparateur_tissue_portion = {{preparateur_tissue_portion}})

    }
  ) %>%
  left_join(x = ., y = df_tissue_sample_final, by = "id_tissue_sample_num") %>%
  mutate(date_of_creation = date_of_extraction) %>%
  make_tissue_portion_ids()

# raw_data_df_final
example_portions <-
  filter(df_tissue_portion_final, storage_mode %in% c("frozen", "paraffin")) %>%
  pull(id_tissue_portion_num) %>% sample(x = ., size = 40)

df_raw_data_final <-
  purrr::map_df(
    .x = example_portions,
    .f = function(ep){

      na <- sample(1:3, size = 1)

      assays <- sample(dlist$assay_tademarks, size = na, replace = T)

      ts_id <- extract_id(ep, "tissue_sample")

      date_of_creation <-
        filter(df_tissue_sample_final, id_tissue_sample_num == {{ts_id}}) %>%
        pull(date_of_extraction)


      tidyr::expand_grid(
        id_tissue_portion_num = ep,
        assay_trademark = assays
      ) %>%
        mutate(
          raw_data_index = row_number(),
          date_of_creation = {{date_of_creation}}
          )

    }
  ) %>%
  make_raw_data_ids() %>%
  mutate(
    pub_citation = "",
    preparateur_raw_data = sample(x = dlist$preparateur, size = nrow(.), replace = T),
    pub_ref = str_c(sample(dlist$authors, size = nrow(.), replace = T), " et al. ", sample(dlist$pub_years, size = nrow(.), replace = T)),
    pub_year = str_extract(pub_ref, pattern = "[0-9]*$") %>% as.numeric()
    ) %>%
  group_by(pub_ref, pub_year) %>%
  mutate(
    pub_journal = sample(x = dlist$journals, size = 1)
  ) %>%
  ungroup()


list(
  tissue_donor = df_tissue_donor_final,
  tissue_sample = df_tissue_sample_final,
  tissue_portion = df_tissue_portion_final,
  raw_data = df_raw_data_final
) %>% iwalk(.f = function(x,y){

  dt <- data_tables[[y]]

  for(nm in names(dt)){

    if(!nm %in% names(x)){

      x[[nm]] <- vector(mode = class(dt[[nm]]), length = nrow(x))

    }

  }

  shrink_storage_df(df = x) %>%
    saveRDS(file = str_c("data_private/current_", y, "_storage_df.RDS"))

})


