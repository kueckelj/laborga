

# tissue donor
input_id_vars <-
  list(
    institution = "University Clinic Freiburg",
    workgroup = "AG Heiland",
    donor_species = "Human",
    donor_tag = as.character(2)
  ) %>% map(as.factor)

input_info_vars <-
  list(
    date_of_birth = as.Date("1980-07-07"),
    sex = "female"
  )

# tissue sample
input_id_vars <-
  list(
    date_of_extraction = as.Date("2022-07-07")
  )

prev_id_merged <- extract_id(id = df$id_tissue_sample_num[[1]], 1)

input_info_vars <-
  list(
    organ = "Cerebrum",
    histo_class = "Glioblastoma",
    grade = "IV",
    mutations = list(c("EGFR", "IDH"))
  )

df_new <-
make_entry(
  df = df,
  prev_id_merged = prev_id_merged,
  input_id_vars = input_id_vars,
  input_info_vars = input_info_vars
  )


edit_entry(
  df = df_new,
  selected_id = "1.1.1.1.19180.1",
  input_info_vars = list(mutations = list(c("EGFR", "CCC")))
) %>% filter(id_tissue_sample_num == "1.1.1.1.19180.1") %>% pull(mutations)


id_part_to_number <- function(x, var){

  var_content <- get_id_content(var)

  if(base::is.factor(var_content)){

    groups <- base::levels(x = var_content)

    if(x %in% groups){

      out <- base::which(x = groups == x)

    } else {



    }

  }

  return(out)

}

get_id_content <- function(var, unique = FALSE){

  out <-
    get_storage_df(d_level = base::length(data_levels)) %>%
    dplyr::pull(!!rlang::sym(var))

  if(base::isTRUE(unique)){

    if(base::is.factor(out)){

      out <- base::levels(out)

    } else {

      out <- base::unique(out)

    }

  }

  return(out)

}

selected_id = "1.3.1.1"
input_info_vars <-
  list(
    ed_date_of_birth = as.Date("1980-07-07"),
    ed_sex = "female"
  )









