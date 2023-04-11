

input_id_vars <-
  list(
    institution = "UKF",
    working_group = "milolab",
    donor_species = "homo_sapiens",
    donor_index = as.integer(123456789)
  )




id_separate <- function(string, d_level = NULL){

  id_split <- stringr::str_split(string = string, pattern = "\\|")[[1]]

  id_named <-
    purrr::set_names(
      x = id_split,
      nm = id_vars_vec[1:base::length(out)]
    )

  if(base::is.character(d_level)){

    id_vars <- get_id_vars(d_level = d_level)

    out <- id_named[id_vars]

  } else {

    out <- id_named

  }

  return(out)

}

id_unite <- function(lst){

  purrr::map_chr(.x = input_id_vars, .f = ~ .x) %>%
    base::unname() %>%
    stringr::str_c(collapse = "|")

}


get_meta_vars("tissue_sample")


if(d_level == "tissue_sample"){

  # check extraction incident
  last_incident <-
    dplyr::filter(df, !!rlang::sym(united_id_var) == {{new_id}}) %>%
    dplyr::pull(extraction_incident) %>%
    base::max()

  if(input_id_vars[["extraction_incident"]] != (last_incident+1)){

    stop(
      glue::glue(
        "Expected extraction incident {last_incident+1}.
          Input is {input_id_vars[['extraction_incident']]}."
      )
    )

  }

}
