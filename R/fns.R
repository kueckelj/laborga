


# a -----------------------------------------------------------------------




add_id_columns <- function(df, d_level = NULL, sort = TRUE){

  input_d_level <- get_d_level(df)

  id_col_df <- stringr::str_c("id_", input_d_level, "_num")

  if(base::is.null(d_level)){

    d_levels <- 1:dln(input_d_level)

  } else {

    d_levels <- dln(d_level)

  }

  for(dl in d_levels){

    id_col_to_add <- stringr::str_c("id_", dlc(dl), "_num")

    if(id_col_df != id_col_to_add & !(id_col_to_add %in% base::colnames(df))){

      df <-
        dplyr::mutate(
          .data = df,
          {{id_col_to_add}} := extract_id(id = df[[id_col_df]], d_level_out = dlc(dl))
        )

    }

  }

  if(base::isTRUE(sort)){

    df <- sort_storage_df(df)

  }

  return(df)

}

# deprecated
add_id_column <- add_id_columns

add_sample <- function(df,
                       institution,
                       working_group,
                       donor_species,
                       donor_index,
                       extraction_date){

  # number of existing samples per operation
  nes <-
    dplyr::filter(
      .data = df,
      institution == {{institution}},
      woring_group == {{working_group}},
      donor_species == {{donor_species}},
      donor_index == {{donor_index}},
      extraction_date == {{extraction_date}}
    ) %>%
    base::nrow()

  sample_index <- nes+1

  dplyr::add_row(
    .data = df,
    institution = {{institution}},
    woring_group = {{working_group}},
    donor_species = {{donor_species}},
    donor_index = {{donor_index}},
    extraction_date = {{extraction_date}}
  )

}



#' @title Track repository manipulation
#'
#' @description Tracks repository manipulation by adding it to the logfile.
#'
#' @param username The name of the user currently logged in.
#' @param data_level The current data level.
#' @param entry_id The ID of the entry that is manipulated.
#' @param fn_name The name of the function with which the repository was manipulated.
#' @param fn_input The input of the function with which the repository was manipulated.
#'
#' @return Invisible `TRUE`if everything went fine.
#' @export
#'
add_to_logfile <- function(username, data_level, entry_id, fn_name, fn_input){

  lf_df <- load_logfile_df()

  n <- base::nrow(lf_df)

  df_new <-
    tibble::tibble(
      username = username,
      sys_time = base::Sys.time(),
      data_level = data_level,
      entry_id = entry_id,
      fn_name = fn_name,
      fn_input = list(fn_input)
    )

  lf_df_new <- base::rbind(lf_df, df_new)

  save_logfile_df(lf_df_new)

  base::invisible(TRUE)

}


# c -----------------------------------------------------------------------


check_author <- function(input, error = FALSE, in_shiny = FALSE){

  check_string(
    input = input,
    pattern = stringr::str_c("^", rgx$author),
    ref = "author",
    error = error,
    in_shiny = in_shiny
    )

}


check_input_id_vars <- function(input_id_vars, d_level, in_shiny = FALSE){

  if(d_level == "tissue_donor"){

    purrr::imap(
      .x = input_id_vars,
      .f = function(x, var_name){

        var_label <- data_variables[[var_name]][["label"]]

        if(x == ""){

          confuns::give_feedback(
            msg = glue::glue("Input for '{var_label}' is missing."),
            fdb.fn = "stop",
            in.shiny = in_shiny,
            with.time = FALSE
          )

        }

      }
    )

  } else if(d_level == "tissue_sample"){

    if(input_id_vars$date_of_extraction == "1900-01-01"){

      confuns::give_feedback(
        msg = "I doubt that 1900-01-01 was the date of extraction...",
        fdb.fn = "stop",
        in.shiny = in_shiny,
        with.time = FALSE
      )

    }

  }

}

check_input_info_vars <- function(input_info_vars, d_level, in_shiny = FALSE){

  if(d_level == "tissue_donor"){

    if(input_info_vars$date_of_birth == "1900-01-01"){

      confuns::give_feedback(
        msg = "The oldest person, as of May 19, 2023, is Marya Branyas and she was born in the year 1907. There must be an error here...",
        fdb.fn = "stop",
        in.shiny = in_shiny,
        with.time = FALSE
      )

    }

    if(input_info_vars$sex == ""){

      confuns::give_feedback(
        msg = "Input for 'Sex' is missing.",
        fdb.fn = "stop",
        in.shiny = in_shiny,
        with.time = FALSE
      )

    }

  } else if(d_level == "tissue_sample"){

    if(input_info_vars$organ == ""){

      confuns::give_feedback(
        msg = "Input for 'Organ' is missing.",
        fdb.fn = "stop",
        in.shiny = in_shiny,
        with.time = FALSE
      )

    }

    if(input_info_vars$organ %in% base::names(organs_parts)){

      if(input_info_vars$organ_part == ""){

        confuns::give_feedback(
          msg = "Input for 'Organ Part' is missing.",
          fdb.fn = "stop",
          in.shiny = in_shiny,
          with.time = FALSE
        )

      }

    }

    if(input_info_vars$organ %in% organs_side_needed){

      if(input_info_vars$side == ""){

        confuns::give_feedback(
          msg = "Input for 'Side' is missing.",
          fdb.fn = "stop",
          in.shiny = in_shiny,
          with.time = FALSE
        )

      }

    }

    if(input_info_vars$workgroup == ""){

      confuns::give_feedback(
        msg = "Input for 'Workgroup' is missing.",
        fdb.fn = "stop",
        in.shiny = in_shiny,
        with.time = FALSE
      )

    }

  } else if(d_level == "tissue_portion"){

    # is checked prior to the call to make_entry()

  } else if(d_level == "raw_data"){

    if(input_info_vars$assay_trademark == ""){

      confuns::give_feedback(
        msg = "Input for 'Assay Trademark' is missing.",
        fdb.fn = "stop",
        in.shiny = in_shiny,
        with.time = FALSE
      )

    }

    if(input_info_vars$link_raw_data == ""){

      confuns::give_feedback(
        msg = "Input for 'Link to Data' is missing.",
        fdb.fn = "stop",
        in.shiny = in_shiny,
        with.time = FALSE
      )

    }

    if(!input_info_vars$pub_ref %in% c("", "unknown")){

      valid <- check_pub_ref(input = input_info_vars$pub_ref, error = FALSE)

      if(!valid){

        confuns::give_feedback(
          msg = "Input for 'Publication Reference' is invalid.",
          fdb.fn = "stop",
          in.shiny = in_shiny,
          with.time = FALSE
        )

      }

    }

  }

}

check_input_req_vars_tissue_portion <- function(input_tissue_portion, in_shiny = FALSE){

  purrr::imap(
    .x = input_tissue_portion,
    .f = function(input_tp, index){

      if(input_tp$info$storage_mode == ""){

        confuns::give_feedback(
          msg = glue::glue("Input for 'Storage Mode' of tissue portion {index} is missing."),
          fdb.fn = "stop",
          with.time = FALSE,
          in.shiny = in_shiny
        )

      }

      if(input_tp$info$date_of_creation == "1900-01-01"){

        confuns::give_feedback(
          msg = glue::glue("I doubt that the 'Date of Creation' for tissue portion {index} is correct."),
          fdb.fn = "stop",
          with.time = FALSE,
          in.shiny = in_shiny
        )

      }

    }
  )

}

check_password <- function(password){

  stringr::str_split(password, pattern = "") %>%
    base::unlist() %>%
    purrr::map_lgl(.f = ~stringr::str_detect(.x, pattern = "[A-Z]|[a-z]|[0-9]|\\!|\\?|\\_|\\-")) %>%
    base::all()

}

check_pub_ref <- function(input, error = FALSE, in_shiny = FALSE){

  check_string(
    input = input,
    pattern = rgx_pub_ref_final,
    ref = "pub reference",
    error = error,
    in_shiny = in_shiny
  )

}

check_string <- function(input, pattern, ref, error = FALSE, in_shiny = in_shiny){

  valid <- stringr::str_detect(string = input, pattern = pattern)

  if(base::isTRUE(error) & base::any(!valid)){

    pos <-
      base::which(!valid) %>%
      confuns::scollapse()

    confuns::give_feedback(
      msg = glue::glue("Invalid {ref} input at '{pos}'."),
      fdb.fn = "stop",
      with.time = FALSE,
      in.shiny = in_shiny
      )

  }

  return(valid)

}

check_username <- function(username){

  stringr::str_split(username, pattern = "") %>%
    base::unlist() %>%
    purrr::map_lgl(.f = ~stringr::str_detect(.x, pattern = "[A-Z]|[a-z]| {1}")) %>%
    base::all()

}

create_dialoge <- function(situation,
                           question,
                           in.shiny = FALSE){

  if(base::isTRUE(in.shiny)){



  } else {

    out <-
      base::readline(prompt = stringr::str_c(situation, question, "(yes/no)", sep = " ")) %>%
      base::tolower()

  }

  return(out)

}


#' @title Shiny feedback messages
#'
#' @description Wrapper around \code{shiny::req()} and \code{shiny::showNotification()}.
#' Prevents application from crashing and displays guiding message about what the user
#' is supposed to do in order to continue without this message to appear.
#'
#' @param evaluate A vector of logical tests to be evaluated.
#' @param case_false A character string indicating the message to be displayed if one element of
#' \code{evaluate} turns out to be FALSE. Needs to be in \code{base::names(\code{error/warning_notifiations})}.
#' @param error_notifications A named list of character strings.
#' @param warning_notifications A named list of character strings.
#' @param duration The duration the message is displayed.
#' @param stop_process,stop_app Logical. What is supposed to happen if one element of \code{evaluate}
#' turns out to be FALSE.
#'
#' @return A shiny notification.
#'
checkpoint <- function(evaluate = TRUE,
                       case_false = NULL,
                       error_notifications = list(

                         invalid_password = "The password must only contain letters, numbers, !, ?, _ or -.",

                         invalid_project_dir = "The storage directory is invalid.",
                         invalid_username = "Username must not contain anything else than letters and empty space.",

                         no_entry_selected = "Please select a row in the table.",
                         no_project_ids = "Please add at least one data entry.",
                         non_matching_passwords = "Passwords must be identical.",

                         username_taken = "Username is already taken.",
                         username_unknown = "Username is unknown.",

                         zero_tissue_portions = "Number of tissue portions to add must not be 0."

                       ),
                       warning_notifications = list(),
                       duration = 4,
                       stop_process = TRUE,
                       stop_app = FALSE){

  ##-- check if truthy for all elements
  results <- shiny::isTruthy(evaluate)

  if(any(results == F)){##-- at least one of the elements is not truthy

    if(!is.null(case_false) & case_false %in% names(warning_notifications)){

      ##-- show notification
      shiny::showNotification(ui = warning_notifications[[case_false]], duration = duration, closeButton = T, type = "warning")

    } else if(!is.null(case_false) & case_false %in% names(error_notifications)){

      ##-- show notification
      shiny::showNotification(ui = error_notifications[[case_false]], duration = duration, closeButton = T, type = "error")

      ##-- stop computation and or stop app?
      if(isFALSE(stop_app) & isTRUE(stop_process)){

        shiny::req(evaluate)

      } else if(isTRUE(stop_app)) {

        shiny::stopApp()

      }

    }

  }

}


combine_path <- function(input){

  if(!base::is.list(input)){

    out <- NULL

  } else {

    # add if else for mac vs. windows

    path <-
      input$path %>%
      purrr::flatten_chr() %>%
      stringr::str_c(collapse = "\\")

    root <-
      stringr::str_remove_all(input$root, pattern = "\\(|\\)")

    out <- stringr::str_c(root, path)

  }

  return(out)

}


complete_storage_df <- function(df, df_list = NULL, rn = FALSE){

  d_level <- get_d_level(df)

  if(d_level != "tissue_donor" & base::nrow(df) >= 1){ # tissue donor is always complete

    if(!is_complete(df)){

      if(dln(d_level) != 1){ # d_level == 1 is always "complete"

        d_levels <- data_levels[1:(dln(d_level)-1)]

        for(dl in d_levels){

          if(base::is.list(df_list)){

            df_merge <- df_list[[dl]]

          } else {

            df_merge <- NULL

          }

          df <- merge_storage_df(df, d_level = dl, df_merge = df_merge)

        }

      }

    } else {

      warning(glue::glue("Storage data.frame level '{d_level}' is already complete."))

    }

  }

  # sort columns
  out <- dplyr::select(df, dplyr::any_of(data_vars))

  if(base::isTRUE(rn)){

    out <- base::as.data.frame(out)

    base::rownames(out) <- out[[id_vars_merged[dln(d_level)]]]

  }

  return(out)

}


create_directory <- function(dir, ask = TRUE, in_shiny = FALSE){

  if(base::dir.exists(dir)){

    if(base::isTRUE(ask)){

      overwrite <-
        create_dialoge(
          situation = glue::glue("The directory '{dir}' already exists."),
          question = "Do you want to overwrite it's content?",
          in.shiny = in_shiny
        )

      if(overwrite == "yes"){

        base::unlink(x = dir, recursive = TRUE, force = TRUE)
        base::dir.create(dir, recursive = recursive)

      } else {

        base::cat("Aborted. Did not create the directory.\n")

        return(base::invisible(FALSE))

      }

    } else {

      base::cat("Directory already exists.\n")

      return(base::invisible(FALSE))

    }

  } else {

    base::dir.create(dir, recursive = recursive)

  }

  base::invisible(TRUE)

}

create_empty_df <- function(name_class_list){

  empty_df <- tibble::tibble()

  for(i in base::seq_along(name_class_list)) {

    col_name <- base::names(name_class_list)[i]
    col_class <- name_class_list[[col_name]]

    if(stringr::str_detect(col_class, pattern = "^list")){

      empty_df[[col_name]] <- base::vector(mode = "list")

    } else if(col_class == "date"){

      empty_df[[col_name]] <- lubridate::ymd()

    } else if(col_class == "factor") {

      empty_df[[col_name]] <- base::factor(levels = base::character(0))

    } else {

      empty_df[[col_name]] <- base::vector(mode = col_class)

    }

  }

  return(empty_df)

}

create_empty_logfile_df <- function(){

  tibble::tibble(
    username = base::character(), # the user currently logged in
    sys_time = base::Sys.time(), # time of the action
    data_level = base::character(), # the data level of the manipulated table
    entry_id = base::character(), # the ID of the entry
    fn_name = base::character(), # the underlying function used to manipulate the repository
    fn_input = base::list() # the argument input used to manipulate the repository
  )

}

create_empty_storage_df <- function(d_level){

  d_level <- dlc(d_level)

  df <-
    purrr::keep(
      .x = data_variables,
      .p = ~ .x$d_level == d_level
    ) %>%
    purrr::map(
      .f = ~ .x$class
    ) %>%
    create_empty_df()

  df[[id_vars_merged[dln(d_level)]]] <- base::character(0)

  df <- sort_storage_df(df)

  return(df)


}

create_empty_users_df <- function(){

  tibble::tibble(
    username = base::character(),
    password = base::character(),
    permissions = base::character(),
    created = lubridate::ymd()
  )

}




create_id_vars <- function(df, d_level){

  id_vars <- get_id_vars(d_level, level_spec = FALSE)

  new_id_var_names <-
    c(
      stringr::str_c("id_", d_level),
      stringr::str_c("id_", d_level, "num")
    )

  df <- dplyr::select(df, -dplyr::any_of(new_id_var_names))



}




# d -----------------------------------------------------------------------

delete_entries <- function(id, username, in_shiny = FALSE, verbose = TRUE){

  entry_level <-
    identify_d_level(id) %>%
    dln()

  entry_level_id_var <- id_vars_merged[[entry_level]]

  info <- list()

  for(i in entry_level:4){

    df <- get_storage_df(d_level = i, complete = TRUE)

    # collect information for logfile
    d_level <- dlc(i)

    id_var <- id_vars_merged[i]

    removed_ids <-
      dplyr::filter(df, (!!rlang::sym(entry_level_id_var) == {{id}})) %>%
      dplyr::pull(var = id_var)

    info[[d_level]] <- list(d_level = d_level, removed_id = removed_ids)

    # remove from repository
    filtered_df <- dplyr::filter(df, !(!!rlang::sym(entry_level_id_var) == {{id}}))

    n <- (base::nrow(df)-base::nrow(filtered_df))

    ref <- base::ifelse(n == 1, yes = "entry", no = "entries")

    saved <- save_storage_df(filtered_df)

    if(base::isTRUE(saved)){

      confuns::give_feedback(
        msg = glue::glue("Deleted {n} {dlc(i)} {ref}."),
        with.time = FALSE,
        in.shiny = in_shiny,
        verbose = verbose
        )

    }

  }

  add_to_logfile(
    username = username,
    data_level = dlc(entry_level),
    entry_id = id,
    fn_name = "delete_entries",
    fn_input = info
  )

  base::invisible(TRUE)

}

discard_id_vars <- function(df, keep = NULL){

  if(base::is.character(keep)){

    id_vars_to_remove <- id_vars_vec[!id_vars_vec %in% keep]

  } else {

    id_vars_to_remove <- id_vars_vec

  }

  dplyr::select(df, -dplyr::any_of(id_vars_to_remove))

}


# data level to character
dlc <- function(d_level){

  if(base::is.numeric(d_level)){

    data_levels[d_level]

  } else if(base::is.character(d_level)) {

    d_level

  }



}

# data level to numeric
dln <- function(d_level){

  if(base::is.character(d_level)){

    base::which(data_levels == d_level)

  } else if(base::is.numeric(d_level)){

    d_level

  }

}

# data level to pretty
dlp <- function(d_level, cap = FALSE){

  d_level <- dlc(d_level)

  if(base::isTRUE(cap)){

    out <- confuns::make_pretty_name(d_level)

  } else {

    out <- stringr::str_replace_all(d_level, pattern = "_", replacement = " ")

  }

  return(out)

}


# requires prepare_df_for_download() to process raw data df
# requires prepare_folder_for_download() to have prepared the necessary folders
download_zip_files <- function(df_prep,
                               project_dir = str_c(getwd(), "/data_private/myProject"),
                               folder_organization = NULL,
                               verbose = TRUE,
                               in_shiny = FALSE,
                               duration = 300
){

  # download files
  df <- df_prep
  n <- base::nrow(df)

  for(i in 1:n){

    download_url <- base::as.character(df[i, "link_raw_data"])
    dir_zip <- base::as.character(df[i, "dir_zip"])

    confuns::give_feedback(
      msg = glue::glue("Started download ({i}/{n}): {download_url} to {dir_zip}."),
      verbose = verbose,
      in.shiny = in_shiny,
      duration = duration
    )

    # set to FALSE for debugging
    if(TRUE){

      base::tryCatch({

        utils::download.file(url = download_url, destfile = dir_zip, mode = "wb")

      })

    }

    confuns::give_feedback(
      msg = glue::glue("Finished download({i}/{n})."),
      verbose = verbose,
      in.shiny = in_shiny,
      duration = duration
    )

  }



  return(list("information about downloads"))

}





# e -----------------------------------------------------------------------


# exchange values in table if provided via ...

edit_entry <- function(df,
                       selected_id,
                       input_info_vars,
                       verbose = TRUE,
                       save = FALSE,
                       in_shiny = FALSE,
                       username = character(0)){

  d_level <- get_d_level(df)

  id_var_merged <- id_vars_merged[dln(d_level)]

  # obtain table specific info variables
  info_vars <- get_info_vars(d_level)

  # keep only table specific input
  input <-
    confuns::keep_named(input_info_vars) %>%
    confuns::lrename_with(lst = ., .f = ~ stringr::str_remove(.x, pattern = "^ed_")) %>%
    confuns::lselect(lst = ., dplyr::any_of(info_vars))

  vars_to_edit <- base::names(input)

  if(base::length(vars_to_edit) >= 1){

    # iterate over all variables to edit
    for(v in vars_to_edit){

      if(base::is.list(df[[v]])){

        df[df[[id_var_merged]] == selected_id, v][[1]] <- input[[v]]

      } else {

        df[df[[id_var_merged]] == selected_id, v] <- input[[v]]

      }

    }

  }

  confuns::give_feedback(
    msg = glue::glue("Edited {dlp(d_level)} '{selected_id}'."),
    verbose = verbose,
    in.shiny = in_shiny
  )

  if(base::isTRUE(save)){

    save_storage_df(df = df)

    add_to_logfile(
      username = username,
      data_level = d_level,
      entry_id = selected_id,
      fn_name = "edit_entry",
      fn_input = list(input_info_vars = input_info_vars)
    )

  }

  return(df)

}

ensure_data_var_validity <- function(x, var){

  if(data_variables[[var]][["class"]] == "character"){

    out <- base::as.character(x)

  } else if (data_variables[[var]][["class"]] == "date"){

    out <- base::as.Date(x)

  } else if(data_variables[[var]][["class"]] == "factor"){

    out <- base::as.factor(x)

  } else if(data_variables[[var]][["class"]] == "numeric"){

    out <- base::as.numeric(x)

  } else if(data_variables[[var]][["class"]] == "list_character"){

    out <- list(x)

    if(purrr::is_empty(out[[1]])){ out[[1]] <- "none" }

  }

  return(out)

}


extract_author_first <- function(pub_ref){

  if("&" %in% pub_ref){

    stringr::str_extract(pub_ref, pattern = stringr::str_c(rgx$author, " & ", rgx$author))

  } else {

    stringr::str_extract(pub_ref, pattern = rgx$author)

  }

}

extract_authors_all <- function(pub_ref){

  stringr::str_extract_all(pub_ref, pattern = rgx$author)

}

extract_id <- function(id, d_level_out){

  if(base::length(id) == 0){

    out <- base::character(0L)

  } else {

    input_level <- identify_d_level(id)

    if(dln(input_level) < dln(d_level_out)){

      stop(
        glue::glue(
          "Data level {input_level} is lower than data level {d_level_out}.",
          " Can not extract {d_level_out}-ID from {input_level}-ID."
        )
      )

    }

    n_seps <- id_sep_count[d_level_out]

    pattern <-
      stringr::str_c(base::rep("[0-9]*", n_seps+1), collapse = id_sep_regex) %>%
      stringr::str_c("^", .)

    out <- stringr::str_extract(string = id, pattern = pattern)

  }

  return(out)

}

extract_year <- function(pub_ref){

  stringr::str_extract(pub_ref, pattern = rgx$year)

}

# f -----------------------------------------------------------------------

# input must be the output of reactiveValuesToList()
filter_storage_table <- function(df, input, suff = NULL){

  d_level <- get_d_level(df)

  flist <-
    list(
      opts = get_filter_options(input = input, d_level = d_level, suff = suff),
      crit = get_filter_criteria(input = input, d_level = d_level, suff = suff)
    )

  if(base::all(purrr::map_lgl(.x = flist, .f = purrr::is_empty))){

    out <- df

  } else {

    pattern <-
      stringr::str_c(c("^filter_opt_", "^filter_crit_"), d_level, "_") %>%
      stringr::str_c(collapse = "|")

    if(base::is.character(suff)){

      pattern <- stringr::str_c(pattern, stringr::str_c("_", suff, "$"), sep = "|")

    }

    f_opts <-
      confuns::lrename_with(flist$opts, .f = ~ stringr::str_remove_all(.x, pattern = pattern))

    f_crit <-
      confuns::lrename_with(flist$crit, .f = ~ stringr::str_remove_all(.x, pattern = pattern))

    keep <-
      purrr::discard(.x = f_opts, .p = ~ .x == "Ignore") %>%
      base::names()

    f_opts <- f_opts[keep]

    f_crit <- f_crit[keep]

    if(base::length(f_opts) != 0){

      for(var in base::names(f_opts)){

        if(data_variables[[var]][["class"]] %in% c("character", "factor", "list_character")){

          criteria <- base::as.character(f_crit[[var]])

          if(f_opts[[var]] == "Keep"){

            df <- dplyr::filter(df, !!rlang::sym(var) %in% {{criteria}})

          } else if(f_opts[[var]] == "Discard"){

            df <- dplyr::filter(df, !(!!rlang::sym(var) %in% {{criteria}}))

          }

        } else if(data_variables[[var]][["class"]] %in% c("date", "numeric")){

          if(f_opts[[var]] == "Keep"){

            df <-
              dplyr::filter(
                .data = df,
                dplyr::between(
                  x = !!rlang::sym(var),
                  left = base::min(f_crit[[var]]),
                  right = base::max(f_crit[[var]])
                )
              )

          } else if(f_opts[[var]] == "Discard"){

            df <-
              dplyr::filter(
                .data = df,
                !dplyr::between(
                  x = !!rlang::sym(var),
                  left = base::min(f_crit[[var]]),
                  right = base::max(f_crit[[var]])
                )
              )

          }

        }

      }

    }

    out <- df

  }


  return(out)

}

# returns a list of argument input of the function within fn_input_as_list() is called
fn_input_as_list <- function(){

  ce <- rlang::caller_env()

  cl_fn <- rlang::caller_fn()

  cl_frame <- base::sys.parent()

  cl_call <- base::sys.call(which = cl_frame)

  cl_fn_name <- base::as.character(cl_call)[1]

  cl_args <-
    rlang::fn_fmls_names(fn = cl_fn)

  cl_args <- cl_args[cl_args != "..."]

  out <-
    purrr::map(
      .x = cl_args,
      .f = function(arg){

        base::parse(text = arg) %>%
          base::eval(envir = ce)

      }
    ) %>%
    purrr::set_names(nm = cl_args)

  return(out)

}

# g -----------------------------------------------------------------------

get_choices <- function(x){

  if(base::is.character(x)){

    out <- base::unique(x)

  } else if(base::is.factor(x)) {

    out <- base::levels(x)

  } else {

    out <- NULL

  }

  return(out)

}


#' @title Obtain data level
#' @export
get_d_level <- function(x, ...){

  UseMethod("get_d_level", x)

}

#' @rdname get_d_level
#' @export
get_d_level.data.frame <- function(x){

  cnames <- base::colnames(x)

  for(d_level in base::names(base::rev(id_vars_list))){

    all_in <- base::all(id_vars_list[[d_level]] %in% cnames)

    if(base::isTRUE(all_in)){

      break()

    }

  }

  return(d_level)

}

#' @rdname get_d_level
#' @export
get_d_level.character <- function(x){

  if(x %in% id_vars_vec){

    out <-
      purrr::keep(.x = id_vars_list, .p = ~ (x %in% .x)) %>%
      base::names()

  } else {

    out <-
      purrr::keep(.x = info_vars_list, .p = ~ (x %in% .x)) %>%
      base::names()

  }

  if(base::is.null(out)){ stop("Unknown variable.") }

  return(out)

}



get_filter_criteria <- function(input, d_level, suff = NULL){

  match <- stringr::str_c("filter_crit_", d_level)

  if(base::is.character(suff)){

    out <-
      confuns::lselect(
        lst = input,
        dplyr::starts_with(match) & dplyr::ends_with(suff)
      )

  } else {

    out <-
      confuns::lselect(lst = input, dplyr::starts_with(match))

  }

  if(base::length(out) == 0){

    out <- list()

  } else {

    out <- confuns::lselect(lst = out, -dplyr::ends_with("open"))

  }

  return(out)



}


get_filter_options <- function(input, d_level, suff = NULL){

  match <- stringr::str_c("filter_opt_", d_level)

  if(base::is.character(suff)){

    out <-
      confuns::lselect(
        lst = input,
        dplyr::starts_with(match) & dplyr::ends_with(suff)
      )

  } else {

    out <-
      confuns::lselect(lst = input, dplyr::starts_with(match))

  }

  if(base::length(out) == 0){

    out <- base::list()

  } else {

    out <- confuns::lselect(lst = out, -dplyr::ends_with("open"))

  }

  return(out)

}


get_id_vars <- function(d_level, level_spec = FALSE, flatten = TRUE){

  if(base::is.numeric(d_level)){  d_level <- data_levels[d_level] }

  confuns::check_one_of(
    input = d_level,
    against = data_levels
  )

  sub <- base::which(x = data_levels == d_level)

  if(base::isFALSE(level_spec)){ sub <- 1:sub}

  if(base::isTRUE(flatten)){

    out <- purrr::flatten_chr(id_vars_list[sub])

  } else {

    out <- id_vars_list[1:sub]

  }

  return(out)

}



get_info_vars <- function(d_level, level_spec = TRUE, flatten = TRUE){

  d_level <- dlc(d_level)

  confuns::check_one_of(
    input = d_level,
    against = data_levels
  )

  sub <- base::which(x = data_levels == d_level)

  if(base::isFALSE(level_spec)){ sub <- 1:sub }

  if(base::isTRUE(flatten)){

    out <- purrr::flatten_chr(info_vars_list[sub])

  } else {

    out <- info_vars_list[sub]

  }

  return(out)

}



get_storage_df <- function(d_level, complete = TRUE){

  df <- load_storage_df(d_level)

  if(base::isTRUE(complete) && dln(d_level) != 1){

    d_levels <-
      1:(dln(d_level)-1)

    for(dl in d_levels){

      df <- merge_storage_df(df, d_level = dl)

    }

    # ensures that all sub id columns exist
    df <- add_id_columns(df, sort = TRUE)

  }

  # sort columns
  dplyr::select(df, dplyr::any_of(data_vars))

}


get_storage_df_empty <- function(d_level, complete = TRUE, sort = TRUE){

  sub <- dln(d_level)

  if(base::isTRUE(complete)){ sub <- 1:sub }

  out <- purrr::map_dfc(.x = data_tables[sub], .f = ~ .x)

  if(base::isTRUE(sort)){

    out <- sort_storage_df(out)

  }

  return(out)

}

get_vars <- function(d_level = laborga::data_levels,
                     type = laborga::data_var_types){

  purrr::keep(data_variables, .p = ~ .x$d_level %in% d_level) %>%
    purrr::keep(.p = ~.x$type %in% type) %>%
    base::names()

}




# h -----------------------------------------------------------------------

hlpr_order_input <- function(order_input){

  order <- NULL

  if(base::is.data.frame(order_input)){

    order <- order_input$text

  } else if(base::is.character(order_input)){

    order <- order_input

  }

  return(order)

}



# i -----------------------------------------------------------------------



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

  purrr::map(.x = lst, .f = ~ base::as.character(.x)) %>%
    purrr::flatten_chr() %>%
    base::unname() %>%
    stringr::str_c(collapse = "|")

}

identify_d_level <- function(id){

  n <- stringr::str_count(string = id[1], pattern = id_sep_regex)

  base::which(x = id_sep_count == n) %>%
    base::names()

}

insert_entry_index <- function(df, groups){

  d_level <- get_d_level(df)

  if(dln(d_level) != 1){

    prev_id_var_merged <- id_vars_merged[dln(d_level)-1]

    index_var <-
      stringr::str_c(
        stringr::str_remove(d_level, pattern = "^tissue_"),
        "_index"
      )

    df <-
      dplyr::group_by(df, dplyr::across(.cols = dplyr::all_of(groups))) %>%
      dplyr::mutate(
        {{index_var}} := insert_entry_index_helper(!!rlang::sym(index_var))
      ) %>%
      dplyr::ungroup()

  }

  return(df)

}

insert_entry_index_helper <- function(indices){

  n <- base::length(indices)

  indices[base::is.na(indices)] <- n

  return(indices)

}

is_complete <- function(df){

  d_level <- get_d_level(df)

  vars_if_complete <- get_info_vars(d_level = d_level, level_spec = FALSE)

  out <- base::all(vars_if_complete %in% base::colnames(df))

  return(out)

}





# l -----------------------------------------------------------------------

launchLabOrga <- function(){

  if(!is_configured()){

    configure_lab_orga()

  }

  shiny::runApp(
    shiny::shinyApp(
      ui = launchLabOrgaUI(),
      server = launchLabOrgaServer
    )
  )

}


# m -----------------------------------------------------------------------




make_entry <- function(df,
                       input_id_vars,
                       input_info_vars,
                       prev_id_merged = NULL,
                       save = FALSE,
                       df_list = NULL,
                       verbose = TRUE,
                       username = character(1),
                       in_shiny = FALSE,
                       ...){

  d_level <- get_d_level(df)
  id_var_merged <- id_vars_merged[dln(d_level)]
  prev_id_var_merged <- id_vars_merged[dln(d_level)-1]

  check_input_id_vars(
    input_id_vars = input_id_vars,
    d_level = d_level,
    in_shiny = in_shiny
  )

  check_input_info_vars(
    input_info_vars = input_info_vars,
    d_level = d_level,
    in_shiny = in_shiny
  )

  # ----- 1. input check

  # make sure that entries for data levels > 1 contain the prev ID
  # e.g. if a tissue sample is added the ID of the tissue donor must be provided
  if(dln(d_level) > 1){

    confuns::is_value(x = prev_id_merged, mode = "character")

    df <- add_id_column(df = df, d_level = dln(d_level)-1)

    input_id_vars <-
      c(input_id_vars, purrr::set_names(x = prev_id_merged, nm = prev_id_var_merged))

  }

  # ----- 2. make a new entry in the storage data.frame

  old_ids <- base::unique(df[[id_var_merged]])

  # make a new entry and
  df_new <-
    # add input_id_vars using add_row as factor levels are automatically integrated
    dplyr::add_row(.data = df, !!!input_id_vars, !!!input_info_vars) %>%
    # in case of data level > 1 insert index
    # e.g. tissue_sample needs variable sample_index
    insert_entry_index(df = ., groups = base::names(input_id_vars)) %>%
    # update ID variable
    make_id_var(df = ., shrink = FALSE) # input is already shrunk

  # if data level > 1 an index is required
  double_id <-
    dplyr::group_by(df_new, !!rlang::sym(id_var_merged)) %>%
    dplyr::tally() %>%
    dplyr::filter(n == 2) %>%
    dplyr::pull(!!rlang::sym(id_var_merged))

  # check id
  if(base::length(double_id) > 0){

    d_level <- stringr::str_replace(d_level, pattern = "_", replacement = " ")

    msg <- glue::glue("ID {double_id} is already present in {d_level} table.")

    confuns::give_feedback(msg = msg, with.time = FALSE, fdb.fn = "stop", in.shiny = in_shiny, ...)

  } else {

    new_id <- base::as.character(df_new[base::nrow(df_new), id_var_merged])

    msg <-
      glue::glue(
        "Added {ref_entry} entry with ID '{new_id}'.",
        ref_entry = dlp(d_level)
      )

    confuns::give_feedback(
      msg = msg,
      verbose = verbose,
      in.shiny = in_shiny,
      with.time = FALSE, ...
      )

  }

  # add content of variables that are computed
  vars_to_compute <- computed_vars_list[[d_level]]

  if(base::length(vars_to_compute) >= 1){

    # temporarily complete the data.frame
    if(!is_complete(df_new)){

      df_new <- complete_storage_df(df = df_new, df_list = df_list)

    }

    for(vtc in vars_to_compute){

      df_new[[vtc]] <- data_variables[[vtc]][["compute_with"]](df = df_new)

    }

    # ensure shrunk output
    df_new <- shrink_storage_df(df_new)

  }

  if(base::isTRUE(save)){

    save_storage_df(df_new)

    add_to_logfile(
      username = username,
      data_level = d_level,
      entry_id = new_id,
      fn_name = "make_entry",
      fn_input = list(
        input_id_vars = input_id_vars,
        input_info_vars = input_info_vars,
        prev_id_merged = prev_id_merged
      )
    )


  }

  return(df_new)

}



make_id_var <- function(df, shrink = TRUE){

  dl <- get_d_level(df)

  if(dl == "tissue_donor"){

    out <- make_tissue_donor_ids(df)

  } else if(dl == "tissue_sample"){

    out <- make_tissue_sample_ids(df)

  } else if(dl == "tissue_portion"){

    out <- make_tissue_portion_ids(df)

  } else if(dl == "raw_data"){

    out <- make_raw_data_ids(df)

  }

  if(base::isTRUE(shrink)){

    out <- shrink_storage_df(out)

  }

  return(out)

}

make_pretty_storage_df <- function(df){

  d_level <- get_d_level(df)

  id_old <- stringr::str_c("id_", d_level, "_num")
  id_new <- stringr::str_c("ID_", d_level)

  rm_ids <- id_vars_merged[id_vars_merged != id_old]

  df <-
    dplyr::select(.data = df, -dplyr::any_of(rm_ids)) %>%
    dplyr::rename({{id_new}} := {{id_old}} ) %>%
    confuns::make_pretty_df(group.names = FALSE)

  return(df)

}


make_raw_data_ids <- function(df){

  dplyr::mutate(
    .data = df,
    id_raw_data_num = stringr::str_c(
      id_tissue_portion_num,
      base::as.numeric(raw_data_index),
      sep = id_sep
    )
  ) %>%
    dplyr::select(
      id_raw_data_num,
      dplyr::any_of(get_id_vars("raw_data")),
      dplyr::everything()
    )

}

make_tissue_donor_ids <- function(df){

  dplyr::mutate(
    .data = df,
    id_tissue_donor_num = stringr::str_c(
      base::as.numeric(institution),
      base::as.numeric(donor_species),
      base::as.numeric(donor_tag),
      sep = id_sep
    )
  ) %>%
    dplyr::select(
      id_tissue_donor_num,
      dplyr::any_of(get_id_vars("tissue_donor")),
      dplyr::everything()
    )

}

make_tissue_portion_ids <- function(df){

  dplyr::mutate(
    .data = df,
    id_tissue_portion_num = stringr::str_c(
      id_tissue_sample_num,
      base::as.numeric(portion_index),
      sep = id_sep
    )
  ) %>%
    dplyr::select(
      id_tissue_portion_num,
      dplyr::any_of(get_id_vars("tissue_portion")),
      dplyr::everything()
    )

}

make_tissue_sample_ids <- function(df){

  #df <- make_tissue_donor_ids(df)

  dplyr::mutate(
    .data = df,
    id_tissue_sample_num = stringr::str_c(
      id_tissue_donor_num,
      base::as.numeric(date_of_extraction),
      base::as.numeric(sample_index),
      sep = id_sep
    )
  ) %>%
    dplyr::select(
      id_tissue_sample_num,
      dplyr::any_of(get_id_vars("tissue_sample")),
      dplyr::everything()
    )

}


merge_storage_df <- function(df, d_level, df_merge = NULL){

  d_level_input <- get_d_level(df)

  if(base::is.null(df_merge)){

    d_level_merge <- dlc(d_level)

    df_merge <- load_storage_df(d_level_merge)

  } else {

    d_level_merge <- get_d_level(df_merge)

  }

  if(dln(d_level_input) > dln(d_level_merge)){

    out <-
      dplyr::left_join(
        x = add_id_columns(df, d_level_merge),
        y = df_merge,
        by = stringr::str_c("id_", d_level_merge, "_num")
      )

  } else if(dln(d_level_input) < dln(d_level_merge)){

    out <-
      dplyr::left_join(
        x = add_id_column(df_merge, d_level_input),
        y = df,
        by = stringr::str_c("id_", d_level_input, "_num")
      )


  } else {

    out <- df

  }

  out <- dplyr::select(out, dplyr::starts_with("id_"), dplyr::everything())

  return(out)

}



# p -----------------------------------------------------------------------

# creates column 'dir_zip' that contains the final directory under which the
# zip file of each data entry is stored
prepare_df_for_download <- function(df, project_dir, folder_organization = NULL){

  if(base::is.character(folder_organization)){

    df <-
      tidyr::unite(df, col = "x.temp.x", dplyr::all_of(folder_organization), sep = "\\", remove = FALSE) %>%
      dplyr::mutate(
        dir_zip = stringr::str_c({{project_dir}}, "\\zip_files\\", x.temp.x, "\\", id_raw_data_num, ".zip"),
        x.temp.x = NULL
      )

  } else {

    df[["dir_zip"]] <- stringr::str_c({{project_dir}}, "\\zip_files\\", df[["id_raw_data_num"]], ".zip")

  }

  return(df)

}

# creates the project main dir
# creates the zip files folder
# creates necessary zip subfolders if needed
prepare_folder_for_download <- function(df_prep, project_dir, folder_organization){

  # assumes that project_dir has been checked beforehand for validity
  base::dir.create(path = project_dir)

  zip_folder <- stringr::str_c(project_dir, "\\zip_files")
  base::dir.create(zip_folder)

  # create sub folders if necessary
  if(base::is.character(folder_organization)){

    df <- df_prep

    df_adj <-
      purrr::map_dfc(
        .x = folder_organization,
        .f = function(cname){

          df[[cname]] <- base::as.character(df[[cname]])

          df[[cname]][df[[cname]] == ""] <- stringr::str_c(cname, "_unknown")

          df[[cname]][base::is.na(df[[cname]])] <- stringr::str_c(cname, "_unknown")

          df[, cname]

        }
      ) %>%
      dplyr::distinct()

    for(r in 1:base::nrow(df_adj)){

      for(i in base::seq_along(folder_organization)){

        dir_new <-
          stringr::str_c(base::as.character(df_adj[r, (1:i)]), collapse = "\\") %>%
          stringr::str_c(zip_folder, ., sep = "\\")

        if(!base::dir.exists(dir_new)){

          base::dir.create(path = dir_new)

        }

      }

    }

  }

  base::invisible(TRUE)

}


process_choices <- function(x, action = NULL, type = NULL){

  if(base::is.character(x)){

    out <- base::unique(x)

  } else if(base::is.factor(x)){

    out <- base::levels(x)

  } else if(base::is.list(x)){

    out <-
      purrr::flatten(x) %>%
      purrr::flatten_chr() %>%
      base::unique()

  }

  out <- base::sort(out)

  if(!"unknown" %in% out){

    out <- c("unknown", out)

  }

  if(action == "add" & !("" %in% out)){

    out <- c("", out)

  } else if(action == "edit"){

    out <- out[out != ""]

  } else if(action == "filter"){

    out <- out[!out %in% c("unknown", "")]

  }

  return(out)

}

# remove unnecessary empty space
process_pub_ref <- function(x){

  stringr::str_replace_all(string = x, pattern = " {2,}", replacement = " ")

}


# r -----------------------------------------------------------------------

remove_id_column <- function(df, d_level = NULL){

  input_dl <- get_d_level(df)

  id_col_df <- stringr::str_c("id_", input_dl, "_num")

  # by default, remove all id columns except the one of the current level
  if(base::is.null(d_level)){

    id_cols_to_rm <- stringr::str_c("id_", data_levels[-dln(input_dl)], "_num")

  } else {

    id_cols_to_rm <- stringr::str_c("id_", dlc(dl), "_num")

  }

  for(idc_rm in id_cols_to_rm){

    if(id_col_df != idc_rm){

      df[[idc_rm]] <- NULL

    }

  }

  return(df)

}


# s -----------------------------------------------------------------------




show_entry <- function(id){

  d_level <- identify_d_level(id)

  id_var <- id_vars_merged[[d_level]]

  load_storage_df(d_level) %>%
    dplyr::filter(!!rlang::sym(id_var) == {{id}})

}

show_entries <- function(id, up_to = 4){

  d_level <-
    identify_d_level(id) %>%
    dln()

  id_var <- id_vars_merged[[d_level]]

  purrr::map(
    .x = 1:up_to,
    .f = function(i){

      load_storage_df(i) %>%
        add_id_columns(df = .) %>%
        dplyr::filter(!!rlang::sym(id_var) == {{id}}) %>%
        shrink_storage_df()

    }
    ) %>%
    purrr::set_names(data_levels[1:up_to])

}


shrink_storage_df <- function(df){

  d_level <- get_d_level(df)

  dplyr::select(
    .data = df,
    dplyr::any_of(id_vars_merged[dln(d_level)]),
    dplyr::any_of(get_id_vars(d_level, level_spec = TRUE)),
    dplyr::any_of(get_info_vars(d_level, level_spec = TRUE))
  ) %>%
    remove_id_column() %>% # removes id columns from other levels
    sort_storage_df()

}

sort_storage_df <- function(df){

  d_level <- get_d_level(df)

  if(dln(d_level) != 1){

    prev_id_vars <- get_id_vars(dln(d_level)-1, level_spec = FALSE)

  } else {

    prev_id_vars <- base::character()

  }

  dplyr::select(
    .data = df,
    dplyr::any_of(base::rev(id_vars_merged)),
    dplyr::any_of(prev_id_vars),
    dplyr::all_of(get_id_vars(d_level = d_level, level_spec = TRUE)),
    dplyr::any_of(get_info_vars(d_level, level_spec = FALSE))
  )

}



# u -----------------------------------------------------------------------

unzip_files <- function(project_dir,
                        verbose = TRUE,
                        in_shiny = FALSE){

  base::dir.create(stringr::str_c(project_dir, "\\data"))

  files_to_unzip <-
    base::list.files(path = stringr::str_c(project_dir, "\\zip_files"), recursive = TRUE, full.names = TRUE) %>%
    stringr::str_subset(pattern = "\\.zip$")

  for(ftu in files_to_unzip){

    #ftu_ref <- stringr::str_remove(ftu, pattern = project_dir)

    confuns::give_feedback(
      msg = glue::glue("Unzipping {ftu}."),
      verbose = verbose,
      in.shiny = in_shiny
    )

    exdir <-
      stringr::str_replace_all(string = ftu,pattern = "zip_files", replacement = "data") %>%
      stringr::str_remove_all(pattern = "\\.zip$")

    # weird error "Failed to set mtime on `/` while extracting..." makes the function
    # crash although unzipping seems to work fine.
    base::tryCatch({

      zip::unzip(zipfile = ftu, exdir = exdir, overwrite = TRUE)

    }, error = function(error){

      warning(glue::glue("Unzipping of {ftu} failed with message: {error$message}."))

    })

    confuns::give_feedback(
      msg = glue::glue("Unzipped {ftu}."),
      verbose = verbose,
      in.shiny = in_shiny
    )

  }

  return(list("information about unzipping process"))

}


# v -----------------------------------------------------------------------


validate_project_name <- function(pname){

  shiny::isTruthy(pname) &
    stringr::str_detect(pname, pattern = "[A-Z]|[a-z]|[0-9]")

}



# w -----------------------------------------------------------------------


write_empty_logfile_df <- function(dir_repo = get_dir_repository(),
                                   overwrite = FALSE,
                                   verbose = TRUE,
                                   in_shiny = FALSE){

  df <- create_empty_logfile_df()

  filename <- repository_files$logfile$name

  users_file <- stringr::str_c(dir_repo, "/", filename)

  if(base::file.exists(users_file) & !base::isTRUE(overwrite)){

    msg <- glue::glue("'{users_file}' already exists. Set overwrite to TRUE to proceed.")

    confuns::give_feedback(
      msg = msg,
      fdb.fn = "stop",
      with.time = FALSE,
      in.shiny = in_shiny
    )

  }

  base::saveRDS(object = df, file = users_file)

  confuns::give_feedback(
    msg = glue::glue("{users_file} created."),
    verbose = verbose,
    with.time = FALSE,
    in.shiny = in_shiny
  )

  base::invisible(TRUE)

}

#' @title Write empty storage data.frames
#'
#' @seealso [`save_storage_df()`], [`load_storage_df()`]
#'
write_empty_storage_df <- function(d_level,
                                   dir_repo = get_dir_repository(),
                                   overwrite = FALSE,
                                   verbose = TRUE,
                                   in_shiny = FALSE){

  d_level <- dlc(d_level)

  df <- create_empty_storage_df(d_level)

  filename <- repository_files[[stringr::str_c(d_level, "_table")]][["name"]]

  storage_file <- stringr::str_c(dir_repo, "/", filename)

  if(base::file.exists(storage_file) & !base::isTRUE(overwrite)){

    msg <- glue::glue("'{storage_file}' already exists. Set overwrite to TRUE to proceed.")

    confuns::give_feedback(
      msg = msg,
      fdb.fn = "stop",
      with.time = FALSE,
      in.shiny = in_shiny
    )

  }

  base::saveRDS(object = df, file = storage_file)

  confuns::give_feedback(
    msg = glue::glue("{storage_file} created."),
    verbose = verbose,
    with.time = FALSE,
    in.shiny = in_shiny
  )

  base::invisible(TRUE)

}

write_empty_users_df <- function(dir_repo = get_dir_repository(),
                                 overwrite = FALSE,
                                 verbose = TRUE,
                                 in_shiny = FALSE){

  df <- create_empty_users_df()

  filename <- repository_files$users$name

  users_file <- stringr::str_c(dir_repo, "/", filename)

  if(base::file.exists(users_file) & !base::isTRUE(overwrite)){

    msg <- glue::glue("'{users_file}' already exists. Set overwrite to TRUE to proceed.")

    confuns::give_feedback(
      msg = msg,
      fdb.fn = "stop",
      with.time = FALSE,
      in.shiny = in_shiny
    )

  }

  base::saveRDS(object = df, file = users_file)

  confuns::give_feedback(
    msg = glue::glue("{users_file} created."),
    verbose = verbose,
    with.time = FALSE,
    in.shiny = in_shiny
  )

  base::invisible(TRUE)

}

write_empty_set_up <- function(dir_repo = get_dir_repository(),
                               overwrite = FALSE,
                               verbose = TRUE,
                               in_shiny = FALSE){

  #
  obj <-
    list(
      connected_with = list(device1 = Sys.info()),
      created_by = base::Sys.info(),
      created_at = base::Sys.time(),
      version = current_lab_orga_version
      )

  filename <- repository_files$setup$name

  set_up_file <- stringr::str_c(dir_repo, "/", filename)

  if(base::file.exists(set_up_file) & !base::isTRUE(overwrite)){

    msg <- glue::glue("'{set_up_file}' already exists. Set overwrite to TRUE to proceed.")

    confuns::give_feedback(
      msg = msg,
      fdb.fn = "stop",
      with.time = FALSE,
      in.shiny = in_shiny
    )

  }

  base::saveRDS(object = obj, file = set_up_file)

  confuns::give_feedback(
    msg = glue::glue("{set_up_file} created."),
    verbose = verbose,
    with.time = FALSE,
    in.shiny = in_shiny
  )

  base::invisible(TRUE)


}


