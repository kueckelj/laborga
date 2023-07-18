


# c -----------------------------------------------------------------------

compare_versions <- function(in_shiny = FALSE, show_warnings = FALSE){

  # check if repo version is later than package version
  pkg_version <-
    current_lab_orga_version %>%
    version_to_chr()

  repo_version <-
    load_repo_setup()$version %>%
    version_to_chr()

  version_dif <- utils::compareVersion(a = pkg_version, b = repo_version)

  # repo version is later than pkg versio -> update package
  if(version_dif == -1){

    text <-
      glue::glue(
        "The repository you are connected with is of version '{repo_version}'.
         Your LabOrga package is of version '{pkg_version}'. Please update
         LabOrga before you continue."
      )

    if(base::isTRUE(in_shiny)){

      shiny::showModal(
        ui = shiny::modalDialog(
          title = "LabOrga must be updated",
          shiny::helpText(text),
          footer = shiny::fluidRow(
            htmlCol(width = 4),
            htmlCol(width = 4, shiny::actionButton(inputId = "update_pkg", label = "Update")),
            htmlCol(width = 4)
          )
        )
      )

    } else {

      if(base::isTRUE(show_warnings)){

        warning(text)

      }


    }

    # pkg_version is higher than repo version -> update repo
  } else if(version_dif == 1){

    text <-
      glue::glue(
        "The repository you are connected with is of version '{repo_version}'.
         Your LabOrga package is of version '{pkg_version}'. Please update the
         repository before you continue."
      )

    if(base::isTRUE(in_shiny)){

      shiny::showModal(
        ui = shiny::modalDialog(
          title = "Repository must be updated",
          shiny::helpText(text),
          footer = shiny::fluidRow(
            htmlCol(width = 4),
            htmlCol(width = 4, shiny::actionButton(inputId = "update_repo", label = "Update")),
            htmlCol(width = 4)
          )
        )
      )

    } else {

      if(base::isTRUE(show_warnings)){

        warning(text)

      }

    }

  } else if(version_dif == 0){

  }

  return(version_dif)

}


#' @title Configure `LabOrga`
#'
#' @description Creates configuration folder via `get_dir_configuration()` and
#' creates the configuration object.
#'
#' @return Invisible `TRUE` if successful.
#' @export
#'
configure_lab_orga <- function(){

  config_dir <- get_dir_configuration()

  if(!base::dir.exists(config_dir)){

    base::dir.create(path = config_dir, recursive = TRUE)

  }

  config_file <- stringr::str_c(config_dir, "/configuration.RDS")

  if(!base::file.exists(config_file)){

    config <-
      list(
        date = base::Sys.Date(),
        dirs = list(config = config_dir),
        version = current_lab_orga_version
        )

    base::saveRDS(config, file = config_file)

  }

  base::invisible(TRUE)

}


#' @title Connect `LabOrga` to repository
#'
#' @description Connects `LabOrga` to a `LabOrga`-repository.
#'
#' @param dir_repo Character value. The directory to the repository.
#'
#' @return Invisible `TRUE` if successful.
#' @export
connect_lab_orga <- function(dir_repo, verbose = TRUE, in_shiny = FALSE){

  dir_repo <- base::normalizePath(dir_repo, winslash = "/")

  base::stopifnot(is_repository(dir_repo))

  config_obj <- get_config_obj()

  config_obj[["dirs"]][["repository"]] <- dir_repo

  save_config_obj(config_obj, verbose = FALSE)

  msg <- glue::glue("LabOrga connected to repository in: '{dir_repo}'.")

  confuns::give_feedback(
    msg = msg,
    verbose = verbose,
    in.shiny = in_shiny,
    with.time = FALSE
  )

  base::invisible(TRUE)

}



#' @title Disconnect LabOrga from repository
#'
#' @description Disconnects `LabOrga` from repository.
#'
#' @inherit argument_dummy params
#'
#' @return Invisble `TRUE`
#' @export
disconnect_lab_orga <- function(verbose = TRUE, in_shiny = FALSE){

  config_obj <- get_config_obj()

  config_obj$dirs$repository <- NULL

  save_config_obj(config_obj, verbose = FALSE)

  confuns::give_feedback(
    msg = "LabOrga disconnected from repository.",
    verbose = verbose
  )

  base::invisible(TRUE)

}



# g -----------------------------------------------------------------------

#' @title Load configuration file
#' @return List.
get_config_obj <- function(){

  base::stopifnot(is_configured())

  get_dir_configuration() %>%
    stringr::str_c(., "/configuration.RDS") %>%
    base::readRDS()

}

load_configuration <- get_config_obj

#' @title Obtain dir to configuration folder
#'
#' @return Character value. Depends on the system and the hard drive set up
#' of the device.
#' @export
#'
get_dir_configuration <- function(){

  if(base::Sys.info()["sysname"] == "Windows"){

    standard_dir <- base::Sys.getenv(x = "ProgramData")

  } else {

    standard_dir <- base::Sys.getenv(x = "ProgramData")

  }

  out <- stringr::str_c(standard_dir, "/", "LabOrgaConfig")

  return(out)

}


#' @title Obtain dir to repository
#'
#' @return Character value. Depends on the system and the hard drive set up
#' of the device.
#'
#' @export
#'
get_dir_repository <- function(){

  get_config_obj()[["dirs"]][["repository"]]

}



# l -----------------------------------------------------------------------



#' @title Load repository files
#'
#' @description Basic loading functions for `repository_files`. In some
#' cases there are additional `get_*()`-functions with more functionalities.
#'
#' @return Depends on the nature of the file. Currently either a list or a data.frame.
#' @export
#'
load_logfile_df <- function(){

  base::stopifnot(is_connected())

  base::readRDS(file = stringr::str_c(get_dir_repository(), "/", repository_files$logfile$name))

}

#' @rdname load_logfile_df
#' @export
load_repo_setup <- function(){

  base::stopifnot(is_connected())

  base::readRDS(file = stringr::str_c(get_dir_repository(), "/", repository_files$setup$name))

}

#' @rdname load_logfile_df
#' @export
load_storage_df <- function(d_level){

  base::stopifnot(is_connected())

  d_level <- dlc(d_level)

  file_dir <- stringr::str_c(get_dir_repository(), "/dlevel", dln(d_level), "_", d_level, ".RDS")

  base::readRDS(file_dir)

}

#' @rdname load_logfile_df
#' @export
load_users_df <- function(){

  base::stopifnot(is_connected())

  base::readRDS(file = stringr::str_c(get_dir_repository(), "/", repository_files$users$name))

}


# i -----------------------------------------------------------------------


#' @title Test configuration
#'
#' @description Tests output of `configure_lab_orga()`.
#'
#' @inherit argument_dummy params
#'
#' @return Logical value. `TRUE` if directory as obtained by `get_dir_configuration()`
#' exists and the *configuration.RDS* file exists within this directory and is a list.
#' @export
#'
is_configured <- function(show_warnings = FALSE, fdb.fn = "warning", in_shiny = FALSE){

  out <- base::logical(length = 3L)

  config_dir <- get_dir_configuration()

  out[1] <- base::dir.exists(config_dir)

  if(base::isFALSE(out[1]) & base::isTRUE(show_warnings)){

    msg <- glue::glue("Configuration directory '{config_dir}' does not exist.")

    confuns::give_feedback(
      msg = msg,
      fdb.fn = fdb.fn,
      with.time = FALSE,
      in.shiny = in_shiny
    )

  }

  config_file <-
    stringr::str_c(config_dir, "/configuration.RDS")

  out[2] <- base::file.exists(config_file)

  if(base::isTRUE(out[2])){

    out[3] <- base::is.list(base::readRDS(config_file))

    if(base::isFALSE(out[3])){

      if(base::isTRUE(show_warnings)){

        msg <- glue::glue("'File {config_file}' isn't a list.")

        confuns::give_feedback(
          msg = msg,
          fdb.fn = fdb.fn,
          with.time = FALSE,
          in.shiny = in_shiny
        )

      }

    }

  } else {

    if(base::isTRUE(show_warnings)){

      msg <- glue::glue("File '{config_file}' does not exist.")

      confuns::give_feedback(
        msg = msg,
        fdb.fn = fdb.fn,
        with.time = FALSE,
        in.shiny = in_shiny
      )

    }

  }

  return(base::all(out))

}


#' @title Test connection to repository
#'
#' @inherit argument_dummy params
#'
#' @return Logical value. `TRUE` if configuration object in config directory as
#' obtained by `get_config_obj()`contains a directory in slot `$dirs$repo` that leads to a valid `LabOrga`
#' repository as tested by `is_repository()`.
#'
#' @export
#'
is_connected <- function(show_warnings = FALSE, fdb_fn = "warning", in_shiny = FALSE){

  out <- base::logical(length = 2L)

  config_obj <- get_config_obj()

  dir_repo <- config_obj$dirs[["repository"]]

  out[1] <- base::is.character(dir_repo)

  if(base::isTRUE(out[1])){

    out[2] <- is_repository(dir_repo, show_warnings = show_warnings, in_shiny = in_shiny)

    if(base::isFALSE(out[2]) & base::isTRUE(show_warnings)){

      msg <- glue::glue("Directory '{dir_repo}' leads to an invalid directory.")

      confuns::give_feedback(
        msg = msg,
        fdb.fn = fdb_fn,
        with.time = FALSE,
        in.shiny = in_shiny
      )

    }

  } else {

    if(base::isTRUE(show_warnings)){

      msg <- "No directory to repository deposited."

      confuns::give_feedback(
        msg = msg,
        fdb.fn = fdb_fn,
        with.time = FALSE,
        in.shiny = in_shiny
      )

    }

  }

  return(base::all(out))

}


#' @title Test validity of repository
#'
#' @description Tests availability and validity of all expected `repository_files`
#' in a supposed repository folder as set up by `create_repository()`.
#'
#' @param dir_repo Directory to a supposed repository.
#' @inherit argument_dummy params
#'
#' @return Logical value. `TRUE`, if repository directory as obtained by
#' `get_dir_repository()` exists, all repository files are found
#' and all files are valid.
#'
#' @export
is_repository <- function(dir_repo = get_dir_repository(),
                          show_warnings = FALSE,
                          in_shiny = FALSE){

  out <- base::logical()

  out[1] <- base::dir.exists(dir_repo)

  if(base::isTRUE(out[1])){

    out[2] <-
      purrr::map_lgl(
        .x = repository_files,
        .f = function(rf){

          out_rf <- base::logical(length = 2L)

          file_path <- stringr::str_c(dir_repo, "/", rf$name)

          out_rf[1] <- base::file.exists(file_path)

          if(base::isTRUE(out_rf[1])){

            file <- base::readRDS(file = file_path)

            out_rf[2] <- rf$test(file)

            if(base::isFALSE(out_rf[2]) & base::isTRUE(show_warnings)){

              msg <- glue::glue("File '{rf$name}' in repository `{dir_repo}` is invalid.")

              confuns::give_feedback(
                msg = msg,
                fdb.fn = "warning",
                with.time = FALSE,
                in.shiny = in_shiny
              )

            }

          } else {

            if(base::isTRUE(show_warnings)){

              msg <- glue::glue("File '{rf$name}' is missing in repository `{dir_repo}`.")

              confuns::give_feedback(
                msg = msg,
                fdb.fn = "warning",
                with.time = FALSE,
                in.shiny = in_shiny
              )

            }

          }

          return(base::all(out_rf))

        }
      ) %>%
      base::all()

  } else {

    if(base::isTRUE(show_warnings)){

      confuns::give_feedback(
        msg = glue::glue("'Directory to repository '{dir_repo}' does not exist."),
        fdb.fn = "warning",
        with.time = FALSE,
        in.shiny = in_shiny
      )

    }

  }

  return(base::all(out))

}



# s -----------------------------------------------------------------------

#' @title Save configuration object
#'
#' @description Saves configuration object under the directory of slot
#' `$dirs$config`.
save_config_obj <- function(obj, verbose = TRUE){

  base::saveRDS(object = obj, file = stringr::str_c(obj$dirs$config, "/configuration.RDS"))

  confuns::give_feedback(
    msg = "Configuration saved.",
    verbose = verbose
  )

  base::invisible(TRUE)

}

#' @title Save repository files
#'
#' @description Standardized saving of repository files.
#'
#' @return Invisible `TRUE`, if saving was successfull.
#' @export
#'
save_logfile_df <- function(df){

  base::stopifnot(is_connected())

  base::saveRDS(
    object = df,
    file = stringr::str_c(get_dir_repository(), "/", repository_files$logfile$name)
  )

  base::invisible(TRUE)

}

#' @rdname save_logfile_df
#' @export
save_repo_setup <- function(setup){

  base::stopifnot(is_connected())

  base::saveRDS(
    object = setup,
    file = stringr::str_c(get_dir_repository(), "/", repository_files$setup$name)
  )

  base::invisible(TRUE)

}

#' @rdname save_logfile_df
#' @export
save_storage_df <- function(df){

  d_level <- get_d_level(df)

  # always stores the shrinked version!!!
  # do not change, other functions rely on that
  df <- shrink_storage_df(df)

  info_vars <- get_info_vars(d_level, level_spec = TRUE)

  base::stopifnot(base::all(info_vars %in% base::colnames(df)))

  base::saveRDS(
    object = df,
    file = stringr::str_c(get_dir_repository(), "/dlevel", dln(d_level), "_", d_level, ".RDS")
  )

  base::invisible(TRUE)

}

#' @rdname save_logfile_df
#' @export
save_users_df <- function(df){

  base::stopifnot(is_connected())

  base::saveRDS(
    object = df,
    file = stringr::str_c(get_dir_repository(), "/", repository_files$users$name)
  )

  base::invisible(TRUE)

}

#' @title Set up a `LabOrga`-Repository
#'
#' @description Sets up a `LabOrga`-Repository. By creating the files
#' in `repository_files`.
#'
#' @param dir_repo Directory to the repository.
#' @inherit argument_dummy params
#'
#' @details This function creates the following files within `dir_repo` making
#' it a valid `LabOrga` repository:
#'
#' \itemize{
#'   \item \code{dlevel1_tissue_donor.RDS}: A data frame containing tissue donor information.
#'   \item \code{dlevel2_tissue_sample.RDS}: A data frame containing tissue sample information.
#'   \item \code{dlevel3_tissue_portion.RDS}: A data frame containing tissue portion information.
#'   \item \code{dlevel4_raw_data.RDS}: A data frame containing raw data information.
#'   \item \code{logfile.RDS}: A log file storing relevant information and events.
#'   \item \code{setup.RDS}: A list containing setup details.
#'   \item \code{users.RDS}: A data frame containing user information.
#' }
#'
#' @return Invisible `TRUE` if successful.
#'
create_repository <- function(dir_repo,
                              overwrite = FALSE,
                              in_shiny = FALSE,
                              verbose = TRUE){

  dir_repo <- base::normalizePath(dir_repo, winslash = "/", mustWork = FALSE)

  if(!base::dir.exists(dir_repo)){

    base::dir.create(dir_repo, recursive = TRUE)

    msg <- glue::glue("'{dir_repo}/' created.")

    confuns::give_feedback(
      msg = msg,
      verbose = verbose,
      with.time = FALSE,
      in.shiny = in_shiny
    )

  }

  # write storage tables
  for(dl in data_levels){

    write_empty_storage_df(
      d_level = dl,
      dir_repo = dir_repo,
      overwrite = overwrite,
      in_shiny = in_shiny,
      verbose = verbose
    )

  }

  # write logfile
  write_empty_logfile_df(
    dir_repo = dir_repo,
    overwrite = overwrite,
    in_shiny = in_shiny,
    verbose = verbose
  )

  # write users data.frame
  write_empty_users_df(
    dir_repo = dir_repo,
    overwrite = overwrite,
    in_shiny = in_shiny,
    verbose = verbose
  )

  # write set up
  write_empty_set_up(
    dir_repo = dir_repo,
    overwrite = overwrite,
    in_shiny = in_shiny,
    verbose = verbose
  )

  base::invisible(TRUE)


}

# u -----------------------------------------------------------------------

#' @title Update repository
#'
#' @description Updates repository analogue to the notes of *versions.txt*
#'
#' @inherit argument_dummy params
#'
#' @section Side effects:
#' Loads files from connected repository, updates them and saves them
#' under the same direction, overwriting the old ones.
#'
#' @return Invisible `TRUE` if updating was successful.
#' @export

update_repository <- function(in_shiny = FALSE, verbose = TRUE){

  base::stopifnot(is_connected())

  # repo needs to be updated
  if(compare_versions() == 1){

    repo_setup <- load_repo_setup()

    # 1.update: Version 0.1.0 -> 0.2.0
    if(version_to_chr(repo_setup$version) == "0.1.0"){

      confuns::give_feedback(
        msg = "Updating from v0.1.0 to v0.2.0.",
        verbose = verbose,
        in.shiny = in_shiny,
        with.time = FALSE
      )

      # adds optional variable 'storage_loc' to data level tissue portion
      df <- load_storage_df(d_level = "tissue_portion")

      df[["storage_loc"]] <- "unknown"

      save_storage_df(df)

      # done - new version 0.2.0
      repo_setup$version <- list(major = 0L, minor = 2L, patch = 0L)

    }

    # 2.update: Version 0.2.0 -> 0.3.0
    if(version_to_chr(repo_setup$version) == "0.2.0"){

      confuns::give_feedback(
        msg = "Updating from v0.2.0 to v0.3.0.",
        verbose = verbose,
        with.time = FALSE,
        in.shiny = in_shiny,
        with.time = FALSE
      )

      # add comment_<data_level> variable to storage data.frames
      for(dl in data_levels){

        df <- load_storage_df(dl)

        df[[stringr::str_c("comment_", dl)]] <- ""

        save_storage_df(df)

      }

      repo_setup$version <- list(major = 0L, minor = 3L, patch = 0L)

    }

    # 3. update: Version 0.3.0 -> 0.4.0
    if(version_to_chr(repo_setup$version) == "0.2.0"){

     # changes done manually, see versions.txt for information

      repo_setup$version <- list(major = 0L, minor = 4L, patch = 0L)

    }

    # save after latest update
    save_repo_setup(repo_setup)

    # give feedback
    confuns::give_feedback(
      msg = glue::glue("Successfully updated repository to v{version_to_chr(repo_setup$version)}."),
      verbose = verbose,
      in.shiny = in_shiny,
      with.time = FALSE,
      duration = 10
    )

    # package needs to be updated
  } else if(compare_versions() == -1){

    # give feedback
    confuns::give_feedback(
      msg = glue::glue("Your LabOrga is of lower version than the repository. Please update the package."),
      verbose = verbose,
      fdb.fn = "warning",
      in.shiny = in_shiny,
      with.time = FALSE,
      duration = 10
    )

    # nothing needs to be updated
  } else if(compare_versions() == 0){

    # give feedback
    confuns::give_feedback(
      msg = "LabOrga and repository are both up to date.",
      verbose = verbose,
      in.shiny = in_shiny,
      with.time = FALSE,
      duration = 10
    )

  }

  base::invisible(TRUE)

}


# v -----------------------------------------------------------------------

version_to_chr <- function(version){

  purrr::map_chr(.x = version, .f = ~ .x) %>%
    base::unname() %>%
    stringr::str_c(collapse = ".")

}

version_to_number <- function(version){

  purrr::map_dbl(.x = version, .f = ~ .x) %>%
    base::unname() %>%
    stringr::str_c(collapse = "") %>%
    base::as.numeric()

}






