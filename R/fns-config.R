

# a -----------------------------------------------------------------------

#' @title Obtain configured storage directory
#'
#' @description Retrieves the directory to folder ~/ProgramData and assembles
#' the required `laborga` directories.
#'
#' @return Character value.
#'
#' @export
assembleLabOrgaDir <- function(){

  if(base::Sys.info()["sysname"] == "Windows"){

    standard_dir <- base::Sys.getenv(x = "ProgramData")

  } else {

    standard_dir <- base::Sys.getenv(x = "ProgramData")

  }

  out <- stringr::str_c(standard_dir, "/", "LabOrga")

  return(out)

}

#' @rdname assembleLabOrgaDir
#' @export
assembleLabOrgaStorageDir <- function(){

  assembleLabOrgaDir() %>%
    stringr::str_c(., "/storage")

}


# c -----------------------------------------------------------------------

#' @title Checks `laborga`-configuration
#'
#' @description Checks if `laborga`-configuration is valid
#' and returns warnings in case of negative test results.
#'
#' @param in_shiny Logical.
#'
#' @return `TRUE` if everything is fine. `FALSE` and warnings in case of problems.
#'
#' @export
#'
checkLaborgaConfiguration <- function(in_shiny = FALSE){

  dir <- getStorageDirectory()

  problems <- 0

  if(base::is.null(dir)){

    msg <- "Storage directory has not been set. Run `configureLaborga()`."

    confuns::give_feedback(
      msg = msg,
      fdb.fn = "warning",
      with.time = FALSE,
      in.shiny = in_shiny
    )

  } else if(base::is.na(dir)){

    msg <- "configured storage directory is not of length 1. Run `configureLaborga()`."

    confuns::give_feedback(
      msg = msg,
      fdb.fn = "warning",
      with.time = FALSE,
      in.shiny = in_shiny
    )

    problems <- problems + 1

  } else if(base::is.character(dir)){

    if(!base::length(dir) == 1){

      msg <- "configured storage directory is not of length 1. Run `configureLaborga()`."

      confuns::give_feedback(
        msg = msg,
        fdb.fn = "warning",
        with.time = FALSE,
        in.shiny = in_shiny
      )

      problems <- problems + 1

    } else if(dir == ""){

      msg <- "Storage directory has not been set. Run `configureLaborga()`."

      confuns::give_feedback(
        msg = msg,
        fdb.fn = "warning",
        with.time = FALSE,
        in.shiny = in_shiny
      )

      problems <- problems + 1

    } else if(!base::dir.exists(dir)){

      msg <- glue::glue("Can not find configured storage directory '{dir}'.")

      confuns::give_feedback(
        msg = msg,
        fdb.fn = "warning",
        with.time = FALSE,
        in.shiny = in_shiny
      )

      problems <- problems + 1

    } else {

      for(dl in data_levels){

        storage_file <- stringr::str_c(dl, "_storage_df.RDS")

        storage_dir <- stringr::str_c(dir, "/", storage_file)

        if(!base::file.exists(storage_file)){

          msg <- glue::glue("Can not find file '{storage_file}' under laborga storage directory '{dir}'.")

          confuns::give_feedback(
            msg = msg ,
            fdb.fn = "warning",
            with.time = FALSE,
            in.shiny = in_shiny
          )

          problems <- problems + 1

        }

      }

    }

  }

  out <- problems == 0

  return(out)

}


#' @title Create `laborga` directories
#'
#' @description Savely creates directories required for `laborga` to work.
#'
#' @inherit argument_dummy params
#'
#' @return Invisible `TRUE` if successful.
#' @export
#'
createLabOrgaDir<- function(in_shiny = FALSE){

  dir <- assembleLabOrgaDir()

  create_directory(dir, in_shiny = in_shiny)

}

#' @rdname createLabOrgaDir
#' @export
createLabOrgaStorageDir <- function(){

  dir <- assembleLabOrgaStorageDir()

  create_directory(dir, in_shiny = in_shiny)

}




#' @title Configure `laborga` storage tables
#'
#' @description This function configures the `laborga`directories by checking
#' if they exist and creating them if not.
#'
#' @inherit argument_dummy params
#'
#' @return An invisible `TRUE` if configuration was succesfull. An invisible `FALSE`, if not.
#'
configureStorageDirectory <- function(in_shiny = FALSE, verbose = TRUE){

  createLabOrgaDir(in_shiny = in_shiny)

  createLabOrgaStorageDir()

}



#' @title Configure `laborga` storage tables
#'
#' @description This function configures the four tables in which
#' the information is stored. It requires prior configuration of the
#' storage directory via `configureStorageDirectory()`.
#'
#' @param overwrite Logical. Must be set to `TRUE` in case of equal file names
#' in the storage directory.
#'
#' @inherit argument_dummy params
#'
#' @return An invisible `TRUE` if configuration was succesfull. An invisible `FALSE`, if not.
#'
#' @details Configures storage tables with `laborga` by writing four empty files
#' in the configured storage directory.
#'
#'  \itemize{
#'   \item *<storage_dir>/tissue_donor_storage_df.RDS*
#'   \item *<storage_dir>/tissue_sample_storage_df.RDS*
#'   \item *<storage_dir>/tissue_portion_storage_df.RDS*
#'   \item *<storage_dir>/raw_data_storage_df.RDS*
#'   }
#'
configureStorageTables <- function(overwrite = FALSE,
                                   in_shiny = FALSE,
                                   verbose = TRUE){

  #storageDirectorySet(error = TRUE)
  labOrgaStorageDirExists(error = TRUE)

  dir <- getLabOrgaStorageDir(in_shiny = in_shiny)

  for(dl in data_levels){

    write_empty_storage_df(
      d_level = dl,
      dir = dir,
      overwrite = overwrite,
      verbose = verbose,
      in_shiny = in_shiny
    )

  }

}


#' @title Configures `laborga` users table
#'
#' @inherit argument_dummy params
#'
#' @return An invisible `TRUE` if configuration was succesfull. An invisible `FALSE`, if not.
#'
#' @seealso [`load_users_df()`]
#'
configureUserTable <- function(overwrite = FALSE,
                               in_shiny = FALSE,
                               verbose = TRUE){

  labOrgaDirExists(error = TRUE)

  dir <- stringr::str_c(assembleLabOrgaDir(), "/users_df.RDS")

  if(base::file.exists(dir)){

    overwrite <-
      create_dialoge(
        situation = glue::glue("File '{dir}' already exists."),
        question = "Do you want to overwrite it?"
      )

    if(overwrite == "yes"){

      base::unlink(x = dir, recursive = TRUE, force = TRUE)

    } else {

      base::cat("Aborted. Did not overwrite directory.")

      return(base::invisible(FALSE))

    }

  }

  base::saveRDS(object = empty_users_df, file = dir)

  confuns::give_feedback(
    msg = glue::glue("{dir} created."),
    verbose = verbose,
    in_shiny = in_shiny,
    with.time = FALSE
  )

}



# g -----------------------------------------------------------------------


#' @title Obtain `laborga` storage directory
#'
#' @description Assembles `laborga` storage directory **AND** initiates
#' configuration if it does not exist via `configureStorageDirectory()`.
#'
#' @inherit argument_dummy params
#'
#' @return Character.
#' @export
#'

getLabOrgaStorageDir <- function(in_shiny = FALSE){

  dir <- assembleLabOrgaStorageDir()

  if(!base::dir.exists(dir)){

    configureStorageDirectory(in_shiny = in_shiny)

  }

  return(dir)

}



# s -----------------------------------------------------------------------


#' @title Set `laborga` storage directory --- probably not needed
#'
#' @description Sets the environment variable LABORGA_STORAGE_DIRECTORY.
#'
#' @param dir The directory to be set.
#'
#' @return Invisible `TRUE`.
#'
#' @export

setStorageDirectory <- function(dir, verbose = TRUE){

  base::stopifnot(base::is.character(dir) & base::length(dir) == 1)

  if(Sys.info()["sysname"] == "Windows"){

    command <- stringr::str_c('setx LABORGA_STORAGE_DIRECTORY "', dir, '"', sep = "")

  } else {

    command <- stringr::str_c('export LABORGA_STORAGE_DIRECTORY ="', dir, '"',  sep = "")

  }

  base::system(command = command)

  if(FALSE){

    base::Sys.setenv("LABORGA_STORAGE_DIRECTORY" = dir)

    confuns::give_feedback(
      msg = glue::glue("Sys. environment variable 'LABORGA_STORAGE_DIRECTORY' set to '{dir}'."),
      verbose = verbose
    )

  }

  base::invisible(TRUE)

}




#' @title Test existence of `laborga` directories
#'
#' @param error Logical. If `TRUE`, throws an error if test results
#' are negative.
#'
#' @return Logical value.
#' @export
#'

labOrgaDirExists <- function(error = FALSE){

  out <-
    assembleLabOrgaStorageDir() %>%
    base::dir.exists()

  if(base::isFALSE(out) & base::isTRUE(error)){

    stop("LabOrga directory does not exist.")

  }

  return(out)

}

#' @rdname labOrgaDirExists
#' @export
labOrgaStorageDirExists <- function(error = FALSE){

  out <-
    assembleLabOrgaStorageDir() %>%
    base::dir.exists()

  if(base::isFALSE(out) & base::isTRUE(error)){

    stop("LabOrga storage directory does not exist.")

  }

  return(out)

}

