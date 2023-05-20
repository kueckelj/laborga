



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



#' @title Configure `laborga` storage directory
#'
#' @description This function allows the user to configure the `laborga` storage directory by setting the storage
#' directory path. It checks if the directory exists and prompts the user to create it if it
#' doesn't exist. The user is then asked to confirm the directory path before setting it as
#' an environment variable
#'
#' @param dir Character string specifying the storage directory path for `laborga`.
#'
#' @return An invisible `TRUE` if configuration was succesfull. An invisible `FALSE`, if not.
#'
#' @details
#' The `configureLaborga()` function is used to configure the `laborga` storage directory. It takes
#' a directory path as input and performs the following steps:
#'
#' \itemize{
#'   \item Normalizes the directory path using `normalizePath()`.
#'   \item Checks if the directory exists using `dir.exists()`.
#'   \item If the directory doesn't exist, prompts the user to create it.
#'   \item If the user confirms to create the directory, it uses `dir.create(recursive = TRUE)` to create it.
#'   \item Asks the user to confirm the directory path.
#'   \item If the user confirms, sets the directory path as an environment
#'   variable named LABORGA_STORAGE_DIRECTORY using `Sys.setenv()`.
#' }
#'
#' If the directory already exists the user is asked to overwrite it. If it does not exist
#' the user is asked if the directory should be created. If he chooses not to create it, the function is
#' aborted and the `laborga` storage directory is not configured.
#'
#' @seealso [`configureStorageTables()`]
#'
#' @examples
#' # Configure the `laborga` storage directory
#' configureLaborga("/path/to/laborga")
#'
#' @export
configureStorageDirectory <- function(dir) {

  # 1. Set the default directory --------------------------------------------

  # check if already a valid directory exists
  if(storageDirectorySet() & storageDirectoryExists()){

    set_dir <- getStorageDirectory()

    overwrite_dir <-
      base::readline(
        base::paste(
          "There is already an existing storage directory set: '",
          set_dir,
          "' Do you want to overwrite? (yes/no)"
          )
        )

    if(base::tolower(overwrite_dir) != "yes"){

      base::cat("Configuration stopped. Storage directory remains as is.")

      return(base::invisible(TRUE))

    }

  }

  # continue with new input
  dir <- base::normalizePath(dir, mustWork = FALSE)

  # Check if the directory exists
  if(!base::dir.exists(dir)){

    # Prompt the user to create the directory
    create_dir <- base::readline(paste("The directory does not exist. Create the directory? (yes/no): "))

    if(base::tolower(create_dir) == "yes"){

      # Create the directory
      dir_created <- base::dir.create(dir, recursive = TRUE)

      if(dir_created) {

        base::cat("Directory created successfully.\n")

      } else {

        base::cat("Failed to create the directory.\n")

        return(base::invisible(FALSE))

      }

    } else {

      base::cat("Configuration aborted. Please run configureLaborga() again with a valid directory.\n")

      return(base::invisible(FALSE))

    }
  }

  # Prompt the user to confirm the directory path
  confirm <-
    base::readline(
      base::paste(
        "Is this the correct directory you want to set as the laborga storage directory?",
        dir,
        "(yes/no): "
        )
      )

  if(base::tolower(confirm) == "yes"){

    base::Sys.setenv("LABORGA_STORAGE_DIRECTORY" = dir)

    base::cat("Laborga storage directory successfully configured.\n")

  } else {

    base::cat("Configuration aborted. Please run configureLaborga() again to set the correct directory.\n")

    return(base::invisible(FALSE))

  }

  base::invisible(TRUE)

}



#' @title Configure `laborga` storage tables
#'
#' @description This function configures the four tables in which
#' the information is stored. I requires prior configuration of the
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
#'   \item *<storage_df>/raw_data_storage_df.RDS*
#'   }
#'
#' @export
configureStorageTables <- function(overwrite = FALSE,
                                   in_shiny = FALSE,
                                   verbose = TRUE){

  storageDirectorySet(error = TRUE)

  storageDirectoryExists(error = TRUE)

  dir <- getStorageDirectory()

  for(dl in data_levels){

    write_empty_storage_df(
      d_level = dl,
      dir = dir,
      overwrite = overwrite,
      verbose = verbose,
      in_shiny = FALSE
    )

  }

}


# g -----------------------------------------------------------------------

#' @title Obtain configured storage directory
#'
#' @description Retrieves what is currently set as the environment
#' variable *LABORGA_STORAGE_DIRECTORY*.
#'
#' @return Character value.
#'
#' @export
#'
#' @examples
#'
#'  getStorageDirectory()
#'
getStorageDirectory <- function(){

  base::Sys.getenv(x = "LABORGA_STORAGE_DIRECTORY")

}



# s -----------------------------------------------------------------------


#' @title Set `laborga` storage directory
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

  base::Sys.setenv("LABORGA_STORAGE_DIRECTORY" = dir)

  confuns::give_feedback(
    msg = glue::glue("Sys. environment variable 'LABORGA_STORAGE_DIRECTORY' set to '{dir}'."),
    verbose = verbose
  )

  base::invisible(TRUE)

}




#' @title Test storage directory validity
#'
#' @param error Logical. If `TRUE`, throws an error if test results
#' are negative.
#'
#' @return Logical value.
#' @export
#'
storageDirectoryExists <- function(error = FALSE){

  out <-
    getStorageDirectory() %>%
    base::dir.exists()

  if(base::isFALSE(out) & base::isTRUE(error)){

    stop("laborga storage directory does not exist.")

  }

  return(out)

}


#' @rdname storageDirectoryExists
#' @export
storageDirectorySet <- function(error = FALSE){

  dir <- getStorageDirectory()

  out <- dir != ""

  if(base::isFALSE(out) & base::isTRUE(error)){

    stop("laborga storage directory has not been set.")

  }

  return(out)

}
