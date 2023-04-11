



utils::download.file(
  url = "https://www.dropbox.com/scl/fo/6u5x7ri3j0rajiwmtm17z/h?dl=1&rlkey=jmfe94snc163dh4njpsdnw4ln",
  destfile = "data_private/example.zip",
  mode = "wb"
  )

utils::unzip(
  zipfile = "data_private\\example.zip",
  exdir = "data_private\\data_downloaded"
)





doit <- function(){

  verbose <- T
  zipdir <- str_c(getwd(),"downloads\\zip", sep = "\\")
  datadir <- str_c(getwd(), "downloads\\data", sep = "\\")


  if (!file.exists("downloads")) dir.create("downloads")
  if (!file.exists(zipdir)) dir.create(zipdir)
  if (!file.exists(datadir)) dir.create(datadir)


  filename <- "UKF_test"

  fileurl <- "https://www.dropbox.com/scl/fo/8rqsz1xgr4jjnq6lbo7vg/h?dl=1&rlkey=4jxeycf0ayl9k33l02iucszgf"

  if (verbose == TRUE) print(str_c("File url: ",fileurl))

  zipfile <- stringr::str_c(zipdir, "\\", filename, ".zip")
  if (verbose == TRUE) print(str_c("File: ",zipfile))

  #These are the modified lines in the code

  #Mode = wb is required to download binary files

  download.file(fileurl, zipfile, mode = "wb")

  #Changed the function so that it specifies the target directory
  #I recommend overwrite=TRUE otherwise it might crash. Alternative would be to check with file.exists

  zip::unzip(zipfile, exdir=str_c(datadir, filename, sep = "\\"), overwrite=TRUE)

  print("done")

}

df <- get_storage_df(d_level = 4) %>%
  head(6)

df$link_raw_data <-
  c(
    "https://www.dropbox.com/scl/fo/8rqsz1xgr4jjnq6lbo7vg/h?dl=1&rlkey=4jxeycf0ayl9k33l02iucszgf",
    "https://www.dropbox.com/scl/fo/o8ibf7qlxqeyuwvoip98i/h?dl=1&rlkey=5chtiuf8oatbqpck16v8uunmf",
    "https://www.dropbox.com/scl/fo/ddebi23dts2k6i8r1g7e3/h?dl=1&rlkey=5y6efzon33um9hlr1rntstrhd",
    "https://www.dropbox.com/scl/fo/4o74dr5fg6io0jhdenc4o/h?dl=1&rlkey=wtxbt1k82x0sxhpf11lmdvnv8",
    "https://www.dropbox.com/scl/fo/2nbj17f04xhdrwg4i1xtx/h?dl=1&rlkey=rmon9n8eabptwzc1c4sawdhrd",
    "https://www.dropbox.com/scl/fo/gwile9gzc6n9yr0gwadcm/h?dl=1&rlkey=8kch0iwvvhjsx2djqupn9fwsk"
  )


folder_organize = c("institution", "workgroup", "organ")

download_zip_files(df, folder_organize = folder_organize)





