#' @include fns.R
NULL


# a -----------------------------------------------------------------------

assay_trademarks_all <- c("10X Visium Spatial Gene Expression")



# c -----------------------------------------------------------------------

choices_usage_status <- c("Unused" = "unused", "Used" = "used", "Used Up" = "used_up")

# compute_var_list (see below due to dependencies)

current_lab_orga_version <- list(major = 0L, minor = 4L, patch = 0L)

# d -----------------------------------------------------------------------

data_levels <- c("tissue_donor", "tissue_sample", "tissue_portion", "raw_data")

# data tables
{
data_tables <-
  list(
    # tissue donor
    tissue_donor = tibble::tibble(

      ## identifier
      id_tissue_donor_num = character(1),

      institution = factor("Inst", "Inst"),
      workgroup = factor("Workgr", "Workgr"),
      donor_species = factor("ds", "ds"),
      donor_tag = factor("dt", "dt"),


      ## info
      date_of_birth = as.Date("1980-07-07"),
      sex = character(1),
      #height = numeric(1), # units?
      #weight = numeric(1), # units?
      #comorbidities = list(cm1 = list(icd10 = character(1), since = date())) # ICD-10

    ),
    # tissue sample
    tissue_sample = tibble::tibble(

      # identifier
      id_tissue_sample_num = character(1),

      date_of_extraction = as.Date("2022-07-07"), # id_tissue_donor_num
      sample_index = numeric(1),

      ## info
      # histology/molecular pathology
      organ = character(1),
      organ_part = character(1),
      side = character(1),

      histo_class = character(1),
      histo_subclass = list(ts1 = list(level1 = character(1), level2 = character(1))), # expand_tissue_table(cols = "histo_subclass")
      histo_numeric = list(ts1 = list(KI67 = numeric(1))),
      mutations = list(ts1 = c("mut1", "mut2")), # only existing mutations
      grade = character(1),
      grade_sub = character(1),
      # misc
      tissue_age = numeric(1) # = age of donor at extraction event -> date_of_extraction - date_of_birth

    ),
    # tissue portion
    tissue_portion = tibble::tibble(

      # identifier
      id_tissue_portion_num = character(1),

      portion_index = numeric(1), # + id_tissue_sample_num

      ## info
      storage_mode = character(1),
      storage_size = numeric(1), # unit?
      preparateur_tissue_portion = character(1),
      date_of_creation = as.Date("2022-07-07"),
      usage_status = character(1) # unused, partly_used_succ, partly_used_unsucc, used_up_succ, lost

    ),
    # raw data
    raw_data = tibble::tibble(

      ## identifier
      id_raw_data_num = character(1),

      raw_data_index = numeric(1),

      ## info
      link_raw_data = character(1),
      pub_journal = character(1),
      pub_ref = character(1),
      pub_year = numeric(1),
      pub_citation = character(1),
      assay_trademark = factor("at", "at"), # tm = trademark (registered if possible, else unregistered)
      assay_type = character(1), #scRNAseq, bulkRNAseq, singleCellImaging
      preparateur_raw_data = character(1)

    )
  )
}

# computed: Variable content is computed based on or extracted from other variables.
#           e.g. pub_year is extracted from pub_ref
#           required function is stored in slot $compute_with of the respective data
#           variable. This function always takes the data.frame and returns the variable!
#           Slots content is `NULL` if it is not of that type computed.
# id      : Variable is an ID variable. Content of ID variables are merged to the data_level
#           ID variable *id_<data_level>_num* using the function make_id_var().
#           If $shiny_input is not a function, the variable is a simple index that
#           is computed by make_id_var() and its subfunctions based on the number of
#            previous entries. (e.g. tissue_portion_index)
# optional: Variable content is optional and information can (and should) be added
#           but it is not required to make the entry as sometimes information is
#           only available later on (e.g. histo_class).
# required: Variable content is not part of the ID but too relevant to be omitted and
#           always available.
data_var_types <- c("computed", "id", "optional", "required")

# data variables

data_variables <- list(
  # assay trademark
  assay_trademark = list(
    class = "character",
    d_level = "raw_data",
    description = c(
      "The registered or unregistered trademark of the assay the tissue portion was",
      " used for."
    ),
    filter = TRUE,
    label = "Assay trademark:",
    name = "assay_trademark",
    shiny_input = function(pref = NULL, selected = NULL, choices = NULL, descr = TRUE, ...){

      shiny::selectizeInput(
        inputId = stringr::str_c(pref, "assay_trademark", sep = "_"),
        label = "Assay Trademark:",
        selected = selected,
        choices = choices,
        multiple = FALSE,
        options = list(create = TRUE)
      ) %>%
        htmlPopify(var = "assay_trademark", descr = descr, placement = "right")

    },
    type = "required"
  ),
  # comment raw data
  comment_raw_data = list(
    class = "character",
    d_level = "raw_data",
    descripton = c("A formless comment to provide additional information about the raw data."),
    filter = FALSE,
    label = "Comment:",
    name = "comment_raw_data",
    shiny_input = function(pref = NULL, selected = NULL, descr = TRUE, ...){

      shiny::textInput(
        inputId = stringr::str_c(pref, "comment_raw_data", sep = "_"),
        label = "Comment:",
        width = "100%",
        value = selected
      ) %>% htmlPopify(var = "comment_raw_data", descr = descr)

    },
    type = "optional"
  ),
  # comment tissue donor
  comment_tissue_donor = list(
    class = "character",
    d_level = "tissue_donor",
    descripton = c("A formless comment to provide additional information about the tissue donor."),
    filter = FALSE,
    label = "Comment:",
    name = "comment_tissue_donor",
    shiny_input = function(pref = NULL, selected = NULL, descr = TRUE, ...){

      shiny::textInput(
        inputId = stringr::str_c(pref, "comment_tissue_donor", sep = "_"),
        label = "Comment:",
        width = "100%",
        value = selected
      ) %>% htmlPopify(var = "comment_tissue_donor", descr = descr)

    },
    type = "optional"
  ),
  # comment tissue portion
  comment_tissue_portion = list(
    class = "character",
    d_level = "tissue_portion",
    descripton = c("A formless comment to provide additional information about the tissue portion."),
    filter = FALSE,
    label = "Comment:",
    name = "comment_tissue_portion",
    shiny_input = function(pref = NULL, nth, selected = NULL, descr = TRUE, ...){

      shiny::textInput(
        inputId = stringr::str_c(pref, "comment_tissue_portion", nth, sep = "_"),
        label = "Comment:",
        width = "100%",
        value = selected
      ) %>% htmlPopify(var = "comment_tissue_portion", descr = descr)

    },
    type = "optional"
  ),
  # comment tissue sample
  comment_tissue_sample = list(
    class = "character",
    d_level = "tissue_sample",
    descripton = c("A formless comment to provide additional information about the tissue sample"),
    filter = FALSE,
    label = "Comment:",
    name = "comment_tissue_sample",
    shiny_input = function(pref = NULL, selected = NULL, descr = TRUE, ...){

      shiny::textInput(
        inputId = stringr::str_c(pref, "comment_tissue_sample", sep = "_"),
        label = "Comment:",
        width = "100%",
        value = selected
      ) %>% htmlPopify(var = "comment_tissue_sample", descr = descr)

    },
    type = "optional"
  ),
  # data of birth
  date_of_birth = list(
    class = "date",
    d_level = "tissue_donor",
    description = c("The donors date of birth."),
    filter = TRUE,
    label = "Date of Birth:",
    name = "date_of_birth",
    shiny_input = function(pref = NULL, selected = NULL, descr = TRUE, ...){

      if(base::is.null(selected)){

        value <- "1900-01-01"

      } else {

        value <- selected

      }

      shiny::dateInput(
        inputId = stringr::str_c(pref, "date_of_birth", sep = "_"),
        label = "Date of Birth:",
        value = value,
        startview = "year",
        width = "100%"
      ) %>%
        htmlPopify(var = "date_of_birth", descr = descr, placement = "right")

    },
    type = "required"
  ),
  # date of creation
  date_of_creation = list(
    class = "date",
    d_level = "tissue_portion",
    description = c("The date at which the tissue portion was created."),
    filter = TRUE,
    label = "Date of Creation:",
    name = "date_of_creation",
    shiny_input = function(pref = NULL, nth, selected = NULL, descr = TRUE, ...){

      if(pref == "inp"){

        html <-
          shinyWidgets::actionBttn(
            inputId = stringr::str_c("date_of_creation_today_", nth),
            label = "Today",
            style = "material-flat",
            color = "primary",
            size = "sm"
          ) %>% htmlPopify(descr = "Switch date to today.")

      } else {

        html <- shiny::tagList()

      }

      shiny::tagList(
        shiny::column(
          width = 12,
          shiny::dateInput(
            inputId = stringr::str_c(pref, "date_of_creation", nth, sep = "_"),
            label = "Date of Creation:",
            value = selected,
            width = "100%"
          ) %>%
            htmlPopify(var = "date_of_creation", descr = descr, placement = "right"),
          html

        )
      )


    },
    type = "required"
  ),
  # date of extraction
  date_of_extraction = list(
    class = "date",
    d_level = "tissue_sample",
    description = c("The date at which the donor was operated and the tissue sample was extracted."),
    filter = FALSE,
    label = "Date of Extraction:",
    name = "date_of_extraction",
    shiny_input = function(pref = NULL, selected = NULL, descr = TRUE, ...){

      shiny::tagList(
        shiny::column(
          width = 12,
          shiny::dateInput(
            inputId = stringr::str_c(pref, "date_of_extraction", sep = "_"),
            label = "Date of Extraction:",
            value = selected,
            width = "100%"
          ) %>%
            htmlPopify(var = "date_of_extration", descr = descr, placement = "right"),
          shinyWidgets::actionBttn(
            inputId = "date_of_extraction_today",
            label = "Today",
            style = "material-flat",
            color = "primary",
            size = "sm"
          ) %>% htmlPopify(descr = "Switch date to today.")
        )
      )

    },
    type = "id"
  ),
  # donor species
  donor_species = list(
    class = "factor",
    d_level = "tissue_donor",
    description = c("The species to which the tissue donor belongs."),
    filter = TRUE,
    label = "Donor Species:",
    name = "donor_species",
    shiny_input = function(pref = NULL, selected = NULL, choices = NULL, descr = TRUE, ...){

      shiny::selectizeInput(
        inputId = stringr::str_c(pref, "donor_species", sep = "_"),
        label = "Donor Species:",
        selected = selected,
        choices = choices,
        multiple = FALSE,
        options = list(create = TRUE)
      ) %>%
        htmlPopify(var = "donor_species", descr = descr, placement = "right")

    },
    type = "id"
  ),
  # donor tag
  donor_tag = list(
    class = "factor",
    d_level = "tissue_donor",
    description = c("A combination of numbers and/or letter that identifies",
                  " the donor among donors of the same species from the same workgroup.",
                  " This should be the identifier based on which clinical data is stored",
                  " in case of human patients in hospitals and university clinics (e.g. PIZ",
                  "of the University Clinic Freiburg) in order to facilitate matching of clinical data."),
    filter = FALSE,
    label = "Donor tag:",
    name = "donor_tag",
    shiny_input = function(pref = NULL, descr = TRUE, ...){

      shiny::textInput(
        inputId = stringr::str_c(pref, "donor_tag", sep = "_"),
        label = "Donor Tag:",
        placeholder = "e.g 345903485 or P4458X34"
      ) %>%
        htmlPopify(var = "donor_tag", descr = descr, placement = "right")

    },
    type = "id"
  ),
  # grade
  grade = list(
    class = "character",
    d_level = "tissue_sample",
    description = c("The grade in case of established grading scores for pathological tissue samples (E.g. WHO° IV for Glioblastoma.)"),
    filter = TRUE,
    label = "Pathol. Grade:",
    name = "grade",
    shiny_input = function(pref = NULL, selected = NULL, descr = TRUE, ...){

      shiny::selectInput(
        inputId = stringr::str_c(pref, "grade", sep = "_"),
        label = "Grade of Disease:",
        selected = selected,
        choices = c("", base::as.character(utils::as.roman(1:20)))
      ) %>%
        htmlPopify(var = "grade", descr = descr, placement = "right")

    },
    type = "optional"
  ),
  # grade sub
  grade_sub = list(
    class = "character",
    d_level = "tissue_sample",
    description = c("The sub grade if needed."),
    filter = TRUE,
    label = "Pathol. Subgrade:",
    name = "grade_sub",
    shiny_input = function(pref = NULL, selected = NULL, descr = TRUE, ...){

      shiny::selectInput(
        inputId = stringr::str_c(pref, "grade_sub", sep = "_"),
        selected = selected,
        label = "Subgrade of Disease:",
        choices = c("", letters)
      ) %>%
        htmlPopify(var = "grade_sub", descr = descr, placement = "right")

    },
    type = "optional"
  ),
  # histo_class
  histo_class = list(
    class = "character",
    d_level = "tissue_sample",
    description = c("Histological classification of the tissue. e.g. Glioblastoma"),
    filter = TRUE,
    label = "Histological Classification:",
    name = "histo_class",
    shiny_input = function(pref = NULL, selected = NULL, choices = NULL, descr = TRUE, ...){

      shiny::selectizeInput(
        inputId = stringr::str_c(pref, "histo_class", sep = "_"),
        label = "Histological Classification:",
        selected = selected,
        choices = choices,
        multiple = FALSE,
        options = list(create = TRUE)
      ) %>%
        htmlPopify(var = "histo_class", descr = descr, placement = "right")

    },
    type = "optional"
  ),
  # institution
  institution = list(
    class = "factor",
    d_level = "tissue_donor",
    description = c("The institution under which the working group that received",
                    " and owns the tissue portion is registered."),
    filter = TRUE,
    label = "Institution:",
    name = "institution",
    shiny_input = function(pref = NULL, selected = NULL, choices = NULL, descr = TRUE, ...){

      shiny::selectizeInput(
        inputId = stringr::str_c(pref, "institution", sep = "_"),
        label = "Institution:",
        selecte = selected,
        choices = choices,
        multiple = FALSE,
        options = list(create = TRUE)
      ) %>%
        htmlPopify(var = "institution", descr = descr, placement = "right")

    },
    type = "id"
  ),
  # link raw data
  link_raw_data = list(
    class = "character",
    d_level = "raw_data",
    description = c("The data link leading to the folder where the data set is stored.",
                    " Can be a system- or a weblink."),
    filter = FALSE,
    label = "Link to Data:",
    name = "link_raw_data",
    shiny_input = function(pref = NULL, selected = NULL, descr = TRUE, ...){

      shiny::textInput(
        inputId = stringr::str_c(pref, "link_raw_data", sep = "_"),
        label = "Link to Data:",
        placeholder = selected
      ) %>%
        htmlPopify(var = "link_raw_data", descr = descr, placement = "right")

    },
    type = "required"
  ),
  # mutations
  mutations = list(
    class = "list_character",
    d_level = "tissue_sample",
    description = c("Specific mutations for which the tissue has been tested",
                    " positively (e.g. BRAF1, IDH)."),
    filter = TRUE,
    label = "Mutations:",
    name = "mutations",
    shiny_input = function(pref = NULL, selected = NULL, choices = NULL, descr = TRUE, ...){

      if(base::is.list(selected)){

        selected <- purrr::flatten_chr(selected)

      }

      shiny::selectizeInput(
        inputId = stringr::str_c(pref, "mutations", sep = "_"),
        label = "Mutations:",
        selected = selected,
        choices = choices,
        multiple = TRUE,
        options = list(create = TRUE)
      ) %>%
        htmlPopify(var = "mutations", descr = descr, placement = "right")

    },
    type = "optional"
  ),
  # organ
  organ = list(
    class = "character",
    d_level = "tissue_sample",
    description = c("The organ from which the tissue derives."),
    filter = TRUE,
    label = "Organ:",
    name = "organ",
    shiny_input = function(pref = NULL, selected = NULL, descr = TRUE, ...){

      shiny::selectInput(
        inputId = stringr::str_c(pref, "organ", sep = "_"),
        label = "Organ:",
        choices = c("", organs_all),
        selected = selected
      ) %>%
        htmlPopify(var = "organ", descr = descr, placement = "right")

    },
    type = "required"
  ),
  # organ part
  organ_part = list(
    class = "character",
    d_level = "tissue_sample",
    description = c("If the organ is divided into several parts or lobes,",
                    " the precise localisation of the extracted tissue sample."),
    filter = TRUE,
    label = "Organ Part:",
    name = "organ_part",
    shiny_input = function(pref = NULL, ...){

      shiny::uiOutput(outputId = stringr::str_c(pref, "organ_part", sep = "_"))

    },
    shiny_uiOutput = function(pref = NULL, selected = NULL, organ = NULL, choices = NULL, descr = TRUE, ...){

      shiny::validate(
        shiny::need(
          expr = shiny::isTruthy(organ),
          message = "Select the organ."
        )
      )

      shiny::req(organ %in% base::names(organs_parts))

      shiny::selectizeInput(
        inputId = stringr::str_c(pref, "organ_part", sep = "_"),
        label = "Organ Part:",
        choices = process_choices(choices, action = pref, type = "required"),
        multiple = FALSE,
        selected = selected,
        options = list(create = TRUE)
      ) %>%
        htmlPopify(var = "organ_part", descr = descr, placement = "right")

    },
    type = "required"
  ),
  # preparateur data
  preparateur_raw_data = list(
    class = "character",
    d_level = "raw_data",
    description = c("Coworker responsible for the generation of this data set."),
    filter = TRUE,
    label = "Preparateur (data):",
    name = "preparateur_raw_data",
    shiny_input = function(pref = NULL, selected = NULL, choices = NULL, descr = TRUE, ...){

      shiny::selectizeInput(
        inputId = stringr::str_c(pref, "preparateur_raw_data", sep = "_"),
        label = "Preparateur:",
        selected = selected,
        choices = choices,
        options = list(create = TRUE)
      ) %>%
        htmlPopify(var = "preparateur_raw_data", descr = descr, placement = "right")


    },
    type = "optional"
  ),
  # preparateur tissue portion
  preparateur_tissue_portion = list(
    class = "character",
    d_level = "tissue_portion",
    description = c("Coworker responsible for creation of the tissue portion."),
    filter = TRUE,
    label = "Preparateur (tissue):",
    name = "preparateur_tissue_portion",
    shiny_input = function(pref = NULL, selected = NULL, nth = NULL, choices = NULL, descr = TRUE, ...){

      shiny::selectizeInput(
        inputId = stringr::str_c(pref, "preparateur_tissue_portion", nth, sep = "_"),
        label = "Preparateur:",
        selected = selected,
        choices = choices,
        options = list(create = TRUE)
      ) %>%
        htmlPopify(var = "preparateur_tissue_portion", descr = descr, placement = "right")

    },
    type = "optional"
  ),
  # pub citation
  pub_citation = list(
    class = "character",
    d_level = "raw_data",
    description = c("Complete citation as suggested by the respective journal."),
    filter = FALSE,
    label = "Citation:",
    name = "pub_citation",
    shiny_input = function(pref = NULL, selected = NULL, choices = NULL, descr = TRUE, ...){

      shiny::textInput(
        inputId = stringr::str_c(pref, "citation", sep = "_"),
        label = "Publ. Citation",
        value = ""
      ) %>%
        htmlPopify(var = "pub_citation", descr = descr, placement = "right")

    },
    type = "optional"
  ),
  # pub journal
  pub_journal = list(
    class = "character",
    d_level = "raw_data",
    description = c("The journal that published the paper in which the data set was used first."),
    filter = TRUE,
    label = "Publication Journal:",
    name = "pub_journal",
    shiny_input = function(pref = NULL, selected = NULL, choices = NULL, descr = TRUE, ...){

      shiny::selectizeInput(
        inputId = stringr::str_c(pref, "pub_journal", sep = "_"),
        label = "Publ. Journal:",
        selected = "",
        choices = choices,
        options = list(create = TRUE)
      ) %>%
        htmlPopify(var = "pub_journal", descr = descr, placement = "right")

    },
    type = "optional"
  ),
  # pub ref
  pub_ref = list(
    class = "character",
    d_level = "raw_data",
    description = "",
    filter = TRUE,
    label = "Publication Reference:",
    name = "pub_ref",
    shiny_input = function(pref = NULL, selected = NULL, choices = NULL, descr = TRUE, ...){

      shiny::tagList(
        shiny::column(
          width = 12,
          shiny::selectizeInput(
            inputId = stringr::str_c(pref, "pub_ref", sep = "_"),
            label = "Publ. Reference:",
            selected = "",
            choices = choices,
            options = list(create = TRUE)
          ),
          shiny::helpText("Valid input formats:"),
          purrr::map(
            .x = valid_pub_ref_formats,
            .f = ~shiny::helpText(.x)
          )
        )
      )

    },
    type = "optional"
  ),
  # pub year
  pub_year = list(
    class = "numeric",
    compute_with = function(df){

      extract_year(pub_ref = df[["pub_ref"]]) %>%
        base::as.numeric()

    },
    d_level = "raw_data",
    description = c("The year of the publication as numeric."),
    filter = TRUE,
    label = "Publication Year:",
    name = "pub_year",
    shiny_input = NULL,
    type = "computed"
  ),
  # portion index
  portion_index = list(
    class = "numeric",
    d_level = "tissue_portion",
    description = c("The index of portions of tissue sample in case the extracted tissue sample is",
                    " partitioned after extraction. e.g. 2 if the tissue portion was cut in half",
                    " after extraction and it is the second of both halfs."),
    filter = FALSE,
    label = "Portion Index:",
    name = "portion_index",
    shiny_input = NULL,
    type = "id"
  ),
  # raw data index
  raw_data_index = list(
    class = "numeric",
    d_level = "raw_data",
    description = c("The index of assays by tissue portion."),
    filter = FALSE,
    label = "Raw Data Index:",
    name = "raw_data_index",
    shiny_input = NULL,
    type = "id"
  ),
  # sample index
  sample_index = list(
    class = "numeric",
    d_level = "tissue_sample",
    description = c("The index of every tissue sample extracted during the extraction event.",
                    " (e.g. '2') if it is the second tissue smaple extracted during the same",
                    " operation."),
    filter = FALSE,
    label = "Tissue Sample Index:",
    name = "sample_index",
    shiny_input = NULL,
    type = "id"
  ),
  # sex
  sex = list(
    class = "character",
    d_level = "tissue_donor",
    description = c("Biological sex of the tissue donor."),
    filter = TRUE,
    label = "Sex:",
    name = "sex",
    shiny_input = function(pref = NULL, selected = NULL, descr = TRUE, ...){

      if(base::is.null(selected)){

        value <- ""

      } else {

        value <- selected

      }

      shiny::selectInput(
        inputId = stringr::str_c(pref, "sex", sep = "_"),
        label = "Sex:",
        selected = value,
        choices = c("", "unknown", "female", "male")
      )  %>%
        htmlPopify(var = "sex", descr = descr, placement = "right")

    },
    type = "required"
  ),
  # side
  side = list(
    class = "character",
    d_level = "tissue_sample",
    description = c("In case of pairwise organs or pairwise organ parts the side needed",
                    " to descripe the precise localisation of the extracted tissue sample."),
    filter = TRUE,
    label = "Side:",
    name = "side",
    shiny_input = function(pref = NULL, ...){

      shiny::uiOutput(outputId = stringr::str_c(pref, "side", sep = "_"))

    },
    shiny_uiOutput = function(pref = NULL, selected = NULL, organ = NULL, descr = TRUE, ...){

      shiny::req(organ %in% organs_side_needed)

      choices <- c("unknown", "left", "right")

      if(pref == "inp"){

        choices <- c("", choices)

      }

      shiny::selectInput(
        inputId = stringr::str_c(pref, "side", sep = "_"),
        label = "Side:",
        selected = selected,
        choices = choices
      ) %>%
        htmlPopify(var = "side", descr = descr, placement = "right")

    },
    type = "required"
  ),
  surgeon = list(
    class = "character",
    d_level = "tissue_sample",
    description = c("The surgeon who extracted the tissue sample."),
    filter = TRUE,
    label = "Surgeon:",
    name = "surgeon",
    shiny_input = function(pref = NULL, choices = NULL, selected = NULL, descr = TRUE, ...){

      shiny::selectizeInput(
        inputId = stringr::str_c(pref, "surgeon", sep = "_"),
        label = "Surgeon:",
        selected = selected,
        choices = choices,
        multiple = FALSE,
        options = list(create = TRUE)
      ) %>%
        htmlPopify(var = "surgeon", descr = descr, placement = "right")

    },
    type = "optional"
  ),
  # storage localisation
  storage_loc = list(
    class = "character",
    d_level = "tissue_portion",
    description = c("The location where the tissue portion is currently stored."),
    filter = TRUE,
    label = "Storage Location:",
    name = "storage_loc",
    shiny_input = function(pref = NULL, nth, choices = NULL, selected = NULL, descr = TRUE, ...){

      shiny::selectizeInput(
        inputId = stringr::str_c(pref, "storage_loc", nth, sep = "_"),
        label = "Storage Location:",
        selected = selected,
        choices = choices,
        multiple = FALSE,
        options = list(create = TRUE)
      ) %>%
        htmlPopify(var = "storage_loc", descr = descr, placement = "right")

    },
    type = "optional"
  ),

  # storage mode
  storage_mode = list(
    class = "character",
    d_level = "tissue_portion",
    description = c("The way the tissue portion has been processed and has been stored."),
    filter = TRUE,
    label = "Storage Mode:",
    name = "storage_mode",
    shiny_input = function(pref = NULL, nth, selected = NULL, descr = TRUE, ...){

      shiny::selectInput(
        inputId = stringr::str_c(pref, "storage_mode", nth, sep = "_"),
        label = "Storage Mode:",
        selected = selected,
        choices = c("", storage_modes)
      ) %>%
        htmlPopify(var = "storage_mode", descr = descr, placement = "right")

    },
    type = "required"
  ),
  # storage size
  storage_size = list(
    class = "numeric",
    d_level = "tissue_portion",
    description = c("Volume of the tissue portion."),
    filter = FALSE,
    label = "Tissue Size:",
    name = "tissue_size",
    shiny_input = function(pref = NULL, nth, selected = NULL, descr = TRUE, ...){

      shiny::numericInput(
        inputId = stringr::str_c(pref, "storage_size", nth, sep = "_"),
        label = "Size:",
        value = selected,
        min = 0
      ) %>%
        htmlPopify(var = "storage_size", descr = descr, placement = "right")

    },
    type = "optional"
  ),
  # storage unit
  storage_unit = list(
    class = "character",
    d_level = "tissue_portion",
    description = c("The unit of the volume of the tissue portion"),
    filter = FALSE,
    label = "Unit:",
    name = "storage_unit",
    shiny_input = function(pref = NULL, nth, selected = NULL, descr = TRUE, ...){

      shiny::selectInput(
        inputId = stringr::str_c(pref, "storage_unit", nth, sep = "_"),
        label = "Unit:",
        selected = selected,
        choices = storage_units
      ) %>%
        htmlPopify(var = "storage_unit", descr = descr, placement = "right")

    },
    type = "optional"
  ),
  # tissue age
  tissue_age = list(
    class = "numeric",
    compute_with = function(df){

      lubridate::interval(
        start = lubridate::ymd(df[["date_of_birth"]]),
        end = lubridate::ymd(df[["date_of_extraction"]])
      ) %>%
        lubridate::as.period() %>%
        lubridate::year()

    },
    d_level = "tissue_sample",
    description = c("Age of the donor during the extraction of the tissue sample."),
    filter = TRUE,
    label = "Tissue Age (Years):",
    name = "tissue_age",
    shiny_input = NULL,
    type = "computed"
  ),
  # usage status
  usage_status = list(
    class = "character",
    d_level = "tissue_portion",
    description = c("The current status of the tissue portion. Important to",
                    " distinguish between available and used up portions."),
    filter = TRUE,
    label = "Usage Status:",
    name = "usage_status",
    shiny_input = function(pref = NULL, selected = NULL, descr = TRUE, ...){

      shiny::selectInput(
        inputId = stringr::str_c(pref, "usage_status", sep = "_"),
        label = "Usage Status:",
        selected = selected,
        choices = choices_usage_status,
        multiple = FALSE
      ) %>%
        htmlPopify(var = "usage_status", descr = descr, placement = "right")

    },
    type = "optional"
  ),
  # workgroup
  workgroup = list(
    class = "character",
    d_level = "tissue_sample",
    description = c("The working group that received and owns the tissue sample. (e.g. AG Heiland)"),
    filter = TRUE,
    label = "Workgroup:",
    name = "workgroup",
    shiny_input = function(pref = NULL, ...){

      shiny::uiOutput(outputId = stringr::str_c(pref, "workgroup", sep = "_"))

    },
    shiny_uiOutput = function(pref = NULL, selected = NULL, choices = NULL, descr = TRUE, ...){

      shiny::selectizeInput(
        inputId = stringr::str_c(pref, "workgroup", sep = "_"),
        label = "Workgroup:",
        selected = selected,
        choices = choices,
        multiple = FALSE,
        options = list(create = TRUE)
      ) %>%
        htmlPopify(var = "workgroup", descr = descr, placement = "right")

    },
    type = "required"
  )
)


descr_tt <- list(
  unzip_dl = c("If activated, the downloaded .zip files are automatically unzipped.",
               " (Unzipped folders are stored separately, in addition to the .zip files."
               ),
  verbose_dl = c("If activated, informative messages about the progress of the",
                 " download and the unzipping process are displayed.")
)

dt_options <-
  list(
    pageLength = 5,
    scrollX = TRUE,
    scrollY = TRUE
    )



# e -----------------------------------------------------------------------

empty_users_df <-
  tibble::tibble(
    username = character(),
    password = character(),
    permissions = factor(levels = c("admin", "standard"))
  )

# f -----------------------------------------------------------------------

first_name <- ", [A-Z]*\\." # only in combination with last name (adds ', ' to last name)



# h -----------------------------------------------------------------------

helper_content <- list(
  project_name =
    c("The name of the project. This value will be used as the folder name to store all project-related files and data."),
  storage_directory =
    c("The directory in which a new folder will be created. The new folder's name will be based on the project
      name provided."),
  subfolders =
    c("Defines the criteria for creating subfolders to organize raw data. Useful for categorizing data based
      on assay trademarks, organs, sex, anatomic locations, etc. For example, if assay trademark and organ are selected,
      the function will create subfolders for each assay trademark, and within each of these assay trademark folders,
      subfolders will be created for every organ.")
)



# i -----------------------------------------------------------------------

id_sep <- "."
id_sep_count <- c(
  "tissue_donor" = 2,
  "tissue_sample" = 4,
  "tissue_portion" = 5,
  "raw_data" = 6
  )
id_sep_regex <- "\\."


id_vars_list <-
  purrr::map(
    .x = data_levels,
    .f = function(dl){

      purrr::keep(
        .x = data_variables,
        .p = function(dv){

          dv$type == "id" & dv$d_level == dl

        }
      ) %>%
        base::names()

    }
  ) %>%
  purrr::set_names(nm = data_levels)

# do not change names or order of this vector
# other functions depend on it
id_vars_merged <- stringr::str_c("id_", data_levels, "_num")

id_vars_vec <- purrr::flatten_chr(.x = id_vars_list)




# l -----------------------------------------------------------------------

last_name <- "[A-Z]{1}[a-z]*"
last_name_hyphon <- "[A-Z][a-z]*-[A-Z]{1}[a-z]*"



# m -----------------------------------------------------------------------

info_vars_list <-
  purrr::map(
    .x = data_levels,
    .f = function(d_level){

      purrr::keep(
        .x = data_variables,
        .p = ~ .x$type != "id" & .x$d_level == d_level
      ) %>%
        base::names() %>%
        base::sort()

    }
  ) %>%
  purrr::set_names(nm = data_levels)



# o -----------------------------------------------------------------------

optional_vars_list <-
  purrr::map(
    .x = data_levels,
    .f = function(dl){

      purrr::keep(.x = data_variables, .p = ~ .x$d_level == dl & .x$type == "optional") %>%
        base::names()

    }
  ) %>%
  purrr::set_names(nm = data_levels)


organs_all <-
  SPATAData::all_organs %>%
  stringr::str_subset(
    pattern = "Cerebellar Hemisphere|Leukocyte|Temporal Lobe",
    negate = TRUE
    ) %>%
  c("Cerebrum", "Hypothalamus", "Thalamus") %>%
  base::sort()

organs_all <- organs_all[organs_all != "Brain"]


organs_pairwise <-
  c("Adrenal Gland", "Amygdala",
    "Breast", "Bronchus",
    "Cerebellum", # left right hemisphere
    "Cerebrum", # left/right lobes
    "Epididymis",
    "Fallopian Tube",
    "Hippocampus",
    "Kidney",
    "Lung",
    "Parotid Gland",
    "Ovary",
    "Pleura",
    "Salivary Gland", "Submandibular Gland",
    "Testis",
    "Vas Deferens"
    )

organs_parts <-
  list(
    "Brainstem" = c("Midbrain", "Pons", "Medulla"),
    "Cerebellum" = c("Hemisphere", "Vermis"),
    "Cerebrum" = c("Frontal Lobe", "Temporal Lobe", "Parietal Lobe", "Occipital Lobe", "Insula"),
    "Colon" = c("Ascending", "Transverse", "Descending", "Sigmoid"),
    "Heart" = c("Atrium", "Ventricle", "AV Valve", "SL Valve"), # av = atrioventricular, sv = semilunar
    "Liver" = stringr::str_c("Segment ", utils::as.roman(1:8)),
    "Lung" = stringr::str_c("Segment ", utils::as.roman(1:10))
  )

pathologies <- list(
  "Adipose Tissue" = c("Lipoma", "Liposarcoma"),
  "Adrenal Gland" = c("Adenoma", "Carcinoma"),
  "Amygdala" = c("Neuroglioma", "Glioblastoma"),
  "Aorta" = c("Atherosclerosis", "Dissecting aneurysm"),
  "Appendix" = c("Appendicitis", "Carcinoid tumor"),
  "Atrial Appendage" = c("Thrombus", "Endocarditis"),
  "Bone" = c("Osteosarcoma", "Ewing sarcoma"),
  "Bone Marrow" = c("Leukemia", "Myelodysplastic syndrome"),
  "Brainstem" = c("Glioma", "Medulloblastoma"),
  "Breast" = c("Ductal carcinoma in situ", "Invasive ductal carcinoma"),
  "Bronchus" = c("Squamous cell carcinoma", "Small cell carcinoma"),
  "Caecum" = c("Adenocarcinoma", "Diverticulitis"),
  "Cartilage" = c("Chondroma", "Chondrosarcoma"),
  "Cerebellum" = c("Astrocytoma", "Medulloblastoma"),
  "Cerebrum" = c("Glioma", "Meningioma"),
  "Colon" = c("Adenocarcinoma", "Ulcerative colitis"),
  "Coronary Artery" = c("Atherosclerosis", "Coronary artery disease"),
  "Diaphragm" = c("Hernia", "Tumor"),
  "Duodenum" = c("Adenocarcinoma", "Celiac disease"),
  "Ectocervix" = c("Squamous cell carcinoma", "Cervical intraepithelial neoplasia"),
  "Endometrium" = c("Endometrial hyperplasia", "Endometrial carcinoma"),
  "Epididymis" = c("Spermatocele", "Epididymitis"),
  "Esophagus" = c("Barrett's esophagus", "Esophageal carcinoma"),
  "Fallopian Tube" = c("Salpingitis", "Serous carcinoma"),
  "Gall Bladder" = c("Cholecystitis", "Adenocarcinoma"),
  "Gastroesophageal Junction" = c("Gastroesophageal reflux disease", "Adenocarcinoma"),
  "Heart" = c("Myocardial infarction", "Cardiomyopathy"),
  "Hippocampus" = c("Glioma", "Medulloblastoma"),
  "Hypothalamus" = c("Pituitary adenoma", "Craniopharyngioma"),
  "Ileum" = c("Crohn's disease", "Adenocarcinoma"),
  "Jejunum" = c("Adenocarcinoma", "Celiac disease"),
  "Kidney" = c("Renal cell carcinoma", "Pyelonephritis", "Glomerulonephritis"),
  "Liver" = c("Hepatitis", "Hepatocellular carcinoma"),
  "Lung" = c("Adenocarcinoma", "Small cell carcinoma"),
  "Lymph Node" = c("Lymphoma", "Metastatic carcinoma", "Infectious lymphadenitis", "Kikuchi-Fujimoto disease", "Castleman disease"),
  "Nasal Pharynx" = c("Nasopharyngeal carcinoma", "Nonkeratinizing squamous cell carcinoma", "Lymphoma", "Sinonasal carcinoma", "Inverted papilloma"),
  "Nasal Septum" = c("Chondrosarcoma", "Olfactory neuroblastoma", "Inverted papilloma", "Hemangioma", "Angiofibroma"),
  "Nerve" = c("Neurofibroma", "Schwannoma", "Malignant peripheral nerve sheath tumor", "Ganglioneuroma", "Neuroblastoma"),
  "Nose" = c("Sinonasal carcinoma", "Inverted papilloma", "Squamous cell carcinoma", "Adenocarcinoma", "Esthesioneuroblastoma"),
  "Ovary" = c("Serous carcinoma", "Mucinous carcinoma", "Endometrioid carcinoma", "Dysgerminoma", "Immature teratoma"),
  "Pancreas" = c("Pancreatic adenocarcinoma", "Pancreatic neuroendocrine tumor", "Acinar cell carcinoma", "Solid pseudopapillary neoplasm", "Intraductal papillary mucinous neoplasm"),
  "Parotid Gland" = c("Mucoepidermoid carcinoma", "Warthin tumor", "Acinic cell carcinoma", "Adenoid cystic carcinoma", "Pleomorphic adenoma"),
  "Penis" = c("Squamous cell carcinoma", "Verrucous carcinoma", "Melanoma", "Adenocarcinoma", "Sarcoma"),
  "Pituitary Gland" = c("Pituitary adenoma", "Craniopharyngioma", "Meningioma", "Metastasis", "Lymphoma"),
  "Placenta" = c("Chorioamnionitis", "Abruptio placentae", "Placenta previa", "Placental infarction", "Placental cysts"),
  "Pleura" = c("Malignant mesothelioma", "Metastasis", "Benign fibrous tumor", "Solitary fibrous tumor", "Desmoplastic mesothelioma"),
  "Prostate" = c("Prostatic adenocarcinoma", "Prostatic intraepithelial neoplasia", "Atypical small acinar proliferation", "Benign prostatic hyperplasia", "Prostate cancer metastasis"),
  "Rectum" = c("Adenocarcinoma", "Mucinous adenocarcinoma", "Signet ring cell carcinoma", "Neuroendocrine tumor", "Gastrointestinal stromal tumor"),
  "Salivary Gland" = c("Mucoepidermoid carcinoma", "Acinic cell carcinoma", "Adenoid cystic carcinoma", "Pleomorphic adenoma", "Warthin tumor"),
  "Seminal Vesicle" = c("Adenocarcinoma", "Atypical adenomatous hyperplasia", "Prostatic adenocarcinoma", "Atypical small acinar proliferation", "Benign prostatic hyperplasia"),
  "Skeletal Muscle" = c("Rhabdomyosarcoma", "Muscular dystrophy", "Myositis", "Myopathies"),
  "Skin" = c("Melanoma", "Basal cell carcinoma", "Squamous cell carcinoma", "Actinic keratosis"),
  "Small Intestine" = c("Adenocarcinoma", "Gastrointestinal stromal tumors", "Lymphoma", "Carcinoid tumors"),
  "Smooth Muscle" = c("Leiomyosarcoma", "Gastrointestinal stromal tumors", "Leiomyoma", "Uterine fibroids"),
  "Spinal Cord" = c("Astrocytoma", "Ependymoma", "Meningioma", "Neurofibroma"),
  "Spleen" = c("Lymphoma", "Leukemia", "Hemangioma", "Cysts"),
  "Stomach" = c("Adenocarcinoma", "Gastric lymphoma", "Gastrointestinal stromal tumors", "Peptic ulcers"),
  "Submandibular Gland" = c("Mucoepidermoid carcinoma", "Adenoid cystic carcinoma", "Warthin tumor", "Sialadenitis"),
  "Testis" = c("Germ cell tumors", "Seminoma", "Teratoma", "Leydig cell tumors"),
  "Thalamus" = c("Glioma", "Hemangioblastoma", "Metastatic tumors", "Astrocytoma"),
  "Throat" = c("Squamous cell carcinoma", "Lymphoma", "Pharyngitis", "Tonsillitis"),
  "Thyroid Gland" = c("Papillary carcinoma", "Follicular carcinoma", "Medullary carcinoma", "Anaplastic carcinoma"),
  "Tongue" = c("Squamous cell carcinoma", "Lymphoma", "Leukoplakia", "Erythroplakia"),
  "Tonsil" = c("Squamous cell carcinoma", "Lymphoma", "Tonsillitis", "Peritonsillar abscess"),
  "Trachea" = c("Adenoid cystic carcinoma", "Tracheobronchial papillomatosis", "Laryngotracheal stenosis", "Tracheitis"),
  "Urinary Bladder" = c("Transitional cell carcinoma", "Squamous cell carcinoma", "Adenocarcinoma", "Bladder stones"),
  "Uterine Cervix" = c("Cervical intraepithelial neoplasia", "Squamous cell carcinoma", "Adenocarcinoma", "Cervical polyps"),
  "Uterus" = c("Endometrial hyperplasia", "Endometrial carcinoma", "Leiomyoma"),
  "Vagina" = c("Clear cell adenocarcinoma", "Squamous cell carcinoma"),
  "Vas_Deferens" = c("Adenocarcinoma", "Sarcoma")
) %>%
  purrr::map(.f = base::sort)


organs_side_needed <-
  base::sort(c(organs_pairwise, "Cerebrum", "Cerebellum"))



# r -----------------------------------------------------------------------

rgx <- list(
  author = stringr::str_c(
    stringr::str_c("(", last_name_hyphon, first_name, ")"),
    stringr::str_c("(", last_name, first_name, ")"),
    stringr::str_c("(", last_name_hyphon, ")"),
    stringr::str_c("(", last_name, ")"),
    sep = "|"
  ) %>% stringr::str_c("(", ., ")"), # ^ must be added
  et_al = " et al\\.", # note the empty space at the front
  year = " [0-9]{4}$" # note the empty space at the front
)

rgx_pub_ref_classic <-
  purrr::flatten_chr(rgx[c("author", "et_al", "year")]) %>%
  stringr::str_c(collapse = "") %>%
  stringr::str_c("^", .)

rgx_pub_ref_double <-
  stringr::str_c(rgx$author, " & ", rgx$author, rgx$year)

rgx_pub_ref_final <- stringr::str_c("(", rgx_pub_ref_classic, "|", rgx_pub_ref_double, ")")

required_vars_list <-
  purrr::map(
    .x = data_levels,
    .f = function(dl){

      purrr::keep(.x = data_variables, .p = ~ .x$d_level == dl & .x$type == "required") %>%
        base::names()

    }
  ) %>%
  purrr::set_names(nm = data_levels)

#' @title Repository Files
#'
#' @description A list of names and tests with which to refer to the `LabOrga`
#' repository files.
#'
#' @details Currently, to be a valid `LabOrga` repository the folder must contain
#' 7 files.
#'
#' **1. dlevel1_tissue_donor.RDS **
#' The tissue donor table is a data.frame in which each row corresponds to a tissue
#' donor from whom tissue samples have been extracted. It must contain the variables
#' *institution*, *donor_species* and *donor_tag*.
#'
#' **2. dlevel2_tissue_sample.RDS **
#' The tissue sample table is a data.frame in which each row corresponds to a
#' tissue sample, a contiguous tissue sample or, in case of liquids, a tube.
#' In addition to the ID variables from its tissue donor (merged in *id_tissue_donor_num*),
#' with which tissue samples inherit all information of their respective tissue donor, the
#' the table must contain the ID variables *date_of_extraction* and *sample_index*.
#'
#' **3. dlevel3_tissue_portion.RDS **
#' The tissue portion table is a data.frame in which each row corresponds to
#' a tissue portion, the unit in which tissue is stored in the lab after processing
#' the tissue sample it derived from. In addition to the ID variables from its
#' tissue sample (merged in *id_tissue_sample_num*), with which tissue portions
#' inherit all information of their respective tissue sample, the table must contain
#' the ID variable *portion_index*.
#'
#' ** 4. dlevel4_raw_data.RDS **
#' The raw data table is a data.frame in which each row corresponds to the data output
#' of an assay/experiment that was conducted with a tissue portion. In addition to the
#' ID variables from the tissue portion with which the data was created (merged in
#' *id_tissue_portion_num*), the table must contain the ID variable *raw_data_index* as
#' well as the variables *link_raw_data* and *assay_trademark*.
#'
#' ** 5. logfile.RDS **
#' The logfile is data.frame in which each row corresponds to a change made in the
#' repository during a session within the `LabOrga` interface. This includes,
#' adding, edditing and deleting data entries. It must contain the variables *username*,
#' *fn_name* and *entry_id*.
#'
#' ** 6. setup.RDS **
#' The setup file is a list in which information regarding the repository set up
#' are stored.
#'
#' \itemize{
#'  \item{*connected_with*:}{ A list of devices suffixed with an index. Each slot of this list corresponds to the output
#'  of `Sys.info()` for the device that was connected to the repository.}
#'  \item{*created_by*:}{ The output of `Sys.info()` fo rthe device that created the repository.}
#'  \item{*created_at*:}{ The date time when the repository was created.}
#'  \item{*version*:}{ A list of three slots named *major*, *minor* and *patch*. Used to keep track
#'  if the installed version of `LabOrga` corresponds to the version of the repository.}
#'  }
#'
#'
#' ** 7. users.RDS**
#' A data.frame in which each row corresponds to a user. In addition
#' to the name (*username*) the variables *password* and *permission*
#' must exist.
#'
#' @seealso [data_variables]
#'
#' @export
repository_files <- list(
  tissue_donor_table = list(name = "dlevel1_tissue_donor.RDS", test = base::is.data.frame),
  tissue_sample_table = list(name = "dlevel2_tissue_sample.RDS", test = base::is.data.frame),
  tissue_portion_table = list(name = "dlevel3_tissue_portion.RDS", test = base::is.data.frame),
  raw_data_table = list(name = "dlevel4_raw_data.RDS", test = base::is.data.frame),
  logfile = list(name = "logfile.RDS", test = base::is.data.frame),
  setup = list(name = "setup.RDS", test = base::is.list),
  users = list(name = "users.RDS", test = base::is.data.frame)
)




# s -----------------------------------------------------------------------

shiny_input_info_vars <- list(
  assay_trademark = function(pref = NULL, selected = NULL, choices = NULL, ...){

    shiny::selectizeInput(
      inputId = stringr::str_c(pref, "assay_trademark", sep = "_"),
      label = "Assay Trademark:",
      selected = selected,
      choices = choices,
      multiple = FALSE,
      options = list(create = TRUE)
    )

  },
  citation = function(pref = NULL, selected = NULL, choices = NULL, ...){

    shiny::selectizeInput(
      inputId = stringr::str_c(pref, "citation", sep = "_"),
      label = "Citation",
      selected = selected,
      choices = choices,
      multiple = FALSE,
      options = list(create = TRUE)
    )

  },
  date_of_birth = function(pref = NULL, selected = NULL, ...){

    shiny::dateInput(
      inputId = stringr::str_c(pref, "date_of_birth", sep = "_"),
      label = "Date of Birth:",
      value = selected
    )

  },
  date_of_creation = function(pref = NULL, nth, selected = NULL, ...){

    shiny::dateInput(
      inputId = stringr::str_c(pref, "date_of_creation", nth, sep = "_"),
      label = "Date of Creation:",
      value = selected
    )

  },
  histo_class = function(pref = NULL, selected = NULL, choices = NULL, ...){

    shiny::selectizeInput(
      inputId = stringr::str_c(pref, "histo_class", sep = "_"),
      label = "Histological Classification:",
      selected = selected,
      choices = choices,
      multiple = FALSE,
      options = list(create = TRUE)
    )

  },
  mutations = function(pref = NULL, selected = NULL, choices = NULL, ...){

    shiny::selectizeInput(
      inputId = stringr::str_c(pref, "mutations", sep = "_"),
      label = "Mutations:",
      selected = selected,
      choices = choices,
      multiple = TRUE,
      options = list(create = TRUE)
    )

  },
  organ = function(pref = NULL, selected = NULL, ...){

    shiny::selectInput(
      inputId = stringr::str_c(pref, "organ", sep = "_"),
      label = "Organ:",
      choices = c("", organs_all),
      selected = selected
    )

  },
  organ_part = function(pref = NULL, ...){

    shiny::uiOutput(outputId = stringr::str_c(pref, "organ_part", sep = "_"))

  },
  preparateur_tissue_portion = function(pref = NULL, selected = NULL, choices = NULL, ...){

    shiny::selectizeInput(
      inputId = stringr::str_c(pref, "preparateur_tissue_portion", sep = "_"),
      label = "Preparateur:",
      selected = selected,
      choices = choices,
      options = list(create = TRUE)
    )

  },
  preparateur_raw_data = function(pref = NULL, selected = NULL, choices = NULL, ...){

    shiny::selectizeInput(
      inputId = stringr::str_c(pref, "preparateur_tissue_portion", sep = "_"),
      label = "Preparateur:",
      selected = selected,
      choices = choices,
      options = list(create = TRUE)
    )

  },
  pub_journal = function(pref = NULL, selected = NULL, choices = NULL, ...){

    shiny::selectizeInput(
      inputId = stringr::str_c(pref, "pub_journal", sep = "_"),
      label = "Publ. Journal:",
      selected = selected,
      choices = choices,
      options = list(create = TRUE)
    )

  },
  pub_ref = function(pref = NULL, selected = NULL, choices = NULL, ...){

    shiny::selectizeInput(
      inputId = stringr::str_c(pref, "pub_ref", sep = "_"),
      label = "Publ. Reference:",
      selected = selected,
      choices = choices,
      options = list(create = TRUE)
    )

  },
  sex = function(pref = NULL, selected = NULL, ...){

    shiny::selectInput(
      inputId = stringr::str_c(pref, "sex", sep = "_"),
      label = "Sex:",
      selected = selected,
      choices = c("unknown", "female", "male")
    )

  },
  side = function(pref = NULL, ...){

    shiny::uiOutput(outputId = stringr::str_c(pref, "side", sep = "_"))

  },
  storage_mode = function(pref = NULL, nth, selected = NULL, ...){

    shiny::selectInput(
      inputId = stringr::str_c(pref, "storage_mode", nth, sep = "_"),
      label = "Storage Mode:",
      selected = selected,
      choices = storage_modes
    )


  },
  storage_size = function(pref = NULL, nth, selected = NULL, ...){

    shiny::numericInput(
      inputId = stringr::str_c(pref, "storage_size", nth, sep = "_"),
      label = "Size:",
      value = selected

    )

  },
  storage_unit = function(pref = NULL, nth, selected = NULL, ...){

    shiny::selectInput(
      inputId = stringr::str_c(pref, "storage_unit", sep = "_"),
      label = "Unit:",
      selected = selected,
      choices = storage_units
    )

  },
  usage_status = function(pref = NULL, nth, selected = NULL, choices = NULL, ...){

    shiny::selectInput(
      inputId = stringr::str_c(pref, "usage_status", sep = "_"),
      label = "Usage Status:",
      selected = selected,
      choices = choices
    )

  },
  grade = function(pref = NULL, selected = NULL, ...){

    shiny::selectInput(
      inputId = stringr::str_c(pref, "grade", sep = "_"),
      label = "Grade of Disease:",
      selected = selected,
      choices = c("", base::as.character(utils::as.roman(1:20)))
    )

  },
  grade_sub = function(pref = NULL, selected = NULL, ...){

    shiny::selectInput(
      inputId = stringr::str_c(pref, "grade_sub", sep = "_"),
      selected = selected,
      label = "Subgrade of Disease:",
      choices = c("", letters)
    )

  }

)

shiny_ui_output <-
  list(
    organ_part = function(pref = NULL, selected, organ){

      shiny::validate(
        shiny::need(
          expr = shiny::isTruthy(organ),
          message = "Select the organ."
        )
      )

      shiny::req(organ %in% base::names(organs_parts))

      shiny::selectizeInput(
        inputId = stringr::str_c(pref, "organ_part", sep = "_"),
        label = "Organ part:",
        choices = choices,
        multiple = FALSE,
        selected = selected,
        options = list(create = TRUE)
      )

    },
    side = function(pref = NULL, selected, organ){

      shiny::req(organ %in% organs_side_needed)

      choices <- c("unknown", "left", "right")

      if(pref == "inp"){

        choices <- c("", choices)

      }

      shiny::selectInput(
        inputId = stringr::str_c(pref, "side", sep = "_"),
        label = "Side:",
        selected = selected,
        choices = choices
      )

    }
  )



specific_pretty_names <- list(
  "histo_class" = "Histological Classification",
  "histo_numeric" = "Histological Scores:",
  "histo_subclass" = "Histological Subclassifications"
)

storage_modes <- c("cell culture", "frozen", "frozen (RNA)", "paraffin", "section")

storage_units <-
  c(
    stringr::str_c(c("mm", "cm"), "3"),
    c("ul", "ml", "cl", "dl")
  )


shiny_tissue_portion_regex <- function(pattern = "[0-9]$"){

  stringr::str_c(
    "inp_",
    get_info_vars(d_level = "tissue_portion"),
    "_",
    pattern,
    "$"
    ) %>%
    stringr::str_c(collapse = "|")

}


# v -----------------------------------------------------------------------

valid_pub_ref_formats <-
  c(
    "Smith et al. 2000",
    "Smith, S. et al. 2000",
    "Smith & Miller 2000",
    "Smith, S. & Miller, M. 2000"
    )




# below due to dependencies -----------------------------------------------

# depends on data_levels
computed_vars_list <-
  purrr::map(
    .x = data_levels,
    .f = function(dl){

      purrr::keep(.x = data_variables, .p = ~ .x$d_level == dl & .x$type == "computed") %>%
        base::names()

    }
  ) %>%
  purrr::set_names(data_levels)

# depends on id_vars_list within get_id_vars
data_vars <-
  c(
    "id_raw_data_num",
    "id_tissue_portion_num",
    "id_tissue_sample_num",
    "id_tissue_donor_num",
    dplyr::all_of(get_id_vars(d_level = "raw_data")),
    dplyr::all_of(get_info_vars(d_level = "raw_data", level_spec = FALSE))
  )

