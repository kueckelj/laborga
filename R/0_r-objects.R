#' @include fns.R
NULL


# a -----------------------------------------------------------------------

assay_trademarks_all <- c("10X Visium Spatial Gene Expression")

# d -----------------------------------------------------------------------

data_levels <- c("tissue_donor", "tissue_sample", "tissue_portion", "raw_data")

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
      usage_count = numeric(1),
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

data_var_types <- c("computed", "id", "optional", "required")

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
  # data of birth
  date_of_birth = list(
    class = "date",
    d_level = "tissue_donor",
    description = c("The donors date of birth."),
    filter = TRUE,
    label = "Date of Birth:",
    name = "date_of_birth",
    shiny_input = function(pref = NULL, selected = NULL, descr = TRUE, ...){

      shiny::dateInput(
        inputId = stringr::str_c(pref, "date_of_birth", sep = "_"),
        label = "Date of Birth:",
        value = selected
      ) %>%
        htmlPopify(var = "assay_trademark", descr = descr, placement = "right")

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

      shiny::dateInput(
        inputId = stringr::str_c(pref, "date_of_creation", nth, sep = "_"),
        label = "Date of Creation:",
        value = selected
      ) %>%
        htmlPopify(var = "date_of_creation", descr = descr, placement = "right")

    },
    type = "required"
  ),
  # date of extraction
  date_of_extraction = list(
    class = "date",
    d_level = "tissue_sample",
    description = c("The date at which the donor was operated and the tissue sample was extracted."),
    filter = TRUE,
    label = "Date of Extraction:",
    name = "date_of_extraction",
    shiny_input = function(pref = NULL, selected = NULL, descr = TRUE, ...){

      shiny::dateInput(
        inputId = stringr::str_c(pref, "date_of_extraction", sep = "_"),
        label = "Date of Extraction:",
        value = selected
      ) %>%
        htmlPopify(var = "date_of_extration", descr = descr, placement = "right")

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
        label = "Organ part:",
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
    shiny_input = function(pref = NULL, selected = NULL, choices = NULL, descr = TRUE, ...){

      shiny::selectizeInput(
        inputId = stringr::str_c(pref, "preparateur_tissue_portion", sep = "_"),
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
        selected = selected,
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
    description = c("The abbreviated reference with which to refer to the paper.",
                    " E.g. 'Ravi et al., 2022'.",
                    " Should conform to '*Author* et al., *Year*"),
    filter = TRUE,
    label = "Publication Reference:",
    name = "pub_ref",
    shiny_input = function(pref = NULL, selected = NULL, choices = NULL, descr = TRUE, ...){

      shiny::selectizeInput(
        inputId = stringr::str_c(pref, "pub_ref", sep = "_"),
        label = "Publ. Reference:",
        selected = selected,
        choices = choices,
        options = list(create = TRUE)
      ) %>%
        htmlPopify(var = "pub_ref", descr = descr, placement = "top")

    },
    type = "optional"
  ),
  # pub year
  pub_year = list(
    class = "numeric",
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

      shiny::selectInput(
        inputId = stringr::str_c(pref, "sex", sep = "_"),
        label = "Sex:",
        selected = selected,
        choices = c("unknown", "female", "male")
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
        choices = storage_modes
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
        value = selected
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
        inputId = stringr::str_c(pref, "storage_unit", sep = "_"),
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
    d_level = "tissue_sample",
    description = c("Age of the donor during the extraction of the tissue sample."),
    filter = FALSE,
    label = "Tissue Age:",
    name = "tissue_age",
    shiny_input = NULL,
    type = "computed"
  ),
  # usage count
  usage_count = list(
    class = "character",
    d_level = "tissue_portion",
    description = c("Keeps track of the times a tissue portion was used to generate raw data entries."),
    filter = FALSE,
    label = "Usage Count:",
    name = "usage_count",
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
    shiny_input = function(pref = NULL, selected = NULL, choices = NULL, descr = TRUE, ...){

      shiny::selectInput(
        inputId = stringr::str_c(pref, "usage_status", sep = "_"),
        label = "Usage Status:",
        selected = selected,
        choices = choices,
        multiple = FALSE
      ) %>%
        htmlPopify(var = "usage_status", descr = descr, placement = "right")

    },
    type = "optional"
  ),
  # workgroup
  workgroup = list(
    class = "factor",
    d_level = "tissue_donor",
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
    type = "id"
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


# h -----------------------------------------------------------------------




# i -----------------------------------------------------------------------

id_sep <- "."
id_sep_count <- c(
  "tissue_donor" = 3,
  "tissue_sample" = 5,
  "tissue_portion" = 6,
  "raw_data" = 7
  )
id_sep_regex <- "\\."


id_vars_list <-
  list(
    tissue_donor = c("institution", "workgroup", "donor_species", "donor_tag"),
    tissue_sample = c("date_of_extraction", "sample_index"),
    tissue_portion = c("portion_index"),
    raw_data = c("raw_data_index")
  )

id_vars_merged <- stringr::str_c("id_", data_levels, "_num")

id_vars_vec <- purrr::flatten_chr(.x = id_vars_list)




# l -----------------------------------------------------------------------



# m -----------------------------------------------------------------------

info_vars_list <-
  purrr::map(
    .x = data_levels,
    .f = function(d_level){

      id_vars <- get_id_vars(d_level = d_level)

      data_tables[[d_level]] %>%
        dplyr::select(
          -dplyr::any_of(c(id_vars, id_vars_merged)),
          -dplyr::starts_with("id_")
          ) %>%
        base::colnames()

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

required_vars_list <-
  purrr::map(
    .x = data_levels,
    .f = function(dl){

      purrr::keep(.x = data_variables, .p = ~ .x$d_level == dl & .x$type == "required") %>%
        base::names()

    }
  ) %>%
  purrr::set_names(nm = data_levels)






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
    c("storage_mode", "storage_size", "preparateur_tissue"),
    "_",
    pattern,
    "$"
    ) %>%
    stringr::str_c(collapse = "|")

}




# below -------------------------------------------------------------------

data_vars <-
  purrr::flatten_chr(
    purrr::map(
      .x = data_tables,
      .f = base::names
    )
  ) %>%
  base::unique() %>%
  confuns::vselect(
    id_raw_data_num,
    id_tissue_portion_num,
    id_tissue_sample_num,
    id_tissue_donor_num,
    dplyr::all_of(get_id_vars(d_level = "raw_data")),
    dplyr::all_of(get_info_vars(d_level = "raw_data", level_spec = F))
  )



