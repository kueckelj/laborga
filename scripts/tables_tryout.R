

tissue_donor_table <- tibble::tibble(

  ## identifier
  institution = character(1),
  workgroup = character(1),
  donor_species = character(1),
  donor_index = character(1),

  ## meta
  date_of_birth = date(),
  sex = character(1),
  height = numeric(1), # units?
  weight = numeric(1), # units?
  comorbidities = list(cm1 = list(icd10 = character(1), since = date())) # ICD-10

)


tissue_sample_table <- tibble::tibble(

  ## identifier
  # donor
  institution = character(1),
  workgroup = character(1),
  donor_species = character(1),
  donor_index = character(1),

  # tissue sample
  extraction_incident = numeric(),
  sample_index = numeric(1),

  ## meta
  # histology/molecular pathology
  organ = character(1),
  anatomical_region = character(1),
  histo_class = character(1),
  histo_subclass = list(list(level1 = character(1), level2 = character(1))), # expand_tissue_table(cols = "histo_subclass")
  histo_num = list(list(KI67 = numeric(1))),
  mutations = list(list(mutation1 = logical(1))),
  stainings = list(list(staining1 = logical(1))),
  who_grade = numeric(1),
  who_sub_grade = character(1),
  # misc
  extraction_date = date(),
  tissue_age = numeric(1) # = age of donor at extraction event -> date_of_extraction - date_of_birth

)


tissue_portion_table <- tibble::tibble(

  ## identifier
  # donor
  institution = character(1),
  workgroup = character(1),
  donor_species = character(1),
  donor_index = character(1),

  # tissue sample
  extraction_incident = numeric(),
  sample_index = numeric(1),

  # tissue portion
  portion_index = numeric(1),

  ## meta
  storage_mode = character(1),
  storage_size = numeric(1), # unit?
  preparateur = character(1),
  creation_date = date(),
  usage_count = numeric(1),
  usage_status = character(1) # unused, partly_used_succ, partly_used_unsucc, used_up_succ, used_up_unsucc

)

raw_data_table <- tibble::tibble(

  ## identifier
  # donor
  institution = character(1),
  workgroup = character(1),
  donor_species = character(1),
  donor_index = character(1),

  # tissue sample
  extraction_incident = numeric(),
  sample_index = numeric(1),

  # tissue portion
  portion_index = numeric(1),

  # assay
  assay_trademark = character(1), # tm = trademark (registered if possible, else unregistered)
  assay_index = numeric(1),

  ## meta
  assay_type = character(1) #scRNAseq, bulkRNAseq, singleCellImaging

)










