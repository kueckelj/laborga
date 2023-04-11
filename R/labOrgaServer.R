

launchLabOrgaServer <- function(input, output, session){



# ui matches --------------------------------------------------------------

  shinyFiles::shinyDirChoose(
    input = input,
    id = "choose_storage_folder",
    roots = shinyFiles::getVolumes()()
  )

# steady values -----------------------------------------------------------

  save_tables <- TRUE

  storage_df_empty <- get_storage_df_empty(d_level = base::length(data_levels))

# reactive values ---------------------------------------------------------

  active_d_level <- shiny::reactiveVal(value = "")

  df_tissue_donor <- shiny::reactiveVal(value = get_storage_df("tissue_donor", complete = FALSE))
  df_tissue_sample <- shiny::reactiveVal(value = get_storage_df("tissue_sample", complete = FALSE))
  df_tissue_portion <- shiny::reactiveVal(value = get_storage_df("tissue_portion", complete = FALSE))
  df_raw_data <- shiny::reactiveVal(value = get_storage_df("raw_data", complete = FALSE))

  project_ids <- shiny::reactiveVal(value = "")

# render UI ---------------------------------------------------------------

  output$ed_organ_part <- shiny::renderUI({

    selected <-
      dplyr::filter(df_tissue_sample(), id_tissue_sample_num == selected_tissue_sample()) %>%
      dplyr::pull(organ_part)

    data_variables$organ_part$shiny_uiOutput(
      pref = "ed",
      selected = selected,
      organ = input$ed_organ,
      choices = organs_parts[[input$ed_organ]]
    )

  })

  output$ed_side <- shiny::renderUI({

    selected <-
      dplyr::filter(df_tissue_sample(), id_tissue_sample_num == selected_tissue_sample()) %>%
      dplyr::pull(side)

    data_variables$side$shiny_uiOutput(
      pref = "ed",
      selected = selected,
      organ = input$ed_organ
    )

  })

  output$folder_orga_sorted <- shiny::renderUI({

    shiny::req(input$folder_orga_select)

    shinyjqui::orderInput(
      inputId = "folder_orga_sorted",
      label = "In order:",
      items = input$folder_orga_select,
      item_class = "default"
    )

  })

  output$filter_table_all_tissue_donor <- shiny::renderUI({

    input$reset_filter_tissue_donor

    htmlFilterOptions(df = complete_df_tissue_donor())

  })

  output$filter_table_all_tissue_sample <- shiny::renderUI({

    input$reset_filter_tissue_sample

    htmlFilterOptions(df = complete_df_tissue_sample())

  })

  output$filter_table_all_tissue_portion <- shiny::renderUI({

    input$reset_filter_tissue_portion

    htmlFilterOptions(df = complete_df_tissue_portion())

  })

  output$filter_table_all_raw_data <- shiny::renderUI({

    input$reset_filter_raw_data

    htmlFilterOptions(df = complete_df_raw_data())

  })

  output$filter_table_all_raw_data_projects <- shiny::renderUI({

    input$reset_filter_raw_data_projects

    htmlFilterOptions(df = complete_df_raw_data(), suff = "projects")

  })

  output$inp_organ_part <- shiny::renderUI({

    data_variables$organ_part$shiny_uiOutput(
      pref = "inp",
      selected = NULL,
      organ = input$inp_organ,
      choices = organs_parts[[input$inp_organ]]
    )

  })

  output$inp_side <- shiny::renderUI({

    data_variables$side$shiny_uiOutput(
      pref = "inp",
      selected = NULL,
      organ = input$inp_organ
    )

  })

  output$inp_workgroup <- shiny::renderUI({

    workgroups <-
      dplyr::filter(df_tissue_donor(), institution == input$inp_institution) %>%
      dplyr::pull(workgroup) %>%
      base::unique()

    data_variables$workgroup$shiny_uiOutput(
      pref = "inp",
      selected = NULL,
      choices = workgroups
    )

  })





# reactive values ---------------------------------------------------------

  complete_df_tissue_donor <- shiny::reactive({

    complete_storage_df(df = df_tissue_donor())

  })

  complete_df_tissue_sample <- shiny::reactive({

    complete_storage_df(
      df = df_tissue_sample(),
      df_list = list(tissue_donor = df_tissue_donor())
      ) %>%
      dplyr::select(
        id_tissue_sample_num,
        dplyr::any_of(info_vars_list[["tissue_sample"]]),
        dplyr::everything()
        )

  })

  complete_df_tissue_portion <- shiny::reactive({

    complete_storage_df(
      df = df_tissue_portion(),
      df_list = list(
        tissue_donor = df_tissue_donor(),
        tissue_sample = df_tissue_sample()
      )
      ) %>%
      dplyr::select(
        id_tissue_portion_num,
        dplyr::any_of(info_vars_list[["tissue_portion"]]),
        dplyr::everything()
      )

  })

  complete_df_raw_data <- shiny::reactive({

    complete_storage_df(
      df = df_raw_data(),
      df_list = list(
        tissue_donor = df_tissue_donor(),
        tissue_sample = df_tissue_sample(),
        tissue_portion = df_tissue_portion()
      )
      ) %>%
      dplyr::select(
        id_raw_data_num,
        dplyr::any_of(info_vars_list[["raw_data"]]),
        dplyr::everything()
      )

  })

  df_raw_data_added_to_project <- shiny::reactive({

    p_ids <- project_ids()

    complete_df_raw_data() %>%
      dplyr::filter(id_raw_data_num %in% {{p_ids}}) %>%
      dplyr::select(id_raw_data_num, assay_trademark)

  })

  edit_data_vars <- shiny::reactive({

    out_list <-
      shiny::reactiveValuesToList(input) %>%
      # select only input from laborga data variables using inp_ prefix
      confuns::lselect(lst = ., dplyr::any_of(stringr::str_c("ed_", data_vars))) %>%
      # remove inp_ prefix to align with actual variable names
      confuns::lrename_with(.fn = ~ stringr::str_remove(string = .x, pattern = "^ed_")) %>%
      # ensure validity regarding class and content
      purrr::imap(.f = ensure_data_var_validity)

    return(out_list)

  })

  folder_organization <- shiny::reactive({

    hlpr_order_input(input$folder_orga_sorted)

  })

  input_data_vars <- shiny::reactive({

    out_list <-
      shiny::reactiveValuesToList(input) %>%
      # select only input from laborga data variables using inp_ prefix
      confuns::lselect(lst = ., dplyr::any_of(stringr::str_c("inp_", data_vars))) %>%
      # remove inp_ prefix to align with actual variable names
      confuns::lrename_with(.fn = ~ stringr::str_remove(string = .x, pattern = "^inp_")) %>%
      # ensure validity regarding class and content
      purrr::imap(.f = ensure_data_var_validity)

    return(out_list)

  })

  input_tissue_donor <- shiny::reactive({

    out <-
      list(
        id = confuns::lselect(input_data_vars(), dplyr::any_of(get_id_vars("tissue_donor", level_spec = TRUE))),
        info = confuns::lselect(input_data_vars(), dplyr::any_of(get_info_vars("tissue_donor", level_spec = TRUE)))
      )

    return(out)

  })

  input_tissue_sample <- shiny::reactive({

    list(
      id = confuns::lselect(input_data_vars(), dplyr::any_of(get_id_vars("tissue_sample", level_spec = TRUE))),
      info = confuns::lselect(input_data_vars(), dplyr::any_of(get_info_vars("tissue_sample", level_spec = TRUE)))
    )

  })

  # multiple tissue portions can be added at once
  # input$... is suffixed with _n where n corresponds to the nth tissue portion
  input_tissue_portion <- shiny::reactive({

    tissue_portion_list <-
      shiny::reactiveValuesToList(input) %>%
      confuns::lselect(lst = ., dplyr::matches(shiny_tissue_portion_regex()))

    # creates a list of lists where each sub list is equal in it structure to the other
    # data level input lists.
    purrr::map(
      .x = 1:input$n_tissue_portions,
      .f = function(n){

        tp_input <-
          confuns::lselect(
            lst = tissue_portion_list,
            dplyr::matches(shiny_tissue_portion_regex(pattern = n))
            ) %>%
          confuns::lrename_with(.fn = ~ stringr::str_remove_all(.x, pattern = "^inp_|_[0-9]*$"))

        out <- list(
          id = confuns::lselect(tp_input, dplyr::any_of(get_id_vars("tissue_portion", level_spec = TRUE))),
          info = confuns::lselect(tp_input, dplyr::any_of(get_info_vars("tissue_portion", level_spec = TRUE)))
        )

        return(out)

      }
    )

  })

  input_raw_data <- shiny::reactive({

    list(
      id = confuns::lselect(input_data_vars(), dplyr::any_of(get_id_vars("raw_data", level_spec = TRUE))),
      info = confuns::lselect(input_data_vars(), dplyr::any_of(get_info_vars("raw_data", level_spec = TRUE)))
    )

  })

  prel_dir <- shiny::reactive({

    combine_path(input$choose_storage_folder)

  })

  project_dir <- shiny::reactive({

    stringr::str_c(prel_dir(), input$project_name, sep = "\\")

  })

  selected_tissue_donor <- shiny::reactiveVal(value = "")
  selected_tissue_sample <- shiny::reactiveVal(value = "")
  selected_tissue_portion <- shiny::reactiveVal(value = "")
  selected_raw_data <- shiny::reactiveVal(value = "")

# eventReactives ----------------------------------------------------------

  filtered_df_tissue_donor <- shiny::eventReactive(c(input$apply_filter_tissue_donor,
                                                     complete_df_tissue_donor()), {

    input_list <- shiny::reactiveValuesToList(input)

    filter_storage_table(
      df = complete_df_tissue_donor(),
      input = input_list
    )

  }, ignoreNULL = FALSE)

  filtered_df_tissue_sample <- shiny::eventReactive(c(input$apply_filter_tissue_sample,
                                                      complete_df_tissue_sample()), {

    input_list <- shiny::reactiveValuesToList(input)

    filter_storage_table(
      df = complete_df_tissue_sample(),
      input = input_list
    )

  }, ignoreNULL = FALSE)

  filtered_df_tissue_portion <- shiny::eventReactive(c(input$apply_filter_tissue_portion,
                                                       complete_df_tissue_portion()), {

    input_list <- shiny::reactiveValuesToList(input)

    filter_storage_table(
      df = complete_df_tissue_portion(),
      input = input_list
    )

  }, ignoreNULL = FALSE)

  filtered_df_raw_data <- shiny::eventReactive(c(input$apply_filter_raw_data,
                                                 complete_df_raw_data()), {

    input_list <- shiny::reactiveValuesToList(input)

    filter_storage_table(
      df = complete_df_raw_data(),
      input = input_list
    )

  }, ignoreNULL = FALSE)

  filtered_df_raw_data_projects <- shiny::eventReactive(c(input$apply_filter_raw_data_projects,
                                                          complete_df_raw_data()), {

    input_list <- shiny::reactiveValuesToList(input)

    filter_storage_table(
      df = complete_df_raw_data(),
      input = input_list,
      suff = "projects"
    )

  }, ignoreNULL = FALSE)


# observers ---------------------------------------------------------------

  # selected data entries
  # tissue donor
  o <- shiny::observe({

    df <- shiny::isolate(df_tissue_donor())

    df[input$filtered_data_table_tissue_donor_rows_selected,] %>%
      dplyr::pull(var = "id_tissue_donor_num") %>%
      selected_tissue_donor()

  })

  o <- shiny::observe({

    df <- shiny::isolate(df_tissue_donor())

    df[input$table_all_tissue_donor_rows_selected,] %>%
      dplyr::pull(var = "id_tissue_donor_num") %>%
      selected_tissue_donor()

  })

  # tissue sample
  o <- shiny::observe({

    df <- shiny::isolate(df_tissue_sample())

    df[input$filtered_data_table_tissue_sample_rows_selected,] %>%
      dplyr::pull(var = "id_tissue_sample_num") %>%
      selected_tissue_sample()

  })

  o <- shiny::observe({

    df <- shiny::isolate(df_tissue_sample())

    df[input$table_all_tissue_sample_rows_selected,] %>%
      dplyr::pull(var = "id_tissue_sample_num") %>%
      selected_tissue_sample()

  })

  # tissue portion
  o <- shiny::observe({

    df <- shiny::isolate(df_tissue_portion())

    df[input$filtered_data_table_tissue_portion_rows_selected,] %>%
      dplyr::pull(var = "id_tissue_portion_num") %>%
      selected_tissue_portion()

  })

  o <- shiny::observe({

    df <- shiny::isolate(df_tissue_portion())

    df[input$table_all_tissue_portion_rows_selected,] %>%
      dplyr::pull(var = "id_tissue_portion_num") %>%
      selected_tissue_portion()

  })

  # raw data
  o <- shiny::observe({

    df <- shiny::isolate(df_raw_data())

    df[input$filtered_data_table_raw_data_rows_selected, ] %>%
      dplyr::pull(var = "id_raw_data_num") %>%
      selected_raw_data()

  })

  o <- shiny::observe({

    df <- shiny::isolate(df_raw_data())

    df[input$table_all_raw_data_rows_selected, ] %>%
      dplyr::pull(var = "id_raw_data_num") %>%
      selected_raw_data()

  })


  # tester
  ot <- shiny::observe({

  })

  ot <- shiny::observe({

    req(input$tissue_donor_workgroup_filter_opt)

  })

# observe events ----------------------------------------------------------

  oe <- shiny::observeEvent(c(input$add_tissue_donor, input$add_tissue_donor_bt), {

    # nothing of origin must be selected as tissue donor is first data level
    active_d_level("tissue_donor")

    htmlModalAddTissueDonor(df = df_tissue_donor())

  }, ignoreInit = TRUE)

  oe <- shiny::observeEvent(c(input$add_tissue_sample, input$add_tissue_sample_bt), {

    # start with selecting the tissue donor of origin
    # selection modal contains input$move_ahead_to_add_tissue_sample action button
    htmlModalSelectTissueDonor(action = "add")

  }, ignoreInit = TRUE)

  oe <- shiny::observeEvent(c(input$add_tissue_portions, input$add_tissue_portion_bt), {

    # start with selecting the tissue sample of origin
    htmlModalSelectTissueSample(action = "add")

  }, ignoreInit = TRUE)

  oe <- shiny::observeEvent(c(input$add_raw_data, input$add_raw_data_bt), {

    # start with selecting the tissue portion of origin
    htmlModalSelectTissuePortion(action = "add")

  }, ignoreInit = TRUE)

  oe <- shiny::observeEvent(input$move_ahead_to_add_tissue_sample, {

    checkpoint(
      evaluate = shiny::isTruthy(selected_tissue_donor()),
      case_false = "no_entry_selected"
    )

    active_d_level("tissue_sample")

    htmlModalAddTissueSample(
      selected_id = selected_tissue_donor(),
      df = df_tissue_sample()
    )

  }, ignoreInit = TRUE)

  # first, number of tissue portions must be declared
  oe <- shiny::observeEvent(input$move_ahead_to_add_tissue_portion, {

    checkpoint(
      evaluate = shiny::isTruthy(selected_tissue_sample()),
      case_false = "no_entry_selected"
    )

    htmlDetermineNumberOfTissuePortions()

  }, ignoreInit = TRUE)

  oe <- shiny::observeEvent(input$continue_with_number_of_tissue_portions, {

    checkpoint(
      evaluate = input$n_tissue_portions != 0,
      case_false = "zero_tissue_portions"
      )

    active_d_level("tisse_portion")

    htmlModalAddTissuePortion(
      selected_id = selected_tissue_sample(),
      df = df_tissue_portion(),
      n_portions = shiny::isolate(input$n_tissue_portions)
    )

  }, ignoreInit = TRUE)

  oe <- shiny::observeEvent(input$move_ahead_to_add_raw_data, {

    checkpoint(
      evaluate = shiny::isTruthy(selected_tissue_portion()),
      case_false = "no_entry_selected"
    )

    active_d_level("raw_data")

    htmlModalAddRawData(
      selected_id = selected_tissue_portion(),
      df = df_raw_data()
    )

  }, ignoreInit = TRUE)

  oe <- shiny::observeEvent(input$make_entry_tissue_donor, {

    old_ids <- df_tissue_donor()[["id_tissue_donor_num"]]

    out <-
      make_entry(
        df = df_tissue_donor(),
        input_id_vars = input_tissue_donor()$id,
        input_info_vars = input_tissue_donor()$info,
        prev_id_merged = NULL, # no prev id at data level 1
        save = save_tables, # overwrites the data file on the harddrive
        in_shiny = TRUE
      )

    new_ids <- out[["id_tissue_donor_num"]]

    selected_tissue_donor(new_ids[!new_ids %in% old_ids])

    htmlModalHowToProceed(
      d_level = "tissue_donor",
      selected_id = selected_tissue_donor(),
      prev_selected_id = NULL
    )

    df_tissue_donor(out)

  }, ignoreInit = TRUE)

  oe <- shiny::observeEvent(input$make_entry_tissue_sample, {

    old_ids <- df_tissue_sample()[["id_tissue_sample_num"]]

    out <- make_entry(
      df = df_tissue_sample(),
      input_id_vars = input_tissue_sample()$id,
      input_info_vars = input_tissue_sample()$info,
      prev_id_merged = selected_tissue_donor(),
      save = save_tables, # overwrites the data file on the harddrive
      in_shiny = TRUE
    )

    new_ids <- out[["id_tissue_sample_num"]]

    selected_tissue_sample(new_ids[!new_ids %in% old_ids])

    htmlModalHowToProceed(
      d_level = "tissue_sample",
      selected_id = selected_tissue_sample(),
      prev_selected_id = selected_tissue_donor()
    )

    df_tissue_sample(out)

  }, ignoreInit = TRUE)

  oe <- shiny::observeEvent(input$make_entry_tissue_portion, {

    active_d_level("tissue_portion")

    for(i in base::seq_along(input_tissue_portion())){

      old_ids <- df_tissue_portion()[["id_tissue_portion_num"]]

      out <- make_entry(
        df = df_tissue_portion(),
        input_id_vars = input_tissue_portion()[[i]]$id,
        input_info_vars = input_tissue_portion()[[i]]$info,
        prev_id_merged = selected_tissue_sample(),
        save = save_tables, # overwrites the data file on the harddrive
        in_shiny = TRUE
      )

      new_ids <- out[["id_tissue_portion_num"]]

      selected_tissue_portion(new_ids[!new_ids %in% old_ids])

      df_tissue_portion(out)

    }

    htmlModalHowToProceed(
      d_level = "tissue_portion",
      selected_id = selected_tissue_portion(),
      prev_selected_id = selected_tissue_sample(),
      # only allow if one tissue portion was added
      allow_move_ahead = base::length(input_tissue_portion()) == 1
    )

  }, ignoreInit = TRUE)

  oe <- shiny::observeEvent(input$make_entry_raw_data, {

    out <- make_entry(
      df = df_raw_data(),
      input_id_vars = input_raw_data()$id,
      input_info_vars = input_raw_data()$info,
      prev_id_merged = selected_tissue_portion(),
      save = save_tables, # overwrites the data file on the harddrive
      in_shiny = TRUE
    )

    htmlModalHowToProceed(
      d_level = "raw_data",
      prev_selected_id = selected_tissue_portion()
    )

    df_raw_data(out)

  }, ignoreInit = TRUE)

  oe <- shiny::observeEvent(input$continue_with_d_level, {

    shiny::removeModal()

    if(active_d_level() == "tissue_donor"){

      htmlModalAddTissueDonor(df = df_tissue_donor())

    } else if(active_d_level() == "tissue_sample"){

      htmlModalAddTissueSample(
        df = df_tissue_sample(),
        selected_id = selected_tissue_donor()
      )

    } else if(active_d_level() == "tissue_portion"){

      htmlDetermineNumberOfTissuePortions()

    } else if(active_d_level() == "raw_data"){

      htmlModalAddRawData(
        df = df_raw_data(),
        selected_id = selected_tissue_portion()
      )

    }

  }, ignoreInit = TRUE)

  oe <- shiny::observeEvent(c(input$edit_tissue_donor), {

    htmlModalSelectTissueDonor(action = "edit")

  }, ignoreInit = TRUE)

  oe <- shiny::observeEvent(c(input$edit_tissue_sample), {

    htmlModalSelectTissueSample(action = "edit")

  }, ignoreInit = TRUE)

  oe <- shiny::observeEvent(c(input$edit_tissue_portion), {

    htmlModalSelectTissuePortion(action = "edit")

  }, ignoreInit = TRUE)

  oe <- shiny::observeEvent(c(input$edit_raw_data), {

    htmlModalSelectRawData(action = "edit")

  }, ignoreInit = TRUE)

  oe <- shiny::observeEvent(c(input$continue_to_edit_tissue_donor, input$edit_tissue_donor_bt), {

    checkpoint(
      evaluate = shiny::isTruthy(selected_tissue_donor()),
      case_false = "no_entry_selected"
    )

    entry <-
      dplyr::filter(
        .data = df_tissue_donor(),
        id_tissue_donor_num == selected_tissue_donor()
      )

    shiny::showModal(
      ui = shiny::modalDialog(
        title = stringr::str_c("Edit tissue donor: ", selected_tissue_donor()),
        size = "xl",
        footer = htmlFooterForEditModals(d_level = "tissue_donor"),
        htmlContainer(
          htmlOrganizeInRows(
            shiny::tagList(
              purrr::map(
                .x = purrr::discard(.x = data_variables[get_info_vars("tissue_donor")], .p = base::is.null),
                .f = ~ .x$shiny_input(
                  pref = "ed",
                  selected = entry[[.x$name]],
                  choices = process_choices(x = df_tissue_donor()[[.x$name]], action = "edit")
                ) %>% htmlCol(width = 4)
              )
            ),
            ncol = 3
          )
        )
      )
    )

  }, ignoreInit = TRUE)

  oe <- shiny::observeEvent(c(input$continue_to_edit_tissue_sample, input$edit_tissue_sample_bt), {

    checkpoint(
      evaluate = shiny::isTruthy(selected_tissue_sample()),
      case_false = "no_entry_selected"
    )

    entry <-
      dplyr::filter(
        .data = df_tissue_sample(),
        id_tissue_sample_num == selected_tissue_sample()
      )

    shiny::showModal(
      ui = shiny::modalDialog(
        title = stringr::str_c("Edit tissue sample: ", selected_tissue_sample()),
        size = "xl",
        footer = htmlFooterForEditModals(d_level = "tissue_sample"),
        htmlContainer(
          htmlOrganizeInRows(
            shiny::tagList(
              purrr::map(
                .x = purrr::discard(
                  .x = data_variables[get_info_vars("tissue_sample")],
                  .p = ~ base::is.null(.x$shiny_input)
                  ),
                .f = ~ .x$shiny_input(
                  pref = "ed",
                  selected = entry[[.x$name]],
                  choices = process_choices(x = df_tissue_sample()[[.x$name]], action = "edit")
                )
              )
            ),
            ncol = 3
          )
        )
      )
    )

  }, ignoreInit = TRUE)

  oe <- shiny::observeEvent(c(input$continue_to_edit_tissue_portion, input$edit_tissue_portion_bt), {

    checkpoint(
      evaluate = shiny::isTruthy(selected_tissue_portion()),
      case_false = "no_entry_selected"
    )

    entry <-
      dplyr::filter(
        .data = df_tissue_portion(),
        id_tissue_portion_num == selected_tissue_portion()
      )

    shiny::showModal(
      ui = shiny::modalDialog(
        title = stringr::str_c("Edit tissue portion: ", selected_tissue_portion()),
        size = "xl",
        footer = htmlFooterForEditModals(d_level = "tissue_portion"),
        htmlContainer(
          htmlOrganizeInRows(
            tag_list = shiny::tagList(
              purrr::map(
                .x = purrr::discard(
                  .x = data_variables[get_info_vars("tissue_portion")],
                  .p = ~ base::is.null(.x$shiny_input)
                  ),
                .f = ~ .x$shiny_input(
                  pref = "ed",
                  nth = NULL,
                  selected = entry[[.x$name]],
                  choices = process_choices(x = df_tissue_portion()[[.x$name]], action = "edit")
                )
              )
            ),
            ncol = 3

          )
        )
      )
    )

  }, ignoreInit = TRUE)

  oe <- shiny::observeEvent(c(input$continue_to_edit_raw_data, input$edit_raw_data_bt), {

    checkpoint(
      evaluate = shiny::isTruthy(selected_raw_data()),
      case_false = "no_entry_selected"
    )

    entry <-
      dplyr::filter(
        .data = df_raw_data(),
        id_raw_data_num == selected_raw_data()
      )

    shiny::showModal(
      ui = shiny::modalDialog(
        title = stringr::str_c("Edit raw data: ", selected_raw_data()),
        size = "xl",
        footer = htmlFooterForEditModals(d_level = "raw_data"),
        htmlContainer(
          htmlOrganizeInRows(
            shiny::tagList(
              purrr::map(
                .x = purrr::discard(
                  .x = data_variables[get_info_vars("raw_data")],
                  .p = ~ base::is.null(.x$shiny_input)
                  ),
                .f = ~ .x$shiny_input(
                  pref = "ed",
                  selected = entry[[.x$name]],
                  choices = process_choices(x = df_raw_data()[[.x$name]], action = "edit")
                )
              )
            ),
            ncol = 3
          )
        )
      )
    )

  }, ignoreInit = TRUE)

  oe <- shiny::observeEvent(input$save_edit_tissue_donor, {

    out <-
      edit_entry(
        df = df_tissue_donor(),
        selected_id = selected_tissue_donor(),
        input_info_vars = shiny::isolate(edit_data_vars()),
        save = save_tables, # overwrites the data file on the harddrive
        in_shiny = TRUE
      )

    df_tissue_donor(out)

    shiny::removeModal()

  }, ignoreInit = TRUE)

  oe <- shiny::observeEvent(input$save_edit_tissue_sample, {

    out <-
      edit_entry(
        df = shiny::isolate(df_tissue_sample()),
        selected_id = shiny::isolate(selected_tissue_sample()),
        input_info_vars = shiny::isolate(edit_data_vars()),
        save = save_tables, # overwrites the data file on the harddrive
        in_shiny = TRUE
      )

    df_tissue_sample(out)

    shiny::removeModal()

  }, ignoreInit = TRUE)

  oe <- shiny::observeEvent(input$save_edit_tissue_portion, {

    out <-
      edit_entry(
        df = shiny::isolate(df_tissue_portion()),
        selected_id = shiny::isolate(selected_tissue_portion()),
        input_info_vars = shiny::isolate(edit_data_vars()),
        save = save_tables, # overwrites the data file on the harddrive
        in_shiny = TRUE
      )

    df_tissue_portion(out)

    shiny::removeModal()

  }, ignoreInit = TRUE)

  oe <- shiny::observeEvent(input$save_edit_raw_data, {

    out <-
      edit_entry(
        df = df_raw_data(),
        selected_id = selected_raw_data(),
        input_info_vars = shiny::isolate(edit_data_vars()),
        save = save_tables, # overwrites the data file on the harddrive
        in_shiny = TRUE
      )

    df_raw_data(out)

    shiny::removeModal()

  }, ignoreInit = TRUE)

  oe <- shiny::observeEvent(input$add_all_to_project, {

    selected_ids <-
      filtered_df_raw_data_projects() %>%
      dplyr::pull(id_raw_data_num)

    selected_ids <- selected_ids[!selected_ids %in% project_ids()]

    n <- base::length(selected_ids)

    checkpoint(
      evaluate = n >= 1,
      case_false = "no_entry_selected_project"
    )

    ref <- confuns::adapt_reference(input = selected_ids, sg = "entry", pl = "entries")

    shiny::showModal(
      ui = shiny::modalDialog(
        title = "Add Raw Data",
        shiny::helpText(
          glue::glue(
            "Do you want to add {n} new raw data {ref} to the data set?"
          )
        ),
        footer =
          shiny::fluidRow(
            htmlCol(
              width = 6,
              htmlMediumButton(inputId = "confirm_add_all_to_project", label = "Yes, add to project")
            ),
            htmlCol(
              width = 6,
              htmlMediumButton(inputId = "close_modal", label = "No, return to project", color = "warning")
            )
          )
      )
    )

  })

  oe <- shiny::observeEvent(input$confirm_add_all_to_project, {

    shiny::removeModal()

    selected_ids <-
      filtered_df_raw_data_projects() %>%
      dplyr::pull(id_raw_data_num)

    selected_ids <- selected_ids[!selected_ids %in% project_ids()]

    n <- base::length(selected_ids)

    ref <- confuns::adapt_reference(input = selected_ids, sg = "entry", pl = "entries")

    confuns::give_feedback(
      msg = glue::glue("Added {n} new raw data {ref} to the data set."),
      verbose = TRUE,
      in.shiny = TRUE,
      with.time = FALSE,
      duration = 15
    )

    all_ids <- c(project_ids(), selected_ids)

    project_ids(all_ids)

  })

  oe <- shiny::observeEvent(input$add_selected_to_project, {

    selected_ids <-
      filtered_df_raw_data_projects()[input$table_all_raw_data_projects_rows_selected, ] %>%
      dplyr::pull(id_raw_data_num)

    selected_ids <- selected_ids[!selected_ids %in% project_ids()]

    n <- base::length(selected_ids)

    checkpoint(
      evaluate = (n >= 1),
      case_false = "no_entry_selected"
    )

    ref <- confuns::adapt_reference(input = selected_ids, sg = "entry", pl = "entries")

    confuns::give_feedback(
      msg = glue::glue("Added {n} new raw data {ref} to the data set."),
      verbose = TRUE,
      in.shiny = TRUE,
      with.time = FALSE,
      duration = 15
    )

    all_ids <- c(project_ids(), selected_ids)

    project_ids(all_ids)

  })

  oe <- shiny::observeEvent(input$remove_selected_from_project, {

    selected_ids <-
      df_raw_data_added_to_project()[input$table_added_to_project_rows_selected, ] %>%
      dplyr::pull(id_raw_data_num)

    n <- base::length(selected_ids)

    checkpoint(
      evaluate = n >= 1,
      case_false = "no_entry_selected"
    )

    ref <- confuns::adapt_reference(input = selected_ids, sg = "entry", pl = "entries")

    confuns::give_feedback(
      msg = glue::glue("Removed {n} raw data {ref} from the data set."),
      verbose = TRUE,
      in.shiny = TRUE,
      with.time = FALSE,
      duration = 15
    )

    p_ids <- project_ids()

    remaining_ids <- p_ids[!p_ids %in% selected_ids]

    project_ids(remaining_ids)

  })

  oe <- shiny::observeEvent(input$create_project, {

    checkpoint(
      evaluate = base::length(project_ids()) >= 1,
      case_false = "no_project_ids"
    )

    checkpoint(
      evaluate = (shiny::isTruthy(prel_dir()) & validate_project_name(input$project_name)),
      case_false = "invalid_project_dir"
    )

    if(base::length(folder_organization()) >= 1){

      subfolders <- stringr::str_c(folder_organization(), collapse = ", ")

    } else {

      subfolders <- "none"

    }

    shiny::showModal(
      ui = shiny::modalDialog(
        title = "Create Project",
        shiny::tags$h5(
          glue::glue("Number of zipfiles: {base::length(project_ids())}")
        ),
        shiny::tags$h5(
          glue::glue("Project directory: {project_dir()}")
        ),
        shiny::tags$h5(
          glue::glue("Subfolders for: {subfolders}")
        ),
        shinyWidgets::materialSwitch(
          inputId = "verbose_dl",
          label = "Inform about progress of download",
          value = TRUE,
          status = "success"
          ) %>%
          shinyBS::popify(
            title = NULL,
            content = descr_tt$verbose_dl,
            placement = "right"
          ),
        shinyWidgets::materialSwitch(
          inputId = "unzip_dl",
          label = "Unzip files automatically",
          value = TRUE,
          status = "success"
          ) %>% shinyBS::popify(
            title = NULL,
            content = descr_tt$unzip_dl,
            placement = "right"
            ),
        shiny::tags$h5("Click 'Confirm' to start the download."),
        footer =
          shiny::fluidRow(
            htmlCol(
              width = 6,
              htmlMediumButton(
                inputId = "confirm_create_project",
                label = "Confirm"
              )
            ),
            htmlCol(
              width = 6,
              htmlMediumButton(
                inputId = "close_modal",
                label = "Go Back",
                color = "warning"
              )
            )
          )
      )
    )

  })

  oe <- shiny::observeEvent(input$confirm_create_project, {

    p_ids <- project_ids()

    # create subsetted df with dir_zip column
    df_prep <-
      dplyr::filter(complete_df_raw_data(), id_raw_data_num %in% {{p_ids}}) %>%
      prepare_df_for_download(
        folder_organization = folder_organization(),
        project_dir = project_dir()
      )

    # prepare the folder and sub folders
    prepare_folder_for_download(
      project_dir = project_dir(),
      folder_organization = folder_organization()
    )

    # is called for side effects (downloads the zip file)
    # feedback list contains information about the downloading process
    feedback_list_dl <-
      download_zip_files(
        df_prep = df_prep,
        folder_organization = folder_organization(),
        project_dir = project_dir(),
        verbose = input$verbose_dl,
        in_shiny = TRUE
      )

    if(input$unzip_dl){

      feedback_list_unzip <-
        unzip_files(
          project_dir = project_dir(),
          verbose = input$verbose_dl,
          in_shiny = TRUE
        )

    }

    # save information
    readr::write_csv(x = df_prep, file = stringr::str_c(project_dir(), "\\raw_data_table.csv") )

    # write .txt file about process using feedback lists
    # ...

    confuns::give_feedback(
      msg = "Project created!",
      verbose = TRUE,
      in.shiny = TRUE
      )

  })


  ### tester
  oe <- shiny::observeEvent(input$test, {

    print("test")

    print(folder_organization())
    print(project_dir())
    print(project_ids())

  })


  ### close modal
  oe <- shiny::observeEvent(input$close_modal, {

    shiny::removeModal()

  })


# outputs -----------------------------------------------------------------


  output$filtered_data_table_tissue_donor <- DT::renderDataTable({

    make_pretty_storage_df(df_tissue_donor())

  }, options = dt_options, selection = list(mode = "single", selected = NULL, target = "row"))

  output$filtered_data_table_tissue_portion <- DT::renderDataTable({

    make_pretty_storage_df(df_tissue_portion())

  }, options = dt_options, selection = list(mode = "single", selected = NULL, target = "row"))

  output$filtered_data_table_tissue_sample <- DT::renderDataTable({

    make_pretty_storage_df(df_tissue_sample())

  }, options = dt_options, selection = list(mode = "single", selected = NULL, target = "row"))

  output$filtered_data_table_raw_data <- DT::renderDataTable({

    make_pretty_storage_df(df_raw_data())

  }, options = dt_options, selection = list(mode = "single", selected = NULL, target = "row"))


  output$table_all_tissue_donor <- DT::renderDataTable({

    make_pretty_storage_df(filtered_df_tissue_donor())

  },
  options = list(pageLength = 15, scrollX = TRUE, scrollY = TRUE),
  selection = list(mode = "single", selected = NULL, target = "row")
  )

  output$table_all_tissue_sample <- DT::renderDataTable({

    make_pretty_storage_df(filtered_df_tissue_sample())

  },
  options = list(pageLength = 10, scrollX = TRUE, scrollY = TRUE),
  selection = list(mode = "single", selected = NULL, target = "row")
  )

  output$table_all_tissue_portion <- DT::renderDataTable({

    make_pretty_storage_df(filtered_df_tissue_portion())

  },
  options = list(pageLength = 10, scrollX = TRUE, scrollY = TRUE),
  selection = list(mode = "single", selected = NULL, target = "row")
  )

  output$table_all_raw_data <- DT::renderDataTable({

    make_pretty_storage_df(filtered_df_raw_data())

  },
  options = list(pageLength = 10, scrollX = TRUE, scrollY = TRUE),
  selection = list(mode = "single", selected = NULL, target = "row")
  )

  output$table_all_raw_data_projects <- DT::renderDataTable({

    make_pretty_storage_df(filtered_df_raw_data_projects())

  },
  options = list(pageLength = 10, scrollX = TRUE, scrollY = TRUE, searching = FALSE, lengthChange = FALSE),
  selection = list(mode = "multiple", selected = NULL, target = "row")
  )

  output$table_added_to_project <- DT::renderDataTable({

    shiny::validate(
      shiny::need(
        expr = base::nrow(df_raw_data_added_to_project()) >= 1,
        message = "No data entries added yet."
      )
    )

    df_raw_data_added_to_project() %>%
      dplyr::rename(`ID Raw Data` = id_raw_data_num, `Assay Trademark` = assay_trademark)

  },
  options = list(pageLength = 30, scrollX = TRUE, scrollY = TRUE, searching = FALSE, lengthChange = F),
  selection = list(mode = "multiple", selected = NULL, target = "row"))


  output$chosen_storage_folder <- shiny::renderText({

    shiny::validate(
      shiny::need(
        expr = shiny::isTruthy(prel_dir()),
        message = "No storage folder chosen."
      )
    )

    shiny::validate(
      shiny::need(
        expr = validate_project_name(input$project_name),
        message = "Invalid or missing project name."
      )
    )

    shiny::validate(
      shiny::need(
        expr = !base::dir.exists(project_dir()),
        message = glue::glue("Directory {project_dir()} already exists.")
      )
    )

    project_dir()

  })

}
