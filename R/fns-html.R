



# A -----------------------------------------------------------------------

htmlActionButtonsBelowTables <- function(d_level){

  shiny::fluidRow(
    shiny::tagList(
      purrr::map2(
        .x = c("add", "view", "edit", "delete"),
        .y = c("success", "success", "success", "warning"),
        .f = function(action, color){

          if(action != "add"){

            insert <- " select the corresponding row in the table and "

          } else {

            insert <- ""

          }

          htmlCol(
            width = 3,
            htmlMediumButton(
              inputId = stringr::str_c(action, d_level, "bt", sep = "_"),
              label = stringr::str_c(action, d_level, sep = "_") %>%
                confuns::make_pretty_name(),
              color = color,
              text = NULL #glue::glue("To {action} a {dlp(d_level)}{insert}click here.")
            )
          )

        }
      )
    )
  )

}

htmlAddHelper <- function(shiny_tag,
                          content,
                          title = "What do I have to do here?",
                          type = "inline",
                          size = "s", ...){

  shinyhelper::helper(
    shiny_tag = shiny_tag,
    content = content,
    title = title,
    size = size,
    type = type,
    ...
  )

}

htmlAddRawData <- function(df){

  html_req_input <-
    shiny::tagList(
      htmlCol(
        width = 6,
        htmlHeadlineReqInput(d_level = "raw_data"),
        shiny::tagList(
          purrr::map(
            .x = required_vars_list[["raw_data"]],
            .f = ~ data_variables[[.x]][["shiny_input"]](
              pref = "inp",
              selected = NULL,
              choices = process_choices(x = df[[.x]], action = "add", type = "required")
            )
          )
        )
      )
    )

  html_opt_input <-
    shiny::tagList(
      htmlCol(
        width = 6,
        htmlHeadlineOptInput(d_level = "raw_data"),
        purrr::map(
          .x = purrr::discard(
            .x = data_variables[optional_vars_list[["raw_data"]]],
            .p = base::is.null
            ),
          .f = ~ .x[["shiny_input"]](
            pref = "inp",
            selected = NULL,
            choices = process_choices(x = df[[.x$name]], action = "add", type = "optional")
          )
        )
      )
    )

  shiny::tagList(
    shiny::fluidRow(
      html_req_input,
      html_opt_input
    )
  )

}



htmlAddTissueDonor <- function(df){

  # id input
  # all donor id vars are factors
  html_id_input <-
    shiny::tagList(
      htmlCol(
        width = 4,
        htmlHeadlineIdInput(d_level = "tissue_donor"),
        purrr::map(
          .x = id_vars_list[["tissue_donor"]],
          .f = ~ data_variables[[.x]][["shiny_input"]](
            pref = "inp",
            selected = NULL,
            choices = process_choices(df[[.x]], action = "add", type = "id")
          )
        )
      )
    )

  # required input
  html_req_input <-
    shiny::tagList(
      htmlCol(
        width = 4,
        htmlHeadlineReqInput(d_level = "tissue_donor"),
        purrr::map(
          .x = required_vars_list[["tissue_donor"]],
          .f = ~ data_variables[[.x]][["shiny_input"]](
            pref = "inp",
            selected = NULL,
            choices = process_choices(df[[.x]], action = "add", type = "required")
          )
        )
      )
    )

  # in future: add Optional, clinical variables (comorbidities, treatment, survival etc.)
  # html_opt_input <- shiny::tagList(...)
  html_opt_input <-
    shiny::tagList(
      htmlCol(
        width = 4,
        htmlHeadlineOptInput(d_level = "tissue_donor"),
        purrr::map(
          .x = optional_vars_list[["tissue_donor"]],
          .f = ~ data_variables[[.x]][["shiny_input"]](
            pref = "inp",
            selected = NULL,
            choices = process_choices(df[[.x]], action = "add", type = "optional")
          )
        )
      )
    )

  # output
  shiny::tagList(
    shiny::fluidRow(
      html_id_input,
      html_req_input,
      html_opt_input
    )
  )

}


htmlAddTissuePortion <- function(df, nth){

  html_req_input <- shiny::tagList(
    htmlCol(
      width = 6,
      htmlHeadlineReqInput(d_level = "tissue_portion"),
      data_variables$storage_mode$shiny_input(pref = "inp", nth = nth),
      data_variables$date_of_creation$shiny_input(pref = "inp", selected = "1900-01-01", nth = nth)
    )
  )

  html_opt_input <-
    shiny::tagList(
      htmlCol(
        width = 6,
        htmlHeadlineOptInput(d_level = "tissue_portion"),
        shiny::fluidRow(
          htmlCol(
            width = 6,
            data_variables$storage_size$shiny_input(pref = "inp", nth = nth)
          ),
          htmlCol(
            width = 6,
            data_variables$storage_unit$shiny_input(pref = "inp", nth = nth)
          )
        ),
        data_variables$preparateur_tissue_portion$shiny_input(
          pref = "inp",
          nth = nth,
          choices = process_choices(df[["preparateur_tissue_portion"]], action = "add", type = "optional")
          )
      )
    )

  html_opt_input <-
    shiny::tagList(
      htmlCol(
        width = 6,
        shiny::tagList(
          htmlHeadlineOptInput(d_level = "tissue_portion"),
          shiny::fluidRow(
            htmlCol(
              width = 6,
              data_variables$storage_size$shiny_input(pref = "inp", nth = nth)
            ),
            htmlCol(
              width = 6,
              data_variables$storage_unit$shiny_input(pref = "inp", nth = nth)
            )
          )
        ),
        shiny::tagList(
          htmlOrganizeInRows(
            ncol = 2,
            breaks = 0,
            tag_list = purrr::map(
              .x = confuns::vselect(optional_vars_list[["tissue_portion"]], -storage_size, -storage_unit),
              .f = ~ data_variables[[.x]][["shiny_input"]](
                pref = "inp",
                selected = NULL,
                choices = get_choices(df[[data_variables[[.x]]$name]]),
                nth = nth
              )
            )
          )
        )
      )
    )

  shinydashboard::box(
    title = h2(stringr::str_c("Tissue portion ", nth)),
    collapsible = FALSE,
    solidHeader = TRUE,
    width = 12,
    shiny::tagList(
      html_req_input,
      html_opt_input
    )
  )

}


htmlAddTissueSample <- function(df){

  # id vars
  html_id_input <-
    shiny::tagList(
      htmlCol(
        width = 4,
        htmlHeadlineIdInput(d_level = "tissue_sample"),
        data_variables$date_of_extraction$shiny_input(pref = "inp", selected = "1900-01-01")
      )
    )

  # req vars
  html_req_input <-
    shiny::tagList(
      htmlCol(
        width = 4,
        htmlHeadlineReqInput(d_level = "tissue_sample"),
        shiny::tagList(
          purrr::map(
            .x = required_vars_list[["tissue_sample"]],
            .f = ~ data_variables[[.x]][["shiny_input"]](
              pref = "inp",
              selected = NULL,
              choices = process_choices(x = df[[.x]], action = "add")
            )
          )
        )
      )
    )

  # opt vars
  html_opt_input <-
    shiny::tagList(
      htmlCol(
        width = 4,
        htmlHeadlineOptInput(d_level = "tissue_sample"),
        shiny::tagList(
          purrr::map(
            .x = optional_vars_list[["tissue_sample"]],
            .f = ~ data_variables[[.x]][["shiny_input"]](
              pref = "inp",
              selected = NULL,
              choices = process_choices(x = df[[.x]], action = "add")
            )
          )
        )
      )
    )

  # output
  shiny::tagList(
    shiny::fluidRow(
      html_id_input,
      html_req_input,
      html_opt_input
    )
  )

}


htmlApplyFilter <- function(d_level){

  shinyWidgets::materialSwitch(
    inputId = "apply_filter_tissue_sample",
    label = "Apply Filter:",
    value = FALSE,
    status = "success"
    )

}



# B -----------------------------------------------------------------------

htmlBreak <- function(n){

  shiny::HTML(stringr::str_c(base::rep("<br>", n), collapse = ""))

}

htmlButtonCloseModal <- function(){

  htmlMediumButton(inputId = "close_modal", color = "warning", label = "Close")

}



# C -----------------------------------------------------------------------

htmlCol <- function(width, ...,  align = "center", offset = 0){

  shiny::column(width = width, ..., align = align, offset = offset)

}

htmlContainer <- function(..., width = 12, align = "center"){

  shiny::fluidRow(
    shiny::column(
      width = width,
      align = align,
      ...
    )
  )

}



# D -----------------------------------------------------------------------

htmlDetermineNumberOfTissuePortions <- function(){

  shiny::showModal(
    ui = shiny::modalDialog(
      title = "How many tissue portions do you want to add?",
      footer = shiny::tagList(
        shiny::fluidRow(
          htmlCol(width = 3),
          htmlCol(
            width = 3,
            htmlMediumButton(
              inputId = "continue_with_number_of_tissue_portions",
              label = "Continue"
            )
          ),
          htmlCol(
            width = 3,
            htmlButtonCloseModal()
          ),
          htmlCol(width = 3)
        )
      ),
      size = "xl",
      shiny::numericInput(
        inputId = "n_tissue_portions",
        label = "Number of tissue portions:",
        value = 0,
        min = 1,
        step = 1,
        max = 5
      ),
      shiny::helpText(
        "After specifying the number of portions you are prompted to enter
         detailed information for each portion you want to store."
      )
    )
  )

}


# F -----------------------------------------------------------------------

htmlFeedback <- function(msg, ...){

  confuns::give_feedback(
    msg = msg,
    in.shiny = TRUE,
    verbose = TRUE,
    ...
  )

}

htmlFilteredDataTable <- function(d_level){

  htmlContainer(
    DT::dataTableOutput(
      outputId = stringr::str_c("filtered_data_table_", d_level)
    )
  )

}


htmlFilterButtons <- function(d_level, suff = NULL){

  shiny::fluidRow(
    htmlCol(width = 4),
    htmlCol(
      width = 2,
      shinyWidgets::actionBttn(
        inputId = stringr::str_c("apply_filter", d_level, suff, sep = "_"),
        label = "Apply Filter",
        style = "material-flat",
        color = "primary",
        size = "sm"
      )
    ),
    htmlCol(
      width = 2,
      shinyWidgets::actionBttn(
        inputId = stringr::str_c("reset_filter", d_level, suff, sep = "_"),
        label = "Reset Filter",
        style = "material-flat",
        color = "primary",
        size = "sm"
      )
    ),
    htmlCol(width = 4)
  )

}

htmlFilterOptions <- function(df, suff = NULL){

  d_level <- get_d_level(df)

  d_levels <- data_levels[dln(d_level):1]

  dvars <-
    purrr::keep(
      .x = data_variables,
      .p = ~ .x$d_level %in% {{d_levels}} & base::isTRUE(.x$filter)
    )

  dvars_by_dlevel <-
    purrr::map(
      .x = d_levels,
      .f = function(dl){

        purrr::keep(.x = dvars, .p = ~ .x$d_level == {{dl}})

      }
    ) %>%
    purrr::set_names(nm = d_levels)

  filter_input_sorted <-
    purrr::map(
      .x = dvars_by_dlevel,
      .f = ~ purrr::map(
        .x = .x,
        .f = function(dvar){

          if(dvar$class %in% c("character", "factor", "list_character")){

            choices <- process_choices(x = df[[dvar$name]], action = "filter")

            filter_content <-
              shinyWidgets::pickerInput(
                inputId = stringr::str_c("filter_crit", d_level, dvar$name, suff, sep = "_"),
                label = dvar$label,
                choices = choices,
                selected = choices,
                multiple = TRUE,
                options = list(`actions-box` = TRUE, `live-search` = TRUE)
              ) %>%
              htmlPopify(var = dvar$name)

            #width <- 3

          } else if(dvar$class %in% c("date", "numeric")){

            r <- base::range(df[[dvar$name]], na.rm = TRUE)

            if(dvar$name %in% c("pub_year")){

              step <- 1

            } else {

              step <- NULL

            }

            filter_content <-
              shiny::sliderInput(
                inputId = stringr::str_c("filter_crit", d_level, dvar$name, suff, sep = "_"),
                label = dvar$label,
                min = r[1],
                max = r[2],
                value = r
              ) %>%
              htmlPopify(var = dvar$name)

            #width <- 12

          }

          filter_opt <-
            shinyWidgets::radioGroupButtons(
              inputId = stringr::str_c("filter_opt", d_level, dvar$name, suff, sep = "_"),
              label = NULL,
              size = "sm",
              selected = "Ignore",
              choices = c("Ignore", "Keep", "Discard"),
              status = "default",
              checkIcon = list(
                yes = shiny::icon("ok", lib = "glyphicon"),
                no = shiny::icon("remove", lib = "glyphicon")
              )
            )

          htmlCol(
            width = 12,
            filter_content,
            filter_opt
          )

        }
      )
    ) %>%
    purrr::map(
      .f = function(html_list){

        select_names <-
          purrr::keep(data_variables, .p = ~ .x$class %in% c("character", "factor", "list_character")) %>%
          base::names()

        select_inputs <-
          htmlOrganizeInRows(
            tag_list = confuns::lselect(html_list, dplyr::any_of(select_names)),
            ncol = 4,
            breaks = 0
          )

        numeric_names <-
          purrr::keep(data_variables, .p = ~ .x$class %in% c("numeric", "date")) %>%
          base::names()

        numeric_input <-
          htmlOrganizeInRows(
            tag_list = confuns::lselect(html_list, dplyr::any_of(numeric_names)),
            ncol = 1,
            breaks = 0
          )

        shiny::tagList(
          shiny::fluidRow(
            htmlCol(width = 8, select_inputs),
            htmlCol(width = 4, numeric_input)
          )
        )

      }
    ) %>%
    purrr::imap(
      .x = .,
      .f = function(html, dl){

        shiny::tagList(
          shiny::column(
            width = 12,
            htmlH3(stringr::str_c("Level: ", dlp(dl, cap = TRUE))),
            html
            )
          )

      }
    ) %>%
    purrr::flatten()

  shiny::tagList(
    htmlFilterButtons(d_level = d_level, suff = suff),
    filter_input_sorted
  )

}


htmlFooterForAddModals <- function(d_level){

  inputId <- stringr::str_c("make_entry_", d_level)
  label <-
    stringr::str_c("add_", d_level) %>%
    confuns::make_pretty_name()

  shiny::fluidRow(
    htmlCol(width = 1),
    htmlCol(
      width = 5,
      htmlMediumButton(inputId = inputId, label = label)
    ),
    htmlCol(
      width = 5,
      htmlButtonCloseModal()
    ),
    htmlCol(width = 1)
  )

}

htmlFooterForDeletion <- function(d_level){

  shiny::fluidRow(
    htmlCol(width = 1),
    htmlCol(
      width = 5,
      htmlMediumButton(inputId = stringr::str_c("delete_", d_level, "_confirm"), label = "Delete!", color = "danger")
    ),
    htmlCol(
      width = 5,
      htmlButtonCloseModal()
    ),
    htmlCol(width = 1)
  )

}

htmlFooterForEditModals <- function(d_level){

  shiny::fluidRow(
    htmlCol(
      width = 6,
      htmlMediumButton(inputId = stringr::str_c("save_edit_", d_level), label = "Save")
    ),
    htmlCol(
      width = 6,
      htmlButtonCloseModal()
    )
  )

}

htmlFooterForNextLevelModal <- function(d_level, move_ahead_button = NULL){

  continue_button <-
    htmlMediumButton(inputId = "continue_with_d_level", label = "Continue")

  if(base::is.null(move_ahead_button)){

    shiny::fluidRow(
      htmlCol(
        width = 6,
        continue_button
      ),
      htmlCol(
        width = 6,
        htmlButtonCloseModal()
      )
    )

  } else {

    shiny::fluidRow(
      htmlCol(
        width = 4,
        move_ahead_button
      ),
      htmlCol(
        width = 4,
        continue_button
      ),
      htmlCol(
        width = 4,
        htmlButtonCloseModal()
      )
    )

  }

}

# action = c("add", "edit", "delete")
htmlFooterForSelectModals <- function(d_level, action = "add"){

  nd_level <- data_levels[dln(d_level)+1]

  inputId <- stringr::str_c("move_ahead_to_", action, "_", nd_level)

  shiny::fluidRow(
    htmlCol(width = 1),
    htmlCol(
      width = 5,
      htmlMediumButton(inputId = inputId, label = "Continue")
    ),
    htmlCol(
      width = 5,
      htmlButtonCloseModal()
    ),
    htmlCol(width = 1)
  )

}


# H -----------------------------------------------------------------------


htmlH3 <- function(text){

  shiny::h3(shiny::strong(text))

}

htmlH4 <- function(text){

  shiny::h4(shiny::strong(text))

}

htmlH5 <- function(text){

  shiny::h5(shiny::strong(text))
}

htmlHeadlineIdInput <- function(d_level){

  level <-
    stringr::str_remove(string = d_level, pattern = "tissue") %>%
    stringr::str_remove(string = ., pattern = "_")

  shiny::tagList(
    htmlH4("ID Input:") %>%
      htmlHelperDD(
        title = "What does this mean?",
        content =
          c(
            glue::glue("ID input refers to information about the entry that are required for identification of the {level}."),
            glue::glue("All of the information must be provided!")
          ) %>% base::as.character()
      ),
    htmlBreak(1)
  )

}

htmlHeadlineReqInput <- function(d_level){

  level <-
    stringr::str_remove(string = d_level, pattern = "tissue") %>%
    stringr::str_remove(string = ., pattern = "_")

  shiny::tagList(
    htmlH4("Required Input:") %>%
      htmlHelperDD(
        title = "What does this mean?",
        content =
          c(
            glue::glue(
              "Required input refers to information that must be provided
            although it is not needed for identification of the {level}."
            )
          ) %>% base::as.character()
      ),
    htmlBreak(1)
  )

}

htmlHeadlineOptInput <- function(d_level){

  level <-
    stringr::str_remove(string = d_level, pattern = "tissue") %>%
    stringr::str_remove(string = ., pattern = "_")

  shiny::tagList(
    htmlH4("Optional Input:") %>%
      htmlHelperDD(
        shiny_tag = .,
        content =
          c(
            glue::glue(
              "Optional input refers to information that can be provided while making the {level} entry.",
              "It is not necessary, however, to provide the information as some data often has to be added later
            (e.g. histological classification).",
              "This can be done using the 'Edit {confuns::make_pretty_name(d_level)}' button."
            )
          ) %>% base::as.character()
      ),
    htmlBreak(1)
  )

}

htmlHelper <- function(shiny_tag, content, title = "What input is required?", type = "inline", size = "s", ...){

  shinyhelper::helper(
    shiny_tag = shiny_tag,
    content = content,
    title = title,
    size = size,
    type = type,
    ...)

}

htmlHelperDD <- function(shiny_tag, content, ...){

  # dropdonw buttons appear higher than their neighbours
  if(FALSE){

    shiny::tagList(
      shiny::fluidRow(
        htmlCol(
          width = 1,
          shinyWidgets::dropdownButton(
            size = "xs",
            icon = shiny::icon("question"),
            shiny::helpText(content),
            up = FALSE
          )
        ),
        htmlCol(width = 11, shiny_tag)
      )
    )

  }

  return(shiny_tag)

}


# I -----------------------------------------------------------------------

htmlInvalidPW <- function(test){

  if(base::isTRUE(test)){

    html <-
      shiny::column(
        width = 10,
        align = "center",
        shiny::tags$h5("Wrong password.", style = "color: red")
      )

  } else {

    html <- NULL

  }

  return(html)

}

# M -----------------------------------------------------------------------

htmlMainButton <- function(inputId, color, nb = 1, label = NULL, text = NULL){

  if(base::is.null(label)){

    label <- confuns::make_pretty_name(inputId)

  }

  if(base::is.null(text)){

    shiny::tagList(
      htmlBreak(nb),
      shinyWidgets::actionBttn(
        inputId = inputId,
        label = label,
        color = color,
        style = "gradient",
        size = "lg",
        block = TRUE
      ),
      htmlBreak(nb)
    )

  } else {

    shiny::tagList(
      htmlBreak(nb),
      shinyBS::popify(
        el = shinyWidgets::actionBttn(
          inputId = inputId,
          label = label,
          color = color,
          style = "gradient",
          size = "lg",
          block = TRUE
        ),
        title = "",
        content = text
      ),
      htmlBreak(nb)
    )

  }



}

htmlMediumButton <- function(inputId, label, color = "success", nb = 1, text = NULL, placement = "right"){

  if(base::is.null(text)){

    out <-
      shiny::tagList(
        htmlBreak(nb),
        shinyWidgets::actionBttn(
          inputId = inputId,
          label = label,
          color = color,
          style = "gradient",
          size = "md",
          block = TRUE
        ),
        htmlBreak(nb)
      )

  } else {

    out <-
      shiny::tagList(
        htmlBreak(nb),
        shinyBS::popify(
          el =  shinyWidgets::actionBttn(
            inputId = inputId,
            label = label,
            color = color,
            style = "gradient",
            size = "md",
            block = TRUE
          ),
          title = "",
          content = text,
          placement = placement
        ),
        htmlBreak(nb)
      )

  }

  return(out)

}

htmlModalAddRawData <- function(selected_id, df){

  shiny::showModal(
    ui = shiny::modalDialog(
      title = glue::glue("Add raw data (generated with tissue portion '{selected_id}')"),
      footer = htmlFooterForAddModals(d_level = "raw_data"),
      htmlAddRawData(df = df),
      htmlModalAdjustments()
    )
  )

}

htmlModalAddTissueDonor <- function(df){

  shiny::showModal(
    ui = shiny::modalDialog(
      title = "Add tissue donor",
      footer = htmlFooterForAddModals(d_level = "tissue_donor"),
      size = "xl",
      htmlAddTissueDonor(df = df),
      htmlModalAdjustments()
    )
  )

}

htmlModalAddTissuePortion <- function(selected_id, df, n_portions){

  ref_p <- base::ifelse(n_portions == 1, "portion", "portions")

  shiny::showModal(
    ui = shiny::modalDialog(
      title = glue::glue("Add tissue {ref_p} (of tissue sample '{selected_id}')"),
      footer = htmlFooterForAddModals(d_level = "tissue_portion"),
      size = "xl",
      htmlContainer(
        shiny::tagList(
          purrr::map(
            .x = 1:n_portions,
            .f = function(i){

              shiny::fluidRow(
                htmlAddTissuePortion(df = df, nth = i)
              )

            }
          )
        )
      ),
      htmlModalAdjustments()
    )
  )

}

htmlModalAddTissueSample <- function(selected_id, df){

  shiny::showModal(
    ui = shiny::modalDialog(
      title = glue::glue("Add tissue sample (from tissue donor '{selected_id}')"),
      footer = htmlFooterForAddModals(d_level = "tissue_sample"),
      size = "xl",
      htmlAddTissueSample(df = df),
      htmlModalAdjustments()
    )
  )

}

htmlModalAdjustments <- function(){

  shiny::tagList(
    # allows xl size of modal
    shiny::tags$script(shiny::HTML('
      if (jQuery.fn.tooltip.Constructor.VERSION.startsWith("3.")) {{
        if (document.getElementById("shiny-modal").children[0].classList.contains("modal-xl")) {{
          document.getElementById("shiny-modal").children[0].classList.remove("modal-xl");
          document.getElementById("shiny-modal").children[0].classList.add("modal-lg");
        }};
      }};
     '))
  )

}

htmlModalConnectToRepository <- function(){

  shiny::showModal(
    ui = shiny::modalDialog(
      title = "Repository Missing",
      shiny::helpText(
        "Your current set up of `LabOrga` is not connected to a repository.
         By clicking on 'Connect to Repository' you are forwarded to the file system.
         Choose the folder in which the repository exists you want to connect to or
         an empty folder where a new repository is created."
        ),
      footer =  shiny::fluidRow(
        htmlCol(width = 3, align = "center"),
        htmlCol(
          width = 6,
          align = "center",
          shinyFiles::shinyDirButton(
            id = "connect_to_repo",
            label = "Connect to Repository",
            title = NULL
          )
        ),
        htmlCol(width = 3, align = "center")
      )
    )
  )

}

htmlModalDataEntryView <- function(df, id){

  d_level <- get_d_level(df)

  id_var <- id_vars_merged[dln(d_level)]

  df_selected <-
    dplyr::filter(df, !!rlang::sym(id_var) == {{id}}) %>%
    dplyr::select(dplyr::any_of(base::names(data_variables)))


  var_order <-
    purrr::map_chr(data_variables[base::colnames(df_selected)], .f = ~ .x$label) %>%
    base::order() %>%
    base::unname()


  shiny::showModal(
    ui = shiny::modalDialog(
      title = stringr::str_c(dlp(d_level, cap = TRUE), ": ", id),
      htmlCol(
        width = 12,
        align = "left",
        shiny::tagList(
          purrr::map(
            .x = base::colnames(df_selected)[var_order],
            .f = function(cname){
print(cname)
              val <- df_selected[[cname]]

              if(base::is.na(val) | base::is.null(val) | purrr::is_empty(val)){

                x <- "missing"

              } else {

                x <- base::as.character(df_selected[[cname]])

              }

              x <- stringr::str_c(data_variables[[cname]][["label"]], " ", val, sep = "")

              print(x)

              shiny::fluidRow(
                shiny::tags$p(x)
              )

            }
          )
        )
      ),
      footer = shiny::fluidRow(
        htmlCol(width = 4),
        htmlCol(width = 4, htmlButtonCloseModal()),
        htmlCol(width = 4)
      )
    )
  )

}

htmlModalDeleteEntry <- function(d_level, id){

  if(dln(d_level) != 4){

    dependent_d_levels <-
      data_levels[(dln(d_level)+1):4] %>%
      dlp() %>%
      stringr::str_c(., " entries ") %>%
      confuns::scollapse(sep = ", ", last = " and ")

    text <- glue::glue("Deleting this entry will delete all dependent {dependent_d_levels}, too. This can not be undone.")

  } else {

    text <- "This can not be undone."

  }



  shiny::showModal(
    ui = shiny::modalDialog(
      size = "xl",
      title = glue::glue("Really delete {dlp(d_level)} '{id}'?"),
      footer = htmlFooterForDeletion(d_level = d_level),
      shiny::helpText(text)
    )
  )

}

htmlModalEditTissueDonor <- function(df, selected_id){

  tissue_donor <- dplyr::filter(df, id_tissue_donor_num == {{selected_id}})

  shiny::showModal(
    ui = shiny::modalDialog(
      title = stringr::str_c("Edit tissue donor: ", selected_id),
      size = "xl",
      footer = htmlFooterForEditModals(d_level = "tissue_donor"),
      htmlContainer(
        htmlOrganizeInRows(
          shiny::tagList(
            shiny::dateInput(
              inputId = "ed_date_of_birth",
              label = "Date of birth:",
              value = tissue_donor$date_of_birth
            ),
            shiny::selectInput(
              inputId = "ed_sex",
              label = "Sex:",
              choices = c("unknown", "female", "male"),
              selected = tissue_donor$sex
            )
          ),
          ncol = 3
        )
      )
    )
  )

}

htmlModalHowToProceed <- function(d_level, # the current data level
                                  selected_id = NULL,
                                  prev_selected_id = NULL,
                                  allow_move_ahead = TRUE){

  if(dln(d_level) == 1){

    text1 <- ""

  } else {

    prev_d_level <- data_levels[dln(d_level)-1]

    text1 <- glue::glue(" of {dlp(prev_d_level)} '{prev_selected_id}'")

  }

  # if not last data level allow option move ahead and add new data entries
  # that derive from the just added entry (e.g. allow to add tisse samples that
  # derive from the tissue donor just added)
  if(dln(d_level) != base::length(data_levels) && base::isTRUE(allow_move_ahead)){

    next_d_level <- data_levels[dln(d_level)+1]

    text2 <-
      glue::glue(
        "If you want to proceed with {dlp(next_d_level)} entries for {dlp(d_level)} '{selected_id}'",
        " click on 'Move ahead'."
      )

    move_ahead_button <-
      htmlMediumButton(
        inputId = stringr::str_c("move_ahead_to_add_", next_d_level),
        label = "Move ahead"
      )

  } else {

    text2 <- ""
    move_ahead_button <- NULL

  }

  title <-
    glue::glue("You have added {dlp(d_level)} '{selected_id}'! What now?") %>%
    base::as.character()

  shiny::showModal(
    ui = shiny::modalDialog(
      title = title,
      footer = htmlFooterForNextLevelModal(d_level, move_ahead_button),
      size = "xl",
      shiny::helpText(
        glue::glue(
          "If you want to continue to make {dlp(d_level)} entries{text1}, click on 'Continue'.",
          " {text2}",
          " Else click on 'Close' to return to the menu."
        )
      )
    )
  )

}

htmlModalInvalidRepositoryFolder <- function(dir_to_repo){

  shiny::showModal(
    ui = shiny::modalDialog(
      title = "Invalid Folder",
      shiny::helpText(
        glue::glue(
          "The folder '{dir_to_repo}' is neither empty nor a valid
                repository."
        )
      ),
      footer = shiny::fluidRow(
        htmlCol(width = 3, align = "center"),
        htmlCol(
          width = 6,
          align = "center",
          shinyFiles::shinyDirButton(
            id = "connect_to_repo",
            label = "Connect to Repository",
            title = NULL
          )
        ),
        htmlCol(width = 3, align = "center")
      )
    )
  )

}

htmlModalLogIn <- function(users_df){

  shiny::showModal(
    ui = shiny::modalDialog(
      title = shiny::tags$h2(shiny::strong("Please log in"), style = "padding-top: 0;"),
      footer =
        shiny::fluidRow(
          htmlCol(width = 1, align = "center"),
          htmlCol(width = 5, align = "center", shiny::actionButton(inputId = "login", label = "Login", width = "100%")),
          htmlCol(width = 5, align = "center", shiny::actionButton(inputId = "create_new_user", label = "New User", width = "100%")),
          htmlCol(width = 1, align = "center")
        ),
      easyClose = FALSE,
      size = "xl",
      shinyjs::hidden(
        shiny::div(
          id = "login_panel",
          style = "width: 500px; max-width: 100%; margin: 0 auto; padding: 20px;",
            htmlContainer(
              width = 12,
              align = "left",
              shinyWidgets::pickerInput(
                inputId = "username",
                label = shiny::tagList(shiny::icon("user"), "User Name:"),
                options = shinyWidgets::pickerOptions(
                  title = "Username",
                  liveSearch = TRUE
                ),
                choices = users_df[["username"]],
                width = "100%"
              ),
              shiny::passwordInput(
                inputId = "password",
                label = shiny::tagList(shiny::icon("unlock-alt"), "Password:"),
                placeholder = "Password",
                width = "100%"
              )
            )
        )
      )
    )
  )

}

htmlModalNewUser <- function(){

  shiny::showModal(
    ui = shiny::modalDialog(
      title = "Create new user profile",
      footer = shiny::fluidRow(
        shiny::fluidRow(
          htmlCol(width = 1),
          htmlCol(
            width = 4,
            htmlMediumButton(inputId = "add_new_user", label = "Add User")
          ),
          htmlCol(width = 2),
          htmlCol(
            width = 4,
            htmlMediumButton(inputId = "back_to_login", label = "Close")
          ),
          htmlCol(width = 1)
        )
      ),
      shiny::helpText("To create a new user profile enter your name and you password."),
      shiny::textInput(inputId = "new_username", label = NULL, placeholder = "Username"),
      shiny::passwordInput(inputId = "new_password1", label = NULL, placeholder = "Password"),
      shiny::passwordInput(inputId = "new_password2", label = NULL, placeholder = "Repeat Password")
    )
  )

}

htmlModalSelectTissueDonor <- function(action = "add"){

  shiny::showModal(
    ui = shiny::modalDialog(
      size = "xl",
      title = "Select Tissue Donor Entry",
      footer = htmlFooterForSelectModals(d_level = "tissue_donor", action = action),
      htmlSelectEntryBody(
        d_level_select = "tissue_donor",
        d_level_goal = "tissue_sample",
        action = action
      ),
      htmlFilteredDataTable(d_level = "tissue_donor"),
      htmlModalAdjustments()
    )
  )

}

htmlModalSelectTissuePortion <- function(action = "add"){

  shiny::showModal(
    ui = shiny::modalDialog(
      size = "xl",
      title = "Select Tissue Portion Entry",
      footer = htmlFooterForSelectModals(d_level = "tissue_portion", action = action),
      htmlSelectEntryBody(
        d_level_select = "tissue_portion",
        d_level_goal = "raw_data",
        action = action
      ),
      htmlFilteredDataTable(d_level = "tissue_portion"),
      htmlModalAdjustments()
    )
  )

}

htmlModalSelectTissueSample <- function(action = "add"){

  shiny::showModal(
    ui = shiny::modalDialog(
      size = "xl",
      title = "Select Tissue Sample Entry",
      footer = htmlFooterForSelectModals(d_level = "tissue_sample", action = action),
      htmlSelectEntryBody(
        d_level_select = "tissue_sample",
        d_level_goal = "tissue_portion",
        action = action
      ),
      htmlFilteredDataTable(d_level = "tissue_sample"),
      htmlModalAdjustments()
    )
  )

}

htmlModalSelectRawData <- function(action = "add"){

  shiny::showModal(
    ui = shiny::modalDialog(
      size = "xl",
      title = "Select Raw Data Entry",
      footer = htmlFooterForSelectModals(d_level = "raw_data", action = action),
      htmlSelectEntryBody(
        d_level_select = "raw_data",
        action = action
      ),
      htmlFilteredDataTable(d_level = "raw_data"),
      htmlModalAdjustments()
    )
  )

}



# O -----------------------------------------------------------------------

htmlOrganizeInCols <- function(tag_list, width, spare = FALSE,  ...){

  if(base::length(width) == 1){

    base::stopifnot(12 %% width == 0)

    n <- 12 / width

    width <- base::rep(width, n)[base::seq_along(tag_list)]

  }

  if(base::isTRUE(spare)){

    width <- c(width, (12-base::sum(width)))

    tag_list <- c(tag_list, shiny::HTML(text = ""))

  } else {

    base::stopifnot(base::length(tag_list) == base::length(width))

  }

  base::stopifnot(base::sum(width) <= 12)

  purrr::map2(
    .x = width,
    .y = tag_list,
    .f = function(w, tag, ...){

      htmlCol(width = w, tag, ...)

    }
  ) %>%
    shiny::tagList()

}


htmlOrganizeInRows <- function(tag_list, ncol = 3, breaks = 1){

  n_inputs <- length(tag_list)

  seq_inputs <- base::seq(1, n_inputs, ncol)

  purrr::map(
    .x = seq_inputs,
    .f = function(nth){

      # 1:4, 5:8, etc.
      selected_inputs <-
        purrr::discard(tag_list[nth:(nth+ncol-1)], .p = base::is.null)

      # return tag list of up to four pickers in row
      shiny::tagList(
        shiny::fluidRow(htmlBreak(n = breaks)),
        shiny::fluidRow(
          htmlOrganizeInCols(
            tag_list = selected_inputs,
            width = 12/ncol,
            spare = FALSE
          )
        ),
        shiny::fluidRow(htmlBreak(n = breaks))
      )


    }
  ) %>%
    shiny::tagList()

}



# P -----------------------------------------------------------------------

htmlPopify <- function(el, var = NULL, descr = TRUE, placement = "right", ...){

  if(base::isTRUE(descr)){

    out <-
      shinyBS::popify(
        el = el,
        title = "",
        content = data_variables[[var]][["description"]],
        placement = placement,
        ...
      )

  } else if(base::is.character(descr)){

    out <-
      shinyBS::popify(
        el = el,
        title = "",
        content = descr,
        placement = placement,
        ...
      )

  } else {

    out <- el

  }

  return(out)

}


# S -----------------------------------------------------------------------

htmlSelectEntryBody <- function(d_level_select, d_level_goal = NULL, action = "add"){

  dls <- d_level_select
  dlg <- d_level_goal

  dls_p <- stringr::str_replace_all(dls, pattern = "_", replacement = " ")
  dlg_p <- stringr::str_replace_all(dlg, pattern = "_", replacement = " ")

  id_var <- stringr::str_c("id_", dls, "_num")

  if(action == "add"){

    first_text <- glue::glue("To add a new {dlg_p} it's {dls_p} must be specified first.")

  } else if(action == "edit"){

    first_text <- ""

  }

  shiny::tagList(
    shiny::fluidRow(
      htmlCol(
        width = 12,
        align = "left",
        shiny::helpText(
          glue::glue(
            "{first_text}",
            " Use the search on the right to enter catchphrases or the respective ID to filter the table.",
            " Then select the {dls_p} of interest by clicking on it such that the row turns blue and click on 'Continue'.",
            " If you can't find the {dls_p} among the data return to the menu and add it first."
          )
        )
      )
    ),
    shiny::fluidRow(
      htmlBreak(1)
      )
  )

}


htmlSelectizeInput <- function(variable, multiple = FALSE){

  d_level <- get_d_level(variable)

  df <- get_storage_df(d_level)

  var_content <- df[[variable]]

  if(base::is.character(var_content)){

    choices <- c("", base::unique(var_content))

  } else if(base::is.list(var_content)) {

    choices <-
      purrr::flatten_chr(var_content) %>%
      base::unique() %>%
      base::sort()

  }

  if(variable %in% base::names(specific_pretty_names)){

    label <- specific_pretty_names[[variable]]

  } else {

    label <- confuns::make_pretty_name(string = variable)

  }

  shiny::selectizeInput(
    inputId = stringr::str_c("inp_", variable),
    label = stringr::str_c(label, ":"),
    choices = choices,
    multiple = multiple,
    selected = "",
    options = list(create = TRUE)
  ) %>%
    htmlHelperDD(content = description_data_table_vars[[variable]])

}


# T -----------------------------------------------------------------------

htmlTextInput <- function(variable, ncol){

  shiny::column(
    width = 12/ncol,
    shiny::tags$h5(shiny::strong(variable)) %>% htmlHelperDD(content = variable_info[[variable]]),
    shiny::textInput(
      inputId = variable,
      label = NULL,
      width = "100%"
    )
  )

}



# U -----------------------------------------------------------------------
