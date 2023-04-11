

htmlMakeFilterModule <- function(d_level, df, complete = TRUE){

  d_levels <- data_levels[1:dln(d_level)]

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

  shiny_input_sorted <-
    purrr::map(
      .x = dvars_by_dlevel,
      .f = ~ purrr::map(
        .x = .x,
        .f = function(dvar){

          if(dvar$class %in% c("character", "factor", "list_character")){

            filter_content <-
              shiny::selectInput(
                inputId = stringr::str_c(d_level, dvar$name, "filter", sep = "_"),
                label = dvar$label,
                choices = process_choices(x = df[[dvar$name]], action = "filter"),
                multiple = TRUE
              ) %>%
              htmlPopify(var = dvar$name)

          } else if(dvar$class %in% c("date", "numeric")){

            r <- base::range(df[[dvar$name]])

            filter_content <-
              shiny::sliderInput(
                inputId = stringr::str_c(d_level, dvar$name, "filter", sep = "_"),
                label = dvar$label,
                min = r[1],
                max = r[2],
                value = r
              ) %>%
              htmlPopify(var = dvar$name)

          }

          filter_opt <-
            shinyWidgets::radioGroupButtons(
              inputId = stringr::str_c(d_level, dvar$name, "filter", sep = "_"),
              label = "Label",
              choices = c("Keep", "Remove"),
              status = "primary",
              checkIcon = list(
                yes = shiny::icon("ok", lib = "glyphicon"),
                no = shiny::icon("remove", lib = "glyphicon")
              )
            )

          htmlContainer(
            filter_content,
            filter_opt
          )

        }
      )
    ) %>%
    purrr::map(.f = ~ htmlOrganizeInColumns(tag_list = shiny::tagList(.x), ncol = 4)) %>%
    purrr::imap(
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

  return(shiny_input_sorted)

}




