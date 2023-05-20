

launchLabOrgaUI <- function(){

  shinydashboard::dashboardPage(

    shinydashboard::dashboardHeader(title = "LabOrga"),

    shinydashboard::dashboardSidebar(
      collapsed = FALSE,
      #shinydashboard::sidebarMenuOutput(outputId = "menu"),
      shinydashboard::sidebarMenu(
        id = "menu",
        #shinydashboard::menuItem(text = "Actions", tabName = "tab_actions"),
        shinydashboard::menuItem(
          text = "Tables",
          tabName = "tab_tables",
          shiny::tagList(
            purrr::map(
              .x = data_levels,
              .f = ~ shinydashboard::menuSubItem(
                text = dlp(.x, cap = TRUE),
                tabName = stringr::str_c("tab_all_", .x)
              )
            )
          )
        ),
        shinydashboard::menuItem( text = "Projects",  tabName = "tab_projects")#,
        #shiny::actionButton("test", label = "Test")
      )
    ),

    shinydashboard::dashboardBody(

      shinybusy::add_busy_spinner(
        spin = "cube-grid",
        color = "red",
        height = "100px",
        width = "100px",
        margins = c(500, 950),
        position = "bottom-right",
        timeout = 100
      ),

      shinydashboard::tabItems(

        shinydashboard::tabItem(
          tabName = "tab_actions",
          htmlContainer(
            shiny::fluidRow(
              htmlCol(width = 1),
              htmlCol(
                width = 3,
                htmlMainButton(inputId = "add_tissue_donor", color = "success") ,
                htmlMainButton(inputId = "add_tissue_sample", color = "success"),
                htmlMainButton(inputId = "add_tissue_portions", color = "success"),
                htmlMainButton(inputId = "add_raw_data", color = "success")
              ),
              htmlCol(width = 1),
              htmlCol(
                width = 3,
                htmlMainButton(inputId = "edit_tissue_donor", color = "primary"),
                htmlMainButton(inputId = "edit_tissue_sample", color = "primary"),
                htmlMainButton(inputId = "edit_tissue_portion", color = "primary"),
                htmlMainButton(inputId = "edit_raw_data", color = "primary")
              ),
              htmlCol(width = 1),
              htmlCol(
                width = 3,
                htmlMainButton(inputId = "delete_tissue_donor", color = "warning"),
                htmlMainButton(inputId = "delete_tissue_sample", color = "warning"),
                htmlMainButton(inputId = "delete_tissue_portion", color = "warning"),
                htmlMainButton(inputId = "delete_raw_data", color = "warning")
              )
            )
          )
        ),
        shinydashboard::tabItem(
          tabName = "tab_all_tissue_donor",
          shinyWidgets::dropdown(
            label = "Filter Options:",
            icon = shiny::icon("sliders"),
            status = "primary",
            circle = FALSE,
            shiny::uiOutput(outputId = "filter_table_all_tissue_donor")
          ),
          htmlBreak(1),
          htmlContainer(
            DT::dataTableOutput(outputId = "table_all_tissue_donor")
          ),
          htmlContainer(
            htmlActionButtonsBelowTables(d_level = "tissue_donor")
          )
        ),
        shinydashboard::tabItem(
          tabName = "tab_all_tissue_sample",
          shinyWidgets::dropdown(
            label = "Filter Options:",
            icon = shiny::icon("sliders"),
            status = "primary",
            circle = FALSE,
            shiny::uiOutput(outputId = "filter_table_all_tissue_sample")
          ),
          htmlBreak(1),
          htmlContainer(
            DT::dataTableOutput(outputId = "table_all_tissue_sample")
          ),
          htmlContainer(
            htmlActionButtonsBelowTables(d_level = "tissue_sample")
          )
        ),
        shinydashboard::tabItem(
          tabName = "tab_all_tissue_portion",
          shinyWidgets::dropdown(
            label = "Filter Options:",
            icon = shiny::icon("sliders"),
            status = "primary",
            circle = FALSE,
            shiny::uiOutput(outputId = "filter_table_all_tissue_portion")
          ),
          htmlBreak(1),
          htmlContainer(
            DT::dataTableOutput(outputId = "table_all_tissue_portion")
          ),
          htmlContainer(
            htmlActionButtonsBelowTables(d_level = "tissue_portion")
          )
        ),
        shinydashboard::tabItem(
          tabName = "tab_all_raw_data",
          shinyWidgets::dropdown(
            label = "Filter Options:",
            icon = shiny::icon("sliders"),
            status = "primary",
            circle = FALSE,
            shiny::uiOutput(outputId = "filter_table_all_raw_data")
          ),
          htmlBreak(1),
          htmlContainer(
            DT::dataTableOutput(outputId = "table_all_raw_data")
          ),
          htmlContainer(
            htmlActionButtonsBelowTables(d_level = "raw_data")
          )
        ),
        shinydashboard::tabItem(
          tabName = "tab_projects",
          shiny::fluidRow(
            htmlCol(
              width = 8,
              align = "left",
              shinydashboard::box(
                title = "Project Organization:",
                width = 12,
                collapsible = TRUE,
                htmlContainer(
                  align = "left",
                  shiny::fluidRow(
                    htmlCol(
                      width = 4,
                      align = "left",
                      htmlH5("Project Name:"),
                      shiny::textInput(
                        inputId = "project_name",
                        label = NULL,
                        value = "New Project"
                      )
                    ),
                    htmlCol(
                      width = 3,
                      align = "left",
                      htmlH5("Storage Folder:"),
                      shinyFiles::shinyDirButton(
                        id = "choose_storage_folder",
                        label = "Choose",
                        title = "Select a Storage Folder:"
                      )
                    ),
                    htmlCol(
                      width = 5,
                      align = "left",
                      htmlH5("Final Storage Directory:"),
                      shiny::textOutput(outputId = "chosen_storage_folder"),
                      htmlBreak(2)
                    )
                  ),
                  shiny::fluidRow(
                    htmlCol(
                      width = 3,
                      align = "left",
                      shiny::selectInput(
                        inputId = "folder_orga_select",
                        label = "Create subfolders based on:",
                        choices = c("assay_trademark", "histo_class", "organ", "organ_part",
                                    "workgroup", "institution"
                                    ),
                        selected = c("assay_trademark"),
                        multiple = TRUE
                      )
                    ),
                    htmlCol(
                      width = 9,
                      align = "left",
                      shiny::uiOutput("folder_orga_sorted")
                    )
                  )
                )
              ),
              htmlContainer(
                width = 12,
                shinyWidgets::dropdown(
                  label = "Filter Options:",
                  icon = shiny::icon("sliders"),
                  status = "primary",
                  circle = FALSE,
                  shiny::uiOutput(outputId = "filter_table_all_raw_data_projects")
                ),
                htmlBreak(1)
              ),
              shinydashboard::box(
                width = 12,
                title = "Data Available:",
                collapsible = TRUE,
                htmlCol(
                  width = 12,
                  DT::dataTableOutput(outputId = "table_all_raw_data_projects"),
                  shiny::fluidRow(
                    htmlCol(
                      width = 4,
                      htmlMediumButton(
                        inputId = "add_selected_to_project",
                        label = "Add Selected",
                        text =
                          c("Adds the selected data entries to the project. If successfull",
                            " they appear in the list on the right. (Selected entries that",
                            " have already been added are not added again.)"
                          )
                      )
                    ),
                    htmlCol(
                      width = 4,
                      htmlMediumButton(
                        inputId = "add_all_to_project",
                        label = "Add All",
                        text =
                          c("Adds all data entries from the current table displayed. Use 'Filter Options'",
                            "to subset the table according to your goals. If successfull,",
                            "added entries appear in the list on the right. (Selected entries that",
                            "have already been added are not added again.)"
                          ),
                        placement = "top"
                      )
                    ),
                    htmlCol(
                      width = 4,
                      htmlMediumButton(inputId = "view_added_to_project", label = "View")
                    )
                  )
              )
              )
            ),
            htmlCol(
              width = 4,
              shinydashboard::box(
                width = 12,
                title = "Added to Project",
                DT::dataTableOutput(outputId = "table_added_to_project"),
                htmlBreak(1),
                shiny::fluidRow(
                  htmlCol(
                    width = 6,
                    htmlMediumButton(
                      inputId = "create_project",
                      label = "Create Project",
                    )
                  ),
                  htmlCol(
                    width = 6,
                    htmlMediumButton(
                      inputId = "remove_selected_from_project",
                      label = "Remove",
                      color = "warning"
                    )
                  )
                )
              )
            )
          )
        )
      )
    )
  )

}
