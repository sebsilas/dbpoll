

#' Run log monitor app
#'
#' @return
#' @export
#'
#' @examples
run_log_monitor <- function() {

  # Define the UI
  ui <- shiny::fluidPage(

    shiny::selectInput("app", "App", choices = character(0)),
    shiny::actionButton("refresh_logs", "Refresh logs"),
    DT::DTOutput("log_table"),
    shiny::uiOutput("log_name"),
    shiny::verbatimTextOutput("log_content")

  )

  # Define the server logic
  server <- function(input, output, session) {

    initSession <- shiny::reactiveVal(FALSE)


    logs <- shiny::reactiveVal(get_logs())


    shiny::observe({
      logs(get_logs())
    }) %>% shiny::bindEvent(input$refresh_logs)



    apps <- shiny::reactive({
      logs() %>%
        dplyr::pull(App) %>%
        unique()
    })

    shiny::observe({
      shiny::req(apps())
      shiny::updateSelectInput(session, "app", choices = apps())
    })

    log_data <- shiny::reactive({
      logs() %>%
        dplyr::filter(App %in% input$app)
    })

    output$log_table <- DT::renderDT({

      shiny::req(input$app)
      shiny::req(logs())

      log_data()

    }, selection = 'single')


    selected_log_file <- shiny::reactive({

      selected_row <- input$log_table_rows_selected

      if (length(selected_row)) {
        selected_log_file <- log_data()$log_file[selected_row]
        log_file_path <- file.path(Sys.getenv("LOG_LOC"), '/', selected_log_file)
      } else {
        log_file_path <- ''
      }
      log_file_path
    })

    output$log_name <- shiny::renderUI({
      shiny::tags$p(shiny::tags$strong(basename(selected_log_file())))
    })

    output$log_content <- shiny::renderText({

      readr::read_file(selected_log_file())

    }) %>% shiny::bindEvent(input$log_table_rows_selected)



  }

  # Create the Shiny app object
  shiny::shinyApp(ui = ui,
                  server = server)

}


get_logs <- function() {

  logs <- list.files(Sys.getenv("LOG_LOC"), pattern = "\\.log$")

  tibble::tibble(log_file = logs,
                 log_file2 = log_file) %>%
    tidyr::separate_wider_delim(log_file2, names = c("App", "Date"), delim = "-shiny-") %>%
    dplyr::mutate(Date = stringr::str_remove_all(Date, ".log"),
                  # Remove the milliseconds bit
                  Date = sub("-[^-]+$", "", Date),
                  # Parse the date and time part using ymd_hms
                  Date = lubridate::ymd_hms(Date, tz = "UTC")) %>%
    dplyr::arrange(dplyr::desc(Date))

}
