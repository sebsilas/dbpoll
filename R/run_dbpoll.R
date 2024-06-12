

run_dbpoll <- function() {

  # Define the UI
  ui <- shiny::fluidPage(

    shiny::tags$style('
                      #topSection {
                        margin: 20px 0 20px;
                      }

                      .form-group.shiny-input-container, .form-group.shiny-input-checkbox {
      display: flex;
      align-items: center;
      float: left;
      margin: 0 20px 0 0;
    }
    .form-group.shiny-input-container label, .form-group.shiny-input-checkbox label {
      margin-right: 10px;
      margin-bottom: 0; /* Ensure no extra margin at the bottom */
    }
    .form-group.shiny-input-container input, .form-group.shiny-input-checkbox input {
      flex: 1; /* Allow input to take available space */
    }
                      '),

    # Application title
    shiny::titlePanel("musicassessr trial monitor"),

    shiny::tags$div(id = "topSection",
      shiny::numericInput("no_trials", "Number of trials to show:", value = 5, min = 1, max = 100),
      shiny::checkboxInput("join_item_data", "Join item data on?"),
      shiny::selectInput("cols_to_show", "Columns to show", choices = character(0), multiple = TRUE, width = 500),
      shiny::actionButton("update_names", "Update column names"),
      shiny::checkboxInput("poll_db", "Poll DB?", value = TRUE)
      ),

    DT::DTOutput("db_table")

  )

  # Define the server logic
  server <- function(input, output, session) {

    # This has to be defined within the server function
    get_trials <- function() {

      shiny::req(input$no_trials && DBI::dbIsValid(db_con) && input$poll_db)

      trials <- dplyr::tbl(db_con, "trials") %>%
        dplyr::slice_max(trial_id, n = input$no_trials)

      trial_ids <- trials %>%
        dplyr::pull(trial_id)

      scores_trials <- dplyr::tbl(db_con, "scores_trial") %>%
        dplyr::filter(measure == "opti3",
                      trial_id %in% trial_ids) %>%
        dplyr::select(-measure) %>%
        dplyr::rename(opti3 = score)

      trials <- trials %>%
        dplyr::left_join(scores_trials, by = "trial_id")


      if(input$join_item_data) {

        item_banks <- trials %>%
          dplyr::pull(item_id) %>%
          musicassessrdb::get_item_bank_names(db_con = db_con)


        trials <- trials %>%
          musicassessrdb::left_join_on_items(db_con,
                                             item_banks = item_banks,
                                             df_with_item_ids = .)
      }

      return(trials)
    }


    last_data <- shiny::reactiveVal(NULL)

    shiny::observe({
      if (input$poll_db) {
        last_data(data())
      }
    })

    data <- shiny::reactivePoll(2000, session,
                         # In this case the check and value functions are the same
                         checkFunc = get_trials,
                         valueFunc = get_trials)


    col_names <- shiny::reactive({
      names(dplyr::collect(data()))
    })

    col_names_inited <- reactiveVal(FALSE)

    output$db_table <- DT::renderDT({

      cols_to_show <- input$cols_to_show

      shiny::req(last_data())


      if(!col_names_inited()) {
        shiny::updateSelectInput(inputId = "cols_to_show", choices = col_names(), selected = col_names() )
        col_names_inited(TRUE)
      }


      shiny::req(cols_to_show)

      data <- last_data() %>%
        dplyr::select(dplyr::all_of(cols_to_show)) %>%
        dplyr::collect()

      if("trial_time_completed" %in% cols_to_show) {
        data <- data %>%
          dplyr::mutate(trial_time_completed = format(lubridate::as_datetime(trial_time_completed), format = "%H:%M:%S %d/%m/%Y"))
      }

      if("trial_time_started" %in% cols_to_show) {
        data <- data %>%
          dplyr::mutate(trial_time_started = format(lubridate::as_datetime(trial_time_started), format = "%H:%M:%S %d/%m/%Y"))
      }

      logging::logwarn("Removing any columns which have all NAs...")

      data %>%
        dplyr::select(dplyr::where(~ !all(is.na(.))))

    })


    shiny::observe({
      shiny::updateSelectInput(inputId = "cols_to_show", choices = col_names(), selected = col_names() )
    }) %>% shiny::bindEvent(input$update_names)


    session$onSessionEnded(function() {
      logging::loginfo("Disconnecting from DB")
      DBI::dbDisconnect(db_con)
    })

  }

  # Create the Shiny app object
  shiny::shinyApp(ui = ui,
           server = server,
           onStart = function() {

             db_con <<- musicassessrdb::musicassessr_con()

           })

}



