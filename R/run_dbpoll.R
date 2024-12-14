

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
      shiny::checkboxInput("automatically_poll_db", "Automatically poll DB?", value = FALSE),
      shiny::actionButton("refresh_db", "Manual refresh")
      ),

    DT::DTOutput("db_table")

  )

  # Define the server logic
  server <- function(input, output, session) {

    # This has to be defined within the server function
    get_trials <- function() {

    shiny::req(input$no_trials && DBI::dbIsValid(db_con))

      trials <- dplyr::tbl(db_con, "trials") %>%
        dplyr::slice_max(trial_id, n = input$no_trials)

      trial_ids <- trials %>%
        dplyr::pull(trial_id)

      scores_trials <- dplyr::tbl(db_con, "scores_trial") %>%
        dplyr::filter(measure %in% c("opti3",
                                     "change_across_all_sessions", "no_times_practised", "avg_no_attempts",
                                     "avg_change_across_attempts", "gradient_across_all_scores", "item_intercept",
                                     "learned_in_current_session", "last_score", "last_score_completed",
                                     "change_in_score_from_last_session", "increase_since_last_session",
                                     "time_since_last_item_studied"),
                      trial_id %in% trial_ids)  %>%
        dplyr::select(-scores_trial_id) %>%
        tidyr::pivot_wider(names_from = "measure", values_from = "score")


      trials <- trials %>%
        dplyr::left_join(scores_trials, by = "trial_id")


      if(input$join_item_data) {

        trials <- musicassessrdb::left_join_on_items(db_con, trials)
      }

      return(trials)

    }


    last_data <- shiny::reactiveVal(NULL)

    shiny::observe({
      if (input$automatically_poll_db) {
        last_data(data())
      }
    })

    # Manual updates
    shiny::observe({
        last_data(get_trials())
    }) %>% shiny::bindEvent(input$refresh_db)

    data <- shiny::reactivePoll(5000, session,
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
        dplyr::select(dplyr::any_of(cols_to_show)) %>%
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
        dplyr::select(dplyr::where(~ !all(is.na(.)))) %>%
        dplyr::arrange(dplyr::desc(trial_id))

    })


    shiny::observe({
      shiny::updateSelectInput(inputId = "cols_to_show", choices = col_names(), selected = col_names() )
    }) %>% shiny::bindEvent(input$update_names)


    session$onSessionEnded(function() {
      logging::loginfo("Disconnecting from DB")
      musicassessrdb::db_disconnect(db_con)
    })

  }

  # Create the Shiny app object
  shiny::shinyApp(ui = ui,
           server = server,
           onStart = function() {

             db_con <<- musicassessrdb::musicassessr_con()

           })

}



