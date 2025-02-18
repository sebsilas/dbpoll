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
                        margin-bottom: 0;
                      }
                      .form-group.shiny-input-container input, .form-group.shiny-input-checkbox input {
                        flex: 1;
                      }
                      '),

    # Application title
    shiny::titlePanel("musicassessr trial monitor"),

    shiny::tags$div(id = "topSection",
                    shiny::selectInput("prod_vs_dev",
                                       label = "Environment",
                                       choices = c("prod", "dev")),
                    shiny::numericInput("no_trials", "Number of trials to show:", value = 5, min = 1, max = 100),
                    shiny::checkboxInput("join_item_data", "Join item data on?"),
                    shiny::selectInput("cols_to_show", "Columns to show", choices = character(0), multiple = TRUE, width = 500),
                    shiny::actionButton("update_names", "Update column names"),
                    shiny::checkboxInput("automatically_poll_db", "Automatically poll DB?", value = FALSE),
                    shiny::actionButton("refresh_db", "Manual refresh")
    ),

    # Audio player section
    shiny::uiOutput("audio_player"),

    # DataTable section
    DT::DTOutput("db_table")

  )

  # Define the server logic
  server <- function(input, output, session) {

    # ReactiveVal to manage the DB connection
    db_con <- shiny::reactiveVal(NULL)

    # Function to connect to the database based on selected environment
    connect_to_db <- function(env) {
      db_name <- if(env == "prod") "melody_prod" else "melody_dev"
      return(musicassessrdb::musicassessr_con(db_name = db_name))
    }

    # Observe the environment switch and update DB connection
    shiny::observeEvent(input$prod_vs_dev, {
      # Disconnect the previous connection if valid
      old_con <- db_con()
      print('1')
      if (!is.null(old_con) && DBI::dbIsValid(old_con)) {
        musicassessrdb::db_disconnect(old_con)
      }

      # Establish a new connection
      new_con <- connect_to_db(input$prod_vs_dev)
      print('2')
      db_con(new_con)
    }, ignoreInit = FALSE)  # Ensure it triggers on startup

    # Function to retrieve trials safely
    get_trials <- function() {

      shiny::req(input$no_trials)

      con <- db_con()

      # Ensure con is not NULL and is valid before proceeding
      if (is.null(con) || !DBI::dbIsValid(con)) {
        return(NULL) # Return NULL instead of throwing an error
      }

      trials <- dplyr::tbl(con, "trials") %>%
        dplyr::slice_max(trial_id, n = input$no_trials)

      trial_ids <- trials %>%
        dplyr::pull(trial_id)

      scores_trials <- dplyr::tbl(con, "scores_trial") %>%
        dplyr::filter(measure %in% c("opti3",
                                     "ngrukkon",
                                     "rhythfuzz",
                                     "harmcore",
                                     "ngrukkon_N2",
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
        trials <- musicassessrdb::left_join_on_items(con, trials)
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
                                checkFunc = get_trials,
                                valueFunc = get_trials)

    col_names <- shiny::reactive({
      names(dplyr::collect(data()))
    })

    col_names_inited <- shiny::reactiveVal(FALSE)

    selected_audio_file <- shiny::reactiveVal(NULL)

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

    }, selection = 'single')

    shiny::observeEvent(input$db_table_rows_selected, {
      shiny::req(input$db_table_rows_selected)
      selected_row <- last_data() %>%
        dplyr::collect() %>%
        dplyr::slice(input$db_table_rows_selected)

      selected_audio_file(selected_row$audio_file)
    })

    output$audio_player <- shiny::renderUI({
      audio_file <- selected_audio_file()

      if (!is.null(audio_file) && nzchar(audio_file)) {
        base_url <- if (input$prod_vs_dev == "prod") {
          "https://musicassessr-media-source.s3.eu-central-1.amazonaws.com/"
        } else {
          "https://shinny-app-source-41630.s3.us-east-1.amazonaws.com/"
        }
        audio_file <- paste0(base_url, audio_file)

        shiny::tags$audio(controls = TRUE, shiny::tags$source(src = audio_file, type = "audio/wav"))
      } else {
        "No audio file selected."
      }
    })

    shiny::observe({
      shiny::updateSelectInput(inputId = "cols_to_show", choices = col_names(), selected = col_names() )
    }) %>% shiny::bindEvent(input$update_names)

    session$onSessionEnded(function() {
      con <- isolate(db_con())  # Extracts the non-reactive value safely
      if (!is.null(con) && DBI::dbIsValid(con)) {
        logging::loginfo("Disconnecting from DB")
        musicassessrdb::db_disconnect(con)
      }
    })


  }

  shiny::shinyApp(ui = ui, server = server)
}
