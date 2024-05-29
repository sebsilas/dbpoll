

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
      shiny::actionButton("update_names", "Update column names")
      ),

    shiny::tableOutput("db_table")

  )

  # Define the server logic
  server <- function(input, output, session) {

    # This has to be defined within the server function
    get_trials <- function() {

      shiny::req(input$no_trials)

      trials <- dplyr::tbl(db_con, "trials") %>%
        dplyr::slice_max(trial_id, n = input$no_trials)

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

    data <- shiny::reactivePoll(2000, session,
                         # In this case the check and value functions are the same
                         checkFunc = get_trials,
                         valueFunc = get_trials)


    col_names <- shiny::reactive({
      names(dplyr::collect(data()))
    })

    col_names_inited <- reactiveVal(FALSE)

    output$db_table <- shiny::renderTable({

      cols_to_show <- input$cols_to_show

      shiny::req(data())


      if(!col_names_inited()) {
        shiny::updateSelectInput(inputId = "cols_to_show", choices = col_names(), selected = col_names() )
        col_names_inited(TRUE)
      }


      shiny::req(cols_to_show)

      data() %>%
        dplyr::select(dplyr::all_of(cols_to_show))
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



