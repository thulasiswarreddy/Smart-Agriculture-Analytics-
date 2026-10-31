library(shiny)
library(bslib)
library(DT)
library(dplyr)
library(ggplot2)
library(plotly)
library(readxl)
library(readr)

# -----------------------------
# Read uploaded CSV or Excel
# -----------------------------

read_agri_file <- function(path, filename) {
  ext <- tolower(tools::file_ext(filename))

  if (ext == "csv") {
    readr::read_csv(path, show_col_types = FALSE)
  } else if (ext %in% c("xlsx", "xls")) {
    readxl::read_excel(path)
  } else {
    stop("Please upload a CSV or Excel file.")
  }
}

# -----------------------------
# UI
# -----------------------------

ui <- fluidPage(
  theme = bs_theme(
    version = 5,
    bootswatch = "minty",
    primary = "#287a43"
  ),

  tags$head(
    tags$link(
      rel = "stylesheet",
      type = "text/css",
      href = "styles.css"
    ),
    tags$script(src = "animations.js")
  ),

  uiOutput("main_screen")
)

# -----------------------------
# SERVER
# -----------------------------

server <- function(input, output, session) {

  page <- reactiveVal("landing")

  original_data <- reactiveVal(NULL)
  cleaned_data <- reactiveVal(NULL)
  cleaning_info <- reactiveVal(NULL)
  validation_message <- reactiveVal(NULL)

  # Multiple dataset storage
  dataset_store <- reactiveVal(list())
  selected_dataset_id <- reactiveVal(NULL)
  selected_dashboard_id <- reactiveVal(NULL)
  upload_status_message <- reactiveVal(NULL)

  # -----------------------------
  # Landing page / Main application
  # -----------------------------

  output$main_screen <- renderUI({

    if (page() == "landing") {

      div(
        class = "agri-bg",

        div(class = "leaf", "🍃"),
        div(class = "leaf", "🌿"),
        div(class = "leaf", "🍃"),
        div(class = "leaf", "🌱"),

        div(
          class = "landing",

          div(
            class = "hero-content",

            h1(
              class = "hero-title",
              "Smart Agriculture Analytics"
            ),

            h2(
              class = "hero-subtitle",
              "Turn agricultural data into practical insights"
            ),

            p(
              class = "hero-description",
              "Explore crop performance, prepare datasets and estimate yield using your uploaded records."
            ),

            actionButton(
              "get_started",
              tagList("Get Started ", icon("arrow-right")),
              class = "get-started"
            )
          )
        )
      )

    } else {

      div(
        class = "app-shell",

        div(
          class = "app-header",

          div(
            class = "brand",
            "🌱 Smart Agriculture Analytics"
          ),

          actionButton(
            "home_button",
            tagList(icon("house"), " Home"),
            class = "btn-success"
          )
        ),

        div(
          class = "content-area",

          tabsetPanel(
            id = "main_tabs",

            # -----------------
            # DATASETS TAB
            # -----------------

            tabPanel(
              title = tagList(icon("database"), " Datasets"),

              br(),

              div(
                class = "panel-card",

                h2(
                  class = "section-heading",
                  "Upload Agriculture Dataset"
                ),

                p(
                  "Upload your agricultural records in CSV, XLS or XLSX format."
                ),

                div(
                  class = "upload-box",

                  icon("cloud-arrow-up", class = "fa-3x"),

                  h4("Choose your agriculture datasets"),

                  fileInput(
                    "dataset_file",
                    label = NULL,
                    accept = c(".csv", ".xls", ".xlsx"),
                    buttonLabel = "Choose Files",
                    placeholder = "Select one or more datasets",
                    multiple = TRUE
                  )
                ),

                uiOutput("upload_status")
              ),

              div(
                class = "panel-card",

                h3(
                  class = "section-heading",
                  "Select Dataset for Preparation"
                ),

                p(
                  "Choose an uploaded dataset to preview, validate and clean."
                ),

                uiOutput("dataset_prepare_selector")
              ),

              div(
                class = "panel-card",

                h3(
                  class = "section-heading",
                  "Dataset Validation"
                ),

                uiOutput("validation_output"),
                uiOutput("missing_output")
              ),

              div(
                class = "panel-card",

                h3(
                  class = "section-heading",
                  "Dataset Preview"
                ),

                DTOutput("dataset_table")
              ),

              div(
                class = "panel-card",

                h3(
                  class = "section-heading",
                  "Data Cleaning"
                ),

                p(
                  "Remove exact duplicate records and rows missing essential Crop or Yield values."
                ),

                actionButton(
                  "clean_data",
                  tagList(icon("broom"), " Clean Dataset"),
                  class = "btn-success"
                ),

                br(),
                br(),

                uiOutput("cleaning_summary"),

                downloadButton(
                  "download_cleaned",
                  "Download Cleaned Dataset",
                  class = "btn-success"
                )
              )
            ),

            # -----------------
            # DASHBOARD TAB
            # -----------------

            tabPanel(
              title = tagList(icon("chart-column"), " Dashboard"),

              br(),

              h2(
                class = "section-heading",
                "Agriculture Analytics Dashboard"
              ),

              p(
                "Visual insights generated from your selected cleaned agricultural dataset."
              ),

              div(
                class = "panel-card",

                h4(
                  class = "section-heading",
                  "Select Dataset for Reports"
                ),

                p(
                  "Choose which cleaned dataset you want to analyze."
                ),

                uiOutput("dashboard_dataset_selector")
              ),

              uiOutput("dashboard_content")
            )
          )
        )
      )
    }
  })

  # -----------------------------
  # Page navigation
  # -----------------------------

  observeEvent(input$get_started, {
    page("application")
  })

  observeEvent(input$home_button, {
    page("landing")
  })

  # -----------------------------
  # Upload multiple datasets
  # -----------------------------

  observeEvent(input$dataset_file, {

    req(input$dataset_file)

    files <- input$dataset_file
    store <- dataset_store()

    uploaded_names <- character(0)
    failed_files <- character(0)

    for (i in seq_len(nrow(files))) {

      file_name <- files$name[i]
      file_path <- files$datapath[i]

      tryCatch({

        data <- read_agri_file(
          file_path,
          file_name
        )

        names(data) <- trimws(names(data))

        # Generate unique dataset ID
        dataset_id <- file_name
        counter <- 2

        while (dataset_id %in% names(store)) {

          dataset_id <- paste0(
            file_name,
            " (",
            counter,
            ")"
          )

          counter <- counter + 1
        }

        store[[dataset_id]] <- list(
          name = file_name,
          raw = data,
          cleaned = NULL,
          cleaning_info = NULL
        )

        uploaded_names <- c(
          uploaded_names,
          file_name
        )

      }, error = function(e) {

        failed_files <<- c(
          failed_files,
          paste0(file_name, ": ", e$message)
        )

      })
    }

    dataset_store(store)

    # Select first dataset if no dataset is selected
    if (
      is.null(selected_dataset_id()) &&
      length(store) > 0
    ) {

      selected_dataset_id(names(store)[1])
    }

    # Load selected dataset
    current_id <- selected_dataset_id()

    if (
      !is.null(current_id) &&
      current_id %in% names(store)
    ) {

      current <- store[[current_id]]

      original_data(current$raw)
      cleaned_data(current$cleaned)
      cleaning_info(current$cleaning_info)
    }

    if (length(uploaded_names) > 0) {

      upload_status_message(
        paste0(
          "Successfully uploaded ",
          length(uploaded_names),
          " dataset(s): ",
          paste(uploaded_names, collapse = ", "),
          ". Select a dataset below to prepare it."
        )
      )

    } else {

      upload_status_message(
        "No dataset was uploaded successfully."
      )
    }

    if (length(failed_files) > 0) {

      showNotification(
        paste(
          "Some files could not be uploaded:",
          paste(failed_files, collapse = " | ")
        ),
        type = "error",
        duration = 8
      )
    }
  })

  # -----------------------------
  # Dataset preparation selector
  # -----------------------------

  output$dataset_prepare_selector <- renderUI({

    store <- dataset_store()

    if (length(store) == 0) {
      return(
        p("Upload one or more datasets to begin.")
      )
    }

    labels <- vapply(
      store,
      function(x) x$name,
      character(1)
    )

    selectInput(
      "dataset_to_prepare",
      "Select Dataset",
      choices = stats::setNames(
        names(store),
        labels
      ),
      selected = selected_dataset_id()
    )
  })

  # Switch preparation dataset
  observeEvent(input$dataset_to_prepare, {

    req(input$dataset_to_prepare)

    store <- dataset_store()

    req(
      input$dataset_to_prepare %in% names(store)
    )

    selected_dataset_id(
      input$dataset_to_prepare
    )

    current <- store[[input$dataset_to_prepare]]

    original_data(current$raw)
    cleaned_data(current$cleaned)
    cleaning_info(current$cleaning_info)
  })

  # -----------------------------
  # Upload status
  # -----------------------------

  output$upload_status <- renderUI({

    req(upload_status_message())

    div(
      class = "alert alert-success",
      upload_status_message()
    )
  })

  # -----------------------------
  # Data validation
  # -----------------------------

  output$validation_output <- renderUI({

    req(original_data())

    dat <- original_data()

    required <- c("Crop", "Yield")

    missing_columns <- setdiff(
      required,
      names(dat)
    )

    if (length(missing_columns) > 0) {

      return(
        div(
          class = "alert alert-danger",

          paste(
            "Required columns missing:",
            paste(
              missing_columns,
              collapse = ", "
            )
          )
        )
      )
    }

    if (!is.numeric(dat$Yield)) {

      return(
        div(
          class = "alert alert-danger",
          "Yield column must contain numeric values."
        )
      )
    }

    div(
      class = "alert alert-success",

      "Dataset validation successful. Required Crop and numeric Yield columns are available."
    )
  })

  # -----------------------------
  # Missing value detection
  # -----------------------------

  output$missing_output <- renderUI({

    req(original_data())

    dat <- original_data()

    total_missing <- sum(is.na(dat))

    div(
      class = if (total_missing > 0) {
        "alert alert-warning"
      } else {
        "alert alert-success"
      },

      paste(
        "Missing values detected:",
        total_missing
      )
    )
  })

  # -----------------------------
  # Dataset preview
  # -----------------------------

  output$dataset_table <- renderDT({

    req(original_data())

    datatable(
      head(original_data(), 500),

      options = list(
        pageLength = 8,
        scrollX = TRUE
      ),

      rownames = FALSE
    )
  })

  # -----------------------------
  # Data cleaning
  # -----------------------------

  observeEvent(input$clean_data, {

    req(original_data())

    dat <- original_data()

    required <- c("Crop", "Yield")

    if (!all(required %in% names(dat))) {

      showNotification(
        "Dataset must contain Crop and Yield columns.",
        type = "error"
      )

      return()
    }

    if (!is.numeric(dat$Yield)) {

      showNotification(
        "Yield must be numeric. Please correct the uploaded file.",
        type = "error"
      )

      return()
    }

    before <- nrow(dat)

    duplicates <- sum(duplicated(dat))

    cleaned <- dat %>%
      distinct() %>%
      filter(
        !is.na(Crop),
        trimws(as.character(Crop)) != "",
        !is.na(Yield),
        is.finite(Yield)
      )

    after <- nrow(cleaned)

    info <- list(
      before = before,
      duplicates = duplicates,
      after = after,
      missing_removed = before - duplicates - after
    )

    cleaning_info(info)
    cleaned_data(cleaned)

    # Save cleaning result for the selected dataset
    current_id <- selected_dataset_id()

    if (
      !is.null(current_id) &&
      current_id %in% names(dataset_store())
    ) {

      store <- dataset_store()

      store[[current_id]]$cleaned <- cleaned
      store[[current_id]]$cleaning_info <- info

      dataset_store(store)

      # Automatically select the first cleaned dataset
      if (is.null(selected_dashboard_id())) {
        selected_dashboard_id(current_id)
      }
    }

    showNotification(
      "Dataset cleaning completed successfully!",
      type = "message"
    )
  })

  # -----------------------------
  # Cleaning summary
  # -----------------------------

  output$cleaning_summary <- renderUI({

    req(cleaning_info())

    info <- cleaning_info()

    div(
      class = "alert alert-success",

      h4("Cleaning Summary"),

      p(
        paste(
          "Original records:",
          info$before
        )
      ),

      p(
        paste(
          "Duplicate records removed:",
          info$duplicates
        )
      ),

      p(
        paste(
          "Additional rows removed due to missing/invalid Crop or Yield:",
          info$missing_removed
        )
      ),

      p(
        strong(
          paste(
            "Cleaned records:",
            info$after
          )
        )
      )
    )
  })

  # -----------------------------
  # Download cleaned dataset
  # -----------------------------

  output$download_cleaned <- downloadHandler(

    filename = function() {

      current_id <- selected_dataset_id()
      store <- dataset_store()

      if (
        !is.null(current_id) &&
        current_id %in% names(store)
      ) {

        original_name <- store[[current_id]]$name

        base_name <- tools::file_path_sans_ext(
          original_name
        )

        return(
          paste0(
            "cleaned_",
            base_name,
            ".csv"
          )
        )
      }

      paste0(
        "cleaned_agriculture_dataset_",
        Sys.Date(),
        ".csv"
      )
    },

    content = function(file) {

      req(cleaned_data())

      write.csv(
        cleaned_data(),
        file,
        row.names = FALSE,
        na = ""
      )
    }
  )

  # -----------------------------
  # Dashboard dataset selector
  # -----------------------------

  output$dashboard_dataset_selector <- renderUI({

    store <- dataset_store()

    available <- names(
      Filter(
        function(x) !is.null(x$cleaned),
        store
      )
    )

    if (length(available) == 0) {

      return(
        p("No cleaned datasets available. Clean a dataset first.")
      )
    }

    labels <- vapply(
      store[available],
      function(x) x$name,
      character(1)
    )

    current_id <- selected_dashboard_id()

    if (
      is.null(current_id) ||
      !current_id %in% available
    ) {
      current_id <- available[1]
      selected_dashboard_id(current_id)
    }

    selectInput(
      "dashboard_dataset",
      "Select Cleaned Dataset",
      choices = stats::setNames(
        available,
        labels
      ),
      selected = current_id
    )
  })

  # Change dashboard dataset
  observeEvent(input$dashboard_dataset, {

    req(input$dashboard_dataset)

    store <- dataset_store()

    req(
      input$dashboard_dataset %in% names(store)
    )

    req(
      !is.null(
        store[[input$dashboard_dataset]]$cleaned
      )
    )

    selected_dashboard_id(
      input$dashboard_dataset
    )

    # Reset filters after switching datasets
    updateSelectInput(
      session,
      "crop_filter",
      selected = "All"
    )

    updateSelectInput(
      session,
      "year_filter",
      selected = "All"
    )

    updateSelectInput(
      session,
      "soil_filter",
      selected = "All"
    )
  })

  # -----------------------------
  # Selected dashboard data
  # -----------------------------

  dashboard_selected_data <- reactive({

    current_id <- selected_dashboard_id()

    req(!is.null(current_id))

    store <- dataset_store()

    req(current_id %in% names(store))

    req(
      !is.null(
        store[[current_id]]$cleaned
      )
    )

    store[[current_id]]$cleaned
  })

  # -----------------------------
  # Dashboard content
  # -----------------------------

  output$dashboard_content <- renderUI({

    store <- dataset_store()

    available <- Filter(
      function(x) !is.null(x$cleaned),
      store
    )

    if (length(available) == 0) {

      return(
        div(
          class = "panel-card",
          style = "text-align:center; padding:60px;",

          icon("database", class = "fa-3x"),

          h3("No cleaned dataset available"),

          p(
            "Please upload and clean an agriculture dataset in the Datasets section before viewing analytics."
          )
        )
      )
    }

    tagList(

      div(
        class = "panel-card",

        h4("Dashboard Filters"),

        fluidRow(

          column(
            4,
            uiOutput("crop_filter")
          ),

          column(
            4,
            uiOutput("year_filter")
          ),

          column(
            4,
            uiOutput("soil_filter")
          )
        )
      ),

      fluidRow(

        column(
          3,

          div(
            class = "panel-card",

            h5("Total Records"),

            h2(textOutput("total_records"))
          )
        ),

        column(
          3,

          div(
            class = "panel-card",

            h5("Total Crops"),

            h2(textOutput("total_crops"))
          )
        ),

        column(
          3,

          div(
            class = "panel-card",

            h5("Average Yield"),

            h2(textOutput("average_yield"))
          )
        ),

        column(
          3,

          div(
            class = "panel-card",

            h5("Highest Yield Crop"),

            h2(textOutput("highest_crop"))
          )
        )
      ),

      fluidRow(
        column(
          6,
          plotlyOutput("crop_chart", height = "350px")
        ),

        column(
          6,
          plotlyOutput("rainfall_chart", height = "350px")
        )
      ),

      fluidRow(
        column(
          6,
          plotlyOutput("production_chart", height = "350px")
        ),

        column(
          6,
          plotlyOutput("soil_chart", height = "350px")
        )
      ),

      fluidRow(
        column(
          6,
          plotlyOutput("temperature_chart", height = "350px")
        ),

        column(
          6,
          plotlyOutput("comparison_chart", height = "350px")
        )
      )
    )
  })

  # -----------------------------
  # Dashboard filtered data
  # -----------------------------

  dashboard_data <- reactive({

    dat <- dashboard_selected_data()

    if (
      "Crop" %in% names(dat) &&
      !is.null(input$crop_filter) &&
      input$crop_filter != "All" &&
      input$crop_filter %in% as.character(dat$Crop)
    ) {

      dat <- dat %>%
        filter(
          Crop == input$crop_filter
        )
    }

    if (
      "Year" %in% names(dat) &&
      !is.null(input$year_filter) &&
      input$year_filter != "All" &&
      input$year_filter %in% as.character(dat$Year)
    ) {

      dat <- dat %>%
        filter(
          as.character(Year) == input$year_filter
        )
    }

    if (
      "Soil_Type" %in% names(dat) &&
      !is.null(input$soil_filter) &&
      input$soil_filter != "All" &&
      input$soil_filter %in% as.character(dat$Soil_Type)
    ) {

      dat <- dat %>%
        filter(
          Soil_Type == input$soil_filter
        )
    }

    dat
  })

  # -----------------------------
  # Dashboard filters
  # -----------------------------

  output$crop_filter <- renderUI({

    dat <- dashboard_selected_data()

    if (!"Crop" %in% names(dat)) {
      return(NULL)
    }

    selectInput(
      "crop_filter",
      "Select Crop",

      choices = c(
        "All",
        sort(
          unique(
            as.character(dat$Crop)
          )
        )
      )
    )
  })

  output$year_filter <- renderUI({

    dat <- dashboard_selected_data()

    if (!"Year" %in% names(dat)) {
      return(NULL)
    }

    selectInput(
      "year_filter",
      "Select Year",

      choices = c(
        "All",
        sort(
          unique(
            as.character(dat$Year)
          )
        )
      )
    )
  })

  output$soil_filter <- renderUI({

    dat <- dashboard_selected_data()

    if (!"Soil_Type" %in% names(dat)) {
      return(NULL)
    }

    selectInput(
      "soil_filter",
      "Select Soil Type",

      choices = c(
        "All",
        sort(
          unique(
            as.character(dat$Soil_Type)
          )
        )
      )
    )
  })

  # -----------------------------
  # KPI outputs
  # -----------------------------

  output$total_records <- renderText({

    nrow(dashboard_data())
  })

  output$total_crops <- renderText({

    dat <- dashboard_data()

    if (!"Crop" %in% names(dat)) {
      return("N/A")
    }

    length(
      unique(dat$Crop)
    )
  })

  output$average_yield <- renderText({

    dat <- dashboard_data()

    if (!"Yield" %in% names(dat)) {
      return("N/A")
    }

    paste(
      round(
        mean(dat$Yield, na.rm = TRUE),
        2
      ),
      "t/ha"
    )
  })

  output$highest_crop <- renderText({

    dat <- dashboard_data()

    if (
      !all(
        c("Crop", "Yield") %in% names(dat)
      )
    ) {
      return("N/A")
    }

    result <- dat %>%
      group_by(Crop) %>%
      summarise(
        avg = mean(Yield, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      filter(
        is.finite(avg)
      ) %>%
      arrange(
        desc(avg)
      )

    if (nrow(result) == 0) {
      return("N/A")
    }

    as.character(
      result$Crop[1]
    )
  })

  # -----------------------------
  # Average yield by crop
  # -----------------------------

  output$crop_chart <- renderPlotly({

    dat <- dashboard_data()

    req(
      all(
        c("Crop", "Yield") %in% names(dat)
      )
    )

    p <- dat %>%
      group_by(Crop) %>%
      summarise(
        Average_Yield = mean(
          Yield,
          na.rm = TRUE
        ),
        .groups = "drop"
      ) %>%
      ggplot(
        aes(
          x = reorder(Crop, Average_Yield),
          y = Average_Yield,
          fill = Crop
        )
      ) +
      geom_col(show.legend = FALSE) +
      coord_flip() +
      labs(
        title = "Average Yield by Crop",
        x = "Crop",
        y = "Yield (t/ha)"
      ) +
      theme_minimal()

    ggplotly(p)
  })

  # -----------------------------
  # Rainfall vs yield
  # -----------------------------

  output$rainfall_chart <- renderPlotly({

    dat <- dashboard_data()

    validate(
      need(
        all(
          c("Rainfall", "Yield") %in% names(dat)
        ),
        "Rainfall and Yield columns are required."
      )
    )

    p <- ggplot(
      dat,
      aes(
        x = Rainfall,
        y = Yield,
        color = Crop
      )
    ) +
      geom_point(
        size = 3,
        alpha = .7
      ) +
      labs(
        title = "Rainfall vs Yield",
        x = "Rainfall (mm)",
        y = "Yield (t/ha)"
      ) +
      theme_minimal()

    ggplotly(p)
  })

  # -----------------------------
  # Production by year
  # -----------------------------

  output$production_chart <- renderPlotly({

    dat <- dashboard_data()

    validate(
      need(
        all(
          c("Year", "Production") %in% names(dat)
        ),
        "Year and Production columns are required."
      )
    )

    p <- dat %>%
      group_by(Year) %>%
      summarise(
        Total_Production = sum(
          Production,
          na.rm = TRUE
        ),
        .groups = "drop"
      ) %>%
      ggplot(
        aes(
          x = as.factor(Year),
          y = Total_Production
        )
      ) +
      geom_col(fill = "#287a43") +
      labs(
        title = "Production by Year",
        x = "Year",
        y = "Total Production"
      ) +
      theme_minimal()

    ggplotly(p)
  })

  # -----------------------------
  # Soil type distribution
  # -----------------------------

  output$soil_chart <- renderPlotly({

    dat <- dashboard_data()

    validate(
      need(
        "Soil_Type" %in% names(dat),
        "Soil_Type column is required."
      )
    )

    p <- ggplot(
      dat,
      aes(
        x = Soil_Type,
        fill = Soil_Type
      )
    ) +
      geom_bar(show.legend = FALSE) +
      labs(
        title = "Soil Type Distribution",
        x = "Soil Type",
        y = "Number of Records"
      ) +
      theme_minimal()

    ggplotly(p)
  })

  # -----------------------------
  # Temperature vs yield
  # -----------------------------

  output$temperature_chart <- renderPlotly({

    dat <- dashboard_data()

    validate(
      need(
        all(
          c("Temperature", "Yield") %in% names(dat)
        ),
        "Temperature and Yield columns are required."
      )
    )

    p <- ggplot(
      dat,
      aes(
        x = Temperature,
        y = Yield,
        color = Crop
      )
    ) +
      geom_point(
        size = 3,
        alpha = .7
      ) +
      labs(
        title = "Temperature vs Yield",
        x = "Temperature (°C)",
        y = "Yield (t/ha)"
      ) +
      theme_minimal()

    ggplotly(p)
  })

  # -----------------------------
  # Crop production comparison
  # -----------------------------

  output$comparison_chart <- renderPlotly({

    dat <- dashboard_data()

    validate(
      need(
        all(
          c("Crop", "Production") %in% names(dat)
        ),
        "Crop and Production columns are required."
      )
    )

    p <- dat %>%
      group_by(Crop) %>%
      summarise(
        Total_Production = sum(
          Production,
          na.rm = TRUE
        ),
        .groups = "drop"
      ) %>%
      ggplot(
        aes(
          x = reorder(Crop, Total_Production),
          y = Total_Production
        )
      ) +
      geom_col(fill = "#d6a64b") +
      coord_flip() +
      labs(
        title = "Crop-wise Production Comparison",
        x = "Crop",
        y = "Production"
      ) +
      theme_minimal()

    ggplotly(p)
  })
}

# -----------------------------
# Run application
# -----------------------------

shinyApp(
  ui = ui,
  server = server
)