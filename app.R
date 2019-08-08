# REQUIRES ----------------------------------------------------------------

library(tidyverse)
library(dr4pl)
library(shiny)
library(shinyFiles)
library(shinycssloaders)
library(DT)
library(dplyr)

options(spinner.size = 3)

source("app_functions/parse_fx.R")
source("app_functions/map_fx.R")
source("app_functions/fit_fx.R")

# UI ----------------------------------------------------------------------

ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      @import url('https://fonts.googleapis.com/css?family=Blinker&display=swap');

      body {
        font-family: 'Blinker', sans-serif;
      }

      div {
        align: 'center'
      }

      h1 {
        font-weight: bold;
        line-height: 1.1;
        color: #FC7018;
      }

      h3 {
        font-weight: bold;
        line-height: 1.1;
      }

      h4 {
        line-height: 1.3;
      }

      th {
        font-weight: bold;
        line-height: 1.1;
      }

      .btn {
      font-size : 16px;
      }
                    
      .btn-default {
                    color: #ffffff;
                    background-color: #333333;
      }

    "))
  ), 
  
  headerPanel("4PL Curve Fitting and Plotting"),

# 1. File Upload -------------------------------------------------------------

  # Input:
  # Uploading Data
  fluidRow(column(
    width = 4,
    h3("1. Upload raw data:")
  )),
  
  # Get path to directory containining raw data
  fluidRow(column(
    width = 4,
    shinyFiles::shinyDirButton("dir", "Choose folder containing EnVision Assay .csv files", "Upload")
  )),
  
  # Deprecated: show user the directory they selected and the CSVs inside it
  # fluidRow(column(
  #   width = 12,
  #   h4("Path to folder:"),
  #   verbatimTextOutput(outputId = "dir_path"),
  #   h4("Files in that folder:"),
  #   verbatimTextOutput(outputId = "files")
  # )),

  # Output:
  # DataTable containing contents of merged (raw + meta)data.frame
  fluidRow(column(
    width = 12,
    dataTableOutput("raw_contents") %>% withSpinner(color="#FC7018")
  )),


# 2. Mapping --------------------------------------------------------------

  # Input:
  # Uploading Design
  fluidRow(column(
    width = 4,
    h3("2a. Upload design file (.csv):")
  )),

  # Get path to design CSV from user
  fluidRow(column(
    width = 4,
    shiny::fileInput("design_fname", label="", accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv"))
  )),

  # Output:
  # DataTable containing contents of design file
  fluidRow(column(
    width = 12,
    dataTableOutput("design_contents") %>% withSpinner(color="#FC7018")
  )),


# # 2. (b) Ask user to choose columns describing 96-well plate num --------

  fluidRow(
    column(width = 12,
           h3("2b. Indicate the two design columns describing the 96-well plate numbers (1-16) and locations (e.g. M13)"))
    ),

  fluidRow(
    # Generate selectInput with choices from columns in design file - this will be the 96-well plate number
    column(width = 3, uiOutput("design_select_num96")),
    # Generate selectInput with choices from columns in design file  - this will be the 96-well well locations
    column(width = 3, uiOutput("design_select_pos96"))
  ),

  fluidRow(
    column(width = 12,
           # Create an actionButton that must be observed to change before running the mapping functions on the chosen vars above.
           actionButton("ready_to_map", "Ready to map signals")
           )
  ),

  fluidRow(
    column(width = 6,
      # Explain what 'long' data is
      htmlOutput("mapped_explain"),
      # Show the long data
      dataTableOutput("mapped_contents") %>% withSpinner(color="#FC7018"),
      # Allow user to download long data
      downloadButton("download_mapped_data", "Download Mapped Data (Long)")
    ),
    column(width = 6,
      # Explain what 'wide' data is
      htmlOutput("mapped_explain_wide"),
      # Display the wide data
      dataTableOutput("mapped_contents_wide") %>% withSpinner(color="#FC7018"),
      # Allow user to download wide data
      downloadButton("download_mapped_data_wide", "Download Mapped Data (Wide)")
    )
  ),

# 3. Select grouping variables specific to experimental configurat --------

  fluidRow(column(
    width = 12,
    h3("3. Select the columns describing the assay setup and signals")
  )),
  
  fluidRow(column(width = 4, uiOutput("condition_cols")), # get grouping vars from user (this will be a selectizeInput)
           column(width = 4, uiOutput("dose_col")), # get dose var (selectInput)
           column(width = 4, uiOutput("response_col"))), # get response var (selectInput)
  fluidRow(column(width = 12, actionButton("ready_to_fit", "Ready to fit"))), # Fit upon changes in this actionButton

  fluidRow(
    column(width = 12,
    # Explain that this table is filterable
    htmlOutput("fitted_explain"),
    # Display fitted values
    dataTableOutput("fitted_contents") %>% withSpinner(color="#FC7018"),
    # Allow user to download fitted values (long)
    downloadButton("download_fitted_data", "Download Fits (Long)"))
  ),
  fluidRow(
    column(width = 12,
    # Show user a plot of the selected row
    plotOutput("fit_plotted") %>% withSpinner(color="#FC7018"),
    # Allow user to download a .png of the plot
    downloadButton("download_plot", "Save Plot (.png)"))
  ),

  fluidRow(
    column(width = 12,
    # Explain that this is a wide version of the first fitted table
    htmlOutput("fitted_explain_wide"),
    # Display the wide version
    dataTableOutput("fitted_contents_wide") %>% withSpinner(color="#FC7018"),
    # Alow user to download the wide data
    downloadButton("download_fitted_data_wide", "Download Fits (Wide)"))
  )

)


# SERVER ------------------------------------------------------------------

server <- function(input, output, session) {
  

# 1. File Upload ----------------------------------------------------------
  
  shinyFiles::shinyDirChoose(input, 'dir',
                 roots = c(home = '~'), filetypes = c('csv', 'txt'))
  
  # Path to dir
  dir_path <- reactive({
    req(input$dir)
    home <- normalizePath("~")
    file.path(home,
              paste(unlist(input$dir$path[-1]),
              collapse = .Platform$file.sep))
  })
  
  
  # Show user the path to their chosen directory
  # output$dir_path <- renderPrint(dir_path())
  
  # Compile list of files in directory, selecting only "Assay" files and not "Standard_Curve"
  file_list <- reactive({
    req(input$dir)
    list.files(dir_path(), full.names = T)
    # all = list.files(dir_path(), full.names = T)
    # assays = all[!grepl("stand|standard", all, ignore.case = T)]
    # assays
  })
  
  # Deprecated:
  # Show user the Assay files in their chosen directory
  # output$files <- renderPrint(file_list())
  
  # Read and collapse each assay file into one data.frame, joined with associated metadata (parsed from filename)
  raw_data <- reactive({
    tryCatch({
    
      # Parse file names into (meta)data.frame called 'fmeta'
      meta = purrr::map_df(file_list(), parse_fname) %>%
        dplyr::mutate(p384=as.integer(p384)) %>%
        # Filter out standard curve files
        dplyr::filter(!is.na(p384))
      
      # Name vector of filenames (meta$fname) with their values (stupid, I know, but necessary for map_df below to retain file names in       the full, 12*384 row raw data.frame)
      fnames_named = meta$fname %>% purrr::set_names(nm = meta$fname)
      
      # Read raw files, cut off at end of signal ratio section, into data.frame called 'raw'
      raw = purrr::map_df(fnames_named, readr::read_csv, skip=1, n_max=384, 
                           col_types=cols_only(
                             Well = col_character(),
                             Signal = col_integer(),
                             Signal_1 = col_integer(),
                             Ratio = col_double()
                           ), .id = "fname"
                          )
      
      # Join metadata and raw files to get the final data.frame
      df_out = dplyr::left_join(meta, raw, by = "fname") %>%
        rename(well384=Well, channel_1=Signal, channel_2=Signal_1, ratio=Ratio) %>%
        # Call map384_96 onto the new variables
        mutate(info_96 = pmap(list(num384=p384, pos384=well384), map384_96)) %>%
        # Split apart info_96 into separate columns
        mutate(info_96 = map(info_96, as_tibble)) %>%
        unnest()
      
    },
    error = function(e) {
      stop(safeError(e))
    })
  
    return(df_out)
    
  })
  
  # Show user the resulting raw data.frame in a DataTable, paginated and searchable
  output$raw_contents <- renderDataTable(DT::datatable(raw_data(),
                                                   options = list(
                                                    paging = T, autoWidth = T
                                                   ))
                                     )
  
# 2. (a) Read in design file -------------------

  design_df <- reactive({
    
    req(input$design_fname)
    
    tryCatch({
      df = readr::read_csv(input$design_fname$datapath)
    },
    error = function(e) {
      stop(safeError(e))
    })
    
    return(df)
    
  })
  
  # Render design contents for user viewing
  output$design_contents <- renderDataTable(DT::datatable(design_df(),
                                                    options = list(
                                                      scrollX = T, paging = T
                                                    ))
  )
  

# 2. (b) Get input from user on which columns in the design file describe 96-well location and plate number (to use as keys for joining with results) ----------
  
  # Generate selectInput for 96-well plate number
  output$design_select_num96 = renderUI({
    selectInput("design_chosen_num96",
                "Plate number:",
                selected = "P96",
                choices = colnames(design_df()))
  })
  
  # Generate selectInput for 96-well plate well locations
  output$design_select_pos96 = renderUI({
    selectInput("design_chosen_pos96",
                "Well location:", 
                selected = "TPOS",
                choices = colnames(design_df()))
  })
  
  # Create reactive object from the two previous selectInputs
  design_join_cols <- reactive({
    req(input$design_chosen_num96, input$design_chosen_pos96)
    
    cols_out = c("num96" = input$design_chosen_num96, "pos96" = input$design_chosen_pos96)
    return(cols_out)
  })
  

# 2. (c, hidden): joining -------------------------------------------------

  # Upon click of the "Ready to map" actionButton, join the raw data and the design on the chosen columns
  mapped_df <- eventReactive(input$ready_to_map, {
    keys = design_join_cols()
    raw_data() %>%
      dplyr::select(-fname) %>%
      dplyr::left_join(design_df(), by = keys)
  })
  
  # Explain what long data is
  observeEvent(input$ready_to_map, {
    output$mapped_explain <- renderText({
    "<h4><b>Long</b> format:<br>Each well gets <b>n</b> rows, one for each of the <b>n</b> analytes </h4>"
    })
  })
  
  # Display the long (mapped) data  
  output$mapped_contents <- renderDataTable({
    DT::datatable(mapped_df() %>% dplyr::select(-c(p384, well384, row96, col96)),
                  options = list(
                    scrollX = T, paging = T
                  ))
  })
  
  output$download_mapped_data <- downloadHandler(
    filename = function(){paste0("results_mapped_to_design_", format(Sys.time(), "%Y%m%d_%H%M_%p"), ".csv")},
    content = function(file){
      mapped_df() %>%
        readr::write_csv(file)
    }
  )
  
  # Take the long data and 'spread' - this function is found in fit_fx.R - basically equivalent to the 'pivot_wider' function under development for the new version of tidyr
  mapped_df_wide <- reactive({
    mapped_df() %>%
      myspread(cytokine, c(channel_1, channel_2, ratio))
  })
  
  # Explain what wide data is
  observeEvent(input$ready_to_map, {
    output$mapped_explain_wide <- renderText({
      "<h4><b>Wide</b> format:<br>Each well gets one row, with three columns (channel 1, channel 2, ratio) for each analyte</h4>"
    })
  })
  # Display the wide (mapped) data
  output$mapped_contents_wide <- renderDataTable({
    DT::datatable(mapped_df_wide() %>% dplyr::select(-c(p384, well384, row96, col96)),
                  options = list(
                    scrollX = T, paging = T
                  ))
  })
  
  output$download_mapped_data_wide <- downloadHandler(
    filename = function(){paste0("results_mapped_to_design_", format(Sys.time(), "%Y%m%d_%H%M_%p"), ".csv")},
    content = function(file){
      mapped_df() %>%
        readr::write_csv(file)
    }
  )
  

# 3. Fitting curves and plotting ------------------------------------------

    # Render all of the selectInputs for the columns describing the setup of the experiments in each well
    output$condition_cols <- renderUI({
    selectizeInput(inputId = "chosen_condition_cols", label = "a) Experimental conditions", choices = as.list(set_names(colnames(mapped_df()), colnames(mapped_df()))), multiple=T, selected = c("TargetCell", "ETRatio", "ACTRDensity", "DonorRunID"))
  })
  
  output$dose_col <- renderUI({
    selectInput(inputId = "chosen_dose_col", label = "b) Dose variable (x)", choices = as.list(set_names(colnames(mapped_df()), colnames(mapped_df()))),
                selected = "Ab ng/mL")
  })
  
  output$response_col <- renderUI({
    selectInput(inputId = "chosen_response_col", label = "b) Response variable (y)", choices = as.list(set_names(colnames(mapped_df()), colnames(mapped_df()))),
                selected = "ratio")
  })
  
  # Currently this isn't displayed, but leaving it in in case this is desired...
  output$chosen_vars <- renderPrint({
    input$chosen_condition_cols
  })
  
  # Carry out the (tidyr::)nesting, fitting, extracting of fit parameters and GOF
  fitted_df <- eventReactive(input$ready_to_fit, {

    mapped_df() %>%
      rename(dose = input$chosen_dose_col) %>%
      mutate(dose = ifelse(dose <= 0, 1e-2, dose)) %>%
      dplyr::group_by(cytokine, !!!syms(input$chosen_condition_cols)) %>%
      tidyr::nest(.key="group_data") %>%
      mutate(fit = purrr::map(group_data, fit_curve, input$chosen_response_col, "dose")) %>%
      mutate(Rsq = map_dbl(fit, get_rsq)) %>%
      unnest(Rsq) %>%
      mutate(parameters = map(fit, get_params)) %>%
      mutate(parameters = map(parameters, as_tibble)) %>%
      unnest(parameters) %>%
      arrange(desc(Rsq))
    
  })
  
  # Helper function for creating actionButtons in each row of the displayed DataTable
  shinyInput <- function(FUN, len, id, ...) {
    inputs <- character(len)
    for (i in seq_len(len)) {
      inputs[i] <- as.character(FUN(paste0(id, i), ...))
    }
    inputs
  }
  
  # Explain that these are the fits and they are filterable
  output$fitted_explain <- renderText({
    if(!is.null(fitted_df())){
  return("<h4>This table is filterable! Click the empty box under column headers to pull up a text input search box for categorical columns, or a slider for numeric columns. If the slider range is too wide, you can type a custom range: e.g. in the box under Rsq, typing  '0.8 ... 1' will select rows with Rsqs between those two values. </h4>")}
    })
  # Display the fits and add a 'plot' column to hold the actionButtons
  output$fitted_contents <- renderDataTable({
    fitted_df() %>%
      mutate(plot = shinyInput(actionButton, nrow(fitted_df()), 'button_', label = "Plot", onclick = 'Shiny.onInputChange(\"plot_button\",  this.id)' )) %>%
      dplyr::select(plot, everything()) %>%
      dplyr::select(-c(group_data, fit, IC50)) %>%
      DT::datatable(escape = FALSE, selection = 'none', filter = 'top', options = list(scrollX = T))
  })
  
  output$download_fitted_data <- downloadHandler(
    filename = function(){paste0("curve_fit_data_", format(Sys.time(), "%Y%m%d_%H%M_%p"), ".csv")},
    content = function(file){
      fitted_df() %>%
        dplyr::select(-c(group_data, fit, IC50)) %>%
        readr::write_csv(file)
    }
  )
  

# Plotting ----------------------------------------------------------------

  # selected_row holds the onclick value from the plot columns in the above DataTable
  selected_row <- reactiveVal(1)
  
  # This observer updates the value of selected_row whenever a plot_button actionButton is clicked
  observeEvent(input$plot_button, {
    new_row <- as.numeric(strsplit(input$plot_button, "_")[[1]][2])
    selected_row(new_row)
  }
  )
  
  # Function to be used in stat_function. This is the 4-parameter logistic curve.
  y_hat <- function(x,A,B,C,D){
    return(D + ((A-D)/(1+(x/C)^B)))
  }
  
  # This function returns a ggplot of the selected fit
  curve_plot <- function(){
    # Get the input data.frame of doses and responses from the selected row
    fitted_data <- fitted_df()$fit[[selected_row()]]$data
    
    # Get the parameters and Rsq from the selected fit
    fitted_params <- fitted_df() %>%
      slice(selected_row()) %>% 
      mutate(Window = UpperLimit/LowerLimit) %>%
      select(Lower = LowerLimit, Slope, EC50, Upper = UpperLimit, Window, Rsq) %>%
      unlist() %>%
      round(3)
    fitted_params_string <- paste(names(fitted_params), fitted_params, sep = ":  ", collapse = "\n")
    
    # Get the experimental conditions from the selected fit
    fitted_conditions <- fitted_df() %>%
      slice(selected_row()) %>%
      select(cytokine, TargetCell, DonorRunID, ACTRDensity, ETRatio) %>%
      unlist()
    fitted_conditions_string <- paste(names(fitted_conditions), fitted_conditions, sep = ":  ", collapse = "\n")
    
    # Plot the points and 4PL curve, with annotations of fitted_params, and fitted_conditions
    ggplot(fitted_data, aes(x = Dose, y = Response)) +
      scale_x_log10(breaks = c(1e-2, 1e0, 1e1, 1e2, 1e3, 1e4, 1e5, 1e6)) +
      scale_y_continuous(limits = c(-1e0, 6e3)) +
      stat_function(fun = y_hat,
                    args = list(A = fitted_params["Lower"],
                                B = fitted_params["Slope"],
                                C = fitted_params["EC50"],
                                D = fitted_params["Upper"]),
                    size = 1,
                    colour = "#333333") +
      geom_point(size = 3, colour = "#FC7018") +
      annotate("label",
               x = c(1e-2, 1e0),
               y = c(5000, 5000),
               label = c(fitted_conditions_string, fitted_params_string),
               hjust = 0,
               vjust = 1) +
      theme(axis.title = element_text(size = 12, face = 'bold'),
            axis.text = element_text(size = 12)
            )
  }
  
  # Take the output of curve_plot and render a plotOutput
  output$fit_plotted <- renderPlot({
    curve_plot()
  })
  
  # filenames for saved plots 
  curve_filename <- function(){fitted_df() %>%
      dplyr::select(!!!syms(input$chosen_condition_cols)) %>%
      slice(selected_row()) %>%
      unlist(., use.names = FALSE) %>%
      paste0(paste(., collapse="_"), ".png")}
  
  # Save the plots
  output$download_plot <- downloadHandler(
    filename = function(){curve_filename()},
    content = function(file){
      ggsave(file, plot = curve_plot(), width = 9, height = 6)
    },
    contentType = "image/png"
  )
  

# Prepare wide data.frame for user to download    -------------------------

  fitted_df_wide <- reactive({
    
    fitted_df() %>%
      dplyr::select(-c(group_data, fit, IC50)) %>%
      myspread(cytokine, c(Rsq, LowerLimit, Slope, EC50, UpperLimit))
    
  })
  
  output$fitted_explain_wide <- renderText({
    if(!is.null(fitted_df_wide())){
      return("<h4>Fitted curves in <b>wide<\b> format. This table is also filterable. </h4>")}
  })
  
  output$fitted_contents_wide <- renderDataTable({
    DT::datatable(fitted_df_wide(), escape = FALSE, selection = 'none', filter = 'top',
                  options = list(
                    scrollX = T, paging = T
                  ))
  })
  
  output$download_fitted_data_wide <- downloadHandler(
    filename = function(){paste0("curve_fits_wide_", format(Sys.time(), "%Y%m%d_%H%M_%p"), ".csv")},
    content = function(file){
      fitted_df_wide() %>%
        readr::write_csv(file)
    }
  )
  
}
shinyApp(ui, server)