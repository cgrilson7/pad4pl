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
        font-weight: 600;
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

  fluidRow(column(
    width = 4,
    shinyFiles::shinyDirButton("dir", "Choose folder containing EnVision Assay .csv files", "Upload")
  )),
  
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
    column(width = 3, uiOutput("design_select_num96")), column(width = 3, uiOutput("design_select_pos96"))
  ),
  fluidRow(
    column(width = 12,
           actionButton("ready_to_map", "Ready to map signals")
           )
  ),

  fluidRow(
    column(
      width = 12,
      h4("Results mapped to design:"),
      dataTableOutput("mapped_contents") %>% withSpinner(color="#FC7018")
    )
  ),
  fluidRow(column(width = 12, downloadButton("download_mapped_data", "Download Mapped Data (Long)"))),

  fluidRow(
    column(
      width = 12,
      h4("Results mapped to design:"),
      dataTableOutput("mapped_contents_wide") %>% withSpinner(color="#FC7018")
    )
  ),
  fluidRow(column(width = 12, downloadButton("download_mapped_data_wide", "Download Mapped Data (Wide)"))),

# 3. Select grouping variables specific to experimental configurat --------

  fluidRow(column(
    width = 12,
    h3("3. Select the columns describing the assay setup and signals")
  )),
  
  fluidRow(column(width = 4, uiOutput("condition_cols")),
           column(width = 4, uiOutput("dose_col")),
           column(width = 4, uiOutput("response_col"))),
  fluidRow(column(width = 12, actionButton("ready_to_fit", "Ready to fit"))),
  fluidRow(column(width = 9, h4("This table is filterable! Click the empty box under column headers to pull up a text input search box for categorical columns, or a slider for numeric columns. If the slider range is too wide, you can type a custom range: e.g. in the box under Rsq, typing  '0.8 ... 1' will select rows with Rsqs between those two values."))),
  
  fluidRow(
    column(width = 12,
    dataTableOutput("fitted_contents") %>% withSpinner(color="#FC7018"),
    downloadButton("download_fitted_data", "Download Fits (Long)"))),
  fluidRow(
    column(width = 12,
    plotOutput("fit_plotted") %>% withSpinner(color="#FC7018"),
    downloadButton("download_plot", "Save Plot (.png)"))
  ),
  fluidRow(column(width = 12, downloadButton("download_fitted_data", "Download Fits (Long)"))),
  # fluidRow(column(
  #     width = 12,
  #   plotOutput("fit_plotted") %>% withSpinner(color="#FC7018")
  # )),

  fluidRow(
    column(
    width = 12,
    h4("Preparing data for download in wide format"),
    dataTableOutput("fitted_contents_wide") %>% withSpinner(color="#FC7018")
  )),
  fluidRow(column(width = 12, downloadButton("download_fitted_data_wide", "Download Fits (Wide)")))

)


# SERVER ------------------------------------------------------------------

server <- function(input, output, session) {
  

# 1. File Upload ----------------------------------------------------------
    
  shinyFiles::shinyDirChoose(input, 'dir',
                 roots = c(home = '~'), filetypes = c('csv', 'txt'))
  
  # Tell the user that reading / mapping is in progress:
  # output$wait_message <- renderPrint(paste0("Currently reading the files in ", input$dir$path[-1], " and carrying out deconvolution of 384-well signals to source 96-well plate locations"))
  
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
      
      # Name vector of filenames (meta$fname) with their values (stupid, I know, but necessary for map_df below to retain file names in the full, 12*384 row raw data.frame)
      
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
  
  output$design_contents <- renderDataTable(DT::datatable(design_df(),
                                                    options = list(
                                                      scrollX = T, paging = T
                                                    ))
  )
  

# 2. (b) Get input from user on which columns in the design file describe 96-well location and plate number (to use as keys for joining with results) ----------
  
  output$design_select_num96 = renderUI({
    selectInput("design_chosen_num96",
                "Plate number:",
                selected = "P96",
                choices = colnames(design_df()))
  })
  
  output$design_select_pos96 = renderUI({
    selectInput("design_chosen_pos96",
                "Well location:", 
                selected = "TPOS",
                choices = colnames(design_df()))
  })
  
  design_join_cols <- reactive({
    req(input$design_chosen_num96, input$design_chosen_pos96)
    
    cols_out = c("num96" = input$design_chosen_num96, "pos96" = input$design_chosen_pos96)
    return(cols_out)
  })
  

# 2. (c, hidden): joining -------------------------------------------------

  mapped_df <- eventReactive(input$ready_to_map, {
    keys = design_join_cols()
    raw_data() %>%
      dplyr::select(-fname) %>%
      dplyr::left_join(design_df(), by = keys)
  })
  

  
  output$mapped_contents <- renderDataTable({
    DT::datatable(mapped_df(),
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
  
  mapped_df_wide <- reactive({
    mapped_df() %>%
      myspread(cytokine, c(channel_1, channel_2, ratio))
  })
  
  output$mapped_contents_wide <- renderDataTable({
    DT::datatable(mapped_df_wide(),
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
  
  # Carry out the (tidyr::)nesting, 
  
  output$chosen_vars <- renderPrint({
    input$chosen_condition_cols
  })
  
  fitted_df <- eventReactive(input$ready_to_fit, {

    mapped_df() %>%
      rename(dose = input$chosen_dose_col) %>%
      mutate(dose = ifelse(dose <= 0, 0.01, dose)) %>%
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
  
  shinyInput <- function(FUN, len, id, ...) {
    inputs <- character(len)
    for (i in seq_len(len)) {
      inputs[i] <- as.character(FUN(paste0(id, i), ...))
    }
    inputs
  }
  
  output$fitted_contents <- renderDataTable({
    fitted_df() %>%
      mutate(plot = shinyInput(actionButton, nrow(fitted_df()), 'button_', label = "Plot", onclick = 'Shiny.onInputChange(\"plot_button\",  this.id)' )) %>%
      dplyr::select(plot, everything()) %>%
      dplyr::select(-c(group_data, fit, IC50)) %>%
      DT::datatable(escape = FALSE, selection = 'none', filter = 'top', options = list(scrollX = T))
  })
  
  output$download_fitted_data <- downloadHandler(
    filename = function(){paste0("curve_fits_", format(Sys.time(), "%Y%m%d_%H%M_%p"), ".csv")},
    content = function(file){
      fitted_df() %>%
        dplyr::select(-c(group_data, fit, IC50)) %>%
        readr::write_csv(file)
    }
  )
  
  
  ## For plotting in a new window:
  # define modal
  # plotModal <- function() {
  #   modalDialog(
  #     plotOutput("fit_plotted")
  #   )
  # }
  
  selected_row <- reactiveVal(1)
  curve_plot <- function(){fitted_df()$fit[[selected_row()]] %>% plot()}
  curve_filename <- function(){fitted_df() %>% select(cytokine:ETRatio) %>% slice(selected_row()) %>% unlist(., use.names = FALSE) %>% paste0(paste(., collapse="_"), ".png")}
  
  observeEvent(input$plot_button, {
                 new_row <- as.numeric(strsplit(input$plot_button, "_")[[1]][2])
                 selected_row(new_row)
                # showModal(modalDialog(plotOutput("fit_plotted", width = "70%")))
    }
  )
  
  output$fit_plotted <- renderPlot({
    print(curve_plot())
  })
  
  output$download_plot <- downloadHandler(
    filename = function(){curve_filename()},
      #paste0(paste("curve",
                                      # paste(fitted_df()[selected_row(), 1:5], collapse = "_"),
                                       # format(Sys.time(), "%Y%m%d_%H%M_%p"), sep="_"),".png"),
    content = function(file){
      ggsave(file, curve_plot())
    },
    contentType = "image/png"
  )
  
  ## Prepare wide data.frame for user to download
  
  fitted_df_wide <- reactive({
    
    fitted_df() %>%
      dplyr::select(-c(group_data, fit, IC50)) %>%
      myspread(cytokine, c(Rsq:UpperLimit))
    
  })
  
  output$fitted_contents_wide <- renderDataTable({
    DT::datatable(fitted_df_wide(),
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