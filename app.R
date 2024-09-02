# Nitric Oxide Analysis App
# Built by Tim Fulton, Oct 2022

# Source helper script
source("scripts/utils.R")

# Define UI ----
ui <- page_fillable(
  padding = 15,
  gap = 15,
  titlePanel(HTML("<b>Nitric Oxide Analysis</b>"), "Nitric Oxide Analysis"),
  style = "background-color: #EBEDF1;",
  tags$style(".progress-bar{background-color:#083A57;}"),
  tags$style(HTML(".js-irs-0 .irs-single, .js-irs-0 .irs-bar, .js-irs-0 .irs-handle, js-irs-0 .irs-handle {background: #083A57} .js-irs-0 .irs-handle:hover, js-irs-0 .irs-handle:active {background: #083A57}")),
  tags$style(HTML(".js-irs-1 .irs-single, .js-irs-1 .irs-bar, .js-irs-1 .irs-handle, js-irs-1 .irs-handle {background: #849DAB} .js-irs-1 .irs-handle:hover, js-irs-1 .irs-handle:active {background: #849DAB}")),
  layout_columns(
    fill = FALSE,
    gap = 25,
    col_widths = c(3, 9),
    layout_column_wrap(
      height = 460,
      width = 1,
      card(
        card_header("Load Data", tooltip(bs_icon("info-circle"), "Load your data using the browse button. Visitors can load demo data by clicking the button below.", placement = "right")),
        min_height = 230,
        max_height = 230, 
        style = "border-radius: 6px; box-shadow: 3px 3px 3px gray", 
        fileInput(inputId = "upload", label = NULL, placeholder = "Upload Excel",  multiple = FALSE, accept = ".xlsx", width = 400),
        actionButton(inputId = "load_demo", label = "Load demo data", width = 200)
      ),
      card(
        card_header("Raw Data Smoothing Parameter", tooltip(bs_icon("info-circle"), "The raw data points (gray circles) are fit with a cubic smoothing spline (dark blue line). The slider adjusts the smoothing parameter. Llower values result in less smoothing and higher values result in more smoothing.", placement = "right")),
        max_height = 150, 
        style = "border-radius: 6px; box-shadow: 3px 3px 3px gray", 
        sliderInput("smooth_parameter", label = NULL, min = 0.01, max = 0.25, value = 0.05, step = 0.01, width = 400),
      ),
    ),
    card(
      card_header("Raw and Smoothed Data Plot"),
      #max_height = 392, 
      max_height = 480,
      style = "border-radius: 6px; box-shadow: 3px 3px 3px gray",
      withSpinner(
        plotOutput("spline_plot"), 
        color = '#083A57',
        type = 5,
        size = 0.5
      )
    )
  ),
  layout_columns(
    fill = FALSE,
    gap = 25,
    col_widths = c(3, 9),
    layout_column_wrap(
      height = 400,
      max_height = 300,
      width = 1,
      card(
        card_header("Peak Threshold", tooltip(bs_icon("info-circle"), "Adjust the slider to exclude any false peaks. Peaks below the threshold will be excluded from the area under curve analysis.", placement = "right")),
        min_height = 150, 
        max_height = 150, 
        style = "border-radius: 6px; box-shadow: 3px 3px 3px gray", 
        sliderInput("min_peak_height", label= NULL, min = 0.5, max = 10, value = 1.0, step = 0.1, width = 400)
      ),
      card(
        card_header("Peaks and Area Under Curve (AUC) Table"),
        min_height = 550,
        style = "border-radius: 6px; box-shadow: 3px 3px 3px gray", 
        tableOutput("peak_table"),
        downloadButton("export_auc_data", label = "Download Data")
      )
    ),
    card(
      card_header("Peaks and Area Under Curve (AUC) Plot"),
      min_height = 500,
      style = "border-radius: 6px; box-shadow: 3px 3px 3px gray",
      withSpinner(
        plotOutput("peak_plot"), 
        color = '#083A57',
        type = 5,
        size = 0.5
      )
    )
  )
)


# Define Server ----
server <- function(input, output) {
  
  # Define a reactive value to store the selected data
  selected_df <- reactiveVal(NULL)

  # Load the uploaded or demo data based on user action
  observeEvent(input$upload, {
    req(input$upload)
    selected_df(input$upload$datapath)
  })

  # Load the demo data
  observeEvent(input$load_demo, {
    selected_df("data/run3.xlsx")  
  })

  # Process the selected data frame
  upload_df <- reactive({
    req(selected_df())
    load_and_process_data(selected_df())  # Process Data
  })
  
  
  ## Perform spline fit and baseline correction ##
  clean_data <- reactive({

    raw_data <- upload_df()

    ## Create spline fit ##
    # create new variable with spline fit data
    lowpass_spline <- smooth.spline(raw_data$Time, raw_data$NO, spar = input$smooth_parameter)

    # extract spline list data and add it to the main data frame as numeric
    raw_data$spline10 <- (as.numeric(lowpass_spline$y))

    # create temp data frame with only Time and Spline Fit so can convert to matrix to use for baseline correction
    raw_data_temp <- raw_data %>%
      dplyr::select(Time_Min, spline10)

    # create matrix to use in data correction
    temp_matrix <- as.matrix(pivot_wider(data = raw_data_temp, names_from = Time_Min, values_from = spline10))

    # use baseline function to correct for signal drift
    base_correction <- baseline(temp_matrix)

    # extract corrected data from base_correction and add to the main data frame
    raw_data$NObasecorr <- as.vector(getCorrected(base_correction))

    raw_data

  })
  
  
  ## Render the Spline Plot ##
  output$spline_plot <- renderPlot({
    
    clean_data <- clean_data()
    
    ggplot(data = clean_data, aes(x = Time_Min, y = NO)) +
      labs(
        x = "Time (min)",
        y = "Raw NO (mV)"
      ) +
      theme_classic() + 
      theme(
        axis.title.x = element_text(size = 16, face = "bold", color = "black", margin = margin(t = 10)),
        axis.title.y = element_text(size = 16, face = "bold", color = "black", margin = margin(r = 10)),
        axis.text = element_text(size = 14, color = "black")
      ) +
      geom_point(
        size = 1.2,
        shape = 21,
        fill = "white", 
        color = "gray70"
      ) +
      geom_line(
        aes(x = Time_Min, y = spline10), 
        color = "#083A57",
        linewidth = 1.3) +
      scale_x_continuous(breaks=seq(0, max(clean_data$Time_Min), by = round((max(clean_data$Time_Min)/12), 0)))
  
  })

  
  ## Create the peak plot and table ##
  peak_plot_and_table <- reactive({
    
    clean_data <- clean_data()
    
    # assign peaks to data frame  
    peaks <- as.data.frame(findpeaks(clean_data$NObasecorr, minpeakheight = input$min_peak_height))
    
    # change column names
    colnames(peaks) <- c("Peak_Value", "Peak_Index", "Start_Index", "End_Index")
    
    # assign peak time to peaks data frame
    peaks$Peak_Time <- clean_data$Time_Min[peaks$Peak_Index]
    peaks$Start_Time <- clean_data$Time_Min[peaks$Start_Index]
    peaks$End_Time <- clean_data$Time_Min[peaks$End_Index]
    peaks$Start_Value <- clean_data$NObasecorr[peaks$Start_Index]
    peaks$End_Value <- clean_data$NObasecorr[peaks$End_Index]
    
    ## Loop to assign peak number looking forward in time ##
    # initialize new column in main data frame
    clean_data$peak_number <- c(NA)
    
    # initialize i starting with 1, and peak_index starting with first row
    i <- 1
    peak_index <- peaks[i, 2]
    
    # for loop
    for (i in 1:nrow(peaks)) {
      
      while (clean_data$NObasecorr[peak_index] > 0) {
        clean_data$peak_number[peak_index] <- i
        peak_index <- peak_index + 1
      }
      
      i = i + 1
      peak_index <- peaks[i, 2]
    }
    
    ## Loop to assign peak number looking backward in time ##
    # initialize i starting with 1, and peak_index starting with first row
    i <- 1
    peak_index <- peaks[i, 2]
    
    # for loop
    for (i in 1:nrow(peaks)) {
      
      while (clean_data$NObasecorr[peak_index] > 0) {
        clean_data$peak_number[peak_index] <- i
        peak_index <- peak_index - 1
      }
      
      i = i + 1
      peak_index <- peaks[i, 2]
    }
    
    ## Create new data frame with Peak Number and AUC calculations ##
    # group by peak number and calculate the AUC for each peak
    peaks_auc <- clean_data %>%
      drop_na() %>%
      group_by(peak_number) %>%
      summarise(round(auc(Time_Min, NObasecorr, type = "spline"), 2))
    
    # change column names
    colnames(peaks_auc) <- c("Peak_Number", "Peak_AUC")
    
    peaks_temp <- cbind(peaks, peaks_auc)
    
    # add column for labeling in the plot
    peaks_temp$Peak_Num_AUC <- paste("Peak #: ", peaks_temp$Peak_Number, "\nPeak Value: ", round(peaks_temp$Peak_Value, 2), "\nAUC: ", peaks_temp$Peak_AUC)
    
    # error control
    validate(
      need(nrow(peaks) == nrow(peaks_auc), "Please increase peak threshold or smoothing parameter."),
    )
    
    # combine peaks df and peaks_auc df
    peaks_final <- peaks_temp
    
    # create table
    peak_table_df <- as.data.frame(peaks_final[ , 10:11])
    
    # create data frame for geom_ribbon
    raw_data_ribbon <- drop_na(clean_data)
    
    # create figure
    peak_plot <- ggplot(data = clean_data, aes(x = Time_Min, y = NO)) +
      labs(
        x = "Time (min)",
        y = "Normalized NO (mV)"
      ) +
      theme_classic() + 
      theme(
        panel.grid.major.y = element_line(color = "gray", linetype = "dotted"),
        panel.grid.minor.y = element_line(color = "gray", linetype = "dotted"),
        axis.title.x = element_text(size = 16, face = "bold", color = "black", margin = margin(t = 10)),
        axis.title.y = element_text(size = 16, face = "bold", color = "black", margin = margin(r = 10)),
        axis.text = element_text(size = 14, color = "black")
      ) +
      geom_ribbon(
        data = raw_data_ribbon, aes(ymax=NObasecorr, ymin=0),
        fill="#C1CED5"
      ) +
      geom_hline(
        yintercept = 0,
        color = "darkgray"
      ) +
      geom_line(
        aes(x = Time_Min, y = NObasecorr), 
        color = "#083A57",
        linewidth = 1.2
      ) +
      geom_point(
        data = peaks_temp, aes(x = Peak_Time, y = Peak_Value),
        shape = 21,
        size = 4,
        color = "#E26743",
        fill = "white"
      ) +
      geom_label_repel(
        data = peaks_temp, aes(x = Peak_Time, y = Peak_Value, label = Peak_Num_AUC),
        nudge_x = 0,
        nudge_y = 3
      ) +
      scale_x_continuous(breaks=seq(0, max(clean_data$Time_Min), by = round((max(clean_data$Time_Min)/12), 0)))
  
    peak_plot_and_table <- list(peak_plot, peak_table_df)
    
    peak_plot_and_table
    
    })
  
  ## Render the Peak Plot ##
  output$peak_plot <- renderPlot({peak_plot_and_table()[[1]]})
  
  
  ## Render the Peak Plot ##
  output$peak_table <- renderTable({
    
    peak_table <- peak_plot_and_table()[[2]]
    
    colnames(peak_table) <- c("Peak Number", "Area Under Curve (AUC)")
    
    return(peak_table)
    
  }, hover = TRUE, align = c("cc"))
  
  
  ## Output Raw and Model Fit Table for Download ##
  output$export_auc_data <- downloadHandler(
    
    filename = function() {
      if (!is.null(input$upload)) {
        # for user-uploaded file
        uploaded_filename <- input$upload$name
      } else {
        # for demo data file
        uploaded_filename <- "demo.xlsx"
      }
      
      # remove the original file extension
      filename_without_extension <- sub("\\.xlsx$", "", uploaded_filename)
      
      # ddd custom file extension
      paste0(basename(filename_without_extension), "_auc_data.xlsx")
    },
    
    content = function(file) {
      write.xlsx(peak_plot_and_table()[[2]], file)
    }
  )
}


# Run the app ----
shinyApp(ui = ui, server = server)
