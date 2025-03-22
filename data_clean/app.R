#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(tidyverse)

# UI definition for main file input page
ui <- fluidPage(

    # Application title
    titlePanel("Cal Poly Baseball Data Input and Cleaning"),

    # Sidebar with season year input
    sidebarLayout(
        sidebarPanel(
            selectInput(inputId="year",
                        label="Choose a season",
                        choices=2000:2100,
                        selected=2025
                  )
        ),

    # Upload file option
    mainPanel(
      fileInput(inputId="file_upload",
                label="Upload Trackman CSV",
                multiple=FALSE,
                accept=".csv"
      ),
      actionButton(inputId="file_submit",
                   label="Submit"),
      tableOutput("preview")
    )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  data_clean <- reactive({
    req(input$file_upload)
    df_trackman <- read_csv(input$file_upload$datapath)
    df_cleaned <- 
      df_trackman |>
      select(PitchNo:HitSpinAxis) |>
      mutate(
        IsBrl = case_when(
          # Exit velo 93+ and launch angle 5-15
          ExitSpeed >= 93 & Angle >= 5 & Angle <= 15 ~ 1,
          # Exit velo 95+ and launch angle 15-35
          ExitSpeed >= 95 & Angle > 15 & Angle <= 35 ~ 1,
          # Exit velo 98+ and launch angle > 35
          ExitSpeed >= 98 & Angle > 35 ~ 1,
          # Exit velo 100+ at any launch angle
          ExitSpeed >= 100 ~ 1,
          # Everything else is not a barrel
          TRUE ~ 0
        ),
        HardHit = ifelse(ExitSpeed >= 95 & PitchCall == "InPlay", 1, 0),
        LA_grade = case_when(
          IsBrl == 1 ~ 25,
          Angle >= 10 & Angle <= 25 ~ 20,
          Angle >= 26 & Angle <= 32 ~ 15,
          Angle >= 33 & Angle <= 50 ~ 10,
          Angle < 10 ~ 5,
          Angle > 50 ~ 2,
          TRUE ~ 0
        ),
        # Create a new column for Swing Decision (1 = swing, 0 = no swing)
        SwingDecision = ifelse(
          PitchCall %in% c("BallCalled", "StrikeCalled", "BallinDirt", "BallIntentional", "HitByPitch"), 0, 1
        )) |>
      group_by(BatterId, Inning, PAofInning) |>
      mutate(AB_UID = cur_group_id()) |>
      ungroup()
  })
  
  observeEvent(input$file_submit, {
    
    write_csv(data_clean(), "initial_data.csv")
    output$preview <- renderTable({
      head(data_clean())
    })
    
    
  })
  # clean_data <- eventReactive()

    # output$distPlot <- renderPlot({
    #     # generate bins based on input$bins from ui.R
    #     x    <- faithful[, 2]
    #     bins <- seq(min(x), max(x), length.out = input$bins + 1)
    # 
    #     # draw the histogram with the specified number of bins
    #     hist(x, breaks = bins, col = 'darkgray', border = 'white',
    #          xlab = 'Waiting time to next eruption (in mins)',
    #          main = 'Histogram of waiting times')
    # })
}

# Run the application 
shinyApp(ui = ui, server = server)
