#    https://shiny.posit.co/
#

library(shiny)
library(tidyverse)
library(jsonlite)
library(httr)
library(shinyFeedback)
library(shinyalert)
library(snakecase)
library(lubridate)

ui <- fluidPage(

  useShinyFeedback(),
  
  titlePanel(
    title = tags$div(
      style = "
    display: flex;
    align-items: center;
    justify-content: space-between;
    padding: 15px 20px;
    background-color: #154734;
    color: white;
    border-radius: 10px;
    box-shadow: 0 2px 6px rgba(0,0,0,0.1);
    margin-bottom: 20px;",
      tags$h2("Cal Poly Baseball Data Upload and Cleaning", style = "margin: 0; font-weight: bold;")
    )
  ),
  


  sidebarLayout(
      sidebarPanel(
        uiOutput("instruct"),
      ),

  mainPanel(
    selectInput(inputId="year",
                label="Choose a season",
                choices=2000:2100,
                selected=2025
    ),
    fileInput(inputId="file_upload",
              label="Upload Trackman CSV",
              multiple=FALSE,
              accept=".csv"
    ),
    actionButton(inputId="file_submit",
                 label="Submit")
  )
  )
)

  # Define server logic for file upload
server <- function(input, output) {
  supabase_url <- Sys.getenv("SUPABASE_URL")
  supabase_key <- Sys.getenv("SUPABASE_SERVICE_ROLE_KEY")
  
  output$instruct <- renderUI({
    HTML('
    <div style="background-color:#f9f9f9; padding:15px; border-radius:10px; border:1px solid #ddd;">
      <h4><strong>Instructions</strong></h4>
      <ol>
        <li>Download the Trackman file from today’s game onto your device.</li>
        <li>Select the year of the current season from the dropdown.</li>
        <li>Upload the Trackman CSV file using the button below.</li>
        <li>Click <strong>Submit</strong> to clean and send the data to Supabase.</li>
      </ol>
    </div>
  ')
  })
  
  
  data_clean <- reactive({
    req(input$file_upload)
    
    df_trackman <- read_csv(input$file_upload$datapath)
    colnames(df_trackman) <- to_any_case(colnames(df), case = "small_camel")
    
    df_cleaned <- 
      df_trackman |>
      rename(paOfInning = pAofInning,
             pitchOfPa = pitchofPa,
             kOrBB = korBb,
             pitchUID = pitchUid,
             gameUID = gameUid) |>
      select(pitchNo:playId, throwSpeed:timeToBase, 
             pitchReleaseConfidence:catcherThrowLocationConfidence) |>
      mutate(
        date = mdy(date),
        isBrl = case_when(
          exitSpeed >= 93 & angle >= 5 & angle <= 15 ~ 1,
          exitSpeed >= 95 & angle > 15 & angle <= 35 ~ 1,
          exitSpeed >= 98 & angle > 35 ~ 1,
          exitSpeed >= 100 ~ 1,
          TRUE ~ 0
        ),
        hardHit = ifelse(exitSpeed >= 95 & pitchCall == "InPlay", 1, 0),
        laGrade = case_when(
          isBrl == 1 ~ 25,
          angle >= 10 & angle <= 25 ~ 20,
          angle >= 26 & angle <= 32 ~ 15,
          angle >= 33 & angle <= 50 ~ 10,
          angle < 10 ~ 5,
          angle > 50 ~ 2,
          TRUE ~ 0
        ),
        SwingDecision = ifelse(
          pitchCall %in% c("BallCalled", "StrikeCalled", "BallinDirt", "BallIntentional", "HitByPitch"), 0, 1
        )) |>
      group_by(batterId, inning, paOfInning) |>
      mutate(abUID = cur_group_id()) |>
      ungroup()
  })
  
  observeEvent(input$file_submit, {
    write_csv(df_cleaned, "test.csv")
    # json_body <- toJSON(data_clean(), dataframe = "rows", auto_unbox = T, na="null")
    # 
    # res <- POST(
    #   url = paste0(supabase_url, "/rest/v1/2025_data"),
    #   add_headers(
    #     `apikey` = supabase_key,
    #     `Authorization` = paste("Bearer", supabase_key),
    #     `Content-Type` = "application/json",
    #     `Prefer` = "return=minimal"
    #   ),
    #   body = json_body
    # )
    # 
    # error_msg <- content(res, as = "text", encoding = "UTF-8")
    # if (status_code(res) == 201) {
    #   shinyalert("Upload Successful", "Your data was sent to Supabase ✅", type = "success")
    # } else {
    #   shinyalert("Upload Failed", "Something went wrong ❌", type = "error")
    # }
    
  })

}

# Run the application 
shinyApp(ui = ui, server = server)
