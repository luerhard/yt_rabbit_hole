######################################################
## Load required packages

library(tidyverse)
library(shiny)
library(shinyjs)
library(digest)


######################################################
## Set file paths **EACH USER SHOULD UPDATE**

# *Update with your source path to the app folder*
app_path <- "."

# Responses path (no need to change)
responses_path <- file.path(app_path, "responses/")

# Data file to pull abstracts from
input_file <- "sample.rds"

######################################################
## Define save and load functions

humanTime <- function() format(Sys.time(), "%Y%m%d-%H%M%OS")

# Save function
saveData <- function(data) {
  # Generate unique file name
  fileName <- sprintf("%s_%s.csv",
                      humanTime(),
                      digest::digest(data))
  # Write csv to responses sub-folder
  write.csv(x = data, 
            file = file.path(responses_path, fileName),
            row.names = FALSE, quote = T)
}



###################################################
## Define User Interface components

## Step 1: Identify coder, ask for new data
names <- c("None Selected", "Annika", "Lukas", 
           "Manika", "Marijn", "Michael", "Zarine")


ui_intro <- sidebarLayout(
  sidebarPanel(
    # who is coding? (will not re-set automatically)
    radioButtons("name", "Coder Name", names)
  ),
  mainPanel(
    # show a new abstract button
    actionButton("new_abstract", "Show New Video") 
  )
)

## Step 1b: Check coder can code
ui_check <- fluidRow(
  shinyjs::hidden(
    div(
      id = "limit_msg",
      h3("You've coded enough!")
    )
  )  
)


## Step 2: Code an Abstract
# coding choices
codes <- c("None selected",
           "Debunking",
           "Neutral", 
           "Spreading",
           "Description missing",
           "I'm not sure")

# layout
ui_code <- sidebarLayout(
  sidebarPanel(
    div( 
      id = "form",
      # make a coding decision
      radioButtons("code_choice", "Coding Choice", codes),
      # submit decision
      actionButton("submit", "Submit")
    )
  ),
  # display the title and abstract
  mainPanel(
    # display current item to code
    HTML(paste0('<iframe width="560" height="315" src="',url,'" frameborder="0" allow="accelerometer; autoplay; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>')),
    tableOutput("title"),
    tableOutput("description"),
    tableOutput("channel_title"),
    tableOutput("url")
  )
)

## Step 3: Confirm submission 
ui_confirm <- fluidRow(
  shinyjs::hidden(
    div(
      id = "thankyou_msg",
      h3("Response submitted!")
    )
  )  
)

## Combine UI elements
ui <- fluidPage(
  shinyjs::useShinyjs(),
  titlePanel("Coding YouTube Content"),
  
  ui_intro,
  ui_check,
  ui_code,
  ui_confirm
)

###################################################
## Define Server functions

# Define the fields we want to save 
fields <- c("name", "code_choice")

server <- function(input, output, session) {
  
  # Call data to code ------------------------------------------
  dataset <- readRDS(file.path(app_path, input_file))
  
  
  
  # Select a random article to code -----------------------------
  to_code <- eventReactive(input$new_abstract, {
    
    
    
    # random selection from the remainder
    dataset %>% 
      data.frame() %>%
      dplyr::sample_n(size = 1) 
  })
  
  # display title, and description----------------------
  output$title <- renderTable(to_code() %>% select(title))
  output$description <- renderTable(to_code() %>% select(description)) 
  output$channel_title <- renderTable(to_code() %>% select(channel_title))
  output$url <- renderTable(to_code() %>% select(url))
  
  # Aggregate form data and s.no. -----------------------
  formData <- reactive({
    data <- sapply(fields, function(x) input[[x]])
    # append s.no.
    data <- c(data, # coder name and choice
              to_code() %>% select(s.no.)) # s.no.
    # transpose
    data <- t(data)
    data
  })
  
  # Enable or disable the submit button -------------------------
  observe({
    # check if all mandatory fields have a value
    mandatoryFilled <-
      vapply(fields,
             function(x) {
               !is.null(input[[x]]) && input[[x]] != ""
             },
             logical(1))
    mandatoryFilled <- all(mandatoryFilled)
    
    # enable/disable the submit button
    shinyjs::toggleState(id = "submit", 
                         condition = mandatoryFilled)
  })
  
  # When Submit button is clicked, save form data and confirm -----
  observeEvent(input$submit, {
    saveData(formData())
    shinyjs::reset("form")
    shinyjs::hide("form")
    shinyjs::hide("title")
    shinyjs::hide("channel_title")
    shinyjs::hide("description")
    shinyjs::hide("url")
    shinyjs::show("thankyou_msg")
  })
  
  # When "Show Abstract" button is clicked again, display clean form ------
  observeEvent(input$new_abstract, {
    shinyjs::show("form")
    shinyjs::show("channel_title")
    shinyjs::show("title")
    shinyjs::show("description")
    shinyjs::show("url")
    shinyjs::hide("thankyou_msg")
  })    
}


###################################################
## Run the app
shinyApp(ui, server)

