## Load required packages

library(tidyverse)
library(shiny)
library(shinyjs)
library(digest)
library(igraph)
library(here)

here::i_am("README.md")

######################################################
## Set file paths **EACH USER SHOULD UPDATE**

# *Update with your source path to the app folder*
graph_name <- "5g_covid"

graph_file <- paste0(graph_name, ".gml")

app_path <- here()

# Responses path (no need to change)
responses_path <- here("shiny_app/responses")

# Data file to pull abstracts from
g <-  read_graph(here("data/clean/networks", graph_file), format = "gml")
input_file <- as_data_frame(g,"vertices")

sample_ids <- read.csv(here("data/clean/label sample/label_ids.csv"))

sample_ids <- sample_ids %>%
  group_by(name) %>%
  slice_head(n = 50)

input_file <- input_file %>%
  mutate(
    s.no. = label,
    url = paste0('<iframe width="560" height="315" src="https://www.youtube.com/embed/', label , '" frameborder="0" allow="accelerometer; autoplay; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>')
    ) %>%
  filter(
    label %in% sample_ids$video_id
  )

input_file$url <- lapply(input_file$url, FUN=HTML)

######################################################
## Define save and load functions

humanTime <- function() format(Sys.time(), "%Y%m%d-%H%M%OS")

# Save function
saveData <- function(data) {
  # Generate unique file name
  fileName <- sprintf("%s_%s_%s.csv",
                      graph_name,
                      humanTime(),
                      digest::digest(data)
                      )
  # Write csv to responses sub-folder
  write.csv(x = data,
            file = file.path(responses_path, fileName),
            row.names = FALSE, quote = T)
}

# Load function (Null default to handle first run)
loadPriors <- function(filesInfo=NULL, prior_responses=NULL) {
  # Set working directory to responses sub-folder
  setwd(responses_path)

  # Determine whether prior responses have been loaded
  if (is.null(prior_responses)) {
    # Get current file list
    filesInfo <- list.files()

    # Load prior responses for first time
    prior_responses <- map(filesInfo,
                           function(x) {
                             read_csv(x, col_names = T,
                                      skip_empty_rows = T,
                                      col_types = "ccd")
                           })
  }
  else {
    # Get new file list
    new_files <- setdiff(list.files(), filesInfo)

    # If >= 1 new file then append to prior_responses and filesInfo
    if (length(new_files) != 0) {
      new_responses <- map(new_files,
                           function(x) {
                             read_csv(x, col_names = T,
                                      skip_empty_rows = T,
                                      col_types = "ccd")
                           })
      prior_responses <- bind_rows(prior_responses, new_responses)
      filesInfo <- c(filesInfo, new_files)
    }
  }
  # Return current file list and responses as a list object
  out <- list(filesInfo = filesInfo,
              prior_responses = bind_rows(prior_responses))
  out
}


###################################################
## Define User Interface components

## Step 1: Identify coder, ask for new data
names <- c("None Selected", "Annika", "Lukas",
           "Manika", "Marijn", "Michael", "Zarine")


ui_intro <- sidebarLayout(
  sidebarPanel(
    # who is coding? (will not re-set automatically)
    selectInput("name", "Coder Name", names)
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
micro <- c("None selected",
           "Debunking",
           "Neutral",
           "Spreading",
           "Description missing",
           "Irrelevant Video",
           "I'm not sure")

topic <- c("None selected",
            "Yes",
            "No",
            "I'm not sure")

# layout
ui_code <- sidebarLayout(
  sidebarPanel(
    div(
      id = "form",
      # make a coding decision

      radioButtons("micro", "Micro", micro),

      fluidRow(
        column=1,
        radioButtons("topic", "On Topic", topic),
        actionButton("submit", "Submit"),
      )
    )
  ),
  # display the title and abstract
  mainPanel(
    # display current item to code
    tableOutput("title"),
    tableOutput("description"),
    tableOutput("channel_title"),
    htmlOutput("url")
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
fields <- c("name", "micro", "topic")

server <- function(input, output, session) {

  # Call data to code ------------------------------------------
  dataset <- input_file

  # Call prior responses and extract file and data list elements
  priorData <- loadPriors()
  fileInfo <- priorData$filesInfo
  priors <- priorData$prior_responses

  # Check if current coder has done too much -------------------

  observeEvent(input$name, {
    limit = 100
    current = sum(priors$name[priors$s.no. %in% dataset$s.no.] == input$name)
    if (current >= limit){
      # display message if so
      shinyjs::show("limit_msg")
    }
  })

  # Select a random article to code -----------------------------
  to_code <- eventReactive(input$new_abstract, {

    # refresh prior responses
    priorData <- loadPriors(fileInfo, priors)
    fileInfo <- priorData$filesInfo
    priors <- priorData$prior_responses

    # exclude articles already coded twice
    # exclude articles already coded six times
    exclude1 <- priors %>%
      group_by(s.no.) %>%
      mutate(total = length(s.no.)) %>%
      filter(total >= 1) %>%
      select(s.no.)

    # exclude articles already coded by current coder
    exclude2 <- priors %>%
      filter(name == input$name) %>%
      select(s.no.)

    # combine exclusions
    exclude <- bind_rows(exclude1, exclude2)

    # random selection from the remainder
    dataset %>%
      data.frame() %>%
      filter(!(s.no. %in% exclude$s.no.)) %>%
      dplyr::sample_n(size = 1)
  })


  # Select a random video to code -----------------------------
  to_code <- eventReactive(input$new_abstract, {



    # random selection from the remainder
    dataset %>%
      data.frame() %>%
      dplyr::sample_n(size = 1)
  })

  # display title, and description ----------------------
  output$title <- renderTable(to_code() %>% select(title))
  output$description <- renderTable(to_code() %>% select(description))
  output$channel_title <- renderTable(to_code() %>% select(channeltitle))
  output$url <- renderUI({ to_code() %>% select(url) })

  # display prior coding choices -----------------------------
  output$prev_choices <- renderTable({priors %>% 
      filter(s.no. == to_code()$s.no.) %>%
      select(name, code_choice, s.no.)
  })
  
  # Aggregate form data and video ID -----------------------
  formData <- reactive({
    data <- sapply(fields, function(x) input[[x]])
    # append video ID
    data <- c(data, # coder name and choice
              to_code() %>% select(s.no.)) # video ID
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
