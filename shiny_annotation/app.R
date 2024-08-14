##
# Run entire script in R
##

library(shiny)
library(shinyFiles)
library(mime)

ui <- fluidPage(
  titlePanel("Ogden Annotations"),

  sidebarLayout(
    
    mainPanel(
      uiOutput("renderPhoto")
    ),
    
    sidebarPanel(
      fluidRow(
        column(12, 
          # Lead through annotations
          div(
            shinyDirButton('dir', 'Select a folder to annotate', 'Please select a folder', FALSE),
            shinyFilesButton('resume_file', 'Select a previous annotations file', 'Please select a file', FALSE),
            h3("Annotations"),
            selectInput("photo_type", "What is the overall photo type",
                        choices = c("", "benthic", "social", "other")), ## in future, pull from controlled vocab file
            conditionalPanel(
              condition = "input.photo_type == 'benthic'",
              selectInput("has_scleractinia", "Are there corals in the photo?",
                          choices = c("", "yes", "no", "unknown"))
            ),
            conditionalPanel(
              condition = "input.photo_type == 'benthic' & input.has_scleractinia != ''",
              selectInput("has_algae", "Is there algae in the photo?",
                          choices = c("", "yes", "no", "unknown"))
            ),
            actionButton("reset_photo", label = "Reset photo"),
            uiOutput("next_photo_button", inline = TRUE)
          )
        )
      ),
      fluidRow(
        column(12, 
               div(
                 h3("Messages"),
                 verbatimTextOutput("info")
               )
        )
      )
    )
  )
)

server <- function(input, output, session) {
  
  # Create observers for filepath buttons
  roots <- getVolumes()()
  shinyDirChoose(input, 'dir', roots=roots)
  shinyFileChoose(input, 'resume_file', roots=roots)
  shinyFileSave(input, 'save_file', roots=roots)

  # Define variables that will change via interaction
  r <- reactiveValues(i           = 1, 
                      files       = NULL,
                      photos      = NULL,
                      photo_name  = NULL,
                      old_annotations_file = NULL,
                      annotations = NULL,
                      save_file   = NULL,
                      messages    = NULL)
  
  # Retrieve info when the annotation folder button is used
  observeEvent(input$dir, {
    
    r$files <- list.files(parseDirPath(roots, input$dir), full.names = TRUE)
    
    # Filter files by MIME type to ensure they are images
    r$photos <- r$files[startsWith(mime::guess_type(r$files), "image/")]
    
    ## need to add logic to filter out photos previously annotated (or more complicated options)
    
    if(length(r$photos) > 0) r$photo_name <- r$photos[[1]]

  }, ignoreInit = TRUE)
  
  # Retrieve info when the previous annotation file button is used
  observeEvent(input$resume_file, {
    
    r$old_annotations_file <- parseFilePaths(roots, input$resume_file)$datapath
    
    # need to read in file and initialize 'annotations' with it
    
  })
  
  # Save old and new combined annotations
  observeEvent(input$save_file, {
    
    r$save_file <- parseSavePath(roots, input$save_file)$datapath
    
    # need to actually save file
    
  })
  
  # Define endpoints that allow the user to save the current photo and move on
  output$next_photo_button <- renderUI({
    if(input$photo_type %in% c('social','other') || ( input$photo_type == 'benthic' && all(c(input$has_scleractinia, input$has_algae) != '') )) {
      actionButton("next_photo", label = "Next photo")
      shinySaveButton('save_file', 'Save data', 'Save file as...')
    }
  })

  # Reset inputs for current photo (this might get messy with long decision tree - careful...)
  reset <- function() {
    
    updateSelectInput(session, "photo_type", selected = '')
    updateSelectInput(session, "has_scleractinia", selected = '')
    updateSelectInput(session, "has_algae", selected = '')
    
  }
  
  # Reset
  observeEvent(input$reset_photo, {
    
    reset()
    
  })
  
  # Move to next photo when 'Next' button is clicked
  # If there are no more photos, add this message to the output
  observeEvent(input$next_photo, {
    
    ## need to save data, then see if there's another photo (use a function that will also be called by the 'save' button?)
    
    if(r$i < length(r$photos)) {
      
      r$i <- r$i + 1
      r$photo_name <- r$photos[[r$i]]
      
      reset()
      
    } else {
      
      r$messages <- 'No more photos!'
      
    }
    
  })
  
  # Create a photo object for the main panel
  output$current_photo <- renderImage({
    
    # allow size to change
    width  <- session$clientData$output_current_photo_width
    height <- session$clientData$output_current_photo_height
    
    # need to fiddle with this to get max size
    list(src = r$photo_name,
         #width = width,
         height = height)
    
  }, deleteFile = FALSE)
  
  # not using click and brush currently, so might remove or make them conditional
  output$renderPhoto <- renderUI({
    if(!is.null(r$photo_name)) {
      plotOutput("current_photo",
                 click = "photo_click",
                 brush = "photo_brush"
      )
    }
  })

  # Display various data and messages
  output$info <- renderText({
    xy_str <- function(e) {
      if(is.null(e)) return("NULL\n")
      paste0("x=", round(e$x, 1), " y=", round(e$y, 1), "\n")
    }
    xy_range_str <- function(e) {
      if(is.null(e)) return("NULL\n")
      paste0("xmin=", round(e$xmin, 1), " xmax=", round(e$xmax, 1), 
             " ymin=", round(e$ymin, 1), " ymax=", round(e$ymax, 1), '\n')
    }

    paste0(
      "click: ", xy_str(input$photo_click),
      "brush: ", xy_range_str(input$photo_brush),
      'photo:',  paste(r$photo_name, '\n'),
      'old_annotations:', paste(r$old_annotations_file, '\n'),
      r$messages
    )
  })
}

shinyApp(ui, server)
