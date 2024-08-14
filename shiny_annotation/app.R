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
            h3("Setup"),
            uiOutput("setup_ui"),
            h3("Annotations"),
            uiOutput("question_ui", inline = TRUE),
            uiOutput("reset_button", inline = TRUE)
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
  
  output$setup_ui <- renderUI({
      shinyFilesButton('vocabulary', 'Select a file with vocabulary', 'Please select a file', FALSE)
  })
        
  # Create observers for filepath buttons
  roots <- c(home = '~', getVolumes()())
  shinyFileChoose(input, 'vocabulary',  roots=roots)
  shinyDirChoose(input,  'dir',         roots=roots, allowDirCreate = FALSE)
  shinyFileChoose(input, 'resume_file', roots=roots)
  shinyFileSave(input,   'save_file',   roots=roots)

  # Define variables that will change via interaction across files
  # want to add session start, session end, photo time
  r <- reactiveValues(vfn             = NULL,
                      vocab           = NULL,
                      dir             = NULL,
                      files           = NULL,
                      photos          = NULL,
                      photos_not_done = NULL,
                      old_annotations_file = NULL,
                      old_annotations      = NULL,
                      annotations = NULL,
                      save_file   = NULL,
                      messages    = NULL,
                      metadata    = list(annotator_id = NA,
                                         session_id   = NA))
  
  # Define variables that will change via interaction within a photo
  a <- reactiveValues(i          = 1,
                      unasked    = NULL, 
                      answers    = list(),
                      metadata   = list(filename = NA))
  
  # Function to check if dependencies are met or if the question is inapplicable
  dependencies_met <- function(question, answers) {
    
    if(is.na(question$dependencies)) {
      return(TRUE)
    }
    
    dependencies <- strsplit(strsplit(question$dependencies, ";")[[1]], ':')
    for(dep in dependencies) {
      answer <- answers[[dep[1]]]
      
      if(is.null(answer)) {
        return(FALSE)  # Dependency not yet met, keep question in the list
      } else if(!answer %in% strsplit(dep[2], ',')[[1]]) {
        return(NA)  # Dependency will never be met, mark as inapplicable
      }
    }
    return(TRUE)
    
  }
  
  # Function to find and render the next available question or check if done
  render_next_question <- function() {
    
    all_inapplicable <- TRUE
    
    for(i in a$unasked) {
      
      current_question <- r$vocab[i, ]
      dependency_status <- dependencies_met(current_question, a$answers)
      
      if(is.na(dependency_status)) {
        # Question is inapplicable, remove it from the unasked list
        a$unasked <- a$unasked[!a$unasked %in% i]
        next
      }
      
      if(dependency_status) {
        
        all_inapplicable <- FALSE
        question_key <- current_question$key
        question_text <- current_question$question_text
        vocab <- c('', strsplit(current_question$values, ',')[[1]])
        
        output$question_ui <- renderUI({
          selectInput(question_key, 
                      question_text,
                      choices = vocab)
        })
        
        observeEvent(input[[question_key]], {
          if(input[[question_key]] != '') {
            isolate({
              a$answers[[question_key]] <- input[[question_key]]
              a$unasked <- a$unasked[!a$unasked %in% i]  # Remove the question from the unasked list
              render_next_question()  # Re-check all unasked questions after an answer
            })
          }
        }, ignoreNULL = FALSE, ignoreInit = TRUE)
        
        break
        
      }
    }
    
    # Check if all remaining questions are inapplicable
    if(length(a$unasked) == 0 || all_inapplicable) {
      
      if(a$i < length(r$photos_not_done)) {
        
        output$question_ui <- renderUI({
          list(
            renderText({"Photo finished - Click Next, Save, or Reset\n"}),
            actionButton("next_photo", label = "Next photo"),
            shinySaveButton('save_file', 'Save data', 'Save file as...')
          )
        })
        
      } else {
        
        output$question_ui <- renderUI({
          list(
            renderText({"All photos finished - Click Save or Reset\n"}),
            shinySaveButton('save_file', 'Save data', 'Save file as...')
          )
        })
        
      }
    }
  }
  
  # Reset inputs for current photo
  initialize_photo <- function() {
    
    for(key in r$vocab$key) {
      if(!is.null(input[[key]])) updateSelectInput(session, key, selected = '')
    }
    a$unasked <- 1:nrow(r$vocab)
    a$answers <- list()
    output$reset_button <- renderUI({
      actionButton("reset_photo", label = "Reset photo")
    })
    # Watch for manual reset button
    observeEvent(input$reset_photo, {
      initialize_photo()
    })
    render_next_question()
    
  }
  
  # Retrieve info when the vocabulary file button is used
  observeEvent(input$vocabulary, {
    
    if(!is.integer(input$vocabulary)) {
      
      r$vfn <- parseFilePaths(roots, input$vocabulary)$datapath
      r$vocab <- read.table(r$vfn, header = TRUE, sep = '\t')
      r$annotations <- setNames(data.frame(matrix(ncol = length(r$metadata) + length(a$metadata) + nrow(r$vocab), nrow = 0)), 
                                c(names(r$metadata), names(a$metadata), r$vocab$key))
      
      output$setup_ui <- renderUI({
          shinyDirButton('dir', 'Select a folder to annotate', 'Please select a folder', FALSE)
      })
      
      # Retrieve info when the annotation folder button is used
      observeEvent(input$dir, {
        
        # event is triggered when clicking the button (inputting integer), but we don't want to move on until a directory is chosen (no longer integer)
        if(!is.integer(input$dir)) {
        
          r$dir <- parseDirPath(roots, input$dir)
          r$files <- list.files(r$dir, full.names = TRUE)
          # Filter files by MIME type to ensure they are images
          r$photos <- r$files[startsWith(mime::guess_type(r$files), "image/")]
          
          if(length(r$photos) > 0) {
            
            output$setup_ui <- renderUI({
              list(
                shinyFilesButton('resume_file', 'Select a previous annotations file', 'Please select a file', FALSE),
                actionButton("begin", label = "Begin")
              )
            })
            
            # Retrieve info when the previous annotation file button is used
            observeEvent(input$resume_file, {
              
              if(!is.integer(input$resume_file)) {

                r$old_annotations_file <- parseFilePaths(roots, input$resume_file)$datapath
                r$old_annotations <- read.table(r$old_annotations_file, header = TRUE, sep = '\t')
              
              }

            })
            
            # Declare that input files are finished, and begin
            observeEvent(input$begin, {
              
              if(!is.null(r$old_annotations_file)) {
                r$photos_not_done <- r$photos[!r$photos %in% r$old_annotations$filename]
              } else {
                r$photos_not_done <- r$photos
              }
          
              a$metadata$filename <- r$photos_not_done[[1]]

              initialize_photo()
              
              output$setup_ui <- renderUI({
                renderText({
                
                  paste0(
                    'Vocabulary file: ', paste0(r$vfn, '\n'),
                    'Annotation directory: ',  paste0(r$dir, '\n'),
                    ifelse(is.null(r$old_annotations_file), 'No old annotations - starting from scratch!\n', paste0('Old annotations: ', paste0(r$old_annotations_file, '\n')))
                  )
                  
                })
              })
          
            }, ignoreInit = TRUE)
            
          } else {
            
            output$setup_ui <- renderUI({
                renderText({"No photos in directory. No ability to reset yet!"})
            })
            
          }
          
        }
    
      }, ignoreInit = TRUE)
      
    }

  }, ignoreInit = TRUE)
  
  intermediate_save <- function() {
    
    r$annotations[a$i,] <- NA
    r$annotations[a$i, names(r$metadata)] <- r$metadata
    r$annotations[a$i, names(a$metadata)] <- a$metadata
    r$annotations[a$i, names(a$answers)]  <- a$answers

  }
  
  # Move to next photo when 'Next' button is clicked
  # If there are no more photos, add this message to the output
  observeEvent(input$next_photo, {
    
    intermediate_save()
    
    a$i <- a$i + 1
    a$metadata$filename <- r$photos_not_done[[a$i]]
    
    initialize_photo()
    
  })
  
  # Save old and new combined annotations
  observeEvent(input$save_file, {
    
    intermediate_save()
    
    if(!is.integer(input$save_file)) {
      
      r$save_file <- parseSavePath(roots, input$save_file)$datapath
      
      old_and_new <- rbind(r$old_annotations, r$annotations)
      
      write.table(old_and_new, r$save_file, quote = FALSE, sep = '\t', row.names = FALSE)
    
    }
    
  })
  
  # Create a photo object for the main panel
  output$current_photo <- renderImage({
    
    # allow size to change
    width  <- session$clientData$output_current_photo_width
    height <- session$clientData$output_current_photo_height
    
    # need to fiddle with this to get max size
    list(src = a$metadata$filename,
         #width = width,
         height = height)
    
  }, deleteFile = FALSE)
  
  # not using click and brush currently, so might remove or make them conditional (will require thought for formats of vocabulary and annotations files)
  output$renderPhoto <- renderUI({
    if(!is.na(a$metadata$filename)) {
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
      'photo:',  paste(a$metadata$filename, '\n'),
      r$messages
    )
    
  })
}

shinyApp(ui, server)
