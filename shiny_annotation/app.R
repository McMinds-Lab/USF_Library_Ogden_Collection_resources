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
            verbatimTextOutput("setup_info"),
            h3("Annotations"),
            uiOutput("question_ui", inline = TRUE),
            uiOutput("reset_button", inline = TRUE),
            uiOutput("notes_ui", inline = TRUE),
          )
        )
      ),
      fluidRow(
        column(12, 
          div(
            h3("Progress"),
            verbatimTextOutput("info")
          )
        )
      )
    )
  )
)

server <- function(input, output, session) {
  
  output$setup_ui <- renderUI({
    list(
      textInput("aid", "Please enter your initials or other identifer"),
      actionButton("aid_submit", label = "Submit")
    )
  })
        
  # Create observers for filepath buttons
  roots <- c(home = '~', getVolumes()())
  shinyFileChoose(input, 'vocabulary',  roots=roots)
  shinyDirChoose(input,  'dir',         roots=roots, allowDirCreate = FALSE)
  shinyFileChoose(input, 'resume_file', roots=roots)
  shinyFileSave(input,   'save_file',   roots=roots)

  # Define variables that will contain global information
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
                                         session_start_time = NA))
  
  # Define variables that are associated with specific photos
  a <- reactiveValues(i          = 1,
                      unasked    = NULL, 
                      answers    = list(),
                      metadata   = list(filename = NA,
                                        photo_annotation_start = NA))
  
  # Define variables that are associated with specific questions
  q <- reactiveValues(i   = 1,
                      key = NULL)
  
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
    
    for(j in a$unasked) {
      
      q$i <- j

      current_question <- r$vocab[j, ]
      q$key <- current_question$key
      
      dependency_status <- dependencies_met(current_question, a$answers)
      
      if(is.na(dependency_status)) {
        # Question is inapplicable, remove it from the unasked list
        a$unasked <- a$unasked[!a$unasked %in% j]
        next
      }
      
      if(dependency_status) {
        
        all_inapplicable <- FALSE
        question_text <- current_question$question_text
        
        if(current_question$values %in% c('box_coordinates','point_coordinates')) {
          
          output$question_ui <- renderUI({
            list(
              actionButton("save_coord", label = "Save selection"),
              actionButton("next_question", label = "Next question (current selection not automatically saved)")
            )
          })
          
        } else {
          
          vocab <- c('', strsplit(current_question$values, ',')[[1]])
        
          output$question_ui <- renderUI({
            selectInput("current_key", 
                        question_text,
                        choices = vocab)
          })
          
        }
        
        break
        
      }
    }
    
    # Check if all remaining questions are inapplicable
    if(length(a$unasked) == 0 || all_inapplicable) {
      
      if(a$i < length(r$photos_not_done)) {
        
        output$question_ui <- renderUI({
          list(
            renderText({"Structured questions finished - finish notes and click Next, Save, or Reset\n"}),
            actionButton("next_photo", label = "Next photo"),
            shinySaveButton('save_file', 'Save data', 'Save file as...')
          )
        })
        
      } else {
        
        output$question_ui <- renderUI({
          list(
            renderText({"All photos finished - Finish notes and click Save or Reset\n"}),
            shinySaveButton('save_file', 'Save data', 'Save file as...')
          )
        })
        
      }
    }
  }
  
  # Reset inputs for current photo
  initialize_photo <- function() {
    
    for(key in r$vocab$key[!r$vocab$values %in% c('box_coordinates','point_coordinates')]) {
      if(!is.null(input[[key]])) updateSelectInput(session, key, selected = '')
    }
    a$metadata$photo_annotation_start <- format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z")
    a$unasked <- 1:nrow(r$vocab)
    a$answers <- list()
    output$reset_button <- renderUI({
      actionButton("reset_photo", label = "Reset photo")
    })

    render_next_question()
    
  }
  
  # Watch for manual reset button
  observeEvent(input$reset_photo, {
    initialize_photo()
  })
  
  # Lead through setup (find required files)
  observeEvent(input$aid_submit, {
    
    if(input$aid != '') {
    
      r$metadata$annotator_id <- input$aid
      
      output$setup_ui <- renderUI({
        shinyFilesButton('vocabulary', 'Select a file with vocabulary', 'Please select a file', FALSE)
      })
      
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
                  
                  r$metadata$session_start_time <- format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z")
                  
                  output$notes_ui <- renderUI({
                    textAreaInput("notes", "Other notes", rows = 8, resize = 'both')
                  })
                  
                  output$setup_ui <- renderUI({})
                  output$setup_info <- renderText({
                    
                    paste0(
                      'Vocabulary file (', nrow(r$vocab), ' questions): ', r$vfn, '\n',
                      'Photo directory (', length(r$photos), ' photos): ', r$dir, '\n',
                      ifelse(is.null(r$old_annotations_file), 
                             'No old annotations - starting from scratch!\n', 
                             paste0('Old annotations (', nrow(r$old_annotations), ' photos): ', r$old_annotations_file, '\n', length(r$photos) - nrow(r$old_annotations), ' new photos\n'))
                    )
                    
                  })
              
                }, ignoreInit = TRUE)
                
              } else {
                
                output$setup_ui <- renderUI({
                    renderText({"No photos in directory. Refresh page to start over."})
                })
                
              }
              
            }
        
          }, ignoreInit = TRUE)
          
        }
    
      }, ignoreInit = TRUE)
      
    }
    
  }, ignoreInit = TRUE)
  
  intermediate_save <- function() {
    
    r$annotations[a$i,] <- NA
    r$annotations[a$i, names(r$metadata)] <- r$metadata
    r$annotations[a$i, names(a$metadata)] <- a$metadata
    r$annotations[a$i, names(a$answers)]  <- a$answers
    r$annotations[a$i,'notes'] <- ifelse(input$notes == '', 'None', gsub("\r?\n|\r|\t", " ", input$notes))

  }
  
  # Detect when an answer is entered via drop-down menu
  observeEvent(input$current_key, {
    if(input$current_key != '') {
      isolate({
        a$answers[[q$key]] <- input$current_key
        a$unasked <- a$unasked[!a$unasked %in% q$i]  # Remove the question from the unasked list
        render_next_question()  # Re-check all unasked questions after an answer
      })
    }
  }, ignoreNULL = FALSE, ignoreInit = TRUE)
     
  # Detect when a point coordinate or bounding box annotation is being submitted         
  observeEvent(input$save_coord, {
    if(r$vocab[q$i,'values'] == 'point_coordinates') {
      a$answers[[q$key]] <- paste(c(a$answers[[q$key]], paste(input$photo_click[c('x','y')], collapse=',')), collapse=';')
    } else {
      a$answers[[q$key]] <- paste(c(a$answers[[q$key]], paste(input$photo_brush[c('xmin','ymin','xmax','ymax')], collapse=',')), collapse=';')
    }
  }, ignoreInit = TRUE)
  
  # Detect when point coordinate or bounding box annotations are finished for a given question      
  observeEvent(input$next_question, {
    isolate({
      a$unasked <- a$unasked[!a$unasked %in% q$i]  # Remove the question from the unasked list
      render_next_question()  # Re-check all unasked questions after an answer
    })
  }, ignoreInit = TRUE)
  
  # Move to next photo when 'Next' button is clicked
  observeEvent(input$next_photo, {
    
    intermediate_save()
    
    a$i <- a$i + 1
    a$metadata$filename <- r$photos_not_done[[a$i]]
    
    updateTextAreaInput(session, 'notes', value = '')
    
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
    
    # could be problematic for taller photos?
    list(src = a$metadata$filename,
         width = '100%')
    
  }, deleteFile = FALSE)
  
  # Render image
  output$renderPhoto <- renderUI({
    if(!is.na(a$metadata$filename)) {
      imageOutput("current_photo",
                 click = "photo_click",
                 brush = brushOpts(id = "photo_brush", resetOnNew = TRUE)
      )
    }
  })

  # Display various data and messages
  output$info <- renderText({
    
    paste0(
      'photo ', a$i, ' (', length(r$photos_not_done) - a$i, ' photos left): ', paste(a$metadata$filename, '\n'),
      paste0(sapply(names(a$answers), \(x) paste0(x, ': ', a$answers[[x]], '\n')), collapse=''),
      paste(r$messages, collapse = '\n')
    )
    
  })
}

shinyApp(ui, server)
