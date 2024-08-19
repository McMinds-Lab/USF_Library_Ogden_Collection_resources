##
# Run entire script in R
##

library(shiny)

# Workaround for Chromium Issue 468227
downloadButton <- function(...) {
 tag <- shiny::downloadButton(...)
 tag$attribs$download <- NULL
 tag
}

ui <- fluidPage(
  titlePanel("Cnidae Gritty Photo Annotations"),

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
  
  # 1000*1024^2 is 1GB per file
  # When app is run locally (including shinylive), each file is copied, but shouldn't be a big deal. 
  # If this is run on a server, large files could be problematic. 
  # Could also be an issue with large numbers of files even locally - might want to look into delaying the file uploads somehow?
  options(shiny.maxRequestSize = 1000*1024^2)
  
  output$setup_ui <- renderUI({
    list(
      textInput("aid", "Please enter your initials or other identifer"),
      actionButton("aid_submit", "Submit")
    )
  })

  # Define variables that will contain global information
  r <- reactiveValues(in_v            = NULL,
                      vocab           = NULL,
                      photo_df        = NULL,
                      photos_not_done = NULL,
                      old_annotations = NULL,
                      annotations     = NULL,
                      messages        = NULL,
                      metadata        = list(annotator_id       = NA,
                                             session_start_time = NA))
  
  # Define variables that are associated with specific photos
  a <- reactiveValues(i        = 1,
                      unasked  = NULL, 
                      answers  = list(),
                      file     = NA,
                      metadata = list(filename               = NA,
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
        return(NA)     # Dependency will never be met, mark as inapplicable
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

        if(current_question$values %in% c('box_coordinates', 'point_coordinates')) {
          
          output$question_ui <- renderUI({
            list(
              renderText({current_question$question_text}),
              actionButton("save_coord",    "Save selection"),
              actionButton("next_question", "Next question (current selection not automatically saved)")
            )
          })
          
        } else {
          
          vocab <- c('', strsplit(current_question$values, ',')[[1]])
        
          output$question_ui <- renderUI({
            selectInput("current_key", 
                        current_question$question_text,
                        vocab)
          })
          
        }
        
        break
        
      }
    }
    
    # Check if all remaining questions are inapplicable
    if(length(a$unasked) == 0 || all_inapplicable) {
      
      if(a$i < nrow(r$photos_not_done)) {
        
        output$question_ui <- renderUI({
          list(
            renderText({"Structured questions finished - finish notes and click Next, Save, or Reset\n"}),
            actionButton("next_photo",  "Next photo"),
            downloadButton("save_file", "Save data")
          )
        })
        
      } else {
        
        output$question_ui <- renderUI({
          list(
            renderText({"All photos finished - Finish notes and click Save or Reset\n"}),
            downloadButton("save_file", "Save data")
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
      actionButton("reset_photo", "Reset photo")
    })

    render_next_question()
    
  }
  
  # Watch for manual reset button
  observeEvent(input$reset_photo, {
    initialize_photo()
  })
  
  chose_vocabulary <- reactiveVal(FALSE)

  observeEvent(input$default_vocabulary, {
    r$in_v <- 'www/example_vocabulary.tsv'
    r$in_v_name <- 'example_vocabulary.tsv'
    chose_vocabulary(TRUE)
  })
  
  observeEvent(input$vocabulary, {
    if(!is.integer(input$vocabulary)) {
      r$in_v <- input$vocabulary$datapath
      r$in_v_name <- input$vocabulary$name
      chose_vocabulary(TRUE)
    }
  })
    
  chose_photos <- reactiveVal(FALSE)

  observeEvent(input$default_photos, {
    r$photo_df <- data.frame(datapath = c('www/photo_1_fish.jpg', 'www/photo_2_coral.jpg'), name = c('photo_1_fish.jpg', 'photo_2_coral.jpg'))
    chose_photos(TRUE)
  })
  
  observeEvent(input$photos, {
    if(!is.integer(input$photos)) {
      r$photo_df <- input$photos
      chose_photos(TRUE)
    }
  })
  
  
  # Lead through setup (find required files)
  observeEvent(input$aid_submit, {
    
    if(input$aid != '') {
    
      r$metadata$annotator_id <- input$aid
      
      output$setup_ui <- renderUI({
        list(
          fileInput("vocabulary", "Select a file with vocabulary"),
          actionButton("default_vocabulary", "Use example")
        )
      })
      
    }
    
  }, ignoreInit = TRUE)
  
  # Retrieve info when the vocabulary file button is used
  observeEvent(chose_vocabulary(), {
    
    r$vocab <- read.table(r$in_v, header = TRUE, sep = '\t')
    r$annotations <- setNames(data.frame(matrix(ncol = length(r$metadata) + length(a$metadata) + nrow(r$vocab), nrow = 0)), 
                              c(names(r$metadata), names(a$metadata), r$vocab$key))
    
    output$setup_ui <- renderUI({
        list(
          fileInput("photos", "Select a set of files to annotate", multiple = TRUE, accept = 'image/*'),
          actionButton("default_photos", "Use examples")
        )
    })
    
  }, ignoreInit = TRUE)
    
  # Retrieve info when the annotation folder button is used
  observeEvent(chose_photos(), {
  
    if(nrow(r$photo_df) > 0) {
      
      output$setup_ui <- renderUI({
        list(
          fileInput("resume_file", "Select a previous annotations file"),
          actionButton("begin", "Begin")
        )
      })
      
    }  else {
    
      output$setup_ui <- renderUI({
          renderText({"No photos in directory. Refresh page to start over."})
      })
    
    }
      
  }, ignoreInit = TRUE)
    
  # Retrieve info when the previous annotation file button is used
  observeEvent(input$resume_file, {
    if(!is.integer(input$resume_file)) {
      r$old_annotations <- read.table(input$resume_file$datapath, header = TRUE, sep = '\t')
    }
  })
    
  # Declare that input files are finished, and begin
  observeEvent(input$begin, {
    
    if(!is.null(input$resume_file$datapath)) {
      r$photos_not_done <- r$photo_df[!r$photo_df$name %in% r$old_annotations$filename,]
    } else {
      r$photos_not_done <- r$photo_df
    }

    a$file <- r$photos_not_done[1,]
    a$metadata$filename <- a$file$name

    initialize_photo()
    
    r$metadata$session_start_time <- format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z")
    
    output$notes_ui <- renderUI({
      textAreaInput("notes", "Other notes", rows = 8, resize = 'both')
    })
    
    output$setup_ui <- renderUI({})
    output$setup_info <- renderText({
      
      paste0(
        'Vocabulary file (', nrow(r$vocab), ' questions): ', r$in_v_name, '\n',
        nrow(r$photo_df), ' photos\n',
        ifelse(is.null(input$resume_file$datapath), 
               'No old annotations - starting from scratch!\n', 
               paste0('Old annotations (', nrow(r$old_annotations), ' photos): ', input$resume_file$name, '\n', nrow(r$photo_df) - nrow(r$old_annotations), ' new photos\n'))
      )
      
    })

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
    a$file <- r$photos_not_done[a$i,]
    a$metadata$filename <- a$file$name
    
    updateTextAreaInput(session, 'notes', value = '')
    
    initialize_photo()
    
  })
  
  # Save old and new combined annotations
  output$save_file <- downloadHandler(
    filename = function() {
      paste0("annotations_", Sys.Date(), ".tsv")
    },
    content = function(file) {
      intermediate_save()
      old_and_new <- rbind(r$old_annotations, r$annotations)
      write.table(old_and_new, file, quote = FALSE, sep = '\t', row.names = FALSE)
    }
  )
  
  # Create a photo object for the main panel
  output$current_photo <- renderImage({
    
    if(!anyNA(a$file)) {
      # could be problematic for taller photos?
      list(src = a$file$datapath,
           width = '100%')
    }
    
  }, deleteFile = FALSE)
  
  # Render image
  output$renderPhoto <- renderUI({
    if(!anyNA(a$file)) {
      imageOutput("current_photo",
                  click = "photo_click",
                  brush = brushOpts(id = "photo_brush", resetOnNew = TRUE)
      )
    }
  })

  # Display various data and messages
  output$info <- renderText({
    
    paste0(
      ifelse(!anyNA(a$file), paste0('photo ', a$i, ' (', nrow(r$photos_not_done) - a$i, ' photos left): ', paste(a$metadata$filename, '\n')), ''),
      paste0(sapply(names(a$answers), \(x) paste0(x, ': ', a$answers[[x]], '\n')), collapse=''),
      paste(r$messages, collapse = '\n')
    )
    
  })
}

shinyApp(ui, server)

#to deploy to static site:
# shinylive::export("~/scripts/USF_Library_Ogden_Collection_resources/shiny_annotation", "~/scripts/thecnidaegritty/photo_annotator", template_dir = "~/scripts/thecnidaegritty/scripts/shinylive_jekyll_template", template_params = list(title = 'Photo Annotator', permalink = '/photo_annotator/'))
