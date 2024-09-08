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

# The ui object defines the page organization
ui <- fluidPage(

  # Link to external CSS file (controls plot size and position)
  tags$head(
    includeCSS("www/styles.css")
  ),
  
  HTML('<br>'),

  sidebarLayout(
    
    mainPanel(
      div(
        style = "position:relative; display:inline-block;",
        imageOutput("current_photo",
                    width  = '100%',
                    height = '100%',
                    click  = "photo_click",
                    brush  = brushOpts(id = "photo_brush", resetOnNew = TRUE)),
        plotOutput("overlay_plot", 
                   width  = "100%", 
                   height = '100%')
      )
    ),
    
    sidebarPanel(
      fluidRow(
        column(12, 
          # Lead through annotations
          div(
            HTML('<p style="font-size: 20px"><b>The <a href="https://thecnidaegritty.org">Cnidae Gritty</a> Photo Annotator</b></p>'),
            uiOutput("setup_ui"),
            verbatimTextOutput("setup_info"),
            HTML('<p style="font-size: 20px"><b>Annotations</b></p>'),
            uiOutput("question_ui",  inline = TRUE),
            uiOutput("notes_ui",     inline = TRUE)
          )
        )
      ),
      fluidRow(
        column(12, 
          div(
            HTML('<p style="font-size: 20px"><b>Progress</b></p>'),
            verbatimTextOutput("info")
          )
        )
      )
    )
  ),
  
  # Link to external JS file (gathers plot size information and will soon enable keyboard shortcuts)
  includeScript("www/custom.js")
  
)

# The server function:
# 1. creates 'observers' that detect user input
# 2. does all the data processing in the background
# 3. creates display objects that plug into the ui (sometimes essentially writes the ui from scratch if it's very dynamic)
server <- function(input, output, session) {
  
  # 1000*1024^2 is 1GB per file
  # When app is run locally (including shinylive), each file is copied, but shouldn't be a big deal. 
  # If this is run on a server, large files could be problematic. 
  # Could also be an issue with large numbers of files even locally - might want to look into delaying the file uploads somehow?
  options(shiny.maxRequestSize = 1000*1024^2)
  
  # Reactive values are variables that have global scope and depend on things like user input
  # Unlike regular variables, they can be monitored by 'observers' to automatically trigger events whenever they change
  
  # Define variables that will contain global information
  r <- reactiveValues(workflow_step     = 'annotator_id',
                      prompts_file      = NULL,
                      prompts           = NULL,
                      aux_files         = list(),
                      aux_data          = list(),
                      aux_i             = 1,
                      photo_df          = NULL,
                      photos_not_done   = NULL,
                      old_annotations   = NULL,
                      annotation_type   = 'setup',
                      annotations       = NULL,
                      observations      = NULL,
                      messages          = NULL,
                      metadata          = list(annotator_id       = NA,
                                               session_start_time = NA))
  
  # Define variables that are associated with specific photos
  a <- reactiveValues(i         = 1,
                      unasked   = NULL, 
                      answers   = list(),
                      file      = NA,
                      metadata  = list(filename               = NA,
                                       photo_annotation_start = NA))
  
  # Define variables that are associated with specific questions
  q <- reactiveValues(j       = 1,
                      i       = 1,
                      answers = list(),
                      dep     = list(fulfilled = FALSE,
                                     dep_name  = NULL,
                                     values    = NULL,
                                     coords    = list()),
                      key     = NULL,
                      current_coords = list(),
                      current_box    = list())
  
  # Reactive UI elements will be automatically updated whenever the reactive values that they depend on change
  # When r$workflow_step is updated, the setup UI will change
  output$setup_ui <- renderUI({
    switch(r$workflow_step,
      'annotator_id' = list(
        textInput("annotator_id", "Please enter your initials or other identifer"),
        actionButton("annotator_id_submit", "Submit")
      ),
      'prompts_file' = list(
        fileInput("prompts", "Select a file with prompts"),
        actionButton("default_prompts", "Use example")
      ),
      'aux_files' = list(
        fileInput("aux_file", paste0("Select the auxiliary `", names(isolate(r$aux_files))[[r$aux_i]], "` file that is named in the prompts file")),
        actionButton("default_aux_file", "Use default Catalog of Life taxonomy")
      ),
      'photo_files' = list(
        fileInput("photos", "Select a set of files to annotate", multiple = TRUE, accept = 'image/*'),
        actionButton("default_photos", "Use examples")
      ),
      'previous_annotations' = list(
        fileInput("resume_file", "Select a previous annotations file (or don't)"),
        actionButton("begin", "Begin")
      ),
      'annotate' = NULL
    )
  })
  
  # This observer waits until the first prompt is filled out
  # The previous UI had an action button and text input that created their own reactive variables under 'input'
  # This observer will trigger only when the action button is clicked, and it will only proceed if the text input is not empty
  # After saving the text, it updates the workflow_step reactive value so the UI will change to the next prompt
  observeEvent(input$annotator_id_submit, {
    req(input$annotator_id)
    r$metadata$annotator_id <- input$annotator_id
    r$workflow_step <- 'prompts_file'
  })
  
  # The second prompt contains two options and needs two observers
  # Both will assign variables for later and trigger a data processing step
  # This one triggers if the button is pressed to use default prompts, assigning predefined filepaths
  observeEvent(input$default_prompts, {
    r$prompts_file <- list(name     = 'example_prompts.tsv',
                           datapath = 'www/example_prompts.tsv')
  })
  
  # This one triggers if the user chooses their own filepath
  # req() is needed here but not for the actionButton observer because the fileInput reactive value can be initially set to NULL on loading of the UI, which would otherwise be enough to trigger this event
  observeEvent(input$prompts, {
    req(input$prompts)
    r$prompts_file <- input$prompts
  })
  
  # This event is triggered by the assignment of a derived reactive value in `r` rather than by direct user input
  # It will process the prompts file and define the data structure and see whether auxiliary files need to be selected
  # If no auxiliary files (like taxonomic data) are needed, then that step is skipped
  observeEvent(r$prompts_file, {
    req(r$prompts_file)
    
    r$prompts <- read.table(r$prompts_file$datapath, header = TRUE, sep = '\t', comment.char='')
    
    parsed <- apply(r$prompts, 1, \(x) {
      
      keys_sep <- strsplit(x['key'], ':')[[1]]

      if(length(keys_sep) > 1) {
        aux_name <- keys_sep[[1]]
        keys <- paste(aux_name, strsplit(keys_sep[[2]], ',')[[1]], sep=':')
        question_text <- sapply(keys, \(y) gsub('{.*}', y, x['question_text'], perl=TRUE))
      } else {
        aux_name <- NA
        keys <- x['key']
        question_text <- x['question_text']
      }
      
      values <- list(unlist(strsplit(x['values'], ",")[[1]]))
      
      if(is.na(x['dependencies']) & !x['key'] %in% c('point_coordinates', 'box_coordinates')) {
        
        dependencies <- list()
        photo_keys <- keys
        observation_keys <- NULL

      } else {
        
        dependencies <- lapply(strsplit(strsplit(x['dependencies'], ";")[[1]], ':'), \(y) list(y[[1]], strsplit(y[[2]],',')[[1]]))
        
        if(x['key'] %in% c('point_coordinates', 'box_coordinates')) {
          photo_keys <- NULL
          observation_keys <- c('observation_type', x['key'])
        } else {
          # parse dependency tree to find if keys are associated with whole photo or sub-observations
          if() {
            photo_keys <- NULL
            observation_keys <- keys
          } else {
            photo_keys <- keys
            observation_keys <- NULL
          }
        }

      }
      
      return(list(aux_name=aux_name, photo_keys=photo_keys, observation_keys=observation_keys, dependencies=dependencies, values=values, question_text=question_text))
      
    })
    
    # Look through keys for the special format `file:`, and create an empty named list to be later filled in via fileInput
    aux_names <- unique(sapply(parsed, \(x) x$aux_name))
    aux_names <- aux_names[!is.na(aux_names)]
    r$aux_files <- r$aux_data <- setNames(vector("list", length(aux_names)), aux_names)
    
    photo_keys <- unique(unlist(lapply(parsed, \(x) x$photo_keys)))
    observation_keys <- unique(unlist(lapply(parsed, \(x) x$observation_keys)))
    
    # Primary data storage for annotations associated with whole photo
    # Includes timestamps, annotator ID, and other metadata in addition to custom annotations
    r$annotations <- setNames(data.frame(matrix(ncol = length(r$metadata) + length(a$metadata) + length(photo_keys), nrow = 0)), 
                              c(names(r$metadata), names(a$metadata), photo_keys))
    
    # Primary data storage for annotations associated with specific coordinate-based observations within the photos
    # Includes photo name and timestamp to link to photo-wide observations for merging, in addition to custom annotations
    r$observations <- setNames(data.frame(matrix(ncol = 2 + length(observation_keys), nrow = 0)), 
                               c(names(r$metadata), names(a$metadata), observation_keys))
    
    if(length(r$aux_files) > 0) {
      r$workflow_step <- 'aux_files'
    } else {
      r$workflow_step <- 'photo_files'
    }
  })
  
  # If the aux_file UI is displayed and the user then chooses an aux_file, store it in the list
  # If there are more aux files to specify, increment the counter to refresh the fileInput prompt
  # Otherwise, trigger the photo input step
  observeEvent(input$aux_file, {
    req(input$aux_file)
    r$aux_files[[r$aux_i]] <- input$aux_file
    r$aux_data[[r$aux_i]] <- read.table(r$aux_files[[r$aux_i]]$datapath, header = TRUE, sep = '\t', comment.char='')
    if(r$aux_i < length(r$aux_files)) {
      r$aux_i <- r$aux_i + 1
    } else {
      r$workflow_step <- 'photo_files'
    }
  })
  
  # If the user chooses the default Catalog of Life taxonomy rather than specifying their own aux file
  observeEvent(input$default_aux_file, {
    r$aux_files[[r$aux_i]] <- list(name     = '',
                                   datapath = 'www/')
    r$aux_data[[r$aux_i]] <- read.table(r$aux_files[[r$aux_i]]$datapath, header = TRUE, sep = '\t', comment.char='')
    if(r$aux_i < length(r$aux_files)) {
      r$aux_i <- r$aux_i + 1
    } else {
      r$workflow_step <- 'photo_files'
    }
  })
  
  # Respond to user photo choices
  observeEvent(input$default_photos, {
    r$photo_df <- data.frame(datapath = c('www/photo_1_fish.jpg', 'www/photo_2_coral.jpg'), name = c('photo_1_fish.jpg', 'photo_2_coral.jpg'))[sample(2),]
    r$workflow_step <- 'previous_annotations'
  })
  
  observeEvent(input$photos, {
    req(input$photos)
    r$photo_df <- input$photos[sample(nrow(input$photos)),] # randomize order
    r$workflow_step <- 'previous_annotations'
  })
  
  # Retrieve info when the previous annotation file button is used
  observeEvent(input$resume_file, {
    req(input$resume_file)
    r$old_annotations <- read.table(input$resume_file$datapath, header = TRUE, sep = '\t')
    r$workflow_step <- 'annotate'
  })
    
  # Skip uploading previous annotations by clicking Begin
  observeEvent(input$begin, {
    r$workflow_step <- 'annotate'
  })
  
  # Once setup is completed, we start filling in other UI elements
  # Load the photo to annotate
  output$current_photo <- renderImage({
    req(a$file)
    list(src = a$file$datapath)
  }, deleteFile = FALSE)
  
  # Overlay points for observations
  output$overlay_plot <- renderPlot({
    req(q$current_coords)
    
    # Get actual image size with external javascript function
    session$sendCustomMessage('getImageDimensions', list())
    
    # Create an empty plot area with size and pixels equal to photo
    par(mar = c(0,0,0,0))
    plot(NULL, 
         type = "n", bty = 'n', xlab = "", ylab = "", xaxt = "n", yaxt = "n", xaxs='i', yaxs='i', asp=1, 
         xlim = c(1, input$image_width), ylim = c(1, input$image_height))
    
    # Make diameter of circles 1/20th of plot height
    point.cex <- floor(input$image_height / 20) / strheight("o", cex = 1)
    
    # Add circles for all sub-items relevant to current question
    for(i in seq_along(q$current_coords)) {
      
      # Current item red; others orange; both with transparency
      current_color <- if(i == q$i) adjustcolor("red", alpha.f = 0.5) else adjustcolor("orange", alpha.f = 0.6)
      
      # y-axis is reversed for plots compared to images
      points(q$current_coords[[i]][[1]], 
             input$image_height - q$current_coords[[i]][[2]], 
             col = thiscolor, 
             pch = 21, 
             bg  = current_color, 
             cex = point.cex)
      
    }

  }, bg = 'transparent')

  # Some text summarizing the setup
  output$setup_info <- renderText({
    if(r$workflow_step = 'annotate') {
      paste0(
        'Prompts file (', nrow(r$prompts), ' prompts): ', r$prompts_file$name, '\n',
        nrow(r$photo_df), ' photos\n',
        ifelse(is.null(input$resume_file$datapath), 
               'No old annotations - starting from scratch!\n', 
               paste0('Old annotations (', nrow(r$old_annotations), ' photos): ', input$resume_file$name, '\n', nrow(r$photo_df) - nrow(r$old_annotations), ' new photos\n'))
      )
    }
  })
  
  # This is the workhorse UI element for annotation input
  output$question_ui <- renderUI({
    switch(r$annotation_type,
      'setup' = NULL,
      'dropdown' = list(
        selectInput("current_key", 
                    r$prompts$question_text[[q$j]],
                    c('', q$dep$values[[q$i]])),
        downloadButton("save_file", "Discard this photo and save the rest"),
        actionButton("reset_photo", "Reset photo")
      ),
      'coordinate' = list(
        renderText({r$prompts$question_text[[q$j]]}),
        renderText({'Be aware that the shinylive export of this app currently has bugs for coordinate selection! Download the app and run it locally, and these annotations should work.'}),
        div(
          actionButton("remove_coord",  "Undo click"),
          actionButton("next_question", "Next question")
        ),
        downloadButton("save_file", "Discard this photo and save the rest"),
        actionButton("reset_photo", "Reset photo")
      ),
      'photo_finished' = list(
        renderText({"Structured questions finished - finish notes and click Next, Save, or Reset\n"}),
        actionButton("next_photo",  "Next photo"),
        downloadButton("save_file", "Save data"),
        actionButton("reset_photo", "Reset photo")
      ),
      'all_finished' = list(
        renderText({"All photos finished - Finish notes and click Save or Reset\n"}),
        downloadButton("save_file", "Save data"),
        actionButton("reset_photo", "Reset photo")
      )
    )
  })
  
  # Add an area to enter unstructured extra photo annotations
  output$notes_ui <- renderUI({
    if(r$workflow_step = 'annotate') {
      textAreaInput("notes", "Other notes", rows = 8, resize = 'both')
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
  
  # Define functions that help construct and iterate through prompts
  # New question parser to generate annotation paths based on current row of question file
  parse_question <- function(question) {
    
    key_split <- strsplit(question$key, ":")[[1]]
    values_split <- strsplit(question$values, ",")[[1]]

    if(is.na(question$dependencies)) {
      
      # No dependencies; all listed values acceptable
      res <- list(fulfilled            = TRUE,
                  values               = list(unlist(values_split)),
                  photo_annotations    = list())

      return(res)

    }
    
  }
  
  # Function to parse dependencies to:
  # 1. check if they are met for the current question, not yet met, or inapplicable
  # 2. return the name of the question's dependency (curently unused)
  # 3. return a list of character vectors: the list is as long as the number of sub-items (e.g. annotated points) relevant to the question, and each vector is the vocab relevant to that sub-item
  # 4. return a list of coordinates relevant to the sub-items to be annotated
  parse_dependencies <- function(question, answers) {
    
    values_split <- strsplit(question$values, ",")[[1]]
    
    if(is.na(question$dependencies)) {
      
      # No dependencies; all listed values acceptable
      fulfilled <- TRUE
      dep_name <- NA
      values <- list(unlist(values_split))
      coords <- list()
      
    } else {
    
      dependencies_sep <- strsplit(question$dependencies, ":")[[1]]
      dep_name <- dependencies_sep[[1]]
      
      if(is.null(answers[[dep_name]])) {
        
        fulfilled <- FALSE # Dependency not yet met; keep question in the list
        values <- NA
        coords <- list()
        
      } else if(is.na(answers[[dep_name]])) {
        
        fulfilled <- NA # Dependency will never be met, mark as inapplicable
        values <- NA
        coords <- list()
        
      } else {
        
        dependencies_split <- strsplit(strsplit(dependencies_sep[2], ';')[[1]], '#')
        dependencies_list <- list(key = strsplit(sapply(dependencies_split, \(x) sub('any!', '', x[[1]])), ','),
                                  acceptable = !grepl('!', sapply(dependencies_split, \(x) x[[1]])),
                                  indices = lapply(dependencies_split, \(x) {
                                    if(length(x) == 1) {
                                      return(seq_along(values_split))
                                    } else {
                                      idx <- unlist(lapply(strsplit(x[2],',')[[1]], \(y) do.call(\(fr, t=fr) seq.int(fr,t), as.list(unlist(strsplit(y,'-'))))))
                                      return(idx)
                                    }
                                  }))
        answers_split <- strsplit(answers[[dep_name]], ';')[[1]]
        
        values <- lapply(answers_split, \(answer) {
          
          answer_in_keys <- sapply(dependencies_list$key, \(x) answer %in% x)
          
          # Unacceptable_answers should be of the form any!x!y!z, implying anything other than x, y, or z is acceptable
          matches <- (answer_in_keys & dependencies_list$acceptable) |    # answer is in a white list
                     (!answer_in_keys & !dependencies_list$acceptable) |  # answer is not in a black list
                     sapply(dependencies_list$key, \(x) 'any' %in% x)     # any answer is acceptable
          
          if(any(matches)) {
            return(values_split[unlist(dependencies_list$indices[matches])])
          } else {
            return(character(0))
          }
          
        })
        
        fulfilled <- if(all(sapply(values,length) == 0)) NA else TRUE
        
        if(!is.na(fulfilled)) {
          # See if annotation is linked to point coordinates and store them if so
          cur_dep <- dep_name
          while(TRUE) {
            if(r$prompts$values[r$prompts$key==cur_dep] == 'point_coordinates') {
              coords <- lapply(strsplit(strsplit(a$answers[[cur_dep]], ';')[[1]], ','), as.numeric)
              break
            } else {
              new_dep <- r$prompts$dependencies[r$prompts$key==cur_dep]
              if(is.na(new_dep)) {
                coords <- list()
                break
              } else {
                cur_dep <- strsplit(new_dep, ":")[[1]][[1]]
              }
            }
          }
        } else {
          coords <- list()
        }
  
      }
    }
    
    res <- list(fulfilled = fulfilled, dep_name = dep_name, values = values, coords = coords)
    
    return(res)

  }
  
  # Function to find and render the next available question or check if done with photo
  render_next_question <- function() {
    
    q$current_coords <- list()
    q$current_box <- list()
    all_inapplicable <- TRUE
    
    for(j in a$unasked) {
      
      q$j <- j

      current_question <- r$prompts[j, ]
      q$key <- current_question$key
      
      q$dep <- parse_dependencies(current_question, a$answers)
      
      # If question is inapplicable, remove it from the unasked list
      if(is.na(q$dep$fulfilled)) {
        a$unasked <- a$unasked[!a$unasked %in% j]
        next
      }
      
      # If question has its dependencies fulfilled, proceed
      if(q$dep$fulfilled) {
        
        all_inapplicable <- FALSE

        # Interactive annotations are handled differently than drop-down ones
        if(current_question$values %in% c('box_coordinates', 'point_coordinates')) {
          
          q$i <- 0
          r$annotation_type <- 'coordinate'
          
        } else {
          
          q$i <- 1
          q$answers <- list()
          q$current_coords <- q$dep$coords

          # Iterate through sub-items until user input is needed
          for(i in 1:length(q$dep$values)) {
            
            # No values are relevant; fill in NA and move to the next sub-item
            if(length(q$dep$values[[q$i]]) == 0) {
              q$answers[[q$i]] <- 'NA'
              q$i <- q$i + 1
              next
            }
            
            # Only one value is relevant; add it automatically and move to the next
            if(length(q$dep$values[[q$i]]) == 1) {
              q$answers[[q$i]] <- q$dep$values[[q$i]]
              q$i <- q$i + 1
              next
            }
            
            # Let user choose which of multiple relevant values is true
            vocab <- c('', q$dep$values[[q$i]])
            r$annotation_type <- 'dropdown'
            
            break
            
          }
          
          # If an entire annotation category is automatically fillable, this should activate
          if(q$i > length(q$dep$values)) {
            a$answers[[q$key]] <- paste(q$answers, collapse = ';') # answers for each iterate saved between semicolons
            a$unasked <- a$unasked[!a$unasked %in% q$j]  # Remove the question from the unasked list
            next
          }
          
        }
        
        break
        
      }
    }
    
    # Check if all remaining questions are inapplicable
    if(length(a$unasked) == 0 || all_inapplicable) {
      if(a$i < nrow(r$photos_not_done)) {
        r$annotation_type <- 'photo_finished'
      } else {
        r$annotation_type <- 'all_finished'
      }
    }
  }
  
  # Reset inputs for current photo
  initialize_photo <- function() {
    
    for(key in r$prompts$key[!r$prompts$values %in% c('box_coordinates','point_coordinates')]) {
      if(!is.null(input[[key]])) updateSelectInput(session, key, selected = '')
    }
    a$metadata$photo_annotation_start <- format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z")
    a$unasked <- 1:nrow(r$prompts)
    a$answers <- list()

    render_next_question()
    
  }
  
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
    
  }, ignoreInit = TRUE)
  
  # Watch for manual reset button
  observeEvent(input$reset_photo, {
    initialize_photo()
  })
  
  # Function to add current photo's annotations to a data frame 
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
        
        # Add the answer to a list
        q$answers[[q$i]] <- input$current_key
        
        # Iterate through sub-items asking same question
        q$i <- q$i + 1
        if(q$i <= length(q$dep$values)) {
          for(i in q$i:length(q$dep$values)) {
            
            # No values are relevant; fill in NA and move to the next sub-item
            if(length(q$dep$values[[q$i]]) == 0) {
              q$answers[[q$i]] <- 'NA'
              q$i <- q$i + 1
              next
            }
            
            # Only one value is relevant; add it automatically and move to the next
            if(length(q$dep$values[[q$i]]) == 1) {
              q$answers[[q$i]] <- q$dep$values[[q$i]]
              q$i <- q$i + 1
              next
            }
            
            # Let user choose which of multiple relevant values is true
            vocab <- c('', q$dep$values[[q$i]])
            updateSelectInput(session, 'current_key', choices = vocab, selected = '')
            break
          }
        }
        
        # Finalize question and move to the next
        if(q$i > length(q$dep$values)) {
          a$answers[[q$key]] <- paste(q$answers, collapse = ';') # answers for each iterate saved between semicolons
          a$unasked <- a$unasked[!a$unasked %in% q$j]  # Remove the question from the unasked list
          render_next_question()  # Re-check all unasked questions after an answer
        }
          
      })
    }
  }, ignoreNULL = FALSE, ignoreInit = TRUE)
     
  # Detect when a point coordinate or bounding box annotation is being submitted         
  observeEvent(input$photo_click, {
    isolate({
      if(r$prompts[q$j,'values'] %in% c('point_coordinates','box_coordinates')) {
        q$i <- q$i + 1
        if(r$prompts[q$j,'values'] == 'point_coordinates') {
          q$current_coords[[q$i]] <- input$photo_click[c('x','y')]
        } else {
          q$current_box[[q$i]] <- input$photo_brush[c('xmin','ymin','xmax','ymax')]
        }
      }
    })
  }, ignoreInit = TRUE)
  
  # Undo a coordinate selection
  observeEvent(input$remove_coord, {
    isolate({
      if(r$prompts[q$j,'values'] == 'point_coordinates') {
        q$current_coords[[q$i]] <- NULL
      } else {
        q$current_box[[q$i]] <- NULL
      }
      q$i <- q$i - 1
    })
  }, ignoreInit = TRUE)
  
  # Detect when point coordinate or bounding box annotations are finished for a given question      
  observeEvent(input$next_question, {
    isolate({
      if(r$prompts[q$j,'values'] == 'point_coordinates') {
        if(length(q$current_coords) == 0) {
          a$answers[[q$key]] <- NA 
        } else {
          a$answers[[q$key]] <- paste(sapply(q$current_coords,paste,collapse=','), collapse=';')
        }
      } else {
        if(length(q$current_box) == 0) {
          a$answers[[q$key]] <- NA 
        } else {
          a$answers[[q$key]] <- paste(sapply(q$current_box,paste,collapse=','), collapse=';')
        }
      }
      a$unasked <- a$unasked[!a$unasked %in% q$j]  # Remove the question from the unasked list
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
      # If the current photo was finished, add it to the data; otherwise don't
      if(length(a$unasked) == 0) {
        intermediate_save()
      } else {
        initialize_photo()
      }
      old_and_new <- rbind(r$old_annotations, r$annotations)
      write.table(old_and_new, file, quote = FALSE, sep = '\t', row.names = FALSE)
    }
  )

}

shinyApp(ui, server)

# to run locally:
# shiny::runApp('~/scripts/USF_Library_Ogden_Collection_resources/shiny_annotation')
#to deploy to static site:
# shinylive::export("~/scripts/USF_Library_Ogden_Collection_resources/shiny_annotation", "~/scripts/thecnidaegritty/photo_annotator", template_dir = "~/scripts/thecnidaegritty/scripts/shinylive_jekyll_template", template_params = list(title = 'Photo Annotator', permalink = '/photo_annotator/'))
