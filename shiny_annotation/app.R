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

  # Ensure photos are scaled to screen
  tags$head(
    tags$style(
      type="text/css",
      "#current_photo img {max-width: 100%; max-height: 100vh}"
    ),
    tags$style(
      type="text/css",
      "#overlay_plot {position:absolute; top:0; left:0; pointer-events:none;}"
    ),
    tags$style(
      type="text/css",
      "#overlay_plot img {max-width: 100%; max-height: 100vh}"
    )
  ),
  
  HTML('<br>'),

  sidebarLayout(
    
    mainPanel(
      uiOutput("renderPhoto"),
      tags$script(HTML(
        "Shiny.addCustomMessageHandler('getImageDimensions', function(message) {
           var img = document.querySelector('#current_photo img');
           if (img) {
             Shiny.setInputValue('image_width', img.naturalWidth);
             Shiny.setInputValue('image_height', img.naturalHeight);
           }
        });"
      ))
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
            uiOutput("reset_button", inline = TRUE),
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
                      current_coords = list())
  
  # Function to check if dependencies are met or if the question is inapplicable
  # return a list of character vectors with vocab length of list equal to number of items relevant to dependency
  # if dependency will never be met, return NA
  # if dependencies are not yet met, return list of length 0
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
            if(r$vocab$values[r$vocab$key==cur_dep] == 'point_coordinates') {
              coords <- lapply(strsplit(strsplit(a$answers[[cur_dep]], ';')[[1]], ','), as.numeric)
              break
            } else {
              new_dep <- r$vocab$dependencies[r$vocab$key==cur_dep]
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
  
  # Function to find and render the next available question or check if done
  render_next_question <- function() {
    
    q$current_coords <- list()
    all_inapplicable <- TRUE
    
    for(j in a$unasked) {
      
      q$j <- j

      current_question <- r$vocab[j, ]
      q$key <- current_question$key
      
      q$dep <- parse_dependencies(current_question, a$answers)
      
      if(is.na(q$dep$fulfilled)) {
        # Question is inapplicable, remove it from the unasked list
        a$unasked <- a$unasked[!a$unasked %in% j]
        next
      }
      
      if(q$dep$fulfilled) {
        
        all_inapplicable <- FALSE

        if(current_question$values %in% c('box_coordinates', 'point_coordinates')) {
          
          q$i <- 1
          
          output$question_ui <- renderUI({
            list(
              renderText({current_question$question_text}),
              renderText({'Be aware that the shinylive export of this app currently has bugs for coordinate selection! Download the app and run it locally, and these annotations should work.'}),
              actionButton("save_coord",    "Save selection"),
              actionButton("next_question", "Next question (current selection not automatically saved)")
            )
          })
          
        } else {
          
          q$i <- 1
          q$answers <- list()
          q$current_coords <- q$dep$coords

          for(i in 1:length(q$dep$values)) {
            
            if(length(q$dep$values[[q$i]]) == 0) {
              q$answers[[q$i]] <- 'NA'
              q$i <- q$i + 1
              next
            }
            
            if(length(q$dep$values[[q$i]]) == 1) {
              q$answers[[q$i]] <- q$dep$values[[q$i]]
              q$i <- q$i + 1
              next
            }
            
            vocab <- c('', q$dep$values[[q$i]])
      
            output$question_ui <- renderUI({
              selectInput("current_key", 
                          current_question$question_text,
                          vocab)
            })
            
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
    r$photo_df <- data.frame(datapath = c('www/photo_1_fish.jpg', 'www/photo_2_coral.jpg'), name = c('photo_1_fish.jpg', 'photo_2_coral.jpg'))[sample(2),]
    chose_photos(TRUE)
  })
  
  observeEvent(input$photos, {
    if(!is.integer(input$photos)) {
      r$photo_df <- input$photos[sample(nrow(input$photos)),] # randomize order
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
    
    r$vocab <- read.table(r$in_v, header = TRUE, sep = '\t', comment.char='')
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
        
        q$answers[[q$i]] <- input$current_key
        
        q$i <- q$i + 1
        if(q$i <= length(q$dep$values)) {
          for(i in q$i:length(q$dep$values)) {
            
            if(length(q$dep$values[[q$i]]) == 0) {
              q$answers[[q$i]] <- 'NA'
              q$i <- q$i + 1
              next
            }
            
            if(length(q$dep$values[[q$i]]) == 1) {
              q$answers[[q$i]] <- q$dep$values[[q$i]]
              q$i <- q$i + 1
              next
            }
              
            vocab <- c('', q$dep$values[[q$i]])
            updateSelectInput(session, 'current_key', choices = vocab, selected = '')
            break
          }
        }
        
        if(q$i > length(q$dep$values)) {
          a$answers[[q$key]] <- paste(q$answers, collapse = ';') # answers for each iterate saved between semicolons
          a$unasked <- a$unasked[!a$unasked %in% q$j]  # Remove the question from the unasked list
          render_next_question()  # Re-check all unasked questions after an answer
        }
          
      })
    }
  }, ignoreNULL = FALSE, ignoreInit = TRUE)
     
  # Detect when a point coordinate or bounding box annotation is being submitted         
  observeEvent(input$save_coord, {
    q$i <- q$i + 1
    if(r$vocab[q$j,'values'] == 'point_coordinates') {
      q$current_coords[[q$i]] <- input$photo_click[c('x','y')]
      a$answers[[q$key]] <- paste(c(a$answers[[q$key]], paste(input$photo_click[c('x','y')], collapse=',')), collapse=';')
    } else {
      a$answers[[q$key]] <- paste(c(a$answers[[q$key]], paste(input$photo_brush[c('xmin','ymin','xmax','ymax')], collapse=',')), collapse=';')
    }
  }, ignoreInit = TRUE)
  
  # Detect when point coordinate or bounding box annotations are finished for a given question      
  observeEvent(input$next_question, {
    isolate({
      if(is.null(a$answers[[q$key]])) a$answers[[q$key]] <- NA
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
      intermediate_save()
      old_and_new <- rbind(r$old_annotations, r$annotations)
      write.table(old_and_new, file, quote = FALSE, sep = '\t', row.names = FALSE)
    }
  )
  
  # Create a photo object for the main panel
  output$current_photo <- renderImage({
    req(a$file)  # Ensure a file is ready
    # Return the filepath for renderImage to display
    list(src = a$file$datapath)
  }, deleteFile = FALSE)
  
  # Overlay points
  output$overlay_plot <- renderPlot({
    
    # Overlay circles 
    if(length(q$current_coords) > 0) {
      session$sendCustomMessage('getImageDimensions', list())
      par(mar = c(0,0,0,0))
      plot(NULL, type = "n", bty = 'n', xlab = "", ylab = "", xaxt = "n", yaxt = "n", xlim = c(1, input$image_width), ylim = c(1, input$image_height), xaxs='i', yaxs='i', asp=1)
      point.cex <- floor(input$image_height / 20) / strheight("o", cex = 1) # make diameter of circles 1/20th of plot height
      for(i in seq_along(q$current_coords)) {
        thiscolor <- if(i == q$i) adjustcolor("red", alpha.f=0.4) else adjustcolor("orange", alpha.f=0.6)
        points(q$current_coords[[i]][[1]], input$image_height - q$current_coords[[i]][[2]], col = thiscolor, pch = 21, bg = thiscolor, cex = point.cex)  # Draw circles
      }
    } else {
      par(mar = c(0,0,0,0))
      plot(NULL, type = "n", xlab = "", ylab = "", xaxt = "n", yaxt = "n", xlim = 1:2, ylim = 1:2)
    }
    
  }, bg = 'transparent')
  
  # Render image
  output$renderPhoto <- renderUI({
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

# to run locally:
# shiny::runApp('~/scripts/USF_Library_Ogden_Collection_resources/shiny_annotation')
#to deploy to static site:
# shinylive::export("~/scripts/USF_Library_Ogden_Collection_resources/shiny_annotation", "~/scripts/thecnidaegritty/photo_annotator", template_dir = "~/scripts/thecnidaegritty/scripts/shinylive_jekyll_template", template_params = list(title = 'Photo Annotator', permalink = '/photo_annotator/'))
