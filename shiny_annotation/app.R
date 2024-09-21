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
        uiOutput("photo_ui"),
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
  
  # hardcode a couple of variables that will be reused a few times
  # temporary output directory
  tmpdir <- file.path(tempdir(), as.integer(Sys.time()))
  dir.create(tmpdir)
  dir.create(file.path(tmpdir,'inputs'))
  dir.create(file.path(tmpdir,'outputs'))
  # custom formatting for tsv output
  write.tsv <- \(o,f) write.table(o, f, quote = FALSE, sep = '\t', row.names = FALSE)
  # slightly shorter to use variable
  coordinate_types <- c('point_coordinates', 'box_coordinates')
  # currently only allowable ranks that can be specified in prompt file, in order (others can exist in tax file)
  rankorder <- c("subspecies", "species", "subgenus", "genus",
                 "subtribe", "tribe", "subfamily", "family", "superfamily",
                 "infraorder", "suborder", "order", "superorder",
                 "infraclass", "subclass", "class", "superclass",
                 "infraphylum", "subphylum", "phylum",
                 "subkingdom", "kingdom", "realm")

  # Reactive values are variables that have global scope and depend on things like user input
  # Unlike regular variables, they can be monitored by 'observers' to automatically trigger events whenever they change
  
  # Define variables that will contain global information for the session
  r <- reactiveValues(workflow_step     = 'annotator_id',
                      prompts_file      = NULL,
                      prompts           = NULL,
                      parsed            = NULL,
                      values_files      = list(),
                      values_data       = list(),
                      values_i          = 1,
                      tax_files         = list(),
                      tax_data          = list(),
                      tax_i             = 1,
                      not_in_database   = character(0),
                      photo_df          = NULL,
                      photos_not_done   = NULL,
                      old_annotations   = data.frame(),
                      annotation_type   = 'setup',
                      photo_keys        = character(0),
                      observation_keys  = character(0),
                      annotations       = NULL,         # only add to this at the end of each photo (row for each photo)
                      observations      = NULL,         # only add to this at the end of each photo (row for each coordinate selection across all photos)
                      messages          = NULL,
                      last_save         = 0,
                      metadata          = list(annotator_id       = NA,
                                               session_start_time = NA))
  
  # Define variables that are reset for each photo
  a <- reactiveValues(i            = NULL, # which photo are we on
                      annotations  = NULL, # add relevant inputs to this directly (single row)
                      observations = NULL, # add relevant inputs to this directly (row for each coordinate selection in photo)
                      idx_type     = NULL, # What is currently iterating
                      metadata     = list(filename               = NA,
                                          photo_annotation_start = NA))
  
  # Define variables that are reset for each line of questioning in the prompts file
  q <- reactiveValues(i = NULL) # which line of the prompts file are we on
  
  # Define variables that are reset for specific sub-questions 
  sq <- reactiveValues(n = 0,    # how many lines of a$observations exist before the current question
                       i = NULL, # which line of a$observations are we on
                       j = NULL, # which stage of a question are we on (e.g. when using an external taxonomy)
                       tax_filt = NULL, # taxonomy filtered to current sub-question
                       question_text = NULL,
                       values = NULL) # what are the values that are currently available for a drop-down
  
  # Reactive UI elements will be automatically updated whenever the reactive values that they depend on change
  # When r$workflow_step is updated, the setup UI will change
  output$setup_ui <- renderUI({
    switch(r$workflow_step,
      annotator_id = list(
        textInput("annotator_id", "Please enter your initials or other identifer"),
        actionButton("annotator_id_submit", "Submit")
      ),
      prompts_file = list(
        fileInput("prompts", "Select a file with prompts"),
        actionButton("default_prompts", "Use example")
      ),
      values_files = list(
        fileInput("values_file", paste0("Select the values file that is named `", names(r$values_files)[[r$values_i]], "` in the prompts file")),
        if(r$values_i == 1) actionButton("default_values_file", "Use example Caribbean coral traits") else if(r$values_i == 2) actionButton("default_values_file", "Use example Caribbean fish list") else ''
      ),
      tax_files = list(
        fileInput("tax_file", paste0("Select the taxonomy file that is named `", names(r$tax_files)[[r$tax_i]], "` in the prompts file")),
        actionButton("default_tax_file", "Use default GBIF backbone taxonomy")
      ),
      photo_files = list(
        fileInput("photos", "Select a set of files to annotate", multiple = TRUE, accept = 'image/*'),
        actionButton("default_photos", "Use examples")
      ),
      previous_annotations = list(
        fileInput("resume_file", "Select a previous annotations file (or don't)"),
        actionButton("begin", "Begin")
      ),
      oops_tax = list(
        HTML(paste0('<p>The following taxa, specified in the "values" column of "', r$prompts_file$name, '", are not present in "', r$tax_files[[1]]$name, '". Please remove or replace them and restart the app.</p>',
                    '<p>', paste(r$not_in_database, collapse = '</p><p>'),'</p>'))
      ),
      oops_vf = list(
        HTML(paste0('<p>The following column names, specified in the "values" column of "', r$prompts_file$name, '", are not present in "', r$vf_files[[1]]$name, '". Please remove or replace them and restart the app.</p>',
                    '<p>', paste(r$not_in_database, collapse = '</p><p>'),'</p>'))
      ),
      annotate = NULL
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
  }, ignoreInit = TRUE)
  
  # This one triggers if the user chooses their own filepath
  # req() is needed here but not for the actionButton observer because the fileInput reactive value can be initially set to NULL on loading of the UI, which would otherwise be enough to trigger this event
  observeEvent(input$prompts, {
    req(input$prompts)
    r$prompts_file <- input$prompts
  })
  
  # This event is triggered by the assignment of a derived reactive value in `r` rather than by direct user input
  # It will process the prompts file and define the data structure and see whether taxonomy files need to be selected
  # If no taxonomic files are needed, then that step is skipped
  observeEvent(r$prompts_file, {
    req(r$prompts_file)
    
    r$prompts <- read.table(r$prompts_file$datapath, header = TRUE, sep = '\t', comment.char='')
    
    # Create a list of named lists by splitting fields for each line of the prompts file
    r$parsed <- apply(r$prompts, 1, \(x) {
      
      keys_sep <- strsplit(x[['key']], ':')[[1]]
      if(length(keys_sep) > 1) {
        # If the keys field has a ':', then it names an taxonomy file and the values to filter from it
        tax_name <- keys_sep[[1]]
        keys <- strsplit(keys_sep[[2]], ',')[[1]]
        # Since tax files lead through multiple prompts, the question text can be configured to specify the current key by placing {key} in the string
        question_text <- setNames(sapply(keys, \(y) gsub('{key}', y, x[['question_text']], perl=TRUE)), keys)
      } else {
        # No tax files
        tax_name <- character(0)
        if(x[['key']] %in% coordinate_types) {
          # coordinate picking is another special case that produces values in multiple predefined columns
          keys <- c('observation_type', 'point_or_box', 'coordinates')
        } else {
          # all other prompts should have a single column name associated with them
          keys <- x[['key']]
        }
        question_text <- setNames(x[['question_text']], x[['key']])
      }
      
      values_sep <- strsplit(x[['values']], ':')[[1]]
      if(length(values_sep) > 1) {
        vf_name <- values_sep[[1]]
        values <- strsplit(values_sep[[2]], ',')[[1]] # actually names of columns in file that contains values
      } else {
        vf_name <- character(0)
        values <- unlist(strsplit(x[['values']], ",")[[1]]) # actual values
      }

      if(is.na(x[['dependencies']]) & !x[['key']] %in% coordinate_types) {
        # an NA specifies that the prompt happens no matter what
        dependencies <- list()
      } else {
        # Multiple dependencies split by ';'; name separated from acceptable values with ':', list of values split by ','
        dependencies <- lapply(strsplit(strsplit(x[['dependencies']], ";")[[1]], ':'), \(y) list(y[[1]], strsplit(y[[2]],',')[[1]]))
        dependencies <- setNames(lapply(dependencies, \(y) y[[2]]), sapply(dependencies, \(y) y[[1]]))
      }
      
      return(list(tax_name=tax_name, keys=keys, dependencies=dependencies, vf_name=vf_name, values=values, question_text=question_text))
      
    })
    
    # Make it easy to see later if a given prompt is associated with coordinate observations or is photo-wide
    # Collect relevant keys for each type for easy retrieval
    for(i in seq_along(r$parsed)) {
      
      # coordinate pickers are the top level for sub-photo observations
      if(r$parsed[[i]]$keys[[1]] == 'observation_type') {
        r$parsed[[i]]$which_df <- 'observations'
        r$observation_keys <- unique(c(r$observation_keys, r$parsed[[i]]$keys))
        next
      }
      
      # anything else without dependencies must be photo-wide
      if(length(r$parsed[[i]]$dependencies) == 0) {
        r$parsed[[i]]$which_df <- 'annotations'
        r$photo_keys <- unique(c(r$photo_keys, r$parsed[[i]]$keys))
        next
      }
      
      # if any dependency is a sub-observation, then so is this
      r$parsed[[i]]$which_df <- if(any(sapply(r$parsed, \(x) names(r$parsed[[i]]$dependencies) %in% r$observation_keys))) 'observations' else 'annotations'
      if(r$parsed[[i]]$which_df == 'observations') {
        r$observation_keys <- unique(c(r$observation_keys, r$parsed[[i]]$keys))
      } else {
        r$photo_keys <- unique(c(r$photo_keys, r$parsed[[i]]$keys))
      }
      
    }
    
    # Look through keys for the special format `file:keys`, and create an empty named list to be later filled in via fileInput
    tax_names <- unique(sapply(r$parsed, \(x) x$tax_name))
    tax_names <- tax_names[sapply(tax_names, length) > 0]
    r$tax_files <- r$tax_data <- setNames(vector("list", length(tax_names)), tax_names)
    
    vf_names <- unique(sapply(r$parsed, \(x) x$vf_name))
    vf_names <- vf_names[sapply(vf_names, length) > 0]
    r$values_files <- r$values_data <- setNames(vector("list", length(vf_names)), vf_names)
    
    # Primary data storage for annotations associated with whole photo
    # Includes timestamps, annotator ID, and other metadata in addition to custom annotations and an extra column for notes
    r$annotations <- setNames(
                       data.frame(
                         matrix(ncol = length(r$metadata) + length(a$metadata) + length(r$photo_keys) + 1,
                                nrow = 0)
                       ),
                       c(names(r$metadata), names(a$metadata), r$photo_keys, 'photo_notes')
                     )
    
    # Primary data storage for annotations associated with specific coordinate-based observations within the photos
    # Includes photo name and timestamp to link to photo-wide observations for merging, in addition to custom annotations
    r$observations <- setNames(data.frame(matrix(ncol = length(a$metadata) + length(r$observation_keys), nrow = 0)), 
                               c(names(a$metadata), r$observation_keys))
    
    # For each photo, specific observations will be stored separately until the photo is finalized
    a$annotations <- r$annotations
    a$observations <- r$observations
    
    if(length(r$values_files) > 0) {
      r$workflow_step <- 'values_files'
    } else if(length(r$tax_files) > 0) {
      r$workflow_step <- 'tax_files'
    } else {
      r$workflow_step <- 'photo_files'
    }
  })
  
  vf_prep <- function() {
    # Load the data
    r$values_data[[r$values_i]] <- read.table(r$values_files[[r$values_i]]$datapath, header = TRUE, sep = '\t', comment.char='', quote='')
    cols <- unique(unlist(sapply(r$parsed[sapply(r$parsed, \(x) length(x$vf_name)>0)], \(y) y$values)))
    r$not_in_database <- cols[!cols %in% colnames(r$values_data[[r$values_i]])]
    if(length(r$not_in_database) > 0) {
      r$workflow_step <- 'oops_vf'
    } else {
      # Either prompt for another values file or move on
      if(r$values_i < length(r$values_files)) {
        r$values_i <- r$values_i + 1
      } else if(length(r$tax_files) > 0) {
        r$workflow_step <- 'tax_files'
      } else {
        r$workflow_step <- 'photo_files'
      }
    }
  }
  
  # If the user uploads their own values file
  observeEvent(input$values_file, {
    req(input$values_file)
    # Record user input
    r$values_files[[r$values_i]] <- input$values_file
    vf_prep()
  })
  
  # If the user chooses the default values file rather than specifying their own
  observeEvent(input$default_values_file, {
    # Assign default info
    r$values_files[[r$values_i]] <- list(
                                      list(name     = 'Example Caribbean corals traits',
                                           datapath = 'www/example_corals.tsv'),
                                      list(name     = 'Example Caribbean fish list',
                                           datapath = 'www/example_fish.tsv')
                                        )[[r$values_i]]
    vf_prep()
  }, ignoreInit = TRUE)
  
  
  # Filter the taxonomy taxa file so it's quicker to search through
  # Currently assumes only one tax file and only the default, pre-filtered GBIF backbone has been tested
  filter_tax <- function(targets, tax) {
    # Entries in the tax file that are directly listed in the prompt file
    res <- tax[tax$canonicalName %in% targets,]
    
    # Parent taxa of the directly mentioned ones that are not already in the list
    missinghigher <- tax$taxonID[tax$taxonID %in% res$parentNameUsageID]
    missinghigher <- missinghigher[!missinghigher %in% res$taxonID]
    
    # Child taxa of the directly mentioned ones that are not already in the list
    missinglower <- tax$taxonID[tax$parentNameUsageID %in% res$taxonID]
    missinglower <- missinglower[!missinglower %in% res$taxonID]
    
    # Iterate toward the root to find all ancestors
    if(length(missinghigher) > 0) {
      higherres <- tax[tax$taxonID %in% missinghigher,]
      while(TRUE) {
        res <- rbind(res, higherres)
        
        missinghigher <- tax$taxonID[tax$taxonID %in% higherres$parentNameUsageID]
        missinghigher <- missinghigher[!missinghigher %in% res$taxonID]
        
        if(length(missinghigher) > 0) {
          higherres <- tax[tax$taxonID %in% missinghigher,]
        } else {
          break
        }
      }
    }
    
    # Iterate toward the tips to find all descendants
    if(length(missinglower) > 0) {
      lowerres <- tax[tax$taxonID %in% missinglower,]
      while(TRUE) {
        res <- rbind(res, lowerres) 
        
        missinglower <- tax$taxonID[tax$parentNameUsageID %in% lowerres$taxonID]
        missinglower <- missinglower[!missinglower %in% res$taxonID]
        
        if(length(missinglower) > 0) {
          lowerres <- tax[tax$taxonID %in% missinglower,]
        } else {
          break
        }
      }
    }
    
    return(res)
  }
  
  # If the tax_file UI is displayed and the user then chooses an tax_file, store it in the list
  # If there are more tax files to specify, increment the counter to refresh the fileInput prompt
  # Otherwise, trigger the photo input step
  tax_prep <- function() {
    # Load the data
    r$tax_data[[r$tax_i]] <- read.table(r$tax_files[[r$tax_i]]$datapath, header = TRUE, sep = '\t', comment.char='', quote='')
    # Filter the file to only contain relevant taxa from the prompt file
    targets <- c(
                 unique(gsub('_', ' ', unlist(sapply(r$parsed[sapply(r$parsed, \(x) length(x$tax_name)>0 & length(x$vf_name) == 0)], \(y) y$values)))),
                 unique(gsub('_', ' ', unlist(sapply(r$parsed[sapply(r$parsed, \(x) length(x$tax_name)>0 & length(x$vf_name) > 0)], \(y) r$values_data[[y$vf_name]][,y$values]))))
                )
    r$not_in_database <- targets[!targets %in% r$tax_data[[1]]$canonicalName]
    if(length(r$not_in_database) > 0) {
      r$workflow_step <- 'oops_tax'
    } else {
      r$tax_data[[r$tax_i]] <- filter_tax(targets, r$tax_data[[r$tax_i]])
      # Either prompt for another tax file or move on
      if(r$tax_i < length(r$tax_files)) {
        r$tax_i <- r$tax_i + 1
      } else {
        r$workflow_step <- 'photo_files'
      }
    }
  }
    
  observeEvent(input$tax_file, {
    req(input$tax_file)
    # Record user input
    r$tax_files[[r$tax_i]] <- input$tax_file
    tax_prep()
  })
  
  # If the user chooses the default GBIF taxonomy rather than specifying their own tax file
  observeEvent(input$default_tax_file, {
    # Assign default info
    r$tax_files[[r$tax_i]] <- list(name     = 'GBIF backbone 2023-08-28, valid and not-extinct taxa',
                                   datapath = 'www/gbif_backbone_accepted_living.tsv.gz')
    tax_prep()
  }, ignoreInit = TRUE)
  
  # Respond to user photo choices
  observeEvent(input$default_photos, {
    r$photo_df <- data.frame(datapath = c('www/photo_1_fish.jpg', 'www/photo_2_coral.jpg'),
                             name     = c('photo_1_fish.jpg',     'photo_2_coral.jpg'))[sample(2),]
    r$workflow_step <- 'previous_annotations'
  }, ignoreInit = TRUE)
  
  observeEvent(input$photos, {
    req(input$photos)
    r$photo_df <- input$photos[sample(nrow(input$photos)),] # randomize order
    r$workflow_step <- 'previous_annotations'
  })
  
  # Before we can respond to user input here, we will need to prepare the upcoming UI elements and define some functions
  # Load the photo to annotate
  output$photo_ui <- renderUI({
    req(a$i, q$i)
    click <- NULL
    brush <- NULL
    if(q$i <= length(r$parsed)) {
      if(r$parsed[[q$i]]$which_df == 'observations' & length(r$parsed[[q$i]]$question_text) == 1) {
        if(names(r$parsed[[q$i]]$question_text) == 'point_coordinates') {
          click <- clickOpts(id = "photo_click", clip = FALSE)
        } else {
          brush <- brushOpts(id = "photo_brush", clip = FALSE, resetOnNew = TRUE)
        }
      }
    }
    imageOutput("current_photo",
                width  = '100%',
                height = '100%',
                click  = click,
                brush  = brush)
  })

  output$current_photo <- renderImage({
    req(r$photos_not_done, a$i)
    list(src = r$photos_not_done$datapath[[a$i]])
  }, deleteFile = FALSE)
  
  # Overlay points for observations
  output$overlay_plot <- renderPlot({
    req(q$i, sq$i)
    if(q$i > length(r$parsed)) return(NULL)
    if(r$parsed[[q$i]]$which_df == 'annotations') return(NULL)
    req(nrow(a$observations) > 0)
    
    current_coords <- lapply(strsplit(a$observations$coordinates, ','), as.numeric)
    is_point <- a$observations$point_or_box == 'point'

    # Get actual image size with external javascript function
    session$sendCustomMessage('getImageDimensions', list())
    req(input$image_width, input$image_height)
    
    # Create an empty plot area with size and pixels equal to photo
    par(mar = c(0,0,0,0))
    plot(NULL, 
         type = "n", bty = 'n', xlab = "", ylab = "", xaxt = "n", yaxt = "n", xaxs='i', yaxs='i', asp=1, 
         xlim = c(1, input$image_width), ylim = c(1, input$image_height))
    # Make diameter of circles 1/20th of plot height
    point.cex <- floor(input$image_height / 20) / strheight("o", cex = 1)
    # Add circles for all sub-items relevant to current question
    for(i in seq_along(current_coords)) {
      # Current item red; others in same category orange; others grey; all with transparency
      picking_now <- r$parsed[[q$i]]$keys[[1]] == 'observation_type'
      typematch <- a$observations$observation_type[i] == a$observations$observation_type[sq$i]
      current_color <- if(i == sq$i & (!picking_now | sq$i > sq$n)) {
        adjustcolor("red", alpha.f = 0.5) 
      } else if(typematch & (!picking_now | sq$i > sq$n)) {
        adjustcolor("orange", alpha.f = 0.6)
      } else {
        adjustcolor("black", alpha.f = 0.5)
      }
      if(is_point[i]) {
        points(current_coords[[i]][[1]],
               input$image_height - current_coords[[i]][[2]], # y-axis is reversed for plots compared to images
               col = current_color,
               pch = 21,
               bg  = current_color,
               cex = point.cex)
      } else {
        rect(current_coords[[i]][[1]],
             input$image_height - current_coords[[i]][[2]],
             current_coords[[i]][[3]],
             input$image_height - current_coords[[i]][[4]],
             border = current_color,
             lwd = point.cex)
      }
    }

  }, bg = 'transparent')

  # Some text summarizing the setup
  output$setup_info <- renderText({
    req(a$i)
    paste0(
      'Prompts file (', nrow(r$prompts), ' prompts): ', r$prompts_file$name, '\n',
      if(length(r$values_files) > 0) paste0('Values files:\n',   paste(sapply(seq_along(r$values_files), \(i) paste0('  ', names(r$values_files)[[i]], ': ', r$values_files[[i]]$name)), collapse = '\n'), '\n'),
      if(length(r$tax_files)    > 0) paste0('Taxonomy files:\n', paste(sapply(seq_along(r$tax_files),    \(i) paste0('  ', names(r$tax_files)[[i]],    ': ', r$tax_files[[i]]$name)),    collapse = '\n'), '\n'),
      nrow(r$photo_df), ' photos\n',
      ifelse(is.null(input$resume_file$datapath), 
             'No old annotations - starting from scratch!\n', 
             paste0('Old annotations (', nrow(r$old_annotations), ' photos): ', input$resume_file$name, '\n', nrow(r$photo_df) - nrow(r$old_annotations), ' new photos\n'))
    )
  })
  
  # This is the workhorse UI element for annotation input
  output$question_ui <- renderUI({
    switch(r$annotation_type,
      setup = NULL,
      dropdown = list(
        selectInput("current_key", 
                    sq$question_text,
                    c('', sq$values)),
        downloadButton("save_file", "Discard this photo and save the rest"),
        actionButton("reset_photo", "Reset photo")
      ),
      coordinate = list(
        renderText({if(q$i < length(r$parsed)) r$parsed[[q$i]]$question_text}),
        HTML('<p>The current item is <span style="color: red">red</span>, related items are <span style="color: orange">orange</span>, and all others are <span style="color: grey">grey</span></p>'),
        div(
          actionButton("remove_coord",  "Undo click"),
          actionButton("next_question", "Next question")
        ),
        downloadButton("save_file", "Discard this photo and save the rest"),
        actionButton("reset_photo", "Reset photo")
      ),
      unstructured_text = list(
        textAreaInput("extra_notes", if(q$i < length(r$parsed)) r$parsed[[q$i]]$question_text, rows = 4, resize = 'both'),
        actionButton("submit_extra_notes",  "Submit observation notes")
      ),
      photo_finished = list(
        HTML("<p>Structured questions finished - finish notes and click Next, Save, or Reset<p>"),
        {
          timespan <- as.numeric(round(difftime(Sys.time(), r$last_save_time, units='mins'),0))
          n_unsaved <- a$i - r$last_save
          HTML(paste0('<p>You have not saved for <span style="color:', if(timespan > 30) 'red' else if(timespan > 15) 'orange' else 'green', '">', timespan, '</span> minutes and <span style="color:', if(n_unsaved > 50) 'red' else if(n_unsaved > 25) 'orange' else 'green', '">', n_unsaved,' photos<p>'))
        },
        actionButton("next_photo",  "Next photo"),
        downloadButton("save_file", "Save data"),
        actionButton("reset_photo", "Reset photo")
      ),
      all_finished = list(
        HTML("<p>All photos finished - Finish notes and click Save or Reset<p>"),
        downloadButton("save_file", "Save data"),
        actionButton("reset_photo", "Reset photo")
      )
    )
  })
  
  # Add an area to enter unstructured extra photo annotations
  output$notes_ui <- renderUI({
    req(a$i)
    textAreaInput("notes", "Other notes", rows = 8, resize = 'both')
  })
  
  # Display various data and messages
  output$info <- renderText({
    req(a$i)
    annotations <- if(nrow(a$annotations) > 0) paste0(sapply(r$photo_keys, \(x) if(!is.na(a$annotations[[x]])) paste0(x, ': ', a$annotations[[x]], '\n') else ''), collapse='') else ''
    observations <- if(nrow(a$observations) > 0) paste0(sapply(r$observation_keys, \(x) if(!is.na(a$observations[sq$i,x])) paste0(x, ': ', a$observations[sq$i,x], '\n') else ''), collapse='') else ''
    coordinate_picking <- if(q$i <= length(r$parsed)) if(names(r$parsed[[q$i]]$question_text)[[1]] %in% coordinate_types) paste0(sq$i - sq$n, ' items identified\n') else NULL
    paste0(
      paste0('photo ', a$i, ' (', nrow(r$photos_not_done) - a$i, ' photos left): ', paste(a$metadata$filename, '\n')),
      annotations,
      observations,
      coordinate_picking,
      paste(r$messages, collapse = '\n')
    )
  })
  
  # Whenever a new question is rendered, these values need to be reset
  initialize_prompt <- function() {
    r$tax_i <- 1
    sq$n <- nrow(a$observations)
    sq$i <- 1
    sq$j <- 1
  }
  
  # Keep incrementation organized
  increment_idx <- function(qis) {
    if(qis == 'qi') {
      q$i <- q$i + 1
      initialize_prompt()
    } else if(qis == 'sqi') {
      sq$j <- 1
      sq$i <- sq$i + 1
    } else if(qis == 'sqj') {
      sq$j <- sq$j + 1
    }
  }
  
  # When a prompt uses an taxonomy file, parse the hierarchy to determine the next question or autofill unique options
  # Intended to ultimately be rather flexible, but for now assumes a single taxonomy file in 'Darwin Core Archive' biological taxonomy format from GBIF
  tax_hierarchy <- function() {
    # Taxonomy file traversal
    # Filter the database to taxa relevant to current prompt
    if(sq$j == 1) sq$tax_filt <- filter_tax(gsub('_', ' ', sq$values), r$tax_data[[1]])
    ordered_keys <- rankorder[rankorder %in% colnames(a[[r$parsed[[q$i]]$which_df]])]
    current_taxonomy <- a[[r$parsed[[q$i]]$which_df]][sq$i,ordered_keys]
    previous_ranknums <- which(!is.na(current_taxonomy))
    current_taxonomy <- current_taxonomy[previous_ranknums]    
    if(length(current_taxonomy) > 0) {
      current_ranknum <- match(r$parsed[[q$i]]$keys[[sq$j]], ordered_keys)
      identified <- previous_ranknums[current_taxonomy != 'unidentifiable']
      if(length(identified) > 0) {
        # If any previous ranks were identifiable, use the most precise one to further filter options
        lowest_identified <- min(identified)
        sq$tax_filt <- filter_tax(gsub('_', ' ', current_taxonomy[ordered_keys[lowest_identified]]), sq$tax_filt)
      }
      unidentified <- previous_ranknums[current_taxonomy == 'unidentifiable']
      if(length(unidentified) > 0) {
        highest_unidentifiable <- max(unidentified)
        if(highest_unidentifiable > current_ranknum || all(!duplicated(sq$tax_filt$parentNameUsageID[sq$tax_filt$taxonRank == ordered_keys[[highest_unidentifiable]]]))) {
          # If any previous unidentifiable ranks were higher than the current one, then it is too 
          # Or, if all the current options have 1:1 correspondence with previously indistinguishable taxa (e.g. each previously indistinguishable genus belonged to a separate family), then the current rank is also unidentifiable
          sq$values <- 'unidentifiable'
          return()
        }
      }
    }
    values <- sort(sq$tax_filt$canonicalName[sq$tax_filt$taxonRank == r$parsed[[q$i]]$keys[[sq$j]]])
    if(length(values) == 0) {
      sq$values <- NA
    } else if(length(values) == 1) {
      sq$values <- values
    } else {
      sq$values <- c(values, 'unidentifiable')
    }
    sq$question_text <- r$parsed[[q$i]]$question_text[[sq$j]]

  }

  # Construct prompts or autofill values using the indices and data stored in global reactives
  render_next_question <- function() {
    
    if(q$i > length(r$parsed)) {
      # If question index is greater than the number of questions, then we are done with this photo
      # Increment indices beyond relevant range to disable annotation plots
      q$i <- length(r$parsed) + 1
      sq$i <- nrow(a$observations) + 1
      # Then activate prompts to continue to next photo or finalize session
      if(a$i < nrow(r$photos_not_done)) {
        r$annotation_type <- 'photo_finished'
      } else {
        r$annotation_type <- 'all_finished'
      }
      return()
    }
    
    if(r$parsed[[q$i]]$which_df == 'observations') {
      # Current call is iterating through within-photo observations
      a$idx_type <- 'sqi'
      if(sq$i > nrow(a$observations) & r$parsed[[q$i]]$keys[[1]] != 'observation_type') {
        # If observation index is greater than the number of observations 
        # and we're not adding more via coordinate selection
        # then we're done with this question
        increment_idx('qi')
        return(render_next_question())
      }
    } else {
      # Current call is iterating through photo-wide annotations
      a$idx_type <- 'qi'
    }
    
    if(length(r$parsed[[q$i]]$dependencies) == 0) {
      # If no dependencies, then always proceed
      fulfilled <- TRUE
    } else if(length(r$parsed[[q$i]]$tax_name) > 0 & sq$j > length(r$parsed[[q$i]]$keys)) {
      # If reaching end of taxonomy file traversal
      fulfilled <- FALSE
    } else {
      # Only proceed if ALL dependencies are fulfilled
      fulfilled <- all(sapply(names(r$parsed[[q$i]]$dependencies), \(x) {
        # Is dependency a photo-wide annotation or a within-photo observation
        deptype <- if(x %in% r$observation_keys) 'observations' else 'annotations'
        # For each dependency, annotation can match ANY of the listed values (or have any value at all if the listed value is the special keyword 'any')
        a[[deptype]][sq$i,x] %in% r$parsed[[q$i]]$dependencies[[x]] | (r$parsed[[q$i]]$dependencies[[x]] == 'any' & !is.na(a[[deptype]][sq$i,x]))
      }))
    }
    
    if(!fulfilled) {
      # Question not applicable; move to next observation or annotation
      increment_idx(a$idx_type)
      return(render_next_question())
    }

    if(r$parsed[[q$i]]$keys[[1]] == 'observation_type') {
      # Send signal to update UI for coordinate selection
      sq$i <- nrow(a$observations)
      r$annotation_type <- 'coordinate'
      return()
    }
    
    if(r$parsed[[q$i]]$values[[1]] == 'unstructured_text') {
      # Send signal to update UI for unstructured text prompt
      r$annotation_type <- 'unstructured_text'
      return()
    }
    
    if(length(r$parsed[[q$i]]$vf_name) == 0) {
      # No values file; starting values directly specified in prompts file
      sq$values <- r$parsed[[q$i]]$values
    } else {
      # Grab values from file and pre-filter based on previous annotations
      df <- r$values_data[[r$parsed[[q$i]]$vf_name]]
      old_answers <- a[[r$parsed[[q$i]]$which_df]][sq$i,, drop = FALSE]
      old_answers <- old_answers[,!is.na(old_answers[1,]) & colnames(old_answers) %in% colnames(df), drop = FALSE]
      if(ncol(old_answers) > 0) {
        shared <- intersect(colnames(old_answers), colnames(df))
        keepers <- apply(df[,shared, drop = FALSE],
                         1,
                         \(x) {
                           # split values in row of values file
                           splits <- strsplit(as.character(x), ',')
                           # if every shared column has at least one match
                           all(sapply(seq_along(x), \(y) any(splits[[y]] %in% old_answers[[y]])))
                         })
        df <- df[keepers,, drop = FALSE]
      }
      sq$values <- df[,r$parsed[[q$i]]$values]
    }

    if(length(r$parsed[[q$i]]$tax_name) == 0) {
      # No taxonomy file; prepare drop-down menu directly from prompts file
      sq$question_text <- r$parsed[[q$i]]$question_text
    } else {
      # Prepare drop-down menu by traversing taxonomic hierarchy
      a$idx_type <- 'sqj'
      tax_hierarchy()
    }

    if(length(sq$values) == 1) {
      # Only one option; autofill it and move on
      a[[r$parsed[[q$i]]$which_df]][sq$i,r$parsed[[q$i]]$keys[[sq$j]]] <- sq$values
      increment_idx(a$idx_type)
      return(render_next_question())
    } else {
      # Send signal to update UI for dropdown menu input
      r$annotation_type <- 'dropdown'
    }

  }
  
  # Initializes or resets inputs for current photo
  initialize_photo <- function() {
    a$annotations <- r$annotations[0,]
    a$observations <- r$observations[0,]
    a$metadata$filename <- r$photos_not_done$name[[a$i]]
    a$metadata$photo_annotation_start <- format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z")
    q$i <- 1
    initialize_prompt()
    updateTextAreaInput(session, 'notes', value = '')
    render_next_question()
  }
  
  begin <- function() {
    file.copy(r$prompts_file$datapath, file.path(tmpdir, 'inputs'))
    for(i in seq_along(r$values_files)) {
      file.copy(r$values_files[[i]]$datapath, file.path(tmpdir, 'inputs'))
    }
    for(i in seq_along(r$tax_files)) {
      oldname_cleaned <- sub('_prefiltered', '', sub('\\..[^\\.]*$', '', r$tax_files[[i]]$name))
      write.tsv(r$tax_data[[i]], file.path(tmpdir, 'inputs', paste0(oldname_cleaned, '_pre-filtered.tsv')))
    }
    r$metadata$session_start_time <- format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z")
    r$last_save_time <- Sys.time()
    a$i <- 1
    r$workflow_step <- 'annotate'
    initialize_photo()
  }
  
  # Now, back to watching for user input to finalize setup
  # Retrieve info when the previous annotation file button is used
  observeEvent(input$resume_file, {
    req(input$resume_file)
    r$old_annotations <- read.table(input$resume_file$datapath, header = TRUE, sep = '\t')
    r$photos_not_done <- r$photo_df[!r$photo_df$name %in% r$old_annotations$filename,]
    begin()
  })
    
  # Skip uploading previous annotations by clicking Begin
  observeEvent(input$begin, {
    r$photos_not_done <- r$photo_df
    begin()
  }, ignoreInit = TRUE)
  
  ## And then watch for responses to the various annotation prompts
  # Detect when an answer is entered via drop-down menu
  observeEvent(input$current_key, {
    req(input$current_key)
    key <- r$parsed[[q$i]]$keys[[sq$j]]
    a[[r$parsed[[q$i]]$which_df]][sq$i,key] <- input$current_key
    increment_idx(a$idx_type)
    updateSelectInput(session, 'current_key', selected = '')
    render_next_question()
  })

  # Detect when a point coordinate is being submitted         
  observeEvent(input$photo_click, {
    req(names(r$parsed[[q$i]]$question_text) == 'point_coordinates')
    sq$i <- sq$i + 1
    a$observations[sq$i,'observation_type'] <- r$parsed[[q$i]]$values
    a$observations[sq$i,'point_or_box'] <- 'point'
    a$observations[sq$i,'coordinates'] <- paste(input$photo_click[c('x','y')], collapse = ',')
  })
  
  # Detect a bounding box annotation
  observeEvent(input$photo_brush, {
    req(names(r$parsed[[q$i]]$question_text) == 'box_coordinates')
    sq$i <- sq$i + 1
    a$observations[sq$i,'observation_type'] <- r$parsed[[q$i]]$values
    a$observations[sq$i,'point_or_box'] <- 'box'
    a$observations[sq$i,'coordinates'] <- paste(input$photo_brush[c('xmin','ymin','xmax','ymax')], collapse = ',')
  })
  
  # Undo a coordinate selection
  observeEvent(input$remove_coord, {
    # Make sure not to remove observations from previous questions
    req(sq$i > sq$n)
    a$observations <- a$observations[-sq$i,]
    sq$i <- sq$i - 1
  })
  
  # Detect when point coordinate or bounding box annotations are finished for a given question      
  observeEvent(input$next_question, {
    q$i <- q$i + 1
    initialize_prompt()
    render_next_question()
  })
  
  # Watch for manual reset button
  observeEvent(input$reset_photo, {
    initialize_photo()
  })
  
  observeEvent(input$submit_extra_notes, {
    a$observations[sq$i,r$parsed[[q$i]]$keys] <- ifelse(input$extra_notes == '', 'None', gsub("\r?\n|\r|\t", " ", input$extra_notes))
    sq$i <- sq$i + 1
    updateTextAreaInput(session, 'extra_notes', value = '')
    render_next_question()
  }, ignoreInit = TRUE)
  
  # Add current photo's annotations to the master dataframe
  intermediate_save <- function() {
    # Should probably make these more automatic in case metadata changes
    a$annotations$annotator_id <- a$metadata$annotator_id
    a$annotations$session_start_time <- a$metadata$session_start_time
    a$annotations$filename <- a$metadata$filename
    a$annotations$photo_annotation_start <- a$metadata$photo_annotation_start
    a$annotations$photo_notes <- ifelse(input$notes == '', 'None', gsub("\r?\n|\r|\t", " ", input$notes))
    r$annotations <- rbind(r$annotations, a$annotations)
    if(nrow(a$observations) > 0) {
      a$observations$filename <- a$metadata$filename
      a$observations$photo_annotation_start <- a$metadata$photo_annotation_start
      r$observations <- rbind(r$observations, a$observations)
    }
    if(a$i < nrow(r$photos_not_done)) {
      a$i <- a$i + 1
      initialize_photo()
    } else {
      r$annotation_type <- 'done'
    }
  }
  
  # Move to next photo when 'Next' button is clicked
  observeEvent(input$next_photo, {
    intermediate_save()
  })
  
  # Save old and new combined annotations
  output$save_file <- downloadHandler(
    filename = function() {
      paste0("annotations_", Sys.Date(), ".zip")
    },
    content = function(file) {
      # If the current photo was finished, add it to the data; otherwise don't
      if(q$i >= length(r$parsed) & sq$i > nrow(a$observations)) {
        intermediate_save()
      } else {
        initialize_photo()
      }
      r$last_save <- a$i - 1
      r$last_save_time <- Sys.time()
      
      # Write the multiple outputs in a temporary directory
      write.tsv(r$annotations,  file.path(tmpdir, 'outputs', 'photo_annotations.tsv'))

      # Create some pre-merged files if there are within-photo observations
      if(nrow(r$observations) > 0) {
        merged_long <- merge(r$annotations, r$observations, by = names(a$metadata), suffixes = c('_whole_photo', ''))
        merged_wide <- r$annotations
        for(key in r$observation_keys) {
          if(key %in% colnames(merged_wide)) colnames(merged_wide)[colnames(merged_wide) == key] <- paste0(key, '_whole_photo')
          for(row in 1:nrow(merged_wide)) {
            idx <- which(apply(r$observations[,names(a$metadata)], 1, \(x) all(sapply(seq_along(x), \(y) x[[y]] == merged_wide[row,names(a$metadata)[[y]]]))))
            merged_wide[row,key] <- paste(r$observations[idx,key], collapse=';')
          }
        }
        write.tsv(r$observations, file.path(tmpdir, 'outputs', 'within_photo_observations.tsv'))
        write.tsv(merged_long,    file.path(tmpdir, 'outputs', 'merged_long.tsv'))
        write.tsv(merged_wide,    file.path(tmpdir, 'outputs', 'merged_wide.tsv'))
      }
      
      # Serve a single .zip file with all the files
      zip::zip(
        zipfile = file,
        files   = list.files(tmpdir, full.names = TRUE),
        mode    = "cherry-pick"
      )
    }
  )

}

shinyApp(ui, server)

# to run locally:
# shiny::runApp('~/scripts/USF_Library_Ogden_Collection_resources/shiny_annotation')
#to deploy to static site:
# shinylive::export("~/scripts/USF_Library_Ogden_Collection_resources/shiny_annotation", "~/scripts/thecnidaegritty/photo_annotator", template_dir = "~/scripts/thecnidaegritty/scripts/shinylive_jekyll_template", template_params = list(title = 'Photo Annotator', permalink = '/photo_annotator/'))
