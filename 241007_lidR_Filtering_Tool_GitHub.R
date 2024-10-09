# shiny packages for GUI
  # htmltools package for GUI
  if(!require(htmltools)){install.packages('htmltools')}
  library(htmltools)
  
  # shiny package
  if(!require(shiny)){install.packages('shiny')}
  library(shiny)
      
  # shinyFiles package
  if(!require(shinyFiles)){install.packages('shinyFiles')}
  library(shinyFiles)
      
  # shinythemes package
  if(!require(shinythemes)){install.packages('shinythemes')}
  library(shinythemes)

  # shinyWidgets package
  if(!require(shinyWidgets)){install.packages('shinyWidgets')}
  library(shinyWidgets)

      
# PREPARATIONS | SETTING UP SHINY GUI -------------------------------------

  # NULL root_path and input_path
  root_path <- NULL
  input_path <- NULL
  
  # Define translation functions
  # Post-Processing
  dict_post_processing <- c(
    "Yes" = TRUE,
    "No" = FALSE
  )
  
  # Cloth rigidness
  dict_cloth_rigidness <- c(
    "very soft (rugged terrain)" = 1L,
    "medium" = 2L,
    "hard (flat terrain)" = 3L
  )
  
  # Define UI
  ui <- fluidPage(
    responsive = TRUE,
    theme = shinytheme("darkly"),
    tags$head(
      tags$style(
        HTML("body, label, input, button, select { 
              font-family: 'Calibri', sans-serif; }
              .centered-button {
                display: block;
                margin: 0 auto;
                text-align: center;
              }
              .js-irs-0 .irs-single, .js-irs-0 .irs-bar-edge, .js-irs-0 .irs-bar {
                background: #00bc8c;
                border-top: 1px solid #00bc8c;
                border-bottom: 1px solid #00bc8c;
              }
              .irs-from, .irs-to, .irs-single {
                background: #00bc8c;
              }
              .js-irs-1 .irs-single, .js-irs-1 .irs-bar-edge, .js-irs-1 .irs-bar {
                background: #00bc8c;
                border-top: 1px solid #00bc8c;
                border-bottom: 1px solid #00bc8c;
              }
              .irs-from, .irs-to, .irs-single {
                background: #00bc8c;
              }
              .js-irs-2 .irs-single, .js-irs-2 .irs-bar-edge, .js-irs-2 .irs-bar {
                background: #00bc8c;
                border-top: 1px solid #00bc8c;
                border-bottom: 1px solid #00bc8c;
              }
              .irs-from, .irs-to, .irs-single {
                background: #00bc8c;
              }
              .js-irs-3 .irs-single, .js-irs-3 .irs-bar-edge, .js-irs-3 .irs-bar {
                background: #00bc8c;
                border-top: 1px solid #00bc8c;
                border-bottom: 1px solid #00bc8c;
              }
              .irs-from, .irs-to, .irs-single {
                background: #00bc8c;
              }
              .js-irs-4 .irs-single, .js-irs-4 .irs-bar-edge, .js-irs-4 .irs-bar {
                background: #00bc8c;
                border-top: 1px solid #00bc8c;
                border-bottom: 1px solid #00bc8c;
              }
              .irs-from, .irs-to, .irs-single {
                background: #00bc8c;
              }
                            .js-irs-5 .irs-single, .js-irs-5 .irs-bar-edge, .js-irs-5 .irs-bar {
                background: #00bc8c;
                border-top: 1px solid #00bc8c;
                border-bottom: 1px solid #00bc8c;
              }
              .irs-from, .irs-to, .irs-single {
                background: #00bc8c;
              }
        ")
      )
    ),
    tags$div(
      h4(strong("LAS Vegetation Filtering")),
      em("B. Müller, J. Walk (2024)")
    ),
    br(),
    tabsetPanel(
      tabPanel("Data Import", 
               br(),
               tags$div(
                 strong("Root Directory"),
                 p("Please select the parent folder."),
                 actionButton('root', 'Select')
               ),
               br(),
               verbatimTextOutput('result'),
               selectInput('shp_file', label = "Study Area (.shp)", choices = NULL),
               selectInput('las_file', label = "Point Cloud (.las)", choices = NULL),
               span(strong("Attention!"), style = "color: #00bc8c;"),
               p("The SHP and LAS files should be in the same CRS.")
      ),
      tabPanel("General Options", 
               br(),
               sliderInput('voxel_size', "Voxel Size [m]", min=0, max=0.25, value=0.02, step=0.005),
               sliderInput('dtm_resolution', "DTM Resolution [m]", min=0, max=0.25, value=0.02, step=0.01),
               span(strong("Attention!"), style = "color: #00bc8c;"),
               p("The voxel size should be equal to or smaller than the DTM resolution.")
      ),
      tabPanel("CSF Options",
               br(),
               sliderInput('cloth_resolution', "Cloth Resolution [m]", min=0, max=0.5, value=0.1, step=0.05),
               sliderInput('class_threshold', "Class Threshold [m]", min=0, max=0.1, value=0.01, step=0.01),
               br(),
               selectInput('cloth_rigidness', "Cloth Rigidness", c("very soft (rugged terrain)", "medium", "hard (flat terrain)")),
               selectInput('post_processing', 'Post-Processing for Steep Slopes:', c("Yes", "No"))
      ),
      tabPanel("GAM Options", 
               br(),
               sliderInput('smoothing_window', "Point Cloud Smoothing Window [m]", min=0, max=1, value=0.4, step=0.05),
               span(strong("Attention!"), style = "color: #00bc8c;"),
               p("The size of the smoothing window greatly affects processing speed.")
      ),
      tabPanel("Accuracy assessment", 
               br(),
               materialSwitch("accuracy_assessment", label = strong("Accuracy Assessment"), status="success", right=FALSE, value=TRUE),
               selectInput('csv_file', label = 'Ground Points (.csv)', choices = NULL),
               numericInput('reflector_height', label = 'Height above ground [m]', value = 0.02),
               span(strong("Attention!"), style = "color: #00bc8c;"),
               p("Accuracy assessment requires a CSV file containing measured ground points."),
               hr(),
               actionButton('start', "Start", width = "200px", height = "100px", style="color: #ffffff; background-color: #00bc8c", class = "centered-button"),
               br()
      ),
    )
  )
  
  server <- function(input, output, session) {
    
    # Define reactive values
    reactive_values <- reactiveValues(
      voxel_size = 0.01,
      dtm_resolution = 0.01,
      previous_voxel_size = 0.01
    )
    
    # Observe and update reactive values for voxel_size and dtm_resolution
    observe({
      if (!is.null(input$voxel_size)) {
        # Check if voxel_size has changed
        if (input$voxel_size != reactive_values$voxel_size) {
          # Check if voxel_size has increased
          if (input$voxel_size > reactive_values$previous_voxel_size) {
            # Update dtm_resolution by the same amount as voxel_size change
            new_dtm_resolution <- reactive_values$dtm_resolution + (input$voxel_size - reactive_values$previous_voxel_size)
            # Ensure dtm_resolution is not less than 0
            new_dtm_resolution <- max(new_dtm_resolution, 0)
            # Update reactive values
            reactive_values$voxel_size <- input$voxel_size
            reactive_values$previous_voxel_size <- input$voxel_size
            reactive_values$dtm_resolution <- new_dtm_resolution
            # Update sliderInput 'dtm_resolution' with the new value
            updateSliderInput(session, "dtm_resolution", value = new_dtm_resolution)
          } else {
            # Voxel size decreased, do not update dtm_resolution
            reactive_values$voxel_size <- input$voxel_size
            reactive_values$previous_voxel_size <- input$voxel_size
          }
        }
      }
    })
    
    # Store input values in global variables
    observe({
      if (!is.null(input$shp_file)) {
        shp_file_path <<- sprintf("%s\\shp\\%s", input_path, input$shp_file)
      }
      if (!is.null(input$las_file)) {
        las_file_path <<- sprintf("%s\\las\\%s", input_path, input$las_file)
      }
      voxel_size <<- reactive_values$voxel_size
      smoothing_window <<- input$smoothing_window
      dtm_resolution <<- reactive_values$dtm_resolution
      accuracy_assessment <<- input$accuracy_assessment
      if (!is.null(input$csv_file)) {
        csv_file_path <<- sprintf("%s\\csv\\%s", input_path, input$csv_file)
      }
      reflector_height <<- input$reflector_height
      cloth_rigidness <<- dict_cloth_rigidness[input$cloth_rigidness]
      cloth_resolution <<- input$cloth_resolution
      class_threshold <<- input$class_threshold
      post_processing <<- dict_post_processing[input$post_processing]
    })
    
    # Output root_path
    output$result <- renderPrint({
      paste("Selected root directory:", root_path)
    })
    
    # Action for Directory button
    observeEvent(input$root, {
      root_path <<- choose.dir(caption = "Select root folder:")
      input_path <<- sprintf("%s\\input", root_path)
      output_path <<- sprintf("%s\\output", root_path)
      
      output$result <- renderPrint({
        paste("Selected root directory:", root_path)
      })
      
      updateSelectInput(session, "shp_file", choices = list.files(sprintf("%s\\shp", input_path), pattern = "*.shp$"))
      updateSelectInput(session, "las_file", choices = list.files(sprintf("%s\\las", input_path), pattern = "*.las$"))
      updateSelectInput(session, "csv_file", choices = list.files(sprintf("%s\\csv", input_path), pattern = "*.csv$"))
    })
    
    # Action for Start button
    observeEvent(input$start, {
      session$close()
      
      # stopping App
#      stopApp()
#    })
#  }
  
  # Run the application
#  runApp(shinyApp(ui = ui, server = server))
  
      
      # REQUIREMENTS | INSTALLING AND IMPORTING PACKAGES ------------------------
        cat("Installing additional packages..\n")
      
        # Install packages if required and load packages
          # crayon package for coloring text outputs
          if(!require(crayon)){install.packages('crayon')}
          library(crayon)
    
          # data.table package to write csv
          if(!require(data.table)){install.packages('data.table')}
          library(data.table)
      
          # dplyr package for data frame manipulation
          if(!require(dplyr)){install.packages('dplyr')}
          library(dplyr)
          
          # lidR package for LIDAR processing
          if(!require(lidR)){install.packages('lidR')}
          library(lidR)
      
          # mgcv package for GAM Computation
          if(!require(mgcv)){install.packages('mgcv')}
          library(mgcv) 
          
          # Progress package to enhance progress estimation
          if(!require(progress)){install.packages('progress')}
          library(progress)
          
          # sf package to load shapefiles
          if(!require(sf)){install.packages('sf')}
          library(sf)
          
          # terra package for raster processing
          if(!require(terra)){install.packages('terra')}
          library(terra)
  
          # TreeLS package for tree detection
          if(!require(TreeLS)){devtools::install_github('tiagodc/TreeLS')}
          library(TreeLS)
          
          # raster package for raster processing
          if(!require(raster)){install.packages('raster')}
          library(raster)

      
      # Format console text output
        # Processing steps
        black <- make_style("black")
        note <- make_style("maroon")
        bgMaroon <- make_style(color = "#00bc8c", bg = TRUE)
        highlight <- combine_styles(black, bgMaroon)
        
      # Extract parent folder name
        # Extract directory path
        dir_path <- dirname(input_path)
        # Exract folder name
        foldername <- basename(dir_path)
        
      # PREPARATIONS | STEP 1 | LOAD SHP DATA -----------------------------
        
        cat(highlight("\n# LOADING STUDY AREA #\n\n"))
        
        # Read the SHP file of the study area
        study_area <- st_read(shp_file_path)
        
        # Buffer study area by XX m to avoid edge effects
        cat("\nBuffering study area..\n")
        study_area_buf <- st_buffer(study_area, dist = 2.5)
          #plot(study_area_buf$geometry, col = "#FFADB0")
          #plot(study_area$geometry, col = "#ADD8E6", add = TRUE)
        
        #Extract bb xy coordinates from buffered study area and save as separate variables
        study_area_bb <- st_bbox(study_area_buf)
          xmin <- as.numeric(study_area_bb[1])
          ymin <- as.numeric(study_area_bb[2])
          xmax <- as.numeric(study_area_bb[3])
          ymax <- as.numeric(study_area_bb[4])
        
        # Calculate the longest side of the buffered study area's bb
        dx = abs(xmax-xmin)
        dy = abs(ymax-ymin)
          
        # Safe longer side as new variable
        longest_side <- round(max(dx, dy)) + 1
        
        # Define chunk size
        cat("Calculating optimal chunk size..\n")
        chunk_size <- longest_side/4
      
      
      # PREPROCESSING | STEP 1 | CATALOG CLIPPING -------------------------------
        
        cat(highlight("\n# CATALOG CLIPPING #\n\n"))
        
        # Check if older exists
        if(file.exists(sprintf('%s\\01_pointcloud_clipped_merged', output_path))){
          cat(note("File already exists and is read from the output directory.\n"))
          las_ctg_clip <- readLAScatalog(folder=sprintf('%s\\01_pointcloud_clipped_merged', output_path))
        } else {
          
          # Create a LAS catalog
          las_ctg <- readLAScatalog(las_file_path,  select = "-0")
          
          # Adjust processing options for clipping
            # Output options -> save in separate folder on disk
              # Specify name of output folder
              output_folder_name <- "01_pointcloud_clipped_merged"
                  
              # Create clip folder in output directory
              dir.create(file.path(output_path, output_folder_name))
                  
              # Set output folder for catalog
              opt_output_files(las_ctg) <-
                # Write output of each chunk on disk
                sprintf("%s\\%s\\clipped", output_path, output_folder_name)
                  
            # Compression settings
            opt_laz_compression(las_ctg) <- 
              # Deativate LAZ compression
              FALSE
                    
            # Filter options
            opt_filter(las_ctg) <- sprintf(
              # Only read points within the bb of the study area
              "-keep_xy %f %f %f %f", xmin, ymin, xmax, ymax)
                  
          # Clip LAS catalog to stud_area_buf
          cat("Clipping LAS catalog to study area..\n")
          cat(note("Caution! Depending on the input file size, this may take a while.\n"))
          las_ctg_clip <- clip_roi(las_ctg, study_area_buf)
          cat("Clipped LAS catalog has been written into 'las_ctg_clipped'.\n")
          
          # Calculate number of points
          clip_points <- las_ctg_clip@data[["Number.of.point.records"]]
          
          # View summary of catalog 
          #summary(las_ctg_clip)
        }
        
        
      # PREPROCESSING | STEP 2 | POINT CLOUD VOXELIZATION  -----------------------------
        
        cat(highlight("\n# POINT CLOUD VOXELIZATION #\n\n"))
        
        # Check if older exists
        if(file.exists(sprintf('%s\\02_pointcloud_voxelized', output_path))){
          cat(note("Files already exist and are read from the output directory.\n"))
          las_ctg_voxelized <- readLAScatalog(folder=sprintf('%s\\02_pointcloud_voxelized', output_path))
        } else {
          
          # Adjust processing options for point cloud decimation
            # Output options -> save in separate folder on disk
              # Specify name of output folder
              output_folder_name <- "02_pointcloud_voxelized"
            
              # Create clip folder in output directory
              dir.create(file.path(output_path, output_folder_name))
            
              # Set output folder for catalog
              opt_output_files(las_ctg_clip) <-
                # Write output of each chunk on disk
                sprintf("%s\\%s\\voxelized_chunk_{ID}", output_path, output_folder_name)
            
            # Compression options -> save as LAS
              # Compression settings
              opt_laz_compression(las_ctg_clip) <- 
                # Deativate LAZ compression
                FALSE
            
            # Filter options
            opt_filter(las_ctg_clip) <- sprintf(
              # Only read points within the bb of the study area
              "-keep_xy %f %f %f %f", xmin, ymin, xmax, ymax)
            
            # Select options
            opt_select(las_ctg_clip) <- 
              # Only select x, y, z, Deviation, Reflectance and Amplitude
              "*-t -a -i -s -k -w -o -u -p -e -d -N -C -W"

            # Chunk options
              # Suppress Messages related to chunk size
              suppressMessages({
                # Set chunk size (Def. 0) -> Aim: reduce memory usage
                opt_chunk_size(las_ctg_clip) <- chunk_size
              })
        
              # Align chunks
              opt_chunk_alignment(las_ctg_clip) <- c(xmin, ymin)
              
              # Set buffer distance (Def. 30m) -> Aim: avoid edge effects
              opt_chunk_buffer(las_ctg_clip) <- 0.5
              
              # Plot chunk pattern + buffer distance
              plot(las_ctg_clip, chunk = TRUE)
              
          # Decimate point cloud
            # Run decimation function
            cat("Starting LAS catalog voxelization..\n")
            las_ctg_voxelized <- voxelize_points(las_ctg_clip, res = voxel_size)
            cat("Voxelized LAS catalog has been written into 'las_ctg_voxelized'.\n")
        }
        
        # Remove unused objects
        cat(note("\nGarbage collection: Deleting unused files to free memory.\n"))
        
        rm(las_ctg_clip)
        invisible(gc(verbose = FALSE))
        
      # PREPROCESSING | STEP 3 | MERGE VOXELIZED LAS FILES ---------------------
        
        # Check if older exists
        if(file.exists(sprintf('%s\\02_pointcloud_voxelized_merged', output_path))){
          las_ctg_voxelized_merged <- readLAScatalog(folder=sprintf('%s\\02_pointcloud_voxelized_merged', output_path))
        } else {
          
          # Adjust processing options
            # Output options -> save in separate folder on disk
              # Specify name of output folder
              output_folder_name <- "02_pointcloud_voxelized_merged"
              
              # Create clip folder in output directory
              dir.create(file.path(output_path, output_folder_name))
                      
              # Set output folder for catalog
              opt_output_files(las_ctg_voxelized) <-
                # Write output of each chunk on disk
                sprintf("%s\\%s\\voxelized_merged", output_path, output_folder_name)
                      
            # Compression options -> save as LAS
              # Compression settings
                opt_laz_compression(las_ctg_voxelized) <- 
                # Deativate LAZ compression
                FALSE
                      
            # Filter options
            opt_filter(las_ctg_voxelized) <- ""
                      
            # Select options
            opt_select(las_ctg_voxelized) <- 
              # Only select x, y, z, Deviation, Reflectance,  Amplitude and Classificaiton
              "*-t -a -i -s -k -w -o -u -p -e -d -N -C -W"
            
            # Chunk options
              # Suppress Messages related to chunk size
              suppressMessages({
                # Set chunk size (Def. 0) -> Aim: reduce memory usage
                opt_chunk_size(las_ctg_voxelized) <- longest_side
              })
              
              # Align chunks
              opt_chunk_alignment(las_ctg_voxelized) <- c(xmin, ymin)
                      
              # Set buffer distance (Def. 30m)
              opt_chunk_buffer(las_ctg_voxelized) <- 0
                      
              # Plot chunk pattern + buffer distance
              plot(las_ctg_voxelized, chunk = TRUE)
                      
          # Write merged las file
          cat("Starting to merge chunks..\n")
          las_ctg_voxelized_merged <- catalog_retile(las_ctg_voxelized)   
          cat("Voxelized chunks have been merged into 'las_ctg_voxelized_merged'.\n")
        }
        
        # Calculate number of points
        voxelized_points <- las_ctg_voxelized_merged@data[["Number.of.point.records"]]
        
        # Remove unused objects
        cat(note("\nGarbage collection: Deleting unused files to free memory.\n"))
        
        rm(las_ctg_voxelized)
        invisible(gc(verbose = FALSE))
        
        
      # PREPROCESSING | STEP 2 | FILTER POINTCLOUD ------------------------------
        
        cat(highlight("\n# POINT CLOUD FILTERING #\n\n"))
        
        # Check if older exists
        if(file.exists(sprintf('%s\\02_pointcloud_filtered', output_path))){
          cat(note("Files already exist and are read from the output directory.\n"))
          las_ctg_filtered <- readLAScatalog(folder=sprintf('%s\\02_pointcloud_filtered', output_path))
        } else {
          
          cat(note("Checking for 'Reflectance' and 'Deviation' attribute..\n"))
          
          # Reading LAS header
          output_folder_name <- "02_pointcloud_voxelized_merged"
          header <- readLASheader(sprintf("%s\\%s\\voxelized_merged.las", output_path, output_folder_name))
          
          # Initialize variables
          reflectance_attribute <- FALSE
          deviation_attribute <- FALSE
          
          # Check for Extra Bytes Description
          if("Reflectance" %in% names(header@VLR$Extra_Bytes$`Extra Bytes Description`)){
            reflectance_attribute <<- TRUE
          }
          
          if("Deviation" %in% names(header@VLR$Extra_Bytes$`Extra Bytes Description`)){
            deviation_attribute <- TRUE
          }
        }
        

        # If attributes are present, filter by deviation and reflectance
        if(reflectance_attribute || deviation_attribute){
          # Adjust processing options for pointcloud filtering
            # Output options -> save in separate folder on disk
              # Specify name of output folder
              output_folder_name <- "02_pointcloud_filtered"
              
              # Create clip folder in output directory
              dir.create(file.path(output_path, output_folder_name))
              
              # Set output folder for catalog
              opt_output_files(las_ctg_voxelized_merged) <-
                # Write output of each chunk on disk
                sprintf("%s\\%s\\filtered_chunk_{ID}", output_path, output_folder_name)
              
              # Compression settings
              opt_laz_compression(las_ctg_voxelized_merged) <- 
                # Deativate LAZ compression
                FALSE
          
            # Filter options
            opt_filter(las_ctg_voxelized_merged) <- sprintf(
              # Only read points within the bb of the study area
              "-keep_xy %f %f %f %f", xmin, ymin, xmax, ymax)
            
            # Select options
            opt_select(las_ctg_voxelized_merged) <- 
              # Only select x, y, z, Deviation, Reflectance and Amplitude
              "*-t -a -i -s -k -w -o -u -p -e -d -N -C -W"
            
            # Chunk options
              # Suppress Messages related to chunk size
              suppressMessages({
                # Set chunk size (Def. 0) -> Aim: reduce memory usage
                opt_chunk_size(las_ctg_voxelized_merged) <- chunk_size
              })
              
              # Align chunks
              opt_chunk_alignment(las_ctg_voxelized_merged) <- c(xmin, ymin)
              
              # Set buffer distance (Def. 30m)
              opt_chunk_buffer(las_ctg_voxelized_merged) <- 0
              
              # Plot chunk pattern + buffer distance
              plot(las_ctg_voxelized_merged, chunk = TRUE)
        
          # Define function for filtering the reflection and deviation
          filter_reflectance_deviation <- function(chunk) { 
            las <- readLAS(chunk)
            if (lidR:::is.empty(las)) {
              return(NULL)
            }
                  
            # Filter based on reflectance if applicable
            if (reflectance_attribute) {
              cat("Filtering by 'Reflectance'..\n")
              las <- las[las$Reflectance <= 0, ]
            } else {
              cat(note("'Reflectance' attribute not found. Skipping filtering by reflectance..\n"))
            }
                  
            # Filter based on deviation if applicable
            if (deviation_attribute) {
              cat("Filtering by 'Deviation'..\n")
              las <- las[las$Deviation <= 20, ]
            } else {
              cat(("'Deviation' attribute not found. Skipping filtering by deviation..\n"))
            }
                
            return(las)
          }
                
          # Set options for own function
          opt <- list(automerge = TRUE)

          # Apply function
          las_ctg_filtered <- catalog_apply(las_ctg_voxelized_merged, filter_reflectance_deviation, .options = opt)

    } else {
      cat("Neither 'Reflectance' nor 'Deviation' attribute found.\n")
      cat(note("Skipping point cloud filtering.\n"))
    }
            
    # PREPROCESSING | STEP 2.1 | MERGE FILTERED LAS FILES ---------------------
        
    if(!reflectance_attribute && !deviation_attribute){
      cat(note("Skipping merging filtered chunks.\n"))
    } else {
          
      # Check if older exists
      if(file.exists(sprintf('%s\\02_pointcloud_filtered_merged', output_path))){
        las_ctg_filtered <- readLAScatalog(folder=sprintf('%s\\02_pointcloud_filtered_merged', output_path))
      } else {
            
        # Adjust processing options
          # Output options -> save in separate folder on disk
            # Specify name of output folder
            output_folder_name <- "02_pointcloud_filtered_merged"
                
            # Create clip folder in output directory
            dir.create(file.path(output_path, output_folder_name))
              
            # Set output folder for catalog
            opt_output_files(las_ctg_filtered) <-
              # Write output of each chunk on disk
              sprintf("%s\\%s\\filtered_merged", output_path, output_folder_name)
                
            # Compression settings
            opt_laz_compression(las_ctg_filtered) <- 
              # Deativate LAZ compression
              FALSE
              
            # Filter options
            opt_filter(las_ctg_filtered) <- ""
            
            # Select options
            opt_select(las_ctg_filtered) <- 
              # Only select x, y, z, Deviation, Reflectance,  Amplitude and Classificaiton
              "*-t -a -i -s -k -w -o -u -p -e -d -N -C -W"
            
          # Chunk options
            # Suppress Messages related to chunk size
            suppressMessages({
              # Set chunk size (Def. 0) -> Aim: reduce memory usage
              opt_chunk_size(las_ctg_filtered) <- longest_side
            })
                
            # Align chunks
            opt_chunk_alignment(las_ctg_filtered) <- c(xmin, ymin)
                
            # Set buffer distance (Def. 30m)
            opt_chunk_buffer(las_ctg_filtered) <- 0
                
            # Plot chunk pattern + buffer distance
            plot(las_ctg_filtered, chunk = TRUE)
              
        # Write merged las file
        cat("Starting to merge chunks..\n")
        las_ctg_filtered_merged <- catalog_retile(las_ctg_filtered)    
        cat("Filtered point cloud chunks have been merged into 'las_ctg_filtered_merged'.\n")
      }
    }
        
      
      # PREPROCESSING | STEP 4 | POINTCLOUD DENOISING ---------------------------
        
        cat(highlight("\n# POINT CLOUD DENOISING #\n\n"))
        
        # Check if older exists
        if(file.exists(sprintf('%s\\03_pointcloud_denoised', output_path))){
          cat(note("Files already exist and are read from the output directory.\n"))
          las_ctg_denoised <- readLAScatalog(folder=sprintf('%s\\03_pointcloud_denoised', output_path))
        } else {
          
          # Assume catalog is initially NULL
          catalog <- NULL
          
          if (exists("las_ctg_filtered_merged")) {
            catalog <- las_ctg_filtered_merged
            cat("Using 'las_ctg_filtered_merged' for denoising.\n")
          } else {
            catalog <- las_ctg_voxelized_merged
            cat("Using 'las_ctg_voxelized_merged' for denoising.\n")
          }
          
          # Adjust processing options for pointcloud denoising
            # Output options -> save in separate folder on disk
              # Specify name of output folder
              output_folder_name <- "03_pointcloud_denoised"
        
              # Create clip folder in output directory
              dir.create(file.path(output_path, output_folder_name))
        
              # Set output folder for catalog
              opt_output_files(catalog) <-
                # Write output of each chunk on disk
                sprintf("%s\\%s\\denoised_chunk_{ID}", output_path, output_folder_name)
        
            # Compression settings
              opt_laz_compression(catalog) <- 
                # Deativate LAZ compression
                FALSE
        
            # Filter options
            opt_filter(catalog) <- sprintf(
            # Only read points within the bb of the study area
            "-keep_xy %f %f %f %f", xmin, ymin, xmax, ymax)
        
            # Select options
            opt_select(catalog) <- 
              # Only select x, y, z, Deviation, Reflectance and Amplitude
              "*-t -a -i -s -k -w -o -u -p -e -d -N -C -W"
            
            # Chunk options
              # Suppress Messages related to chunk size
              suppressMessages({
                # Set chunk size (Def. 0) -> Aim: reduce memory usage
                opt_chunk_size(catalog) <- chunk_size
              })
              
              # Align chunks
              opt_chunk_alignment(catalog) <- c(xmin, ymin)
        
              # Set buffer distance (Def. 30m)
              opt_chunk_buffer(catalog) <- 0
        
              # Plot chunk pattern + buffer distance
              plot(catalog, chunk = TRUE)
        
          # Filter out points (Low-level API)
            # Define function for denoising
            filter_noise <- function(chunk) { 
              las <- readLAS(chunk)
                if (lidR:::is.empty(las)) return(NULL)
                  
                # Classify noise
                noise <- classify_noise(las, ivf(res=0.25, n=100))
                if (lidR:::is.empty(noise)) return(NULL)
                    
                # Drop noise points
                denoised <- noise[noise$Classification != 18]
                if (lidR:::is.empty(denoised)) return(NULL)
                
              return(denoised)
            }
                
            # Set options for own function
            opt <- list(automerge = TRUE, autocrop = TRUE)
                
          # Apply function to catalog
          cat("Starting LAS catalog denoising..\n")
          las_ctg_denoised <- catalog_apply(catalog, filter_noise, .options = opt)
          cat("Denoised LAS catalog has been written into 'las_ctg_denoised'.\n")
        }
        
        # Remove unused objects
        cat(note("\nGarbage collection: Deleting unused files to free memory.\n"))
        
        rm(las_ctg_voxelized_merged)
        invisible(gc(verbose = FALSE))
        
      
      # PREPROCESSING | STEP 5 | MERGE DENOISED LAS FILES ---------------------
        
        # Check if older exists
        if(file.exists(sprintf('%s\\03_pointcloud_denoised_merged', output_path))){
          las_ctg_denoised_merged <- readLAScatalog(folder=sprintf('%s\\03_pointcloud_denoised_merged', output_path))
        } else {
          
          # Adjust processing options
            # Output options -> save in separate folder on disk
              # Specify name of output folder
              output_folder_name <- "03_pointcloud_denoised_merged"
              
              # Create clip folder in output directory
              dir.create(file.path(output_path, output_folder_name))
            
              # Set output folder for catalog
              opt_output_files(las_ctg_denoised) <-
                # Write output of each chunk on disk
                sprintf("%s\\%s\\denoised_merged", output_path, output_folder_name)
            
            # Compression options -> save as LAS
              # Compression settings
              opt_laz_compression(las_ctg_denoised) <- 
                # Deativate LAZ compression
                FALSE
            
            # Filter options
            opt_filter(las_ctg_denoised) <- ""
            
            # Select options
            opt_select(las_ctg_denoised) <- 
              # Only select x, y, z, Deviation, Reflectance,  Amplitude and Classificaiton
              "*-t -a -i -s -k -w -o -u -p -e -d -N -C -W"
            
            # Chunk options
              # Suppress Messages related to chunk size
              suppressMessages({
                # Set chunk size (Def. 0) -> Aim: reduce memory usage
                opt_chunk_size(las_ctg_denoised) <- longest_side
              })
              
              # Align chunks
              opt_chunk_alignment(las_ctg_denoised) <- c(xmin, ymin)
            
              # Set buffer distance (Def. 30m)
              opt_chunk_buffer(las_ctg_denoised) <- 0
              
              # Plot chunk pattern + buffer distance
              plot(las_ctg_denoised, chunk = TRUE)
            
          # Write merged las file
          cat("Starting to merge chunks..\n") 
          las_ctg_denoised_merged <- catalog_retile(las_ctg_denoised)
          cat("Denoised chunks have been merged into 'las_ctg_denoised_merged'.\n")
        }
        
        # Calculate number of points
        denoised_points <- las_ctg_denoised_merged@data[["Number.of.point.records"]]
        
        # Remove unused objects
        cat(note("\nGarbage collection: Deleting unused files to free memory.\n"))
        
        rm(las_ctg_denoised_merged)
        invisible(gc(verbose = FALSE))
        
        
      # PROCESSING | STEP 1 | VEGETATION FILTERING (CSF) ------------------------------
        
        cat(highlight("\n# CSF VEGETATION FILTERING #\n\n"))
        
        # Check if older exists
        if(file.exists(sprintf('%s\\04_pointcloud_classified', output_path))){
          cat(note("Files already exist and are read from the output directory.\n"))
          las_ctg_classified <- readLAScatalog(folder=sprintf('%s\\04_pointcloud_classified', output_path))
        } else {
          
          # Adjust processing options for point classification
            # Output options -> save in separate folder on disk
              # Specify name of output folder
              output_folder_name <- "04_pointcloud_classified"
              
              # Create clip folder in output directory
              dir.create(file.path(output_path, output_folder_name))
              
              # Set output folder for catalog
              opt_output_files(las_ctg_denoised) <-
                # Write output of each chunk on disk
                sprintf("%s\\%s\\classified_chunk_{ID}", output_path, output_folder_name)
              
              # Compression options -> save as LAS
                # Compression settings
                opt_laz_compression(las_ctg_denoised) <- 
                  # Deativate LAZ compression
                  FALSE
              
            # Filter options
            opt_filter(las_ctg_denoised) <- ""
              
            # Select options
            opt_select(las_ctg_denoised) <- 
              # Only select x, y, z, Deviation, Reflectance and Amplitude
              "*-t -a -i -s -k -w -o -u -p -e -d -N -C -W"
            
            # Chunk options
              # Suppress Messages related to chunk size
              suppressMessages({
                # Set chunk size (Def. 0) -> Aim: reduce memory usage
                opt_chunk_size(las_ctg_denoised) <- chunk_size
              })
              
              # Align chunks
              opt_chunk_alignment(las_ctg_denoised) <- c(xmin, ymin)
              
              # Set buffer distance (Def. 30m)
              opt_chunk_buffer(las_ctg_denoised) <- 0.5
            
              # Plot chunk pattern + buffer distance
              plot(las_ctg_denoised, chunk = TRUE)
            
          # Classifying point cloud
            # Define csf function
            mycsf <- csf(
              sloop_smooth = post_processing,
              class_threshold = class_threshold,
              cloth_resolution = cloth_resolution,
              rigidness = cloth_rigidness,
              iterations = 500L, 
              time_step = 0.65
            )
            
            point_classification_csf <- function(chunk) { 
              las <- readLAS(chunk)
                if (lidR:::is.empty(las)) return(NULL)
                  
                # Classify points of chunk
                las_classified_csf <- classify_ground(las, mycsf, last_returns = FALSE)
                las_classified_csf <- filter_poi(las_classified_csf, buffer == 0)
                if (lidR:::is.empty(las_classified_csf)) return(NULL)
                  
                return(las_classified_csf)
              }    
                
            # Set function options
            opt <- list(automerge = TRUE, need_buffer = TRUE, autocrop = TRUE)
                
            # Apply function to catalog
            cat("Starting to classify point cloud using CSF..\n") 
            las_ctg_classified <- catalog_apply(las_ctg_denoised, point_classification_csf, .options = opt)
            cat("Classified LAS catalog has been written into 'las_ctg_classified'.\n")
        }
        
        # Remove unused objects
        cat(note("\nGarbage collection: Deleting unused files to free memory.\n"))
        
        rm(las_ctg_denoised)
        invisible(gc(verbose = FALSE))
        
              
      # PROCESSING | STEP 2 | CSF MERGE CHUNKS --------------------------------
        
        # Check if older exists
        if(file.exists(sprintf('%s\\04_pointcloud_classified_merged', output_path))){
          las_ctg_classified_merged <- readLAScatalog(folder=sprintf('%s\\04_pointcloud_classified_merged', output_path))
        } else {
          
          # Adjust processing options
            # Output options -> save in separate folder on disk
              # Specify name of output folder
              output_folder_name <- "04_pointcloud_classified_merged"
              
              # Create clip folder in output directory
              dir.create(file.path(output_path, output_folder_name))
                  
              # Set output folder for catalog
              opt_output_files(las_ctg_classified) <-
                # Write output of each chunk on disk
                sprintf("%s\\%s\\classified_csf_merged", output_path, output_folder_name)
                  
              # Compression options -> save as LAS
                # Compression settings
                opt_laz_compression(las_ctg_classified) <- 
                  # Deativate LAZ compression
                  FALSE
                  
            # Filter options
            opt_filter(las_ctg_classified) <- ""
                  
            # Select options
            opt_select(las_ctg_classified) <- 
              # Only select x, y, z, Deviation, Reflectance,  Amplitude and Classification
              "*-t -a -i -s -k -w -o -u -p -e -d -N -C -W"
            
            # Chunk options
              # Suppress Messages related to chunk size
              suppressMessages({
                # Set chunk size (Def. 0) -> Aim: reduce memory usage
                opt_chunk_size(las_ctg_classified) <- longest_side
              })
              # Align chunks
              opt_chunk_alignment(las_ctg_classified) <- c(xmin, ymin)
                  
              # Set buffer distance (Def. 30m)
              opt_chunk_buffer(las_ctg_classified) <- 0
                  
              # Plot chunk pattern + buffer distance
              plot(las_ctg_classified, chunk = TRUE)
                  
          # Write merged las file
          cat("Starting to merge chunks..\n") 
          las_ctg_classified_merged <- catalog_retile(las_ctg_classified)
          cat("Classified chunks have been merged into 'las_ctg_classified_merged'.\n")
        }
        
        # Remove unused objects
        cat(note("\nGarbage collection: Deleting unused files to free memory.\n"))
        
        rm(las_ctg_classified)
        invisible(gc(verbose = FALSE))
        
      
      # PROCESSING | STEP 3 | CSF EXTRACT GROUND --------------------------------
        
        cat(highlight("\n# GROUND EXTRACTION #\n\n"))
        
        # Check if older exists
        if(file.exists(sprintf('%s\\05_pointcloud_ground_merged', output_path))){
          cat(note("Files already exist and are read from the output directory.\n"))
          las_ctg_ground_merged <- readLAScatalog(folder=sprintf('%s\\05_pointcloud_ground_merged', output_path))
        } else {
          
          # Adjust processing options
            # Output options -> save in separate folder on disk
              # Specify name of output folder
              output_folder_name <- "05_pointcloud_ground_merged"
              
              # Create clip folder in output directory
              dir.create(file.path(output_path, output_folder_name))
              
              # Set output folder for catalog
              opt_output_files(las_ctg_classified_merged) <-
                # Write output of each chunk on disk
                sprintf("%s\\%s\\ground_merged", output_path, output_folder_name)
              
            # Compression options -> save as LAS
              # Compression settings
              opt_laz_compression(las_ctg_classified_merged) <- 
                # Deativate LAZ compression
                FALSE
          
            # Filter options
            opt_filter(las_ctg_classified_merged) <- ""
            
            # Select options
            opt_select(las_ctg_classified_merged) <- 
              # Only select x, y, z, Deviation, Reflectance,  Amplitude and Classification
              "*-t -a -i -s -k -w -o -u -p -e -d -N -C -W"
            
            # Chunk options
              # Suppress Messages related to chunk size
              suppressMessages({
                # Set chunk size (Def. 0) -> Aim: reduce memory usage
                opt_chunk_size(las_ctg_classified_merged) <- longest_side
              })
              
              # Align chunks
              opt_chunk_alignment(las_ctg_classified_merged) <- c(xmin, ymin)
              
              # Set buffer distance (Def. 30m)
              opt_chunk_buffer(las_ctg_classified_merged) <- 0
              
              # Plot chunk pattern + buffer distance
              plot(las_ctg_classified_merged, chunk = TRUE)
          
          # Extract ground
            # Define function (Low-level API)
            ground_extraction <- function(chunk) { 
              las <- readLAS(chunk)
              if (lidR:::is.empty(las)) return(NULL)
              
              # Extract ground
              ground <- las[las$Classification == 2]
              if (lidR:::is.empty(ground)) return(NULL)
              
              return(ground)
            }    
          
          # Set function options
          opt <- list(automerge = TRUE, need_buffer = TRUE)
          
          # Apply function to catalog
          cat("Starting to extract ground..\n") 
          las_ctg_ground_merged <- catalog_apply(las_ctg_classified_merged, ground_extraction, .options = opt)
          cat("Ground chunks have been merged into 'las_ctg_ground_merged'.\n")
        }
          
        
        # PROCESSING | STEP 4 | TREE DETECTION ------------------------------------
        
        cat(highlight("\n# TREE DETECTION #\n\n"))
        cat("Reading classified las dataset..\n")

        # Read las_ctg_classified
        las_ctg_classified_merged <- readLAScatalog(folder=sprintf('%s\\04_pointcloud_classified_merged', output_path))
        
        # Adjust processing options
          # Select options
          opt_select(las_ctg_classified_merged) <- 
            # Only select x, y, z, Deviation, Reflectance,  Amplitude and Classification
            "*-t -a -i -s -k -w -o -u -p -e -d -N -C -W"
          
          # Chunk options
            # Suppress Messages related to chunk size
            suppressMessages({
              # Set chunk size (Def. 0) -> Aim: reduce memory usage
              opt_chunk_size(las_ctg_classified_merged) <- chunk_size
            })
            
            # Align chunks
            opt_chunk_alignment(las_ctg_classified_merged) <- c(xmin, ymin)
            
            # Set buffer distance (Def. 30m)
            opt_chunk_buffer(las_ctg_classified_merged) <- 1
            
            # Plot chunk pattern + buffer distance
            plot(las_ctg_classified_merged, chunk = TRUE)
        
        # Treetop detection function
        detect_ttops <- function(chunk) { 
          las <- readLAS(chunk)
          if (lidR:::is.empty(las)) {
            return(NULL)
          }
          
          # Calculate chunk bounding box
          chunk_bb <- st_bbox(chunk)
          xmin <- as.numeric(chunk_bb[1])
          ymin <- as.numeric(chunk_bb[2])
          xmax <- as.numeric(chunk_bb[3])
          ymax <- as.numeric(chunk_bb[4])
          
          # Normalize chunk and remove points lower than Z = 0
          las_normalized <- normalize_height(las, algorithm = tin())
          las_normalized <- las_normalized[las_normalized$Z >= 0, ]
          
          # Extract the tree map
#          las_trees = treeMap(las_normalized, map.hough(min_density = 0.1), 0)
          las_trees = tryCatch({
            treeMap(las_normalized, map.hough(min_density = 0.1), 0)
          }, error = function(e) {
            return(NULL) # Skip this chunk if no trees are detected
          })
          
          # Check if any trees were detected
          if (is.null(las_trees) || nrow(las_trees@data) == 0) {
            cat(note("\nNo trees detected in this chunk. Moving to the next chunk.\n"))
            return(NULL)
          }
          
          # Classify tree regions
          las_tree_regions = treePoints(las_normalized, las_trees, trp.crop(circle = FALSE))
          
          # Classify stem points
          las_tree_stems = suppressMessages(stemPoints(las_tree_regions, stm.hough()))
          
          # Initialize lists to store radius and error values from each iteration
          radius_list <- list()
          error_list <- list()
          
          # Number of iterations
          n_iterations <- 0
          complete <- FALSE
          
          # Loop for radius and error calculation
          while (!complete && n_iterations < 25) {
            n_iterations <- n_iterations + 1
            tree_stems_inventory <- tlsInventory(
              las_tree_stems,
              dh = 0.75,
              dw = 0.5,
              hp = 1,
              d_method = shapeFit(shape = "circle", algorithm = "ransac", n = 15, n_best = 20)
            )
            radius_list[[n_iterations]] <- tree_stems_inventory$Radius
            error_list[[n_iterations]] <- tree_stems_inventory$Error
            
            radius_df <- do.call(cbind, radius_list)
            error_df <- do.call(cbind, error_list)
            
            complete_radius <- apply(radius_df, 1, function(x) sum(!is.na(x)) >= 3)
            complete_error <- apply(error_df, 1, function(x) sum(!is.na(x)) >= 3)
            complete <- all(complete_radius & complete_error)
          }
          
          if (n_iterations == 25) {
            cat(note("\nMax iterations reached before all TreeIDs had 3 valid radii!\n"))
            cat("\nUsing max. tree radius for TreeIDs with NaN values.\n")
          } else {
            cat("\nTree detection completed with sufficient values.\n")
          }
          
          mean_radius <- rowMeans(radius_df, na.rm = TRUE)
          mean_error <- rowMeans(error_df, na.rm = TRUE)
          
          # Create current_ttop as a data frame
          current_ttop <- data.frame(
            X = tree_stems_inventory$X,
            Y = tree_stems_inventory$Y,
            Mean_Radius = mean_radius,
            Mean_Error = mean_error,
            Total_Iterations = n_iterations,
            stringsAsFactors = FALSE
          )
          
          # Filter out trees in buffer
          current_ttop <- current_ttop[
            current_ttop$X > xmin & current_ttop$X < xmax &
              current_ttop$Y > ymin & current_ttop$Y < ymax, 
          ]
          
          # Return the current_ttop dataframe
          return(current_ttop)
        }
        
        # Run catalog_apply and collect results
        results <- catalog_apply(las_ctg_classified_merged, detect_ttops)
        
        # Combine results into final ttops dataframe
        ttops <- do.call(rbind, results)
        row.names(ttops) <- NULL
        
        # Eliminate NaN values for Mean_Radius
        # Calculate the maximum valid mean radius (ignoring NaN values)
        max_mean_radius <- max(ttops$Mean_Radius, na.rm = TRUE)
        
        # Replace NaN values with the max_mean_radius
        ttops$Mean_Radius[is.nan(ttops$Mean_Radius)] <- max_mean_radius
        
        # State how many NaN values were replaced
        cat("Number of NaN values replaced:", sum(is.nan(ttops$Mean_Radius)), "\n")
        
        # If you want to reset row names after all processing
        row.names(ttops) <- NULL
        
        # Remove unused objects
        cat(note("\nGarbage collection: Deleting unused files to free memory.\n"))
        
        rm(las_ctg_classified_merged)
        invisible(gc(verbose = FALSE))
        
        
        # PROCESSING | STEP 4.1 | TREE CLIPPING ---------------------------------

          # Output options
            # Create new output folder
            output_folder_name <- "10_pointcloud_trees"
            # Create clip folder in output directory
            dir.create(file.path(output_path, output_folder_name))

            # Create the subfolder 'trees_clipped' inside '10_pointcloud_trees'
            subfolder_name <- "trees_clipped_csf"
            dir.create(file.path(output_path, output_folder_name, subfolder_name))
            
          # Clip treetops from clipped.merged.las
          las_classified_merged <- readLAS(sprintf('%s\\04_pointcloud_classified_merged\\classified_csf_merged.las', output_path))
          
          # Setup progress bar
          pb <- progress_bar$new(total = nrow(ttops), format = "Processing [:bar] :percent :current/:total eta: :eta")
          
          # Loop over ttops and clip them from clipped.las
          cat("Starting to clip trees..\n")
          
          
          for (i in 1:nrow(ttops)) {
            # Extract the XY coordinates from the i-th row of ttops
            x <- ttops$X[i]
            y <- ttops$Y[i]
            
            # Create a point object for clipping
            current_ttop <- st_as_sf(st_sfc(st_point(c(x, y))))  
            
            # Clip the LAS file to the current treetop
            ttop_clipped <- suppressWarnings(clip_roi(las_classified_merged, current_ttop, radius = 2))
            
            # Check if clipped point cloud is empty
            if (lidR:::is.empty(ttop_clipped)){
              cat(note("\nNo ground points found within cutout. Proceeding with next tree top.\n"))
              next
            }
              
            # Write clipped LAS file to disk
            writeLAS(ttop_clipped, file = sprintf("%s\\10_pointcloud_trees\\trees_clipped_csf\\ttop_csf_%d.las", output_path, i))
              
            # Update progress bar
            pb$tick()
          }
          
          # Remove unused objects
          cat(note("\nGarbage collection: Deleting unused files to free memory.\n"))
          
          rm(las_classified_merged)
          invisible(gc(verbose = FALSE))
          
        
        # PROCESSING | STEP 5 | GROUND ESTIMATION ------------------------------------------
        
        cat(highlight("\n# SURFACE ESTIMATION FOR TREE STEM LOCATIONS #\n\n"))     
        
        # Read LAS file
        las_ground_merged <- readLAS(sprintf("%s\\05_pointcloud_ground_merged\\ground_merged.las", output_path))
#        las_ground_merged <- readLAS("F:/SfM_Plot_South/output/05_pointcloud_ground_merged/ground_merged.las", select = "*-t -a -i -s -k -w -o -u -e -d -N -C -W")
        
        # Extract scale and offset factors
        x_scale <- las_ground_merged@header@PHB[["X scale factor"]]
        y_scale <- las_ground_merged@header@PHB[["Y scale factor"]]
        z_scale <- las_ground_merged@header@PHB[["Z scale factor"]]
        
        x_offset <- las_ground_merged@header@PHB[["X offset"]]
        y_offset <- las_ground_merged@header@PHB[["Y offset"]]
        z_offset <- las_ground_merged@header@PHB[["Z offset"]]
        
        # Initialize an empty dataframe to store points that have to be deleted
        las_delete <- data.frame(X=numeric(), Y=numeric(), Z=numeric(), R=numeric(), G=numeric(), B=numeric(), Distance=numeric(), Classification=integer())
        
        # Initialize an empty dataframe to store points that have to be inserted
        las_insert <- data.frame(X=numeric(), Y=numeric(), Z=numeric(), R=numeric(), G=numeric(), B=numeric(), Distance=numeric(), Classification=integer(), PointSourceID=integer())
        
        # Setup progress bar
        pb <- progress_bar$new(total = nrow(ttops), format = "Processing [:bar] :percent :current/:total eta: :eta")
        
        # Loop over ttops, clip classified LAS and smooth clipped LAS file
        cat("Predicting surface at tree locations..\n")
        
        for (i in 1:nrow(ttops)) {
          # Extract the XY coordinates from the i-th row of ttops
          x <- ttops$X[i]
          y <- ttops$Y[i]
          stem_radius <- ttops$Mean_Radius[i]
          
          # Create a point object for clipping
          current_ttop <- st_as_sf(st_sfc(st_point(c(x, y))))  
          
          # Clip the LAS file to the current treetop
          ttop_clipped <- suppressWarnings(clip_roi(las_ground_merged, current_ttop, radius = stem_radius*6))
          
          # Check if clipped point cloud is empty
          if (lidR:::is.empty(ttop_clipped)){
            cat(note("\nNo ground points found for prediction. Proceeding with next tree top.\n"))
            next
          } 
          
          # Calculate the center of the clipped LAS
          center_x <- (max(ttop_clipped$X) + min(ttop_clipped$X)) / 2
          center_y <- (max(ttop_clipped$Y) + min(ttop_clipped$Y)) / 2
          
          # Calculate Distances to the center
          ttop_clipped@data$Distance <- sqrt((ttop_clipped@data$X - center_x)^2 + (ttop_clipped@data$Y - center_y)^2)
          ttop_clipped@data$Distance <- round(ttop_clipped@data$Distance, digits = 2)
          
          # Calculate max Distance to the center
          max_Distance <- max(ttop_clipped@data$Distance)
          
          # ClipLAS points used for prediction
          las_predict <- ttop_clipped[ttop_clipped@data$Distance > stem_radius*4]
          
          # Write artefact points into own df
          las_replace <- ttop_clipped[ttop_clipped@data$Distance <= stem_radius*4]
          las_replace <- las_replace@data
          las_delete <- rbind(las_delete, las_replace)
          
          # Convert las_predict into df
          las_predict_df <- las_predict@data[, c("X", "Y", "Z")]
          
          if (nrow(las_predict_df) == 0) {
            cat(note("\nInsufficient data for GAM modeling.\n"))
            cat("\nSkipping TreeID and moving on to the next TreeID.\n")
          } else {
            
            # Write X, Y, Z into its own variables
            x <- las_predict@data$X
            y <- las_predict@data$Y
            z <- las_predict@data$Z
            
            # Try fitting the model and catching any errors
            fit <- tryCatch({
              gam(z ~ s(x, y), data = las_predict_df)
            }, error = function(e) {
              cat(note("\nInsufficient prediction data for GAM modeling for TreeID: "), i, ". Skipping this tree.\n")
              return(NULL)  # Return NULL if there's an error
            })
            
            # Check if the fit was successful
            if (is.null(fit)) {
              next  # Skip to the next iteration if the model fitting failed
            }
            
            # Predict values on regular XY grid
            x_pred <- seq(min(x), max(x), by = voxel_size)
            y_pred <- seq(min(y), max(y), by = voxel_size)
            xy <- expand.grid(x = x_pred, y = y_pred)
            
            # Calculate the number of rows and columns for the matrix based on the lengths of x_pred and y_pred
            nrow_pred <- length(y_pred)
            ncol_pred <- length(x_pred)
            
            z_pred <- matrix(predict(fit, newdata = xy), 
                             nrow = nrow_pred, ncol = ncol_pred)
            
            # Combine x, y, and predicted z into a single data frame
            grid_points <- data.frame(
              X = xy$x,
              Y = xy$y,
              Z = as.vector(z_pred),
              R = as.integer(255*256),
              G = as.integer(255*256),
              B = as.integer(255*256),
              Distance = NA,
              Classification = as.integer(2),
              PointSourceID = as.integer(1)
            )
            
            # Calculate Distances of predicted points to the center
            grid_points$Distance <- sqrt((grid_points$X - center_x)^2 + (grid_points$Y - center_y)^2)
            grid_points$Distance <- round(grid_points$Distance, digits = 2)
            
            # Only select predicted points with a Distance <= 0.6
            grid_points <- grid_points[grid_points$Distance <= (stem_radius*4), ]
            
            # Append las_insert with grid_points
            las_insert <- rbind(las_insert, grid_points)
            
            # Update progress bar
            pb$tick()
          }
        }
        
        # Write points from las_ground_merged into df
        cat("\nWriting estimated points into LAS file..\n")
        
        # Write data from las_ground_merged into las_points
        las_points <- las_ground_merged@data[, .(X, Y, Z,
                                                 R = as.integer(R), 
                                                 G = as.integer(G),
                                                 B = as.integer(B),
                                                 Classification = as.integer(Classification)),
                                                 PointSourceID]
        
        # Create subset from las_insert
        las_insert <- las_insert[c("X", "Y", "Z", "R", "G", "B", "Classification", "PointSourceID")]

        # Delete las_delete from las_points
        las_points <- anti_join(las_points, las_delete, by = c("X", "Y"))
        
        # Write las_points and las_insert into las_ground_merged
        las_ground_merged@data <- rbind(las_points, las_insert)
        
        # Rescale and reoffset
        cat("Rescaling and reoffsetting LAS file..\n")
        las_ground_merged <- las_rescale(las_ground_merged, x_scale, y_scale, z_scale)
        las_ground_merged <- las_reoffset(las_ground_merged, x_offset, y_offset, z_offset)
        
        # Calculate number of points
        ground_points <- las_ground_merged@header@PHB[["Number of point records"]]
        
#        plot(las_ground_merged)
        
        
        # PROCESSING | STEP 5.1 | TREE CLIPPING ---------------------------------
        
          # Create the subfolder 'trees_clipped_gam' inside '10_pointcloud_trees'
          subfolder_name <- "trees_clipped_gam"
          dir.create(file.path(output_path, output_folder_name, subfolder_name))
          
          # Clip treetops from las_ground_merged
            # Setup progress bar
            pb <- progress_bar$new(total = nrow(ttops), format = "Processing [:bar] :percent :current/:total eta: :eta")
            
            # Loop over ttops and clip them from las_ground_merged
            cat("Starting to clip trees..\n")
            
            for (i in 1:nrow(ttops)) {
              # Extract the XY coordinates from the i-th row of ttops
              x <- ttops$X[i]
              y <- ttops$Y[i]
              
              # Create a point object for clipping
              current_ttop <- st_as_sf(st_sfc(st_point(c(x, y))))  
              
              # Clip the LAS file to the current treetop
              ttop_clipped <- suppressWarnings(clip_roi(las_ground_merged, current_ttop, radius = 2))
              
              # Check if clipped point cloud is empty
              if (lidR:::is.empty(ttop_clipped)){
                cat(note("\nNo ground points found within cutout. Proceeding with next tree top.\n"))
                next
              } 
              
              # Write clipped LAS file to disk
              writeLAS(ttop_clipped, file = sprintf("%s\\10_pointcloud_trees\\trees_clipped_gam\\ttop_gam_%d.las", output_path, i))
              
              # Update progress bar
              pb$tick()
            }
            
        
        # PROCESSING | STEP 5.2 | POINT CLOUD SMOOTHING ---------------------------------
          
          cat(highlight("\n# POINT CLOUD SMOOTHING #\n\n"))
          cat("Starting point cloud smoothing..\n")
          cat(note("Caution! Depending on the chosen window size, this may take a while.\n"))
          
          # Smooth terrain data
#          las_ground_merged_smooth <- smooth_height(las_ground_merged, "average", size = smoothing_window)
          las_ground_merged_smooth <- smooth_height(las_ground_merged, size = smoothing_window, shape = "circle", method = "gaussian")
          
#          plot(las_ground_merged_smooth)  
            
        # PROCESSING | STEP 5.3 | TREE CLIPPING ---------------------------------
            
          # Create the subfolder 'trees_clipped_gam_smoothed' inside '10_pointcloud_trees'
          subfolder_name <- "trees_clipped_gam_smoothed"
          dir.create(file.path(output_path, output_folder_name, subfolder_name))
            
          # Clip treetops from las_ground_merged_smooth
            # Setup progress bar
            pb <- progress_bar$new(total = nrow(ttops), format = "Processing [:bar] :percent :current/:total eta: :eta")
            
            # Loop over ttops and clip them from las_ground_merged_smooth
            cat("Starting to clip trees..\n")
            
            for (i in 1:nrow(ttops)) {
              # Extract the XY coordinates from the i-th row of ttops
              x <- ttops$X[i]
              y <- ttops$Y[i]
              
              # Create a point object for clipping
              current_ttop <- st_as_sf(st_sfc(st_point(c(x, y))))
              
              # Clip the LAS file to the current treetop
              ttop_clipped <- suppressWarnings(clip_roi(las_ground_merged_smooth, current_ttop, radius = 2))
              
              # Check if clipped point cloud is empty
              if (lidR:::is.empty(ttop_clipped)){
                cat(note("\nNo ground points found within cutout. Proceeding with next tree top.\n"))
                next
              } 
              
              # Write clipped LAS file to disk
              writeLAS(ttop_clipped, file = sprintf("%s\\10_pointcloud_trees\\trees_clipped_gam_smoothed\\ttop_gam_smoothed_%d.las", output_path, i))
              
              # Update progress bar
              pb$tick()
            }

        
      # PROCESSING | STEP 6 | RASTERIZATION -------------------------------------
        
        cat(highlight("\n# POINT CLOUD RASTERIZATION #\n\n"))
        
        # Check if older exists
        if(file.exists(sprintf('%s\\07_dtm', output_path))){
          cat(note("File already exists and is read from the output directory.\n"))
          las_ctg_ground_merged <- readLAScatalog(folder=sprintf('%s\\07_dtm', output_path))
        } else {
          
          # Specify name of output folder
          output_folder_name <- "07_dtm"
          
          # Create clip folder in output directory
          dir.create(file.path(output_path, output_folder_name))
          
          # Rasterize pointcloud using TIN method
          cat("Starting to rasterize point cloud using TIN..\n")
          las_dtm <- rasterize_terrain(las_ground_merged_smooth, algorithm = tin(), res = dtm_resolution, pkg ="terra")
          #plot_dtm3d(las_dtm, bg = "white") 
          
          cat("Writing DTM to disk...\n")
          
          # Write dtm
          writeRaster(las_dtm, sprintf("%s\\%s\\dtm_%s.tif", output_path, output_folder_name, foldername), overwrite=TRUE, gdal=c("COMPRESS=NONE", "TFW=YES"))
          
          # Plot shaded relief
          dtm_prod <- terrain(las_dtm, v = c("slope", "aspect"), unit = "radians")
          dtm_hillshade <- shade(slope = dtm_prod$slope, aspect = dtm_prod$aspect)
          plot(dtm_hillshade, col =gray(0:30/30), legend = FALSE)
        }
        
      
      # POST-PROCESSING | STEP 1 | ACCURACY ASSESSMENT --------------------------
        
        # Checking if accuracy assessment is desired
        if(accuracy_assessment){
          
          cat(highlight("\n# ACCURACY ASSESSMENT #\n\n"))
          
          # Check if older exists
          if(file.exists(sprintf('%s\\08_accuracy_assessment', output_path))){
            cat(note("File already exists and is read from the output directory.\n"))
            las_ctg_ground_merged <- readLAScatalog(folder=sprintf('%s\\08_accuracy_assessment', output_path))
          } else {
            
            # Specify name of output folder
            output_folder_name <- "08_accuracy_assessment"
            
            # Create clip folder in output directory
            dir.create(file.path(output_path, output_folder_name))
          
            # Choose check points file
            cat("Loading ground point (GP) coordinates..\n")

            # Read check points csv
            ground_points <- read.csv(csv_file_path)
            options("digits" = 10)
            ground_points
            
            # Create an empty df to store the results
            cat("Assessing accuracies..\n")
            filtering_accuracy <- data.frame(GP=character(), GP_X=numeric(), GP_Y=numeric(), GP_Z=numeric(), CP_X=numeric(), CP_Y=numeric(), CP_Z=numeric(), XY_Delta=numeric(), Z_Delta=numeric())
            
            # Initialization of a list to store results
            results_list <- list()
            
            for (i in 1:nrow(ground_points)) {
              # Extract xyz coordinates of ground point
              gp_x <- ground_points$Rechts[i]
              gp_y <- ground_points$Hoch[i]
              gp_z <- ground_points$oHoehe[i] - reflector_height  # adjusted height value
              
              # Create bounding box around ground point
              bb_size <- 0.1
              x_min <- gp_x - bb_size
              x_max <- gp_x + bb_size
              y_min <- gp_y - bb_size
              y_max <- gp_y + bb_size
              z_min <- gp_z - bb_size
              z_max <- gp_z + bb_size
              
              # Filter points that fall within the bounding box
              filtered_points <- las_ground_merged_smooth@data[las_ground_merged_smooth@data$X >= x_min & 
                                                                 las_ground_merged_smooth@data$X <= x_max &
                                                                 las_ground_merged_smooth@data$Y >= y_min & 
                                                                 las_ground_merged_smooth@data$Y <= y_max &
                                                                 las_ground_merged_smooth@data$Z >= z_min & 
                                                                 las_ground_merged_smooth@data$Z <= z_max, ]
              
              # Calculate 2D distance to GP
              filtered_points$Distance_GP <- sqrt((filtered_points$X - gp_x)^2 + 
                                                    (filtered_points$Y - gp_y)^2)
              
              # Find the closest point
              closest_index <- which.min(filtered_points$Distance_GP)
              closest_point <- filtered_points[closest_index, ]
              
              if (nrow(filtered_points) > 0) {
                # Prepare result entry when points are found
                result_entry <- data.frame(
                  GP = ground_points$PNR[i],
                  GP_X = gp_x,
                  GP_Y = gp_y,
                  GP_Z = gp_z,
                  CP_X = closest_point$X,
                  CP_Y = closest_point$Y,
                  CP_Z = closest_point$Z,
                  XY_Delta = closest_point$Distance_GP,
                  Z_Delta = abs(closest_point$Z - gp_z)
                )
              } else {
                # Prepare result entry when no points are found, ensure all columns are present
                result_entry <- data.frame(
                  GP = ground_points$PNR[i],
                  GP_X = gp_x,
                  GP_Y = gp_y,
                  GP_Z = gp_z,
                  CP_X = NA,
                  CP_Y = NA,
                  CP_Z = NA,
                  XY_Delta = NA,
                  Z_Delta = NA
                )
              }
              
              
              # Append result entry to the list
              results_list[[i]] <- result_entry
            }
            
            # Combine all results into one dataframe
            filtering_accuracy <- do.call(rbind, results_list)
            
            # Safe as CSV
            fwrite(filtering_accuracy, file=sprintf('%s\\08_accuracy_assessment\\accuracy_report_%s.csv', output_path, foldername))
            
            # Calculate mean delta
            mean_delta <- mean(filtering_accuracy$Z_Delta, na.rm = TRUE)
            cat(paste("Mean accuracy:", round(mean_delta, digits = 5), "m.\n"))
          
          }
        
        } else {
          cat("Accuracy assessment not desired.\n")
        }
            
        # Remove unused objects
        cat(note("\nGarbage collection: Deleting unused files to free memory.\n"))
            
        rm(las_ctg_ground, las_ground_merged, las_ground_merged_smooth)
        invisible(gc(verbose = FALSE))
        
        
        # REPORT PROCESSING SETTINGS ----------------------------------------------
        

        csf_settings <- data.frame(cloth_resolution, class_threshold, cloth_rigidness, post_processing, row.names = "value")
        processing_settings <- data.frame(voxel_size, dtm_resolution, smoothing_window)
        accuracy_settings <- data.frame(accuracy_assessment, reflector_height)
        point_counts <- data.frame(
          las_ctg_clip = clip_points,
          las_ctg_voxelized = voxelized_points,
          las_ctg_denoised = denoised_points,
          las_ctg_ground = ground_points
          )
        
        
        if(!require(openxlsx)){install.packages('openxlsx')}
        library(openxlsx)
        
        wb <- createWorkbook()
        
        # Add some sheets to the workbook
        addWorksheet(wb, "csf_settings")
        addWorksheet(wb, "processing_settings")
        addWorksheet(wb, "accuracy_settings")
        addWorksheet(wb, "point_counts")
        
        # Write the data to the sheets
        writeData(wb, sheet = "csf_settings", x = csf_settings)
        writeData(wb, sheet = "processing_settings", x = processing_settings)
        writeData(wb, sheet = "accuracy_settings", x = accuracy_settings)
        writeData(wb, sheet = "point_counts", x = point_counts)
        
        # Export the file
        saveWorkbook(wb, sprintf("%s\\processing_report_%s.xlsx", output_path, foldername))
        
      # stopping App
      stopApp()
    })
  }
        
# Run the application
runApp(shinyApp(ui = ui, server = server))

        
        
