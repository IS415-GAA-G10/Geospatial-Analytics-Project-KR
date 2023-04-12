library(shiny)
library(sf)
library(tmap)
library(tidyverse)
library(readr)
library(shinythemes)
library(leaflet)
library(shinycssloaders)
library(rgdal)
library(spNetwork)
library(spatstat)
library(raster)
library(maptools)
library(sfdep)
options(shiny.maxRequestSize = 30*1024^2)

ui <- navbarPage("Hospital Playlist",
                 theme = shinytheme('flatly'),
                 tabPanel("About Us",
                          fluidPage(
                            sidebarLayout(
                              sidebarPanel(
                                h2(strong("Spatial Point Pattern Analysis by Team 1")),
                                tags$ul(
                                  tags$li("Teo Jun Hao", style = "font-size:150%"),
                                  tags$li("Sherry Ng Shea Li", style = "font-size:150%"),
                                  tags$li("How Xin Yee", style = "font-size:150%")
                                ),
                                h2(strong("Guided by:")),
                                tags$li("Professor Kam Tin Seong (SMU IS415)", style = "font-size:150%"),
                                width = 3
                              ),
                              mainPanel(
                                h2(strong("Problem Statement")),
                                hr(),
                                span("The Korean National Statistical Office reports that the elderly population 
                                     (aged 65 and over) in South Korea is expected to reach 14.9 million by 2067, 
                                     which represents nearly one-third of the total population. This demographic 
                                     shift is likely to increase demand for healthcare services, particularly in regions with higher proportions of elderly residents. 
                                     Do South Korean residents have equal accessibility to healthcare services?",
                                     style = "font-size:150%"),
                                h2(strong("Project Objective")),
                                hr(),
                                span("The objective of this project is to allow the end-user to use our model to 
                                     identify areas with low healthcare accessibility. This app will also show the 
                                     difference between the conventional and Network Constrained Spatial Point Analysis. 
                                     Hence, the app will not only provide insights to healthcare accessibility but also 
                                     serve as an educational tool on the different types of Spatial Point analysis.",
                                     style = "font-size:150%"),
                                h2(strong("App Functions")),
                                hr(),
                                tags$ol(
                                  tags$li(
                                    style = "font-size: 150%",
                                    strong("Conventional Spatial Point Pattern Analysis:"),
                                    tags$ul(
                                      tags$li("Visualisation of variable points"),
                                      tags$li("Kernel Density Plots"),
                                      tags$li("G, K, L Function plots and analysis"))),
                                  tags$li(
                                    style = "font-size: 150%",
                                    strong("Network Constrained Spatial Point Analysis"),
                                    tags$ul(
                                      tags$li("Visualisation of Points and Network"),
                                      tags$li("Visualisation of NetKDE"),
                                      tags$li("Network Constrained G and K Function Analysis"))),
                                  tags$li(
                                    style = "font-size: 150%",
                                    strong("Co-Location Analysis"),
                                    tags$ul(
                                      tags$li("Visualisation of Local Co-Location Points"),
                                      tags$li("Co-Location Statistical Interpretation")))
                                ),
                                h2(strong("Requirements")),
                                hr(),
                                span("Do note that data wrangling should also be done before uploading the files into the application.
                                      Loading of .rds files are required in order to perform the analysis.
                                  
                                      Example of .rds files format are as follows: 
                                      ",
                                     
                                     style = "font-size:150%"),
                                br(),
                                img(src="rds_file_format.png"),
                                br(),
                                br(),
                                br(),
                                br(),
                                br(),
                                br(),
                                br()
                              )
                              
                            )
                          )
                 ),
                 tabPanel("Data Import", fluid = TRUE, icon=icon("database"),
                          sidebarLayout(position = 'left',
                                        sidebarPanel(fluid = TRUE, width = 4,
                                                     tags$strong("RDS Data Import (Healthcare):"),
                                                     tags$br(),
                                                     tags$i("Upload two RDS files containing point data"),
                                                     tags$hr(),
                                                     fileInput("healthcare", 
                                                               "Upload rds file of healthcare points [POINT data]",
                                                               accept = c(".rds")),
                                                     fileInput("other_variable", 
                                                               "Upload rds file of another variable [POINT data]",
                                                               accept = c(".rds")),
                                                     fileInput("network",
                                                               "Upload rds file of transport networks [LINE data]",
                                                               accept = c(".rds")),
                                                     fileInput("studyarea",
                                                               "Upload rds file of study area shapefile [POLYGON data]",
                                                               accept = c(".rds")),
                                                     actionButton("submit", "Submit"),
                                                     uiOutput("district_selector")
                                        ),
                                        mainPanel(width = 8,
                                                  withSpinner(tmapOutput("point_map")),
                                                  
                                        )),
                 ),
                 tabPanel("Conventional Spatial Point Pattern Analysis", fluid = TRUE,
                          sidebarLayout(position = 'left',
                                        sidebarPanel(fluid = TRUE, width = 3,
                                                     conditionalPanel(
                                                       'input.SPPA === "First-Spatial Point Pattern KDE Visualization"',
                                                       tags$strong("Spatial Point Pattern Variable Inputs"),
                                                       numericInput(inputId = "crs",
                                                                    label = "Input the coordinate reference system (CRS)",
                                                                    min =0,
                                                                    value =4326,
                                                                    step =1),
                                                       selectInput(inputId = "SPPA_bw",
                                                                   label = "Select the automatic bandwidth method to be used:",
                                                                   choices = c("bw.diggle()" = "bw.diggle",
                                                                               "bw.CvL()" =  "bw.CvL", 
                                                                               "bw.scott()" = "bw.scott",
                                                                               "bw.ppl()" = "bw.ppl"),
                                                                   selected = "bw.ppl"),
                                                       selectInput(inputId = "SPPA_kernel",
                                                                   label = "Select the Kernel smoothing method to be used:",
                                                                   choices = c("gaussian" = "gaussian",
                                                                               "epanechnikov" = "epanechnikov", 
                                                                               "quartic" = "quartic",
                                                                               "disc" = "disc"),
                                                                   selected = "gaussian"),
                                                       actionButton("SPPA1_Run", "Run Analysis"),
                                                       
                                                     ),
                                                     
                                        ),
                                        mainPanel(width = 9,
                                                  tabsetPanel(
                                                    id = "SPPA",
                                                    tabPanel("First-Spatial Point Pattern KDE Visualization",
                                                             tmapOutput("SPPA1_output")
                                                    ),
                                                    tabPanel("Raster Spatial Point Pattern Visualization",
                                                             plotOutput("Raster_output")
                                                    ),
                                                    tabPanel("G Function",
                                                             plotOutput("G_output")
                                                    ),
                                                    tabPanel("L Function",
                                                             plotOutput("L_output")
                                                    )
                                                  )
                                        )
                                        
                          )
                 ),
                 tabPanel("Co-Location Analysis",fluid = TRUE,
                          sidebarLayout(position = 'left',
                                        sidebarPanel(fluid = TRUE, width = 3,
                                                     tags$strong("Co-Location Analysis Variable Inputs"),
                                                     hr(),
                                                     selectInput(inputId = "kernel",
                                                                 label = "Choose the Kernel to be used:",
                                                                 choices = c("Quartic" = "quartic",
                                                                             "Gaussian" = "gaussian",
                                                                             "Triangular" = "triangular",
                                                                             "Epanechnikov" = "epanechnikov",
                                                                             "Uniform" = "uniform"),
                                                                 selected = "gaussian"),
                                                     numericInput(inputId = "n_sim",
                                                                  label = "Number of Simulations: (key in a number between 0 to 100)",
                                                                  min = 0,
                                                                  max = 100,
                                                                  step = 1,
                                                                  value = 49),
                                                     tags$strong("Visualisation Customisation"),
                                                     selectInput("palette", "Select color palette:",
                                                                 choices = c("Reds", "Blues", "magma", "inferno", "cividis")),
                                                     selectInput("colour", "Select healthcare points color:",
                                                                 choices = c("red", "green", "blue", "yellow", "purple", "lightblue")),
                                                     sliderInput("dot_size", "Select dot size:",
                                                                 min = 0.01, max = 0.1, value = 0.01, step = 0.01),
                                                     actionButton("Colocation_Run", "Run Analysis"),
                                        ),
                                        mainPanel(width = 9,
                                                  tmapOutput("Colocation_V"))
                          )
                 ),
                 tabPanel("Network Constrained Spatial Point Analysis", fluid = TRUE,
                          sidebarLayout(position = 'left',
                                        sidebarPanel(fluid = TRUE, width = 3,
                                                     conditionalPanel(
                                                       'input.NetKDE === "Network Kernal Density Estimate Visualisation"',
                                                       tags$strong("Network Kernel Density Estimation Variable Inputs"),
                                                       selectInput(inputId = "kernel",
                                                                   label = "Choose the Kernel to be used:",
                                                                   choices = c("Quartic" = "quartic",
                                                                               "Triangle" = "triangle",
                                                                               "Tricube" = "tricube",
                                                                               "Cosine" = "cosine",
                                                                               "Triweight" = "triweight",
                                                                               "Epanechnikov" = "epanechnikov",
                                                                               "Uniform" = "uniform"),
                                                                   selected = "quartic"),
                                                       selectInput(inputId = "method",
                                                                   label = "Select the Method to be used:",
                                                                   choices = c("Simple" = "simple",
                                                                               "Discontinuous" = "discontinuous", 
                                                                               "Continuous" = "continuous"),
                                                                   selected = "simple"),
                                                       tags$strong("Visualisation Customisation"),
                                                       selectInput("palette", "Select color palette:",
                                                                   choices = c("Reds", "Blues", "magma", "inferno", "cividis")),
                                                       selectInput("colour", "Select healthcare points color:",
                                                                   choices = c("red", "green", "blue", "yellow", "purple", "lightblue")),
                                                       sliderInput("dot_size", "Select dot size:",
                                                                   min = 0.01, max = 0.1, value = 0.01, step = 0.01),
                                                       actionButton("NetKDE_Run", "Run Analysis"),
                                                       
                                                     ),
                                                     conditionalPanel(
                                                       'input.NetKDE === "Network Constrained K-Function Analysis"',
                                                       numericInput(inputId = "N_SIM",
                                                                    label = "Number of Simulations: (key in a number between 0 to 1000)",
                                                                    min = 0,
                                                                    max = 1000,
                                                                    step = 1,
                                                                    value = 99),
                                                       numericInput(inputId = "conf",
                                                                    label ="Confidence Interval: (Key in either, 0.01, 0.05, 0.10)",
                                                                    min = 0.01,
                                                                    max = 0.10,
                                                                    value = 0.05),
                                                       sliderInput(inputId = "end_distance",
                                                                   label = "Select end distance",
                                                                   min = 50, 
                                                                   max = 10000,
                                                                   value = 5000,
                                                                   step = 50),
                                                       actionButton("NetKDE_Kfunc_run", "Run Analysis")
                                                       
                                                       
                                                       
                                                     ),
                                                     conditionalPanel(
                                                       'input.NetKDE === "Network Constrained K-Cross Function Analysis"',
                                                       numericInput(inputId = "N_SIM",
                                                                    label = "Number of Simulations: (key in a number between 0 to 1000)",
                                                                    min = 0,
                                                                    max = 1000,
                                                                    step = 1,
                                                                    value = 99),
                                                       numericInput(inputId = "conf",
                                                                    label ="Confidence Interval: (Key in either, 0.01, 0.05, 0.10)",
                                                                    min = 0.01,
                                                                    max = 0.10,
                                                                    value = 0.05),
                                                       sliderInput(inputId = "end_distance",
                                                                   label = "Select end distance",
                                                                   min = 50, 
                                                                   max = 10000,
                                                                   value = 5000,
                                                                   step = 50),
                                                       actionButton("NetKDE_Kcross_run", "Run Analysis")
                                                       
                                                     )
                                        ),
                                        mainPanel(width = 9,
                                                  tabsetPanel(
                                                    id = "NetKDE",
                                                    tabPanel("Network Kernal Density Estimate Visualisation",
                                                             tmapOutput("NetKDE_V")
                                                    ),
                                                    tabPanel("Network Constrained K-Function Analysis",
                                                             plotOutput("NetKDE_Kfunction")
                                                    ),
                                                    tabPanel("Network Constrained K-Cross Function Analysis",
                                                             plotOutput("NetKDE_Kcross")
                                                    )
                                                  )
                                        )
                                        
                          )
                          
                 )
)

server <- function(input, output) {
  check_file_extension <- function(filepath, ext) {
    tools::file_ext(filepath) %in% ext
  }
  point1 <- reactiveVal(NULL)
  point2 <- reactiveVal(NULL)
  line1 <- reactiveVal(NULL)
  studyarea <- reactiveVal(NULL)
  
  filtered_data <- reactiveVal(NULL)
  
  observeEvent(input$submit, {
    shinyjs::show("loading_spinner")
    # Check if a file is uploaded
    req(c(input$healthcare,input$other_variable, input$network, input$studyarea))
    
    if (check_file_extension(input$healthcare$name, "rds") && 
        check_file_extension(input$other_variable$name, "rds") && 
        check_file_extension(input$network$name, "rds") &&
        check_file_extension(input$studyarea$name, "rds")) {
      # Read the .rds file into an object called healthcare, other_variable and network
      point1(read_rds(input$healthcare$datapath))
      point2(read_rds(input$other_variable$datapath))
      line1(read_rds(input$network$datapath))
      studyarea(read_rds(input$studyarea$datapath))
      
      Healthcare_filtered <- reactiveVal(NULL)
      Variable_filtered <- reactiveVal(NULL)
      Network_filtered <- reactiveVal(NULL)
      Studyarea_filtered <- reactiveVal(NULL)
      output$district_selector <- renderUI({
        req(studyarea())
        selectInput("selected_district", "Select Study Area:",
                    choices = unique(studyarea()$name_en))
      })
      output$point_map <- renderTmap({
        req(point1(), point2(), line1(), studyarea(), input$selected_district)
        
        selected_studyarea <- studyarea()[studyarea()$name_en == input$selected_district, ]
        Healthcare <- point1()[point1()$name_en == input$selected_district, ]
        Variable <- point2()[point2()$name_en == input$selected_district, ]
        Network <- line1()[line1()$name_en == input$selected_district, ]
        
        #reactive variables to use
        Healthcare_filtered(Healthcare)
        Variable_filtered(Variable)
        Network_filtered(Network)
        Studyarea_filtered(selected_studyarea)
        
        tm_shape(Network) +
          tm_lines(col = "yellow", lwd = 1) +
          tm_shape(Healthcare) +
          tm_dots(col = "lightblue", size = 0.01, border.col = "black") +
          tm_shape(Variable) +
          tm_dots(col = "orange", size = 0.01, border.col = "black") +
          
          tm_layout(title = "Filtered Study Area and Intersections")
      })
      
      
      # SHERRY CODE STARTS HERE 
      #  kdeplot <- density(healthcare_owin.km,
      #               sigma=as.numeric(input$SPPA_bw),
      #                edge=TRUE,
      #               kernel=input$SPPA_kernel) 
      #  kdeplot$plotk
      #=============================================================================
      # SPPA 1
      observeEvent(input$SPPA1_Run, {
        output$SPPA1_output <- renderTmap({
          req(Healthcare_filtered(),  Studyarea_filtered())
          H <- Healthcare_filtered()
          S <- Studyarea_filtered()
          seoul <- as_Spatial(S)
          seoul_sp <- as(seoul, "SpatialPolygons")
          seoul_owin <- as(seoul_sp, "owin") 
          healthcare_s <- as_Spatial(H)
          healthcare_sp <- as(healthcare_s, "SpatialPoints")
          healthcare_ppp <- as(healthcare_sp, "ppp")
          healthcare_owin = healthcare_ppp[seoul_owin]
          healthcare_owin.km <- rescale(healthcare_owin, 1000, "km")
          
          
          if (input$SPPA_bw == "bw.ppl"){
            the_bw <- bw.ppl(healthcare_owin.km)
          } else if (input$SPPA_bw == "bw.diggle"){
            the_bw <- bw.diggle(healthcare_owin.km)
          } else if (input$SPPA_bw == "bw.CvL"){
            the_bw <- bw.CvL(healthcare_owin.km)
          } else if (input$SPPA_bw == "bw.scott"){
            the_bw <- bw.scott(healthcare_owin.km)
          } else if (input$SPPA_bw == "bw.ppl"){
            the_bw <- bw.ppl(healthcare_owin.km)
          }
          kde <- density(healthcare_owin.km,
                         sigma=as.numeric(the_bw),
                         edge=TRUE,
                         kernel=input$SPPA_kernel)
          
          
          gridded_kde <- as.SpatialGridDataFrame.im(kde)
          kde_raster <- raster(gridded_kde)
          c <- paste0("+init=EPSG:", input$crs, " +units=km")
          projection(kde_raster) <- CRS(c)
          #tm_shape(seoul_owin) +
          # tm_borders(col = 'black',
          #           lwd = 1,
          #          alpha = 0.5) +
          tm_shape(kde_raster) + 
            tm_raster("v", alpha = 0.7) +
            tm_layout(legend.outside = TRUE, frame = FALSE, title = "KDE") +
            tmap_options(basemaps = c("Esri.WorldGrayCanvas","OpenStreetMap", "Stamen.TonerLite"),
                         basemaps.alpha = c(0.8, 0.8, 0.8)) +
            tm_view(set.zoom.limits = c(11,13)) 
          
        })
      })         
      
      
      #co-location Section
      #co-location Visualisation
      observeEvent(input$Colocation_Run, {
        output$Colocation_V <- renderTmap({
          req(Healthcare_filtered(), Variable_filtered(), Studyarea_filtered())
          healthcare_variable <- rbind(Healthcare_filtered(), Variable_filtered())
          nb_healthcare <- include_self(
            st_knn(st_geometry(healthcare_variable), 6))
          wt_healthcare <- st_kernel_weights(nb_healthcare, 
                                             healthcare_variable, 
                                             input$kernel, 
                                             adaptive = TRUE)
          A <- Healthcare_filtered()$name_en
          B <- Variable_filtered()$name_en
          LCLQ_healthcare <- local_colocation(A, B, nb_healthcare, wt_healthcare, input$n_sim)
          healthcare_variable_LCLQ <- cbind(healthcare_variable, LCLQ_healthcare)
          new_name = Studyarea_filtered()$name_en[1]
          new_name <- sub("-", ".", new_name)
          tmap_mode("view")
          tm_shape(Studyarea_filtered()) +
            tm_polygons() +
            tm_shape(healthcare_variable_LCLQ)+ 
            tm_dots(col = new_name,
                    size = input$dot_size,
                    border.col = "black",
                    border.lwd = 0.5,
                    palette = input$palette)
        })
      })
      
      #=============================================================================
      # NetKDE Section
      # NetKDE Visualisation
      observeEvent(input$NetKDE_Run, {
        output$NetKDE_V <- renderTmap({
          req(Healthcare_filtered(), Variable_filtered(), Network_filtered(), Studyarea_filtered())
          lixels <- lixelize_lines(Network_filtered(), 700, mindist = 350)
          H <- Healthcare_filtered()
          N <- Network_filtered()
          samples <- lines_center(lixels)
          densities <- nkde(N, 
                            events = H,
                            w = rep(1,nrow(H)),
                            samples = samples,
                            kernel_name = input$kernel,
                            bw = 300, 
                            div= "bw", 
                            adaptive = FALSE,
                            method = input$method, 
                            digits = 1, 
                            tol = 1,
                            grid_shape = c(1,1), 
                            max_depth = 8,
                            agg = 5, #we aggregate events within a 5m radius (faster calculation)
                            sparse = TRUE,
                            verbose = FALSE)
          samples$density <- densities*1000
          lixels$density <- densities*1000
          tm_shape(lixels)+
            tm_lines(col="density",
                     palette = input$palette) +
            tm_shape(Healthcare_filtered()) +
            tm_dots(col = input$colour,
                    size = input$dot_size)
        })
      })
      #=============================================================================
      #NetKDE K Function
      observeEvent(input$NetKDE_Kfunc_run, {
        output$NetKDE_Kfunction <- renderPlot({
          req(Healthcare_filtered(), Variable_filtered(), Network_filtered(), Studyarea_filtered())
          H <- Healthcare_filtered()
          N <- Network_filtered()
          kfun_hospital <- kfunctions(N, 
                                      H,
                                      start = 0, 
                                      end = input$end_distance, 
                                      step = 50, 
                                      width = 50, 
                                      nsim = input$N_SIM, 
                                      resolution = 50,
                                      verbose = FALSE,
                                      digits = 10,
                                      conf_int = input$conf)
          kfun_hospital$plotk
        })
      })
      #=============================================================================
      # Net KDE Kcross
      observeEvent(input$NetKDE_Kcross_run, {
        output$NetKDE_Kcross <- renderPlot({
          req(Healthcare_filtered(), Variable_filtered(), Network_filtered(), Studyarea_filtered())
          H <- Healthcare_filtered()
          V <- Variable_filtered()
          N <- Network_filtered()
          crossfun_hospital<- cross_kfunctions(N, 
                                               V, 
                                               H, 
                                               start= 0, 
                                               end = input$end_distance, 
                                               step = 50, 
                                               width = 50, 
                                               nsim = input$N_SIM,
                                               agg = 100,
                                               conf_int = input$conf)
          crossfun_hospital$plotk
        })
      })
      # End of NetKDE section.
      #=============================================================================
      
    } else {
      # Show an error message if the uploaded files have the wrong extension
      output$point_map <- renderTmap({
        req(FALSE) # Stop rendering
        NULL
      })
      showModal(modalDialog(
        title = "Error",
        "Please upload RDS files.",
        easyClose = TRUE
      ))
    }
  })
  
  
  
}

shinyApp(ui = ui, server = server)









