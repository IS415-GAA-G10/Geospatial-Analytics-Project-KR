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
library(ggplot2) 
library(plotly) 
library(ggthemes)
library(shinyWidgets)
options(shiny.maxRequestSize = 30*1024^2)

ui <- navbarPage("Hospital Playlist",
                 theme = shinytheme('flatly'),
                 tabPanel("About Us",
                          icon = icon("university"),
                          fluidPage(
                            sidebarLayout(
                              sidebarPanel(
                                id = "sidebar_panel",
                                style = "background-color: inherit;",
                                tags$div(style = "position: relative; text-align: left; z-index: 1000;",
                                         switchInput("mode_toggle", "Switch mode:", value = FALSE, onLabel = "Light", offLabel = "Dark")),
                                tags$br(), 
                                tags$br(),
                                img(src = "SMU_logo.png", style = "width: 100%;"),
                                h2(strong("Spatial Point Pattern Analysis by Team 10")),
                                tags$ul(
                                  tags$li("Teo Jun Hao", style = "font-size:150%"),
                                  tags$li("Sherry Ng Shea Li", style = "font-size:150%"),
                                  tags$li("How Xin Yee", style = "font-size:150%")
                                ),
                                h2(strong("Guided by:")),
                                tags$li("Professor Kam Tin Seong (SMU IS415)", style = "font-size:150%"),
                                width = 3,
                                uiOutput("custom_css"),
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
                                      tags$li("K & L Function plots and analysis"))),
                                  tags$li(
                                    style = "font-size: 150%",
                                    strong("Co-Location Analysis"),
                                    tags$ul(
                                      tags$li("Visualisation of Local Co-Location Points"),
                                      tags$li("Co-Location Statistical Interpretation"))),
                                  tags$li(
                                    style = "font-size: 150%",
                                    strong("Network Constrained Spatial Point Analysis"),
                                    tags$ul(
                                      tags$li("Visualisation of NetKDE"),
                                      tags$li("Network Constrained K Function Analysis"),
                                      tags$li("Network Constrained K-cross Function Analysis")))
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
                 tabPanel("Data Import", fluid = TRUE, icon=icon("upload"),
                          sidebarLayout(position = 'left',
                                        sidebarPanel(fluid = TRUE, width = 4, 
                                                     id = "sidebar_panel",
                                                     style = "background-color: inherit;",
                                                     tags$strong("RDS Data Import (Healthcare):"),
                                                     tags$br(),
                                                     tags$i("Upload four RDS files"),
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
                                        )
                          ),
                 ),
                 tabPanel("Conventional Spatial Point Pattern Analysis", fluid = TRUE, icon =icon("map"),
                          sidebarLayout(position = 'left',
                                        sidebarPanel(fluid = TRUE, width = 3,
                                                     id = "sidebar_panel",
                                                     style = "background-color: inherit;",
                                                     conditionalPanel(
                                                       'input.SPPA === "First-Spatial Point Pattern KDE Visualization"',
                                                       tags$strong("Spatial Point Pattern Variable Inputs"),
                                                       hr(),
                                                       numericInput(inputId = "crs",
                                                                    label = "Input the coordinate reference system (CRS)",
                                                                    min =0,
                                                                    value =5179),
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
                                                     conditionalPanel(
                                                       'input.SPPA === "K & L Function"',
                                                       tags$strong("K & L Function Inputs"),
                                                       hr(),
                                                       selectInput(inputId = "n_sim",
                                                                   label = "Choose the desired confidence interval (Significance Level)",
                                                                   choices = c("1% (0.01 Significance Level)" = 99,
                                                                               "5% (0.05 Significance Level)" = 39,
                                                                               "10% (0.10 Significance Level)" = 18),
                                                                   selected = 39),
                                                       selectInput(inputId = "KL",
                                                                   label = "Select the function to be used:",
                                                                   choices = c("Kest",
                                                                               "Lest"),
                                                                   selected = "Kest"),
                                                       actionButton("function_Run", "Run Analysis"),
                                                       
                                                     ),
                                                     
                                        ),
                                        mainPanel(width = 9,
                                                  tabsetPanel(
                                                    id = "SPPA",
                                                    tabPanel("First-Spatial Point Pattern KDE Visualization",
                                                             tmapOutput("SPPA1_output"),
                                                             tabsetPanel(
                                                               id = "SPPA_info",
                                                               tabPanel("About Spatial Point Pattern",
                                                                        column(12,
                                                                               h2("What is Spatial Point Pattern Kernel Density Estimation ?"),
                                                                               h5("Spatial point pattern kernel density estimation is a technique used in spatial statistics 
                                                                                  to estimate the intensity or density of point events or occurrences in a spatial domain. 
                                                                                  It involves smoothing the observed point pattern data to create a continuous surface or map
                                                                                  that represents the estimated density of points at different locations in the study area. "),
                                                                               h5("Kernel density estimation uses a kernel, which is a mathematical function, to spread the 
                                                                               influence of each point event or occurrence across its neighboring area. The kernel function 
                                                                               determines the shape, size, and orientation of the smoothing window around each point, and 
                                                                               the density estimate at a particular location is calculated as the sum of the weighted 
                                                                               contributions from all the points in the study area."),
                                                                               h2("How to interpret the output?"),
                                                                               h5("The darker the color, the higher the relative density of the point features as compared 
                                                                                  to lighter color (meaning lower density).")
                                                                        )))
                                                    ),
                                                    tabPanel("K & L Function",
                                                             plotlyOutput("KL_output"),
                                                             tabsetPanel(
                                                               id = "SPPA_info",
                                                               tabPanel("About K and L function",
                                                                        column(12,
                                                                               h2("What is K function?"),
                                                                               h5("k function calculates for a radius r the proportion of cells with a value below r 
                                                                                  in the distance matrix between all the points Did. The K function estimates 
                                                                                  'the average number of neighbours of a typical random point'. "),
                                                                               
                                                                               h2("What is L function?"),
                                                                               h5("L Function is a summary statistic that provides information on the expected number of points within a certain 
                                                                                  distance of other points, which can help identify patterns of point interactions, repulsions, 
                                                                                  or regularity in the point pattern data."),
                                                                               h2("How to interpret the output?"),
                                                                               h5("- Observed K(r) or L(r) values below the envelope indicate lower than exprected density at radius r, i.e., significant sparseness."),
                                                                               h5("- Observed K(r) or L(r) values within the envelope indicate no significant deviation from sparseness."),
                                                                               h5("- Observed K(r) or L(r) values above the envelope indicate higher than exprected density at radius  r, i.e., significant clustering."),
                                                                               
                                                                               
                                                                               
                                                                        )))
                                                    )
                                                  )
                                        )
                                        
                          )
                 ),
                 tabPanel("Co-Location Analysis",fluid = TRUE, icon = icon("map-marker"),
                          sidebarLayout(position = 'left',
                                        sidebarPanel(fluid = TRUE, width = 3,
                                                     id = "sidebar_panel",
                                                     style = "background-color: inherit;",
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
                                                     selectInput(inputId = "n_sim1",
                                                                 label = "Choose the desired confidence interval (Significance Level)",
                                                                 choices = c("1% (0.01 Significance Level)" = 99,
                                                                             "5% (0.05 Significance Level)" = 39,
                                                                             "10% (0.10 Significance Level)" = 18
                                                                 ),
                                                                 selected = 39),
                                                     numericInput(inputId = "neighbour",
                                                                  label = "Number of neighbours",
                                                                  min = 0,
                                                                  max = 10,
                                                                  step =1,
                                                                  value = 6),
                                                     tags$strong("Visualisation Customisation"),
                                                     selectInput("palette", "Select color palette:",
                                                                 choices = c("Reds"="-Reds", 
                                                                             "Blues" = "-Blues", 
                                                                             "Magma" = "-magma", 
                                                                             "Inferno" = "-inferno", 
                                                                             "Cividis"="-cividis")),
                                                     sliderInput("dot_size", "Select dot size:",
                                                                 min = 0.01, max = 0.1, value = 0.01, step = 0.01),
                                                     selectInput("alpha",
                                                                 "Select significance level (for filtering)",
                                                                 choices = c("0.01", "0.05", "0.10"),
                                                                 selected = "0.05"),
                                                     actionButton("Colocation_Run", "Run Analysis"),
                                        ),
                                        mainPanel(width = 9,
                                                  tmapOutput("Colocation_V"),
                                                  tabPanel("About Co-Location",
                                                           column(12,
                                                                  h2("What is Co-Location?"),
                                                                  h5("Co-Location can be used to determine the spatial relationship between the location of point features (healthcare facility) 
                                                                  and another variable of point features, in this case, residential buildings."),
                                                                  h5("Co-Location analysis can be used to locate the areas with highest accessibility to variables of concern
                                                                     in any situation where being within the vicinity brings great advantage."),
                                                                  h2("How to interpret the plot generated?"),
                                                                  h5("Point-based approach is used. For each point of the first variable (healthcare facilities), 
                                                                  you are as likely to have a point of the second variable as a neighbour. For points marked with the colour chosen, 
                                                                     it indicates that the first variable (healthcare facility) is located as a nearest neighbour."),
                                                                  h5("Those with no access or low access to the first variable (healthcare facility), 
                                                                     the points are displayed in grey."),
                                                                  h5("The points are also generated by the level of signifiance chosen."),
                                                           )
                                                           
                                                  )
                                        )
                          )
                 ),
                 tabPanel("Network Constrained Spatial Point Pattern Analysis", fluid = TRUE, icon = icon("road"),
                          sidebarLayout(position = 'left',
                                        sidebarPanel(fluid = TRUE, width = 3,
                                                     id = "sidebar_panel",
                                                     style = "background-color: inherit;",
                                                     conditionalPanel(
                                                       'input.NetKDE === "Network Kernal Density Estimate Visualisation"',
                                                       tags$strong("Network Kernel Density Estimation Variable Inputs"),
                                                       hr(),
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
                                                       selectInput("palette_N", "Select color palette:",
                                                                   choices = c("Reds", "Blues", "magma", "inferno", "cividis")),
                                                       selectInput("colour", "Select healthcare points color:",
                                                                   choices = c("red", "green", "blue", "yellow", "purple", "lightblue"),
                                                                   selected = "blue"),
                                                       sliderInput("dot_size", "Select dot size:",
                                                                   min = 0.01, max = 0.1, value = 0.01, step = 0.01),
                                                       actionButton("NetKDE_Run", "Enter"),
                                                       
                                                     ),
                                                     conditionalPanel(
                                                       'input.NetKDE === "Network Constrained K-Function Analysis"',
                                                       selectInput(inputId = "N_SIM",
                                                                   label = "Choose the desired confidence interval (Significance Level)",
                                                                   choices = c("1% (0.01 Significance Level)" = 99,
                                                                               "5% (0.05 Significance Level)" = 39,
                                                                               "10% (0.10 Significance Level)" = 18),
                                                                   selected = 39),
                                                       sliderInput(inputId = "end_distance",
                                                                   label = "Select end distance",
                                                                   min = 0, 
                                                                   max = 2500,
                                                                   value = 2000,
                                                                   step = 250),
                                                       actionButton("NetKDE_Kfunc_run", "Run Analysis")
                                                     ),
                                                     conditionalPanel(
                                                       'input.NetKDE === "Network Constrained K-Cross Function Analysis"',
                                                       selectInput(inputId = "N_SIM_2",
                                                                   label = "Choose the desired confidence interval (Significance Level)",
                                                                   choices = c("1% (0.01 Significance Level)" = 99,
                                                                               "5% (0.05 Significance Level)" = 39,
                                                                               "10% (0.10 Significance Level)" = 18),
                                                                   selected = 39),
                                                       numericInput(inputId = "conf",
                                                                    label ="Confidence Interval: (Key in either, 0.01, 0.05, 0.10)",
                                                                    min = 0.01,
                                                                    max = 0.10,
                                                                    value = 0.05),
                                                       sliderInput(inputId = "end_distance",
                                                                   label = "Select end distance",
                                                                   min = 50, 
                                                                   max = 2500,
                                                                   value = 2000,
                                                                   step = 250),
                                                       actionButton("NetKDE_Kcross_run", "Run Analysis")
                                                       
                                                     )
                                        ),
                                        mainPanel(width = 9,
                                                  tabsetPanel(
                                                    id = "NetKDE",
                                                    tabPanel("Network Kernal Density Estimate Visualisation",
                                                             column(12,
                                                                    h6(tags$strong("Note:")),
                                                                    h6(tags$i("By default, Kernel is set to Quartic and Method is set to Simple to plot the map,
                                                                        select alternative choices and click on 'Enter' to update the map.")),
                                                                    h6(tags$i("Please press 'Enter' and wait for the map to load. This section takes slightly longer so do be patient.")),
                                                                    tmapOutput("NetKDE_V"),
                                                                    tabsetPanel(
                                                                      id = "NetSPPA_KDE_info",
                                                                      tabPanel("About Network-Constrained Kernel Density Estimation",
                                                                               column(12,
                                                                                      h2("What is Network-Constrained Kernel Density Estimation?"),
                                                                                      h5("A classical Kernel Density Estimate (KDE) estimates the continuous density of a set of events in a
                                                                                  two-dimensional space, which is not suitable for analysing density of events occuring on a network.
                                                                                  Therefore, the modified Network-Constrained Kernel Density Estimation is used to calculate density of events
                                                                                  occuring along the edges of a network."),
                                                                                      h2("How to interpret the output?"),
                                                                                      h5("The darker the color of the road, the higher the relative density of the point features as compared 
                                                                                  to road segments with lighter color (meaning lower density).")
                                                                               ))))),
                                                    tabPanel("Network Constrained K-Function Analysis",
                                                             column(12,
                                                                    h6(tags$strong("Note:")),
                                                                    h6(tags$i("You can select the number of simulation and confidence level to plot out the Kfunction Graph. In addition,
                                                                    end distance lets you set the x axis to your preferred level. Once done selecting the options, click on 'Run Analysis' to update the plot.")),
                                                                    h6(tags$i("Please for a short while for the graph to load.")),
                                                                    plotlyOutput("NetKDE_Kfunction"),
                                                                    tabsetPanel(
                                                                      id = "NetSPPA_K_info",
                                                                      tabPanel("About K-Function",
                                                                               column(12,
                                                                                      h2("What is K-Function?"),
                                                                                      h5("K-function measures the number of events found up to a 
                                                                                 given distance of any particular event, and the graph helps illustrates the spatial dependence (clustering 
                                                                                    or dispersion) of point features (healthcare facility) over a wide range of distances (m)."),
                                                                                      h2("How to interpret the graph?"),
                                                                                      h4("Ho: The observed spatial point events (i.e distribution of healthcare facilities) are uniformly distributed over a street network in selected area"),
                                                                                      h4("H1: The observed spatial point events (i.e distribution of healthcare facilities) are spatially dependent over a street network in selected Area."),
                                                                                      h5("If the observed K (blue line) is above the envelope, then "),
                                                                                      h5(tags$strong("we can reject null hypothesis (the value is statistically significant) and conclude the points resemble clustered distribution.")),
                                                                                      h5("If not, if the observed K is below the envelope, then "),
                                                                                      h5(tags$strong("we can reject null hypothesis (the value is statistically significant) and conclude the points resemble dispersed distribution.")),
                                                                                      h5("Else, if the observed K is inside the envelope, it means "),
                                                                                      h5(tags$strong("the null hypothesis of CSR cannot be rejected (the value is not statistically significant) and we conclude the points resemble random distribution."))
                                                                                      
                                                                               ))))),
                                                    tabPanel("Network Constrained K-Cross Function Analysis",
                                                             h6(tags$strong("Note:")),
                                                             h6(tags$i("You can select the number of simulation and confidence level to plot out the Kfunction Graph. In addition,
                                                                    end distance lets you set the x axis to your preferred level. Once done selecting the options, click on 'Run Analysis' to update the plot.")),
                                                             h6(tags$i("Please for a short while for the graph to load.")),
                                                             plotlyOutput("NetKDE_Kcross"),
                                                             tabsetPanel(
                                                               id = "NetSPPA_CrossK_info",
                                                               tabPanel("About Cross K-Function",
                                                                        column(12,
                                                                               h2("What is Cross K-Function?"),
                                                                               h5("An extension of K-function, the Cross K-function measures the number of main point events (Healthcare facilities) around
                                                                         a set of secondary point events (Your chosen variable), and the graph illustrates the spatial dependence (clustering 
                                                                         or dispersion) of the Healthcare points around point your chosen variable points over a wide range of distances (m)."),
                                                                               h2("How to interpret the graph?"),
                                                                               h4("Ho: The two types of points resemble random distribution and are independent of each other."),
                                                                               h5("If the observed K (blue line) is above the envelope, then "),
                                                                               h5(tags$strong("we can reject null hypothesis (the value is statistically significant) and conclude the two types of points resemble attraction patterns, suggesting clustering.")),
                                                                               h5("If not, if the observed K (blue line) is below the envelope, then "),
                                                                               h5(tags$strong("we can reject null hypothesis (the value is statistically significant) and conclude the two types of points resemble repulsion patterns, suggesting dispersion.")),
                                                                               h5("Else, if the observed K (blue line) is inside the envelope, it means "),
                                                                               h5(tags$strong("the null hypothesis of CSR cannot be rejected (the value is not statistically significant) and we conclude the two types of points resemble random distribution and are independent of each other."))
                                                                        )))
                                                    )
                                                    
                                                  )
                                        )
                          )
                 )
)




server <- function(input, output) {
  
  mode <- reactiveVal("light")
  
  observeEvent(input$mode_toggle, {
    if (mode() == "light") {
      mode("dark")
    } else {
      mode("light")
    }
  })
  
  output$custom_css <- renderUI({
    if (mode() == "light") {
      tags$style(HTML("
      body {
        background-color: #f0f4fa;
        color: #00008B;
      }
      .dark-mode {
        background-color: #333;
        color: #fof4fa;
      }
    "))
    } else {
      tags$style(HTML("
      body {
        background-color: #333;
        color: #f0f4fa;
      }
      .dark-mode {
        background-color: #ADD8E6;
        color: #00008B;
      }
      .sidebar-panel {
        background-color: #2b2b2b;
      }
    "))
    }
  })
  
  
  
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
        filtered_name <- input$selected_district
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
          tm_layout(title = filtered_name) +
          tmap_options(basemaps = c("Esri.WorldGrayCanvas","OpenStreetMap", "Stamen.TonerLite"),
                       basemaps.alpha =c(0.75, 0.75, 0.75))
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
            tm_view(set.zoom.limits = c(11,13)) +
            tmap_options(basemaps = c("Esri.WorldGrayCanvas","OpenStreetMap", "Stamen.TonerLite"),
                         basemaps.alpha =c(0.75, 0.75, 0.75))
          
        })
      }) 
      
      #K and L Function 
      observeEvent(input$function_Run, {
        output$KL_output <- renderPlotly({
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
          if(input$KL == "Kest"){
            c="K(d)-r"
          }else{
            c="L(d)-r"
          }
          
          klfunction <- envelope(healthcare_owin, input$KL, nsim = as.numeric(input$n_sim), rank = 1, glocal=TRUE)
          klfunc_df <- as.data.frame(klfunction)
          
          colour=c("#0D657D","#ee770d","#D3D3D3")
          csr_plot <- ggplot(klfunc_df, aes(r, obs-r))+
            # plot observed value
            geom_line(colour=c("#4d4d4d"))+
            geom_line(aes(r,theo-r), colour="red", linetype = "dashed")+
            # plot simulation envelopes
            geom_ribbon(aes(ymin=lo-r,ymax=hi-r),alpha=0.1, colour=c("#91bfdb")) +
            xlab("Distance r (m)") +
            ylab(c) +
            geom_rug(data=klfunc_df[klfunc_df$obs > klfunc_df$hi,], sides="b", colour=colour[1])  +
            geom_rug(data=klfunc_df[klfunc_df$obs < klfunc_df$lo,], sides="b", colour=colour[2]) +
            geom_rug(data=klfunc_df[klfunc_df$obs >= klfunc_df$lo & klfunc_df$obs <= klfunc_df$hi,], sides="b", color=colour[3]) +
            theme_tufte()
          
          text1<-"Significant clustering"
          text2<-"Significant segregation"
          text3<-"Not significant clustering/segregation"
          
          # the below conditional statement is required to ensure that the labels (text1/2/3) are assigned to the correct traces
          if (nrow(klfunc_df[klfunc_df$obs > klfunc_df$hi,])==0){ 
            if (nrow(Lcsr_df[klfunc_df$obs < klfunc_df$lo,])==0){ 
              ggplotly(csr_plot, dynamicTicks=T) %>%
                style(text = text3, traces = 4) %>%
                rangeslider() 
            }else if (nrow(klfunc_df[klfunc_df$obs >= klfunc_df$lo & klfunc_df$obs <= klfunc_df$hi,])==0){ 
              ggplotly(csr_plot, dynamicTicks=T) %>%
                style(text = text2, traces = 4) %>%
                rangeslider() 
            }else {
              ggplotly(csr_plot, dynamicTicks=T) %>%
                style(text = text2, traces = 4) %>%
                style(text = text3, traces = 5) %>%
                rangeslider() 
            }
          } else if (nrow(klfunc_df[klfunc_df$obs < klfunc_df$lo,])==0){
            if (nrow(klfunc_df[klfunc_df$obs >= klfunc_df$lo & klfunc_df$obs <= klfunc_df$hi,])==0){
              ggplotly(csr_plot, dynamicTicks=T) %>%
                style(text = text1, traces = 4) %>%
                rangeslider() 
            } else{
              ggplotly(csr_plot, dynamicTicks=T) %>%
                style(text = text1, traces = 4) %>%
                style(text = text3, traces = 5) %>%
                rangeslider()
            }
          } else{
            ggplotly(csr_plot, dynamicTicks=T) %>%
              style(text = text1, traces = 4) %>%
              style(text = text2, traces = 5) %>%
              style(text = text3, traces = 6) %>%
              rangeslider()
          }
          
          
        })
      }) 
      
      
      
      
      #=========================================================================================
      
      #co-location Section
      #co-location Visualisation
      observeEvent(input$Colocation_Run, {
        
        req(Healthcare_filtered(), Variable_filtered(), Studyarea_filtered())
        healthcare_variable <- rbind(Healthcare_filtered(), Variable_filtered())
        nb_healthcare <- include_self(
          st_knn(st_geometry(healthcare_variable), input$neighbour))
        wt_healthcare <- st_kernel_weights(nb_healthcare, 
                                           healthcare_variable, 
                                           input$kernel, 
                                           adaptive = TRUE)
        A <- Healthcare_filtered()$type
        B <- Variable_filtered()$type
        LCLQ_healthcare <- local_colocation(A, B, nb_healthcare, wt_healthcare, as.numeric(input$n_sim1))
        healthcare_variable_LCLQ <- cbind(healthcare_variable, LCLQ_healthcare)
        
        p_sim_column <- grep("p_sim", colnames(healthcare_variable_LCLQ), value = TRUE)
        Significant <-subset(healthcare_variable_LCLQ, healthcare_variable_LCLQ[[p_sim_column]] < as.numeric(input$alpha) | is.na(healthcare_variable_LCLQ[[p_sim_column]])== FALSE)
        Not_significant <- subset(healthcare_variable_LCLQ, healthcare_variable_LCLQ[[p_sim_column]] > as.numeric(input$alpha) | is.na(healthcare_variable_LCLQ[[p_sim_column]]))
        
        if (nrow(Significant) >0 && 
            nrow(Not_significant) > 0){
          output$Colocation_V <- renderTmap({
            tm_shape(Studyarea_filtered())+
              tm_polygons() +
              tm_shape(Significant)+ 
              tm_dots(col = p_sim_column,
                      size = input$dot_size,
                      border.col = "black",
                      border.lwd = 0.5,
                      palette = input$palette) +
              tm_shape(Not_significant) +
              tm_dots(col = "black",
                      size = 0.01) +
              tmap_options(basemaps = c("Esri.WorldGrayCanvas","OpenStreetMap", "Stamen.TonerLite"),
                           basemaps.alpha =c(0.75, 0.75, 0.75))
            
          })
        }
        else {
          output$Colocation_V <- renderTmap({
            req(FALSE) # Stop rendering
            NULL
          })
          showModal(modalDialog(
            title = "No significant points below inputted alpha value",
            "Please try another significance level",
            easyClose = TRUE
          ))
        }
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
                     palette = input$palette_N) +
            tm_shape(Healthcare_filtered()) +
            tm_dots(col = input$colour,
                    size = input$dot_size) +
            tmap_options(basemaps = c("Esri.WorldGrayCanvas","OpenStreetMap", "Stamen.TonerLite"),
                         basemaps.alpha =c(0.75, 0.75, 0.75))
        })
      })
      #=============================================================================
      # NetKDE K Function
      observeEvent(input$NetKDE_Kfunc_run, {
        output$NetKDE_Kfunction <- renderPlotly({
          req(Healthcare_filtered(), Variable_filtered(), Network_filtered(), Studyarea_filtered())
          H <- Healthcare_filtered()
          N <- Network_filtered()
          
          if (input$N_SIM  == 18){
            conf <- 0.10
          } else if (input$N_SIM == 39){
            conf <- 0.05
          } else if (input$N_SIM == 99){
            conf <- 0.01
          }
          
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
                                      conf_int = conf)
          kfun_df <- data.frame(kfun_hospital$values)
          colour=c("#0D657D","#ee770d","#D3D3D3")
          csr_plot <- ggplot(kfun_df, aes(distances, obs_k))+
            # plot observed value
            geom_line(colour=c("#4d4d4d"))+
            # plot simulation envelopes
            geom_ribbon(aes(ymin=lower_k,ymax=upper_k),alpha=0.1, colour=c("#91bfdb")) +
            xlab("Distance (m)") +
            ylab("Empirical K-function") +
            geom_rug(data=kfun_df[kfun_df$obs_k > kfun_df$upper_k,], sides="b", colour=colour[1])  +
            geom_rug(data=kfun_df[kfun_df$obs_k < kfun_df$lower_k,], sides="b", colour=colour[2]) +
            geom_rug(data=kfun_df[kfun_df$obs_k >= kfun_df$lower_k & kfun_df$obs_k <= kfun_df$upper_k,], sides="b", color=colour[3]) +
            theme_tufte()
          
          text1<-"Significant clustering"
          text2<-"Significant segregation"
          text3<-"Not significant clustering/segregation"
          
          # the below conditional statement is required to ensure that the labels (text1/2/3) are assigned to the correct traces
          if (nrow(kfun_df[kfun_df$obs_k > kfun_df$upper_k,])==0){ 
            if (nrow(kfun_df[kfun_df$obs_k < kfun_df$lower_k,])==0){ 
              ggplotly(csr_plot, dynamicTicks=T) %>%
                style(text = text3, traces = 4) %>%
                rangeslider() 
            }else if (nrow(kfun_df[kfun_df$obs_k >= kfun_df$lower_k & kfun_df$obs_k <= kfun_df$upper_k,])==0){ 
              ggplotly(csr_plot, dynamicTicks=T) %>%
                style(text = text2, traces = 4) %>%
                rangeslider() 
            }else {
              ggplotly(csr_plot, dynamicTicks=T) %>%
                style(text = text2, traces = 4) %>%
                style(text = text3, traces = 5) %>%
                rangeslider() 
            }
          } else if (nrow(kfun_df[kfun_df$obs_k < kfun_df$lower_k,])==0){
            if (nrow(kfun_df[kfun_df$obs_k >= kfun_df$lower_k & kfun_df$obs_k <= kfun_df$upper_k,])==0){
              ggplotly(csr_plot, dynamicTicks=T) %>%
                style(text = text1, traces = 4) %>%
                rangeslider() 
            } else{
              ggplotly(csr_plot, dynamicTicks=T) %>%
                style(text = text1, traces = 4) %>%
                style(text = text3, traces = 5) %>%
                rangeslider()
            }
          } else{
            ggplotly(csr_plot, dynamicTicks=T) %>%
              style(text = text1, traces = 4) %>%
              style(text = text2, traces = 5) %>%
              style(text = text3, traces = 6) %>%
              rangeslider()
          }
        })
      })
      #=============================================================================
      # Net KDE Kcross
      observeEvent(input$NetKDE_Kcross_run, {
        output$NetKDE_Kcross <- renderPlotly({
          req(Healthcare_filtered(), Variable_filtered(), Network_filtered(), Studyarea_filtered())
          H <- Healthcare_filtered()
          V <- Variable_filtered()
          N <- Network_filtered()
          if (input$N_SIM_2  == 18){
            conf_C <- 0.10
          } else if (input$N_SIM_2 == 39){
            conf_C <- 0.05
          } else if (input$N_SIM_2 == 99){
            conf_C <- 0.01
          }
          crossfun_hospital<- cross_kfunctions(N, 
                                               V, 
                                               H, 
                                               start= 0, 
                                               end = input$end_distance, 
                                               step = 50, 
                                               width = 50, 
                                               nsim = as.numeric(input$N_SIM_2),
                                               agg = 100,
                                               conf_int = conf_C)
          crossfun_df <- data.frame(crossfun_hospital$values)
          colour=c("#0D657D","#ee770d","#D3D3D3")
          csr_plot <- ggplot(crossfun_df, aes(distances, obs_k))+
            # plot observed value
            geom_line(colour=c("#4d4d4d"))+
            # plot simulation envelopes
            geom_ribbon(aes(ymin=lower_k,ymax=upper_k),alpha=0.1, colour=c("#91bfdb")) +
            xlab("Distance (m)") +
            ylab("Empirical Kcross-function") +
            geom_rug(data=crossfun_df[crossfun_df$obs_k > crossfun_df$upper_k,], sides="b", colour=colour[1])  +
            geom_rug(data=crossfun_df[crossfun_df$obs_k < crossfun_df$lower_k,], sides="b", colour=colour[2]) +
            geom_rug(data=crossfun_df[crossfun_df$obs_k >= crossfun_df$lower_k & crossfun_df$obs_k <= crossfun_df$upper_k,], sides="b", color=colour[3]) +
            theme_tufte()
          
          text1<-"Significant clustering"
          text2<-"Significant segregation"
          text3<-"Not significant clustering/segregation"
          
          # the below conditional statement is required to ensure that the labels (text1/2/3) are assigned to the correct traces
          if (nrow(crossfun_df[crossfun_df$obs_k > crossfun_df$upper_k,])==0){ 
            if (nrow(crossfun_df[crossfun_df$obs_k < crossfun_df$lower_k,])==0){ 
              ggplotly(csr_plot, dynamicTicks=T) %>%
                style(text = text3, traces = 4) %>%
                rangeslider() 
            }else if (nrow(crossfun_df[crossfun_df$obs_k >= crossfun_df$lower_k & crossfun_df$obs_k <= crossfun_df$upper_k,])==0){ 
              ggplotly(csr_plot, dynamicTicks=T) %>%
                style(text = text2, traces = 4) %>%
                rangeslider() 
            }else {
              ggplotly(csr_plot, dynamicTicks=T) %>%
                style(text = text2, traces = 4) %>%
                style(text = text3, traces = 5) %>%
                rangeslider() 
            }
          } else if (nrow(crossfun_df[crossfun_df$obs_k < crossfun_df$lower_k,])==0){
            if (nrow(crossfun_df[crossfun_df$obs_k >= crossfun_df$lower_k & crossfun_df$obs_k <= crossfun_df$upper_k,])==0){
              ggplotly(csr_plot, dynamicTicks=T) %>%
                style(text = text1, traces = 4) %>%
                rangeslider() 
            } else{
              ggplotly(csr_plot, dynamicTicks=T) %>%
                style(text = text1, traces = 4) %>%
                style(text = text3, traces = 5) %>%
                rangeslider()
            }
          } else{
            ggplotly(csr_plot, dynamicTicks=T) %>%
              style(text = text1, traces = 4) %>%
              style(text = text2, traces = 5) %>%
              style(text = text3, traces = 6) %>%
              rangeslider()
          }
          
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





