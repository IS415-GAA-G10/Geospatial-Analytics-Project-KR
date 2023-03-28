#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(sf)
library(tmap)
library(tidyverse)
library(readr)
library(shinythemes)
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
                                      tags$li("Co-Location Statistical Interpretation")))),
                                
                                
                              )
                            )
                          )
                 ),
                 tabPanel("Conventional Spatial Point Pattern Analysis",
                          fluidPage(
                            
                          )),
                 tabPanel("Network Constrained Spatial Point Analysis",
                          fluidPage(
                            
                          )),
                 tabPanel("Co-Location Analysis")
                
)
server <- function(input, output) {
}

shinyApp(ui = ui, server = server)
                      
                    

                                     
                                     
                                     
                                     
                                     
                                     
                                     