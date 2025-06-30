library(shiny)

pacman::p_load(shiny, shinydashboard, shinycssloaders, 
               tidyverse, dplyr, leaflet, leaflet.extras)

source("data.R")
source("tabs/app1.R")
source("tabs/app2.R")
source("tabs/app3.R")

# Define sidebar
sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Sailor Shift’s Career Explorer", tabName = "app1", icon = icon("user-tie")),
    menuItem("Oceanus Folk Influence Tracker", tabName = "app2", icon = icon("music")),
    menuItem("Rising Star Prediction Dashboard", tabName = "app3", icon = icon("chart-simple"))
  )
)

# Define body
body <- dashboardBody(
  tags$head(
    tags$link(
      href = "https://fonts.googleapis.com/css2?family=Comfortaa&display=swap",
      rel = "stylesheet"
    ),
    tags$style(HTML("
    .nav-tabs > li > a {
      font-size: 14px !important;  /* You can increase this to 18px or 20px */
      font-weight: bold;
    }
    ")),
    tags$style(HTML("
        .content-wrapper,
        .skin-blue .wrapper,
        .skin-blue .content {
          background-color: #F5FAFC !important;
        }
        .main-header .navbar {
          background-color: #96B3C2 !important;
        }
        .main-header .logo {
          background-color: #96B3C2 !important;
          color:#123456 !important;
          font-weight: bold;
          font-family: 'Comfortaa', cursive !important;
        }
        .main-sidebar {
          width: 250px !important;
          background-color: #F2F4F5 !important;
        }
        .sidebar-menu > li > a {
          color: #123456 !important;
        }
        .sidebar-menu > li.active > a,
        .sidebar-menu > li > a:hover {
          background-color: #96B3C2 !important;
          color: #123456 !important;
          font-weight: bold;
        }
        .sidebar-menu > li > a > i {
          margin-right: 5px !important;
        }
        .small-box.bg-blue {
          background-color: #96B3C2 !important;
          color: #123456 !important;
          text-align: center;
          font-weight: bold;
        }
        .small-box .inner {
          text-align: center !important;
        }
        .small-box .inner p {
          font-size: 20px !important; 
          font-weight: bold;
        }
       .box.box-solid.box-primary .box-header{
        background-color: #96B3C2 !important;
        }
    "))
  ),
  tabItems(
    app1_ui,
    app2_ui,
    app3_ui
  )
)

# Assemble UI
ui <- dashboardPage(
  skin = "blue",
  dashboardHeader(title = "From Folk to Fame"),
  sidebar,
  body
)

# Server logic
server <- function(input, output, session) {
  app1_server(input, output, session)
  app2_server(input, output, session)
  app3_server(input, output, session)
}

# Launch app
shinyApp(ui, server)