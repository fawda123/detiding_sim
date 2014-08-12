library(shiny)

# Define UI for application
shinyUI(pageWithSidebar(
  
  # Application title
  headerPanel("Evaluation of simulation results"),
  
  # Sidebar with a slider input for number of observations
  sidebarPanel(
    
    selectInput(inputId = "tide_cat",
                label = "Tidal category",
                choices = c('Diurnal', 'Semidiurnal', 'Mixed Semidiurnal'),
                selected = 'Diurnal'),
    selectInput(inputId = "bio_in",
                label = "Amplitude of diel DO component (mg/L)",
                choices = c(0, 1, 2),
                selected = 2),
    selectInput(inputId = "assoc_in",
                label = "Amplitude of DO variation from tidal advection (mg/L)",
                choices = c(0, 1, 2),
                selected = 2),
    selectInput(inputId = "epro_in",
                label = "Magnitude of process uncertainty (mg/L)",
                choices = c(0, 1, 2),
                selected = 2),
    selectInput(inputId = "eobs_in",
                label = "Magnitude of observation uncertainty (mg/L)",
                choices = c(0, 1, 2),
                selected = 2),
    helpText("Select weighted regression windows to view:"),
    selectInput(inputId = "dectm_in",
                label = "Daily:",
                choices = c(1, 3, 8),
                selected = 1),
    selectInput(inputId = "hr_in",
                label = "Hourly:",
                choices = c(6, 12, 24),
                selected = 6),
    selectInput(inputId = "Td_in",
                label = "Tidal proportion:",
                choices = c(0.25, 0.50, 1),
                selected = 0.25)
  ),
  
  # Show a plot of the generated distribution
  mainPanel(
    plotOutput("simplot", width = "100%")
  )
))