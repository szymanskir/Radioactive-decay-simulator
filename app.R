library(ggplot2)
library(plotly)
library(shiny)
library(shinycssloaders)
library(shinythemes)

atoms <- data.frame(
  row.names = c("Americium", "Radium", "Plutonium", "Promethium", "Thallium", "Cobalt", "Tritium", "Carbon"),
  name = c("Americium", "Radium", "Plutonium", "Promethium", "Thallium", "Cobalt", "Tritium", "Carbon"),
  half_life = c(432, 1599, 24113, 2.623, 3.8, 5.27, 12.3, 5715)
)

create_exp_decay_function <- function(atoms_number, half_life) {
    function(time) {
        atoms_number * (2 ^ (-time/half_life))
    }
}

create_decay_probability_function <- function(half_file) {
    function(time_interval) {
        1 - 2 ^ (-time_interval/half_file)
    }
}

get_theoretical_results <- function(atoms_number, half_life, time_step, time_step_count) {
    exp_decay_function <- create_exp_decay_function(atoms_number, half_life)
    time_seq <- seq(0, time_step * time_step_count, time_step)
    exp_decay_function(time_seq)
}

run_simulation <- function(atoms_number, half_life, time_step, time_step_count) {
    decay_probability_function <- create_decay_probability_function(half_life)
    decay_probability <- decay_probability_function(time_step)
    current_atoms_number <- atoms_number
    simulation_counts <- c()
    time_seq <- seq(0, time_step * time_step_count, time_step)
        
    for (time in time_seq) {
        simulation_counts <- c(simulation_counts, current_atoms_number)
        simulation_probabilities <- runif(n = current_atoms_number)
        decayed_atoms_count <- sum(simulation_probabilities < decay_probability)
        current_atoms_number <- current_atoms_number - decayed_atoms_count
    }
    
    simulation_counts
}

ui <- navbarPage(
    "Radioactive decay simulator",
    theme = shinytheme("cosmo"),
    tabPanel("Simulator",
         sidebarLayout(
             sidebarPanel(
                 selectInput(
                     inputId = "atom",
                     label = "Atom",
                     choices = atoms$name
                 ),
                 numericInput(
                     inputId = "initial_atoms_number",
                     label = "Initial number of atoms",
                     min = 1,
                     value = 5000
                 ),
                 numericInput(
                     inputId = "time_step",
                     label = "Simulation time step (in years)",
                     min = 1,
                     value = 1000
                 ),
                 numericInput(
                     inputId = "time_step_count",
                     label = "Simulation time (number of time steps)",
                     min = 1,
                     value = 5
                 ),
                 actionButton(
                     inputId = "simulation_start",
                     label = "Perform simulation"
                 )
             ),
             
             mainPanel(
                 withSpinner(plotlyOutput("simulation_plot")),
                 DT::dataTableOutput("simulation_data")
             )
         )
    )
)


server <- function(input, output, session) {
  simulation_plot <- reactiveVal()
  simulation_result_df <- reactiveVal()
  
  observeEvent(input$simulation_start, {
    validate(
      need(input$initial_atoms_number > 0,
           "The initial number of atoms has to be greater than 0!"),
      need(input$time_step_count > 0,
           "The number of time steps has to be greater than 0!")
    )
    
    half_life <- atoms[input$atom, "half_life"]
    
    theoretical_counts <- get_theoretical_results(
      atoms_number = input$initial_atoms_number,
      half_life = half_life,
      time_step = input$time_step,
      time_step_count = input$time_step_count
    )
    
    simulation_counts <- run_simulation(
      atoms_number = input$initial_atoms_number,
      half_life = half_life,
      time_step = input$time_step,
      time_step_count = input$time_step_count
    )
    
    simulation_results <- data.frame(
      step = rep(seq(0, input$time_step * input$time_step_count, input$time_step), 2),
      atoms_count = c(theoretical_counts, simulation_counts),
      measurement_type = c(
        rep("theory", length(theoretical_counts)),
        rep("simulation", length(simulation_counts))
      )
    )
    
    max_time <- input$time_step * input$time_step_count
    half_life <- atoms[input$atom, "half_life"]
    
    p <- ggplot(simulation_results, mapping = aes(x = step, y = atoms_count, color = measurement_type)) +
      geom_point() +
      geom_line() + 
      scale_x_continuous(breaks = seq(0, max_time, by = half_life))
    
    simulation_result_df(simulation_results)
    simulation_plot(p)
  })
  
   output$simulation_plot <- renderPlotly({
     req(simulation_plot())
     ggplotly(simulation_plot())
   })
   
   output$simulation_data <- DT::renderDataTable({
     req(simulation_result_df())
     DT::datatable(simulation_result_df())
   })
   
}

shinyApp(ui = ui, server = server)
