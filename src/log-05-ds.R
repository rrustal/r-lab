# Data Science Log #5
# Authors: Rrustem Sallauka

library(shiny)
library(ggplot2)
library(tidyverse)
library(shinythemes)


setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# This function just takes a path to a csv-file and stores that data in the "pop_data" variable. This will be usefull throughout the whole app.
load_and_prepare_data <- function(path_to_file) {
  pop_data <- read_csv(path_to_file)

  pop_data <- pop_data %>%
    mutate(age_group = gsub("pop_", "", age_group),
           age_group = gsub("_", "-", age_group),
           age_group = factor(age_group, levels = unique(age_group[order(as.numeric(str_extract(age_group, "^[0-9]+")))]))) %>%
    mutate(year = factor(year, levels = unique(year)))

}

# The UI is one of two necessary parts of a functioning shiny app.
# This is the place where the user interface gets defined and stored for the app to use.


# This function defines an UI object used to create the shiny app.
ui <- navbarPage(
  title = "Germany's Population in Age Groups over the years", #app title
  theme = shinytheme("flatly"),
  tabPanel("Main",
           sidebarLayout(
             sidebarPanel(
               textOutput("authors"), # authors added
               uiOutput("source"),
               sliderInput("year", "Select Year", min = 1950, max = 2021, value = 1950, sep = ""), # slider to select the year 
               selectInput("drop", "Select Datasubset", choices = c("total","male", "female")), # choose subset 
               numericInput("bin", "Bin size (years):", value = 1, min = 1)
             ),
             mainPanel(
               tabsetPanel(
                 tabPanel("Histogram", plotOutput("histogramPlot"),  verbatimTextOutput("population_summary")),
                 tabPanel("Binned histogram", plotOutput("binnedHistogram")),
                 tabPanel("Comparison", plotOutput("plot"))
               )
             )
           )
  ),
  tabPanel("Info",
    mainPanel(
      tabsetPanel(
        tabPanel("Manual", htmlOutput("manual")),
        tabPanel("Changes", htmlOutput("changes"))
      )
    )
  )
)

# The following code sets up the server, the second necessary part for the shiny app.
# It defines the backend of the app.

server <- function(input, output) {

  pop_data <- load_and_prepare_data("data/pop_data_1950-2021.csv")

  url <- a("Federal Statistical Office of Germany", href="https://service.destatis.de/bevoelkerungspyramide/index.html#!")
  output$source <- renderUI({
    tagList("Data source:", url)
  })
  
  output$authors <- renderText({
    "Authors are: Rrustem Sallauka and Benedikt Zimmermann"
  })  
  
  # Filter data for specific year
  pop_year <- reactive({
    pop_data %>% filter(year == input$year) %>% 
    mutate(population = .data[[input$drop]]) #dynamically select the population subset based on user input from the dropdown
  })

  # updated histogram plot
  output$histogramPlot <- renderPlot({
    ggplot(pop_year(), aes(x = age_group, y = population)) +
      geom_bar(stat = "identity", fill = "skyblue") +
      labs(
        title = paste("Population by Age Groups in",input$year),
        x = "Age Groups",
        y = "Total population count"
      ) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
  })
  
  # The Manual
  output$manual <- renderUI({
    HTML("
    <h3>How to use the app</h3>
    <p>
      You can choose between different plots, which show Germany's Population in Age Groups over the years.
    </p>
    <p>
    - Histogram: Shows population by predefined age groups as bars
    </p>
    <p>
    - Binned Histogram: Shows population by ages groups using user-defined bins
    </p> 
    <p>
    - Line Plot: Compares male, female, and total population over time
    </p>
    <p>
      Use the slider to select a year between 1950 and 2021, so a year's population is displayed in the histogram and the subset which you'd like to see.
    </p>
    <p>
    For the Binned Histogram enter the bin size you want.
    </p>
    <p>
      The histogram serves as a visualization of the age distribution in Germany.
    </p>
    <p>
      In the dropdown menu you can choose between male, female and total. The selected option will be shown in the histogram.
    </p>
     The population of older age groups has increased, indicating an aging society in Germany.
     The younger age groups tend to shrink in more recent years, suggesting lower birth rates. 
     When comparing male and female populations, 
     This could be due to better healthcare and therefore higher life expectancy and lower fertility rates, which is a common theme in most western societies.
    </p>
  ")
  })
  
  output$changes <- renderUI({
    HTML("
    <h3>Made changes</h3>
    <p>
    - Created a new plot, where male, female and total are in one line plot.
    </p>
    <p>
    - The total number of people in the given year was added just below the histogram
    </p>
    <p>
    - Changed the design by adding navigation bar, sidebar and theme.
    </p>
  ")
  })
  # binning the Data
  binned_data <- reactive({
    df <- pop_year()
    
    df <- df %>%
      mutate(
        start_age = as.numeric(str_extract(age_group, "^[0-9]+")), # extract the starting age from the age group string, so the age group 0-1 is age 0-, age group 1-2 is 1- and so on
        bin = floor(start_age / input$bin) * input$bin  # Assign each age to a bin with the binsize
      ) %>%
      group_by(bin) %>%
      summarise(total = sum(population, na.rm = TRUE)) %>% 
      mutate(bin_label = paste0(bin, "-", bin + input$bin - 1)) # age groups
  })
  # Plot for the Binned histogram
  output$binnedHistogram <- renderPlot({
    ggplot(binned_data(), aes(x = bin_label, y = total)) +
      geom_bar(stat = "identity", fill = "steelblue") +
      labs(
        title = paste("Binned Age Groups in", input$year),
        x = "Age Group (Binned)",
        y = "Population"
      ) +
      theme_minimal()
  })
  
  #Line-Plot
  output$plot <- renderPlot({
    pop_h <- pop_data %>%
      filter(year == input$year) %>%
      pivot_longer(cols = c(male, female, total),
                   names_to = "gender",
                   values_to = "val")
    
    ggplot(pop_h, aes(x = age_group, y = val, color = gender, group = gender)) +
      geom_line(linewidth = 1) +
      labs(
        title = paste("Population by Age Groups in", input$year),
        x = "Age Groups",
        y = "Total population count",
        color = "Group"
      ) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
  })
  
  # Data used to display the total number of people in Germany
  output$population_summary <- renderText({
    total_pop <- pop_year() %>% summarise(total = sum(total)) %>% pull(total)
    actual_pop <- total_pop * 1000  # Convert thousands to actual numbers
    paste("The total population in Germany in", input$year, "was", format(actual_pop, big.mark = ","))
  })
  
}

shinyApp(ui = ui, server = server)