

library(shiny)
library(tidyverse)
library(ggplot2)
library(dplyr)
library(gridExtra)

baseballdata <- read_csv("../data/swing_data.csv")

# Pitcher name data
playerdata <- read_csv("../data/player_data.csv")

# Data from entire 2024 season, only containing qualified pitchers
pitcherdata <- read_csv("../data/pitcher_data.csv")

# Choosing relevant variables from name dataset
playerdataclean <- playerdata %>%
  select(PLAYERNAME, LASTNAME, MLBID, THROWS)

baseballdataclean <- baseballdata %>%
  select(game_date, release_speed, player_name, events, zone, stand, bb_type, balls, strikes, pfx_x, pfx_z, outs_when_up, inning, hit_distance_sc, launch_angle, launch_speed, effective_speed, release_spin_rate, launch_speed_angle, estimated_ba_using_speedangle, estimated_woba_using_speedangle, woba_value, bat_score, fld_score, pitch_name, pitcher, bat_speed, swing_length, description)

baseballdataclean2 <- baseballdataclean %>% 
  left_join(playerdataclean, by = c("pitcher" = "MLBID"))

baseballdataclean2 <- baseballdataclean2 %>%
  mutate(pitch_name = recode(pitch_name, "4-Seam Fastball" = "4 Seam")) %>%
  mutate(pitch_name = recode(pitch_name, "Knuckle Curve" = "K Curve")) %>%
  mutate(pitch_name = recode(pitch_name, "Split-Finger" = "Splitter"))

baseballdataclean2 <- baseballdataclean2 %>%
  filter(bat_speed != 'NA') %>%
  filter(swing_length != 'NA') %>%
  filter(!description %in% c("ball", "called_strike", "blocked_ball", "pitchout", "hit_by_pitch", "foul_bunt","missed_bunt", "bunt_foul_tip"))

baseballdataclean2 <- baseballdataclean2 %>%
  filter(launch_speed != 'NA') %>%
  filter(PLAYERNAME != 'NA') %>%
  mutate(teamwinning = ifelse(bat_score > fld_score, TRUE, FALSE)) %>%
  mutate(sameside = ifelse(THROWS == stand, TRUE, FALSE)) %>%
  group_by(PLAYERNAME) %>%
  mutate(tot = n())

baseballdatacleanqual <- baseballdataclean2 %>%
  filter(tot > 50) %>%
  filter(!events %in% c("sac_bunt", "catcher_interf")) %>%
  mutate(speeddiff = effective_speed - release_speed)


ui <- navbarPage(
  title = "Swing Speed and Pitch Metrics Analysis",
  
  # Overview tab
  tabPanel("Overview",
           fluidPage(
             h3("Swing Speed and Pitch Metrics Analysis"),
             p("This app explores the relationships between swing speed, swing length, and pitch metrics."),
             p("Guiding Questions:"),
             tags$ul(
               tags$li("Do pitchers modify batters' behavior?"),
               tags$li("Do some pitchers elicit different swing speeds and swing lengths than others?"),
               tags$li("What causes changes in swing speed or length?")
             )
           )
  ), # End of Overview tab
  
  # Data Exploration tab
  tabPanel("Swing Speed Distribution",
           fluidPage(
             sidebarLayout(
               sidebarPanel(
                 selectInput("filter_pitcher", "Select Pitcher:", 
                             choices = unique(baseballdatacleanqual$PLAYERNAME), 
                             selected = unique(baseballdatacleanqual$PLAYERNAME)[1]),
                 selectInput("filter_pitch_type", "Select Pitch Type:", 
                             choices = unique(baseballdatacleanqual$pitch_name), 
                             selected = unique(baseballdatacleanqual$pitch_name)[1])
               ),
               mainPanel(
                 plotOutput("swing_speed_dist_plot")
               )
             )
           )
  ), # End of Swing Speed Distribution tab
  
  # Pitch Metrics tab
  tabPanel("Obtain Pitch Metrics",
           fluidPage(
             sidebarLayout(
               sidebarPanel(
                 selectInput("filter_pitcher2", "Select Pitcher:", 
                             choices = unique(baseballdatacleanqual$PLAYERNAME), 
                             selected = unique(baseballdatacleanqual$PLAYERNAME)[1]),
                 selectInput("filter_pitch_type2", "Select Pitch Type:", 
                             choices = unique(baseballdatacleanqual$pitch_name), 
                             selected = unique(baseballdatacleanqual$pitch_name)[1])
               ),
               mainPanel(
                tableOutput("pitch_metric_table")
               )
             )
           )
  ), # End of Pitch Metrics tab
  
  # Pitcher Comparison tab
  tabPanel("Compare Pitchers",
           fluidPage(
             sidebarLayout(
               sidebarPanel(
                 selectInput("compare_pitchers", "Select Two Pitchers to Compare:",
                             choices = unique(baseballdatacleanqual$PLAYERNAME),
                             selected = unique(baseballdatacleanqual$PLAYERNAME)[1:2],
                             multiple = TRUE)
               ),
               mainPanel(
                 plotOutput("pitcher_comparison_plot")
               )
             )
           )
  ) # End of Pitcher Comparison tab
) # End of navbarPage

# Define Server
server <- function(input, output) {
  
  # Swing Speed Distribution Plot
  output$swing_speed_dist_plot <- renderPlot({
    filtered_data <- baseballdatacleanqual %>%
      filter(PLAYERNAME == input$filter_pitcher, pitch_name == input$filter_pitch_type)
    
    ggplot(filtered_data, aes(x = bat_speed)) +
      geom_histogram(binwidth = 2, fill = "steelblue", color = "white", alpha = 0.8) +
      labs(title = paste("Swing Speed Distribution for", input$filter_pitcher, "-", input$filter_pitch_type),
           x = "Swing Speed",
           y = "Count") +
      theme_minimal()
  })

  
  output$pitch_metric_table <- renderTable({
    #browser()
    # Obtain Mean Metrics
    varsplot <- c("release_speed", "release_spin_rate", "pfx_x", "pfx_z")
    # Compute mean values for the filtered data
    filtered_data <- baseballdatacleanqual %>%
      filter(PLAYERNAME == input$filter_pitcher2, pitch_name == input$filter_pitch_type2)
    filtered_means <- filtered_data %>%
      summarize(across(all_of(varsplot), ~mean(.x, na.rm = TRUE))) %>% select(-PLAYERNAME)
    
    # Compute overall mean values for the entire dataset
    overall_means <- baseballdatacleanqual %>% ungroup() %>%
      filter(pitch_name == input$filter_pitch_type2) %>%
      summarize(across(all_of(varsplot), ~mean(.x, na.rm = TRUE)))
    
    
    combined_table <- bind_rows( filtered_means,overall_means) %>% mutate(Type = c('Filtered','Overall')) # Means for selected filters
 
    
    combined_table
  })
  
  # Compare Pitchers
  output$pitcher_comparison_plot <- renderPlot({
    comparison_data <- baseballdatacleanqual %>%
      filter(PLAYERNAME %in% input$compare_pitchers)
    
    ggplot(comparison_data, aes(x = bat_speed, fill = PLAYERNAME)) +
      geom_density(alpha = 0.6) +
      labs(title = "Comparison of Swing Speed Distributions",
           x = "Swing Speed",
           y = "Density") +
      theme_minimal()
  })
}

# Run the app
shinyApp(ui = ui, server = server)