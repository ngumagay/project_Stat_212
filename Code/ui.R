

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
  mutate(speeddiff = effective_speed - release_speed) %>%
  ungroup()

baseballdatacleanqual <- baseballdatacleanqual %>%
  mutate(zscore = (bat_speed - mean(bat_speed))/sd(bat_speed))


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
  tabPanel("Swing Speed vs. ERA",
           fluidPage(
             h3("Mean Z-Score vs. ERA"),
             plotlyOutput("mean_zscore_vs_era_plot")
           )
  ),
  
  # Pitch Metrics tab
  tabPanel("Obtain Pitch Metrics",
           fluidPage(
             sidebarLayout(
               sidebarPanel(
                 selectInput("filter_pitcher2", "Select Pitcher:", 
                             choices = unique(baseballdatacleanqual$PLAYERNAME), 
                             selected = unique(baseballdatacleanqual$PLAYERNAME)[1]),
                 selectInput("filter_pitch_type2", "Select Pitch Type:", 
                             choices = NULL,  # This will be updated dynamically
                             selected = NULL)  # Initially empty, will be set when pitcher is selected
               ),
               mainPanel(
                 plotOutput("swing_speed_distribution_plot"),
                 tableOutput("pitch_metric_table")
               )
             )
           )
  ), # End of Pitch Metrics tab
  
  # Variable vs Swing Speed tab
  tabPanel("Variable vs Swing Speed",
           fluidPage(
             sidebarLayout(
               sidebarPanel(
                 selectInput("x_var", "Select Variable for X-Axis:", 
                             choices = c("f_strike_percent", "n_fastball_formatted", "fastball_avg_speed", "fastball_avg_spin",
                                         "fastball_avg_break_x", "fastball_avg_break_z","n_breaking_formatted","breaking_avg_speed","breaking_avg_spin",
                                         "breaking_avg_break_x","breaking_avg_break_z","n_offspeed_formatted","offspeed_avg_speed",
                                         "offspeed_avg_spin","offspeed_avg_break_x","offspeed_avg_break_z"), 
                             selected = "n_fastball_formatted")
               ),
               mainPanel(
                 plotOutput("var_vs_swing_speed_plot"),
                 textOutput("r_squared_text")
               )
             )
           )
  ), # End of Variable vs Swing Speed tab
  
  # Batter Swing Comparison tab
  tabPanel("Pitch-Type Z-Score Comparison",
           fluidPage(
             sidebarLayout(
               sidebarPanel(
                 selectInput("filter_player", "Select Player:",
                             choices = unique(baseballdatacleanqual$PLAYERNAME),
                             selected = unique(baseballdatacleanqual$PLAYERNAME)[1])
               ),
               mainPanel(
                 plotOutput("z_score_comparison_pitchtype_plot", height = "600px", width = "100%")
               )
             )
           )
  )
) # End of navbarPage

# Server function
server <- function(input, output, session) {
  
  # Update pitch_name dropdown based on selected pitcher
  observe({
    # Filter pitches for the selected pitcher
    pitcher_pitches <- baseballdatacleanqual %>%
      filter(PLAYERNAME == input$filter_pitcher2) %>%
      distinct(pitch_name) %>%
      pull(pitch_name)  # Extract unique pitch types for the selected pitcher
    
    # Update the pitch_name selectInput choices based on selected pitcher
    updateSelectInput(session, "filter_pitch_type2", 
                      choices = pitcher_pitches, 
                      selected = pitcher_pitches[1])  # Optionally set initial selection to the first pitch
  })
  
  # Plot: Distribution of Swing Speeds for Selected Pitcher vs Overall
  output$swing_speed_distribution_plot <- renderPlot({
    # Filter data for the selected pitcher and pitch type
    selected_data <- baseballdatacleanqual %>%
      filter(PLAYERNAME == input$filter_pitcher2, pitch_name == input$filter_pitch_type2)
    
    # Filter overall data for the selected pitch type (all pitchers)
    overall_data <- baseballdatacleanqual %>%
      filter(pitch_name == input$filter_pitch_type2)
    
    # Plot both distributions
    ggplot() +
      geom_density(data = selected_data, aes(x = bat_speed, fill = "Selected Pitcher"), alpha = 0.5) +
      geom_density(data = overall_data, aes(x = bat_speed, fill = "Overall"), alpha = 0.5) +
      scale_fill_manual(values = c("Selected Pitcher" = "red", "Overall" = "blue")) +
      labs(
        title = paste("Distribution of Swing Speeds for", input$filter_pitcher2, "and Overall for", input$filter_pitch_type2),
        x = "Swing Speed",
        y = "Density",
        fill = "Category"
      ) +
      scale_x_continuous(limits = c(50, 95)) + 
      theme_minimal()
  })
  
  # Pitch Metrics Table
  output$pitch_metric_table <- renderTable({
    varsplot <- c("release_speed", "release_spin_rate", "pfx_x", "pfx_z")
    filtered_data <- baseballdatacleanqual %>%
      filter(PLAYERNAME == input$filter_pitcher2, pitch_name == input$filter_pitch_type2)
    filtered_means <- filtered_data %>%
      summarize(across(all_of(varsplot), ~mean(.x, na.rm = TRUE)))
    
    overall_means <- baseballdatacleanqual %>% 
      filter(pitch_name == input$filter_pitch_type2) %>%
      summarize(across(all_of(varsplot), ~mean(.x, na.rm = TRUE)))
    
    combined_table <- bind_rows(filtered_means, overall_means) %>%
      mutate(Type = c("Filtered", "Overall"))
    
    combined_table
  })
  
  # Mean Z-Score vs. ERA Plot
  output$mean_zscore_vs_era_plot <- renderPlotly({
    # Calculate mean Z-score for each pitcher
    pitcher_stats <- baseballdatacleanqual %>%
      group_by(PLAYERNAME, pitcher) %>%  # Ensure to group by pitcher ID for proper mean calculation
      summarize(
        mean_zscore = mean(zscore, na.rm = TRUE)
      ) %>%
      # Join with pitcherdata to get ERA for each pitcher
      left_join(pitcherdata, by = c("pitcher" = "player_id")) %>%
      filter(!is.na(p_era))  # Ensure ERA is available
    
    # Create scatterplot
    p <- ggplot(pitcher_stats, aes(x = mean_zscore, y = p_era, text = PLAYERNAME)) +
      geom_point(color = "steelblue", size = 3, alpha = 0.7) +
      geom_smooth(method = "lm", se = FALSE, color = "red", linetype = "dashed") +
      labs(
        title = "Mean Z-Score vs. Pitcher ERA",
        y = "ERA (Earned Run Average)",
        x = "Mean Z-Score of Bat Speed"
      ) +
      theme_minimal()
    
    # Convert to interactive plotly plot
    ggplotly(p, tooltip = c("text", "x", "y"))
  })
  
  # Pitch-Type Z-Score Comparison Plot
  output$z_score_comparison_pitchtype_plot <- renderPlot({
    # Filter data for the selected player
    player_data <- baseballdatacleanqual %>%
      filter(PLAYERNAME == input$filter_player)
    
    player_pitch_types <- unique(player_data$pitch_name)
    
    overall_data <- baseballdatacleanqual %>%
      filter(pitch_name %in% player_pitch_types)
    
    # Plot density plots, grouped by pitch type
    ggplot() +
      geom_density(data = overall_data, aes(x = zscore, fill = "Overall"), alpha = 0.3, color = NA) +
      geom_density(data = player_data, aes(x = zscore, fill = "Selected Player"), alpha = 0.3, color = NA) +
      scale_fill_manual(values = c("Overall" = "blue", "Selected Player" = "red")) +
      scale_x_continuous(
        limits = c(-4, 4),                 # Set x-axis range
        breaks = seq(-4, 4, by = 1)        # Set breaks
      ) +
      facet_wrap(~pitch_name) +            # Facet by pitch type
      labs(
        title = paste("Z-Score Distribution of Bat Speed by Pitch Type"),
        x = "Z-Score of Bat Speed",
        y = "Density",
        fill = "Category"
      ) +
      theme_minimal()
  })
  
  # Variable vs Swing Speed Scatterplot
  output$var_vs_swing_speed_plot <- renderPlot({
    ggplot(data= pitcherdata, aes(x = .data[[input$x_var]], y = avg_swing_speed)) +
      geom_point(color = "steelblue", size = 3, alpha = 0.7) +
      geom_smooth(method = "lm", se = FALSE, color = "red", linetype = "dashed") +
      labs(title = paste("Scatterplot of", input$x_var, "vs. Swing Speed"),
           x = input$x_var,
           y = "Swing Speed (mph)") +
      theme_minimal()
  })
  
  output$r_squared_text <- renderText({
    # Create a formula using the selected variable (input$x_var)
    formula <- as.formula(paste("avg_swing_speed ~", input$x_var))
    
    # Create the model using the formula
    model <- lm(formula, data = pitcherdata)
    
    # Get R-squared value
    r_squared <- summary(model)$r.squared
    
    # Display the R-squared value
    paste("R-squared value:", round(r_squared, 3))
  })
}

# Run the app
shinyApp(ui = ui, server = server)
