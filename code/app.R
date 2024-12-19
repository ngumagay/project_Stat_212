

library(shiny)
library(tidyverse)
library(ggplot2)
library(dplyr)
library(gridExtra)
library(plotly)

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

meanbatspeeds <- baseballdatacleanqual %>%
  group_by(player_name) %>%
  summarize(meanbatspeed = mean(bat_speed, na.rm = TRUE))

baseballdatacleanqual <- baseballdatacleanqual %>%
  left_join(meanbatspeeds)

sdbatspeeds <- baseballdatacleanqual %>%
  group_by(player_name) %>%
  summarize(sdbatspeed = sd(bat_speed, na.rm = TRUE))

baseballdatacleanqual <- baseballdatacleanqual %>%
  left_join(sdbatspeeds)


baseballdatacleanqual <- baseballdatacleanqual %>%
  group_by(player_name) %>%
  mutate(zscore = (bat_speed - meanbatspeed) / sdbatspeed) %>%
  ungroup()


ui <- navbarPage(
  title = "Swing Speed and Pitch Metrics Analysis",
  
  # Overview tab
  tabPanel("Overview",
           fluidPage(
             h3("Swing Speed and Pitch Metrics Analysis"),
             p("Created By: Nathan Gumagay, Hayes Waddell, and Peter Ward"),
             p("This app explores the relationships between swing speed and pitch metrics."),
             fluidRow(
               column(width = 6, 
                      p("Looking at the correlations between pitch metrics and bat speed across the overall population, 
                       the effects were minimal. This finding highlights that in baseball, there is no 'one size fits all' 
                       approach to success. Each pitcher is unique in how they throw the ball, the pitch types they use, 
                       and the specific metrics associated with those pitches. The limited overall correlation emphasizes 
                       the need to analyze individual pitchers’ metrics to better understand their impact on bat speed. 
                       By examining these individualized metrics, we can gain insights into what each pitcher does well and 
                       identify areas for improvement. This analysis can help pitchers determine which pitches to use more 
                       frequently to maximize success and which pitches may need refinement. Additionally, it provides valuable 
                       information about how specific aspects of a pitcher’s repertoire influence bat speed, offering actionable 
                       strategies to enhance performance.")
                    )
               ),
             p("Guiding Questions:"),
             tags$ul(
               tags$li("Do pitchers modify batters' behavior?"),
               tags$li("Do some pitchers elicit different swing speeds than others?"),
               tags$li("What causes changes in swing speed?")
             ),
             div(style = "position: absolute; top: 200px; right: 20px; margin-right: 20px; width: 620px; text-align: left;",
                 p("This scatter plot highlights direct correlations between the average bat speed against a pitcher and their success in 4 key stats. 
                  Most importantly, as bat speed increases, pitchers allow more runs. As bat speed increases, players hit the ball harder, strike out less, and make more high-quality contact (barrels). 
                  While these plots do not give the full picture, as there are a large number of factors contributing to pitching success (ERA), the strong correlations between bat speed and these metrics 
                  provide a rationale to investigate how individual pitchers can limit bat speed against them to increase their success."),
                 tags$img(src = "bat_speed_scatter.png", 
                          height = "400px", 
                          width = "600px", 
                          alt = "Data Site"))
           )
  ),
  
  # Glossary tab
  tabPanel("Glossary",
           fluidPage(
             h3("Key Terms"),
             tags$ul(
               tags$li("Swing Speed: How fast the sweet spot of the bat is moving, in mph, at the point of contact with the ball"),
               tags$li("pfx_x: Horizontal movement of a pitch in feet from the catcher's perspective"),
               tags$li("pfx_z: Vertical movement of a pitch in feet from the catcher's perspective"),
               tags$li("release_spin_rate: The rate of spin on a baseball after it is released. It is measured in revolutions per minute"),
               tags$li("release_speed: The maximum speed of a given pitch at any point from its release to the time it crosses home plate"),
               tags$li("era: The number of earned runs a pitcher allows per nine innings")
             ),
             h3("Pitch Types and Descriptions:"),
             tags$ul(
               tags$li("Fastball: The most common pitch in baseball, characterized by its high speed and straight trajectory"),
               tags$li("Slider: A breaking pitch in baseball that moves sideways and breaks sharply as it approaches the batter"),
               tags$li("Curveball: A breaking pitch in baseball and softball that has a large, loopy up-and-down movement"),
               tags$li("Changeup: A baseball pitch that's thrown at a slower speed than a fastball but with the same trajectory"),
               tags$li("Sinker: A type of fastball in baseball that has a downward and horizontal movement, and is known for inducing ground balls"),
               tags$li("Cutter: A type of fastball that moves horizontally toward the pitcher's glove side as it reaches home plate"),
               tags$li("Splitter: Thrown with the effort of a fastball, but it will drop sharply as it nears home plate")
             ),
             div(style = "position: absolute; top: 170px; right: 150px; text-align: center;",
                 tags$img(src = "collect.png", height = "300px", width = "300px", alt = "Collect Stats"),
                 br(),
                 tags$img(src = "grips.jpg", height = "300px", width = "300px", alt = "Pitch Grips")
             )
           )
  ),
  
  # Pitch Metrics tab
  tabPanel("Individual Pitch Metrics",
           fluidPage(
             sidebarLayout(
               sidebarPanel(
                 selectInput("filter_pitcher2", "Select Pitcher:", 
                             choices = unique(baseballdatacleanqual$PLAYERNAME), 
                             selected = unique(baseballdatacleanqual$PLAYERNAME)[1]),
                 selectInput("filter_pitch_type2", "Select Pitch Type:", 
                             choices = NULL,  # Updated dynamically
                             selected = NULL)
               ),
               mainPanel(
                 plotOutput("swing_speed_distribution_plot"),
                 tableOutput("pitch_metric_table")
               )
             ),
             p("This tab emphasizes the pitcher’s individual pitch metrics by comparing one of their specific pitches to the average MLB pitcher’s. First, on the graph we see the distribution of swing speeds 
that the specific pitch omits. Then just below we compare the four most important pitch metrics that we decided on. Pitch speed, spin rate, horizontal break, and vertical break. Comparing those four metrics with the specified pitcher’s specific pitch and the average MLB pitcher’s specific pitch. We are able to draw conclusions on each pitcher’s pitches in their respective pitch arsenal by comparing it to the average MLB pitcher’s.
")
           )
  ),
  
  # Pitcher Metric Correlations tab
  tabPanel("Pitcher Metric Correlations",
           fluidPage(
             sidebarLayout(
               sidebarPanel(
                 selectInput(
                   "correlation_pitcher", "Select Pitcher:",
                   choices = unique(baseballdatacleanqual$PLAYERNAME),
                   selected = unique(baseballdatacleanqual$PLAYERNAME)[1]
                 )
               ),
               mainPanel(
                 tableOutput("correlation_table"),
                 plotOutput("correlation_plot")
               )
             ),
             p("This tab is where we drew most of our conclusions for each of our pitchers. The graph we made on this tab allows us to see how each pitch impacted the pitcher’s z-score in our four pitcher categories. Each pitch was pitted against each category then gave us each category's correlation with the pitcher’s z-score on that pitch. We then highlighted these correlations with our table. The table shows the pitcher’s pitch arsenal, then in the second column is the pitch metric that has the strongest influence on the pitch. Positive or negative, If there is a positive correlation between the metric and the pitch it tells us that the pitch’s specific metric is leading to higher swing speeds. With negative being the opposite. We can draw conclusions on each pitcher and make a strong case as to what metrics on each of their pitches is giving certain swing speed results.")
           )
  ),
  
  # Pitcher Arsenal Z-Score Comparison tab
  tabPanel("Pitcher Arsenal Z-Score Comparison",
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
             ),
             p("We decided to take a look into the z-score of each pitcher. Which was calculated through the batter’s perspective, as we were able to gain data on each batter’s average swing speed. Then calculating each pitcher’s swing speed against each batter. We gained their raw score and using the z score formula calculated the z score against all the batters in general. Our tab represents the z-score for each pitcher’s pitch arsenal compared to the z-score of the overall pitcher distribution of the z-score for the exact same pitch arsenal. Allows us to make conclusions about the difference in swing speed change the pitchers in our dataset have compared to the average MLB pitcher.")
           )
  ),
  
  # Swing Speed Z Score vs. ERA tab
  tabPanel("Swing Speed Z Score vs. ERA",
           fluidPage(
             h3("Mean Z-Score vs. ERA"),
             plotlyOutput("mean_zscore_vs_era_plot"),
             p("This tab allows us to look at the z-score that we calculated on a broader level compared to the previous tab. Looking at the overall z-score of the pitcher, their score will determine on average how much their pitches affect the swing speed of the batter. In the negative range would mean on average, batters will swing slower and positive would mean the opposite. Then throwing this variable onto a graph against pitcher ERA, we tried to see if there was any correlation between the two variables. The graph showed a small correlation but we concluded that it wasn’t enough to make a strong conclusion. But, this furthered our hypothesis that the factors going into pitchers limiting bat speed was more than just the pitcher.")
           )
  ),
  
  # Pitch Metrics Comparison by Z-Score tab
  tabPanel("Pitch Metrics Comparison by Z-Score",
           fluidPage(
             sidebarLayout(
               sidebarPanel(
                 selectInput("metric", "Select Metric to Compare:",
                             choices = c("release_speed", "release_spin_rate", "pfx_x", "pfx_z"),
                             selected = "release_speed")
               ),
               mainPanel(
                 plotlyOutput("zscore_comparison_plot"),
                 textOutput("comparison_text")
               )
             ),
             p("This tab changes the point of analysis in our Shiny app, instead of looking at specific pitchers, we switched focus to looking at specific metrics. By choosing each metric we are able to compare the average across the league to the z-scores. Then, a comparison of Negative and Positive z-scores, looking at what the average is for each of the two types of z-score. Through this tab we can make league wide conclusions on what the pitch metric for certain pitches would look like. But, taking into consideration that there are limitations to the graph because of how much variation there is when analyzing pitching. That there is no specific way a pitcher will be successful especially when it comes to trying to execute statistical specificities.")
           )
  ),
  
  #Overall Average Swing Speed Against Distribution vs. Pitcher Average Swing Speed Against Distribution
  tabPanel("Overall Avg SSA vs Specificed Pitcher SSA",
           fluidPage(
             sidebarLayout(
               sidebarPanel(
                 selectInput("compared_pitcher", "Select Pitcher:",
                             choices = unique(baseballdatacleanqual$PLAYERNAME),
                             selected = unique(baseballdatacleanqual$PLAYERNAME)[1])
               ),
               mainPanel(
                 plotOutput("comparison_plot") 
               )
             ),
             p("This tab gives us a surface level analyzation of how pitchers can affect a batter’s swing speed. Giving us specific comparisons between each selected pitcher and the average MLB pitcher. The distributions between the two density plots will give us the different swing speeds that they induced. The conclusions that we see is that the pitchers that are distributed towards the lower swing speeds compared to the overall distribution were above average to elite pitchers.")
           )
  ),
  
  # Count and Pitch Type tab
  tabPanel("Count and Pitch Type",
           fluidPage(
             sidebarLayout(
               sidebarPanel(
                 selectInput("filter_balls2", "Balls:", 
                             choices = unique(baseballdatacleanqual$balls),
                             selected = unique(baseballdatacleanqual$balls)[1]),
                 selectInput("filter_strikes2", "Strikes:", 
                             choices = unique(baseballdatacleanqual$strikes), 
                             selected = unique(baseballdatacleanqual$strikes)[1]),
                 selectInput("filter_pitch_type3", "Select Pitch Type:", 
                             choices = unique(baseballdatacleanqual$pitch_name),
                             selected = unique(baseballdatacleanqual$pitch_name)[1])
               ),
               mainPanel(
                 plotOutput("count_pitch_type_plot")
               )
             ),
             p("We wanted to add this tab because it puts swing speed into a very different perspective than what we have been looking at. It is super important to look at the swing speed of specific batters on certain counts because first, we can make definitive conclusions. Second, understand certain factors that are crucial to a batter’s tendencies. These graphs show three factors, number of balls, number of strikes, and pitch type.These stats are combined to show the distribution of swing speeds in these specific scenarios. Which are compared to the overall swing speeds, where we can see drastic differences in distribution in different circumstances. We conclude that the batter’s swing speed will change on certain circumstances and especially on certain pitch types. Which is also an important factor to the reasons we can see these differences in swing speeds by batters around the league.")
           )
  )
)
server <- function(input, output, session) {
  
  #Plot specific pitcher's bat speed against to Overall distribution of bat speed against.
  output$comparison_plot <- renderPlot({
    
    #Filter input pitcher as PLAYERNAME and NA values
    compared_pitcher_data <- baseballdatacleanqual %>%
      filter(PLAYERNAME == input$compared_pitcher, !is.na(bat_speed))
    
    #Filter all the NA values
    compared_overall_data <- baseballdatacleanqual %>%
      filter(!is.na(bat_speed))
    
    #Plot pitcher against overall
    ggplot() +
      geom_density(data = compared_overall_data, aes(x = bat_speed, fill = "Overall"), alpha = 1) +
      geom_density(data = compared_pitcher_data, aes(x = bat_speed, fill = "Compared Pitcher"), alpha = 0.5) +
      labs(title = paste("Comparing Swing Speed Against: Distribution of", input$compared_pitcher, " vs Overall Distribution"),
           x = "Swing Speed Against",
           y = "Frequency") +
      xlim(50,100) +
      scale_fill_manual(values = c("Compared Pitcher" = "blue", "Overall" = "red")) +
      theme_minimal()
  })
  
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
        title = paste("Z-Score Distribution of Bat Speed Against by Pitch Type for", input$filter_player),
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
  # Correlation Table
  output$correlation_table <- renderTable({
    pitcher_data <- baseballdatacleanqual %>%
      filter(PLAYERNAME == input$correlation_pitcher)
    
    correlations <- pitcher_data %>%
      group_by(pitch_name) %>%
      summarise(across(c(release_speed, release_spin_rate, pfx_x, pfx_z), 
                       ~cor(.x, zscore, use = "complete.obs"), 
                       .names = "{col}")) %>%
      pivot_longer(cols = -pitch_name, names_to = "Metric", values_to = "Correlation") %>%
      group_by(pitch_name) %>%
      slice_max(abs(Correlation), n = 1)
    
    correlations
  })
  
  # Correlation Plot
  output$correlation_plot <- renderPlot({
    pitcher_data <- baseballdatacleanqual %>%
      filter(PLAYERNAME == input$correlation_pitcher)
    
    correlations <- pitcher_data %>%
      group_by(pitch_name) %>%
      summarise(across(c(release_speed, release_spin_rate, pfx_x, pfx_z), 
                       ~cor(.x, zscore, use = "complete.obs"), 
                       .names = "{col}")) %>%
      pivot_longer(cols = -pitch_name, names_to = "Metric", values_to = "Correlation")
    
    ggplot(correlations, aes(x = Metric, y = Correlation, fill = pitch_name)) +
      geom_bar(stat = "identity", position = "dodge") +
      labs(
        title = paste("Correlation of Z-Score of Swing Speed Against vs Metrics by Pitch Type for", input$correlation_pitcher),
        x = "Metric",
        y = "Correlation"
      ) +
      theme_minimal() +
      scale_fill_brewer(palette = "Set3")
  })
  
  filtered_data <- reactive({
    # Calculate the mean z-score for each pitcher
    mean_zscore_data <- baseballdatacleanqual %>%
      group_by(PLAYERNAME) %>%
      summarise(mean_zscore = mean(zscore, na.rm = TRUE))
    
    # Join the mean z-score back to the base data
    merged_data2 <- baseballdatacleanqual %>%
      left_join(mean_zscore_data, by = "PLAYERNAME")
    
    # Split into two subsets: positive and negative mean z-scores
    positive_zscore_data <- merged_data2 %>%
      filter(mean_zscore > 0)
    
    negative_zscore_data <- merged_data2 %>%
      filter(mean_zscore <= 0)
    
    list(positive = positive_zscore_data, negative = negative_zscore_data)
  })
  
  # Render the plot comparing the selected metric for positive and negative mean z-scores
  output$zscore_comparison_plot <- renderPlotly({
    data <- filtered_data()
    
    # Prepare the data for the selected metric
    positive_data <- data$positive %>%
      group_by(pitch_name) %>%
      summarise(mean_value = mean(get(input$metric), na.rm = TRUE)) %>%
      mutate(zscore_sign = "Positive Z-Score")
    
    negative_data <- data$negative %>%
      group_by(pitch_name) %>%
      summarise(mean_value = mean(get(input$metric), na.rm = TRUE)) %>%
      mutate(zscore_sign = "Negative Z-Score")
    
    # Combine both datasets
    combined_data <- bind_rows(positive_data, negative_data)
    
    combined_data <- combined_data %>%
      filter(!pitch_name %in% c("Forkball", "Knuckleball", "Slow Curve", "Slurve"))
    
    
    # Plot the comparison using column plots
    p <- ggplot(combined_data, aes(x = pitch_name, y = mean_value, fill = zscore_sign)) +
      geom_col(position = "dodge") +
      labs(title = paste("Comparison of", input$metric, "for Positive vs. Negative Z-Scores by Pitch Type"),
           x = "Pitch Type",
           y = paste("Average", input$metric)) +
      scale_fill_manual(values = c("Positive Z-Score" = "blue", "Negative Z-Score" = "red")) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      scale_x_discrete(expand = c(0, 0)) +  # Remove extra space on the x-axis
      scale_y_continuous(expand = c(0, 0))  # Remove extra space on the y-axis
    
    
    ggplotly(p)  # Make the plot interactive with Plotly
  })

  # Plot: Distribution of Swing Speeds for Count vs Pitch Type
  output$count_pitch_type_plot <- renderPlot({
    # Filter data for the selected count and pitch type
    type_count_data <- baseballdatacleanqual %>%
      filter(pitch_name == input$filter_pitch_type3, balls == input$filter_balls2, strikes == input$filter_strikes2)
    
    # Filter overall data for the selected pitch type and count(all pitchers)
    count_data <- baseballdatacleanqual %>%
      filter(balls == input$filter_balls2, strikes == input$filter_strikes2)
    
    #mean bat speed
    avg_bat_speed <- mean(baseballdatacleanqual$bat_speed)
      
    
    # Plot both distributions
    ggplot() +
      geom_density(data = type_count_data, aes(x = bat_speed, fill = "Pitch Type"), alpha = 0.5) +
      geom_density(data = count_data, aes(x = bat_speed, fill = "Overall"), alpha = 0.5) +
      geom_vline(aes(xintercept = 70.210429369938)) +
      scale_fill_manual(values = c("Pitch Type" = "red", "Overall" = "blue")) +
      labs(
        title = paste("Distribution of Swing Speeds for", input$filter_balls2, "-", input$filter_strikes2, "Count"),
        x = "Swing Speed",
        y = "Density",
        fill = "Category",
        subtitle = "Vertical line represents MLB average swing speed for all counts and pitch types"
      ) +
      scale_x_continuous(limits = c(50, 95)) + 
      theme_minimal()
    
  })
}

# Run the app
shinyApp(ui = ui, server = server)
