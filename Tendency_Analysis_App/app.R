#load packages
library(shiny)
library(nflfastR)
library(nflreadr)
library(dplyr)
library(ggplot2)
library(nflplotR)
library(stringr)
#install.packages('spinner')
#library(spinner)
library(gt)
library(gtExtras)
library(ggthemes)
library(shinybusy)
library(bslib)
library(thematic)
library(sysfonts)
library(showtext)

# Load the News Cycle font from Google Fonts
font_add_google("News Cycle", "news_cycle")
showtext_auto()

#bring in some data initially
sample_data <- nflfastR::load_pbp(2023)

teams <- sample_data %>%
  filter(is.na(posteam) == FALSE) %>%
  distinct(posteam) %>%
  arrange(posteam) %>%
  pull()

rm(sample_data)


# Define UI for application that draws a histogram
ui <- fluidPage(
  tags$head(
    tags$link(href = "https://fonts.googleapis.com/css2?family=News+Cycle:wght@400;700&display=swap", rel = "stylesheet")
  ),
  theme = bslib::bs_theme(bootswatch = 'journal', primary = "#007bff"),
  
  # Application title
  br(),
  titlePanel("NFL Tendency Analysis"),
  br(),
  
  uiOutput('team_image'),
  br(),
  
  # Sidebar with input controls
  sidebarLayout(
    sidebarPanel(
      numericInput('season', 'Enter season', 2024, min = 2016, max = 2024, step = 1),
      selectInput("team", "Select offensive team", c('', teams)),
      selectInput("down", "Select down", c('All', '1', '2', '3', '4')),
      sliderInput("distance", "Yards to First Down", min = 0, max = 40, value = c(0, 10), ticks = TRUE),
      sliderInput("field_location", "Yards to Goal Line", min = 0, max = 100, value = c(0, 100), ticks = TRUE),
      sliderInput("time", "Minutes remaining in regulation", min = 0, max = 60, value = c(0, 60), ticks = TRUE),
      sliderInput("win_prob", "Select win probability", min = 0, max = 100, value = c(0, 100), ticks = TRUE),
      selectInput('overtime', 'Include overtime data?', c('No', 'Yes')),
      selectInput("shotgun", "Select QB Pre-Snap Position", c('All', 'Shotgun', 'Under Center')),
      actionButton("goButton", "View")
    ),
    
    # Tabs layout for displaying content
    mainPanel(
      tabsetPanel(
        tabPanel("Tabular Data",
                 verbatimTextOutput("metrics_info"),
                 gt_output("play_counts"),
                 gt_output("all_tendencies")
        ),
        tabPanel("Trends",
                 plotOutput("plot")
        )
      )
    )
  ),
  add_busy_spinner(spin = "fading-circle")
)


# Define server logic
server <- function(input, output) {

  filtered_data_pull <- eventReactive(input$goButton, { 
    nflfastR::load_pbp(input$season) %>%
    mutate(minutes_remaining = game_seconds_remaining / 60) %>%
    mutate(play_outcome =
               case_when(sack == 1 ~ 'dropback resulting in sack',
                         qb_scramble == 1 ~ 'QB scramble',
                         pass == 1 & pass_length == 'deep' ~ 'deep pass',
                         pass == 1 & pass_length == 'short' ~ 'short pass',
                         rush == 1 & (run_location == 'middle' | run_gap == 'guard') ~ 'inside run',
                         rush == 1 & run_gap %in% c('tackle', 'end') ~ 'outside run')) %>%
      filter(
      !is.na(play_outcome),
      posteam == input$team,
           yardline_100 >= input$field_location[[1]],
           yardline_100 <= input$field_location[[2]],
           (input$down == 'All') | (down %in% input$down),
          (input$overtime == 'Yes' & ((qtr < 5 & minutes_remaining >= input$time[[1]] & minutes_remaining <= input$time[[2]]) | qtr == 5))  |
            (input$overtime == 'No' & qtr < 5 & minutes_remaining >= input$time[[1]] & minutes_remaining <= input$time[[2]]),
          ydstogo >= input$distance[[1]],
          ydstogo <= input$distance[[2]],
          wp*100 >= input$win_prob[[1]],
          wp*100 <= input$win_prob[[2]],
          (pass == 1 | rush == 1) & play_type %in% c('pass', 'run'),
          is.na(two_point_conv_result),
          is.na(epa) == FALSE,
      input$shotgun == 'All' | (input$shotgun == 'Under Center' & shotgun == 0) | (input$shotgun == 'Shotgun' & shotgun == 1))
    })
  
  league_data_pull <- eventReactive(input$goButton, { 
    nflfastR::load_pbp(input$season) %>%
      mutate(minutes_remaining = game_seconds_remaining / 60) %>%
      mutate(play_outcome =
               case_when(sack == 1 ~ 'dropback resulting in sack',
                         qb_scramble == 1 ~ 'QB scramble',
                         pass == 1 & pass_length == 'deep' ~ 'deep pass',
                         pass == 1 & pass_length == 'short' ~ 'short pass',
                         rush == 1 & (run_location == 'middle' | run_gap == 'guard') ~ 'inside run',
                         rush == 1 & run_gap %in% c('tackle', 'end') ~ 'outside run')) %>%
      filter(!is.na(play_outcome),
             yardline_100 >= input$field_location[[1]],
             yardline_100 <= input$field_location[[2]],
             (input$down == 'All') | (down %in% input$down),
             (input$overtime == 'Yes' & ((qtr < 5 & minutes_remaining >= input$time[[1]] & minutes_remaining <= input$time[[2]]) | qtr == 5))  |
               (input$overtime == 'No' & qtr < 5 & minutes_remaining >= input$time[[1]] & minutes_remaining <= input$time[[2]]),
             ydstogo >= input$distance[[1]],
             ydstogo <= input$distance[[2]],
             wp*100 >= input$win_prob[[1]],
             wp*100 <= input$win_prob[[2]],
             (pass == 1 | rush == 1) & play_type %in% c('pass', 'run'),
             is.na(epa) == FALSE,
             is.na(two_point_conv_result),
             input$shotgun == 'All' | (input$shotgun == 'Under Center' & shotgun == 0) | (input$shotgun == 'Shotgun' & shotgun == 1)) %>%
      mutate(success = ifelse(epa > 0,1,0)) %>%
      group_by(posteam) %>%
      mutate(total_plays = n_distinct(play_id, old_game_id)) %>%
      ungroup() %>%
      group_by(posteam, play_type) %>%
      mutate(play_count = n_distinct(play_id, old_game_id),
             avg_epa = mean(epa),
             success_rate = mean(success)) %>%
      mutate(pct = play_count/total_plays) %>%
      distinct(posteam, play_type,play_count, pct, avg_epa, success_rate) %>%
      group_by(play_type, posteam) %>%
      summarize(pct = round(mean(pct) * 100,2),
                # max_epa = round(max(avg_epa),2),
                # min_epa = round(min(avg_epa),2),
                # max_success_rate = round(max(success_rate) * 100, 2),
                # min_success_rate = round(min(success_rate) * 100, 2),
                avg_epa = round(mean(avg_epa),2),
                success_rate = round(mean(success_rate) * 100, 2)
               )
  })
  
  #get max and mins for color formatting
  max_pass_epa <- eventReactive(input$goButton, { league_data_pull() %>%
    filter(play_type == 'pass') %>%
    pull(max_epa) })
  
  min_pass_epa <- eventReactive(input$goButton, { league_data_pull() %>%
    filter(play_type == 'pass') %>%
    pull(min_epa) })
  
  max_pass_sr <- eventReactive(input$goButton, { league_data_pull() %>%
    filter(play_type == 'pass') %>%
    pull(max_success_rate) })
  
  min_pass_sr <- eventReactive(input$goButton, { league_data_pull() %>%
    filter(play_type == 'pass') %>%
    pull(min_success_rate) })
  
  max_rush_epa <- eventReactive(input$goButton, { league_data_pull() %>%
    filter(play_type == 'run') %>%
    pull(max_epa) })
  
  min_rush_epa <- eventReactive(input$goButton, { league_data_pull() %>%
    filter(play_type == 'run') %>%
    pull(min_epa) })
  
  max_rush_sr <- eventReactive(input$goButton, { league_data_pull() %>%
    filter(play_type == 'run') %>%
    pull(max_success_rate) })
  
  min_rush_sr <- eventReactive(input$goButton, { league_data_pull() %>%
    filter(play_type == 'run') %>%
    pull(min_success_rate) })
  
  team_logo <- eventReactive(input$goButton, {
    nflfastR::teams_colors_logos %>%
      filter(team_abbr == input$team) %>%
      distinct(team_color, team_color2, team_logo_wikipedia)})
  
  metric_info <- eventReactive(input$goButton, {
    nflfastR::teams_colors_logos %>%
      filter(team_abbr == input$team) %>%
      distinct(team_color, team_color2, team_logo_wikipedia)})

    
    
    output$plot <- renderPlot({
      
      passing_color <- team_logo() %>%
        distinct(team_color) %>%
        pull()
      
      rushing_color <- team_logo() %>%
        distinct(team_color2) %>%
        pull()
      
      custom_colors <- c(passing_color, rushing_color)
      
      return(ggplot(filtered_data_pull() %>%
                      group_by(week, play_type) %>%
                      summarize(average_epa = mean(epa)),
                    aes(x = factor(week), y = average_epa)) +
               
               # Create bars for each play_type per week
               geom_bar(aes(fill = play_type), stat = "identity", position = "dodge", alpha = 0.7) +
               
               # Adjust color and fill scales
               scale_color_manual(values = custom_colors) +
               scale_fill_manual(values = custom_colors) +
               
               # Switch to scale_x_discrete since 'week' is a factor
               scale_x_discrete(breaks = unique(filtered_data_pull()$week)) +
               
               # Labels and titles
               ggtitle("Performance Over the Course of the Season") +
               labs(x = "Week of the Season",
                    y = "EPA Per Play") +
               
               # Adjust theme elements
               theme_minimal(base_family = "news_cycle") +
               theme(
                 plot.title = element_text(face = "bold", size = 20),
                 axis.title = element_text(face = "bold", size = 14),
                 axis.text.x = element_text(size = 12),
                 axis.text.y = element_text(size = 12)
               )
             
             
      )
    })
    
    
    output$play_counts <- render_gt({
      tryCatch({
        # Pull and prepare the data
        filtered_data <- filtered_data_pull() %>%
          mutate(success = ifelse(epa > 0, 1, 0)) %>%
          group_by(posteam) %>%
          mutate(total_plays = n_distinct(play_id, old_game_id)) %>%
          ungroup() %>%
          group_by(play_type) %>%
          mutate(play_count = n_distinct(play_id, old_game_id),
                 avg_epa = round(mean(epa), 2),
                 success_rate = round(mean(success) * 100, 2)) %>%
          mutate(pct = round((play_count / total_plays) * 100, 2)) %>%
          distinct(play_type, play_count, team_pct = pct, team_avg_epa = avg_epa, team_success_rate = success_rate) %>%
          ungroup()
        
        # Check if "run" and "pass" play types exist
        run_exists <- "run" %in% filtered_data$play_type
        pass_exists <- "pass" %in% filtered_data$play_type
        
        # Calculate percentiles only for available play types
        filtered_data <- filtered_data %>%
          mutate(epa_percentile = case_when(
            play_type == 'run' ~ round(ecdf(league_data_pull() %>%
                                              filter(play_type == 'run') %>%
                                              pull(avg_epa))(team_avg_epa) * 100, 2),
            play_type == 'pass' ~ round(ecdf(league_data_pull() %>%
                                               filter(play_type == 'pass') %>%
                                               pull(avg_epa))(team_avg_epa) * 100, 2),
            TRUE ~ NA_real_
          ),
          sr_percentile = case_when(
            play_type == 'run' ~ round(ecdf(league_data_pull() %>%
                                              filter(play_type == 'run') %>%
                                              pull(success_rate))(team_success_rate) * 100, 2),
            play_type == 'pass' ~ round(ecdf(league_data_pull() %>%
                                               filter(play_type == 'pass') %>%
                                               pull(success_rate))(team_success_rate) * 100, 2),
            TRUE ~ NA_real_
          ))
        
        # Generate the gt table with 'Play Type' as a column
        gt_table <- filtered_data %>%
          select(`Play Type` = play_type, `Play Count` = play_count, `Play Type Percentages` = team_pct,
                 `Average EPA` = team_avg_epa, `EPA League Percentile` = epa_percentile, `Success Rate` = team_success_rate, 
                 `Success Rate League Percentile` = sr_percentile) %>%
          arrange(`Play Type`) %>%  # Ensure Play Type is properly ordered
          gt() %>%
          gt_theme_538() %>%
          tab_header(title = 'Team Summary')
        
        # Conditionally apply data_color based on the existence of 'pass' plays
        if (pass_exists) {
          gt_table <- gt_table %>%
            data_color(
              columns = `EPA League Percentile`,
              rows = `Play Type` == "pass",
              fn = scales::col_numeric(
                palette = c("red", "white", "green"),
                domain = c(0, 100)
              )
            ) %>%
            data_color(
              columns = `Success Rate League Percentile`,
              rows = `Play Type` == "pass",
              fn = scales::col_numeric(
                palette = c("red", "white", "green"),
                domain = c(0, 100)
              )
            )
        }
        
        # Conditionally apply data_color based on the existence of 'run' plays
        if (run_exists) {
          gt_table <- gt_table %>%
            data_color(
              columns = `EPA League Percentile`,
              rows = `Play Type` == "run",
              fn = scales::col_numeric(
                palette = c("red", "white", "green"),
                domain = c(0, 100)
              )
            ) %>%
            data_color(
              columns = `Success Rate League Percentile`,
              rows = `Play Type` == "run",
              fn = scales::col_numeric(
                palette = c("red", "white", "green"),
                domain = c(0, 100)
              )
            )
        }
        
        # Return the gt table
        return(gt_table)
        
      }, error = function(e) {
        # Return NULL to avoid crashing the app
        return(NULL)
      })
    })
    

  
  output$all_tendencies <- render_gt({
    return(filtered_data_pull() %>%
             mutate(play_outcome =
                      case_when(sack == 1 ~ 'dropback resulting in sack',
                                qb_scramble == 1 ~ 'QB scramble',
                                pass == 1 & pass_length == 'deep' ~ 'deep pass',
                                pass == 1 & pass_length == 'short' ~ 'short pass',
                                rush == 1 & (run_location == 'middle' | run_gap == 'guard') ~ 'inside run',
                                rush == 1 & run_gap %in% c('tackle', 'end') ~ 'outside run')) %>%
             filter(!is.na(play_outcome)) %>%
             group_by(posteam) %>%
             mutate(total_plays = n_distinct(play_id, old_game_id)) %>%
             ungroup() %>%
             mutate(success = ifelse(epa > 0,1,0)) %>%
             group_by(play_outcome) %>%
             mutate(play_count = n_distinct(play_id, old_game_id),
                    avg_epa = round(mean(epa),2),
                    success_rate = round(mean(success) * 100, 2)) %>%
             mutate(pct = round((play_count/total_plays)*100,2)) %>%
             distinct(pct, avg_epa, success_rate) %>%
             ungroup() %>%
             distinct(`Play Outcome` = play_outcome, `% of Plays in Scenario` = pct,
                      `Average EPA` = avg_epa, `Success Rate` = success_rate) %>%
             arrange(desc(`% of Plays in Scenario`)) %>%
             gt() %>%
             gt_theme_538() %>%
             gt_color_rows(`Average EPA`,
                           palette = c("red","white", "green"),
                           domain = c(-7, 7)) %>%
             gt_color_rows(`Success Rate`,
                           palette = c("red","white", "green"),
                           domain = c(0, 100)) %>%
             tab_header(title = 'Outcome Breakdown')
             )
  })
  
  output$team_image <- renderUI({
    imgurl2 <- team_logo()$team_logo_wikipedia
    tags$img(src = imgurl2,
         height = 100,
         width = 200)})
  
  output$metrics_info <- renderText({
    "
    EPA: Expected Points Added, how many points are likely to be added based on the result of the play
    Success Rate: Rate of plays gaining positive EPA
    "
  })


    
}

# Run the application 
shinyApp(ui = ui, server = server)
