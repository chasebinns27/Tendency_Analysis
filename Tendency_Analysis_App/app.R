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
      group_by(play_type) %>%
      summarize(pct = round(mean(pct) * 100,2),
                avg_epa = round(mean(avg_epa),2),
                success_rate = round(mean(success_rate) * 100, 2))
  })
  
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
        return(filtered_data_pull() %>%
                 mutate(success = ifelse(epa > 0,1,0)) %>%
                 group_by(posteam) %>%
                 mutate(total_plays = n_distinct(play_id, old_game_id)) %>%
                 ungroup() %>%
                 group_by(play_type) %>%
                 mutate(play_count = n_distinct(play_id, old_game_id),
                         avg_epa = round(mean(epa),2),
                        success_rate = round(mean(success)*100, 2)) %>%
                  mutate(pct = round((play_count/total_plays)*100,2)) %>%
                distinct(play_type, play_count, team_pct = pct, team_avg_epa = avg_epa, team_success_rate = success_rate) %>%
                 left_join(league_data_pull(), by = 'play_type') %>%
                 mutate(pct_difference = round((team_pct - pct),2),
                        epa_difference = round((team_avg_epa - avg_epa),2),
                        success_rate_difference = round((team_success_rate - success_rate),2)) %>%
                 select(`Play Type` = play_type, `Play Count` = play_count, `Play Type Percentages` = team_pct, 
                        `Average EPA` = team_avg_epa, `EPA Difference From League` = epa_difference, `Success Rate`= team_success_rate, `Success Rate Difference From League` = success_rate_difference) %>%
                 ungroup() %>%
                 arrange(`Play Type`) %>%
          gt() %>%
          gt_theme_538() %>%
          tab_header(title = 'Team Summary') %>%
                gt_color_rows(`EPA Difference From League`,
                              palette = c("red","white", "green"),
                              domain = c(-2, 2)) %>%
                gt_color_rows(`Success Rate Difference From League`,
                              palette = c("red", "white", "green"),
                              domain = c(-25, 25)))}, 
        
        error = function(e) {#return("Try new filters. The current filters do not produce enough data.")
                                return(NULL)  # Return NULL to prevent the output from being rendered
                              })
      })
    
    # output$league_counts <- render_gt({
    #   return(league_data_pull()  %>%
    #            arrange(play_type) %>%
    #            gt() %>%
    #            gt_theme_538() %>%
    #            tab_header(title = 'League Summary'))
    # })
  
  
  output$rushing_data <- render_gt({
    return(filtered_data_pull() %>%
             group_by(posteam) %>%
             mutate(total_plays = n_distinct(play_id, old_game_id)) %>%
             filter(rush == 1) %>%
             ungroup() %>%
             mutate(shotgun = ifelse(shotgun == 1, 'Yes', 'No')) %>%
             group_by(shotgun, run_location, run_gap) %>%
             mutate(play_count = n_distinct(play_id, old_game_id),
                    avg_epa = round(mean(epa),2)) %>%
             mutate(pct = round((play_count/total_plays)*100,2)) %>%
             distinct(pct, avg_epa) %>%
             ungroup() %>%
             mutate(run_gap = ifelse(run_location == 'middle', 'center', run_gap)) %>%
             distinct(`In Shotgun?` = shotgun, `Run Location` = run_location,
                      `Run Gap` = run_gap, `% of Plays in Scenario` = pct,
                      `Average EPA` = avg_epa) %>%
             arrange(desc(`% of Plays in Scenario`)) %>%
             slice(1:3) %>%
             gt() %>%
             gt_theme_538() %>%
             tab_header(title = 'Rushing Tendencies'))
  })
  
  output$passing_data <- render_gt({
    return(filtered_data_pull() %>%
             mutate(pass_length = ifelse(sack == 1 & is.na(pass_length) == TRUE, 'sack', pass_length),
                    pass_location = ifelse(sack == 1 & is.na(pass_location) == TRUE, 'sack', pass_location)) %>%
             mutate(
                    pass_length = ifelse(qb_scramble == 1 & is.na(pass_length) == TRUE, 'qb_scramble', pass_length),
                    pass_location = ifelse(qb_scramble == 1 & is.na(pass_location) == TRUE, 'qb_scramble', pass_location)) %>%
             group_by(posteam) %>%
             mutate(total_plays = n_distinct(play_id, old_game_id)) %>%
             filter(pass == 1) %>%
             ungroup() %>%
             mutate(shotgun = ifelse(shotgun == 1, 'Yes', 'No')) %>%
             group_by(shotgun, pass_length, pass_location) %>%
             mutate(play_count = n_distinct(play_id, old_game_id),
                    avg_epa = round(mean(epa),2)) %>%
             mutate(pct = round((play_count/total_plays)*100,2)) %>%
             distinct(pct, avg_epa) %>%
             ungroup() %>%
             distinct(`In Shotgun?` = shotgun, `Pass Length` = pass_length,
                      `Pass Location` = pass_location, `% of Plays in Scenario` = pct,
                      `Average EPA` = avg_epa) %>%
             arrange(desc(`% of Plays in Scenario`)) %>%
             slice(1:3) %>%
             gt() %>%
             gt_theme_538() %>%
             tab_header(title = 'Dropback Tendencies'))
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
