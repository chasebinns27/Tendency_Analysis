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
    # Note the wrapping of the string in HTML()
    tags$style(HTML("
      @import url('https://fonts.googleapis.com/css2?family=Chivo+Mono:wght@700&display=swap');
      h2 {
        font-family: 'Chivo Mono', monospace;
      }
      .siderbar h2 {
        font-family: 'Chivo Mono', monospace;
      }"))
  ),

    # Application title
    titlePanel("NFL Tendency Analysis"),
    
    br(),
  
    uiOutput('team_image'),
    
    br(),
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
          numericInput('season',
                      'Enter season',
                      2023, min = 2016, max = 2023, step = 1),
          selectInput("team",
                      "Select offensive team",
                      c('',teams)),
          sliderInput("field_location",
                      "Yards to Goal Line",
                      min = 0,
                      max = 100,
                      value = c(0,100),
                      ticks = TRUE),
          sliderInput("time",
                      "Minutes remaining in regulation",
                      min = 0,
                      max = 60,
                      value = c(0, 60),
                      ticks = TRUE),
          selectInput('overtime',
                      'Include overtime data?',
                      c('No', 'Yes')),
          selectInput("down",
                      "Select down",
                      c('All','1','2','3','4')),
          sliderInput("distance",
                      "Yards to First Down",
                      min = 0,
                      max = 40,
                      value = c(0,10),
                      ticks = TRUE),
          sliderInput("win_prob",
                      "Select win probability",
                      min = 0,
                      max = 100,
                      value = c(0,100),
                      ticks = TRUE),
          actionButton("goButton","View")
        ),

        # Show a plot of the generated distribution
        mainPanel(
          # fluidRow(
          #   # column(2, uiOutput('team_image')),
          #   #column(6, img(src = "https://upload.wikimedia.org/wikipedia/en/thumb/7/72/Arizona_Cardinals_logo.svg/179px-Arizona_Cardinals_logo.svg.png", width = 100, height = 100)),
          # 
          #   # column(5, img(src = "https://www.freepnglogos.com/uploads/nfl-logo-png-0.png", width = 100, height = 100))
          # ),
          verbatimTextOutput("metrics_info"),
          gt_output("play_counts"),
          # fluidRow(
          #   column(7,gt_output("play_counts")),
          #   column(5, gt_output("league_counts"))),
          gt_output("passing_data"),
          gt_output("rushing_data"),
          br(),
          shiny::plotOutput("plot")
        )
    ),
  add_busy_spinner(spin = "fading-circle")
)

# Define server logic
server <- function(input, output) {

  filtered_data_pull <- eventReactive(input$goButton, { 
    nflfastR::load_pbp(input$season) %>%
    mutate(minutes_remaining = game_seconds_remaining / 60) %>%
    filter(posteam == input$team,
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
          is.na(epa) == FALSE)
    })
  
  league_data_pull <- eventReactive(input$goButton, { 
    nflfastR::load_pbp(input$season) %>%
      mutate(minutes_remaining = game_seconds_remaining / 60) %>%
      filter(
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
             is.na(epa) == FALSE) %>%
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
                      summarize(average_epa = mean(epa)),aes(x = week, y = average_epa, fill = play_type, color = play_type)) + geom_point() + 
               scale_color_manual(values = custom_colors) +
               scale_x_continuous(breaks = filtered_data_pull()$week) + geom_smooth(se = FALSE) +
               ggtitle("Performance Over the Course of the Season") +
               labs(x = "Week of the Season",
                    y = "EPA Per Play") +
               theme_minimal() +
               theme(
                 plot.title = element_text(face = "bold", size = 20),  # Make title bold and big
                 axis.title = element_text(face = "bold", size = 14) , # Make axis titles bold
                 axis.text.x = element_text(size = 12),  # Adjust the size as needed
                 axis.text.y = element_text(size = 12) 
               ))
    })
    
  # output$pass_play_counts <- render_gt({
  #     team_data <- filtered_data_pull() %>%
  #              mutate(success = ifelse(epa > 0,1,0)) %>%
  #              group_by(posteam) %>%
  #              mutate(total_plays = n_distinct(play_id, old_game_id)) %>%
  #              ungroup() %>%
  #              group_by(play_type) %>%
  #              mutate(play_count = n_distinct(play_id, old_game_id),
  #                      avg_epa = mean(epa),
  #                     success_rate = mean(success)) %>%
  #               mutate(pct = play_count/total_plays) %>%
  #             distinct(play_type, play_count, pct, avg_epa, success_rate) %>%
  #              ungroup()
  #     
  #     league_pass_epa_value <- league_data_pull() %>%
  #       filter(play_type == 'pass') %>%
  #       pull(avg_epa)
  #     
  #     league_pass_success_rate <- league_data_pull() %>%
  #       filter(play_type == 'pass') %>%
  #       pull(success_rate)
  #     
  #     team_pass_epa_value <- team_data %>%
  #       filter(play_type == 'pass') %>%
  #       pull(avg_epa)
  #     
  #     team_pass_success_rate <- team_data %>%
  #       filter(play_type == 'pass') %>%
  #       pull(success_rate)
  #     
  #     min_pass_epa <- min(c(league_pass_epa_value, team_pass_epa_value))
  #     
  #     max_pass_epa <- max(c(league_pass_epa_value, team_pass_epa_value))
  #     
  #     min_pass_success_rate <- min(c(league_pass_success_rate, team_pass_success_rate))
  #     
  #     max_pass_success_rate <- max(c(league_pass_success_rate, team_pass_success_rate))
  # 
  #     
  #     return(team_data %>%
  #              filter(play_type == 'pass') %>%
  #              gt() %>%
  #              gt_theme_538() %>%
  #              gt_color_rows(avg_epa,
  #                            palette = c("red", "green"),
  #                            domain = c(min_pass_epa, max_pass_epa)) %>%
  #              gt_color_rows(success_rate,
  #                            palette = c("red", "green"),
  #                            domain = c(min_pass_success_rate, max_pass_success_rate)) %>%
  #              tab_header(title = 'Team Passing Summary'))
  #     
  #     
  #     
  #     
  #   })
  # 
  # output$rush_play_counts <- render_gt({
  #   team_data <- filtered_data_pull() %>%
  #     mutate(success = ifelse(epa > 0,1,0)) %>%
  #     group_by(posteam) %>%
  #     mutate(total_plays = n_distinct(play_id, old_game_id)) %>%
  #     ungroup() %>%
  #     group_by(play_type) %>%
  #     mutate(play_count = n_distinct(play_id, old_game_id),
  #            avg_epa = mean(epa),
  #            success_rate = mean(success)) %>%
  #     mutate(pct = play_count/total_plays) %>%
  #     distinct(play_type, play_count, pct, avg_epa, success_rate) %>%
  #     ungroup()
  #   
  #   league_rush_epa_value <- league_data_pull() %>%
  #     filter(play_type == 'run') %>%
  #     pull(avg_epa)
  #   
  #   league_rush_success_rate <- league_data_pull() %>%
  #     filter(play_type == 'run') %>%
  #     pull(success_rate)
  #   
  #   team_rush_epa_value <- team_data %>%
  #     filter(play_type == 'run') %>%
  #     pull(avg_epa)
  #   
  #   team_rush_success_rate <- team_data %>%
  #     filter(play_type == 'run') %>%
  #     pull(success_rate)
  #   
  #   min_rush_epa <- min(c(league_rush_epa_value, team_rush_epa_value))
  #   
  #   max_rush_epa <- max(c(league_rush_epa_value, team_rush_epa_value))
  #   
  #   min_rush_success_rate <- min(c(league_rush_success_rate, team_rush_success_rate))
  #   
  #   max_rush_success_rate <- max(c(league_rush_success_rate, team_rush_success_rate))
  #   
  #   return(team_data %>%
  #     filter(play_type == 'run') %>%
  #     gt() %>%
  #     gt_theme_538() %>%
  #     gt_color_rows(avg_epa,
  #                   palette = c("red", "green"),
  #                   domain = c(min_rush_epa, max_rush_epa)) %>%
  #     gt_color_rows(success_rate,
  #                   palette = c("red", "green"),
  #                   domain = c(min_rush_success_rate, max_rush_success_rate)) %>%
  #       tab_header(title = 'Team Rushing Summary'))
  # })
  # 
  # 
  # 
  # output$pass_league_counts <- render_gt({
  #   return(league_data_pull()  %>%
  #            filter(play_type == 'pass') %>%
  #            gt() %>%
  #            gt_theme_538() %>%
  #            tab_header(title = 'League Passing Summary'))
  # })
  # 
  # output$rush_league_counts <- render_gt({
  #   return(league_data_pull()  %>%
  #            filter(play_type == 'run') %>%
  #            gt() %>%
  #            gt_theme_538() %>%
  #            tab_header(title = 'League Rushing Summary'))
  # })
    
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
