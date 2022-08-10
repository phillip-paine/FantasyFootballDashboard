#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# To do:
# Fix name weird characters
# Create single table for predictions - filter position and value slider - add next three fixtures predictions + average
# lambda predictions are not good poisson model - home advantage is weird
# feature engineering, e.g. team strength between 0 and 100 for random forest etc

library(shiny)
library(shinyWidgets)
library(shinydashboard)
library(dashboardthemes)
library(ggplot2)
list_params <- fromJSON(file="set_parameters.json")

## Fixtures and Result:
df_results <- read.csv(paste("D:/Phillip/GitHub/FantasyFootball2122/Output/ResultsPredicted_", list_params$latest_gw, ".csv", sep = ""), stringsAsFactors = FALSE)
df_results <- df_results %>% filter(event == list_params$next_gw) %>% select(-X) %>% rename(Gameweek = event) %>% 
  select(home_team, Home_win, Draw, Away_win, away_team)
df_results <- df_results %>% mutate(Home_win = format(round(Home_win * 100, 0), nsmall = 0), 
                                    Draw = format(round(Draw * 100, 0), nsmall = 0), 
                                    Away_win = format(round(Away_win * 100, 0), nsmall = 0))
df_results <- df_results %>% rename(Home = home_team, "Home Win %" = Home_win, "Draw %" = Draw, "Away Win %" = Away_win, Away = away_team)

## League Table;
df_table <- read.csv(paste("D:/Phillip/GitHub/FantasyFootball2122/Output/TablePredicted_", list_params$latest_gw, ".csv", sep = ""), stringsAsFactors = FALSE)
# Reshape the table data so that the points are on column and the gameweek aggregate is filtered on:
df_table_long <- df_table %>% pivot_longer(cols = starts_with("Points_"), names_to = "Played", names_prefix = "Points_", values_to = "Points")
df_table_long <- df_table_long %>% arrange(Played, desc(Points))
df_table_long <- df_table_long %>% select(-X)
played_matches <- seq(list_params$latest_gw, 38, by = 1) # for slider
df_table_long <- df_table_long %>% rename(Team = name)

## Players:
df_players <- read.csv(paste("D:/Phillip/GitHub/FantasyFootball2122/Output/PlayersFFPredicted_", list_params$latest_gw, ".csv", sep = ""), stringsAsFactors = FALSE)
df_players$predicted_total_points <- if_else(is.na(df_players$predicted_total_points), 0.0, df_players$predicted_total_points)
# recode(column, val = new_val, etc..) if we want to replace value with a new one and keep all other existing
df_playersStats <- read.csv(paste("D:/Phillip/GitHub/FantasyFootball2122/Output/PlayersFFStats_", list_params$latest_gw, ".csv", sep = ""), stringsAsFactors = FALSE)
df_playersStats <- df_playersStats %>% mutate(value_avg_points = total_points / current_value)


###################################
## Theme:

# Theme colours:

customTheme <- shinyDashboardThemeDIY(
  ### general
  appFontFamily = "Arial"
  ,appFontColor = "#2D2D2D"
  ,primaryFontColor = "#0F0F0F"
  ,infoFontColor = "#0F0F0F"
  ,successFontColor = "#0F0F0F"
  ,warningFontColor = "#0F0F0F"
  ,dangerFontColor = "#0F0F0F"
  ,bodyBackColor = "#F0F0F0"
  
  ### header
  ,logoBackColor = "#3D1757"
  
  ,headerButtonBackColor = "#FFFFFF"
  ,headerButtonIconColor = "#3D1757"
  ,headerButtonBackColorHover = "#646464"
  ,headerButtonIconColorHover = "#3C3C3C"
  
  ,headerBackColor = "#3D1757"
  ,headerBoxShadowColor = ""
  ,headerBoxShadowSize = "0px 0px 0px"
  
  ### sidebar
  ,sidebarBackColor = "#3D1757"
  ,sidebarPadding = "0"
  
  ,sidebarMenuBackColor = "transparent"
  ,sidebarMenuPadding = "10"
  ,sidebarMenuBorderRadius = 0
  
  ,sidebarShadowRadius = ""
  ,sidebarShadowColor = "0px 0px 0px"
  
  ,sidebarUserTextColor = "#737373"
  
  ,sidebarSearchBackColor = "#F0F0F0"
  ,sidebarSearchIconColor = "#646464"
  ,sidebarSearchBorderColor = "#DCDCDC"
  
  ,sidebarTabTextColor = "#FFFFFF"
  ,sidebarTabTextSize = "30"
  ,sidebarTabBorderStyle = "none"
  ,sidebarTabBorderColor = "none"
  ,sidebarTabBorderWidth = "0"
  
  ,sidebarTabBackColorSelected = "#E6E6E6"
  ,sidebarTabTextColorSelected = "#000000"
  ,sidebarTabRadiusSelected = "0px"
  
  ,sidebarTabBackColorHover = "#FFFFFF"
  ,sidebarTabTextColorHover = "#FFFFFF"
  ,sidebarTabBorderStyleHover = "none solid none none"
  ,sidebarTabBorderColorHover = "#C8C8C8"
  ,sidebarTabBorderWidthHover = "4"
  ,sidebarTabRadiusHover = "0px"
  
  ### boxes
  ,boxBackColor = "#FFFFFF"
  ,boxBorderRadius = "5"
  ,boxShadowSize = "none"
  ,boxShadowColor = ""
  ,boxTitleSize = "18"
  ,boxDefaultColor = "#1D8515"
  ,boxPrimaryColor = "#5F9BD5"
  ,boxInfoColor = "#B4B4B4"
  ,boxSuccessColor = "#70AD47"
  ,boxWarningColor = "#ED7D31"
  ,boxDangerColor = "#E84C22"
  
  ,tabBoxTabColor = "#F8F8F8"
  ,tabBoxTabTextSize = "14"
  ,tabBoxTabTextColor = "#646464"
  ,tabBoxTabTextColorSelected = "#2D2D2D"
  ,tabBoxBackColor = "#F8F8F8"
  ,tabBoxHighlightColor = "#C8C8C8"
  ,tabBoxBorderRadius = "5"
  
  ### inputs
  ,buttonBackColor = "#D7D7D7"
  ,buttonTextColor = "#2D2D2D"
  ,buttonBorderColor = "#969696"
  ,buttonBorderRadius = "5"
  
  ,buttonBackColorHover = "#BEBEBE"
  ,buttonTextColorHover = "#000000"
  ,buttonBorderColorHover = "#969696"
  
  ,textboxBackColor = "#FFFFFF"
  ,textboxBorderColor = "#767676"
  ,textboxBorderRadius = "5"
  ,textboxBackColorSelect = "#F5F5F5"
  ,textboxBorderColorSelect = "#6C6C6C"
  
  ### tables
  ,tableBackColor = "#F8F8F8"
  ,tableBorderColor = "#1D8515"
  ,tableBorderTopSize = "1"
  ,tableBorderRowSize = "1"
)

css <- "
.sidebar-menu li:not(:last-child) {
  margin-bottom: 200px;
}
.main-sidebar { font-size: 16px; }
"


######################################
## Dashboard Components:

header <- dashboardHeader(title = "Modelling Premier League Fixtures and Forecasting Fantasy Football Points", titleWidth = 700)

sidebar <- dashboardSidebar(menuItem("Fixture Forecasting", tabName = "res", icon = icon("calendar-alt")),
                            menuItem("Fantasy Football Stats", tabName = "FFst", icon = icon("running")),
                            menuItem("Player Predictions", tabName = "FFprd", icon = icon("running")),
                            menuItem("About", tabName = "about", icon = icon("info"))
                            )

body <- dashboardBody(
  
  #tags$head( 
  #  tags$style(HTML(".main-sidebar { font-size: 16px; }")) #change the font size to 20
  #),
  
  tags$head(
    tags$style(HTML(".main-sidebar li { margin-top: 20px; }", ".main-sidebar { font-size: 16px; }",
                    ".main-sidebar > li.active > a { border-left-color: #FFFFFF;}"
                    # ".main-sidebar { color: #white; }"
                    ))
  ),
  
  #shinyDashboardThemes(
  #  theme = "grey_light"
  #),
  
  customTheme,
  
  #shinythemes::themeSelector(),
  
  tabItems(
    tabItem(tabName = "res", 
            titlePanel( title = "League Table and Next Fixture Forecast"),
            fluidRow(
              box( width = 5, title = "Forecasted League Table", tableOutput("table_league")),
              box( width = 7, title = paste("Forecasting Gameweek: ", list_params$next_gw, sep =""), tableOutput("table_results"))
            ),
            fluidRow(
              setSliderColor("#3D1757", sliderId = c(1)),
              box(
                width = 4, sliderInput(inputId = "games_played", label = "Gameweeks", min = list_params$latest_gw, max = 38,
                                       value = 38, step = 1, sep = ""))
            )
    ),
    
    tabItem(tabName = "FFst",
            titlePanel( title = "Current Fantasy Player Statistics"),
            fluidRow(
            #  box(width = 9, height = 500, title = "Total Points against Current Value", 
            #    plotOutput("TPS_Value", hover = hoverOpts(id = "tps_hover"))),
              box(width = 12, height = 500, title = "Points Against Current Value",
              tabsetPanel(
                tabPanel("Total Points", plotOutput("TPS_Value", hover = hoverOpts(id = "tps_hover"))), 
                tabPanel("Average Points per Value", plotOutput("AvgPS_Value", hover = hoverOpts(id = "avp_hover"))),
                tabPanel("Points Last Five Games", plotOutput("FormTPS_Value", hover = hoverOpts(id = "form_hover")))
              )
              )
            ),
            fluidRow(
              box( # use position to filter plot: 
                width = 4, pickerInput(inputId = "filter_pos", label = "Position Filter", choices = c("GK", "DEF", "MID", "FWD"), 
                                       selected = c("GK", "DEF", "MID", "FWD"), options = list(`actions-box` = TRUE), multiple = TRUE)
                                       
            ),
            box( title = "Hover Over Player:", width = 2, verbatimTextOutput("hover_info1") 
            ),
            box( title = "Hover Over Player:", width = 2, verbatimTextOutput("hover_info2") 
            ),
            box( title = "Hover Over Player:", width = 2, verbatimTextOutput("hover_info3") 
            )
            )
    ),
    
    tabItem(tabName = "FFprd",
            titlePanel( title = "Forecasted Fantasy Player Points"),
            fluidRow(
              box( # use position to filter plot:
                title = paste("Predicted Points Next Fixture: Gameweek ", list_params$next_gw, sep = ""),
                width = 12, height = 450, plotOutput("predict_TPS", hover = hoverOpts(id = "pred_tps_hover")))
              ),
              fluidRow(
                box( # use position to filter plot: 
                  width = 4, pickerInput(inputId = "filter_pos_pred", label = "Position Filter", choices = c("GK", "DEF", "MID", "FWD"), 
                                         selected = c("GK", "DEF", "MID", "FWD"), options = list(`actions-box` = TRUE), multiple = TRUE)
                  
              ),
              box( title = "Hover Over Player: ", width = 4, verbatimTextOutput("hover_info4") 
              )
              ),
              fluidRow(
                box(width = 3, style='width:400px;overflow-x: scroll;height:400px;overflow-y: scroll;',
                    title = "Predicted Points: GK", tableOutput("predict_gk")),
                box(width = 3, style='width:400px;overflow-x: scroll;height:400px;overflow-y: scroll;',
                    title = "Predicted Points: DEF", tableOutput("predict_df")),
                box(width = 3, style='width:400px;overflow-x: scroll;height:400px;overflow-y: scroll;',
                    title = "Predicted Points: MID", tableOutput("predict_mid")),
                box(width = 3, style='width:400px;overflow-x: scroll;height:400px;overflow-y: scroll;',
                    title = "Predicted Points: FWD", tableOutput("predict_fwd"))
              )
       
    ),
    
    tabItem(tabName = "about",
            titlePanel( title = "About and Forecasting Method"),
            fluidRow(
              
            )
    )
  )
)


# Define UI for application that draws a histogram
ui <- dashboardPage(
  header,
  sidebar,
  body
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  # Next Fixture Prediction:
  output$table_results <- renderTable(df_results)
  
  # Table Prediction: a reactive is not a dataframe, use df() to return dataframe from reactive named df, e.g.
  table_played <- reactive({ # create slider filtered dataset using reactive 
    df_table_long <- subset(df_table_long, Played == input$games_played)
  })
  output$table_league <- renderTable({
    table_played()
  })  # render_gt(expr = table_played) # uses data filtered by slider
  
  ## Fantasy Football Points Summary:
  
  player_stats <- reactive({
    df_playersStats <- subset(df_playersStats, position %in% input$filter_pos)
  })
  
  # Total Points:
  output$TPS_Value <- renderPlot({
    My_Theme = theme(
      axis.title.x = element_text(size = 16),
      axis.text.x = element_text(size = 12),
      axis.text.y = element_text(size = 12),
      axis.title.y = element_text(size = 16),
      legend.text=element_text(size=12),
      legend.title=element_text(size=14))
    ggplot(player_stats(), aes(x = total_points, y = current_value, color = position)) + 
      geom_point(aes(x = total_points, y = current_value, color = position)) + expand_limits(y=0) + 
      labs(x = "Total Points", y = "Current Value", color = "Position") + My_Theme
  })
  
  # Points per Value:
  output$AvgPS_Value <- renderPlot({
    My_Theme = theme(
      axis.title.x = element_text(size = 16),
      axis.text.x = element_text(size = 12),
      axis.text.y = element_text(size = 12),
      axis.title.y = element_text(size = 16),
      legend.text=element_text(size=12),
      legend.title=element_text(size=14))
    ggplot(player_stats(), aes(x = value_avg_points, y = current_value, color = position)) + 
      geom_point(aes(x = value_avg_points, y = current_value, color = position)) + expand_limits(y=0) + 
      labs(x = "Points per Value", y = "Current Value", color = "Position") + My_Theme
  })
  
  # Points Last 5:
  output$FormTPS_Value <- renderPlot({
    My_Theme = theme(
      axis.title.x = element_text(size = 16),
      axis.text.x = element_text(size = 12),
      axis.text.y = element_text(size = 12),
      axis.title.y = element_text(size = 16),
      legend.text=element_text(size=12),
      legend.title=element_text(size=14))
    ggplot(player_stats(), aes(x = form_total_points, y = current_value, color = position)) + 
      geom_point(aes(x = form_total_points, y = current_value, color = position)) + expand_limits(y=0) + 
      labs(x = "Total Points Last 5 Games", y = "Current Value", color = "Position") + My_Theme
  })
  
  
  output$hover_info1 <- renderPrint({ ## Create the hover text when mouse within X distance of point
    if(!is.null(input$tps_hover)){
      hover=input$tps_hover
      dist=sqrt((hover$x - df_playersStats$total_points)^2+(hover$y - df_playersStats$current_value)^2)
      cat("Player: \n")
      if(min(dist) < 3){
        df_playersStats$name[which.min(dist)]
        #df_playersStats$total_points[which.min(dist)]
        #df_playersStats$current_value[which.min(dist)]
      }
    }
  })
  
  output$hover_info2 <- renderPrint({ ## Create the hover text when mouse within X distance of point
    if(!is.null(input$avp_hover)){
      hover=input$avp_hover
      dist=sqrt((hover$x - df_playersStats$value_avg_points)^2+(hover$y - df_playersStats$current_value)^2)
      cat("Player: \n")
      if(min(dist) < 3){
        df_playersStats$name[which.min(dist)]
      }
    }
    
  })
  
  output$hover_info3 <- renderPrint({ ## Create the hover text when mouse within X distance of point
    if(!is.null(input$form_hover)){
      hover=input$form_hover
      dist=sqrt((hover$x - df_playersStats$form_total_points)^2+(hover$y - df_playersStats$current_value)^2)
      cat("Player: \n")
      if(min(dist) < 3){
        df_playersStats$name[which.min(dist)]
      }
    }
    
  })
  
  ## Fantasy Football Points Predictor:
  
  predict_players <- reactive({
    df_players <- subset(df_players, position %in% input$filter_pos_pred)
  })
  
  output$predict_TPS <- renderPlot({
    My_Theme = theme(
      axis.title.x = element_text(size = 16),
      axis.text.x = element_text(size = 12),
      axis.text.y = element_text(size = 12),
      axis.title.y = element_text(size = 16),
      legend.text=element_text(size=12),
      legend.title=element_text(size=14))
    ggplot(predict_players(), aes(x = predicted_total_points, y = current_value, color = position)) + 
      geom_point(aes(x = predicted_total_points, y = current_value, color = position)) + expand_limits(y=0) + 
      labs(x = "Predicted Points Next Fixture", y = "Current Value", color = "Position") + My_Theme
  })
  
  output$hover_info4 <- renderPrint({ ## Create the hover text when mouse within X distance of point
    if(!is.null(input$pred_tps_hover)){
      hover=input$pred_tps_hover
      dist=sqrt((hover$x - df_players$predicted_total_points)^2+(hover$y - df_players$current_value)^2)
      cat("Player: \n")
      if(min(dist) < 3){
        df_players$name[which.min(dist)]
      }
    }
    
  })
  
  # Tables for each position sorted by predicted total points - need team name too
  output$predict_gk <- renderTable({
    df_players %>% filter(position == "GK") %>% ungroup() %>% select(name, team, predicted_total_points, current_value) %>% mutate(team = substr(team, 1, 6)) %>%
       rename(Player = name, Team = team, Predicted = predicted_total_points, Value = current_value) %>% arrange(desc(Predicted))
  })
  
  output$predict_df <- renderTable({
    df_players %>% filter(position == "DEF") %>% ungroup() %>% select(name, team, predicted_total_points, current_value) %>% mutate(team = substr(team, 1, 6)) %>%
      rename(Player = name, Team = team, Predicted = predicted_total_points, Value = current_value) %>% arrange(desc(Predicted))
  })
  
  output$predict_mid <- renderTable({
    df_players %>% filter(position == "MID") %>% ungroup() %>% select(name, team, predicted_total_points, current_value) %>% mutate(team = substr(team, 1, 6)) %>%
      rename(Player = name, Team = team, Predicted = predicted_total_points, Value = current_value) %>% arrange(desc(Predicted))
  })
  
  output$predict_fwd <- renderTable({
    df_players %>% filter(position == "FWD") %>% ungroup() %>% select(name, team, predicted_total_points, current_value) %>% mutate(team = substr(team, 1, 6)) %>% 
      rename(Player = name, Team = team, Predicted = predicted_total_points, Value = current_value) %>% arrange(desc(Predicted))
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)

