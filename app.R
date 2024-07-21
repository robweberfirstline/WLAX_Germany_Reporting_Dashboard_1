##################### 2024 Germany Women's Dashboard #####################

############## Setup ##############

#### Librarying ####

library(shiny) # base
library(tidyverse) # data manipulation, etc
library(shinydashboard) # aesthetics
library(shinyWidgets) # fancy boxes and inputs
library(shinycssloaders) # loading spinners
library(plotly) # viz
library(png) # reading png files
library(scales) # for "percent" function
library(lubridate) # to treat game dates correctly
library(DT) #datatable controls
library(leaflet) # munual continuous color scales

#### Options ####

options(warn=-1)
options(dplyr.summarise.inform = FALSE)

#### Reading in External Data/Info ####

df_master <- read.csv("WLAX_Worlds_Master_File.csv", header = T, stringsAsFactors = F)

df_team_info <- read.csv("Team_Info.csv", header = T, stringsAsFactors = F)

goal_marker_coords <- read.csv("Lacrosse-Goal-2_MarkerCoords.csv", header = T, stringsAsFactors = F)

field_marker_coords <- read.csv("Lacrosse-Field-2_Grid_MarkerCoords.csv", header = T, stringsAsFactors = F)

#### Plot Pictures ####

goal_base <- "www/Lacrosse-Goal-2.png"
field_base <- "www/Lacrosse-Field-2.png"
field_grid <- "www/Lacrosse-Field-2_Grid.png"
field_transparent <- "www/Lacrosse-Field-2_Transparent.png"
shot_angle <- "www/Release Point for Tracking_6zones.png"

#### Parameters ####

# Report team
report_team_row <- which(df_team_info$Team == "Germany")

# Game list
df_master$Game <- paste(df_master$Game_Date, ": ", df_master$Home_Team, " vs. ", df_master$Away_Team, sep = "")
game_list <- df_master %>% 
  group_by(Game_Date, Game) %>% 
  count() %>% 
  ungroup() %>% 
  select(-n) %>% 
  mutate(Game_Date = as.Date(Game_Date, "%m/%d/%Y")) %>% 
  arrange(desc(Game_Date), Game)
game_list <- game_list$Game

# Starting Game Row
starting_game_row <- 1

# dates data frame object for server
df_dates <- df_master %>% 
  group_by(Game_Date, Game, Home_Team, Away_Team) %>% 
  count() %>% 
  ungroup() %>% 
  select(-n)

# Goalie List
all_goalies <- unique(c(df_master$Home_Goalie_Name, df_master$Away_Goalie_Name))

# Field Location Bin df
fieldloc_bins <- data.frame("Bin" = c("Back Right", "Back Left", "Right Back", "Left Back", 
                                      "1", "2", "3", 
                                      "4", "5", "6", 
                                      "7", "8", "9", 
                                      "Right Front", "Left Front", "Crease Right", "Crease Left", 
                                      "Behind the Net", "Right Under", "Left Under"), 
                            "x_max" = c(0.729, 0.729, 0.845, 0.845, 
                                        0.788, 0.788, 0.788, 
                                        0.845, 0.845, 0.845, 
                                        0.902, 0.902, 0.902, 
                                        0.99, 0.99, 0.99, 0.99, 
                                        2, 2, 2), 
                            "x_min" = c(-1, -1, 0.729, 0.729, 
                                        0.729, 0.729, 0.729, 
                                        0.788, 0.788, 0.788, 
                                        0.845, 0.845, 0.845, 
                                        0.845, 0.845, 0.902, 0.902, 
                                        0.99, 0.99, 0.99), 
                            "y_max" = c(2, 0.49, 2, 0.15, 
                                        0.372, 0.6181, 0.8505, 
                                        0.372, 0.6181, 0.8505, 
                                        0.372, 0.6181, 0.8505, 
                                        2, 0.15, 0.8505, 0.49, 
                                        0.696, 2, 0.299), 
                            "y_min" = c(0.49, -1, 0.8505, -1, 
                                        0.15, 0.372, 0.6181, 
                                        0.15, 0.372, 0.6181, 
                                        0.15, 0.372, 0.6181, 
                                        0.8505, -1, 0.49, 0.15, 
                                        0.299, 0.696, -1), 
                            stringsAsFactors = F)

# Shot On, Off, and Post lists
shot_on <- c("5-Hole", "Left Leg", "Left Shoulder", "Under Left Arm", "Right Leg", "Right Shoulder", "Under Right Arm")
shot_off <- c("Left Miss", "Right Miss", "Top Miss")
shot_post <- c("Crossbar", "Left Post", "Right Post")

# General shooting percentages
gen_shoot_per <- length(which(df_master$Event_Type == "Shot" & (df_master$Result == "Goal" | df_master$Result == "Miss")))
gen_shoot_per <- length(which(df_master$Event_Type == "Shot" & df_master$Result == "Goal")) / gen_shoot_per
shoton_shoot_per <- length(which(df_master$Event_Type == "Shot" & df_master$Net_Location %in% shot_on))
shoton_shoot_per <- length(which(df_master$Result == "Goal")) / shoton_shoot_per

# Need to add some image sourcing objects to the team info df
df_team_info$HTML <- "none"
for(i in 1:nrow(df_team_info)) {
  df_team_info$HTML[i] <- sprintf(paste("<img src = '", df_team_info$Logo_Path[i], 
                                        "' width = 30px><div class = 'jhr'>%s</div></img>", sep = ""), 
                                  df_team_info$Full_Name[i])
}

# Field bin Dataframe
field_df.b <- data.frame("Field_Location_Bin" = c("Back Right", "Back Left", "Right Back", "Left Back", 
                                                  "1", "2", "3", "4", "5", "6", "7", "8", "9", 
                                                  "Right Front", "Left Front", "Crease Right", "Crease Left", 
                                                  "Behind the Net", "Right Under", "Left Under"), 
                         stringsAsFactors = F)

# Shot Location Dataframe
goal_info.b <- data.frame("Net_Location" = rep(c("5-Hole", "Chest", "Crossbar", "Empty Net",
                                                 "Left Leg", "Left Miss", "Left Shoulder", "Left Post", "Under Left Arm", 
                                                 "Right Leg", "Right Miss", "Right Shoulder", "Right Post", "Under Right Arm", 
                                                 "Top Miss")), 
                          stringsAsFactors = F)

# Shot Angle bin Dataframe
shotangle_info.b <- data.frame("Shot_Angle_Bin" = c("Dive/Other", "LowRight", "MidRight", "HighRight", "HighLeft", "MidLeft", "LowLeft"), 
                               stringsAsFactors = F)

shot_angle_coords <- data.frame("Shot_Angle_Bin" = c("Dive/Other", "LowRight", "MidRight", "HighRight", "HighLeft", "MidLeft", "LowLeft"), 
                                "Dot_Loc_x" = c(0.227, 0.74, 0.975, 0.74, 0.26, 0.025, 0.26), 
                                "Dot_Loc_y" = c(0.435, 0.11, 0.50, 0.89, 0.89, 0.50, 0.11), 
                                stringsAsFactors = F)

# Game Clock Chunked Ranges
c("Quarter 1 [15:00-10:00]", 
  "Quarter 1 [10:00-5:00]", 
  "Quarter 1 [5:00-End]", 
  "Quarter 2 [15:00-10:00]", 
  "Quarter 2 [10:00-5:00]", 
  "Quarter 2 [5:00-End]", 
  "Quarter 3 [15:00-10:00]", 
  "Quarter 3 [10:00-5:00]", 
  "Quarter 3 [5:00-End]", 
  "Quarter 4 [15:00-10:00]", 
  "Quarter 4 [10:00-5:00]", 
  "Quarter 4 [5:00-End]", 
  "Overtime") -> game_clock_chunked

# Pull in png pixel sizes
df_team_info$Logo_Path_Height <- 0
df_team_info$Logo_Path_Width <- 0
df_team_info$Shadow_Logo_Path_Height <- 0
df_team_info$Shadow_Logo_Path_Width <- 0
df_team_info$Wordmark_Path_Height <- 0
df_team_info$Wordmark_Path_Width <- 0
for(i in 1:nrow(df_team_info)) {
  png_dim <- dim(readPNG(paste("www/", df_team_info$Logo_Path[i], sep = "")))
  df_team_info$Logo_Path_Height[i] <- png_dim[1]
  df_team_info$Logo_Path_Width[i] <- png_dim[2]
  
  png_dim <- dim(readPNG(paste("www/", df_team_info$Shadow_Logo_Path[i], sep = "")))
  df_team_info$Shadow_Logo_Path_Height[i] <- png_dim[1]
  df_team_info$Shadow_Logo_Path_Width[i] <- png_dim[2]
  
  png_dim <- dim(readPNG(paste("www/", df_team_info$Wordmark_Path[i], sep = "")))
  df_team_info$Wordmark_Path_Height[i] <- png_dim[1]
  df_team_info$Wordmark_Path_Width[i] <- png_dim[2]
}

############## UI ##############

ui <- fluidPage(
  
  #### Aesthetics ####
  
  includeCSS("www/custom.css"), 
  
  #### Top Grid ####
  
  br(), # probably remove this once launched 
  
  uiOutput(outputId = "top_grid"), 
  
  br(), br(), 
  
  #### Navigation Bar ####
  
  navbarPage(
    title = "", 
    id = "navbar", 
    position = "static-top", 
    inverse = TRUE, 
    
    #### Boxscore ####
    
    tabPanel(
      id = "boxscore", title = "BOXSCORE", 
      
      uiOutput(outputId = "boxscore_presubmit")
    ), 
    
    #### Scoring ####
    
    tabPanel(
      id = "scoring", title = "SCORING", 
      
      uiOutput(outputId = "scoring_presubmit")
    ), 
    
    #### REB / DEF ####
    
    tabPanel(
      id = "rdf", title = "REBOUNDS / DEFENSE / FACEOFFS", 
      
      uiOutput(outputId = "rdf_presubmit")
    ), 
    
    #### Player Profile ####
    
    tabPanel(
      id = "playprof", title = "PLAYER PROFILE", 
      
      uiOutput(outputId = "playprof_presubmit")
    )
  )
)



############## Server ##############

server <- function(input, output, session) {
  
  #### Reactive Values (RV) ####
  
  ##### RV: Data Selectors #####
  
  # if the given selection button is pressed, it triggers the reactive variable to get changed to that selection
  
  data_sel <- reactiveValues("Current_Selection" = "Team Summary")
  
  observeEvent(input$team_summary_bttn, {
    if(data_sel$Current_Selection != "Team Summary") {
      data_sel$Current_Selection <- "Team Summary"
    }
  })
  
  observeEvent(input$team_vs_team_bttn, {
    if(data_sel$Current_Selection != "Team vs. Team") {
      data_sel$Current_Selection <- "Team vs. Team"
    }
  })
  
  observeEvent(input$individual_game_bttn, {
    if(data_sel$Current_Selection != "Ind Game") {
      data_sel$Current_Selection <- "Ind Game"
    }
  })
  
  ##### RV: Side Variables #####
  
  # based on the data selection choices, there will always be two sides for viz. 
  # this will determine the logos and the colors used throughout the dashboard
  # the df_team_info corresponding rows will get stored in reactive variables
  
  side_vars <- reactiveValues("Side1_Row" = "none", 
                              "Side2_Row" = "none")
  
  # need the length part of these conditions to keep it from feeding into the functions before the value exists
  observeEvent(input$team_summary_submit, {
    if(data_sel$Current_Selection == "Team Summary" & length(input$team_summary_selection) > 0) {
      side_vars$Side1_Row <- which(df_team_info$Full_Name == input$team_summary_selection)
      side_vars$Side2_Row <- which(df_team_info$Full_Name == "World Lacrosse")
    }
  })
  
  observeEvent(input$individual_game_submit, {
    if(data_sel$Current_Selection == "Ind Game" & length(input$ind_game_selection) > 0) { 
      side_vars$Side1_Row <- which(df_team_info$Team == str_split_fixed(str_split_fixed(input$ind_game_selection, ": ", 2)[[2]], " vs. ", 2)[[1]])
      side_vars$Side2_Row <- which(df_team_info$Team == str_split_fixed(str_split_fixed(input$ind_game_selection, ": ", 2)[[2]], " vs. ", 2)[[2]])
    }
  })
  
  ##### RV: Game Dates Variable #####
  
  game_sel <- reactive({
    if(data_sel$Current_Selection == "Team Summary" & length(input$team_summary_selection) > 0) {
      df_dates %>% 
        filter((Home_Team == df_team_info$Team[side_vars$Side1_Row]) | (Away_Team == df_team_info$Team[side_vars$Side1_Row]))
    } else {
      if(data_sel$Current_Selection == "Ind Game" & length(input$ind_game_selection) > 0) {
        df_dates %>% 
          filter(Game == input$ind_game_selection)
      }
    }
  })
  
  ##### RV: Data Pull On Submit #####
  
  # Need reactive values when the submit buttons are actually clicked
  # Otherwise, the eventReactive expression later will be triggered simply by the button appearing AND when it's clicked
  # this process creates a degree of separation in the reactivity that let's the submit buttons be the only trigger
  submit_button_clicks <- reactiveValues("Team_Summary" = 0, 
                                         "Team_vs_Team" = 0, 
                                         "Individual_Game" = 0)
  
  observeEvent(input$team_summary_submit, {
    submit_button_clicks$Team_Summary <- submit_button_clicks$Team_Summary + 1
  })
  
  observeEvent(input$individual_game_submit, {
    submit_button_clicks$Individual_Game <- submit_button_clicks$Individual_Game + 1
  })
  
  # Lets any change in those reactive counts be a trigger for the data frame
  submit_buttons <- reactive({
    list(submit_button_clicks$Team_Summary, 
         submit_button_clicks$Individual_Game)
  })
  
  # also need these clean reactive items to trigger an actual "current" selection
  lastest_submit <- reactiveValues("Current_Submission" = "Team Summary")
  
  observeEvent(submit_button_clicks$Team_Summary, {
    if(lastest_submit$Current_Submission != "Team Summary") {
      lastest_submit$Current_Submission <- "Team Summary"
    }
  })
  
  observeEvent(submit_button_clicks$Individual_Game, {
    if(lastest_submit$Current_Submission != "Ind Game") {
      lastest_submit$Current_Submission <- "Ind Game"
    }
  })
  
  df_master_fltr <- eventReactive(submit_buttons(), {
    # Start a dataframe with the selected games
    df_mf <- df_master %>% 
      filter(Game %in% game_sel()$Game)
    
    # need to go game by game and figure out which side of the floor each side started on and played the 3rd quarter on to figure out coord switching
    df_mf_sides <- df_mf %>% 
      filter(Quarter %in% c(1, 3) & !is.na(Team)) %>% 
      group_by(Game, Team) %>% 
      summarise(Field_Side = ifelse(length(which(Location1_x < 0.5)) > length(which(Location1_x > 0.5)), "West", "East"))
    
    # normalize the event locations so that they're always with respect to the same side of the field for either team
    # the goal is to get it so that side1 is on the east side of the field and side2 is on the west side of the field
    for(i in unique(df_mf$Game)) {
      for(j in which(df_mf$Game == i)[which(!is.na(df_mf$Team[which(df_mf$Game == i)]))]) {
        # if it's 
        if((df_mf$Team[j] == df_team_info$Team[side_vars$Side1_Row]) & 
           (df_mf_sides$Field_Side[df_mf_sides$Game == i & df_mf_sides$Team == df_team_info$Team[side_vars$Side1_Row]] == "East")) {
          if(df_mf$Quarter[j] == 2) {
            df_mf$Location1_x[j] <- 0.5 + -1*(df_mf$Location1_x[j] - 0.5)
            df_mf$Location1_y[j] <- 0.5 + -1*(df_mf$Location1_y[j] - 0.5)
            df_mf$Location2_x[j] <- 0.5 + -1*(df_mf$Location2_x[j] - 0.5)
            df_mf$Location2_y[j] <- 0.5 + -1*(df_mf$Location2_y[j] - 0.5)
          } else {
            if(df_mf$Quarter[j] == 4) {
              df_mf$Location1_x[j] <- 0.5 + -1*(df_mf$Location1_x[j] - 0.5)
              df_mf$Location1_y[j] <- 0.5 + -1*(df_mf$Location1_y[j] - 0.5)
              df_mf$Location2_x[j] <- 0.5 + -1*(df_mf$Location2_x[j] - 0.5)
              df_mf$Location2_y[j] <- 0.5 + -1*(df_mf$Location2_y[j] - 0.5)
            }
          }
        } else {
          if((df_mf$Team[j] == df_team_info$Team[side_vars$Side1_Row]) & 
             (df_mf_sides$Field_Side[df_mf_sides$Game == i & df_mf_sides$Team == df_team_info$Team[side_vars$Side1_Row]] == "West")) {
            if(df_mf$Quarter[j] == 1) {
              df_mf$Location1_x[j] <- 0.5 + -1*(df_mf$Location1_x[j] - 0.5)
              df_mf$Location1_y[j] <- 0.5 + -1*(df_mf$Location1_y[j] - 0.5)
              df_mf$Location2_x[j] <- 0.5 + -1*(df_mf$Location2_x[j] - 0.5)
              df_mf$Location2_y[j] <- 0.5 + -1*(df_mf$Location2_y[j] - 0.5)
            } else {
              if(df_mf$Quarter[j] == 3) {
                df_mf$Location1_x[j] <- 0.5 + -1*(df_mf$Location1_x[j] - 0.5)
                df_mf$Location1_y[j] <- 0.5 + -1*(df_mf$Location1_y[j] - 0.5)
                df_mf$Location2_x[j] <- 0.5 + -1*(df_mf$Location2_x[j] - 0.5)
                df_mf$Location2_y[j] <- 0.5 + -1*(df_mf$Location2_y[j] - 0.5)
              } else {
                if(df_mf$Quarter[j] == 5) {
                  df_mf$Location1_x[j] <- 0.5 + -1*(df_mf$Location1_x[j] - 0.5)
                  df_mf$Location1_y[j] <- 0.5 + -1*(df_mf$Location1_y[j] - 0.5)
                  df_mf$Location2_x[j] <- 0.5 + -1*(df_mf$Location2_x[j] - 0.5)
                  df_mf$Location2_y[j] <- 0.5 + -1*(df_mf$Location2_y[j] - 0.5)
                }
              }
            }
          } else {
            if((df_mf$Team[j] != df_team_info$Team[side_vars$Side1_Row]) & 
               (df_mf_sides$Field_Side[df_mf_sides$Game == i & df_mf_sides$Team != df_team_info$Team[side_vars$Side1_Row]] == "East")) {
              if(df_mf$Quarter[j] == 1) {
                df_mf$Location1_x[j] <- 0.5 + -1*(df_mf$Location1_x[j] - 0.5)
                df_mf$Location1_y[j] <- 0.5 + -1*(df_mf$Location1_y[j] - 0.5)
                df_mf$Location2_x[j] <- 0.5 + -1*(df_mf$Location2_x[j] - 0.5)
                df_mf$Location2_y[j] <- 0.5 + -1*(df_mf$Location2_y[j] - 0.5)
              } else {
                if(df_mf$Quarter[j] == 3) {
                  df_mf$Location1_x[j] <- 0.5 + -1*(df_mf$Location1_x[j] - 0.5)
                  df_mf$Location1_y[j] <- 0.5 + -1*(df_mf$Location1_y[j] - 0.5)
                  df_mf$Location2_x[j] <- 0.5 + -1*(df_mf$Location2_x[j] - 0.5)
                  df_mf$Location2_y[j] <- 0.5 + -1*(df_mf$Location2_y[j] - 0.5)
                } else {
                  if(df_mf$Quarter[j] == 5) {
                    df_mf$Location1_x[j] <- 0.5 + -1*(df_mf$Location1_x[j] - 0.5)
                    df_mf$Location1_y[j] <- 0.5 + -1*(df_mf$Location1_y[j] - 0.5)
                    df_mf$Location2_x[j] <- 0.5 + -1*(df_mf$Location2_x[j] - 0.5)
                    df_mf$Location2_y[j] <- 0.5 + -1*(df_mf$Location2_y[j] - 0.5)
                  }
                }
              }
            } else {
              if(df_mf$Quarter[j] == 2) {
                df_mf$Location1_x[j] <- 0.5 + -1*(df_mf$Location1_x[j] - 0.5)
                df_mf$Location1_y[j] <- 0.5 + -1*(df_mf$Location1_y[j] - 0.5)
                df_mf$Location2_x[j] <- 0.5 + -1*(df_mf$Location2_x[j] - 0.5)
                df_mf$Location2_y[j] <- 0.5 + -1*(df_mf$Location2_y[j] - 0.5)
              } else {
                if(df_mf$Quarter[j] == 4) {
                  df_mf$Location1_x[j] <- 0.5 + -1*(df_mf$Location1_x[j] - 0.5)
                  df_mf$Location1_y[j] <- 0.5 + -1*(df_mf$Location1_y[j] - 0.5)
                  df_mf$Location2_x[j] <- 0.5 + -1*(df_mf$Location2_x[j] - 0.5)
                  df_mf$Location2_y[j] <- 0.5 + -1*(df_mf$Location2_y[j] - 0.5)
                }
              }
            }
          }
        }
      }
    }
    
    # add a game clock chunked column
    df_mf <- df_mf %>%
      mutate(Time_Stamp_Chunked = ifelse(Time_Stamp == 0, 1, ceiling(Time_Stamp / 300)))
    
    # create a column with the binned shot locations for both sides
    # WILL BREAK AND CAUSE AN ERROR IF THERE'S A PROBLEM WITH THE DATA
    df_mf <- df_mf %>% 
      mutate(Field_Location_Bin = "none")
    
    for(i in which(df_mf$Team == df_team_info$Team[side_vars$Side1_Row] &
                   df_mf$Event == "Shot" &
                   !is.na(df_mf$Location1_x) & !is.na(df_mf$Location1_y))) {
      bin_found <- fieldloc_bins$Bin[which(df_mf$Location1_x[i] >= fieldloc_bins$x_min &
                                             df_mf$Location1_x[i] < fieldloc_bins$x_max &
                                             df_mf$Location1_y[i] >= fieldloc_bins$y_min &
                                             df_mf$Location1_y[i] < fieldloc_bins$y_max)]
      if(length(bin_found) == 1) {
        df_mf$Field_Location_Bin[i] <- bin_found
      } else {
        break
      }
    }
    for(i in which(df_mf$Team != df_team_info$Team[side_vars$Side1_Row] &
                   df_mf$Event == "Shot" &
                   !is.na(df_mf$Location1_x) & !is.na(df_mf$Location1_y))) {
      bin_found <- fieldloc_bins$Bin[which((0.5 + -1*(df_mf$Location1_x[i] - 0.5)) >= fieldloc_bins$x_min &
                                             (0.5 + -1*(df_mf$Location1_x[i] - 0.5)) < fieldloc_bins$x_max &
                                             (0.5 + -1*(df_mf$Location1_y[i] - 0.5)) >= fieldloc_bins$y_min &
                                             (0.5 + -1*(df_mf$Location1_y[i] - 0.5)) < fieldloc_bins$y_max)]
      if(length(bin_found) == 1) {
        df_mf$Field_Location_Bin[i] <- bin_found
      } else {
        break
      }
    }
    
    df_mf
  })
  
  ##### RV: Plot Clicks #####
  
  # Scoring Side 1
  sco_side1_goal_sel <- reactiveValues("Net_Location" = "none")
  observeEvent(event_data(event = "plotly_click", source = "sco_side1_goal_click"), {
    sco_side1_goal_sel_temp <- event_data(event = "plotly_click", source = "sco_side1_goal_click")
    if(sco_side1_shotangle_sel$Shot_Angle_Bin[1] != "none" | sco_side1_field_sel$Field_Location_Bin[1] != "none") {
      sco_side1_goal_sel$Net_Location <- "none"
      sco_side1_shotangle_sel$Shot_Angle_Bin <- "none"
      sco_side1_field_sel$Field_Location_Bin <- "none"
    } else {
      sco_side1_goal_sel$Net_Location <- c(as.character(sco_side1_goal_sel_temp[5]), sco_side1_goal_sel$Net_Location)
    }
  })
  
  sco_side1_shotangle_sel <- reactiveValues("Shot_Angle_Bin" = "none")
  observeEvent(event_data(event = "plotly_click", source = "sco_side1_shotangle_click"), {
    sco_side1_shotangle_sel_temp <- event_data(event = "plotly_click", source = "sco_side1_shotangle_click")
    if(sco_side1_goal_sel$Net_Location[1] != "none" | sco_side1_field_sel$Field_Location_Bin[1] != "none") {
      sco_side1_goal_sel$Net_Location <- "none"
      sco_side1_shotangle_sel$Shot_Angle_Bin <- "none"
      sco_side1_field_sel$Field_Location_Bin <- "none"
    } else {
      sco_side1_shotangle_sel$Shot_Angle_Bin <- c(as.character(sco_side1_shotangle_sel_temp[5]), sco_side1_shotangle_sel$Shot_Angle_Bin)
    }
  })
  
  sco_side1_field_sel <- reactiveValues("Field_Location_Bin" = "none")
  observeEvent(event_data(event = "plotly_click", source = "sco_side1_field_click"), {
    sco_side1_field_sel_temp <- event_data(event = "plotly_click", source = "sco_side1_field_click")
    if(sco_side1_goal_sel$Net_Location[1] != "none" | sco_side1_shotangle_sel$Shot_Angle_Bin[1] != "none") {
      sco_side1_goal_sel$Net_Location <- "none"
      sco_side1_shotangle_sel$Shot_Angle_Bin <- "none"
      sco_side1_field_sel$Field_Location_Bin <- "none"
    } else {
      sco_side1_field_sel$Field_Location_Bin <- c(as.character(sco_side1_field_sel_temp[5]), sco_side1_field_sel$Field_Location_Bin)
    }
  })
  
  # Scoring Side 2
  sco_side2_goal_sel <- reactiveValues("Net_Location" = "none")
  observeEvent(event_data(event = "plotly_click", source = "sco_side2_goal_click"), {
    sco_side2_goal_sel_temp <- event_data(event = "plotly_click", source = "sco_side2_goal_click")
    if(sco_side2_shotangle_sel$Shot_Angle_Bin[1] != "none" | sco_side2_field_sel$Field_Location_Bin[1] != "none") {
      sco_side2_goal_sel$Net_Location <- "none"
      sco_side2_shotangle_sel$Shot_Angle_Bin <- "none"
      sco_side2_field_sel$Field_Location_Bin <- "none"
    } else {
      sco_side2_goal_sel$Net_Location <- c(as.character(sco_side2_goal_sel_temp[5]), sco_side2_goal_sel$Net_Location)
    }
  })
  
  sco_side2_shotangle_sel <- reactiveValues("Shot_Angle_Bin" = "none")
  observeEvent(event_data(event = "plotly_click", source = "sco_side2_shotangle_click"), {
    sco_side2_shotangle_sel_temp <- event_data(event = "plotly_click", source = "sco_side2_shotangle_click")
    if(sco_side2_goal_sel$Net_Location[1] != "none" | sco_side2_field_sel$Field_Location_Bin[1] != "none") {
      sco_side2_goal_sel$Net_Location <- "none"
      sco_side2_shotangle_sel$Shot_Angle_Bin <- "none"
      sco_side2_field_sel$Field_Location_Bin <- "none"
    } else {
      sco_side2_shotangle_sel$Shot_Angle_Bin <- c(as.character(sco_side2_shotangle_sel_temp[5]), sco_side2_shotangle_sel$Shot_Angle_Bin)
    }
  })
  
  sco_side2_field_sel <- reactiveValues("Field_Location_Bin" = "none")
  observeEvent(event_data(event = "plotly_click", source = "sco_side2_field_click"), {
    sco_side2_field_sel_temp <- event_data(event = "plotly_click", source = "sco_side2_field_click")
    if(sco_side2_goal_sel$Net_Location[1] != "none" | sco_side2_shotangle_sel$Shot_Angle_Bin[1] != "none") {
      sco_side2_goal_sel$Net_Location <- "none"
      sco_side2_shotangle_sel$Shot_Angle_Bin <- "none"
      sco_side2_field_sel$Field_Location_Bin <- "none"
    } else {
      sco_side2_field_sel$Field_Location_Bin <- c(as.character(sco_side2_field_sel_temp[5]), sco_side2_field_sel$Field_Location_Bin)
    }
  })
  
  # Player Profile
  ply_goal_sel <- reactiveValues("Net_Location" = "none")
  observeEvent(event_data(event = "plotly_click", source = "ply_goal_click"), {
    ply_goal_sel_temp <- event_data(event = "plotly_click", source = "ply_goal_click")
    if(ply_shotangle_sel$Shot_Angle_Bin[1] != "none" | ply_field_sel$Field_Location_Bin[1] != "none") {
      ply_goal_sel$Net_Location <- "none"
      ply_shotangle_sel$Shot_Angle_Bin <- "none"
      ply_field_sel$Field_Location_Bin <- "none"
    } else {
      ply_goal_sel$Net_Location <- c(as.character(ply_goal_sel_temp[5]), ply_goal_sel$Net_Location)
    }
  })
  
  ply_shotangle_sel <- reactiveValues("Shot_Angle_Bin" = "none")
  observeEvent(event_data(event = "plotly_click", source = "ply_shotangle_click"), {
    ply_shotangle_sel_temp <- event_data(event = "plotly_click", source = "ply_shotangle_click")
    if(ply_goal_sel$Net_Location[1] != "none" | ply_field_sel$Field_Location_Bin[1] != "none") {
      ply_goal_sel$Net_Location <- "none"
      ply_shotangle_sel$Shot_Angle_Bin <- "none"
      ply_field_sel$Field_Location_Bin <- "none"
    } else {
      ply_shotangle_sel$Shot_Angle_Bin <- c(as.character(ply_shotangle_sel_temp[5]), ply_shotangle_sel$Shot_Angle_Bin)
    }
  })
  
  ply_field_sel <- reactiveValues("Field_Location_Bin" = "none")
  observeEvent(event_data(event = "plotly_click", source = "ply_field_click"), {
    ply_field_sel_temp <- event_data(event = "plotly_click", source = "ply_field_click")
    if(ply_goal_sel$Net_Location[1] != "none" | ply_shotangle_sel$Shot_Angle_Bin[1] != "none") {
      ply_goal_sel$Net_Location <- "none"
      ply_shotangle_sel$Shot_Angle_Bin <- "none"
      ply_field_sel$Field_Location_Bin <- "none"
    } else {
      ply_field_sel$Field_Location_Bin <- c(as.character(ply_field_sel_temp[5]), ply_field_sel$Field_Location_Bin)
    }
  })
  
  observeEvent(input$sco_resetclicks_bttn, {
    sco_side1_goal_sel$Net_Location <- "none"
    sco_side1_shotangle_sel$Shot_Angle_Bin <- "none"
    sco_side1_field_sel$Field_Location_Bin <- "none"
    sco_side2_goal_sel$Net_Location <- "none"
    sco_side2_shotangle_sel$Shot_Angle_Bin <- "none"
    sco_side2_field_sel$Field_Location_Bin <- "none"
  })
  
  observeEvent(input$ply_resetclicks_bttn, {
    ply_goal_sel$Net_Location <- "none"
    ply_shotangle_sel$Shot_Angle_Bin <- "none"
    ply_field_sel$Field_Location_Bin <- "none"
  })
  
  ##### RV: Player List and Info #####
  
  player_list_triggers <- reactive({
    list(submit_button_clicks$Team_Summary, 
         submit_button_clicks$Individual_Game, 
         input$ply_season)
  })
  
  player_list <- eventReactive(player_list_triggers(), {
    
    df_player <- df_master_fltr()
    
    if(length(unique(df_master_fltr()$Game)) > 1) {
      if(input$ply_season != "Cumulative") {
        df_player <- df_player %>%
          filter(Season %in% input$ply_season)
      }
    }
    
    df1 <- df_player %>% 
      filter(!is.na(Player1_Name)) %>% 
      group_by(Team, Player1, Player1_Name) %>% 
      summarise("Max_Date" = max(mdy(Game_Date))) %>% 
      ungroup() %>% 
      rename("Player_Num" = Player1, 
             "Player_Name" = Player1_Name)
    df2 <- df_player %>% 
      filter(!is.na(Primary_Assist_Name)) %>% 
      group_by(Team, Primary_Assist, Primary_Assist_Name) %>% 
      summarise("Max_Date" = max(mdy(Game_Date))) %>% 
      ungroup() %>% 
      rename("Player_Num" = Primary_Assist, 
             "Player_Name" = Primary_Assist_Name)
    df3 <- df_player %>% 
      filter(!is.na(Secondary_Assist_Name)) %>% 
      group_by(Team, Secondary_Assist, Secondary_Assist_Name) %>% 
      summarise("Max_Date" = max(mdy(Game_Date))) %>% 
      ungroup() %>% 
      rename("Player_Num" = Secondary_Assist, 
             "Player_Name" = Secondary_Assist_Name)
    df4 <- df_player %>% 
      filter(!is.na(Rebound_Name) & Rebound_Type == "Offensive") %>% 
      group_by(Team, Rebound, Rebound_Name) %>% 
      summarise("Max_Date" = max(mdy(Game_Date))) %>% 
      ungroup() %>% 
      rename("Player_Num" = Rebound, 
             "Player_Name" = Rebound_Name)
    df5 <- df_player %>% 
      filter(!is.na(Rebound_Name) & Rebound_Type == "Defensive") %>% 
      mutate(Defensive_Team = ifelse(Team == Home_Team, Away_Team, Home_Team)) %>% 
      group_by(Defensive_Team, Rebound, Rebound_Name) %>% 
      summarise("Max_Date" = max(mdy(Game_Date))) %>% 
      ungroup() %>% 
      rename("Player_Num" = Rebound, 
             "Player_Name" = Rebound_Name, 
             "Team" = Defensive_Team)
    df6 <- df_player %>% 
      filter(!is.na(FO_Home_Name)) %>% 
      group_by(Home_Team, FO_Home, FO_Home_Name) %>% 
      summarise("Max_Date" = max(mdy(Game_Date))) %>% 
      ungroup() %>% 
      rename("Player_Num" = FO_Home, 
             "Player_Name" = FO_Home_Name, 
             "Team" = Home_Team)
    df7 <- df_player %>% 
      filter(!is.na(FO_Away_Name)) %>% 
      group_by(Away_Team, FO_Away, FO_Away_Name) %>% 
      summarise("Max_Date" = max(mdy(Game_Date))) %>% 
      ungroup() %>% 
      rename("Player_Num" = FO_Away, 
             "Player_Name" = FO_Away_Name, 
             "Team" = Away_Team)
    df8 <- df_player %>% 
      filter(!is.na(Home_Goalie_Name)) %>% 
      group_by(Home_Team, Home_Goalie, Home_Goalie_Name) %>% 
      summarise("Max_Date" = max(mdy(Game_Date))) %>% 
      ungroup() %>% 
      rename("Player_Num" = Home_Goalie, 
             "Player_Name" = Home_Goalie_Name, 
             "Team" = Home_Team)
    df9 <- df_player %>% 
      filter(!is.na(Away_Goalie_Name)) %>% 
      group_by(Away_Team, Away_Goalie, Away_Goalie_Name) %>% 
      summarise("Max_Date" = max(mdy(Game_Date))) %>% 
      ungroup() %>% 
      rename("Player_Num" = Away_Goalie, 
             "Player_Name" = Away_Goalie_Name, 
             "Team" = Away_Team)
    
    df_player2 <- rbind(df1, df2, df3, df4, df5, df6, df7, df8, df9)
    df_player2 <- df_player2 %>% 
      group_by(Player_Name, Team, Player_Num) %>% 
      summarise(Max_Date = max(Max_Date))
    df_player2 <- df_player2 %>% 
      group_by(Player_Name) %>% 
      summarise(Team = Team[which(Max_Date == max(Max_Date))], 
                Player_Num = Player_Num[which(Max_Date == max(Max_Date))])
    df_player2 <- df_player2 %>% 
      ungroup() %>% 
      rowwise() %>% 
      mutate(LastName = str_split_fixed(Player_Name, ". ", 2)[2]) %>% 
      ungroup() %>% 
      arrange(Team, LastName)
    
    if(lastest_submit$Current_Submission == "Team Summary") {
      df_player2 <- df_player2 %>% 
        filter(Team == df_team_info$Team[side_vars$Side1_Row])
    } else {
      df_player2 <- df_player2 %>% 
        filter(Team == df_team_info$Team[side_vars$Side1_Row] | Team == df_team_info$Team[side_vars$Side2_Row])
    }
    
    df_player2
  })
  
  #### Server - Top Grid (TG) ####
  
  ##### TG: Top Grid #####
  
  # Separated into a 3 part if-else statement, changes the formatting depending on the selection through the style arguments
  
  output$top_grid <- renderUI({
    
    # if Team Summary is selected
    if(data_sel$Current_Selection == "Team Summary") {
      fluidRow(
        column(
          width = 4, 
          wellPanel(
            actionButton(
              inputId = "team_summary_bttn", label = "TEAM SUMMARY", width = "100%", 
              style = "background-color: #4b86cb; color: white;"
            ), 
            br(), br(), 
            tags$head(tags$style("
                       .jhr{
                       display: inline;
                       vertical-align: middle;
                       padding-left: 10px;
                       }")),
            pickerInput(
              inputId = "team_summary_selection", label = "Pick One Team", 
              choices = df_team_info$Full_Name[which(df_team_info$Full_Name != "World Lacrosse")], 
              selected = df_team_info$Full_Name[report_team_row]
            ), 
            fluidRow(uiOutput(outputId = "team_summary_logo"), 
                     style = "height: 105px;"), 
            actionButton(
              inputId = "team_summary_submit", label = "SUBMIT", width = "100%", 
              style = "color: #ff1400; border-color: #ff1400;"
            ), 
            style = "height: 345px; box-shadow: 0px 10px 5px 5px #888888; border-width: thick;"
          )
        ), 
        
        column(
          width = 4, 
          wellPanel(
            actionButton(
              inputId = "individual_game_bttn", label = "INDIVIDUAL GAME", width = "100%"
            ), 
            style = "height: 85px"
          )
        )
      )
    } else {
      
      # if Ind Game is selected
      fluidRow(
        column(
          width = 4, 
          wellPanel(
            actionButton(
              inputId = "team_summary_bttn", label = "TEAM SUMMARY", width = "100%"
            ),  
            style = "height: 85px"
          )
        ), 
        
        column(
          width = 4, 
          wellPanel(
            actionButton(
              inputId = "individual_game_bttn", label = "INDIVIDUAL GAME", width = "100%", 
              style = "background-color: #4b86cb; color: white;"
            ), 
            br(), br(), 
            pickerInput(
              inputId = "ind_game_selection", label = "Pick One Game", 
              choices = game_list, selected = game_list[starting_game_row], 
              options = pickerOptions(
                liveSearch = T
              )
            ), 
            fluidRow(column(6, uiOutput(outputId = "individual_game_logo1")),
                     column(6, uiOutput(outputId = "individual_game_logo2")), 
                     style = "height: 105px;"), 
            actionButton(
              inputId = "individual_game_submit", label = "SUBMIT", width = "100%", 
              style = "color: #ff1400; border-color: #ff1400;"
            ), 
            style = "height: 345px; box-shadow: 0px 10px 5px 5px #888888; border-width: thick;"
          )
        )
      )
    }
  })
  
  ##### TG: Team Summary Logo #####
  
  output$team_summary_logo <- renderUI({
    div(class = "top_grid_logo", img(src = df_team_info$Shadow_Logo_Path[df_team_info$Full_Name == input$team_summary_selection], 
                                     height = "115px", width = "auto"))
  })
  
  ##### TG: Ind. Game Logos #####
  
  output$individual_game_logo1 <- renderUI({
    div(class = "top_grid_logo", img(src = df_team_info$Shadow_Logo_Path[df_team_info$Team == str_split_fixed(str_split_fixed(input$ind_game_selection, ": ", 2)[[2]], " vs. ", 2)[[1]]], 
                                     height = "115px", width = "auto"))
  })
  
  output$individual_game_logo2 <- renderUI({
    div(class = "top_grid_logo", img(src = df_team_info$Shadow_Logo_Path[df_team_info$Team == str_split_fixed(str_split_fixed(input$ind_game_selection, ": ", 2)[[2]], " vs. ", 2)[[2]]], 
                                     height = "115px", width = "auto"))
  })
  
  #### Server - Boxscore (BOX) ####
  
  ##### BOX: Presubmit Screen #####
  
  output$boxscore_presubmit <- renderUI({
    if(side_vars$Side1_Row == "none") {
      fluidPage(
        br(), 
        br(), 
        br(), 
        br(), 
        
        wellPanel(
          h1("Press SUBMIT to View"), 
          
          style = "text-align: center; height: 250px; box-shadow: 0px 10px 5px 5px #888888; border-width: thick;"
        )
      )
    } else {
      fluidPage(
        br(), 
        br(), 
        
        uiOutput(outputId = "boxscore_tab")
      )
    }
  })
  
  ##### BOX: Tab UI #####
  
  output$boxscore_tab <- renderUI({
    fluidPage(
      fluidRow(
        column(
          width = 12, uiOutput(outputId = "boxscore_filters")
        )
      ), 
      
      navbarPage(
        title = "", 
        id = "boxscore_navbar", 
        position = "static-top", 
        inverse = TRUE, 
        
        tabPanel(
          id = "boxscore_scoring", title = "SCORING", 
          
          br(), br(), 
          fluidPage(
            fluidRow(
              column(
                width = 6, 
                wellPanel(
                  uiOutput(outputId = "boxscore_scoring_logo_side1"), 
                  br(), 
                  dataTableOutput(outputId = "boxscore_scoring_side1")
                )
              ), 
              
              column(
                width = 6, 
                wellPanel(
                  uiOutput(outputId = "boxscore_scoring_logo_side2"), 
                  br(), 
                  dataTableOutput(outputId = "boxscore_scoring_side2")
                )
              )
            )
          )
        ), 
        
        tabPanel(
          id = "boxscore_rebto", title = "REBOUNDS / DEFENSE / TURNOVERS", 
          
          br(), br(), 
          fluidPage(
            fluidRow(
              column(
                width = 6, 
                wellPanel(
                  uiOutput(outputId = "boxscore_rebto_logo_side1"), 
                  br(), 
                  dataTableOutput(outputId = "boxscore_rebto_side1")
                )
              ), 
              
              column(
                width = 6, 
                wellPanel(
                  uiOutput(outputId = "boxscore_rebto_logo_side2"), 
                  br(), 
                  dataTableOutput(outputId = "boxscore_rebto_side2")
                )
              )
            )
          )
        ), 
        
        tabPanel(
          id = "boxscore_goalies", title = "GOALIES", 
          
          br(), br(), 
          fluidPage(
            fluidRow(
              column(
                width = 6, 
                wellPanel(
                  uiOutput(outputId = "boxscore_goalies_logo_side1"), 
                  br(), 
                  dataTableOutput(outputId = "boxscore_goalies_side1")
                )
              ), 
              
              column(
                width = 6, 
                wellPanel(
                  uiOutput(outputId = "boxscore_goalies_logo_side2"), 
                  br(), 
                  dataTableOutput(outputId = "boxscore_goalies_side2")
                )
              )
            )
          )
        )
      )
    )
  })
  
  ##### BOX: Filters ##### 
  
  output$boxscore_filters <- renderUI({
    if(length(unique(df_master_fltr()$Game)) > 1) {
      wellPanel(
        fluidRow(
          column(
            width = 4, 
            h3("TOTAL OR AVG"), 
            prettyRadioButtons(inputId = "boxscore_total_avg", label = "", 
                               choices = c("Totals", "Averages"), 
                               selected = "Totals", status = "primary", shape = "round", outline = T, 
                               fill = T, thick = T, animation = "smooth", icon = icon("check"), plain = T, 
                               bigger = T, inline = T)
          ), 
          
          column(
            width = 4, 
            h3("STATE"), 
            prettyRadioButtons(inputId = "boxscore_state", label = "", 
                               choices = c("All", "Set", "Transition"), 
                               selected = "All", status = "primary", shape = "round", outline = T, 
                               fill = T, thick = T, animation = "smooth", icon = icon("check"), plain = T, 
                               bigger = T, inline = T)
          ), 
          
          column(
            width = 4, 
            h3("STRENGTH OF THE OFFENSE"), 
            prettyRadioButtons(inputId = "boxscore_strength", label = "", 
                               choices = c("All", "Even-Strength", "Man-Advantage", "Short-Handed"), 
                               selected = "All", status = "primary", shape = "round", outline = T, 
                               fill = T, thick = T, animation = "smooth", icon = icon("check"), plain = T, 
                               bigger = T, inline = T)
          )
        )
      )
    } else {
      wellPanel(
        fluidRow(
          column(
            width = 6, 
            h3("STATE OF THE OFFENSE"), 
            prettyRadioButtons(inputId = "boxscore_state", label = "", 
                               choices = c("All", "Set", "Transition"), 
                               selected = "All", status = "primary", shape = "round", outline = T, 
                               fill = T, thick = T, animation = "smooth", icon = icon("check"), plain = T, 
                               bigger = T, inline = T)
          ), 
          
          column(
            width = 6, 
            h3("STRENGTH OF THE OFFENSE"), 
            prettyRadioButtons(inputId = "boxscore_strength", label = "", 
                               choices = c("All", "Even-Strength", "Man-Advantage", "Short-Handed"), 
                               selected = "All", status = "primary", shape = "round", outline = T, 
                               fill = T, thick = T, animation = "smooth", icon = icon("check"), plain = T, 
                               bigger = T, inline = T)
          )
        )
      )
    }
  })
  
  ##### BOX: Scoring Table Logo Side1 #####
  
  output$boxscore_scoring_logo_side1 <- renderUI({
    if(length(input$boxscore_strength) > 0) {
      div(class = "ply_player_title_logo", 
          img(src = df_team_info$Wordmark_Path[which(df_team_info$Team == df_team_info$Team[side_vars$Side1_Row])], 
              height = "100px", width = "auto"))
    }
  })
  
  ##### BOX: Scoring Table Side1 #####
  
  output$boxscore_scoring_side1 <- renderDataTable({
    if(length(input$boxscore_strength) > 0) {
      shot_df <- df_master_fltr() %>% 
        filter(Team == df_team_info$Team[side_vars$Side1_Row]) %>% 
        filter(Event_Type == "Shot")
      
      if(length(unique(df_master_fltr()$Team)) > 2) {
        shot_df <- shot_df %>% 
          mutate(Team = ifelse(Team != df_team_info$Team[side_vars$Side1_Row], "Opponent", Team))
      }
      
      # Filter down by strength input
      if(input$boxscore_strength == "Even-Strength") {
        shot_df <- shot_df %>% 
          filter(Home_Strength == Away_Strength)    
      } else {
        if(input$boxscore_strength == "Man-Advantage") {
          shot_df <- shot_df %>% 
            group_by(Game) %>% 
            mutate(Side1_Strength = ifelse(Home_Team == df_team_info$Team[side_vars$Side1_Row], 
                                           Home_Strength, Away_Strength), 
                   Side2_Strength = ifelse(Home_Team == df_team_info$Team[side_vars$Side1_Row], 
                                           Away_Strength, Home_Strength)) %>% 
            ungroup() %>% 
            mutate(Keep = ifelse((Team == df_team_info$Team[side_vars$Side1_Row] & Side1_Strength > Side2_Strength) | (Team != df_team_info$Team[side_vars$Side1_Row] & Side1_Strength < Side2_Strength), "Yes", "No")) %>% 
            filter(Keep == "Yes")
        } else {
          if(input$boxscore_strength == "Short-Handed") {
            shot_df <- shot_df %>% 
              group_by(Game) %>% 
              mutate(Side1_Strength = ifelse(Home_Team == df_team_info$Team[side_vars$Side1_Row], 
                                             Home_Strength, Away_Strength), 
                     Side2_Strength = ifelse(Home_Team == df_team_info$Team[side_vars$Side1_Row], 
                                             Away_Strength, Home_Strength)) %>% 
              ungroup() %>% 
              mutate(Keep = ifelse((Team == df_team_info$Team[side_vars$Side1_Row] & Side1_Strength < Side2_Strength) | (Team != df_team_info$Team[side_vars$Side1_Row] & Side1_Strength > Side2_Strength), "Yes", "No")) %>% 
              filter(Keep == "Yes")
          }
        }
      }
      
      # Filter down by state input
      if(input$boxscore_state != "All") {
        shot_df <- shot_df %>% 
          filter(State %in% input$boxscore_state)
      }
      
      if(length(input$boxscore_total_avg) > 0) {
        if(input$boxscore_total_avg == "Totals") {
          shot_df1 <- shot_df %>% 
            rename("PLAYER" = Player1_Name) %>% 
            group_by(PLAYER) %>% 
            summarise(GOALS = length(which((Result == "Goal"))),
                      SHOTS = length(which(!is.na(Result))))
          shot_df2 <- shot_df %>% 
            rename("PLAYER" = Primary_Assist_Name) %>% 
            group_by(PLAYER) %>% 
            summarise("1ST AST" = length(which(!is.na(Primary_Assist))))
          shot_df3 <- shot_df %>% 
            rename("PLAYER" = Secondary_Assist_Name) %>% 
            group_by(PLAYER) %>% 
            summarise("2ND AST" = length(which(!is.na(Secondary_Assist))))
          shot_df <- left_join(shot_df1, shot_df2, by = "PLAYER")
          shot_df <- left_join(shot_df, shot_df3, by = "PLAYER")
        } else {
          games <- length(unique(shot_df$Game))
          shot_df1 <- shot_df %>% 
            rename("PLAYER" = Player1_Name) %>% 
            group_by(PLAYER) %>% 
            summarise(GOALS = round(length(which((Result == "Goal"))) / games, 1),
                      SHOTS = round(length(which(!is.na(Result))) / games, 1))
          shot_df2 <- shot_df %>% 
            rename("PLAYER" = Primary_Assist_Name) %>% 
            group_by(PLAYER) %>% 
            summarise("1ST AST" = round(length(which(!is.na(Primary_Assist))) / games, 1))
          shot_df3 <- shot_df %>% 
            rename("PLAYER" = Secondary_Assist_Name) %>% 
            group_by(PLAYER) %>% 
            summarise("2ND AST" = round(length(which(!is.na(Secondary_Assist))) / games, 1))
          shot_df <- left_join(shot_df1, shot_df2, by = "PLAYER")
          shot_df <- left_join(shot_df, shot_df3, by = "PLAYER")
        }
      } else {
        shot_df1 <- shot_df %>% 
          rename("PLAYER" = Player1_Name) %>% 
          group_by(PLAYER) %>% 
          summarise(GOALS = length(which((Result == "Goal"))),
                    SHOTS = length(which(!is.na(Result))))
        shot_df2 <- shot_df %>% 
          rename("PLAYER" = Primary_Assist_Name) %>% 
          group_by(PLAYER) %>% 
          summarise("1ST AST" = length(which(!is.na(Primary_Assist))))
        shot_df3 <- shot_df %>% 
          rename("PLAYER" = Secondary_Assist_Name) %>% 
          group_by(PLAYER) %>% 
          summarise("2ND AST" = length(which(!is.na(Secondary_Assist))))
        shot_df <- left_join(shot_df1, shot_df2, by = "PLAYER")
        shot_df <- left_join(shot_df, shot_df3, by = "PLAYER")
      }
      
      shot_df <- shot_df %>% 
        filter(!is.na(PLAYER))
      shot_df[is.na(shot_df)] <- 0
      shot_df <- shot_df %>% 
        mutate("SHOT CONTRIBUTIONS" = SHOTS + `1ST AST` + `2ND AST`) %>% 
        arrange(desc(`SHOT CONTRIBUTIONS`))
      
      datatable(shot_df, 
                options = list(
                  paging = TRUE,
                  pageLength = 10, 
                  autowidth = FALSE, 
                  searching = FALSE, 
                  info = FALSE, 
                  scrollX = TRUE
                ), 
                rownames = FALSE
      )
    }
  })
  
  ##### BOX: Scoring Table Logo Side2 #####
  
  output$boxscore_scoring_logo_side2 <- renderUI({
    if(length(input$boxscore_strength) > 0) {
      div(class = "ply_player_title_logo", 
          img(src = df_team_info$Wordmark_Path[which(df_team_info$Team == df_team_info$Team[side_vars$Side2_Row])], 
              height = "100px", width = "auto"))
    }
  })
  
  ##### BOX: Scoring Table Side2 #####
  
  output$boxscore_scoring_side2 <- renderDataTable({
    if(length(input$boxscore_strength) > 0) {
      shot_df <- df_master_fltr() %>% 
        filter(Team != df_team_info$Team[side_vars$Side1_Row]) %>% 
        filter(Event_Type == "Shot")
      
      if(length(unique(df_master_fltr()$Team)) > 2) {
        shot_df <- shot_df %>% 
          mutate(Team = ifelse(Team != df_team_info$Team[side_vars$Side1_Row], "Opponent", Team))
      }
      
      # Filter down by strength input
      if(input$boxscore_strength == "Even-Strength") {
        shot_df <- shot_df %>% 
          filter(Home_Strength == Away_Strength)    
      } else {
        if(input$boxscore_strength == "Man-Advantage") {
          shot_df <- shot_df %>% 
            group_by(Game) %>% 
            mutate(Side1_Strength = ifelse(Home_Team == df_team_info$Team[side_vars$Side1_Row], 
                                           Home_Strength, Away_Strength), 
                   Side2_Strength = ifelse(Home_Team == df_team_info$Team[side_vars$Side1_Row], 
                                           Away_Strength, Home_Strength)) %>% 
            ungroup() %>% 
            mutate(Keep = ifelse((Team == df_team_info$Team[side_vars$Side1_Row] & Side1_Strength > Side2_Strength) | (Team != df_team_info$Team[side_vars$Side1_Row] & Side1_Strength < Side2_Strength), "Yes", "No")) %>% 
            filter(Keep == "Yes")
        } else {
          if(input$boxscore_strength == "Short-Handed") {
            shot_df <- shot_df %>% 
              group_by(Game) %>% 
              mutate(Side1_Strength = ifelse(Home_Team == df_team_info$Team[side_vars$Side1_Row], 
                                             Home_Strength, Away_Strength), 
                     Side2_Strength = ifelse(Home_Team == df_team_info$Team[side_vars$Side1_Row], 
                                             Away_Strength, Home_Strength)) %>% 
              ungroup() %>% 
              mutate(Keep = ifelse((Team == df_team_info$Team[side_vars$Side1_Row] & Side1_Strength < Side2_Strength) | (Team != df_team_info$Team[side_vars$Side1_Row] & Side1_Strength > Side2_Strength), "Yes", "No")) %>% 
              filter(Keep == "Yes")
          }
        }
      }
      
      # Filter down by state input
      if(input$boxscore_state != "All") {
        shot_df <- shot_df %>% 
          filter(State %in% input$boxscore_state)
      }
      
      if(length(input$boxscore_total_avg) > 0) {
        if(input$boxscore_total_avg == "Totals") {
          shot_df1 <- shot_df %>% 
            rename("PLAYER" = Player1_Name) %>% 
            group_by(PLAYER) %>% 
            summarise(GOALS = length(which((Result == "Goal"))),
                      SHOTS = length(which(!is.na(Result))))
          shot_df2 <- shot_df %>% 
            rename("PLAYER" = Primary_Assist_Name) %>% 
            group_by(PLAYER) %>% 
            summarise("1ST AST" = length(which(!is.na(Primary_Assist))))
          shot_df3 <- shot_df %>% 
            rename("PLAYER" = Secondary_Assist_Name) %>% 
            group_by(PLAYER) %>% 
            summarise("2ND AST" = length(which(!is.na(Secondary_Assist))))
          shot_df <- left_join(shot_df1, shot_df2, by = "PLAYER")
          shot_df <- left_join(shot_df, shot_df3, by = "PLAYER")
        } else {
          shot_df1 <- shot_df %>% 
            rename("PLAYER" = Player1_Name) %>% 
            group_by(PLAYER) %>% 
            summarise(GOALS = round(length(which((Result == "Goal"))) / length(unique(Game)), 1),
                      SHOTS = round(length(which(!is.na(Result))) / length(unique(Game)), 1))
          shot_df2 <- shot_df %>% 
            rename("PLAYER" = Primary_Assist_Name) %>% 
            group_by(PLAYER) %>% 
            summarise("1ST AST" = round(length(which(!is.na(Primary_Assist))) / length(unique(Game)), 1))
          shot_df3 <- shot_df %>% 
            rename("PLAYER" = Secondary_Assist_Name) %>% 
            group_by(PLAYER) %>% 
            summarise("2ND AST" = round(length(which(!is.na(Secondary_Assist))) / length(unique(Game)), 1))
          shot_df <- left_join(shot_df1, shot_df2, by = "PLAYER")
          shot_df <- left_join(shot_df, shot_df3, by = "PLAYER")
        }
      } else {
        shot_df1 <- shot_df %>% 
          rename("PLAYER" = Player1_Name) %>% 
          group_by(PLAYER) %>% 
          summarise(GOALS = length(which((Result == "Goal"))),
                    SHOTS = length(which(!is.na(Result))))
        shot_df2 <- shot_df %>% 
          rename("PLAYER" = Primary_Assist_Name) %>% 
          group_by(PLAYER) %>% 
          summarise("1ST AST" = length(which(!is.na(Primary_Assist))))
        shot_df3 <- shot_df %>% 
          rename("PLAYER" = Secondary_Assist_Name) %>% 
          group_by(PLAYER) %>% 
          summarise("2ND AST" = length(which(!is.na(Secondary_Assist))))
        shot_df <- left_join(shot_df1, shot_df2, by = "PLAYER")
        shot_df <- left_join(shot_df, shot_df3, by = "PLAYER")
      }
      
      shot_df <- shot_df %>% 
        filter(!is.na(PLAYER))
      shot_df[is.na(shot_df)] <- 0
      shot_df <- shot_df %>% 
        mutate("SHOT CONTRIBUTIONS" = SHOTS + `1ST AST` + `2ND AST`) %>% 
        arrange(desc(`SHOT CONTRIBUTIONS`))
      
      datatable(shot_df, 
                options = list(
                  paging = TRUE,
                  pageLength = 10, 
                  autowidth = FALSE, 
                  searching = FALSE, 
                  info = FALSE, 
                  scrollX = TRUE
                ), 
                rownames = FALSE
      )
    }
  })

  ##### BOX: Reb/TO Table Logo Side1 #####
  
  output$boxscore_rebto_logo_side1 <- renderUI({
    if(length(input$boxscore_strength) > 0) {
      div(class = "ply_player_title_logo", 
          img(src = df_team_info$Wordmark_Path[which(df_team_info$Team == df_team_info$Team[side_vars$Side1_Row])], 
              height = "100px", width = "auto"))
    }
  })
  
  ##### BOX: Reb/TO Table Side1 #####
  
  output$boxscore_rebto_side1 <- renderDataTable({
    if(length(input$boxscore_strength) > 0) {
      
      rdf_df <- df_master_fltr()
      
      ## Rebounds
      
      if(length(unique(df_master_fltr()$Team)) > 2) {
        rdf_df1 <- rdf_df %>% 
          filter(Rebound_Type %in% c("Offensive", "Defensive")) %>% 
          mutate(Team = ifelse(Team != df_team_info$Team[side_vars$Side1_Row], "Opponent", Team), 
                 Keep = ifelse(Team == df_team_info$Team[side_vars$Side1_Row] & Rebound_Type == "Offensive", "Yes", 
                               ifelse(Team != df_team_info$Team[side_vars$Side1_Row] & Rebound_Type == "Defensive", "Yes", "No"))) %>% 
          filter(Keep == "Yes")
      } else {
        rdf_df1 <- rdf_df %>% 
          filter(Rebound_Type %in% c("Offensive", "Defensive")) %>% 
          mutate(Keep = ifelse(Team == df_team_info$Team[side_vars$Side1_Row] & Rebound_Type == "Offensive", "Yes", 
                               ifelse(Team != df_team_info$Team[side_vars$Side1_Row] & Rebound_Type == "Defensive", "Yes", "No"))) %>% 
          filter(Keep == "Yes")
      }
      
      # Filter down by state input
      if(input$boxscore_state != "All") {
        rdf_df1 <- rdf_df1 %>% 
          filter(State %in% input$boxscore_state)
      }
      
      # Filter down by strength input
      if(input$boxscore_strength == "Even-Strength") {
        rdf_df1 <- rdf_df1 %>% 
          filter(Home_Strength == Away_Strength)    
      } else {
        if(input$boxscore_strength == "Man-Advantage") {
          rdf_df1 <- rdf_df1 %>% 
            mutate(Keep = ifelse((Home_Team == df_team_info$Team[side_vars$Side1_Row] & Home_Strength > Away_Strength) | (Home_Team != df_team_info$Team[side_vars$Side1_Row] & Home_Strength < Away_Strength), "Yes", "No")) %>% 
            filter(Keep == "Yes")
        } else {
          if(input$boxscore_strength == "Short-Handed") {
            rdf_df1 <- rdf_df1 %>% 
              mutate(Keep = ifelse((Home_Team == df_team_info$Team[side_vars$Side1_Row] & Home_Strength < Away_Strength) | (Home_Team != df_team_info$Team[side_vars$Side1_Row] & Home_Strength > Away_Strength), "Yes", "No")) %>% 
              filter(Keep == "Yes")
          }
        }
      }
      
      # Filter depending on Totals or Averages
      if(length(input$boxscore_total_avg) > 0) {
        if(input$boxscore_total_avg == "Totals") {
          rdf_df1 <- rdf_df1 %>% 
            rename("PLAYER" = Rebound_Name) %>% 
            group_by(PLAYER) %>% 
            summarise("DEF. REB" = length(which((Rebound_Type == "Defensive"))),
                      "OFF. REB" = length(which(Rebound_Type == "Offensive")))
        } else {
          games <- length(unique(rdf_df1$Game))
          rdf_df1 <- rdf_df1 %>% 
            rename("PLAYER" = Rebound_Name) %>% 
            group_by(PLAYER) %>% 
            summarise("DEF. REB" = round(length(which((Rebound_Type == "Defensive"))) / games, 1),
                      "OFF. REB" = round(length(which((Rebound_Type == "Offensive"))) / games, 1))
        }
      } else {
        rdf_df1 <- rdf_df1 %>% 
          rename("PLAYER" = Rebound_Name) %>% 
          group_by(PLAYER) %>% 
          summarise("DEF. REB" = length(which(Rebound_Type == "Defensive")),
                    "OFF. REB" = length(which(Rebound_Type == "Offensive")))
      } 
      
      ## Turnovers
      
      rdf_df2 <- rdf_df %>% 
        filter(Event_Type == "Turnover") %>% 
        filter(Team == df_team_info$Team[side_vars$Side1_Row])
      
      # Filter down by strength input
      if(input$boxscore_strength == "Even-Strength") {
        rdf_df2 <- rdf_df2 %>% 
          filter(Home_Strength == Away_Strength)    
      } else {
        if(input$boxscore_strength == "Man-Advantage") {
          rdf_df2 <- rdf_df2 %>% 
            group_by(Game) %>% 
            mutate(Side1_Strength = ifelse(Home_Team == df_team_info$Team[side_vars$Side1_Row], 
                                           Home_Strength, Away_Strength), 
                   Side2_Strength = ifelse(Home_Team == df_team_info$Team[side_vars$Side1_Row], 
                                           Away_Strength, Home_Strength)) %>% 
            ungroup() %>% 
            mutate(Keep = ifelse((Team == df_team_info$Team[side_vars$Side1_Row] & Side1_Strength > Side2_Strength) | (Team != df_team_info$Team[side_vars$Side1_Row] & Side1_Strength < Side2_Strength), "Yes", "No")) %>% 
            filter(Keep == "Yes")
        } else {
          if(input$boxscore_strength == "Short-Handed") {
            rdf_df2 <- rdf_df2 %>% 
              group_by(Game) %>% 
              mutate(Side1_Strength = ifelse(Home_Team == df_team_info$Team[side_vars$Side1_Row], 
                                             Home_Strength, Away_Strength), 
                     Side2_Strength = ifelse(Home_Team == df_team_info$Team[side_vars$Side1_Row], 
                                             Away_Strength, Home_Strength)) %>% 
              ungroup() %>% 
              mutate(Keep = ifelse((Team == df_team_info$Team[side_vars$Side1_Row] & Side1_Strength < Side2_Strength) | (Team != df_team_info$Team[side_vars$Side1_Row] & Side1_Strength > Side2_Strength), "Yes", "No")) %>% 
              filter(Keep == "Yes")
          }
        }
      }
      
      # Filter down by state input
      if(input$boxscore_state != "All") {
        rdf_df2 <- rdf_df2 %>% 
          filter(State %in% input$boxscore_state)
      }
      
      # Filter depending on Totals or Averages
      if(length(input$boxscore_total_avg) > 0) {
        if(input$boxscore_total_avg == "Totals") {
          rdf_df2 <- rdf_df2 %>%
            rename("PLAYER" = Player1_Name) %>% 
            group_by(PLAYER) %>% 
            summarise("BALL LOST TURNOVER" = length(which(Turnover_Type == "Ball Lost")), 
                      "PASSING TURNOVER" = length(which(Turnover_Type == "Stray Pass" | Turnover_Type == "Interrupted Pass")))
        } else {
          games <- length(unique(rdf_df2$Game))
          rdf_df2 <- rdf_df2 %>%
            rename("PLAYER" = Player1_Name) %>% 
            group_by(PLAYER) %>% 
            summarise("BALL LOST TURNOVER" = round(length(which(Turnover_Type == "Ball Lost")) / games, 1), 
                      "PASSING TURNOVER" = round(length(which(Turnover_Type == "Stray Pass" | Turnover_Type == "Interrupted Pass")) / games, 1))
        }
      } else {
        rdf_df2 <- rdf_df2 %>%
          rename("PLAYER" = Player1_Name) %>% 
          group_by(PLAYER) %>% 
          summarise("BALL LOST TURNOVER" = length(which(Turnover_Type == "Ball Lost")), 
                    "PASSING TURNOVER" = length(which(Turnover_Type == "Stray Pass" | Turnover_Type == "Interrupted Pass")))
      }
      
      ## Blocked Shots
      
      rdf_df3 <- rdf_df %>% 
        filter(Event_Type == "Shot" & Result == "Blocked") %>% 
        filter(Shot_Blocking_Team == df_team_info$Team[side_vars$Side1_Row])
      
      if(nrow(rdf_df3) > 0) {
        # Filter down by state input
        if(input$boxscore_state != "All") {
          rdf_df3 <- rdf_df3 %>% 
            filter(State %in% input$boxscore_state)
        }
        
        # Filter down by strength input
        if(input$boxscore_strength == "Even-Strength") {
          rdf_df3 <- rdf_df3 %>% 
            filter(Home_Strength == Away_Strength)    
        } else {
          if(input$boxscore_strength == "Man-Advantage") {
            rdf_df3 <- rdf_df3 %>% 
              mutate(Keep = ifelse((Home_Team == df_team_info$Team[side_vars$Side1_Row] & Home_Strength > Away_Strength) | (Home_Team != df_team_info$Team[side_vars$Side1_Row] & Home_Strength < Away_Strength), "Yes", "No")) %>% 
              filter(Keep == "Yes")
          } else {
            if(input$boxscore_strength == "Short-Handed") {
              rdf_df3 <- rdf_df3 %>% 
                mutate(Keep = ifelse((Home_Team == df_team_info$Team[side_vars$Side1_Row] & Home_Strength < Away_Strength) | (Home_Team != df_team_info$Team[side_vars$Side1_Row] & Home_Strength > Away_Strength), "Yes", "No")) %>% 
                filter(Keep == "Yes")
            }
          }
        }
        
        # Filter depending on Totals or Averages
        if(length(input$boxscore_total_avg) > 0) {
          if(input$boxscore_total_avg == "Totals") {
            rdf_df3 <- rdf_df3 %>% 
              rename("PLAYER" = Shot_Blocker_Name) %>% 
              group_by(PLAYER) %>% 
              summarise("BLOCKED SHOTS" = length(which(!is.na(Shot_Blocking_Team))))
          } else {
            games <- length(unique(rdf_df3$Game))
            rdf_df3 <- rdf_df3 %>% 
              rename("PLAYER" = Shot_Blocker_Name) %>% 
              group_by(PLAYER) %>% 
              summarise("BLOCKED SHOTS" = round(length(which(!is.na(Shot_Blocking_Team))) / games, 1))
          }
        } else {
          rdf_df3 <- rdf_df3 %>% 
            rename("PLAYER" = Shot_Blocker_Name) %>% 
            group_by(PLAYER) %>% 
            summarise("BLOCKED SHOTS" = length(which(!is.na(Shot_Blocking_Team))))
        }
      }
      
      rdf_df1 <- rdf_df1 %>% 
        filter(!is.na(PLAYER))
      rdf_df2 <- rdf_df2 %>% 
        filter(!is.na(PLAYER))
      if(nrow(rdf_df3) > 0) {
        rdf_df3 <- rdf_df3 %>% 
          filter(!is.na(PLAYER))
        rdf_df <- full_join(rdf_df1, rdf_df3, by = "PLAYER")
        rdf_df <- full_join(rdf_df, rdf_df2, by = "PLAYER")
      } else {
        rdf_df <- full_join(rdf_df1, rdf_df2, by = "PLAYER")
      }
      rdf_df[is.na(rdf_df)] <- 0
      rdf_df <- rdf_df %>% 
        arrange(desc(`DEF. REB`))
      
      datatable(rdf_df, 
                options = list(
                  paging = TRUE,
                  pageLength = 10, 
                  autowidth = FALSE, 
                  searching = FALSE, 
                  info = FALSE, 
                  scrollX = TRUE
                ), 
                rownames = FALSE
      )
    }
  })
  
  ##### BOX: Reb/TO Table Logo Side2 #####
  
  output$boxscore_rebto_logo_side2 <- renderUI({
    if(length(input$boxscore_strength) > 0) {
      div(class = "ply_player_title_logo", 
          img(src = df_team_info$Wordmark_Path[which(df_team_info$Team == df_team_info$Team[side_vars$Side2_Row])], 
              height = "100px", width = "auto"))
    }
  })
  
  ##### BOX: Reb/TO Table Side2 #####
  
  output$boxscore_rebto_side2 <- renderDataTable({
    if(length(input$boxscore_strength) > 0) {
      
      rdf_df <- df_master_fltr()
      
      ## Rebounds
      
      if(length(unique(df_master_fltr()$Team)) > 2) {
        rdf_df1 <- rdf_df %>% 
          filter(Rebound_Type %in% c("Offensive", "Defensive")) %>% 
          mutate(Team = ifelse(Team != df_team_info$Team[side_vars$Side1_Row], "Opponent", Team), 
                 Keep = ifelse(Team != df_team_info$Team[side_vars$Side1_Row] & Rebound_Type == "Offensive", "Yes", 
                               ifelse(Team == df_team_info$Team[side_vars$Side1_Row] & Rebound_Type == "Defensive", "Yes", "No"))) %>% 
          filter(Keep == "Yes")
      } else {
        rdf_df1 <- rdf_df %>% 
          filter(Rebound_Type %in% c("Offensive", "Defensive")) %>% 
          mutate(Keep = ifelse(Team == df_team_info$Team[side_vars$Side1_Row] & Rebound_Type == "Offensive", "Yes", 
                               ifelse(Team != df_team_info$Team[side_vars$Side1_Row] & Rebound_Type == "Defensive", "Yes", "No"))) %>% 
          filter(Keep == "Yes")
      }
      
      # Filter down by state input
      if(input$boxscore_state != "All") {
        rdf_df1 <- rdf_df1 %>% 
          filter(State %in% input$boxscore_state)
      }
      
      # Filter down by strength input
      if(input$boxscore_strength == "Even-Strength") {
        rdf_df1 <- rdf_df1 %>% 
          filter(Home_Strength == Away_Strength)    
      } else {
        if(input$boxscore_strength == "Man-Advantage") {
          rdf_df1 <- rdf_df1 %>% 
            mutate(Keep = ifelse((Home_Team == df_team_info$Team[side_vars$Side1_Row] & Away_Strength > Home_Strength) | (Home_Team != df_team_info$Team[side_vars$Side1_Row] & Away_Strength < Home_Strength), "Yes", "No")) %>% 
            filter(Keep == "Yes")
        } else {
          if(input$boxscore_strength == "Short-Handed") {
            rdf_df1 <- rdf_df1 %>% 
              mutate(Keep = ifelse((Home_Team == df_team_info$Team[side_vars$Side1_Row] & Away_Strength < Home_Strength) | (Home_Team != df_team_info$Team[side_vars$Side1_Row] & Away_Strength > Home_Strength), "Yes", "No")) %>% 
              filter(Keep == "Yes")
          }
        }
      }
      
      # Filter depending on Totals or Averages
      if(length(input$boxscore_total_avg) > 0) {
        if(input$boxscore_total_avg == "Totals") {
          rdf_df1 <- rdf_df1 %>% 
            rename("PLAYER" = Rebound_Name) %>% 
            group_by(PLAYER) %>% 
            summarise("DEF. REB" = length(which((Rebound_Type == "Defensive"))),
                      "OFF. REB" = length(which(Rebound_Type == "Offensive")))
        } else {
          games <- length(unique(rdf_df1$Game))
          rdf_df1 <- rdf_df1 %>% 
            rename("PLAYER" = Rebound_Name) %>% 
            group_by(PLAYER) %>% 
            summarise("DEF. REB" = round(length(which((Rebound_Type == "Defensive"))) / games, 1),
                      "OFF. REB" = round(length(which((Rebound_Type == "Offensive"))) / games, 1))
        }
      } else {
        rdf_df1 <- rdf_df1 %>% 
          rename("PLAYER" = Rebound_Name) %>% 
          group_by(PLAYER) %>% 
          summarise("DEF. REB" = length(which(Rebound_Type == "Defensive")),
                    "OFF. REB" = length(which(Rebound_Type == "Offensive")))
      } 
      
      ## Turnovers
      
      rdf_df2 <- rdf_df %>% 
        filter(Event_Type == "Turnover") %>% 
        filter(Team != df_team_info$Team[side_vars$Side1_Row])
      
      if(length(unique(df_master_fltr()$Team)) > 2) {
        rdf_df2 <- rdf_df2 %>% 
          mutate(Team = ifelse(Team != df_team_info$Team[side_vars$Side1_Row], "Opponent", Team))
      }
      
      # Filter down by strength input
      if(input$boxscore_strength == "Even-Strength") {
        rdf_df2 <- rdf_df2 %>% 
          filter(Home_Strength == Away_Strength)    
      } else {
        if(input$boxscore_strength == "Man-Advantage") {
          rdf_df2 <- rdf_df2 %>% 
            group_by(Game) %>% 
            mutate(Side1_Strength = ifelse(Home_Team == df_team_info$Team[side_vars$Side1_Row], 
                                           Home_Strength, Away_Strength), 
                   Side2_Strength = ifelse(Home_Team == df_team_info$Team[side_vars$Side1_Row], 
                                           Away_Strength, Home_Strength)) %>% 
            ungroup() %>% 
            mutate(Keep = ifelse((Team == df_team_info$Team[side_vars$Side1_Row] & Side1_Strength > Side2_Strength) | (Team != df_team_info$Team[side_vars$Side1_Row] & Side1_Strength < Side2_Strength), "Yes", "No")) %>% 
            filter(Keep == "Yes")
        } else {
          if(input$boxscore_strength == "Short-Handed") {
            rdf_df2 <- rdf_df2 %>% 
              group_by(Game) %>% 
              mutate(Side1_Strength = ifelse(Home_Team == df_team_info$Team[side_vars$Side1_Row], 
                                             Home_Strength, Away_Strength), 
                     Side2_Strength = ifelse(Home_Team == df_team_info$Team[side_vars$Side1_Row], 
                                             Away_Strength, Home_Strength)) %>% 
              ungroup() %>% 
              mutate(Keep = ifelse((Team == df_team_info$Team[side_vars$Side1_Row] & Side1_Strength < Side2_Strength) | (Team != df_team_info$Team[side_vars$Side1_Row] & Side1_Strength > Side2_Strength), "Yes", "No")) %>% 
              filter(Keep == "Yes")
          }
        }
      }
      
      # Filter down by state input
      if(input$boxscore_state != "All") {
        rdf_df2 <- rdf_df2 %>% 
          filter(State %in% input$boxscore_state)
      }
      
      # Filter depending on Totals or Averages
      if(length(input$boxscore_total_avg) > 0) {
        if(input$boxscore_total_avg == "Totals") {
          rdf_df2 <- rdf_df2 %>%
            rename("PLAYER" = Player1_Name) %>% 
            group_by(PLAYER) %>% 
            summarise("BALL LOST TURNOVER" = length(which(Turnover_Type == "Ball Lost")), 
                      "PASSING TURNOVER" = length(which(Turnover_Type == "Stray Pass" | Turnover_Type == "Interrupted Pass")))
        } else {
          rdf_df2 <- rdf_df2 %>%
            rename("PLAYER" = Player1_Name) %>% 
            group_by(PLAYER) %>% 
            summarise("BALL LOST TURNOVER" = round(length(which(Turnover_Type == "Ball Lost")) / length(unique(Game)), 1), 
                      "PASSING TURNOVER" = round(length(which(Turnover_Type == "Stray Pass" | Turnover_Type == "Interrupted Pass")) / length(unique(Game)), 1))
        }
      } else {
        rdf_df2 <- rdf_df2 %>%
          rename("PLAYER" = Player1_Name) %>% 
          group_by(PLAYER) %>% 
          summarise("BALL LOST TURNOVER" = length(which(Turnover_Type == "Ball Lost")), 
                    "PASSING TURNOVER" = length(which(Turnover_Type == "Stray Pass" | Turnover_Type == "Interrupted Pass")))
      } 
      
      ## Blocked Shots
      
      rdf_df3 <- rdf_df %>% 
        filter(Event_Type == "Shot" & Result == "Blocked") %>% 
        filter(Shot_Blocking_Team != df_team_info$Team[side_vars$Side1_Row])
      
      if(nrow(rdf_df3) > 0) {
        # Filter down by state input
        if(input$boxscore_state != "All") {
          rdf_df3 <- rdf_df3 %>% 
            filter(State %in% input$boxscore_state)
        }
        
        # Filter down by strength input
        if(input$boxscore_strength == "Even-Strength") {
          rdf_df3 <- rdf_df3 %>% 
            filter(Home_Strength == Away_Strength)    
        } else {
          if(input$boxscore_strength == "Man-Advantage") {
            rdf_df3 <- rdf_df3 %>% 
              group_by(Game) %>% 
              mutate(Side1_Strength = ifelse(Home_Team == df_team_info$Team[side_vars$Side1_Row], 
                                             Home_Strength, Away_Strength), 
                     Side2_Strength = ifelse(Home_Team == df_team_info$Team[side_vars$Side1_Row], 
                                             Away_Strength, Home_Strength)) %>% 
              ungroup() %>% 
              mutate(Keep = ifelse((Team == df_team_info$Team[side_vars$Side1_Row] & Side1_Strength > Side2_Strength) | (Team != df_team_info$Team[side_vars$Side1_Row] & Side1_Strength < Side2_Strength), "Yes", "No")) %>% 
              filter(Keep == "Yes")
          } else {
            if(input$boxscore_strength == "Short-Handed") {
              rdf_df3 <- rdf_df3 %>% 
                group_by(Game) %>% 
                mutate(Side1_Strength = ifelse(Home_Team == df_team_info$Team[side_vars$Side1_Row], 
                                               Home_Strength, Away_Strength), 
                       Side2_Strength = ifelse(Home_Team == df_team_info$Team[side_vars$Side1_Row], 
                                               Away_Strength, Home_Strength)) %>% 
                ungroup() %>% 
                mutate(Keep = ifelse((Team == df_team_info$Team[side_vars$Side1_Row] & Side1_Strength < Side2_Strength) | (Team != df_team_info$Team[side_vars$Side1_Row] & Side1_Strength > Side2_Strength), "Yes", "No")) %>% 
                filter(Keep == "Yes")
            }
          }
        }
        
        # Filter depending on Totals or Averages
        if(length(input$boxscore_total_avg) > 0) {
          if(input$boxscore_total_avg == "Totals") {
            rdf_df3 <- rdf_df3 %>% 
              rename("PLAYER" = Shot_Blocker_Name) %>% 
              group_by(PLAYER) %>% 
              summarise("BLOCKED SHOTS" = length(which(!is.na(Shot_Blocking_Team))))
          } else {
            games <- length(unique(rdf_df3$Game))
            rdf_df3 <- rdf_df3 %>% 
              rename("PLAYER" = Shot_Blocker_Name) %>% 
              group_by(PLAYER) %>% 
              summarise("BLOCKED SHOTS" = round(length(which(!is.na(Shot_Blocking_Team))) / games, 1))
          }
        } else {
          rdf_df3 <- rdf_df3 %>% 
            rename("PLAYER" = Shot_Blocker_Name) %>% 
            group_by(PLAYER) %>% 
            summarise("BLOCKED SHOTS" = length(which(!is.na(Shot_Blocking_Team))))
        }
      }
      
      rdf_df1 <- rdf_df1 %>% 
        filter(!is.na(PLAYER))
      rdf_df2 <- rdf_df2 %>% 
        filter(!is.na(PLAYER))
      if(nrow(rdf_df3) > 0) {
        rdf_df3 <- rdf_df3 %>% 
          filter(!is.na(PLAYER))
        rdf_df <- full_join(rdf_df1, rdf_df3, by = "PLAYER")
        rdf_df <- full_join(rdf_df, rdf_df2, by = "PLAYER")
      } else {
        rdf_df <- full_join(rdf_df1, rdf_df2, by = "PLAYER")
      }
      rdf_df[is.na(rdf_df)] <- 0
      rdf_df <- rdf_df %>% 
        arrange(desc(`DEF. REB`))
      
      datatable(rdf_df, 
                options = list(
                  paging = TRUE,
                  pageLength = 10, 
                  autowidth = FALSE, 
                  searching = FALSE, 
                  info = FALSE, 
                  scrollX = TRUE
                ), 
                rownames = FALSE
      )
    }
  })
  
  ##### BOX: Faceoffs Table Logo Side1 #####
  
  output$boxscore_fo_logo_side1 <- renderUI({
    if(length(input$boxscore_strength) > 0) {
      div(class = "ply_player_title_logo", 
          img(src = df_team_info$Wordmark_Path[which(df_team_info$Team == df_team_info$Team[side_vars$Side1_Row])], 
              height = "100px", width = "auto"))
    }
  })
  
  ##### BOX: Faceoffs Table Side1 #####
  
  output$boxscore_fo_side1 <- renderDataTable({
    if(length(input$boxscore_strength) > 0) {
      
      rdf_df <- df_master_fltr()
      
      if(length(unique(df_master_fltr()$Team)) > 2) {
        rdf_df2 <- rdf_df %>%
          mutate(FO_Winning_Team = ifelse(FO_Winning_Team != df_team_info$Team[side_vars$Side1_Row], "Opponent", FO_Winning_Team)) %>% 
          mutate(FO_Winning_Team = ifelse(FO_Result == "Unclear", "Neither", FO_Winning_Team)) %>% 
          filter(!is.na(FO_Winning_Team)) %>% 
          mutate(FO_Side1_Name = ifelse(Home_Team == df_team_info$Team[side_vars$Side1_Row], FO_Home_Name, FO_Away_Name), 
                 FO_Side2_Name = ifelse(Home_Team != df_team_info$Team[side_vars$Side1_Row], FO_Away_Name, FO_Home_Name))
      } else {
        rdf_df2 <- rdf_df %>%
          mutate(FO_Winning_Team = ifelse(FO_Result == "Unclear", "Neither", FO_Winning_Team)) %>% 
          filter(!is.na(FO_Winning_Team)) %>% 
          mutate(FO_Side1_Name = ifelse(Home_Team == df_team_info$Team[side_vars$Side1_Row], FO_Home_Name, FO_Away_Name), 
                 FO_Side2_Name = ifelse(Home_Team != df_team_info$Team[side_vars$Side1_Row], FO_Away_Name, FO_Home_Name))
      }
      
      # Filter down by strength input
      if(input$boxscore_strength == "Even-Strength") {
        rdf_df2 <- rdf_df2 %>% 
          filter(Home_Strength == Away_Strength)    
      } else {
        if(input$boxscore_strength == "Man-Advantage") {
          rdf_df2 <- rdf_df2 %>% 
            mutate(Keep = ifelse((Home_Team == df_team_info$Team[side_vars$Side1_Row] & Home_Strength > Away_Strength) | (Home_Team != df_team_info$Team[side_vars$Side1_Row] & Home_Strength < Away_Strength), "Yes", "No")) %>% 
            filter(Keep == "Yes")
        } else {
          if(input$boxscore_strength == "Short-Handed") {
            rdf_df2 <- rdf_df2 %>% 
              mutate(Keep = ifelse((Home_Team == df_team_info$Team[side_vars$Side1_Row] & Home_Strength < Away_Strength) | (Home_Team != df_team_info$Team[side_vars$Side1_Row] & Home_Strength > Away_Strength), "Yes", "No")) %>% 
              filter(Keep == "Yes")
          }
        }
      }
      
      # Filter depending on Totals or Averages
      if(length(input$boxscore_total_avg) > 0) {
        if(input$boxscore_total_avg == "Totals") {
          rdf_df2 <- rdf_df2 %>%
            rename("PLAYER" = FO_Side1_Name) %>% 
            group_by(PLAYER) %>% 
            summarise("FACEOFFS" = length(which(!is.na(FO_Result))), 
                      "CLEAN WIN %" = round((length(which((FO_Result == "Clean" | FO_Result == "Keeper" | FO_Result == "Direct Flip") & 
                                                            FO_Winning_Team == df_team_info$Team[side_vars$Side1_Row])) / length(which(!is.na(FO_Result)))), 
                                            digits = 3)*100, 
                      "WING PICK-UP %" = round((length(which(FO_Result == "Wing" & FO_Winning_Team == df_team_info$Team[side_vars$Side1_Row])) / length(which(!is.na(FO_Result)))), 
                                               digits = 3)*100)
        } else {
          games <- length(unique(rdf_df2$Game))
          rdf_df2 <- rdf_df2 %>%
            rename("PLAYER" = FO_Side1_Name) %>% 
            group_by(PLAYER) %>% 
            summarise("FACEOFFS" = round(length(which(!is.na(FO_Result))) / games, 1), 
                      "CLEAN WIN %" = round((length(which((FO_Result == "Clean" | FO_Result == "Keeper" | FO_Result == "Direct Flip") & 
                                                            FO_Winning_Team == df_team_info$Team[side_vars$Side1_Row])) / length(which(!is.na(FO_Result)))), 
                                            digits = 3)*100,
                      "WING PICK-UP %" = round((length(which(FO_Result == "Wing" & FO_Winning_Team == df_team_info$Team[side_vars$Side1_Row])) / length(which(!is.na(FO_Result)))), 
                                               digits = 3)*100)
        }
      } else {
        rdf_df2 <- rdf_df2 %>%
          rename("PLAYER" = FO_Side1_Name) %>% 
          group_by(PLAYER) %>% 
          summarise("FACEOFFS" = length(which(!is.na(FO_Result))), 
                    "CLEAN WIN %" = round((length(which((FO_Result == "Clean" | FO_Result == "Keeper" | FO_Result == "Direct Flip") & 
                                                          FO_Winning_Team == df_team_info$Team[side_vars$Side1_Row])) / length(which(!is.na(FO_Result)))), 
                                          digits = 3)*100,
                    "WING PICK-UP %" = round((length(which(FO_Result == "Wing" & FO_Winning_Team == df_team_info$Team[side_vars$Side1_Row])) / length(which(!is.na(FO_Result)))), 
                                             digits = 3)*100)
      } 
      
      rdf_df2 <- rdf_df2 %>% 
        filter(!is.na(PLAYER)) %>% 
        arrange(desc(FACEOFFS))
      
      datatable(rdf_df2, 
                options = list(
                  paging = TRUE,
                  pageLength = 10, 
                  autowidth = FALSE, 
                  searching = FALSE, 
                  info = FALSE, 
                  scrollX = TRUE
                ), 
                rownames = FALSE
      )
    }
  })
  
  ##### BOX: Faceoffs Table Logo Side2 #####
  
  output$boxscore_fo_logo_side2 <- renderUI({
    if(length(input$boxscore_strength) > 0) {
      div(class = "ply_player_title_logo", 
          img(src = df_team_info$Wordmark_Path[which(df_team_info$Team == df_team_info$Team[side_vars$Side2_Row])], 
              height = "100px", width = "auto"))
    }
  })
  
  ##### BOX: Faceoffs Table Side2 #####
  
  output$boxscore_fo_side2 <- renderDataTable({
    if(length(input$boxscore_strength) > 0) {
      
      rdf_df <- df_master_fltr()
      
      if(length(unique(df_master_fltr()$Team)) > 2) {
        rdf_df2 <- rdf_df %>%
          mutate(FO_Winning_Team = ifelse(FO_Winning_Team != df_team_info$Team[side_vars$Side1_Row], "Opponent", FO_Winning_Team)) %>% 
          mutate(FO_Winning_Team = ifelse(FO_Result == "Unclear", "Neither", FO_Winning_Team)) %>% 
          filter(!is.na(FO_Winning_Team)) %>% 
          mutate(FO_Side1_Name = ifelse(Home_Team == df_team_info$Team[side_vars$Side1_Row], FO_Home_Name, FO_Away_Name), 
                 FO_Side2_Name = ifelse(Home_Team == df_team_info$Team[side_vars$Side1_Row], FO_Away_Name, FO_Home_Name))
      } else {
        rdf_df2 <- rdf_df %>%
          mutate(FO_Winning_Team = ifelse(FO_Result == "Unclear", "Neither", FO_Winning_Team)) %>% 
          filter(!is.na(FO_Winning_Team)) %>% 
          mutate(FO_Side1_Name = ifelse(Home_Team == df_team_info$Team[side_vars$Side1_Row], FO_Home_Name, FO_Away_Name), 
                 FO_Side2_Name = ifelse(Home_Team == df_team_info$Team[side_vars$Side1_Row], FO_Away_Name, FO_Home_Name))
      }
      
      # Filter down by strength input
      if(input$boxscore_strength == "Even-Strength") {
        rdf_df2 <- rdf_df2 %>% 
          filter(Home_Strength == Away_Strength)    
      } else {
        if(input$boxscore_strength == "Man-Advantage") {
          rdf_df2 <- rdf_df2 %>% 
            mutate(Keep = ifelse((Home_Team == df_team_info$Team[side_vars$Side1_Row] & Away_Strength > Home_Strength) | (Home_Team != df_team_info$Team[side_vars$Side1_Row] & Away_Strength < Home_Strength), "Yes", "No")) %>% 
            filter(Keep == "Yes")
        } else {
          if(input$boxscore_strength == "Short-Handed") {
            rdf_df2 <- rdf_df2 %>% 
              mutate(Keep = ifelse((Home_Team == df_team_info$Team[side_vars$Side1_Row] & Away_Strength < Home_Strength) | (Home_Team != df_team_info$Team[side_vars$Side1_Row] & Away_Strength > Home_Strength), "Yes", "No")) %>% 
              filter(Keep == "Yes")
          }
        }
      }
      
      # Filter depending on Totals or Averages
      if(length(input$boxscore_total_avg) > 0) {
        if(input$boxscore_total_avg == "Totals") {
          rdf_df2 <- rdf_df2 %>%
            rename("PLAYER" = FO_Side2_Name) %>% 
            group_by(PLAYER) %>% 
            summarise("FACEOFFS" = length(which(!is.na(FO_Result))), 
                      "CLEAN WIN %" = round((length(which((FO_Result == "Clean" | FO_Result == "Keeper" | FO_Result == "Direct Flip") & 
                                                            FO_Winning_Team != df_team_info$Team[side_vars$Side1_Row])) / length(which(!is.na(FO_Result)))), 
                                            digits = 3)*100,
                      "WING PICK-UP %" = round((length(which(FO_Result == "Wing" & FO_Winning_Team != df_team_info$Team[side_vars$Side1_Row])) / length(which(!is.na(FO_Result)))), 
                                               digits = 3)*100)
        } else {
          rdf_df2 <- rdf_df2 %>%
            rename("PLAYER" = FO_Side2_Name) %>% 
            group_by(PLAYER) %>% 
            summarise("FACEOFFS" = round(length(which(!is.na(FO_Result))) / length(unique(Game)), 1), 
                      "CLEAN WIN %" = round((length(which((FO_Result == "Clean" | FO_Result == "Keeper" | FO_Result == "Direct Flip") & 
                                                            FO_Winning_Team != df_team_info$Team[side_vars$Side1_Row])) / length(which(!is.na(FO_Result)))), 
                                            digits = 3)*100,
                      "WING PICK-UP %" = round((length(which(FO_Result == "Wing" & FO_Winning_Team != df_team_info$Team[side_vars$Side1_Row])) / length(which(!is.na(FO_Result)))), 
                                               digits = 3)*100)
        }
      } else {
        rdf_df2 <- rdf_df2 %>%
          rename("PLAYER" = FO_Side2_Name) %>% 
          group_by(PLAYER) %>% 
          summarise("FACEOFFS" = length(which(!is.na(FO_Result))), 
                    "CLEAN WIN %" = round((length(which((FO_Result == "Clean" | FO_Result == "Keeper" | FO_Result == "Direct Flip") & 
                                                          FO_Winning_Team != df_team_info$Team[side_vars$Side1_Row])) / length(which(!is.na(FO_Result)))), 
                                          digits = 3)*100,
                    "WING PICK-UP %" = round((length(which(FO_Result == "Wing" & FO_Winning_Team != df_team_info$Team[side_vars$Side1_Row])) / length(which(!is.na(FO_Result)))), 
                                             digits = 3)*100)
      } 
      
      rdf_df2 <- rdf_df2 %>% 
        filter(!is.na(PLAYER)) %>% 
        arrange(desc(FACEOFFS))
      
      datatable(rdf_df2, 
                options = list(
                  paging = TRUE,
                  pageLength = 10, 
                  autowidth = FALSE, 
                  searching = FALSE, 
                  info = FALSE, 
                  scrollX = TRUE
                ), 
                rownames = FALSE
      )
    }
  })
  
  ##### BOX: Goalies Table Logo Side1 #####
  
  output$boxscore_goalies_logo_side1 <- renderUI({
    if(length(input$boxscore_strength) > 0) {
      div(class = "ply_player_title_logo", 
          img(src = df_team_info$Wordmark_Path[which(df_team_info$Team == df_team_info$Team[side_vars$Side1_Row])], 
              height = "100px", width = "auto"))
    }
  })
  
  ##### BOX: Goalies Table Side1 #####
  
  output$boxscore_goalies_side1 <- renderDataTable({
    if(length(input$boxscore_strength) > 0) {
      
      shot_df <- df_master_fltr()
      
      shot_df <- shot_df %>%
        filter(Event_Type == "Shot") %>% 
        filter(Team != df_team_info$Team[side_vars$Side1_Row]) %>% 
        mutate(Side1_Goalie_Name = ifelse(Home_Team == df_team_info$Team[side_vars$Side1_Row], Home_Goalie_Name, Away_Goalie_Name), 
               Side2_Goalie_Name = ifelse(Home_Team != df_team_info$Team[side_vars$Side1_Row], Home_Goalie_Name, Away_Goalie_Name))
      
      # Filter down by strength input
      if(input$boxscore_strength == "Even-Strength") {
        shot_df <- shot_df %>% 
          filter(Home_Strength == Away_Strength)    
      } else {
        if(input$boxscore_strength == "Man-Advantage") {
          shot_df <- shot_df %>% 
            mutate(Keep = ifelse((Home_Team == df_team_info$Team[side_vars$Side1_Row] & Home_Strength < Away_Strength) | (Home_Team != df_team_info$Team[side_vars$Side1_Row] & Home_Strength > Away_Strength), "Yes", "No")) %>% 
            filter(Keep == "Yes")
        } else {
          if(input$boxscore_strength == "Short-Handed") {
            shot_df <- shot_df %>% 
              mutate(Keep = ifelse((Home_Team == df_team_info$Team[side_vars$Side1_Row] & Home_Strength > Away_Strength) | (Home_Team != df_team_info$Team[side_vars$Side1_Row] & Home_Strength < Away_Strength), "Yes", "No")) %>% 
              filter(Keep == "Yes")
          }
        }
      }
      
      # Filter down by state input
      if(input$boxscore_state != "All") {
        shot_df <- shot_df %>% 
          filter(State %in% input$boxscore_state)
      }
      
      # Filter depending on Totals or Averages
      if(length(input$boxscore_total_avg) > 0) {
        if(input$boxscore_total_avg == "Totals") {
          shot_df <- shot_df %>% 
            rename("PLAYER" = Side1_Goalie_Name) %>% 
            group_by(PLAYER) %>% 
            summarise("SHOTS AGAINST" = length(which(!is.na(Result))), 
                      "SHOOT % AGAINST" = round((length(which(Result == "Goal")) / length(which(!is.na(Result))))*100, digits = 1), 
                      "% of SAVES CLEAN" = round((length(which(Goalie_Response == "Clean Save")) / 
                                                    length(which(Result == "Miss" & Net_Location %in% shot_on)))*100, digits = 1))
        } else {
          games <- length(unique(shot_df$Game))
          shot_df <- shot_df %>% 
            rename("PLAYER" = Side1_Goalie_Name) %>% 
            group_by(PLAYER) %>% 
            summarise("SHOTS AGAINST" = round(length(which(!is.na(Result))) / games, 1), 
                      "SHOOT % AGAINST" = round((length(which(Result == "Goal")) / length(which(!is.na(Result))))*100, digits = 1), 
                      "% of SAVES CLEAN" = round((length(which(Goalie_Response == "Clean Save")) / 
                                                    length(which(Result == "Miss" & Net_Location %in% shot_on)))*100, digits = 1))
        }
      } else {
        shot_df <- shot_df %>% 
          rename("PLAYER" = Side1_Goalie_Name) %>% 
          group_by(PLAYER) %>% 
          summarise("SHOTS AGAINST" = length(which(!is.na(Result))), 
                    "SHOOT % AGAINST" = round((length(which(Result == "Goal")) / length(which(!is.na(Result))))*100, digits = 1), 
                    "% of SAVES CLEAN" = round((length(which(Goalie_Response == "Clean Save")) / 
                                                  length(which(Result == "Miss" & Net_Location %in% shot_on)))*100, digits = 1))
      } 
      
      shot_df <- shot_df %>% 
        filter(!is.na(PLAYER)) %>% 
        arrange(desc(`SHOTS AGAINST`))
      
      datatable(shot_df, 
                options = list(
                  paging = TRUE,
                  pageLength = 10, 
                  autowidth = FALSE, 
                  searching = FALSE, 
                  info = FALSE, 
                  scrollX = TRUE
                ), 
                rownames = FALSE
      )
    }
  })
  
  ##### BOX: Goalies Table Logo Side2 #####
  
  output$boxscore_goalies_logo_side2 <- renderUI({
    if(length(input$boxscore_strength) > 0) {
      div(class = "ply_player_title_logo", 
          img(src = df_team_info$Wordmark_Path[which(df_team_info$Team == df_team_info$Team[side_vars$Side2_Row])], 
              height = "100px", width = "auto"))
    }
  })
  
  ##### BOX: Goalies Table Side2 #####
  
  output$boxscore_goalies_side2 <- renderDataTable({
    if(length(input$boxscore_strength) > 0) {
      
      shot_df <- df_master_fltr()
      
      shot_df <- shot_df %>%
        filter(Event_Type == "Shot") %>% 
        filter(Team == df_team_info$Team[side_vars$Side1_Row]) %>% 
        mutate(Side1_Goalie_Name = ifelse(Home_Team == df_team_info$Team[side_vars$Side1_Row], Home_Goalie_Name, Away_Goalie_Name), 
               Side2_Goalie_Name = ifelse(Home_Team != df_team_info$Team[side_vars$Side1_Row], Home_Goalie_Name, Away_Goalie_Name))
      
      # Filter down by strength input
      if(input$boxscore_strength == "Even-Strength") {
        shot_df <- shot_df %>% 
          filter(Home_Strength == Away_Strength)    
      } else {
        if(input$boxscore_strength == "Man-Advantage") {
          shot_df <- shot_df %>% 
            mutate(Keep = ifelse((Home_Team == df_team_info$Team[side_vars$Side1_Row] & Away_Strength < Home_Strength) | (Home_Team != df_team_info$Team[side_vars$Side1_Row] & Away_Strength > Home_Strength), "Yes", "No")) %>% 
            filter(Keep == "Yes")
        } else {
          if(input$boxscore_strength == "Short-Handed") {
            shot_df <- shot_df %>% 
              mutate(Keep = ifelse((Home_Team == df_team_info$Team[side_vars$Side1_Row] & Away_Strength > Home_Strength) | (Home_Team != df_team_info$Team[side_vars$Side1_Row] & Away_Strength < Home_Strength), "Yes", "No")) %>% 
              filter(Keep == "Yes")
          }
        }
      }
      
      # Filter down by state input
      if(input$boxscore_state != "All") {
        shot_df <- shot_df %>% 
          filter(State %in% input$boxscore_state)
      }
      
      # Filter depending on Totals or Averages
      if(length(input$boxscore_total_avg) > 0) {
        if(input$boxscore_total_avg == "Totals") {
          shot_df <- shot_df %>% 
            rename("PLAYER" = Side2_Goalie_Name) %>% 
            group_by(PLAYER) %>% 
            summarise("SHOTS AGAINST" = length(which(!is.na(Result))), 
                      "SHOOT % AGAINST" = round((length(which(Result == "Goal")) / length(which(!is.na(Result))))*100, digits = 1), 
                      "% of SAVES CLEAN" = round((length(which(Goalie_Response == "Clean Save")) / 
                                                    length(which(Result == "Miss" & Net_Location %in% shot_on)))*100, digits = 1))
        } else {
          shot_df <- shot_df %>% 
            rename("PLAYER" = Side2_Goalie_Name) %>% 
            group_by(PLAYER) %>% 
            summarise("SHOTS AGAINST" = round(length(which(!is.na(Result))) / length(unique(Game)), 1), 
                      "SHOOT % AGAINST" = round((length(which(Result == "Goal")) / length(which(!is.na(Result))))*100, digits = 1), 
                      "% of SAVES CLEAN" = round((length(which(Goalie_Response == "Clean Save")) / 
                                                    length(which(Result == "Miss" & Net_Location %in% shot_on)))*100, digits = 1))
        }
      } else {
        shot_df <- shot_df %>% 
          rename("PLAYER" = Side2_Goalie_Name) %>% 
          group_by(PLAYER) %>% 
          summarise("SHOTS AGAINST" = length(which(!is.na(Result))), 
                    "SHOOT % AGAINST" = round((length(which(Result == "Goal")) / length(which(!is.na(Result))))*100, digits = 1), 
                    "% of SAVES CLEAN" = round((length(which(Goalie_Response == "Clean Save")) / 
                                                  length(which(Result == "Miss" & Net_Location %in% shot_on)))*100, digits = 1))
      } 
      
      shot_df <- shot_df %>% 
        filter(!is.na(PLAYER)) %>% 
        arrange(desc(`SHOTS AGAINST`))
      
      datatable(shot_df, 
                options = list(
                  paging = TRUE,
                  pageLength = 10, 
                  autowidth = FALSE, 
                  searching = FALSE, 
                  info = FALSE, 
                  scrollX = TRUE
                ), 
                rownames = FALSE
      )
    }
  })
  
  #### Server - Scoring (SCO) ####
  
  ##### SCO: Presubmit Screen #####
  
  output$scoring_presubmit <- renderUI({
    if(side_vars$Side1_Row == "none") {
      fluidPage(
        br(), 
        br(), 
        br(), 
        br(), 
        
        wellPanel(
          h1("Press SUBMIT to View"), 
          
          style = "text-align: center; height: 250px; box-shadow: 0px 10px 5px 5px #888888; border-width: thick;"
        )
      )
    } else {
      fluidPage(
        br(), 
        br(), 
        
        uiOutput(outputId = "scoring_tab")
      )
    }
  })
  
  ##### SCO: Tab UI #####
  
  output$scoring_tab <- renderUI({
    if(lastest_submit$Current_Submission == "Team Summary") {
      fluidPage(
        fluidRow(
          column(12, uiOutput(outputId = "scoring_filters"))
        ), 
        
        fluidRow(
          column(12,
                 wellPanel(
                   h1("SHOOTING PROFILE"),
                   fluidRow(
                     column(6, 
                            h3(textOutput(outputId = "scoring_profile_side1_title")), 
                            plotlyOutput(outputId = "scoring_profile_side1", height = "160px"), 
                            plotlyOutput(outputId = "scoring_poss_side1", height = "160px"), 
                            plotlyOutput(outputId = "scoring_create_side1", height = "200px")), 
                     column(6, 
                            h3(textOutput(outputId = "scoring_profile_side2_title")), 
                            plotlyOutput(outputId = "scoring_profile_side2", height = "160px"), 
                            plotlyOutput(outputId = "scoring_poss_side2", height = "160px"), 
                            plotlyOutput(outputId = "scoring_create_side2", height = "200px"))
                   )
                 )
          )
        ),
        
        fluidRow(
          column(
            width = 4, offset = 4, 
            actionButton(
              inputId = "sco_resetclicks_bttn", label = "RESET LOCATION CLICKS", width = "auto", 
              style = "color: #ff1400; border-color: #ff1400;"
            )
          )
        ), 
        
        br(), br(), 
        
        fluidRow(
          column(6, 
                 wellPanel(
                   h1(paste(toupper(df_team_info$Team[side_vars$Side1_Row]), " SHOOTING", sep = "")), 
                   plotlyOutput(outputId = "scoring_side1_goal", width = "100%", height = "500px"), 
                   plotlyOutput(outputId = "scoring_side1_shotangle", width = "100%", height = "600px"), 
                   plotlyOutput(outputId = "scoring_side1_field", width = "100%", height = "850px")
                 )
          ), 
          
          column(6, 
                 wellPanel(
                   h1("OPPONENT SHOOTING"), 
                   plotlyOutput(outputId = "scoring_side2_goal", width = "100%", height = "500px"), 
                   plotlyOutput(outputId = "scoring_side2_shotangle", width = "100%", height = "600px"), 
                   plotlyOutput(outputId = "scoring_side2_field", width = "100%", height = "850px")
                 )
          )
        ), 
        
        fluidRow(
          column(
            width = 12, 
            dropdownButton(
              circle = FALSE, 
              size = "lg", 
              label = h3("CLICK INSTRUCTIONS", style = "color: red;"), 
              up = TRUE, 
              h4("Clicking a location on one view will show the shooting at that locaiton in the other views"), 
              h4("Reset Clicks button will reset to all shots selected"), 
              h4("Multiple locations can be selected in each view")
            )
          )
        )
      )
    } else {
      fluidPage(
        fluidRow(
          column(12, uiOutput(outputId = "scoring_filters"))
        ), 
        
        fluidRow(
          column(12,
                 wellPanel(
                   h1("SHOOTING PROFILE"),
                   fluidRow(
                     column(6,  
                            h3(textOutput(outputId = "scoring_profile_side1_title")), 
                            plotlyOutput(outputId = "scoring_profile_side1", height = "160px"), 
                            plotlyOutput(outputId = "scoring_poss_side1", height = "160px"), 
                            plotlyOutput(outputId = "scoring_create_side1", height = "200px")), 
                     column(6, 
                            h3(textOutput(outputId = "scoring_profile_side2_title")), 
                            plotlyOutput(outputId = "scoring_profile_side2", height = "160px"), 
                            plotlyOutput(outputId = "scoring_poss_side2", height = "160px"), 
                            plotlyOutput(outputId = "scoring_create_side2", height = "200px"))
                   )
                 )
          )
        ),
        
        fluidRow(
          column(
            width = 4, offset = 4, 
            actionButton(
              inputId = "sco_resetclicks_bttn", label = "RESET LOCATION CLICKS", width = "auto", 
              style = "color: #ff1400; border-color: #ff1400;"
            )
          )
        ), 
        
        br(), br(), 
        
        fluidRow(
          column(6, 
                 wellPanel(
                   h1(paste(toupper(df_team_info$Team[side_vars$Side1_Row]), " SHOOTING", sep = "")), 
                   plotlyOutput(outputId = "scoring_side1_goal", width = "100%", height = "500px"), 
                   plotlyOutput(outputId = "scoring_side1_shotangle", width = "100%", height = "600px"), 
                   plotlyOutput(outputId = "scoring_side1_field", width = "100%", height = "850px")
                 )
          ), 
          
          column(6, 
                 wellPanel(
                   h1(paste(toupper(df_team_info$Team[side_vars$Side2_Row]), " SHOOTING", sep = "")), 
                   plotlyOutput(outputId = "scoring_side2_goal", width = "100%", height = "500px"), 
                   plotlyOutput(outputId = "scoring_side2_shotangle", width = "100%", height = "600px"), 
                   plotlyOutput(outputId = "scoring_side2_field", width = "100%", height = "850px")
                 )
          )
        ), 
        
        fluidRow(
          column(
            width = 12, 
            dropdownButton(
              circle = FALSE, 
              size = "lg", 
              label = h3("CLICK INSTRUCTIONS", style = "color: red;"), 
              up = TRUE, 
              h4("Clicking a location on one view will show the shooting at that locaiton in the other views"), 
              h4("Reset Clicks button will reset to all shots selected"), 
              h4("Multiple locations can be selected in each view")
            )
          )
        )
      )
    } 
  })
  
  ##### SCO: Filters #####
  
  output$scoring_filters <- renderUI({
    if(length(unique(df_master_fltr()$Game)) > 1) {
      wellPanel(
        fluidRow(
          column(
            width = 3, 
            h3("SEASON"), 
            prettyRadioButtons(inputId = "scoring_season", label = "", 
                               choices = c("Cumulative", unique(df_master_fltr()$Season)), 
                               selected = as.character(max(unique(df_master_fltr()$Season))), status = "primary", shape = "round", outline = T, 
                               fill = T, thick = T, animation = "smooth", icon = icon("check"), plain = T, 
                               bigger = T, inline = T)
          ), 
          
          column(
            width = 2, 
            h3("TOTAL OR AVG"), 
            prettyRadioButtons(inputId = "scoring_total_avg", label = "", 
                               choices = c("Totals", "Averages"), 
                               selected = "Totals", status = "primary", shape = "round", outline = T, 
                               fill = T, thick = T, animation = "smooth", icon = icon("check"), plain = T, 
                               bigger = T, inline = T)
          ), 
          
          column(
            width = 3, 
            h3("STATE OF THE OFFENSE"), 
            prettyRadioButtons(inputId = "scoring_state", label = "", 
                               choices = c("All", "Set", "Transition"), 
                               selected = "All", status = "primary", shape = "round", outline = T, 
                               fill = T, thick = T, animation = "smooth", icon = icon("check"), plain = T, 
                               bigger = T, inline = T)
          ), 
          
          column(
            width = 4, 
            h3("STRENGTH OF THE OFFENSE"), 
            prettyRadioButtons(inputId = "scoring_strength", label = "", 
                               choices = c("All", "Even-Strength", "Man-Advantage", "Short-Handed"), 
                               selected = "All", status = "primary", shape = "round", outline = T, 
                               fill = T, thick = T, animation = "smooth", icon = icon("check"), plain = T, 
                               bigger = T, inline = T)
          )
        )
      )
    } else {
      wellPanel(
        fluidRow(
          column(
            width = 6, 
            h3("STATE OF THE OFFENSE"), 
            prettyRadioButtons(inputId = "scoring_state", label = "", 
                               choices = c("All", "Set", "Transition"), 
                               selected = "All", status = "primary", shape = "round", outline = T, 
                               fill = T, thick = T, animation = "smooth", icon = icon("check"), plain = T, 
                               bigger = T, inline = T)
          ), 
          
          column(
            width = 6, 
            h3("STRENGTH OF THE OFFENSE"), 
            prettyRadioButtons(inputId = "scoring_strength", label = "", 
                               choices = c("All", "Even-Strength", "Man-Advantage", "Short-Handed"), 
                               selected = "All", status = "primary", shape = "round", outline = T, 
                               fill = T, thick = T, animation = "smooth", icon = icon("check"), plain = T, 
                               bigger = T, inline = T)
          )
        )
      )
    }
  })
  
  ##### SCO: Bars Title - Side1 #####
  
  output$scoring_profile_side1_title <- renderText({
    if(length(input$scoring_strength) > 0) {
      shot_df <- df_master_fltr() %>% 
        filter(Event_Type == "Shot")
      
      if(data_sel$Current_Selection == "Team Summary") {
        shot_df <- shot_df %>% 
          mutate(Team = ifelse(Team != df_team_info$Team[side_vars$Side1_Row], "Opponent ", Team))
      }
      
      # Filter down by strength input
      if(input$scoring_strength == "Even-Strength") {
        shot_df <- shot_df %>% 
          filter(Home_Strength == Away_Strength)    
      } else {
        if(input$scoring_strength == "Man-Advantage") {
          shot_df <- shot_df %>% 
            group_by(Game) %>% 
            mutate(Side1_Strength = ifelse(Home_Team == df_team_info$Team[side_vars$Side1_Row], 
                                           Home_Strength, Away_Strength), 
                   Side2_Strength = ifelse(Home_Team == df_team_info$Team[side_vars$Side1_Row], 
                                           Away_Strength, Home_Strength)) %>% 
            ungroup() %>% 
            mutate(Keep = ifelse((Team == df_team_info$Team[side_vars$Side1_Row] & Side1_Strength > Side2_Strength) | (Team != df_team_info$Team[side_vars$Side1_Row] & Side1_Strength < Side2_Strength), "Yes", "No")) %>% 
            filter(Keep == "Yes")
        } else {
          if(input$scoring_strength == "Short-Handed") {
            shot_df <- shot_df %>% 
              group_by(Game) %>% 
              mutate(Side1_Strength = ifelse(Home_Team == df_team_info$Team[side_vars$Side1_Row], 
                                             Home_Strength, Away_Strength), 
                     Side2_Strength = ifelse(Home_Team == df_team_info$Team[side_vars$Side1_Row], 
                                             Away_Strength, Home_Strength)) %>% 
              ungroup() %>% 
              mutate(Keep = ifelse((Team == df_team_info$Team[side_vars$Side1_Row] & Side1_Strength < Side2_Strength) | (Team != df_team_info$Team[side_vars$Side1_Row] & Side1_Strength > Side2_Strength), "Yes", "No")) %>% 
              filter(Keep == "Yes")
          }
        }
      }
      
      # Filter down by state input
      if(input$scoring_state != "All") {
        shot_df <- shot_df %>% 
          filter(State %in% input$scoring_state)
      }
      
      if(length(input$scoring_total_avg) > 0) {
        if(input$scoring_total_avg == "Totals") {
          shot_df <- shot_df %>% 
            group_by(Team) %>% 
            summarise(Total = length(which(!is.na(Result))))
        } else {
          games <- length(unique(shot_df$Game))
          shot_df <- shot_df %>% 
            group_by(Team) %>% 
            summarise(Total = round(length(which(!is.na(Result))) / games, 1))
        }
      } else {
        shot_df <- shot_df %>% 
          group_by(Team) %>% 
          summarise(Total = length(which(!is.na(Result))))
      }
      
      paste(as.numeric(shot_df$Total[shot_df$Team == df_team_info$Team[side_vars$Side1_Row]])[[1]], 
            toupper(df_team_info$Team[side_vars$Side1_Row]), "SHOTS", sep = " ")
    }
  })
  
  ##### SCO: Profile Bars - Side1 #####
  
  output$scoring_profile_side1 <- renderPlotly({
    if(length(input$scoring_strength) > 0) {
      
      shot_df <- df_master_fltr()
      
      if(data_sel$Current_Selection == "Team Summary") {
        shot_df <- shot_df %>% 
          mutate(Team = ifelse(Team != df_team_info$Team[side_vars$Side1_Row], "Opponent ", Team))
      }
      
      # Filter down by strength input
      if(input$scoring_strength == "Even-Strength") {
        shot_df <- shot_df %>% 
          filter(Home_Strength == Away_Strength)    
      } else {
        if(input$scoring_strength == "Man-Advantage") {
          shot_df <- shot_df %>% 
            group_by(Game) %>% 
            mutate(Side1_Strength = ifelse(Home_Team == df_team_info$Team[side_vars$Side1_Row], 
                                           Home_Strength, Away_Strength), 
                   Side2_Strength = ifelse(Home_Team == df_team_info$Team[side_vars$Side1_Row], 
                                           Away_Strength, Home_Strength)) %>% 
            ungroup() %>% 
            mutate(Keep = ifelse((Team == df_team_info$Team[side_vars$Side1_Row] & Side1_Strength > Side2_Strength) | (Team != df_team_info$Team[side_vars$Side1_Row] & Side1_Strength < Side2_Strength), "Yes", "No")) %>% 
            filter(Keep == "Yes")
        } else {
          if(input$scoring_strength == "Short-Handed") {
            shot_df <- shot_df %>% 
              group_by(Game) %>% 
              mutate(Side1_Strength = ifelse(Home_Team == df_team_info$Team[side_vars$Side1_Row], 
                                             Home_Strength, Away_Strength), 
                     Side2_Strength = ifelse(Home_Team == df_team_info$Team[side_vars$Side1_Row], 
                                             Away_Strength, Home_Strength)) %>% 
              ungroup() %>% 
              mutate(Keep = ifelse((Team == df_team_info$Team[side_vars$Side1_Row] & Side1_Strength < Side2_Strength) | (Team != df_team_info$Team[side_vars$Side1_Row] & Side1_Strength > Side2_Strength), "Yes", "No")) %>% 
              filter(Keep == "Yes")
          }
        }
      }
      
      # Filter down by state input
      if(input$scoring_state != "All") {
        shot_df <- shot_df %>% 
          filter(State %in% input$scoring_state)
      }
      
      if(length(input$scoring_total_avg) > 0) {
        if(input$scoring_total_avg == "Totals") {
          shot_df <- shot_df %>% 
            group_by(Team) %>% 
            summarise(Goal = length(which((Result == "Goal"))),
                      Saved = length(which(Result == "Miss" & Net_Location %in% shot_on)),
                      Off = length(which(Result == "Miss" & Net_Location %in% shot_off)),
                      Post = length(which(Result == "Miss" & Net_Location %in% shot_post)),
                      Blocked = length(which(Result == "Blocked")), 
                      Total = length(which(!is.na(Result))))
        } else {
          games <- length(unique(shot_df$Game))
          shot_df <- shot_df %>% 
            group_by(Team) %>% 
            summarise(Goal = round(length(which((Result == "Goal"))) / games, 1),
                      Saved = round(length(which(Result == "Miss" & Net_Location %in% shot_on)) / games, 1),
                      Off = round(length(which(Result == "Miss" & Net_Location %in% shot_off)) / games, 1),
                      Post = round(length(which(Result == "Miss" & Net_Location %in% shot_post)) / games, 1),
                      Blocked = round(length(which(Result == "Blocked")) / games, 1), 
                      Total = round(length(which(!is.na(Result))) / games, 1))
        }
      } else {
        shot_df <- shot_df %>% 
          group_by(Team) %>% 
          summarise(Goal = length(which((Result == "Goal"))),
                    Saved = length(which(Result == "Miss" & Net_Location %in% shot_on)),
                    Off = length(which(Result == "Miss" & Net_Location %in% shot_off)),
                    Post = length(which(Result == "Miss" & Net_Location %in% shot_post)),
                    Blocked = length(which(Result == "Blocked")), 
                    Total = length(which(!is.na(Result))))
      }
      
      shot_df <- shot_df %>% 
        mutate(Goal_Label = paste("<B>Goal\n", Goal, " - ", percent((Goal / Total), accuracy = 0.1), "</B>",  sep = ""),
               Saved_Label = paste("<B>Saved\n", Saved, " - ", percent((Saved / Total), accuracy = 0.1), "</B>", sep = ""),
               Off_Label = paste("<B>Off\n", Off, " - ", percent((Off / Total), accuracy = 0.1), "</B>", sep = ""),
               Post_Label = paste("<B>Post\n", Post, " - ", percent((Post / Total), accuracy = 0.1), "</B>", sep = ""),
               Blocked_Label = paste("<B>Blocked\n", Blocked, " - ", percent((Blocked / Total), accuracy = 0.1), "</B>", sep = ""), 
               Total_Label = paste("Total\n", Total, sep = ""))
      shot_df <- shot_df %>% 
        filter(Team == df_team_info$Team[side_vars$Side1_Row])
      
      plot_ly(shot_df, type = "bar", x = ~Goal, y = ~Team, name = "Goals", orientation = "h", 
              text = ~Goal_Label, textposition = "inside", insidetextanchor = "middle", textfont = list(size = 15), hoverinfo = "text", 
              marker = list(
                color = df_team_info$Color1[side_vars$Side1_Row], 
                line = list(
                  color = "#000000", 
                  width = 2
                )
              )) %>% 
        add_bars(x = ~Saved, y = ~Team, name = "Saved Shots", text = ~Saved_Label, textposition = 'auto',  
                 marker = list(
                   color = df_team_info$Color2[side_vars$Side1_Row], 
                   line = list(
                     color = "#000000", 
                     width = 2
                   )
                 )) %>% 
        add_bars(x = ~Off, y = ~Team, name = "Off Shots", text = ~Off_Label, textposition = 'auto', 
                 marker = list(
                   color = df_team_info$Color3[side_vars$Side1_Row], 
                   line = list(
                     color = "#000000", 
                     width = 2
                   )
                 )) %>% 
        add_bars(x = ~Post, y = ~Team, name = "Post Shots", text = ~Post_Label, textposition = 'auto', 
                 marker = list(
                   color = df_team_info$Color1[side_vars$Side1_Row], 
                   line = list(
                     color = "#000000", 
                     width = 2
                   )
                 )) %>% 
        add_bars(x = ~Blocked, y = ~Team, name = "Blocked Shots", text = ~Blocked_Label, textposition = 'inside', 
                 marker = list(
                   color = df_team_info$Color2[side_vars$Side1_Row], 
                   line = list(
                     color = "#000000", 
                     width = 2
                   )
                 )) %>% 
        layout(
          barmode = "stack", 
          height = 150, 
          plot_bgcolor = "#FFFFFF", 
          hoverlabel = list(
            font = list(
              size = 18
            )
          ), 
          yaxis = list(
            title = "", 
            side = "right", 
            visible = FALSE, 
            fixedrange = TRUE
          ), 
          xaxis = list(
            autorange = "reversed", 
            visible = FALSE, 
            fixedrange = TRUE
          ), 
          margin = list(
            r = 10, 
            t = 0, 
            b = 0
          )
        ) %>% 
        hide_legend() %>% 
        config(
          modeBarButtonsToRemove = c('toImage', "zoom2d", "toggleSpikelines", "hoverClosestCartesian", 
                                     "hoverCompareCartesian", "drawline", "autoScale2d", "resetScale2d", 
                                     "zoomIn2d", "zoomOut2d", "pan2d", 'select2d', 'lasso2d')
        )  
    }
  })
  
  ##### SCO: Possession Bars - Side1 #####
  
  output$scoring_poss_side1 <- renderPlotly({
    if(length(input$scoring_strength) > 0) {
      
      shot_df <- df_master_fltr()
      
      if(data_sel$Current_Selection == "Team Summary") {
        shot_df <- shot_df %>% 
          mutate(Team = ifelse(Team != df_team_info$Team[side_vars$Side1_Row], "Opponent ", Team))
      }
      
      # Filter down by strength input
      if(input$scoring_strength == "Even-Strength") {
        shot_df <- shot_df %>% 
          filter(Home_Strength == Away_Strength)    
      } else {
        if(input$scoring_strength == "Man-Advantage") {
          shot_df <- shot_df %>% 
            group_by(Game) %>% 
            mutate(Side1_Strength = ifelse(Home_Team == df_team_info$Team[side_vars$Side1_Row], 
                                           Home_Strength, Away_Strength), 
                   Side2_Strength = ifelse(Home_Team == df_team_info$Team[side_vars$Side1_Row], 
                                           Away_Strength, Home_Strength)) %>% 
            ungroup() %>% 
            mutate(Keep = ifelse((Team == df_team_info$Team[side_vars$Side1_Row] & Side1_Strength > Side2_Strength) | (Team != df_team_info$Team[side_vars$Side1_Row] & Side1_Strength < Side2_Strength), "Yes", "No")) %>% 
            filter(Keep == "Yes")
        } else {
          if(input$scoring_strength == "Short-Handed") {
            shot_df <- shot_df %>% 
              group_by(Game) %>% 
              mutate(Side1_Strength = ifelse(Home_Team == df_team_info$Team[side_vars$Side1_Row], 
                                             Home_Strength, Away_Strength), 
                     Side2_Strength = ifelse(Home_Team == df_team_info$Team[side_vars$Side1_Row], 
                                             Away_Strength, Home_Strength)) %>% 
              ungroup() %>% 
              mutate(Keep = ifelse((Team == df_team_info$Team[side_vars$Side1_Row] & Side1_Strength < Side2_Strength) | (Team != df_team_info$Team[side_vars$Side1_Row] & Side1_Strength > Side2_Strength), "Yes", "No")) %>% 
              filter(Keep == "Yes")
          }
        }
      }
      
      # Filter down by state input
      if(input$scoring_state != "All") {
        shot_df <- shot_df %>% 
          filter(State %in% input$scoring_state)
      }
      
      # Filter depending on Totals or Averages
      if(length(input$scoring_total_avg) > 0) {
        if(input$scoring_total_avg == "Totals") {
          shot_df <- shot_df %>% 
            filter(Event_Type == "Shot") %>% 
            group_by(Team) %>% 
            summarise(Set = length(which((State == "Set"))),
                      Transition = length(which(State == "Transition")), 
                      Total = length(which(!is.na(State))))
        } else {
          games <- length(unique(shot_df$Game))
          shot_df <- shot_df %>% 
            filter(Event_Type == "Shot") %>% 
            group_by(Team) %>% 
            summarise(Set = round(length(which((State == "Set"))) / games, 1),
                      Transition = round(length(which((State == "Transition"))) / games, 1), 
                      Total = round(length(which(!is.na(State))) / games, 1))
        }
      } else {
        shot_df <- shot_df %>% 
          filter(Event_Type == "Shot") %>% 
          group_by(Team) %>% 
          summarise(Set = length(which((State == "Set"))),
                    Transition = length(which(State == "Transition")), 
                    Total = length(which(!is.na(State))))
      }
      
      shot_df <- shot_df %>% 
        mutate(Set_Label = paste("<B>Set\n", Set, " - ", percent((Set / Total), accuracy = 0.1), "</B>", sep = ""),
               Transition_Label = paste("<B>Transition\n", Transition, " - ", percent((Transition / Total), accuracy = 0.1), "</B>", sep = ""))
      shot_df <- shot_df %>% 
        filter(Team == df_team_info$Team[side_vars$Side1_Row])
      
      plot_ly(shot_df, type = "bar", x = ~Transition, y = ~Team, name = "Transition", orientation = "h", 
              text = ~Transition_Label, textposition = "inside", insidetextanchor = "middle", textfont = list(size = 15), hoverinfo = "text", 
              marker = list(
                color = df_team_info$Color1[side_vars$Side1_Row], 
                line = list(
                  color = "#000000", 
                  width = 2
                )
              )) %>% 
        add_bars(x = ~Set, y = ~Team, name = "Set", text = ~Set_Label, textposition = 'inside', 
                 marker = list(
                   color = df_team_info$Color2[side_vars$Side1_Row], 
                   line = list(
                     color = "#000000", 
                     width = 2
                   )
                 )) %>% 
        layout(
          barmode = "stack", 
          height = 150, 
          plot_bgcolor = "#FFFFFF", 
          hoverlabel = list(
            font = list(
              size = 18
            )
          ), 
          yaxis = list(
            title = "", 
            side = "right", 
            visible = FALSE, 
            fixedrange = TRUE
          ), 
          xaxis = list(
            autorange = "reversed", 
            visible = FALSE, 
            fixedrange = TRUE
          ), 
          margin = list(
            r = 10, 
            t = 0, 
            b = 0
          )
        ) %>% 
        hide_legend() %>% 
        config(
          modeBarButtonsToRemove = c('toImage', "zoom2d", "toggleSpikelines", "hoverClosestCartesian", 
                                     "hoverCompareCartesian", "drawline", "autoScale2d", "resetScale2d", 
                                     "zoomIn2d", "zoomOut2d", "pan2d", 'select2d', 'lasso2d')
        )  
    }
  })
  
  ##### SCO: Creation Bars - Side1 #####
  
  output$scoring_create_side1 <- renderPlotly({
    if(length(input$scoring_strength) > 0) {
      
      shot_df <- df_master_fltr()
      
      if(data_sel$Current_Selection == "Team Summary") {
        shot_df <- shot_df %>% 
          mutate(Team = ifelse(Team != df_team_info$Team[side_vars$Side1_Row], "Opponent ", Team))
      }
      
      # Filter down by strength input
      if(input$scoring_strength == "Even-Strength") {
        shot_df <- shot_df %>% 
          filter(Home_Strength == Away_Strength)    
      } else {
        if(input$scoring_strength == "Man-Advantage") {
          shot_df <- shot_df %>% 
            group_by(Game) %>% 
            mutate(Side1_Strength = ifelse(Home_Team == df_team_info$Team[side_vars$Side1_Row], 
                                           Home_Strength, Away_Strength), 
                   Side2_Strength = ifelse(Home_Team == df_team_info$Team[side_vars$Side1_Row], 
                                           Away_Strength, Home_Strength)) %>% 
            ungroup() %>% 
            mutate(Keep = ifelse((Team == df_team_info$Team[side_vars$Side1_Row] & Side1_Strength > Side2_Strength) | (Team != df_team_info$Team[side_vars$Side1_Row] & Side1_Strength < Side2_Strength), "Yes", "No")) %>% 
            filter(Keep == "Yes")
        } else {
          if(input$scoring_strength == "Short-Handed") {
            shot_df <- shot_df %>% 
              group_by(Game) %>% 
              mutate(Side1_Strength = ifelse(Home_Team == df_team_info$Team[side_vars$Side1_Row], 
                                             Home_Strength, Away_Strength), 
                     Side2_Strength = ifelse(Home_Team == df_team_info$Team[side_vars$Side1_Row], 
                                             Away_Strength, Home_Strength)) %>% 
              ungroup() %>% 
              mutate(Keep = ifelse((Team == df_team_info$Team[side_vars$Side1_Row] & Side1_Strength < Side2_Strength) | (Team != df_team_info$Team[side_vars$Side1_Row] & Side1_Strength > Side2_Strength), "Yes", "No")) %>% 
              filter(Keep == "Yes")
          }
        }
      }
      
      # Filter down by state input
      if(input$scoring_state != "All") {
        shot_df <- shot_df %>% 
          filter(State %in% input$scoring_state)
      }
      
      # Filter depending on Totals or Averages
      if(length(input$scoring_total_avg) > 0) {
        if(input$scoring_total_avg == "Totals") {
          shot_df <- shot_df %>% 
            group_by(Team) %>% 
            summarise(Movement = length(which((Shot_Type == "Movement"))),
                      Assisted = length(which(Shot_Type == "Assisted")), 
                      Rebound = length(which(Shot_Type == "Rebound")), 
                      Total = length(which(!is.na(Shot_Type))))
        } else {
          games <- length(unique(shot_df$Game))
          shot_df <- shot_df %>% 
            filter(Event_Type == "Shot") %>% 
            group_by(Team) %>% 
            summarise(Movement = round(length(which((Shot_Type == "Movement"))) / games, 1),
                      Assisted = round(length(which((Shot_Type == "Assisted"))) / games, 1), 
                      Rebound = round(length(which((Shot_Type == "Rebound"))) / games, 1), 
                      Total = round(length(which(!is.na(Shot_Type))) / games, 1))
        }
      } else {
        shot_df <- shot_df %>% 
          group_by(Team) %>% 
          summarise(Movement = length(which((Shot_Type == "Movement"))),
                    Assisted = length(which(Shot_Type == "Assisted")), 
                    Rebound = length(which(Shot_Type == "Rebound")), 
                    Total = length(which(!is.na(Shot_Type))))
      }
      
      shot_df <- shot_df %>% 
        mutate(Movement_Label = paste("<B>Movement\n", Movement, " - ", percent((Movement / Total), accuracy = 0.1), "</B>", sep = ""),
               Assisted_Label = paste("<B>Assisted\n", Assisted, " - ", percent((Assisted / Total), accuracy = 0.1), "</B>", sep = ""), 
               Rebound_Label = paste("<B>Offensive Rebound\n", Rebound, " - ", percent((Rebound / Total), accuracy = 0.1), "</B>", sep = ""))
      shot_df <- shot_df %>% 
        filter(Team == df_team_info$Team[side_vars$Side1_Row])
      
      plot_ly(shot_df, type = "bar", x = ~Rebound, y = ~Team, name = "Rebound", orientation = "h", 
              text = ~Rebound_Label, textposition = "inside", insidetextanchor = "middle", textfont = list(size = 15), hoverinfo = "text", 
              marker = list(
                color = df_team_info$Color3[side_vars$Side1_Row], 
                line = list(
                  color = "#000000", 
                  width = 2
                )
              )) %>% 
        add_bars(x = ~Movement, y = ~Team, name = "Movement", text = ~Movement_Label, textposition = 'auto',  
                 marker = list(
                   color = df_team_info$Color2[side_vars$Side1_Row], 
                   line = list(
                     color = "#000000", 
                     width = 2
                   )
                 )) %>% 
        add_bars(x = ~Assisted, y = ~Team, name = "Assisted", text = ~Assisted_Label, textposition = 'inside', 
                 marker = list(
                   color = df_team_info$Color1[side_vars$Side1_Row], 
                   line = list(
                     color = "#000000", 
                     width = 2
                   )
                 )) %>% 
        layout(
          barmode = "stack", 
          height = 150, 
          plot_bgcolor = "#FFFFFF", 
          hoverlabel = list(
            font = list(
              size = 18
            )
          ), 
          yaxis = list(
            title = "", 
            side = "right", 
            visible = FALSE, 
            fixedrange = TRUE
          ), 
          xaxis = list(
            autorange = "reversed", 
            visible = FALSE, 
            fixedrange = TRUE
          ), 
          margin = list(
            r = 10, 
            t = 0, 
            b = 0
          )
        ) %>% 
        hide_legend() %>% 
        config(
          modeBarButtonsToRemove = c('toImage', "zoom2d", "toggleSpikelines", "hoverClosestCartesian", 
                                     "hoverCompareCartesian", "drawline", "autoScale2d", "resetScale2d", 
                                     "zoomIn2d", "zoomOut2d", "pan2d", 'select2d', 'lasso2d')
        )  
    }
  })
  
  ##### SCO: Bars Title - Side2 #####
  
  output$scoring_profile_side2_title <- renderText({
    if(length(input$scoring_strength) > 0) {
      shot_df <- df_master_fltr() %>% 
        filter(Event_Type == "Shot")
      
      if(data_sel$Current_Selection == "Team Summary") {
        shot_df <- shot_df %>% 
          mutate(Team = ifelse(Team != df_team_info$Team[side_vars$Side1_Row], "Opponent ", Team))
      }
      
      if(length(input$scoring_season) > 0) {
        if(input$scoring_season != "Cumulative") {
          shot_df <- shot_df %>%
            filter(Season %in% input$scoring_season)
        }
      }
      
      # Filter down by strength input
      if(input$scoring_strength == "Even-Strength") {
        shot_df <- shot_df %>% 
          filter(Home_Strength == Away_Strength)    
      } else {
        if(input$scoring_strength == "Man-Advantage") {
          shot_df <- shot_df %>% 
            group_by(Game) %>% 
            mutate(Side1_Strength = ifelse(Home_Team == df_team_info$Team[side_vars$Side1_Row], 
                                           Home_Strength, Away_Strength), 
                   Side2_Strength = ifelse(Home_Team == df_team_info$Team[side_vars$Side1_Row], 
                                           Away_Strength, Home_Strength)) %>% 
            ungroup() %>% 
            mutate(Keep = ifelse((Team == df_team_info$Team[side_vars$Side1_Row] & Side1_Strength > Side2_Strength) | (Team != df_team_info$Team[side_vars$Side1_Row] & Side1_Strength < Side2_Strength), "Yes", "No")) %>% 
            filter(Keep == "Yes")
        } else {
          if(input$scoring_strength == "Short-Handed") {
            shot_df <- shot_df %>% 
              group_by(Game) %>% 
              mutate(Side1_Strength = ifelse(Home_Team == df_team_info$Team[side_vars$Side1_Row], 
                                             Home_Strength, Away_Strength), 
                     Side2_Strength = ifelse(Home_Team == df_team_info$Team[side_vars$Side1_Row], 
                                             Away_Strength, Home_Strength)) %>% 
              ungroup() %>% 
              mutate(Keep = ifelse((Team == df_team_info$Team[side_vars$Side1_Row] & Side1_Strength < Side2_Strength) | (Team != df_team_info$Team[side_vars$Side1_Row] & Side1_Strength > Side2_Strength), "Yes", "No")) %>% 
              filter(Keep == "Yes")
          }
        }
      }
      
      # Filter down by state input
      if(input$scoring_state != "All") {
        shot_df <- shot_df %>% 
          filter(State %in% input$scoring_state)
      }
      
      if(length(input$scoring_total_avg) > 0) {
        if(input$scoring_total_avg == "Totals") {
          shot_df <- shot_df %>% 
            group_by(Team) %>% 
            summarise(Total = length(which(!is.na(Result))))
        } else {
          games <- length(unique(shot_df$Game))
          shot_df <- shot_df %>% 
            group_by(Team) %>% 
            summarise(Total = round(length(which(!is.na(Result))) / games, 1))
        }
      } else {
        shot_df <- shot_df %>% 
          group_by(Team) %>% 
          summarise(Total = length(which(!is.na(Result))))
      }
      
      if(data_sel$Current_Selection == "Team Summary") {
        paste(as.numeric(shot_df$Total[shot_df$Team != df_team_info$Team[side_vars$Side1_Row]])[[1]], 
              "OPPONENT", "SHOTS", sep = " ")
      } else {
        paste(as.numeric(shot_df$Total[shot_df$Team != df_team_info$Team[side_vars$Side1_Row]])[[1]], 
              toupper(df_team_info$Team[side_vars$Side2_Row]), "SHOTS", sep = " ")
      }
    }
  })
  
  ##### SCO: Profile Bars - Side2 #####
  
  output$scoring_profile_side2 <- renderPlotly({
    if(length(input$scoring_strength) > 0) {
      
      shot_df <- df_master_fltr()
      
      if(data_sel$Current_Selection == "Team Summary") {
        shot_df <- shot_df %>% 
          mutate(Team = ifelse(Team != df_team_info$Team[side_vars$Side1_Row], "Opponent ", Team))
      }
      
      # Filter down by strength input
      if(input$scoring_strength == "Even-Strength") {
        shot_df <- shot_df %>% 
          filter(Home_Strength == Away_Strength)    
      } else {
        if(input$scoring_strength == "Man-Advantage") {
          shot_df <- shot_df %>% 
            group_by(Game) %>% 
            mutate(Side1_Strength = ifelse(Home_Team == df_team_info$Team[side_vars$Side1_Row], 
                                           Home_Strength, Away_Strength), 
                   Side2_Strength = ifelse(Home_Team == df_team_info$Team[side_vars$Side1_Row], 
                                           Away_Strength, Home_Strength)) %>% 
            ungroup() %>% 
            mutate(Keep = ifelse((Team == df_team_info$Team[side_vars$Side1_Row] & Side1_Strength > Side2_Strength) | (Team != df_team_info$Team[side_vars$Side1_Row] & Side1_Strength < Side2_Strength), "Yes", "No")) %>% 
            filter(Keep == "Yes")
        } else {
          if(input$scoring_strength == "Short-Handed") {
            shot_df <- shot_df %>% 
              group_by(Game) %>% 
              mutate(Side1_Strength = ifelse(Home_Team == df_team_info$Team[side_vars$Side1_Row], 
                                             Home_Strength, Away_Strength), 
                     Side2_Strength = ifelse(Home_Team == df_team_info$Team[side_vars$Side1_Row], 
                                             Away_Strength, Home_Strength)) %>% 
              ungroup() %>% 
              mutate(Keep = ifelse((Team == df_team_info$Team[side_vars$Side1_Row] & Side1_Strength < Side2_Strength) | (Team != df_team_info$Team[side_vars$Side1_Row] & Side1_Strength > Side2_Strength), "Yes", "No")) %>% 
              filter(Keep == "Yes")
          }
        }
      }
      
      # Filter down by state input
      if(input$scoring_state != "All") {
        shot_df <- shot_df %>% 
          filter(State %in% input$scoring_state)
      }
      
      if(length(input$scoring_total_avg) > 0) {
        if(input$scoring_total_avg == "Totals") {
          shot_df <- shot_df %>% 
            group_by(Team) %>% 
            summarise(Goal = length(which((Result == "Goal"))),
                      Saved = length(which(Result == "Miss" & Net_Location %in% shot_on)),
                      Off = length(which(Result == "Miss" & Net_Location %in% shot_off)),
                      Post = length(which(Result == "Miss" & Net_Location %in% shot_post)),
                      Blocked = length(which(Result == "Blocked")), 
                      Total = length(which(!is.na(Result))))
        } else {
          games <- length(unique(shot_df$Game))
          shot_df <- shot_df %>% 
            group_by(Team) %>% 
            summarise(Goal = round(length(which((Result == "Goal"))) / games, 1),
                      Saved = round(length(which(Result == "Miss" & Net_Location %in% shot_on)) / games, 1),
                      Off = round(length(which(Result == "Miss" & Net_Location %in% shot_off)) / games, 1),
                      Post = round(length(which(Result == "Miss" & Net_Location %in% shot_post)) / games, 1),
                      Blocked = round(length(which(Result == "Blocked")) / games, 1), 
                      Total = round(length(which(!is.na(Result))) / games, 1))
        }
      } else {
        shot_df <- shot_df %>% 
          group_by(Team) %>% 
          summarise(Goal = length(which((Result == "Goal"))),
                    Saved = length(which(Result == "Miss" & Net_Location %in% shot_on)),
                    Off = length(which(Result == "Miss" & Net_Location %in% shot_off)),
                    Post = length(which(Result == "Miss" & Net_Location %in% shot_post)),
                    Blocked = length(which(Result == "Blocked")), 
                    Total = length(which(!is.na(Result))))
      }
      
      shot_df <- shot_df %>% 
        mutate(Goal_Label = paste("<B>Goal\n", Goal, " - ", percent((Goal / Total), accuracy = 0.1), "</B>",  sep = ""),
               Saved_Label = paste("<B>Saved\n", Saved, " - ", percent((Saved / Total), accuracy = 0.1), "</B>", sep = ""),
               Off_Label = paste("<B>Off\n", Off, " - ", percent((Off / Total), accuracy = 0.1), "</B>", sep = ""),
               Post_Label = paste("<B>Post\n", Post, " - ", percent((Post / Total), accuracy = 0.1), "</B>", sep = ""),
               Blocked_Label = paste("<B>Blocked\n", Blocked, " - ", percent((Blocked / Total), accuracy = 0.1), "</B>", sep = ""), 
               Total_Label = paste("Total\n", Total, sep = ""))
      shot_df <- shot_df %>% 
        filter(Team != df_team_info$Team[side_vars$Side1_Row])
      
      plot_ly(shot_df, type = "bar", x = ~Goal, y = ~Team, name = "Goals", orientation = "h",  
              text = ~Goal_Label, textposition = "inside", insidetextanchor = "middle", textfont = list(size = 15), hoverinfo = "text", 
              marker = list(
                color = df_team_info$Color1[side_vars$Side2_Row], 
                line = list(
                  color = "#000000", 
                  width = 2
                )
              )) %>% 
        add_bars(x = ~Saved, y = ~Team, name = "Saved Shots", text = ~Saved_Label, textposition = 'auto',  
                 marker = list(
                   color = df_team_info$Color2[side_vars$Side2_Row], 
                   line = list(
                     color = "#000000", 
                     width = 2
                   )
                 )) %>% 
        add_bars(x = ~Off, y = ~Team, name = "Off Shots", text = ~Off_Label, textposition = 'auto', 
                 marker = list(
                   color = df_team_info$Color3[side_vars$Side2_Row], 
                   line = list(
                     color = "#000000", 
                     width = 2
                   )
                 )) %>% 
        add_bars(x = ~Post, y = ~Team, name = "Post Shots", text = ~Post_Label, textposition = 'auto', 
                 marker = list(
                   color = df_team_info$Color1[side_vars$Side2_Row], 
                   line = list(
                     color = "#000000", 
                     width = 2
                   )
                 )) %>% 
        add_bars(x = ~Blocked, y = ~Team, name = "Blocked Shots", text = ~Blocked_Label, textposition = 'inside', 
                 marker = list(
                   color = df_team_info$Color2[side_vars$Side2_Row], 
                   line = list(
                     color = "#000000", 
                     width = 2
                   )
                 )) %>% 
        layout(
          barmode = "stack", 
          height = 150, 
          plot_bgcolor = "#FFFFFF", 
          hoverlabel = list(
            font = list(
              size = 18
            )
          ), 
          yaxis = list(
            title = "", 
            visible = FALSE, 
            fixedrange = TRUE
          ), 
          xaxis = list(
            visible = FALSE, 
            fixedrange = TRUE
          ), 
          margin = list(
            l = 10, 
            t = 0, 
            b = 0
          )
        ) %>% 
        hide_legend() %>% 
        config(
          modeBarButtonsToRemove = c('toImage', "zoom2d", "toggleSpikelines", "hoverClosestCartesian", 
                                     "hoverCompareCartesian", "drawline", "autoScale2d", "resetScale2d", 
                                     "zoomIn2d", "zoomOut2d", "pan2d", 'select2d', 'lasso2d')
        )  
    }
  })
  
  ##### SCO: Possession Bars - Side2 #####
  
  output$scoring_poss_side2 <- renderPlotly({
    if(length(input$scoring_strength) > 0) {
      
      shot_df <- df_master_fltr()
      
      if(data_sel$Current_Selection == "Team Summary") {
        shot_df <- shot_df %>% 
          mutate(Team = ifelse(Team != df_team_info$Team[side_vars$Side1_Row], "Opponent ", Team))
      }
      
      if(length(input$scoring_season) > 0) {
        if(input$scoring_season != "Cumulative") {
          shot_df <- shot_df %>%
            filter(Season %in% input$scoring_season)
        }
      }
      
      # Filter down by strength input
      if(input$scoring_strength == "Even-Strength") {
        shot_df <- shot_df %>% 
          filter(Home_Strength == Away_Strength)    
      } else {
        if(input$scoring_strength == "Man-Advantage") {
          shot_df <- shot_df %>% 
            group_by(Game) %>% 
            mutate(Side1_Strength = ifelse(Home_Team == df_team_info$Team[side_vars$Side1_Row], 
                                           Home_Strength, Away_Strength), 
                   Side2_Strength = ifelse(Home_Team == df_team_info$Team[side_vars$Side1_Row], 
                                           Away_Strength, Home_Strength)) %>% 
            ungroup() %>% 
            mutate(Keep = ifelse((Team == df_team_info$Team[side_vars$Side1_Row] & Side1_Strength > Side2_Strength) | (Team != df_team_info$Team[side_vars$Side1_Row] & Side1_Strength < Side2_Strength), "Yes", "No")) %>% 
            filter(Keep == "Yes")
        } else {
          if(input$scoring_strength == "Short-Handed") {
            shot_df <- shot_df %>% 
              group_by(Game) %>% 
              mutate(Side1_Strength = ifelse(Home_Team == df_team_info$Team[side_vars$Side1_Row], 
                                             Home_Strength, Away_Strength), 
                     Side2_Strength = ifelse(Home_Team == df_team_info$Team[side_vars$Side1_Row], 
                                             Away_Strength, Home_Strength)) %>% 
              ungroup() %>% 
              mutate(Keep = ifelse((Team == df_team_info$Team[side_vars$Side1_Row] & Side1_Strength < Side2_Strength) | (Team != df_team_info$Team[side_vars$Side1_Row] & Side1_Strength > Side2_Strength), "Yes", "No")) %>% 
              filter(Keep == "Yes")
          }
        }
      }
      
      # Filter down by state input
      if(input$scoring_state != "All") {
        shot_df <- shot_df %>% 
          filter(State %in% input$scoring_state)
      }
      
      # Filter depending on Totals or Averages
      if(length(input$scoring_total_avg) > 0) {
        if(input$scoring_total_avg == "Totals") {
          shot_df <- shot_df %>% 
            filter(Event_Type == "Shot") %>% 
            group_by(Team) %>% 
            summarise(Set = length(which((State == "Set"))),
                      Transition = length(which(State == "Transition")), 
                      Total = length(which(!is.na(State))))
        } else {
          games <- length(unique(shot_df$Game))
          shot_df <- shot_df %>% 
            filter(Event_Type == "Shot") %>% 
            group_by(Team) %>% 
            summarise(Set = round(length(which((State == "Set"))) / games, 1),
                      Transition = round(length(which((State == "Transition"))) / games, 1), 
                      Total = round(length(which(!is.na(State))) / games, 1))
        }
      } else {
        shot_df <- shot_df %>% 
          filter(Event_Type == "Shot") %>% 
          group_by(Team) %>% 
          summarise(Set = length(which((State == "Set"))),
                    Transition = length(which(State == "Transition")), 
                    Total = length(which(!is.na(State))))
      }
      
      shot_df <- shot_df %>% 
        mutate(Set_Label = paste("<B>Set\n", Set, " - ", percent((Set / Total), accuracy = 0.1), "</B>", sep = ""),
               Transition_Label = paste("<B>Transition\n", Transition, " - ", percent((Transition / Total), accuracy = 0.1), "</B>", sep = ""))
      shot_df <- shot_df %>% 
        filter(Team != df_team_info$Team[side_vars$Side1_Row])
      
      plot_ly(shot_df, type = "bar", x = ~Transition, y = ~Team, name = "Transition", orientation = "h", 
              text = ~Transition_Label, textposition = "inside", insidetextanchor = "middle", textfont = list(size = 15), hoverinfo = "text", 
              marker = list(
                color = df_team_info$Color1[side_vars$Side2_Row], 
                line = list(
                  color = "#000000", 
                  width = 2
                )
              )) %>% 
        add_bars(x = ~Set, y = ~Team, name = "Set", text = ~Set_Label, textposition = 'inside', 
                 marker = list(
                   color = df_team_info$Color2[side_vars$Side2_Row], 
                   line = list(
                     color = "#000000", 
                     width = 2
                   )
                 )) %>% 
        layout(
          barmode = "stack", 
          height = 150, 
          plot_bgcolor = "#FFFFFF", 
          hoverlabel = list(
            font = list(
              size = 18
            )
          ), 
          yaxis = list(
            title = "", 
            visible = FALSE, 
            fixedrange = TRUE
          ), 
          xaxis = list(
            visible = FALSE, 
            fixedrange = TRUE
          ), 
          margin = list(
            l = 10, 
            t = 0, 
            b = 0
          )
        ) %>% 
        hide_legend() %>% 
        config(
          modeBarButtonsToRemove = c('toImage', "zoom2d", "toggleSpikelines", "hoverClosestCartesian", 
                                     "hoverCompareCartesian", "drawline", "autoScale2d", "resetScale2d", 
                                     "zoomIn2d", "zoomOut2d", "pan2d", 'select2d', 'lasso2d')
        )  
    }
  })
  
  ##### SCO: Creation Bars - Side2 #####
  
  output$scoring_create_side2 <- renderPlotly({
    if(length(input$scoring_strength) > 0) {
      
      shot_df <- df_master_fltr()
      
      if(data_sel$Current_Selection == "Team Summary") {
        shot_df <- shot_df %>% 
          mutate(Team = ifelse(Team != df_team_info$Team[side_vars$Side1_Row], "Opponent ", Team))
      }
      
      # Filter down by strength input
      if(input$scoring_strength == "Even-Strength") {
        shot_df <- shot_df %>% 
          filter(Home_Strength == Away_Strength)    
      } else {
        if(input$scoring_strength == "Man-Advantage") {
          shot_df <- shot_df %>% 
            group_by(Game) %>% 
            mutate(Side1_Strength = ifelse(Home_Team == df_team_info$Team[side_vars$Side1_Row], 
                                           Home_Strength, Away_Strength), 
                   Side2_Strength = ifelse(Home_Team == df_team_info$Team[side_vars$Side1_Row], 
                                           Away_Strength, Home_Strength)) %>% 
            ungroup() %>% 
            mutate(Keep = ifelse((Team == df_team_info$Team[side_vars$Side1_Row] & Side1_Strength > Side2_Strength) | (Team != df_team_info$Team[side_vars$Side1_Row] & Side1_Strength < Side2_Strength), "Yes", "No")) %>% 
            filter(Keep == "Yes")
        } else {
          if(input$scoring_strength == "Short-Handed") {
            shot_df <- shot_df %>% 
              group_by(Game) %>% 
              mutate(Side1_Strength = ifelse(Home_Team == df_team_info$Team[side_vars$Side1_Row], 
                                             Home_Strength, Away_Strength), 
                     Side2_Strength = ifelse(Home_Team == df_team_info$Team[side_vars$Side1_Row], 
                                             Away_Strength, Home_Strength)) %>% 
              ungroup() %>% 
              mutate(Keep = ifelse((Team == df_team_info$Team[side_vars$Side1_Row] & Side1_Strength < Side2_Strength) | (Team != df_team_info$Team[side_vars$Side1_Row] & Side1_Strength > Side2_Strength), "Yes", "No")) %>% 
              filter(Keep == "Yes")
          }
        }
      }
      
      # Filter down by state input
      if(input$scoring_state != "All") {
        shot_df <- shot_df %>% 
          filter(State %in% input$scoring_state)
      }
      
      # Filter depending on Totals or Averages
      if(length(input$scoring_total_avg) > 0) {
        if(input$scoring_total_avg == "Totals") {
          shot_df <- shot_df %>% 
            group_by(Team) %>% 
            summarise(Movement = length(which((Shot_Type == "Movement"))),
                      Assisted = length(which(Shot_Type == "Assisted")), 
                      Rebound = length(which(Shot_Type == "Rebound")), 
                      Total = length(which(!is.na(Shot_Type))))
        } else {
          games <- length(unique(shot_df$Game))
          shot_df <- shot_df %>% 
            filter(Event_Type == "Shot") %>% 
            group_by(Team) %>% 
            summarise(Movement = round(length(which((Shot_Type == "Movement"))) / games, 1),
                      Assisted = round(length(which((Shot_Type == "Assisted"))) / games, 1), 
                      Rebound = round(length(which((Shot_Type == "Rebound"))) / games, 1), 
                      Total = round(length(which(!is.na(Shot_Type))) / games, 1))
        }
      } else {
        shot_df <- shot_df %>% 
          group_by(Team) %>% 
          summarise(Movement = length(which((Shot_Type == "Movement"))),
                    Assisted = length(which(Shot_Type == "Assisted")), 
                    Rebound = length(which(Shot_Type == "Rebound")), 
                    Total = length(which(!is.na(Shot_Type))))
      }
      
      shot_df <- shot_df %>% 
        mutate(Movement_Label = paste("<B>Movement\n", Movement, " - ", percent((Movement / Total), accuracy = 0.1), "</B>", sep = ""),
               Assisted_Label = paste("<B>Assisted\n", Assisted, " - ", percent((Assisted / Total), accuracy = 0.1), "</B>", sep = ""), 
               Rebound_Label = paste("<B>Offensive Rebound\n", Rebound, " - ", percent((Rebound / Total), accuracy = 0.1), "</B>", sep = ""))
      shot_df <- shot_df %>% 
        filter(Team != df_team_info$Team[side_vars$Side1_Row])
      
      plot_ly(shot_df, type = "bar", x = ~Rebound, y = ~Team, name = "Rebound", orientation = "h", 
              text = ~Rebound_Label, textposition = "inside", insidetextanchor = "middle", textfont = list(size = 15), hoverinfo = "text", 
              marker = list(
                color = df_team_info$Color3[side_vars$Side2_Row], 
                line = list(
                  color = "#000000", 
                  width = 2
                )
              )) %>% 
        add_bars(x = ~Movement, y = ~Team, name = "Movement", text = ~Movement_Label, textposition = 'auto',  
                 marker = list(
                   color = df_team_info$Color2[side_vars$Side2_Row], 
                   line = list(
                     color = "#000000", 
                     width = 2
                   )
                 )) %>% 
        add_bars(x = ~Assisted, y = ~Team, name = "Assisted", text = ~Assisted_Label, textposition = 'inside', 
                 marker = list(
                   color = df_team_info$Color1[side_vars$Side2_Row], 
                   line = list(
                     color = "#000000", 
                     width = 2
                   )
                 )) %>% 
        layout(
          barmode = "stack", 
          height = 150, 
          plot_bgcolor = "#FFFFFF",
          hoverlabel = list(
            font = list(
              size = 18
            )
          ),  
          yaxis = list(
            title = "", 
            visible = FALSE, 
            fixedrange = TRUE
          ), 
          xaxis = list(
            visible = FALSE, 
            fixedrange = TRUE
          ), 
          margin = list(
            l = 10, 
            t = 0, 
            b = 0
          )
        ) %>% 
        hide_legend() %>% 
        config(
          modeBarButtonsToRemove = c('toImage', "zoom2d", "toggleSpikelines", "hoverClosestCartesian", 
                                     "hoverCompareCartesian", "drawline", "autoScale2d", "resetScale2d", 
                                     "zoomIn2d", "zoomOut2d", "pan2d", 'select2d', 'lasso2d')
        )  
    }
  })
  
  ##### SCO: Side1 Goal #####
  
  output$scoring_side1_goal <- renderPlotly({
    if(length(input$scoring_strength) > 0) {
      
      shot_df <- df_master_fltr() %>% 
        filter(Team == df_team_info$Team[side_vars$Side1_Row]) %>% 
        filter(Empty_Net == "No" & Shot_Type != "Penalty Shot")
      
      # Filter down by strength input
      if(input$scoring_strength == "Even-Strength") {
        shot_df <- shot_df %>% 
          filter(Home_Strength == Away_Strength)    
      } else {
        if(input$scoring_strength == "Man-Advantage") {
          shot_df <- shot_df %>% 
            group_by(Game) %>% 
            mutate(Side1_Strength = ifelse(Home_Team == df_team_info$Team[side_vars$Side1_Row], 
                                           Home_Strength, Away_Strength), 
                   Side2_Strength = ifelse(Home_Team == df_team_info$Team[side_vars$Side1_Row], 
                                           Away_Strength, Home_Strength)) %>% 
            ungroup() %>% 
            mutate(Keep = ifelse((Team == df_team_info$Team[side_vars$Side1_Row] & Side1_Strength > Side2_Strength) | (Team != df_team_info$Team[side_vars$Side1_Row] & Side1_Strength < Side2_Strength), "Yes", "No")) %>% 
            filter(Keep == "Yes")
        } else {
          if(input$scoring_strength == "Short-Handed") {
            shot_df <- shot_df %>% 
              group_by(Game) %>% 
              mutate(Side1_Strength = ifelse(Home_Team == df_team_info$Team[side_vars$Side1_Row], 
                                             Home_Strength, Away_Strength), 
                     Side2_Strength = ifelse(Home_Team == df_team_info$Team[side_vars$Side1_Row], 
                                             Away_Strength, Home_Strength)) %>% 
              ungroup() %>% 
              mutate(Keep = ifelse((Team == df_team_info$Team[side_vars$Side1_Row] & Side1_Strength < Side2_Strength) | (Team != df_team_info$Team[side_vars$Side1_Row] & Side1_Strength > Side2_Strength), "Yes", "No")) %>% 
              filter(Keep == "Yes")
          }
        }
      }
      
      # Filter down by state input
      if(input$scoring_state != "All") {
        shot_df <- shot_df %>% 
          filter(State %in% input$scoring_state)
      }
      
      # Filter by click selections
      if(sco_side1_goal_sel$Net_Location[1] != "none"){
        shot_df <- shot_df %>% 
          filter(Net_Location %in% sco_side1_goal_sel$Net_Location)
      }
      
      if(sco_side1_shotangle_sel$Shot_Angle_Bin[1] != "none") {
        shot_df <- shot_df %>% 
          filter(Shot_Angle != "NA") %>% 
          mutate(Shot_Angle_Bin = ifelse(Shot_Angle == "Dive/Other", "Dive/Other", 
                                         ifelse(as.numeric(Shot_Angle) < 60, "LowRight", 
                                                ifelse(as.numeric(Shot_Angle) < 120, "MidRight", 
                                                       ifelse(as.numeric(Shot_Angle) < 180, "HighRight", 
                                                              ifelse(180 & as.numeric(Shot_Angle) < 240, "HighLeft", 
                                                                     ifelse(as.numeric(Shot_Angle) < 300, "MidLeft", "LowLeft"))))))) %>%
          filter(Shot_Angle_Bin %in% sco_side1_shotangle_sel$Shot_Angle_Bin)
      }
      
      if(sco_side1_field_sel$Field_Location_Bin[1] != "none"){
        shot_df <- shot_df %>% 
          filter(Field_Location_Bin %in% sco_side1_field_sel$Field_Location_Bin)
      }
      
      goal_info.a <- shot_df %>%
        group_by(Net_Location) %>%
        summarise(Shots = length(Result), 
                  Goals = length(Result[Event_Type == "Shot" & Result == "Goal"])) %>%
        mutate(per = Goals / Shots, 
               Percentage = percent(Goals / Shots, accuracy = 0.1))
      goal_info <- left_join(goal_info.b, goal_info.a, by = c("Net_Location"))
      goal_info[is.na(goal_info)] <- 0
      
      if(length(input$scoring_total_avg) > 0) {
        if(input$scoring_total_avg == "Totals") {
          goal_info <- goal_info %>% 
            mutate(Ratio = ifelse(Net_Location %in% c("Chest", "Crossbar", "Empty Net", "Left Miss", "Left Post", 
                                                      "Right Miss", "Right Post", "Top Miss"), 
                                  paste(Shots), paste(Goals, "/", Shots, sep = "")))
        } else {
          games <- length(unique(shot_df$Game))
          goal_info <- goal_info %>% 
            mutate(Ratio = ifelse(Net_Location %in% c("Chest", "Crossbar", "Empty Net", "Left Miss", "Left Post", 
                                                      "Right Miss", "Right Post", "Top Miss"), 
                                  paste(round((Shots/games), 2)), paste(round((Goals/games), 2), "/", round((Shots/games), 2), sep = "")))
        }
      } else {
        goal_info <- goal_info %>% 
          mutate(Ratio = ifelse(Net_Location %in% c("Chest", "Crossbar", "Empty Net", "Left Miss", "Left Post", 
                                                    "Right Miss", "Right Post", "Top Miss"), 
                                paste(Shots), paste(Goals, "/", Shots, sep = "")))
      }
      
      goal_info <- left_join(goal_info, goal_marker_coords, by = "Net_Location")
      
      goal_info <- goal_info %>% filter(!is.na(Ratio_Loc_x))
      
      goal_info <- rbind(goal_info, 
                         data.frame("Net_Location" = "Filler", "Shots" = 0, "Goals" = 0, "per" = 0, 
                                    "Percentage" = 0, "Ratio" = 0, "Dot_Loc_x" = 2, "Dot_Loc_y" = 2, 
                                    "Ratio_Loc_x" = 2, "Ratio_Loc_y" = 2, "Percentage_Loc_x" = NA, 
                                    "Percentage_Loc_y" = NA, "Goal_Possible" = "No", stringsAsFactors = F))
      
      goal_info <- goal_info %>% 
        mutate(per = ifelse(Goal_Possible == "No", 0, per), 
               per_col = "#000000")
      
      for(i in c(1:nrow(goal_info))) {
        if(goal_info$Shots[i] == 0) {
          goal_info$per_col[i] <- "#000000"
        } else {
          if(goal_info$per[i] >= (shoton_shoot_per*2)) {
            goal_info$per_col[i] <- "#008B00"
          } else {
            shotper_palette <- colorQuantile(palette = c("#CD0000", "#CDCD00", "#008B00"), domain = seq(0, (shoton_shoot_per*2), 0.0001), n = 50)
            shotper_data <- c(0, shoton_shoot_per, (shoton_shoot_per*2), as.numeric(goal_info$per[i]))
            goal_info$per_col[i] <- as.character(shotper_palette(shotper_data)[4])
          }
        }
      }
      
      plot_ly(type = "scatter", x = goal_info$Dot_Loc_x[goal_info$Goal_Possible == "Yes"], 
              y = goal_info$Dot_Loc_y[goal_info$Goal_Possible == "Yes"], hoverinfo = "none", 
              source = "sco_side1_goal_click", key = goal_info$Net_Location[goal_info$Goal_Possible == "Yes"],  
              mode = "markers", 
              marker = list(
                symbol = "circle", 
                size = 80, 
                color = goal_info$per_col[goal_info$Goal_Possible == "Yes"], 
                line = list(
                  color = "white", 
                  width = 3
                )
              )) %>% 
        add_trace(type = "scatter", x = goal_info$Dot_Loc_x[goal_info$Goal_Possible == "No"], 
                  y = goal_info$Dot_Loc_y[goal_info$Goal_Possible == "No"], hoverinfo = "none", 
                  source = "sco_side1_goal_click", key = goal_info$Net_Location[goal_info$Goal_Possible == "No"],  
                  mode = "markers", 
                  marker = list(
                    symbol = "square",
                    size = 50, 
                    color = "#000000", 
                    line = list(
                      color = "white", 
                      width = 3
                    )
                  )) %>%
        hide_colorbar() %>% 
        hide_legend() %>% 
        layout(
          images = list(
            list(
              source = base64enc::dataURI(file = goal_base), 
              xref = "x",
              yref = "y",
              x = -0.04,
              y = 1.01,
              sizex = 1.08,
              sizey = 1.02,
              sizing = "stretch",
              layer = "below"
            ), 
            list(
              source = base64enc::dataURI(file = paste("www/", df_team_info$Logo_Path[df_team_info$Team == df_team_info$Team[side_vars$Side1_Row]], sep = "")), 
              xref = "x",
              yref = "y",
              x = -0.01,
              y = 0.97,
              sizex = (0.22*df_team_info$Logo_Path_Width[df_team_info$Team == df_team_info$Team[side_vars$Side1_Row]]) / df_team_info$Logo_Path_Width[df_team_info$Team == df_team_info$Team[side_vars$Side1_Row]],
              sizey = (0.22*df_team_info$Logo_Path_Height[df_team_info$Team == df_team_info$Team[side_vars$Side1_Row]]) / df_team_info$Logo_Path_Height[df_team_info$Team == df_team_info$Team[side_vars$Side1_Row]],
              layer = "above"
            )
          ), 
          margin = list(
            t = 0, 
            b = 0, 
            l = 0, 
            r = 0, 
            pad = 0
          ), 
          xaxis = list(
            range = c(-0.1, 1.1), 
            visible = F, 
            fixedrange = TRUE
          ),
          yaxis = list(
            range = c(-0.1, 1.1),
            visible = F, 
            scaleanchor = "x", 
            scaleratio = 0.7013, 
            fixedrange = TRUE
          ), 
          dragmode = F
        ) %>% 
        config(
          modeBarButtonsToRemove = c("zoom2d", "toggleSpikelines", "hoverClosestCartesian", 
                                     "hoverCompareCartesian", "drawline", "autoScale2d", "resetScale2d", 
                                     "zoomIn2d", "zoomOut2d", "pan2d", 'lasso2d')
        ) %>%
        add_annotations(x = goal_info$Ratio_Loc_x,
                        y = goal_info$Ratio_Loc_y,
                        text = goal_info$Ratio,
                        xref = "x",
                        yref = "y",
                        showarrow = F,
                        # Styling annotations' text:
                        font = list(color = "white",
                                    size = 13)
        ) %>%
        add_annotations(x = goal_info$Percentage_Loc_x[!is.na(goal_info$Percentage_Loc_x)],
                        y = goal_info$Percentage_Loc_y[!is.na(goal_info$Percentage_Loc_x)],
                        text = goal_info$Percentage[!is.na(goal_info$Percentage_Loc_x)],
                        xref = "x",
                        yref = "y",
                        showarrow = F,
                        # Styling annotations' text:
                        font = list(color = "white",
                                    size = 13)
        ) %>% 
        event_register("plotly_click") %>% 
        event_register("plotly_selecting")
      
    }
  })
  
  ##### SCO: Side1 Shot Angle #####
  
  output$scoring_side1_shotangle <- renderPlotly({
    if(length(input$scoring_strength) > 0) {
      shot_df <- df_master_fltr() %>% 
        filter(Team == df_team_info$Team[side_vars$Side1_Row]) %>% 
        filter(Empty_Net == "No" & Shot_Type != "Penalty Shot")
      
      # Filter down by strength input
      if(input$scoring_strength == "Even-Strength") {
        shot_df <- shot_df %>% 
          filter(Home_Strength == Away_Strength)    
      } else {
        if(input$scoring_strength == "Man-Advantage") {
          shot_df <- shot_df %>% 
            group_by(Game) %>% 
            mutate(Side1_Strength = ifelse(Home_Team == df_team_info$Team[side_vars$Side1_Row], 
                                           Home_Strength, Away_Strength), 
                   Side2_Strength = ifelse(Home_Team == df_team_info$Team[side_vars$Side1_Row], 
                                           Away_Strength, Home_Strength)) %>% 
            ungroup() %>% 
            mutate(Keep = ifelse((Team == df_team_info$Team[side_vars$Side1_Row] & Side1_Strength > Side2_Strength) | (Team != df_team_info$Team[side_vars$Side1_Row] & Side1_Strength < Side2_Strength), "Yes", "No")) %>% 
            filter(Keep == "Yes")
        } else {
          if(input$scoring_strength == "Short-Handed") {
            shot_df <- shot_df %>% 
              group_by(Game) %>% 
              mutate(Side1_Strength = ifelse(Home_Team == df_team_info$Team[side_vars$Side1_Row], 
                                             Home_Strength, Away_Strength), 
                     Side2_Strength = ifelse(Home_Team == df_team_info$Team[side_vars$Side1_Row], 
                                             Away_Strength, Home_Strength)) %>% 
              ungroup() %>% 
              mutate(Keep = ifelse((Team == df_team_info$Team[side_vars$Side1_Row] & Side1_Strength < Side2_Strength) | (Team != df_team_info$Team[side_vars$Side1_Row] & Side1_Strength > Side2_Strength), "Yes", "No")) %>% 
              filter(Keep == "Yes")
          }
        }
      }
      
      # Filter down by state input
      if(input$scoring_state != "All") {
        shot_df <- shot_df %>% 
          filter(State %in% input$scoring_state)
      }
      
      # Filter by click selections
      if(sco_side1_goal_sel$Net_Location[1] != "none"){
        shot_df <- shot_df %>% 
          filter(Net_Location %in% sco_side1_goal_sel$Net_Location)
      }
      
      if(sco_side1_shotangle_sel$Shot_Angle_Bin[1] != "none") {
        shot_df <- shot_df %>% 
          filter(Shot_Angle != "NA") %>% 
          mutate(Shot_Angle_Bin = ifelse(Shot_Angle == "Dive/Other", "Dive/Other", 
                                         ifelse(as.numeric(Shot_Angle) < 60, "LowRight", 
                                                ifelse(as.numeric(Shot_Angle) < 120, "MidRight", 
                                                       ifelse(as.numeric(Shot_Angle) < 180, "HighRight", 
                                                              ifelse(180 & as.numeric(Shot_Angle) < 240, "HighLeft", 
                                                                     ifelse(as.numeric(Shot_Angle) < 300, "MidLeft", "LowLeft"))))))) %>%
          filter(Shot_Angle_Bin %in% sco_side1_shotangle_sel$Shot_Angle_Bin)
      }
      
      if(sco_side1_field_sel$Field_Location_Bin[1] != "none"){
        shot_df <- shot_df %>% 
          filter(Field_Location_Bin %in% sco_side1_field_sel$Field_Location_Bin)
      }
      
      shotangle_info.a <- shot_df %>% 
        filter(Shot_Angle != "NA") %>% 
        mutate(Shot_Angle_Bin = ifelse(Shot_Angle == "Dive/Other", "Dive/Other", 
                                       ifelse(as.numeric(Shot_Angle) < 60, "LowRight", 
                                              ifelse(as.numeric(Shot_Angle) < 120, "MidRight", 
                                                     ifelse(as.numeric(Shot_Angle) < 180, "HighRight", 
                                                            ifelse(180 & as.numeric(Shot_Angle) < 240, "HighLeft", 
                                                                   ifelse(as.numeric(Shot_Angle) < 300, "MidLeft", "LowLeft")))))))
      
      shotangle_info.a <- shotangle_info.a %>%
        group_by(Shot_Angle_Bin) %>%
        summarise(Shots = length(Result), 
                  Goals = length(Result[Event_Type == "Shot" & Result == "Goal"])) %>%
        mutate(per = Goals / Shots, 
               Percentage = percent(Goals / Shots, accuracy = 0.1))
      shotangle_info <- left_join(shotangle_info.b, shotangle_info.a, by = c("Shot_Angle_Bin"))
      shotangle_info[is.na(shotangle_info)] <- 0
      
      if(length(input$scoring_total_avg) > 0) {
        if(input$scoring_total_avg == "Totals") {
          shotangle_info <- shotangle_info %>% 
            mutate(Ratio = paste(Goals, "/", Shots, sep = ""))
        } else {
          games <- length(unique(shot_df$Game))
          shotangle_info <- shotangle_info %>% 
            mutate(Ratio = paste(round((Goals/games), 2), "/", round((Shots/games), 2), sep = ""))
        }
      } else {
        shotangle_info <- shotangle_info %>% 
          mutate(Ratio = paste(Goals, "/", Shots, sep = ""))
      }
      
      shotangle_info <- left_join(shotangle_info, shot_angle_coords, by = "Shot_Angle_Bin")
      
      shotangle_info <- shotangle_info %>% 
        mutate(per_col = "#000000")
      
      for(i in c(1:nrow(shotangle_info))) {
        if(shotangle_info$Shots[i] == 0) {
          shotangle_info$per_col[i] <- "#000000"
        } else {
          if(shotangle_info$per[i] >= (gen_shoot_per*2)) {
            shotangle_info$per_col[i] <- "#008B00"
          } else {
            shotper_palette <- colorQuantile(palette = c("#CD0000", "#CDCD00", "#008B00"), domain = seq(0, (gen_shoot_per*2), 0.0001), n = 50)
            shotper_data <- c(0, gen_shoot_per, (gen_shoot_per*2), as.numeric(shotangle_info$per[i]))
            shotangle_info$per_col[i] <- as.character(shotper_palette(shotper_data)[4])
          }
        }
      }
      
      plot_ly(type = "scatter", x = shotangle_info$Dot_Loc_x, y = shotangle_info$Dot_Loc_y, hoverinfo = "none", 
              source = "sco_side1_shotangle_click", key = shotangle_info$Shot_Angle_Bin,  
              mode = "markers", 
              marker = list(
                symbol = "circle", 
                size = 80, 
                color = shotangle_info$per_col, 
                line = list(
                  color = "#A2002A", 
                  width = 3
                )
              )) %>% 
        hide_colorbar() %>% 
        hide_legend() %>% 
        layout(
          images = list(
            list(
              source = base64enc::dataURI(file = shot_angle), 
              xref = "x",
              yref = "y",
              x = -0.04,
              y = 1.01,
              sizex = 1.08,
              sizey = 1.02,
              sizing = "stretch",
              layer = "below"
            ), 
            list(
              source = base64enc::dataURI(file = paste("www/", df_team_info$Logo_Path[df_team_info$Team == df_team_info$Team[side_vars$Side1_Row]], sep = "")), 
              xref = "x",
              yref = "y",
              x = -0.1,
              y = 0.95,
              sizex = (0.17*df_team_info$Logo_Path_Width[df_team_info$Team == df_team_info$Team[side_vars$Side1_Row]]) / df_team_info$Logo_Path_Width[df_team_info$Team == df_team_info$Team[side_vars$Side1_Row]],
              sizey = (0.17*df_team_info$Logo_Path_Height[df_team_info$Team == df_team_info$Team[side_vars$Side1_Row]]) / df_team_info$Logo_Path_Height[df_team_info$Team == df_team_info$Team[side_vars$Side1_Row]],
              layer = "above"
            )
          ), 
          margin = list(
            t = 0, 
            b = 0, 
            l = 0, 
            r = 0, 
            pad = 0
          ), 
          xaxis = list(
            range = c(-0.1, 1.1), 
            visible = F, 
            fixedrange = TRUE
          ),
          yaxis = list(
            range = c(-0.1, 1.1),
            visible = F, 
            scaleanchor = "x", 
            scaleratio = 1, 
            fixedrange = TRUE
          ), 
          dragmode = F
        ) %>% 
        config(
          modeBarButtonsToRemove = c("zoom2d", "toggleSpikelines", "hoverClosestCartesian", 
                                     "hoverCompareCartesian", "drawline", "autoScale2d", "resetScale2d", 
                                     "zoomIn2d", "zoomOut2d", "pan2d", 'lasso2d')
        ) %>%
        add_annotations(x = shotangle_info$Dot_Loc_x ,
                        y = shotangle_info$Dot_Loc_y + 0.025,
                        text = shotangle_info$Ratio,
                        xref = "x",
                        yref = "y",
                        showarrow = F,
                        # Styling annotations' text:
                        font = list(color = "white",
                                    size = 13)
        ) %>%
        add_annotations(x = shotangle_info$Dot_Loc_x,
                        y = shotangle_info$Dot_Loc_y - 0.025,
                        text = shotangle_info$Percentage,
                        xref = "x",
                        yref = "y",
                        showarrow = F,
                        # Styling annotations' text:
                        font = list(color = "white",
                                    size = 13)
        ) %>% 
        event_register("plotly_click")
      
    }
  })
  
  ##### SCO: Side1 Field #####
  
  output$scoring_side1_field <- renderPlotly({
    if(length(input$scoring_strength) > 0){
      
      shot_df <- df_master_fltr() %>% 
        filter(Team == df_team_info$Team[side_vars$Side1_Row]) %>% 
        filter(Empty_Net == "No" & Shot_Type != "Penalty Shot")
      
      # Filter down by strength input
      if(input$scoring_strength == "Even-Strength") {
        shot_df <- shot_df %>% 
          filter(Home_Strength == Away_Strength)    
      } else {
        if(input$scoring_strength == "Man-Advantage") {
          shot_df <- shot_df %>% 
            group_by(Game) %>% 
            mutate(Side1_Strength = ifelse(Home_Team == df_team_info$Team[side_vars$Side1_Row], 
                                           Home_Strength, Away_Strength), 
                   Side2_Strength = ifelse(Home_Team == df_team_info$Team[side_vars$Side1_Row], 
                                           Away_Strength, Home_Strength)) %>% 
            ungroup() %>% 
            mutate(Keep = ifelse((Team == df_team_info$Team[side_vars$Side1_Row] & Side1_Strength > Side2_Strength) | (Team != df_team_info$Team[side_vars$Side1_Row] & Side1_Strength < Side2_Strength), "Yes", "No")) %>% 
            filter(Keep == "Yes")
        } else {
          if(input$scoring_strength == "Short-Handed") {
            shot_df <- shot_df %>% 
              group_by(Game) %>% 
              mutate(Side1_Strength = ifelse(Home_Team == df_team_info$Team[side_vars$Side1_Row], 
                                             Home_Strength, Away_Strength), 
                     Side2_Strength = ifelse(Home_Team == df_team_info$Team[side_vars$Side1_Row], 
                                             Away_Strength, Home_Strength)) %>% 
              ungroup() %>% 
              mutate(Keep = ifelse((Team == df_team_info$Team[side_vars$Side1_Row] & Side1_Strength < Side2_Strength) | (Team != df_team_info$Team[side_vars$Side1_Row] & Side1_Strength > Side2_Strength), "Yes", "No")) %>% 
              filter(Keep == "Yes")
          }
        }
      }
      
      # Filter down by state input
      if(input$scoring_state != "All") {
        shot_df <- shot_df %>% 
          filter(State %in% input$scoring_state)
      } 
      
      # Filter by click selections
      if(sco_side1_goal_sel$Net_Location[1] != "none"){
        shot_df <- shot_df %>% 
          filter(Net_Location %in% sco_side1_goal_sel$Net_Location)
      }
      
      if(sco_side1_shotangle_sel$Shot_Angle_Bin[1] != "none") {
        shot_df <- shot_df %>% 
          filter(Shot_Angle != "NA") %>% 
          mutate(Shot_Angle_Bin = ifelse(Shot_Angle == "Dive/Other", "Dive/Other", 
                                         ifelse(as.numeric(Shot_Angle) < 60, "LowRight", 
                                                ifelse(as.numeric(Shot_Angle) < 120, "MidRight", 
                                                       ifelse(as.numeric(Shot_Angle) < 180, "HighRight", 
                                                              ifelse(180 & as.numeric(Shot_Angle) < 240, "HighLeft", 
                                                                     ifelse(as.numeric(Shot_Angle) < 300, "MidLeft", "LowLeft"))))))) %>%
          filter(Shot_Angle_Bin %in% sco_side1_shotangle_sel$Shot_Angle_Bin)
      }
      
      if(sco_side1_field_sel$Field_Location_Bin[1] != "none"){
        shot_df <- shot_df %>% 
          filter(Field_Location_Bin %in% sco_side1_field_sel$Field_Location_Bin)
      }
      
      if(length(input$scoring_total_avg) > 0) {
        if(input$scoring_total_avg == "Totals") {
          field_df.a <- shot_df %>% 
            group_by(Field_Location_Bin) %>% 
            summarise(Goals = length(Game_Clock[Result == "Goal"]), 
                      Shots = length(Game_Clock)) %>% 
            ungroup() %>% 
            mutate(per = Goals / Shots, 
                   Ratio = paste(Goals, "/", Shots, sep = ""), 
                   Percentage = percent(per, accuracy = 0.1))
        } else {
          games <- length(unique(shot_df$Game))
          field_df.a <- shot_df %>% 
            group_by(Field_Location_Bin) %>% 
            summarise(Goals = length(Game_Clock[Result == "Goal"]), 
                      Shots = length(Game_Clock)) %>% 
            ungroup() %>% 
            mutate(per = Goals / Shots, 
                   Ratio = paste(round((Goals/games), 1), "/", round((Shots/games), 1), sep = ""), 
                   Percentage = percent(per, accuracy = 0.1))
        }
      } else {
        field_df.a <- shot_df %>% 
          group_by(Field_Location_Bin) %>% 
          summarise(Goals = length(Game_Clock[Result == "Goal"]), 
                    Shots = length(Game_Clock)) %>% 
          ungroup() %>% 
          mutate(per = Goals / Shots, 
                 Ratio = paste(Goals, "/", Shots, sep = ""), 
                 Percentage = percent(per, accuracy = 0.1))
      }
      
      field_df <- left_join(field_df.b, field_df.a, by = c("Field_Location_Bin"))
      
      field_df <- left_join(field_df, field_marker_coords, by = "Field_Location_Bin")
      
      field_df$Percentage_Loc_x <- field_df$Ratio_Loc_x
      field_df$Percentage_Loc_y <- field_df$Ratio_Loc_y - 0.04
      field_df$Dot_Loc_x <- field_df$Ratio_Loc_x
      field_df$Dot_Loc_y <- field_df$Ratio_Loc_y - 0.02
      field_df[is.na(field_df)] <- 0
      field_df <- field_df %>% 
        mutate(Percentage = ifelse(Percentage == "0", "", Percentage), 
               per_col = "#000000")
      
      for(i in c(1:nrow(field_df))) {
        if(field_df$Shots[i] == 0) {
          field_df$per_col[i] <- "#000000"
        } else {
          if(field_df$per[i] >= (gen_shoot_per*2)) {
            field_df$per_col[i] <- "#008B00"
          } else {
            shotper_palette <- colorQuantile(palette = c("#CD0000", "#CDCD00", "#008B00"), domain = seq(0, (gen_shoot_per*2), 0.0001), n = 50)
            shotper_data <- c(0, gen_shoot_per, (gen_shoot_per*2), as.numeric(field_df$per[i]))
            field_df$per_col[i] <- as.character(shotper_palette(shotper_data)[4])
          }
        }
      }
      
      plot_ly(type = "scatter", x = field_df$Dot_Loc_x, y = field_df$Dot_Loc_y,  hoverinfo = "none", 
              source = "sco_side1_field_click", key = field_df$Field_Location_Bin, 
              mode = "markers", 
              marker = list(
                symbol = "square", 
                size = 60, 
                color = field_df$per_col, 
                line = list(
                  color = "white", 
                  width = 2
                )
              )) %>% 
        hide_colorbar() %>% 
        layout(
          images = list(
            list(
              source = base64enc::dataURI(file = field_grid), 
              xref = "x",
              yref = "y",
              x = -0.036,
              y = 1.027,
              sizex = 1.08,
              sizey = 1.05,
              sizing = "stretch",
              layer = "below"
            ), 
            list(
              source = base64enc::dataURI(file = paste("www/", df_team_info$Wordmark_Path[df_team_info$Team == df_team_info$Team[side_vars$Side1_Row]], sep = "")), 
              xref = "x",
              yref = "y",
              x = 0.02,
              y = 1,
              sizex = (0.3*df_team_info$Wordmark_Path_Width[df_team_info$Team == df_team_info$Team[side_vars$Side1_Row]]) / df_team_info$Wordmark_Path_Width[df_team_info$Team == df_team_info$Team[side_vars$Side1_Row]],
              sizey = (0.3*df_team_info$Wordmark_Path_Height[df_team_info$Team == df_team_info$Team[side_vars$Side1_Row]]) / df_team_info$Wordmark_Path_Height[df_team_info$Team == df_team_info$Team[side_vars$Side1_Row]],
              sizing = "contain", 
              layer = "above"
            )
          ), 
          margin = list(
            t = 0, 
            b = 0, 
            l = 0, 
            r = 0, 
            pad = 0
          ), 
          xaxis = list(
            range = c(-0.1, 1.1), 
            visible = F, 
            fixedrange = TRUE
          ),
          yaxis = list(
            range = c(-0.1, 1.1),
            visible = F, 
            scaleanchor = "x", 
            scaleratio = 1.125, 
            fixedrange = TRUE
          ), 
          dragmode = F
        ) %>% 
        config(
          modeBarButtonsToRemove = c("zoom2d", "toggleSpikelines", "hoverClosestCartesian", 
                                     "hoverCompareCartesian", "drawline", "autoScale2d", "resetScale2d", 
                                     "zoomIn2d", "zoomOut2d", "pan2d", 'lasso2d')
        ) %>%
        add_annotations(x = field_df$Ratio_Loc_x,
                        y = field_df$Ratio_Loc_y,
                        text = field_df$Ratio,
                        xref = "x",
                        yref = "y",
                        showarrow = F,
                        # Styling annotations' text:
                        font = list(color = "white",
                                    size = 13)
                        
        ) %>% 
        add_annotations(x = field_df$Percentage_Loc_x,
                        y = field_df$Percentage_Loc_y,
                        text = field_df$Percentage,
                        xref = "x",
                        yref = "y",
                        showarrow = F,
                        # Styling annotations' text:
                        font = list(color = "white",
                                    size = 13)
        ) %>% 
        event_register("plotly_click")
      
    }
  })
  
  ##### SCO: Side2 Goal #####
  
  output$scoring_side2_goal <- renderPlotly({
    if(length(input$scoring_strength) > 0) {
      
      if(df_team_info$Team[side_vars$Side2_Row] == "World Lacrosse") {
        shot_df <- df_master_fltr() %>% 
          filter(Team != df_team_info$Team[side_vars$Side1_Row]) %>% 
          filter(Empty_Net == "No" & Shot_Type != "Penalty Shot")
      } else {
        shot_df <- df_master_fltr() %>% 
          filter(Team == df_team_info$Team[side_vars$Side2_Row]) %>% 
          filter(Empty_Net == "No" & Shot_Type != "Penalty Shot")
      }
      
      # Filter down by strength input
      if(input$scoring_strength == "Even-Strength") {
        shot_df <- shot_df %>% 
          filter(Home_Strength == Away_Strength)    
      } else {
        if(input$scoring_strength == "Man-Advantage") {
          shot_df <- shot_df %>% 
            group_by(Game) %>% 
            mutate(Side1_Strength = ifelse(Home_Team == df_team_info$Team[side_vars$Side1_Row], 
                                           Home_Strength, Away_Strength), 
                   Side2_Strength = ifelse(Home_Team == df_team_info$Team[side_vars$Side1_Row], 
                                           Away_Strength, Home_Strength)) %>% 
            ungroup() %>% 
            mutate(Keep = ifelse((Team == df_team_info$Team[side_vars$Side1_Row] & Side1_Strength > Side2_Strength) | (Team != df_team_info$Team[side_vars$Side1_Row] & Side1_Strength < Side2_Strength), "Yes", "No")) %>% 
            filter(Keep == "Yes")
        } else {
          if(input$scoring_strength == "Short-Handed") {
            shot_df <- shot_df %>% 
              group_by(Game) %>% 
              mutate(Side1_Strength = ifelse(Home_Team == df_team_info$Team[side_vars$Side1_Row], 
                                             Home_Strength, Away_Strength), 
                     Side2_Strength = ifelse(Home_Team == df_team_info$Team[side_vars$Side1_Row], 
                                             Away_Strength, Home_Strength)) %>% 
              ungroup() %>% 
              mutate(Keep = ifelse((Team == df_team_info$Team[side_vars$Side1_Row] & Side1_Strength < Side2_Strength) | (Team != df_team_info$Team[side_vars$Side1_Row] & Side1_Strength > Side2_Strength), "Yes", "No")) %>% 
              filter(Keep == "Yes")
          }
        }
      }
      
      # Filter down by state input
      if(input$scoring_state != "All") {
        shot_df <- shot_df %>% 
          filter(State %in% input$scoring_state)
      } 
      
      # Filter by click selections
      if(sco_side2_goal_sel$Net_Location[1] != "none"){
        shot_df <- shot_df %>% 
          filter(Net_Location %in% sco_side2_goal_sel$Net_Location)
      }
      
      if(sco_side2_shotangle_sel$Shot_Angle_Bin[1] != "none") {
        shot_df <- shot_df %>% 
          filter(Shot_Angle != "NA") %>% 
          mutate(Shot_Angle_Bin = ifelse(Shot_Angle == "Dive/Other", "Dive/Other", 
                                         ifelse(as.numeric(Shot_Angle) < 60, "LowRight", 
                                                ifelse(as.numeric(Shot_Angle) < 120, "MidRight", 
                                                       ifelse(as.numeric(Shot_Angle) < 180, "HighRight", 
                                                              ifelse(180 & as.numeric(Shot_Angle) < 240, "HighLeft", 
                                                                     ifelse(as.numeric(Shot_Angle) < 300, "MidLeft", "LowLeft"))))))) %>%
          filter(Shot_Angle_Bin %in% sco_side2_shotangle_sel$Shot_Angle_Bin)
      }
      
      if(sco_side2_field_sel$Field_Location_Bin[1] != "none"){
        shot_df <- shot_df %>% 
          filter(Field_Location_Bin %in% sco_side2_field_sel$Field_Location_Bin)
      }
      
      goal_info.a <- shot_df %>%
        group_by(Net_Location) %>%
        summarise(Shots = length(Result), 
                  Goals = length(Result[Event_Type == "Shot" & Result == "Goal"])) %>%
        mutate(per = Goals / Shots, 
               Percentage = percent(Goals / Shots, accuracy = 0.1))
      goal_info <- left_join(goal_info.b, goal_info.a, by = c("Net_Location"))
      goal_info[is.na(goal_info)] <- 0
      
      if(length(input$scoring_total_avg) > 0) {
        if(input$scoring_total_avg == "Totals") {
          goal_info <- goal_info %>% 
            mutate(Ratio = ifelse(Net_Location %in% c("Chest", "Crossbar", "Empty Net", "Left Miss", "Left Post", 
                                                      "Right Miss", "Right Post", "Top Miss"), 
                                  paste(Shots), paste(Goals, "/", Shots, sep = "")))
        } else {
          games <- length(unique(shot_df$Game))
          goal_info <- goal_info %>% 
            mutate(Ratio = ifelse(Net_Location %in% c("Chest", "Crossbar", "Empty Net", "Left Miss", "Left Post", 
                                                      "Right Miss", "Right Post", "Top Miss"), 
                                  paste(round((Shots/games), 2)), paste(round((Goals/games), 2), "/", round((Shots/games), 2), sep = "")))
        }
      } else {
        goal_info <- goal_info %>% 
          mutate(Ratio = ifelse(Net_Location %in% c("Chest", "Crossbar", "Empty Net", "Left Miss", "Left Post", 
                                                    "Right Miss", "Right Post", "Top Miss"), 
                                paste(Shots), paste(Goals, "/", Shots, sep = "")))
      }
      
      goal_info <- left_join(goal_info, goal_marker_coords, by = "Net_Location")
      
      goal_info <- goal_info %>% filter(!is.na(Ratio_Loc_x))
      
      goal_info <- rbind(goal_info, 
                         data.frame("Net_Location" = "Filler", "Shots" = 0, "Goals" = 0, "per" = 0, 
                                    "Percentage" = 0, "Ratio" = 0, "Dot_Loc_x" = 2, "Dot_Loc_y" = 2, 
                                    "Ratio_Loc_x" = 2, "Ratio_Loc_y" = 2, "Percentage_Loc_x" = NA, 
                                    "Percentage_Loc_y" = NA, "Goal_Possible" = "No", stringsAsFactors = F))
      
      goal_info <- goal_info %>% 
        mutate(per = ifelse(Goal_Possible == "No", 0, per), 
               per_col = "#000000")
      
      for(i in c(1:nrow(goal_info))) {
        if(goal_info$Shots[i] == 0) {
          goal_info$per_col[i] <- "#000000"
        } else {
          if(goal_info$per[i] >= (shoton_shoot_per*2)) {
            goal_info$per_col[i] <- "#008B00"
          } else {
            shotper_palette <- colorQuantile(palette = c("#CD0000", "#CDCD00", "#008B00"), domain = seq(0, (shoton_shoot_per*2), 0.0001), n = 50)
            shotper_data <- c(0, shoton_shoot_per, (shoton_shoot_per*2), as.numeric(goal_info$per[i]))
            goal_info$per_col[i] <- as.character(shotper_palette(shotper_data)[4])
          }
        }
      }
      
      plot_ly(type = "scatter", x = goal_info$Dot_Loc_x[goal_info$Goal_Possible == "Yes"], 
              y = goal_info$Dot_Loc_y[goal_info$Goal_Possible == "Yes"], hoverinfo = "none", 
              source = "sco_side2_goal_click", key = goal_info$Net_Location[goal_info$Goal_Possible == "Yes"],  
              mode = "markers", 
              marker = list(
                symbol = "circle", 
                size = 80, 
                color = goal_info$per_col[goal_info$Goal_Possible == "Yes"], 
                line = list(
                  color = "white", 
                  width = 3
                )
              )) %>% 
        add_trace(type = "scatter", x = goal_info$Dot_Loc_x[goal_info$Goal_Possible == "No"], 
                  y = goal_info$Dot_Loc_y[goal_info$Goal_Possible == "No"], hoverinfo = "none",
                  source = "sco_side2_goal_click", key = goal_info$Net_Location[goal_info$Goal_Possible == "No"],  
                  mode = "markers", 
                  marker = list(
                    symbol = "square",
                    size = 50, 
                    color = "#000000", 
                    line = list(
                      color = "white", 
                      width = 3
                    )
                  )) %>%
        hide_colorbar() %>% 
        hide_legend() %>% 
        layout(
          images = list(
            list(
              source = base64enc::dataURI(file = goal_base), 
              xref = "x",
              yref = "y",
              x = -0.04,
              y = 1.01,
              sizex = 1.08,
              sizey = 1.02,
              sizing = "stretch",
              layer = "below"
            ), 
            list(
              source = base64enc::dataURI(file = paste("www/", df_team_info$Logo_Path[df_team_info$Team == df_team_info$Team[side_vars$Side2_Row]], sep = "")), 
              xref = "x",
              yref = "y",
              x = -0.01,
              y = 0.97,
              sizex = (0.22*df_team_info$Logo_Path_Width[df_team_info$Team == df_team_info$Team[side_vars$Side2_Row]]) / df_team_info$Logo_Path_Width[df_team_info$Team == df_team_info$Team[side_vars$Side2_Row]],
              sizey = (0.22*df_team_info$Logo_Path_Height[df_team_info$Team == df_team_info$Team[side_vars$Side2_Row]]) / df_team_info$Logo_Path_Height[df_team_info$Team == df_team_info$Team[side_vars$Side2_Row]],
              layer = "above"
            )
          ), 
          margin = list(
            t = 0, 
            b = 0, 
            l = 0, 
            r = 0, 
            pad = 0
          ), 
          xaxis = list(
            range = c(-0.1, 1.1), 
            visible = F, 
            fixedrange = TRUE
          ),
          yaxis = list(
            range = c(-0.1, 1.1),
            visible = F, 
            scaleanchor = "x", 
            scaleratio = 0.7013, 
            fixedrange = TRUE
          ), 
          dragmode = F
        ) %>% 
        config(
          modeBarButtonsToRemove = c("zoom2d", "toggleSpikelines", "hoverClosestCartesian", 
                                     "hoverCompareCartesian", "drawline", "autoScale2d", "resetScale2d", 
                                     "zoomIn2d", "zoomOut2d", "pan2d", 'lasso2d')
        ) %>%
        add_annotations(x = goal_info$Ratio_Loc_x,
                        y = goal_info$Ratio_Loc_y,
                        text = goal_info$Ratio,
                        xref = "x",
                        yref = "y",
                        showarrow = F,
                        # Styling annotations' text:
                        font = list(color = "white",
                                    size = 13)
        ) %>%
        add_annotations(x = goal_info$Percentage_Loc_x[!is.na(goal_info$Percentage_Loc_x)],
                        y = goal_info$Percentage_Loc_y[!is.na(goal_info$Percentage_Loc_x)],
                        text = goal_info$Percentage[!is.na(goal_info$Percentage_Loc_x)],
                        xref = "x",
                        yref = "y",
                        showarrow = F,
                        # Styling annotations' text:
                        font = list(color = "white",
                                    size = 13)
        ) %>% 
        event_register("plotly_click")
      
    }
  })
  
  ##### SCO: Side2 Shot Angle #####
  
  output$scoring_side2_shotangle <- renderPlotly({
    if(length(input$scoring_strength) > 0) {
      
      if(df_team_info$Team[side_vars$Side2_Row] == "World Lacrosse") {
        shot_df <- df_master_fltr() %>% 
          filter(Team != df_team_info$Team[side_vars$Side1_Row]) %>% 
          filter(Empty_Net == "No" & Shot_Type != "Penalty Shot")
      } else {
        shot_df <- df_master_fltr() %>% 
          filter(Team == df_team_info$Team[side_vars$Side2_Row]) %>% 
          filter(Empty_Net == "No" & Shot_Type != "Penalty Shot")
      }
      
      # Filter down by strength input
      if(input$scoring_strength == "Even-Strength") {
        shot_df <- shot_df %>% 
          filter(Home_Strength == Away_Strength)    
      } else {
        if(input$scoring_strength == "Man-Advantage") {
          shot_df <- shot_df %>% 
            group_by(Game) %>% 
            mutate(Side1_Strength = ifelse(Home_Team == df_team_info$Team[side_vars$Side1_Row], 
                                           Home_Strength, Away_Strength), 
                   Side2_Strength = ifelse(Home_Team == df_team_info$Team[side_vars$Side1_Row], 
                                           Away_Strength, Home_Strength)) %>% 
            ungroup() %>% 
            mutate(Keep = ifelse((Team == df_team_info$Team[side_vars$Side1_Row] & Side1_Strength > Side2_Strength) | (Team != df_team_info$Team[side_vars$Side1_Row] & Side1_Strength < Side2_Strength), "Yes", "No")) %>% 
            filter(Keep == "Yes")
        } else {
          if(input$scoring_strength == "Short-Handed") {
            shot_df <- shot_df %>% 
              group_by(Game) %>% 
              mutate(Side1_Strength = ifelse(Home_Team == df_team_info$Team[side_vars$Side1_Row], 
                                             Home_Strength, Away_Strength), 
                     Side2_Strength = ifelse(Home_Team == df_team_info$Team[side_vars$Side1_Row], 
                                             Away_Strength, Home_Strength)) %>% 
              ungroup() %>% 
              mutate(Keep = ifelse((Team == df_team_info$Team[side_vars$Side1_Row] & Side1_Strength < Side2_Strength) | (Team != df_team_info$Team[side_vars$Side1_Row] & Side1_Strength > Side2_Strength), "Yes", "No")) %>% 
              filter(Keep == "Yes")
          }
        }
      }
      
      # Filter down by state input
      if(input$scoring_state != "All") {
        shot_df <- shot_df %>% 
          filter(State %in% input$scoring_state)
      } 
      
      # Filter by click selections
      if(sco_side2_goal_sel$Net_Location[1] != "none"){
        shot_df <- shot_df %>% 
          filter(Net_Location %in% sco_side2_goal_sel$Net_Location)
      }
      
      if(sco_side2_shotangle_sel$Shot_Angle_Bin[1] != "none") {
        shot_df <- shot_df %>% 
          filter(Shot_Angle != "NA") %>% 
          mutate(Shot_Angle_Bin = ifelse(Shot_Angle == "Dive/Other", "Dive/Other", 
                                         ifelse(as.numeric(Shot_Angle) < 60, "LowRight", 
                                                ifelse(as.numeric(Shot_Angle) < 120, "MidRight", 
                                                       ifelse(as.numeric(Shot_Angle) < 180, "HighRight", 
                                                              ifelse(180 & as.numeric(Shot_Angle) < 240, "HighLeft", 
                                                                     ifelse(as.numeric(Shot_Angle) < 300, "MidLeft", "LowLeft"))))))) %>%
          filter(Shot_Angle_Bin %in% sco_side2_shotangle_sel$Shot_Angle_Bin)
      }
      
      if(sco_side2_field_sel$Field_Location_Bin[1] != "none"){
        shot_df <- shot_df %>% 
          filter(Field_Location_Bin %in% sco_side2_field_sel$Field_Location_Bin)
      }
      
      shotangle_info.a <- shot_df %>% 
        filter(Shot_Angle != "NA") %>% 
        mutate(Shot_Angle_Bin = ifelse(Shot_Angle == "Dive/Other", "Dive/Other", 
                                       ifelse(as.numeric(Shot_Angle) < 60, "LowRight", 
                                              ifelse(as.numeric(Shot_Angle) < 120, "MidRight", 
                                                     ifelse(as.numeric(Shot_Angle) < 180, "HighRight", 
                                                            ifelse(180 & as.numeric(Shot_Angle) < 240, "HighLeft", 
                                                                   ifelse(as.numeric(Shot_Angle) < 300, "MidLeft", "LowLeft")))))))
      
      shotangle_info.a <- shotangle_info.a %>%
        group_by(Shot_Angle_Bin) %>%
        summarise(Shots = length(Result), 
                  Goals = length(Result[Event_Type == "Shot" & Result == "Goal"])) %>%
        mutate(per = Goals / Shots, 
               Percentage = percent(Goals / Shots, accuracy = 0.1))
      shotangle_info <- left_join(shotangle_info.b, shotangle_info.a, by = c("Shot_Angle_Bin"))
      shotangle_info[is.na(shotangle_info)] <- 0
      
      if(length(input$scoring_total_avg) > 0) {
        if(input$scoring_total_avg == "Totals") {
          shotangle_info <- shotangle_info %>% 
            mutate(Ratio = paste(Goals, "/", Shots, sep = ""))
        } else {
          games <- length(unique(shot_df$Game))
          shotangle_info <- shotangle_info %>% 
            mutate(Ratio = paste(round((Goals/games), 2), "/", round((Shots/games), 2), sep = ""))
        }
      } else {
        shotangle_info <- shotangle_info %>% 
          mutate(Ratio = paste(Goals, "/", Shots, sep = ""))
      }
      
      shotangle_info <- left_join(shotangle_info, shot_angle_coords, by = "Shot_Angle_Bin")
      
      shotangle_info <- shotangle_info %>% 
        mutate(per_col = "#000000")
      
      for(i in c(1:nrow(shotangle_info))) {
        if(shotangle_info$Shots[i] == 0) {
          shotangle_info$per_col[i] <- "#000000"
        } else {
          if(shotangle_info$per[i] >= (gen_shoot_per*2)) {
            shotangle_info$per_col[i] <- "#008B00"
          } else {
            shotper_palette <- colorQuantile(palette = c("#CD0000", "#CDCD00", "#008B00"), domain = seq(0, (gen_shoot_per*2), 0.0001), n = 50)
            shotper_data <- c(0, gen_shoot_per, (gen_shoot_per*2), as.numeric(shotangle_info$per[i]))
            shotangle_info$per_col[i] <- as.character(shotper_palette(shotper_data)[4])
          }
        }
      }
      
      plot_ly(type = "scatter", x = shotangle_info$Dot_Loc_x, y = shotangle_info$Dot_Loc_y, hoverinfo = "none", 
              source = "sco_side2_shotangle_click", key = shotangle_info$Shot_Angle_Bin,  
              mode = "markers", 
              marker = list(
                symbol = "circle", 
                size = 80, 
                color = shotangle_info$per_col, 
                line = list(
                  color = "#A2002A", 
                  width = 3
                )
              )) %>% 
        hide_colorbar() %>% 
        hide_legend() %>% 
        layout(
          images = list(
            list(
              source = base64enc::dataURI(file = shot_angle), 
              xref = "x",
              yref = "y",
              x = -0.04,
              y = 1.01,
              sizex = 1.08,
              sizey = 1.02,
              sizing = "stretch",
              layer = "below"
            ), 
            list(
              source = base64enc::dataURI(file = paste("www/", df_team_info$Logo_Path[df_team_info$Team == df_team_info$Team[side_vars$Side2_Row]], sep = "")), 
              xref = "x",
              yref = "y",
              x = -0.1,
              y = 0.95,
              sizex = (0.17*df_team_info$Logo_Path_Width[df_team_info$Team == df_team_info$Team[side_vars$Side2_Row]]) / df_team_info$Logo_Path_Width[df_team_info$Team == df_team_info$Team[side_vars$Side2_Row]],
              sizey = (0.17*df_team_info$Logo_Path_Height[df_team_info$Team == df_team_info$Team[side_vars$Side2_Row]]) / df_team_info$Logo_Path_Height[df_team_info$Team == df_team_info$Team[side_vars$Side2_Row]],
              layer = "above"
            )
          ), 
          margin = list(
            t = 0, 
            b = 0, 
            l = 0, 
            r = 0, 
            pad = 0
          ), 
          xaxis = list(
            range = c(-0.1, 1.1), 
            visible = F, 
            fixedrange = TRUE
          ),
          yaxis = list(
            range = c(-0.1, 1.1),
            visible = F, 
            scaleanchor = "x", 
            scaleratio = 1, 
            fixedrange = TRUE
          ), 
          dragmode = F
        ) %>% 
        config(
          modeBarButtonsToRemove = c("zoom2d", "toggleSpikelines", "hoverClosestCartesian", 
                                     "hoverCompareCartesian", "drawline", "autoScale2d", "resetScale2d", 
                                     "zoomIn2d", "zoomOut2d", "pan2d", 'lasso2d')
        ) %>%
        add_annotations(x = shotangle_info$Dot_Loc_x ,
                        y = shotangle_info$Dot_Loc_y + 0.025,
                        text = shotangle_info$Ratio,
                        xref = "x",
                        yref = "y",
                        showarrow = F,
                        # Styling annotations' text:
                        font = list(color = "white",
                                    size = 13)
        ) %>%
        add_annotations(x = shotangle_info$Dot_Loc_x,
                        y = shotangle_info$Dot_Loc_y - 0.025,
                        text = shotangle_info$Percentage,
                        xref = "x",
                        yref = "y",
                        showarrow = F,
                        # Styling annotations' text:
                        font = list(color = "white",
                                    size = 13)
        ) %>% 
        event_register("plotly_click")
      
    }
  })
  
  
  ##### SCO: Side2 Field #####
  
  output$scoring_side2_field <- renderPlotly({
    if(length(input$scoring_strength) > 0) {
      
      if(df_team_info$Team[side_vars$Side2_Row] == "World Lacrosse") {
        shot_df <- df_master_fltr() %>% 
          filter(Team != df_team_info$Team[side_vars$Side1_Row]) %>% 
          filter(Empty_Net == "No" & Shot_Type != "Penalty Shot")
      } else {
        shot_df <- df_master_fltr() %>% 
          filter(Team == df_team_info$Team[side_vars$Side2_Row]) %>% 
          filter(Empty_Net == "No" & Shot_Type != "Penalty Shot")
      }
      
      # Filter down by strength input
      if(input$scoring_strength == "Even-Strength") {
        shot_df <- shot_df %>% 
          filter(Home_Strength == Away_Strength)    
      } else {
        if(input$scoring_strength == "Man-Advantage") {
          shot_df <- shot_df %>% 
            group_by(Game) %>% 
            mutate(Side1_Strength = ifelse(Home_Team == df_team_info$Team[side_vars$Side1_Row], 
                                           Home_Strength, Away_Strength), 
                   Side2_Strength = ifelse(Home_Team == df_team_info$Team[side_vars$Side1_Row], 
                                           Away_Strength, Home_Strength)) %>% 
            ungroup() %>% 
            mutate(Keep = ifelse((Team == df_team_info$Team[side_vars$Side1_Row] & Side1_Strength > Side2_Strength) | (Team != df_team_info$Team[side_vars$Side1_Row] & Side1_Strength < Side2_Strength), "Yes", "No")) %>% 
            filter(Keep == "Yes")
        } else {
          if(input$scoring_strength == "Short-Handed") {
            shot_df <- shot_df %>% 
              group_by(Game) %>% 
              mutate(Side1_Strength = ifelse(Home_Team == df_team_info$Team[side_vars$Side1_Row], 
                                             Home_Strength, Away_Strength), 
                     Side2_Strength = ifelse(Home_Team == df_team_info$Team[side_vars$Side1_Row], 
                                             Away_Strength, Home_Strength)) %>% 
              ungroup() %>% 
              mutate(Keep = ifelse((Team == df_team_info$Team[side_vars$Side1_Row] & Side1_Strength < Side2_Strength) | (Team != df_team_info$Team[side_vars$Side1_Row] & Side1_Strength > Side2_Strength), "Yes", "No")) %>% 
              filter(Keep == "Yes")
          }
        }
      }
      
      # Filter down by state input
      if(input$scoring_state != "All") {
        shot_df <- shot_df %>% 
          filter(State %in% input$scoring_state)
      }
      
      # Filter by click selections
      if(sco_side2_goal_sel$Net_Location[1] != "none"){
        shot_df <- shot_df %>% 
          filter(Net_Location %in% sco_side2_goal_sel$Net_Location)
      }
      
      if(sco_side2_shotangle_sel$Shot_Angle_Bin[1] != "none") {
        shot_df <- shot_df %>% 
          filter(Shot_Angle != "NA") %>% 
          mutate(Shot_Angle_Bin = ifelse(Shot_Angle == "Dive/Other", "Dive/Other", 
                                         ifelse(as.numeric(Shot_Angle) < 60, "LowRight", 
                                                ifelse(as.numeric(Shot_Angle) < 120, "MidRight", 
                                                       ifelse(as.numeric(Shot_Angle) < 180, "HighRight", 
                                                              ifelse(180 & as.numeric(Shot_Angle) < 240, "HighLeft", 
                                                                     ifelse(as.numeric(Shot_Angle) < 300, "MidLeft", "LowLeft"))))))) %>%
          filter(Shot_Angle_Bin %in% sco_side2_shotangle_sel$Shot_Angle_Bin)
      }
      
      if(sco_side2_field_sel$Field_Location_Bin[1] != "none"){
        shot_df <- shot_df %>% 
          filter(Field_Location_Bin %in% sco_side2_field_sel$Field_Location_Bin)
      }
      
      if(length(input$scoring_total_avg) > 0) {
        if(input$scoring_total_avg == "Totals") {
          field_df.a <- shot_df %>% 
            group_by(Field_Location_Bin) %>% 
            summarise(Goals = length(Game_Clock[Result == "Goal"]), 
                      Shots = length(Game_Clock)) %>% 
            ungroup() %>% 
            mutate(per = Goals / Shots, 
                   Ratio = paste(Goals, "/", Shots, sep = ""), 
                   Percentage = percent(per, accuracy = 0.1))
        } else {
          games <- length(unique(shot_df$Game))
          field_df.a <- shot_df %>% 
            group_by(Field_Location_Bin) %>% 
            summarise(Goals = length(Game_Clock[Result == "Goal"]), 
                      Shots = length(Game_Clock)) %>% 
            ungroup() %>% 
            mutate(per = Goals / Shots, 
                   Ratio = paste(round((Goals/games), 1), "/", round((Shots/games), 1), sep = ""), 
                   Percentage = percent(per, accuracy = 0.1))
        }
      } else {
        field_df.a <- shot_df %>% 
          group_by(Field_Location_Bin) %>% 
          summarise(Goals = length(Game_Clock[Result == "Goal"]), 
                    Shots = length(Game_Clock)) %>% 
          ungroup() %>% 
          mutate(per = Goals / Shots, 
                 Ratio = paste(Goals, "/", Shots, sep = ""), 
                 Percentage = percent(per, accuracy = 0.1))
      }
      
      field_df <- left_join(field_df.b, field_df.a, by = c("Field_Location_Bin"))
      
      field_df <- left_join(field_df, field_marker_coords, by = "Field_Location_Bin")
      
      field_df$Percentage_Loc_x <- field_df$Ratio_Loc_x
      field_df$Percentage_Loc_y <- field_df$Ratio_Loc_y - 0.04
      field_df$Dot_Loc_x <- field_df$Ratio_Loc_x
      field_df$Dot_Loc_y <- field_df$Ratio_Loc_y - 0.02
      field_df[is.na(field_df)] <- 0
      field_df <- field_df %>% 
        mutate(Percentage = ifelse(Percentage == "0", "", Percentage), 
               per_col = "#000000")
      
      for(i in c(1:nrow(field_df))) {
        if(field_df$Shots[i] == 0) {
          field_df$per_col[i] <- "#000000"
        } else {
          if(field_df$per[i] >= (gen_shoot_per*2)) {
            field_df$per_col[i] <- "#008B00"
          } else {
            shotper_palette <- colorQuantile(palette = c("#CD0000", "#CDCD00", "#008B00"), domain = seq(0, (gen_shoot_per*2), 0.0001), n = 50)
            shotper_data <- c(0, gen_shoot_per, (gen_shoot_per*2), as.numeric(field_df$per[i]))
            field_df$per_col[i] <- as.character(shotper_palette(shotper_data)[4])
          }
        }
      }
      
      plot_ly(type = "scatter", x = field_df$Dot_Loc_x, y = field_df$Dot_Loc_y, hoverinfo = "none", 
              source = "sco_side2_field_click", key = field_df$Field_Location_Bin,  
              mode = "markers", 
              marker = list(
                symbol = "square", 
                size = 60, 
                color = field_df$per_col, 
                line = list(
                  color = "white", 
                  width = 2
                )
              )) %>% 
        hide_colorbar() %>% 
        layout(
          images = list(
            list(
              source = base64enc::dataURI(file = field_grid), 
              xref = "x",
              yref = "y",
              x = -0.036,
              y = 1.027,
              sizex = 1.08,
              sizey = 1.05,
              sizing = "stretch",
              layer = "below"
            ), 
            list(
              source = base64enc::dataURI(file = paste("www/", df_team_info$Wordmark_Path[df_team_info$Team == df_team_info$Team[side_vars$Side2_Row]], sep = "")), 
              xref = "x",
              yref = "y",
              x = 0.02,
              y = 1,
              yanchor = "top", 
              sizex = (0.3*df_team_info$Wordmark_Path_Width[df_team_info$Team == df_team_info$Team[side_vars$Side2_Row]]) / df_team_info$Wordmark_Path_Width[df_team_info$Team == df_team_info$Team[side_vars$Side2_Row]],
              sizey = (0.3*df_team_info$Wordmark_Path_Height[df_team_info$Team == df_team_info$Team[side_vars$Side2_Row]]) / df_team_info$Wordmark_Path_Height[df_team_info$Team == df_team_info$Team[side_vars$Side2_Row]],
              sizing = "contain", 
              layer = "above"
            )
          ), 
          margin = list(
            t = 0, 
            b = 0, 
            l = 0, 
            r = 0, 
            pad = 0
          ), 
          xaxis = list(
            range = c(-0.1, 1.1), 
            visible = F, 
            fixedrange = TRUE
          ),
          yaxis = list(
            range = c(-0.1, 1.1),
            visible = F, 
            scaleanchor = "x", 
            scaleratio = 1.125, 
            fixedrange = TRUE
          ), 
          dragmode = F
        ) %>% 
        config(
          modeBarButtonsToRemove = c("zoom2d", "toggleSpikelines", "hoverClosestCartesian", 
                                     "hoverCompareCartesian", "drawline", "autoScale2d", "resetScale2d", 
                                     "zoomIn2d", "zoomOut2d", "pan2d", 'lasso2d')
        ) %>%
        add_annotations(x = field_df$Ratio_Loc_x,
                        y = field_df$Ratio_Loc_y,
                        text = field_df$Ratio,
                        xref = "x",
                        yref = "y",
                        showarrow = F,
                        # Styling annotations' text:
                        font = list(color = "white",
                                    size = 13)
        ) %>% 
        add_annotations(x = field_df$Percentage_Loc_x,
                        y = field_df$Percentage_Loc_y,
                        text = field_df$Percentage,
                        xref = "x",
                        yref = "y",
                        showarrow = F,
                        # Styling annotations' text:
                        font = list(color = "white",
                                    size = 13)
        ) %>% 
        event_register("plotly_click")
      
    }
  })
  
  #### Server - Reb/Def/FO (RDF) ####
  
  ##### RDF: Presubmit Screen #####
  
  output$rdf_presubmit <- renderUI({
    if(side_vars$Side1_Row == "none") {
      fluidPage(
        br(), 
        br(), 
        br(), 
        br(), 
        
        wellPanel(
          h1("Press SUBMIT to View"), 
          
          style = "text-align: center; height: 250px; box-shadow: 0px 10px 5px 5px #888888; border-width: thick;"
        )
      )
    } else {
      fluidPage(
        br(), 
        br(), 
        br(), 
        br(), 
        
        uiOutput(outputId = "rdf_tab")
      )
    }
  })
  
  ##### RDF: Tab UI #####
  
  output$rdf_tab <- renderUI({
    if(data_sel$Current_Selection == "Team Summary") {
      fluidPage(
        fluidRow(
          column(12,
                 wellPanel(
                   fluidRow(
                     column(6, br(), br(), h1("REBOUNDING")), 
                     column(6, 
                            hr(), 
                            h3("STRENGTH OF THE DEFENSE"), 
                            prettyRadioButtons(inputId = "rdf_strength", label = "", 
                                               choices = c("All", "Even-Strength", "Man-Advantage", "Short-Handed"), 
                                               selected = "All", status = "primary", shape = "round", outline = T, 
                                               fill = T, thick = T, animation = "smooth", icon = icon("check"), plain = T, 
                                               bigger = T, inline = T), 
                            hr()
                     )
                   ), 
                   
                   br(), 
                   
                   fluidRow(
                     column(6, 
                            h3(textOutput(outputId = "rdf_reb_side1_title")), 
                            h3("SAVE NOT CLEAN or NOT ON NET"), 
                            plotlyOutput(outputId = "rdf_reb_side1", height = "200px")), 
                     column(6, 
                            h3(textOutput(outputId = "rdf_reb_side2_title")), 
                            h3("SAVE NOT CLEAN or NOT ON NET"), 
                            plotlyOutput(outputId = "rdf_reb_side2", height = "200px"))
                   )
                 )
          )
        ), 
        
        fluidRow(
          column(12,
                 wellPanel(
                   fluidRow(
                     column(6, br(), br(), br(), br(), br(), br(), h1("TURNOVERS")), 
                     column(6, 
                            hr(), 
                            h3("STRENGTH"), 
                            prettyRadioButtons(inputId = "rdf_to_strength", label = "", 
                                               choices = c("All", "Even-Strength", "Man-Advantage", "Short-Handed"), 
                                               selected = "All", status = "primary", shape = "round", outline = T, 
                                               fill = T, thick = T, animation = "smooth", icon = icon("check"), plain = T, 
                                               bigger = T, inline = T), 
                            br(), 
                            h3("STATE OF THE OFFENSE"), 
                            prettyRadioButtons(inputId = "rdf_to_state", label = "", 
                                               choices = c("All", "Set", "Transition"), 
                                               selected = "All", status = "primary", shape = "round", outline = T, 
                                               fill = T, thick = T, animation = "smooth", icon = icon("check"), plain = T, 
                                               bigger = T, inline = T), 
                            hr()
                     )
                   ), 
                   
                   br(), 
                   
                   fluidRow(
                     column(6, 
                            h3(textOutput(outputId = "rdf_to_side1_title")), 
                            plotlyOutput(outputId = "rdf_to_side1", height = "200px")), 
                     column(6, 
                            h3(textOutput(outputId = "rdf_to_side2_title")), 
                            plotlyOutput(outputId = "rdf_to_side2", height = "200px"))
                   ), 
                   
                   fluidRow(
                     dropdownButton(
                       circle = FALSE, 
                       size = "lg", 
                       label = h3("TURNOVER TYPE DEFINITIONS", style = "color: red;"), 
                       up = TRUE, 
                       
                       h3("BALL LOST: "), h4("Possessing Player lost the ball, either forced or un-forced."), 
                       h3("INTERRUPTED PASS: "), h4("Possessing Player passed the ball and it was interrupted by a player on the opposing team."), 
                       h3("STRAY PASS: "), h4("Possessing Player passed the ball and it resulted in a turnover 
                                              without being interrupted by a player on the opposing team. Can be in-bounds or out-of-bounds."), 
                       h3("CONCESSION: "), h4("Possessing Player freely conceded possession of the ball. 
                                              (i.e. End of Shot Clock on Shorthanded possession)")
                     )
                   )
                 )
          )
        )
      )
    } else {
      fluidPage(
        fluidRow(
          column(12,
                 wellPanel(
                   fluidRow(
                     column(6, br(), br(), h1("REBOUNDING")), 
                     column(6, 
                            hr(), 
                            h3("STRENGTH OF THE DEFENSE"), 
                            prettyRadioButtons(inputId = "rdf_strength", label = "", 
                                               choices = c("All", "Even-Strength", "Man-Advantage", "Short-Handed"), 
                                               selected = "All", status = "primary", shape = "round", outline = T, 
                                               fill = T, thick = T, animation = "smooth", icon = icon("check"), plain = T, 
                                               bigger = T, inline = T), 
                            hr()
                     )
                   ), 
                   
                   br(), br(), 
                   
                   fluidRow(
                     column(6, 
                            h3(textOutput(outputId = "rdf_reb_side1_title")), 
                            h3("SAVE NOT CLEAN or NOT ON NET"), 
                            plotlyOutput(outputId = "rdf_reb_side1", height = "200px")), 
                     column(6, 
                            h3(textOutput(outputId = "rdf_reb_side2_title")), 
                            h3("SAVE NOT CLEAN or NOT ON NET"), 
                            plotlyOutput(outputId = "rdf_reb_side2", height = "200px"))
                   )
                 )
          )
        ),
        
        fluidRow(
          column(12,
                 wellPanel(
                   fluidRow(
                     column(6, br(), br(), h1("TURNOVERS")), 
                     column(6, 
                            hr(), 
                            h3("STRENGTH"), 
                            prettyRadioButtons(inputId = "rdf_to_strength", label = "", 
                                               choices = c("All", "Even-Strength", "Man-Advantage", "Short-Handed"), 
                                               selected = "All", status = "primary", shape = "round", outline = T, 
                                               fill = T, thick = T, animation = "smooth", icon = icon("check"), plain = T, 
                                               bigger = T, inline = T), 
                            br(), 
                            h3("STATE OF THE OFFENSE"), 
                            prettyRadioButtons(inputId = "rdf_to_state", label = "", 
                                               choices = c("All", "Set", "Transition"), 
                                               selected = "All", status = "primary", shape = "round", outline = T, 
                                               fill = T, thick = T, animation = "smooth", icon = icon("check"), plain = T, 
                                               bigger = T, inline = T), 
                            hr()
                     )
                   ), 
                   
                   br(), br(), 
                   
                   fluidRow(
                     column(6, 
                            h3(textOutput(outputId = "rdf_to_side1_title")), 
                            plotlyOutput(outputId = "rdf_to_side1", height = "200px")), 
                     column(6, 
                            h3(textOutput(outputId = "rdf_to_side2_title")), 
                            plotlyOutput(outputId = "rdf_to_side2", height = "200px"))
                   ), 
                   
                   fluidRow(
                     dropdownButton(
                       circle = FALSE, 
                       size = "lg", 
                       label = h3("TURNOVER TYPE DEFINITIONS", style = "color: red;"), 
                       up = TRUE, 
                       
                       h3("BALL LOST: "), h4("Possessing Player lost the ball, either forced or un-forced."), 
                       h3("INTERRUPTED PASS: "), h4("Possessing Player passed the ball and it was interrupted by a player on the opposing team."), 
                       h3("STRAY PASS: "), h4("Possessing Player passed the ball and it resulted in a turnover 
                                              without being interrupted by a player on the opposing team. Can be in-bounds or out-of-bounds."), 
                       h3("CONCESSION: "), h4("Possessing Player freely conceded possession of the ball. 
                                              (i.e. End of Shot Clock on Shorthanded possession)")
                     )
                   )
                 )
          )
        )
      )
    }
  })
  
  ##### RDF: Reb. Bars Title - Side1 #####
  
  output$rdf_reb_side1_title <- renderText({
    if(length(input$rdf_strength) > 0) {
      rdf_df <- df_master_fltr() %>% 
        filter(Rebound_Type %in% c("Offensive", "Defensive"))
      
      if(data_sel$Current_Selection == "Team Summary") {
        rdf_df <- rdf_df %>% 
          mutate(Team = ifelse(Team != df_team_info$Team[side_vars$Side1_Row], "Opponent", Team), 
                 Home_Team = ifelse(Home_Team != df_team_info$Team[side_vars$Side1_Row], "Opponent", Home_Team), 
                 Away_Team = ifelse(Away_Team != df_team_info$Team[side_vars$Side1_Row], "Opponent", Away_Team)) %>% 
          mutate(Defensive_Team = ifelse(Team == Home_Team, Away_Team, Home_Team))
      } else {
        rdf_df <- rdf_df %>% 
          mutate(Defensive_Team = ifelse(Team == Home_Team, Away_Team, Home_Team))
      }
      
      # Filter down by strength input
      if(input$rdf_strength == "Even-Strength") {
        rdf_df <- rdf_df %>% 
          filter(Home_Strength == Away_Strength)    
      } else {
        if(input$rdf_strength == "Man-Advantage") {
          rdf_df <- rdf_df %>% 
            group_by(Game) %>% 
            mutate(Side1_Strength = ifelse(Home_Team == df_team_info$Team[side_vars$Side1_Row], 
                                           Home_Strength, Away_Strength), 
                   Side2_Strength = ifelse(Home_Team == df_team_info$Team[side_vars$Side1_Row], 
                                           Away_Strength, Home_Strength)) %>% 
            ungroup() %>% 
            mutate(Keep = ifelse((Defensive_Team == df_team_info$Team[side_vars$Side1_Row] & Side1_Strength > Side2_Strength) | (Defensive_Team != df_team_info$Team[side_vars$Side1_Row] & Side1_Strength < Side2_Strength), "Yes", "No")) %>% 
            filter(Keep == "Yes")
        } else {
          if(input$rdf_strength == "Short-Handed") {
            rdf_df <- rdf_df %>% 
              group_by(Game) %>% 
              mutate(Side1_Strength = ifelse(Home_Team == df_team_info$Team[side_vars$Side1_Row], 
                                             Home_Strength, Away_Strength), 
                     Side2_Strength = ifelse(Home_Team == df_team_info$Team[side_vars$Side1_Row], 
                                             Away_Strength, Home_Strength)) %>% 
              ungroup() %>% 
              mutate(Keep = ifelse((Defensive_Team == df_team_info$Team[side_vars$Side1_Row] & Side1_Strength < Side2_Strength) | (Defensive_Team != df_team_info$Team[side_vars$Side1_Row] & Side1_Strength > Side2_Strength), "Yes", "No")) %>% 
              filter(Keep == "Yes")
          }
        }
      }
      
      if(length(input$rdf_total_avg) > 0) {
        if(input$rdf_total_avg == "Totals") {
          rdf_df <- rdf_df %>% 
            group_by(Defensive_Team) %>% 
            summarise(Total = length(which(!is.na(Result))))
        } else {
          games <- length(unique(rdf_df$Game))
          rdf_df <- rdf_df %>% 
            group_by(Defensive_Team) %>% 
            summarise(Total = round(length(which(!is.na(Result))) / games, 1))
        }
      } else {
        rdf_df <- rdf_df %>% 
          group_by(Defensive_Team) %>% 
          summarise(Total = length(which(!is.na(Result))))
      }
      
      paste(as.numeric(rdf_df$Total[rdf_df$Defensive_Team == df_team_info$Team[side_vars$Side1_Row]])[[1]], 
            "REBOUNDABLE", toupper(df_team_info$Team[side_vars$Side1_Row]), "SHOTS", sep = " ")
    }
  })
  
  ##### RDF: Reb. Bars Side1 #####
  
  output$rdf_reb_side1 <- renderPlotly({
    if(length(input$rdf_strength) > 0) {
      rdf_df <- df_master_fltr() %>% 
        filter(Rebound_Type %in% c("Offensive", "Defensive"))
      
      if(data_sel$Current_Selection == "Team Summary") {
        rdf_df <- rdf_df %>% 
          mutate(Team = ifelse(Team != df_team_info$Team[side_vars$Side1_Row], "Opponent", Team), 
                 Home_Team = ifelse(Home_Team != df_team_info$Team[side_vars$Side1_Row], "Opponent", Home_Team), 
                 Away_Team = ifelse(Away_Team != df_team_info$Team[side_vars$Side1_Row], "Opponent", Away_Team)) %>% 
          mutate(Defensive_Team = ifelse(Team == Home_Team, Away_Team, Home_Team))
      } else {
        rdf_df <- rdf_df %>% 
          mutate(Defensive_Team = ifelse(Team == Home_Team, Away_Team, Home_Team))
      }
      
      # Filter down by strength input
      if(input$rdf_strength == "Even-Strength") {
        rdf_df <- rdf_df %>% 
          filter(Home_Strength == Away_Strength)    
      } else {
        if(input$rdf_strength == "Man-Advantage") {
          rdf_df <- rdf_df %>% 
            group_by(Game) %>% 
            mutate(Side1_Strength = ifelse(Home_Team == df_team_info$Team[side_vars$Side1_Row], 
                                           Home_Strength, Away_Strength), 
                   Side2_Strength = ifelse(Home_Team == df_team_info$Team[side_vars$Side1_Row], 
                                           Away_Strength, Home_Strength)) %>% 
            ungroup() %>% 
            mutate(Keep = ifelse((Defensive_Team == df_team_info$Team[side_vars$Side1_Row] & Side1_Strength > Side2_Strength) | (Defensive_Team != df_team_info$Team[side_vars$Side1_Row] & Side1_Strength < Side2_Strength), "Yes", "No")) %>% 
            filter(Keep == "Yes")
        } else {
          if(input$rdf_strength == "Short-Handed") {
            rdf_df <- rdf_df %>% 
              group_by(Game) %>% 
              mutate(Side1_Strength = ifelse(Home_Team == df_team_info$Team[side_vars$Side1_Row], 
                                             Home_Strength, Away_Strength), 
                     Side2_Strength = ifelse(Home_Team == df_team_info$Team[side_vars$Side1_Row], 
                                             Away_Strength, Home_Strength)) %>% 
              ungroup() %>% 
              mutate(Keep = ifelse((Defensive_Team == df_team_info$Team[side_vars$Side1_Row] & Side1_Strength < Side2_Strength) | (Defensive_Team != df_team_info$Team[side_vars$Side1_Row] & Side1_Strength > Side2_Strength), "Yes", "No")) %>% 
              filter(Keep == "Yes")
          }
        }
      }
      
      # Filter depending on Totals or Averages
      if(length(input$rdf_total_avg) > 0) {
        if(input$rdf_total_avg == "Totals") {
          rdf_df <- rdf_df %>% 
            group_by(Defensive_Team) %>% 
            summarise(Won_Def = length(which((Rebound_Type == "Defensive"))),
                      Gave_Off = length(which(Rebound_Type == "Offensive")), 
                      Total = length(which(!is.na(Rebound_Type))))
        } else {
          games <- length(unique(rdf_df$Game))
          rdf_df <- rdf_df %>% 
            group_by(Defensive_Team) %>% 
            summarise(Won_Def = round(length(which((Rebound_Type == "Defensive"))) / games, 1),
                      Gave_Off = round(length(which((Rebound_Type == "Offensive"))) / games, 1), 
                      Total = round(length(which(!is.na(Rebound_Type))) / games, 1))
        }
      } else {
        rdf_df <- rdf_df %>% 
          group_by(Defensive_Team) %>% 
          summarise(Won_Def = length(which((Rebound_Type == "Defensive"))),
                    Gave_Off = length(which(Rebound_Type == "Offensive")), 
                    Total = length(which(!is.na(Rebound_Type))))
      }
      
      rdf_df <- rdf_df %>% 
        mutate(Won_Def_Label = paste("<B>Won Defensive Reb.\n", Won_Def, " - ", percent((Won_Def / Total), accuracy = 0.1), "</B>", sep = ""),
               Gave_Off_Label = paste("<B>Gave Up Offensive Reb.\n", Gave_Off, " - ", percent((Gave_Off / Total), accuracy = 0.1), "</B>\n", sep = ""))
      rdf_df <- rdf_df %>% 
        filter(Defensive_Team == df_team_info$Team[side_vars$Side1_Row])
      
      plot_ly(rdf_df, type = "bar", x = ~Gave_Off, y = ~Defensive_Team, name = "Gave_Off", orientation = "h", 
              text = ~Gave_Off_Label, textposition = "inside", insidetextanchor = "middle", textfont = list(size = 15), hoverinfo = "text", 
              marker = list(
                color = df_team_info$Color1[side_vars$Side1_Row], 
                line = list(
                  color = "#000000", 
                  width = 2
                )
              )) %>% 
        add_bars(x = ~Won_Def, y = ~Defensive_Team, name = "Won_Def", text = ~Won_Def_Label, textposition = 'inside', 
                 marker = list(
                   color = df_team_info$Color2[side_vars$Side1_Row], 
                   line = list(
                     color = "#000000", 
                     width = 2
                   )
                 )) %>% 
        layout(
          barmode = "stack", 
          height = 150, 
          plot_bgcolor = "#FFFFFF", 
          hoverlabel = list(
            font = list(
              size = 18
            )
          ), 
          yaxis = list(
            title = "", 
            side = "right", 
            visible = FALSE, 
            fixedrange = TRUE
          ), 
          xaxis = list(
            autorange = "reversed", 
            visible = FALSE, 
            fixedrange = TRUE
          ), 
          margin = list(
            r = 10, 
            t = 0, 
            b = 0
          )
        ) %>% 
        hide_legend() %>% 
        config(
          modeBarButtonsToRemove = c('toImage', "zoom2d", "toggleSpikelines", "hoverClosestCartesian", 
                                     "hoverCompareCartesian", "drawline", "autoScale2d", "resetScale2d", 
                                     "zoomIn2d", "zoomOut2d", "pan2d", 'select2d', 'lasso2d')
        )  
    }
  })
  
  ##### RDF: Reb. Bars Title - Side2 #####
  
  output$rdf_reb_side2_title <- renderText({
    if(length(input$rdf_strength) > 0) {
      rdf_df <- df_master_fltr() %>% 
        filter(Rebound_Type %in% c("Offensive", "Defensive"))
      
      if(data_sel$Current_Selection == "Team Summary") {
        rdf_df <- rdf_df %>% 
          mutate(Team = ifelse(Team != df_team_info$Team[side_vars$Side1_Row], "Opponent", Team), 
                 Home_Team = ifelse(Home_Team != df_team_info$Team[side_vars$Side1_Row], "Opponent", Home_Team), 
                 Away_Team = ifelse(Away_Team != df_team_info$Team[side_vars$Side1_Row], "Opponent", Away_Team)) %>% 
          mutate(Defensive_Team = ifelse(Team == Home_Team, Away_Team, Home_Team))
      } else {
        rdf_df <- rdf_df %>% 
          mutate(Defensive_Team = ifelse(Team == Home_Team, Away_Team, Home_Team))
      }
      
      # Filter down by strength input
      if(input$rdf_strength == "Even-Strength") {
        rdf_df <- rdf_df %>% 
          filter(Home_Strength == Away_Strength)    
      } else {
        if(input$rdf_strength == "Man-Advantage") {
          rdf_df <- rdf_df %>% 
            group_by(Game) %>% 
            mutate(Side1_Strength = ifelse(Home_Team == df_team_info$Team[side_vars$Side1_Row], 
                                           Home_Strength, Away_Strength), 
                   Side2_Strength = ifelse(Home_Team == df_team_info$Team[side_vars$Side1_Row], 
                                           Away_Strength, Home_Strength)) %>% 
            ungroup() %>% 
            mutate(Keep = ifelse((Defensive_Team == df_team_info$Team[side_vars$Side1_Row] & Side1_Strength > Side2_Strength) | (Defensive_Team != df_team_info$Team[side_vars$Side1_Row] & Side1_Strength < Side2_Strength), "Yes", "No")) %>% 
            filter(Keep == "Yes")
        } else {
          if(input$rdf_strength == "Short-Handed") {
            rdf_df <- rdf_df %>% 
              group_by(Game) %>% 
              mutate(Side1_Strength = ifelse(Home_Team == df_team_info$Team[side_vars$Side1_Row], 
                                             Home_Strength, Away_Strength), 
                     Side2_Strength = ifelse(Home_Team == df_team_info$Team[side_vars$Side1_Row], 
                                             Away_Strength, Home_Strength)) %>% 
              ungroup() %>% 
              mutate(Keep = ifelse((Defensive_Team == df_team_info$Team[side_vars$Side1_Row] & Side1_Strength < Side2_Strength) | (Defensive_Team != df_team_info$Team[side_vars$Side1_Row] & Side1_Strength > Side2_Strength), "Yes", "No")) %>% 
              filter(Keep == "Yes")
          }
        }
      }
      
      if(length(input$rdf_total_avg) > 0) {
        if(input$rdf_total_avg == "Totals") {
          rdf_df <- rdf_df %>% 
            group_by(Defensive_Team) %>% 
            summarise(Total = length(which(!is.na(Result))))
        } else {
          games <- length(unique(rdf_df$Game))
          rdf_df <- rdf_df %>% 
            group_by(Defensive_Team) %>% 
            summarise(Total = round(length(which(!is.na(Result))) / games, 1))
        }
      } else {
        rdf_df <- rdf_df %>% 
          group_by(Defensive_Team) %>% 
          summarise(Total = length(which(!is.na(Result))))
      }
      
      if(data_sel$Current_Selection == "Team Summary") {
        paste(as.numeric(rdf_df$Total[rdf_df$Defensive_Team != df_team_info$Team[side_vars$Side1_Row]])[[1]], 
              "REBOUNDABLE OPPONENT SHOTS", sep = " ")
      } else {
        paste(as.numeric(rdf_df$Total[rdf_df$Defensive_Team != df_team_info$Team[side_vars$Side1_Row]])[[1]], 
              "REBOUNDABLE", toupper(df_team_info$Team[side_vars$Side2_Row]), "SHOTS", sep = " ")
      }
    }
  })
  
  ##### RDF: Reb. Bars Side2 #####
  
  output$rdf_reb_side2 <- renderPlotly({
    if(length(input$rdf_strength) > 0) {
      rdf_df <- df_master_fltr() %>% 
        filter(Rebound_Type %in% c("Offensive", "Defensive"))
      
      if(data_sel$Current_Selection == "Team Summary") {
        rdf_df <- rdf_df %>% 
          mutate(Team = ifelse(Team != df_team_info$Team[side_vars$Side1_Row], "Opponent", Team), 
                 Home_Team = ifelse(Home_Team != df_team_info$Team[side_vars$Side1_Row], "Opponent", Home_Team), 
                 Away_Team = ifelse(Away_Team != df_team_info$Team[side_vars$Side1_Row], "Opponent", Away_Team)) %>% 
          mutate(Defensive_Team = ifelse(Team == Home_Team, Away_Team, Home_Team))
      } else {
        rdf_df <- rdf_df %>% 
          mutate(Defensive_Team = ifelse(Team == Home_Team, Away_Team, Home_Team))
      }
      
      # Filter down by strength input
      if(input$rdf_strength == "Even-Strength") {
        rdf_df <- rdf_df %>% 
          filter(Home_Strength == Away_Strength)    
      } else {
        if(input$rdf_strength == "Man-Advantage") {
          rdf_df <- rdf_df %>% 
            group_by(Game) %>% 
            mutate(Side1_Strength = ifelse(Home_Team == df_team_info$Team[side_vars$Side1_Row], 
                                           Home_Strength, Away_Strength), 
                   Side2_Strength = ifelse(Home_Team == df_team_info$Team[side_vars$Side1_Row], 
                                           Away_Strength, Home_Strength)) %>% 
            ungroup() %>% 
            mutate(Keep = ifelse((Defensive_Team == df_team_info$Team[side_vars$Side1_Row] & Side1_Strength > Side2_Strength) | (Defensive_Team != df_team_info$Team[side_vars$Side1_Row] & Side1_Strength < Side2_Strength), "Yes", "No")) %>% 
            filter(Keep == "Yes")
        } else {
          if(input$rdf_strength == "Short-Handed") {
            rdf_df <- rdf_df %>% 
              group_by(Game) %>% 
              mutate(Side1_Strength = ifelse(Home_Team == df_team_info$Team[side_vars$Side1_Row], 
                                             Home_Strength, Away_Strength), 
                     Side2_Strength = ifelse(Home_Team == df_team_info$Team[side_vars$Side1_Row], 
                                             Away_Strength, Home_Strength)) %>% 
              ungroup() %>% 
              mutate(Keep = ifelse((Defensive_Team == df_team_info$Team[side_vars$Side1_Row] & Side1_Strength < Side2_Strength) | (Defensive_Team != df_team_info$Team[side_vars$Side1_Row] & Side1_Strength > Side2_Strength), "Yes", "No")) %>% 
              filter(Keep == "Yes")
          }
        }
      }
      
      # Filter depending on Totals or Averages
      if(length(input$rdf_total_avg) > 0) {
        if(input$rdf_total_avg == "Totals") {
          rdf_df <- rdf_df %>% 
            group_by(Defensive_Team) %>% 
            summarise(Won_Def = length(which((Rebound_Type == "Defensive"))),
                      Gave_Off = length(which(Rebound_Type == "Offensive")), 
                      Total = length(which(!is.na(Rebound_Type))))
        } else {
          games <- length(unique(rdf_df$Game))
          rdf_df <- rdf_df %>% 
            group_by(Defensive_Team) %>% 
            summarise(Won_Def = round(length(which((Rebound_Type == "Defensive"))) / games, 1),
                      Gave_Off = round(length(which((Rebound_Type == "Offensive"))) / games, 1), 
                      Total = round(length(which(!is.na(Rebound_Type))) / games, 1))
        }
      } else {
        rdf_df <- rdf_df %>% 
          group_by(Defensive_Team) %>% 
          summarise(Won_Def = length(which((Rebound_Type == "Defensive"))),
                    Gave_Off = length(which(Rebound_Type == "Offensive")), 
                    Total = length(which(!is.na(Rebound_Type))))
      }
      
      rdf_df <- rdf_df %>% 
        mutate(Won_Def_Label = paste("<B>Won Defensive Reb.\n", Won_Def, " - ", percent((Won_Def / Total), accuracy = 0.1), "</B>", sep = ""),
               Gave_Off_Label = paste("<B>Gave Up Offensive Reb.\n", Gave_Off, " - ", percent((Gave_Off / Total), accuracy = 0.1), "</B>", sep = ""))
      rdf_df <- rdf_df %>% 
        filter(Defensive_Team != df_team_info$Team[side_vars$Side1_Row])
      
      plot_ly(rdf_df, type = "bar", x = ~Gave_Off, y = ~Defensive_Team, name = "Gave_Off", orientation = "h", 
              text = ~Gave_Off_Label, textposition = "inside", insidetextanchor = "middle", textfont = list(size = 15), hoverinfo = "text", 
              marker = list(
                color = df_team_info$Color1[side_vars$Side2_Row], 
                line = list(
                  color = "#000000", 
                  width = 2
                )
              )) %>% 
        add_bars(x = ~Won_Def, y = ~Defensive_Team, name = "Won_Def", text = ~Won_Def_Label, textposition = 'inside', 
                 marker = list(
                   color = df_team_info$Color2[side_vars$Side2_Row], 
                   line = list(
                     color = "#000000", 
                     width = 2
                   )
                 )) %>% 
        layout(
          barmode = "stack", 
          height = 150, 
          plot_bgcolor = "#FFFFFF", 
          hoverlabel = list(
            font = list(
              size = 18
            )
          ), 
          yaxis = list(
            title = "", 
            visible = FALSE, 
            fixedrange = TRUE
          ), 
          xaxis = list(
            visible = FALSE, 
            fixedrange = TRUE
          ), 
          margin = list(
            l = 10, 
            t = 0, 
            b = 0
          )
        ) %>% 
        hide_legend() %>% 
        config(
          modeBarButtonsToRemove = c('toImage', "zoom2d", "toggleSpikelines", "hoverClosestCartesian", 
                                     "hoverCompareCartesian", "drawline", "autoScale2d", "resetScale2d", 
                                     "zoomIn2d", "zoomOut2d", "pan2d", 'select2d', 'lasso2d')
        )  
    }
  })
  
  ##### RDF: Turnover Bars Title - Side1 #####
  
  output$rdf_to_side1_title <- renderText({
    if(length(input$rdf_to_strength) > 0) {
      rdf_df <- df_master_fltr() %>% 
        filter(Event_Type == "Turnover")
      
      if(length(unique(df_master_fltr()$Team)) > 2) {
        rdf_df <- rdf_df %>% 
          mutate(Team = ifelse(Team != df_team_info$Team[side_vars$Side1_Row], "Opponent ", Team))
      }
      
      # Filter down by state input
      if(input$rdf_to_state != "All") {
        rdf_df <- rdf_df %>% 
          filter(State %in% input$rdf_to_state)
      }
      
      # Filter down by strength input
      if(input$rdf_to_strength == "Even-Strength") {
        rdf_df <- rdf_df %>% 
          filter(Home_Strength == Away_Strength)    
      } else {
        if(input$rdf_to_strength == "Man-Advantage") {
          rdf_df <- rdf_df %>% 
            group_by(Game) %>% 
            mutate(Side1_Strength = ifelse(Home_Team == df_team_info$Team[side_vars$Side1_Row], 
                                           Home_Strength, Away_Strength), 
                   Side2_Strength = ifelse(Home_Team == df_team_info$Team[side_vars$Side1_Row], 
                                           Away_Strength, Home_Strength)) %>% 
            ungroup() %>% 
            mutate(Keep = ifelse((Team == df_team_info$Team[side_vars$Side1_Row] & Side1_Strength > Side2_Strength) | (Team != df_team_info$Team[side_vars$Side1_Row] & Side1_Strength < Side2_Strength), "Yes", "No")) %>% 
            filter(Keep == "Yes")
        } else {
          if(input$rdf_to_strength == "Short-Handed") {
            rdf_df <- rdf_df %>% 
              group_by(Game) %>% 
              mutate(Side1_Strength = ifelse(Home_Team == df_team_info$Team[side_vars$Side1_Row], 
                                             Home_Strength, Away_Strength), 
                     Side2_Strength = ifelse(Home_Team == df_team_info$Team[side_vars$Side1_Row], 
                                             Away_Strength, Home_Strength)) %>% 
              ungroup() %>% 
              mutate(Keep = ifelse((Team == df_team_info$Team[side_vars$Side1_Row] & Side1_Strength < Side2_Strength) | (Team != df_team_info$Team[side_vars$Side1_Row] & Side1_Strength > Side2_Strength), "Yes", "No")) %>% 
              filter(Keep == "Yes")
          }
        }
      }
      
      if(nrow(rdf_df) > 0) {
        if(length(input$rdf_total_avg) > 0) {
          if(input$rdf_total_avg == "Totals") {
            rdf_df <- rdf_df %>% 
              group_by(Team) %>% 
              summarise(Total = length(which(!is.na(Event_Type))))
          } else {
            games <- length(unique(rdf_df$Game))
            rdf_df <- rdf_df %>% 
              group_by(Team) %>% 
              summarise(Total = round(length(which(!is.na(Event_Type))) / games, 1))
          }
        } else {
          rdf_df <- rdf_df %>% 
            group_by(Team) %>% 
            summarise(Total = length(which(!is.na(Event_Type))))
        }
        
        paste(as.numeric(rdf_df$Total[rdf_df$Team == df_team_info$Team[side_vars$Side1_Row]])[[1]], 
              toupper(df_team_info$Team[side_vars$Side1_Row]), 
              "TURNOVERS", sep = " ")
      } else {
        paste("0", 
              toupper(df_team_info$Team[side_vars$Side1_Row]), 
              "TURNOVERS", sep = " ")
      }
    }
  })
  
  ##### RDF: Turnover Bars Side1 #####
  
  output$rdf_to_side1 <- renderPlotly({
    if(length(input$rdf_to_strength) > 0) {
      rdf_df <- df_master_fltr() %>% 
        filter(Event_Type == "Turnover")
      
      if(length(unique(df_master_fltr()$Team)) > 2) {
        rdf_df <- rdf_df %>% 
          mutate(Team = ifelse(Team != df_team_info$Team[side_vars$Side1_Row], "Opponent ", Team))
      }
      
      # Filter down by state input
      if(input$rdf_to_state != "All") {
        rdf_df <- rdf_df %>% 
          filter(State %in% input$rdf_to_state)
      }
      
      # Filter down by strength input
      if(input$rdf_to_strength == "Even-Strength") {
        rdf_df <- rdf_df %>% 
          filter(Home_Strength == Away_Strength)    
      } else {
        if(input$rdf_to_strength == "Man-Advantage") {
          rdf_df <- rdf_df %>% 
            group_by(Game) %>% 
            mutate(Side1_Strength = ifelse(Home_Team == df_team_info$Team[side_vars$Side1_Row], 
                                           Home_Strength, Away_Strength), 
                   Side2_Strength = ifelse(Home_Team == df_team_info$Team[side_vars$Side1_Row], 
                                           Away_Strength, Home_Strength)) %>% 
            ungroup() %>% 
            mutate(Keep = ifelse((Team == df_team_info$Team[side_vars$Side1_Row] & Side1_Strength > Side2_Strength) | (Team != df_team_info$Team[side_vars$Side1_Row] & Side1_Strength < Side2_Strength), "Yes", "No")) %>% 
            filter(Keep == "Yes")
        } else {
          if(input$rdf_to_strength == "Short-Handed") {
            rdf_df <- rdf_df %>% 
              group_by(Game) %>% 
              mutate(Side1_Strength = ifelse(Home_Team == df_team_info$Team[side_vars$Side1_Row], 
                                             Home_Strength, Away_Strength), 
                     Side2_Strength = ifelse(Home_Team == df_team_info$Team[side_vars$Side1_Row], 
                                             Away_Strength, Home_Strength)) %>% 
              ungroup() %>% 
              mutate(Keep = ifelse((Team == df_team_info$Team[side_vars$Side1_Row] & Side1_Strength < Side2_Strength) | (Team != df_team_info$Team[side_vars$Side1_Row] & Side1_Strength > Side2_Strength), "Yes", "No")) %>% 
              filter(Keep == "Yes")
          }
        }
      }
      
      # Filter depending on Totals or Averages
      if(length(input$rdf_total_avg) > 0) {
        if(input$rdf_total_avg == "Totals") {
          rdf_df <- rdf_df %>% 
            group_by(Team) %>% 
            summarise(BallLost = length(which((Turnover_Type == "Ball Lost"))),
                      StrayPass = length(which(Turnover_Type == "Stray Pass")), 
                      InterPass = length(which(Turnover_Type == "Interrupted Pass")), 
                      Concession = length(which(Turnover_Type == "Concession")), 
                      Total = length(which(!is.na(Turnover_Type))))
        } else {
          games <- length(unique(rdf_df$Game))
          rdf_df <- rdf_df %>% 
            group_by(Team) %>% 
            summarise(BallLost = round(length(which((Turnover_Type == "Ball Lost"))) / games, 1),
                      StrayPass = round(length(which((Turnover_Type == "Stray Pass"))) / games, 1), 
                      InterPass = round(length(which((Turnover_Type == "Interrupted Pass"))) / games, 1),
                      Concession = round(length(which((Turnover_Type == "Concession"))) / games, 1), 
                      Total = round(length(which(!is.na(Turnover_Type))) / games, 1))
        }
      } else {
        rdf_df <- rdf_df %>% 
          group_by(Team) %>% 
          summarise(BallLost = length(which((Turnover_Type == "Ball Lost"))),
                    StrayPass = length(which(Turnover_Type == "Stray Pass")), 
                    InterPass = length(which(Turnover_Type == "Interrupted Pass")), 
                    Concession = length(which(Turnover_Type == "Concession")), 
                    Total = length(which(!is.na(Turnover_Type))))
      }
      
      rdf_df <- rdf_df %>% 
        mutate(BallLost_Label = paste("<B>Ball Lost\n", BallLost, " - ", percent((BallLost / Total), accuracy = 0.1), "</B>", sep = ""),
               StrayPass_Label = paste("<B>Stray Pass\n", StrayPass, " - ", percent((StrayPass / Total), accuracy = 0.1), "</B>", sep = ""), 
               InterPass_Label = paste("<B>Interrupted Pass\n", InterPass, " - ", percent((InterPass / Total), accuracy = 0.1), "</B>", sep = ""),
               Concession_Label = paste("<B>Concession\n", Concession, " - ", percent((Concession / Total), accuracy = 0.1), "</B>", sep = ""))
      rdf_df <- rdf_df %>% 
        filter(Team == df_team_info$Team[side_vars$Side1_Row])
      
      plot_ly(rdf_df, type = "bar", x = ~BallLost, y = ~Team, name = "BallLost", orientation = "h", 
              text = ~BallLost_Label, textposition = "inside", insidetextanchor = "middle", textfont = list(size = 15), hoverinfo = "text", 
              marker = list(
                color = df_team_info$Color1[side_vars$Side1_Row], 
                line = list(
                  color = "#000000", 
                  width = 2
                )
              )) %>% 
        add_bars(x = ~StrayPass, y = ~Team, name = "StrayPass", text = ~StrayPass_Label, 
                 marker = list(
                   color = df_team_info$Color2[side_vars$Side1_Row], 
                   line = list(
                     color = "#000000", 
                     width = 2
                   )
                 )) %>% 
        add_bars(x = ~InterPass, y = ~Team, name = "InterPass", text = ~InterPass_Label, 
                 marker = list(
                   color = df_team_info$Color3[side_vars$Side1_Row], 
                   line = list(
                     color = "#000000", 
                     width = 2
                   )
                 )) %>% 
        add_bars(x = ~Concession, y = ~Team, name = "Concession", text = ~Concession_Label, textposition = 'inside', 
                 marker = list(
                   color = df_team_info$Color2[side_vars$Side1_Row], 
                   line = list(
                     color = "#000000", 
                     width = 2
                   )
                 )) %>% 
        layout(
          barmode = "stack", 
          height = 150, 
          plot_bgcolor = "#FFFFFF", 
          hoverlabel = list(
            font = list(
              size = 18
            )
          ), 
          yaxis = list(
            title = "", 
            side = "right", 
            visible = FALSE, 
            fixedrange = TRUE
          ), 
          xaxis = list(
            autorange = "reversed", 
            visible = FALSE, 
            fixedrange = TRUE
          ), 
          margin = list(
            r = 10, 
            t = 0, 
            b = 0
          )
        ) %>% 
        hide_legend() %>% 
        config(
          modeBarButtonsToRemove = c('toImage', "zoom2d", "toggleSpikelines", "hoverClosestCartesian", 
                                     "hoverCompareCartesian", "drawline", "autoScale2d", "resetScale2d", 
                                     "zoomIn2d", "zoomOut2d", "pan2d", 'select2d', 'lasso2d')
        )  
    }
  })
  
  ##### RDF: Turnover Bars Title - Side2 #####
  
  output$rdf_to_side2_title <- renderText({
    if(length(input$rdf_to_strength) > 0) {
      rdf_df <- df_master_fltr() %>% 
        filter(Event_Type == "Turnover")
      
      if(length(unique(df_master_fltr()$Team)) > 2) {
        rdf_df <- rdf_df %>% 
          mutate(Team = ifelse(Team != df_team_info$Team[side_vars$Side1_Row], "Opponent ", Team))
      }
      
      # Filter down by state input
      if(input$rdf_to_state != "All") {
        rdf_df <- rdf_df %>% 
          filter(State %in% input$rdf_to_state)
      }
      
      # Filter down by strength input
      if(input$rdf_to_strength == "Even-Strength") {
        rdf_df <- rdf_df %>% 
          filter(Home_Strength == Away_Strength)    
      } else {
        if(input$rdf_to_strength == "Man-Advantage") {
          rdf_df <- rdf_df %>% 
            group_by(Game) %>% 
            mutate(Side1_Strength = ifelse(Home_Team == df_team_info$Team[side_vars$Side1_Row], 
                                           Home_Strength, Away_Strength), 
                   Side2_Strength = ifelse(Home_Team == df_team_info$Team[side_vars$Side1_Row], 
                                           Away_Strength, Home_Strength)) %>% 
            ungroup() %>% 
            mutate(Keep = ifelse((Team == df_team_info$Team[side_vars$Side1_Row] & Side1_Strength > Side2_Strength) | (Team != df_team_info$Team[side_vars$Side1_Row] & Side1_Strength < Side2_Strength), "Yes", "No")) %>% 
            filter(Keep == "Yes")
        } else {
          if(input$rdf_to_strength == "Short-Handed") {
            rdf_df <- rdf_df %>% 
              group_by(Game) %>% 
              mutate(Side1_Strength = ifelse(Home_Team == df_team_info$Team[side_vars$Side1_Row], 
                                             Home_Strength, Away_Strength), 
                     Side2_Strength = ifelse(Home_Team == df_team_info$Team[side_vars$Side1_Row], 
                                             Away_Strength, Home_Strength)) %>% 
              ungroup() %>% 
              mutate(Keep = ifelse((Team == df_team_info$Team[side_vars$Side1_Row] & Side1_Strength < Side2_Strength) | (Team != df_team_info$Team[side_vars$Side1_Row] & Side1_Strength > Side2_Strength), "Yes", "No")) %>% 
              filter(Keep == "Yes")
          }
        }
      }
      
      if(nrow(rdf_df) > 0) {
        if(length(input$rdf_total_avg) > 0) {
          if(input$rdf_total_avg == "Totals") {
            rdf_df <- rdf_df %>% 
              group_by(Team) %>% 
              summarise(Total = length(which(!is.na(Event_Type))))
          } else {
            games <- length(unique(rdf_df$Game))
            rdf_df <- rdf_df %>% 
              group_by(Team) %>% 
              summarise(Total = round(length(which(!is.na(Event_Type))) / games, 1))
          }
        } else {
          rdf_df <- rdf_df %>% 
            group_by(Team) %>% 
            summarise(Total = length(which(!is.na(Event_Type))))
        }
        
        if(length(unique(df_master_fltr()$Team)) > 2) {
          paste(as.numeric(rdf_df$Total[rdf_df$Team != df_team_info$Team[side_vars$Side1_Row]])[[1]], 
                "OPPONENT TURNOVERS", sep = " ")
        } else {
          paste(as.numeric(rdf_df$Total[rdf_df$Team != df_team_info$Team[side_vars$Side1_Row]])[[1]], 
                toupper(df_team_info$Team[side_vars$Side2_Row]), 
                "TURNOVERS", sep = " ")
        }
      } else {
        if(length(unique(df_master_fltr()$Team)) > 2) {
          paste("0", 
                "OPPONENT TURNOVERS", sep = " ")
        } else {
          paste("0", 
                toupper(df_team_info$Team[side_vars$Side2_Row]), 
                "TURNOVERS", sep = " ")
        }
      }
      
    }
  })
  
  ##### RDF: Turnover Bars Side2 #####
  
  output$rdf_to_side2 <- renderPlotly({
    if(length(input$rdf_to_strength) > 0) {
      rdf_df <- df_master_fltr() %>% 
        filter(Event_Type == "Turnover")
      
      if(length(unique(df_master_fltr()$Team)) > 2) {
        rdf_df <- rdf_df %>% 
          mutate(Team = ifelse(Team != df_team_info$Team[side_vars$Side1_Row], "Opponent ", Team))
      }
      
      # Filter down by state input
      if(input$rdf_to_state != "All") {
        rdf_df <- rdf_df %>% 
          filter(State %in% input$rdf_to_state)
      }
      
      # Filter down by strength input
      if(input$rdf_to_strength == "Even-Strength") {
        rdf_df <- rdf_df %>% 
          filter(Home_Strength == Away_Strength)    
      } else {
        if(input$rdf_to_strength == "Man-Advantage") {
          rdf_df <- rdf_df %>% 
            group_by(Game) %>% 
            mutate(Side1_Strength = ifelse(Home_Team == df_team_info$Team[side_vars$Side1_Row], 
                                           Home_Strength, Away_Strength), 
                   Side2_Strength = ifelse(Home_Team == df_team_info$Team[side_vars$Side1_Row], 
                                           Away_Strength, Home_Strength)) %>% 
            ungroup() %>% 
            mutate(Keep = ifelse((Team == df_team_info$Team[side_vars$Side1_Row] & Side1_Strength > Side2_Strength) | (Team != df_team_info$Team[side_vars$Side1_Row] & Side1_Strength < Side2_Strength), "Yes", "No")) %>% 
            filter(Keep == "Yes")
        } else {
          if(input$rdf_to_strength == "Short-Handed") {
            rdf_df <- rdf_df %>% 
              group_by(Game) %>% 
              mutate(Side1_Strength = ifelse(Home_Team == df_team_info$Team[side_vars$Side1_Row], 
                                             Home_Strength, Away_Strength), 
                     Side2_Strength = ifelse(Home_Team == df_team_info$Team[side_vars$Side1_Row], 
                                             Away_Strength, Home_Strength)) %>% 
              ungroup() %>% 
              mutate(Keep = ifelse((Team == df_team_info$Team[side_vars$Side1_Row] & Side1_Strength < Side2_Strength) | (Team != df_team_info$Team[side_vars$Side1_Row] & Side1_Strength > Side2_Strength), "Yes", "No")) %>% 
              filter(Keep == "Yes")
          }
        }
      }
      
      # Filter depending on Totals or Averages
      if(length(input$rdf_total_avg) > 0) {
        if(input$rdf_total_avg == "Totals") {
          rdf_df <- rdf_df %>% 
            group_by(Team) %>% 
            summarise(BallLost = length(which((Turnover_Type == "Ball Lost"))),
                      StrayPass = length(which(Turnover_Type == "Stray Pass")), 
                      InterPass = length(which(Turnover_Type == "Interrupted Pass")), 
                      Concession = length(which(Turnover_Type == "Concession")), 
                      Total = length(which(!is.na(Turnover_Type))))
        } else {
          games <- length(unique(rdf_df$Game))
          rdf_df <- rdf_df %>% 
            group_by(Team) %>% 
            summarise(BallLost = round(length(which((Turnover_Type == "Ball Lost"))) / games, 1),
                      StrayPass = round(length(which((Turnover_Type == "Stray Pass"))) / games, 1), 
                      InterPass = round(length(which((Turnover_Type == "Interrupted Pass"))) / games, 1),
                      Concession = round(length(which((Turnover_Type == "Concession"))) / games, 1), 
                      Total = round(length(which(!is.na(Turnover_Type))) / games, 1))
        }
      } else {
        rdf_df <- rdf_df %>% 
          group_by(Team) %>% 
          summarise(BallLost = length(which((Turnover_Type == "Ball Lost"))),
                    StrayPass = length(which(Turnover_Type == "Stray Pass")), 
                    InterPass = length(which(Turnover_Type == "Interrupted Pass")), 
                    Concession = length(which(Turnover_Type == "Concession")), 
                    Total = length(which(!is.na(Turnover_Type))))
      }
      
      rdf_df <- rdf_df %>% 
        mutate(BallLost_Label = paste("<B>Ball Lost\n", BallLost, " - ", percent((BallLost / Total), accuracy = 0.1), "</B>", sep = ""),
               StrayPass_Label = paste("<B>Stray Pass\n", StrayPass, " - ", percent((StrayPass / Total), accuracy = 0.1), "</B>", sep = ""), 
               InterPass_Label = paste("<B>Interrupted Pass\n", InterPass, " - ", percent((InterPass / Total), accuracy = 0.1), "</B>", sep = ""),
               Concession_Label = paste("<B>Concession\n", Concession, " - ", percent((Concession / Total), accuracy = 0.1), "</B>", sep = ""))
      rdf_df <- rdf_df %>% 
        filter(Team != df_team_info$Team[side_vars$Side1_Row])
      
      plot_ly(rdf_df, type = "bar", x = ~BallLost, y = ~Team, name = "BallLost", orientation = "h", 
              text = ~BallLost_Label, textposition = "inside", insidetextanchor = "middle", textfont = list(size = 15), hoverinfo = "text", 
              marker = list(
                color = df_team_info$Color1[side_vars$Side2_Row], 
                line = list(
                  color = "#000000", 
                  width = 2
                )
              )) %>% 
        add_bars(x = ~StrayPass, y = ~Team, name = "StrayPass", text = ~StrayPass_Label, 
                 marker = list(
                   color = df_team_info$Color2[side_vars$Side2_Row], 
                   line = list(
                     color = "#000000", 
                     width = 2
                   )
                 )) %>% 
        add_bars(x = ~InterPass, y = ~Team, name = "InterPass", text = ~InterPass_Label, 
                 marker = list(
                   color = df_team_info$Color3[side_vars$Side2_Row], 
                   line = list(
                     color = "#000000", 
                     width = 2
                   )
                 )) %>% 
        add_bars(x = ~Concession, y = ~Team, name = "Concession", text = ~Concession_Label, textposition = 'inside', 
                 marker = list(
                   color = df_team_info$Color2[side_vars$Side2_Row], 
                   line = list(
                     color = "#000000", 
                     width = 2
                   )
                 )) %>% 
        layout(
          barmode = "stack", 
          height = 150, 
          plot_bgcolor = "#FFFFFF", 
          hoverlabel = list(
            font = list(
              size = 18
            )
          ), 
          yaxis = list(
            title = "", 
            visible = FALSE, 
            fixedrange = TRUE
          ), 
          xaxis = list(
            visible = FALSE, 
            fixedrange = TRUE
          ), 
          margin = list(
            l = 10, 
            t = 0, 
            b = 0
          )
        ) %>% 
        hide_legend() %>% 
        config(
          modeBarButtonsToRemove = c('toImage', "zoom2d", "toggleSpikelines", "hoverClosestCartesian", 
                                     "hoverCompareCartesian", "drawline", "autoScale2d", "resetScale2d", 
                                     "zoomIn2d", "zoomOut2d", "pan2d", 'select2d', 'lasso2d')
        )  
    }
  })
  
  #### Server - Player Profile (PLY) ####
  
  ##### PLY: Presubmit Screen #####
  
  output$playprof_presubmit <- renderUI({
    if(side_vars$Side1_Row == "none") {
      fluidPage(
        br(), 
        br(), 
        br(), 
        br(), 
        
        wellPanel(
          h1("Press SUBMIT to View"), 
          
          style = "text-align: center; height: 250px; box-shadow: 0px 10px 5px 5px #888888; border-width: thick;"
        )
      )
    } else {
      fluidPage(
        br(), 
        br(), 
        br(), 
        br(), 
        
        uiOutput(outputId = "ply_tab")
      )
    }
  })
  
  ##### PLY: Tab UI #####
  
  output$ply_tab <- renderUI({
    if(length(unique(df_master_fltr()$Game)) > 1) {
      fluidPage(
        fluidRow(
          column(
            width = 8, 
            wellPanel(
              fluidRow(
                column(
                  width = 4, 
                  h3("PLAYER SELECTION"), 
                  uiOutput(outputId = "ply_filter")
                ), 
                column(
                  width = 8, 
                  h3("STRENGTH"), 
                  prettyRadioButtons(inputId = "ply_strength", label = "", 
                                     choices = c("All", "Even-Strength", "Man-Advantage", "Short-Handed"), 
                                     selected = "All", status = "primary", shape = "round", outline = T, 
                                     fill = T, thick = T, animation = "smooth", icon = icon("check"), plain = T, 
                                     bigger = T, inline = T)
                )
              ), 
              style = "box-shadow: 0px 10px 5px 5px #888888; border-width: thick;"
            )
          ), 
          
          column(
            width = 4, 
            wellPanel(
              fluidRow(
                column(
                  width = 12, 
                  h3("TOTAL OR AVG"), 
                  prettyRadioButtons(inputId = "ply_total_avg", label = "", 
                                     choices = c("Totals", "Averages"), 
                                     selected = "Totals", status = "primary", shape = "round", outline = T, 
                                     fill = T, thick = T, animation = "smooth", icon = icon("check"), plain = T, 
                                     bigger = T, inline = T)
                )
              )
            )
          )
        ), 
        
        br(), br(), 
        
        fluidRow(
          column(
            width = 12,
            wellPanel(
              uiOutput(outputId = "ply_player_title"), 
              h4(textOutput(outputId = "ply_games_note"), style = "color: red;"), 
              br(), 
              uiOutput(outputId = "ply_bars")
            )
          )
        ), 
        
        fluidRow(
          column(
            width = 12, 
            uiOutput(outputId = "ply_plots")
          )
        ), 
        
        fluidRow(
          column(
            width = 12, 
            dropdownButton(
              circle = FALSE, 
              size = "lg", 
              label = h3("CLICK INSTRUCTIONS", style = "color: red;"), 
              up = TRUE, 
              h4("Clicking a location on one view will show the shooting at that locaiton in the other views"), 
              h4("Reset Clicks button will reset to all shots selected"), 
              h4("Multiple locations can be selected in each view")
            )
          )
        ), 
        
        fluidRow(
          column(
            width = 12, 
            dataTableOutput(outputId = "ply_timestamps")
          )
        )
      )
    } else {
      fluidPage(
        fluidRow(
          column(
            width = 7, 
            wellPanel(
              fluidRow(
                column(
                  width = 4, 
                  h3("PLAYER SELECTION"), 
                  uiOutput(outputId = "ply_filter")
                ), 
                column(
                  width = 8, 
                  h3("STRENGTH"), 
                  prettyRadioButtons(inputId = "ply_strength", label = "", 
                                     choices = c("All", "Even-Strength", "Man-Advantage", "Short-Handed"), 
                                     selected = "All", status = "primary", shape = "round", outline = T, 
                                     fill = T, thick = T, animation = "smooth", icon = icon("check"), plain = T, 
                                     bigger = T, inline = T)
                )
              ), 
              style = "box-shadow: 0px 10px 5px 5px #888888; border-width: thick;"
            )
          )
        ), 
        
        br(), br(), 
        
        fluidRow(
          column(
            width = 12,
            wellPanel(
              uiOutput(outputId = "ply_player_title"), 
              h4(textOutput(outputId = "ply_games_note"), style = "color: red;"), 
              br(), 
              uiOutput(outputId = "ply_bars")
            )
          )
        ), 
        
        fluidRow(
          column(
            width = 12, 
            uiOutput(outputId = "ply_plots")
          )
        ), 
        
        fluidRow(
          column(
            width = 12, 
            dropdownButton(
              circle = FALSE, 
              size = "lg", 
              label = h3("CLICK INSTRUCTIONS", style = "color: red;"), 
              up = TRUE, 
              h4("Clicking a location on one view will show the shooting at that locaiton in the other views"), 
              h4("Reset Clicks button will reset to all shots selected"), 
              h4("Multiple locations can be selected in each view")
            )
          )
        ), 
        
        fluidRow(
          column(
            width = 12, 
            dataTableOutput(outputId = "ply_timestamps")
          )
        )
      )
    }
  })
  
  ##### PLY: Player Filter #####
  
  output$ply_filter <- renderUI({
    df_player <- player_list()
    
    all_teams <- as.list(unique(df_player$Team))
    names(all_teams) <- unique(df_player$Team)
    for(i in 1:length(all_teams)) {
      all_teams[[i]] <- as.vector(df_player$Player_Name[df_player$Team == names(all_teams)[i]])
    }
    
    pickerInput(inputId = "ply_player", label = "", choices = all_teams, 
                choicesOpt = list(
                  subtext = df_player$Player_Num
                ), 
                options = pickerOptions(
                  liveSearch = T
                ))
  })
  
  ##### PLY: Player Title #####
  
  output$ply_player_title <- renderUI({
    if(length(input$ply_player) > 0) {
      fluidRow(
        div(class = "ply_player_title_logo", 
            img(src = df_team_info$Shadow_Logo_Path[df_team_info$Team == player_list()$Team[player_list()$Player_Name == input$ply_player]], 
                height = "115px", width = "auto")), 
        
        div(class = "ply_player_title_num", 
            style = paste("color: ", df_team_info$Color2[df_team_info$Team == player_list()$Team[player_list()$Player_Name == input$ply_player]], ";", sep = ""), 
            p(paste(player_list()$Player_Num[player_list()$Player_Name == input$ply_player]))), 
        
        div(class = "ply_player_title_name", 
            style = paste("color: ", df_team_info$Color1[df_team_info$Team == player_list()$Team[player_list()$Player_Name == input$ply_player]], ";", sep = ""), 
            p(paste(str_to_upper(player_list()$LastName[player_list()$Player_Name == input$ply_player]))))
      )
    }
  })
  
  ##### PLY: Games Played Note #####
  
  output$ply_games_note <- renderText({
    if(length(input$ply_player) > 0) {
      if(lastest_submit$Current_Submission != "Ind Game") {
        
        gp_df <- df_master_fltr()
        gp1 <- gp_df %>% filter(Player1_Name == input$ply_player)
        gp2 <- gp_df %>% filter(Primary_Assist_Name == input$ply_player)
        gp3 <- gp_df %>% filter(Secondary_Assist_Name == input$ply_player)
        gp4 <- gp_df %>% filter(Rebound_Name == input$ply_player)
        gp5 <- gp_df %>% filter(FO_Home_Name == input$ply_player)
        gp6 <- gp_df %>% filter(FO_Away_Name == input$ply_player)
        gp7 <- gp_df %>% filter(Home_Goalie_Name == input$ply_player)
        gp8 <- gp_df %>% filter(Away_Goalie_Name == input$ply_player)
        games_played <- length(unique(c(gp1$Game, gp2$Game, gp3$Game, gp4$Game, gp5$Game, gp6$Game, gp7$Game, gp8$Game)))
        paste("* Stats Compiled from ", as.integer(games_played), " Tracked Appearances *", sep = "")
      }
    }
  })
  
  ##### PLY: Bars UI #####
  
  output$ply_bars <- renderUI({
    if(length(input$ply_player) > 0) {
      if((input$ply_player %in% df_master_fltr()$Home_Goalie_Name) | (input$ply_player %in% df_master_fltr()$Away_Goalie_Name)) {
        fluidPage(
          fluidRow(
            column(
              width = 6, 
              h2(textOutput(outputId = "ply_goalie_profile_title"), style = "color: black;"), 
              plotlyOutput(outputId = "ply_goalie_profile", height = "160px"), 
              plotlyOutput(outputId = "ply_goalie_profile_poss", height = "160px"), 
              plotlyOutput(outputId = "ply_goalie_profile_create", height = "160px")
            ), 
            column(
              width = 6, 
              h2(textOutput(outputId = "ply_goalie_saveper_title"), style = "color: black;"), 
              plotlyOutput(outputId = "ply_goalie_saveper", height = "160px"), 
              h2(textOutput(outputId = "ply_goalie_control_title"), style = "color: black;"), 
              plotlyOutput(outputId = "ply_goalie_control", height = "160px")
            )
          )
        )
      } else {
        fluidPage(
          fluidRow(
            column(
              width = 6, 
              h2(textOutput(outputId = "ply_shotcont_title"), style = "color: black;"), 
              plotlyOutput(outputId = "ply_shotcont", height = "160px"), 
              h2(textOutput(outputId = "ply_reb_title"), style = "color: black;"), 
              plotlyOutput(outputId = "ply_reb", height = "160px"), 
              h2(textOutput(outputId = "ply_to_title"), style = "color: black;"), 
              plotlyOutput(outputId = "ply_to", height = "160px")
            ), 
            column(
              width = 6, 
              h2(textOutput(outputId = "ply_shooting_profile_title"), style = "color: black;"), 
              plotlyOutput(outputId = "ply_shooting_profile", height = "160px"), 
              plotlyOutput(outputId = "ply_shooting_profile_poss", height = "160px"), 
              plotlyOutput(outputId = "ply_shooting_profile_create", height = "160px")
            )
          )
        )
      }
    }
  })
  
  ##### PLY: Plots UI #####
  
  output$ply_plots <- renderUI({
    if(length(input$ply_player) > 0) {
      if((input$ply_player %in% df_master_fltr()$Home_Goalie_Name) | (input$ply_player %in% df_master_fltr()$Away_Goalie_Name)) {
        fluidRow(
          fluidRow(
            column(
              width = 4, offset = 4, 
              actionButton(
                inputId = "ply_resetclicks_bttn", label = "RESET LOCATION CLICKS", width = "auto", 
                style = "color: #ff1400; border-color: #ff1400;"
              )
            )
          ), 
          
          br(), br(), 
          
          column(
            width = 6, 
            wellPanel(
              h2(textOutput(outputId = "ply_goal_title"), style = "color: black;"), 
              plotlyOutput(outputId = "ply_goal", width = "100%", height = "500px")
            ), 
            
            wellPanel(
              h2(textOutput(outputId = "ply_shotangle_title"), style = "color: black;"), 
              plotlyOutput(outputId = "ply_shotangle", width = "100%", height = "600px")
            )
          ), 
          
          column(
            width = 6, 
            wellPanel(
              h2(textOutput(outputId = "ply_field_title"), style = "color: black;"), 
              plotlyOutput(outputId = "ply_field", width = "100%", height = "850px")
            )
          )
        )
      } else {
        fluidRow(
          br(), 
          
          fluidRow(
            column(
              width = 4, offset = 4, 
              actionButton(
                inputId = "ply_resetclicks_bttn", label = "RESET LOCATION CLICKS", width = "auto", 
                style = "color: #ff1400; border-color: #ff1400;"
              )
            )
          ), 
          
          br(), br(), 
          
          column(
            width = 6, 
            wellPanel(
              h2(textOutput(outputId = "ply_goal_title"), style = "color: black;"), 
              plotlyOutput(outputId = "ply_goal", width = "100%", height = "500px")
            ), 
            
            wellPanel(
              h2(textOutput(outputId = "ply_shotangle_title"), style = "color: black;"), 
              plotlyOutput(outputId = "ply_shotangle", width = "100%", height = "600px")
            )
          ), 
          
          column(
            width = 6, 
            wellPanel(
              h2(textOutput(outputId = "ply_field_title"), style = "color: black;"), 
              plotlyOutput(outputId = "ply_field", width = "100%", height = "850px")
            )
          )
        )
      }
    }
  })
  
  ##### PLY: Shot Contributions Title #####
  
  output$ply_shotcont_title <- renderText({
    if(length(input$ply_strength) > 0) {
      shot_df <- df_master_fltr() %>% 
        filter(Event_Type == "Shot" & (Player1_Name == input$ply_player | Primary_Assist_Name == input$ply_player | Secondary_Assist_Name == input$ply_player))
      
      # Filter down by strength input
      if(input$ply_strength == "Even-Strength") {
        shot_df <- shot_df %>% 
          filter(Home_Strength == Away_Strength)    
      } else {
        if(input$ply_strength == "Man-Advantage") {
          shot_df <- shot_df %>% 
            group_by(Game) %>% 
            mutate(Side1_Strength = ifelse(Home_Team == player_list()$Team[player_list()$Player_Name == input$ply_player], 
                                           Home_Strength, Away_Strength), 
                   Side2_Strength = ifelse(Home_Team == player_list()$Team[player_list()$Player_Name == input$ply_player], 
                                           Away_Strength, Home_Strength)) %>% 
            ungroup() %>% 
            mutate(Keep = ifelse((Team == player_list()$Team[player_list()$Player_Name == input$ply_player] & Side1_Strength > Side2_Strength) | (Team != player_list()$Team[player_list()$Player_Name == input$ply_player] & Side1_Strength < Side2_Strength), "Yes", "No")) %>% 
            filter(Keep == "Yes")
        } else {
          if(input$ply_strength == "Short-Handed") {
            shot_df <- shot_df %>% 
              group_by(Game) %>% 
              mutate(Side1_Strength = ifelse(Home_Team == player_list()$Team[player_list()$Player_Name == input$ply_player], 
                                             Home_Strength, Away_Strength), 
                     Side2_Strength = ifelse(Home_Team == player_list()$Team[player_list()$Player_Name == input$ply_player], 
                                             Away_Strength, Home_Strength)) %>% 
              ungroup() %>% 
              mutate(Keep = ifelse((Team == player_list()$Team[player_list()$Player_Name == input$ply_player] & Side1_Strength < Side2_Strength) | (Team != player_list()$Team[player_list()$Player_Name == input$ply_player] & Side1_Strength > Side2_Strength), "Yes", "No")) %>% 
              filter(Keep == "Yes")
          }
        }
      }
      
      if(length(input$ply_total_avg) > 0) {
        if(input$ply_total_avg == "Totals") {
          shot_total <- nrow(shot_df[!is.na(shot_df$Result), ])
        } else {
          shot_total <- nrow(shot_df[!is.na(shot_df$Result), ]) / length(unique(shot_df$Game))
        }
      } else {
        shot_total <- nrow(shot_df[!is.na(shot_df$Result), ])
      }
      
      paste(as.numeric(shot_total)[[1]], "SHOT CONTRIBUTIONS", sep = " ")
    }
  })
  
  ##### PLY: Shot Contributions Bars #####
  
  output$ply_shotcont <- renderPlotly({
    if(length(input$ply_strength) > 0) {
      
      shot_df <- df_master_fltr() %>% 
        filter(Event_Type == "Shot" & (Player1_Name == input$ply_player | Primary_Assist_Name == input$ply_player | Secondary_Assist_Name == input$ply_player))
      
      # Filter down by strength input
      if(input$ply_strength == "Even-Strength") {
        shot_df <- shot_df %>% 
          filter(Home_Strength == Away_Strength)    
      } else {
        if(input$ply_strength == "Man-Advantage") {
          shot_df <- shot_df %>% 
            group_by(Game) %>% 
            mutate(Side1_Strength = ifelse(Home_Team == player_list()$Team[player_list()$Player_Name == input$ply_player], 
                                           Home_Strength, Away_Strength), 
                   Side2_Strength = ifelse(Home_Team == player_list()$Team[player_list()$Player_Name == input$ply_player], 
                                           Away_Strength, Home_Strength)) %>% 
            ungroup() %>% 
            mutate(Keep = ifelse((Team == player_list()$Team[player_list()$Player_Name == input$ply_player] & Side1_Strength > Side2_Strength) | (Team != player_list()$Team[player_list()$Player_Name == input$ply_player] & Side1_Strength < Side2_Strength), "Yes", "No")) %>% 
            filter(Keep == "Yes")
        } else {
          if(input$ply_strength == "Short-Handed") {
            shot_df <- shot_df %>% 
              group_by(Game) %>% 
              mutate(Side1_Strength = ifelse(Home_Team == player_list()$Team[player_list()$Player_Name == input$ply_player], 
                                             Home_Strength, Away_Strength), 
                     Side2_Strength = ifelse(Home_Team == player_list()$Team[player_list()$Player_Name == input$ply_player], 
                                             Away_Strength, Home_Strength)) %>% 
              ungroup() %>% 
              mutate(Keep = ifelse((Team == player_list()$Team[player_list()$Player_Name == input$ply_player] & Side1_Strength < Side2_Strength) | (Team != player_list()$Team[player_list()$Player_Name == input$ply_player] & Side1_Strength > Side2_Strength), "Yes", "No")) %>% 
              filter(Keep == "Yes")
          }
        }
      }
      
      if(length(input$ply_total_avg) > 0) {
        if(input$ply_total_avg == "Totals") {
          shot_df <- shot_df %>% 
            group_by(Team) %>% 
            summarise(Shots = length(which((Player1_Name == input$ply_player))),
                      PrimAst = length(which(Primary_Assist_Name == input$ply_player)),
                      SecAst = length(which(Secondary_Assist_Name == input$ply_player)),
                      Total = length(which(!is.na(Result))))
        } else {
          games <- length(unique(shot_df$Game))
          shot_df <- shot_df %>% 
            group_by(Team) %>% 
            summarise(Shots = round(length(which((Player1_Name == input$ply_player))) / games, 1),
                      PrimAst = round(length(which(Primary_Assist_Name == input$ply_player)) / games, 1),
                      SecAst = round(length(which(Secondary_Assist_Name == input$ply_player)) / games, 1),
                      Total = round(length(which(!is.na(Result))) / games, 1))
        }
      } else {
        shot_df <- shot_df %>% 
          group_by(Team) %>% 
          summarise(Shots = length(which((Player1_Name == input$ply_player))),
                    PrimAst = length(which(Primary_Assist_Name == input$ply_player)),
                    SecAst = length(which(Secondary_Assist_Name == input$ply_player)),
                    Total = length(which(!is.na(Result))))
      }
      
      shot_df <- shot_df %>% 
        mutate(Shots_Label = paste("Shot Attempts\n", Shots, " - ", percent((Shots / Total), accuracy = 0.1), sep = ""),
               PrimAst_Label = paste("Primary Shot Assists\n", PrimAst, " - ", percent((PrimAst / Total), accuracy = 0.1), sep = ""),
               SecAst_Label = paste("Secondary Shot Assists\n", SecAst, " - ", percent((SecAst / Total), accuracy = 0.1), sep = ""))
      
      plot_ly(shot_df, type = "bar", x = ~Shots, y = ~Team, name = "Shots", orientation = "h", 
              text = ~Shots_Label, textposition = "inside", insidetextanchor = "middle", textfont = list(size = 15), hoverinfo = "text", 
              marker = list(
                color = df_team_info$Color1[which(df_team_info$Team == player_list()$Team[player_list()$Player_Name == input$ply_player])], 
                line = list(
                  color = "#000000", 
                  width = 2
                )
              )) %>% 
        add_bars(x = ~PrimAst, y = ~Team, name = "PrimAst", text = ~PrimAst_Label, textposition = 'inside',  
                 marker = list(
                   color = df_team_info$Color2[which(df_team_info$Team == player_list()$Team[player_list()$Player_Name == input$ply_player])], 
                   line = list(
                     color = "#000000", 
                     width = 2
                   )
                 )) %>% 
        add_bars(x = ~SecAst, y = ~Team, name = "SecAst", text = ~SecAst_Label, textposition = 'inside', 
                 marker = list(
                   color = df_team_info$Color3[which(df_team_info$Team == player_list()$Team[player_list()$Player_Name == input$ply_player])], 
                   line = list(
                     color = "#000000", 
                     width = 2
                   )
                 )) %>% 
        layout(
          barmode = "stack", 
          height = 150, 
          plot_bgcolor = "#FFFFFF", 
          hoverlabel = list(
            font = list(
              size = 18
            )
          ), 
          yaxis = list(
            title = "", 
            side = "right", 
            visible = FALSE, 
            fixedrange = TRUE
          ), 
          xaxis = list(
            autorange = "reversed", 
            visible = FALSE, 
            fixedrange = TRUE
          ), 
          margin = list(
            r = 95, 
            t = 0, 
            b = 0
          )
        ) %>% 
        hide_legend() %>% 
        config(
          modeBarButtonsToRemove = c('toImage', "zoom2d", "toggleSpikelines", "hoverClosestCartesian", 
                                     "hoverCompareCartesian", "drawline", "autoScale2d", "resetScale2d", 
                                     "zoomIn2d", "zoomOut2d", "pan2d", 'select2d', 'lasso2d')
        )  
    }
  })
  
  ##### PLY: Shooting Bars Title #####
  
  output$ply_shooting_profile_title <- renderText({
    if(length(input$ply_strength) > 0) {
      shot_df <- df_master_fltr() %>% 
        filter(Event_Type == "Shot" & Player1_Name == input$ply_player)
      
      # Filter down by strength input
      if(input$ply_strength == "Even-Strength") {
        shot_df <- shot_df %>% 
          filter(Home_Strength == Away_Strength)    
      } else {
        if(input$ply_strength == "Man-Advantage") {
          shot_df <- shot_df %>% 
            group_by(Game) %>% 
            mutate(Side1_Strength = ifelse(Home_Team == player_list()$Team[player_list()$Player_Name == input$ply_player], 
                                           Home_Strength, Away_Strength), 
                   Side2_Strength = ifelse(Home_Team == player_list()$Team[player_list()$Player_Name == input$ply_player], 
                                           Away_Strength, Home_Strength)) %>% 
            ungroup() %>% 
            mutate(Keep = ifelse((Team == player_list()$Team[player_list()$Player_Name == input$ply_player] & Side1_Strength > Side2_Strength) | (Team != player_list()$Team[player_list()$Player_Name == input$ply_player] & Side1_Strength < Side2_Strength), "Yes", "No")) %>% 
            filter(Keep == "Yes")
        } else {
          if(input$ply_strength == "Short-Handed") {
            shot_df <- shot_df %>% 
              group_by(Game) %>% 
              mutate(Side1_Strength = ifelse(Home_Team == player_list()$Team[player_list()$Player_Name == input$ply_player], 
                                             Home_Strength, Away_Strength), 
                     Side2_Strength = ifelse(Home_Team == player_list()$Team[player_list()$Player_Name == input$ply_player], 
                                             Away_Strength, Home_Strength)) %>% 
              ungroup() %>% 
              mutate(Keep = ifelse((Team == player_list()$Team[player_list()$Player_Name == input$ply_player] & Side1_Strength < Side2_Strength) | (Team != player_list()$Team[player_list()$Player_Name == input$ply_player] & Side1_Strength > Side2_Strength), "Yes", "No")) %>% 
              filter(Keep == "Yes")
          }
        }
      }
      
      if(length(input$ply_total_avg) > 0) {
        if(input$ply_total_avg == "Totals") {
          shot_total <- nrow(shot_df[!is.na(shot_df$Result), ])
        } else {
          shot_total <- nrow(shot_df[!is.na(shot_df$Result), ]) / length(unique(shot_df$Game))
        }
      } else {
        shot_total <- nrow(shot_df[!is.na(shot_df$Result), ])
      }
      
      paste(as.numeric(shot_total)[[1]], "SHOTS", sep = " ")
    }
  })
  
  ##### PLY: Shooting Bars #####
  
  output$ply_shooting_profile <- renderPlotly({
    if(length(input$ply_strength) > 0) {
      
      shot_df <- df_master_fltr() %>% 
        filter(Event_Type == "Shot" & Player1_Name == input$ply_player)
      
      # Filter down by strength input
      if(input$ply_strength == "Even-Strength") {
        shot_df <- shot_df %>% 
          filter(Home_Strength == Away_Strength)    
      } else {
        if(input$ply_strength == "Man-Advantage") {
          shot_df <- shot_df %>% 
            group_by(Game) %>% 
            mutate(Side1_Strength = ifelse(Home_Team == player_list()$Team[player_list()$Player_Name == input$ply_player], 
                                           Home_Strength, Away_Strength), 
                   Side2_Strength = ifelse(Home_Team == player_list()$Team[player_list()$Player_Name == input$ply_player], 
                                           Away_Strength, Home_Strength)) %>% 
            ungroup() %>% 
            mutate(Keep = ifelse((Team == player_list()$Team[player_list()$Player_Name == input$ply_player] & Side1_Strength > Side2_Strength) | (Team != player_list()$Team[player_list()$Player_Name == input$ply_player] & Side1_Strength < Side2_Strength), "Yes", "No")) %>% 
            filter(Keep == "Yes")
        } else {
          if(input$ply_strength == "Short-Handed") {
            shot_df <- shot_df %>% 
              group_by(Game) %>% 
              mutate(Side1_Strength = ifelse(Home_Team == player_list()$Team[player_list()$Player_Name == input$ply_player], 
                                             Home_Strength, Away_Strength), 
                     Side2_Strength = ifelse(Home_Team == player_list()$Team[player_list()$Player_Name == input$ply_player], 
                                             Away_Strength, Home_Strength)) %>% 
              ungroup() %>% 
              mutate(Keep = ifelse((Team == player_list()$Team[player_list()$Player_Name == input$ply_player] & Side1_Strength < Side2_Strength) | (Team != player_list()$Team[player_list()$Player_Name == input$ply_player] & Side1_Strength > Side2_Strength), "Yes", "No")) %>% 
              filter(Keep == "Yes")
          }
        }
      }
      
      if(length(input$ply_total_avg) > 0) {
        if(input$ply_total_avg == "Totals") {
          shot_df <- shot_df %>% 
            group_by(Team) %>% 
            summarise(Goal = length(which((Result == "Goal"))),
                      Saved = length(which(Result == "Miss" & Net_Location %in% shot_on)),
                      Off = length(which(Result == "Miss" & Net_Location %in% shot_off)),
                      Post = length(which(Result == "Miss" & Net_Location %in% shot_post)),
                      Blocked = length(which(Result == "Blocked")), 
                      Total = length(which(!is.na(Result))))
        } else {
          games <- length(unique(shot_df$Game))
          shot_df <- shot_df %>% 
            group_by(Team) %>% 
            summarise(Goal = round(length(which((Result == "Goal"))) / games, 1),
                      Saved = round(length(which(Result == "Miss" & Net_Location %in% shot_on)) / games, 1),
                      Off = round(length(which(Result == "Miss" & Net_Location %in% shot_off)) / games, 1),
                      Post = round(length(which(Result == "Miss" & Net_Location %in% shot_post)) / games, 1),
                      Blocked = round(length(which(Result == "Blocked")) / games, 1), 
                      Total = round(length(which(!is.na(Result))) / games, 1))
        }
      } else {
        shot_df <- shot_df %>% 
          group_by(Team) %>% 
          summarise(Goal = length(which((Result == "Goal"))),
                    Saved = length(which(Result == "Miss" & Net_Location %in% shot_on)),
                    Off = length(which(Result == "Miss" & Net_Location %in% shot_off)),
                    Post = length(which(Result == "Miss" & Net_Location %in% shot_post)),
                    Blocked = length(which(Result == "Blocked")), 
                    Total = length(which(!is.na(Result))))
      }
      
      shot_df <- shot_df %>% 
        mutate(Goal_Label = paste("Goal\n", Goal, " - ", percent((Goal / Total), accuracy = 0.1), sep = ""),
               Saved_Label = paste("Saved\n", Saved, " - ", percent((Saved / Total), accuracy = 0.1), sep = ""),
               Off_Label = paste("Off\n", Off, " - ", percent((Off / Total), accuracy = 0.1), sep = ""),
               Post_Label = paste("Post\n", Post, " - ", percent((Post / Total), accuracy = 0.1), sep = ""),
               Blocked_Label = paste("Blocked\n", Blocked, " - ", percent((Blocked / Total), accuracy = 0.1), sep = ""), 
               Total_Label = paste("Total\n", Total, sep = ""))
      
      shot_df <- shot_df %>% 
        filter(Team == player_list()$Team[player_list()$Player_Name == input$ply_player])
      
      plot_ly(shot_df, type = "bar", x = ~Goal, y = ~Team, name = "Goals", orientation = "h", 
              text = ~Goal_Label, textposition = "inside", insidetextanchor = "middle", textfont = list(size = 15), hoverinfo = "text", 
              marker = list(
                color = df_team_info$Color1[which(df_team_info$Team == player_list()$Team[player_list()$Player_Name == input$ply_player])], 
                line = list(
                  color = "#000000", 
                  width = 2
                )
              )) %>% 
        add_bars(x = ~Saved, y = ~Team, name = "Saved Shots", text = ~Saved_Label, textposition = 'inside',  
                 marker = list(
                   color = df_team_info$Color2[which(df_team_info$Team == player_list()$Team[player_list()$Player_Name == input$ply_player])], 
                   line = list(
                     color = "#000000", 
                     width = 2
                   )
                 )) %>% 
        add_bars(x = ~Off, y = ~Team, name = "Off Shots", text = ~Off_Label, textposition = 'inside', 
                 marker = list(
                   color = df_team_info$Color3[which(df_team_info$Team == player_list()$Team[player_list()$Player_Name == input$ply_player])], 
                   line = list(
                     color = "#000000", 
                     width = 2
                   )
                 )) %>% 
        add_bars(x = ~Post, y = ~Team, name = "Post Shots", text = ~Post_Label, textposition = 'inside', 
                 marker = list(
                   color = df_team_info$Color1[which(df_team_info$Team == player_list()$Team[player_list()$Player_Name == input$ply_player])], 
                   line = list(
                     color = "#000000", 
                     width = 2
                   )
                 )) %>% 
        add_bars(x = ~Blocked, y = ~Team, name = "Blocked Shots", text = ~Blocked_Label, textposition = 'inside', 
                 marker = list(
                   color = df_team_info$Color2[which(df_team_info$Team == player_list()$Team[player_list()$Player_Name == input$ply_player])], 
                   line = list(
                     color = "#000000", 
                     width = 2
                   )
                 )) %>% 
        layout(
          barmode = "stack", 
          height = 150, 
          plot_bgcolor = "#FFFFFF", 
          hoverlabel = list(
            font = list(
              size = 18
            )
          ), 
          yaxis = list(
            title = "", 
            side = "right", 
            visible = FALSE, 
            fixedrange = TRUE
          ), 
          xaxis = list(
            autorange = "reversed", 
            visible = FALSE, 
            fixedrange = TRUE
          ), 
          margin = list(
            r = 95, 
            t = 0, 
            b = 0
          )
        ) %>% 
        hide_legend() %>% 
        config(
          modeBarButtonsToRemove = c('toImage', "zoom2d", "toggleSpikelines", "hoverClosestCartesian", 
                                     "hoverCompareCartesian", "drawline", "autoScale2d", "resetScale2d", 
                                     "zoomIn2d", "zoomOut2d", "pan2d", 'select2d', 'lasso2d')
        )  
    }
  })
  
  ##### PLY: Possession Bars #####
  
  output$ply_shooting_profile_poss <- renderPlotly({
    if(length(input$ply_strength) > 0) {
      
      shot_df <- df_master_fltr() %>% 
        filter(Event_Type == "Shot" & Player1_Name == input$ply_player)
      
      # Filter down by strength input
      if(input$ply_strength == "Even-Strength") {
        shot_df <- shot_df %>% 
          filter(Home_Strength == Away_Strength)    
      } else {
        if(input$ply_strength == "Man-Advantage") {
          shot_df <- shot_df %>% 
            group_by(Game) %>% 
            mutate(Side1_Strength = ifelse(Home_Team == player_list()$Team[player_list()$Player_Name == input$ply_player], 
                                           Home_Strength, Away_Strength), 
                   Side2_Strength = ifelse(Home_Team == player_list()$Team[player_list()$Player_Name == input$ply_player], 
                                           Away_Strength, Home_Strength)) %>% 
            ungroup() %>% 
            mutate(Keep = ifelse((Team == player_list()$Team[player_list()$Player_Name == input$ply_player] & Side1_Strength > Side2_Strength) | (Team != player_list()$Team[player_list()$Player_Name == input$ply_player] & Side1_Strength < Side2_Strength), "Yes", "No")) %>% 
            filter(Keep == "Yes")
        } else {
          if(input$ply_strength == "Short-Handed") {
            shot_df <- shot_df %>% 
              group_by(Game) %>% 
              mutate(Side1_Strength = ifelse(Home_Team == player_list()$Team[player_list()$Player_Name == input$ply_player], 
                                             Home_Strength, Away_Strength), 
                     Side2_Strength = ifelse(Home_Team == player_list()$Team[player_list()$Player_Name == input$ply_player], 
                                             Away_Strength, Home_Strength)) %>% 
              ungroup() %>% 
              mutate(Keep = ifelse((Team == player_list()$Team[player_list()$Player_Name == input$ply_player] & Side1_Strength < Side2_Strength) | (Team != player_list()$Team[player_list()$Player_Name == input$ply_player] & Side1_Strength > Side2_Strength), "Yes", "No")) %>% 
              filter(Keep == "Yes")
          }
        }
      }
      
      # Filter depending on Totals or Averages
      if(length(input$ply_total_avg) > 0) {
        if(input$ply_total_avg == "Totals") {
          shot_df <- shot_df %>% 
            filter(Event_Type == "Shot") %>% 
            group_by(Team) %>% 
            summarise(Set = length(which((State == "Set"))),
                      Transition = length(which(State == "Transition")), 
                      Total = length(which(!is.na(State))))
        } else {
          games <- length(unique(shot_df$Game))
          shot_df <- shot_df %>% 
            filter(Event_Type == "Shot") %>% 
            group_by(Team) %>% 
            summarise(Set = round(length(which((State == "Set"))) / games, 1),
                      Transition = round(length(which((State == "Transition"))) / games, 1), 
                      Total = round(length(which(!is.na(State))) / games, 1))
        }
      } else {
        shot_df <- shot_df %>% 
          filter(Event_Type == "Shot") %>% 
          group_by(Team) %>% 
          summarise(Set = length(which((State == "Set"))),
                    Transition = length(which(State == "Transition")), 
                    Total = length(which(!is.na(State))))
      }
      
      shot_df <- shot_df %>% 
        mutate(Set_Label = paste("Set\n", Set, " - ", percent((Set / Total), accuracy = 0.1), sep = ""),
               Transition_Label = paste("Transition\n", Transition, " - ", percent((Transition / Total), accuracy = 0.1), sep = ""))
      
      shot_df <- shot_df %>% 
        filter(Team == player_list()$Team[player_list()$Player_Name == input$ply_player])
      
      plot_ly(shot_df, type = "bar", x = ~Transition, y = ~Team, name = "Transition", orientation = "h", 
              text = ~Transition_Label, textposition = "inside", insidetextanchor = "middle", textfont = list(size = 15), hoverinfo = "text", 
              marker = list(
                color = df_team_info$Color1[which(df_team_info$Team == player_list()$Team[player_list()$Player_Name == input$ply_player])], 
                line = list(
                  color = "#000000", 
                  width = 2
                )
              )) %>% 
        add_bars(x = ~Set, y = ~Team, name = "Set", text = ~Set_Label, textposition = 'inside',  cliponaxis = FALSE, 
                 marker = list(
                   color = df_team_info$Color2[which(df_team_info$Team == player_list()$Team[player_list()$Player_Name == input$ply_player])], 
                   line = list(
                     color = "#000000", 
                     width = 2
                   )
                 )) %>% 
        layout(
          barmode = "stack", 
          height = 150, 
          plot_bgcolor = "#FFFFFF", 
          hoverlabel = list(
            font = list(
              size = 18
            )
          ), 
          yaxis = list(
            title = "", 
            side = "right", 
            visible = FALSE, 
            fixedrange = TRUE
          ), 
          xaxis = list(
            autorange = "reversed", 
            visible = FALSE, 
            fixedrange = TRUE
          ), 
          margin = list(
            r = 95, 
            t = 0, 
            b = 0
          )
        ) %>% 
        hide_legend() %>% 
        config(
          modeBarButtonsToRemove = c('toImage', "zoom2d", "toggleSpikelines", "hoverClosestCartesian", 
                                     "hoverCompareCartesian", "drawline", "autoScale2d", "resetScale2d", 
                                     "zoomIn2d", "zoomOut2d", "pan2d", 'select2d', 'lasso2d')
        )  
    }
  })
  
  ##### PLY: Creation Bars #####
  
  output$ply_shooting_profile_create <- renderPlotly({
    if(length(input$ply_strength) > 0) {
      
      shot_df <- df_master_fltr() %>% 
        filter(Event_Type == "Shot" & Player1_Name == input$ply_player)
      
      # Filter down by strength input
      if(input$ply_strength == "Even-Strength") {
        shot_df <- shot_df %>% 
          filter(Home_Strength == Away_Strength)    
      } else {
        if(input$ply_strength == "Man-Advantage") {
          shot_df <- shot_df %>% 
            group_by(Game) %>% 
            mutate(Side1_Strength = ifelse(Home_Team == player_list()$Team[player_list()$Player_Name == input$ply_player], 
                                           Home_Strength, Away_Strength), 
                   Side2_Strength = ifelse(Home_Team == player_list()$Team[player_list()$Player_Name == input$ply_player], 
                                           Away_Strength, Home_Strength)) %>% 
            ungroup() %>% 
            mutate(Keep = ifelse((Team == player_list()$Team[player_list()$Player_Name == input$ply_player] & Side1_Strength > Side2_Strength) | (Team != player_list()$Team[player_list()$Player_Name == input$ply_player] & Side1_Strength < Side2_Strength), "Yes", "No")) %>% 
            filter(Keep == "Yes")
        } else {
          if(input$ply_strength == "Short-Handed") {
            shot_df <- shot_df %>% 
              group_by(Game) %>% 
              mutate(Side1_Strength = ifelse(Home_Team == player_list()$Team[player_list()$Player_Name == input$ply_player], 
                                             Home_Strength, Away_Strength), 
                     Side2_Strength = ifelse(Home_Team == player_list()$Team[player_list()$Player_Name == input$ply_player], 
                                             Away_Strength, Home_Strength)) %>% 
              ungroup() %>% 
              mutate(Keep = ifelse((Team == player_list()$Team[player_list()$Player_Name == input$ply_player] & Side1_Strength < Side2_Strength) | (Team != player_list()$Team[player_list()$Player_Name == input$ply_player] & Side1_Strength > Side2_Strength), "Yes", "No")) %>% 
              filter(Keep == "Yes")
          }
        }
      }
      
      # Filter depending on Totals or Averages
      if(length(input$ply_total_avg) > 0) {
        if(input$ply_total_avg == "Totals") {
          shot_df <- shot_df %>% 
            group_by(Team) %>% 
            summarise(Movement = length(which((Shot_Type == "Movement"))),
                      Assisted = length(which(Shot_Type == "Assisted")), 
                      Rebound = length(which(Shot_Type == "Rebound")), 
                      Total = length(which(!is.na(Shot_Type))))
        } else {
          games <- length(unique(shot_df$Game))
          shot_df <- shot_df %>% 
            filter(Event_Type == "Shot") %>% 
            group_by(Team) %>% 
            summarise(Movement = round(length(which((Shot_Type == "Movement"))) / games, 1),
                      Assisted = round(length(which((Shot_Type == "Assisted"))) / games, 1), 
                      Rebound = round(length(which((Shot_Type == "Rebound"))) / games, 1), 
                      Total = round(length(which(!is.na(Shot_Type))) / games, 1))
        }
      } else {
        shot_df <- shot_df %>% 
          group_by(Team) %>% 
          summarise(Movement = length(which((Shot_Type == "Movement"))),
                    Assisted = length(which(Shot_Type == "Assisted")), 
                    Rebound = length(which(Shot_Type == "Rebound")), 
                    Total = length(which(!is.na(Shot_Type))))
      }
      
      shot_df <- shot_df %>% 
        mutate(Movement_Label = paste("Movement\n", Movement, " - ", percent((Movement / Total), accuracy = 0.1), sep = ""),
               Assisted_Label = paste("Assisted\n", Assisted, " - ", percent((Assisted / Total), accuracy = 0.1), sep = ""), 
               Rebound_Label = paste("Offensive Rebound\n", Rebound, " - ", percent((Rebound / Total), accuracy = 0.1), sep = ""))
      
      shot_df <- shot_df %>% 
        filter(Team == player_list()$Team[player_list()$Player_Name == input$ply_player])
      
      plot_ly(shot_df, type = "bar", x = ~Rebound, y = ~Team, name = "Rebound", orientation = "h", 
              text = ~Rebound_Label, textposition = "inside", insidetextanchor = "middle", textfont = list(size = 15), hoverinfo = "text", 
              marker = list(
                color = df_team_info$Color3[which(df_team_info$Team == player_list()$Team[player_list()$Player_Name == input$ply_player])], 
                line = list(
                  color = "#000000", 
                  width = 2
                )
              )) %>% 
        add_bars(x = ~Movement, y = ~Team, name = "Movement", text = ~Movement_Label, textposition = 'inside',  
                 marker = list(
                   color = df_team_info$Color2[which(df_team_info$Team == player_list()$Team[player_list()$Player_Name == input$ply_player])], 
                   line = list(
                     color = "#000000", 
                     width = 2
                   )
                 )) %>% 
        add_bars(x = ~Assisted, y = ~Team, name = "Assisted", text = ~Assisted_Label, textposition = 'inside', 
                 marker = list(
                   color = df_team_info$Color1[which(df_team_info$Team == player_list()$Team[player_list()$Player_Name == input$ply_player])], 
                   line = list(
                     color = "#000000", 
                     width = 2
                   )
                 )) %>% 
        layout(
          barmode = "stack", 
          height = 150, 
          plot_bgcolor = "#FFFFFF", 
          hoverlabel = list(
            font = list(
              size = 18
            )
          ), 
          yaxis = list(
            title = "", 
            side = "right", 
            visible = FALSE, 
            fixedrange = TRUE
          ), 
          xaxis = list(
            autorange = "reversed", 
            visible = FALSE, 
            fixedrange = TRUE
          ), 
          margin = list(
            r = 95, 
            t = 0, 
            b = 0
          )
        ) %>% 
        hide_legend() %>% 
        config(
          modeBarButtonsToRemove = c('toImage', "zoom2d", "toggleSpikelines", "hoverClosestCartesian", 
                                     "hoverCompareCartesian", "drawline", "autoScale2d", "resetScale2d", 
                                     "zoomIn2d", "zoomOut2d", "pan2d", 'select2d', 'lasso2d')
        )  
    }
  })
  
  ##### PLY: Rebound Bars Title #####
  
  output$ply_reb_title <- renderText({
    if(length(input$ply_strength) > 0) {
      shot_df <- df_master_fltr() %>% 
        filter(Rebound_Type %in% c("Offensive", "Defensive") & Rebound_Name == input$ply_player)
      
      if(length(unique(df_master_fltr()$Game)) > 1) {
        shot_df <- shot_df %>% 
          mutate(Team = ifelse(Team != player_list()$Team[player_list()$Player_Name == input$ply_player], "Opponent", Team), 
                 Home_Team = ifelse(Home_Team != player_list()$Team[player_list()$Player_Name == input$ply_player], "Opponent", Home_Team), 
                 Away_Team = ifelse(Away_Team != player_list()$Team[player_list()$Player_Name == input$ply_player], "Opponent", Away_Team)) %>% 
          mutate(Defensive_Team = ifelse(Team == Home_Team, Away_Team, Home_Team))
      } else {
        shot_df <- shot_df %>% 
          mutate(Defensive_Team = ifelse(Team == Home_Team, Away_Team, Home_Team))
      }
      
      # Filter down by strength input
      if(input$ply_strength == "Even-Strength") {
        shot_df <- shot_df %>% 
          filter(Home_Strength == Away_Strength)    
      } else {
        if(input$ply_strength == "Man-Advantage") {
          shot_df <- shot_df %>% 
            group_by(Game) %>% 
            mutate(Side1_Strength = ifelse(Home_Team == player_list()$Team[player_list()$Player_Name == input$ply_player], 
                                           Home_Strength, Away_Strength), 
                   Side2_Strength = ifelse(Home_Team == player_list()$Team[player_list()$Player_Name == input$ply_player], 
                                           Away_Strength, Home_Strength)) %>% 
            ungroup() %>% 
            mutate(Keep = ifelse((Defensive_Team == player_list()$Team[player_list()$Player_Name == input$ply_player] & Side1_Strength > Side2_Strength) | (Defensive_Team != player_list()$Team[player_list()$Player_Name == input$ply_player] & Side1_Strength < Side2_Strength), "Yes", "No")) %>% 
            filter(Keep == "Yes")
        } else {
          if(input$ply_strength == "Short-Handed") {
            shot_df <- shot_df %>% 
              group_by(Game) %>% 
              mutate(Side1_Strength = ifelse(Home_Team == player_list()$Team[player_list()$Player_Name == input$ply_player], 
                                             Home_Strength, Away_Strength), 
                     Side2_Strength = ifelse(Home_Team == player_list()$Team[player_list()$Player_Name == input$ply_player], 
                                             Away_Strength, Home_Strength)) %>% 
              ungroup() %>% 
              mutate(Keep = ifelse((Defensive_Team == player_list()$Team[player_list()$Player_Name == input$ply_player] & Side1_Strength < Side2_Strength) | (Defensive_Team != player_list()$Team[player_list()$Player_Name == input$ply_player] & Side1_Strength > Side2_Strength), "Yes", "No")) %>% 
              filter(Keep == "Yes")
          }
        }
      }
      
      if(length(input$ply_total_avg) > 0) {
        if(input$ply_total_avg == "Totals") {
          reb_total <- nrow(shot_df)
        } else {
          reb_total <- nrow(shot_df) / length(unique(shot_df$Game))
        }
      } else {
        reb_total <- nrow(shot_df)
      }
      
      paste(as.numeric(reb_total)[[1]], "REBOUNDS", sep = " ")
    }
  })
  
  ##### PLY: Rebounding Bars #####
  
  output$ply_reb <- renderPlotly({
    if(length(input$ply_strength) > 0) {
      
      shot_df <- df_master_fltr() %>% 
        filter(Rebound_Type %in% c("Offensive", "Defensive") & Rebound_Name == input$ply_player)
      
      if(length(unique(df_master_fltr()$Game)) > 1) {
        shot_df <- shot_df %>% 
          mutate(Team = ifelse(Team != player_list()$Team[player_list()$Player_Name == input$ply_player], "Opponent", Team), 
                 Home_Team = ifelse(Home_Team != player_list()$Team[player_list()$Player_Name == input$ply_player], "Opponent", Home_Team), 
                 Away_Team = ifelse(Away_Team != player_list()$Team[player_list()$Player_Name == input$ply_player], "Opponent", Away_Team)) %>% 
          mutate(Defensive_Team = ifelse(Team == Home_Team, Away_Team, Home_Team))
      } else {
        shot_df <- shot_df %>% 
          mutate(Defensive_Team = ifelse(Team == Home_Team, Away_Team, Home_Team))
      }
      
      # Filter down by strength input
      if(input$ply_strength == "Even-Strength") {
        shot_df <- shot_df %>% 
          filter(Home_Strength == Away_Strength)    
      } else {
        if(input$ply_strength == "Man-Advantage") {
          shot_df <- shot_df %>% 
            group_by(Game) %>% 
            mutate(Side1_Strength = ifelse(Home_Team == player_list()$Team[player_list()$Player_Name == input$ply_player], 
                                           Home_Strength, Away_Strength), 
                   Side2_Strength = ifelse(Home_Team == player_list()$Team[player_list()$Player_Name == input$ply_player], 
                                           Away_Strength, Home_Strength)) %>% 
            ungroup() %>% 
            mutate(Keep = ifelse((Defensive_Team == player_list()$Team[player_list()$Player_Name == input$ply_player] & Side1_Strength > Side2_Strength) | (Defensive_Team != player_list()$Team[player_list()$Player_Name == input$ply_player] & Side1_Strength < Side2_Strength), "Yes", "No")) %>% 
            filter(Keep == "Yes")
        } else {
          if(input$ply_strength == "Short-Handed") {
            shot_df <- shot_df %>% 
              group_by(Game) %>% 
              mutate(Side1_Strength = ifelse(Home_Team == player_list()$Team[player_list()$Player_Name == input$ply_player], 
                                             Home_Strength, Away_Strength), 
                     Side2_Strength = ifelse(Home_Team == player_list()$Team[player_list()$Player_Name == input$ply_player], 
                                             Away_Strength, Home_Strength)) %>% 
              ungroup() %>% 
              mutate(Keep = ifelse((Defensive_Team == player_list()$Team[player_list()$Player_Name == input$ply_player] & Side1_Strength < Side2_Strength) | (Defensive_Team != player_list()$Team[player_list()$Player_Name == input$ply_player] & Side1_Strength > Side2_Strength), "Yes", "No")) %>% 
              filter(Keep == "Yes")
          }
        }
      }
      
      # Filter depending on Totals or Averages
      if(length(input$ply_total_avg) > 0) {
        if(input$ply_total_avg == "Totals") {
          shot_df <- shot_df %>% 
            group_by(Rebound_Name) %>% 
            summarise(Won_Def = length(which((Rebound_Type == "Defensive"))),
                      Gave_Off = length(which(Rebound_Type == "Offensive")), 
                      Total = length(which(!is.na(Rebound_Type))))
        } else {
          games <- length(unique(shot_df$Game))
          shot_df <- shot_df %>% 
            group_by(Rebound_Name) %>% 
            summarise(Won_Def = round(length(which((Rebound_Type == "Defensive"))) / games, 1),
                      Gave_Off = round(length(which((Rebound_Type == "Offensive"))) / games, 1), 
                      Total = round(length(which(!is.na(Rebound_Type))) / games, 1))
        }
      } else {
        shot_df <- shot_df %>% 
          group_by(Rebound_Name) %>% 
          summarise(Won_Def = length(which((Rebound_Type == "Defensive"))),
                    Gave_Off = length(which(Rebound_Type == "Offensive")), 
                    Total = length(which(!is.na(Rebound_Type))))
      }
      
      shot_df <- shot_df %>% 
        mutate(Won_Def_Label = paste("Defensive Reb.\n", Won_Def, " - ", percent((Won_Def / Total), accuracy = 0.1), sep = ""),
               Gave_Off_Label = paste("Offensive Reb.\n", Gave_Off, " - ", percent((Gave_Off / Total), accuracy = 0.1), sep = ""))
      
      plot_ly(shot_df, type = "bar", x = ~Gave_Off, y = ~Rebound_Name, name = "Gave_Off", orientation = "h", 
              text = ~Gave_Off_Label, textposition = "inside", insidetextanchor = "middle", textfont = list(size = 15), hoverinfo = "text", 
              marker = list(
                color = df_team_info$Color1[which(df_team_info$Team == player_list()$Team[player_list()$Player_Name == input$ply_player])], 
                line = list(
                  color = "#000000", 
                  width = 2
                )
              )) %>% 
        add_bars(x = ~Won_Def, y = ~Rebound_Name, name = "Won_Def", text = ~Won_Def_Label, textposition = 'inside', 
                 marker = list(
                   color = df_team_info$Color2[which(df_team_info$Team == player_list()$Team[player_list()$Player_Name == input$ply_player])], 
                   line = list(
                     color = "#000000", 
                     width = 2
                   )
                 )) %>% 
        layout(
          barmode = "stack", 
          height = 150, 
          plot_bgcolor = "#FFFFFF", 
          hoverlabel = list(
            font = list(
              size = 18
            )
          ), 
          yaxis = list(
            title = "", 
            side = "right", 
            visible = FALSE, 
            fixedrange = TRUE
          ), 
          xaxis = list(
            autorange = "reversed", 
            visible = FALSE, 
            fixedrange = TRUE
          ), 
          margin = list(
            r = 95, 
            t = 0, 
            b = 0
          )
        ) %>% 
        hide_legend() %>% 
        config(
          modeBarButtonsToRemove = c('toImage', "zoom2d", "toggleSpikelines", "hoverClosestCartesian", 
                                     "hoverCompareCartesian", "drawline", "autoScale2d", "resetScale2d", 
                                     "zoomIn2d", "zoomOut2d", "pan2d", 'select2d', 'lasso2d')
        )  
    }
  })
  
  ##### PLY: Turnover Bars Title #####
  
  output$ply_to_title <- renderText({
    if(length(input$ply_strength) > 0) {
      shot_df <- df_master_fltr() %>% 
        filter(Event_Type == "Turnover" & Player1_Name == input$ply_player)
      
      # Filter down by strength input
      if(input$ply_strength == "Even-Strength") {
        shot_df <- shot_df %>% 
          filter(Home_Strength == Away_Strength)    
      } else {
        if(input$ply_strength == "Man-Advantage") {
          shot_df <- shot_df %>% 
            group_by(Game) %>% 
            mutate(Side1_Strength = ifelse(Home_Team == player_list()$Team[player_list()$Player_Name == input$ply_player], 
                                           Home_Strength, Away_Strength), 
                   Side2_Strength = ifelse(Home_Team == player_list()$Team[player_list()$Player_Name == input$ply_player], 
                                           Away_Strength, Home_Strength)) %>% 
            ungroup() %>% 
            mutate(Keep = ifelse((Team == player_list()$Team[player_list()$Player_Name == input$ply_player] & Side1_Strength > Side2_Strength) | (Team != player_list()$Team[player_list()$Player_Name == input$ply_player] & Side1_Strength < Side2_Strength), "Yes", "No")) %>% 
            filter(Keep == "Yes")
        } else {
          if(input$ply_strength == "Short-Handed") {
            shot_df <- shot_df %>% 
              group_by(Game) %>% 
              mutate(Side1_Strength = ifelse(Home_Team == player_list()$Team[player_list()$Player_Name == input$ply_player], 
                                             Home_Strength, Away_Strength), 
                     Side2_Strength = ifelse(Home_Team == player_list()$Team[player_list()$Player_Name == input$ply_player], 
                                             Away_Strength, Home_Strength)) %>% 
              ungroup() %>% 
              mutate(Keep = ifelse((Team == player_list()$Team[player_list()$Player_Name == input$ply_player] & Side1_Strength < Side2_Strength) | (Team != player_list()$Team[player_list()$Player_Name == input$ply_player] & Side1_Strength > Side2_Strength), "Yes", "No")) %>% 
              filter(Keep == "Yes")
          }
        }
      }
      
      if(nrow(shot_df) > 0){
        if(length(input$ply_total_avg) > 0) {
          if(input$ply_total_avg == "Totals") {
            to_total <- nrow(shot_df)
          } else {
            to_total <- nrow(shot_df) / length(unique(shot_df$Game))
          }
        } else {
          to_total <- nrow(shot_df)
        }
      } else {
        to_total <- 0
      }
      
      paste(as.numeric(to_total)[[1]], "TURNOVERS", sep = " ")
    }
  })
  
  ##### PLY: Turnover Bars #####
  
  output$ply_to <- renderPlotly({
    if(length(input$ply_strength) > 0) {
      shot_df <- df_master_fltr() %>% 
        filter(Event_Type == "Turnover" & Player1_Name == input$ply_player)
      
      # Filter down by strength input
      if(input$ply_strength == "Even-Strength") {
        shot_df <- shot_df %>% 
          filter(Home_Strength == Away_Strength)    
      } else {
        if(input$ply_strength == "Man-Advantage") {
          shot_df <- shot_df %>% 
            group_by(Game) %>% 
            mutate(Side1_Strength = ifelse(Home_Team == player_list()$Team[player_list()$Player_Name == input$ply_player], 
                                           Home_Strength, Away_Strength), 
                   Side2_Strength = ifelse(Home_Team == player_list()$Team[player_list()$Player_Name == input$ply_player], 
                                           Away_Strength, Home_Strength)) %>% 
            ungroup() %>% 
            mutate(Keep = ifelse((Team == player_list()$Team[player_list()$Player_Name == input$ply_player] & Side1_Strength > Side2_Strength) | (Team != player_list()$Team[player_list()$Player_Name == input$ply_player] & Side1_Strength < Side2_Strength), "Yes", "No")) %>% 
            filter(Keep == "Yes")
        } else {
          if(input$ply_strength == "Short-Handed") {
            shot_df <- shot_df %>% 
              group_by(Game) %>% 
              mutate(Side1_Strength = ifelse(Home_Team == player_list()$Team[player_list()$Player_Name == input$ply_player], 
                                             Home_Strength, Away_Strength), 
                     Side2_Strength = ifelse(Home_Team == player_list()$Team[player_list()$Player_Name == input$ply_player], 
                                             Away_Strength, Home_Strength)) %>% 
              ungroup() %>% 
              mutate(Keep = ifelse((Team == player_list()$Team[player_list()$Player_Name == input$ply_player] & Side1_Strength < Side2_Strength) | (Team != player_list()$Team[player_list()$Player_Name == input$ply_player] & Side1_Strength > Side2_Strength), "Yes", "No")) %>% 
              filter(Keep == "Yes")
          }
        }
      }
      
      # Filter depending on Totals or Averages
      if(length(input$ply_total_avg) > 0) {
        if(input$ply_total_avg == "Totals") {
          shot_df <- shot_df %>% 
            group_by(Team) %>% 
            summarise(BallLost = length(which((Turnover_Type == "Ball Lost"))),
                      StrayPass = length(which(Turnover_Type == "Stray Pass")), 
                      InterPass = length(which(Turnover_Type == "Interrupted Pass")), 
                      Concession = length(which(Turnover_Type == "Concession")), 
                      Total = length(which(!is.na(Turnover_Type))))
        } else {
          games <- length(unique(shot_df$Game))
          shot_df <- shot_df %>% 
            group_by(Team) %>% 
            summarise(BallLost = round(length(which((Turnover_Type == "Ball Lost"))) / games, 1),
                      StrayPass = round(length(which((Turnover_Type == "Stray Pass"))) / games, 1), 
                      InterPass = round(length(which((Turnover_Type == "Interrupted Pass"))) / games, 1),
                      Concession = round(length(which((Turnover_Type == "Concession"))) / games, 1), 
                      Total = round(length(which(!is.na(Turnover_Type))) / games, 1))
        }
      } else {
        shot_df <- shot_df %>% 
          group_by(Team) %>% 
          summarise(BallLost = length(which((Turnover_Type == "Ball Lost"))),
                    StrayPass = length(which(Turnover_Type == "Stray Pass")), 
                    InterPass = length(which(Turnover_Type == "Interrupted Pass")), 
                    Concession = length(which(Turnover_Type == "Concession")), 
                    Total = length(which(!is.na(Turnover_Type))))
      }
      
      shot_df <- shot_df %>% 
        mutate(BallLost_Label = paste("Ball Lost\n", BallLost, " - ", percent((BallLost / Total), accuracy = 0.1), sep = ""),
               StrayPass_Label = paste("Stray Pass\n", StrayPass, " - ", percent((StrayPass / Total), accuracy = 0.1), sep = ""), 
               InterPass_Label = paste("Interrupted Pass\n", InterPass, " - ", percent((InterPass / Total), accuracy = 0.1), sep = ""),
               Concession_Label = paste("Concession\n", Concession, " - ", percent((Concession / Total), accuracy = 0.1), sep = ""))
      
      plot_ly(shot_df, type = "bar", x = ~BallLost, y = ~Team, name = "BallLost", orientation = "h", 
              text = ~BallLost_Label, textposition = "inside", insidetextanchor = "middle", textfont = list(size = 15), hoverinfo = "text", 
              marker = list(
                color = df_team_info$Color1[which(df_team_info$Team == player_list()$Team[player_list()$Player_Name == input$ply_player])], 
                line = list(
                  color = "#000000", 
                  width = 2
                )
              )) %>% 
        add_bars(x = ~StrayPass, y = ~Team, name = "StrayPass", text = ~StrayPass_Label, textposition = 'inside', 
                 marker = list(
                   color = df_team_info$Color2[which(df_team_info$Team == player_list()$Team[player_list()$Player_Name == input$ply_player])], 
                   line = list(
                     color = "#000000", 
                     width = 2
                   )
                 )) %>% 
        add_bars(x = ~InterPass, y = ~Team, name = "InterPass", text = ~InterPass_Label, textposition = 'inside', 
                 marker = list(
                   color = df_team_info$Color3[which(df_team_info$Team == player_list()$Team[player_list()$Player_Name == input$ply_player])], 
                   line = list(
                     color = "#000000", 
                     width = 2
                   )
                 )) %>% 
        add_bars(x = ~Concession, y = ~Team, name = "Concession", text = ~Concession_Label, textposition = 'inside', 
                 marker = list(
                   color = df_team_info$Color2[which(df_team_info$Team == player_list()$Team[player_list()$Player_Name == input$ply_player])], 
                   line = list(
                     color = "#000000", 
                     width = 2
                   )
                 )) %>% 
        layout(
          barmode = "stack", 
          height = 150, 
          plot_bgcolor = "#FFFFFF", 
          hoverlabel = list(
            font = list(
              size = 18
            )
          ), 
          yaxis = list(
            title = "", 
            side = "right", 
            visible = FALSE, 
            fixedrange = TRUE
          ), 
          xaxis = list(
            autorange = "reversed", 
            visible = FALSE, 
            fixedrange = TRUE
          ), 
          margin = list(
            r = 95, 
            t = 0, 
            b = 0
          )
        ) %>% 
        hide_legend() %>% 
        config(
          modeBarButtonsToRemove = c('toImage', "zoom2d", "toggleSpikelines", "hoverClosestCartesian", 
                                     "hoverCompareCartesian", "drawline", "autoScale2d", "resetScale2d", 
                                     "zoomIn2d", "zoomOut2d", "pan2d", 'select2d', 'lasso2d')
        )  
    }
  })
  
  ##### PLY: Goalie Save Per Title #####
  
  output$ply_goalie_saveper_title <- renderText({
    if(length(input$ply_strength) > 0) {
      
      shot_df <- df_master_fltr() %>% 
        filter(!is.na(Result)) %>% 
        filter((Home_Team == Team & Away_Goalie_Name == input$ply_player) | (Away_Team == Team & Home_Goalie_Name == input$ply_player)) %>% 
        filter(Net_Location %in% shot_on)
      
      if(length(unique(shot_df$Game)) > 1) {
        shot_df <- shot_df %>% 
          mutate(Team = ifelse(Team != player_list()$Team[player_list()$Player_Name == input$ply_player], "Opponent", Team), 
                 Home_Team = ifelse(Home_Team != player_list()$Team[player_list()$Player_Name == input$ply_player], "Opponent", Home_Team), 
                 Away_Team = ifelse(Away_Team != player_list()$Team[player_list()$Player_Name == input$ply_player], "Opponent", Away_Team)) %>% 
          mutate(Defensive_Team = ifelse(Team == Home_Team, Away_Team, Home_Team))
      } else {
        shot_df <- shot_df %>% 
          mutate(Defensive_Team = ifelse(Team == Home_Team, Away_Team, Home_Team))
      }
      
      # Filter down by strength input
      if(input$ply_strength == "Even-Strength") {
        shot_df <- shot_df %>% 
          filter(Home_Strength == Away_Strength)    
      } else {
        if(input$ply_strength == "Man-Advantage") {
          shot_df <- shot_df %>% 
            group_by(Game) %>% 
            mutate(Side1_Strength = ifelse(Home_Team == player_list()$Team[player_list()$Player_Name == input$ply_player], 
                                           Home_Strength, Away_Strength), 
                   Side2_Strength = ifelse(Home_Team == player_list()$Team[player_list()$Player_Name == input$ply_player], 
                                           Away_Strength, Home_Strength)) %>% 
            ungroup() %>% 
            mutate(Keep = ifelse((Defensive_Team == player_list()$Team[player_list()$Player_Name == input$ply_player] & Side1_Strength > Side2_Strength) | (Defensive_Team != player_list()$Team[player_list()$Player_Name == input$ply_player] & Side1_Strength < Side2_Strength), "Yes", "No")) %>% 
            filter(Keep == "Yes")
        } else {
          if(input$ply_strength == "Short-Handed") {
            shot_df <- shot_df %>% 
              group_by(Game) %>% 
              mutate(Side1_Strength = ifelse(Home_Team == player_list()$Team[player_list()$Player_Name == input$ply_player], 
                                             Home_Strength, Away_Strength), 
                     Side2_Strength = ifelse(Home_Team == player_list()$Team[player_list()$Player_Name == input$ply_player], 
                                             Away_Strength, Home_Strength)) %>% 
              ungroup() %>% 
              mutate(Keep = ifelse((Defensive_Team == player_list()$Team[player_list()$Player_Name == input$ply_player] & Side1_Strength < Side2_Strength) | (Defensive_Team != player_list()$Team[player_list()$Player_Name == input$ply_player] & Side1_Strength > Side2_Strength), "Yes", "No")) %>% 
              filter(Keep == "Yes")
          }
        }
      }
      
      if(length(input$ply_total_avg) > 0) {
        if(input$ply_total_avg == "Totals") {
          shot_total <- nrow(shot_df[!is.na(shot_df$Result), ])
        } else {
          shot_total <- round(nrow(shot_df[!is.na(shot_df$Result), ]) / length(unique(shot_df$Game)), 1)
        }
      } else {
        shot_total <- nrow(shot_df[!is.na(shot_df$Result), ])
      }
      
      paste(as.numeric(shot_total)[[1]], "SHOTS AGAINST ON-NET", sep = " ")
    }
  })
  
  ##### PLY: Goalie Save Per Bars #####
  
  output$ply_goalie_saveper <- renderPlotly({
    if(length(input$ply_strength) > 0) {
      
      shot_df <- df_master_fltr() %>% 
        filter(!is.na(Result)) %>% 
        filter((Home_Team == Team & Away_Goalie_Name == input$ply_player) | (Away_Team == Team & Home_Goalie_Name == input$ply_player)) %>% 
        filter(Net_Location %in% shot_on)
      
      if(length(unique(shot_df$Game)) > 1) {
        shot_df <- shot_df %>% 
          mutate(Team = ifelse(Team != player_list()$Team[player_list()$Player_Name == input$ply_player], "Opponent", Team), 
                 Home_Team = ifelse(Home_Team != player_list()$Team[player_list()$Player_Name == input$ply_player], "Opponent", Home_Team), 
                 Away_Team = ifelse(Away_Team != player_list()$Team[player_list()$Player_Name == input$ply_player], "Opponent", Away_Team)) %>% 
          mutate(Defensive_Team = ifelse(Team == Home_Team, Away_Team, Home_Team))
      } else {
        shot_df <- shot_df %>% 
          mutate(Defensive_Team = ifelse(Team == Home_Team, Away_Team, Home_Team))
      }
      
      # Filter down by strength input
      if(input$ply_strength == "Even-Strength") {
        shot_df <- shot_df %>% 
          filter(Home_Strength == Away_Strength)    
      } else {
        if(input$ply_strength == "Man-Advantage") {
          shot_df <- shot_df %>% 
            group_by(Game) %>% 
            mutate(Side1_Strength = ifelse(Home_Team == player_list()$Team[player_list()$Player_Name == input$ply_player], 
                                           Home_Strength, Away_Strength), 
                   Side2_Strength = ifelse(Home_Team == player_list()$Team[player_list()$Player_Name == input$ply_player], 
                                           Away_Strength, Home_Strength)) %>% 
            ungroup() %>% 
            mutate(Keep = ifelse((Defensive_Team == player_list()$Team[player_list()$Player_Name == input$ply_player] & Side1_Strength > Side2_Strength) | (Defensive_Team != player_list()$Team[player_list()$Player_Name == input$ply_player] & Side1_Strength < Side2_Strength), "Yes", "No")) %>% 
            filter(Keep == "Yes")
        } else {
          if(input$ply_strength == "Short-Handed") {
            shot_df <- shot_df %>% 
              group_by(Game) %>% 
              mutate(Side1_Strength = ifelse(Home_Team == player_list()$Team[player_list()$Player_Name == input$ply_player], 
                                             Home_Strength, Away_Strength), 
                     Side2_Strength = ifelse(Home_Team == player_list()$Team[player_list()$Player_Name == input$ply_player], 
                                             Away_Strength, Home_Strength)) %>% 
              ungroup() %>% 
              mutate(Keep = ifelse((Defensive_Team == player_list()$Team[player_list()$Player_Name == input$ply_player] & Side1_Strength < Side2_Strength) | (Defensive_Team != player_list()$Team[player_list()$Player_Name == input$ply_player] & Side1_Strength > Side2_Strength), "Yes", "No")) %>% 
              filter(Keep == "Yes")
          }
        }
      }
      
      if(length(input$ply_total_avg) > 0) {
        if(input$ply_total_avg == "Totals") {
          shot_df <- shot_df %>% 
            group_by(Defensive_Team) %>% 
            summarise(Goal = length(which((Result == "Goal"))),
                      Save = length(which((Result == "Miss"))),
                      Total = length(which(!is.na(Result)))) %>% 
            ungroup()
        } else {
          games <- length(unique(shot_df$Game))
          shot_df <- shot_df %>% 
            group_by(Defensive_Team) %>% 
            summarise(Goal = round(length(which((Result == "Goal"))) / games, 1),
                      Save = round(length(which((Result == "Miss"))) / games, 1),
                      Total = round(length(which(!is.na(Result))) / games, 1)) %>% 
            ungroup()
        }
      } else {
        shot_df <- shot_df %>% 
          group_by(Defensive_Team) %>% 
          summarise(Goal = length(which((Result == "Goal"))),
                    Save = length(which((Result == "Miss"))),
                    Total = length(which(!is.na(Result)))) %>% 
          ungroup() 
      }
      
      shot_df <- shot_df %>% 
        mutate(Goal_Label = paste("Goal\n", Goal, " - ", percent((Goal / Total), accuracy = 0.1), sep = ""),
               Save_Label = paste("Save\n", Save, " - ", percent((Save / Total), accuracy = 0.1), sep = ""))
      
      shot_df <- shot_df %>% 
        filter(Defensive_Team == player_list()$Team[player_list()$Player_Name == input$ply_player])
      
      plot_ly(shot_df, type = "bar", x = ~Goal, y = ~Defensive_Team, name = "Goal", orientation = "h", 
              text = ~Goal_Label, textposition = "inside", insidetextanchor = "middle", textfont = list(size = 15), hoverinfo = "text", 
              marker = list(
                color = df_team_info$Color1[which(df_team_info$Team == player_list()$Team[player_list()$Player_Name == input$ply_player])], 
                line = list(
                  color = "#000000", 
                  width = 2
                )
              )) %>% 
        add_bars(x = ~Save, y = ~Defensive_Team, name = "Save", text = ~Save_Label, textposition = 'inside',  
                 marker = list(
                   color = df_team_info$Color2[which(df_team_info$Team == player_list()$Team[player_list()$Player_Name == input$ply_player])], 
                   line = list(
                     color = "#000000", 
                     width = 2
                   )
                 )) %>% 
        layout(
          barmode = "stack", 
          height = 150, 
          plot_bgcolor = "#FFFFFF", 
          hoverlabel = list(
            font = list(
              size = 18
            )
          ), 
          yaxis = list(
            title = "", 
            side = "right", 
            visible = FALSE, 
            fixedrange = TRUE
          ), 
          xaxis = list(
            autorange = "reversed", 
            visible = FALSE, 
            fixedrange = TRUE
          ), 
          margin = list(
            r = 95, 
            t = 0, 
            b = 0
          )
        ) %>% 
        hide_legend() %>% 
        config(
          modeBarButtonsToRemove = c('toImage', "zoom2d", "toggleSpikelines", "hoverClosestCartesian", 
                                     "hoverCompareCartesian", "drawline", "autoScale2d", "resetScale2d", 
                                     "zoomIn2d", "zoomOut2d", "pan2d", 'select2d', 'lasso2d')
        )  
    }
  })
  
  
  ##### PLY: Goalie Control Bars Title #####
  
  output$ply_goalie_control_title <- renderText({
    if(length(input$ply_strength) > 0) {
      
      shot_df <- df_master_fltr() %>% 
        filter(!is.na(Result)) %>% 
        filter((Home_Team == Team & Away_Goalie_Name == input$ply_player) | (Away_Team == Team & Home_Goalie_Name == input$ply_player)) %>% 
        filter(Net_Location %in% shot_on)
      
      if(length(unique(shot_df$Game)) > 1) {
        shot_df <- shot_df %>% 
          mutate(Team = ifelse(Team != player_list()$Team[player_list()$Player_Name == input$ply_player], "Opponent", Team), 
                 Home_Team = ifelse(Home_Team != player_list()$Team[player_list()$Player_Name == input$ply_player], "Opponent", Home_Team), 
                 Away_Team = ifelse(Away_Team != player_list()$Team[player_list()$Player_Name == input$ply_player], "Opponent", Away_Team)) %>% 
          mutate(Defensive_Team = ifelse(Team == Home_Team, Away_Team, Home_Team))
      } else {
        shot_df <- shot_df %>% 
          mutate(Defensive_Team = ifelse(Team == Home_Team, Away_Team, Home_Team))
      }
      
      # Filter down by strength input
      if(input$ply_strength == "Even-Strength") {
        shot_df <- shot_df %>% 
          filter(Home_Strength == Away_Strength)    
      } else {
        if(input$ply_strength == "Man-Advantage") {
          shot_df <- shot_df %>% 
            group_by(Game) %>% 
            mutate(Side1_Strength = ifelse(Home_Team == player_list()$Team[player_list()$Player_Name == input$ply_player], 
                                           Home_Strength, Away_Strength), 
                   Side2_Strength = ifelse(Home_Team == player_list()$Team[player_list()$Player_Name == input$ply_player], 
                                           Away_Strength, Home_Strength)) %>% 
            ungroup() %>% 
            mutate(Keep = ifelse((Defensive_Team == player_list()$Team[player_list()$Player_Name == input$ply_player] & Side1_Strength > Side2_Strength) | (Defensive_Team != player_list()$Team[player_list()$Player_Name == input$ply_player] & Side1_Strength < Side2_Strength), "Yes", "No")) %>% 
            filter(Keep == "Yes")
        } else {
          if(input$ply_strength == "Short-Handed") {
            shot_df <- shot_df %>% 
              group_by(Game) %>% 
              mutate(Side1_Strength = ifelse(Home_Team == player_list()$Team[player_list()$Player_Name == input$ply_player], 
                                             Home_Strength, Away_Strength), 
                     Side2_Strength = ifelse(Home_Team == player_list()$Team[player_list()$Player_Name == input$ply_player], 
                                             Away_Strength, Home_Strength)) %>% 
              ungroup() %>% 
              mutate(Keep = ifelse((Defensive_Team == player_list()$Team[player_list()$Player_Name == input$ply_player] & Side1_Strength < Side2_Strength) | (Defensive_Team != player_list()$Team[player_list()$Player_Name == input$ply_player] & Side1_Strength > Side2_Strength), "Yes", "No")) %>% 
              filter(Keep == "Yes")
          }
        }
      }
      
      if(length(input$ply_total_avg) > 0) {
        if(input$ply_total_avg == "Totals") {
          shot_total <- nrow(shot_df[(shot_df$Result == "Miss"), ])
        } else {
          shot_total <- round(nrow(shot_df[(shot_df$Result == "Miss"), ]) / length(unique(shot_df$Game)), 1)
        }
      } else {
        shot_total <- nrow(shot_df[(shot_df$Result == "Miss"), ])
      }
      
      paste(as.numeric(shot_total)[[1]], "SAVED SHOTS", sep = " ")
    }
  })
  
  ##### PLY: Goalie Control Bars #####
  
  output$ply_goalie_control <- renderPlotly({
    if(length(input$ply_strength) > 0) {
      
      shot_df <- df_master_fltr() %>% 
        filter(!is.na(Result)) %>% 
        filter((Home_Team == Team & Away_Goalie_Name == input$ply_player) | (Away_Team == Team & Home_Goalie_Name == input$ply_player)) %>% 
        filter(Net_Location %in% shot_on)
      
      if(length(unique(shot_df$Game)) > 1) {
        shot_df <- shot_df %>% 
          mutate(Team = ifelse(Team != player_list()$Team[player_list()$Player_Name == input$ply_player], "Opponent", Team), 
                 Home_Team = ifelse(Home_Team != player_list()$Team[player_list()$Player_Name == input$ply_player], "Opponent", Home_Team), 
                 Away_Team = ifelse(Away_Team != player_list()$Team[player_list()$Player_Name == input$ply_player], "Opponent", Away_Team)) %>% 
          mutate(Defensive_Team = ifelse(Team == Home_Team, Away_Team, Home_Team))
      } else {
        shot_df <- shot_df %>% 
          mutate(Defensive_Team = ifelse(Team == Home_Team, Away_Team, Home_Team))
      }
      
      # Filter down by strength input
      if(input$ply_strength == "Even-Strength") {
        shot_df <- shot_df %>% 
          filter(Home_Strength == Away_Strength)    
      } else {
        if(input$ply_strength == "Man-Advantage") {
          shot_df <- shot_df %>% 
            group_by(Game) %>% 
            mutate(Side1_Strength = ifelse(Home_Team == player_list()$Team[player_list()$Player_Name == input$ply_player], 
                                           Home_Strength, Away_Strength), 
                   Side2_Strength = ifelse(Home_Team == player_list()$Team[player_list()$Player_Name == input$ply_player], 
                                           Away_Strength, Home_Strength)) %>% 
            ungroup() %>% 
            mutate(Keep = ifelse((Defensive_Team == player_list()$Team[player_list()$Player_Name == input$ply_player] & Side1_Strength > Side2_Strength) | (Defensive_Team != player_list()$Team[player_list()$Player_Name == input$ply_player] & Side1_Strength < Side2_Strength), "Yes", "No")) %>% 
            filter(Keep == "Yes")
        } else {
          if(input$ply_strength == "Short-Handed") {
            shot_df <- shot_df %>% 
              group_by(Game) %>% 
              mutate(Side1_Strength = ifelse(Home_Team == player_list()$Team[player_list()$Player_Name == input$ply_player], 
                                             Home_Strength, Away_Strength), 
                     Side2_Strength = ifelse(Home_Team == player_list()$Team[player_list()$Player_Name == input$ply_player], 
                                             Away_Strength, Home_Strength)) %>% 
              ungroup() %>% 
              mutate(Keep = ifelse((Defensive_Team == player_list()$Team[player_list()$Player_Name == input$ply_player] & Side1_Strength < Side2_Strength) | (Defensive_Team != player_list()$Team[player_list()$Player_Name == input$ply_player] & Side1_Strength > Side2_Strength), "Yes", "No")) %>% 
              filter(Keep == "Yes")
          }
        }
      }
      
      if(length(input$ply_total_avg) > 0) {
        if(input$ply_total_avg == "Totals") {
          shot_df <- shot_df %>% 
            group_by(Defensive_Team) %>% 
            summarise(Clean_Save = length(which((Goalie_Response == "Clean Save"))),
                      Not_Clean = length(which(Goalie_Response == "Not Clean")),
                      Total = length(which(!is.na(Result)))) %>% 
            ungroup() 
        } else {
          games <- length(unique(shot_df$Game))
          shot_df <- shot_df %>% 
            group_by(Defensive_Team) %>% 
            summarise(Clean_Save = round(length(which(Goalie_Response == "Clean Save")) / games, 1),
                      Not_Clean = round(length(which(Goalie_Response == "Not Clean")) / games, 1),
                      Total = round(length(which(!is.na(Result))) / games, 1)) %>% 
            ungroup() 
        }
      } else {
        shot_df <- shot_df %>% 
          group_by(Defensive_Team) %>% 
          summarise(Clean_Save = length(which((Goalie_Response == "Clean Save"))),
                    Not_Clean = length(which(Goalie_Response == "Not Clean")),
                    Total = length(which(!is.na(Result)))) %>% 
          ungroup() 
      }
      
      shot_df <- shot_df %>% 
        mutate(Clean_Save_Label = paste("Clean Save\n", Clean_Save, " - ", percent((Clean_Save / Total), accuracy = 0.1), sep = ""),
               Not_Clean_Label = paste("Not Clean\n", Not_Clean, " - ", percent((Not_Clean / Total), accuracy = 0.1), sep = ""))
      
      shot_df <- shot_df %>% 
        filter(Defensive_Team == player_list()$Team[player_list()$Player_Name == input$ply_player])
      
      plot_ly(shot_df, type = "bar", x = ~Clean_Save, y = ~Defensive_Team, name = "Clean_Save", orientation = "h", 
              text = ~Clean_Save_Label, textposition = "inside", insidetextanchor = "middle", textfont = list(size = 15), hoverinfo = "text", 
              marker = list(
                color = df_team_info$Color1[which(df_team_info$Team == player_list()$Team[player_list()$Player_Name == input$ply_player])], 
                line = list(
                  color = "#000000", 
                  width = 2
                )
              )) %>% 
        add_bars(x = ~Not_Clean, y = ~Defensive_Team, name = "Not_Clean", text = ~Not_Clean_Label, textposition = 'inside',  
                 marker = list(
                   color = df_team_info$Color2[which(df_team_info$Team == player_list()$Team[player_list()$Player_Name == input$ply_player])], 
                   line = list(
                     color = "#000000", 
                     width = 2
                   )
                 )) %>% 
        layout(
          barmode = "stack", 
          height = 150, 
          plot_bgcolor = "#FFFFFF", 
          hoverlabel = list(
            font = list(
              size = 18
            )
          ), 
          yaxis = list(
            title = "", 
            side = "right", 
            visible = FALSE, 
            fixedrange = TRUE
          ), 
          xaxis = list(
            autorange = "reversed", 
            visible = FALSE, 
            fixedrange = TRUE
          ), 
          margin = list(
            r = 95, 
            t = 0, 
            b = 0
          )
        ) %>% 
        hide_legend() %>% 
        config(
          modeBarButtonsToRemove = c('toImage', "zoom2d", "toggleSpikelines", "hoverClosestCartesian", 
                                     "hoverCompareCartesian", "drawline", "autoScale2d", "resetScale2d", 
                                     "zoomIn2d", "zoomOut2d", "pan2d", 'select2d', 'lasso2d')
        )  
    }
  })
  
  ##### PLY: Goalie Profile Title #####
  
  output$ply_goalie_profile_title <- renderText({
    if(length(input$ply_strength) > 0) {
      shot_df <- df_master_fltr() %>% 
        filter(!is.na(Result)) %>% 
        filter((Home_Team == Team & Away_Goalie_Name == input$ply_player) | (Away_Team == Team & Home_Goalie_Name == input$ply_player))
      
      if(length(unique(shot_df$Game)) > 1) {
        shot_df <- shot_df %>% 
          mutate(Team = ifelse(Team != player_list()$Team[player_list()$Player_Name == input$ply_player], "Opponent", Team), 
                 Home_Team = ifelse(Home_Team != player_list()$Team[player_list()$Player_Name == input$ply_player], "Opponent", Home_Team), 
                 Away_Team = ifelse(Away_Team != player_list()$Team[player_list()$Player_Name == input$ply_player], "Opponent", Away_Team)) %>% 
          mutate(Defensive_Team = ifelse(Team == Home_Team, Away_Team, Home_Team))
      } else {
        shot_df <- shot_df %>% 
          mutate(Defensive_Team = ifelse(Team == Home_Team, Away_Team, Home_Team))
      }
      
      # Filter down by strength input
      if(input$ply_strength == "Even-Strength") {
        shot_df <- shot_df %>% 
          filter(Home_Strength == Away_Strength)    
      } else {
        if(input$ply_strength == "Man-Advantage") {
          shot_df <- shot_df %>% 
            group_by(Game) %>% 
            mutate(Side1_Strength = ifelse(Home_Team == player_list()$Team[player_list()$Player_Name == input$ply_player], 
                                           Home_Strength, Away_Strength), 
                   Side2_Strength = ifelse(Home_Team == player_list()$Team[player_list()$Player_Name == input$ply_player], 
                                           Away_Strength, Home_Strength)) %>% 
            ungroup() %>% 
            mutate(Keep = ifelse((Defensive_Team == player_list()$Team[player_list()$Player_Name == input$ply_player] & Side1_Strength > Side2_Strength) | (Defensive_Team != player_list()$Team[player_list()$Player_Name == input$ply_player] & Side1_Strength < Side2_Strength), "Yes", "No")) %>% 
            filter(Keep == "Yes")
        } else {
          if(input$ply_strength == "Short-Handed") {
            shot_df <- shot_df %>% 
              group_by(Game) %>% 
              mutate(Side1_Strength = ifelse(Home_Team == player_list()$Team[player_list()$Player_Name == input$ply_player], 
                                             Home_Strength, Away_Strength), 
                     Side2_Strength = ifelse(Home_Team == player_list()$Team[player_list()$Player_Name == input$ply_player], 
                                             Away_Strength, Home_Strength)) %>% 
              ungroup() %>% 
              mutate(Keep = ifelse((Defensive_Team == player_list()$Team[player_list()$Player_Name == input$ply_player] & Side1_Strength < Side2_Strength) | (Defensive_Team != player_list()$Team[player_list()$Player_Name == input$ply_player] & Side1_Strength > Side2_Strength), "Yes", "No")) %>% 
              filter(Keep == "Yes")
          }
        }
      }
      
      if(length(input$ply_total_avg) > 0) {
        if(input$ply_total_avg == "Totals") {
          shot_total <- nrow(shot_df[!is.na(shot_df$Result), ])
        } else {
          shot_total <- round(nrow(shot_df[!is.na(shot_df$Result), ]) / length(unique(shot_df$Game)), 1)
        }
      } else {
        shot_total <- nrow(shot_df[!is.na(shot_df$Result), ])
      }
      
      paste(as.numeric(shot_total)[[1]], "TOTAL SHOTS AGAINST", sep = " ")
    }
  })
  
  ##### PLY: Goalie Shot Profile #####
  
  output$ply_goalie_profile <- renderPlotly({
    if(length(input$ply_strength) > 0) {
      
      shot_df <- df_master_fltr() %>% 
        filter(!is.na(Result)) %>% 
        filter((Home_Team == Team & Away_Goalie_Name == input$ply_player) | (Away_Team == Team & Home_Goalie_Name == input$ply_player))
      
      if(length(unique(shot_df$Game)) > 1) {
        shot_df <- shot_df %>% 
          mutate(Team = ifelse(Team != player_list()$Team[player_list()$Player_Name == input$ply_player], "Opponent", Team), 
                 Home_Team = ifelse(Home_Team != player_list()$Team[player_list()$Player_Name == input$ply_player], "Opponent", Home_Team), 
                 Away_Team = ifelse(Away_Team != player_list()$Team[player_list()$Player_Name == input$ply_player], "Opponent", Away_Team)) %>% 
          mutate(Defensive_Team = ifelse(Team == Home_Team, Away_Team, Home_Team))
      } else {
        shot_df <- shot_df %>% 
          mutate(Defensive_Team = ifelse(Team == Home_Team, Away_Team, Home_Team))
      }
      
      # Filter down by strength input
      if(input$ply_strength == "Even-Strength") {
        shot_df <- shot_df %>% 
          filter(Home_Strength == Away_Strength)    
      } else {
        if(input$ply_strength == "Man-Advantage") {
          shot_df <- shot_df %>% 
            group_by(Game) %>% 
            mutate(Side1_Strength = ifelse(Home_Team == player_list()$Team[player_list()$Player_Name == input$ply_player], 
                                           Home_Strength, Away_Strength), 
                   Side2_Strength = ifelse(Home_Team == player_list()$Team[player_list()$Player_Name == input$ply_player], 
                                           Away_Strength, Home_Strength)) %>% 
            ungroup() %>% 
            mutate(Keep = ifelse((Defensive_Team == player_list()$Team[player_list()$Player_Name == input$ply_player] & Side1_Strength > Side2_Strength) | (Defensive_Team != player_list()$Team[player_list()$Player_Name == input$ply_player] & Side1_Strength < Side2_Strength), "Yes", "No")) %>% 
            filter(Keep == "Yes")
        } else {
          if(input$ply_strength == "Short-Handed") {
            shot_df <- shot_df %>% 
              group_by(Game) %>% 
              mutate(Side1_Strength = ifelse(Home_Team == player_list()$Team[player_list()$Player_Name == input$ply_player], 
                                             Home_Strength, Away_Strength), 
                     Side2_Strength = ifelse(Home_Team == player_list()$Team[player_list()$Player_Name == input$ply_player], 
                                             Away_Strength, Home_Strength)) %>% 
              ungroup() %>% 
              mutate(Keep = ifelse((Defensive_Team == player_list()$Team[player_list()$Player_Name == input$ply_player] & Side1_Strength < Side2_Strength) | (Defensive_Team != player_list()$Team[player_list()$Player_Name == input$ply_player] & Side1_Strength > Side2_Strength), "Yes", "No")) %>% 
              filter(Keep == "Yes")
          }
        }
      }
      
      if(length(input$ply_total_avg) > 0) {
        if(input$ply_total_avg == "Totals") {
          shot_df <- shot_df %>% 
            group_by(Defensive_Team) %>% 
            summarise(Goal = length(which((Result == "Goal"))),
                      Saved = length(which(Result == "Miss" & Net_Location %in% shot_on)),
                      Off = length(which(Result == "Miss" & Net_Location %in% shot_off)),
                      Post = length(which(Result == "Miss" & Net_Location %in% shot_post)),
                      Blocked = length(which(Result == "Blocked")), 
                      Total = length(which(!is.na(Result))))
        } else {
          games <- length(unique(shot_df$Game))
          shot_df <- shot_df %>% 
            group_by(Defensive_Team) %>% 
            summarise(Goal = round(length(which((Result == "Goal"))) / games, 1),
                      Saved = round(length(which(Result == "Miss" & Net_Location %in% shot_on)) / games, 1),
                      Off = round(length(which(Result == "Miss" & Net_Location %in% shot_off)) / games, 1),
                      Post = round(length(which(Result == "Miss" & Net_Location %in% shot_post)) / games, 1),
                      Blocked = round(length(which(Result == "Blocked")) / games, 1), 
                      Total = round(length(which(!is.na(Result))) / games, 1))
        }
      } else {
        shot_df <- shot_df %>% 
          group_by(Defensive_Team) %>% 
          summarise(Goal = length(which((Result == "Goal"))),
                    Saved = length(which(Result == "Miss" & Net_Location %in% shot_on)),
                    Off = length(which(Result == "Miss" & Net_Location %in% shot_off)),
                    Post = length(which(Result == "Miss" & Net_Location %in% shot_post)),
                    Blocked = length(which(Result == "Blocked")), 
                    Total = length(which(!is.na(Result))))
      }
      
      shot_df <- shot_df %>% 
        mutate(Goal_Label = paste("Goal\n", Goal, " - ", percent((Goal / Total), accuracy = 0.1), sep = ""),
               Saved_Label = paste("Saved\n", Saved, " - ", percent((Saved / Total), accuracy = 0.1), sep = ""),
               Off_Label = paste("Off\n", Off, " - ", percent((Off / Total), accuracy = 0.1), sep = ""),
               Post_Label = paste("Post\n", Post, " - ", percent((Post / Total), accuracy = 0.1), sep = ""),
               Blocked_Label = paste("Blocked\n", Blocked, " - ", percent((Blocked / Total), accuracy = 0.1), sep = ""), 
               Total_Label = paste("Total\n", Total, sep = ""))
      
      shot_df <- shot_df %>% 
        filter(Defensive_Team == player_list()$Team[player_list()$Player_Name == input$ply_player])
      
      plot_ly(shot_df, type = "bar", x = ~Goal, y = ~Defensive_Team, name = "Goals", orientation = "h", 
              text = ~Goal_Label, textposition = "inside", insidetextanchor = "middle", textfont = list(size = 15), hoverinfo = "text", 
              marker = list(
                color = df_team_info$Color1[which(df_team_info$Team == player_list()$Team[player_list()$Player_Name == input$ply_player])], 
                line = list(
                  color = "#000000", 
                  width = 2
                )
              )) %>% 
        add_bars(x = ~Saved, y = ~Defensive_Team, name = "Saved Shots", text = ~Saved_Label, textposition = 'inside',  
                 marker = list(
                   color = df_team_info$Color2[which(df_team_info$Team == player_list()$Team[player_list()$Player_Name == input$ply_player])], 
                   line = list(
                     color = "#000000", 
                     width = 2
                   )
                 )) %>% 
        add_bars(x = ~Off, y = ~Defensive_Team, name = "Off Shots", text = ~Off_Label, textposition = 'inside', 
                 marker = list(
                   color = df_team_info$Color3[which(df_team_info$Team == player_list()$Team[player_list()$Player_Name == input$ply_player])], 
                   line = list(
                     color = "#000000", 
                     width = 2
                   )
                 )) %>% 
        add_bars(x = ~Post, y = ~Defensive_Team, name = "Post Shots", text = ~Post_Label, textposition = 'inside', 
                 marker = list(
                   color = df_team_info$Color1[which(df_team_info$Team == player_list()$Team[player_list()$Player_Name == input$ply_player])], 
                   line = list(
                     color = "#000000", 
                     width = 2
                   )
                 )) %>% 
        add_bars(x = ~Blocked, y = ~Defensive_Team, name = "Blocked Shots", text = ~Blocked_Label, textposition = 'inside', 
                 marker = list(
                   color = df_team_info$Color2[which(df_team_info$Team == player_list()$Team[player_list()$Player_Name == input$ply_player])], 
                   line = list(
                     color = "#000000", 
                     width = 2
                   )
                 )) %>% 
        layout(
          barmode = "stack", 
          height = 150, 
          plot_bgcolor = "#FFFFFF", 
          hoverlabel = list(
            font = list(
              size = 18
            )
          ), 
          yaxis = list(
            title = "", 
            side = "right", 
            visible = FALSE, 
            fixedrange = TRUE
          ), 
          xaxis = list(
            autorange = "reversed", 
            visible = FALSE, 
            fixedrange = TRUE
          ), 
          margin = list(
            r = 95, 
            t = 0, 
            b = 0
          )
        ) %>% 
        hide_legend() %>% 
        config(
          modeBarButtonsToRemove = c('toImage', "zoom2d", "toggleSpikelines", "hoverClosestCartesian", 
                                     "hoverCompareCartesian", "drawline", "autoScale2d", "resetScale2d", 
                                     "zoomIn2d", "zoomOut2d", "pan2d", 'select2d', 'lasso2d')
        )  
    }
  })
  
  ##### PLY: Goalie Possession Bars #####
  
  output$ply_goalie_profile_poss <- renderPlotly({
    if(length(input$ply_strength) > 0) {
      
      shot_df <- df_master_fltr() %>% 
        filter(!is.na(Result)) %>% 
        filter((Home_Team == Team & Away_Goalie_Name == input$ply_player) | (Away_Team == Team & Home_Goalie_Name == input$ply_player))
      
      if(length(unique(shot_df$Game)) > 1) {
        shot_df <- shot_df %>% 
          mutate(Team = ifelse(Team != player_list()$Team[player_list()$Player_Name == input$ply_player], "Opponent", Team), 
                 Home_Team = ifelse(Home_Team != player_list()$Team[player_list()$Player_Name == input$ply_player], "Opponent", Home_Team), 
                 Away_Team = ifelse(Away_Team != player_list()$Team[player_list()$Player_Name == input$ply_player], "Opponent", Away_Team)) %>% 
          mutate(Defensive_Team = ifelse(Team == Home_Team, Away_Team, Home_Team))
      } else {
        shot_df <- shot_df %>% 
          mutate(Defensive_Team = ifelse(Team == Home_Team, Away_Team, Home_Team))
      }
      
      # Filter down by strength input
      if(input$ply_strength == "Even-Strength") {
        shot_df <- shot_df %>% 
          filter(Home_Strength == Away_Strength)    
      } else {
        if(input$ply_strength == "Man-Advantage") {
          shot_df <- shot_df %>% 
            group_by(Game) %>% 
            mutate(Side1_Strength = ifelse(Home_Team == player_list()$Team[player_list()$Player_Name == input$ply_player], 
                                           Home_Strength, Away_Strength), 
                   Side2_Strength = ifelse(Home_Team == player_list()$Team[player_list()$Player_Name == input$ply_player], 
                                           Away_Strength, Home_Strength)) %>% 
            ungroup() %>% 
            mutate(Keep = ifelse((Defensive_Team == player_list()$Team[player_list()$Player_Name == input$ply_player] & Side1_Strength > Side2_Strength) | (Defensive_Team != player_list()$Team[player_list()$Player_Name == input$ply_player] & Side1_Strength < Side2_Strength), "Yes", "No")) %>% 
            filter(Keep == "Yes")
        } else {
          if(input$ply_strength == "Short-Handed") {
            shot_df <- shot_df %>% 
              group_by(Game) %>% 
              mutate(Side1_Strength = ifelse(Home_Team == player_list()$Team[player_list()$Player_Name == input$ply_player], 
                                             Home_Strength, Away_Strength), 
                     Side2_Strength = ifelse(Home_Team == player_list()$Team[player_list()$Player_Name == input$ply_player], 
                                             Away_Strength, Home_Strength)) %>% 
              ungroup() %>% 
              mutate(Keep = ifelse((Defensive_Team == player_list()$Team[player_list()$Player_Name == input$ply_player] & Side1_Strength < Side2_Strength) | (Defensive_Team != player_list()$Team[player_list()$Player_Name == input$ply_player] & Side1_Strength > Side2_Strength), "Yes", "No")) %>% 
              filter(Keep == "Yes")
          }
        }
      }
      
      # Filter depending on Totals or Averages
      if(length(input$ply_total_avg) > 0) {
        if(input$ply_total_avg == "Totals") {
          shot_df <- shot_df %>% 
            group_by(Defensive_Team) %>% 
            summarise(Set = length(which((State == "Set"))),
                      Transition = length(which(State == "Transition")), 
                      Total = length(which(!is.na(State))))
        } else {
          games <- length(unique(shot_df$Game))
          shot_df <- shot_df %>% 
            group_by(Defensive_Team) %>% 
            summarise(Set = round(length(which((State == "Set"))) / games, 1),
                      Transition = round(length(which((State == "Transition"))) / games, 1), 
                      Total = round(length(which(!is.na(State))) / games, 1))
        }
      } else {
        shot_df <- shot_df %>% 
          group_by(Defensive_Team) %>% 
          summarise(Set = length(which((State == "Set"))),
                    Transition = length(which(State == "Transition")), 
                    Total = length(which(!is.na(State))))
      }
      
      shot_df <- shot_df %>% 
        mutate(Set_Label = paste("Set\n", Set, " - ", percent((Set / Total), accuracy = 0.1), sep = ""),
               Transition_Label = paste("Transition\n", Transition, " - ", percent((Transition / Total), accuracy = 0.1), sep = ""))
      
      shot_df <- shot_df %>% 
        filter(Defensive_Team == player_list()$Team[player_list()$Player_Name == input$ply_player])
      
      plot_ly(shot_df, type = "bar", x = ~Transition, y = ~Defensive_Team, name = "Transition", orientation = "h", 
              text = ~Transition_Label, textposition = "inside", insidetextanchor = "middle", textfont = list(size = 15), hoverinfo = "text", 
              marker = list(
                color = df_team_info$Color1[which(df_team_info$Team == player_list()$Team[player_list()$Player_Name == input$ply_player])], 
                line = list(
                  color = "#000000", 
                  width = 2
                )
              )) %>% 
        add_bars(x = ~Set, y = ~Defensive_Team, name = "Set", text = ~Set_Label, textposition = 'inside',  cliponaxis = FALSE, 
                 marker = list(
                   color = df_team_info$Color2[which(df_team_info$Team == player_list()$Team[player_list()$Player_Name == input$ply_player])], 
                   line = list(
                     color = "#000000", 
                     width = 2
                   )
                 )) %>% 
        layout(
          barmode = "stack", 
          height = 150, 
          plot_bgcolor = "#FFFFFF", 
          hoverlabel = list(
            font = list(
              size = 18
            )
          ), 
          yaxis = list(
            title = "", 
            side = "right", 
            visible = FALSE, 
            fixedrange = TRUE
          ), 
          xaxis = list(
            autorange = "reversed", 
            visible = FALSE, 
            fixedrange = TRUE
          ), 
          margin = list(
            r = 95, 
            t = 0, 
            b = 0
          )
        ) %>% 
        hide_legend() %>% 
        config(
          modeBarButtonsToRemove = c('toImage', "zoom2d", "toggleSpikelines", "hoverClosestCartesian", 
                                     "hoverCompareCartesian", "drawline", "autoScale2d", "resetScale2d", 
                                     "zoomIn2d", "zoomOut2d", "pan2d", 'select2d', 'lasso2d')
        )  
    }
  })
  
  ##### PLY: Goalie Creation Bars #####
  
  output$ply_goalie_profile_create <- renderPlotly({
    if(length(input$ply_strength) > 0) {
      
      shot_df <- df_master_fltr() %>% 
        filter(!is.na(Result)) %>% 
        filter((Home_Team == Team & Away_Goalie_Name == input$ply_player) | (Away_Team == Team & Home_Goalie_Name == input$ply_player))
      
      if(length(unique(shot_df$Game)) > 1) {
        shot_df <- shot_df %>% 
          mutate(Team = ifelse(Team != player_list()$Team[player_list()$Player_Name == input$ply_player], "Opponent", Team), 
                 Home_Team = ifelse(Home_Team != player_list()$Team[player_list()$Player_Name == input$ply_player], "Opponent", Home_Team), 
                 Away_Team = ifelse(Away_Team != player_list()$Team[player_list()$Player_Name == input$ply_player], "Opponent", Away_Team)) %>% 
          mutate(Defensive_Team = ifelse(Team == Home_Team, Away_Team, Home_Team))
      } else {
        shot_df <- shot_df %>% 
          mutate(Defensive_Team = ifelse(Team == Home_Team, Away_Team, Home_Team))
      }
      
      # Filter down by strength input
      if(input$ply_strength == "Even-Strength") {
        shot_df <- shot_df %>% 
          filter(Home_Strength == Away_Strength)    
      } else {
        if(input$ply_strength == "Man-Advantage") {
          shot_df <- shot_df %>% 
            group_by(Game) %>% 
            mutate(Side1_Strength = ifelse(Home_Team == player_list()$Team[player_list()$Player_Name == input$ply_player], 
                                           Home_Strength, Away_Strength), 
                   Side2_Strength = ifelse(Home_Team == player_list()$Team[player_list()$Player_Name == input$ply_player], 
                                           Away_Strength, Home_Strength)) %>% 
            ungroup() %>% 
            mutate(Keep = ifelse((Defensive_Team == player_list()$Team[player_list()$Player_Name == input$ply_player] & Side1_Strength > Side2_Strength) | (Defensive_Team != player_list()$Team[player_list()$Player_Name == input$ply_player] & Side1_Strength < Side2_Strength), "Yes", "No")) %>% 
            filter(Keep == "Yes")
        } else {
          if(input$ply_strength == "Short-Handed") {
            shot_df <- shot_df %>% 
              group_by(Game) %>% 
              mutate(Side1_Strength = ifelse(Home_Team == player_list()$Team[player_list()$Player_Name == input$ply_player], 
                                             Home_Strength, Away_Strength), 
                     Side2_Strength = ifelse(Home_Team == player_list()$Team[player_list()$Player_Name == input$ply_player], 
                                             Away_Strength, Home_Strength)) %>% 
              ungroup() %>% 
              mutate(Keep = ifelse((Defensive_Team == player_list()$Team[player_list()$Player_Name == input$ply_player] & Side1_Strength < Side2_Strength) | (Defensive_Team != player_list()$Team[player_list()$Player_Name == input$ply_player] & Side1_Strength > Side2_Strength), "Yes", "No")) %>% 
              filter(Keep == "Yes")
          }
        }
      }
      
      # Filter depending on Totals or Averages
      if(length(input$ply_total_avg) > 0) {
        if(input$ply_total_avg == "Totals") {
          shot_df <- shot_df %>% 
            group_by(Defensive_Team) %>% 
            summarise(Movement = length(which((Shot_Type == "Movement"))),
                      Assisted = length(which(Shot_Type == "Assisted")), 
                      Rebound = length(which(Shot_Type == "Rebound")), 
                      Total = length(which(!is.na(Shot_Type))))
        } else {
          games <- length(unique(shot_df$Game))
          shot_df <- shot_df %>% 
            group_by(Defensive_Team) %>% 
            summarise(Movement = round(length(which((Shot_Type == "Movement"))) / games, 1),
                      Assisted = round(length(which((Shot_Type == "Assisted"))) / games, 1), 
                      Rebound = round(length(which((Shot_Type == "Rebound"))) / games, 1), 
                      Total = round(length(which(!is.na(Shot_Type))) / games, 1))
        }
      } else {
        shot_df <- shot_df %>% 
          group_by(Defensive_Team) %>% 
          summarise(Movement = length(which((Shot_Type == "Movement"))),
                    Assisted = length(which(Shot_Type == "Assisted")), 
                    Rebound = length(which(Shot_Type == "Rebound")), 
                    Total = length(which(!is.na(Shot_Type))))
      }
      
      shot_df <- shot_df %>% 
        mutate(Movement_Label = paste("Movement\n", Movement, " - ", percent((Movement / Total), accuracy = 0.1), sep = ""),
               Assisted_Label = paste("Assisted\n", Assisted, " - ", percent((Assisted / Total), accuracy = 0.1), sep = ""), 
               Rebound_Label = paste("Offensive Rebound\n", Rebound, " - ", percent((Rebound / Total), accuracy = 0.1), sep = ""))
      
      shot_df <- shot_df %>% 
        filter(Defensive_Team == player_list()$Team[player_list()$Player_Name == input$ply_player])
      
      plot_ly(shot_df, type = "bar", x = ~Rebound, y = ~Defensive_Team, name = "Rebound", orientation = "h", 
              text = ~Rebound_Label, textposition = "inside", insidetextanchor = "middle", textfont = list(size = 15), hoverinfo = "text", 
              marker = list(
                color = df_team_info$Color3[which(df_team_info$Team == player_list()$Team[player_list()$Player_Name == input$ply_player])], 
                line = list(
                  color = "#000000", 
                  width = 2
                )
              )) %>% 
        add_bars(x = ~Movement, y = ~Defensive_Team, name = "Movement", text = ~Movement_Label, textposition = 'inside',  
                 marker = list(
                   color = df_team_info$Color2[which(df_team_info$Team == player_list()$Team[player_list()$Player_Name == input$ply_player])], 
                   line = list(
                     color = "#000000", 
                     width = 2
                   )
                 )) %>% 
        add_bars(x = ~Assisted, y = ~Defensive_Team, name = "Assisted", text = ~Assisted_Label, textposition = 'inside', 
                 marker = list(
                   color = df_team_info$Color1[which(df_team_info$Team == player_list()$Team[player_list()$Player_Name == input$ply_player])], 
                   line = list(
                     color = "#000000", 
                     width = 2
                   )
                 )) %>% 
        layout(
          barmode = "stack", 
          height = 150, 
          plot_bgcolor = "#FFFFFF", 
          hoverlabel = list(
            font = list(
              size = 18
            )
          ), 
          yaxis = list(
            title = "", 
            side = "right", 
            visible = FALSE, 
            fixedrange = TRUE
          ), 
          xaxis = list(
            autorange = "reversed", 
            visible = FALSE, 
            fixedrange = TRUE
          ), 
          margin = list(
            r = 95, 
            t = 0, 
            b = 0
          )
        ) %>% 
        hide_legend() %>% 
        config(
          modeBarButtonsToRemove = c('toImage', "zoom2d", "toggleSpikelines", "hoverClosestCartesian", 
                                     "hoverCompareCartesian", "drawline", "autoScale2d", "resetScale2d", 
                                     "zoomIn2d", "zoomOut2d", "pan2d", 'select2d', 'lasso2d')
        )  
    }
  })
  
  ##### PLY: Goalie Placefiller Logo #####
  
  output$ply_goalie_placefiller <- renderUI({
    if(length(input$ply_player) > 0) {
      div(class = "ply_player_title_logo", 
          img(src = df_team_info$Wordmark_Path[which(df_team_info$Team == player_list()$Team[player_list()$Player_Name == input$ply_player])], 
              height = "150px", width = "auto"))
    }
  })
  
  ##### PLY: Goal Plot Title #####
  
  output$ply_goal_title <- renderText({
    if(length(input$ply_player) > 0) {
      if((input$ply_player %in% df_master_fltr()$Home_Goalie_Name) | (input$ply_player %in% df_master_fltr()$Away_Goalie_Name)) {
        paste(toupper(input$ply_player), "SHOTS AGAINST: GOAL VIEW")
      } else {
        paste(toupper(input$ply_player), "SHOTS: GOAL VIEW")
      }
    }
  })
  
  ##### PLY: Goal Plot #####
  
  output$ply_goal <- renderPlotly({
    if(length(input$ply_player) > 0) {
      
      if((input$ply_player %in% df_master_fltr()$Home_Goalie_Name) | (input$ply_player %in% df_master_fltr()$Away_Goalie_Name)) {
        # Shots against if it's a goalie
        shot_df <- df_master_fltr() %>% 
          filter(!is.na(Result)) %>% 
          filter((Home_Team == Team & Away_Goalie_Name == input$ply_player) | (Away_Team == Team & Home_Goalie_Name == input$ply_player)) %>% 
          filter(Empty_Net == "No" & Shot_Type != "Penalty Shot")
        
        if(length(unique(shot_df$Game)) > 1) {
          shot_df <- shot_df %>% 
            mutate(Team = ifelse(Team != player_list()$Team[player_list()$Player_Name == input$ply_player], "Opponent", Team), 
                   Home_Team = ifelse(Home_Team != player_list()$Team[player_list()$Player_Name == input$ply_player], "Opponent", Home_Team), 
                   Away_Team = ifelse(Away_Team != player_list()$Team[player_list()$Player_Name == input$ply_player], "Opponent", Away_Team)) %>% 
            mutate(Defensive_Team = ifelse(Team == Home_Team, Away_Team, Home_Team))
        } else {
          shot_df <- shot_df %>% 
            mutate(Defensive_Team = ifelse(Team == Home_Team, Away_Team, Home_Team))
        }
        
        # Filter down by strength input
        if(input$ply_strength == "Even-Strength") {
          shot_df <- shot_df %>% 
            filter(Home_Strength == Away_Strength)    
        } else {
          if(input$ply_strength == "Man-Advantage") {
            shot_df <- shot_df %>% 
              group_by(Game) %>% 
              mutate(Side1_Strength = ifelse(Home_Team == player_list()$Team[player_list()$Player_Name == input$ply_player], 
                                             Home_Strength, Away_Strength), 
                     Side2_Strength = ifelse(Home_Team == player_list()$Team[player_list()$Player_Name == input$ply_player], 
                                             Away_Strength, Home_Strength)) %>% 
              ungroup() %>% 
              mutate(Keep = ifelse((Defensive_Team == player_list()$Team[player_list()$Player_Name == input$ply_player] & Side1_Strength > Side2_Strength) | (Defensive_Team != player_list()$Team[player_list()$Player_Name == input$ply_player] & Side1_Strength < Side2_Strength), "Yes", "No")) %>% 
              filter(Keep == "Yes")
          } else {
            if(input$ply_strength == "Short-Handed") {
              shot_df <- shot_df %>% 
                group_by(Game) %>% 
                mutate(Side1_Strength = ifelse(Home_Team == player_list()$Team[player_list()$Player_Name == input$ply_player], 
                                               Home_Strength, Away_Strength), 
                       Side2_Strength = ifelse(Home_Team == player_list()$Team[player_list()$Player_Name == input$ply_player], 
                                               Away_Strength, Home_Strength)) %>% 
                ungroup() %>% 
                mutate(Keep = ifelse((Defensive_Team == player_list()$Team[player_list()$Player_Name == input$ply_player] & Side1_Strength < Side2_Strength) | (Defensive_Team != player_list()$Team[player_list()$Player_Name == input$ply_player] & Side1_Strength > Side2_Strength), "Yes", "No")) %>% 
                filter(Keep == "Yes")
            }
          }
        }
      } else {
        # shots by the player if it's a runner
        shot_df <- df_master_fltr() %>% 
          filter(Event_Type == "Shot" & Player1_Name == input$ply_player) %>% 
          filter(Empty_Net == "No" & Shot_Type != "Penalty Shot")
        
        if(length(unique(df_master_fltr()$Game)) > 1) {
          if(input$ply_season != "Cumulative") {
            shot_df <- shot_df %>%
              filter(Season %in% input$ply_season)
          }
        }
        
        # Filter down by strength input
        if(input$ply_strength == "Even-Strength") {
          shot_df <- shot_df %>% 
            filter(Home_Strength == Away_Strength)    
        } else {
          if(input$ply_strength == "Man-Advantage") {
            shot_df <- shot_df %>% 
              group_by(Game) %>% 
              mutate(Side1_Strength = ifelse(Home_Team == player_list()$Team[player_list()$Player_Name == input$ply_player], 
                                             Home_Strength, Away_Strength), 
                     Side2_Strength = ifelse(Home_Team == player_list()$Team[player_list()$Player_Name == input$ply_player], 
                                             Away_Strength, Home_Strength)) %>% 
              ungroup() %>% 
              mutate(Keep = ifelse((Team == player_list()$Team[player_list()$Player_Name == input$ply_player] & Side1_Strength > Side2_Strength) | (Team != player_list()$Team[player_list()$Player_Name == input$ply_player] & Side1_Strength < Side2_Strength), "Yes", "No")) %>% 
              filter(Keep == "Yes")
          } else {
            if(input$ply_strength == "Short-Handed") {
              shot_df <- shot_df %>% 
                group_by(Game) %>% 
                mutate(Side1_Strength = ifelse(Home_Team == player_list()$Team[player_list()$Player_Name == input$ply_player], 
                                               Home_Strength, Away_Strength), 
                       Side2_Strength = ifelse(Home_Team == player_list()$Team[player_list()$Player_Name == input$ply_player], 
                                               Away_Strength, Home_Strength)) %>% 
                ungroup() %>% 
                mutate(Keep = ifelse((Team == player_list()$Team[player_list()$Player_Name == input$ply_player] & Side1_Strength < Side2_Strength) | (Team != player_list()$Team[player_list()$Player_Name == input$ply_player] & Side1_Strength > Side2_Strength), "Yes", "No")) %>% 
                filter(Keep == "Yes")
            }
          }
        }
      }
      
      # Filter by click selections
      if(ply_goal_sel$Net_Location[1] != "none"){
        shot_df <- shot_df %>% 
          filter(Net_Location %in% ply_goal_sel$Net_Location)
      }
      
      if(ply_shotangle_sel$Shot_Angle_Bin[1] != "none") {
        shot_df <- shot_df %>% 
          filter(Shot_Angle != "NA") %>% 
          mutate(Shot_Angle_Bin = ifelse(Shot_Angle == "Dive/Other", "Dive/Other", 
                                         ifelse(as.numeric(Shot_Angle) < 60, "LowRight", 
                                                ifelse(as.numeric(Shot_Angle) < 120, "MidRight", 
                                                       ifelse(as.numeric(Shot_Angle) < 180, "HighRight", 
                                                              ifelse(180 & as.numeric(Shot_Angle) < 240, "HighLeft", 
                                                                     ifelse(as.numeric(Shot_Angle) < 300, "MidLeft", "LowLeft"))))))) %>%
          filter(Shot_Angle_Bin %in% ply_shotangle_sel$Shot_Angle_Bin)
      }
      
      if(ply_field_sel$Field_Location_Bin[1] != "none"){
        shot_df <- shot_df %>% 
          filter(Field_Location_Bin %in% ply_field_sel$Field_Location_Bin)
      }
      
      goal_info.a <- shot_df %>%
        group_by(Net_Location) %>%
        summarise(Shots = length(Result), 
                  Goals = length(Result[Event_Type == "Shot" & Result == "Goal"])) %>%
        mutate(per = Goals / Shots, 
               Percentage = percent(Goals / Shots, accuracy = 0.1))
      goal_info <- left_join(goal_info.b, goal_info.a, by = c("Net_Location"))
      goal_info[is.na(goal_info)] <- 0
      
      if(length(input$ply_total_avg) > 0) {
        if(input$ply_total_avg == "Totals") {
          goal_info <- goal_info %>% 
            mutate(Ratio = ifelse(Net_Location %in% c("Chest", "Crossbar", "Empty Net", "Left Miss", "Left Post", 
                                                      "Right Miss", "Right Post", "Top Miss"), 
                                  paste(Shots), paste(Goals, "/", Shots, sep = "")))
        } else {
          games <- length(unique(shot_df$Game))
          goal_info <- goal_info %>% 
            mutate(Ratio = ifelse(Net_Location %in% c("Chest", "Crossbar", "Empty Net", "Left Miss", "Left Post", 
                                                      "Right Miss", "Right Post", "Top Miss"), 
                                  paste(round((Shots/games), 2)), paste(round((Goals/games), 2), "/", round((Shots/games), 2), sep = "")))
        }
      } else {
        goal_info <- goal_info %>% 
          mutate(Ratio = ifelse(Net_Location %in% c("Chest", "Crossbar", "Empty Net", "Left Miss", "Left Post", 
                                                    "Right Miss", "Right Post", "Top Miss"), 
                                paste(Shots), paste(Goals, "/", Shots, sep = "")))
      }
      
      goal_info <- left_join(goal_info, goal_marker_coords, by = "Net_Location")
      
      goal_info <- goal_info %>% filter(!is.na(Ratio_Loc_x))
      
      goal_info <- rbind(goal_info, 
                         data.frame("Net_Location" = "Filler", "Shots" = 0, "Goals" = 0, "per" = 0, 
                                    "Percentage" = 0, "Ratio" = 0, "Dot_Loc_x" = 2, "Dot_Loc_y" = 2, 
                                    "Ratio_Loc_x" = 2, "Ratio_Loc_y" = 2, "Percentage_Loc_x" = NA, 
                                    "Percentage_Loc_y" = NA, "Goal_Possible" = "No", stringsAsFactors = F))
      
      goal_info <- goal_info %>% 
        mutate(per = ifelse(Goal_Possible == "No", 0, per), 
               per_col = "#000000")
      
      for(i in c(1:nrow(goal_info))) {
        if(goal_info$Shots[i] == 0) {
          goal_info$per_col[i] <- "#000000"
        } else {
          if(goal_info$per[i] >= (shoton_shoot_per*2)) {
            goal_info$per_col[i] <- "#008B00"
          } else {
            shotper_palette <- colorQuantile(palette = c("#CD0000", "#CDCD00", "#008B00"), domain = seq(0, (shoton_shoot_per*2), 0.0001), n = 50)
            shotper_data <- c(0, shoton_shoot_per, (shoton_shoot_per*2), as.numeric(goal_info$per[i]))
            goal_info$per_col[i] <- as.character(shotper_palette(shotper_data)[4])
          }
        }
      }
      
      plot_ly(type = "scatter", x = goal_info$Dot_Loc_x[goal_info$Goal_Possible == "Yes"], 
              y = goal_info$Dot_Loc_y[goal_info$Goal_Possible == "Yes"], hoverinfo = "none", 
              source = "ply_goal_click", key = goal_info$Net_Location[goal_info$Goal_Possible == "Yes"],  
              mode = "markers", 
              marker = list(
                symbol = "circle", 
                size = 80, 
                color = goal_info$per_col[goal_info$Goal_Possible == "Yes"], 
                line = list(
                  color = "white", 
                  width = 3
                )
              )) %>% 
        add_trace(type = "scatter", x = goal_info$Dot_Loc_x[goal_info$Goal_Possible == "No"], 
                  y = goal_info$Dot_Loc_y[goal_info$Goal_Possible == "No"], hoverinfo = "none",
                  source = "ply_goal_click", key = goal_info$Net_Location[goal_info$Goal_Possible == "No"],  
                  mode = "markers", 
                  marker = list(
                    symbol = "square",
                    size = 50, 
                    color = "#000000", 
                    line = list(
                      color = "white", 
                      width = 3
                    )
                  )) %>%
        hide_colorbar() %>% 
        hide_legend() %>% 
        layout(
          images = list(
            list(
              source = base64enc::dataURI(file = goal_base), 
              xref = "x",
              yref = "y",
              x = -0.04,
              y = 1.01,
              sizex = 1.08,
              sizey = 1.02,
              sizing = "stretch",
              layer = "below"
            )
          ), 
          margin = list(
            t = 0, 
            b = 0, 
            l = 0, 
            r = 0, 
            pad = 0
          ), 
          xaxis = list(
            range = c(-0.1, 1.1), 
            visible = F, 
            fixedrange = TRUE
          ),
          yaxis = list(
            range = c(-0.1, 1.1),
            visible = F, 
            scaleanchor = "x", 
            scaleratio = 0.7013, 
            fixedrange = TRUE
          ), 
          dragmode = F
        ) %>% 
        config(
          modeBarButtonsToRemove = c('toImage', "zoom2d", "toggleSpikelines", "hoverClosestCartesian", 
                                     "hoverCompareCartesian", "drawline", "autoScale2d", "resetScale2d", 
                                     "zoomIn2d", "zoomOut2d", "pan2d", 'select2d', 'lasso2d')
        ) %>%
        add_annotations(x = goal_info$Ratio_Loc_x,
                        y = goal_info$Ratio_Loc_y,
                        text = goal_info$Ratio,
                        xref = "x",
                        yref = "y",
                        showarrow = F,
                        # Styling annotations' text:
                        font = list(color = "white",
                                    size = 13)
        ) %>%
        add_annotations(x = goal_info$Percentage_Loc_x[!is.na(goal_info$Percentage_Loc_x)],
                        y = goal_info$Percentage_Loc_y[!is.na(goal_info$Percentage_Loc_x)],
                        text = goal_info$Percentage[!is.na(goal_info$Percentage_Loc_x)],
                        xref = "x",
                        yref = "y",
                        showarrow = F,
                        # Styling annotations' text:
                        font = list(color = "white",
                                    size = 13)
        ) %>% 
        event_register("plotly_click")
      
    }
  })
  
  ##### PLY: Field Plot Title #####
  
  output$ply_field_title <- renderText({
    if(length(input$ply_player) > 0) {
      if((input$ply_player %in% df_master_fltr()$Home_Goalie_Name) | (input$ply_player %in% df_master_fltr()$Away_Goalie_Name)) {
        paste(toupper(input$ply_player), "SHOTS AGAINST: FIELD VIEW")
      } else {
        paste(toupper(input$ply_player), "SHOTS: FIELD VIEW")
      }
    }
  })
  
  ##### PLY: Field Plot #####
  
  output$ply_field <- renderPlotly({
    if(length(input$ply_player) > 0){
      
      if((input$ply_player %in% df_master_fltr()$Home_Goalie_Name) | (input$ply_player %in% df_master_fltr()$Away_Goalie_Name)) {
        # Shots against if it's a goalie
        shot_df <- df_master_fltr() %>% 
          filter(!is.na(Result)) %>% 
          filter((Home_Team == Team & Away_Goalie_Name == input$ply_player) | (Away_Team == Team & Home_Goalie_Name == input$ply_player)) %>% 
          filter(Empty_Net == "No" & Shot_Type != "Penalty Shot")
        
        if(length(unique(shot_df$Game)) > 1) {
          shot_df <- shot_df %>% 
            mutate(Team = ifelse(Team != player_list()$Team[player_list()$Player_Name == input$ply_player], "Opponent", Team), 
                   Home_Team = ifelse(Home_Team != player_list()$Team[player_list()$Player_Name == input$ply_player], "Opponent", Home_Team), 
                   Away_Team = ifelse(Away_Team != player_list()$Team[player_list()$Player_Name == input$ply_player], "Opponent", Away_Team)) %>% 
            mutate(Defensive_Team = ifelse(Team == Home_Team, Away_Team, Home_Team))
        } else {
          shot_df <- shot_df %>% 
            mutate(Defensive_Team = ifelse(Team == Home_Team, Away_Team, Home_Team))
        }
        
        # Filter down by strength input
        if(input$ply_strength == "Even-Strength") {
          shot_df <- shot_df %>% 
            filter(Home_Strength == Away_Strength)    
        } else {
          if(input$ply_strength == "Man-Advantage") {
            shot_df <- shot_df %>% 
              group_by(Game) %>% 
              mutate(Side1_Strength = ifelse(Home_Team == player_list()$Team[player_list()$Player_Name == input$ply_player], 
                                             Home_Strength, Away_Strength), 
                     Side2_Strength = ifelse(Home_Team == player_list()$Team[player_list()$Player_Name == input$ply_player], 
                                             Away_Strength, Home_Strength)) %>% 
              ungroup() %>% 
              mutate(Keep = ifelse((Defensive_Team == player_list()$Team[player_list()$Player_Name == input$ply_player] & Side1_Strength > Side2_Strength) | (Defensive_Team != player_list()$Team[player_list()$Player_Name == input$ply_player] & Side1_Strength < Side2_Strength), "Yes", "No")) %>% 
              filter(Keep == "Yes")
          } else {
            if(input$ply_strength == "Short-Handed") {
              shot_df <- shot_df %>% 
                group_by(Game) %>% 
                mutate(Side1_Strength = ifelse(Home_Team == player_list()$Team[player_list()$Player_Name == input$ply_player], 
                                               Home_Strength, Away_Strength), 
                       Side2_Strength = ifelse(Home_Team == player_list()$Team[player_list()$Player_Name == input$ply_player], 
                                               Away_Strength, Home_Strength)) %>% 
                ungroup() %>% 
                mutate(Keep = ifelse((Defensive_Team == player_list()$Team[player_list()$Player_Name == input$ply_player] & Side1_Strength < Side2_Strength) | (Defensive_Team != player_list()$Team[player_list()$Player_Name == input$ply_player] & Side1_Strength > Side2_Strength), "Yes", "No")) %>% 
                filter(Keep == "Yes")
            }
          }
        }
      } else {
        # shots by the player if it's a runner
        shot_df <- df_master_fltr() %>% 
          filter(Event_Type == "Shot" & Player1_Name == input$ply_player) %>% 
          filter(Empty_Net == "No" & Shot_Type != "Penalty Shot")
        
        if(length(unique(df_master_fltr()$Game)) > 1) {
          if(input$ply_season != "Cumulative") {
            shot_df <- shot_df %>%
              filter(Season %in% input$ply_season)
          }
        }
        
        # Filter down by strength input
        if(input$ply_strength == "Even-Strength") {
          shot_df <- shot_df %>% 
            filter(Home_Strength == Away_Strength)    
        } else {
          if(input$ply_strength == "Man-Advantage") {
            shot_df <- shot_df %>% 
              group_by(Game) %>% 
              mutate(Side1_Strength = ifelse(Home_Team == player_list()$Team[player_list()$Player_Name == input$ply_player], 
                                             Home_Strength, Away_Strength), 
                     Side2_Strength = ifelse(Home_Team == player_list()$Team[player_list()$Player_Name == input$ply_player], 
                                             Away_Strength, Home_Strength)) %>% 
              ungroup() %>% 
              mutate(Keep = ifelse((Team == player_list()$Team[player_list()$Player_Name == input$ply_player] & Side1_Strength > Side2_Strength) | (Team != player_list()$Team[player_list()$Player_Name == input$ply_player] & Side1_Strength < Side2_Strength), "Yes", "No")) %>% 
              filter(Keep == "Yes")
          } else {
            if(input$ply_strength == "Short-Handed") {
              shot_df <- shot_df %>% 
                group_by(Game) %>% 
                mutate(Side1_Strength = ifelse(Home_Team == player_list()$Team[player_list()$Player_Name == input$ply_player], 
                                               Home_Strength, Away_Strength), 
                       Side2_Strength = ifelse(Home_Team == player_list()$Team[player_list()$Player_Name == input$ply_player], 
                                               Away_Strength, Home_Strength)) %>% 
                ungroup() %>% 
                mutate(Keep = ifelse((Team == player_list()$Team[player_list()$Player_Name == input$ply_player] & Side1_Strength < Side2_Strength) | (Team != player_list()$Team[player_list()$Player_Name == input$ply_player] & Side1_Strength > Side2_Strength), "Yes", "No")) %>% 
                filter(Keep == "Yes")
            }
          }
        }
      } 
      
      # Filter by click selections
      if(ply_goal_sel$Net_Location[1] != "none"){
        shot_df <- shot_df %>% 
          filter(Net_Location %in% ply_goal_sel$Net_Location)
      }
      
      if(ply_shotangle_sel$Shot_Angle_Bin[1] != "none") {
        shot_df <- shot_df %>% 
          filter(Shot_Angle != "NA") %>% 
          mutate(Shot_Angle_Bin = ifelse(Shot_Angle == "Dive/Other", "Dive/Other", 
                                         ifelse(as.numeric(Shot_Angle) < 60, "LowRight", 
                                                ifelse(as.numeric(Shot_Angle) < 120, "MidRight", 
                                                       ifelse(as.numeric(Shot_Angle) < 180, "HighRight", 
                                                              ifelse(180 & as.numeric(Shot_Angle) < 240, "HighLeft", 
                                                                     ifelse(as.numeric(Shot_Angle) < 300, "MidLeft", "LowLeft"))))))) %>%
          filter(Shot_Angle_Bin %in% ply_shotangle_sel$Shot_Angle_Bin)
      }
      
      if(ply_field_sel$Field_Location_Bin[1] != "none"){
        shot_df <- shot_df %>% 
          filter(Field_Location_Bin %in% ply_field_sel$Field_Location_Bin)
      }
      
      if(length(input$ply_total_avg) > 0) {
        if(input$ply_total_avg == "Totals") {
          field_df.a <- shot_df %>% 
            group_by(Field_Location_Bin) %>% 
            summarise(Goals = length(Game_Clock[Result == "Goal"]), 
                      Shots = length(Game_Clock)) %>% 
            ungroup() %>% 
            mutate(per = Goals / Shots, 
                   Ratio = paste(Goals, "/", Shots, sep = ""), 
                   Percentage = percent(per, accuracy = 0.1))
        } else {
          games <- length(unique(shot_df$Game))
          field_df.a <- shot_df %>% 
            group_by(Field_Location_Bin) %>% 
            summarise(Goals = length(Game_Clock[Result == "Goal"]), 
                      Shots = length(Game_Clock)) %>% 
            ungroup() %>% 
            mutate(per = Goals / Shots, 
                   Ratio = paste(round((Goals/games), 1), "/", round((Shots/games), 1), sep = ""), 
                   Percentage = percent(per, accuracy = 0.1))
        }
      } else {
        field_df.a <- shot_df %>% 
          group_by(Field_Location_Bin) %>% 
          summarise(Goals = length(Game_Clock[Result == "Goal"]), 
                    Shots = length(Game_Clock)) %>% 
          ungroup() %>% 
          mutate(per = Goals / Shots, 
                 Ratio = paste(Goals, "/", Shots, sep = ""), 
                 Percentage = percent(per, accuracy = 0.1))
      }
      
      field_df <- left_join(field_df.b, field_df.a, by = c("Field_Location_Bin"))
      
      field_df <- left_join(field_df, field_marker_coords, by = "Field_Location_Bin")
      
      field_df$Percentage_Loc_x <- field_df$Ratio_Loc_x
      field_df$Percentage_Loc_y <- field_df$Ratio_Loc_y - 0.04
      field_df$Dot_Loc_x <- field_df$Ratio_Loc_x
      field_df$Dot_Loc_y <- field_df$Ratio_Loc_y - 0.02
      field_df[is.na(field_df)] <- 0
      field_df <- field_df %>% 
        mutate(Percentage = ifelse(Percentage == "0", "", Percentage), 
               per_col = "#000000")
      
      for(i in c(1:nrow(field_df))) {
        if(field_df$Shots[i] == 0) {
          field_df$per_col[i] <- "#000000"
        } else {
          if(field_df$per[i] >= (gen_shoot_per*2)) {
            field_df$per_col[i] <- "#008B00"
          } else {
            shotper_palette <- colorQuantile(palette = c("#CD0000", "#CDCD00", "#008B00"), domain = seq(0, (gen_shoot_per*2), 0.0001), n = 50)
            shotper_data <- c(0, gen_shoot_per, (gen_shoot_per*2), as.numeric(field_df$per[i]))
            field_df$per_col[i] <- as.character(shotper_palette(shotper_data)[4])
          }
        }
      }
      
      plot_ly(type = "scatter", x = field_df$Dot_Loc_x, y = field_df$Dot_Loc_y, hoverinfo = "none", 
              source = "ply_field_click", key = field_df$Field_Location_Bin, 
              mode = "markers", 
              marker = list(
                symbol = "square", 
                size = 60, 
                color = field_df$per_col, 
                line = list(
                  color = "white", 
                  width = 2
                )
              )) %>% 
        hide_colorbar() %>% 
        layout(
          images = list(
            list(
              source = base64enc::dataURI(file = field_grid), 
              xref = "x",
              yref = "y",
              x = -0.036,
              y = 1.027,
              sizex = 1.08,
              sizey = 1.05,
              sizing = "stretch",
              layer = "below"
            )
          ), 
          margin = list(
            t = 0, 
            b = 0, 
            l = 0, 
            r = 0, 
            pad = 0
          ), 
          xaxis = list(
            range = c(-0.1, 1.1), 
            visible = F, 
            fixedrange = TRUE
          ),
          yaxis = list(
            range = c(-0.1, 1.1),
            visible = F, 
            scaleanchor = "x", 
            scaleratio = 1.125, 
            fixedrange = TRUE
          ), 
          dragmode = F
        ) %>% 
        config(
          modeBarButtonsToRemove = c('toImage', "zoom2d", "toggleSpikelines", "hoverClosestCartesian", 
                                     "hoverCompareCartesian", "drawline", "autoScale2d", "resetScale2d", 
                                     "zoomIn2d", "zoomOut2d", "pan2d", 'select2d', 'lasso2d')
        ) %>%
        add_annotations(x = field_df$Ratio_Loc_x,
                        y = field_df$Ratio_Loc_y,
                        text = field_df$Ratio,
                        xref = "x",
                        yref = "y",
                        showarrow = F,
                        # Styling annotations' text:
                        font = list(color = "white",
                                    size = 13)
        ) %>% 
        add_annotations(x = field_df$Percentage_Loc_x,
                        y = field_df$Percentage_Loc_y,
                        text = field_df$Percentage,
                        xref = "x",
                        yref = "y",
                        showarrow = F,
                        # Styling annotations' text:
                        font = list(color = "white",
                                    size = 13)
        ) %>% 
        event_register("plotly_click")
      
    }
  })
  
  ##### PLY: Shot Angle Plot Title #####
  
  output$ply_shotangle_title <- renderText({
    if(length(input$ply_player) > 0) {
      if((input$ply_player %in% df_master_fltr()$Home_Goalie_Name) | (input$ply_player %in% df_master_fltr()$Away_Goalie_Name)) {
        paste(toupper(input$ply_player), "SHOTS AGAINST: SHOT ANGLE VIEW")
      } else {
        paste(toupper(input$ply_player), "SHOTS: SHOT ANGLE VIEW")
      }
    }
  })
  
  ##### PLY: Shot Angle Plot #####
  
  output$ply_shotangle <- renderPlotly({
    if(length(input$ply_player) > 0) {
      
      if((input$ply_player %in% df_master_fltr()$Home_Goalie_Name) | (input$ply_player %in% df_master_fltr()$Away_Goalie_Name)) {
        # Shots against if it's a goalie
        shot_df <- df_master_fltr() %>% 
          filter(!is.na(Result)) %>% 
          filter((Home_Team == Team & Away_Goalie_Name == input$ply_player) | (Away_Team == Team & Home_Goalie_Name == input$ply_player)) %>% 
          filter(Empty_Net == "No" & Shot_Type != "Penalty Shot")
        
        if(length(unique(shot_df$Game)) > 1) {
          shot_df <- shot_df %>% 
            mutate(Team = ifelse(Team != player_list()$Team[player_list()$Player_Name == input$ply_player], "Opponent", Team), 
                   Home_Team = ifelse(Home_Team != player_list()$Team[player_list()$Player_Name == input$ply_player], "Opponent", Home_Team), 
                   Away_Team = ifelse(Away_Team != player_list()$Team[player_list()$Player_Name == input$ply_player], "Opponent", Away_Team)) %>% 
            mutate(Defensive_Team = ifelse(Team == Home_Team, Away_Team, Home_Team))
        } else {
          shot_df <- shot_df %>% 
            mutate(Defensive_Team = ifelse(Team == Home_Team, Away_Team, Home_Team))
        }
        
        # Filter down by strength input
        if(input$ply_strength == "Even-Strength") {
          shot_df <- shot_df %>% 
            filter(Home_Strength == Away_Strength)    
        } else {
          if(input$ply_strength == "Man-Advantage") {
            shot_df <- shot_df %>% 
              group_by(Game) %>% 
              mutate(Side1_Strength = ifelse(Home_Team == player_list()$Team[player_list()$Player_Name == input$ply_player], 
                                             Home_Strength, Away_Strength), 
                     Side2_Strength = ifelse(Home_Team == player_list()$Team[player_list()$Player_Name == input$ply_player], 
                                             Away_Strength, Home_Strength)) %>% 
              ungroup() %>% 
              mutate(Keep = ifelse((Defensive_Team == player_list()$Team[player_list()$Player_Name == input$ply_player] & Side1_Strength > Side2_Strength) | (Defensive_Team != player_list()$Team[player_list()$Player_Name == input$ply_player] & Side1_Strength < Side2_Strength), "Yes", "No")) %>% 
              filter(Keep == "Yes")
          } else {
            if(input$ply_strength == "Short-Handed") {
              shot_df <- shot_df %>% 
                group_by(Game) %>% 
                mutate(Side1_Strength = ifelse(Home_Team == player_list()$Team[player_list()$Player_Name == input$ply_player], 
                                               Home_Strength, Away_Strength), 
                       Side2_Strength = ifelse(Home_Team == player_list()$Team[player_list()$Player_Name == input$ply_player], 
                                               Away_Strength, Home_Strength)) %>% 
                ungroup() %>% 
                mutate(Keep = ifelse((Defensive_Team == player_list()$Team[player_list()$Player_Name == input$ply_player] & Side1_Strength < Side2_Strength) | (Defensive_Team != player_list()$Team[player_list()$Player_Name == input$ply_player] & Side1_Strength > Side2_Strength), "Yes", "No")) %>% 
                filter(Keep == "Yes")
            }
          }
        }
      } else {
        # shots by the player if it's a runner
        shot_df <- df_master_fltr() %>% 
          filter(Event_Type == "Shot" & Player1_Name == input$ply_player) %>% 
          filter(Empty_Net == "No" & Shot_Type != "Penalty Shot")
        
        if(length(unique(df_master_fltr()$Game)) > 1) {
          if(input$ply_season != "Cumulative") {
            shot_df <- shot_df %>%
              filter(Season %in% input$ply_season)
          }
        }
        
        # Filter down by strength input
        if(input$ply_strength == "Even-Strength") {
          shot_df <- shot_df %>% 
            filter(Home_Strength == Away_Strength)    
        } else {
          if(input$ply_strength == "Man-Advantage") {
            shot_df <- shot_df %>% 
              group_by(Game) %>% 
              mutate(Side1_Strength = ifelse(Home_Team == player_list()$Team[player_list()$Player_Name == input$ply_player], 
                                             Home_Strength, Away_Strength), 
                     Side2_Strength = ifelse(Home_Team == player_list()$Team[player_list()$Player_Name == input$ply_player], 
                                             Away_Strength, Home_Strength)) %>% 
              ungroup() %>% 
              mutate(Keep = ifelse((Team == player_list()$Team[player_list()$Player_Name == input$ply_player] & Side1_Strength > Side2_Strength) | (Team != player_list()$Team[player_list()$Player_Name == input$ply_player] & Side1_Strength < Side2_Strength), "Yes", "No")) %>% 
              filter(Keep == "Yes")
          } else {
            if(input$ply_strength == "Short-Handed") {
              shot_df <- shot_df %>% 
                group_by(Game) %>% 
                mutate(Side1_Strength = ifelse(Home_Team == player_list()$Team[player_list()$Player_Name == input$ply_player], 
                                               Home_Strength, Away_Strength), 
                       Side2_Strength = ifelse(Home_Team == player_list()$Team[player_list()$Player_Name == input$ply_player], 
                                               Away_Strength, Home_Strength)) %>% 
                ungroup() %>% 
                mutate(Keep = ifelse((Team == player_list()$Team[player_list()$Player_Name == input$ply_player] & Side1_Strength < Side2_Strength) | (Team != player_list()$Team[player_list()$Player_Name == input$ply_player] & Side1_Strength > Side2_Strength), "Yes", "No")) %>% 
                filter(Keep == "Yes")
            }
          }
        }
      } 
      
      # Filter by click selections
      if(ply_goal_sel$Net_Location[1] != "none"){
        shot_df <- shot_df %>% 
          filter(Net_Location %in% ply_goal_sel$Net_Location)
      }
      
      if(ply_shotangle_sel$Shot_Angle_Bin[1] != "none") {
        shot_df <- shot_df %>% 
          filter(Shot_Angle != "NA") %>% 
          mutate(Shot_Angle_Bin = ifelse(Shot_Angle == "Dive/Other", "Dive/Other", 
                                         ifelse(as.numeric(Shot_Angle) < 60, "LowRight", 
                                                ifelse(as.numeric(Shot_Angle) < 120, "MidRight", 
                                                       ifelse(as.numeric(Shot_Angle) < 180, "HighRight", 
                                                              ifelse(180 & as.numeric(Shot_Angle) < 240, "HighLeft", 
                                                                     ifelse(as.numeric(Shot_Angle) < 300, "MidLeft", "LowLeft"))))))) %>%
          filter(Shot_Angle_Bin %in% ply_shotangle_sel$Shot_Angle_Bin)
      }
      
      if(ply_field_sel$Field_Location_Bin[1] != "none"){
        shot_df <- shot_df %>% 
          filter(Field_Location_Bin %in% ply_field_sel$Field_Location_Bin)
      }
      
      shotangle_info.a <- shot_df %>% 
        filter(Shot_Angle != "NA") %>% 
        mutate(Shot_Angle_Bin = ifelse(Shot_Angle == "Dive/Other", "Dive/Other", 
                                       ifelse(as.numeric(Shot_Angle) < 60, "LowRight", 
                                              ifelse(as.numeric(Shot_Angle) < 120, "MidRight", 
                                                     ifelse(as.numeric(Shot_Angle) < 180, "HighRight", 
                                                            ifelse(180 & as.numeric(Shot_Angle) < 240, "HighLeft", 
                                                                   ifelse(as.numeric(Shot_Angle) < 300, "MidLeft", "LowLeft")))))))
      
      shotangle_info.a <- shotangle_info.a %>%
        group_by(Shot_Angle_Bin) %>%
        summarise(Shots = length(Result), 
                  Goals = length(Result[Event_Type == "Shot" & Result == "Goal"])) %>%
        mutate(per = Goals / Shots, 
               Percentage = percent(Goals / Shots, accuracy = 0.1))
      shotangle_info.a$Shot_Angle_Bin <- as.character(shotangle_info.a$Shot_Angle_Bin)
      shotangle_info <- left_join(shotangle_info.b, shotangle_info.a, by = c("Shot_Angle_Bin"))
      shotangle_info[is.na(shotangle_info)] <- 0
      
      if(length(input$scoring_total_avg) > 0) {
        if(input$scoring_total_avg == "Totals") {
          shotangle_info <- shotangle_info %>% 
            mutate(Ratio = paste(Goals, "/", Shots, sep = ""))
        } else {
          games <- length(unique(shot_df$Game))
          shotangle_info <- shotangle_info %>% 
            mutate(Ratio = paste(round((Goals/games), 2), "/", round((Shots/games), 2), sep = ""))
        }
      } else {
        shotangle_info <- shotangle_info %>% 
          mutate(Ratio = paste(Goals, "/", Shots, sep = ""))
      }
      
      shotangle_info$Shot_Angle_Bin <- as.character(shotangle_info$Shot_Angle_Bin)
      shotangle_info <- left_join(shotangle_info, shot_angle_coords, by = "Shot_Angle_Bin")
      
      shotangle_info <- shotangle_info %>% 
        mutate(per_col = "#000000")
      
      for(i in c(1:nrow(shotangle_info))) {
        if(shotangle_info$Shots[i] == 0) {
          shotangle_info$per_col[i] <- "#000000"
        } else {
          if(shotangle_info$per[i] >= (gen_shoot_per*2)) {
            shotangle_info$per_col[i] <- "#008B00"
          } else {
            shotper_palette <- colorQuantile(palette = c("#CD0000", "#CDCD00", "#008B00"), domain = seq(0, (gen_shoot_per*2), 0.0001), n = 50)
            shotper_data <- c(0, gen_shoot_per, (gen_shoot_per*2), as.numeric(shotangle_info$per[i]))
            shotangle_info$per_col[i] <- as.character(shotper_palette(shotper_data)[4])
          }
        }
      }
      
      plot_ly(type = "scatter", x = shotangle_info$Dot_Loc_x, y = shotangle_info$Dot_Loc_y, hoverinfo = "none", 
              source = "ply_shotangle_click", key = shotangle_info$Shot_Angle_Bin,  
              mode = "markers", 
              marker = list(
                symbol = "circle", 
                size = 80, 
                color = shotangle_info$per_col, 
                line = list(
                  color = "#A2002A", 
                  width = 3
                )
              )) %>% 
        hide_colorbar() %>% 
        hide_legend() %>% 
        layout(
          images = list(
            list(
              source = base64enc::dataURI(file = shot_angle), 
              xref = "x",
              yref = "y",
              x = -0.04,
              y = 1.01,
              sizex = 1.08,
              sizey = 1.02,
              sizing = "stretch",
              layer = "below"
            )
          ), 
          margin = list(
            t = 0, 
            b = 0, 
            l = 0, 
            r = 0, 
            pad = 0
          ), 
          xaxis = list(
            range = c(-0.1, 1.1), 
            visible = F, 
            fixedrange = TRUE
          ),
          yaxis = list(
            range = c(-0.1, 1.1),
            visible = F, 
            scaleanchor = "x", 
            scaleratio = 1, 
            fixedrange = TRUE
          ), 
          dragmode = F
        ) %>% 
        config(
          modeBarButtonsToRemove = c('toImage', "zoom2d", "toggleSpikelines", "hoverClosestCartesian", 
                                     "hoverCompareCartesian", "drawline", "autoScale2d", "resetScale2d", 
                                     "zoomIn2d", "zoomOut2d", "pan2d", 'select2d', 'lasso2d')
        ) %>%
        add_annotations(x = shotangle_info$Dot_Loc_x ,
                        y = shotangle_info$Dot_Loc_y + 0.025,
                        text = shotangle_info$Ratio,
                        xref = "x",
                        yref = "y",
                        showarrow = F,
                        # Styling annotations' text:
                        font = list(color = "white",
                                    size = 13)
        ) %>%
        add_annotations(x = shotangle_info$Dot_Loc_x,
                        y = shotangle_info$Dot_Loc_y - 0.025,
                        text = shotangle_info$Percentage,
                        xref = "x",
                        yref = "y",
                        showarrow = F,
                        # Styling annotations' text:
                        font = list(color = "white",
                                    size = 13)
        ) %>% 
        event_register("plotly_click")
      
    }
  })
  
  ##### PLY: Data Filter Timestamps #####
  
  output$ply_timestamps <- renderDataTable({
    if(length(input$ply_player) > 0) {
      if((input$ply_player %in% df_master_fltr()$Home_Goalie_Name) | (input$ply_player %in% df_master_fltr()$Away_Goalie_Name)) {
        # Shots against if it's a goalie
        shot_df <- df_master_fltr() %>% 
          filter(!is.na(Result)) %>% 
          filter((Home_Team == Team & Away_Goalie_Name == input$ply_player) | (Away_Team == Team & Home_Goalie_Name == input$ply_player)) %>% 
          filter(Empty_Net == "No" & Shot_Type != "Penalty Shot")
        
        if(length(unique(shot_df$Game)) > 1) {
          shot_df <- shot_df %>% 
            mutate(Team = ifelse(Team != player_list()$Team[player_list()$Player_Name == input$ply_player], "Opponent", Team), 
                   Home_Team = ifelse(Home_Team != player_list()$Team[player_list()$Player_Name == input$ply_player], "Opponent", Home_Team), 
                   Away_Team = ifelse(Away_Team != player_list()$Team[player_list()$Player_Name == input$ply_player], "Opponent", Away_Team)) %>% 
            mutate(Defensive_Team = ifelse(Team == Home_Team, Away_Team, Home_Team))
        } else {
          shot_df <- shot_df %>% 
            mutate(Defensive_Team = ifelse(Team == Home_Team, Away_Team, Home_Team))
        }
        
        # Filter down by strength input
        if(input$ply_strength == "Even-Strength") {
          shot_df <- shot_df %>% 
            filter(Home_Strength == Away_Strength)    
        } else {
          if(input$ply_strength == "Man-Advantage") {
            shot_df <- shot_df %>% 
              group_by(Game) %>% 
              mutate(Side1_Strength = ifelse(Home_Team == player_list()$Team[player_list()$Player_Name == input$ply_player], 
                                             Home_Strength, Away_Strength), 
                     Side2_Strength = ifelse(Home_Team == player_list()$Team[player_list()$Player_Name == input$ply_player], 
                                             Away_Strength, Home_Strength)) %>% 
              ungroup() %>% 
              mutate(Keep = ifelse((Defensive_Team == player_list()$Team[player_list()$Player_Name == input$ply_player] & Side1_Strength > Side2_Strength) | (Defensive_Team != player_list()$Team[player_list()$Player_Name == input$ply_player] & Side1_Strength < Side2_Strength), "Yes", "No")) %>% 
              filter(Keep == "Yes")
          } else {
            if(input$ply_strength == "Short-Handed") {
              shot_df <- shot_df %>% 
                group_by(Game) %>% 
                mutate(Side1_Strength = ifelse(Home_Team == player_list()$Team[player_list()$Player_Name == input$ply_player], 
                                               Home_Strength, Away_Strength), 
                       Side2_Strength = ifelse(Home_Team == player_list()$Team[player_list()$Player_Name == input$ply_player], 
                                               Away_Strength, Home_Strength)) %>% 
                ungroup() %>% 
                mutate(Keep = ifelse((Defensive_Team == player_list()$Team[player_list()$Player_Name == input$ply_player] & Side1_Strength < Side2_Strength) | (Defensive_Team != player_list()$Team[player_list()$Player_Name == input$ply_player] & Side1_Strength > Side2_Strength), "Yes", "No")) %>% 
                filter(Keep == "Yes")
            }
          }
        }
      } else {
        # shots by the player if it's a runner
        shot_df <- df_master_fltr() %>% 
          filter(Event_Type == "Shot" & Player1_Name == input$ply_player) %>% 
          filter(Empty_Net == "No" & Shot_Type != "Penalty Shot")
        
        if(length(unique(df_master_fltr()$Game)) > 1) {
          if(input$ply_season != "Cumulative") {
            shot_df <- shot_df %>%
              filter(Season %in% input$ply_season)
          }
        }
        
        # Filter down by strength input
        if(input$ply_strength == "Even-Strength") {
          shot_df <- shot_df %>% 
            filter(Home_Strength == Away_Strength)    
        } else {
          if(input$ply_strength == "Man-Advantage") {
            shot_df <- shot_df %>% 
              group_by(Game) %>% 
              mutate(Side1_Strength = ifelse(Home_Team == player_list()$Team[player_list()$Player_Name == input$ply_player], 
                                             Home_Strength, Away_Strength), 
                     Side2_Strength = ifelse(Home_Team == player_list()$Team[player_list()$Player_Name == input$ply_player], 
                                             Away_Strength, Home_Strength)) %>% 
              ungroup() %>% 
              mutate(Keep = ifelse((Team == player_list()$Team[player_list()$Player_Name == input$ply_player] & Side1_Strength > Side2_Strength) | (Team != player_list()$Team[player_list()$Player_Name == input$ply_player] & Side1_Strength < Side2_Strength), "Yes", "No")) %>% 
              filter(Keep == "Yes")
          } else {
            if(input$ply_strength == "Short-Handed") {
              shot_df <- shot_df %>% 
                group_by(Game) %>% 
                mutate(Side1_Strength = ifelse(Home_Team == player_list()$Team[player_list()$Player_Name == input$ply_player], 
                                               Home_Strength, Away_Strength), 
                       Side2_Strength = ifelse(Home_Team == player_list()$Team[player_list()$Player_Name == input$ply_player], 
                                               Away_Strength, Home_Strength)) %>% 
                ungroup() %>% 
                mutate(Keep = ifelse((Team == player_list()$Team[player_list()$Player_Name == input$ply_player] & Side1_Strength < Side2_Strength) | (Team != player_list()$Team[player_list()$Player_Name == input$ply_player] & Side1_Strength > Side2_Strength), "Yes", "No")) %>% 
                filter(Keep == "Yes")
            }
          }
        }
      } 
      
      # Filter by click selections
      if(ply_goal_sel$Net_Location[1] != "none"){
        shot_df <- shot_df %>% 
          filter(Net_Location %in% ply_goal_sel$Net_Location)
      }
      
      if(ply_shotangle_sel$Shot_Angle_Bin[1] != "none") {
        shot_df <- shot_df %>% 
          filter(Shot_Angle != "NA") %>% 
          mutate(Shot_Angle_Bin = ifelse(Shot_Angle == "Dive/Other", "Dive/Other", 
                                         ifelse(as.numeric(Shot_Angle) < 60, "LowRight", 
                                                ifelse(as.numeric(Shot_Angle) < 120, "MidRight", 
                                                       ifelse(as.numeric(Shot_Angle) < 180, "HighRight", 
                                                              ifelse(180 & as.numeric(Shot_Angle) < 240, "HighLeft", 
                                                                     ifelse(as.numeric(Shot_Angle) < 300, "MidLeft", "LowLeft"))))))) %>%
          filter(Shot_Angle_Bin %in% ply_shotangle_sel$Shot_Angle_Bin)
      }
      
      if(ply_field_sel$Field_Location_Bin[1] != "none"){
        shot_df <- shot_df %>% 
          filter(Field_Location_Bin %in% ply_field_sel$Field_Location_Bin)
      }
      
      shot_df <- shot_df %>% 
        select(Game, Quarter, Game_Clock, Player1_Name, Result, Goalie_Response, Shot_Type, 
               Primary_Assist_Name, Secondary_Assist_Name, Rebound_Type, Rebound_Name) %>%
        rename('Timestamp' = Game_Clock, 
               'Shooter' = Player1_Name, 
               'Goalie Response' = Goalie_Response, 
               'Creation' = Shot_Type, 
               'Prim. Assist' = Primary_Assist_Name, 
               'Sec. Assist' = Secondary_Assist_Name, 
               'Reb.' = Rebound_Type, 
               'Reb. Player' = Rebound_Name)
      
      datatable(shot_df, 
                options = list(
                  paging = TRUE,
                  pageLength = 10, 
                  autowidth = FALSE, 
                  searching = FALSE, 
                  info = FALSE, 
                  scrollX = TRUE
                ), 
                rownames = FALSE
      )
    }
  })
  
}



shinyApp(ui, server)