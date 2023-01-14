library(shiny)
library(dplyr)
library(ggplot2)
library(reactable)
library(readr)
library(shinydashboard)
library(DT)
library(cowplot)
library(readr)
library(rvest)
library(shinyjs)
library(rvest)
library(waiter)
library(shinybusy)
library(shinyWidgets)
library(extrafont)
library(showtext)
library(Cairo)
library(ggforce)
library(RMySQL)
library(DBI)
library(purrr)
library(mgcv)
library(rmcorr) #need for rmcorr tab
library(rdrop2)
library(tidyr) # need for separate function for pairing names
library(stringr)
suppressMessages(library(ggpubr))
suppressMessages(library(gridExtra))
suppressMessages(library(gsubfn))
suppressMessages(library(gtable))
suppressMessages(library(png))
suppressMessages(library(grid))
library(janitor)

#Clean up percentiles a touch for cleaner display on metric comparison tab
add_suffix_to_percentiles <- function(perc_to_conv){
  suff <- case_when(perc_to_conv %in% c(11,12,13) ~ "th",
                    perc_to_conv %% 10 == 1 ~ 'st',
                    perc_to_conv %% 10 == 2 ~ 'nd',
                    perc_to_conv %% 10 == 3 ~'rd',
                    TRUE ~ "th")
  paste0(perc_to_conv, suff)
}

############################
#####  READ IN FILES ####
############################

# read in fonts
font_add(family = "Gotham", regular = "www/Font.ttf")
font_add("gotham" , regular = "www/GothamOffice-Regular_Web.ttf", bold =  "www/GothamOffice-Bold_Web.ttf")
showtext_auto()
#read in DL logo
plot_path = as.character("www/driveline-baseball_logo.png")
plot1 <- readPNG(plot_path)
dl_logo <- rasterGrob(plot1)

#PULL DATA FROM DB
mydb <- dbConnect(MySQL(), user=Sys.getenv('CLUSTER_USERNAME_LUCENA'),
                   password=Sys.getenv('CLUSTER_PASSWORD_LUCENA'), dbname=Sys.getenv('DATABASE_LUCENA_V6_KT'), 
                  port=as.numeric(Sys.getenv('CLUSTER_PORT_LUCENA')),
                   host=Sys.getenv('CLUSTER_HOST_LUCENA'))


merged_table_query <- 'SELECT users.subject_traq AS traq_id, users.name, sesh.session_date, sesh.playing_level AS session_playing_level, 
sesh.session_tag, round(sesh.session_height_m * 39.37, 1) AS session_height, sesh.session_mass_kg * 2.2 AS session_weight,
poi.*
  FROM bp_poi_metrics poi 
LEFT JOIN bp_sessions sesh USING(SESSION)
LEFT JOIN bp_users users USING(USER)'


data_from_db <- dbGetQuery(mydb, merged_table_query)
# Remove Duplicate Column Names - since session and session_pitch show up in both bp_poi_metrics and bp_calculated_metrics, so end up duplicated here
data_from_db <- data_from_db[!duplicated(colnames(data_from_db))]
data_from_db$session_date <- as.Date(data_from_db$session_date) #reformat date as date instead of character string
data_from_db$session_playing_level <- tolower(data_from_db$session_playing_level)



#Calculate Session-Level Averages - rather than displaying each separate throw
init_data <- aggregate(data_from_db[, c(12:ncol(data_from_db))], list(data_from_db$traq_id,  data_from_db$name, data_from_db$session_date,
                                                                      data_from_db$p_throws, data_from_db$pitch_type,
                                                                      data_from_db$session_weight), 
                       FUN = function(x) round(mean(x), 2))

colnames(init_data)[1:6] <- c("traq_id",  "name", "session_date",
                              "session_throws", "pitch_type", 
                              "session_weight")

#Create velo buckets for subsetting for plots
init_data$velo_buckets <- cut(init_data$pitch_speed_mph, breaks = c(0, 40, 50, 60, 70, 75, 80, 85, 90, 94, 96, 110),
                              labels = c("<40", "40-50", "50-60", "60-70", "70-75", "75-80", "80-85", "85-90", "90-94", "94-96", "96+"))

data_from_db$velo_buckets <- cut(data_from_db$pitch_speed_mph, breaks = c(0, 40, 50, 60, 70, 75, 80, 85, 90, 94, 96, 110),
                                 labels = c("<40", "40-50", "50-60", "60-70", "70-75", "75-80", "80-85", "85-90", "90-94", "94-96", "96+"))


init_data_colnames_renaming_key <- data.table::fread("biomech_db_var_name_key.csv")
colnames(init_data) <- (as.data.frame(colnames(init_data)) %>% 
                          left_join(init_data_colnames_renaming_key, 
                                    by = c('colnames(init_data)' = 'start_colnames')))$clean_colnames

colnames(data_from_db) <- (as.data.frame(colnames(data_from_db)) %>% 
                             left_join(init_data_colnames_renaming_key, 
                                       by = c('colnames(data_from_db)' = 'start_colnames')))$clean_colnames
data_from_db <- data_from_db[, colnames(data_from_db)[!is.na(colnames(data_from_db))]] #remove columns that don't have clean names
init_data <- init_data[, colnames(init_data)[!is.na(colnames(init_data))]]

quant_vars <- colnames(init_data)[!colnames(init_data) %in% c("TRAQ ID", "Date" , "Playing Level", "Session Tag", "Pitch Type",
                                                              "Handedness", "Name",   "Velo Bucket")] #some of these are from the pitch-by-pitch data, so don't apply
#####Create Quant Vars to be input for different ######

#Create a list of the quantitative variables to be available variables for plots
quant_vars_wtm <- colnames(init_data)[!colnames(init_data) %in% c("TRAQ ID", "Date" , "Playing Level", "Session Tag", "Pitch Type",
                                                                  "Handedness", "Name",   "Velo Bucket")] #some of these are from the pitch-by-pitch data, so don't apply


#############################
########    TABS    #########
#############################


#Tab layout for Metric Comparison (i.e. Scatterplots and linear regressions)
met_comp_tab <- fluidPage(sidebarLayout(
  sidebarPanel(
    selectInput('x', h5('X Variable'), choices = quant_vars_wtm, selected = "Shoulder Horizontal Abduction at FP"),
               selectInput('y', h5('Y Variable'), choices = quant_vars_wtm, selected = "Pitch Speed (mph)"),
                           pickerInput("velo_bux_for_plot", h5("Velocity Ranges"), choices = c("<40", "40-50", "50-60", "60-70", "70-75", "75-80", "80-85", "85-90", "90-94", "94-96", "96+"), 
                           selected = c("75-80", "80-85", "85-90", "90-94", "94-96", "96+"),multiple=TRUE),
               selectInput("athletes_for_plot", h5("Specific Athletes to Highlight"), choices = sort(unique(init_data$Name)), selected = c(),multiple=TRUE),
               radioGroupButtons("met_comp_fastballs_or_all","",
                      choiceNames = list(tags$span("Fastballs Only", style = "font-family:Lato;"),
                                         tags$span("All Pitch Types", style = "font-family:Lato;")),
                      choiceValues = c("Fastballs Only", "All Pitch Types"), selected = "Fastballs Only"),
      width = 3),
  mainPanel(
      fluidRow(column(10,
        div(
          style = "position:relative",
          plotOutput("scat_plot",  brush = brushOpts(id = "user_brush", resetOnNew = TRUE),
                    dblclick = "plot1_dblclick",
                    hover = hoverOpts("plot_hover", delay = 100, delayType = "debounce"),
                    height = "auto"),
          uiOutput("hover_info"),
          h5("Models to Fit", style="color:black"),
          pickerInput("models_to_add",  choices = c("Linear", "Quadratic", "Cubic"), 
                      selected = c("Linear"),multiple=TRUE),
          br(),
          h5("Model Metrics", style="color:black"),
          htmlOutput("model_metrics_text")
          ),
        ),
              column(2)), #Add margin, so tooltips don't go off page - and to get desired dimensions for plot
      width = 9),
  )
)


#Tab layout for Percentiles (i.e. density plots and percentiles)
percentiles_tab <- fluidPage(sidebarLayout(
    sidebarPanel(
      selectInput('dens_var', h5('Variable'), choices = quant_vars_wtm, selected = "Shoulder Horizontal Abduction at FP"),
      numericInput("dens_num_input", h5("Metric Value for Percentiles"), NA, -1000, 1000),
      tags$style(type = "text/css", "#dens_num_input.form-control.shiny-bound-input {font-family: Lato}"), #couldn't get to work in CSS style sheet, so dropping here
      pickerInput("velo_bux_for_plot_dens", h5("Velocity Ranges"), choices = c("<40", "40-50", "50-60", "60-70", "70-75", "75-80", "80-85", "85-90", "90-94", "94-96", "96+"), 
                  selected = c("75-80", "80-85", "85-90", "90-94", "94-96", "96+"),multiple=TRUE),
      radioGroupButtons("percentiles_fastballs_or_all","",
                        choiceNames = list(tags$span("Fastballs Only", style = "font-family:Lato;"),
                                           tags$span("All Pitch Types", style = "font-family:Lato;")),
                        choiceValues = c("Fastballs Only", "All Pitch Types"), selected = "Fastballs Only"),
      
      width = 3),
    mainPanel(
      fluidRow(column(10,
                      div(
                        plotOutput("density_plot",  brush = brushOpts(id = "user_brush", resetOnNew = TRUE),
                                   dblclick = "plot1_dblclick",
                                   height = "auto"),
                        br(),
                        h5("Percentiles", style="color:black"),
                        htmlOutput("density_plot_text")
                      ),
      ),
      column(2)), #Add margin, so tooltips don't go off page - and to get desired dimensions for plot
      width = 9)
  )
)


#Tab layout for Table Viewer
table_viewer_tab <- fluidPage(
  mainPanel(
    fluidRow(column(4, style = "background-colour: black;",
             selectInput("table_viewer_vars_input", h5("Variables for Table", style="color:black"), 
                         choices = sort(colnames(init_data)[!colnames(init_data) %in% c("Name", "Date", "Pitch Type")]), selected = c(),multiple=TRUE)),
                         column(4, pickerInput("velo_bux_for_plot_table", h5("Velocity Ranges", style="color:black"), choices = c("<40", "40-50", "50-60", "60-70", "70-75", "75-80", "80-85", "85-90", "90-94", "94-96", "96+"), 
                                   selected = c("75-80", "80-85", "85-90", "90-94", "94-96", "96+"),multiple=TRUE))),
    fluidRow(column(6, br(), downloadLink(outputId = "downloadData", label = "Download CSV", style="color: black; font-family: Gotham")),
             column(3, radioGroupButtons("tables_fastballs_or_all","",
                                               choiceNames = list(tags$span("Fastballs Only", style = "font-family:Gotham;"),
                                                                  tags$span("All Pitch Types", style = "font-family:Gotham;")),
                                               choiceValues = c("Fastballs Only", "All Pitch Types"), selected = "Fastballs Only")),
             column(3, radioGroupButtons("pitchorsession","",
      choiceNames = list(tags$span("Pitch level", style = "font-family:Gotham;"),
                         tags$span("Session level", style = "font-family:Gotham;")),
      choiceValues = c("Pitch level", "Session level"), selected = "Session level"))
      ),
    DT::DTOutput("table")
  )
)


rmcorr_tab <- fluidPage(
  mainPanel(
    fluidRow(column(4, 
                    selectInput('x_rmcorr', h5('X Variable', style="color:black"), choices = quant_vars, selected = "Shoulder Horizontal Abduction at FP"),
                    ),
             column(4, 
                    selectInput('y_rmcorr', h5('Y Variable', style="color:black"), choices = quant_vars, selected = "Pitch Speed (mph)"),
             )),
    h5("Repeated Measures Correlation", style = "color:black"),
    p("Evalutes the with-in subject correlation between the selected metrics - comparing the different throws from each athlete.", style="font-family:Gotham"),
    br(),
    htmlOutput("rmcorr_text_output"),
    add_busy_bar(color = "#b0b0b0")
  )
)


#############################
########    UI    ###########
#############################

ui <- navbarPage(windowTitle = "DL Biomech DB Viewer",
                 title = div(img(src = 'https://www.drivelinebaseball.com/wp-content/uploads/2017/07/driveline-baseball_logo_full__black-1.png', class = "center", style="margin-top: -14px; padding-right:10px;padding-bottom:8px", height = 60)), 
                 tabPanel("Metric Comparisons", met_comp_tab), 
                 tabPanel("Percentiles", percentiles_tab),
                 tabPanel("Table Viewer", table_viewer_tab),
                 tabPanel("Intra-Athlete Correlation", rmcorr_tab),
                 theme = 'DL_style_testing5.css'
                 )


#############################
########    SERVER    #######
#############################
server <- function(input, output, session) {
  options(shiny.usecairo=T)

  
  ############Scatter plot Tab####
  
  #Filter data for the scatterplot based on the level/velo subsetting options
  filter_data_to_plot <- reactive({
    filter_data_to_plot = init_data %>% 
      filter(`Velo Bucket` %in% input$velo_bux_for_plot)
    if (input$met_comp_fastballs_or_all == "Fastballs Only"){
      filter_data_to_plot = filter_data_to_plot %>% filter(`Pitch Type` %in% c('FF', 'SI'))
    }
    filter_data_to_plot
  })
  
  #Create new data frame for data from athletes to highlight input
  spec_athlete_data_to_plot <- reactive({
    spec_athlete_data_to_plot = init_data %>% filter(Name %in% input$athletes_for_plot)
    if (input$met_comp_fastballs_or_all == "Fastballs Only"){
      spec_athlete_data_to_plot = spec_athlete_data_to_plot %>% filter(`Pitch Type` %in% c('FF', 'SI'))
    }
    spec_athlete_data_to_plot
  })
  
  
#Dynamic Zooming for the plot (this same ranges function is used for Percentiles tab, FYI)
  #based on the brushing example here: https://shiny.rstudio.com/gallery/plot-interaction-zoom.html
  ranges <- reactiveValues(x = NULL, y = NULL)
  
  observeEvent(input$plot1_dblclick, {
    brush <- input$user_brush
    if (!is.null(brush)) {
      ranges$x <- c(brush$xmin, brush$xmax)
      ranges$y <- c(brush$ymin, brush$ymax)
      
    } else {
      ranges$x <- NULL
      ranges$y <- NULL
    }
  })
  
  zoomed_data <- reactive({ #store zoomed data when brushed as dataframe for reference in calculating correlations
    non_zoomed_data <- filter_data_to_plot() 
    if(!is.null(ranges$x)){
      non_zoomed_data <- non_zoomed_data[non_zoomed_data[,c(paste(input$x))] >= ranges$x[1] & non_zoomed_data[,c(paste(input$x))]<= ranges$x[2] &
                                           non_zoomed_data[,c(paste(input$y))] >= ranges$y[1] & non_zoomed_data[,c(paste(input$y))]<= ranges$y[2], ]
    }
    non_zoomed_data #is actually the zoomed data at this point, but that's besides the point - gets output as zoomed_data, so that's what we need
  })
  
 
  #Creates our plot
  scatter <- reactive({
    plot_object <- ggplot()
    plot_object <- ggplot() +
      geom_point(filter_data_to_plot(), mapping = aes_string(x = as.name(input$x), y = as.name(input$y))) + 
      geom_point(spec_athlete_data_to_plot(), mapping = aes_string(x = as.name(input$x), y = as.name(input$y)), color = "#ffa300", size = 4) + 
      theme(plot.title = element_text(face = "bold", size=16, family="gotham", hjust = 0.50),
            plot.subtitle = element_text(hjust =0.5),
            text=element_text(size=12,  family="gotham"),
            panel.grid = element_line(colour = 'gray90', linetype = 'dashed'),
            panel.background = element_blank(),
            legend.position="none",
            legend.title = element_blank())  +
            labs(title = paste(input$y, "vs", input$x))
    if(nrow(zoomed_data() != 0)){     #Add subtitle title only if data exists - since otherwise, no r value and it crashes the plot 
      plot_object = plot_object + 
        labs(subtitle = paste("r = ", round(cor(zoomed_data()[, input$x], zoomed_data()[, input$y], use = "pairwise.complete.obs"), 2)))}
    if(!is.null(ranges$x)){ #return only brushed area if the brush exists
      plot_object <- plot_object +
        coord_cartesian(xlim = ranges$x, ylim = ranges$y, expand = FALSE)}
    #Adding our fit lines based on what's selected in the Models to Fit input
    if("Linear" %in% input$models_to_add){
      plot_object <- plot_object + geom_smooth(data = zoomed_data(), mapping = aes_string(x = as.name(input$x), y = as.name(input$y)), size = 1.5, color = "#ffa300", formula = y ~x, method = "lm", se = FALSE)
    }
    if("Quadratic" %in% input$models_to_add){
      plot_object <- plot_object + geom_smooth(data = zoomed_data(), mapping = aes_string(x = as.name(input$x), y = as.name(input$y)), size = 1.5, color = "gray80", formula = y ~x + I(x^2), method = "lm", se = FALSE)
    }
    if("Cubic" %in% input$models_to_add){
      plot_object <- plot_object + geom_smooth(data = zoomed_data(), mapping = aes_string(x = as.name(input$x), y = as.name(input$y)), size = 1.5, color = "#0047BB", formula = y ~x + I(x^2) + I(x^3), method = "lm", se = FALSE)
    }
    plot_object
  })
  
  
#Output Scatter Plot
  output$scat_plot <- renderPlot({scatter() 
  }, height = function() {session$clientData$output_scat_plot_width*0.6 }
  )

  
#Hover Tip Code - from https://gitlab.com/-/snippets/16220
  output$hover_info <- renderUI({
    hover <- input$plot_hover
    point <- nearPoints(filter_data_to_plot(), hover, threshold = 20, maxpoints = 1, addDist = TRUE)
    if (nrow(point) == 0) return(NULL)
    
    # calculate point position INSIDE the image as percent of total dimensions
    # from left (horizontal) and from top (vertical)
    left_pct <- (hover$x - hover$domain$left) / (hover$domain$right - hover$domain$left)
    top_pct <- (hover$domain$top - hover$y) / (hover$domain$top - hover$domain$bottom)
    
    # calculate distance from left and bottom side of the picture in pixels
    left_px <- hover$range$left + left_pct * (hover$range$right - hover$range$left)
    top_px <- hover$range$top + top_pct * (hover$range$bottom - hover$range$top)
    
    # create style property for tooltip
    # background color is set so tooltip is a bit transparent
    # z-index is set so we are sure are tooltip will be on top
    style <- paste0("position:absolute; z-index:100; background-color: rgba(245, 245, 245, 0.85); ",
                    "left:", left_px + 2, "px; top:", top_px + 2, "px; font-family: 'Gotham';")
    
    # actual tooltip created as wellPanel
    wellPanel(
      style = style,
      container = "body",
      p(HTML(paste0("<b> Athlete: </b>", point$`Name`, "<br/>",
                    "<b> Date: </b>", point$`Date`, "<br/>",
                    ifelse(input$x == "Pitch Speed (mph)" | input$y == "Pitch Speed (mph)", "", paste0("<b> Velo: </b>", point$`Pitch Speed (mph)`, "<br/>")),
                    "<b>", input$x, "</b> ", round(point[, input$x], 1), " (", add_suffix_to_percentiles(round(mean(point[, input$x]>= filter_data_to_plot()[, input$x], na.rm = T), 2)*100),  ")", "<br/>",
                    "<b>", input$y, "</b> ", round(point[, input$y], 1), " (", add_suffix_to_percentiles(round(mean(point[, input$y]>= filter_data_to_plot()[, input$y], na.rm = T), 2)*100),  ")"))
    ))
  })
  
  
  #Generate our notes summarizing the models that get fit
  model_summary_text = reactive({
    model_data <- zoomed_data()
    model_x <- model_data[, input$x]
    model_y <- model_data[, input$y]
    linmod <- summary(lm(model_y ~ model_x, data = model_data))
    quadmod <- summary(lm(model_y ~ model_x + I(model_x^2), data = model_data))
    cubicmod <- summary(lm(model_y ~ model_x + I(model_x^2) + I(model_x^3), data = model_data))
    modsumtext <- ""
    if(is_empty(input$models_to_add)){modsumtext = "No Models fit"}
    
    if("Linear" %in% input$models_to_add){
      modsumtext <- paste0(modsumtext, "<b>Linear Model</b> <br/> R&sup2 =",  round(linmod$r.squared, 3),
                           "<br/>", input$y, " = ", 
                          round(linmod$coefficients[,"Estimate"][[1]], 3), " + ", "(", round(linmod$coefficients[,"Estimate"][[2]], 3), ")", "*", input$x,
                          "<br/>", "<br/>")
      }
    if("Quadratic" %in% input$models_to_add){
      modsumtext <- paste0(modsumtext, "<b>Quadratic Model</b> <br/> R&sup2 =", round(quadmod$r.squared, 3),
                          "<br/>", input$y, " = ", 
                          round(quadmod$coefficients[,"Estimate"][[1]], 2), "+", "(", round(quadmod$coefficients[,"Estimate"][[2]], 5), ")", "*", input$x,
                          "+", "(", round(quadmod$coefficients[,"Estimate"][[3]], 5), ")", "*", input$x, "&sup2",
                          "<br/>", "<br/>")
      }
    if("Cubic" %in% input$models_to_add){
      modsumtext <- paste0(modsumtext, "<b>Cubic Model</b> <br/> R&sup2 =", round(cubicmod$r.squared, 3),
                           "<br/>", input$y, " = ", 
                           round(cubicmod$coefficients[,"Estimate"][[1]], 2), "+", "(", round(cubicmod$coefficients[,"Estimate"][[2]], 5), ")", "*", input$x,
                           "+", "(", round(cubicmod$coefficients[,"Estimate"][[3]], 5), ")", "*", input$x, "&sup2",
                           "+", "(", round(cubicmod$coefficients[,"Estimate"][[4]], 5), ")", "*", input$x, "&sup3",
                           "<br/>", "<br/>")
      }
    
    modsumtext
  })
  

  #And output that text as HTML for ease of formatting.
  output$model_metrics_text <- renderUI({
    HTML(
      paste0(
        '<p style = "font-family:Gotham;font-size:14px;">',
        model_summary_text(),
        '</p>'
    ))
  })
  
  
  
  ############Density plot Tab####
  #Need to subset the data based on the level/velo subsets chosen
  filter_data_to_plot_density <- reactive({
    filter_data_to_plot_density = init_data %>%
      filter(`Velo Bucket` %in% input$velo_bux_for_plot_dens)
    if (input$percentiles_fastballs_or_all == "Fastballs Only"){
      filter_data_to_plot_density = filter_data_to_plot_density %>% filter(`Pitch Type` %in% c('FF', 'SI'))
    }
    filter_data_to_plot_density
  })
  
  #Create a plot again
  density_plot <- reactive({
    plot_object_dens <- ggplot()
    plot_object_dens <- ggplot() +
      geom_density(filter_data_to_plot_density(), mapping = aes_string(x = as.name(input$dens_var)), color = "#ffa300", fill = "#ffa300", alpha = 0.6) +
      theme(plot.title = element_text(face = "bold", size=16, family="gotham", hjust = 0.50),
            plot.subtitle = element_text(hjust =0.5),
            text=element_text(size=12,  family="gotham"),
            panel.grid = element_line(colour = 'gray90', linetype = 'dashed'),
            panel.background = element_blank(),
            legend.position="none",
            legend.title = element_blank())  +
      labs(title = paste("Distribution of", input$dens_var))
    if(!is.null(ranges$x)){ #return only brushed area if the brush exists
      plot_object_dens <- plot_object_dens +
        coord_cartesian(xlim = ranges$x, expand = FALSE)}
    #If we have an input value - we add the vertical line and calculate the percentile to add to the plot subtitle
    if(!is.na(input$dens_num_input)){
      plot_object_dens <- plot_object_dens + geom_vline(xintercept = input$dens_num_input, color = "gray70", linetype = "longdash", size = 1)
      
      plot_object_dens = plot_object_dens + 
             labs(subtitle = paste0(input$dens_var, " of ", input$dens_num_input, " - ", add_suffix_to_percentiles(round(mean(input$dens_num_input > filter_data_to_plot_density()[,input$dens_var]) * 100)), " percentile"))
      }
    plot_object_dens
  })
  
  output$density_plot <- renderPlot({density_plot()}, 
                      height = function() {session$clientData$output_density_plot_width*0.6})
  

  #Percentile Text Output
  density_plot_text = reactive({
    quantiles <- quantile(filter_data_to_plot_density()[,input$dens_var], c(0.05, 0.1, 0.25, 0.5, 0.75, 0.9, 0.95))
    density_plot_text <- paste0("<b>5th: </b>", round(quantiles[1],1), "<br/>", "<b>10th: </b>", round(quantiles[2],1), "<br/>",
                                "<b>25th: </b>", round(quantiles[3],1), "<br/>", "<b>50th: </b>", round(quantiles[4],1), "<br/>",
                                "<b>75th: </b>", round(quantiles[5],1), "<br/>", "<b>90th: </b>", round(quantiles[6],1), "<br/>",
                                "<b>95th: </b>", round(quantiles[7],1))
    density_plot_text
  })
  
  output$density_plot_text <- renderUI({
    HTML(
      paste0(
        '<p style = "font-family:Gotham;font-size:14px;">',
        density_plot_text(),
        '</p>'
      ))
  })
  
  ############Table Viewer Tab####
  #Do the level/velo subsetting thing again
  filter_data_to_plot_table <- reactive({
    if(input$pitchorsession == "Session level"){
    data_to_return <- init_data %>% 
      filter(`Velo Bucket` %in% input$velo_bux_for_plot_table)
    }
    
    if(input$pitchorsession == "Pitch level"){
      data_to_return <- data_from_db %>% 
        filter(`Velo Bucket` %in% input$velo_bux_for_plot_table)
    }
    
    if (input$tables_fastballs_or_all == "Fastballs Only"){data_to_return = data_to_return %>% filter(`Pitch Type` %in% c('FF', 'SI'))}
    
    data_to_return
  })
  
  #Push to table - with a couple different options manually specified to get filtering options, column names right aligned, 25 rows showing, etc.
  output$table <- DT::renderDT(filter_data_to_plot_table()[, c("Name", "Date", "Pitch Type", input$table_viewer_vars_input)],
                               filter = "top", options = list(pageLength = 25, columnDefs = list(list(className = 'dt-right', targets = "_all"))))
  
  
  output$downloadData <- downloadHandler(
    filename = function() {paste("biomech_data_", Sys.Date(), ".csv",sep="")},
    content = function(file) {
      write.csv(filter_data_to_plot_table()[, c("Name", "Date", "Pitch Type", input$table_viewer_vars_input)], file, row.names=FALSE)
    }
  )
  
  
  ############rmcorr Tab####
  rmcorr_text_output = reactive({
    data_from_db_rmcorr <- data_from_db %>% filter(`Pitch Speed (mph)` >= 75, `Pitch Type` %in% c('FF', 'SI'))
    round(rmcorr(as.factor(`TRAQ ID`), input$x_rmcorr, input$y_rmcorr, data_from_db_rmcorr)$r, 3)
  })

  #cor(data_from_db[,input$x_rmcorr], data_from_db[, input$y_rmcorr])
    
  output$rmcorr_text_output <- renderUI({
    HTML(
      paste0(
        '<p style = "font-family:Gotham;font-size:14px;">',
        "<b>r = ", rmcorr_text_output(), "</b>",
        '</p>'
      ))
  })
  
  
  


}  


# Run the application
shinyApp(ui = ui, server = server)
