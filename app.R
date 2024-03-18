library(shiny)
library(tidyverse)
library(rvest)
library(scales)

# Define UI
ui <- fluidPage(
  titlePanel("Shot Timeline"),
  mainPanel(
    tags$div(
      style = "margin-bottom: 20px;",
      HTML("<h3>Welcome</h3><p>Please choose colors for Squad 1 and Squad 2, and paste the URL of the Match Report on FBRef.</p><p>Example: https://fbref.com/en/matches/84a22633/Atalanta-Sporting-CP-March-14-2024-Europa-League</p>")
    ),
    fluidRow(
      column(4,
             selectInput("squad1_color", "Squad 1 Color:", 
                         choices = c("blue", "red", "darkgreen"), selected = "blue")
      ),
      column(4,
             selectInput("squad2_color", "Squad 2 Color:", 
                         choices = c("darkgreen", "red", "blue"), selected = "darkgreen")
      ),
      column(4,
             selectInput("neutral_color", "Neutral Color:", 
                         choices = c("grey", "black", "white"), selected = "grey")
      )
    ),
    textInput("url", "Enter FBref URL:", ""),
    actionButton("submit_button", "Submit"),
    actionButton("calculate_prob_button", "Calculate Probabilities"),
    tabsetPanel(
      tabPanel("Shot Timeline",
               fluidRow(
                 column(width = 12,
                        plotOutput("shot_plot")
                 )
               ),
               fluidRow(
                 column(width = 12,
                        plotOutput("cumulative_plot")
                 )
               ),
               tableOutput("data_table"),
               textOutput("team_info")
      ),
      tabPanel("Probabilities",
               fluidRow(
                 column(width = 12,
                        plotOutput("heatmap")
                 ),
                 column(width = 6,
                        plotOutput("histogram_squad1")
                 ),
                 column(width = 6,
                        plotOutput("histogram_squad2")
                 )
               )
      )
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  
  # Initialize outputs
  output$shot_plot <- renderPlot(NULL)
  output$data_table <- renderTable(NULL)
  output$cumulative_plot <- renderPlot(NULL)
  output$team_info <- renderText(NULL)
  output$heatmap <- renderPlot(NULL)
  output$histogram_squad1 <- renderPlot(NULL)
  output$histogram_squad2 <- renderPlot(NULL)
  
  # Function to scrape the FBref URL and generate shot timeline
  scrape_and_plot <- function(url, squad1_color, squad2_color, neutral_color) {
    # Send a GET request to the URL and read the HTML content
    page <- read_html(url)
    
    # Find the "SHOTS" table
    shots_table <- page %>%
      html_nodes(xpath = '//h2[contains(.,"Shots")]/following::table[1]') %>%
      html_table(fill = TRUE)
    
    # Convert the table to a data frame
    if (length(shots_table) > 0) {
      df <- shots_table[[1]]
      colnames(df) <- make.unique(as.character(df[1, ]))
      df <- df[-1, ]
      df <- df[complete.cases(df$xG), ]
      df <- df[!is.na(df$Squad), ]
      df <- df[str_detect(df$Squad, "\\b\\w{3,}\\b"), ]  # Filter out squads with less than 3 letters
      squads <- unique(df$Squad)
      squad_colors <- c(squad1_color, squad2_color, neutral_color)
      df$Minute <- as.numeric(gsub("\\D", "", df$Minute))
      df$Minute <- ifelse(df$Minute > 900, df$Minute - 900 + 90, ifelse(df$Minute > 450, df$Minute - 450 + 45, df$Minute))
      df$xG <- as.numeric(df$xG)
      last_shot_minute <- max(df$Minute, na.rm = TRUE)
      match_duration <- ifelse(is.na(last_shot_minute) || last_shot_minute == 0, 100, last_shot_minute)
      circle_sizes <- 100 * df$xG
      p <- ggplot(df, aes(x = Minute, y = 1, color = Squad, fill = Squad, shape = ifelse(Outcome == "Goal", "Goal", "Other"))) +
        geom_point(position = position_jitter(height = 0), alpha = 0.6, size = circle_sizes) +
        scale_color_manual(values = setNames(squad_colors, unique(squads)), na.value = "black") +
        scale_fill_manual(values = setNames(squad_colors, unique(squads)), na.value = "black") +
        scale_shape_manual(values = c("Goal" = 15, "Other" = 16), name = "Outcome") +
        scale_x_continuous(breaks = seq(0, match_duration, by = 15), labels = seq(0, match_duration, by = 15)) +
        scale_y_continuous(breaks = NULL) +
        labs(title = "Shot Timeline", x = "Minute", y = NULL) +
        theme_minimal() +
        theme(axis.title.y = element_blank(),
              axis.text.y = element_blank(),
              axis.ticks.y = element_blank(),
              legend.text = element_text(size = 12))  # Increase size of legend text here
      cumulative_xg <- df %>%
        group_by(Squad) %>%
        mutate(cumulative_xg = cumsum(xG))
      cumulative_plot <- ggplot(cumulative_xg, aes(x = Minute, y = cumulative_xg, color = Squad, label = round(cumulative_xg, 2))) +
        geom_step() +
        geom_text(aes(label = round(cumulative_xg, 2)), nudge_y = 0.05, size = 3) +  # Removed check_overlap
        scale_color_manual(values = setNames(squad_colors, unique(squads)), na.value = "black") +
        labs(title = "Cumulative xG", x = "Minute", y = "Cumulative xG") +
        theme_minimal() +
        theme(legend.text = element_text(size = 12))  # Increase size of legend text here
      
      # Increase size of legend shapes
      p <- p + guides(shape = guide_legend(override.aes = list(size = 5)))
      
      return(list(shot_plot = p, data_table = df, cumulative_plot = cumulative_plot))
    } else {
      print("SHOTS table not found.")
    }
  }
  
  # Reactive expression to handle submission
  observeEvent(input$submit_button, {
    url <- input$url
    if (url != "") {
      result <- scrape_and_plot(url, input$squad1_color, input$squad2_color, input$neutral_color)
      output$shot_plot <- renderPlot({
        result$shot_plot
      })
      output$data_table <- renderTable({
        result$data_table
      })
      output$cumulative_plot <- renderPlot({
        result$cumulative_plot
      })
      output$team_info <- renderText({
        paste("Teams:", unique(result$data_table$Squad))
      })
    }
  })
  
  # Function to calculate probabilities and plot heatmap and histograms
  observeEvent(input$calculate_prob_button, {
    url <- input$url
    if (url != "") {
      result <- scrape_and_plot(url, input$squad1_color, input$squad2_color, input$neutral_color)
      
      # Separate data frames for each squad
      df_squad1 <- result$data_table[result$data_table$Squad == unique(result$data_table$Squad)[1], ]
      df_squad2 <- result$data_table[result$data_table$Squad == unique(result$data_table$Squad)[2], ]
      
      # Define xG values for each squad
      squad1_xgs <- df_squad1$xG
      squad2_xgs <- df_squad2$xG
      
      # Define a function to simulate match
      simulate_match <- function(team_a_xgs, team_b_xgs) {
        team_a_shots_simulation <- runif(length(team_a_xgs))
        team_b_shots_simulation <- runif(length(team_b_xgs))
        team_a_goals <- sum(team_a_shots_simulation <= team_a_xgs)
        team_b_goals <- sum(team_b_shots_simulation <= team_b_xgs)
        return(c(team_a_goals, team_b_goals))
      }
      
      # Perform simulation
      n_times <- 1000000
      result_sim <- matrix(nrow = n_times, ncol = 2)
      for (i in 1:n_times) {
        result_sim[i, ] <- simulate_match(team_a_xgs = squad1_xgs, team_b_xgs = squad2_xgs)
      }
      result_sim <- as.data.frame(result_sim)
      colnames(result_sim) <- c(unique(result$data_table$Squad)[1], unique(result$data_table$Squad)[2])
      
      # Calculate frequency of each combination
      result_freq <- table(result_sim)
      
      # Convert to data frame
      result_df <- as.data.frame(result_freq)
      
      # Convert Lecce and Frosinone to numeric
      result_df[[1]] <- as.numeric(as.character(result_df[[1]]))
      result_df[[2]] <- as.numeric(as.character(result_df[[2]]))
      
      # Calculate probability values
      result_df$Probability <- result_df$Freq / sum(result_df$Freq)
      
      # Create density heatmap using ggplot2
      heatmap <- ggplot(result_df, aes(x = as.numeric(as.character(result_df[[1]])), y = as.numeric(as.character(result_df[[2]])))) +
        geom_rect(aes(xmin = as.numeric(as.character(result_df[[1]])) - 0.5, 
                      xmax = as.numeric(as.character(result_df[[1]])) + 0.5, 
                      ymin = as.numeric(as.character(result_df[[2]])) - 0.5, 
                      ymax = as.numeric(as.character(result_df[[2]])) + 0.5, 
                      fill = Probability)) +
        scale_fill_viridis_c(name = "Probability", labels = scales::percent_format(accuracy = 0)) +
        labs(x = unique(result$data_table$Squad)[1], y = unique(result$data_table$Squad)[2]) +
        theme_minimal() +
        geom_text(aes(label = scales::percent(Probability, accuracy = 1)), size = 3, color = "white", fontface = "bold") + # Adding text labels for probabilities
        scale_fill_viridis_c(name = "Probability", labels = scales::percent_format(accuracy = 0)) +
        labs(x = unique(result$data_table$Squad)[1], y = unique(result$data_table$Squad)[2]) +
        theme_minimal() +
        scale_x_continuous(breaks = seq(0, max(result_df[[1]])), labels = seq(0, max(result_df[[1]]))) +  # Setting x-axis labels
        scale_y_continuous(breaks = seq(0, max(result_df[[2]])), labels = seq(0, max(result_df[[2]])))  # Setting y-axis labels
      find_max_value <- function(df) {
        max_value <- max(max(df[[1]]), max(df[[2]]))
        return(max_value)
      }
      
      library(patchwork)
      
      # Calculate the maximum value for the x-axis
      max_x_value <- max(find_max_value(result_df), max(result_df[[1]]), max(result_df[[2]]))
      
      # Create histograms for each squad with subdivisions
      histogram_squad1 <- ggplot(result_df, aes(x = as.factor(as.numeric(as.character(result_df[[1]]))), y = Probability)) +
        geom_bar(stat = "identity", fill = input$squad1_color, color = NA) +  # Use selected color for Squad 1
        labs(x = "Number of Goals", y = "Probability", title = paste("Probability Distribution of", unique(result$data_table$Squad)[1], "'s Goals")) +
        theme_minimal() +
        scale_x_discrete(limits = as.character(0:max_x_value)) +  # Setting x-axis limits and ensuring zero is included
        scale_y_continuous(limits = c(0, 1), labels = scales::percent_format(accuracy = 1)) +  # Adjusting y-axis limits and labels
        theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotating x-axis labels for better readability
      
      histogram_squad2 <- ggplot(result_df, aes(x = as.factor(as.numeric(as.character(result_df[[2]]))), y = Probability)) +
        geom_bar(stat = "identity", fill = input$squad2_color, color = NA) +  # Use selected color for Squad 2
        labs(x = "Number of Goals", y = "Probability", title = paste("Probability Distribution of", unique(result$data_table$Squad)[2], "'s Goals")) +
        theme_minimal() +
        scale_x_discrete(limits = as.character(0:max_x_value)) +  # Setting x-axis limits and ensuring zero is included
        scale_y_continuous(limits = c(0, 1), labels = scales::percent_format(accuracy = 1)) +  # Adjusting y-axis limits and labels
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
      
      # Combine histograms side by side
      combined_histogram <- histogram_squad1 + histogram_squad2 + plot_layout(ncol = 2)
      
      # Print the combined histograms
      print(combined_histogram)
      
      # Update outputs
      output$heatmap <- renderPlot({
        heatmap <- ggplot(result_df, aes(x = as.numeric(as.character(result_df[[1]])), y = as.numeric(as.character(result_df[[2]])))) +
          geom_rect(aes(xmin = as.numeric(as.character(result_df[[1]])) - 0.5, 
                        xmax = as.numeric(as.character(result_df[[1]])) + 0.5, 
                        ymin = as.numeric(as.character(result_df[[2]])) - 0.5, 
                        ymax = as.numeric(as.character(result_df[[2]])) + 0.5, 
                        fill = Probability)) +
          scale_fill_viridis_c(name = "Probability", labels = scales::percent_format(accuracy = 0)) +
          labs(x = paste("Probability of", unique(result$data_table$Squad)[1], "score given number of goals"),
               y = paste("Probability of", unique(result$data_table$Squad)[2], "score given number of goals")) +
          theme_minimal() +
          geom_text(aes(label = scales::percent(Probability, accuracy = 1)), size = 3, color = "white", fontface = "bold") +
          scale_fill_viridis_c(name = "Probability", labels = scales::percent_format(accuracy = 0)) +
          scale_x_continuous(breaks = seq(0, max(result_df[[1]])), labels = seq(0, max(result_df[[1]]))) +
          scale_y_continuous(breaks = seq(0, max(result_df[[2]])), labels = seq(0, max(result_df[[2]])))
        heatmap
      })
      
      output$histogram_squad1 <- renderPlot({
        histogram_squad1
      })
      output$histogram_squad2 <- renderPlot({
        histogram_squad2
      })
    }
  })
}

# Run the application
shinyApp(ui = ui, server = server)
