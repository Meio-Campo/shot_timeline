# Shot Timeline App
This Shiny application is designed to visualize shot timelines and calculate probabilities based on match data obtained from FBRef. 
It allows users to n visualize shot timelines along with probabilities of different match outcomes just by pasting a URL for a FBRef Match Report.

# How to Use
To run the application, execute the code provided in an R environment where the necessary packages are installed. Ex: RStudio

Welcome Message: Upon launching the app, you will be greeted with a welcome message. It provides instructions on how to use the app, including selecting colors for squads and pasting the FBRef URL.

Select Squad Colors: Choose colors for Squad 1 and Squad 2 from the dropdown menus provided. # this is only intended for customizing your viz. It will eventually be updated to fetch the colors dynamically. 

Enter FBref URL: Paste the URL of the Match Report from FBRef into the text box.

Submit: Click the "Submit" button to generate Shot-Timelines and other visualizations based on the provided URL and color selections.
Circles: Circles are employed to denote shots that did not result in a goal. These shots could include attempts that were saved by the goalkeeper, blocked by defenders, or missed the target altogether. The presence of circles in the plot indicates instances where the attacking team attempted to score but was unsuccessful.

Squares: Squares, conversely, represent shots that resulted in goals. These are successful attempts where the ball found its way into the back of the net. The squares highlight moments of achievement for the attacking team, signifying goals scored during the match.

Calculate Probabilities: After submitting the URL, click the "Calculate Probabilities" button to calculate probabilities and visualize outcomes. It takes a while to compute the probabilities, bu will eventually work. 

Shot Timeline Tab: This tab displays the shot timeline plot, cumulative xG plot, data table, and team information.

Probabilities Tab: In this tab, you can view the heatmap representing the probability distribution of match outcomes and histograms showing the probability distribution of each squad's goals.

Dependencies
This app utilizes the following R packages:

shiny: Provides the framework for building interactive web applications in R.
tidyverse: A collection of R packages for data manipulation and visualization.
rvest: Enables web scraping capabilities in R.
scales: Provides functions for scaling data and formatting labels in plots.
Running the Application
To run the application, execute the code provided in an R environment where the necessary packages are installed. Then, launch the application, and it will open in your default web browser.

R
Copy code
#Load required libraries
library(shiny)
library(tidyverse)
library(rvest)
library(scales)

#Define UI and server logic
#...

#Run the application
shinyApp(ui = ui, server = server)

Ensure that you have an active internet connection while using the app to fetch data from FBRef.

# Author
This app was created by [O Meio Campo].

# License
This project is licensed under the .
