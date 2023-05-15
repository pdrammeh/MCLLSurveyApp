library(shiny)
library(readxl)
library(tidyverse)
library(scales)
library(ggpubr)
library(here)

# Read the Excel file and extract available lectures
data <- read_xlsx(path = "Spring 2023 Lecture Survey.xlsx")
available_lectures <-
    unique(data$`Please select the Lecture you just attended`) #Store unique available lectures 

# Define UI for Shiny app
ui <- fluidPage(
    tags$head(
        tags$style(
            "
      .footer {
        text-align: center;
        font-size: 12px;
        color: #888;
        margin-top: 20px;
      }
    "
        )
    ),
    titlePanel("MCLL Lecture Feedback Dashboard"),
    sidebarLayout(
        sidebarPanel(
            selectInput(
                inputId = "lecture",
                label = "Select a Lecture:",
                choices = available_lectures
            ),
            actionButton(inputId = "generate_button", label = "Generate Report")
            #downloadButton(inputId = "download_button", label = "Download Report")
        ),
        mainPanel(
            textOutput(outputId = "course_name"),
            textOutput(outputId = "number_of_responses"),
            plotOutput(outputId = "lecture_prep_plot"),
            plotOutput(outputId = "presentation_plot"),
            plotOutput(outputId = "clarity_delivery_plot"),
            plotOutput(outputId = "questions_discussion_plot"),
            plotOutput(outputId = "overall_assessment_plot"),
            verbatimTextOutput(outputId = "comments"),
            tags$div(class = "footer", "© 2023 MCLL MGILL | Designed by SALLAH")
        )
    )
)


# Define server for Shiny app
server <- function(input, output) {
    observeEvent(input$generate_button, {
        lecture <- input$lecture
        
        # Filter data for the selected lecture
        course.data <- data %>%
            filter(`Please select the Lecture you just attended` == lecture) %>%
            select(c(7:13))
        colnames(course.data) <-
            c("Lecture",
              "lec.prep",
              "present.",
              "clarity",
              "time",
              "overall",
              "comments")
        
        # Generate plots
        lecture.prep <-
            ggplot(course.data, aes(
                x = reorder(lec.prep, lec.prep, function(x)
                    - length(x)),
                fill = interaction(lec.prep, lec.prep)
            )) +
            geom_bar(position = "dodge") +
            labs(x = "Lecturer's preparation and Knowledge",
                 y = "Number of responses") +
            scale_y_continuous(labels = label_number(accuracy = 1)) +
            theme_classic() +
            theme(legend.position = "none")
        
        present <-
            ggplot(course.data, aes(
                x = reorder(present., present., function(x)
                    - length(x)),
                fill = interaction(present., present.)
            )) +
            geom_bar(position = "dodge") +
            labs(x = "Presentation was interesting and relevant",
                 y = "Number of responses") +
            scale_y_continuous(labels = label_number(accuracy = 1)) +
            theme_classic() +
            theme(legend.position = "none")
        
        clarity.and.delivery <-
            ggplot(course.data, aes(
                x = reorder(clarity, clarity, function(x)
                    - length(x)),
                fill = interaction(clarity, clarity)
            )) +
            geom_bar(position = "dodge", na.rm = TRUE) +
            labs(x = "Lecturer's clarity and delivery", y = "Number of responses") +
            scale_y_continuous(labels = label_number(accuracy = 1)) + theme_classic() +
            theme(legend.position = "none")
        questions.and.dis <-
            ggplot(course.data, aes(
                x = reorder(time, time, function(x)
                    - length(x)),
                fill = interaction(time, time)
            )) +
            geom_bar(position = "dodge", na.rm = TRUE) +
            labs(x = "Time the lecturer provided for questions and discussion",
                 y = "Number of responses") +
            scale_y_continuous(labels = label_number(accuracy = 1)) +
            theme_classic() +
            theme(legend.position = "none")
        
        overall.assessment <-
            ggplot(course.data, aes(
                x = reorder(overall, overall, function(x)
                    - length(x)),
                fill = interaction(overall, overall)
            )) +
            geom_bar(position = "dodge", na.rm = TRUE) +
            labs(x = "Overall assessment of the lecture", y = "Number of responses") +
            scale_y_continuous(labels = label_number(accuracy = 1)) +
            theme_classic() +
            theme(legend.position = "none")
        
        comments <- course.data$comments
        comments <- comments[which(!is.na(comments))]
        
        # Render outputs
        output$course_name <- renderText({
            paste("Results for lecture:", lecture)
        })
        
        output$number_of_responses <- renderText({
            paste("The number of responses are:", nrow(course.data))
        })
        
        output$lecture_prep_plot <- renderPlot({
            lecture.prep
        })
        
        output$presentation_plot <- renderPlot({
            present
        })
        
        output$clarity_delivery_plot <- renderPlot({
            clarity.and.delivery
        })
        
        output$questions_discussion_plot <- renderPlot({
            questions.and.dis
        })
        
        output$overall_assessment_plot <- renderPlot({
            overall.assessment
        })
        
        output$comments <- renderPrint({
            comments
        })
        
    })
}


# Run the Shiny app
shinyApp(ui = ui, server = server)
