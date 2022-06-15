library(shiny)
library(shinysurveys)
library(googlesheets4)
library(dplyr)
library(tidyr)

# questions ----
df1 <- data.frame(question = '<p>tags$img(src = myImgResources[1], width = "200px", height = "200px")</p>',
                 option = NA,
                 input_type = "textSlider",
                 input_id = "q1",
                 dependence = NA,
                 dependence_value = NA,
                 required = T,
                 page = 1)

df2 = df1
df3 = df1

df2$question = "pytanie 2"
df2$input_id = "q2"
df2$page = 2

df3$question = "pytanie 3"
df3$input_id = "q3"
df3$page = 3

questions_df = rbind(df1,df2)
questions_df = rbind(questions_df,df3)

# images ----
myImgResources <- paste0("imgResources/img", seq_len(2), ".jpg")

# Add directory of static resources to Shiny's web server
addResourcePath(prefix = "imgResources", directoryPath = "resources")


# app ----
extendInputType(input_type = "textSlider", {
  shinyWidgets::sliderTextInput(
    inputId = surveyID(),
    label = surveyLabel(),
    force_edges = TRUE,
    choices = c(LETTERS[1:7]),
    selected = LETTERS[4],
    grid = TRUE
  ) 
})

ui <- fluidPage(
  tags$img(src = myImgResources[1], width = "250px", height = "250px"),
  tags$img(src = myImgResources[2], width = "250px", height = "250px"),
  surveyOutput(df = questions_df,
               survey_title = "Porównanie metod określania zmian struktury przestrzennej kategorii pokrycia terenu",
               survey_description = "Opis ankiety?")
)

server <- function(input, output, session) {
  renderSurvey()
  
  observeEvent(input$submit, {
    showModal(modalDialog(
      title = "Dziękuję za wypełnienie ankiety",
      "Kontakt: blakos1@st.amu.edu.pl"
      #"You can customize what actions happen when a user finishes a survey using input$submit."
    ))
  })
}

shinyApp(ui, server)