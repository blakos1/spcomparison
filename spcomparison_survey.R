library(shiny)
library(shinysurveys)
library(googlesheets4)
library(dplyr)
library(tidyr)

gsheets = "https://docs.google.com/spreadsheets/d/1oEMCCRoSdRS-iiXGgWlku2h-4SgOREkCv65z2BxRsRE/edit?usp=sharing"
sheet_test = read_sheet(gsheets, col_names = FALSE)


# images ------------------------------------------------------------------
myImgResources <- paste0("imgResources/img", seq_len(10), ".jpg")

# Add directory of static resources to Shiny's web server
addResourcePath(prefix = "imgResources", directoryPath = "resources")


# questions ---------------------------------------------------------------
dataset = combn(myImgResources, 2)
dataset = as.data.frame(dataset)
dataset = as.list(dataset)

datasample = sample(dataset, 10)
# x[[1]][1]


questions_df = tibble::tibble()

for (i in 1:length(datasample)){
  question_id = datasample[[i]] %>% 
    stringr::str_remove_all("imgResources/") %>%
    stringr::str_remove_all(".jpg") %>%
    paste(collapse = '+')
  
  question = tibble::tibble(question = list(div(img(src=datasample[[i]][1], width="49%", height="250vh"),
                                                img(src=datasample[[i]][2], width="49%", height="250vh"),
                                                br(),
                                                paste0("Pytanie ", i, ": Określ podobieństwo między dwoma obrazami"))),
                            option = NA,
                            input_type = "textSlider",
                            input_id = question_id,
                            dependence = NA,
                            dependence_value = NA,
                            required = T,
                            page = i)
  
  
  questions_df = rbind(questions_df, question)
}


# app ---------------------------------------------------------------------
sliderScale <<- c(
  "Brak",
  "Bardzo male",
  "Niewielkie ",
  "Lekkie",
  "Duze",
  "Calkowite"
) #global variable bo inaczej nie dziala w linijce 63

# 1 - identyczne
# 2 - bardzo podobne
# 3 - trochę podobne
# 4 - niewielkie podobieństwo
# 5 - brak podobieństw



extendInputType(input_type = "textSlider", {
  shinyWidgets::sliderTextInput(
    inputId = surveyID(),
    label = surveyLabel(),
    force_edges = TRUE,
    choices = sliderScale,
    selected = sliderScale[4],
    grid = TRUE
  ) 
})

ui <- fluidPage(
  surveyOutput(df = questions_df,
               survey_title = "Zmiany struktury przestrzennej kategorii pokrycia terenu",
               survey_description = "Opis ankiety?")
)

server <- function(input, output, session) {
  renderSurvey()
  
  observeEvent(input$submit, {
    response_data = getSurveyData()
    response_data = subset(response_data, select = -c(question_type))
    print(response_data)
    sheet_append(gsheets, data = response_data)
    
    showModal(modalDialog(
      title = "Dziękuję za wypełnienie ankiety",
      "Kontakt: blakos1@st.amu.edu.pl",
      footer = actionButton("dismiss", "Powrót do strony głównej")
      #"You can customize what actions happen when a user finishes a survey using input$submit."
    ))
  })
  observeEvent(input$dismiss, session$reload())
  
}

shinyApp(ui, server)