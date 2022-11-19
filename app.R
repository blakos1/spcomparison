library(shiny)
library(shinysurveys)
library(googlesheets4)
library(dplyr)
library(tidyr)


# gsheets setup -----------------------------------------------------------
options(
  # whenever there is one account token found, use the cached token
  gargle_oauth_email = TRUE,
  # specify auth tokens should be stored in a hidden directory ".secrets"
  gargle_oauth_cache = ".secrets"
)

gsheets = "https://docs.google.com/spreadsheets/d/1oEMCCRoSdRS-iiXGgWlku2h-4SgOREkCv65z2BxRsRE/edit?usp=sharing"
sheet_test = read_sheet(gsheets, col_names = FALSE)


# images ------------------------------------------------------------------
# Add directory of static resources to Shiny's web server
addResourcePath(prefix = "resources", directoryPath = "resources")

datasets = c("sample1.csv","sample2.csv"
             # ,"sample3.csv","sample4.csv"
             )
dataset_name = sample(datasets,1)

# dataset_name = "sample1.csv"
datasample = as.list(read.csv(file = dataset_name, row.names = 1))
questions_df = tibble::tibble()

example_width = "49%"
example_height = "49%"

question1 = tibble::tibble(question = list(div(style="overflow-wrap:break-word",
  paste0("Pytanie 1/", length(datasample)+1, ": Jak interpretować różnice na obrazach?"),
  br(),
  br(),

  tags$table(
    tags$tr(
      tags$th(width="65%",
        div(
          img(src="resources/img_2classes_row6_col4.png", width=example_width, height=example_height),
          img(src="resources/img_2classes_row6_col5.png", width=example_width, height=example_height)
          ),
        tags$th("Inna liczba kolorów w tym samym układzie")
        )
      ),
    tags$tr(
      tags$th(
        div(
          img(src="resources/img_2classes_row1_col2.png", width=example_width, height=example_height),
          img(src="resources/img_2classes_row6_col2.png", width=example_width, height=example_height)
        ),
        tags$th("Ta sama liczba kolorów w innym układzie")
      )
    ),
    tags$tr(
      tags$th(
        div(
          img(src="resources/img_2classes_row2_col1.png", width=example_width, height=example_height),
          img(src="resources/img_2classes_row5_col5.png", width=example_width, height=example_height)
        ),
        tags$th("Różna liczba kolorów w różnych układach")
        )
      )
    ),
)),
option = "Zaznacz mnie aby przejść dalej",
input_type = "mc",
input_id = "example_question",
dependence = NA,
dependence_value = NA,
required = F,
page = 1)

questions_df = rbind(questions_df, question1)

for (i in 1:length(datasample)){
  question_id = datasample[[i]] %>% 
    stringr::str_remove_all("resources/") %>%
    stringr::str_remove_all(".png") %>%
    paste(collapse = '+')
  
  question = tibble::tibble(question = list(div(#style="text-align:center",
    img(src=datasample[[i]][1], width="49%", height="49%"),
    img(src=datasample[[i]][2], width="49%", height="49%"),
    br(),
    br(),
    paste0("Pytanie ", i+1, "/", length(datasample)+1, ": Określ podobieństwo (liczba kolorów i ich ułożenie) między dwoma obrazami")
  )),
  option = NA,
  input_type = "textSlider",
  input_id = question_id,
  dependence = NA,
  dependence_value = NA,
  required = T,
  page = i+1)
  
  
  questions_df = rbind(questions_df, question)
}


# sliderscale customization -----------------------------------------------
sliderScale <<- c(
  "Brak",
  "Bardzo małe",
  "Umiarkowane",
  "Bardzo duże",
  "Pełne"
) #global variable bo inaczej nie dziala w linijce 63

extendInputType(input_type = "textSlider", {
  shinyWidgets::sliderTextInput(
    inputId = surveyID(),
    label = surveyLabel(),
    force_edges = TRUE,
    choices = sliderScale,
    selected = sliderScale[3],
    grid = TRUE
  ) 
})


# app ---------------------------------------------------------------------
ui <- fluidPage(
  tags$head(tags$style(type='text/css', ".irs-grid-text { font-size: 14pt; }")),
  
  surveyOutput(df = questions_df,
               survey_title = "Zmiany struktury przestrzennej kategorii pokrycia terenu",
               survey_description = "Celem ankiety jest określenie różnic między
               postrzeganiem zmian struktur przestrzennych przez człowieka
               a uzyskanymi wartościami różnych miar niepodobieństwa.",
               theme = "#F4D03F") #"#F9E79F"?
)


server <- function(input, output, session) {
  renderSurvey()
  
  observeEvent(input$submit, {
    showModal(modalDialog(
      title = "Dziękuję za wypełnienie ankiety",
      "Kontakt: blakos1@st.amu.edu.pl",
      footer = actionButton("dismiss", "Powrót do strony głównej")
      #"You can customize what actions happen when a user finishes a survey using input$submit."
    ))
    
    response_data = getSurveyData()
    response_data = subset(response_data, select = -c(question_type))
    response_data$subject_id = sample(1:10000000000,1) #simulating random userIDs
    response_data$dataset_name = dataset_name
    response_data$sys_time = Sys.time()
    print(response_data)
    sheet_append(gsheets, data = response_data)
  })
  observeEvent(input$dismiss, session$reload())
}


shinyApp(ui, server)