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

# app ---------------------------------------------------------------------
server <- function(input, output, session) {
  
  sliderScale <<- c(
    "Brak",
    "Bardzo male",
    "Niewielkie ",
    "Lekkie",
    "Duze",
    "Calkowite"
  ) #global variable bo inaczej nie dziala w linijce 63
  
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
  
  filename_df = as.data.frame(list.files(path = "resources", full.names = TRUE))
  colnames(filename_df) = "filenames"
  
  filename_df$classes = as.integer(substr(filename_df$filenames, 15,15))
  filename_df$row = as.integer(substr(filename_df$filenames, 27,27))
  filename_df$col = as.integer(substr(filename_df$filenames, 32,32))
  
  dataset = list()
  samplesize = 12
  
  #2 klasy
  for(j in 1:2){ #pick row or col
    for(k in 1:6){ #pick items in rows/cols
      
      if (j == 2){
        subset_df = subset(filename_df, classes == 2 & col == k)
      } else {
        subset_df = subset(filename_df, classes == 2 & row == k)
      }
      
      smpl = subset_df$filenames %>% 
        combn(m = 2) %>% 
        as.data.frame() %>%
        as.list() %>% 
        sample(size = 1)
      
      dataset = append(dataset, smpl)
    }
  }
  
  #wybranie wszystkich obrazów z 2 klasami
  subset2 = subset(filename_df, classes == 2)$filenames %>% 
    combn(m = 2) %>% 
    as.data.frame() %>%
    as.list()
  
  #samplowanie tylko z reszty obrazów (2 klasy)
  smpl2 = subset(subset2, !(subset2 %in% dataset)) %>% 
    sample(size = samplesize)
  
  dataset = append(dataset, smpl2)
  
  #3 klasy
  for(j in 1:2){ #pick row or col
    for(k in 1:6){ #pick items in rows/cols
      
      if (j == 2){
        subset_df = subset(filename_df, classes == 3 & col == k)
      } else {
        subset_df = subset(filename_df, classes == 3 & row == k)
      }
      
      smpl = subset_df$filenames %>% 
        combn(m = 2) %>% 
        as.data.frame() %>%
        as.list() %>% 
        sample(size = 1)
      
      dataset = append(dataset, smpl)
    }
  }
  
  #wybranie wszystkich obrazów z 3 klasami
  subset3 = subset(filename_df, classes == 3)$filenames %>% 
    combn(m = 2) %>% 
    as.data.frame() %>%
    as.list()
  
  #samplowanie tylko z reszty obrazów (3 klasy)
  smpl3 = subset(subset3, !(subset3 %in% dataset)) %>% 
    sample(size = samplesize)
  
  dataset = append(dataset, smpl3)
  
  datasample = dataset
  questions_df = tibble::tibble()
  
  for (i in 1:length(datasample)){
    question_id = datasample[[i]] %>% 
      stringr::str_remove_all("resources/") %>%
      stringr::str_remove_all(".png") %>%
      paste(collapse = '+')
    
    question = tibble::tibble(question = list(div(#style="text-align:center",
      img(src=datasample[[i]][1], width="49%", height="49%"),
      img(src=datasample[[i]][2], width="49%", height="49%"),
      br(),
      paste0("Pytanie ", i, ": Określ podobieństwo (ilość kolorów i ich ułożenie) między dwoma obrazami")
    )),
    option = NA,
    input_type = "textSlider",
    input_id = question_id,
    dependence = NA,
    dependence_value = NA,
    required = T,
    page = i)
    
    
    questions_df = rbind(questions_df, question)
  }
  
  
  
  
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