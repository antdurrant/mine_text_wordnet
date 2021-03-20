library(word.lists)
library(shiny)
library(reticulate)
library(udpipe)
library(pdftools)
library(qdapTools)
library(shinythemes)
library(tidyverse)
library(tesseract)
library(writexl)


# nope, can't get these working right, no idea how this works

# # Create a virtual environment selecting your desired python version
#  virtualenv_create(envname = "python_environment", python= "python3")
# # # Explicitly install python libraries that you want to use, e.g. pandas, numpy
#  virtualenv_install("python_environment", packages = c("nltk"))
# # # Select the virtual environment
#  use_virtualenv("python_environment")
#
# nltk <- reticulate::import("nltk")
#
# synsets <- nltk$wordnet$wordnet$synsets



list_exp <- read_csv("./ref/list_exp.csv")

# Define UI ####
ui <-  fluidPage(
  ## theme ####
    theme =  shinytheme("cerulean"),
    h1("Produce word lists from text, word, or pdf"),
    p("This tool will scrape text out of your pdfs or word files, or parse text that you type directly into the textbox."),


  ## input data ####
    selectInput("type_of_text",
                "Select your text-type:",
                choices = c("typed-text",
                            "pdf-digital",
                            "pdf-scan",
                            "ms-word")
                ),
    uiOutput("type_exp"),

    uiOutput("input_type"),


  ## selections ####
    h4("Text Preview"),
    tags$details(tags$summary("Click to see how the text has been parsed"),
    tableOutput("text_check")
    ),


    h4("Output Preview"),
 sidebarLayout(
   sidebarPanel(
    uiOutput("select_preview_content"),

    uiOutput("select_language"),
    uiOutput("include_definitions"),


    br(),
    selectInput(
      "word_list",
      "Select a word-list to filter against:",
      choices = c(
          "NGSL",
          "NGSL + NAWL (Academic)",
          "NGSL + NBL (Business)",
          "NGSL + TOEIC",
          "NGSL + NBL + NAWL + TOEIC",
          "NGSL-35000",
          "Wikipedia-5000",
          "Flemma-5000",
          "New Dolch List"
        )
      ),
    uiOutput("filter_list"),
    uiOutput("filename"),
    uiOutput("dl"),
   ),
   mainPanel(

  ## about the list ####
    textOutput("list_exp"),


  ## preview ####
    dataTableOutput("preview_content"),
   )
),

  ## explanation of tool ####
  h4("About"),
  h5("About the output"),
  p("This tool is intended for ESL practitioners to expedite the process of making word-lists.",
    br(),
    "Be aware that it is not perfect and will still require some work on your end.",
    br(),
    "The translations do take into account Part-of-Speech, but do not handle Word-Sense-Disambiguation.",
    br(),
    "The first two translations for each meaning found for the word are provided, then filtered to produce the first four unique translations.
    That typically means that the most common three senses of the word (according to NLTK) will be given.
    Accordingly, the first three sense-definitions are provided, separated by semi-colons.",
    br(),
    "When you download, there is a count-in-doc column, which you can use to filter down to provide only the first occurrence of each term,
    as well as doc_id, sentence_id, token_id columns to help you get the words back in order if necessary, and the sentence that the term came from as a reference.
    Translations are only provided for verbs, adjectives, adverbs, and nouns.
    Check the information below to find out how large and connected the dictionaries are for each language.
    "),
  h5("How does it work?"),
  p("It uses the following tools:"),
  p(tags$a(href="https://docs.ropensci.org/pdftools", "the pdftools package"), ", which is built on ",
    tags$a(href = "0https://poppler.freedesktop.org", "libpoppler"),
    "to scrape text from digitally produced pdfs",
    br(),

    tags$a(href = "https://github.com/ropensci/tesseract", "the tesseract package"), "interface to ",
    tags$a(href = "https://opensource.google.com/projects/tesseract", "Tesseract"),
    "to perform OCR on scanned pdfs",
    br(),

    tags$a(href = "http://trinker.github.com/qdapTools/", "qdapTools"),
    "to pull text from Word documents",
    br(),

    tags$a(href = "https://github.com/bnosac/udpipe", "the udpipe package"), "interface to ",
    tags$a(href = "https://ufal.mff.cuni.cz/udpipe", "the udpipe project"), "which uses ",
    tags$a(href = "https://github.com/UniversalDependencies/UD_English-EWT/tree/master", "the EWT Universal Dependencies Model"),
    br(),

    tags$a(href = "https://www.python.org/", "python"), "module ",
    tags$a(href = "https://www.nltk.org/", "nltk"), "is used to interface with",
    tags$a(href = "http://compling.hss.ntu.edu.sg/omw/", "the Open Multilingual Wordnet"),
    "to provide translations and definitions.",
    br(),

    "The app itself is built with ",
    tags$a(href = "https://shiny.rstudio.com/", "shiny"), "a framework for making web-apps with ",
    tags$a(href = "https://www.r-project.org/", "R"), ", by ",
    tags$a(href = "linkedin.com/in/anthony-durrant-600a2159/", "Anthony Durrant"),
    "as a pet-project."

  )

)





# server logic ####
server <- function(input, output) {
  # allow up to 50mb files
  options(shiny.maxRequestSize = 50*1024^2)


# dynamic UI ####

  # output file-type explanation ####
  output$type_exp <- renderText({
        case_when(
        input$type_of_text == "pdf-digital" ~ "This will extract embedded text from digitally produced pdfs. Scans will come out with gibberish.",
        input$type_of_text == "pdf-scan" ~ "This will attempt to extract text from your pdf using Tesseract's OCR engine. If the scan is not very clean, it will struggle. This will also take a long time; don't bother trying with files more than ~5 pages long.",
        input$type_of_text == "ms-word" ~ "This will pull the text out of your Word file. It will not read any text in images in your file.",
        input$type_of_text == "typed-text" ~ "This will use the text you type in the box.",
        TRUE ~ "")
        })

    # output file-type ####
  output$input_type <- renderUI({
    req(input$type_of_text)
        pdf_input <-  list(fileInput("pdf",
                                        "Drag-and-drop or browse for your pdf",
                                        accept = c(".pdf")
        ))
        word_input <- list(fileInput("word",
                                        "Drag-and-drop or browse for your Word file",
                                        accept = c(".docx")
        ))
        text_input <- list(textAreaInput("text",
                                         "Type your text here:",
                                         value = "See this tool in action.",
                                         width = "100%",
                                         height = "200px"
        ))

        case_when(
            input$type_of_text == "pdf-digital" ~ pdf_input,
            input$type_of_text == "pdf-scan"    ~ pdf_input,
            input$type_of_text == "ms-word"     ~ word_input,
            input$type_of_text == "typed-text"  ~ text_input
            )

    })

      # output select what to preview ####
  output$select_preview_content <- renderUI({
    selectInput("preview_doc_ids",
                "Select which parts to preview",
                choices = text()$doc_id,
                multiple = TRUE,
                selected = 1
    )
  })


      # output include definitions? ####
  output$include_definitions <- renderUI({
    req(input$preview_doc_ids)
    radioButtons("def",
                 "Include English definitions of terms?",
                 choices = c("Yes" = TRUE, "No" = FALSE),
                 inline = TRUE)
  })


      # output select language #####
  output$select_language <- renderUI({
    req(input$def)
    selectInput("lang",
                 "Select language for translations",
                selected = "jpn",
                choices = c("English" = "eng",
                             "Finnish" = "fin",
                             "Thai" = "tha",
                             "French" = "fra",
                             "Japanese" = "jpn",
                             # "Romanian" = "ron",
                             "Catalan" = "cat",
                             "Portuguese" = "por",
                             "Slovenian" = "slv",
                             "Chinese - Mandarin" = "cmn",
                             "Bahasa" = "ind",
                             "Polish" = "pol",
                             "Dutch" = "nld",
                             "Basque" = "eus",
                             "Hebrew" = "heb",
                             "Spanish" = "spa",
                             # "Croatian" = "cro",
                             "Galician" = "glg",
                             "Greek" = "ell",
                             # "Slovakian" = "slk",
                             "Persian" = "fas",
                             "Italian" = "ita",
                             "Arabic" = "arb",
                             "Swedish" = "swe",
                             "Hebrew" = "heb",
                             # "Icelandic" = "isl",
                             "Danish" = "dan",
                             "Norwegian" = "nob",
                             "Bulgarian" = "bul",
                             "Chinese - Taiwanese" = "qcn"
                             )
                )
  })

    # output filename ####
  output$filename <- renderUI({
    req(input$lang)
    textInput ("file_name",
               label = NULL,
               placeholder = "Name your file for download.")
  })

      # output download ####
  output$dl <- renderUI({
    req(input$lang)
    downloadButton("download",
                   "Process and Download Vocab File")
  })


    # output list explanation ####
  output$list_exp <- renderText({
  list_exp %>% filter(list == input$word_list) %>% pull(exp)
} )

  # output filter list

  output$filter_list <- renderUI({
    checkboxGroupInput(
      "filter_groups",
      "Groups to include in wordlist:",
      choices =c("OFF LIST", "NUM", "NON-WORD", sort(unique(list_def()$group))),
      selected = c( unique(list_def()$group))
    )
  })


# non-displayed reactives ####

  # list conversion####
  list_def <- reactive({
        if(  input$word_list == "NGSL"  ){
          list_ngsl
        } else if(input$word_list ==   "NGSL + NAWL (Academic)" ){
          word.lists::list_academic
        } else if(input$word_list == "NGSL + TOEIC" ){
          word.lists::list_toeic
        } else if (input$word_list == "NGSL + NBL (Business)"){
          word.lists::list_business
        } else if(input$word_list == "NGSL + NBL + NAWL + TOEIC"){
          word.lists::list_general_plus
        } else if(input$word_list ==  "NGSL-35000"){
          word.lists::list_ngsl_all
        } else if(input$word_list == "Wikipedia-5000") {
          word.lists::list_wiki
        } else if(input$word_list == "Flemma-5000" ){
          word.lists::list_flemma
        } else if(input$word_list == "New Dolch List"){
          word.lists::list_dolch
        }
  })


  text <- reactive({

    withProgress( message = "reading file..." , {

      if(input$type_of_text %in% c("pdf-digital", "pdf-scan")){
        req(input$pdf$datapath)
        if(input$type_of_text == "pdf-digital") {
          tibble(text = pdftools::pdf_text(input$pdf$datapath)) %>%
            mutate(doc_id = row_number(), .before = text)
        } else if(input$type_of_text == "pdf-scan") {
          a <- tibble(text = tesseract::ocr(input$pdf$datapath)) %>%
            mutate(doc_id = row_number(), .before = text) %>%
            mutate(text = str_replace_all(text, "|", "I"))

          file.remove(list.files(pattern = "png"))
          a
        }

      } else if(input$type_of_text == "ms-word") {
        req(input$word$datapath)
        tibble(text = qdapTools::read_docx(input$word$datapath)) %>%
          mutate(doc_id = row_number(), .before = text)
      } else if(input$type_of_text == "typed-text"){
        req(input$text)
        tibble(text = input$text) %>%
          separate_rows(text, sep = "\n") %>%
          mutate(doc_id = row_number(), .before = text)
      }
    })
  })

  #
  piped <- reactive({
    withProgress(
      message = "converting file to text and parsing...",{

        text() %>%
          # filter(doc_id %in% input$preview_doc_ids) %>%
          udpipe(object = "english")  %>%
          select(doc_id, sentence_id, token_id, token, lemma, upos, sentence) %>%
          get_wordlist(language = input$lang,
                       def = input$def) %>%
          left_join(list_def()) %>%
          mutate(group = case_when(is.na(group) & str_detect(lemma, "[0-9]") ~ "NUM",
                                   is.na(group) & upos == "SYM"              ~ "NON-WORD",
                                   is.na(group)                              ~ "OFF LIST",
                                   TRUE                                      ~ as.character(group))
          ) %>%          filter(group %in% input$filter_groups) %>%
          select(group, everything()) %>%
          select(-doc_id, -sentence_id, -token_id, -pos, -sentence, -on_list, -contains("rank"))


      # udpipe(text(), object = "english") %>%
      # get_wordlist(language = input$lang,
      #               def = input$def) %>%
      # group_by(lemma) %>%
      # mutate(count_in_doc = row_number()) %>%
      # ungroup() %>%
      # left_join(list_def()) %>%
      # mutate(group = if_else(is.na(group), "OFF LIST", as.character(group))) %>%
      #  filter(group %in% input$filter_groups) %>%
      # select(-pos, -contains("rank"))
      })
    })


# output reactives ####

  output$text_check <- renderTable({
    head(text(), 2)
  })


  pipe_preview <- reactive({
    text() %>%
      filter(doc_id %in% input$preview_doc_ids) %>%
      udpipe(object = "english")
  })

  # output preview ####
  output$preview_content <- renderDataTable({


    req(nchar(input$lang) >=1)
     pipe_preview() %>%
      select(doc_id, sentence_id, token_id, token, lemma, upos, sentence) %>%
      get_wordlist(language = input$lang,
                    def = input$def) %>%
      left_join(list_def()) %>%
      mutate(group = case_when(is.na(group) & str_detect(lemma, "[0-9]") ~ "NUM",
                               is.na(group)                              ~ "OFF LIST",
                               TRUE                                      ~ as.character(group))
             ) %>%
      distinct(group, lemma, .keep_all = TRUE) %>%
      filter(group %in% input$filter_groups) %>%
      select(group, everything()) %>%
      select(-doc_id, -sentence_id, -token_id, -pos, -sentence, -on_list, -contains("rank"))
  },
  options = list(
    width = "100%",
    filter = "top"
  )

  )


  output$download <- downloadHandler(
      filename = function() {
        if (input$file_name != "" ) {
          glue::glue("{input$file_name}_{format(Sys.time(), '%Y-%m-%d-%H%M%S')}.xlsx")
        } else {
        # auto-name the file as it was uploaded
        glue::glue("vocab_output_{format(Sys.time(), '%Y-%m-%d-%H%M%S')}.xlsx")
      }
      },
      content = function(file) {
        # write excel workbook
        write_xlsx(piped(), file)
      }
    )






}

# Run the application
shinyApp(ui = ui, server = server)




