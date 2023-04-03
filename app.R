library(tidyverse)
library(readxl)
library(shiny)


il_vekil <- read_excel("il_vekil.xlsx")
party_names <- c("AKP", "CHP", "HDP/YSP", "IYI", "MHP", "SP", "DEVA", "GP", "YRP", "TIP")

ui <- fluidPage(
  tags$head(
    tags$style(
      HTML("
      .sidebar {
        position: fixed !important;
        top: 0;
        left: 0;
        bottom: 0;
        overflow-y: auto;
        padding: 20px;
        width: 300px;
        background-color: #f8f9fa;
      }
    ")
    )
  ),
  
  tags$head(
    tags$style(
      HTML("
        h3 {
          font-size: 14px;
          text-align:center;
          font-weight: bold;
        }
        .dhont-input-box {
          width: 220px;
          margin-left: 60px;
          margin-right: 0px;
          align-items: right;
        }
        .dhont-input-container {
          display: flex;
          flex-direction: row;
          justify-content: flex-end;
          align-items: center;
          margin-bottom: 5px;
        }
      ")
    )
  ),
  tags$style(
    HTML("
      * {
        font-family: 'Fira Sans', sans-serif;
      }
    ")
  ),
  
  tags$head(tags$style("#vekil_output {font-weight: bold; font-size: 34px; text-align:center; color: darkblue}")),
  
  titlePanel("D'Hondt Yöntemi ile Sandalye Dağılımı"),
  sidebarLayout(
    sidebarPanel(
      selectInput("il", "İlgili seçim bölgesine ait sandalye sayısını görmek için aşağıdaki kutucuktan bir il seçiniz", choices = il_vekil$il),
      textOutput("vekil_output"),
      
      h3("Partilere Ait Oy Miktarını Girin (Yüzde kullanabilirsiniz)"),
      lapply(1:10, function(i) {
        tagList(
          div(
            class = "dhont-input-container",
            tags$label(paste0(party_names[i], ":")),
            tags$input(
              type = "number",
              class = "dhont-input-box",
              id = paste0("votes_", i),
              name = party_names[i],
              min = 0,
              max = 100,
              value = 0
            )
          )
        )
      }),
      
    ),
    mainPanel(
      tableOutput("seat_table")
    )
  )
)

server <- function(input, output) {
  
  il_vekil <- read_excel("il_vekil.xlsx")
  
  parties <- reactive({
    tibble(
      party = party_names,
      votes = sapply(party_names, function(p) input[[paste0("votes_", which(party_names == p))]])
    )
  })
  
  num_Sandalye <- reactive({
    il_vekil %>%
      filter(il == input$il) %>%
      pull(vekil)
  })
  
  allocation <- reactive({
    quota <- sum(parties()$votes) / num_Sandalye()
    Sandalye <- rep(0, 10)
    for (i in 1:num_Sandalye()) {
      ratios <- parties()$votes / (Sandalye + 1)
      next_seat <- which.max(ratios)
      Sandalye[next_seat] <- Sandalye[next_seat] + 1
    }
    tibble(
      Parti = parties()$party,
      Sandalye = round(Sandalye, 0)
    )
  })
  
  output$seat_table <- renderTable({
    allocation() %>%
      mutate(Sandalye = round(Sandalye, 0)) %>%
      mutate(Sandalye = format(Sandalye, big.mark = ""))
  })
  
  
  output$vekil_output <- renderText({
    il_vekil %>%
      filter(il == input$il) %>%
      pull(vekil)
  })
  
  lapply(1:10, function(i) {
    output[[paste0("party_", i)]] <- renderText({
      party_names[i]
    })
  })
}

shinyApp(ui, server)

