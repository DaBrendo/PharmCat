library(shiny)
library(plotly)
library(gridlayout)
library(bslib)
library(DT)


ui <- grid_page(
  layout = c(
    "header header",
    "area4  Start ",
    ".      .     ",
    "outpit test  "
  ),
  row_sizes = c(
    "100px",
    "1.34fr",
    "0.48fr",
    "1.18fr"
  ),
  col_sizes = c(
    "1fr",
    "1fr"
  ),
  gap_size = "1rem",
  grid_card_text(
    area = "header",
    content = "PharmCat v3.0",
    alignment = "center",
    is_title = FALSE
  ),
  grid_card(
    area = "area4",
    card_header(strong("Inputs")),
    card_body(
      textInput(
        inputId = "THREEFOUR",
        label = "3/4ID",
        value = ""
      ),
      textInput(
        inputId = "m",
        label = "Project Dataset Name",
        value = ""
      ),
      checkboxGroupInput(
        inputId = "Med_Datasett",
        label = "Medication Datasets Needed",
        choices = list("Inpatient" = "a", "Home Meds" = "b")
      )
    )
  ),
  grid_card(
    area = "Start",
    card_header(strong("Commands")),
    card_body(
      radioButtons(
        inputId = "myRadioButtons",
        label = "RxCUI Lookup",
        choices = list("RxNav" = "a", "Local File" = "b"),
        width = "100%"
      ),
      actionButton(inputId = "myButton", label = "Run"),
      "RxCUIs Loaded?",
      textOutput(outputId = "textOutput")
    )
  ),
  grid_card(area = "outpit", card_body()),
  grid_card(
    area = "test",
    card_body(
      value_box(
        title = "Look at me!",
        showcase = bsicons::bs_icon("database")
      )
    )
  )
)


server <- function(input, output) {
   
  output$distPlot <- renderPlotly({
    # generate bins based on input$bins from ui.R
    plot_ly(x = ~ faithful[, 2], type = "histogram")
  })
  
  output$bluePlot <- renderPlot({
    # generate bins based on input$bins from ui.R
    x <- faithful[, 2]
    bins <- seq(min(x), max(x), length.out = input$bins + 1)
  
    # draw the histogram with the specified number of bins
    hist(x, breaks = bins, col = "steelblue", border = "white")
  })
  
  output$myTable <- renderDT({
    head(faithful, input$numRows)
  })
}

shinyApp(ui, server)
  

