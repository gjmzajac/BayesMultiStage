library(shiny)
library(clinfun)

source("BayesMultiStage.R")

# Define UI for dataset viewer app ----
ui <- fluidPage(

  # App title ----
  titlePanel("More Widgets"),

  # Sidebar layout with input and output definitions ----
  sidebarLayout(

    # Sidebar panel for inputs ----
    sidebarPanel(

      # Input: Select a dataset ----
      # selectInput("dataset", "Choose a dataset:",
                  # choices = c("rock", "pressure", "cars")),

      # Input: Specify the number of observations to view ----
      numericInput("alpha", "Type I error rate, α (one-sided):", 0.05, step = 0.01),
      numericInput("power", "Power:", 0.8, step = 0.01),
      numericInput("p0", "Response probability of poor drug, p0:", 0.05, step = 0.01),
      numericInput("p1", "Response probability of good drug, p1:", 0.15, step = 0.01),

      # Include clarifying text ----
      helpText("Note: while the data view will show only the specified",
               "number of observations, the summary will still be based",
               "on the full dataset."),

      # Input: actionButton() to defer the rendering of output ----
      # until the user explicitly clicks the button (rather than
      # doing it immediately when inputs change). This is useful if
      # the computations required to render output are inordinately
      # time-consuming.
      actionButton("update", "Update View")

    ),

    # Main panel for displaying outputs ----
    mainPanel(

      # Output: Header + summary of distribution ----
      h4("Summary"),
      verbatimTextOutput("summary"),

      # Output: Header + table of distribution ----
      h4("Observations"),
      tableOutput("view"),
      plotOutput("distPlot")

    )

  )
)

# Define server logic to summarize and view selected dataset ----
server <- function(input, output) {

  # Return the requested dataset ----
  # Note that we use eventReactive() here, which depends on
  # input$update (the action button), so that the output is only
  # updated when the user clicks the button
  # datasetInput <- eventReactive(input$update, {
    # switch(input$dataset,
           # "rock" = rock,
           # "pressure" = pressure,
           # "cars" = cars)
  # }, ignoreNULL = FALSE)
  datasetInput <- eventReactive(input$update, {
    # list(pu = input$p0, pa = input$p1, ep1 = input$alpha, ep2 = 1-input$power)
	# Sys.sleep(10)
    ph2simon(pu = input$p0, pa = input$p1, ep1 = input$alpha, ep2 = 1-input$power)
  }, ignoreNULL = T)

  # Generate a summary of the dataset ----
  output$summary <- renderPrint({
    # dataset <- datasetInput()
    # summary(dataset)
	
	# ph2simon(0.05, 0.15, 0.05, 0.2)
	# ph2simon(input$p0, input$p1, input$alpha, 1-input$power)
    dataset <- datasetInput()
	# do.call(what = ph2simon, args = dataset)
	dataset
  })

  # Show the first "n" observations ----
  # The use of isolate() is necessary because we don't want the table
  # to update whenever input$obs changes (only when the user clicks
  # the action button)
  # output$view <- renderTable({
    # head(datasetInput(), n = isolate(input$alpha))
  # })
  
  
    output$distPlot <- renderPlot({

#     x    <- faithful$waiting
#     bins <- seq(min(x), max(x), length.out = input$bins + 1)
# 
#     hist(x, breaks = bins, col = "#75AADB", border = "white",
#          xlab = "Waiting time to next eruption (in mins)",
#          main = "Histogram of waiting times")

#   sim_geno(pA = input$bins)
  # randomVals()
#   sim_geno(pA = randomVals())
	    dataset <- datasetInput()
		ph2out = dataset$out
		minimax_idx = which.min(ph2out[,"n"])
		optimal_idx = which.min(ph2out[,"EN(p0)"])
		minimax = ph2out[minimax_idx,]
		optimal = ph2out[optimal_idx,]
		plot(x = c(minimax[["n1"]], minimax[["n"]], optimal[["n1"]], optimal[["n"]]), y = c(minimax[["r1"]], minimax[["r"]], optimal[["r1"]], optimal[["r"]]),
			col = c("blue", "blue", "red", "red"),
			cex = 3, lwd = 3, las = 1,
			xlab = "n",
			ylab = "r",
			xlim = c(0,optimal[["n"]]), ylim = c(0,max(minimax[["r"]], optimal[["r"]])))
    })


}

# Create Shiny app ----
shinyApp(ui, server)
