library(shiny)
# library(clinfun)

source("BayesMultiStage.R")

# Define UI for dataset viewer app ----
# ui <- fluidPage(
ui <- navbarPage("Single-endpoint Trial Design",
tabPanel("Trial Parameter Search",
  # App title ----
  titlePanel("Bayesian Predictive Probability"),

  # Sidebar layout with input and output definitions ----
  sidebarLayout(

    # Sidebar panel for inputs ----
    sidebarPanel(

      # Input: Select a dataset ----
      # selectInput("dataset", "Choose a dataset:",
                  # choices = c("rock", "pressure", "cars")),

      # Input: Specify the number of observations to view ----
	  # search_n3(min_n1 = 10, max_n=36, alpha = 0.1, beta = 0.1, p0 = 0.2, p1 = 0.4)

      helpText("Required Parameters"),
	  fluidRow(
		column(6, numericInput("salpha", "Type I error rate, (one-sided):", 0.1, min = 0.01, max = 0.99, step = 0.01)),
		column(6, numericInput("spower", "Power:", 0.9, min = 0.01, max = 0.99, step = 0.01))
	  ),
      # numericInput("alpha", "Type I error rate, (one-sided):", 0.1, min = 0.01, max = 0.99, step = 0.01),
      # withMathJax(), 
      # numericInput("alpha", "\\alpha", 0.05, min = 0.01, max = 0.99, step = 0.01),
      # numericInput("power", "Power:", 0.9, min = 0.01, max = 0.99, step = 0.01),
	  
      # numericInput("p0", "Response probability of poor drug, p0:", 0.2, min = 0.01, max = 0.99, step = 0.01),
      # numericInput("p1", "Response probability of good drug, p1:", 0.4, min = 0.01, max = 0.99, step = 0.01),
	  fluidRow(
		column(6, numericInput("sp0", "Response probability of poor drug, p0:", 0.2, min = 0.01, max = 0.99, step = 0.01)),
		column(6, numericInput("sp1", "Response probability of good drug, p1:", 0.4, min = 0.01, max = 0.99, step = 0.01))
	  ),

      # Include clarifying text ----
      numericInput("smin_n1", "Min sample size for first stage, min_n1:", 10, min = 1, step = 1),
      numericInput("smax_n", "Max total sample size, max_n:", 42, min = 1, step = 1),

      helpText("Choose at most one of the following two options for limiting the number of stages:"),
	  numericInput("sk", "Fixed number of stages, k:", NULL, min = 1, step = 1),
      numericInput("sd", "Fixed size of each stage, d:", NULL, min = 1, step = 1),

      helpText("Optional parameters for beta-binomial prior. Leave blank for the default."),
      # numericInput("a0", "Beta-binomial prior number of successes, a0 (default p0):", NULL, min = 0, max = 1, step = 0.01),
      # numericInput("b0", "Beta-binomial prior number of failures, b0 (default 1-p0):", NULL, min = 0, max = 1, step = 0.01),
	  fluidRow(
		column(6, numericInput("sa0", "Beta-binomial prior number of successes, a0 (default p0):", NULL, min = 0, max = 1, step = 0.01)),
		column(6, numericInput("sb0", "Beta-binomial prior number of failures, b0 (default 1-p0):", NULL, min = 0, max = 1, step = 0.01))
	  ),
      actionButton("supdate", "Calculate")

      # Input: actionButton() to defer the rendering of output ----
      # until the user explicitly clicks the button (rather than
      # doing it immediately when inputs change). This is useful if
      # the computations required to render output are inordinately
      # time-consuming.

    ),

    # Main panel for displaying outputs ----
    mainPanel(

      # Output: Header + summary of distribution ----
      h4("Calculated Trial Parameters"),
      verbatimTextOutput("summary"),

      # Output: Header + table of distribution ----
      h4("Plot of efficacy cutoffs at each stage"),
      # tableOutput("view"),
      plotOutput("distPlot"),
	  
      h4("Effect of p0 on T1E rate"),
      plotOutput("p0_t1E_Plot"),

      h4("Effect of p1 on power"),
      plotOutput("p1_power_Plot")

    )

  )
),
tabPanel("Fixed Parameter Entry",
  titlePanel("Bayesian Predictive Probability"),

  # Sidebar layout with input and output definitions ----
  sidebarLayout(

    # Sidebar panel for inputs ----
    sidebarPanel(

      # Input: Select a dataset ----
      # selectInput("dataset", "Choose a dataset:",
                  # choices = c("rock", "pressure", "cars")),

      # Input: Specify the number of observations to view ----
	  # search_n3(min_n1 = 10, max_n=36, alpha = 0.1, beta = 0.1, p0 = 0.2, p1 = 0.4)

      helpText("Required Parameters"),
      # withMathJax(), 
      # numericInput("alpha", "\\alpha", 0.05, min = 0.01, max = 0.99, step = 0.01),
	  fluidRow(
		column(6, numericInput("fp0", "Response probability of poor drug, p0:", 0.2, min = 0.01, max = 0.99, step = 0.01)),
		column(6, numericInput("fp1", "Response probability of good drug, p1:", 0.4, min = 0.01, max = 0.99, step = 0.01))
	  ),
      textInput("fn", "Planned number of participants at each stage, n: (comma delimited, e.g. 10,20)", "10,13,18,21,25,28,30,32,34,35,36"),

      helpText("Specify either cx alone or both theta_L AND theta_T:"),
	  # textInput("cx", "Response count cutoffs at each stage, cx: (comma delimited, e.g. 5,10)", "0,1,2,3,4,5,6,7,8,9,10"),
	  textInput("fcx", "Response count cutoffs at each stage, cx: (comma delimited, e.g. 5,10)", ""),
      helpText("--or--"),
	  fluidRow(
		column(6, numericInput("ftheta_T", "Posterior probability threshold, theta_T:", 0.922, min = 0, max = 1, step = 0.001)),
		column(6, numericInput("ftheta_L", "Predictive probability threshold, theta_L:", 0.008, min = 0, max = 1, step = 0.001))
	  ),
      helpText("Optional parameters for beta-binomial prior. Leave blank for the default."),
	  fluidRow(
		column(6, numericInput("fa0", "Beta-binomial prior number of successes, a0 (default p0):", NULL, min = 0, max = 1, step = 0.01)),
		column(6, numericInput("fb0", "Beta-binomial prior number of failures, b0 (default 1-p0):", NULL, min = 0, max = 1, step = 0.01))
	  ),
      actionButton("fupdate", "Calculate")

      # Input: actionButton() to defer the rendering of output ----
      # until the user explicitly clicks the button (rather than
      # doing it immediately when inputs change). This is useful if
      # the computations required to render output are inordinately
      # time-consuming.

    ),

    # Main panel for displaying outputs ----
    mainPanel(

      # Output: Header + summary of distribution ----
      h4("Trial performance"),
      verbatimTextOutput("perf")#,

      # Output: Header + table of distribution ----
      # h4("Plot of efficacy cutoffs at each stage"),
      # tableOutput("perf")#,
      # plotOutput("distPlot"),
	  
      # h4("Effect of p0 on T1E rate"),
      # plotOutput("p0_t1E_Plot"),

      # h4("Effect of p1 on power"),
      # plotOutput("p1_power_Plot")

    )

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
  datasetInput <- eventReactive(input$supdate, {
    # list(pu = input$p0, pa = input$p1, ep1 = input$alpha, ep2 = 1-input$power)
	# Sys.sleep(10)
    # ph2simon(pu = input$p0, pa = input$p1, ep1 = input$alpha, ep2 = 1-input$power)
	# input$a0
	a0 = ifelse(is.na(input$sa0), input$sp0, input$sa0)
	b0 = ifelse(is.na(input$sb0), 1 - input$sp0, input$sb0)
	# result = search_n3(min_n1 = input$min_n1, max_n = input$max_n, alpha = input$alpha, beta = 1-input$power, p0 = input$p0, p1 = input$p1)
	# result = search_n3(k = input$k, min_n1 = input$min_n1, d = input$d, max_n = input$max_n, alpha = input$alpha, beta = 1-input$power, p0 = input$p0, p1 = input$p1, a0 = input$a0, b0 = input$b0)
	result = search_n3(k = input$sk, min_n1 = input$smin_n1, d = input$sd, max_n = input$smax_n, alpha = input$salpha, beta = 1-input$spower, p0 = input$sp0, p1 = input$sp1, a0 = a0, b0 = b0)
	# minimax = result[result$design == "minimax",]
	# optimal = result[result$design == "optimal",]
	
	# minimax_n = eval(parse(text = paste0("c(", minimax$n, ")")))
	# minimax_r = eval(parse(text = paste0("c(", minimax$x_fail, ")")))

	# minimax_idx_nodup = which(!is.na(minimax_r) & !duplicated(minimax_r))
	# minimax_n_nodup = minimax_n[minimax_idx_nodup]
	# minimax_r_nodup = minimax_r[minimax_idx_nodup]
	
	# optimal_n = eval(parse(text = paste0("c(", optimal$n, ")")))
	# optimal_r = eval(parse(text = paste0("c(", optimal$x_fail, ")")))

	# optimal_idx_nodup = which(!is.na(optimal_r) & !duplicated(optimal_r))
	# optimal_n_nodup = optimal_n[optimal_idx_nodup]
	# optimal_r_nodup = optimal_r[optimal_idx_nodup]
	
	# result[result$design == "minimax","n"] = paste0(minimax_n_nodup, collapse=",")
	# result[result$design == "minimax","x_fail"] = paste0(minimax_r_nodup, collapse=",")
	# result[result$design == "optimal","n"] = paste0(optimal_n_nodup, collapse=",")
	# result[result$design == "optimal","x_fail"] = paste0(optimal_r_nodup, collapse=",")
	result

  }, ignoreNULL = T)

  # Generate a summary of the dataset ----
  output$summary <- renderPrint({
    # dataset <- datasetInput()
    # summary(dataset)
	
	# ph2simon(0.05, 0.15, 0.05, 0.2)
	# ph2simon(input$p0, input$p1, input$alpha, 1-input$power)
    dataset <- datasetInput()
	# do.call(what = ph2simon, args = dataset)
	t.dataset = as.data.frame(t(dataset))
	names(t.dataset) = NULL
	t.dataset
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
		# ph2out = dataset$out
		# minimax_idx = which.min(ph2out[,"n"])
		# optimal_idx = which.min(ph2out[,"EN(p0)"])
		minimax = dataset[dataset$design == "minimax",]
		optimal = dataset[dataset$design == "optimal",]
		
		minimax_n = eval(parse(text = paste0("c(", minimax$n, ")")))
		minimax_r = eval(parse(text = paste0("c(", minimax$x_fail, ")")))

		# minimax_idx_nodup = which(!is.na(minimax_r) & !duplicated(minimax_r))
		# minimax_n_nodup = minimax_n[minimax_idx_nodup]
		# minimax_r_nodup = minimax_r[minimax_idx_nodup]
		
		optimal_n = eval(parse(text = paste0("c(", optimal$n, ")")))
		optimal_r = eval(parse(text = paste0("c(", optimal$x_fail, ")")))

		# optimal_idx_nodup = which(!is.na(optimal_r) & !duplicated(optimal_r))
		# optimal_n_nodup = optimal_n[optimal_idx_nodup]
		# optimal_r_nodup = optimal_r[optimal_idx_nodup]
		
		# plot(x = c(minimax_n_nodup, optimal_n_nodup), y = c(minimax_r_nodup, optimal_r_nodup),
			# col = c(rep("blue", length(minimax_n_nodup)), rep("red", length(optimal_n_nodup))),
			# pch = c(rep(1, length(minimax_n_nodup)), rep(2, length(optimal_n_nodup))),
			# cex = 3, lwd = 3, las = 1,
			# xlab = "n",
			# ylab = "CxR",
			# xlim = c(0,max(optimal_n_nodup)), ylim = c(0,max(minimax_r_nodup, optimal_r_nodup, na.rm=T)))
		plot(x = c(minimax_n, optimal_n), y = c(minimax_r, optimal_r),
			col = c(rep("blue", length(minimax_n)), rep("red", length(optimal_n))),
			pch = c(rep(1, length(minimax_n)), rep(2, length(optimal_n))),
			cex = 3, lwd = 3, las = 1, cex.lab = 1.5, cex.axis = 1.5,
			xlab = "n",
			ylab = "Observed Responses Cutoff",
			xlim = c(0,max(optimal_n)), ylim = c(0,max(minimax_r, optimal_r, na.rm=T)))
		legend("topleft", legend = c("Minimax", "Optimal"), 
			col = c("blue", "red"), pch = c(1,2), pt.cex = 3, pt.lwd = 3, cex=1.5, bty = "n")
    })
    output$p0_t1E_Plot <- renderPlot({
		# plot(1:10)
	    dataset <- datasetInput()
		minimax = dataset[dataset$design == "minimax",]
		optimal = dataset[dataset$design == "optimal",]
		
		minimax_n = eval(parse(text = paste0("c(", minimax$n, ")")))
		minimax_r = eval(parse(text = paste0("c(", minimax$x_fail, ")")))
		optimal_n = eval(parse(text = paste0("c(", optimal$n, ")")))
		optimal_r = eval(parse(text = paste0("c(", optimal$x_fail, ")")))

		minimax_cx = list(x_fail = minimax_r, x_pass = c(rep(NA, length(minimax_r)-1), max(minimax_r, na.rm=T)))
		optimal_cx = list(x_fail = optimal_r, x_pass = c(rep(NA, length(optimal_r)-1), max(optimal_r, na.rm=T)))
		p0 = seq(input$sp0-0.05, input$sp0+0.05, by = 0.01)
		minimax_T1E = vapply(X = p0, FUN = p_success_mat, FUN.VALUE = 1, n = minimax_n, cx = minimax_cx)
		optimal_T1E = vapply(X = p0, FUN = p_success_mat, FUN.VALUE = 1, n = optimal_n, cx = optimal_cx)

		# par(mar = par("mar")+c(0,10,0,0))
		plot(x = c(p0, p0), y = c(minimax_T1E, optimal_T1E),
			col = c(rep("blue", length(p0)), rep("red", length(p0))),
			pch = c(rep(1, length(p0)), rep(2, length(p0))),
			cex = 3, lwd = 3, #las = 1, 
			cex.lab = 1.5, cex.axis = 1.5,
			xlab = "Unacceptable Response Rate p0",
			ylab = "Type 1 Error",
			ylim = c(min(minimax_T1E, optimal_T1E, na.rm=T),max(minimax_T1E, optimal_T1E, na.rm=T)))
		abline(h = input$salpha, lty = 3, lwd = 2)
		legend("topleft", legend = c("Minimax", "Optimal"), 
			col = c("blue", "red"), pch = c(1,2), pt.cex = 3, pt.lwd = 3, cex=1.5, bty = "n")
	})

    output$p1_power_Plot <- renderPlot({
		# plot(1:10)
	    dataset <- datasetInput()
		minimax = dataset[dataset$design == "minimax",]
		optimal = dataset[dataset$design == "optimal",]
		
		minimax_n = eval(parse(text = paste0("c(", minimax$n, ")")))
		minimax_r = eval(parse(text = paste0("c(", minimax$x_fail, ")")))
		optimal_n = eval(parse(text = paste0("c(", optimal$n, ")")))
		optimal_r = eval(parse(text = paste0("c(", optimal$x_fail, ")")))

		minimax_cx = list(x_fail = minimax_r, x_pass = c(rep(NA, length(minimax_r)-1), max(minimax_r, na.rm=T)))
		optimal_cx = list(x_fail = optimal_r, x_pass = c(rep(NA, length(optimal_r)-1), max(optimal_r, na.rm=T)))
		p1 = seq(input$sp1-0.05, input$sp1+0.05, by = 0.01)
		minimax_pow = vapply(X = p1, FUN = p_success_mat, FUN.VALUE = 1, n = minimax_n, cx = minimax_cx)
		optimal_pow = vapply(X = p1, FUN = p_success_mat, FUN.VALUE = 1, n = optimal_n, cx = optimal_cx)

		# par(mar = par("mar")+c(0,10,0,0))
		plot(x = c(p1, p1), y = c(minimax_pow, optimal_pow),
			col = c(rep("blue", length(p1)), rep("red", length(p1))),
			pch = c(rep(1, length(p1)), rep(2, length(p1))),
			cex = 3, lwd = 3, #las = 1, 
			cex.lab = 1.5, cex.axis = 1.5,
			xlab = "Acceptable Response Rate p1",
			ylab = "Power",
			ylim = c(min(minimax_pow, optimal_pow, na.rm=T),max(minimax_pow, optimal_pow, na.rm=T)))
		abline(h = input$spower, lty = 3, lwd = 2)
		legend("bottomright", legend = c("Minimax", "Optimal"), 
			col = c("blue", "red"), pch = c(1,2), pt.cex = 3, pt.lwd = 3, cex=1.5, bty = "n")
	})

  fixedInput <- eventReactive(input$fupdate, {
	#check if both cx and theta were specified
	stopifnot((input$fcx != "" && is.na(input$ftheta_T) && is.na(input$ftheta_L)) || 
		(input$fcx == "" && !is.na(input$ftheta_T) && !is.na(input$ftheta_L)))

	n = as.integer(unlist(strsplit(input$fn, ",")))
	a0 = ifelse(is.na(input$fa0), input$fp0, input$fa0)
	b0 = ifelse(is.na(input$fb0), 1 - input$fp0, input$fb0)
	#calc cutoffs if not given
	
	#TODO: continue here
	cx = NA
	if(input$fcx == ""){
		cx = calc_cutoffs(theta_L = input$ftheta_L, theta_T = input$ftheta_T, p0 = input$fp0, p1 = input$fp1, a0 = a0, b0 = b0, n = n)
	}
	else{
		cx = list(x_fail = as.integer(unlist(strsplit(input$fcx, ","))), 
			x_pass = c(rep(NA, length(n)-1), max(as.integer(unlist(strsplit(input$fcx, ","))), na.rm=T)+1))
	}
	# cx = ifelse(input$cx == "", 
		# calc_cutoffs(theta_L = input$theta_L, theta_T = input$theta_T, p0 = input$p0, p1 = input$p1, a0 = a0, b0 = b0, n = n),
		# list(x_fail = as.integer(unlist(strsplit(input$cx, ","))), x_pass = c(rep(NA, length(n)-1), max(as.integer(unlist(strsplit(input$cx, ","))), na.rm=T)+1)))
	
	# list(input$n, input$p0, input$p1, input$a0, input$b0, input$cx, input$theta_L, input$theta_T, cx)
	# list(cx$x_fail, cx$x_pass)
	# names(cx)
	# result = search_n3(k = input$k, min_n1 = input$min_n1, d = input$d, max_n = input$max_n, alpha = input$alpha, beta = 1-input$power, p0 = input$p0, p1 = input$p1, a0 = a0, b0 = b0)
	# result
	result = trial_perf(n = n, p0 = input$fp0, p1 = input$fp1, cx = cx)
	data.frame(c(round(result,4), n = paste0(n, collapse=","), x_fail = paste0(cx$x_fail, collapse=",")))
  }, ignoreNULL = T)

  output$perf <- renderPrint({
    # dataset <- datasetInput()
    # summary(dataset)
	
	# ph2simon(0.05, 0.15, 0.05, 0.2)
	# ph2simon(input$p0, input$p1, input$alpha, 1-input$power)
    fixed <- fixedInput()
	# fixed
	names(fixed) = NULL
	fixed
  })

}

# Create Shiny app ----
shinyApp(ui, server)
