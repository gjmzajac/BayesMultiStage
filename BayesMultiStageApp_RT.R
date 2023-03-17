library(shiny)
# library(clinfun)

source("BayesMultiStage.R")

# Define UI for dataset viewer app ----
# ui <- fluidPage(
ui <- navbarPage("Two-endpoint Independent Trial Design",
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
		column(4, numericInput("salpha_R", "Response Type I error rate, (one-sided):", 0.15, min = 0.01, max = 0.99, step = 0.01)),
		column(4, numericInput("salpha_T", "Nontoxicity Type I error rate, (one-sided):", 0.15, min = 0.01, max = 0.99, step = 0.01)),
		column(4, numericInput("spower", "Power:", 0.85, min = 0.01, max = 0.99, step = 0.01))
	  ),
      # numericInput("alpha", "Type I error rate, (one-sided):", 0.1, min = 0.01, max = 0.99, step = 0.01),
	  # withMathJax(helpText("Some math here $$\\alpha+\\beta$$"))
	  # div("more math here $$\\sqrt{2}$$")

      # numericInput("alpha", "\\alpha", 0.05, min = 0.01, max = 0.99, step = 0.01),
      # numericInput("power", "Power:", 0.9, min = 0.01, max = 0.99, step = 0.01),
	  
      # numericInput("p0", "Response probability of poor drug, p0:", 0.2, min = 0.01, max = 0.99, step = 0.01),
      # numericInput("p1", "Response probability of good drug, p1:", 0.4, min = 0.01, max = 0.99, step = 0.01),
	  fluidRow(
		column(3, numericInput("spR0", "Response probability of poor drug, pR0:", 0.3, min = 0.01, max = 0.99, step = 0.01)),
		column(3, numericInput("spR1", "Response probability of good drug, pR1:", 0.5, min = 0.01, max = 0.99, step = 0.01)),
	  # ),

	  # fluidRow(
		column(3, numericInput("spT0", "Nontoxicity probability of poor drug, pT0:", 0.6, min = 0.01, max = 0.99, step = 0.01)),
		column(3, numericInput("spT1", "Nontoxicity probability of good drug, pT1:", 0.8, min = 0.01, max = 0.99, step = 0.01))
	  ),

      # Include clarifying text ----
      numericInput("smin_n1", "Min sample size for first stage, min_n1:", 19, min = 1, step = 1),
      numericInput("smax_n", "Max total sample size, max_n:", 33, min = 1, step = 1),

      helpText("Choose at most one of the following two options for limiting the number of stages:"),
	  numericInput("sk", "Fixed number of stages, k:", NULL, min = 1, step = 1),
      numericInput("sd", "Fixed size of each stage, d:", NULL, min = 1, step = 1),

      helpText("Optional parameters for beta-binomial prior. Leave blank for the default."),
      # numericInput("a0", "Beta-binomial prior number of successes, a0 (default p0):", NULL, min = 0, max = 1, step = 0.01),
      # numericInput("b0", "Beta-binomial prior number of failures, b0 (default 1-p0):", NULL, min = 0, max = 1, step = 0.01),
	  fluidRow(
		column(3, numericInput("saR0", "Response Beta-binomial prior number of successes, aR0 (default pR0):", NULL, min = 0, max = 1, step = 0.01)),
		column(3, numericInput("sbR0", "Response Beta-binomial prior number of failures, bR0 (default 1-pR0):", NULL, min = 0, max = 1, step = 0.01)),
		column(3, numericInput("saT0", "Nontoxicity Beta-binomial prior number of successes, aT0 (default pT0):", NULL, min = 0, max = 1, step = 0.01)),
		column(3, numericInput("sbT0", "Nontoxicity Beta-binomial prior number of failures, bT0 (default 1-pT0):", NULL, min = 0, max = 1, step = 0.01))
	  ),
      actionButton("supdate", "Calculate"),
	  width = 6
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
      plotOutput("p1_power_Plot"),
	  width=6
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
	  withMathJax(),
      # withMathJax(), 
      # numericInput("alpha", "\\alpha", 0.05, min = 0.01, max = 0.99, step = 0.01),
	  fluidRow(
		column(3, numericInput("fpR0", "Response probability of poor drug, \\(p_{R0}\\):", 0.3, min = 0.01, max = 0.99, step = 0.01)),
		column(3, numericInput("fpR1", "Response probability of good drug, \\(p_{R1}\\):", 0.5, min = 0.01, max = 0.99, step = 0.01)),
		column(3, numericInput("fpT0", "Nontoxicity probability of poor drug, \\(p_{T0}\\):", 0.6, min = 0.01, max = 0.99, step = 0.01)),
		column(3, numericInput("fpT1", "Nontoxicity probability of good drug, \\(p_{T1}\\):", 0.8, min = 0.01, max = 0.99, step = 0.01))
	  ),
      textInput("fn", "Planned number of participants at each stage, \\(n\\): (comma delimited, e.g. 10,20)", "19,33"),

	  # withMathJax(helpText("Specify either cxR alone or both theta_RL AND theta_RT: $$\\theta_{RL}$$")),
	  helpText('Specify either \\(C_R\\) alone or both \\(\\theta_{RL}\\) AND \\(\\theta_{RT}\\):'),

  # withMathJax(),
  # section below allows in-line LaTeX via $ in mathjax. Replace less-than-sign with < 
  # and grater-than-sign with >
  # tags$div(HTML("less-than-sign script type='text/x-mathjax-config' greater-than-sign
                # MathJax.Hub.Config({
                # tex2jax: {inlineMath: [['$','$'], ['\\(','\\)']]}
                # });
                # less-than-sign /script greater-than-sign
                # ")),

	  # withMathJax(helpText("Some math here $$\\alpha+\\beta$$")),
	  # div("more math here $$\\sqrt{2}$$"),

	  # textInput("cx", "Response count cutoffs at each stage, cx: (comma delimited, e.g. 5,10)", "0,1,2,3,4,5,6,7,8,9,10"),
	  fluidRow(
		column(5, textInput("fcxR", "Response count cutoffs at each stage, \\(C_R\\): (comma delimited, e.g. 5,10)", "")),
		column(1,helpText("or")),
		column(3, numericInput("ftheta_RT", "Response Posterior probability threshold, \\(\\theta_{RT}\\):", 0.8, min = 0, max = 1, step = 0.001)),
		column(3, numericInput("ftheta_RL", "Response Predictive probability threshold, \\(\\theta_{RL}\\):", 0.1, min = 0, max = 1, step = 0.001))
	  ),
      # helpText("or"),
	  # helpText("Specify either cxT alone or both theta_TL AND theta_TT:"),
	  helpText('Specify either \\(C_T\\) alone or both \\(\\theta_{TL}\\) AND \\(\\theta_{TT}\\):'),
	  fluidRow(
		column(5, textInput("fcxT", "Nontoxicity count cutoffs at each stage, \\(C_T\\): (comma delimited, e.g. 5,10)", "")),
		column(1,helpText("or")),
		column(3, numericInput("ftheta_TT", "Nontoxicity Posterior probability threshold, \\(\\theta_{TT}\\):", 0.8, min = 0, max = 1, step = 0.001)),
		column(3, numericInput("ftheta_TL", "Nontoxicity Predictive probability threshold, \\(\\theta_{TL}\\):", 0.1, min = 0, max = 1, step = 0.001))
	  ),
      helpText("Optional parameters for beta-binomial prior. Leave blank for the default."),
	  fluidRow(
		column(3, numericInput("faR0", "Response Beta-binomial prior number of successes, aR0 (default pR0):", NULL, min = 0, max = 1, step = 0.01)),
		column(3, numericInput("fbR0", "Response Beta-binomial prior number of failures, bR0 (default 1-pR0):", NULL, min = 0, max = 1, step = 0.01)),
		column(3, numericInput("faT0", "Nontoxicity Beta-binomial prior number of successes, aT0 (default pT0):", NULL, min = 0, max = 1, step = 0.01)),
		column(3, numericInput("fbT0", "Nontoxicity Beta-binomial prior number of failures, bT0 (default 1-pT0):", NULL, min = 0, max = 1, step = 0.01))
	  ),
      actionButton("fupdate", "Calculate"),
	  width=6

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
      verbatimTextOutput("perf"),

      # Output: Header + table of distribution ----
      # h4("Plot of efficacy cutoffs at each stage"),
      # tableOutput("perf")#,
      # plotOutput("distPlot"),
	  
      # h4("Effect of p0 on T1E rate"),
      # plotOutput("p0_t1E_Plot"),

      # h4("Effect of p1 on power"),
      # plotOutput("p1_power_Plot")
	  width=6
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
	aR0 = ifelse(is.na(input$saR0), input$spR0, input$saR0)
	bR0 = ifelse(is.na(input$sbR0), 1 - input$spR0, input$sbR0)
	aT0 = ifelse(is.na(input$saT0), input$spT0, input$saT0)
	bT0 = ifelse(is.na(input$sbT0), 1 - input$spT0, input$sbT0)
	# result = search_n3(min_n1 = input$min_n1, max_n = input$max_n, alpha = input$alpha, beta = 1-input$power, p0 = input$p0, p1 = input$p1)
	# result = search_n3(k = input$k, min_n1 = input$min_n1, d = input$d, max_n = input$max_n, alpha = input$alpha, beta = 1-input$power, p0 = input$p0, p1 = input$p1, a0 = input$a0, b0 = input$b0)
	# result = search_n3(k = input$sk, min_n1 = input$smin_n1, d = input$sd, max_n = input$smax_n, alpha = input$salpha, beta = 1-input$spower, p0 = input$sp0, p1 = input$sp1, a0 = a0, b0 = b0)
	result = search_n_RT(k = input$sk, 
		min_n1 = input$smin_n1, 
		d = input$sd, 
		max_n = input$smax_n, 
		alphaR = input$salpha_R, 
		alphaT = input$salpha_T, 
		beta = 1-input$spower, 
		pR0 = input$spR0, 
		pR1 = input$spR1, 
		pT0 = input$spT0, 
		pT1 = input$spT1, 
		aR0 = aR0, 
		bR0 = bR0, 
		aT0 = aT0, 
		bT0 = bT0)

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
		
		# minimax_n = eval(parse(text = paste0("c(", minimax$n, ")")))
		# minimax_r = eval(parse(text = paste0("c(", minimax$cxR_fail, ")")))

		# minimax_idx_nodup = which(!is.na(minimax_r) & !duplicated(minimax_r))
		# minimax_n_nodup = minimax_n[minimax_idx_nodup]
		# minimax_r_nodup = minimax_r[minimax_idx_nodup]
		
		# optimal_n = eval(parse(text = paste0("c(", optimal$n, ")")))
		# optimal_r = eval(parse(text = paste0("c(", optimal$cxR_fail, ")")))

		minimax_n = eval(parse(text = paste0("c(", minimax$n, ")")))
		minimax_cxR_fail = eval(parse(text = paste0("c(", minimax$cxR_fail, ")")))
		minimax_cxT_fail = eval(parse(text = paste0("c(", minimax$cxT_fail, ")")))
		optimal_n = eval(parse(text = paste0("c(", optimal$n, ")")))
		optimal_cxR_fail = eval(parse(text = paste0("c(", optimal$cxR_fail, ")")))
		optimal_cxT_fail = eval(parse(text = paste0("c(", optimal$cxT_fail, ")")))

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
		mar = par("mar")
		par(mfrow = c(1,2), mar = mar+0.5)
		plot(x = c(minimax_n, optimal_n), y = c(minimax_cxR_fail, optimal_cxR_fail),
			col = c(rep("blue", length(minimax_n)), rep("red", length(optimal_n))),
			pch = c(rep(1, length(minimax_n)), rep(2, length(optimal_n))),
			cex = 3, lwd = 3, las = 1, cex.lab = 1.5, cex.axis = 1.5,
			xlab = "n",
			ylab = "Observed Responses Cutoff",
			xlim = c(0,max(optimal_n)), ylim = c(0,max(minimax_cxR_fail, optimal_cxR_fail, na.rm=T)))
		legend("topleft", legend = c("Minimax", "Optimal"), 
			col = c("blue", "red"), pch = c(1,2), pt.cex = 3, pt.lwd = 3, cex=1.5, bty = "n")
		plot(x = c(minimax_n, optimal_n), y = c(minimax_cxT_fail, optimal_cxT_fail),
			col = c(rep("blue", length(minimax_n)), rep("red", length(optimal_n))),
			pch = c(rep(1, length(minimax_n)), rep(2, length(optimal_n))),
			cex = 3, lwd = 3, las = 1, cex.lab = 1.5, cex.axis = 1.5,
			xlab = "n",
			ylab = "Observed Nontoxicity Cutoff",
			xlim = c(0,max(optimal_n)), ylim = c(0,max(minimax_cxT_fail, optimal_cxT_fail, na.rm=T)))
		legend("topleft", legend = c("Minimax", "Optimal"), 
			col = c("blue", "red"), pch = c(1,2), pt.cex = 3, pt.lwd = 3, cex=1.5, bty = "n")
		par(mfrow=c(1,1), mar = mar)
    })
    output$p0_t1E_Plot <- renderPlot({
		# plot(1:10)
	    dataset <- datasetInput()
		minimax = dataset[dataset$design == "minimax",]
		optimal = dataset[dataset$design == "optimal",]
		
		minimax_n = eval(parse(text = paste0("c(", minimax$n, ")")))
		minimax_cxR_fail = eval(parse(text = paste0("c(", minimax$cxR_fail, ")")))
		minimax_cxT_fail = eval(parse(text = paste0("c(", minimax$cxT_fail, ")")))
		optimal_n = eval(parse(text = paste0("c(", optimal$n, ")")))
		optimal_cxR_fail = eval(parse(text = paste0("c(", optimal$cxR_fail, ")")))
		optimal_cxT_fail = eval(parse(text = paste0("c(", optimal$cxT_fail, ")")))

		minimax_cxR = list(x_fail = minimax_cxR_fail, x_pass = c(rep(NA, length(minimax_cxR_fail)-1), max(minimax_cxR_fail, na.rm=T)))
		minimax_cxT = list(x_fail = minimax_cxT_fail, x_pass = c(rep(NA, length(minimax_cxT_fail)-1), max(minimax_cxT_fail, na.rm=T)))
		optimal_cxR = list(x_fail = optimal_cxR_fail, x_pass = c(rep(NA, length(optimal_cxR_fail)-1), max(optimal_cxR_fail, na.rm=T)))
		optimal_cxT = list(x_fail = optimal_cxT_fail, x_pass = c(rep(NA, length(optimal_cxT_fail)-1), max(optimal_cxT_fail, na.rm=T)))
		pR0 = seq(input$spR0-0.05, input$spR0+0.05, by = 0.01)
		pT0 = seq(input$spT0-0.05, input$spT0+0.05, by = 0.01)
		minimax_T1E_R0T1 = vapply(X = pR0, FUN = p_success_RT_mat, FUN.VALUE = 1, n = minimax_n, pT = input$spT1, cxR = minimax_cxR, cxT = minimax_cxT)
		minimax_T1E_R1T0 = vapply(X = pT0, FUN = p_success_RT_mat, FUN.VALUE = 1, n = minimax_n, pR = input$spR1, cxR = minimax_cxR, cxT = minimax_cxT)
		
		# optimal_T1E = vapply(X = pR0, FUN = p_success_mat, FUN.VALUE = 1, n = optimal_n, cx = optimal_cx)
		optimal_T1E_R0T1 = vapply(X = pR0, FUN = p_success_RT_mat, FUN.VALUE = 1, n = optimal_n, pT = input$spT1, cxR = optimal_cxR, cxT = optimal_cxT)
		optimal_T1E_R1T0 = vapply(X = pT0, FUN = p_success_RT_mat, FUN.VALUE = 1, n = optimal_n, pR = input$spR1, cxR = optimal_cxR, cxT = optimal_cxT)

		# par(mar = par("mar")+c(0,10,0,0))
		mar = par("mar")
		par(mfrow = c(1,2), mar = mar+0.5)
		#response
		plot(x = c(pR0, pR0), y = c(minimax_T1E_R0T1, optimal_T1E_R0T1),
			col = c(rep("blue", length(pR0)), rep("red", length(pR0))),
			pch = c(rep(1, length(pR0)), rep(2, length(pR0))),
			cex = 3, lwd = 3, #las = 1, 
			cex.lab = 1.5, cex.axis = 1.5,
			xlab = "Unacceptable Response Rate pR0",
			# ylab = "Type 1 Error alpha01",
			ylab = expression(Type~1~Error~alpha["01"]),
			ylim = c(min(minimax_T1E_R0T1, optimal_T1E_R0T1, na.rm=T),max(minimax_T1E_R0T1, optimal_T1E_R0T1, na.rm=T)))
		abline(h = input$salpha_R, lty = 3, lwd = 2)
		legend("topleft", legend = c("Minimax", "Optimal"), 
			col = c("blue", "red"), pch = c(1,2), pt.cex = 3, pt.lwd = 3, cex=1.5, bty = "n")
		#nontox
		plot(x = c(pT0, pT0), y = c(minimax_T1E_R1T0, optimal_T1E_R1T0),
			col = c(rep("blue", length(pT0)), rep("red", length(pT0))),
			pch = c(rep(1, length(pT0)), rep(2, length(pT0))),
			cex = 3, lwd = 3, #las = 1, 
			cex.lab = 1.5, cex.axis = 1.5,
			xlab = "Unacceptable Nontoxicity Rate pT0",
			# ylab = "Type 1 Error alpha10", 
			ylab = expression(Type~1~Error~alpha["10"]),
			ylim = c(min(minimax_T1E_R1T0, optimal_T1E_R1T0, na.rm=T),max(minimax_T1E_R1T0, optimal_T1E_R1T0, na.rm=T)))
		abline(h = input$salpha_T, lty = 3, lwd = 2)
		legend("topleft", legend = c("Minimax", "Optimal"), 
			col = c("blue", "red"), pch = c(1,2), pt.cex = 3, pt.lwd = 3, cex=1.5, bty = "n")
		par(mfrow=c(1,1), mar = mar)
	})

    output$p1_power_Plot <- renderPlot({
		# plot(1:10)
	    dataset <- datasetInput()
		minimax = dataset[dataset$design == "minimax",]
		optimal = dataset[dataset$design == "optimal",]
		
		minimax_n = eval(parse(text = paste0("c(", minimax$n, ")")))
		minimax_cxR_fail = eval(parse(text = paste0("c(", minimax$cxR_fail, ")")))
		minimax_cxT_fail = eval(parse(text = paste0("c(", minimax$cxT_fail, ")")))
		optimal_n = eval(parse(text = paste0("c(", optimal$n, ")")))
		optimal_cxR_fail = eval(parse(text = paste0("c(", optimal$cxR_fail, ")")))
		optimal_cxT_fail = eval(parse(text = paste0("c(", optimal$cxT_fail, ")")))

		minimax_cxR = list(x_fail = minimax_cxR_fail, x_pass = c(rep(NA, length(minimax_cxR_fail)-1), max(minimax_cxR_fail, na.rm=T)))
		minimax_cxT = list(x_fail = minimax_cxT_fail, x_pass = c(rep(NA, length(minimax_cxT_fail)-1), max(minimax_cxT_fail, na.rm=T)))
		optimal_cxR = list(x_fail = optimal_cxR_fail, x_pass = c(rep(NA, length(optimal_cxR_fail)-1), max(optimal_cxR_fail, na.rm=T)))
		optimal_cxT = list(x_fail = optimal_cxT_fail, x_pass = c(rep(NA, length(optimal_cxT_fail)-1), max(optimal_cxT_fail, na.rm=T)))
		pR1 = seq(input$spR1-0.05, input$spR1+0.05, by = 0.01)
		pT1 = seq(input$spT1-0.05, input$spT1+0.05, by = 0.01)
		# minimax_pow = vapply(X = pR1, FUN = p_success_mat, FUN.VALUE = 1, n = minimax_n, cx = minimax_cx)
		minimax_pow_R = vapply(X = pR1, FUN = p_success_RT_mat, FUN.VALUE = 1, n = minimax_n, pT = input$spT1, cxR = minimax_cxR, cxT = minimax_cxT)
		minimax_pow_T = vapply(X = pT1, FUN = p_success_RT_mat, FUN.VALUE = 1, n = minimax_n, pR = input$spR1, cxR = minimax_cxR, cxT = minimax_cxT)
		# optimal_pow = vapply(X = pR1, FUN = p_success_mat, FUN.VALUE = 1, n = optimal_n, cx = optimal_cx)
		optimal_pow_R = vapply(X = pR1, FUN = p_success_RT_mat, FUN.VALUE = 1, n = optimal_n, pT = input$spT1, cxR = optimal_cxR, cxT = optimal_cxT)
		optimal_pow_T = vapply(X = pT1, FUN = p_success_RT_mat, FUN.VALUE = 1, n = optimal_n, pR = input$spR1, cxR = optimal_cxR, cxT = optimal_cxT)

		# par(mar = par("mar")+c(0,10,0,0))
		mar = par("mar")
		par(mfrow = c(1,2), mar = mar+0.5)
		plot(x = c(pR1, pR1), y = c(minimax_pow_R, optimal_pow_R),
			col = c(rep("blue", length(pR1)), rep("red", length(pR1))),
			pch = c(rep(1, length(pR1)), rep(2, length(pR1))),
			cex = 3, lwd = 3, #las = 1, 
			cex.lab = 1.5, cex.axis = 1.5,
			xlab = "Acceptable Response Rate pR1",
			ylab = "Power",
			ylim = c(min(minimax_pow_R, optimal_pow_R, na.rm=T),max(minimax_pow_R, optimal_pow_R, na.rm=T)))
		abline(h = input$spower, lty = 3, lwd = 2)
		legend("bottomright", legend = c("Minimax", "Optimal"), 
			col = c("blue", "red"), pch = c(1,2), pt.cex = 3, pt.lwd = 3, cex=1.5, bty = "n")
		#nontox
		plot(x = c(pT1, pT1), y = c(minimax_pow_T, optimal_pow_T),
			col = c(rep("blue", length(pT1)), rep("red", length(pT1))),
			pch = c(rep(1, length(pT1)), rep(2, length(pT1))),
			cex = 3, lwd = 3, #las = 1, 
			cex.lab = 1.5, cex.axis = 1.5,
			xlab = "Acceptable Nontoxicity Rate pT1",
			ylab = "Power",
			ylim = c(min(minimax_pow_T, optimal_pow_T, na.rm=T),max(minimax_pow_T, optimal_pow_T, na.rm=T)))
		abline(h = input$spower, lty = 3, lwd = 2)
		legend("bottomright", legend = c("Minimax", "Optimal"), 
			col = c("blue", "red"), pch = c(1,2), pt.cex = 3, pt.lwd = 3, cex=1.5, bty = "n")
		par(mfrow=c(1,1), mar = mar)
	})

  fixedInput <- eventReactive(input$fupdate, {
	#check if both cx and theta were specified
	stopifnot(((input$fcxR != "" && is.na(input$ftheta_RT) && is.na(input$ftheta_RL)) || 
			(input$fcxR == "" && !is.na(input$ftheta_RT) && !is.na(input$ftheta_RL))) &&
		((input$fcxT != "" && is.na(input$ftheta_TT) && is.na(input$ftheta_TL)) || 
			(input$fcxT == "" && !is.na(input$ftheta_TT) && !is.na(input$ftheta_TL))))

	n = as.integer(unlist(strsplit(input$fn, ",")))
	aR0 = ifelse(is.na(input$faR0), input$fpR0, input$faR0)
	aT0 = ifelse(is.na(input$faT0), input$fpT0, input$faT0)
	bR0 = ifelse(is.na(input$fbR0), 1 - input$fpR0, input$fbR0)
	bT0 = ifelse(is.na(input$fbT0), 1 - input$fpT0, input$fbT0)
	#calc cutoffs if not given
	
	#TODO: continue here
	cxR = NA
	if(input$fcxR == ""){
		cxR = calc_cutoffs(theta_L = input$ftheta_RL, theta_T = input$ftheta_RT, p0 = input$fpR0, p1 = input$fpR1, a0 = aR0, b0 = bR0, n = n)
	}
	else{
		cxR = list(x_fail = as.integer(unlist(strsplit(input$fcxR, ","))), 
			x_pass = c(rep(NA, length(n)-1), max(as.integer(unlist(strsplit(input$fcxR, ","))), na.rm=T)+1))
	}
	cxT = NA
	if(input$fcxT == ""){
		cxT = calc_cutoffs(theta_L = input$ftheta_TL, theta_T = input$ftheta_TT, p0 = input$fpT0, p1 = input$fpT1, a0 = aT0, b0 = bT0, n = n)
	}
	else{
		cxT = list(x_fail = as.integer(unlist(strsplit(input$fcxT, ","))), 
			x_pass = c(rep(NA, length(n)-1), max(as.integer(unlist(strsplit(input$fcxT, ","))), na.rm=T)+1))
	}
	# cx = ifelse(input$cx == "", 
		# calc_cutoffs(theta_L = input$theta_L, theta_T = input$theta_T, p0 = input$p0, p1 = input$p1, a0 = a0, b0 = b0, n = n),
		# list(x_fail = as.integer(unlist(strsplit(input$cx, ","))), x_pass = c(rep(NA, length(n)-1), max(as.integer(unlist(strsplit(input$cx, ","))), na.rm=T)+1)))
	
	# list(input$n, input$p0, input$p1, input$a0, input$b0, input$cx, input$theta_L, input$theta_T, cx)
	# list(cx$x_fail, cx$x_pass)
	# names(cx)
	# result = search_n3(k = input$k, min_n1 = input$min_n1, d = input$d, max_n = input$max_n, alpha = input$alpha, beta = 1-input$power, p0 = input$p0, p1 = input$p1, a0 = a0, b0 = b0)
	# result
	result = trial_perf_RT(n = n, pR0 = input$fpR0, pT0 = input$fpT0, pR1 = input$fpR1, pT1 = input$fpT1, cxR = cxR, cxT = cxT)
	data.frame(c(round(result,4), n = paste0(n, collapse=","), xR_fail = paste0(cxR$x_fail, collapse=","), xT_fail = paste0(cxT$x_fail, collapse=",")))
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
