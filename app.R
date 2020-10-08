library(shiny)

ui <- fluidPage(
  
  titlePanel(strong("Normal, Normal-Ogive and Exponential Distributions")),
  hr(),

  fluidRow(

    column(6,
      plotOutput("plot1", width = 550, height = 550)
    ),

    column(6,
      plotOutput("plot2", width = 550, height = 550)
    )
  ),

  fluidRow(

    style = "background-color: lightblue;",
    p(h4(strong("Enter the parameters:"))),
    br(),
    
    column(2,
      numericInput("mean", "Mean", value = 0)
    ),

    column(2,
      numericInput("sd", "Standard Deviation", value = 1)
    ),

    column(2,
      numericInput("theta", "Theta", value = 3)
    ),

    column(2,
      numericInput("D", "D-constant", value = 1)
    ),

    column(4,
      br(),
      actionButton("draw", "Draw", class = "btn-primary")
    )
  )
)

server <- function(input, output){
  
  output$plot1 <- renderPlot({

    input$draw

    isolate({
 
      mean <- input$mean
      sd <- input$sd
      theta <- input$theta
 
      if(is.null(theta)) theta = mean + 3*sd 
      if(theta < mean - 3*sd) theta = mean - 3*sd
      if(theta > mean + 3*sd) theta = mean + 3*sd
      x.alt <- mean - 3* sd
      x.ust <- mean + 3* sd

      curve(dnorm(x, mean, sd)*sd, x.alt, x.ust, ylim = c(0, 1), 
        ylab = "Probability-Density",
        main = paste("Normal and Normal-Ogive Distributions\n", "(theta =", 
          theta, ", mean =", mean, ", sd =", sd, ")"),
        lwd = 3, font = 2, yaxt = "n")
      axis(2, c(0, 1), las = 2, font = 2)

      xv <- seq(x.alt, theta, .01)
      yv <- dnorm(xv, mean, sd)*sd
      polygon(c(x.alt, xv, theta), c(0, yv, 0), col = "lightgreen")
 
      x0 <- x.alt
      y0 <- max(yv)
      x1 <- (x.alt + theta ) / 2
      y1 <- max(yv) / 4
      text(x0, y0, round(pnorm(theta, mean, sd), 2), pos = 3, font = 2, 
        col = "blue")
      arrows(x0, y0, x1, y1, lty = 2, length = .15, col = "blue") 

      curve(pnorm(x, mean, sd), add = TRUE, lwd = 3, col = "blue")
      lines(c(theta, theta), c(0, pnorm(theta, mean, sd)), lty = 2)
      lines(c(x.alt, theta), c(pnorm(theta, mean, sd), pnorm(theta, mean, sd)), lty = 2)
      axis(2, round(pnorm(theta, mean, sd), 2), las = 1, font = 2, col.axis = "blue")
    })
  })

  output$plot2 <- renderPlot({

    input$draw

    isolate({

      mean <- input$mean
      sd <- input$sd
      theta <- input$theta
      D <- input$D

      if(is.null(theta)) theta = mean + 3*sd 
      if(theta < mean - 3*sd) theta = mean - 3*sd
      if(theta > mean + 3*sd) theta = mean + 3*sd  
      x.alt <- mean - 3*sd
      x.ust <- mean + 3*sd    

      t <- seq(x.alt, x.ust, .01)
      a = 1/sd
      b = mean
      c = 0
      d = 1
      p.t <- c + (d - c) * 1 / (1 + exp(-D * a * (t - b)))

      plot(t, p.t, type = "l", las = 1, ylim = c(0, 1), lwd = 3,
        main = paste("Normal-Ogive and Exponential Functions\n", "(D = ", D, ")"),
        ylab = "Probability",
        font = 2, yaxt = "n")
      axis(2, c(0, 1), font = 2, las = 1)
 
      y1 <- round(pnorm(theta, mean, sd), 2)
      y2 <- round(c + (d - c) * 1 / (1 + exp(-D * a * (theta - b))), 2) 

      curve(pnorm(x, mean, sd), add = T, col = "blue", lwd = 3)
      axis(2, c(y1, y2), font = 2, las = 1, col.axis = "blue") 

      lines(c(theta, theta), c(0, y1), lty = 2)
      lines(c(theta, theta), c(0, y2), lty = 2)
      lines(c(x.alt, theta), c(y1, y1), lty = 2)
      lines(c(x.alt, theta), c(y2, y2), lty = 2)

      legend("right", c("Normal-Ogive", "Exponential"), pch = 15:16, 
        col = c("blue", "black"), lty = 1 )
    })
  })
  
}

shinyApp(ui, server)
 
