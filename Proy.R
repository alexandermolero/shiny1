library(shinydashboard)

ui <- dashboardPage(
  dashboardHeader(title = "Distribution painter"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Univariate", tabName = "univariate", icon = icon("bar-chart-o")),
      menuItem("Bivariate", tabName = "bivariate", icon = icon("dot-circle-o")),
      menuItem("Game", tabName = "game", icon = icon("play-circle-o"))
    )
  ),
  dashboardBody(
    tabItems(
      # First tab content
      tabItem(tabName = "univariate",
              box(
                title = "Histogram", status = "primary", solidHeader = TRUE,
                collapsible = TRUE,
                plotOutput("plot0", height = 350,click = "plot_click0",hover = hoverOpts(id = "plot_hover0"))
              ),
              
              fluidRow(column(width = 3,verbatimTextOutput("mean0")),
                       column(width = 3,verbatimTextOutput("median")),
                       column(width = 3,verbatimTextOutput("sd0")),
                       column(width = 3,verbatimTextOutput("info02"))
              ),
              fluidRow(
                verbatimTextOutput("info01")
              )
      ),
      
      # Second tab content
      tabItem(tabName = "bivariate",
              box(
                title = "Scatter plot", status = "primary", solidHeader = TRUE,
                collapsible = TRUE,
                plotOutput("plot1", height = 350,click = "plot_click1",hover = hoverOpts(id = "plot_hover1"))
              ),
              
              fluidRow(
                column(width = 3,verbatimTextOutput("mean1")),
                column(width = 3,verbatimTextOutput("sd1")),
                column(width = 3,verbatimTextOutput("correlation")),
                column(width = 3,verbatimTextOutput("info12"))
              ),
              fluidRow(
                verbatimTextOutput("info11")
              )
              ),
    
    # Third tab content
    tabItem(tabName = "game",
            h4("Try to generate a sample with:"),
            
            fluidRow(column(width = 3,h4("Mean:"),h4(floor(runif(1,min=2,max=8)))),
                     column(width = 3,h4("sd:"),h4(floor(runif(1,min=1,max=3))))
            ),
            box(
              title = "Histogram", status = "primary", solidHeader = TRUE,
              collapsible = TRUE,
              plotOutput("plot2", height = 350,click = "plot_click2",hover = hoverOpts(id = "plot_hover2"))
            ),
            
            fluidRow(column(width = 4,verbatimTextOutput("info22")),
                     column(width = 4,verbatimTextOutput("title")),
                     actionButton("goButton", "Finish"),
                     column(width = 4,verbatimTextOutput("mean2")),
                     column(width = 4,verbatimTextOutput("sd2"))),
            fluidRow(verbatimTextOutput("info21")))
    )
  )
    )

server <- function(input, output) {
  val0 <- reactiveValues(clickx = NULL, clicky = NULL)
  observe({
    input$plot_click0
    isolate({
      val0$clickx = as.numeric(c(val0$clickx, input$plot_click0$x))
      val0$clicky = as.numeric(c(val0$clicky, input$plot_click0$y))
    })
  }) 
    
    output$plot0 <- renderPlot({
    plot(NULL, type="n", xlab="n", ylab="f", xlim=c(0, 11), ylim=c(0, 10))
    val0$clickx<-floor(val0$clickx+0)
    val0$clicky<-floor(val0$clicky+0)
    barplot(tabulate(rep(val0$clickx,val0$clicky),nbins=11),xlim=c(0,11),ylim=c(0,10),xlab="n", ylab="f",names.arg = as.character(c(1,2,3,4,5,6,7,8,9,10,11)))
    })
  
  output$mean0 <- renderPrint({
    cat("mean:\n")
    str(mean(rep(val0$clickx,val0$clicky)+0))
  })
  
  output$median <- renderPrint({
    cat("median:\n")
    str(median(rep(val0$clickx,val0$clicky)+0))
  })
  
  output$sd0 <- renderPrint({
    cat("sd:\n")
    str(sd(rep(val0$clickx,val0$clicky)+0))
  })
  
  output$info02 <- renderPrint({
    cat("hover \n")
    cat(paste('n = ',floor(input$plot_hover0$x+0), ', f = ',floor(input$plot_hover0$y+0)))
  })
  

  output$info01 <- renderPrint({
    cat("historial \n")
    cat(paste('n = ',val0$clickx, ',  f = ',val0$clicky,';  '))
  })
  
  val1 <- reactiveValues(clickx = NULL, clicky = NULL)
  observe({
    input$plot_click1
    isolate({
      val1$clickx = round(c(val1$clickx, input$plot_click1$x)+0,2)
      val1$clicky = round(c(val1$clicky, input$plot_click1$y)+0,2)     
    })
  }) 
  output$plot1 <- renderPlot({
    plot(NULL, type="n", xlab="x", ylab="y", xlim=c(0, 10), ylim=c(0, 10))
    points(val1$clickx, val1$clicky,xlab="x", ylab="y")
  })
  
  output$mean1 <- renderPrint({
    cat("mean x:\n")
    str(mean(val1$clickx))
    cat("mean y:\n")
    str(mean(val1$clicky))
  })
  
  output$sd1 <- renderPrint({
    cat("sd x:\n")
    str(sd(val1$clickx))
    cat("sd y:\n")
    str(sd(val1$clicky))
  })
  
  output$correlation <- renderPrint({
    cat("correlation:\n")
    str(cor(val1$clickx,val1$clicky))
  })

  
  output$info12 <- renderPrint({
    cat("hover \n")
    cat(paste('x = ',floor(input$plot_hover1$x+0), ', y = ',floor(input$plot_hover1$y+0)))
  })
  
  
  output$info11 <- renderPrint({
    cat("historial \n")
    cat(paste('x = ',val1$clickx, ',  y = ',val1$clicky,';  '))
  })
  
  val2 <- reactiveValues(clickx = NULL, clicky = NULL)
  observe({
    input$plot_click2
    isolate({
      val2$clickx = c(val2$clickx, input$plot_click2$x)
      val2$clicky = c(val2$clicky, input$plot_click2$y)     
    })
  }) 

  output$plot2 <- renderPlot({
    plot(NULL, type="n", xlab="n", ylab="f", xlim=c(0, 11), ylim=c(0, 10))
    val2$clickx<-floor(val2$clickx+0)
    val2$clicky<-floor(val2$clicky+0)
    barplot(tabulate(rep(val2$clickx,val2$clicky),nbins=11),xlim=c(0,11),ylim=c(0,10),xlab="n", ylab="f",names.arg = as.character(c(1,2,3,4,5,6,7,8,9,10,11)))
  })
  
  
  output$info22 <- renderPrint({
    cat("hover \n")
    cat(paste('n = ',floor(input$plot_hover2$x+0), ', f = ',floor(input$plot_hover2$y+0)))
  })
  
  
  output$info21 <- renderPrint({
    cat("historial \n")
    cat(paste('n = ',val2$clickx, ',  f = ',val2$clicky,';  '))
  })
  
  observeEvent(input$goButton,{
    
    output$title <- renderPrint({
      cat("Your results are:  ")
    })
    
    output$mean2 <- renderPrint({
      cat("mean:")
      str(mean(rep(val2$clickx,val2$clicky)+0))
    })
    
    output$sd2 <- renderPrint({
      cat("sd:")
      str(sd(rep(val2$clickx,val2$clicky)+0))
    })
    })
}

shinyApp(ui, server)