options(bitmapType='cairo')

#install.packages("shiny")
#install.packages("ggplot2")
#install.packages("package")

library(shiny)
library(ggplot2)
library(robustbase)

require(foreign)
require(MASS)

x <- rep(0:400)

ratio_1 = 1.361
ratio_2 = 1.688
ratio_3 = 1.868

alldata = as.data.frame(as.matrix(x, ncol = 1))
names(alldata) = c("X")
alldata$y1 <- alldata$X*ratio_1
alldata$y2 <- alldata$X*ratio_2
alldata$y3 <- alldata$X*ratio_3

alldata$y1_reverse <- alldata$X/ratio_1
alldata$y2_reverse <- alldata$X/ratio_2
alldata$y3_reverse <- alldata$X/ratio_3

find_axis_limit <- function(x){
  (x %/% 10 + 1) * 10
}
find_axis_split <- function(x){
  (x %/% 200 + 1) * 10
}

generate_figure <- function(value_1st, value_2nd, ci_level_input){
  #value_1st = 15
  #value_2nd = 40
  if(ci_level_input == "Likely"){
    ratio_std = ratio_1
    alldata$y_upper = alldata$y1
    alldata$y_lower = alldata$y1_reverse
    pct = "75%"
  }else if(ci_level_input == "Possible"){
    ratio_std = ratio_2
    alldata$y_upper = alldata$y2
    alldata$y_lower = alldata$y2_reverse
    pct = "94.5%"
  }else if(ci_level_input == "Less Likely"){
    ratio_std = ratio_3
    alldata$y_upper = alldata$y3
    alldata$y_lower = alldata$y3_reverse
    pct = "97.5%"
  }
  
  x_limit = max(30, find_axis_limit(value_1st))
  y_limit = max(60, find_axis_limit(value_2nd), find_axis_limit(x_limit*ratio_std))
  ggplot(alldata, aes(X, y1)) +
    #theme_bw(value_2nd < value_1st*ratio_2 | value_2nd > value_1st*ratio_2) + 
    theme_bw() + 
    theme(legend.position="bottom", legend.text = element_text(size=15), 
          axis.title.x = element_text(size=18, face="bold"), 
          axis.title.y = element_text(size=18, face="bold"),
          axis.text.x = element_text(size=15), 
          axis.text.y = element_text(size=15)) + 
    xlab("Baseline Tryptase (ng/mL)") + ylab("Acute tryptase measurement (ng/mL)") + 
    geom_point(aes(x=value_1st, y=value_2nd), colour="dark green", size=2.5) +
    scale_x_continuous(limits = c(NA, x_limit),breaks=seq(0, x_limit, find_axis_split(x_limit))) +
    scale_y_continuous(limits = c(NA, y_limit),breaks=seq(0, y_limit, find_axis_split(y_limit))) +
    geom_line(data = alldata, aes(y=y_upper, x=X, color = "blue"), color = "blue", linetype = "dashed", size=1)+
    geom_line(data = alldata, aes(y=y_lower, x=X, color = "blue"), color = "blue", linetype = "dashed", size=1)+
    #scale_color_discrete(name = "", labels = c("Confidence Interval"))+
    labs(colour="") 
}

generate_statement <- function(value_1st, value_2nd, ci_level_input){
  if(ci_level_input == "Likely"){
    ratio_std = ratio_1
    alldata$y_upper = alldata$y1
    alldata$y_lower = alldata$y1_reverse
  }else if(ci_level_input == "Possible"){
    ratio_std = ratio_2
    alldata$y_upper = alldata$y2
    alldata$y_lower = alldata$y2_reverse
  }else if(ci_level_input == "Less Likely"){
    ratio_std = ratio_3
    alldata$y_upper = alldata$y3
    alldata$y_lower = alldata$y3_reverse
  }
  
  if(value_2nd < value_1st * ratio_std & value_2nd > value_1st / ratio_std){
    #paste("It is", ci_level_input, "to be in anaphylaxis")
    #paste('<span style=\"color:', 'green', 
    #      '\"><b>It is not ', ci_level_input, " to be in anaphylaxis", 
    #      ' </b></span>', sep = "")
    paste('<span style=\"color:', 'green', 
          '\"><b>The change in total serum tryptase is consistent with the clinical diagnosis of anaphylaxis.
           </b></span>', sep = "")
    #The change in total serum tryptase is consistent with the clinical diagnosis of anaphylaxis.
  }else{
    #paste("It is not", ci_level_input, "to be in anaphylaxis")
    paste('<span style=\"color:', 'red', 
          '\"><b>The change in total serum tryptase is NOT consistent with the clinical diagnosis of anaphylaxis.
          </b></span>', sep = "")
  }
}
library(DescTools)

#########################################################
#########################################################

#fields <- c("a", "b", "bst", "ci_level")

# Define UI for dataset viewer app ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("Total Rise In Peripheral Tryptase After Systemic Event (TRIPTASE) Calculator"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      #numericInput("value_1st", "1st BST Measurement (ng/mL):", 0, min = 0, max = 400),
      #numericInput("value_2nd", "2nd BST Measurement (ng/mL):", 0, min = 0, max = 400),
      textInput("value_1st", "Baseline Tryptase (ng/mL):", "10"),
      textInput("value_2nd", "Acute tryptase measurement* (ng/mL):", "10"),
      #textInput("bst", "BST (ng/mL) (Optional):"),
      selectInput("ci_level", "Clinical Suspicion", c("Likely", "Possible", "Less Likely")),
      helpText("*Total serum tryptase measured within 4 hours of symptom onset during an episode suggestive of a systemic immediate hypersensitivity reaction."),
      helpText("Disclaimer: A failure to detect a significant increase in serum tryptase during an acute event does not rule out the diagnosis of anaphylaxis."),
      actionButton("submit", "Submit")
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      #verbatimTextOutput("Text1"),
      #textOutput("Text1"),
      htmlOutput("Text1"),
      br(),
      plotOutput('plot1', width = "750px", height = "550px")
    )
  )
)

# Define server logic to summarize and view selected dataset ----
server <- function(input, output) {
  output$Text1 = renderText({
    if(is.null(input$value_1st) | is.null(input$value_2nd) | is.na(input$value_1st) | is.na(input$value_2nd) | input$value_1st == ""){
      paste("Warning: Please input.")
    }else if(is.na(as.numeric(input$value_1st)) | is.na(as.numeric(input$value_2nd))){
      paste("Warning: Please input valid numbers")
    }else{
      if(as.numeric(input$value_1st) > 400){
        paste("Warning: The baseline tryptase is too large. Please check your inputs")
      }else{
        generate_statement(as.numeric(input$value_1st), as.numeric(input$value_2nd), input$ci_level)
      }
    }
  })
  
  output$plot1 <- renderPlot({
    if(input$value_1st == "" | input$value_2nd == ""){
      paste(' ', ' ', sep = "")
    }else if(is.na(as.numeric(input$value_1st)) | is.na(as.numeric(input$value_2nd))){
      paste(' ', ' ', sep = "")
    }else if(input$value_1st > 400){
      paste(' ', ' ', sep = "")
    }else{
      generate_figure(as.numeric(input$value_1st), as.numeric(input$value_2nd), input$ci_level)
    }
  })
}

# Create Shiny app ----
shinyApp(ui, server)