library(shiny)

shinyUI(fluidPage(
  theme = "bootstrap.css",
  headerPanel("Coursera Data Science Capstone: Predicting the next word"),
  fluidRow(column(4, offset = 2,
                  h4("Please insert your sentence below:"),
      div(textInput("entry","",
                "Coursera is the"),class="text-primary"),
      h6("(Enter does not work, please click SUBMIT)"),
#      br(),
      submitButton("SUBMIT")),
    column(4,
               h5('The sentence you were typing:'),                             
               h4(strong(textOutput('sent'),class="text-primary")),
               h5('Word Predictions:'),
               h4(strong(textOutput('text'),class= "text-info"))),
    column(10,
              hr(),
              h3("Instructions:"),
              "The app might take a while on the first load. To predict the next words, please type a
               sentence into the input field and then click on SUBMIT.
              The predicted words are then shown on the right, where the most likely word is in first position."
    ),
    column(10,
              hr(),
              h3('How it works:'),
              "First, your input is checked against a vocabulary. It contains over 40.000 words extracted from Twitter and blogs.
              If your word is not included in the vocabulary, it is replaced by a placeholder.
              This placeholder is also generated for every word in the basis for the words in this prediction app that are not in the vocabulary.
              The vocabulary itself contains words that can describe 95% of all tokens. Want to know more? Contact me",
              a("ulizellbeck@gmail.com",href="mailto:ulizellbeck@gmail.com"),
              br(),
              br(),
              "CSS-Style is made by Thomas Park for", 
              a("Bootswatch",href="http://bootswatch.com/superhero/"), 
              "Contact him at ",
              a("thomas@bootswatch.com",href="thomas@bootswatch.com"),
              ".Code released under the MIT License.
               Based on Bootstrap.",
           "Icons from Font Awesome. Web fonts from Google."
          )
#                span(h4(textOutput('top1')),style = "color:purple"),
#                br(),
#                #h6('Word Prediction #2:'),
#                span(h4(textOutput('top2')),style = "color:green"),
#                br(),
#                #h6('Word Prediction #3:'),
#                span(h4(textOutput('top3')),style = "color:green"),
#                br(),
#        tabPanel('Predictions',
#                         h5('lallala')) 
                  )
    ))