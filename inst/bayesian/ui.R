library(shinythemes);

shinyUI(
    fluidPage(theme = shinytheme("cosmo"),
              ## includeScript('www/tools.js'),
              ##css
              tags$head(tags$title("Bayesian"),
                        tags$link(rel = "stylesheet", type = "text/css",
                                  href = "styles.css"),
                        tags$link(rel = "stylesheet", type = "text/css",
                                  href = "//fonts.googleapis.com/css?family=Oswald"),
                        tags$link(rel = "stylesheet", type = "text/css",
                                  href = "//fonts.googleapis.com/css?family=Lora")
                        ,tags$script(src="https://cdn.mathjax.org/mathjax/latest/MathJax.js?config=TeX-AMS-MML_HTMLorMML",
                                     type="text/javascript")
                        ),

              ##title box
              withTags({
                  div(class="cheader",
                      "An Introduction to Bayesian Clinical Trial Design",
                      tags$button(
                               id = 'close',
                               type = "button",
                               class = "btn action-button",
                               ## close browser
                               onclick = "setTimeout(function(){window.close();},500);",
                               "Exit",
                               style = "float: right;
                                       background-image: url(texturebg2.jpg);"
                           )
                      )
              }),

              ##wait for starting
              conditionalPanel(condition = "$('html').hasClass('shiny-busy')",
                               tags$div("", id = "loadmessage")),

              ##main page
              uiOutput("mainpage"),

              ##foot
              withTags({
                  div(class = "cfooter",
                      "A",
                      a("Statistical Innovation", href="http://www.regeneron.com/"),
                      "Project")
              })
              )
)
