library(shiny)
options(shiny.reactlog = TRUE)

valA <- reactiveVal(1, label = "valA")
valB <- reactiveVal(2, label = "valB")
valC <- reactiveVal(3, label = "valC")

rxA <- reactive({
  valA()
})

rxB <- reactive({
  valB()
})

rxC <- reactive({
  valC()
})

obsA <- observe({
  rxA()
})

obsB <- observe({
  rxB()
})

obsC <- observe({
  rxC()
})

shiny:::flushReact()

valA(10)
shiny:::flushReact()

valC(30)
shiny:::flushReact()

valA(100)
shiny:::flushReact()

valB(20)
shiny:::flushReact()

valC(300)
shiny:::flushReact()

reactlog::showReactGraph()
