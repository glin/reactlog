library(shiny)
options(shiny.reactlog = TRUE)

valA <- reactiveVal(1, label = "valA")
rxA <- reactive({
  valA()
})

obs <- observe({
  # call it in a reactive context
  reactlog::traceInvalidation()
  rxA()
})

observe(valA(3))

# trigger flush event (happens automatically in a Shiny app)
shiny:::flushReact()

# or call it on a reactive expression or observer
reactlog::traceInvalidation(rxA)
