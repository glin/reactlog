library(shiny)
options(shiny.reactlog = TRUE)

valA <- reactiveVal(1, label = "valA")
valB <- reactiveVal(1, label = "valB")
rxA <- reactive({
  valA()
})

obs <- observe({
  # when multiple dependencies change, it might be useful to see them all
  # not just the one that invalidated the context
  reactlog::listDependencies()
  rxA() + valA() + valB()
}, label = "obs")

observe(valA(2))

shiny:::flushReact()

# also works
reactlog::listDependencies(rxA, invalidated = TRUE)

# when called without a reactive context, shows the entire dependency tree
reactlog::listDependencies()
