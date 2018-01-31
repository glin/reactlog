# reactlog

[![Build Status](https://travis-ci.org/glin/reactlog.svg?branch=master)](https://travis-ci.org/glin/reactlog)

reactlog extends Shiny with new features that make it easier to debug and trace reactivity, especially in larger apps.

#### Analyze the reactive log from R

- Print the call stack that caused invalidation ([rstudio/shiny#1846](https://github.com/rstudio/shiny/issues/1846))
- List reactive dependencies

**[Demo](https://shiny.glin.io/reactlog-demo)**

#### Reactive log visualizer enhancements

- Interactive filtering on selected nodes ([rstudio/shiny#1532](https://github.com/rstudio/shiny/issues/1532))
- Jump to the previous/next flush cycle (i.e. all white nodes) ([rstudio/shiny#1532](https://github.com/rstudio/shiny/issues/1532))

**[Demo](https://glin.github.io/reactlog/articles/demos/react-graph.html)**


## Installation

```r
# install.packages("devtools")
devtools::install_github("glin/reactlog")
```

## Usage

#### [`traceInvalidation()`](https://glin.github.io/reactlog/reference/traceInvalidation.html)

```r
options(shiny.reactlog = TRUE)

rxA <- reactive({
  input$A
})

observe({
  # call it in a reactive context
  reactlog::traceInvalidation()
  rxA()
})

# or on a reactive expression/observer
reactlog::traceInvalidation(rxA)
```

#### [`listDependencies()`](https://glin.github.io/reactlog/reference/listDependencies.html)

```r
reactive({
  # when multiple dependencies change, it might be useful to see them all
  # not just the one that invalidated the context
  reactlog::listDependencies()
  rxA() + input$A + input$B
})

# also works
reactlog::listDependencies(rxA, invalidated = TRUE)

# when called without a reactive context, shows the entire dependency tree
reactlog::listDependencies()
```

#### [`showReactGraph()`](https://glin.github.io/reactlog/reference/showReactGraph.html)

```r
# show the graph for the most recent session
reactlog::showReactGraph()

observe({
  # show the graph filtered on this observer and its dependencies
  reactlog::showReactGraph()
})
```


## API

https://glin.github.io/reactlog/reference/index.html


## Caveats

reactlog is experimental and relies on Shiny internals which are subject to change. Use at your own risk, for debugging and development only!
