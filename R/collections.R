Stack <- R6::R6Class("Stack",
  public = list(
    push = function(x) {
      private$stack <- c(private$stack, list(x))
      invisible(self)
    },

    pop = function() {
      size <- self$size()
      if (size == 0) return(NULL)
      x <- private$stack[[size]]
      private$stack[[size]] <- NULL
      x
    },

    peek = function() {
      size <- self$size()
      if (size == 0) return(NULL)
      private$stack[[self$size()]]
    },

    size = function() {
      length(private$stack)
    },

    asList = function() {
      private$stack
    }
  ),

  private = list(
    stack = list()
  )
)

Queue <- R6::R6Class("Queue",
  public = list(
    enqueue = function(x) {
      private$queue <- c(private$queue, list(x))
      invisible(self)
    },

    dequeue = function() {
      size <- self$size()
      if (size == 0) return(NULL)
      x <- private$queue[[1]]
      private$queue[[1]] <- NULL
      x
    },

    size = function() {
      length(private$queue)
    },

    asList = function() {
      private$queue
    }
  ),

  private = list(
    queue = list()
  )
)
