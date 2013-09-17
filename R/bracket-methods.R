setMethod("[[", "rpath_matchList",
          function(x, i, j, ...)
      {
          x@matches[[i]]
      })

setMethod("[", "rpath_matchList",
          function(x, i, j, ...)
      {
          res = x@matches[i, ...]
          new("rpath_matchList", matches = res)
      })

setMethod("[[", "rpath_match",
          function(x, i, j, ...)
      {
          x@value[[i, ...]]
      })
setMethod("[", "rpath_match",
          function(x, i, j, ...)
      {
          x@value[i, ...]
      })


setMethod("[[", "terminal_node",
          function(x, i, j, ...)
      {
          no_match_found()
      })
setMethod("[", "terminal_node",
          function(x, i, j, ...)
      {
          no_match_found()
      })


setMethod("[[", "no_match",
          function(x, i, j, ...)
      {
          no_match_found()
      })
setMethod("[", "no_match",
          function(x, i, j, ...)
      {
          no_match_found()
      })

