

rpath_compare = function(lNode, rNode)
{
    #what should we be returning in this case? it should probably never be happening anyway ...
 #   if(no_match(lNode) && no_match(rNode))
  #      return(TRUE)
   # else if (no_match(lNode) || no_match(rNode))
    if (no_match(lNode) || no_match(rNode))
        return(FALSE)
    
    if(is(lNode, "rpath_match"))
        lNode = lNode@value
    else if (is(lNode, "rpath_matchList"))
        lNode = lNode@matches

    if(is(rNode, "rpath_match"))
        rNode = rNode@value
    else if (is(rNode, "rpath_matchList"))
        rNode = rNode@matches
 
    
    llst = is.vector(lNode)
    rlst = is.vector(rNode)

    ret = FALSE
    
    if(rlst && !llst)
    {
        llst = TRUE
        rrst = FALSE
        tmp = rNode
        rNode = lNode
        lNode = tmp
    }

    if(rlst && llst)
    {
        for(li in seq(length(lNode), 1, by=-1)){
            for(ri in seq(length(rNode), 1, by=-1)){
                if(lNode[[li]] == rNode[[ri]])
                    ret = TRUE
            }
        }
    } else if (llst && !rlst) {
        for(li in seq(length(lNode), 1, by=-1)) {
            if(lNode[[li]] == rNode)
                ret = TRUE
        }
    } else if (lNode == rNode) {
        ret = TRUE
    }

    ret
}

    




rpath = function(robj, path, state = new.env(), use_classes = FALSE, names_fun = if(use_classes) getClassesVec else names, term_condition = list_termination)
{
    state$lastSteps = list()
    state$result = list()
    state$names_fun = names_fun
    state$term_condition = term_condition
    if(is.null(robj) || !is(path, "character") || !nchar(path))
        return(list())

    if(identical(path, ".") || identical(path, "/"))
        return(robj)

    steps = rpath_split(path, state =  state)
         
    res = rpath_exec(robj, steps[[1]], executors = executors, state = state)
    #i = 3
    i = 2
    while(!no_match(res) && i <= length(steps))
    {
        res = rpath_exec(res, steps[[i]], executors = executors, state = state)
        i = i + 1
    }
    if(no_match(res))
        list()
    else if (is(res, "rpath_matchList"))
    {
        res = res[sapply(res@matches, function(x) !no_match(x))]
        res = lapply(res@matches, function(x) x@value)
    } else if (is(res, "rpath_match")) {
        res = res@value
    }
    
    res
}

