

rpath_compare = function(lNode, rNode)
{
    #what should we be returning in this case? it should probably never be happening anyway ...
 #   if(no_match(lNode) && no_match(rNode))
  #      return(TRUE)
   # else if (no_match(lNode) || no_match(rNode))
    if (no_match(lNode) || no_match(rNode))
        return(FALSE)


    ret = logical()
    const_type = NULL
    llst = FALSE
    rlst = FALSE
    
    if(is(lNode, "rpath_constant") )
    {
        const_type = lNode@type
        lNode = lNode@value
    } else if(is(lNode, "rpath_match"))
        lNode = lNode@value
    else if (is(lNode, "rpath_matchList"))
    {
        lNode = lNode@matches
        llst = TRUE
    }
    
    if(is(rNode, "rpath_constant") )
    {
        const_type = rNode@type
        rNode = rNode@value
    } else if(is(rNode, "rpath_match"))
        rNode = rNode@value
    else if (is(rNode, "rpath_matchList")) {
        rNode = rNode@matches
        rlst = TRUE
    }
    
      
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
            tmp = logical()
            for(ri in seq(length(rNode), 1, by=-1)){
             #   if(lNode[[li]] == rNode[[ri]])
         #       if(do_compare(lNode[[li]], rNode[[ri]], const_type))
          #          ret = TRUE
                tmp = c(tmp, do_compare(lNode[[li]]@value, rNode[[ri]]@value, const_type))
           
                #ret = c(ret, do_compare(lNode[[li]]@value, rNode[[ri]]@value, const_type))
            }
            #XXXdo we want any or all here???
            ret = c(ret, any(tmp))
        }
    } else if (llst && !rlst) {
        for(li in seq(length(lNode), 1, by=-1)) {
                                        # if(lNode[[li]] == rNode)
 #           if(do_compare(lNode[[li]], rNode, const_type))
  #              ret = TRUE
            ret = c(ret, do_compare(lNode[[li]]@value, rNode, const_type))
        }
    } else {
        ret = do_compare(lNode, rNode, const_type)
    }
    ret
}

do_compare = function(l, r, force_type = NULL)
{
    if(!is.null(force_type))
    {
        res = tryCatch({
            l = as(l, force_type)
            r = as(r, force_type)
        }, error = function(e) e)
        if(is(res, "error"))
            return(FALSE)
    }

    l == r
}



rpath = function(robj, path, state = new.env(), use_classes = FALSE, names_fun = if(use_classes) getClassesVec else names, term_condition = list_termination, attr_fun = attributes)
{
    state$lastSteps = list()
    state$result = list()
    state$names_fun = names_fun
    state$term_condition = term_condition
    state$attr_fun = attr_fun
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
        res = trimMatchList(res)
        res = lapply(res@matches, function(x) x@value)
    } else if (is(res, "rpath_match")) {
        res = list(res@value)
    }
    
    res
}

