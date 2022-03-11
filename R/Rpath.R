

rpath_compare = function(lNode, rNode, op = "==")
{
    if(is.character(op))
        op = get(op, mode = "function")
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
                tmp = c(tmp, do_compare(lNode[[li]]@value, rNode[[ri]]@value, const_type, op))

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
            ret = c(ret, do_compare(lNode[[li]]@value, rNode, const_type, op))
        }
    } else {
        ret = do_compare(lNode, rNode, const_type, op)
    }
    ret
}

do_compare = function(l, r, force_type = NULL, op)
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

    op(l, r) # l == r
}


##' Rpath 'language spec'
##'
##' "namespace" indicates how to resolve names, e.g. cl:numeric means elements with (first) class "numeric" match, nm:hi means elements with name (as returned by names() ) "hi" match.
##' default, namespaceless node 'name' resolution is controlled by names_fun
##' Attributes (@) are controlled by the "attributespace", indicated by '~', i.e. nodename@ar~dim would resolve via attrib, while nodename@sl~data would resolve as an S4 slot.
##' default, attributespaceless attributes resolution is controlled by attr_fun
##' predicates are indicated via [] as in XPath, they can be indexes (including support for vectors of indexs via nodename[x:y] ), rpath expressions, or logical operations
##' equality in predicates uses "==", as in R
##' Axes other than / and // (eg parent, ancestor) are not supported, as most R objects are unable to point to their parent object
##' Wildcard (matches everything) is indicated by '*', as in /numeric/* will return all elements of the vector
##' The 'and' and 'or' operators are indicated by & and | respectively, as in their vectorized forms in R
##' Terminal nodes are recognized via the term_condition argument. This is necessary because in R, you can infinitely subset a vector of length one via v[1][1][1] etc.
##' @param robj Object to match against
##' @param path A character value containing the rpath expression
##' @param state an environment, used internally to track state
##' @param default_ns Namespace to use when no-namespace is specified. Defaults to \code{nm} for names.
##' @param term_condition function. Function returning true if maximum depth has been reached (ie no more recursion should be done).
##' @param default_as character. Attribute space to use when none is specified. Defaults to \code{a} indicating R attributes.
##' @param as_funcs list. Named list of functions to resolve path attributes of different types (ie different attribute spaces).
##' @export

rpath = function(robj, path, state = new.env(), default_ns="nm", ns_funcs = nsFuncs, term_condition = list_termination, default_as=names(as_funcs)[1], as_funcs = asFuncs)
{
    state$lastSteps = list()
    state$result = list()
#    state$names_fun = names_fun
    state$nsFuncs = ns_funcs
    state$defaultNSFunc = ns_funcs[[default_ns]]
    if(is(as_funcs, "function"))
        as_funcs = list(a = as_funcs)
    state$asFuncs = as_funcs
    state$defaultASFunc = as_funcs[[default_as]]

    state$term_condition = term_condition
#    state$attr_fun = attr_fun
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
        res = list()
    else if (is(res, "rpath_matchList"))
    {
        res = trim_matchList(res)
        res = lapply(res@matches, function(x) x@value)
    } else if (is(res, "rpath_match")) {
        res = list(res@value)
    }

    res
}

