

allnodes_exec = function(robj, node, exist, executors, state)
{
    ret = list()
    path = "*"
    tmp = rpath_exec(robj, list("node", "*"), exist = FALSE, executors = executors, state = state)
    ret = list(tmp)
  
    cnt  =1 
    while(!no_match(tmp) && cnt < 1000)
    {
        tmp = tmp[!sapply(tmp, checkTermCondition, term_fun = state$term_condition)]
        tmp = lapply(tmp, rpath_exec, step = list("node", "*"), exist = FALSE, executors = executors, state = state)
        tmp = combineMatchLists(lst = tmp)
#        if(length(tmp) == 1)
 #           tmp = tmp[[1]]
       
        
      
        if(!length(tmp))
            tmp = no_match_found()
        else
            ret = c(ret, tmp)
        
        cnt = cnt  + 1
    }
    combineMatchLists(lst = ret)
}
 

node_exec = function(robj, node, exist, executors, state)
{
    node = unlist(node, recursive = FALSE)
    if(is.character(node) && identical(node, ""))
        if(!exist)
            return(robj)
        else
            return(TRUE)

    len = length(robj)
    if(is(robj, "rpath_matchList"))
    {
        res = lapply(robj, rpath_exec, node = node, exist = exist, executors = executors, state = state)
        ret = combineMatchLists(lst= res, trim = TRUE)
    } else if (is.list(node) && length(node) > 1) {
                                        #XXX when does this code ever get invoked???
        print("I'm in the weird place")
        browser()
        res = robj
        for(i in seq(1, length(node), by=2)) {
            res = rpath_exec(res, node[i + 0:1], exist, executors = executors, state = state)
        }
        ret = res
    }  else  {
        tcond = state$term_condition
        if(identical(node, "*"))
        {
            found = seq(along = state$names_fun(robj))
            if(exist)
                ret = TRUE
            else
            {
                arr = new("rpath_matchList")
                for(i in seq(along=state$names_fun(robj)))
                    arr@matches = c(arr@matches, rpath_match(robj[[i]], tcond))
                ret = arr
#                ret = lapply(robj, function(x) x)
#                if(length(ret) != length(robj))
#                    unlist(ret, recursive = FALSE)
            }
        } else if (node %in% state$names_fun(robj)) {
            if(exist)
                ret = TRUE
            else
            {
                
                found = which(node == state$names_fun(robj))
                if(length(found) == 1)
                    ret = rpath_match(robj[[found]], tcond)
                else
                {
                    arr = new("rpath_matchList")
                    for(i in found)
                        arr@matches = c(arr@matches, rpath_match(robj[[i]], tcond))
                    ret = arr
                                        #                    ret = robj[found]
#                ret = robj[found]
 #               if(length(found) == 1)
 #                   ret = ret[[1]]
                }
            }
        } else {
            if(exist)
                ret = FALSE
            else
                ret = no_match_found()
        }
    }
 #   if(!exist)
 #   {
 #       if(length(ret) > 1)
 #       {
 #           terms = sapply(ret, state$term_condition)
 #           ret[terms] = lapply(ret[terms], terminal_node)
 #       } else if( state$term_condition(ret)) {
 #           ret = terminal_node(ret)
 #       }
  #  }
    ret
}


index_exec = function(robj, index, executors, state, exist = FALSE)
{
    if(!is(index, "numeric"))
       index =  as.numeric(index)
    if(exist)
        ret = FALSE
    else
        ret = no_match_found()
    if(index <= length(robj))
    {
        if(exist)
            ret = index
        else
          ret=  robj[[index]]
    }
    ret
}
       
    
executors <- list( 
    node = node_exec,
    allnodes = allnodes_exec,
    index =  index_exec,
    not = function(robj, operand, executors, state, exist) rpath_exec(robj, operand, TRUE, executors = executors, state = state, exist = exist),
    eq = function(robj, operands, executors, state, exist) rpath_compare(robj, operands[[1]], operands[[2]], exist = exist),
    noteq = function(robj, operands, executors, state, exist) !rpath_compare(robj, operands[[1]], operands[[2]], exist = exist),
    or = function(robj, operands, executors, state, exist) rpath_exec(robj, operands[[1]], exist = TRUE, executors = executors, state = state) || rpath_exec(robj, operands[[2]], exist = TRUE, executors = executors, state = state),
    and = function(robj, operands, executors, state, exist) rpath_exec(robj, operands[[1]], exist = TRUE, executors = executors, state = state) && rpath_exec(robj, operands[[2]], exist =  TRUE, executors = executors, state = state),
    string = function(robj, string, executors, state, exist) string
    )


rpath_exec <- function(robj, step, exist=FALSE, executors = executors, state)
{
    if(exist)
        res = FALSE
    else
        res = no_match_found()
    
    #if we hav emultiple matches from a previous step, run this step on all of them
    #separately and combine the results
    if (is(robj, "rpath_matchList"))
    {
        res = sapply(robj@matches, rpath_exec, step = step, exist = exist, executors = executors, state = state)
        
        if(!exist)
            res = combineMatchLists(lst = res, trim = TRUE)
        
        return(res)
    }
    
    if(is(robj, "rpath_match"))
        robj = robj@value
    
    if(step[[1]] == "predicate") {
        pred_type = step[[2]][[1]]
    #    if (is.vector(robj) && length(robj) > 1 && pred_type!= 'index') {
        if ( length(robj) > 1 && pred_type!= 'index') {
            found = rpath_exec(robj, step[[2]], exist = TRUE, executors = executors, state = state)
            if(exist)
                res = any(found)
            #if its a predicate, the matching element is robj (or no match)
            else if(found)
                res = robj
        } else if(pred_type == "index") {
            found = rpath_exec(robj, step[[2]], exist = TRUE, executors = executors, state = state)
            if(exist)
                res = (found > 0)
            else if(found > 0)
                res = robj[[ as.numeric( step[[ 2 ]][[ 2 ]] ) ]]
                
        } else {
            #exist=TRUE is hardcoded, we are checking for the predicate condition
            found = rpath_exec(robj, step = step[[2]], exist = TRUE, executors= executors, state = state)
            #if its a predicate, the matching element is robj (or no matc)h
            if(!exist && found)
                res = robj
        }
    } else {
        res = executors[[ step[[ 1 ]] ]](robj, step[-1], exist = exist, executors = executors, state = state)
    }
    if(!exist && !is(res, "rpath_match") && !is(res, "rpath_matchList"))
        res = rpath_match(res, state$term_condition)
    res
}

