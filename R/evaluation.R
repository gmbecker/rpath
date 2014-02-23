

allnodes_exec = function(robj, node, exist, executors, state)
{
    if(is(robj, "rpath_matchList"))
        return(combineMatchLists(lst = lapply(robj@matches, allnodes_exec, node = node, exist = exist, executors = executors, state = state)))
    ret = list()
    path = "*"
    all = rpath_exec(robj, rpath_step("node", "*"), exist = FALSE, executors = executors, state = state)
#    ret = list(tmp)
    tmp = rpath_exec(robj, node@payload[[1]], exist = FALSE, executors = executors, state = state)
    ret = tmp
    cnt  =1
    while(!no_match(all) && cnt < 1000)
    {
#is this causing problems when we try to have predicates on terminal nodes?
        if(is(all, "rpath_matchList"))
            all = all[!sapply(all, checkTermCondition, term_fun = state$term_condition)]
        tmp = rpath_exec(all, node@payload[[1]], exist = FALSE, executors = executors, state = state)
        

        if(length(node@payload) ==2 && node@payload[[2]]@type == "predicate")
            tmp = rpath_exec(tmp, node@payload[[2]], exist = FALSE, executors = executors, state = state)
               
        tmp = combineMatchLists(lst = tmp)

        if(length(tmp) > 0)
            ret = c(ret, tmp)

        all = rpath_exec(all, rpath_step("node", "*"), exist = FALSE, executors = executors, state = state)
        cnt = cnt  + 1
    }
    ret = combineMatchLists(lst = ret)
    if(exist)
        no_match(ret)
    else
        ret
}


node_exec = function(robj, node, exist, executors, state)
{
     if(is(node, "rpath_step") && length(node@index))
        {
            index = node@index
           # node = node[-2] #will the index always be in the second spot? I think so...
        } else {
            index = NULL
        }

     if(is.list(node))
     {
         res = lapply(node, function(x) node_exec(robj, x, exist = exist, executors = executors, state = state))
         return(rpath_matchList(matches = res))
     }
     pload = node@payload[[1]]
#    node = unlist(node, recursive = FALSE)
    if(is.character(pload) && identical(pload, ""))
        if(!exist)
            return(robj)
        else
            return(TRUE)

    len = length(robj)
    if(is(robj, "rpath_matchList"))
    {
        res = lapply(robj, rpath_exec, node = node, exist = exist, executors = executors, state = state)
        if(!exist)
            ret = combineMatchLists(lst= res, trim = TRUE)
        else
            ret = simplify2array(res) #a logical vector should come out...
#    } else if (is.list(node) && length(node) > 1) {
                                        #XXX when does this code ever get invoked???
 #       print("I'm in the weird place")
  #      browser()
   #     res = robj
    #    for(i in seq(1, length(node), by=2)) {
     #       res = rpath_exec(res, node[i + 0:1], exist, executors = executors, state = state)
      #  }
       # ret = res
    }  else  {
        tcond = state$term_condition
        if(identical(pload, "*"))
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
            }
        } else if (pload %in% state$names_fun(robj)) {
            if(exist)
                ret = TRUE
            else
            {

                found = which(pload == state$names_fun(robj))
                if(length(found) == 1)
                    ret = rpath_match(robj[[found]], tcond)
                else
                {
                    arr = new("rpath_matchList")
                    for(i in found)
                        arr@matches = c(arr@matches, rpath_match(robj[[i]], tcond))
                    ret = arr
                }
            }
        } else {
            if(exist)
                ret = FALSE
            else
                ret = no_match_found()
            return(ret)
        }
    }

    #doing index based existence checking is harder, but is it even possible to specify with our subset of the xpath spec?
     if(!is.null(index))
     {
         if(is(ret, "rpath_match"))
             ret = as(list(ret), "rpath_matchList")
         
         if(!exist)
         {
             if(any(index > length(ret@matches)))
                 index = index[which(index <= length(ret@matches))]
             if(!length(index))
                 ret = no_match_found()
             else
                 ret = if(length(index) > 1) ret[index] else  ret[[index]]
         } else {
             ret = all(index > length(ret@matches))
         }
     }
     ret
}

attribute_exec = function(robj, step, executors, state, exist)
{
    if(exist)
        ret = FALSE
    else
        ret = no_match_found()
    ats = call_attr_fun(robj, state$attr_fun)
    if(step@payload[[1]] %in% names(ats))
    {
        if(exist)
            ret = TRUE
        else
            ret = ats[[ step@payload[[ 1 ]] ]]
    }
    ret
}

index_exec = function(robj, index, executors, state, exist = FALSE)
{
    if(is(index, "rpath_step"))
        index = index@payload[[1]]
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


#second argument is always a rpath_step or list of rpath_steps
executors <- list(
    node = node_exec,
    allnodes = allnodes_exec,
    index =  index_exec,
    not = function(robj, operand, executors, state, exist) !rpath_exec(robj, operand@payload[[1]],  executors = executors, state = state, exist = exist),
    eq = function(robj, operands, executors, state, exist) {
        rpath_compare(rpath_exec(robj, operands@payload[[1]], exist = FALSE, state = state, executors = executors),
                      rpath_exec(robj, operands@payload[[2]], exist = FALSE, state = state, executors = executors))
    },
    noteq = function(robj, operands, executors, state, exist) {
        !rpath_compare(rpath_exec(robj, operands@payload[[1]], exist = FALSE, state = state, executors = executors),
                      rpath_exec(robj, operands@payload[[2]], exist = FALSE, state = state, executors = executors))
    },
    lt = function(robj, operands, executors, state, exist) {
        rpath_compare(rpath_exec(robj, operands@payload[[1]], exist = FALSE, state = state, executors = executors),
                      rpath_exec(robj, operands@payload[[2]], exist = FALSE, state = state, executors = executors), "<")
    },
    lteq = function(robj, operands, executors, state, exist) {
        rpath_compare(rpath_exec(robj, operands@payload[[1]], exist = FALSE, state = state, executors = executors),
                      rpath_exec(robj, operands@payload[[2]], exist = FALSE, state = state, executors = executors), "<=")
    },
    gt = function(robj, operands, executors, state, exist) {
        rpath_compare(rpath_exec(robj, operands@payload[[1]], exist = FALSE, state = state, executors = executors),
                      rpath_exec(robj, operands@payload[[2]], exist = FALSE, state = state, executors = executors), ">")
    },
    gteq = function(robj, operands, executors, state, exist) {
        rpath_compare(rpath_exec(robj, operands@payload[[1]], exist = FALSE, state = state, executors = executors),
                      rpath_exec(robj, operands@payload[[2]], exist = FALSE, state = state, executors = executors), ">=")
    },

    or = function(robj, operands, executors, state, exist) rpath_exec(robj, operands@payload[[1]], exist = TRUE, executors = executors, state = state) || rpath_exec(robj, operands@payload[[2]], exist = TRUE, executors = executors, state = state),
    and = function(robj, operands, executors, state, exist) rpath_exec(robj, operands@payload[[1]], exist = TRUE, executors = executors, state = state) && rpath_exec(robj, operands@payload[[2]], exist =  TRUE, executors = executors, state = state),
    string = function(robj, string, executors, state, exist) rpath_const("character", string@payload[[1]]),
    attribute = attribute_exec
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

    #this will happen when we pass in the payload from an rpath_step object
    if(is(step, "list"))
    {
        res = sapply(step, function(st) rpath_exec(robj, step = st, exist = exist, executors = executors, state = state))
        if(!exist)
            res = combineMatchLists(lst = res, trim = TRUE)
        return(res)
    }
    
    if(is(robj, "rpath_match"))
        robj = robj@value

    
    if(step@type == "predicate") {
        pred_type = step@payload[[1]]@type
        if ( length(robj) > 1 && pred_type != 'index') {
            found = rpath_exec(robj, step@payload[[1]], exist = TRUE, executors = executors, state = state)
            if(exist)
                res = any(found)
            #if its a predicate, the matching element is robj (or no match)
            else if(found)
                res = robj
        } else if(pred_type == "index") {
            found = rpath_exec(robj, step@payload, exist = TRUE, executors = executors, state = state)
            if(exist)
                res = (found > 0)
            else if(found > 0)
                res = robj[[ as.numeric( step@payload[[1]]@payload[[1]] ) ]]

        } else {
            #exist=TRUE is hardcoded, we are checking for the predicate condition
            found = rpath_exec(robj, step = step@payload[[1]], exist = TRUE, executors= executors, state = state)
            #if its a predicate, the matching element is robj (or no matc)h
            if(exist)
                any(found)
             else if(!exist && found)
                res = robj
        }
    } else {
      #  res = executors[[ step@type ]](robj, step@payload, exist = exist, executors = executors, state = state)
          res = executors[[ step@type ]](robj, step, exist = exist, executors = executors, state = state)
    }
    if(!exist && !is(res, "rpath_match") && !is(res, "rpath_matchList"))
        res = rpath_match(res, state$term_condition)
    res
}

