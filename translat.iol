################################################################
# translator.iol

global var_count

procedure translate( x )                                # procedure translate() is the interface procedure to the translator.
  var_count := 1                                        # first we make all the gamma variables unique so they won't collide.
  fix_gamma_variables( x )    
  every t := inner_translate( x ) do                    # now go on to the inner translator.  Every time a translation is 
    if *t = 1 then                                      # generated, check to see that there is nothing in storage (i.e.
    {                                                   # the list is length 1).
      t := gamma(t[1])                                  # gamma reduce the result.
      if \disp_translation then                         # if the translation-display switch is on 
        fout$Write( lisp_stree( t ))                    # then show the translation.
      suspend t                                         # generate the translation.
    }
end

procedure inner_translate( x, pos, var )                # procedure inner_translate() does all the translation.
  local name                                            # pos tells which child node we're working on.
                                                        # var is the variable that was created by "!"
  if type(x$trans()) == "string"                        # at a leaf return the words semantic value.
    then return [x$trans()]

  /pos := 1                                             # start at the left of the list by default.

  if pos > *(x$trans())                                 # if we've reached the end of the list of children then
    then return [""]                                    # return "" to be concatenated on the end.
  else if pos = 1 & x$trans()[1] == "!" then            # check to see if we're building a quantifier.
  {
    var := next_Var()                                                    # get a new variable for the evaluator.
    name := ( inner_translate(x$children()[numeric(x$trans()[2])])[1] |  # set the name to the value of the second thing in the
              x$trans()[2]                                               # translation rule.
            )
    every t := inner_translate(x, 3, var) do                    # now get every translation of the rest of the rule.
      suspend check_node( x, pos, var, [ [name, t[1]] ] ||| t[2:0] )  # generate var as the translation, 
  }                                                                   # and the translation as stored.
  else                                                          # if it isn't a quantifier
    every t_next := inner_translate(x, pos+1, var) do           # iterate over the translations of the rest of the rule.
    {
      if n := numeric(x$trans()[pos]) then                      # if the thing at pos in the translation rule is a number
        every t_this := inner_translate(x$children()[n]) do     # get the translation of that child,
          suspend check_node(x, pos,                            # and generate it with the translation of the rest of the rule.
                             t_this[1] || " " || t_next[1],
                             t_this[2:0] ||| t_next[2:0])
      else if x$trans()[pos] == "&var" then                     # if the thing at pos is "&var" 
        suspend check_node(x, pos, var || " " || t_next[1], t_next[2:0]) # then put the variable in the translation
      else                                                                          # otherwise
        suspend check_node(x, pos, x$trans()[pos] || " " || t_next[1], t_next[2:0]) # just use the thing in the rule at pos.
    }
end

################################################
# cooper operations

procedure check_node( x, pos, translation, storage )            # procedure check_nodes() checks to see if we have a cooper node.
  if pos > 1                                                    # if we haven't got the whole rule translated, then 
    then return [translation] ||| storage                       # don't permute.
  if member( cur_c$nodes(), x$cat() )                           # otherwise, if the current node is a cooper node
    then suspend check_restricted( permute( translation, storage ), x$cat() )  # do the permutations
    else return check_restricted( [translation] ||| storage, x$cat() )         # else don't
end

procedure permute( t, coop )                                    # procedure permute() checks for gamma functions and then passes the
                                                                # parameter to inner_permute().
  if type(t) == "string"                                        # if it may not have been gamma reduced
    then t := gamma( t )                                        # then do so now.
  if \preserve_type & type(t) == "list" & *t > 2 & t[1] == "gamma" then  # if we have a gamma function
  {
    every x := permute( t[3], coop ) do                                  # then strip it off, permute on that, 
      suspend ["(gamma " || t[2] || " " || x[1] || ")"] ||| x[2:0]       # and un-strip it.
  }
  else                                                          # otherwise, 
  {
    t := lisp_stree(t)                                          # make it back into a string
    suspend inner_permute( t, coop )                            # and do the real permute.
  }
end

procedure inner_permute( t, coop )                              # procedure inner_permute() does the real permute.
  local i
  suspend [t] ||| coop                                          # first do the one with nothing brought out of storage.
  every i := 1 to *coop do                                      # for every item in storage
    if not violates( coop, i ) then                             # check for raising violations,
    {
      suspend inner_permute(                                    # and recurse
        "(" || coop[i][2] || " " || t || ")",                   # with the quantifier brought in
        coop[1:i]|||coop[i+1:0]                                 # and storage with that quantifier extracted.
      )
    }
end

procedure violates( l, c )                                      # violates() checks names of things in storage.
  name := l[c][1]
  every i := 1 to c-1 do
  {
    if (name == l[i][1]) == ("E" | "A")                         # don't raise an "E" past an "E", or an "A" past an "A".
      then return
  }
  fail
end

procedure check_restricted( t, nd_name )                        # procedure check_restricted() checks to see 
  if member( cur_c$restricted(), nd_name ) then                 # if the current node is a cooper-restricted node.
  {
    if *t > 1                                                   # if there is something in storage
      then fail                                                 # then fail
      else return t                                             # otherwise, let the translation pass
  }                                                             # if the node is not restricted
  return t                                                      # let the translation pass
end

################################################
# gamma function operations

procedure fix_gamma_variables( p )                              # procedure fix_gamma_variables() goes thru the p_tree making 
  local i, t                                                    # all the variable unique.
  if type(p) ~== "idol_object" then return                      # if at a leaf, return.
  t := p$trans()                                                # get the translation of the node.
  every i := 1 to *t - 1 do                                     # examine every part of the translation rule
  {
    if t[i] == "gamma" then                                     # for a gamma
    {
      t := l_replace( t[i+1], t, t[i+1] || "-" || var_count )   # and replace all occurances with the variable a number on it.
      var_count +:= 1                                           # go to the next number.
    }
  }
  p$set_trans( t )                                              # reset the translation of the node to have the unique variables.
  every fix_gamma_variables( !(p$children()) )                  # and fix all the children, too.
end

procedure gamma( x )                                            # procedure gamma() changes a string to a list
  return gamma_reduce(                                          # and does the gamma reduction.
    if type(x) == "string"                                      # if the parameter is a string,
        then lisp_ltree(x)                                      # make it a list
        else x                                                  # otherwise, just use the parameter.
  )
end

procedure gamma_reduce( x )                                     # procedure gamma_reduce() looks for gamma functions.
  local new_x
  if type(x) == "list" then                                     # if we have an expression (not just a word)
  {
    if (*x = 2) &                                               # and there are two items in it,
       (*x[1] = 3) &                                            # and the first one has three things in it (gamma var xxx).
       (x[1][1] == "gamma") &                                   # and the first thing there is a gamma
       (type(x[1][2]) == "string")                              # and its argument is a string
    then                                                        # we have: ( ( gamma string  xxx ) yyy )
      return gamma_reduce(l_replace( x[1][2], x[1][3], x[2] ))  # then replace the string in xxx with yyy, and gamma reduce that.
    new_x := []                                                 # start a new expression.
    every put( new_x, gamma_reduce( !x ) )                      # gamma reduce each thing into the new expression.
    return new_x                                                # return the new one.
  }
  else                                                          # if it isn't an expression
    return x                                                    # just return it.
end

procedure l_replace( var, exp, val )                            # procedure l_replace() replaces every var in exp with val.
  local new_exp 
  if type(exp) == "list" then                                   # if we have an expression
  {
    new_exp := []                                               # start a new expression,
    every put( new_exp, l_replace( var, !exp, val ) )           # and put every item, with replacements made, in the new one,
    return new_exp                                              # and return it.
  }
  else                                                          # if we don't have an expression
    if exp == var                                               # check to see if what we have is the variable.
      then return val                                           # if so replace it,
      else return exp                                           # otherwise leave it alone.
end

procedure lisp_stree( ltree )                                   # procedure lisp_stree() changes a list to a string.
    local s
    if type(ltree) == "string" then return ( ltree || " ")      # put a space on the end of a leaf.
    s := "("                                                    # start with a left paren,
    every s ||:= (lisp_stree( !ltree ))                         # get all the sublists,
    return s[1:-1] || ") "                                      # and put on a right paren.
end

procedure lisp_ltree( stree )                                   # procedure listp_ltree() changes a string to a list.
    if type(stree) == "string"                                  # if the input is a string
        then stree := toke_list( stree, '()' )                  # make it a list of tokens
    if *stree = 1                                               # if there is only one thing in the list
        then return stree[1]                                    # then return it.
    if lbal(stree) ~= *stree                                    # check to make sure the parentheses are balanced, or
        then fail                                               # fail.
    p := 2                                                      # go to the item after the first paren.
    thelist := []                                               # start a new list.
    while p < *stree do                                         # while we not at the end of the list of tokens
    {
        rp := lbal(stree[p:0])+p                                # get a paren-balanced unconverted sublist
        put(thelist, lisp_ltree(stree[p:rp]))                   # and put the converted sublist in the new list.
        p := rp                                                 # move ahead to the next token.
    }
    return thelist                                              # return the new list.
end
