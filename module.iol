################################################################	
#   module.iol
#
# This file contains the code for the modules that the user can
# manipulate.  These are the grammar, lexicon, model and
# cooper-storage parameters modules.
#
# All the modules have a name field.  This is the name that they
# were created with.
#
# All the modules have a "show" method.  If the "show" method is
# called with no argument or with an empty string then everything in
# the module is shown.  If a string is passed to a "show" method then
# only things that are relevant to that string are shown.  
#
# All of the modules have an "add" method.  For the main three modules
# success or failure is meaningful.  If the method succeeds it means
# that the module used the string parameter as a definition and
# created at least one new entry in the module.  For instance, the
# grammar module tries to recognize it's parameter as a grammar rule.
# If it can then it creates a new entry and if it can't it fails.
# With this behavior it is easy to pass an input string to all the
# modules and see if one recognizes it, and the input handler doesn't
# have to know anything about the form of rules or definitions for
# these modules.

procedure select( which )
    cur_g := \grammar_t[which]
    cur_l := \lexicon_t[which]
    cur_m := \model_t[which]
    cur_c := \coop_t[which]
end

procedure new( what )                                           # procedure new() is for making new modules.
    local kind, name
    kind := toke_list( what )[1]                                # the first argument to the new is the kind of module to make.
    name := toke_list( what )[2] | fail                         # the second is the name of the new module.
    if ambiguous_match( "grammar", kind )                       # if the kind matches "grammar"
        then cur_g := grammar_t[name] := grammar(name) & return # then make a new grammar, put it in the table, and make it current.
    else if ambiguous_match( "lexicon", kind )                  # if the kind matches "lexicon"
        then cur_l := lexicon_t[name] := lexicon(name) & return # then make a new one, put it in the table and make it current.
    else if ambiguous_match( "model", kind )                    # if the kind matches "model"
        then cur_m := model_t[name] := model(name) & return     # then make a new one, put it in the table and make it current.
    else if ambiguous_match( "cooper-storage", kind )           # if the kind matches "cooper-storage"
        then cur_c := coop_t[name] := coop(name) & return       # then make a new one, put it in the table and make it current.
end

procedure module_status()                                       # module_status() shows which modules are current.
    msg$Write( "grammar is        " || cur_g$Name() )
    msg$Write( "lexicon is        " || cur_l$Name() )
    msg$Write( "model is          " || cur_m$Name() )
    msg$Write( "cooper-storage is " || cur_c$Name() )
end

procedure ambiguous_match( a, b )                               # ambiguous_match() checks two different length strings
    if *a < *b                                                  # to see if the common length portion matches.
        then { if b[1:*a+1] == a then return b }
        else { if a[1:*b+1] == b then return a }
    fail
end


####################################
# the grammar
#
# The grammar uses a "rule" structure to keep the rules in.  It keeps
# lists of these structures associated with strings. 
#
# The grammatical expansion part is kept in the "exp" part of the
# structure.  The semantic translation rule is kept in the "tran"
# part. Each of these parts is stored as a list of strings.  
class rule( public exp, public tran )
end

class grammar( public Name, rules, public punc )
  method add( x )
    the_list := toke_list( x, self.punc )                       # tokenize the input string
    if *the_list < 4 then fail                                  # if there isn't enough for lhs -> rhs => then fail.
    if the_list[2] ~== "->" then fail                           # if the second thing isn't -> then fail.
    every trans_pos := 1 to *the_list do                        # go through the list
      if the_list[trans_pos] == "=>"                            # looking for the =>
        then break
    if trans_pos > *the_list                                    # if we went past the end of the list
      then fail                                                 # then it is not a grammar rule.
    if trans_pos = *the_list                                    # if there is no translation rule 
      then put( the_list, "" )                                  # then put in a null string.
    r := rule( the_list[3:trans_pos],                           # make a new rule structure.
               the_list[trans_pos+1:0] )
    /self.rules[the_list[1]] := []                              # if needed, put a list in the table by the lhs symbol.
    put( self.rules[the_list[1]], r )                           # add this rule.
    return "rule added"                                         # throw back an informative message.
  end

  method expansion_list( goal )
    return \(self.rules[goal])                                  # fetch the rule expansion/right-hand side.
  end

  method show( s )                                              # show grammar information.
    s := trim(s, ' \t')                                         # trim off white space.
    if *s = 0 then self$show_all()                              # if a null string then show everything (show_all)
    (r_list := \self.rules[s]) | fail                           # get the rules with 's' as the lhs.
    every r := !r_list do                                       # iterate through the list
    {
      outs := s || " ->"                                        # start with lhs ->,
      every outs ||:= " " || !r$exp()                           # put on the stuff from the expansion,
      outs ||:= " =>"                                           # put on the =>,
      every outs ||:= " " || !r$tran()                          # and put on the translation rule;
      fout$Write( outs )                                        # display the rule.
    }
  end

  method show_all()
    x := sort( self.rules )                                     # sort everything in the grammar
    every self$show( (!x)[1] )                                  # and show all of it.
  end

initially 
  self.rules := table()                                         # setup the table to keep the rules in.
  self.punc := '!():;\'",.?'                                    # define punctuation marks.  should probably be user-definable.
end

####################################
# the lexicon
#
# the lexicon uses word structure to keep lexical items.
# w is the form of the word, and trans is the semantic value.
class word( public category, public w, public trans )
  method show()
    fout$Write( self.category || ":  " || self.w || " " || self$trans() )  # show a word like: "category:  form translation"
  end
end

procedure new_word( category, w, trans )
  return word( category, w, trans )                             # make a new word 
end

################################################
# class lexicon
#
# has a table by word form, and by category.  also
# has a field for punctuation.
#
class lexicon ( public Name, word_t, category_t, punc )
  method word_list( w )
    return \(self.word_t[w])                                    # fetch the list of words with a particular form.
  end

  method add( x )
    local category, form, value
    x := toke_list( x, self.punc )                              # tokenize the input string.
    category := pop(x)                                          # the first item should be the category.
    if (pop(x) ~== ":")                                         # if the second item is not a colon
      then fail                                                 # then fail.
    word_list := []                                             # start a list.
    while *x > 0 do                                             # go throught the token list
    {
      form := pop(x)                                            # getting word forms,
      value := pop(x)                                           # and semantic values;
      put( word_list, new_word( category, form, value ))        # putting each new word in the list;
      if *x > 0 & pop(x) ~== ";"                                # if there is more to go, make sure there is a semi-colon.
        then fail    
    }
    if *word_list < 1                                           # if we haven't got a single word
      then fail                                                 # then fail.
    /self.category_t[category] := []                            # create a new category in the category table if necessary.
    self.category_t[category] |||:= word_list                   # put the new words in.
    every x := !word_list do                                    # go throught the list of words
    {
      /self.word_t[x$w()] := []                                 # putting the form in the word form table
      put( self.word_t[x$w()], x )                              # and putting in the word structures of what we just read.
    }
    return string(*word_list) || " words added"                 # throw back an informative message.
  end

  method show( s )
    if *trim(s) = 0 then self$show_all()                        # if there is no argument then show all the lexicon.
    show_list( \self.word_t[s] )                                # otherwise: get the words by form and show them all.

    l := \self.category_t[s] | fail                             # get a list of words by category or fail
    str := s || ": "                                            # start building a string with "category: "
    every w := !l do                                            # every word structure in the list
    {
      str ||:= " " || w$w()                                     # put in the form,
      str ||:= " " || w$trans()                                 # the translation,
      str ||:= ";"                                              # and semi-colon.
    }
    fout$Write(str)                                             # display what we've built.
  end

  method show_all()
    l := sort(self.category_t)                                  # sort all the lexicon by category.
    every self$show( (!l)[1] )                                  # show each category.
  end

initially
  self.word_t := table()                                        # initialize the word table
  self.category_t := table()                                    # the category table
  self.punc := ';:()'                                           # and the punctuation
end

procedure show_list( x )
  every (!x)$show()                                             # show every thing in the list.
end


####################################
# the model
#
# A model is just a place to keep sets associated with
# predicates, as defined in evaluator.iol.  Also it keeps a
# collection of all the entities (atoms from types.iol) that
# have appeared in a predicate definition.
#
# the domain field is a set of all the atoms.
# the predicate field is a table by predicate name of 
# evaluator-type Sets.
class model ( name, domain, predicate )

    method show( s )
        if /s | s == "" then self$show_all()                    # if no argument then show all the model.
        if t := (\(self.predicate[s]))$Image()                  # get the image of the set object
            then fout$Write( s || "  " || t )                   # and show it with the name of the predicate.
    end

    method show_all()
        zip := sort(self.predicate)                             # sort the predicate table.
        every self$show( (!zip)[1] )                            # show all the predicates in the table.
    end

    method add( f )
        line := toke_list( f, '(){}<>' )                        # tokenize the list.

        if (*line = 0) |                                        # if it is empty
           (line[1] == string(!'(){}<>')) |                     # or the first symbol is a bracket
           (line[2] ~== "{") |                                  # or the second item is NOT a bracket
           (line[-1] ~== "}") |                                 # or the last item is NOT a bracket
           (not balanced( line[2:0] ))                          # of if the thing is not balanced right
          then fail                                             # then fail.

        title := convert(line[1])                               # convert the first token.
        if title$Type() ~== "Predicate" then fail               # and if it is not a predicate then fail.

        obj := convert( line[2:0] )                             # convert the rest of the input.
        if obj$Type() ~== "Set" then fail                       # check that it is a Set.
        self.predicate[title$Image()] := obj                    # put the set in the predicate table referenced by the predicate.

        every x := obj$element() do                             # get all the atoms in the set
          /(self.domain[x$Image()]) := x                        # and put them in the domain.
        return "predicate added to model"                       # return an informative message.
    end

    method Eval(x)                                              # Eval() gets the value of a predicate.
      if r := \(self.predicate[x])                              # if there is a predicate in the predicate table by this name
        then return r                                           # then return it
        else {                                                  # otherwise
          err_flag := 1                                         # set the error flag
          msg$Write( "undefined predicate: " || x )             # show an error message
          return Nil                                            # and bomb.
        }
    end

    method Name()
        return self.name
    end

    method domain()                                             # domain() is used to get all the atoms in the current model.
      every x := !self.domain do                                # for every thing in the model
        if /x                                                   # if it is &null
          then next                                             # goto the next one
          else suspend x                                        # otherwise generate it.
    end
    
initially
    self.domain := table()                                      # setup the domain table
    self.predicate := table()                                   # and the predicate table.
end

####################################
# cooper-storage flags
#
class coop( public Name, public nodes, public restricted )
  method add( x )
    y := toke_list(x,' \t')                                     # break the input into words.
    every insert( self.nodes, !y )                              # add each word to the set of cooper-nodes.
  end

  method restrict( x )
    y := toke_list(x,' \t')                                     # break the input into words.
    every insert( self.restricted, !y )                         # add each word to the set of cooper-restricted nodes.
  end

  method show( x )
    self$show_node(x)                                           # show cooper-nodes.
    self$show_restricted(x)                                     # show restricted nodes.
  end

  method show_node(x)
    s := "cooper-node "                                         # start with "cooper-node "
    if trim(x) == "" then                                       # if no particular node is asked for
      every s ||:= !self.nodes || " "                           # put them all in.
    else                                                        # otherwise
      if member(self.nodes, x)                                  # find out if the parameter is a cooper-node.
        then s ||:= x                                           # if so, put it on the string.
        else fail                                               # otherwise fail.
    fout$Write( s )                                             # show the string we built.
  end

  method show_restricted(x)
    s := "cooper-restrict "                                     # start with "cooper-restrict "
    if trim(x) == "" then                                       # if no particular node is asked for
      every s ||:= !self.restricted || " "                      # put them all in.
    else                                                        # otherwise
      if member(self.restricted, x)                             # check if the parameter is a restricted node.
        then s ||:= x                                           # if so, put it on the string.
        else fail                                               # otherwise fail.
    fout$Write( s )                                             # display the string.
  end

  initially
    self.restricted := set()                                    # setup the sets for restricted
    self.nodes := set()                                         # and cooper nodes.
  end
end   

