################################################################
# parser.iol

class p_tree( public trans, public cat, public children )       # Declare the p_tree structure.
  method set_trans( x )
    self.trans := x
  end
end                                                             # The 'trans' part holds the translation rule.  'cat' holds the node
                                                                # category.  'children' is a list of children.
global p_subject, p_pos                                         # 'p_subject' will hold the list of words in the input sentence.
                                                                # 'p_pos' holds the current position in the list.
procedure parse( s, goal )
  (p_subject := toke_list( \s, cur_g$punc() )) | fail           # set p_subject to have the list of words and punctuation marks from
                                                                # the input sentence.
  p_pos := 1                                                    # set p_pos to 1 so that we start with the first word of the
                                                                # sentence. 
  every t := rd_parse( goal, *p_subject ) do                    # every time the recursive descent parser generates a p_tree
                                                                # check to make sure the current position in the list is beyond
    if p_pos > *p_subject then                                  # the last word in the list.  i.e. make sure all the words have
    {                                                           # been accounted for.
      display_tree( t )                                         # go display whatever we need to about the parse tree.
      suspend t                                                 # generate the p_tree as a result.
    }
end


procedure rd_parse( goal, target_length )                       # 'rd_parse' is the main parsing procedure.  the target_length    
								# paramter keeps track of how many words would need to be in the list 
								# to match all the pending branches of the parse tree.  This is used  
                                                                # to limit infinite left recursion. 
  if target_length < 1                                          # make sure we have enough words in the list to match all the
    then fail							# branches of the tree.  If we don't then fail.              

  case type( goal ) of
  {
    "string":                                                   # If the goal is a string type
    {
      if expansion_list := cur_g$expansion_list( goal ) then    # then check the grammar for rules based on this symbol.  What
								# we'll get back is a list of 'rule' structures.              
        every e := !expansion_list do                           # we got a list of rules.  now we need to try each one in the
								# list.                                                      
          suspend p_tree(e$tran(), goal,                        # if the rd_parse call fails then this whole expression will
                         rd_parse( e$exp(), target_length )	# fail and nothing will be generated.  Also, if it does work
                        )					# it will be resumed until it doesn't.                      
      suspend parse_match( goal )                               # Now check to see if we can get a lexical match with in the
                                                                # sentence. 
    }
    "list":                                                     # If the goal is a list type
      if *goal = 1 then                                         # is this list of length 1?
        suspend [rd_parse( goal[1], target_length )]            # then we don't need to break it down.
      else                                                      # if it's longer than 1 long then we split it into the first
                                                                # item in the list and everything else.
        suspend [rd_parse( goal[1], target_length-(*goal-1) )] |||    # the left side ("goal[1]")
                 rd_parse( goal[2:0], target_length-1 )               # and the rest. ("goal[2:0]")
    default:
      msg$Write( "illegal parse request: " || image(goal) )
  }
end

procedure parse_match( goal )
  local cur_w

  cur_w := p_subject[p_pos]                                     # get the word at the current position.
  p_pos +:= 1                                                   # increment the current position.

  wlist := cur_l$word_list( cur_w )                             # get the list of words that have the form of the current
                                                                # word from the current lexicon.
  if /wlist then                                                # if there are NO items in the lexicon with this form
  {
    if \cur_w == goal                                           # check to see if the current word matches what is in the
      then suspend p_tree( "", goal, [cur_w] )			# grammatical rule and generate a p_tree if so.
  }
  else
    every w := !wlist do                                        # otherwise check all the word structures in the
      if goal == w$category() then				# word list to see if the category matches the  
        suspend p_tree( w$trans(), goal, [cur_w] )		# goal.  Generate a p_tree when there is a match.

  p_pos -:= 1                                                   # reset the current position
end

################################################################
#  Hereafter are support routines for displays.

procedure display_tree( x )                                     # display_tree checks the parse-display flags and displays whatever
    local out_line, temp_file                                   # is necessary. 
    
    if \disp_parse_string then                                  # check the flag to see if we should display the parse string. 
    {
      out_line := cb_tree( x )                                  # if so, then display it.
      msg$Write( out_line )
    }
    if \disp_parse_tree then                                    # check the flag for the parse tree display.
    {
      out_line := cb_tree_trans( x )                            # construct the string that will be the input to chris barkers 
								# tree program.                                                
      temp_file := open( "x.x.x.x", "c" )                       # open a temporary file.
      write( temp_file, ".tr " || out_line )                    # write out the string.
      close( temp_file )                                        # close the file.
      system( "tree <x.x.x.x >y.y.y.y" )                        # execute the tree command and put the output in a file.
      every line := !open("y.y.y.y") do                         # now we can copy the tree to the current output stream.
        fout$Write( line )
      system( "rm x.x.x.x y.y.y.y" )                            # and delete the temporary files.
    }
end

procedure backslash( x )                                        # backslash backslashes parenthesis in translation rules so that 
    if type(x) == "string"					# chris barkers program won't use them as tree information.                 
        then x := [x]
    s := ""                                                     # start with an empty string.
    state := 0                                                  # start with the state where we don't need a space before the next
								# item.                                                           
    every p := !x do                                            # traverse the list of strings.
        if p == ("(" | ")") then                                # if it is a paren
        {
            s ||:= "\\" || p                                    # then put a backslash in front of it,
            state := 0                                          # and set the state so that we don't put a space after the paren.
        } else {                                                # otherwise:
            if state = 1 then s ||:= " "                        # if the state says so, put a space.
            state := 1                                          # set the state so that the next thing will have a space
								# before it.                                            
            s ||:= p                                            # put the next item on the end of the string.
        }
    return s                                                    # return the new string.
end

procedure cb_tree_trans( pt )                                   # cb_tree_trans converts a p_tree into something that chris barkers
    local s, r, n						# tree program can deal with.  The translation rules are put in on the
								# second line of each node.                                           
    case type( pt ) of
    {
        "idol_object":                                          # if it's a p_tree
        {
            s := backslash( pt$trans() )                        # get a usable string of the translation rule.
            r := pt$cat()                                       # get the node category.
            if *s > 0 then                                      # if there is a translation rule (if the length of the
            {                                                   # string returned by 'backslash()' is larger than 0)
                n := (*r < *s | *r)                             # get the maximum width between the translation rule
                                                                # string and the category string.
                r := center(r,n)                                # center the two things.
                s := center(s,n)
                s := "(" || r || "\n" || s                      # put them together with a newline.
            } 
            else s := "(" || r                                  # if there is no translation then just use the category label.
            every s ||:= cb_tree_trans( !(pt$children()) )      # now, append all the children.
            s ||:= ")"                                          # put on the closing paren
            return s                                            # and return.
        }
        
        "string": return "(" || pt || ")"                       # if it's a string then put it in parenthesis and return it.
        default: write( "*** bad input to cb_tree_trans()" )
    }
end

procedure cb_tree( pt )                                         # cb_tree is the oldfashioned one.  it doesn't show translations.
    local s                                                     # This is what is used for the parse-string that can be displayed. 
    case type( pt ) of
    {
        "string": return "(" || pt || ")"
        "idol_object":
        {
            s := "(" || pt$cat()
            every s ||:= cb_tree( !(pt$children()) )
            return s || ")"
        }
        default: write( "*** bad input to cb_tree()" )
    }
end
