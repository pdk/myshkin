################################################################
# myshkin.iol
#

procedure main(args)                                            # the main() procedure
  create_defaults()                                             # create the defaults for the global variables.
  if *args > 0 then fin$Open( args )                            # open files on the command line.
  execute_commands()                                            # execute all incoming commands.
end

global fin                                                      # input file
global fout                                                     # output file
global msg                                                      # message/error file
global model_t                                                  # table of models
global cur_m                                                    # current model
global grammar_t                                                # table of grammars
global cur_g                                                    # current grammar
global lexicon_t                                                # table of lexicons
global cur_l                                                    # current lexicon
global coop_t                                                   # table of cooper-storages
global cur_c                                                    # current cooper-storage
global True                                                     # Boolean object "true"
global False                                                    # Boolean object "false"
global Nil                                                      # Boolean object "nil" ("unknown")
global IndInd                                                   # individual indeterminate "?".
global disp_parse_string                                        # parse-string display switch
global disp_parse_tree                                          # parse-tree display switch
global disp_translation                                         # translation display switch
global preserve_type                                            # type-preserve switch
global cur_goal                                                 # current goal for parsing

procedure create_defaults()                                     # create_defaults() sets defaults for the global variables.
  True := Boolean("true")                                       # set the objects for the evaluator.
  False := Boolean("false")
  Nil := Boolean("nil")
  IndInd := new_Atom("?")

  disp_parse_string := &null                                    # set the parse-string display switch "off".
  disp_parse_tree := 1                                          # set the parse-tree display switch "on".
  disp_translation := 1                                         # "on"
  preserve_type := 1                                            # "on"

  cur_goal := "s"                                               # make the default goal "s".

  grammar_t := table()                                          # set up the grammar table, and 
  grammar_t["default"] := grammar("default")                    # create the default grammar.
  cur_g := grammar_t["default"]                                 # make the default grammar the current one.

  lexicon_t := table()                                          # set up the lexicon table, and
  lexicon_t["default"] := lexicon("default")                    # create the default lexicon.
  cur_l := lexicon_t["default"]                                 # make the default lexicon the current one.

  model_t := table()                                            # set up the model table, and
  model_t["default"] := model("default")                        # create the default model.
  cur_m := model_t["default"]                                   # make the default model the current one.

  coop_t := table()                                             # set up the cooper-storage table, and
  coop_t["default"] := coop("default")                          # create the default cooper-storage.
  cur_c := coop_t["default"]                                    # make the default cooper-storage the current one.
  cur_c$add("s")                                                # make "s" a cooper-node. (where quants can be brought in.)

  fin := director( "in" )                                       # set up the input file,
  fout := director( "out" )                                     # the output file,
  msg := director( "out" )                                      # and the msg/error file.
end

procedure status()                                              # procedure status shows the status of the system.
   module_status()                                              # show the module status,
   file_status()                                                # the file status,
   switch_status()                                              # and the switch status.
end

procedure switch_status()                                       # procedure switch_status displays the status of the switchs.
   msg$Write( "goal is           " || cur_goal )                # show the current goal for parsing,
   msg$Write( "parse-string is   " ||                           # the state of the parse-string display switch,
               if /disp_parse_string then "off" else "on" )     
   msg$Write( "parse-tree is     " ||                           # the parse-tree switch,
               if /disp_parse_tree then "off" else "on" )
   msg$Write( "translation is    " ||                           # the translation switch,
               if /disp_translation then "off" else "on" )
   msg$Write( "preserve-type is  " ||                           # and the type-preservation switch.
               if /preserve_type then "off" else "on" )
end

procedure execute_commands()                                    # procedure execute_commands() reads the input file 
                                                                # until there is no more.
  local p, p_count, argument, command, message
  repeat
  {
    (line := fin$Read()) | return                               # get the next line or stop.
    command := toke(line) |                                     # get the first word on the line as the 'command', or 
       next                                                     # get the next line.
    argument := trim(line[(*command)+2:0]) | ""                 # set 'argument' to be the rest of the input line.
    case command of {                                       
      "!": system( argument )                                   # do a shell command.
      "echo": fout$Write( argument )                            # echo the argument to the output file.

      "trace": set_trace( argument )                            # set the trace counter.

      "parse-string":  disp_parse_string := set_flag( toke(argument) | "on" )    # set the parse-string display switch.
      "parse-tree":    disp_parse_tree := set_flag( toke(argument) | "on" )      # set the parse-tree display switch.
      "translation":   disp_translation := set_flag( toke(argument) | "on" )     # set the translation display switch.
      "preserve-type": preserve_type := set_flag( toke(argument) | "on" )        # set the type-preservation switch.
      "goal":          cur_goal := toke(argument)                                # set the current goal to a symbol.

      "done"|"end": return                                      # stop the program.
      "read": fin$Open( toke_list(argument) )                   # open an input file.
      "write": fout$Open( argument )                            # open an output file.
      "eof": fin$Close() | return                               # close the input, pop the input file stack
      "close":                                                  # close
        if argument == ("read"|"input")
          then fin$Close()                                      # the input file,
          else fout$Close()                                     # or the output file (output is default).

      "parse": every parse( argument, cur_goal )                # parse the input string. (don't translate or evaluate it.)
      "translate": every translate( parse( argument, cur_goal ))# translate the input string (don't evaluate it.)

      "new": new( argument )                                    # create a new module.
      "select": select( argument )                              # select modules.
      "status": status()                                        # display the system status.
      "cooper-node": cur_c$add( argument )                      # add a cooper-node.
      "cooper-restrict": cur_c$restrict( argument )             # add a cooper-restricted node.

      "show-grammar": cur_g$show(argument)                      # show information in the current grammar.
      "show-lexicon": cur_l$show(argument)                      # show information in the current lexicon.
      "show-model": cur_m$show(argument)                        # show information in the current model.
      "show-cooper": cur_c$show(argument)                       # show information in the current cooper-storage.
      "show":                                                   # show information in all of 
      {
        cur_g$show(argument)                                    # the current grammar,
        cur_l$show(argument)                                    # the current lexicon, 
        cur_m$show(argument)                                    # the current model,
        cur_c$show(argument)                                    # AND the current cooper-storage.
      }
      default:                                                  # if we don't recognize any of the above commands, 
      {
        if message := (                                         # try adding to 
          cur_g$add(line) |                                     # the current grammar,
          cur_l$add(line) |                                     # lexicon,
          cur_m$add(line)                                       # of model.
        )
        then msg$Write( message )                               # if something accepted it, then display the confirmation message.
        else                                                    # otherwise
        {
          p_count := 0
          every p := parse(line, cur_goal) do                   # try to parse the input line
          {
            p_count +:= 1                                       # every time we get a successful parse, count it,
            every evaluate( lisp_stree( translate( p )))        # and evaluate every translation of it.
          }
          if p_count = 0 then                                   # if there were no successful parses,
            evaluate( line ) |                                  # try to evaluate it as an expression.
              msg$Write( "not interpretable: " || line )        # otherwise say nothing can be done with the input.
        }
      }
    }
  }
end

procedure set_flag( s )                                         # procedure set_flag() returns
  if s == "off" 
    then return &null                                           # &null if "off",
    else return 1                                               # 1 by default.
end

procedure set_trace( f )                                        # procedure set_trace() sets the trace counter
    if f == ("off")
        then &trace := 0                                        # off or
        else &trace := -1                                       # on.
end
