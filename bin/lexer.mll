{
        open Token
}

rule token = parse
   [' ' '\t' '\n']
     {token lexbuf}
   | ['+']
      { ADD }
   | ['-']
      { SUB }
   | ['*']
      { MUL }
   | ['/']
      { DIV }
   | ['0'-'9']+ as lxm
      { INT(int_of_string lxm) }
   | eof
     { EOF }
