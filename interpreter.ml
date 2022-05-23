exception Invalid_cm of string

exception End_cm of string

type stackValue = BOOL of bool | INT of int | ERROR | STRING of string | NAME of string | UNIT 
                 | CLOSURES of (stackValue * (command list)* (string * stackValue)list list)

and command = ADD |SUB | MUL | DIV | REM | NEG 
        | SWAP | POP | PUSH of stackValue | TOSTRING | PRINTLN | QUIT 
        | CAT | AND | OR | NOT | EQUAL | LESSTHAN | BIND | IF | LET | END | FUN of stackValue * stackValue | FUNEND | RETURN | CALL 

let interpreter ( input, output) : unit = 
	let ic = open_in input in

  (* Use the second argument as file name to open an output channel,
     and bind it to variable oc for later reference. *)
  let oc = open_out output in 

  (* Helper function: file input function. It reads file line by line
     and return the result as a list of string.  *)
  let rec loop_read acc =
      (* We use try with to catch the End_of_file exception. *)
      try 
          (* Read a line from ic. Build a new list with l::acc
             and pass to next recursive call. *)
          let l = String.trim(input_line ic) in loop_read (l::acc)
      with
        (* At the end of file, we will reverse the string since
           the list is building by keeping add new element at the 
           head of old list. *)
      | End_of_file -> List.rev acc 
			
		in 
   let strList = loop_read [] in

   let file_write str_val = Printf.fprintf oc "%s\n" str_val 
   in

   let str2val s =
         (match s with 
         | ":error:" -> ERROR
         | ":true:" -> BOOL true
         | ":false:" -> BOOL false 
         | ":unit:"  -> UNIT
         
         | _ -> 
            (match s.[0] with
            | '_' ->  NAME s
            | 'a'..'z' -> NAME s
            | 'A'..'Z' -> NAME s

            | '"' -> STRING  (String.sub s 1 (String.length s-2) )


            | _ -> 
               (match (String.contains s '.') with
               | true -> ERROR
               | false -> INT (int_of_string s ) ) ) )
      

   in
	let str2com s = 
			(match s with
			| "add" -> ADD
			| "sub" -> SUB
			(*more command on here *)
         | "mul" -> MUL
         | "div" -> DIV
         | "rem" -> REM
         | "neg" -> NEG
         | "swap" -> SWAP
         | "pop" -> POP
         | "toString" -> TOSTRING
         | "println" -> PRINTLN
         | "quit" -> QUIT
         | "cat" -> CAT
         | "and" -> AND
         | "or"  -> OR
         | "not" -> NOT
         | "equal" -> EQUAL
         | "lessThan" -> LESSTHAN
         | "bind" -> BIND
         | "if" -> IF
         | "let" -> LET
         | "end" -> END
          (* (for part 3 function) *)
             
         | "funEnd" -> FUNEND
         | "return" -> RETURN
         | "call" -> CALL
      
 			| _ ->
      
              (match (String.sub s 0 3) with
               | "fun" -> 
                    (match (String.split_on_char ' ' s) with 
                    | hd::fun_name::fun_arg::lol -> FUN ( (str2val fun_name), (str2val fun_arg) )
                    | _ ->  raise (Invalid_cm s) ) 
                | _ -> PUSH (str2val (String.sub s 5 (String.length s - 5) ) ) )
            
      )

               in 
      (* (###########################helper function for PART 3 ) *###########################*)
let stk_val2str stk_val =
  (match stk_val with 
  | BOOL true -> ":true:"
  | BOOL false -> ":false:"
  | ERROR -> ":error:"
  | UNIT -> ":unit:"
  | NAME(s) -> s
  | STRING (s) -> s
  | INT(x) -> string_of_int x) 


in

let com2str com =
  match com with 
  | ADD -> "add"
  | SUB -> "sub"
  (*more command on here *)
     | MUL -> "mul"
     | DIV -> "div"
     | REM -> "rem"
     | NEG -> "neg"
     | SWAP -> "swap"
     | POP -> "pop"
     | TOSTRING -> "toString"
     | PRINTLN -> "print"
     | QUIT-> "quit"
     | CAT -> "cat"
     | AND -> "and"
     | OR  -> "or"
     | NOT -> "not"
     | EQUAL -> "equal"
     | LESSTHAN -> "lessThan"
     | BIND -> "bind"
     | IF -> "if"
     | LET -> "let"
     | END -> "let"
      (* (for part 3 function) *)
         
     | FUNEND -> "funend"
     | RETURN -> "return"
     | CALL -> "call"

     | PUSH x -> "push " ^ (stk_val2str x)
     | FUN (a,b) -> "fun " ^ (stk_val2str a) ^ (stk_val2str b) 
    
      (* (###########################helper function for PART 3 ) *###########################*)

               

			(* use (sub to get first 4 char, and rest would be our value)
			use ( split will remove the white space, but if we push 3 value in a row, there would be problem) *)

    in
  
    (* the next function will take a list of string, where we get from file 
       call each string and make it into a command
       store the result into a command_list *)

    (* ( take our input strlist and divide each string into a command ) *)
    let comList = List.map str2com strList     

  in

  (* for debug purpose, I need to somehow print out the result of stack *)

  let sv2str sv =
   (match sv with 
   | INT(sv) -> (string_of_int sv)
   | BOOL true -> ":true:"
   | BOOL false -> ":false:"
   | ERROR -> ":error:"
   | UNIT -> ":unit:"
   | STRING sv -> sv
   | NAME sv -> sv )
   (* | CLOSURES ->  *)
   
  in 
   
   (* let rec print_all stack : unit = 
   match stack with
   | top::restack -> print_endline (sv2str top) ; print_all restack
   |_ ->  ()

  in *)

(* let rec processor cl stack env=  *)
(* (first of all, to add, we at least have to have TWO ITEM in the stack
      1. two int, just add it
      2. one name one int, convert name back to int, and add it
in case the convert is none, then error needed to be push)  
      3. one int one name
      4. two name
      5. any other case, error

      Repeat this processor for 'add' 'mul' 'sub' 'div'
      and rest any deal with 'int' type variable
*)

(* PART 2 helper
   find assoc value for whole enviromennt) *)

let rec find_assoc key env =
  match env with 
  | hd::tl -> 
    (match (List.assoc_opt key hd) with
    | Some(x) -> Some x  
    | _ -> find_assoc key tl )
  | _ -> None
  in 


  (* PART 3 helper) 
  (this function take comlist, and return separate it into two list,
     list 1 -> command before 'funend' which is the body of function
     list 2 -> rest command, for the main program
    >>>return (list1, list 2) ) *)
let rec function_sep_cml comList list1=
  match comList with 
  | FUNEND::list2 -> ( (List.rev list1), list2)
  | a::restcl -> (*file_write (com2str a);*) function_sep_cml restcl (a::list1) 
  | _ -> (list1,[])

in


(* let rec processor (cl: command list) (stackList: stackValue list list) (env: (stackValue * stackValue) list list) = *)

  (*while we have mutiple stack, we need to match the first 1  *)
let rec processor cl stackList (env : (string * stackValue) list list) = 
match (cl,stackList, env) with

  | (ADD::restcl, top_stack::restsk_list, env) ->
      (match top_stack with 

      | (NAME(a)::INT(b)::rest_top) ->
        (match (find_assoc a env) with
        | Some(INT (x) ) -> processor restcl ((INT(x+b)::rest_top)::restsk_list) env 
        | _ -> processor restcl ((ERROR::top_stack)::restsk_list) env )

      | (INT(a)::NAME(b)::rest_top) ->
        (match (find_assoc b env) with
        | Some(INT (x) ) -> processor restcl ((INT(x+a)::rest_top)::restsk_list) env 
        | _ -> processor restcl ((ERROR::top_stack)::restsk_list) env ) 

      | (NAME(a)::NAME(b)::rest_top) ->
        (match (find_assoc a env, find_assoc b env) with
        |  (Some(INT(x)), Some(INT(y)) ) -> processor restcl ((INT(x+y)::rest_top)::restsk_list) env 
        | _ -> processor restcl ((ERROR::top_stack)::restsk_list) env  )

      |  (INT(a)::INT(b)::rest_top) -> processor restcl ((INT(a+b)::rest_top)::restsk_list) env

      | _ -> processor restcl ((ERROR::top_stack)::restsk_list) env )

  | (SUB::restcl, top_stack::restsk_list, env) ->
        ( match top_stack with 
  
        | NAME(a)::INT(b)::rest_top ->
          (match (find_assoc a env) with
          | Some(INT (x) ) -> processor restcl ((INT(b-x)::rest_top)::restsk_list) env 
          | _ -> processor restcl ((ERROR::top_stack)::restsk_list) env )
  
        | INT(a)::NAME(b)::rest_top ->
          (match (find_assoc b env) with
          | Some(INT (x) ) -> processor restcl ((INT(x-a)::rest_top)::restsk_list) env 
          | _ -> processor restcl ((ERROR::top_stack)::restsk_list) env ) 
  
        | NAME(a)::NAME(b)::rest_top ->
          (match (find_assoc a env, find_assoc b env) with
          |  (Some(INT(x)), Some(INT(y)) ) -> processor restcl ((INT(y-x)::rest_top)::restsk_list) env 
          | _ -> processor restcl ((ERROR::top_stack)::restsk_list) env  )
  
        |  INT(a)::INT(b)::rest_top -> processor restcl ((INT(b-a)::rest_top)::restsk_list) env
  
        | _ -> processor restcl ((ERROR::top_stack)::restsk_list) env )

  | (MUL::restcl, top_stack::restsk_list, env) ->
          ( match top_stack with 
    
          | NAME(a)::INT(b)::rest_top ->
            (match (find_assoc a env) with
            | Some(INT (x) ) -> processor restcl ((INT(b*x)::rest_top)::restsk_list) env 
            | _ -> processor restcl ((ERROR::top_stack)::restsk_list) env )
    
          | INT(a)::NAME(b)::rest_top ->
            (match (find_assoc b env) with
            | Some(INT (x) ) -> processor restcl ((INT(x*a)::rest_top)::restsk_list) env 
            | _ -> processor restcl ((ERROR::top_stack)::restsk_list) env ) 
    
          | NAME(a)::NAME(b)::rest_top ->
            (match (find_assoc a env, find_assoc b env) with
            |  (Some(INT(x)), Some(INT(y)) ) -> processor restcl ((INT(y*x)::rest_top)::restsk_list) env 
            | _ -> processor restcl ((ERROR::top_stack)::restsk_list) env  )
    
          |  INT(a)::INT(b)::rest_top -> processor restcl ((INT(b*a)::rest_top)::restsk_list) env
    
          | _ -> processor restcl ((ERROR::top_stack)::restsk_list) env )
      
    | (DIV::restcl, top_stack::restsk_list, env) ->
             ( match top_stack with 
        
              | NAME(a)::INT(b)::rest_top ->
                (match (find_assoc a env) with
                | Some(INT (x) ) -> 
                  (match x with 
                    | 0 ->  processor restcl ((ERROR::top_stack)::restsk_list) env 
                    | _ -> processor restcl ((INT(b/x)::rest_top)::restsk_list) env)      

                | _ -> processor restcl ((ERROR::top_stack)::restsk_list) env )
        
              | INT(a)::NAME(b)::rest_top ->
                (match (find_assoc b env) with
                  | Some(INT(x)) -> 
                  (match a with 
                    | 0 ->  processor restcl ((ERROR::top_stack)::restsk_list) env
                    | _ -> processor restcl ((INT(x/a)::rest_top)::restsk_list) env) 
                | _ -> processor restcl ((ERROR::top_stack)::restsk_list) env )
                
              | NAME(a)::NAME(b)::rest_top ->
                (match (find_assoc a env, find_assoc b env) with
                  | (Some(INT(x)), Some(INT(y)) ) -> 
                      (match x with 
                        | 0 ->  processor restcl ((ERROR::top_stack)::restsk_list) env
                        | _ -> processor restcl ((INT(y/x)::rest_top)::restsk_list) env) 

                  | _ -> processor restcl ((ERROR::top_stack)::restsk_list) env )
        
              
              |  INT(0)::INT(b)::rest_top -> processor restcl ((ERROR::top_stack)::restsk_list) env 
              |  INT(a)::INT(b)::rest_top -> processor restcl ((INT(b/a)::rest_top)::restsk_list) env
        
              | _ -> processor restcl ((ERROR::top_stack)::restsk_list) env )

            
    | (REM::restcl, top_stack::restsk_list, env) ->
      ( match top_stack with 
 
       | NAME(a)::INT(b)::rest_top ->
         (match (find_assoc a env) with
         | Some(INT (x) ) -> 
           (match x with 
             | 0 ->  processor restcl ((ERROR::top_stack)::restsk_list) env 
             | _ -> processor restcl ((INT(b mod x)::rest_top)::restsk_list) env)      

         | _ -> processor restcl ((ERROR::top_stack)::restsk_list) env )
 
       | INT(a)::NAME(b)::rest_top ->
         (match (find_assoc b env) with
           | Some(INT(x)) -> 
           (match a with 
             | 0 ->  processor restcl ((ERROR::top_stack)::restsk_list) env
             | _ -> processor restcl ((INT(x mod a)::rest_top)::restsk_list) env) 
         | _ -> processor restcl ((ERROR::top_stack)::restsk_list) env )
         
       | NAME(a)::NAME(b)::rest_top ->
         (match (find_assoc a env, find_assoc b env) with
           | (Some(INT(x)), Some(INT(y)) ) -> 
               (match x with 
                 | 0 ->  processor restcl ((ERROR::top_stack)::restsk_list) env
                 | _ -> processor restcl ((INT(y mod x)::rest_top)::restsk_list) env) 

           | _ -> processor restcl ((ERROR::top_stack)::restsk_list) env )
 
       
       |  INT(0)::INT(b)::rest_top -> processor restcl ((ERROR::top_stack)::restsk_list) env 
       |  INT(a)::INT(b)::rest_top -> processor restcl ((INT(b mod a)::rest_top)::restsk_list) env
 
       | _ -> processor restcl ((ERROR::top_stack)::restsk_list) env )

       (* (equal) *)

       | (EQUAL::restcl, top_stack::restsk_list, env) ->
        (match top_stack with 
  
        | (NAME(a)::INT(b)::rest_top) ->
          (match (find_assoc a env) with
          | Some(INT (x) ) -> processor restcl ((BOOL(x==b)::rest_top)::restsk_list) env 
          | _ -> processor restcl ((ERROR::top_stack)::restsk_list) env )
  
        | (INT(a)::NAME(b)::rest_top) ->
          (match (find_assoc b env) with
          | Some(INT (x) ) -> processor restcl ((BOOL(x==a)::rest_top)::restsk_list) env 
          | _ -> processor restcl ((ERROR::top_stack)::restsk_list) env ) 
  
        | (NAME(a)::NAME(b)::rest_top) ->
          (match (find_assoc a env, find_assoc b env) with
          |  (Some(INT(x)), Some(INT(y)) ) -> processor restcl ((BOOL(x==y)::rest_top)::restsk_list) env 
            | _ -> processor restcl ((ERROR::top_stack)::restsk_list) env  )
    
          |  (INT(a)::INT(b)::rest_top) -> processor restcl ((BOOL(a==b)::rest_top)::restsk_list) env
    
          | _ -> processor restcl ((ERROR::top_stack)::restsk_list) env )

          | (LESSTHAN::restcl, top_stack::restsk_list, env) ->
            (match top_stack with 
      
            | (NAME(a)::INT(b)::rest_top) ->
              (match (find_assoc a env) with
              | Some(INT (x) ) -> processor restcl ((BOOL( b < x)::rest_top)::restsk_list) env 
              | _ -> processor restcl ((ERROR::top_stack)::restsk_list) env )
      
            | (INT(a)::NAME(b)::rest_top) ->
              (match (find_assoc b env) with
              | Some(INT (x) ) -> processor restcl ((BOOL( x < a)::rest_top)::restsk_list) env 
              | _ -> processor restcl ((ERROR::top_stack)::restsk_list) env ) 
      
            | (NAME(a)::NAME(b)::rest_top) ->
              (match (find_assoc a env, find_assoc b env) with
              |  (Some(INT(x)), Some(INT(y)) ) -> processor restcl ((BOOL( y < x)::rest_top)::restsk_list) env 
                | _ -> processor restcl ((ERROR::top_stack)::restsk_list) env  )
        
              |  (INT(a)::INT(b)::rest_top) -> processor restcl ((BOOL( b < a)::rest_top)::restsk_list) env
        
              | _ -> processor restcl ((ERROR::top_stack)::restsk_list) env )



              | (AND::restcl, top_stack::restsk_list, env) ->
                (match top_stack with 
          
                | (NAME(a)::BOOL(b)::rest_top) ->
                  (match (find_assoc a env) with
                  | Some(BOOL (x) ) -> processor restcl ((BOOL(x && b)::rest_top)::restsk_list) env 
                  | _ -> processor restcl ((ERROR::top_stack)::restsk_list) env )
          
                | (BOOL(a)::NAME(b)::rest_top) ->
                  (match (find_assoc b env) with
                  | Some(BOOL (x) ) -> processor restcl ((BOOL(x && a)::rest_top)::restsk_list) env 
                  | _ -> processor restcl ((ERROR::top_stack)::restsk_list) env ) 
          
                | (NAME(a)::NAME(b)::rest_top) ->
                  (match (find_assoc a env, find_assoc b env) with
                  |  (Some(BOOL(x)), Some(BOOL(y)) ) -> processor restcl ((BOOL(x && y)::rest_top)::restsk_list) env 
                    | _ -> processor restcl ((ERROR::top_stack)::restsk_list) env  )
            
                  |  (BOOL(a)::BOOL(b)::rest_top) -> processor restcl ((BOOL(a && b)::rest_top)::restsk_list) env
            
                  | _ -> processor restcl ((ERROR::top_stack)::restsk_list) env )


                  | (OR::restcl, top_stack::restsk_list, env) ->
                    (match top_stack with 
              
                    | (NAME(a)::BOOL(b)::rest_top) ->
                      (match (find_assoc a env) with
                      | Some(BOOL (x) ) -> processor restcl ((BOOL(x||b)::rest_top)::restsk_list) env 
                      | _ -> processor restcl ((ERROR::top_stack)::restsk_list) env )
              
                    | (BOOL(a)::NAME(b)::rest_top) ->
                      (match (find_assoc b env) with
                      | Some(BOOL (x) ) -> processor restcl ((BOOL(x||a)::rest_top)::restsk_list) env 
                      | _ -> processor restcl ((ERROR::top_stack)::restsk_list) env ) 
              
                    | (NAME(a)::NAME(b)::rest_top) ->
                      (match (find_assoc a env, find_assoc b env) with
                      |  (Some(BOOL(x)), Some(BOOL(y)) ) -> processor restcl ((BOOL(x||y)::rest_top)::restsk_list) env 
                        | _ -> processor restcl ((ERROR::top_stack)::restsk_list) env  )
                
                      |  (BOOL(a)::BOOL(b)::rest_top) -> processor restcl ((BOOL(a||b)::rest_top)::restsk_list) env
                
                      | _ -> processor restcl ((ERROR::top_stack)::restsk_list) env )




                      | (NEG::restcl, top_stack::restsk_list, env) ->
                        (match top_stack with 
                        | NAME(a)::rest_top ->
                          (match (find_assoc a env) with
                            | Some(INT(x)) -> processor restcl ((INT(-1*x)::rest_top)::restsk_list) env 
                            | _ -> processor restcl ((ERROR::top_stack)::restsk_list) env )  
                            
                        | INT(a)::rest_top -> processor restcl ((INT(-1*a)::rest_top)::restsk_list) env
                        | _ -> processor restcl ((ERROR::top_stack)::restsk_list) env ) 



                        | (NOT::restcl, top_stack::restsk_list, env) ->
                          (match top_stack with 
                          | NAME(a)::rest_top ->
                            (match (find_assoc a env) with
                              | Some(BOOL(x)) -> processor restcl ((BOOL( not x)::rest_top)::restsk_list) env 
                              | _ -> processor restcl ((ERROR::top_stack)::restsk_list) env )  
                              
                          | BOOL(a)::rest_top -> processor restcl ((BOOL( not a)::rest_top)::restsk_list) env
                          | _ -> processor restcl ((ERROR::top_stack)::restsk_list) env ) 


                          | (CAT::restcl, top_stack::restsk_list, env) ->
                            (match top_stack with 
                      
                            | (NAME(a)::STRING(b)::rest_top) ->
                              (match (find_assoc a env) with
                              | Some(STRING (x) ) -> processor restcl ((STRING( b ^ x)::rest_top)::restsk_list) env   
                              | _ -> processor restcl ((ERROR::top_stack)::restsk_list) env )
                      
                            | (STRING(a)::NAME(b)::rest_top) ->
                              (match (find_assoc b env) with
                              | Some(STRING (x) ) -> processor restcl ((STRING(x^a )::rest_top)::restsk_list) env 
                              | _ -> processor restcl ((ERROR::top_stack)::restsk_list) env ) 
                      
                            | (NAME(a)::NAME(b)::rest_top) ->
                              (match (find_assoc a env, find_assoc b env) with
                              |  (Some(STRING(x)), Some(STRING(y)) ) -> processor restcl ((STRING( y ^ x)::rest_top)::restsk_list) env 
                              | _ -> processor restcl ((ERROR::top_stack)::restsk_list) env  )
                      
                            |  (STRING(a)::STRING(b)::rest_top) -> processor restcl ((STRING(b^a)::rest_top)::restsk_list) env
                      
                            | _ -> processor restcl ((ERROR::top_stack)::restsk_list) env )


                            | (SWAP::restcl, top_stack::restsk_list, env) ->
                                (match top_stack with 
                                | a::b::rest_top -> processor restcl ((b::a::rest_top)::restsk_list) env
                                | _ -> processor restcl ((ERROR::top_stack)::restsk_list) env )

                            | (POP::restcl, top_stack::restsk_list, env) ->
                                  (match top_stack with 
                                  | a::rest_top ->  processor restcl (rest_top::restsk_list) env
                                  | _ -> processor restcl ((ERROR::top_stack)::restsk_list) env )


                                
                            | (TOSTRING::restcl, top_stack::restsk_list, env) ->
                                    (match top_stack with 
                                    | a::rest_top ->  processor restcl ((STRING(sv2str a)::rest_top)::restsk_list) env
                                    | _ -> processor restcl ((ERROR::top_stack)::restsk_list) env )

                            | (PRINTLN::restcl, top_stack::restsk_list, env) ->
                                      (match top_stack with 
                                      | a::rest_top ->  
                                        (match a with
                                        | STRING(a) -> file_write a; processor restcl (rest_top::restsk_list) env
                                        | _ -> processor restcl ((ERROR::top_stack)::restsk_list) env ) 
                                      | _ -> processor restcl ((ERROR::top_stack)::restsk_list) env )


                            | (  (PUSH stack_value)::restcl, top_stack::restsk_list, env) -> 
                                    processor restcl ((stack_value::top_stack)::restsk_list) env

                                    | (LET::restcl, stackList, env) -> processor restcl ([]::stackList) ([]::env) 

                                    | (END::restcl, (a::b::restsk_list), (new_env::rest_env) ) ->  
                                              (match a with 
                                              | hd::tl ->
                                                processor restcl ( (hd::b) ::restsk_list) rest_env
                                              | _ -> processor restcl (b::restsk_list) rest_env )

(* (for bind ) *)

| (BIND::restcl, ((NAME(a)::NAME(b)::rest_top)::restsk_list), top_env::env) ->
  (match (find_assoc a (top_env::env)) with
  | Some(x) ->  processor restcl ((UNIT::rest_top)::restsk_list) (((b,x)::top_env)::env)
  | None -> processor restcl ((ERROR::NAME(a)::NAME(b)::rest_top)::restsk_list) (top_env::env) )
  
(* (deal with name name, bascially find what value that Name bind to, and if it does, bind it to)  *)
| (BIND::restcl, top_stack::restsk_list, top_env::env) -> 
    (match top_stack with 
    | a::NAME(b)::rest_top ->
      (match a with 
      | INT(x) -> processor restcl ( (UNIT::rest_top)::restsk_list) ( ((b,a)::top_env)::env )
      | BOOL(x) ->(*file_write b ; file_write "bind to bool "; file_write (sv2str (BOOL(x)));*)processor restcl ( (UNIT::rest_top)::restsk_list) ( ((b,a)::top_env)::env )
      | STRING(x) -> processor restcl ( (UNIT::rest_top)::restsk_list) ( ((b,a)::top_env)::env )
      | UNIT -> (*file_write (b);file_write "bind to unit";*)processor restcl ( (UNIT::rest_top)::restsk_list) ( ((b,a)::top_env)::env )
      | _ -> processor restcl ((ERROR::top_stack)::restsk_list) (top_env::env) ) 
    | _ -> processor restcl ((ERROR::top_stack)::restsk_list) (top_env::env) ) 
      
| (BIND::restcl, top_stack::restsk_list, env) -> processor restcl ((ERROR::top_stack)::restsk_list) env  


| (IF::restcl, top_stack::restsk_list, env) -> 
    (match top_stack with 
    | a::b::c::rest_top ->
      (match c with 
      | NAME(x) ->
          (match (find_assoc x env) with 
          | Some(BOOL(v)) -> 
              (match v with
              | true -> processor restcl ((a::rest_top)::restsk_list) env
              | false ->processor restcl ((b::rest_top)::restsk_list) env)
          | _ ->  processor restcl ((ERROR::top_stack)::restsk_list) env )

      | BOOL true -> processor restcl ((a::rest_top)::restsk_list) env
      | BOOL false ->processor restcl ((b::rest_top)::restsk_list) env
      |_-> processor restcl ((ERROR::top_stack)::restsk_list) env )

    | _ -> processor restcl ((ERROR::top_stack)::restsk_list) env )

| ( ( FUN (a,b) )::restcl, top_stack::restsk_list, top_env::env) ->
    (* (here I nned to make a closure, bind function name, with colosure I created) *)
    (match (a,b) with 
    | ( NAME(function_name), NAME(arg_name) ) -> 
        (match (function_sep_cml restcl [] ) with 
          | (function_cmlist, rest_cmlist) -> 
          processor rest_cmlist   ( (UNIT::top_stack)::restsk_list) ( ( (function_name, CLOSURES( b , function_cmlist , (top_env::env) ) ) :: top_env ):: env)
          | _ -> (top_stack::restsk_list, top_env::env ) 
        )
    | _ -> (top_stack::restsk_list,  top_env::env) )


    | (CALL::restcl, top_stack::restsk_list, env) ->

      ( match top_stack with 
        | NAME(a)::NAME(b)::rest_stack -> 
            (match (find_assoc b env) with 
              | Some( CLOSURES( NAME(arg_name), comList, fun_env ) )-> 
                  (* (if a is bind to a value, I need to store it into the environment) *)
                  ( match (find_assoc a env ) with
                    | Some(x) -> 
                        let ( (last_stack_frame::tl)::fun_stack, return_env::rest_fun_env)  = processor comList ([]::top_stack::restsk_list) (  ( (arg_name, x )::[] )::fun_env ) 
                         in processor restcl  ((last_stack_frame::rest_stack)::restsk_list) (return_env::env)  (*are we suppose to keep the env?*)
      
                     | _ -> processor restcl ((ERROR::top_stack)::restsk_list) env 
                   )
              | _ -> processor restcl ((ERROR::top_stack)::restsk_list) env
            )

        | a:: NAME(b)::rest_stack -> 
          (match (find_assoc b env )  with 
              | Some( CLOSURES( NAME(arg_name), comList, fun_env ) )->
                let ( (last_stack_frame::tl)::fun_stack, return_env::rest_fun_env)  = processor comList ([]::top_stack::restsk_list) (  ( (arg_name, a )::[] )::fun_env ) 
                 in processor restcl  ((last_stack_frame::rest_stack)::restsk_list) (return_env::env)  (*are we suppose to keep the env?*)
      
              | _ -> processor restcl ((ERROR::top_stack)::restsk_list) env
          )
      
      
      
        | _ -> processor restcl ((ERROR::top_stack)::restsk_list) env 
        
        )


| (RETURN::restcl, stack, env) -> (stack, env)

     

  (* (Do I need to handle the FUNEND, if I encounter it? I think I might just drop it from my command) *)
(* (return) *)

(* | (RETURN::restcl, top_stack::restsk_list, top_env::env) ->
    (* (in here, I should try to return the value from the top of first stack, append to next) *)
    processor  *)

(* ( in the call, I may two processor call, one for return, one for next comming command) *)


                  



      

| (QUIT::restcl, stackList, env) -> (stackList, env)

| _ -> (stackList, env)


in processor comList [[]] [[]]; ();;


interpreter ("my_test_input.txt",  "my_output.txt") 
(* interpreter ("input1.txt",  "output1.txt") *)
         
                

  