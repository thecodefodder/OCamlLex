open Printf
open Token

let get_token_list lexbuf =
        let rec work acc = 
                match Lexer.token lexbuf with
                | EOF -> acc
                | t -> work (t::acc)
        in List.rev (work [])

let eval_expr tokens = 
    let rec factor = function
        | INT(i) :: rest -> (i, rest)
        | _ -> failwith "Unexpected token in factor"

    and term tokens =
        let rec term_acc acc tokens = 
            match tokens with
            | (MUL | DIV as op) :: rest -> 
                let (right, rest') = factor rest in
                let new_acc = 
                    match op with
                    | MUL -> acc * right
                    | DIV -> acc / right
                    | _ -> acc
                in term_acc new_acc rest'
            | _ -> (acc, tokens)
        in
        match factor tokens with
        | (left, rest) -> term_acc left rest

    and expr tokens = 
        let rec expr_acc acc tokens = 
            match tokens with
            | (ADD | SUB as op) :: rest -> 
                let (right, rest') = term rest in
                let new_acc = 
                    match op with
                    | ADD -> acc + right
                    | SUB -> acc - right
                    | _ -> acc
                in expr_acc new_acc rest'
            | _ -> (acc, tokens)
        in
        match term tokens with
        | (left, rest) -> expr_acc left rest

    in
    match expr tokens with
    | (result, []) -> result
    | _ -> failwith "Unexpected token after expression"

let () =
        let lexbuf = Lexing.from_string "10 + 2 * 3" in
        let token_list = get_token_list lexbuf in
        
        let result = eval_expr token_list in
        printf "Result: %d\n" result
