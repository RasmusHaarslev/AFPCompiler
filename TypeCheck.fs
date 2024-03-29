﻿namespace GuardedCommands.Frontend
// Michael R. Hansen 06-01-2016

open System
open Machine
open GuardedCommands.Frontend.AST

module TypeCheck =

/// tcE gtenv ltenv e gives the type for expression e on the basis of type environments gtenv and ltenv
/// for global and local variables
   let rec tcE gtenv ltenv = function
         | N _              -> ITyp
         | B _              -> BTyp
         | Access acc       -> tcA gtenv ltenv acc
         | Addr acc         -> PTyp(tcA gtenv ltenv acc)

         | Apply(f,[e]) when List.exists (fun x ->  x=f) ["-"; "!"]
                            -> tcMonadic gtenv ltenv f e

         | Apply(f,[e1;e2]) when List.exists (fun x ->  x=f) ["+"; "*"; "="; "&&"; "-"; "<"; ">"; ">=";"<=";"%";"/"]
                            -> tcDyadic gtenv ltenv f e1 e2

         | Apply(f,es) ->
              // procedures not checked here....
              // bør heller ikke tjekkes her tror jeg
              tcNaryFunction gtenv ltenv f es

         | x                -> failwith "tcE: not supported yet"

   and tcMonadic gtenv ltenv f e = match (f, tcE gtenv ltenv e) with
                                   | ("-", ITyp) -> ITyp
                                   | ("!", BTyp) -> BTyp
                                   | _           -> failwith "illegal/illtyped monadic expression"

   and tcDyadic gtenv ltenv f e1 e2 = match (f, tcE gtenv ltenv e1, tcE gtenv ltenv e2) with
                                      | (o, ITyp, ITyp) when List.exists (fun x ->  x=o) ["+";"*";"-";"%";"/"]  -> ITyp
                                      | (o, ITyp, ITyp) when List.exists (fun x ->  x=o) ["=";"<";">";">=";"<="] -> BTyp
                                      | (o, BTyp, BTyp) when List.exists (fun x ->  x=o) ["&&";"="]     -> BTyp

                                      | _                      -> failwith("illegal/illtyped dyadic expression: " + f)

   and tcNaryFunction gtenv ltenv f es =
        let r = match Map.tryFind f ltenv with
                             | None   -> match Map.tryFind f gtenv with
                                         | None   -> failwith ("ano declaration for : " + f)
                                         | Some t -> t
                             | Some t -> t
        match r with
            | FTyp (rtList, Some t) ->
                if rtList = (List.map (tcE gtenv ltenv) es) then
                    t
                else
                    failwith "function call params does not match"
            | _ ->
            failwith "error in function call"

   and tcNaryProcedure gtenv ltenv f es = failwith "type check: procedures not supported yet"


/// tcA gtenv ltenv e gives the type for access acc on the basis of type environments gtenv and ltenv
/// for global and local variables
   and tcA gtenv ltenv =
         function
         | AVar x         -> match Map.tryFind x ltenv with
                             | None   -> match Map.tryFind x gtenv with
                                         | None   -> failwith ("ino declaration for : " + x)
                                         | Some t -> t
                             | Some t -> t

         | AIndex(acc, e) -> (*match acc with
                              | AIndex _   -> failwith "Nested array not allowed."
                              // I say pointers not implemented yet, but I'm not sure how exactly
                              // pointers and arrays are going to work out.
                              | ADeref _   -> failwith "Pointers not implemented yet."
                              | AVar x     ->

                                let (ATyp(z,_)) = tcA gtenv ltenv acc
                                // Check if user is accessing with integer.
                                if tcE gtenv ltenv e <> ITyp then
                                  failwith "Array indexing must be done with integer."
                                // Do regular variable loop otherwise.
                                match Map.tryFind x ltenv with
                                | None   -> match Map.tryFind x gtenv with
                                            | None   -> failwith ("ino declaration for : " + x)
                                            | Some t -> t
                                | Some t -> t
                                *)
                                match (tcA gtenv ltenv acc) with
                                  | ATyp (x,_) -> x
                                  | x -> x

         | ADeref e       -> match tcE gtenv ltenv e with
                             | PTyp ITyp -> ITyp
                             | PTyp BTyp -> BTyp
                             | PTyp (ATyp(t, _)) -> t
                             | _-> failwith "Deref: Illtyped dereference."

/// tcS gtenv ltenv retOpt s checks the well-typeness of a statement s on the basis of type environments gtenv and ltenv
/// for global and local variables and the possible type of return expressions
   and tcS gtenv ltenv = function
                         | PrintLn e -> ignore(tcE gtenv ltenv e)
                         | Ass(acc,e) -> if tcA gtenv ltenv acc = tcE gtenv ltenv e
                                         then ()
                                         else failwith "tcS: illtyped assignment"

                         | Block([],stms) -> List.iter (tcS gtenv ltenv) stms

                         | Block(xs,stms) ->
                              //update ltenv med xs
                              //eller kald tcGdecs
                              let ltenv = tcGDecs ltenv xs
                              List.iter (tcS gtenv ltenv) stms

                         | Return (Some e) ->
                              // ???
                              tcE gtenv ltenv e |> ignore
                              ()

                         | Call (n, expL) ->
                              //skal måske tilføje expl til ltenv her?
                              List.map (tcE gtenv ltenv) expL |> ignore
                              ()

                         | Alt (GC gc) | Do (GC gc) -> List.iter (tcGC gtenv ltenv) gc
                         | x              ->
                              failwith "tcS: this statement is not supported yet %A"

   and tcGC gtenv ltenv (e, stms) =  if tcE gtenv ltenv e = BTyp
                                     then List.iter (tcS gtenv ltenv) stms

   and tcGDec gtenv = function
                      // Array declaration.
                      | VarDec(ATyp (t,Some i),s) ->
                        if i < 0 then
                          failwith "Array size must be larger than 0."
                        Map.add s (ATyp (t,None)) gtenv
                      // Array formal parameter.
                      | VarDec(ATyp (t,None),s)   -> Map.add s (ATyp (t,None)) gtenv
                      | VarDec(t,s)               -> Map.add s t gtenv
                      | FunDec(t,f,decs,stm) as b ->
                        let typList = (tcGDecs Map.empty decs
                                          |> Map.toList
                                          |> List.map snd)

                        let mkLtenv acc x =
                            match x with
                                | VarDec (typ, v) -> Map.add v typ acc
                                | FunDec (t, f, decs, stm) -> Map.add f (FTyp (typList, t)) acc

                        let stmDecs = match stm with
                                      | Block(x,_) -> x
                                      | _ -> []

                        let ltenv = List.fold mkLtenv Map.empty (b::decs@stmDecs)


                        //check stm is well-typedi
                        tcS gtenv ltenv stm

                        // check returntype is correct...
                        let checkReturn t = function
                          | Return (Some x) ->
                              if tcE gtenv ltenv x = t then
                                  ()
                              else
                                  failwith "return type failure in function"
                          | _ -> ()

                        match t with
                          | None -> ()
                          | Some t ->
                              match stm with
                                  | Block(_,stms) -> List.iter (checkReturn t) stms
                                  | Return _ as k -> (checkReturn t) k
                                  // XD
                                  | _ -> ()

                        // check parameter are all different
                        let decToList = function
                            | VarDec(_,s) -> s
                            | FunDec(_, f, _, _) -> f

                        let decsAsList = List.map decToList decs

                        if List.distinct decsAsList = decsAsList then
                            Map.add f (FTyp (typList,t)) gtenv

                        else
                            failwith "illtyped function declaration"

   and tcGDecs gtenv = function
                       | dec::decs -> tcGDecs (tcGDec gtenv dec) decs
                       | _         -> gtenv


/// tcP prog checks the well-typeness of a program prog
   and tcP(P(decs, stms)) = let gtenv = tcGDecs Map.empty decs
                            List.iter (tcS gtenv Map.empty) stms
