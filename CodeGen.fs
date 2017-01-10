namespace GuardedCommands.Backend
// Michael R. Hansen 05-01-2016
// This file is obtained by an adaption of the file MicroC/Comp.fs by Peter Sestoft
open System
open Machine

open GuardedCommands.Frontend.AST
module CodeGeneration =


(* A global variable has an absolute address, a local one has an offset: *)
   type Var =
     | GloVar of int                   (* absolute address in stack           *)
     | LocVar of int                   (* address relative to bottom of frame *)

(* The variable environment keeps track of global and local variables, and
   keeps track of next available offset for local variables *)

   type varEnv = Map<string, Var*Typ> * int

(* The function environment maps function name to label and parameter decs *)

   type ParamDecs = (Typ * string) list
   type funEnv = Map<string, label * Typ option * ParamDecs>

/// CE vEnv fEnv e gives the code for an expression e on the basis of a variable and a function environment
   let rec CE vEnv fEnv =
       function
       | N n          -> [CSTI n]
       | B b          -> [CSTI (if b then 1 else 0)]
       | Access acc   -> CA vEnv fEnv acc @ [LDI]

       | Apply("-", [e]) -> CE vEnv fEnv e @  [CSTI 0; SWAP; SUB]

       | Apply("!", [e]) -> CE vEnv fEnv e @ [NOT]

       // behøver den her sin egen case???
       // er det noget med man gerne vil have så lidt maskine kode som muligt?
       | Apply("&&",[b1;b2]) -> let labend   = newLabel()
                                let labfalse = newLabel()
                                CE vEnv fEnv b1 @ [IFZERO labfalse] @ CE vEnv fEnv b2
                                @ [GOTO labend; Label labfalse; CSTI 0; Label labend]

       | Apply(o,[e1;e2]) when List.exists (fun x -> o=x) ["+"; "*"; "="; "-"; "<";"<="; ">"; ">=";"%";"/"]
                             -> let ins = match o with
                                          | "+"  -> [ADD]
                                          | "*"  -> [MUL]
                                          | "="  -> [EQ]
                                          | "<"  -> [LT]
                                          | ">=" -> [LT;NOT] // Not implemented in parser yet I think. AND NOT TESTED.
                                          | "%"  -> [MOD]
                                          | "/"  -> [DIV]
                                          | "<=" -> [SUB; CSTI 1; LT] // Skal bruge input for at overbevise mig selv om korrektheden for dette. Er bange for edgecases.
                                          | "-"  -> [SUB]
                                          | _    -> failwith "CE: this case is not possible"
                                CE vEnv fEnv e1 @ CE vEnv fEnv e2 @ ins

       | Apply(o,[e1;e2]) when List.exists (fun x -> o=x) [">";"<="]
                             -> let ins = match o with
                                          | ">"  -> [LT]
                                          | _    -> failwith "CE: this case is not possible"
                                CE vEnv fEnv e2 @ CE vEnv fEnv e1 @ ins

       | Apply(o,es) ->
              let (l, p, paraNames) = Map.find o fEnv
              List.collect (fun e -> CE vEnv fEnv e) es @ [CALL(List.length es, l)]

       | _            -> failwith "CE: not supported yet"


/// CA vEnv fEnv acc gives the code for an access acc on the basis of a variable and a function environment
   and CA vEnv fEnv = function | AVar x         -> match Map.find x (fst vEnv) with
                                                   | (GloVar addr,_) -> [CSTI addr]
                                                   | (LocVar addr,_) -> failwith "CA: Local variables not supported yet"
                               | AIndex(acc, e) ->
                                   // Array indexing takes an "access" and an expression
                                   // Not sure why, but I think it's because of pointers later.
                                   match acc with
                                     // We only allow the user to access arrays by referencing the array name
                                     // Hence why we error on ADeref and AIndex.
                                     | AVar x     ->
                                         // We look up and see the array address
                                         match Map.find x (fst vEnv) with
                                           | (GloVar addr,ATyp (_,Some size)) ->
                                              //match t with
                                              //  | _   -> printfn "%A" t
                                              // To find the right position, we
                                              // take the array stack position and add the index.
                                              let expCode = CE vEnv fEnv e
                                              //we need to check if size > rvalue of e.
                                              let outOfBounds = newLabel()
                                              let labelEnd = newLabel()
                                              // First we check if index out of bounds.
                                              expCode @ [CSTI size;LT;IFZERO outOfBounds] @
                                              // If index is within bounds, we find the address by
                                              // expression result + array address.
                                              // After the addition, we GOTO labelEnd.
                                              expCode @ [CSTI addr; ADD; GOTO labelEnd; Label outOfBounds; STOP; Label labelEnd]
                                           | (GloVar addr,_) -> failwith "Array formal parameter access not yet supported."
                                           | (LocVar addr,_) -> failwith "CA: Local variables not supported yet"
                                     | ADeref _   -> failwith "Pointers not implemented yet."
                                     | AIndex _   -> failwith "Nested arrays detected I think."
                               | ADeref e       -> failwith "CA: pointer dereferencing not supported yet"


(* Bind declared variable in env and generate code to allocate it: *)
   let allocate (kind : int -> Var) (typ, x) (vEnv : varEnv)  =
    let (env, fdepth) = vEnv
    match typ with
    | ATyp (ATyp _, _) ->
      raise (Failure "allocate: array of arrays not permitted")
    | ATyp (t, Some i) ->
      //  We initialize the array values to 0.
      let newEnv = (Map.add x (kind fdepth, typ) env, fdepth+i)
      let code = [INCSP i]
      (newEnv, code)
    | _ ->
      let newEnv = (Map.add x (kind fdepth, typ) env, fdepth+1)
      let code = [INCSP 1]
      (newEnv, code)


/// CS vEnv fEnv s gives the code for a statement s on the basis of a variable and a function environment
   let rec CS vEnv fEnv = function
       | PrintLn e        -> CE vEnv fEnv e @ [PRINTI; INCSP -1]

       | Ass(acc,e)       -> CA vEnv fEnv acc @ CE vEnv fEnv e @ [STI; INCSP -1]

       | Block([],stms)   -> CSs vEnv fEnv stms

       | Return (Some e) ->
            printfn "%A" "inside return"
            printfn "%A" vEnv
            printfn "%A" fEnv
            CE vEnv fEnv e @ [RET 2]


       | Alt (GC gc)      ->
            let labend = newLabel()
            in List.collect (CSgcAlt vEnv fEnv labend) gc @ [STOP; Label labend]

       | Do (GC gc)       ->
            let labstart = newLabel()
            in [Label labstart] @ List.collect (CSgcAlt vEnv fEnv labstart) gc

       | _                -> failwith "CS: this statement is not supported yet"

   and CSgcAlt vEnv fEnv labend (e, stms) =
       let labelNext = newLabel()
       in CE vEnv fEnv e
          @ [IFZERO labelNext]
          @ CSs vEnv fEnv stms
          @ [GOTO labend]
          @ [Label labelNext]

   and CSgcDo vEnv fEnv labstart (e, stms) =
      let labelNext = newLabel()
      in CE vEnv fEnv e
         @ [IFZERO labelNext]
         @ CSs vEnv fEnv stms
         @ [GOTO labstart]
         @ [Label labelNext]

   and CSs vEnv fEnv stms = List.collect (CS vEnv fEnv) stms



(* ------------------------------------------------------------------- *)

(* Build environments for global variables and functions *)

   let makeGlobalEnvs decs =
       let rec addv decs (vEnv : varEnv) (fEnv : funEnv) =
           match decs with
           | []         -> (vEnv, fEnv, [])
           | dec::decr  ->
             match dec with
             | VarDec (typ, var) -> let (vEnv1, code1) = allocate GloVar (typ, var) vEnv
                                    let (vEnv2, fEnv2, code2) = addv decr vEnv1 fEnv
                                    (vEnv2, fEnv2, code1 @ code2)
             // We need to discuss this together. IMO do codegen together /Gustav.
             | FunDec (Some typ, f, xs, body) ->
                   (* The function environment maps function name to label and parameter decs

                      type ParamDecs = (Typ * string) list
                      type funEnv = Map<string, label * Typ option * ParamDecs> *)
                    let tExtract d =
                      match d with
                        | FunDec _ -> failwith "Functions as parameters NOT supported."
                        | VarDec (t,paraName)         -> (t,paraName)
                    let parList = List.map tExtract xs
                    let funcLabel = newLabel()
                    let newFEnv = Map.add f (funcLabel,Some typ,parList) fEnv
                    let code = CS vEnv fEnv body
                    printfn "Old Function Env: %A" fEnv
                    printfn "New Function Env: %A" newFEnv
                    printfn "%A" vEnv
                    addv decr vEnv newFEnv
             | _ ->
             failwith "makeGlobalEnvs: function/procedure declarations not supported yet"
       addv decs (Map.empty, 0) Map.empty

/// CP prog gives the code for a program prog
   let CP (P(decs,stms)) =
       let _ = resetLabels ()
       let ((gvM,_) as gvEnv, fEnv, initCode) = makeGlobalEnvs decs

       let compilefun (typ, f, xs, body) =
            let (l, p,parName) = Map.find f fEnv
            // let (envf, fdepthf) = bindParams p (globalVarEnv, 0)
            [Label l] @ CS gvEnv fEnv body

            //[Label labf] @ code @ [RET (List.length paras-1)]
       let functions =
                List.choose (function
                         | FunDec (Some typ, f, xs, body)
                                    -> Some (compilefun (typ, f, xs, body))
                         | _ -> None) decs

       printfn "fun %A" functions
       initCode @ CSs gvEnv fEnv stms @ [STOP]
       @ List.concat functions
