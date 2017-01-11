// Michael R. Hansen 05-01-2016

// You must revise 4 pathes occurring in this file
// The first three are:
#r @"bin/Debug/FSharp.PowerPack.dll";;
#r @"bin/Debug/Machine.dll";
#r @"bin/Debug/virtualMachine.dll";

#load "AST.fs"
#load "Parser.fs"
#load "Lexer.fs"
#load "TypeCheck.fs"
#load "CodeGen.fs"
#load "CodeGenOpt.fs"
#load "Util.fs"


open GuardedCommands.Util
open GuardedCommands.Frontend.TypeCheck
open GuardedCommands.Frontend.AST
open GuardedCommands.Backend.CodeGeneration

open ParserUtil
open CompilerUtil

open Machine
open VirtualMachine

//ignore(exec "test_programs/testBooleanOperators.gc")
//ignore(exec "test_programs/testModDiv.gc")
//ignore(exec "test_programs/functionTest.gc")

ignore(exec "test_programs/PntrTest0.gc")

//let ex1Tree = parseFromFile "test_programs/functionTest.gc"
//ignore(tcP ex1Tree)
// You must revise this path
//System.IO.Directory.SetCurrentDirectory @"C:\Users\mire\Documents\MRH data\Kurser\02257-16\GuardedCommandsVersion2\GuardedCommands\GuardedCommands";;

// The Ex0.gc example:
(*
let ex0Tree = parseFromFile "test_programs/Ex0.gc";;

let _ = tcP ex0Tree;;

let ex0Code = CP ex0Tree;;

let _ = go ex0Tree;;

let _ = goTrace ex0Tree;;


// Parsing of Ex1.gc

let ex1Tree = parseFromFile "test_programs/Ex1.gc";;

// -- is typechecked as follows:

let _ = tcP ex1Tree;;

// obtain symbolic code:
let ex1Code = CP ex1Tree;;

// -- is executed with trace as follows:
let stack = goTrace ex1Tree;;

// -- is executed as follows (no trace):
let sameStack = go ex1Tree;;
*)
// "All in one" parse from file, type check, compile and run

//let _ = exec "test_programs/Ex1.gc";;

//let _ = exec "test_programs/Ex2.gc";;

// Test of programs covered by the fifth task using optimized compilation (Section 8.2):
//List.iter execOpt ["test_programs/Ex1.gc"; "test_programs/Ex2.gc"];;

// All programs relating to the basic version can be parsed:
//let pts = List.map parseFromFile ["test_programs/Ex1.gc"; "test_programs/Ex2.gc";"test_programs/Ex3.gc"; "test_programs/Ex4.gc"; "test_programs/Ex5.gc"; "test_programs/Ex6.gc"; "test_programs/Skip.gc"];;

// The parse tree for Ex3.gc
//List.nth pts 2;


// Test of programs covered by the first task (Section 3.7):
//List.iter exec ["test_programs/Ex1.gc"; "test_programs/Ex2.gc";"test_programs/Ex3.gc"; "test_programs/Ex4.gc"; "test_programs/Ex5.gc"; "test_programs/Ex6.gc"; "test_programs/Skip.gc"];;

// Test of programs covered by the second task (Section 4.3):
//List.iter exec ["test_programs/Ex7.gc"];;

// Test of programs covered by the fourth task (Section 5.4):
//List.iter exec ["test_programs/A0.gc"; "test_programs/A1.gc"; "test_programs/A2.gc"; "test_programs/A3.gc"];;

// Test of programs covered by the fifth task (Section 6.1):
//List.iter exec ["test_programs/A4.gc"; "test_programs/Swap.gc"; "test_programs/QuickSortV1.gc"];;

// Test of programs covered by the fifth task (Section 7.4):
//List.iter exec ["test_programs/par1.gc"; "test_programs/factImpPTyp.gc"; "test_programs/QuickSortV2.gc"; "test_programs/par2.gc"];;

// Test of programs covered by the fifth task using optimized compilation (Section 8.2):
//List.iter execOpt ["test_programs/par1.gc"; "test_programs/factImpPTyp.gc"; "test_programs/QuickSortV2.gc"; "test_programs/par2.gc"];;
