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

// Operator tests
ignore(exec "test_programs/testBooleanOperators.gc")
ignore(exec "test_programs/testModDiv.gc")
ignore(exec "test_programs/Ex1.gc")
ignore(exec "test_programs/Ex2.gc")
ignore(exec "test_programs/Ex3.gc")
ignore(exec "test_programs/Ex4.gc")
ignore(exec "test_programs/Ex5.gc")
ignore(exec "test_programs/Ex6.gc")
ignore(exec "test_programs/Skip.gc")

// Function tests
ignore(exec "test_programs/functionTest.gc")
ignore(exec "test_programs/Ex7.gc")
ignore(exec "test_programs/fact.gc")
ignore(exec "test_programs/factRec.gc")
ignore(exec "test_programs/factCBV.gc")

// Local Declaration tests
// ignore(exec "test_programs/A0.gc")
ignore(exec "test_programs/A1.gc")
// ignore(exec "test_programs/A2.gc")
// ignore(exec "test_programs/A3.gc")

// Array tests
ignore(exec "test_programs/arrayTest.gc")

// Procedure tests
ignore(exec "test_programs/A4.gc")

// Pointer tests
ignore(exec "test_programs/PntrTest0.gc")
ignore(exec "test_programs/PntrTest1.gc")
ignore(exec "test_programs/par1.gc")
// ignore(exec "test_programs/factImpPTyp.gc")
// ignore(exec "test_programs/QuickSortV2.gc")
// ignore(exec "test_programs/par2.gc")
