If you need to update lexer/parser, use the following commands in Project "Properties" -> "Build Events" -> "Prebuild event command line":

fslex "$(ProjectDir)Lexer.fsl"
fsyacc --module Parser "$(ProjectDir)Parser.fsy"

/usr/lib/cli/fsharp/fslex.exe Lexer.fsl -o Lexer.fs

Note: You must revise 4 pathes occurring in Script.fsx
