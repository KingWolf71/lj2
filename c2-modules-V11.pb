; -- lexical parser to VM for a simplified C Language 
; Tested in UTF8
; PBx64 v6.20
;
; Based on  https://rosettacode.org/wiki/Compiler/lexical_analyzer
; And
; https://rosettacode.org/wiki/Compiler/syntax_analyzer
; Distribute and use freely
; 
; Kingwolf71 May/2025
; 
;
; Common Structures

; ======================================================================================================
;- Constants
; ======================================================================================================

DisableDebugger

DeclareModule C2Common

   #DEBUG = 1
   XIncludeFile         "c2-inc-v07.pbi"
EndDeclareModule

Module C2Common
   ;Empty by design
   
EndModule

DeclareModule C2Lang
   EnableExplicit
   #WithEOL = 1   
   #C2PROFILER = 0
   UseModule C2Common

   Global               gExit
   Global               gszlastError.s
 
   Structure stTree
      NodeType.i
      TypeHint.i
      value.s
      paramCount.i     ; For function calls - actual parameter count
      *left.stTree
      *right.stTree
   EndStructure
   
   Declare.s            Error( *error.Integer )
   Declare              Compile()
   Declare              ListCode( gadget = 0 )
   Declare              LoadLJ( file.s )
EndDeclareModule

XIncludeFile            "c2-vm-V06.pb"

Module C2Lang
   EnableExplicit
   
; ======================================================================================================
;- Structures
; ======================================================================================================

   #C2REG_FLOATS        = 1
   #C2FUNCSTART         = 2
   #MAX_RECURSESTACK    = 150
      
   Structure stSymbols
      name.s
      TokenType.i
   EndStructure
   
   Structure stToken
      name.s
      TokenType.l
      TokenExtra.l
      value.s
      row.l
      col.l
      function.l
      typeHint.w          ; Type suffix: 0=none, #ljFLOAT, #ljSTRING
   EndStructure
   
   Structure stPrec
      bRightAssociation.i
      bBinary.i
      bUnary.i
      Precedence.i
      NodeType.i
   EndStructure
   
   Structure stModInfo
      *code.stTree
      function.l
      params.s
      nParams.i
      nLocals.i       ; Number of local variables (non-parameters) in function
      Index.l
      row.l
      nCall.u
      *NewPos
      bTypesLocked.i  ; Flag: Types locked on first call
      returnType.w    ; Return type flags (INT/FLOAT/STR)
      List paramTypes.w()  ; Parameter type flags (INT/FLOAT/STR) in order
   EndStructure
   
   Structure stMacro
      name.s
      body.s
      List llParams.s()
   EndStructure
   
   Structure stHole
      mode.l
      id.l
      *src
      *location
   EndStructure

; ======================================================================================================
;- Functions
; ======================================================================================================
   
Declare                 FetchVarOffset(text.s, *assignmentTree.stTree = 0, syntheticType.i = 0)
Declare.w               GetExprResultType( *x.stTree, depth.i = 0 )   
Declare                 expand_params( op = #ljpop, nModule = -1 )
   
; ======================================================================================================
;- Globals
; ======================================================================================================
  
   Global Dim           gPreTable.stPrec( #C2TOKENCOUNT )
   
   Global NewList       llSymbols.stSymbols()
   Global NewList       llTokenList.stToken()   
   Global NewList       llHoles.stHole()
   Global NewList       llObjects.stType()
   Global NewMap        mapMacros.stMacro()
   Global NewMap        mapModules.stModInfo()
   Global NewMap        mapBuiltins.stBuiltinDef()
   Global NewMap        mapVariableTypes.w()  ; Track variable types during parsing (name → type flags)

   Global               gLineNumber
   Global               gStack
   Global               gCol
   Global               gMemSize
   Global               gPos
   Global               gHoles
   Global               gFileFormat
   Global               gFloats
   Global               gIntegers
   Global               gStrings
   Global               gNextFunction
   Global               gCurrFunction
   Global               gCodeGenFunction
   Global               gCodeGenParamIndex
   Global               gCodeGenLocalIndex      ; Current local variable offset (nParams + local count)
   Global               gCodeGenRecursionDepth
   Global               gCurrentFunctionName.s  ; Current function being compiled (for local variable scoping)
   Global               gLastExpandParamsCount  ; Last actual parameter count from expand_params() for built-ins
   Global               gIsNumberFlag
   Global               gEmitIntCmd.i
   Global               gEmitIntLastOp
   Global               gInTernary.b      ; Flag to disable PUSH/FETCH→MOV optimization inside ternary

   Global               gszFileText.s
   Global               gszOriginalSource.s  ; Original source before comment stripping
   Global Dim           gSourceLines.s(0)     ; Array of source lines for efficient lookup
   Global               gNextChar.s
   Global               gLastError
   Global               gszEOF.s          = Chr( 255 )
   
   CompilerIf #PB_OS_Linux
      Global            gszSep.s          = #LF$
   CompilerElse
      Global            gszSep.s          = #CRLF$
   CompilerEndIf
      
   Global               gszFloating.s = "^[+-]?(\d+(\.\d*)?|\.\d+)([eE][+-]?\d+)?$"
   ;- =====================================
   ;- Add compiler parts
   ;- =====================================
   XIncludeFile         "c2-postprocessor-v01.pbi"
   
   CreateRegularExpression( #C2REG_FLOATS, gszFloating )
   
   Declare              paren_expr()
   ;- =====================================
   ;- Generic Macros
   ;- =====================================
   Macro             TOKEN()
      llTokenList()
   EndMacro
   Macro             Install( symbolname, id  )
      AddElement( llSymbols() )
         llSymbols()\name        = LCase(symbolname)
         llSymbols()\TokenType   = id
   EndMacro

   Macro             InstallBuiltin( funcName, funcOpcode, funcMinP, funcMaxP, funcRetType )
      AddMapElement(mapBuiltins(), LCase(funcName))
      mapBuiltins()\name       = funcName
      mapBuiltins()\opcode     = funcOpcode
      mapBuiltins()\minParams  = funcMinP
      mapBuiltins()\maxParams  = funcMaxP
      mapBuiltins()\returnType = funcRetType
   EndMacro

   Procedure.s          GetSourceLine( lineNum.i )
      ; Use array for O(1) lookup instead of parsing string each time
      If lineNum >= 1 And lineNum <= ArraySize(gSourceLines())
         ProcedureReturn Trim(gSourceLines(lineNum))
      EndIf
      
      ProcedureReturn ""
   EndProcedure
   
   Macro                SetError( text, err )
      If err > 0 And err < 10
         gszlastError = text + " on line " + Str( gLineNumber ) + ", col = " + Str( gCol )
         If GetSourceLine(gLineNumber) <> ""
            gszlastError + #CRLF$ + ">> " + GetSourceLine(gLineNumber)
         EndIf
      ElseIf err >= 10
         gszlastError = text + " on line " + Str(llTokenList()\row) + ", col = " + Str(llTokenList()\col)
         If GetSourceLine(llTokenList()\row) <> ""
            gszlastError + #CRLF$ + ">> " + GetSourceLine(llTokenList()\row)
         EndIf
      Else
         gszlastError = text
      EndIf

      gLastError = err

      ProcedureReturn err
   EndMacro
   
   Macro             NextToken()
      ;Debug "---[ " + #PB_Compiler_Procedure + " ]---"
      ;Debug Str( ListIndex( llTokenList() ) ) + "   " + llTokenList()\name
      NextElement( llTokenList() )
   EndMacro
   ;-
   Procedure.s          Error( *error.Integer )
      Protected         szerror.s
   
      If gLastError
         
         *error\i       = gLastError
         szerror        = gszlastError
         gLastError     = 0
         gszlastError   = ""
   
         ProcedureReturn szerror
      EndIf
      
      *error\i = 0
      ProcedureReturn ""
   EndProcedure
   
   Procedure            Logging( id, Text.s, pos = -1, UseDebug = 1 )
      If UseDebug
         Debug text
      EndIf
   EndProcedure
   ;- =====================================
   ;- Compiler init   
   ;- =====================================
   Macro                par_AddMacro( vname, value )
      AddMapElement( mapMacros(), vname )
         mapMacros()\name  = vname
         mapMacros()\body  = value
   EndMacro
   Macro                par_SetPre2( id, op )
      gPreTable( id )\bRightAssociation   = 0
      gPreTable( id )\bBinary             = 0
      gPreTable( id )\bUnary              = 0
      gPreTable( id )\Precedence          = -1
      gPreTable( id )\NodeType            = op
   EndMacro
   Macro                par_SetPre( id, right, bi, un, prec )
      gPreTable( id )\NodeType            = id
      gPreTable( id )\bRightAssociation   = right
      gPreTable( id )\bBinary             = bi
      gPreTable( id )\bUnary              = un
      gPreTable( id )\Precedence          = prec
   EndMacro
   Macro             par_AddTokenSimple( tkentype )
      AddElement( llTokenList() )
         llTokenList()\TokenType = tkentype
         llTokenList()\TokenExtra= tkentype
         llTokenList()\name      = gszATR( tkentype )\s
         llTokenList()\row       = gLineNumber
         llTokenList()\col       = gCol
         llTokenList()\function  = gCurrFunction
   EndMacro
   
   Macro             par_AddToken( tkentype, tkenextra, text, info )
      AddElement( llTokenList() )
         llTokenList()\TokenType = tkentype
         llTokenList()\TokenExtra= tkenextra
         llTokenList()\row       = gLineNumber
         llTokenList()\col       = gCol
         llTokenList()\function  = gCurrFunction
         
         If text = ""
            If tkentype = #ljSTRING
               gStrings + 1
               llTokenList()\name = "_str" + Str(gStrings)
            ElseIf tkentype = #ljINT
               gIntegers + 1
               llTokenList()\name = "_int" + Str(gIntegers)
            ElseIf tkentype = #ljFLOAT
               gFloats + 1
               llTokenList()\name = "_flt" + Str(gFloats)
            Else
               llTokenList()\name = gszATR( tkenextra )\s
            EndIf
         Else
            llTokenList()\name      = text
         EndIf
         
         llTokenList()\value    = info
   EndMacro
   Macro                par_NextCharacter()
      gNextChar = Mid( gszFileText, gPos, 1 )
      gPos + 1 : gCol + 1
      
      If gNextChar = #LF$
         gCol = 1
         gLineNumber + 1
         
         gNextChar = Mid( gszFileText, gPos, 1 )
         gPos + 1
      EndIf
   EndMacro

   ;-
   ;- Built-in Functions Registration
   ;-

   CompilerIf #True  ; Enable built-in functions
   ; Helper: Check if a function name is a built-in
   Procedure.i IsBuiltinFunction(name.s)
      ProcedureReturn FindMapElement(mapBuiltins(), LCase(name))
   EndProcedure

   ; Helper: Get built-in opcode by name
   Procedure.i GetBuiltinOpcode(name.s)
      If FindMapElement(mapBuiltins(), LCase(name))
         ProcedureReturn mapBuiltins()\opcode
      EndIf
      ProcedureReturn 0
   EndProcedure

   ; Register all built-in functions with the compiler
   Procedure RegisterBuiltins()
      InstallBuiltin( "random",            #ljBUILTIN_RANDOM,       0, 2, #C2FLAG_INT )
      InstallBuiltin( "abs",               #ljBUILTIN_ABS,          1, 1, #C2FLAG_INT )
      InstallBuiltin( "min",               #ljBUILTIN_MIN,          2, 2, #C2FLAG_INT )
      InstallBuiltin( "max",               #ljBUILTIN_MAX,          2, 2, #C2FLAG_INT )
      InstallBuiltin( "assertEqual",       #ljBUILTIN_ASSERT_EQUAL, 2, 2, #C2FLAG_INT )
      InstallBuiltin( "assertFloatEqual",  #ljBUILTIN_ASSERT_FLOAT, 2, 3, #C2FLAG_INT )
      InstallBuiltin( "assertStringEqual", #ljBUILTIN_ASSERT_STRING,2, 2, #C2FLAG_INT )
   EndProcedure
   CompilerEndIf

   ;-
   Procedure            Init()
      Protected         temp.s
      Protected         i, n, m
      Protected         verFile.i, verString.s

      For i = 0 To #C2MAXCONSTANTS
         gVarMeta(i)\name   = ""
         gVarMeta(i)\flags  = 0
         gVarMeta(i)\paramOffset = -1  ; -1 means unassigned
      Next
      
      ;Read tokens
      gnTotalTokens = 0
      Restore c2tokens
      
      Repeat
         Read.s temp
         If temp = "-" : Break : EndIf
         gszATR(gnTotalTokens)\s = temp
         Read m
         Read n
         
         gszATR(gnTotalTokens)\strtoken = n
         gszATR(gnTotalTokens)\flttoken = m
         
         gnTotalTokens + 1
      ForEver

      ; Ensure arrays are large enough for both runtime count and compile-time enum values
      If gnTotalTokens < #C2TOKENCOUNT
         gnTotalTokens = #C2TOKENCOUNT
      EndIf

      ReDim gPreTable.stPrec(gnTotalTokens)
      ReDim gszATR(gnTotalTokens)
   
      par_SetPre2( #ljEOF, -1 )
      par_SetPre( #ljMULTIPLY,     0, 1, 0, 13 )
      par_SetPre( #ljDIVIDE,       0, 1, 0, 13 )
      par_SetPre( #ljMOD,          0, 1, 0, 13 )
      par_SetPre( #ljADD,          0, 1, 0, 12 )
      par_SetPre( #ljSUBTRACT,     0, 1, 0, 12 )
      par_SetPre( #ljNEGATE,       0, 0, 1, 14 )
      par_SetPre( #ljNOT,          0, 0, 1, 14 )
      par_SetPre( #ljLESS,         0, 1, 0, 10 )
      par_SetPre( #ljLESSEQUAL,    0, 1, 0, 10 )
      par_SetPre( #ljGREATER,      0, 1, 0, 10 )
      par_SetPre( #ljGreaterEqual, 0, 1, 0, 10 )
      par_SetPre( #ljEQUAL,        0, 1, 0, 9 )
      par_SetPre( #ljNotEqual,     0, 1, 0, 9 )
      par_SetPre2( #ljASSIGN,      #ljASSIGN )
      par_SetPre( #ljAND,          0, 1, 0, 5 )
      par_SetPre( #ljOr,           0, 1, 0, 4 )
      par_SetPre( #ljQUESTION,     1, 1, 0, 3 )  ; Ternary: right-assoc, binary-like, precedence 3
      par_SetPre2( #ljIF,          #ljIF )
      
      par_SetPre2( #ljElse,        -1 )
      par_SetPre2( #ljWHILE,       #ljWHILE )
      par_SetPre2( #ljPRTS,        -1 )
      par_SetPre2( #ljPRTI,        -1 )
      par_SetPre2( #ljPRTC,        -1 )
      par_SetPre2( #ljLeftParent,  -1 )
      par_SetPre2( #ljLeftBrace,   -1 )
      par_SetPre2( #ljRightParent,  -1 )
      par_SetPre2( #ljRightBrace, -1 )
      par_SetPre2( #ljComma,  -1 )
      par_SetPre2( #ljSemi,  -1 )
      par_SetPre2( #ljfunction,  -1 )
      par_SetPre2( #ljreturn,  -1 )
      par_SetPre2( #ljIDENT,  #ljIDENT )
      par_SetPre2( #ljINT,    #ljINT )
      par_SetPre2( #ljSTRING, #ljSTRING )
      par_SetPre2( #ljFLOAT,    #ljFLOAT )
      
      ; Reset list positions before clearing
      ResetList( llObjects() )
      ClearList( llObjects() )
      ResetList( llTokenList() )
      ClearList( llTokenList() )
      ResetList( llSymbols() )
      ClearList( llSymbols() )
      ResetList( llHoles() )
      ClearList( llHoles() )
      ClearMap( mapPragmas() )
      ClearMap( mapMacros() )
      ClearMap( mapModules() )
      ClearMap( mapVariableTypes() )

      ; Add #LJ2_VERSION from _lj2.ver file
      verFile = ReadFile(#PB_Any, "_lj2.ver")
      If verFile
         verString = ReadString(verFile)
         CloseFile(verFile)
      Else
         verString = "0"  ; Default if file not found
      EndIf
      AddMapElement(mapMacros(), "#LJ2_VERSION")
      mapMacros()\name = "#LJ2_VERSION"
      mapMacros()\body = verString

      ReDim arCode(1)
      ; Clear the code array by putting HALT at position 0
      arCode(0)\code = #ljHALT
      arCode(0)\i = 0
      arCode(0)\j = 0

      gLineNumber             = 1
      gCol                    = 1
      gPos                    = 1
      gStack                  = 0
      gExit                   = 0
      gszlastError            = ""
      gLastError              = 0
      gHoles                  = 0
      gnLastVariable          = 0
      gStrings                = 0
      gFloats                 = 0
      gIntegers               = 0
      gNextFunction           = #C2FUNCSTART
      gCodeGenFunction        = 0
      gCodeGenParamIndex      = -1
      gCodeGenLocalIndex      = 0
      gCodeGenRecursionDepth  = 0
      gCurrentFunctionName    = ""  ; Empty = global scope
      gLastExpandParamsCount  = 0
      gIsNumberFlag           = 0
      gEmitIntCmd             = #LJUnknown
      gEmitIntLastOp          = 0
      gInTernary              = #False

      Install( "else", #ljElse )
      install( "if",    #ljIF )
      install( "print", #ljPRint )
      install( "putc",  #ljPRTC )
      install( "while", #ljWHILE )
      install( "func", #ljfunction )
      install( "return", #ljreturn )
      install( "call", #ljCall )

      ; Register built-in functions (random, abs, min, max, etc.)
      RegisterBuiltins()

      mapPragmas("console") = "on"
      mapPragmas("appname") = "Untitled"
      mapPragmas("consolesize") = "600x420"
      
      par_AddMacro( "#True", "1" )
      par_AddMacro( "#False", "0" )
      par_AddMacro( "#PI", "3.14159265359" )

   EndProcedure
   
   Procedure            LoadLJ( filename.s )
      Protected         f, *mem

      gMemSize = FileSize( filename )
      
      If gMemSize > 0
         f = ReadFile( #PB_Any, filename, #PB_File_NoBuffering )
         gFileFormat = ReadStringFormat( f )    
         
         If Not f
            SetError( "Could not open file", #C2ERR_FILE_OPEN_FAILED )
         EndIf
         
         If gFileFormat <> #PB_Ascii And gFileFormat <> #PB_UTF8 And gFileFormat <> #PB_Unicode
            gFileFormat = #PB_Unicode
         EndIf
         
         *Mem = AllocateMemory( gMemSize + 16 )
         ReadData( f, *Mem, gMemSize )
         CloseFile( f )
         
         CompilerIf( #WithEOL = 1 )
            gszFileText = PeekS( *mem, -1, gFileFormat ) + gszEOF
         CompilerElse
            gszFileText = PeekS( *mem, -1, gFileFormat )
         CompilerEndIf   
            
         gMemSize = Len( gszFileText )
         FreeMemory( *mem )
         ProcedureReturn 0
      EndIf

      SetError( "Invalid file", #C2ERR_INVALID_FILE )
   EndProcedure
   
   
   ;- =====================================
   ;- Preprocessors
   ;- =====================================
   Macro                pre_FindNextWord( tsize, withinv, extra )
      p = Trim( Mid( p, tsize ) )
      
      CompilerIf withinv
         temp = "_abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789" + #INV$ + extra
      CompilerElse
         temp = "_abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789" + extra
      CompilerEndIf  
      
      Repeat
         If Not FindString( temp, Mid( p, j, 1 ), #PB_String_NoCase ) : Break : EndIf   
         j + 1
      ForEver
   EndMacro
   Macro                pre_TrimWhiteSpace( string )
      Trim( Trim( string ), #TAB$ )
   EndMacro
   Macro                par_AddModule( modname, mparams )
      
      If FindMapElement( mapModules(), modname )
         SetError( "Function already declared", #C2ERR_FUNCTION_REDECLARED )
      Else
         AddMapElement( mapModules(), modname )
         mapModules()\function      = gNextFunction
         mapModules()\NewPos        = 0
         mapModules()\params        = mparams
         mapModules()\nParams       = -1
         gNextFunction + 1
      EndIf
   EndMacro
   ;-
   ; We parse for #pragmas and function calls as well as macros
   Procedure            ParseFunctions( line.s, row.i )
      Protected.s       temp, nc, name, p, params, baseName
      Protected         i, j, funcReturnType.w

      ;Debug "Checking functions for line: " + line
      i     = 1 : j = 1
      p     = pre_TrimWhiteSpace( line )

      If FindString( p, "func", #PB_String_NoCase ) = 1
         ;It's probably a function
         i + 4
         pre_FindNextWord( 5, 0, "." )
         name  = Left( p, j - 1 )

         ; Extract return type from function name suffix (.f or .s)
         funcReturnType = #C2FLAG_INT  ; Default to INT
         baseName = name
         
         If Right(name, 2) = ".f" Or Right(name, 2) = ".d" 
            funcReturnType = #C2FLAG_FLOAT
            baseName = Left(name, Len(name) - 2)
         ElseIf Right(name, 2) = ".s"
            funcReturnType = #C2FLAG_STR
            baseName = Left(name, Len(name) - 2)
         EndIf

         temp  = "_" + LCase( baseName )
         p     = Mid( p, j )
         i = Len( p )

         If Mid( p, i, 1 ) = #CR$
            p = Left( p, i - 1 )
         EndIf

         p = pre_TrimWhiteSpace( p )

         If Mid( p, 1, 1) = "("
            ;Debug " - Found function: " + temp + " (" + name + ")"
            ; definetely a function
            par_AddModule( temp, p )
            mapModules()\row = row
            mapModules()\returnType = funcReturnType

            ; Parse parameter types from params string
            Protected paramStr.s = p
            Protected paramType.w, paramIdx.i
            Protected closeParenPos.i

            ; Find the closing parenthesis and extract only what's between ( and )
            paramStr = Trim(paramStr)
            If Left(paramStr, 1) = "("
               closeParenPos = FindString(paramStr, ")", 1)
               If closeParenPos > 0
                  ; Extract substring between ( and )
                  paramStr = Mid(paramStr, 2, closeParenPos - 2)
               Else
                  ; No closing paren found, skip opening paren
                  paramStr = Mid(paramStr, 2)
               EndIf
            EndIf
            paramStr = Trim(paramStr)

            ; Parse each parameter
            If paramStr <> ""
               For paramIdx = 1 To CountString(paramStr, ",") + 1
                  Protected param.s = Trim(StringField(paramStr, paramIdx, ","))
                  paramType = #C2FLAG_INT  ; Default

                  ; Check for type suffix (case-insensitive)
                  Protected paramLower.s = LCase(param)
                  If Right(paramLower, 2) = ".f" Or Right(paramLower, 2) = ".d"
                     paramType = #C2FLAG_FLOAT
                  ElseIf Right(paramLower, 2) = ".s"
                     paramType = #C2FLAG_STR
                  EndIf

                  AddElement(mapModules()\paramTypes())
                  mapModules()\paramTypes() = paramType
               Next
            EndIf
         EndIf
      EndIf
   EndProcedure
   
   Procedure            ParseDefinitions( line.s )
      Protected         bInv, Bracket
      Protected         i, j
      Protected         tmpMod.stModInfo
      Protected.s       temp, nc, name, p, param
      Protected         depth = 0, start = 2
      Protected         mret = #True
      
      i     = 1 : j = 1
      p     = pre_TrimWhiteSpace( line )
      
      ; It has to be at the beginning 
      If FindString( p, "#pragma", #PB_String_NoCase ) = 1
         pre_FindNextWord( 8, 1, "" )
         name  = LCase( Trim( Left( p, j - 1 ), #INV$ ) )
         p = Mid( p, j + 1 )
         i = Len( p )
         
         If Mid( p, i, 1 ) = #CR$
            p = Trim( Left( p, i - 1 ) )
         EndIf
         
         p = pre_TrimWhiteSpace( p )
         param = Trim( p, #INV$ )
         AddMapElement( mapPragmas(), name )
         If param = "" : param = "-" : EndIf
         mapPragmas() = param
         mret         = #False
         ;Debug name + " --> [" + param + "]," + Str(Len( param))
      ; It has to be at the beginning
      ElseIf FindString( p, "#define", #PB_String_NoCase ) = 1
         pre_FindNextWord( 8, 0, "" )
         name  = Left( p, j - 1 )
         temp  = UCase( name )
         
         AddMapElement( mapMacros(), temp )
         p     = Trim( Mid( p, j ) )
          
         If Left( p, 1 ) = "("
            Repeat
               i + 1
               nc = Mid( p, i, 1 )
               If nc = "" : Break : EndIf
               If nc = "(" : depth + 1
               ElseIf nc = ")" : depth - 1
               EndIf
            Until depth < 0

            ; params between positions 2 and i-2
            temp = Trim( Mid( p, 2, i - 2 ) )
            j = 1
                 
            Repeat
               nc = StringField( temp, j, "," )
               If nc = "" : Break : EndIf
               AddElement( mapMacros()\llParams() )
               mapMacros()\llParams() = Trim( nc )
               j + 1
            ForEver
            
            ;mapMacros()\strparams = temp
            p = Trim ( Mid( p, i + 1 ) ) ; remainder after ')'
         EndIf
         
         mapMacros()\name  = name
         mapMacros()\body  = p
         mret              = #False
      EndIf
      
      ProcedureReturn mret
   EndProcedure
   
   Procedure.s          ExpandMacros( line.s )
      Protected.s       output, temp
      Protected.s       ident, expanded
      Protected.i       depth, argStart
      Protected.i       i
      Protected         m.stMacro
      Protected         p = 1
      Protected         lenInput, start
      Protected NewList ll.s()

      lenInput = Len( line )
      output   = ""
      
      If Mid( line, lenInput , 1 ) = #CR$
         lenInput - 1
      EndIf
      
      While p <= lenInput
         ; If identifier start
         If FindString( "#abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ_", Mid( line, p, 1 ), 1 )
            start = p
         
            While p <= lenInput And FindString( "#abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789_", Mid( line, p, 1 ), 1 )
              p + 1
            Wend
            
            ident    = Mid( line, start, p - start )
            temp     = UCase( ident )
      
            ; Macro lookup
            If FindMapElement( mapMacros(), temp )
               m = mapMacros()
               
               ; If function-like, parse args
               If Left( Mid( line, p, 1 ), 1 ) = "(" And ListSize( m\llParams() )
                  ; Consume '('
                  p + 1 : depth = 0
                  argStart = p
                  ClearList( ll() )
      
                  ; Build argument list by counting parentheses
                  While p <= lenInput
                     If Mid( line, p, 1 ) = "(" : depth + 1
                     ElseIf Mid( line, p, 1 ) = ")" 
                        If depth = 0 : Break : EndIf
                        depth - 1
                     ElseIf Mid( line, p, 1 ) = "," And depth = 0
                        ; Split argument
                        AddElement( ll() )
                        ll() = Trim( Mid( line, argStart, p - argStart ) )
                        argStart = p + 1
                     EndIf
                  
                     p + 1
                  Wend
                  
                  ; Last argument
                  AddElement( ll() )
                  ll() = Trim( Mid( line, argStart, p - argStart ) )
                  ; Substitute parameters in body
                  expanded = m\body
                  FirstElement( m\llParams() )
                  
                  ForEach ll()
                     expanded = ReplaceString( expanded, m\llParams(), ll() )
                     NextElement( m\llParams() )
                  Next
                  
                  ; Recursively expand inside
                  expanded = ExpandMacros( expanded )
                  output + expanded
                  p + 1 ; skip ')'
                 Continue
               Else
                  ; Object-like macro or no args
                  output + ExpandMacros( m\body )
                  Continue
               EndIf
            EndIf
      
            ; Not a macro: just copy the identifier
            output + ident
            Continue
         EndIf
      
         ; Otherwise copy single character
         output + Mid( line, p, 1 )
         p + 1
      Wend

         ;Debug output + "<--"
      ProcedureReturn output
   EndProcedure

   ; Strip comments from source while preserving strings
   Procedure.s          StripComments( source.s )
      Protected.s       result, char, nextChar
      Protected.i       i, len, inString, inChar, inLineComment, inBlockComment
      Protected.i       escaped

      result = ""
      len = Len(source)
      i = 1

      While i <= len
         char = Mid(source, i, 1)
         nextChar = ""

         If i < len
            nextChar = Mid(source, i + 1, 1)
         EndIf

         ; Handle escape sequences
         If escaped
            If Not inLineComment And Not inBlockComment
               result + char
            EndIf
            escaped = #False
            i + 1
            Continue
         EndIf

         If char = "\"
            escaped = #True
            If Not inLineComment And Not inBlockComment
               result + char
            EndIf
            i + 1
            Continue
         EndIf

         ; Track string state (ignore comment markers inside strings)
         If Not inLineComment And Not inBlockComment And Not inChar
            If char = #DQUOTE$
               inString = 1 - inString  ; Toggle
               result + char
               i + 1
               Continue
            EndIf
         EndIf

         ; Track character literal state
         If Not inLineComment And Not inBlockComment And Not inString
            If char = "'"
               inChar = 1 - inChar  ; Toggle
               result + char
               i + 1
               Continue
            EndIf
         EndIf

         ; Don't process comment markers if we're inside a string or char
         If inString Or inChar
            result + char
            i + 1
            Continue
         EndIf

         ; Handle end of line comment
         If inLineComment
            If char = #LF$ Or char = #CR$
               inLineComment = #False
               result + char  ; Preserve newline
            Else
               result + " "   ; Replace comment chars with space to preserve column position
            EndIf
            i + 1
            Continue
         EndIf

         ; Handle block comment
         If inBlockComment
            If char = "*" And nextChar = "/"
               inBlockComment = #False
               result + "  "  ; Replace */ with spaces to preserve column position
               i + 2  ; Skip */
               Continue
            EndIf
            ; Replace comment content with space or newline to preserve positioning
            If char = #LF$ Or char = #CR$
               result + char  ; Preserve newlines for line numbering
            Else
               result + " "   ; Replace comment chars with space to preserve column position
            EndIf
            i + 1
            Continue
         EndIf

         ; Check for start of comments (only if not in string/char)
         If char = "/" And nextChar = "/"
            inLineComment = #True
            result + "  "  ; Replace // with spaces to preserve column position
            i + 2  ; Skip //
            Continue
         EndIf

         If char = "/" And nextChar = "*"
            inBlockComment = #True
            result + "  "  ; Replace /* with spaces to preserve column position
            i + 2  ; Skip /*
            Continue
         EndIf

         ; Normal character - add to result
         result + char
         i + 1
      Wend

      ProcedureReturn result
   EndProcedure

   ; Finds and expands macros and functions
   Procedure            Preprocessor()
      Protected         i
      Protected         bFlag
      Protected.s       line
      Protected.s       szNewBody
      Protected.i       sizeBeforeStrip, sizeAfterStrip, sizeAfterMacros

      sizeBeforeStrip = Len(gszFileText)

      ; Preserve original source for error messages
      gszOriginalSource = gszFileText
      
      ; Populate source lines array for efficient line lookup
      Protected lineCount.i, lineIdx.i
      lineCount = CountString(gszOriginalSource, #LF$) + 1
      ReDim gSourceLines(lineCount)
      For lineIdx = 1 To lineCount
         gSourceLines(lineIdx) = StringField(gszOriginalSource, lineIdx, #LF$)
      Next
      
      ; Strip all comments from source before processing
      gszFileText = StripComments(gszFileText)
      sizeAfterStrip = Len(gszFileText)
      szNewBody = ""

      ; First we find and store our macros
      Repeat
         i + 1 : bFlag = #True
         line = StringField( gszFileText, i, gszSep )
         If line = "" : Break : EndIf
         
         If FindString( line, "#define", #PB_String_NoCase ) Or FindString( line, "#pragma", #PB_String_NoCase )
            bFlag = ParseDefinitions( line )
         EndIf
         
         If bFlag = #True
            szNewBody + line + #LF$
         Else
            ;Debug mapMacros()\name + " --> " + mapMacros()\strparams + " --> " + mapMacros()\body
         EndIf
      ForEver
      
      ; Macro Expansion
      i = 0 : gszFileText = ""

      Repeat
         i + 1
         line = StringField( szNewBody, i, #LF$ )
         If line = "" : Break : EndIf

         ;- I don't know why the below line works - but it does
         Line = ExpandMacros( line) + #CRLF$
         ;Line = ExpandMacros( line) + #LF$
         gszFileText + line
      ForEver

      szNewBody = gszFileText
      gszFileText = "" : i = 0

      Repeat
         i + 1
         line = StringField( szNewBody, i, #LF$ )
         If line = "" : Break : EndIf
         gszFileText + line + #LF$ 
         
         If FindString( line, "func", #PB_String_NoCase )
            ParseFunctions( line, i )
         EndIf
      ForEver

      gMemSize = Len( gszFileText )
      sizeAfterMacros = gMemSize

      ; Display preprocessing statistics
      Debug "=== Preprocessor Statistics ==="
      Debug "Size before stripping:  " + Str(sizeBeforeStrip) + " chars"
      Debug "Size after stripping:   " + Str(sizeAfterStrip) + " chars (removed " + Str(sizeBeforeStrip - sizeAfterStrip) + ")"
      Debug "Size after macros:      " + Str(sizeAfterMacros) + " chars"
      Debug "==============================="
   EndProcedure
   ;- =====================================
   ;- Parser 
   ;- =====================================
   ; PureBasic procedure to detect if a string represents an Integer, Float, or neither (String)
   Macro                par_DebugParser()
      Debug "---[ Parser ]--------------"
   
      ForEach llTokenList()
         temp = RSet( Str(llTokenList()\row), 6 ) + "   " + RSet( Str(llTokenList()\col), 6 ) + "   "
         
         If llTokenList()\TokenExtra <> llTokenList()\TokenType
            temp + LSet( gszATR( llTokenList()\TokenType )\s + "_" + llTokenList()\name, 34 ) + llTokenList()\value
         Else
            temp + LSet( llTokenList()\name, 34 ) + llTokenList()\value
         EndIf
         
         If llTokenList()\function >= #C2FUNCSTART
            temp +  RSet( "{mod#" + Str( llTokenList()\function - #C2FUNCSTART + 1 ) + "}", 15 )
         EndIf
         
         Debug temp
      Next
   EndMacro             
   ;-
   Procedure            DetectType( Input.s )
      Protected.s       s = Trim(Input)
      Protected.s       c
      Protected.b       isInteger = #True
      Protected.i       i
      
      If s = ""
         ; Empty string considered as String type
         ProcedureReturn #ljSTRING
      EndIf

      ; Check integer: optional leading + or -, followed by digits only
   
      For i = 1 To Len(s)
         c = Mid( s, i, 1 )
         If i = 1 And ( c = "+" Or c = "-" )
            Continue ; sign is allowed at first position
         ElseIf c >= "0" And c <= "9"
            Continue ; digit is allowed
         Else
            isInteger = #False
            Break
         EndIf
      Next i
      
      If isInteger = #True
         ProcedureReturn #ljINT
      EndIf
   
      ; Check float: optional leading + or -, one decimal point, digits around it
      Protected         dotCount.i = 0
      Protected         digitCount.i = 0
      Protected         hasDigitBeforeDot.b = #False
      Protected         hasDigitAfterDot.b = #False
   
      For i = 1 To Len(s)
         c = Mid( s, i, 1 )
         If c = "."
            dotCount + 1
            If dotCount > 1
               ; more than one decimal point -> not a valid float
               dotCount = -1
               Break
            EndIf
         ElseIf i = 1 And ( c = "+" Or c = "-" )
            Continue ; sign allowed at first position
         ElseIf c >= "0" And c <= "9"
            digitCount + 1
            If dotCount = 0
               hasDigitBeforeDot = #True
            Else
               hasDigitAfterDot = #True
            EndIf
         Else
            ; invalid character for float
            dotCount = -1
            Break
         EndIf
      Next i
   
      If dotCount = 1 And hasDigitBeforeDot And hasDigitAfterDot
         ProcedureReturn #ljFLOAT
      EndIf
   
   ; If not integer or float, treat as string
   ProcedureReturn #ljSTRING
EndProcedure
   
   Procedure            IsNumber( init.i = 0 )
      If init
         gIsNumberFlag = 0
      Else
         If gNextChar >= "0" And gNextChar <= "9"
            ProcedureReturn 1
         ElseIf Not gIsNumberFlag And gNextChar = "."
            gIsNumberFlag + 1
            ProcedureReturn 1
         EndIf
      EndIf

      ProcedureReturn 0
   EndProcedure
   
   Procedure            IsAlpha()
      If ( gNextChar >= "a" And gNextChar <= "z" ) Or (gNextChar >= "A" And gNextChar <= "Z"  ) Or IsNumber()
         ProcedureReturn 1
      EndIf
      
      ProcedureReturn 0
   EndProcedure
   
   Procedure            Follow( expect.s, ifyes.i, ifno.i, *err.Integer )
      par_NextCharacter()
      
      If gNextChar = expect
         par_AddToken( #ljOP, ifyes, "", "" )
      Else
         If ifno = -1
            *err\i = #C2ERR_UNRECOGNIZED_CHAR
            SetError( "Unrecognized character sequence", #C2ERR_UNRECOGNIZED_CHAR )
         Else
            par_AddToken( #ljOP, ifno, "", ""  )
            gPos - 1
         EndIf
      EndIf
      
      ProcedureReturn 0
   EndProcedure
   ; Reads character by character creating tokens used by the syntax checker and code generator
   Procedure            Scanner()
      Protected         err, first, i
      Protected.i       dots, bFloat, e
      Protected.i       braces
      Protected.s       text, temp
      
      gpos           = 1
      gCurrFunction  = 1

      While gPos <= gMemSize
         par_NextCharacter()
         
         Select gNextChar
            Case gszEOF
               par_AddTokenSimple( #ljEOF )
               Break

            Case " ", #CR$, #TAB$, ""
               Continue
            
            Case "{"
               braces + 1
               par_AddTokenSimple( #ljLeftBrace )
               
            Case "}"
               braces - 1
               par_AddTokenSimple( #ljRightBrace )
               If braces = 0 : gCurrFunction = 1 : EndIf
               
            Case "("
               par_AddTokenSimple( #ljLeftParent )
            Case ")"
               par_AddTokenSimple( #ljRightParent )
            Case "+"
               par_AddToken( #ljOP, #ljADD, "", "" )
            Case "-"
               par_AddToken( #ljOP, #ljSUBTRACT, "", "" )
            Case "*"
               par_AddToken( #ljOP, #ljMULTIPLY, "", "" )
            Case "%"    
               par_AddToken( #ljOP, #ljMOD, "", "" )
            Case ";"
               par_AddTokenSimple( #ljSemi )
            Case ","
               par_AddTokenSimple( #ljComma )
            Case "?"
               par_AddTokenSimple( #ljQUESTION )
            Case ":"
               par_AddTokenSimple( #ljCOLON )
            Case "/"
               par_AddToken( #ljOP, #ljDIVIDE, "", "" )
            Case "'"
               par_NextCharacter()


               If gNextChar = "'"
                  SetError( "Empty character literal", #C2ERR_EMPTY_CHAR_LITERAL )
               ElseIf gNextChar = "\"
                  par_NextCharacter()
                  
                  Select gNextChar
                     Case "'"
                        SetError( "Empty escape character literal", #C2ERR_EMPTY_CHAR_LITERAL )
                     Case "n"
                        first = 10
                     Case "r"
                        first = 13
                     Case "\"
                        first = 92
                     Default
                        SetError( "Invalid escape character", #C2ERR_INVALID_ESCAPE_CHAR )
                  EndSelect
               Else
                  first = Asc( gNextChar )
               EndIf
               
               par_NextCharacter()

               If gNextChar <> "'"
                  SetError( "Multi-character literal", #C2ERR_MULTI_CHAR_LITERAL )
               Else
                  par_AddToken( #ljINT, #ljINT, "", Str(first) )
               EndIf
               
            Case "<"
               If Follow( "=", #ljLESSEQUAL, #ljLESS, @err ) : ProcedureReturn err : EndIf
            Case ">"
               If Follow( "=", #ljGreaterEqual, #ljGREATER, @err ) : ProcedureReturn err : EndIf
            Case "!"
               If Follow( "=", #ljNotEqual, #ljNOT, @err ) : ProcedureReturn err : EndIf
            Case "="
               If Follow( "=", #ljEQUAL, #ljASSIGN, @err ) : ProcedureReturn err : EndIf
            Case "&"
               If Follow( "&", #ljAND, -1, @err ) : ProcedureReturn err : EndIf
            Case "|"
               If Follow( "|", #ljOr, -1, @err ) : ProcedureReturn err : EndIf
            Case "%"
               If Follow( "%%", #ljxOr, -1, @err ) : ProcedureReturn err : EndIf
            
            Case #INV$
               par_NextCharacter()

               ; Check for empty string
               If gNextChar = #INV$
                  par_AddToken( #ljSTRING, #ljSTRING, "", "" )
               Else
                  text = gNextChar

                  Repeat
                     par_NextCharacter()

                     If gNextChar = #INV$
                        e = DetectType( text )
                        par_AddToken( e, e, "", text )
                        Break
                     ElseIf gNextChar = #CR$
                        SetError( "EOL in string", #C2ERR_EOL_IN_STRING )
                     Else
                        text + gNextChar
                     EndIf

                  Until gPos >= gMemSize

                  If gPos >= gMemSize
                     SetError( "EOF in string", #C2ERR_EOF_IN_STRING )
                  EndIf
               EndIf
            Default
               ; Handle EOF character explicitly
               If gNextChar = gszEOF Or Asc(gNextChar) = 255
                  par_AddTokenSimple( #ljEOF )
                  Break
               EndIf

               IsNumber( 1 )        ; reset digit flag

               first    = IsNumber()
               text     = ""
               dots     = 0
               bFloat   = 0
               e        = 0
               
               While gPos < gMemSize And ( IsAlpha() Or gNextChar = "_" Or gNextChar = "." )
                  If gNextChar = "." : dots + 1 : EndIf
                  If gNextChar = "e" Or gNextChar = "E" : e + 1 : EndIf
                  If Not IsNumber() : first = 0 : EndIf
                  text + gNextChar
                  par_NextCharacter()
               Wend

               If gPos >= gMemSize
                  SetError( "EOL in identifier '" + text + "'", #C2ERR_EOL_IN_IDENTIFIER )
               EndIf

               If Len( text ) < 1
                  SetError( "Unknown sequence or identifier '" + text + "'", #C2ERR_UNKNOWN_SEQUENCE )
               EndIf
               
               gPos - 1
               i = 0
               
               If (dots Or e) And MatchRegularExpression( #C2REG_FLOATS , text )
                  bFloat = 1
                  ;Debug text + " is a float."
               Else
                  ;Debug text + " Not float."
               EndIf
               
               If bFloat
                  par_AddToken( #ljFLOAT, #ljFLOAT, "", text )
               Else
                  temp = LCase( text )

                  ; Check for type suffix (.f or .s)
                  Protected typeHint.w = 0
                  Protected varName.s = text

                  If Right(temp, 2) = ".f" Or Right(temp, 2) = ".d"
                     typeHint = #ljFLOAT
                     varName = Left(text, Len(text) - 2)
                     temp = LCase(varName)
                  ElseIf Right(temp, 2) = ".s"
                     typeHint = #ljSTRING
                     varName = Left(text, Len(text) - 2)
                     temp = LCase(varName)
                  EndIf

                  ; Check keywords FIRST (before functions) - keywords have priority
                  ForEach llSymbols()
                     i + 1

                     If LCase(llSymbols()\name) = temp
                        ;Debug "SYMBOL: " + temp
                        par_AddToken( llSymbols()\TokenType, llSymbols()\TokenType, "", varName )
                        TOKEN()\typeHint = typeHint
                        i = -1
                        Break
                     EndIf
                  Next

                  If i > 0
                     ; Not a keyword - check if it's a function
                     If FindMapElement( mapModules(), "_" + temp )
                        If mapModules()\row = gLineNumber And TOKEN()\TokenType = #ljFunction
                           gCurrFunction     = mapModules()\function
                           TOKEN()\function  = gCurrFunction
                           TOKEN()\value     = Str( gCurrFunction )
                        Else
                           par_AddToken( #ljCall, #ljCall, "", Str( mapModules()\function ) )
                        EndIf
                     Else
                        ; NOTE: Don't check built-ins here - allows variables to shadow built-in names
                        ; Built-ins will be checked in parser when identifier is followed by '('

                        ; Not a keyword or function - check if it's a number or identifier
                        If first
                           par_AddToken( #ljINT, #ljINT, "", text )
                        Else
                           par_AddToken( #ljIDENT, #ljIDENT, "", varName )
                           TOKEN()\typeHint = typeHint
                        EndIf
                     EndIf
                  EndIf
               EndIf
         EndSelect
      Wend
      
      ProcedureReturn 0
   
   EndProcedure
   Procedure            ReorderTokens()
      Protected NewList llTemp.stToken()
      
      CopyList( llTokenList(), lltemp() )
      ClearList( llTokenList() )
      ; We need to put non function tokens at the top so all functions start after code end
      
      ForEach llTemp()
         If llTemp()\TokenType = #ljEOF
            ;Skip
         ElseIf llTemp()\function < #C2FUNCSTART
            AddElement( llTokenList() )
            llTokenList() = llTemp()
         EndIf
      Next
   
      par_AddTokenSimple( #ljHalt )
   
      ForEach llTemp()
         If llTemp()\function >= #C2FUNCSTART
            AddElement( llTokenList() )
            llTokenList() = llTemp()
         EndIf
      Next
      
      par_AddTokenSimple( #ljEOF )
      par_AddToken( #ljINT,    #ljINT, "10",  "10" )
      par_AddToken( #ljSTRING, #ljSTRING, "NULL", "" )
      par_AddToken( #ljINT,    #ljINT, "-1", "-1" )
      par_AddToken( #ljINT,   #ljINT,   "0", "0" )
   EndProcedure
   ;- =====================================
   ;- Syntax Analyzer
   ;- =====================================  
   Procedure            Expect( function.s, TokenType )
      
      ;Debug "--Expect--"
      ;Debug TOKEN()\name + " --> " + gszATR( TokenType )
      
      If TOKEN()\TokenExtra = TokenType
         NextToken()
         ProcedureReturn 0
      EndIf

      SetError( "Expecting " + gszATR( TokenType )\s + " but found " + gszATR( TOKEN()\TokenExtra )\s + " for " + function, #C2ERR_SYNTAX_EXPECTED )
   
   EndProcedure
   
   Procedure            MakeNode( NodeType, *left.stTree, *right.stTree )
      Protected         *p.stTree

      *p = AllocateStructure( stTree )

      If *p
         ; Set all fields explicitly (don't use ClearStructure with strings!)
         *p\NodeType = NodeType
         *p\TypeHint = 0
         *p\value    = ""  ; This properly initializes the string
         *p\left     = *left
         *p\right    = *right
      EndIf

      ProcedureReturn *p
   EndProcedure
   
   Procedure            Makeleaf( NodeType, value.s )
      Protected         *p.stTree

      *p = AllocateStructure( stTree )

      If *p
         ; Set all fields explicitly (don't use ClearStructure with strings!)
         *p\NodeType = NodeType
         *p\TypeHint = 0
         *p\value    = value  ; This properly handles the string
         *p\left     = 0       ; Explicitly null
         *p\right    = 0       ; Explicitly null
      EndIf

      ProcedureReturn *p
   EndProcedure

   Procedure            expr( var )
      Protected.stTree  *p, *node, *r, *e, *trueExpr, *falseExpr, *branches
      Protected         op, q
      Protected         moduleId.i

      ;Debug "expr>" + RSet(Str(TOKEN()\row),4," ") + RSet(Str(TOKEN()\col),4," ") + "   " + TOKEN()\name + " --> " + gszATR( llTokenList()\TokenType )\s

      ; Set gCurrentFunctionName based on TOKEN()\function for local variable lookups
      If TOKEN()\function >= #C2FUNCSTART
         ; Inside a function - find the function name from mapModules
         ForEach mapModules()
            If mapModules()\function = TOKEN()\function
               gCurrentFunctionName = MapKey(mapModules())
               Break
            EndIf
         Next
      Else
         ; In global scope
         gCurrentFunctionName = ""
      EndIf

      Select TOKEN()\TokenExtra
         Case #ljLeftParent
            *p = paren_expr()
            
         Case #ljSUBTRACT, #ljADD
            op = TOKEN()\TokenExtra
            NextToken()
            *node = expr( gPreTable( #ljNEGATE )\Precedence )
            
            If op = #ljSUBTRACT
               *p = MakeNode( #ljNEGATE, *node, 0 )
            Else
               *p = *Node
            EndIf
            
         Case  #ljNOT
            NextToken()
            *p = MakeNode( #ljNOT, expr( gPreTable( #ljNOT )\Precedence ), 0 )
            
         Case #ljIDENT
            ; Check if this is a built-in function call (identifier followed by '(')
            If FindMapElement(mapBuiltins(), LCase(TOKEN()\value))
               ; Peek at next token to see if it's '('
               ; Save current position in token list
               Protected savedListIndex.i = ListIndex(llTokenList())
               NextToken()

               If TOKEN()\TokenExtra = #ljLeftParent
                  ; It's a built-in function call
                  Protected builtinOpcode.i = mapBuiltins()\opcode
                  *e = expand_params(#ljPush, -1)  ; -1 for built-ins
                  *node = Makeleaf(#ljCall, Str(builtinOpcode))  ; Use #ljCall with opcode as value
                  *node\paramCount = gLastExpandParamsCount
                  *p = MakeNode(#ljSEQ, *e, *node)
               Else
                  ; Not a function call, restore position and treat as variable
                  SelectElement(llTokenList(), savedListIndex)
                  *p = Makeleaf( #ljIDENT, TOKEN()\value )
                  *p\TypeHint = TOKEN()\typeHint
                  NextToken()
               EndIf
            Else
               ; Not a built-in - check if it looks like a function call (identifier followed by '(')
               Protected savedListIndex2.i = ListIndex(llTokenList())
               Protected identName.s = TOKEN()\value
               NextToken()

               If TOKEN()\TokenExtra = #ljLeftParent
                  ; Identifier followed by '(' but not a built-in or defined function - this is an error
                  SetError( "Undefined function '" + identName + "'", #C2ERR_UNDEFINED_FUNCTION )
                  ProcedureReturn 0
               Else
                  ; Regular identifier, restore position
                  SelectElement(llTokenList(), savedListIndex2)
                  *p = Makeleaf( #ljIDENT, TOKEN()\value )
                  *p\TypeHint = TOKEN()\typeHint
                  NextToken()
               EndIf
            EndIf

         Case #ljINT
            *p = Makeleaf( #ljINT, TOKEN()\value )
            *p\TypeHint = TOKEN()\typeHint 
            NextToken()
            
         Case #ljFLOAT
            *p = Makeleaf( #ljFLOAT, TOKEN()\value )
            *p\TypeHint = TOKEN()\typeHint 
            NextToken()
         
         Case #ljSTRING
            *p = Makeleaf( #ljSTRING, TOKEN()\value )
            *p\TypeHint = TOKEN()\typeHint
            NextToken()

         Case #ljCALL
            ; Handle function calls in expressions
            moduleId = Val(TOKEN()\value)
            *node = Makeleaf( #ljCall, TOKEN()\value )
            NextToken()
            *e = expand_params( #ljPush, moduleId )
            *node\paramCount = gLastExpandParamsCount  ; Store actual param count in node
            *p = MakeNode( #ljSEQ, *e, *node )


         Default
            SetError( "Expecting a primary, found " + TOKEN()\name, #C2ERR_EXPECTED_PRIMARY )

      EndSelect
      
      While gPreTable( TOKEN()\TokenExtra )\bBinary And gPreTable( TOKEN()\TokenExtra )\Precedence >= var
         op = TOKEN()\TokenExtra

         ; Special handling for ternary operator
         If op = #ljQUESTION
            NextToken()  ; Skip ?
            *trueExpr = expr( 0 )  ; Parse true expression

            If gLastError
               ProcedureReturn *p
            EndIf

            Expect( "ternary", #ljCOLON )  ; Expect :

            If gLastError
               ProcedureReturn *p
            EndIf

            *falseExpr = expr( gPreTable( op )\Precedence )  ; Parse false expression (right-assoc)

            If gLastError
               ProcedureReturn *p
            EndIf

            ; Create ternary node: left=condition, right=node containing true/false branches
            ; Only create the node if we have valid pointers
            If *trueExpr And *falseExpr And *p
               *branches = MakeNode( #ljCOLON, *trueExpr, *falseExpr )

               ; Validate the created node
               If Not *branches
                  SetError( "Failed to allocate memory for ternary branches", #C2ERR_MEMORY_ALLOCATION )
                  ProcedureReturn *p
               EndIf

               *p = MakeNode( #ljTERNARY, *p, *branches )

               ; Validate the final ternary node
               If Not *p
                  SetError( "Failed to allocate memory for ternary node", #C2ERR_MEMORY_ALLOCATION )
                  ProcedureReturn 0
               EndIf
            EndIf
         Else
            NextToken()

            q = gPreTable( op )\Precedence

            If Not gPreTable( op )\bRightAssociation
               q + 1
            EndIf

            *node = expr( q )
            *p = MakeNode( gPreTable( op )\NodeType, *p, *node )
         EndIf
      Wend
      
      ProcedureReturn *p

   EndProcedure
   
   Procedure            paren_expr()
      Protected         *p.stTree
      
      Expect( "paren_expr", #ljLeftParent )
      *p = expr( 0 )
      Expect( "paren_expr", #ljRightParent )
      ProcedureReturn *p
   EndProcedure
   
   Procedure            expand_params( op = #ljpop, nModule = -1 )

      Protected.stTree  *p, *e, *v, *first, *last, *param
      Protected         nParams
      NewList           llParams.i()  ; FIXED: Store pointers (integers), not structures

      ; IMPORTANT: Initialize all pointers to null (they contain garbage otherwise!)
      *p = 0 : *e = 0 : *v = 0 : *first = 0 : *last = 0 : *param = 0

      Expect( "expand_params", #ljLeftParent )

      ; Build parameter list in correct order
      If TOKEN()\TokenExtra <> #ljRightParent
         Repeat
            *e = expr( 0 )

            If *e
               AddElement( llParams() )
               llParams() = *e  ; Store pointer value as integer
               nParams + 1
            EndIf

            If TOKEN()\TokenExtra = #ljComma
               NextToken()
            Else
               Break
            EndIf
         ForEver
      EndIf

      Expect( "expand_params", #ljRightParent )

      ; Generate code based on operation mode
      If op = #ljPOP

         If LastElement( llParams() )
            Repeat
               *param.stTree = llParams()  ; Get pointer from list

               If *param\value > ""
                  *e = Makeleaf( #ljPOP, *param\value )
                  *e\typeHint = *param\typeHint
               Else
                  *e = Makeleaf( #ljPOP, "?unknown?" )
               EndIf

               If *p
                  *p = MakeNode( #ljSEQ, *p, *e )
               Else
                  *p = *e
               EndIf

            Until Not PreviousElement( llParams() )
         EndIf
      Else
         ; For function calls: PUSH params onto stack (forward order)
         ; Apply type conversions if function signature is known
         Protected expectedType.w, actualType.w, paramIndex.i = 0
         Protected *convertedParam.stTree

         ; Try to find function signature for type checking
         Protected hasParamTypes.b = #False
         Protected targetFuncName.s = ""
         If nModule > -1
            ForEach mapModules()
               If mapModules()\function = nModule
                  targetFuncName = MapKey(mapModules())
                  FirstElement(mapModules()\paramTypes())
                  hasParamTypes = #True
                  Break
               EndIf
            Next
         EndIf

         ForEach llParams()
            *param.stTree = llParams()  ; Get pointer from list
            *convertedParam = *param

            ; Insert type conversion if needed
            If hasParamTypes And SelectElement(mapModules()\paramTypes(), paramIndex)
               expectedType = mapModules()\paramTypes()
               actualType = GetExprResultType(*param)

               ; Insert conversion node if types don't match
               If expectedType <> actualType
                  If (expectedType & #C2FLAG_FLOAT) And (actualType & #C2FLAG_INT)
                     ; INT to FLOAT conversion
                     *convertedParam = MakeNode(#ljITOF, *param, 0)
                  ElseIf (expectedType & #C2FLAG_INT) And (actualType & #C2FLAG_FLOAT)
                     ; FLOAT to INT conversion
                     *convertedParam = MakeNode(#ljFTOI, *param, 0)
                  ElseIf (expectedType & #C2FLAG_STR) And (actualType & #C2FLAG_INT)
                     ; INT to STRING conversion
                     *convertedParam = MakeNode(#ljITOS, *param, 0)
                  ElseIf (expectedType & #C2FLAG_STR) And (actualType & #C2FLAG_FLOAT)
                     ; FLOAT to STRING conversion
                     *convertedParam = MakeNode(#ljFTOS, *param, 0)
                  EndIf
               EndIf
            EndIf

            If *p
               *p = MakeNode( #ljSEQ, *p, *convertedParam )
            Else
               *p = *convertedParam
            EndIf

            paramIndex + 1
         Next
      EndIf

      ; Store parameter count in module info
      If nModule > -1
         ForEach mapModules()
            If mapModules()\function = nModule
               mapModules()\nParams = nParams
               Break
            EndIf
         Next
      EndIf

      ; Store actual parameter count for validation/built-ins
      gLastExpandParamsCount = nParams

      ProcedureReturn *p
   EndProcedure
   
   Procedure            stmt()
      Protected.i       i, n
      Protected.stTree  *p, *v, *e, *r, *s, *s2
      Protected.s       text, param
      Protected         printType.i
      Protected         varIdx.i
      Protected         moduleId.i

      ; CRITICAL: Initialize all pointers to null (they contain garbage otherwise!)
      *p = 0 : *v = 0 : *e = 0 : *r = 0 : *s = 0 : *s2 = 0

      ; Set gCurrentFunctionName based on TOKEN()\function for local variable lookups
      If TOKEN()\function >= #C2FUNCSTART
         ; Inside a function - find the function name from mapModules
         ForEach mapModules()
            If mapModules()\function = TOKEN()\function
               gCurrentFunctionName = MapKey(mapModules())
               Break
            EndIf
         Next
      Else
         ; In global scope
         gCurrentFunctionName = ""
      EndIf

      gStack + 1

      If gStack > #MAX_RECURSESTACK
         NextToken()
         SetError( "Stack overflow", #C2ERR_STACK_OVERFLOW )
      EndIf

      Select TOKEN()\TokenType
         Case #ljIF
            NextToken()
            *e    = paren_expr()
            *s    = stmt()
            *s2   = 0 
            
            If TOKEN()\TokenType = #ljElse
               NextToken()
               *s2 = stmt()
            EndIf
            
            *p = MakeNode( #ljIF, *e, MakeNode( #ljIF, *s, *s2 ) )
         
         Case #ljPRTC
            NextToken()
            *e    = paren_expr()
            *p    = MakeNode( #ljPRTC, *e, 0 )
            expect( "putc", #ljSemi )
         
         Case #ljPrint
            NextToken()            
            expect( "print", #ljLeftParent )

            Repeat
               printType = #ljPRTI

               If TOKEN()\TokenExtra = #ljSTRING
                  *r = Makeleaf( #ljSTRING, TOKEN()\value )
                  printType = #ljPRTS
                  NextToken()
               ElseIf TOKEN()\TokenExtra = #ljFLOAT
                  *r = expr( 0 )
                  printType = #ljPRTF
               Else
                  ; Handle all other expressions (variables, arithmetic, etc.)
                  *r = expr( 0 )
                  ; Determine type from the expression tree
                  Protected exprType.w = GetExprResultType(*r)
                  If exprType & #C2FLAG_FLOAT
                     printType = #ljPRTF
                  ElseIf exprType & #C2FLAG_STR
                     printType = #ljPRTS
                  EndIf
               EndIf

               *e = MakeNode( printType, *r, 0 )
               *p = MakeNode( #ljSEQ, *p, *e )
               
               If TOKEN()\TokenType <> #ljComma
                  Break
               EndIf
               
               expect( "print", #ljComma )
               
            Until TOKEN()\TokenType = #ljEOF            
            
            ; Add LineFeed as end of string
            *r = Makeleaf( #ljINT, "10" )
            *e = MakeNode( #ljPRTC, *r, 0 )
            *p = MakeNode( #ljSEQ, *p, *e )
            
            ;*r = Makeleaf( #ljSTRING, gszNL, llTokenList()\ModID )
            ;*e = MakeNode( #ljPRTS, *r, 0, llTokenList()\ModID )
            ;*p = MakeNode( #ljSEQ, *p, *e, llTokenList()\ModID )
            
            Expect( "Print", #ljRightParent )
            Expect( "Print", #ljSemi )

         Case #ljSemi
            NextToken()
            
         Case #ljIDENT
            ; Check if this is a function call (built-in or user-defined)
            ; Peek ahead to see if next token is '('
            Protected identName.s = TOKEN()\value
            Protected savedListIndex2.i = ListIndex(llTokenList())
            NextToken()

            If TOKEN()\TokenExtra = #ljLeftParent
               ; It's a function call - restore position and parse as expression statement
               SelectElement(llTokenList(), savedListIndex2)
               *e = expr(0)
               *p = *e
               Expect("Statement", #ljSemi)
            Else
               ; It's an assignment statement - restore position and parse normally
               SelectElement(llTokenList(), savedListIndex2)

               *v = Makeleaf( #ljIDENT, TOKEN()\value )
               *v\TypeHint = TOKEN()\typeHint

               ; Track variable type for later lookups in GetExprResultType()
               If *v\TypeHint <> 0
                  Protected varTypeFlags.w = #C2FLAG_INT  ; Default
                  Protected varKey.s = *v\value

                  ; Convert typeHint to type flags
                  If *v\TypeHint = #ljFLOAT
                     varTypeFlags = #C2FLAG_FLOAT
                  ElseIf *v\TypeHint = #ljSTRING
                     varTypeFlags = #C2FLAG_STR
                  EndIf

                  ; Store both global name and mangled name (if in function)
                  AddMapElement(mapVariableTypes(), varKey)
                  mapVariableTypes() = varTypeFlags

                  If gCurrentFunctionName <> ""
                     Protected mangledKey.s = gCurrentFunctionName + "_" + varKey
                     AddMapElement(mapVariableTypes(), mangledKey)
                     mapVariableTypes() = varTypeFlags
                  EndIf
               EndIf

               NextToken()
               Expect( "Assign", #ljASSIGN )

               ; Parse right-hand side using expr() - handles all cases including nested calls
               *e = expr( 0 )

               ; Insert automatic type conversion for assignment if needed
               If *v\TypeHint <> 0 And *e
                  Protected lhsType.w = #C2FLAG_INT
                  Protected rhsType.w = GetExprResultType(*e)

                  ; Convert LHS typeHint to type flags
                  If *v\TypeHint = #ljFLOAT
                     lhsType = #C2FLAG_FLOAT
                  ElseIf *v\TypeHint = #ljSTRING
                     lhsType = #C2FLAG_STR
                  EndIf

                  ; Insert conversion node if types don't match
                  If lhsType <> rhsType
                     If (lhsType & #C2FLAG_FLOAT) And (rhsType & #C2FLAG_INT)
                        ; INT to FLOAT conversion
                        *e = MakeNode(#ljITOF, *e, 0)
                     ElseIf (lhsType & #C2FLAG_INT) And (rhsType & #C2FLAG_FLOAT)
                        ; FLOAT to INT conversion
                        *e = MakeNode(#ljFTOI, *e, 0)
                     ElseIf (lhsType & #C2FLAG_STR) And (rhsType & #C2FLAG_INT)
                        ; INT to STRING conversion
                        *e = MakeNode(#ljITOS, *e, 0)
                     ElseIf (lhsType & #C2FLAG_STR) And (rhsType & #C2FLAG_FLOAT)
                        ; FLOAT to STRING conversion
                        *e = MakeNode(#ljFTOS, *e, 0)
                     EndIf
                  EndIf
               EndIf

               *p = MakeNode( #ljASSIGN, *v, *e )

               Expect( "Assign", #ljSemi )
            EndIf
         Case #ljWHILE
            NextToken()
            *e = paren_expr()
            *s = stmt()
            *p = MakeNode( #ljWHILE, *e, *s )
            
         Case #ljLeftBrace
            Expect( "Left Bracket", #ljLeftBrace )
            
            While TOKEN()\TokenExtra <> #ljRightBrace And TOKEN()\TokenExtra <> #ljEOF
               *p = MakeNode( #ljSEQ, *p, stmt() )
            Wend
            
            Expect( "Left Bracket", #ljRightBrace )
            
         Case #ljEOF
            gExit = 1
            
         Case #ljHalt
            NextToken()
            *p = MakeNode( #ljHalt, *p, 0 )
            
         Case #ljFunction
            *v = Makeleaf( #ljFunction, TOKEN()\value )
            n = Val( TOKEN()\value )
            NextToken() ; : NextToken()
            *e = expand_params( #ljPOP, n )
            *p = MakeNode( #ljSEQ, *v, *e )
         
         Case #ljCALL
            moduleId = Val(TOKEN()\value)
            *v = Makeleaf( #ljCall, TOKEN()\value )
            NextToken()
            *e = expand_params( #ljPush, moduleId )
            *v\paramCount = gLastExpandParamsCount  ; Store actual param count in node
            ; Statement-level calls need to pop unused return value
            *s = Makeleaf( #ljPOP, "?discard?" )
            *p = MakeNode( #ljSEQ, *e, MakeNode( #ljSEQ, *v, *s ) )
            
         Case #ljreturn
            NextToken()

            ; NEW CODE - generate expr, then return:
            If TOKEN()\TokenType = #ljSemi
               ; return with no value - push 0
               *e = Makeleaf( #ljINT, "0" )
               *v = MakeNode( #ljSEQ, *e, Makeleaf( #ljreturn, "0" ) )
               NextToken()
            Else
               ; return with value - evaluate expr, then return
               *e = expr(0)

               ; NOTE: Return type conversion removed from parser
               ; Type conversions should be added by postprocessor when type info is accurate
               ; The VM handles return types via RETF/RETS/RET opcodes

               *v = MakeNode( #ljSEQ, *e, Makeleaf( #ljreturn, "0" ) )
               Expect( "Return", #ljSemi )
            EndIf

            *p = MakeNode( #ljSEQ, *p, *v )


         Default
            SetError( "Expecting beginning of a statement, found " + TOKEN()\name, #C2ERR_EXPECTED_STATEMENT )

      EndSelect
      
      ProcedureReturn *p
   EndProcedure
   
   Procedure            DisplayNode( *p.stTree )
      If *p
         If *p\NodeType = #ljIDENT Or *p\NodeType = #ljINT Or *p\NodeType = #ljSTRING
            Debug LSet( gszATR( *p\NodeType )\s, 30 ) + *p\value
         Else
            Debug LSet( gszATR( *p\NodeType )\s, 30 )
            DisplayNode( *p\left )
            DisplayNode( *p\right )
         EndIf
      Else
         Debug ";"
      EndIf
   EndProcedure
   ;- =====================================
   ;- Code Generator
   ;- =====================================
   Procedure            hole()
      gHoles + 1

      AddElement( llHoles() )
      llHoles()\location   = llObjects()
      llHoles()\mode       = 0
      llHoles()\id         = gHoles
      
      ProcedureReturn gHoles
   EndProcedure
   
   Procedure            fix( id, dst = -1 )
      
      AddElement( llHoles() )
      
      If dst = -1
         llHoles()\mode = 1
         llHoles()\id = id
         llHoles()\location = llObjects()
      Else                                   ; Used by blind JMP
         llHoles()\mode = 3
         llHoles()\location = LastElement( llObjects() )
         llHoles()\src = dst
      EndIf

   EndProcedure
    
   ; Helper: Check if variable should use local opcodes (LocalVars array)
   ; Returns true for both parameters and local variables in functions
   Procedure.b          IsLocalVar(varIndex.i)
      If varIndex < 0 Or varIndex >= gnLastVariable
         ProcedureReturn #False
      EndIf

      ; Parameters use LocalVars array
      If gVarMeta(varIndex)\flags & #C2FLAG_PARAM
         ProcedureReturn #True
      EndIf

      ; Non-parameter locals: check if name is mangled with function name OR synthetic ($temp)
      If gCurrentFunctionName <> ""
         If LCase(Left(gVarMeta(varIndex)\name, Len(gCurrentFunctionName) + 1)) = LCase(gCurrentFunctionName + "_")
            ProcedureReturn #True
         EndIf
         ; Synthetic temporaries (starting with $) are also local when inside a function
         If Left(gVarMeta(varIndex)\name, 1) = "$"
            ProcedureReturn #True
         EndIf
      EndIf

      ProcedureReturn #False
   EndProcedure

   Procedure            EmitInt( op.i, nVar.i = -1 )
      Protected sourceFlags.w, destFlags.w
      Protected isSourceLocal.b, isDestLocal.b
      Protected sourceFlags2.w, destFlags2.w

      If gEmitIntCmd = #ljpush And op = #ljStore
         ; PUSH+STORE optimization
         ; Don't optimize inside ternary expressions - both branches need stack values
         ; Check if PUSH instruction is marked as part of ternary
         Protected inTernary.b = (llObjects()\flags & #INST_FLAG_TERNARY)

         sourceFlags = gVarMeta( llObjects()\i )\flags
         destFlags = gVarMeta( nVar )\flags
         isSourceLocal = IsLocalVar(llObjects()\i)
         isDestLocal = IsLocalVar(nVar)

         ; Only optimize to MOV if BOTH are not local (globals can use MOV)
         ; Or if destination is local (use LMOV)
         If Not inTernary And Not ((sourceFlags & #C2FLAG_PARAM) Or (destFlags & #C2FLAG_PARAM))
            ; Neither is parameter - can optimize to MOV
            If isDestLocal
               ; Destination is local - use LMOV
               ; For LMOV: i = paramOffset (destination), j = source varIndex
               Protected savedSource.i = llObjects()\i  ; Save source BEFORE overwriting
               If sourceFlags & #C2FLAG_STR
                  llObjects()\code = #ljLMOVS
               ElseIf sourceFlags & #C2FLAG_FLOAT
                  llObjects()\code = #ljLMOVF
               Else
                  llObjects()\code = #ljLMOV
               EndIf
               llObjects()\j = savedSource  ; j = source varIndex
               llObjects()\i = gVarMeta(nVar)\paramOffset  ; i = destination paramOffset
            Else
               ; Global destination - use regular MOV
               If sourceFlags & #C2FLAG_STR
                  llObjects()\code = #ljMOVS
                  gVarMeta( nVar )\flags = #C2FLAG_IDENT | #C2FLAG_STR
               ElseIf sourceFlags & #C2FLAG_FLOAT
                  llObjects()\code = #ljMOVF
                  gVarMeta( nVar )\flags = #C2FLAG_IDENT | #C2FLAG_FLOAT
               Else
                  llObjects()\code = #ljMOV
                  gVarMeta( nVar )\flags = #C2FLAG_IDENT | #C2FLAG_INT
               EndIf
               llObjects()\j = llObjects()\i
            EndIf
         Else
            ; One is a parameter - keep as PUSH+STORE but use local version if dest is local
            gEmitIntLastOp = AddElement( llObjects() )
            If isDestLocal
               If destFlags & #C2FLAG_STR
                  llObjects()\code = #ljLSTORES
               ElseIf destFlags & #C2FLAG_FLOAT
                  llObjects()\code = #ljLSTOREF
               Else
                  llObjects()\code = #ljLSTORE
               EndIf

            Else
               llObjects()\code = op
            EndIf
         EndIf
      ElseIf gEmitIntCmd = #ljfetch And op = #ljstore
         ; FETCH+STORE optimization
         ; Don't optimize inside ternary expressions - both branches need stack values
         ; Check if FETCH instruction is marked as part of ternary
         Protected inTernary2.b = (llObjects()\flags & #INST_FLAG_TERNARY)

         sourceFlags2 = gVarMeta( llObjects()\i )\flags
         destFlags2 = gVarMeta( nVar )\flags
         isSourceLocal = IsLocalVar(llObjects()\i)
         isDestLocal = IsLocalVar(nVar)

         If Not inTernary2 And Not ((sourceFlags2 & #C2FLAG_PARAM) Or (destFlags2 & #C2FLAG_PARAM))
            ; Can optimize to MOV or LMOV
            If isDestLocal
               ; Use LMOV for local destination
               If sourceFlags2 & #C2FLAG_STR
                  llObjects()\code = #ljLMOVS
               ElseIf sourceFlags2 & #C2FLAG_FLOAT
                  llObjects()\code = #ljLMOVF
               Else
                  llObjects()\code = #ljLMOV
               EndIf
               Protected savedSrc2.i = llObjects()\i
               llObjects()\i = gVarMeta(nVar)\paramOffset
               llObjects()\j = savedSrc2
            Else
               ; Use regular MOV for global destination
               If sourceFlags2 & #C2FLAG_STR
                  llObjects()\code = #ljMOVS
               ElseIf sourceFlags2 & #C2FLAG_FLOAT
                  llObjects()\code = #ljMOVF
               Else
                  llObjects()\code = #ljMOV
               EndIf
               llObjects()\j = llObjects()\i
            EndIf
         Else
            ; Keep as FETCH+STORE but use local version if appropriate
            gEmitIntLastOp = AddElement( llObjects() )
            If isDestLocal
               If destFlags2 & #C2FLAG_STR
                  llObjects()\code = #ljLSTORES
               ElseIf destFlags2 & #C2FLAG_FLOAT
                  llObjects()\code = #ljLSTOREF
               Else
                  llObjects()\code = #ljLSTORE
               EndIf
            Else
               llObjects()\code = op
            EndIf
         EndIf
      Else
         ; Standard emission - check if we should use local opcode
         gEmitIntLastOp = AddElement( llObjects() )

         If nVar >= 0 And IsLocalVar(nVar)
            ; This is a local variable - convert to local opcode and translate index
            Select op
               Case #ljFetch
                  llObjects()\code = #ljLFETCH
                  llObjects()\i = gVarMeta(nVar)\paramOffset
               Case #ljFETCHS
                  llObjects()\code = #ljLFETCHS
                  llObjects()\i = gVarMeta(nVar)\paramOffset
               Case #ljFETCHF
                  llObjects()\code = #ljLFETCHF
                  llObjects()\i = gVarMeta(nVar)\paramOffset
               Case #ljStore
                  llObjects()\code = #ljLSTORE
                  llObjects()\i = gVarMeta(nVar)\paramOffset
               Case #ljSTORES
                  llObjects()\code = #ljLSTORES
                  llObjects()\i = gVarMeta(nVar)\paramOffset
               Case #ljSTOREF
                  llObjects()\code = #ljLSTOREF
                  llObjects()\i = gVarMeta(nVar)\paramOffset
               Default
                  llObjects()\code = op
            EndSelect
         Else
            llObjects()\code = op
         EndIf
      EndIf

      If nVar > -1
         ; For local opcodes, store paramOffset; for globals, store varIndex
         If IsLocalVar(nVar)
            ; For local variables, store offset (not global variable index)
            llObjects()\i = gVarMeta(nVar)\paramOffset
         Else
            llObjects()\i = nVar
         EndIf
      EndIf

      ; Mark instruction if inside ternary expression
      If gInTernary
         llObjects()\flags = llObjects()\flags | #INST_FLAG_TERNARY
      EndIf

      gEmitIntCmd = llObjects()\code
   EndProcedure
   
   Procedure            FetchVarOffset(text.s, *assignmentTree.stTree = 0, syntheticType.i = 0)
      Protected         i, j
      Protected         temp.s
      Protected         inferredType.w
      Protected         savedIndex
      Protected         tokenFound.i = #False
      Protected         searchName.s
      Protected         mangledName.s

      j = -1

      ; Apply name mangling for local variables inside functions
      ; Synthetic variables (starting with $) and constants are never mangled
      If gCurrentFunctionName <> "" And Left(text, 1) <> "$" And syntheticType = 0
         ; Inside a function - first try to find as local variable (mangled)
         mangledName = gCurrentFunctionName + "_" + text
         searchName = mangledName

         ; Check if mangled (local) version exists
         For i = 0 To gnLastVariable - 1
            If gVarMeta(i)\name = searchName
               ProcedureReturn i  ; Found local variable
            EndIf
         Next

         ; Not found as local - check if global exists
         ; If global exists: use it for READ, but create local for WRITE
         ; If global doesn't exist, create as local

         If gCodeGenParamIndex < 0
            ; Not processing parameters - check if global exists
            If Not *assignmentTree
               ; Reading a variable - use global if it exists
               For i = 0 To gnLastVariable - 1
                  If gVarMeta(i)\name = text
                     ; Found as global - use it for READ
                     ProcedureReturn i
                  EndIf
               Next
            EndIf
            ; Assigning to variable (*assignmentTree <> 0) - always create local
         EndIf

         ; Global not found (or assigning) - create as local
         text = mangledName
      EndIf

      ; Check if variable already exists (with final name after mangling)
      For i = 0 To gnLastVariable - 1
         If gVarMeta(i)\name = text
            ; Variable exists - check if it's a local variable that needs an offset assigned
            If gCurrentFunctionName <> "" And gCodeGenParamIndex < 0 And gCodeGenFunction > 0
               If gVarMeta(i)\paramOffset < 0
                  ; This is a local variable without an offset - assign one
                  If LCase(Left(text, Len(gCurrentFunctionName) + 1)) = LCase(gCurrentFunctionName + "_") Or Left(text, 1) = "$"
                     gVarMeta(i)\paramOffset = gCodeGenLocalIndex
                     gCodeGenLocalIndex + 1

                     ; Update nLocals in mapModules immediately
                     ForEach mapModules()
                        If mapModules()\function = gCodeGenFunction
                           mapModules()\nLocals = gCodeGenLocalIndex - mapModules()\nParams
                           Break
                        EndIf
                     Next
                  EndIf
               EndIf
            EndIf
            ProcedureReturn i
         EndIf
      Next

      ; New variable - find token (unless it's a synthetic $ variable)
      i = -1
      savedIndex = ListIndex(TOKEN())

      ; Don't look up synthetic variables (starting with $) in token list
      If Left(text, 1) <> "$"
         ForEach TOKEN()
            If TOKEN()\value = text
               i = ListIndex( TOKEN() )
               Break
            EndIf
         Next

         If savedIndex >= 0
            SelectElement(TOKEN(), savedIndex)
         EndIf
      EndIf

      gVarMeta(gnLastVariable)\name  = text

      ; Check if this is a synthetic temporary variable (starts with $)
      If Left(text, 1) = "$"
         ; Synthetic variable - determine type from suffix or syntheticType parameter
         If syntheticType & #C2FLAG_FLOAT Or Right(text, 1) = "f"
            ;gVarFloat(gnLastVariable) = 0.0
            gVarMeta(gnLastVariable)\flags = #C2FLAG_IDENT | #C2FLAG_FLOAT
         ElseIf syntheticType & #C2FLAG_STR Or Right(text, 1) = "s"
            ;gVarString(gnLastVariable) = ""
            gVarMeta(gnLastVariable)\flags = #C2FLAG_IDENT | #C2FLAG_STR
         Else
            ;gVarInt(gnLastVariable) = 0
            gVarMeta(gnLastVariable)\flags = #C2FLAG_IDENT | #C2FLAG_INT
         EndIf
      ; Check if this is a synthetic constant (syntheticType passed in)
      ElseIf syntheticType = #ljINT
         gVarMeta(gnLastVariable)\valueInt = Val(text)
         gVarMeta(gnLastVariable)\flags = #C2FLAG_CONST | #C2FLAG_INT
      ElseIf syntheticType = #ljFLOAT
         gVarMeta(gnLastVariable)\valueFloat = ValF(text)
         gVarMeta(gnLastVariable)\flags = #C2FLAG_CONST | #C2FLAG_FLOAT
      ElseIf syntheticType = #ljSTRING
         gVarMeta(gnLastVariable)\valueString = text
         gVarMeta(gnLastVariable)\flags = #C2FLAG_CONST | #C2FLAG_STR
      Else
         ; Set type for constants (literals)
         If TOKEN()\TokenType = #ljINT
            gVarMeta(gnLastVariable)\valueInt = Val(text)
            gVarMeta(gnLastVariable)\flags = #C2FLAG_CONST | #C2FLAG_INT
         ElseIf TOKEN()\TokenType = #ljSTRING
            gVarMeta(gnLastVariable)\valueString = text
            gVarMeta(gnLastVariable)\flags = #C2FLAG_CONST | #C2FLAG_STR
         ElseIf TOKEN()\TokenType = #ljFLOAT
            gVarMeta(gnLastVariable)\valueFloat = ValF(text)
            gVarMeta(gnLastVariable)\flags = #C2FLAG_CONST | #C2FLAG_FLOAT
         ElseIf TOKEN()\TokenType = #ljIDENT
            ; NEW: Check for explicit type hint from suffix (.f or .s)
            If TOKEN()\typeHint = #ljFLOAT
               gVarMeta(gnLastVariable)\flags = #C2FLAG_IDENT | #C2FLAG_FLOAT
            ElseIf TOKEN()\typeHint = #ljSTRING
               gVarMeta(gnLastVariable)\flags = #C2FLAG_IDENT | #C2FLAG_STR
            Else
               ; No suffix - infer from assignment if provided
               inferredType = 0
               If *assignmentTree
                  ; Use the helper function to determine expression result type
                  inferredType = GetExprResultType(*assignmentTree)
               EndIf
   
               ; Default to INT if no inference possible
               If inferredType = 0
                  inferredType = #C2FLAG_INT
               EndIf
   
               gVarMeta(gnLastVariable)\flags = #C2FLAG_IDENT | inferredType
            EndIf
            ;gVarInt(gnLastVariable) = gnLastVariable
   
         Else
            ;Debug ": " + text + " Not found"
            ;ProcedureReturn -1
         EndIf
      EndIf

      ; If we're creating a local variable (inside a function, not a parameter),
      ; assign it an offset and update nLocals count
      ; This includes both mangled variables (funcname_varname) and synthetic variables ($temp)
      If gCurrentFunctionName <> "" And gCodeGenParamIndex < 0 And gCodeGenFunction > 0
         If LCase(Left(text, Len(gCurrentFunctionName) + 1)) = LCase(gCurrentFunctionName + "_") Or Left(text, 1) = "$"
            ; This is a new local variable (mangled name or synthetic temporary)
            gVarMeta(gnLastVariable)\paramOffset = gCodeGenLocalIndex
            gCodeGenLocalIndex + 1

            ; Update nLocals in mapModules immediately
            ForEach mapModules()
               If mapModules()\function = gCodeGenFunction
                  mapModules()\nLocals = gCodeGenLocalIndex - mapModules()\nParams
                  Break
               EndIf
            Next
         EndIf
      EndIf

      gnLastVariable + 1
      ProcedureReturn gnLastVariable - 1
   EndProcedure
  
   ; Helper: Determine the result type of an expression
   Procedure.w          GetExprResultType( *x.stTree, depth.i = 0 )
      Protected         n
      Protected         funcId 
      Protected         leftType.w, rightType.w
      Protected         *funcNode.stTree
      
      ; Prevent infinite recursion / stack overflow
      If depth > 100
         ProcedureReturn #C2FLAG_INT
      EndIf

      ; Check if pointer is valid
      If Not *x
         ProcedureReturn #C2FLAG_INT
      EndIf

      ; Additional safety: check if pointer looks obviously invalid
      ; (very small addresses are typically invalid)
      If *x < 4096  ; First page is typically unmapped
         ProcedureReturn #C2FLAG_INT
      EndIf

      Select *x\NodeType
         ; UNUSED/0 will fall through to default case
         Case #ljUNUSED
            ProcedureReturn #C2FLAG_INT
         Case #ljSTRING
            ProcedureReturn #C2FLAG_STR

         Case #ljFLOAT
            ProcedureReturn #C2FLAG_FLOAT

         Case #ljINT
            ProcedureReturn #C2FLAG_INT

         Case #ljIDENT
            ; Check variable type - search existing variables
            ; Apply name mangling for local variables (same logic as FetchVarOffset)
            Protected searchName.s = *x\value
            If gCurrentFunctionName <> "" And Left(*x\value, 1) <> "$"
               ; Try mangled name first (local variable)
               searchName = gCurrentFunctionName + "_" + *x\value
            EndIf

            For n = 0 To gnLastVariable - 1
               If gVarMeta(n)\name = searchName
                  ; Found the variable - return its type flags
                  ProcedureReturn gVarMeta(n)\flags & #C2FLAG_TYPE
               EndIf
            Next

            ; If mangled name not found and we tried mangling, try global name
            If searchName <> *x\value
               For n = 0 To gnLastVariable - 1
                  If gVarMeta(n)\name = *x\value
                     ; Found the global variable - return its type flags
                     ProcedureReturn gVarMeta(n)\flags & #C2FLAG_TYPE
                  EndIf
               Next
            EndIf

            ; Variable not found in gVarMeta - might be a parameter during parsing
            ; Check current function's parameter types in mapModules
            If gCurrentFunctionName <> ""
               ForEach mapModules()
                  If MapKey(mapModules()) = gCurrentFunctionName
                     ; Parse the parameter string to find this parameter's type
                     Protected paramStr.s = mapModules()\params
                     Protected closeParenPos.i = FindString(paramStr, ")", 1)
                     If closeParenPos > 0
                        paramStr = Mid(paramStr, 2, closeParenPos - 2)
                     Else
                        paramStr = Mid(paramStr, 2)
                     EndIf
                     paramStr = Trim(paramStr)

                     If paramStr <> ""
                        Protected paramIdx.i
                        For paramIdx = 1 To CountString(paramStr, ",") + 1
                           Protected param.s = Trim(StringField(paramStr, paramIdx, ","))
                           ; Extract parameter name (before type suffix)
                           Protected paramName.s = param
                           If FindString(param, ".f", 1, #PB_String_NoCase)
                              paramName = Left(param, FindString(param, ".f", 1, #PB_String_NoCase) - 1)
                           ElseIf FindString(param, ".d", 1, #PB_String_NoCase)
                              paramName = Left(param, FindString(param, ".d", 1, #PB_String_NoCase) - 1)
                           ElseIf FindString(param, ".s", 1, #PB_String_NoCase)
                              paramName = Left(param, FindString(param, ".s", 1, #PB_String_NoCase) - 1)
                           EndIf

                           If LCase(paramName) = LCase(*x\value)
                              ; Found the parameter - return its type from paramTypes list
                              If SelectElement(mapModules()\paramTypes(), paramIdx - 1)
                                 ProcedureReturn mapModules()\paramTypes()
                              EndIf
                           EndIf
                        Next
                     EndIf
                     Break
                  EndIf
               Next
            EndIf

            ; Variable not found in gVarMeta or parameters
            ; Check mapVariableTypes (populated during parsing from typeHints)
            If FindMapElement(mapVariableTypes(), searchName)
               ProcedureReturn mapVariableTypes()
            EndIf

            ; If mangled name not found, try global name
            If searchName <> *x\value And FindMapElement(mapVariableTypes(), *x\value)
               ProcedureReturn mapVariableTypes()
            EndIf

            ; Variable not found anywhere - default to INT
            ProcedureReturn #C2FLAG_INT

         Case #ljAdd, #ljSUBTRACT, #ljMULTIPLY, #ljDIVIDE
            ; Arithmetic operations: result is string if any operand is string,
            ; else float if any operand is float, else int
            leftType = #C2FLAG_INT
            rightType = #C2FLAG_INT

            If *x\left
               leftType = GetExprResultType(*x\left, depth + 1)
            EndIf

            If *x\right
               rightType = GetExprResultType(*x\right, depth + 1)
            EndIf

            If leftType & #C2FLAG_STR Or rightType & #C2FLAG_STR
               ProcedureReturn #C2FLAG_STR
            ElseIf leftType & #C2FLAG_FLOAT Or rightType & #C2FLAG_FLOAT
               ProcedureReturn #C2FLAG_FLOAT
            Else
               ProcedureReturn #C2FLAG_INT
            EndIf

         Case #ljNEGATE
            ; Negation preserves type
            If *x\left
               ProcedureReturn GetExprResultType(*x\left, depth + 1)
            EndIf
            ProcedureReturn #C2FLAG_INT

         ; Type conversion operators - return the target type
         Case #ljITOF
            ProcedureReturn #C2FLAG_FLOAT
         Case #ljFTOI
            ProcedureReturn #C2FLAG_INT
         Case #ljITOS, #ljFTOS
            ProcedureReturn #C2FLAG_STR

         Case #ljTERNARY
            ; Ternary operator: result type is determined by true/false branches
            ; *x\right is a COLON node with true_expr in left, false_expr in right
            If *x\right And *x\right\left And *x\right\right
               leftType = GetExprResultType(*x\right\left, depth + 1)    ; true branch
               rightType = GetExprResultType(*x\right\right, depth + 1)  ; false branch

               ; Result type is string if either branch is string
               If leftType & #C2FLAG_STR Or rightType & #C2FLAG_STR
                  ProcedureReturn #C2FLAG_STR
               ; Result type is float if either branch is float
               ElseIf leftType & #C2FLAG_FLOAT Or rightType & #C2FLAG_FLOAT
                  ProcedureReturn #C2FLAG_FLOAT
               Else
                  ProcedureReturn #C2FLAG_INT
               EndIf
            EndIf
            ; Default to INT if structure is invalid
            ProcedureReturn #C2FLAG_INT

         Case #ljCall, #ljSEQ
            ; Function call or SEQ node containing a call - look up function's return type
            
            ; For SEQ nodes, check if right child is a Call
            If *x\NodeType = #ljSEQ And *x\right And *x\right\NodeType = #ljCall
               *funcNode = *x\right
            ElseIf *x\NodeType = #ljCall
               *funcNode = *x
            Else
               ; SEQ without call - try to infer from left or right
               If *x\left
                  leftType = GetExprResultType(*x\left, depth + 1)
                  If leftType <> #C2FLAG_INT
                     ProcedureReturn leftType
                  EndIf
               EndIf
               If *x\right
                  ProcedureReturn GetExprResultType(*x\right, depth + 1)
               EndIf
               ProcedureReturn #C2FLAG_INT
            EndIf
            
            ; Look up the function's declared return type
            If *funcNode
               funcId = Val(*funcNode\value)
               
               ; Check if it's a built-in function
               If funcId >= #ljBUILTIN_RANDOM
                  ; Built-in functions - look up in mapBuiltins for return type
                  ForEach mapBuiltins()
                     If mapBuiltins()\opcode = funcId
                        ProcedureReturn mapBuiltins()\returnType
                     EndIf
                  Next
                  ; Default for unknown built-ins
                  ProcedureReturn #C2FLAG_INT
               Else
                  ; User-defined function - look up in mapModules
                  ForEach mapModules()
                     If mapModules()\function = funcId
                        ProcedureReturn mapModules()\returnType
                     EndIf
                  Next
               EndIf
            EndIf
            
            ProcedureReturn #C2FLAG_INT

         Default
            ; Comparisons and other operations return INT
            ProcedureReturn #C2FLAG_INT
      EndSelect
   EndProcedure

   ; Helper function to detect if an expression tree contains a function call
   Procedure.b          ContainsFunctionCall(*node.stTree)
      If Not *node
         ProcedureReturn #False
      EndIf

      If *node\NodeType = #ljCall
         ProcedureReturn #True
      EndIf

      ; Recursively check left and right subtrees
      If ContainsFunctionCall(*node\left)
         ProcedureReturn #True
      EndIf

      If ContainsFunctionCall(*node\right)
         ProcedureReturn #True
      EndIf

      ProcedureReturn #False
   EndProcedure

   ; Helper function to collect all variable references in an expression tree
   Procedure            CollectVariables(*node.stTree, List vars.s())
      If Not *node
         ProcedureReturn
      EndIf

      If *node\NodeType = #ljIDENT
         ; Add variable to list if not already there
         Protected found.b = #False
         ForEach vars()
            If vars() = *node\value
               found = #True
               Break
            EndIf
         Next
         If Not found
            AddElement(vars())
            vars() = *node\value
         EndIf
      EndIf

      CollectVariables(*node\left, vars())
      CollectVariables(*node\right, vars())
   EndProcedure

   Procedure            CodeGenerator( *x.stTree, *link.stTree = 0 )
      Protected         p1, p2, n
      Protected         temp.s
      Protected         leftType.w
      Protected         rightType.w
      Protected         opType.w = #C2FLAG_INT
      Protected         negType.w = #C2FLAG_INT
      Protected         returnType.w
      Protected         funcId.i
      Protected         paramCount.i

      ; Reset state on top-level call
      If gCodeGenRecursionDepth = 0
         gCodeGenParamIndex = -1
         gCodeGenFunction = 0
         gCodeGenLocalIndex = 0
         gCurrentFunctionName = ""
      EndIf
      gCodeGenRecursionDepth + 1

      ; If no node, return immediately
      If Not *x
         gCodeGenRecursionDepth - 1
         ProcedureReturn
      EndIf
   
      ;Debug gszATR( *x\NodeType )\s + " --> " + *x\value
      
      Select *x\NodeType
         Case #ljEOF
            gCodeGenRecursionDepth - 1
            ProcedureReturn
         Case #ljPOP
            n = FetchVarOffset(*x\value)

            ; Check if this is a function parameter
            If gCodeGenParamIndex >= 0
               ; This is a function parameter - mark it and don't emit POP
               gVarMeta( n )\flags = gVarMeta( n )\flags | #C2FLAG_PARAM
               gVarMeta( n )\paramOffset = gCodeGenParamIndex

               ; Set type flags
               If *x\typeHint = #ljFLOAT
                  gVarMeta( n )\flags = gVarMeta( n )\flags | #C2FLAG_FLOAT
               ElseIf *x\typeHint = #ljSTRING
                  gVarMeta( n )\flags = gVarMeta( n )\flags | #C2FLAG_STR
               Else
                  gVarMeta( n )\flags = gVarMeta( n )\flags | #C2FLAG_INT
               EndIf

               ; Decrement parameter index (parameters processed in reverse, last to first)
               gCodeGenParamIndex - 1

               ; Note: We DON'T emit POP - parameters stay on stack
            ElseIf gCurrentFunctionName <> ""
               ; Local variable inside a function - assign offset and emit POP
               gVarMeta( n )\paramOffset = gCodeGenLocalIndex
               gCodeGenLocalIndex + 1  ; Increment for next local

               ; Update nLocals in mapModules immediately
               ForEach mapModules()
                  If mapModules()\function = gCodeGenFunction
                     mapModules()\nLocals = gCodeGenLocalIndex - mapModules()\nParams
                     Break
                  EndIf
               Next

               ; Set type flags
               If *x\typeHint = #ljFLOAT
                  EmitInt( #ljPOPF, n )
                  gVarMeta( n )\flags = #C2FLAG_IDENT | #C2FLAG_FLOAT
               ElseIf *x\typeHint = #ljSTRING
                  EmitInt( #ljPOPS, n )
                  gVarMeta( n )\flags = #C2FLAG_IDENT | #C2FLAG_STR
               Else
                  EmitInt( #ljPOP, n )
                  gVarMeta( n )\flags = #C2FLAG_IDENT | #C2FLAG_INT
               EndIf
            Else
               ; Global variable - emit POP as usual
               If *x\typeHint = #ljFLOAT
                  EmitInt( #ljPOPF, n )
                  gVarMeta( n )\flags = #C2FLAG_IDENT | #C2FLAG_FLOAT
               ElseIf *x\typeHint = #ljSTRING
                  EmitInt( #ljPOPS, n )
                  gVarMeta( n )\flags = #C2FLAG_IDENT | #C2FLAG_STR
               Else
                  EmitInt( #ljPOP, n )
                  gVarMeta( n )\flags = #C2FLAG_IDENT | #C2FLAG_INT
               EndIf
            EndIf
         
         Case #ljIDENT
            n = FetchVarOffset(*x\value)
            ; Emit appropriate FETCH variant based on variable type
            If gVarMeta(n)\flags & #C2FLAG_STR
               EmitInt( #ljFETCHS, n )
            ElseIf gVarMeta(n)\flags & #C2FLAG_FLOAT
               EmitInt( #ljFETCHF, n )
            Else
               EmitInt( #ljFetch, n )
            EndIf
            gVarMeta( n )\flags = gVarMeta( n )\flags | #C2FLAG_IDENT
            
         Case #ljINT, #ljFLOAT, #ljSTRING
            n = FetchVarOffset( *x\value, 0, *x\NodeType )
            EmitInt( #ljPush, n )
            
         Case #ljASSIGN
            n = FetchVarOffset( *x\left\value, *x\right )

            ; Apply explicit type hint if provided
            If *x\left\TypeHint = #ljFLOAT And Not (gVarMeta(n)\flags & #C2FLAG_FLOAT)
               gVarMeta(n)\flags = (gVarMeta(n)\flags & ~#C2FLAG_TYPE) | #C2FLAG_FLOAT
            ElseIf *x\left\TypeHint = #ljSTRING And Not (gVarMeta(n)\flags & #C2FLAG_STR)
               gVarMeta(n)\flags = (gVarMeta(n)\flags & ~#C2FLAG_TYPE) | #C2FLAG_STR
            ElseIf Not *x\left\TypeHint
               ; No explicit hint - ensure type inference happened correctly
               rightType = GetExprResultType(*x\right)
               
               If rightType & #C2FLAG_FLOAT And Not (gVarMeta(n)\flags & #C2FLAG_FLOAT)
                  gVarMeta(n)\flags = (gVarMeta(n)\flags & ~#C2FLAG_TYPE) | #C2FLAG_FLOAT
               ElseIf rightType & #C2FLAG_STR And Not (gVarMeta(n)\flags & #C2FLAG_STR)
                  gVarMeta(n)\flags = (gVarMeta(n)\flags & ~#C2FLAG_TYPE) | #C2FLAG_STR
               EndIf
            EndIf

            CodeGenerator( *x\right )

            ; Emit appropriate STORE variant based on variable type
            If gVarMeta(n)\flags & #C2FLAG_STR
               EmitInt( #ljSTORES, n )
            ElseIf gVarMeta(n)\flags & #C2FLAG_FLOAT
               EmitInt( #ljSTOREF, n )
            Else
               EmitInt( #ljSTORE, n )
            EndIf

            ; Type propagation: If assigning a typed value to an untyped var, update the var
            If llObjects()\code <> #ljMOV And llObjects()\code <> #ljMOVS And llObjects()\code <> #ljMOVF And
               llObjects()\code <> #ljLMOV And llObjects()\code <> #ljLMOVS And llObjects()\code <> #ljLMOVF
               ; Keep the variable's declared type (don't change it)
               ; Type checking could be added here later
            EndIf

         Case #ljreturn
            ; Note: The actual return type is determined at the SEQ level
            ; This case handles fallback for direct return processing
            EmitInt( #ljreturn )

         Case #ljIF
            CodeGenerator( *x\left )
            EmitInt( #ljJZ)
            p1 = hole()
            CodeGenerator( *x\right\left )

            If *x\right\right
               EmitInt( #ljJMP)
               p2 = hole()
            EndIf

            fix( p1 )

            If *x\right\right
               CodeGenerator( *x\right\right )
               fix( p2 )
            EndIf

         Case #ljTERNARY
            ; Ternary operator: condition ? true_expr : false_expr
            ; *x\left = condition
            ; *x\right = COLON node with true_expr in left, false_expr in right
            ; Using dedicated TENIF/TENELSE opcodes for cleaner implementation
            If *x\left And *x\right
               gInTernary = #True                ; Disable PUSH/FETCH→MOV optimization

               CodeGenerator( *x\left )          ; Evaluate condition
               EmitInt( #ljTENIF )               ; Ternary IF: Jump if condition false
               p1 = hole()                       ; Remember jump location for false branch

               If *x\right\left
                  CodeGenerator( *x\right\left )    ; Evaluate true expression
               EndIf

               EmitInt( #ljTENELSE )             ; Ternary ELSE: Jump past false branch
               p2 = hole()

               ; Emit NOOPIF marker at false branch start - makes offset calculation trivial
               EmitInt( #ljNOOPIF )
               fix( p1 )                         ; Fix TENIF to NOOPIF marker position

               If *x\right\right
                  CodeGenerator( *x\right\right )   ; Evaluate false expression
               EndIf

               ; Emit NOOPIF marker after false branch - target for TENELSE jump
               EmitInt( #ljNOOPIF )
               fix( p2 )                         ; Fix TENELSE to NOOPIF marker position

               gInTernary = #False               ; Re-enable optimization
            EndIf

         Case #ljWHILE
            p1 = llObjects()
            CodeGenerator( *x\left )
            EmitInt( #ljJZ)
            p2 = Hole()
            CodeGenerator( *x\right )
            EmitInt( #ljJMP)
            fix( gHoles, p1 )
            fix( p2 )
            
         Case #ljSEQ
            ; Check if this is a return statement (SEQ with return as right node)
            If *x\right And *x\right\NodeType = #ljreturn
               ; Evaluate the expression being returned
               CodeGenerator( *x\left )

               ; Use the function's DECLARED return type from mapModules, not the inferred expression type
               ; This ensures functions with .s or .f suffixes always return the correct type
               returnType = #C2FLAG_INT  ; Default

               If gCodeGenFunction > 0
                  ; Find the current function's declared return type
                  ForEach mapModules()
                     If mapModules()\function = gCodeGenFunction
                        returnType = mapModules()\returnType
                        Break
                     EndIf
                  Next
               EndIf

               ; Emit the appropriate return opcode based on DECLARED function return type
               If returnType & #C2FLAG_STR
                  EmitInt( #ljreturnS )
               ElseIf returnType & #C2FLAG_FLOAT
                  EmitInt( #ljreturnF )
               Else
                  EmitInt( #ljreturn )  ; Default to integer return
               EndIf
            Else
               ; Normal SEQ processing
               CodeGenerator( *x\left )
               CodeGenerator( *x\right )

               ; NOTE: Don't reset gCodeGenFunction here!
               ; The AST has nested SEQ nodes, and resetting here happens too early.
               ; Function body may continue in outer SEQ nodes.
               ; Like gCurrentFunctionName, gCodeGenFunction will be overwritten when next function starts.
               ; The nLocals count is updated incrementally in FetchVarOffset as variables are created.
            EndIf
            
         Case #ljFunction
            ForEach mapModules()
               If mapModules()\function = Val( *x\value )
                  ; Store BOTH index and pointer to list element for post-optimization fixup
                  mapModules()\Index = ListIndex( llObjects() ) + 1
                  mapModules()\NewPos = @llObjects()  ; Store pointer to element
                  ; Initialize parameter tracking
                  ; Parameters processed in reverse, so start from (nParams - 1) and decrement
                  gCodeGenParamIndex = mapModules()\nParams - 1
                  ; Local variables start after parameters
                  gCodeGenLocalIndex = mapModules()\nParams
                  ; Set current function name for local variable scoping
                  gCurrentFunctionName = MapKey(mapModules())
                  ; Track current function ID for nLocals counting
                  gCodeGenFunction = mapModules()\function
                  Break
               EndIf
            Next
            
         Case #ljPRTC, #ljPRTI, #ljPRTS, #ljPRTF, #ljprint
            CodeGenerator( *x\left )
            EmitInt( *x\NodeType )

         Case #ljLESS, #ljGREATER, #ljLESSEQUAL, #ljGreaterEqual, #ljEQUAL, #ljNotEqual,
              #ljAdd, #ljSUBTRACT, #ljDIVIDE, #ljMULTIPLY

            leftType    = GetExprResultType(*x\left)
            rightType   = GetExprResultType(*x\right)

            ; With proper stack frames, parameters are stack-local and won't be corrupted
            ; No need for temp variables or special handling
            CodeGenerator( *x\left )
            
            ; For string addition, convert left operand to string if needed
            If *x\NodeType = #ljAdd And (leftType & #C2FLAG_STR Or rightType & #C2FLAG_STR)
               If Not (leftType & #C2FLAG_STR)
                  ; Left is not a string - emit conversion
                  If leftType & #C2FLAG_FLOAT
                     EmitInt( #ljFTOS )
                  Else
                     EmitInt( #ljITOS )
                  EndIf
               EndIf
            ElseIf leftType & #C2FLAG_FLOAT And Not (rightType & #C2FLAG_FLOAT) And rightType & #C2FLAG_INT
               ; Left is float, right will be int - no conversion needed yet (convert right after it's pushed)
            ElseIf leftType & #C2FLAG_INT And rightType & #C2FLAG_FLOAT
               ; Left is int, right will be float - convert left to float now
               EmitInt( #ljITOF )
            EndIf
            
            CodeGenerator( *x\right )

            ; Special handling for ADD with strings - emit type conversions
            If *x\NodeType = #ljAdd And (leftType & #C2FLAG_STR Or rightType & #C2FLAG_STR)
               ; Convert right operand to string if needed
               If Not (rightType & #C2FLAG_STR)
                  If rightType & #C2FLAG_FLOAT
                     EmitInt( #ljFTOS )
                  Else
                     EmitInt( #ljITOS )
                  EndIf
               EndIf
               ; Now both operands are strings - emit STRADD
               EmitInt( #ljSTRADD )
            Else
               ; Standard arithmetic/comparison - determine result type
               opType = #C2FLAG_INT

               If leftType & #C2FLAG_FLOAT Or rightType & #C2FLAG_FLOAT
                  opType = #C2FLAG_FLOAT
                  ; Convert right operand to float if needed
                  If rightType & #C2FLAG_INT And Not (rightType & #C2FLAG_FLOAT)
                     EmitInt( #ljITOF )
                  EndIf
               EndIf

               ; Emit correct opcode
               If opType & #C2FLAG_FLOAT And gszATR(*x\NodeType)\flttoken > 0
                  EmitInt( gszATR(*x\NodeType)\flttoken )
               Else
                  EmitInt( *x\NodeType )
               EndIf
            EndIf

         Case #ljOr, #ljAND, #ljMOD, #ljXOR
            CodeGenerator( *x\left )
            CodeGenerator( *x\right )
            EmitInt( *x\NodeType)

         Case #ljNOT
            CodeGenerator( *x\left )
            EmitInt( *x\NodeType)

         Case #ljNEGATE
            CodeGenerator( *x\left )
            
            If *x\left\NodeType = #ljIDENT
               n = FetchVarOffset(*x\left\value)
               negType = gVarMeta(n)\flags & #C2FLAG_TYPE
            ElseIf *x\left\NodeType = #ljFLOAT
               negType = #C2FLAG_FLOAT
            EndIf

            If negType & #C2FLAG_FLOAT
               EmitInt( #ljFLOATNEG )
            Else
               EmitInt( #ljNEGATE )
            EndIf
            
         Case #ljCall
            funcId = Val( *x\value )
            paramCount = *x\paramCount  ; Get actual param count from tree node

            ; Check if this is a built-in function (opcode >= #ljBUILTIN_RANDOM)
            If funcId >= #ljBUILTIN_RANDOM
               ; Built-in function - emit opcode directly
               EmitInt( funcId )
               llObjects()\j = paramCount
            Else
               ; User-defined function - emit CALL with function ID
               ; Store nParams in j and nLocals in n (no packing)
               Protected nLocals.l
               EmitInt( #ljCall, funcId )

               ; Find nLocals for this function
               ForEach mapModules()
                  If mapModules()\function = funcId
                     nLocals = mapModules()\nLocals
                     Break
                  EndIf
               Next

               ; Store separately: j = nParams, n = nLocals
               llObjects()\j = paramCount
               llObjects()\n = nLocals
            EndIf
            
         Case #ljHalt
            EmitInt( *x\NodeType, 0 )

         ; Type conversion operators (unary - operate on left child)
         Case #ljITOF, #ljFTOI, #ljITOS, #ljFTOS
            CodeGenerator( *x\left )
            EmitInt( *x\NodeType )

         Default
            SetError("Error in CodeGenerator at node " + Str(*x\NodeType) + " " + *x\value + " ---> " + gszATR(*x\NodeType)\s, #C2ERR_CODEGEN_FAILED)

      EndSelect

      gCodeGenRecursionDepth - 1

      ; Reset code generation state when returning from root level
      ; This ensures clean state for next compilation even if Init() isn't called
      If gCodeGenRecursionDepth = 0
         gCodeGenFunction = 0
         gCodeGenParamIndex = -1
         gCodeGenLocalIndex = 0
         gCurrentFunctionName = ""
      EndIf
   EndProcedure
    
   Procedure            FixJMP()
      Protected         i, pos, pair
      Protected         srcPos.i
      Protected         offset.i

      ForEach llHoles()
         If llHoles()\mode = 1
            PushListPosition( llHoles() )
               llHoles()\mode = 2
               pair  = llHoles()\id
               ChangeCurrentElement( llObjects(), llHoles()\location )
               pos   = ListIndex( llObjects() )
               i     = 0
               
               ForEach llHoles()
                  If llHoles()\mode = 0 And llHoles()\id = pair
                     llHoles()\mode = 2
                        ChangeCurrentElement( llObjects(), llHoles()\location )
                        srcPos = ListIndex( llObjects() )                        

                        ; Ternary opcodes jump to NOOPIF markers - use direct offset
                        ; Other opcodes use +1 to account for fix() marker semantics
                        If llObjects()\code = #ljTENIF Or llObjects()\code = #ljTENELSE
                           offset = (pos - srcPos)
                        Else
                           offset = (pos - srcPos) + 1
                        EndIf

                        ; Debug jump offset calculation
                        ;If llObjects()\code = #ljTENIF Or llObjects()\code = #ljTENELSE
                        ;   Debug "FixJMP: " + gszATR(llObjects()\code)\s + " at " + Str(srcPos) + " target " + Str(pos) + " (NOOPIF) offset " + Str(offset)
                        ;ElseIf llObjects()\code = #ljJMP And (llObjects()\flags & #INST_FLAG_TERNARY)
                        ;   Debug "FixJMP: Ternary JMP at " + Str(srcPos) + " target " + Str(pos) + " offset " + Str(offset)
                        ;EndIf
                        llObjects()\i = offset
                     Break
                  EndIf
               Next
            PopListPosition( llHoles() )
         ElseIf llHoles()\mode = 3
            llHoles()\mode = 2
            ChangeCurrentElement( llObjects(), llHoles()\src )
            pos = ListIndex( llObjects() )
            ChangeCurrentElement( llObjects(), llHoles()\location )
            ; Perhaps, keep an eye on this
            llObjects()\i = (pos - ListIndex( llObjects() ) ) + 1
         EndIf
      Next

      ; Recalculate function addresses after PostProcessor optimizations
      ; This is critical because PostProcessor may add/remove/optimize instructions
      ; Strategy: Use stored element pointers to find actual post-optimization positions

      ; Recalculate function indexes using stored element pointers
      ForEach mapModules()
         If mapModules()\NewPos
            ; Use stored pointer to find current position of function entry
            If ChangeCurrentElement(llObjects(), mapModules()\NewPos)
               ; Scan forward from this element to skip any NOOPs added by optimizer
               While ListIndex(llObjects()) < ListSize(llObjects())
                  If llObjects()\code <> #ljNOOP
                     ; Found first non-NOOP instruction - this is the function entry
                     mapModules()\Index = ListIndex(llObjects()) + 1
                     Break
                  EndIf
                  If Not NextElement(llObjects())
                     Break
                  EndIf
               Wend
            EndIf
         EndIf
      Next

      ; Now patch all CALL instructions with correct function addresses AND nLocals count
      ForEach llObjects()
         If llObjects()\code = #ljCall
            ForEach mapModules()
               If mapModules()\function = llObjects()\i
                  llObjects()\i = mapModules()\Index
                  llObjects()\n = mapModules()\nLocals  ; Update nLocals from final count
                  Break
               EndIf
            Next
         EndIf
      Next

      ; Convert all NOOPIF markers to NOOP - they were only needed for offset calculation
      ; We replace instead of delete to preserve positions (deleting would shift all offsets!)
      ForEach llObjects()
         If llObjects()\code = #ljNOOPIF
            llObjects()\code = #ljNOOP
         EndIf
      Next

   EndProcedure
   Procedure            ListCode( gadget = 0 )
      Protected         i
      Protected         flag
      Protected.s       temp, line, FullCode

      Debug ";--"
      Debug ";-- Variables & Constants --"
      Debug ";--"

      For i = 0 To gnLastVariable - 1
         If gVarMeta(i)\flags & #C2FLAG_INT
            temp = "Integer"
         ElseIf gVarMeta(i)\flags & #C2FLAG_FLOAT
            temp = "Float"
         ElseIf gVarMeta(i)\flags & #C2FLAG_STR
            temp = "String"
         ElseIf gVarMeta(i)\flags & #C2FLAG_IDENT
            temp = "Variable"
         EndIf

         If gVarMeta(i)\flags & #C2FLAG_CONST
            temp + " constant"
         EndIf

         Debug RSet(Str(i),6, " ") + "   " + LSet(gVarMeta(i)\name,20," ") + "  (" + temp + ")"
      Next

      Debug ";--"
      Debug ";--     Code Section      --"
      Debug ";--"

      ForEach llObjects()
         ASMLine( llObjects(),0 )
         Debug Line
         FullCode + Line +  #CRLF$
      Next

      Debug ";--"
      Debug ";--     End Program       --"
      Debug ";--"
      SetClipboardText( FullCode )
   EndProcedure
   ;- =====================================
   ;- Compiler
   ;- =====================================
   Procedure         Compile()
      Protected      i
      Protected      err
      Protected      *p.stTree
      Protected      total
      Protected.s    temp

      Init()
      Preprocessor()

      If Scanner()
         Debug "Scanner failed with error: " + gszlastError
         ProcedureReturn 1
      EndIf

      ;par_DebugParser()
      ReorderTokens()
      FirstElement( TOKEN() )
      total = ListSize( TOKEN() ) - 1

      Repeat
         gStack = 0
         *p = MakeNode( #ljSEQ, *p, stmt() )

         If gLastError
            Debug gszlastError
            Debug "AST Error"
            gExit = -1
            Break
         EndIf

      Until ListIndex( TOKEN() ) >= total Or gExit

      If gExit >= 0
         ;- DisplayNode( *p )
         CodeGenerator( *p )

         ; PostProcessor does type fixups AND optimizations
         ; Type fixups are necessary, so we always run it
         ; The pragma controls optimization passes within PostProcessor
         PostProcessor()

         ; IMPORTANT: FixJMP must run AFTER PostProcessor to account for optimizations
         FixJMP()

         vm_ListToArray( llObjects, arCode )

         ; List assembly if requested (check pragma listasm - default OFF)
         If FindMapElement(mapPragmas(), "listasm")
            If LCase(mapPragmas()) = "on" Or mapPragmas() = "1"
               ListCode()
            EndIf
         EndIf

         ; Successful compilation - reset gExit to 0
         gExit = 0
      Else
         Debug "gExit=" + Str(gExit)
      EndIf

      ProcedureReturn gExit
   EndProcedure
EndModule

CompilerIf #PB_Compiler_IsMainFile
   ; -- Module demo
   EnableExplicit   

   Define         err
   Define.s       filename
   
   ;filename = ".\Examples\00 comprehensive test.lj"
   filename = ".\Examples\bug fix.lj"
   ;filename = ".\Examples\temp.lj"
   
   ;filename = OpenFileRequester( "Please choose source", ".\Examples\", "LJ Files|*.lj", 0 )

   If filename > ""
      If C2Lang::LoadLJ( filename )
         Debug "Error: " + C2Lang::Error( @err )
      Else
         C2Lang::Compile()
         C2VM::RunVM()  ; Auto-run after compilation
         
         ;If C2Lang::gExit
         ;   Debug "Failed...."
         ;   Debug "gxit="+str*gExit)
         ;   
         ;   C2Lang::ListCode()
         ;Else
         ;   Debug "Executing..."
         ;   C2VM::RunVM()
         ;EndIf
      EndIf
   EndIf

CompilerEndIf


; IDE Options = PureBasic 6.21 (Windows - x64)
; CursorPosition = 3374
; FirstLine = 3288
; Folding = -----f-----
; Markers = 1068
; Optimizer
; EnableThread
; EnableXP
; CPU = 1
; EnableCompileCount = 680
; EnableBuildCount = 0
; EnableExeConstant
; IncludeVersionInfo