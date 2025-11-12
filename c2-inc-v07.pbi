
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
; Common constants and structures

; ======================================================================================================
;- Constants
; ======================================================================================================

#INV$             = ~"\""

#C2MAXTOKENS      = 500   ; Legacy, use #C2TOKENCOUNT for actual count   
#C2MAXCONSTANTS   = 8192

#C2FLAG_TYPE      = 30
#C2FLAG_CONST     = 1
#C2FLAG_IDENT     = 2
#C2FLAG_INT       = 4
#C2FLAG_FLOAT     = 8
#C2FLAG_STR       = 16
#C2FLAG_CHG       = 32
#C2FLAG_PARAM     = 64


Enumeration
   #ljUNUSED
   #ljIDENT
   #ljINT
   #ljFLOAT
   #ljSTRING
   #ljIF
   #ljElse   
   #ljWHILE
   #ljJZ
   #ljJMP
   #ljNEGATE
   #ljFLOATNEG
   #ljNOT
   #ljASSIGN  
   
   #ljADD
   #ljSUBTRACT
   #ljMULTIPLY
   #ljDIVIDE
   #ljFLOATADD
   #ljFLOATSUB
   #ljFLOATMUL
   #ljFLOATDIV
   #ljSTRADD
   #ljFTOS         ; Float To String conversion
   #ljITOS         ; Integer To String conversion
   #ljITOF         ; Integer To Float conversion
   #ljFTOI         ; Float To Integer conversion

   #ljOr
   #ljAND
   #ljXOR
   #ljMOD
  
   #ljEQUAL 
   #ljNotEqual
   #ljLESSEQUAL
   #ljGreaterEqual
   #ljGREATER
   #ljLESS
   #ljFLOATEQ
   #ljFLOATNE
   #ljFLOATLE
   #ljFLOATGE
   #ljFLOATGR
   #ljFLOATLESS
   
   #ljMOV
   #ljFetch
   #ljPOP
   #ljPOPS
   #ljPOPF
   #ljPush
   #ljPUSHS
   #ljPUSHF
   #ljStore
   #ljHALT
   
   #ljPrint
   #ljPRTC
   #ljPRTI
   #ljPRTF
   #ljPRTS

   #ljLeftBrace
   #ljRightBrace
   #ljLeftParent
   #ljRightParent
   #ljSemi
   #ljComma   
   #ljfunction
   #ljreturn
   #ljreturnF
   #ljreturnS
   #ljCall
   
   #ljUNKNOWN
   #ljNOOP
   #ljOP
   #ljSEQ
   #ljKeyword

   #ljTERNARY
   #ljQUESTION
   #ljCOLON
   #ljTENIF        ; Ternary IF: Jump if condition false
   #ljTENELSE      ; Ternary ELSE: Jump past false branch
   #ljNOOPIF       ; Marker for ternary fix() positions (removed after FixJMP)

   #ljMOVS
   #ljMOVF
   #ljFETCHS
   #ljFETCHF
   #ljSTORES
   #ljSTOREF

   ;- Local Variable Opcodes (frame-relative access, no flag checks)
   #ljLMOV       ; Move constant/global to local variable
   #ljLMOVS      ; Move string to local variable
   #ljLMOVF      ; Move float to local variable
   #ljLFETCH     ; Fetch local variable to stack
   #ljLFETCHS    ; Fetch local string to stack
   #ljLFETCHF    ; Fetch local float to stack
   #ljLSTORE     ; Store from stack to local variable
   #ljLSTORES    ; Store string from stack to local
   #ljLSTOREF    ; Store float from stack to local

   ;- Built-in Function Opcodes
   #ljBUILTIN_RANDOM      ; random() or random(max) or random(min, max)
   #ljBUILTIN_ABS         ; abs(x) - absolute value
   #ljBUILTIN_MIN         ; min(a, b) - minimum of two values
   #ljBUILTIN_MAX         ; max(a, b) - maximum of two values
   #ljBUILTIN_ASSERT_EQUAL      ; assertEqual(expected, actual) - assert integers are equal
   #ljBUILTIN_ASSERT_FLOAT      ; assertFloatEqual(expected, actual, tolerance) - assert floats are equal within tolerance
   #ljBUILTIN_ASSERT_STRING     ; assertStringEqual(expected, actual) - assert strings are equal

   #ljEOF
EndEnumeration

; Calculate total token count at compile time
#C2TOKENCOUNT = #ljEOF + 1


;- Error Codes
Enumeration C2ErrorCodes
   #C2ERR_INVALID_FILE = -2
   #C2ERR_FILE_OPEN_FAILED = -3

   #C2ERR_EMPTY_CHAR_LITERAL = 2
   #C2ERR_INVALID_ESCAPE_CHAR = 3
   #C2ERR_MULTI_CHAR_LITERAL = 4
   #C2ERR_UNRECOGNIZED_CHAR = 5
   #C2ERR_EOF_IN_STRING = 6
   #C2ERR_EOL_IN_STRING = 7
   #C2ERR_EOL_IN_IDENTIFIER = 8
   #C2ERR_UNKNOWN_SEQUENCE = 9
   #C2ERR_SYNTAX_EXPECTED = 10
   #C2ERR_EXPECTED_PRIMARY = 11
   #C2ERR_EXPECTED_STATEMENT = 12
   #C2ERR_STACK_OVERFLOW = 14
   #C2ERR_FUNCTION_REDECLARED = 15
   #C2ERR_UNDEFINED_FUNCTION = 16

   #C2ERR_MEMORY_ALLOCATION = 17
   #C2ERR_CODEGEN_FAILED = 18
EndEnumeration

;- Structures
Structure stType
   code.l
   i.l
   j.l
   n.l
   flags.b     ; Instruction flags (bit 0: in ternary expression)
EndStructure

; Instruction flags
#INST_FLAG_TERNARY = 1

; Runtime value arrays - separated by type for maximum VM performance
Structure stVarMeta  ; Compile-time metadata and constant values
   name.s
   flags.w
   paramOffset.i        ; For PARAM variables: offset from callerSp (0=first param, 1=second, etc)
   typeSpecificIndex.i  ; For local variables: index within type-specific local array (0-based)
   ; Constant values (set at compile time, copied to gVar at VM init)
   valueInt.i           ; Integer constant value
   valueFloat.d         ; Float constant value
   valueString.s        ; String constant value
EndStructure

Structure stATR
   s.s
   strtoken.w
   flttoken.w
EndStructure

Structure stBuiltinDef
   name.s          ; Function name as it appears in source code
   opcode.i        ; Opcode for this built-in
   minParams.i     ; Minimum parameter count
   maxParams.i     ; Maximum parameter count (-1 = unlimited)
   returnType.i    ; Return type: #C2FLAG_INT, #C2FLAG_FLOAT, or #C2FLAG_STR
EndStructure

;- Globals

Global Dim           gszATR.stATR(#C2TOKENCOUNT)
Global Dim           gVarMeta.stVarMeta(#C2MAXCONSTANTS)  ; Compile-time info only
;Global Dim           gVarInt.i(#C2MAXCONSTANTS)           ; Runtime integer values
;Global Dim           gVarFloat.d(#C2MAXCONSTANTS)         ; Runtime float values
;Global Dim           gVarString.s(#C2MAXCONSTANTS)        ; Runtime string values
Global Dim           arCode.stType(1)
Global NewMap        mapPragmas.s()

Global               gnLastVariable.i
Global               gnTotalTokens.i

;- Macros
Macro          _ASMLineHelper1(view, uvar)
   CompilerIf view
      If gVarMeta( uvar )\flags & #C2FLAG_INT
         temp = " (" + Str( gVarMeta( uvar )\valueInt ) + ")"
      ElseIf gVarMeta( uvar )\flags & #C2FLAG_FLOAT
         temp = " (" + StrF( gVarMeta( uvar )\valueFloat, 3 ) + ")"
      ElseIf gVarMeta( uvar )\flags & #C2FLAG_STR
         temp = " (" + gVarMeta( uvar )\valueString + ")"
      EndIf
   CompilerEndIf
EndMacro

Macro          _ASMLineHelper2(uvar)
   If gVarMeta( uvar )\flags & #C2FLAG_IDENT
      temp = gVarMeta( uvar )\name
   ElseIf gVarMeta( uvar )\flags & #C2FLAG_STR
      temp = gVarMeta( uvar )\valueString
   ElseIf gVarMeta( uvar )\flags & #C2FLAG_FLOAT
      temp = StrD( gVarMeta( uvar )\valueFloat )
   Else
      temp = Str( gVarMeta( uvar )\valueInt )
   EndIf
EndMacro

Macro                   _VarExpand(vr)
   temp = ""
   If gVarMeta( vr )\flags & #C2FLAG_INT :   temp + " INT "   : EndIf
   If gVarMeta( vr )\flags & #C2FLAG_FLOAT : temp + " FLT "   : EndIf
   If gVarMeta( vr )\flags & #C2FLAG_STR   : temp + " STR "   : EndIf
   If gVarMeta( vr )\flags & #C2FLAG_CONST : temp + " CONST " : EndIf
   If gVarMeta( vr )\flags & #C2FLAG_PARAM : temp + " PARAM " : EndIf
   If gVarMeta( vr )\flags & #C2FLAG_IDENT : temp + " VAR"    : EndIf
EndMacro

Macro          ASMLine(obj,show)
   CompilerIf show
      line = RSet( Str( pc ), 7 ) + "  "
   CompilerElse
      line = RSet( Str( ListIndex(obj) ), 7 ) + "  "
   CompilerEndIf
   
   line + LSet( gszATR( obj\code )\s, 10 ) + "  "
   temp = "" : flag = 0
   CompilerIf show
      line + "[" + RSet(Str(sp),5," ") + "] " 
   CompilerEndIf
   If obj\code = #ljJMP Or obj\code = #ljJZ
      CompilerIf show
         line + "  (" +Str(obj\i) + ") " + Str(pc+obj\i)
      CompilerElse
         line + "  (" +Str(obj\i) + ") " + Str(ListIndex(obj)+obj\i)
      CompilerEndIf
   ElseIf obj\code = #ljCall
      CompilerIf show
         line + "  (" +Str(obj\i) + ") " + Str(pc+obj\i) + " [nParams=" + Str(obj\j) + " nLocals=" + Str(obj\n) + "]"
      CompilerElse
         line + "  (" +Str(obj\i) + ") " + Str(ListIndex(obj)+obj\i) + " [nParams=" + Str(obj\j) + " nLocals=" + Str(obj\n) + "]"
      CompilerEndIf
   ElseIf obj\code = #ljMOV
      _ASMLineHelper1( show, obj\j )
      line + "[" + gVarMeta( obj\j )\name + temp + "] --> [" + gVarMeta( obj\i )\name + "]"
      flag + 1
   ElseIf obj\code = #ljSTORE
      _ASMLineHelper1( show, sp - 1 )
      line + "[sp" + temp + "] --> [" + gVarMeta( obj\i )\name + "]"
      flag + 1
   ElseIf obj\code = #ljPUSH Or obj\code = #ljFetch Or obj\code = #ljPUSHS Or obj\code = #ljPUSHF
      flag + 1
      _ASMLineHelper1( show, obj\i )
      If gVarMeta( obj\i )\flags & #C2FLAG_IDENT
         line + "[" + gVarMeta( obj\i )\name + "] --> [sp]"
      ElseIf gVarMeta( obj\i )\flags & #C2FLAG_STR
         line + "[" + gVarMeta( obj\i )\valueString + "] --> [sp]"
      ElseIf gVarMeta( obj\i )\flags & #C2FLAG_INT
         line + "[" + Str(gVarMeta( obj\i )\valueInt) +  "] --> [sp]"
      ElseIf gVarMeta( obj\i )\flags & #C2FLAG_FLOAT
         line + "[" + StrD(gVarMeta( obj\i )\valueFloat,3) +  "] --> [sp]"
      Else
         line + "[" + gVarMeta( obj\i )\name + "] --> [sp]"
      EndIf
   ElseIf obj\code = #ljPOP Or obj\code = #ljPOPS Or obj\code = #ljPOPF
      flag + 1
      _ASMLineHelper1( show, obj\i )
      line + "[sp] --> [" + gVarMeta( obj\i )\name + "]"
   ElseIf obj\code = #ljNEGATE Or obj\code = #ljNOT 
      flag + 1
      CompilerIf show
         _ASMLineHelper2(sp - 1)
         line  + "  op (" + temp + ")"
      CompilerElse
         line  + "  op (sp - 1)"
      CompilerEndIf
      
   ElseIf obj\code <> #ljHALT And obj\code <> #ljreturn And obj\code <> #ljreturnF And obj\code <> #ljreturnS
      CompilerIf show
         _ASMLineHelper2(sp - 2)
         line  + "   (" + temp + ") -- ("
         _ASMLineHelper2(sp - 1)
         line  + temp + ")"
      CompilerElse
         line  + "   (sp - 2) -- (sp - 1)"
      CompilerEndIf
   EndIf
   CompilerIf Not show
      If flag
         _VarExpand( obj\i )
         line + " FLAGS ["+ temp +"]"
      EndIf
   CompilerEndIf
EndMacro

Macro                   vm_ListToArray( ll, ar )
   i = ListSize( ll() )
   ReDim ar( i )
   i = 0
   
   ForEach ll()
      ar( i ) = ll()
      i + 1
   Next
EndMacro
Macro                   CPC()
   arCode(pc)\code
EndMacro
Macro                   _AR()
   arCode(pc)
EndMacro
;- End of file

DataSection
c2tokens:
   Data.s   "UNUSED"
   Data.i   0, 0
   Data.s   "VAR"
   Data.i   0, 0
   Data.s   "INT"
   Data.i   0, 0
   Data.s   "FLT"
   Data.i   0, 0
   Data.s   "STR"
   Data.i   0, 0
   
   Data.s   "IF"
   Data.i   0, 0
   Data.s   "ELSE"   
   Data.i   0, 0
   Data.s   "WHILE"
   Data.i   0, 0
   Data.s   "JZ"
   Data.i   0, 0
   Data.s   "JMP"
   Data.i   0, 0
   Data.s   "NEG"
   Data.i   #ljFLOATNEG, 0
   Data.s   "FLNEG"
   Data.i   #ljFLOATNEG, 0
   Data.s   "NOT"
   Data.i   0, 0
   Data.s   "ASSIGN"
   Data.i   0, 0
   
   Data.s   "ADD"
   Data.i   #ljFLOATADD, #ljSTRADD
   Data.s   "SUB"
   Data.i   #ljFLOATSUB, 0
   Data.s   "MUL"
   Data.i   #ljFLOATMUL, 0
   Data.s   "DIV"
   Data.i   #ljFLOATDIV, 0
   Data.s   "FLADD"
   Data.i   #ljFLOATADD, #ljSTRADD
   Data.s   "FLSUB"
   Data.i   #ljFLOATSUB, 0
   Data.s   "FLMUL"
   Data.i   #ljFLOATMUL, 0
   Data.s   "FLDIV"
   Data.i   #ljFLOATDIV, 0
   Data.s   "STRADD"
   Data.i   #ljFLOATADD, #ljSTRADD
   Data.s   "FTOS"
   Data.i   0, 0
   Data.s   "ITOS"
   Data.i   0, 0
   Data.s   "ITOF"
   Data.i   0, 0
   Data.s   "FTOI"
   Data.i   0, 0

   Data.s   "OR"
   Data.i   0, 0
   Data.s   "AND"
   Data.i   0, 0
   Data.s   "XOR"
   Data.i   0, 0
   Data.s   "MOD"
   Data.i   0, 0
   
   Data.s   "EQ"
   Data.i   #ljFLOATEQ, 0
   Data.s   "NE"
   Data.i   #ljFLOATNE, 0
   Data.s   "LTE"
   Data.i   #ljFLOATLE, 0
   Data.s   "GTE"
   Data.i   #ljFLOATGE, 0
   Data.s   "GT"
   Data.i   #ljFLOATGR, 0
   Data.s   "LT"
   Data.i   #ljFLOATLE, 0
   Data.s   "FLEQ"
   Data.i   0, 0
   Data.s   "FLNE"
   Data.i   0, 0
   Data.s   "FLLTE"
   Data.i   0, 0
   Data.s   "FLGTE"
   Data.i   0, 0
   Data.s   "FLGT"
   Data.i   0, 0
   Data.s   "FLLT"
   Data.i   0, 0
   
   Data.s   "MOV"
   Data.i   #ljMOVF, #ljMOVS
   Data.s   "FETCH"
   Data.i   #ljFETCHF, #ljFETCHS
   Data.s   "POP"
   Data.i   #ljPOPF, #ljPOPS
   Data.s   "POPS"
   Data.i   0, 0
   Data.s   "POPF"
   Data.i   0, 0
   Data.s   "PUSH"
   Data.i   #ljPUSHF, #ljPUSHS
   Data.s   "PUSHS"
   Data.i   0, 0
   Data.s   "PUSHF"
   Data.i   0, 0
   Data.s   "STORE"
   Data.i   #ljSTOREF, #ljSTORES
   Data.s   "HALT"
   Data.i   0, 0
   
   Data.s   "PRINT"
   Data.i   #ljPRTF, #ljPRTS
   Data.s   "PRTC"
   Data.i   #ljPRTF, #ljPRTS
   Data.s   "PRTI"
   Data.i   #ljPRTF, #ljPRTS
   Data.s   "PRTF"
   Data.i   #ljPRTF, #ljPRTS
   Data.s   "PRTS"
   Data.i   #ljPRTF, #ljPRTS

   Data.s   "LeftBrace"
   Data.i   0, 0
   Data.s   "RightBrace"
   Data.i   0, 0
   Data.s   "LeftParent"
   Data.i   0, 0
   Data.s   "RightParent"
   Data.i   0, 0
   Data.s   "SemiColon"
   Data.i   0, 0
   Data.s   "Comma"
   Data.i   0, 0
   Data.s   "function"
   Data.i   0, 0
   Data.s   "RET"
   Data.i   #ljReturnF, #ljReturnS
   Data.s   "RETF"
   Data.i   0, 0
   Data.s   "RETS"
   Data.i   0, 0
   Data.s   "CALL"
   Data.i   0, 0
   
   
   Data.s   "Unknown"
   Data.i   0, 0
   Data.s   "NOOP"
   Data.i   0, 0
   Data.s   "OP"
   Data.i   0, 0
   Data.s   "SEQ"
   Data.i   0, 0
   Data.s   "Keyword"
   Data.i   0, 0

   Data.s   "TERNARY"
   Data.i   0, 0
   Data.s   "QUESTION"
   Data.i   0, 0
   Data.s   "COLON"
   Data.i   0, 0
   Data.s   "TENIF"
   Data.i   0, 0
   Data.s   "TENELSE"
   Data.i   0, 0
   Data.s   "NOOPIF"
   Data.i   0, 0

   Data.s   "MOVS"
   Data.i   0, 0
   Data.s   "MOVF"
   Data.i   0, 0
   Data.s   "FETCHS"
   Data.i   0, 0
   Data.s   "FETCHF"
   Data.i   0, 0
   Data.s   "STORES"
   Data.i   0, 0
   Data.s   "STOREF"
   Data.i   0, 0

   ; Local variable opcodes (frame-relative, no flag checks)
   Data.s   "LMOV"
   Data.i   #ljLMOVF, #ljLMOVS
   Data.s   "LMOVS"
   Data.i   0, 0
   Data.s   "LMOVF"
   Data.i   0, 0
   Data.s   "LFETCH"
   Data.i   #ljLFETCHF, #ljLFETCHS
   Data.s   "LFETCHS"
   Data.i   0, 0
   Data.s   "LFETCHF"
   Data.i   0, 0
   Data.s   "LSTORE"
   Data.i   #ljLSTOREF, #ljLSTORES
   Data.s   "LSTORES"
   Data.i   0, 0
   Data.s   "LSTOREF"
   Data.i   0, 0

   ; Built-in functions
   Data.s   "RANDOM"
   Data.i   0, 0
   Data.s   "ABS"
   Data.i   0, 0
   Data.s   "MIN"
   Data.i   0, 0
   Data.s   "MAX"
   Data.i   0, 0
   Data.s   "ASSERT_EQ"
   Data.i   0, 0
   Data.s   "ASSERT_FLT"
   Data.i   0, 0
   Data.s   "ASSERT_STR"
   Data.i   0, 0

   Data.s   "EOF"
   Data.i   0, 0
   Data.s   "-"
EndDataSection

; IDE Options = PureBasic 6.21 (Windows - x64)
; CursorPosition = 222
; FirstLine = 207
; Folding = --
; Optimizer
; EnableAsm
; EnableThread
; EnableXP
; SharedUCRT
; CPU = 1
; EnablePurifier
; EnableCompileCount = 23
; EnableBuildCount = 0
; EnableExeConstant