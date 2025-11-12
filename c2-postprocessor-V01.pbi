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
;  Compiler
;- Fixes code and optimizer
;
Procedure            PostProcessor()
      Protected n.i
      Protected fetchVar.i
      Protected opCode.i
      Protected const1.i, const2.i, const2Idx.i
      Protected result.i
      Protected canFold.i
      Protected mulConst.i
      Protected newConstIdx.i
      Protected strIdx.i, str2Idx.i, newStrIdx.i
      Protected str1.s, str2.s, combinedStr.s
      Protected const1f.d, const2f.d, const2fIdx.i
      Protected resultf.d
      Protected newConstFIdx.i
      Protected varIdx.i

      ; Fix up opcodes based on actual variable types
      ; This handles cases where types weren't known at parse time
      ForEach llObjects()
         Select llObjects()\code
            Case #ljPush
               ; Check if this push should be typed
               n = llObjects()\i
               If n >= 0 And n < gnLastVariable
                  ; Skip parameters - they're generic and handled at runtime
                  If Not (gVarMeta(n)\flags & #C2FLAG_PARAM)
                     If gVarMeta(n)\flags & #C2FLAG_FLOAT
                        llObjects()\code = #ljPUSHF
                     ElseIf gVarMeta(n)\flags & #C2FLAG_STR
                        llObjects()\code = #ljPUSHS
                     EndIf
                  EndIf
               EndIf
               
            Case #ljPRTI
               ; Check if print should use different type
               ; Look back to find what's being printed (previous FETCH/PUSH)
               If PreviousElement(llObjects())
                  If llObjects()\code = #ljFETCHF Or llObjects()\code = #ljPUSHF Or llObjects()\code = #ljLFETCHF
                     ; Typed float fetch/push - change to PRTF
                     NextElement(llObjects())
                     llObjects()\code = #ljPRTF
                     PreviousElement(llObjects())
                  ElseIf llObjects()\code = #ljFETCHS Or llObjects()\code = #ljPUSHS Or llObjects()\code = #ljLFETCHS
                     ; Typed string fetch/push - change to PRTS
                     NextElement(llObjects())
                     llObjects()\code = #ljPRTS
                     PreviousElement(llObjects())
                  ElseIf llObjects()\code = #ljFetch Or llObjects()\code = #ljPush
                     n = llObjects()\i
                     If n >= 0 And n < gnLastVariable
                        If gVarMeta(n)\flags & #C2FLAG_FLOAT
                           NextElement(llObjects())  ; Move back to PRTI
                           llObjects()\code = #ljPRTF
                           PreviousElement(llObjects())  ; Stay positioned
                        ElseIf gVarMeta(n)\flags & #C2FLAG_STR
                           NextElement(llObjects())  ; Move back to PRTI
                           llObjects()\code = #ljPRTS
                           PreviousElement(llObjects())
                        EndIf
                     EndIf
                  ElseIf llObjects()\code = #ljFLOATADD Or llObjects()\code = #ljFLOATSUB Or
                         llObjects()\code = #ljFLOATMUL Or llObjects()\code = #ljFLOATDIV Or
                         llObjects()\code = #ljFLOATNEG
                     ; Float operations always produce float results
                     NextElement(llObjects())  ; Move back to PRTI
                     llObjects()\code = #ljPRTF
                     PreviousElement(llObjects())  ; Stay positioned
                  ; Check if previous operation is a string operation
                  ElseIf llObjects()\code = #ljSTRADD
                     ; String operations always produce string results
                     NextElement(llObjects())  ; Move back to PRTI
                     llObjects()\code = #ljPRTS
                     PreviousElement(llObjects())  ; Stay positioned   
                  EndIf
                  NextElement(llObjects())  ; Return to PRTI position
               EndIf
               
            Case #ljPRTF
               ; Check if float print is actually int/string
               If PreviousElement(llObjects())
                  If llObjects()\code = #ljFetch Or llObjects()\code = #ljPush
                     n = llObjects()\i
                     If n >= 0 And n < gnLastVariable
                        If gVarMeta(n)\flags & #C2FLAG_INT
                           NextElement(llObjects())
                           llObjects()\code = #ljPRTI
                           PreviousElement(llObjects())
                        ElseIf gVarMeta(n)\flags & #C2FLAG_STR
                           NextElement(llObjects())
                           llObjects()\code = #ljPRTS
                           PreviousElement(llObjects())
                        EndIf
                     EndIf
                  EndIf
                  NextElement(llObjects())
               EndIf
               
            Case #ljPRTS
               ; Check if string print is actually int/float
               If PreviousElement(llObjects())
                  If llObjects()\code = #ljFetch Or llObjects()\code = #ljPush
                     n = llObjects()\i
                     If n >= 0 And n < gnLastVariable
                        If gVarMeta(n)\flags & #C2FLAG_INT
                           NextElement(llObjects())
                           llObjects()\code = #ljPRTI
                           PreviousElement(llObjects())
                        ElseIf gVarMeta(n)\flags & #C2FLAG_FLOAT
                           NextElement(llObjects())
                           llObjects()\code = #ljPRTF
                           PreviousElement(llObjects())
                        EndIf
                     EndIf
                  EndIf
                  NextElement(llObjects())
               EndIf
         EndSelect
      Next

      ;- ==================================================================
      ;- Enhanced Instruction Fusion Optimizations (backward compatible)
      ;- ==================================================================

      ; Check if optimizations are enabled (default ON)
      Protected optimizationsEnabled.i = #True
      If FindMapElement(mapPragmas(), "optimizecode")
         If LCase(mapPragmas()) = "off" Or mapPragmas() = "0"
            optimizationsEnabled = #False
         EndIf
      EndIf

      If optimizationsEnabled
      ; Pass 2: Redundant assignment elimination (x = x becomes NOP)
      ForEach llObjects()
         Select llObjects()\code
            Case #ljStore, #ljSTORES, #ljSTOREF
               ; Check if previous instruction fetches/pushes the same variable
               If PreviousElement(llObjects())
                  If (llObjects()\code = #ljFetch Or llObjects()\code = #ljFETCHS Or
                      llObjects()\code = #ljFETCHF Or llObjects()\code = #ljPush Or
                      llObjects()\code = #ljPUSHS Or llObjects()\code = #ljPUSHF)
                     ; Check if it's the same variable
                     fetchVar = llObjects()\i
                     NextElement(llObjects())  ; Back to STORE
                     If llObjects()\i = fetchVar
                        ; Redundant assignment: x = x
                        llObjects()\code = #ljNOOP
                        PreviousElement(llObjects())
                        llObjects()\code = #ljNOOP
                        NextElement(llObjects())
                     Else
                        ; Different variables, restore position
                     EndIf
                  Else
                     NextElement(llObjects())
                  EndIf
               EndIf
         EndSelect
      Next

      ; Pass 3: Dead code elimination (PUSH/FETCH followed immediately by POP)
      ForEach llObjects()
         Select llObjects()\code
            Case #ljPOP, #ljPOPS, #ljPOPF
               ; Check if previous instruction is PUSH/FETCH
               If PreviousElement(llObjects())
                  If (llObjects()\code = #ljFetch Or llObjects()\code = #ljFETCHS Or
                      llObjects()\code = #ljFETCHF Or llObjects()\code = #ljPush Or
                      llObjects()\code = #ljPUSHS Or llObjects()\code = #ljPUSHF)
                     ; Dead code: value pushed then immediately popped
                     llObjects()\code = #ljNOOP
                     NextElement(llObjects())  ; Back to POP
                     llObjects()\code = #ljNOOP
                  Else
                     NextElement(llObjects())
                  EndIf
               EndIf
         EndSelect
      Next

      ; Pass 4: Constant folding for integer arithmetic
      ForEach llObjects()
         Select llObjects()\code
            Case #ljADD, #ljSUBTRACT, #ljMULTIPLY, #ljDIVIDE, #ljMOD
               opCode = llObjects()\code
               ; Look back for two consecutive constant pushes
               If PreviousElement(llObjects())
                  If (llObjects()\code = #ljPush Or llObjects()\code = #ljPUSHF Or llObjects()\code = #ljPUSHS) And (gVarMeta( llObjects()\i )\flags & #C2FLAG_CONST)
                     const2 = gVarMeta( llObjects()\i )\valueInt
                     const2Idx = llObjects()\i
                     If PreviousElement(llObjects())
                        If (llObjects()\code = #ljPush Or llObjects()\code = #ljPUSHF Or llObjects()\code = #ljPUSHS) And (gVarMeta( llObjects()\i )\flags & #C2FLAG_CONST)
                           const1 = gVarMeta( llObjects()\i )\valueInt
                           canFold = #True

                           ; Compute the constant result
                           Select opCode
                              Case #ljADD
                                 result = const1 + const2
                              Case #ljSUBTRACT
                                 result = const1 - const2
                              Case #ljMULTIPLY
                                 result = const1 * const2
                              Case #ljDIVIDE
                                 If const2 <> 0
                                    result = const1 / const2
                                 Else
                                    canFold = #False  ; Don't fold division by zero
                                 EndIf
                              Case #ljMOD
                                 If const2 <> 0
                                    result = const1 % const2
                                 Else
                                    canFold = #False
                                 EndIf
                           EndSelect

                           If canFold
                              ; Create a new constant for the folded result
                              newConstIdx = gnLastVariable
                              ; Clear all fields first
                              gVarMeta(newConstIdx)\name = "$fold" + Str(newConstIdx)
                              gVarMeta(newConstIdx)\valueInt = result
                              gVarMeta(newConstIdx)\valueFloat = 0.0
                              gVarMeta(newConstIdx)\valueString = ""
                              gVarMeta(newConstIdx)\flags = #C2FLAG_CONST | #C2FLAG_INT
                              gVarMeta(newConstIdx)\paramOffset = -1  ; Constants don't need frame offsets
                              gnLastVariable + 1

                              ; Replace first PUSH with new constant, eliminate second PUSH and operation
                              llObjects()\i = newConstIdx
                              NextElement(llObjects())  ; Second PUSH
                              llObjects()\code = #ljNOOP
                              NextElement(llObjects())  ; Operation
                              llObjects()\code = #ljNOOP
                           Else
                              NextElement(llObjects())
                              NextElement(llObjects())
                           EndIf
                        Else
                           NextElement(llObjects())
                           NextElement(llObjects())
                        EndIf
                     Else
                        NextElement(llObjects())
                     EndIf
                  Else
                     NextElement(llObjects())
                  EndIf
               EndIf
         EndSelect
      Next

      ; Pass 4b: Constant folding for float arithmetic
      ForEach llObjects()
         Select llObjects()\code
            Case #ljFLOATADD, #ljFLOATSUB, #ljFLOATMUL, #ljFLOATDIV
               opCode = llObjects()\code
               ; Look back for two consecutive constant pushes
               If PreviousElement(llObjects())
                  If (llObjects()\code = #ljPUSHF Or llObjects()\code = #ljPush) And (gVarMeta( llObjects()\i )\flags & #C2FLAG_CONST)
                     const2fIdx = llObjects()\i
                     If PreviousElement(llObjects())
                        If (llObjects()\code = #ljPUSHF Or llObjects()\code = #ljPush) And (gVarMeta( llObjects()\i )\flags & #C2FLAG_CONST)
                           canFold = #True

                           ; Compute the constant result
                           Select opCode
                              Case #ljFLOATADD
                                 resultf = const1f + const2f
                              Case #ljFLOATSUB
                                 resultf = const1f - const2f
                              Case #ljFLOATMUL
                                 resultf = const1f * const2f
                              Case #ljFLOATDIV
                                 If const2f <> 0.0
                                    resultf = const1f / const2f
                                 Else
                                    canFold = #False  ; Don't fold division by zero
                                 EndIf
                           EndSelect

                           If canFold
                              ; Create a new constant for the folded result
                              newConstFIdx = gnLastVariable
                              ; Clear all fields first
                              gVarMeta(newConstFIdx)\name = "$ffold" + Str(newConstFIdx)
                              gVarMeta(newConstFIdx)\valueInt = 0
                              gVarMeta(newConstFIdx)\valueFloat = resultf
                              gVarMeta(newConstFIdx)\valueString = ""
                              gVarMeta(newConstFIdx)\flags = #C2FLAG_CONST | #C2FLAG_FLOAT
                              gVarMeta(newConstFIdx)\paramOffset = -1  ; Constants don't need frame offsets
                              gnLastVariable + 1

                              ; Replace first PUSH with new constant, eliminate second PUSH and STRADD
                              llObjects()\i = newConstFIdx
                              NextElement(llObjects())  ; Second PUSH
                              llObjects()\code = #ljNOOP
                              NextElement(llObjects())  ; STRADD
                              llObjects()\code = #ljNOOP
                           Else
                              NextElement(llObjects())
                              NextElement(llObjects())
                           EndIf
                        Else
                           NextElement(llObjects())
                           NextElement(llObjects())
                        EndIf
                     Else
                        NextElement(llObjects())
                     EndIf
                  Else
                     NextElement(llObjects())
                  EndIf
               EndIf
         EndSelect
      Next

      ; Pass 5: Arithmetic identity optimizations
      ForEach llObjects()
         Select llObjects()\code
            Case #ljADD
               ; x + 0 = x, eliminate ADD and the constant 0 push
               If PreviousElement(llObjects())
                  If (llObjects()\code = #ljPush Or llObjects()\code = #ljPUSHF Or llObjects()\code = #ljPUSHS) And (gVarMeta( llObjects()\i )\flags & #C2FLAG_CONST)
                     If gVarMeta( llObjects()\i )\valueInt = 0
                        llObjects()\code = #ljNOOP  ; Eliminate PUSH 0
                        NextElement(llObjects())     ; Back to ADD
                        llObjects()\code = #ljNOOP  ; Eliminate ADD
                     Else
                        NextElement(llObjects())
                     EndIf
                  Else
                     NextElement(llObjects())
                  EndIf
               EndIf

            Case #ljSUBTRACT
               ; x - 0 = x
               If PreviousElement(llObjects())
                  If (llObjects()\code = #ljPush Or llObjects()\code = #ljPUSHF Or llObjects()\code = #ljPUSHS) And (gVarMeta( llObjects()\i )\flags & #C2FLAG_CONST)
                     If gVarMeta( llObjects()\i )\valueInt = 0
                        llObjects()\code = #ljNOOP
                        NextElement(llObjects())
                        llObjects()\code = #ljNOOP
                     Else
                        NextElement(llObjects())
                     EndIf
                  Else
                     NextElement(llObjects())
                  EndIf
               EndIf

            Case #ljMULTIPLY
               ; x * 1 = x, x * 0 = 0
               If PreviousElement(llObjects())
                  If (llObjects()\code = #ljPush Or llObjects()\code = #ljPUSHF Or llObjects()\code = #ljPUSHS) And (gVarMeta( llObjects()\i )\flags & #C2FLAG_CONST)
                     mulConst = gVarMeta( llObjects()\i )\valueInt
                     If mulConst = 1
                        ; x * 1 = x, eliminate multiply and the constant
                        llObjects()\code = #ljNOOP
                        NextElement(llObjects())
                        llObjects()\code = #ljNOOP
                     ElseIf mulConst = 0
                        ; x * 0 = 0, keep the PUSH 0 but eliminate value below and multiply
                        ; This requires looking back 2 instructions
                        If PreviousElement(llObjects())
                           llObjects()\code = #ljNOOP  ; Eliminate the x value
                           NextElement(llObjects())     ; Back to PUSH 0
                           NextElement(llObjects())     ; To MULTIPLY
                           llObjects()\code = #ljNOOP  ; Eliminate MULTIPLY
                        Else
                           NextElement(llObjects())
                           NextElement(llObjects())
                        EndIf
                     Else
                        NextElement(llObjects())
                     EndIf
                  Else
                     NextElement(llObjects())
                  EndIf
               EndIf

            Case #ljDIVIDE
               ; x / 1 = x
               If PreviousElement(llObjects())
                  If (llObjects()\code = #ljPush Or llObjects()\code = #ljPUSHF Or llObjects()\code = #ljPUSHS) And (gVarMeta( llObjects()\i )\flags & #C2FLAG_CONST)
                     If gVarMeta( llObjects()\i )\valueInt = 1
                        llObjects()\code = #ljNOOP
                        NextElement(llObjects())
                        llObjects()\code = #ljNOOP
                     Else
                        NextElement(llObjects())
                     EndIf
                  Else
                     NextElement(llObjects())
                  EndIf
               EndIf
         EndSelect
      Next

      ;- Pass 7: String identity optimization (str + "" â†’ str)
      ForEach llObjects()
         If llObjects()\code = #ljSTRADD
            ; Check if previous instruction is PUSHS with empty string
            If PreviousElement(llObjects())
               If llObjects()\code = #ljPUSHS Or llObjects()\code = #ljPush
                  strIdx = llObjects()\i
                  If (gVarMeta(strIdx)\flags & #C2FLAG_STR) And gVarMeta(strIdx)\valueString = ""
                     ; Empty string found - eliminate it and STRADD
                     llObjects()\code = #ljNOOP
                     NextElement(llObjects())  ; Back to STRADD
                     llObjects()\code = #ljNOOP
                  Else
                     NextElement(llObjects())
                  EndIf
               Else
                  NextElement(llObjects())
               EndIf
            EndIf
         EndIf
      Next

      ;- Pass 8: String constant folding ("a" + "b" â†’ "ab")
      ForEach llObjects()
         If llObjects()\code = #ljSTRADD
            ; Look back for two consecutive string constant pushes
            If PreviousElement(llObjects())
               If (llObjects()\code = #ljPUSHS Or llObjects()\code = #ljPush) And (gVarMeta( llObjects()\i )\flags & #C2FLAG_CONST)
                  str2Idx = llObjects()\i
                  str2 = gVarMeta(str2Idx)\valueString
                  If PreviousElement(llObjects())
                     If (llObjects()\code = #ljPUSHS Or llObjects()\code = #ljPush) And (gVarMeta( llObjects()\i )\flags & #C2FLAG_CONST)
                        str1 = gVarMeta( llObjects()\i )\valueString
                        combinedStr = str1 + str2

                        ; Create new constant for combined string
                        newStrIdx = gnLastVariable
                        ; Clear all fields first
                        gVarMeta(newStrIdx)\name = "$strfold" + Str(newStrIdx)
                        gVarMeta(newStrIdx)\valueInt = 0
                        gVarMeta(newStrIdx)\valueFloat = 0.0
                        gVarMeta(newStrIdx)\valueString = combinedStr
                        gVarMeta(newStrIdx)\flags = #C2FLAG_CONST | #C2FLAG_STR
                        gVarMeta(newStrIdx)\paramOffset = -1  ; Constants don't need frame offsets
                        gnLastVariable + 1

                        ; Replace first PUSH with combined string, eliminate second PUSH and STRADD
                        llObjects()\i = newStrIdx
                        NextElement(llObjects())  ; Second PUSH
                        llObjects()\code = #ljNOOP
                        NextElement(llObjects())  ; STRADD
                        llObjects()\code = #ljNOOP
                     Else
                        NextElement(llObjects())
                        NextElement(llObjects())
                     EndIf
                  Else
                     NextElement(llObjects())
                  EndIf
               Else
                  NextElement(llObjects())
               EndIf
            EndIf
         EndIf
      Next

      ;- Pass 9: Return value type conversions
      ; Insert type conversions before return statements when return value type doesn't match function return type
      ; Only valid conversions: FTOI, ITOF, FTOS, ITOS (no string-to-number conversions exist)
      ForEach llObjects()
         Select llObjects()\code
            Case #ljreturn
               ; Function returns INT - check if we're pushing a FLOAT
               If PreviousElement(llObjects())
                  Select llObjects()\code
                     Case #ljPUSHF, #ljFETCHF, #ljLFETCHF
                        ; Returning FLOAT from INT function - insert FTOI
                        NextElement(llObjects())
                        InsertElement(llObjects())
                        llObjects()\code = #ljFTOI
                        llObjects()\i = 0
                        llObjects()\n = 0
                        PreviousElement(llObjects())
                        NextElement(llObjects())
                     Default
                        NextElement(llObjects())
                  EndSelect
               EndIf

            Case #ljreturnF
               ; Function returns FLOAT - check if we're pushing an INT
               If PreviousElement(llObjects())
                  Select llObjects()\code
                     Case #ljPush, #ljFetch, #ljLFETCH
                        ; Need to verify this is not a FLOAT constant marked as PUSH
                        varIdx = llObjects()\i
                        If varIdx >= 0 And varIdx < gnLastVariable
                           If Not (gVarMeta(varIdx)\flags & #C2FLAG_FLOAT)
                              ; Returning INT from FLOAT function - insert ITOF
                              NextElement(llObjects())
                              InsertElement(llObjects())
                              llObjects()\code = #ljITOF
                              llObjects()\i = 0
                              llObjects()\n = 0
                              PreviousElement(llObjects())
                              NextElement(llObjects())
                           Else
                              NextElement(llObjects())
                           EndIf
                        Else
                           NextElement(llObjects())
                        EndIf
                     Default
                        NextElement(llObjects())
                  EndSelect
               EndIf

            Case #ljreturnS
               ; Function returns STRING - check if we're pushing an INT or FLOAT
               If PreviousElement(llObjects())
                  Select llObjects()\code
                     Case #ljPush, #ljFetch, #ljLFETCH
                        ; Need to verify this is not a STRING constant marked as PUSH
                        varIdx = llObjects()\i
                        If varIdx >= 0 And varIdx < gnLastVariable
                           If Not (gVarMeta(varIdx)\flags & #C2FLAG_STR)
                              ; Returning INT from STRING function - insert ITOS
                              NextElement(llObjects())
                              InsertElement(llObjects())
                              llObjects()\code = #ljITOS
                              llObjects()\i = 0
                              llObjects()\n = 0
                              PreviousElement(llObjects())
                              NextElement(llObjects())
                           Else
                              NextElement(llObjects())
                           EndIf
                        Else
                           NextElement(llObjects())
                        EndIf
                     Case #ljPUSHF, #ljFETCHF, #ljLFETCHF
                        ; Returning FLOAT from STRING function - insert FTOS
                        NextElement(llObjects())
                        InsertElement(llObjects())
                        llObjects()\code = #ljFTOS
                        llObjects()\i = 0
                        llObjects()\n = 0
                        PreviousElement(llObjects())
                        NextElement(llObjects())
                     Default
                        NextElement(llObjects())
                  EndSelect
               EndIf
         EndSelect
      Next

      ;- Pass 10: Remove all NOOP instructions from the code stream
      ForEach llObjects()
         If llObjects()\code = #ljNOOP
            DeleteElement(llObjects())
         EndIf
      Next
      EndIf  ; optimizationsEnabled

   EndProcedure

; IDE Options = PureBasic 6.21 (Windows - x64)
; CursorPosition = 13
; Folding = -
; EnableAsm
; EnableThread
; EnableXP
; CPU = 1
; EnablePurifier
; EnableCompileCount = 0
; EnableBuildCount = 0
; EnableExeConstant