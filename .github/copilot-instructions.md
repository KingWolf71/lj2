1. priority is on VM execution speed
2. don't use command line compiler as it won't work; I can compile manually via IDE
3. always leave definitions at beginning of procedure - no exceptions
4. No procedure static variables; we use global variables which can be reset between runs
5. if there is an _*.ver file add a .1 everytime we interact (MAJ.MIN.FIX)
6. don't touch the *.lj files unless I expressively ask you to
7. Purebasic functions and variables and constants are case insesitive
8. Compiler flow pre-processor - scanner - AST - codegenerator - postprocessor
9. Postprocessor is crucial to properly correct JMP and CALL calls
10. Postprocessor is crucial and final step to make sure type "guessing" is not needed in the VirtualMachine
