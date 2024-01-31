DEFINT A-Z

CONST FALSE = 0
CONST TRUE = -1

TYPE DTAStr
 Reserved AS STRING * 20
 Attribute AS INTEGER
 FileTime AS INTEGER
 FileDate AS INTEGER
 FileSize AS LONG
 FileName AS STRING * 13
END TYPE

TYPE RegTypeX
 AX AS INTEGER
 bx AS INTEGER
 CX AS INTEGER
 DX AS INTEGER
 bp AS INTEGER
 si AS INTEGER
 di AS INTEGER
 Flags AS INTEGER
 DS AS INTEGER
 es AS INTEGER
END TYPE

DECLARE FUNCTION Choose$ (Prompt$, Choices$)
DECLARE FUNCTION DecodeCharacter$ (Encoded$)
DECLARE FUNCTION DecodeText$ (Text$, UnencodedCount)
DECLARE FUNCTION EncodeCharacter$ (Character$)
DECLARE FUNCTION EncodeText$ (Text$)
DECLARE FUNCTION FileExists (FileName$)
DECLARE FUNCTION GetExtension$ (FileName$)
DECLARE FUNCTION IsCharacterCode (Encoded$)
DECLARE FUNCTION IsTextFile (FileName$)
DECLARE FUNCTION ReverseSearch (Text$, Character$)
DECLARE SUB DecodeFile (InputFile$, UnencodeCount)
DECLARE SUB EncodeFile (FileName$)
DECLARE SUB INTERRUPTX (intnum AS INTEGER, inreg AS RegTypeX, outreg AS RegTypeX)
DECLARE SUB Main ()

CALL Main

FUNCTION Choose$ (Prompt$, Choices$)
 PRINT Prompt$;

 DO
  Choice$ = UCASE$(INPUT$(1))
 LOOP UNTIL INSTR(UCASE$(Choices$), Choice$) > 0
 PRINT " "; Choice$;

 Choose$ = Choice$
END FUNCTION

FUNCTION DecodeCharacter$ (Encoded$)
 Decoded$ = ""
 Buffer$ = Encoded$
 IF LEFT$(Buffer$, 1) = "[" THEN
  Position = INSTR(Buffer$, "]")
  IF Position > 0 THEN
   Buffer$ = MID$(Buffer$, 2, Position - 2)
   IF IsCharacterCode(Buffer$) THEN
    CharacterCode = VAL(Buffer$)
    SELECT CASE CharacterCode
     CASE 9, 13, IS > 31
      Decoded$ = CHR$(CharacterCode)
    END SELECT
   END IF
  END IF
 END IF

 DecodeCharacter$ = Decoded$
END FUNCTION

SUB DecodeFile (InputFile$, UnencodeCount)
 InputFileH = FREEFILE
 OPEN InputFile$ FOR INPUT LOCK READ WRITE AS InputFileH
  LINE INPUT #InputFileH, Extension$
  OutputFile$ = LEFT$(InputFile$, LEN(InputFile$) - LEN(GetExtension$(InputFile$))) + Extension$

  Continue = FALSE
  IF FileExists(OutputFile$) THEN
   Continue = (Choose$(OutputFile$ + " already exists. Overwrite y/n?", "yn") = "Y")
  ELSE
   Continue = TRUE
  END IF

  IF Continue THEN
   PRINT "Decoding "; InputFile$; " -> "; OutputFile$
   OutputFileH = FREEFILE
   OPEN OutputFile$ FOR OUTPUT LOCK READ WRITE AS OutputFileH
    DO UNTIL EOF(InputFileH)
     LINE INPUT #InputFileH, Text$
     Text$ = DecodeText$(Text$, UnencodeCount)
     PRINT #OutputFileH, Text$
    LOOP
   CLOSE OutputFileH
  END IF
 CLOSE InputFileH
END SUB

FUNCTION DecodeText$ (Text$, UnencodedCount)
 InputBuffer$ = Text$
 OutputBuffer$ = ""

 DO UNTIL InputBuffer$ = ""
  Character$ = LEFT$(InputBuffer$, 1)
  IF Character$ = "[" THEN
   Position = INSTR(InputBuffer$, "]")
   IF Position > 0 THEN
    Decoded$ = DecodeCharacter$(InputBuffer$)
    IF Decoded$ = "" THEN
     OutputBuffer$ = OutputBuffer$ + LEFT$(InputBuffer$, Position)
    ELSE
     OutputBuffer$ = OutputBuffer$ + Decoded$
    END IF
    InputBuffer$ = MID$(InputBuffer$, Position + 1)
   END IF
  ELSE
   OutputBuffer$ = OutputBuffer$ + Character$
   SELECT CASE ASC(Character$)
    CASE 0 TO 8, 10 TO 12, 14 TO 31, 128 TO 255
     UnencodedCount = UnencodedCount + 1
   END SELECT

   InputBuffer$ = MID$(InputBuffer$, 2)
  END IF
 LOOP

 DecodeText$ = OutputBuffer$
END FUNCTION

FUNCTION EncodeCharacter$ (Character$)
 Encoded$ = ""

 SELECT CASE ASC(Character$)
  CASE IS > 127
   Encoded$ = "[" + LTRIM$(STR$(ASC(Character$))) + "]"
  CASE 9, 13, IS > 31
   Encoded$ = Character$
 END SELECT

 EncodeCharacter$ = Encoded$
END FUNCTION

SUB EncodeFile (InputFile$)
 IF NOT IsTextFile(InputFile$) THEN
  Extension$ = GetExtension$(InputFile$)
  IF NOT Extension$ = "" THEN
   InputFileH = FREEFILE
   OPEN InputFile$ FOR INPUT LOCK READ WRITE AS InputFileH
    OutputFile$ = LEFT$(InputFile$, LEN(InputFile$) - LEN(Extension$)) + "TXT"

    Continue = FALSE
    IF FileExists(OutputFile$) THEN
     Continue = (Choose$(OutputFile$ + " already exists. Overwrite y/n?", "yn") = "Y")
    ELSE
     Continue = TRUE
    END IF

    IF Continue THEN
     PRINT "Encoding "; InputFile$; " -> "; OutputFile$
     OutputFileH = FREEFILE
     OPEN OutputFile$ FOR OUTPUT LOCK READ WRITE AS OutputFileH
      PRINT #OutputFileH, Extension$
      DO UNTIL EOF(InputFileH)
       LINE INPUT #InputFileH, Text$
       Text$ = EncodeText$(Text$)
       PRINT #OutputFileH, Text$
      LOOP
     CLOSE OutputFileH
    END IF
   CLOSE InputFileH
  END IF
 END IF
END SUB

FUNCTION EncodeText$ (Text$)
 OutputBuffer$ = ""
 Position = 1
 DO UNTIL Position > LEN(Text$)
  Encoded$ = EncodeCharacter$(MID$(Text$, Position, 1))
  IF Encoded$ = "" THEN PRINT "Unsupported character code"; ASC(MID$(Text$, Position, 1))
  OutputBuffer$ = OutputBuffer$ + Encoded$
  Position = Position + 1
 LOOP

 EncodeText$ = OutputBuffer$
END FUNCTION

FUNCTION FileExists (FileName$)
DIM DTA AS DTAStr
DIM Registers AS RegTypeX

 Registers.AX = &H1A00
 Registers.DX = VARPTR(DTA)
 Registers.DS = VARSEG(DTA)
 INTERRUPTX &H21, Registers, Registers

 Attributes = 0
 Pattern$ = FileName$ + CHR$(0)

 Registers.AX = &H4E00
 Registers.CX = Attributes
 Registers.DX = SADD(Pattern$)
 Registers.DS = VARSEG(Pattern$)
 INTERRUPTX &H21, Registers, Registers

 FileExists = (INSTR(DTA.FileName, CHR$(0)) > 1)
END FUNCTION

FUNCTION GetExtension$ (FileName$)
 Extension$ = ""
 Position = ReverseSearch(FileName$, ".")
 IF Position > 0 THEN Extension$ = MID$(FileName$, Position + 1)
 GetExtension$ = UCASE$(Extension$)
END FUNCTION

FUNCTION IsCharacterCode (Encoded$)
 CharacterCode = VAL(Encoded$)
 IsCharacterCode = (Encoded$ = LTRIM$(STR$(CharacterCode)) AND (CharacterCode >= 0 AND CharacterCode <= 255))
END FUNCTION

FUNCTION IsTextFile (FileName$)
 IsTextFile = (GetExtension$(FileName$) = "TXT")
END FUNCTION

SUB Main
 InputFile$ = COMMAND$

 IF InputFile$ = "" THEN
  PRINT "Specify an input file."
 ELSE
  IF IsTextFile(InputFile$) THEN
   UnencodeCount = 0
   DecodeFile InputFile$, UnencodeCount
   IF UnencodeCount > 0 THEN PRINT "Warning: unencoded special characters found."
  ELSE
   EncodeFile InputFile$
  END IF
 END IF
END SUB

FUNCTION ReverseSearch (Text$, Character$)
 Position = LEN(Text$)

 DO UNTIL Position = 0
  IF MID$(Text$, Position, 1) = Character$ THEN
   EXIT DO
  ELSE
   Position = Position - 1
  END IF
 LOOP

 ReverseSearch = Position
END FUNCTION

