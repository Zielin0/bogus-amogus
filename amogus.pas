program Amogus;
{$mode objfpc}
{$WARN 6058 OFF}

uses
    SysUtils, Variants, StrUtils, Types, Classes;

type
    Lines = array of String;
    {$scopedEnums on}
    Operation = (Load, Add, Sub, Stop, ILLEGAL);
    Register = (Zeroth, First, Second, Third, INVALID);
    Instruction = record
        OpCode: Operation;
        OperandLeft: Variant;
        OperandRight: Variant;
    end;
    InstructionArray = array of Instruction;
    ByteArray = array of Byte;
    PByteArray = ^ByteArray;

procedure PrintUsage(ProgramName: String);
begin
    WriteLn(Format('Usage: %s <program.amogus> <output.bogus>', [ProgramName]));
end;

procedure ThrowParseError(Error, FileName: String; LineNumber, CharNumber: Integer);
begin
    WriteLn(Format('%s:%d:%d - Parse ERROR: %s', [FileName, LineNumber, CharNumber, Error]));
    ExitCode := 1;
    Exit;
end;

procedure ThrowCompilationError(Error: String; InstructionIndex: Integer);
begin
    WriteLn(Format('Instruction-%d - Compile ERROR: %s', [InstructionIndex, Error]));
    ExitCode := 1;
    Exit;
end;

function InitInstruction(OpCode: Operation; left, right: Variant): Instruction;
var
    ReturnInstruction: Instruction;
begin
    ReturnInstruction.OpCode := OpCode;
    ReturnInstruction.OperandLeft := left;
    ReturnInstruction.OperandRight := right;

    InitInstruction := ReturnInstruction;
end;

function ConvertStrToOperation(OpCode: String): Operation;
begin
    case LowerCase(OpCode) of
        'load' : ConvertStrToOperation := Operation.Load;
        'add' : ConvertStrToOperation := Operation.Add;
        'sub' : ConvertStrToOperation := Operation.Sub;
        'stop' : ConvertStrToOperation := Operation.Stop;
    else
        ConvertStrToOperation := Operation.ILLEGAL;
    end;
end;

function ConvertStrToRegister(RegisterStr: String): Register;
begin
    case LowerCase(RegisterStr) of
        'reg0' : ConvertStrToRegister := Register.Zeroth;
        'reg1' : ConvertStrToRegister := Register.First;
        'reg2' : ConvertStrToRegister := Register.Second;
        'reg3' : ConvertStrToRegister := Register.Third;
    else
        ConvertStrToRegister := Register.INVALID;
    end;
end;

function ValidateRegister(RegisterStr: String): Boolean;
var
    TempStr: String;
    RegisterNumber: Integer;
begin
    TempStr := Copy(RegisterStr, 0, 3);

    if CompareStr(TempStr, 'reg') <> 0 then
    begin
        ValidateRegister := false;
    end
    else
    begin
        TempStr := Copy(RegisterStr, 4, Length(RegisterStr)-3);

        try
            RegisterNumber := StrToInt(TempStr);

            if (RegisterNumber < 0) or (RegisterNumber >= 4) then
            begin
                ValidateRegister := false;
            end
            else
            begin
                ValidateRegister := true;
            end;
        except
            on E: EConvertError do
            begin
                ValidateRegister := false;
            end;
        end;

    end;
end;

function ParseLine(Line, FilePath: String; LineNumber: Integer): Instruction;
var
    ReturnInstruction: Instruction;
    SplitBuffer: TStringDynArray;
    OpCodeString, OperandsString: String;
    OperandLeft, OperandRight: Variant;
    OperationOp: Operation;
    i, SpaceCount, OperandsSpaceCount, IntBuffer: Integer;
begin
    SpaceCount := 0;
    OperandsSpaceCount := 0;
    ReturnInstruction := InitInstruction(Operation.ILLEGAL, Unassigned, Unassigned);

    for i := 0 to Length(Line) do
    begin
        if (Line[i] = ' ') then
        begin
            Inc(SpaceCount);
            if SpaceCount <= 1 then
            begin
                OpCodeString := Copy(Line, 0, i-1);
            end;
        end;
    end;

    if SpaceCount = 0 then
    begin
        OpCodeString := Copy(Line, 0, Length(Line));
        OperationOp := ConvertStrToOperation(OpCodeString);
        if OperationOp = Operation.ILLEGAL then
        begin
            ThrowParseError(Format('Illegal Operation `%s`', [OpCodeString]), FilePath, LineNumber, 0);
        end;

        ReturnInstruction := InitInstruction(OperationOp, Unassigned, Unassigned);
        ParseLine := ReturnInstruction;
    end
    else
    begin
        OperandsString := Copy(Line, Length(OpCodeString)+1, Length(Line)-Length(OpCodeString));
        for i := 0 to Length(OperandsString) do
        begin
            if OperandsString[i] = '' then Inc(OperandsSpaceCount);
        end;
        OperandsString := StringReplace(OperandsString, ' ', '', [rfReplaceAll]);

        SplitBuffer := SplitString(OperandsString, ',');

        if Length(SplitBuffer) > 2 then
        begin
            ThrowParseError(Format('Expected 2 Operands, Got %d instead', [Length(SplitBuffer)]), FilePath, LineNumber, SpaceCount+Length(OpCodeString));
        end;

        try
            StrToInt(SplitBuffer[0]);
            ThrowParseError('First argument must be a REGISTER', FilePath, LineNumber, 2+Length(OpCodeString));
        except
            on E: EConvertError do
            begin
                if not ValidateRegister(SplitBuffer[0]) then
                begin
                    ThrowParseError(Format('Invalid Register `%s`; List of Valid Registers: reg0, reg1, reg2, reg3', [SplitBuffer[0]]), FilePath, LineNumber, SpaceCount+Length(OpCodeString));
                end;

                OperandLeft := ConvertStrToRegister(SplitBuffer[0]);
            end;
        end;

        try
            IntBuffer := StrToInt(SplitBuffer[1]);
            OperandRight := IntBuffer;
        except
            on E: EConvertError do
            begin
                if not ValidateRegister(SplitBuffer[1]) then
                begin
                    ThrowParseError(Format('Invalid Register `%s`; List of Valid Registers: reg0, reg1, reg2, reg3', [SplitBuffer[1]]), FilePath, LineNumber, SpaceCount+Length(OpCodeString)+Length(SplitBuffer[0])+2+OperandsSpaceCount);
                end;

                OperandRight := ConvertStrToRegister(SplitBuffer[1]);
            end;
        end;

        OperationOp := ConvertStrToOperation(OpCodeString);
        if OperationOp = Operation.ILLEGAL then
        begin
            ThrowParseError(Format('Illegal Operation `%s`', [OpCodeString]), FilePath, LineNumber, 0);
        end;

        ReturnInstruction := InitInstruction(OperationOp, OperandLeft, OperandRight);
        ParseLine := ReturnInstruction;
    end;
end;

function ParseAmogus(FilePath: String; Source: Lines): InstructionArray;
var
    LineInstruction: Instruction;
    InstructionsReturn: InstructionArray;
    i, InstructionCount: Integer;
begin
    InstructionCount := 0;
    InstructionsReturn := InstructionArray.create(InitInstruction(Operation.ILLEGAL, Unassigned, Unassigned));

    for i := 0 to Length(Source)-1 do
    begin
        if Source[i] = '' then
            Continue;

        LineInstruction := ParseLine(Source[i], FilePath, i+1);
        InstructionsReturn[InstructionCount] := LineInstruction;
        Inc(InstructionCount);
        SetLength(InstructionsReturn, InstructionCount+1);
    end;

    ParseAmogus := InstructionsReturn;
end;

procedure PushByteToArray(ArrayOfBytes: PByteArray; Count: PInteger; B: Byte);
begin
    ArrayOfBytes^[Count^] := B;
    Inc(Count^);
    SetLength(ArrayOfBytes^, Count^+1);
end;

function CompileAmogus(Instructions: InstructionArray): ByteArray;
var
    ReturnBytes: ByteArray;
    ByteCount, i: Integer;
begin
    ByteCount := 0;
    ReturnBytes := ByteArray.create(0);

    for i := 0 to Length(Instructions)-1 do
    begin
        case Instructions[i].OpCode of
            Operation.Load : PushByteToArray(@ReturnBytes, @ByteCount, $0000);
            Operation.Add  : PushByteToArray(@ReturnBytes, @ByteCount, $0001);
            Operation.Sub  : PushByteToArray(@ReturnBytes, @ByteCount, $0002);
            Operation.Stop : PushByteToArray(@ReturnBytes, @ByteCount, $00FF);
        else
            ThrowCompilationError('Invalid Operation', i);
        end;

        case VarType(Instructions[i].OperandLeft) of
            varInt64 :
            begin
                case Register(Instructions[i].OperandLeft) of
                    Register.Zeroth : PushByteToArray(@ReturnBytes, @ByteCount, $0000);
                    Register.First  : PushByteToArray(@ReturnBytes, @ByteCount, $0001);
                    Register.Second : PushByteToArray(@ReturnBytes, @ByteCount, $0002);
                    Register.Third  : PushByteToArray(@ReturnBytes, @ByteCount, $0003);
                else
                    ThrowCompilationError('Invalid Register', i);
                end;
            end;
            varEmpty : Break;
        else
            ThrowCompilationError('Invalid Left Operand', i);
        end;

        case VarType(Instructions[i].OperandRight) of
            varInt64 :
            begin
                case Register(Instructions[i].OperandRight) of
                    Register.Zeroth : PushByteToArray(@ReturnBytes, @ByteCount, $0000);
                    Register.First  : PushByteToArray(@ReturnBytes, @ByteCount, $0001);
                    Register.Second : PushByteToArray(@ReturnBytes, @ByteCount, $0002);
                    Register.Third  : PushByteToArray(@ReturnBytes, @ByteCount, $0003);
                else
                    ThrowCompilationError('Invalid Register', i);
                end;
            end;
            varInteger :
            begin
                if (Integer(Instructions[i].OperandRight) > 255) or (Integer(Instructions[i].OperandRight) < 0) then
                begin
                    ThrowCompilationError('Invalid Integer Range. Keep numbers in 0..255', i);
                end;

                PushByteToArray(@ReturnBytes, @ByteCount, Byte(Integer(Instructions[i].OperandRight)));
            end;
            varEmpty : Break;
        else
            ThrowCompilationError('Invalid Right Operand', i);
        end;
    end;

    CompileAmogus := ReturnBytes;
end;

{$ifdef DEBUG}
procedure PrintInstructions(Instructions: InstructionArray);
var
    i: Integer;
begin
    for i := 0 to High(Instructions)-1 do
    begin
        WriteLn(Instructions[i].OpCode);
        WriteLn(Instructions[i].OperandLeft);
        WriteLn(Instructions[i].OperandRight);
        WriteLn();
    end;
end;

procedure PrintBytes(Bytes: ByteArray);
var
    i: Integer;
begin
    for i := 0 to High(Bytes)-1 do
    begin
        WriteLn(HexStr(Bytes[i], 8));
    end;
end;
{$endif}

var
    InputFilePath, OutputFilePath: String;
    InputFile: TextFile;
    OutputFile: TMemoryStream;
    AmogusContent: Lines;
    InstructionsParsed: InstructionArray;
    BytesToWrite: ByteArray;
    i: Integer;

begin
    WriteLn('*** Amogus Compiler ***');
    if ParamCount = 2 then
    begin
        InputFilePath := ParamStr(1);
        OutputFilePath := ParamStr(2);

        WriteLn('Input: ', InputFilePath);
        WriteLn('Output: ', OutputFilePath);

        AssignFile(InputFile, InputFilePath);

        try
            Reset(InputFile);

            AmogusContent := Lines.create('');
            i := 0;
            while not EOF(InputFile) do
            begin
                ReadLn(InputFile, AmogusContent[i]);
                Inc(i);
                SetLength(AmogusContent, i+1);
            end;

            CloseFile(InputFile);
        except
            on E: EInOutError do
            begin
                WriteLn('Could not open a file. Details: ', E.Message);
                ExitCode := 1;
                Exit;
            end;
        end;

        WriteLn(Format('Parsing `%s`', [InputFilePath]));
        InstructionsParsed := ParseAmogus(InputFilePath, AmogusContent);

        {$ifdef DEBUG}
        PrintInstructions(InstructionsParsed);
        {$endif}

        WriteLn(Format('Compiling %d instructions', [Length(InstructionsParsed)-1]));

        BytesToWrite := CompileAmogus(InstructionsParsed);

        {$ifdef DEBUG}
        PrintBytes(BytesToWrite);
        {$endif}

        WriteLn(Format('Writing to `%s`', [OutputFilePath]));

        OutputFile := TMemoryStream.Create();

        try

            for i := 0 to High(BytesToWrite)-1 do
            begin
                OutputFile.WriteByte(BytesToWrite[i]);
            end;

            OutputFile.SaveToFile(OutputFilePath);
        except
            on E: Exception do
            begin
                WriteLn('Could not open a file. Details: ', E.Message);
                ExitCode := 1;
                Exit;
            end;
        end;

        OutputFile.Free();

        WriteLn('Compiled Successfully!');

        ExitCode := 0;
        Exit;
    end
    else
    begin
        PrintUsage(ParamStr(0));
        ExitCode := 1;
        Exit;
    end;
end.

