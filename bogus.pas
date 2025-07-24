program Bogus;
{$mode objfpc}
{$WARN 6058 OFF}

uses
    SysUtils, Classes;

type
    TCPU = record
        Reg0: Byte;
        Reg1: Byte;
        Reg2: Byte;
        Reg3: Byte;
    end;
    { Hypothetical Code Segment starts at $0000; Hypothetical Stack Starts at $0255; There is no stack. Yet... }
    TMemory = array[0..511] of Byte;
    { Something for I/O?? }
    PTCPU = ^TCPU;
    PTMemory = ^TMemory;
    PCounter = ^Integer;
    {$scopedEnums on}
    Operation = (Load, Add, Sub, Stop, ILLEGAL);
    Register = (Zeroth, First, Second, Third, INVALID);
    Instruction = record
        OpCode: Operation;
        OperandLeft: Variant;
        OperandRight: Variant;
    end;
    InstructionArray = array of Instruction;
    InstructionChunk = array[0..2] of Byte;

procedure PrintUsage(ProgramName: String);
begin
    WriteLn(Format('Usage: %s <input.bogus>', [ProgramName]));
end;

procedure ThrowRuntimeError(Error: String; InstructionIndex: Integer);
begin
    WriteLn(Format('Instruction #%d - Runtime ERROR: %s', [InstructionIndex, Error]));
    ExitCode := 1;
    Exit;
end;

procedure PrintMachineState(CPU: PTCPU; Memory: PTMemory; Index: Integer; Inst: Instruction);
var
    MemoryUsed: Integer;
begin
    WriteLn();
    WriteLn(Format('-- Machine State: #%d --', [Index]));
    case Inst.OpCode of
        Operation.Load :
        begin
            WriteLn(Format('Operation Load %d (%s) to Register ', [Integer(Inst.OperandRight), HexStr(Integer(Inst.OperandRight), 8)]), Register(Inst.OperandLeft));
        end;
        Operation.Add :
        begin
            WriteLn('Operation Add Register ', Register(Inst.OperandRight), ' to Register ', Register(Inst.OperandLeft));
        end;
        Operation.Sub :
        begin
            WriteLn('Operation Sub Register ', Register(Inst.OperandRight), ' from Register ', Register(Inst.OperandLeft));
        end;
        Operation.Stop : WriteLn('Operation Stop');
    else
        ThrowRuntimeError('Illegal operation', Index);
    end;
    WriteLn(Format('  Reg0 Val: %s (%d)', [HexStr(CPU^.Reg0, 8), CPU^.Reg0]));
    WriteLn(Format('  Reg1 Val: %s (%d)', [HexStr(CPU^.Reg1, 8), CPU^.Reg1]));
    WriteLn(Format('  Reg2 Val: %s (%d)', [HexStr(CPU^.Reg2, 8), CPU^.Reg2]));
    WriteLn(Format('  Reg3 Val: %s (%d)', [HexStr(CPU^.Reg3, 8), CPU^.Reg3]));

    MemoryUsed := 0;

    while Memory^[MemoryUsed] <> $00FF do
    begin
        Inc(MemoryUsed);
    end;

    WriteLn(Format('Memory: %d/%d B', [MemoryUsed, Length(Memory^)]));
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

function ProcessInstructionChunk(Chunk: InstructionChunk): Instruction;
var
    ReturnInstruction: Instruction;
begin
    ReturnInstruction := InitInstruction(Operation(Chunk[0]), Register(Chunk[1]), Unassigned);

    case ReturnInstruction.OpCode of
        Operation.Add, Operation.Sub : ReturnInstruction.OperandRight := Register(Chunk[2]);
    else
        ReturnInstruction.OperandRight := Integer(Chunk[2]);
    end;

    ProcessInstructionChunk := ReturnInstruction;
end;

function GetRegisterPointer(CPU: PTCPU; Reg: Register; InstructionIndex: Integer): PByte;
begin
    repeat
        case Reg of
            Register.Zeroth : GetRegisterPointer := @CPU^.Reg0;
            Register.First  : GetRegisterPointer := @CPU^.Reg1;
            Register.Second : GetRegisterPointer := @CPU^.Reg2;
            Register.Third  : GetRegisterPointer := @CPU^.Reg3;
        else
            ThrowRuntimeError('Invalid register', InstructionIndex);
        end;
    until true;
end;

procedure ExecuteInstruction(CPU: PTCPU; Memory: PTMemory; Index: Integer; Inst: Instruction);
var
    Register1, Register2: Register;
    Value: Integer;

    RegPtr1, RegPtr2: PByte;
begin
    repeat
        case Inst.OpCode of
            Operation.Load :
            begin
                Register1 := Register(Inst.OperandLeft);
                Value := Integer(Inst.OperandRight);

                RegPtr1 := GetRegisterPointer(CPU, Register1, Index);

                RegPtr1^ := Byte(Value);

            end;
            Operation.Add :
            begin
                Register1 := Register(Inst.OperandLeft);
                Register2 := Register(Inst.OperandRight);

                RegPtr1 := GetRegisterPointer(CPU, Register1, Index);
                RegPtr2 := GetRegisterPointer(CPU, Register2, Index);

                Value := Integer(RegPtr1^) + Integer(RegPtr2^);

                RegPtr1^ := Byte(Value);
            end;
            Operation.Sub :
            begin
                Register1 := Register(Inst.OperandLeft);
                Register2 := Register(Inst.OperandRight);

                RegPtr1 := GetRegisterPointer(CPU, Register1, Index);
                RegPtr2 := GetRegisterPointer(CPU, Register2, Index);

                Value := Integer(RegPtr2^) - Integer(RegPtr1^);

                RegPtr1^ := Byte(Value);
            end;
            Operation.Stop : Break;
        else
            ThrowRuntimeError('Illegal operation', Index);
        end;
    until true;

    PrintMachineState(CPU, Memory, Index, Inst);
end;

procedure ExecuteBogus(CPU: PTCPU; Memory: PTMemory; Counter: PCounter);
var
    CurrentByte: Byte;
    CurrentChunk: InstructionChunk;
    CurrentInstruction: Instruction;
    Instructions: InstructionArray;
    i, j, InstructionCount: Integer;
begin
    i := 0;
    j := 0;
    InstructionCount := 0;
    Instructions := InstructionArray.Create(InitInstruction(Operation.ILLEGAL, Register.INVALID, Unassigned));
    CurrentByte := $0000;
    CurrentChunk[0] := $0000;
    CurrentChunk[1] := $0000;
    CurrentChunk[2] := $0000;

    while CurrentByte <> $00FF do
    begin
        CurrentByte := Memory^[i];
        if j = 3 then
        begin
            j := 0;
            CurrentInstruction := ProcessInstructionChunk(CurrentChunk);
            Instructions[InstructionCount] := CurrentInstruction;
            Inc(InstructionCount);
            SetLength(Instructions, InstructionCount+1);
        end;

        CurrentChunk[j] := CurrentByte;

        Inc(j);
        Inc(i);
    end;

    CurrentInstruction := InitInstruction(Operation.Stop, Unassigned, Unassigned);
    Instructions[InstructionCount] := CurrentInstruction;
    Inc(InstructionCount);
    SetLength(Instructions, InstructionCount+1);

    for i := 0 to High(Instructions)-1 do
    begin
        CurrentInstruction := Instructions[i];

        ExecuteInstruction(CPU, Memory, Counter^, CurrentInstruction);

        Inc(Counter^);
    end;
end;

var
    CPU: TCPU;
    Memory: TMemory;
    Counter: Integer;

    InputFilePath: String;
    InputFile: TFileStream;

begin
    WriteLn('*** Bogus Virtual Machine ***');
    if ParamCount = 1 then
    begin
        InputFilePath := ParamStr(1);

        CPU.Reg0 := $0000;
        CPU.Reg1 := $0000;
        CPU.Reg2 := $0000;
        CPU.Reg3 := $0000;

        Counter := 0;

        try
            InputFile := TFileStream.Create(InputFilePath, fmOpenRead);
            InputFile.Position := 0;

            InputFile.Read(Memory, SizeOf(Memory));

            InputFile.Free();
        except
            on E: Exception do
            begin
                WriteLn('Could not open a file. Details: ', E.Message);
                ExitCode := 1;
                Exit;
            end;
        end;

        ExecuteBogus(@CPU, @Memory, @Counter);

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

