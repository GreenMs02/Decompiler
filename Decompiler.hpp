#pragma once
// Written by (Co)
#include "Deserializer.hpp"
#include <iomanip> 
#include <sstream>
#include <map>
#include <unordered_map>
#include <vector>



struct RegisterState { // stolen from unluac just for test
	bool temporary;
	bool local;
	bool read;
	bool written;

	RegisterState() {
		temporary = false;
		local = false;
		read = false;
		written = false;
	}
};

class RegisterStates {
private:
	int registers;
	int lines;
	std::map<std::pair<int, int>, RegisterState> states;

public:
	bool temporary;
	bool local;
	bool read;
	bool written;


	RegisterStates()
	{
		temporary = false;
		local = false;
		read = false;
		written = false;
	}

	RegisterStates(int registers, int lines)
	{
		registers = registers;
		lines = lines;
		states = { {std::make_pair(lines, registers), RegisterState()} };
		for (int line = 0; line < lines; line++) {
			for (int reg = 0; reg < registers; reg++) {
				states[std::make_pair(lines, registers)] = RegisterState();
			}
		}
	}

	RegisterState& get(int reg, int line)
	{
		return states[std::make_pair(reg, line)];
	}
	
	void setLocal(int reg, int line)
	{
		for (int r = 0; r <= reg; r++) {
			get(r, line).local = true;
		}
	}

	void setTemporary(int reg, int line) {
		for (int r = reg; r < registers; r++) {
			get(r, line).temporary = true;
		}
	}
};

class Decompiler
{
public:
	std::string Decompile(const std::string& Bytecode)
	{
		return GetBlock(Deserialize(Bytecode.c_str(), Bytecode.size()));
	}

private:



	// TODO: assignment struct
	// TODO: Make local as upvalue
	struct Local
	{
		std::string Name;
		std::string Value;
		uint8_t Reg;

		bool Used;
		bool Empty;

		Local() {};
		Local(uint8_t A, std::string name, std::string val) { Reg = A; Name = name; Value = val; Used = true;  Empty = false; }
	};

	struct LateStrc
	{
		std::string Apply;
		bool skip;

		LateStrc() {};
		LateStrc(std::string a, bool b) { Apply = a; skip = b; }
	};

	struct DecompiledStrc
	{
		unsigned int pc;
		std::string value;


		DecompiledStrc() {};
		DecompiledStrc(unsigned int a, std::string b) { pc = a, value = b; };
	};

	struct StatementData
	{
		std::string Condition;
		unsigned int EndPC;
		StatementData() {};
		StatementData(std::string a, unsigned int b) { Condition = a; EndPC = b; };
	};

	unsigned int varIndex = 1;
	std::unordered_map<uint8_t, DecompiledStrc> vTempInstructions = {};
	std::unordered_map<uint8_t, Local> vLocals = {};
	std::unordered_map<unsigned int, LateStrc> LateApply = {};
	std::unordered_map<unsigned int, std::string> AfterApply = {};
	std::unordered_map<unsigned int, bool> ClearApply = {};
	std::unordered_map<unsigned int, StatementData> StatementSave = {};


	inline std::string GenerateLocal(uint8_t Reg, std::string Value, bool Empty=false)
	{
		if (vLocals.find(Reg) != vLocals.cend())
			return vLocals[Reg].Name;

		std::string Name = "v" + std::to_string(varIndex++);
		if (Empty)
		{
			Local EmptyVar;
			EmptyVar.Name = Name;
			EmptyVar.Empty = true;
			EmptyVar.Used = true;

			vLocals[Reg] = EmptyVar;
			return Name;
		}

		vLocals[Reg] = Local(Reg, Name, Value);
		return Name;
	}

	inline InstructionU GetNextInstruction(Proto& p)
	{
		return p.code[++p.pc];
	}

	inline DecompiledStrc GetInstruction(const uint8_t A)
	{
		if (vLocals.find(A) != vLocals.cend())
			return DecompiledStrc(0, vLocals[A].Name);

		if (vTempInstructions.find(A) != vTempInstructions.cend())
			return vTempInstructions[A];
	}

	inline bool IsAssignment(const uint8_t OpCode)
	{
		return OpCode == U::LOADK || OpCode == U::LOADKX || OpCode == U::GETENV || OpCode == U::GETENVM || OpCode == U::LOADINT || OpCode == U::LOADBOOL || OpCode == U::LOADNIL 
			|| OpCode == U::GETUPVAL || OpCode == U::NEWTABLE || OpCode == U::LOADTABLE || U::GETTABLEK || OpCode == U::GETTABLE || OpCode == U::GETTABLEN || OpCode == U::ADD || OpCode == U::SUB || OpCode == U::MUL
	        || OpCode == U::DIV || OpCode == U::POW || OpCode == U::MOD || OpCode == U::ADDK || OpCode == U::SUBK || OpCode == U::MULK
			|| OpCode == U::DIVK || OpCode == U::POWK || OpCode == U::MODK || OpCode == U::UNM || OpCode == U::LEN || OpCode == U::NOT || OpCode == U::CONCAT;
	}

    std::string GetConstantExpr(Proto& p, const int Idx) // LOADK Only
	{
		const auto Constant = p.k[Idx];
		switch (Constant.type)
		{
		case 1: // boolean
			return Constant.b ? "true" : "false";
		case 2: // number
		{
			char ret[255];
			sprintf(ret, "%.14g", Constant.n);
			return ret;
		}
		case 3: // string
		{
			std::string Result = "\"";

			for (unsigned char chr : Constant.str)
			{
				switch (chr)
				{
				case '"':
					Result += "\\\"";
					break;
				case '\\':
					Result += "\\\\";
					break;
				case '\a':
					Result += "\\a";
					break;
				case '\b':
					Result += "\\b";
					break;
				case '\f':
					Result += "\\f";
					break;
				case '\n':
					Result += ("\\n");
					break;
				case '\r':
					Result += ("\\r");
					break;
				case '\t':
					Result += ("\\t");
					break;
				case '\v':
					Result += ("\\v");
					break;
				default:
					std::setfill(0);
					if (isprint(chr)) Result += chr; 
					else
					{
						std::stringstream ss;
						ss << std::setfill('0') << std::setw(3) << chr;
						Result += ss.str();
					}
					break;
				}
			}
			return Result + "\"";
		}
		default:
			return "nil";
		}
	}

	std::string GetBinaryOperation(InstructionU Instruction)
	{
		switch (Instruction.Opcode)
		{
		case U::ADD:
		case U::ADDK:
			return " + ";
		case U::SUB:
		case U::SUBK:
			return " - ";
		case U::MUL:
		case U::MULK:
			return " * ";
		case U::DIV:
		case U::DIVK:
			return " / ";	
		case U::MOD:
		case U::MODK:
			return " % ";
		case U::POW:
		case U::POWK:
			return " ^ ";
		}
	}

	std::string GetExpression(Proto& p, InstructionU Instruction)
	{
		switch (Instruction.Opcode)
		{
		case U::LOADINT:
			return std::to_string(static_cast<int>(Instruction.sBx));
		case U::LOADK:
			return GetConstantExpr(p, static_cast<int>(Instruction.Bx));
		case U::LOADKX:
			return GetConstantExpr(p, GetNextInstruction(p).Val);
		case U::LOADNIL:
			return "nil";
		case U::LOADBOOL:
			return Instruction.B ? "true" : "false";

		case U::MOVE:
		{
			if (vTempInstructions.find(Instruction.B) == vTempInstructions.cend()) // not found? put nil
				return GenerateLocal(Instruction.B, "nil");

			return GenerateLocal(Instruction.B, GetInstruction(Instruction.B).value);

		}
		case U::GETENVM:
		{
			auto Encoded = GetNextInstruction(p).Val;

			uint32_t Idx = Encoded >> 30;
			uint32_t Idx1 = Idx ? (Encoded >> 20) & 0x3FF : -1;
			uint32_t Idx2 = Idx <= 1 ? -1 : (Encoded >> 10) & 0x3FF;
			uint32_t Idx3 = Idx > 2 ? Encoded & 0x3FF : -1;

			std::string Global = p.k[Idx1].str;

			if (Idx2 != -1 && Idx3 != -1)

				if (Idx2 != -1)
					Global += "." + p.k[Idx2].str;

			if (Idx3 != -1)
				Global += "." + p.k[Idx3].str;

			return Global;
		}
		case U::GETENV:
		{
			return p.k[GetNextInstruction(p).Val].str;
		}
		case U::GETTABLEK:
		{
			return GetInstruction(Instruction.B).value + "." + p.k[GetNextInstruction(p).Val].str;
		}
		case U::GETTABLE:
		{
			return GetInstruction(Instruction.B).value + "[" + GetInstruction(Instruction.C).value + "]";
		}
		case U::GETTABLEN:
		{
			return GetInstruction(Instruction.B).value + "[" + std::to_string(static_cast<double>((Instruction.C + 1))) + "]";
		}
		case U::TESTSETAND:
		{
			return GetInstruction(Instruction.B).value + " and " + GetInstruction(Instruction.C).value;
		}
		case U::TESTSETANDK:
		{
			return GetInstruction(Instruction.B).value + " and " + GetConstantExpr(p, Instruction.C);

		}
		case U::TESTSETOR:
		{
			return GetInstruction(Instruction.B).value + " or " + GetInstruction(Instruction.C).value;
		}
		case U::TESTSETORK:
		{
			return GetInstruction(Instruction.B).value + " or " + GetConstantExpr(p, Instruction.C);
		}
		case U::NOT:
		{
			return "not " + GetInstruction(Instruction.B).value;
		}
		case U::ADD:
		case U::SUB:
		case U::MUL:
		case U::DIV:
		case U::MOD:
		case U::POW:
		{
			const auto Operator = GetBinaryOperation(Instruction);
			printf("Arith Statement non constant with operation: %s\n", Operator.c_str());
			return GetInstruction(Instruction.B).value + Operator + GetInstruction(Instruction.C).value;
		}
		case U::ADDK:
		case U::SUBK:
		case U::MULK:
		case U::DIVK:
		case U::MODK:
		case U::POWK:
		{
			const auto Operator = GetBinaryOperation(Instruction);
			printf("Arith Statement constant with operation: %s\n", Operator.c_str());
			return GetInstruction(Instruction.B).value + Operator + GetConstantExpr(p, Instruction.C);
		}
		}
		return "nil";
	}


	// TODO: Do early call statement so eq,lt,le,lt will know if it's inside call statement
	std::string GetStatement(Proto& p, InstructionU Instruction, unsigned int pc, bool InsideStatement = false)
	{
		const auto o = Instruction.Opcode;
		const auto ArgA = Instruction.A;

		bool Default = false;
		bool InsideLoopStatement = false;
		std::string TempResult = "";



		switch (o)
		{
	
		case U::SETENV:
		{
			const auto Key = p.k[GetNextInstruction(p).Val].str;
			printf("Assign Global Statement with key: %s\n", Key.c_str());
			TempResult += Key + " = " + GetInstruction(ArgA).value + "\n";
			break;
		}
		case U::SETTABLEK:
		{
			const auto Key = p.k[GetNextInstruction(p).Val].str;

			printf("Assign Index Statement with key: %s\n", Key.c_str());

			TempResult += GetInstruction(Instruction.B).value + "." + Key + " = " + GetInstruction(ArgA).value;
			break;
		}
		case U::SETTABLEV:
		{
			printf("Assign Index Statement without constant key!\n");
			TempResult += GetInstruction(Instruction.B).value + "[" + GetInstruction(Instruction.C).value + "] = " + GetInstruction(ArgA).value;
			break;
		}
		case U::SETTABLEN:
		{
			printf("Assign Index Statement with integer key: %d\n", Instruction.C + 1);
			TempResult += GetInstruction(Instruction.B).value + "[" + std::to_string(static_cast<double>((Instruction.C + 1))) + "] = " + GetInstruction(ArgA).value;
			break;
		}
		
		case U::JMP:
		{
			const auto NextInstruction = p.code[pc + 1].Opcode;
			if (Instruction.sBx == 1 && (NextInstruction == U::TFORLOOP || NextInstruction == U::FORLOOP || NextInstruction == U::JMP || NextInstruction == U::JMPH || NextInstruction == U::JMPHX))
				TempResult += "\nbreak";
			break;
		}
		case U::TEST:
		{
			const auto& TestCond = GetInstruction(ArgA);

			if (TestCond.value != "nil")
				ClearApply[TestCond.pc] = true;

			StatementData Save(TestCond.value, 0); // TODO for eq,lt,le,gt,ge etc

			TempResult += "if " + TestCond.value + " then\n";

			const auto Target = p.code[pc + Instruction.sBx];

			if (Target.Opcode == U::JMP || Target.Opcode == U::JMPHX) // else statement
			{
				printf("If else statement!\n");
				const auto ElseStatement = Target.Opcode == U::JMP ? Target.sBx : GETSAx(Target.Val);

				LateApply[pc + Instruction.sBx] = LateStrc("\nelse\n", false);
				LateApply[pc + Instruction.sBx + ElseStatement] = LateStrc("\nend\n", false);
				Save.EndPC = pc + Instruction.sBx + ElseStatement;
				break;
			}
			else if (Target.Opcode == U::RETURN && Target.A == 0 && Target.B == 1)
			{
				printf("If else statement!\n");
				LateApply[pc + Instruction.sBx] = LateStrc("\nelse\n", true);
				LateApply[p.sizecode - 1] = LateStrc("\nend", false);
				Save.EndPC = p.sizecode - 1;
			}
			else // if statement
			{
				LateApply[pc + Instruction.sBx + 1] = LateStrc("\nend", false);
				printf("If statement: %d\n", pc + Instruction.sBx);
				Save.EndPC = pc + Instruction.sBx + 1;
			}

			StatementSave[pc] = Save;
			break;
		}
		case U::TESTNOT:
		{
			const auto& TestCond = GetInstruction(ArgA);

			if (TestCond.value != "nil")
				ClearApply[TestCond.pc] = true;

			StatementData Save(TestCond.value, 0); // TODO for eq,lt,le,gt,ge etc
			printf("pc: %d\n", pc);
			TempResult += "if not " + TestCond.value + " then\n";

			const auto Target = p.code[pc + Instruction.sBx];

			if (Target.Opcode == U::JMP || Target.Opcode == U::JMPHX) // else statement
			{
				printf("If not else statement!\n");
				const auto ElseStatement = Target.Opcode == U::JMP ? Target.sBx : GETSAx(Target.Val);

				LateApply[pc + Instruction.sBx] = LateStrc("\nelse\n", false);
				LateApply[pc + Instruction.sBx + ElseStatement] = LateStrc("\nend\n", false);
				Save.EndPC = pc + Instruction.sBx + ElseStatement;
				break;
			}
			else if (Target.Opcode == U::RETURN && Target.A == 0 && Target.B == 1)
			{
				printf("If not else statement!\n");
				LateApply[pc + Instruction.sBx] = LateStrc("\nelse\n", true);
				LateApply[p.sizecode - 1] = LateStrc("\nend", false);
				Save.EndPC = p.sizecode - 1;
			}
			else // if statement
			{
				LateApply[pc + Instruction.sBx + 1] = LateStrc("\nend", false);
				printf("If not statement: %d\n", pc + Instruction.sBx + 1);
				Save.EndPC = pc + Instruction.sBx + 1;
			}

			StatementSave[pc] = Save;
			break;
		}
		
		case U::JMPH:
		{
			const auto Sbx = Instruction.sBx;
			if (Sbx < 0) // Invert jump and maybe while statement or repeat stat
			{
				const auto IsRepeat = p.code[pc - 1];
				if (IsRepeat.Opcode == U::TEST || IsRepeat.Opcode == U::TESTNOT)
				{
					printf("Repeat statement with condition!\n");
					printf("pc repeat: %d\n", pc);
					const auto StatData = StatementSave[pc - 1];
					AfterApply[pc + Sbx] = "repeat\n";

					TempResult += "\nuntil(" + StatData.Condition + ")\n";
					ClearApply[pc - 1] = true;
					ClearApply[StatData.EndPC + 1] = true;
					break;
				}

				const auto Target = pc - (pc + Sbx + 1);
				int HasCondition = 0;

				std::string Condition = "while ";

				for (int InBlock = 1; InBlock <= Target; InBlock++)
				{
					const auto Instr = p.code[InBlock];
					if (Instr.Opcode == U::TEST || Instr.Opcode == U::TESTNOT) // TODO another condition like eq,lt,le,gt,ge
					{
						printf("While statement with condition!\n");
						HasCondition = InBlock;
						Condition += StatementSave[InBlock].Condition + " do\n";
						ClearApply[InBlock] = true;
						break;
					}
				}

				const auto TargetInstr = p.code[pc + Sbx + 1];
				if (TargetInstr.Opcode == U::TEST || TargetInstr.Opcode == U::TESTNOT) // TODO another condition like eq,lt,le,gt,ge
				{
					printf("While statement with condition!\n");
					HasCondition = pc + Sbx + 1;
					Condition += StatementSave[pc + Sbx + 1].Condition + " do\n";
					ClearApply[pc + Sbx + 1] = true;
				}

				if (!HasCondition)
				{
					printf("While statement with without condition: %d\n", pc);
					Condition += "true do\n";
					TempResult += "end\n";
				}

				AfterApply[pc + Sbx] = Condition;
			}

			break;
		}
		case U::FORPREP:
		{
			printf("For statement!\n");
			std::string Name = "v" + std::to_string(varIndex++);
		
			TempResult += "for " + Name + " = " + GetInstruction(ArgA + 2).value + ", " + GetInstruction(ArgA).value;
			if (GetInstruction(ArgA + 1).value != "1")
				TempResult += ", " + GetInstruction(ArgA + 1).value;

			TempResult += " do\n";

			vLocals[ArgA + 2] = Local(ArgA + 2, Name, "");
			vLocals[ArgA + 2].Used = false;

			InsideLoopStatement = true;
			break;
		}
		case U::FORLOOP:
		{
			TempResult += "\nend";
			InsideLoopStatement = false;
			printf("end!\n");
			break;
		}
		// TODO: Fix call
		case U::CALL:
		{
			const auto ArgB = Instruction.B == 0 ? p.maxstacksize - ArgA : Instruction.B;
		    auto Arguments = ArgA + ArgB - 1; // 1 is function

		

			printf("Base: %d\n", ArgA);

			if (Instruction.C == 1 && Instruction.B == 0)
				Arguments -= 1;

			printf("Args: %d Func: %s\n", Arguments, GetInstruction(ArgA).value.c_str());

			std::string TempCall =  GetInstruction(ArgA).value + "("; // function
			for (int a = ArgA + 1; a <= Arguments; a++)
			{
				printf("Call Statement with reg: %d\n", a);

				const auto Arg = GetInstruction(a);


				if (Arg.value != "nil")
					ClearApply[Arg.pc] = true;

				TempCall += Arg.value;

				if (a != Arguments)
					TempCall += ", ";
			}
			TempCall +=  ")";

			if (Instruction.C != 2) // Variable set
				TempResult += TempCall;

		/*	if (!InsideStatement)
				ss << "";*/

			break;
		}
		case U::RETURN:
		{
			const auto Values = Instruction.B - 1;
			if (!Values)
			{
				if (pc == p.sizecode - 1)
					break;
				TempResult += "\nreturn";
				break;
			}
			
			TempResult += "\nreturn ";

			for (int a = 0; a < Values; a++)
			{
				TempResult += GetInstruction(ArgA + a).value;
				if (a != Values-1)
					TempResult += ", ";
			}
			break;
		}
		case U::INIT:
		case U::VAINIT:
			break;
		default:
			vTempInstructions[ArgA] = DecompiledStrc(pc, GetExpression(p, Instruction));
			Default = true;
			break;
		}

		if (!Default && o == U::CALL)
		{
			//printf("yes call appended: %s\n", TempResult.c_str());
			vTempInstructions[ArgA] = DecompiledStrc(pc, TempResult);

		}

		return TempResult;
	}

	// todo: fix for arguments
	std::map<int, std::string> VariableFinder(Proto target)
	{
		const auto args = target.numparams;
		const auto registers = target.maxstacksize;

		auto states = RegisterStates(registers, target.sizecode);
		bool* skip = (bool*)malloc(target.sizecode);

		for (int line = 0; line < target.sizecode; line++) {
			if (skip[line - 1]) continue;

			const auto Instruction = target.code[line];

			switch (Instruction.Opcode)
			{

			case U::TESTSETAND:
			case U::TESTSETOR:
				states.get(Instruction.A, line).written = true;
				states.get(Instruction.B, line).read = true;
				states.get(Instruction.C, line).read = true;
				break;
			case U::TESTSETORK:
			case U::TESTSETANDK:
				states.get(Instruction.A, line).written = true;
				states.get(Instruction.B, line).read = true;
				break;
			case U::GETENV:
			case U::GETENVM:
			case U::GETUPVAL:
			case U::NEWTABLE:
			case U::LOADTABLE:
			case U::LOADBOOL:
			case U::LOADNIL:
			case U::LOADK:
			case U::LOADKX:
			case U::LOADINT:
				states.get(Instruction.A, line).written = true;
				break;
			case U::MOVE:
				states.get(Instruction.A, line).written = true;
				states.get(Instruction.B, line).read = true;
				states.setLocal(min(Instruction.A, Instruction.B), line);
				break;
			case U::GETTABLE:
				states.get(Instruction.A, line).written = true;
				states.get(Instruction.B, line).read = true;
				states.get(Instruction.C, line).read = true;
				break;
			case U::GETTABLEN:
			case U::GETTABLEK:
				states.get(Instruction.A, line).written = true;
				states.get(Instruction.B, line).read = true;
				break;
			case U::SETUPVAL:
			case U::SETENV:
				states.get(Instruction.A, line).read = true;
				break;
			case U::SETTABLEN:
			case U::SETTABLEK:
				states.get(Instruction.A, line).read = true;
				states.get(Instruction.B, line).read = true;
				break;
			case U::ADD:
			case U::SUB:
			case U::MUL:
			case U::DIV:
			case U::MOD:
			case U::POW:
			case U::SETTABLEV:
				states.get(Instruction.A, line).read = true;
				states.get(Instruction.B, line).read = true;
				states.get(Instruction.C, line).read = true;
				break;

			case U::ADDK:
			case U::SUBK:
			case U::MULK:
			case U::DIVK:
			case U::MODK:
			case U::POWK:
				states.get(Instruction.A, line).read = true;
				states.get(Instruction.B, line).read = true;
				break;
			case U::UNM:
			case U::NOT:
			case U::LEN:
				states.get(Instruction.A, line).written = true;
				states.get(Instruction.B, line).read = true;
				break;
			case U::CONCAT:
				states.get(Instruction.A, line).written = true;
				for (int reg = Instruction.B; reg <= Instruction.C; reg++) {
					states.get(reg, line).read = true;
					states.setTemporary(reg, line);
				}
				break;
			case U::EQ:
			case U::LE:
			case U::LT:
			case U::NEQ:
			case U::GT:
			case U::GE:
				states.get(Instruction.A, line).read = true;
				states.get(target.code[++line].Val, line-1).read = true;
				break;
			case U::TEST:
			case U::TESTNOT:
				states.get(Instruction.A, line).read = true;
				break;
			case U::CALL:
			{
				const int A = Instruction.A;
				const int B = Instruction.B;
				const int C = Instruction.C;

				if (C >= 2) {
					for (int reg = A; reg <= A + C - 2; reg++) {
						states.get(reg, line).written = true;
					}
				}
				
				for (int reg = A; reg <= A + B - 1; reg++) {
					states.get(A, line).read = true;
					states.setTemporary(A, line);
				}
				if (C >= 2) {
					int nline = line + 1;
					int reg = A + C - 2;
					while (reg >= A && nline <= target.sizecode) {
						const auto tnline = target.code[nline];
						if (tnline.Opcode == U::MOVE && tnline.B == reg) {
							states.get(tnline.A, nline).written = true;
							states.get(tnline.B, nline).read = true;
							states.setLocal(tnline.A, nline);
							skip[nline - 1] = true;
						}
						reg--;
						nline++;
					}
				}
				break;
			}
			}

		}

		free(skip);

		std::map<int, std::string> Guessed;

		for (int reg = 0; reg < registers; reg++) {
			std::string id = "v";
			bool local = false;
			bool temporary = false;
			int read = 0;
			int written = 0;
			if (reg < args) {
				local = true;
				id = "a";
			}
			if (!local && !temporary) {
				for (int line = 0; line < target.sizecode; line++) {
					RegisterState state = states.get(reg, line);
					if (state.local) local = true;
					if (state.temporary) temporary = true;
					if (state.read) read++;
					if (state.written) written++;
				}
			}
			if (!local && !temporary) {
				if (read >= 2 || read == 0) {
					local = true;
				}
			}
			if (local) {
				Guessed[reg] = id + std::to_string(varIndex++);
			}
		}
		return Guessed;
	}

	std::string GetBlock(Proto target)
	{
		auto& pc = target.pc;
		auto& code = target.code;

		std::map<unsigned int, std::string> Decompiled;



		for (pc = 0; pc < target.sizecode; pc++)
		{
			const auto Instruction = code[pc];
	

			if (LateApply.find(pc) != LateApply.cend())
			{
				const auto Apply = LateApply[pc];
				//TempResult += Apply.Apply;
				Decompiled.insert({ pc  + 1, Apply.Apply });
				LateApply.erase(pc);
				if (Apply.skip)
					continue;
			}

			Decompiled[pc] = GetStatement(target, Instruction, pc, false);

		}

		for (auto& [Pc, Clear] : ClearApply)
		{
			if (Clear && Decompiled.find(Pc) != Decompiled.cend()) // found
			{
				//printf("Erasing pc: %d\n", Pc);
				Decompiled.erase(Pc);

			}
		}

		for (auto& [Pc, Value] : AfterApply)
		{
			printf("After apply found on pc: %d\n", Pc);
			if (Decompiled.find(Pc) != Decompiled.cend()) // found
			{
				Decompiled.insert({ Pc + 1, Value });

			}
		}


		std::string FinalResult = "";


		//for (const auto& [reg, varname] : VariableFinder(target))
		//{
		//	if (vLocals.find(reg) == vLocals.cend()) // not found
		//	{
		//		printf("Guessed local at reg: %d\n", reg);
		//		FinalResult += "local " + varname + " = " + vTempInstructions[reg].value + "\n";
		//	}
		//}

		for (const auto& local : vLocals)
		{
			const auto object = local.second;
			if (object.Used && !object.Empty)
			{
				FinalResult += "local " + object.Name + " = " + object.Value + "\n";
			}
		}

		for (const auto& [Reg, Out] : Decompiled)
		{
			//printf("PC: %d Val: %s\n", Reg, Out.c_str());
			FinalResult += Out;

		}

		return FinalResult;
	}
};

