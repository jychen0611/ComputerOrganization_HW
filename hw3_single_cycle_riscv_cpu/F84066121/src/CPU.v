
module ControlUnit(
	input [6:0] opcode,
	input [2:0] funct3,
	output reg r1_able,r2_able, rd_able, JorU_type,
	output reg is_ALU, is_shift, MemRe, MemWr, ALUSrc,
	output reg [1:0] special_type, rdSrc, PCSrc,
	output reg [2:0] ALUop, ImmType
); 
initial begin
	MemRe = 0;
end
always@(*)
begin
	case(opcode)
		7'b0110011:
		begin
			r1_able = 1; r2_able = 1; rd_able = 1; ALUSrc = 0;
			ImmType = 3'b000;
			JorU_type = 0; special_type = 2'b00;
			PCSrc = 2'b00;	MemRe = 0; MemWr = 0;
			case(funct3)				
				3'b000,3'b100,3'b110,3'b111: begin
					is_ALU = 1;
					is_shift = 0;
					rdSrc = 2'b00;
					ALUop = funct3;					
				end
				3'b001,3'b101: begin
					is_ALU = 0;
					is_shift = 1;
					rdSrc = 2'b01;
					ALUop = 3'b000;
				end
				3'b010,3'b011: begin
					is_ALU = 1;
					is_shift = 0;
					rdSrc = 2'b00;
					ALUop = 3'b011;
				end
			endcase
		end
		7'b0000011: //LW
		begin
			r1_able = 1; r2_able = 0; rd_able = 1; ALUSrc = 1;
			ALUop = 3'b000; ImmType = 3'b001;
			JorU_type = 0; special_type = 2'b00;
			is_ALU = 1; is_shift = 0; rdSrc = 2'b11;
			PCSrc = 2'b00; MemRe = 1; MemWr = 0;		
		end
		7'b0010011: //imm
		begin
			r1_able = 1; r2_able = 0; rd_able = 1; ALUSrc = 1;
			JorU_type = 0; special_type = 2'b00;			
			PCSrc = 2'b00; MemRe = 0; MemWr = 0;
			case(funct3)				
				3'b000,3'b100,3'b110,3'b111: begin
					ImmType = 3'b001;
					is_ALU = 1;
					is_shift = 0;
					rdSrc = 2'b00;
					ALUop = funct3;
				end
				3'b001,3'b101: begin
					ImmType = 3'b010;
					is_ALU = 0;
					is_shift = 1;
					rdSrc = 2'b01;
					ALUop = 3'b000;
				end
				3'b010,3'b011: begin
					ImmType = 3'b001;
					is_ALU = 1;
					is_shift = 0;
					rdSrc = 2'b00;
					ALUop = 3'b011;
				end
			endcase
		end
		7'b1100111: //JALR
		begin
			r1_able = 1; r2_able = 0; rd_able = 1; ALUSrc = 0;
			ALUop = 3'b000; ImmType = 3'b001;
			JorU_type = 1; special_type = 2'b00;
			is_ALU = 0; is_shift = 0; rdSrc = 2'b10;
			PCSrc = 2'b10; MemRe = 0; MemWr = 0;
		end
		7'b0100011: //SW
		begin
			r1_able = 1; r2_able = 1; rd_able = 0; ALUSrc = 1;
			ALUop = 3'b000; ImmType = 3'b011;
			JorU_type = 0; special_type = 2'b00;
			is_ALU = 1; is_shift = 0; rdSrc = 2'b00;
			PCSrc = 2'b00; MemRe = 0; MemWr = 1;
		end
		7'b1100011: //branch
		begin
			r1_able = 1; r2_able = 1; rd_able = 0; ALUSrc = 0;
			ImmType = 3'b100;			
			JorU_type = 0; special_type = 2'b00;
			is_ALU = 1; is_shift = 0; rdSrc = 2'b00;
			PCSrc = 2'b01; MemRe = 0; MemWr = 0;
			case(funct3)				
				3'b000:					
					ALUop = 3'b001;
				3'b001:
					ALUop = 3'b010;
				3'b100,3'b110:
					ALUop = 3'b011;
				default:
					ALUop = 3'b101;
			endcase
		end
		7'b0010111: //AUIPC
		begin
			r1_able = 0; r2_able = 0; rd_able = 1; ALUSrc = 0;
			ALUop = 3'b000; ImmType = 3'b101;
			JorU_type = 1; special_type = 2'b01;
			is_ALU = 0; is_shift = 0; rdSrc = 2'b10;
			PCSrc = 2'b10; MemRe = 0; MemWr = 0;
		end
		7'b0110111: //LUI
		begin
			r1_able = 0; r2_able = 0; rd_able = 1; ALUSrc = 0;
			ALUop = 3'b000; ImmType = 3'b101;
			JorU_type = 1; special_type = 2'b10;
			is_ALU = 0; is_shift = 0; rdSrc = 2'b10;
			PCSrc = 2'b10; MemRe = 0; MemWr = 0;
		end
		default: //JAL
		begin
			r1_able = 0; r2_able = 0; rd_able = 1; ALUSrc = 0;
			ALUop = 3'b000; ImmType = 3'b110;
			JorU_type = 1; special_type = 2'b11;
			is_ALU = 0; is_shift = 0; rdSrc = 2'b10;
			PCSrc = 2'b10; MemRe = 0; MemWr = 0;
		end
	endcase
end
endmodule

module ALU(
	input is_ALU, is_sub, is_branch, unsign_sl, unsign_branch,
	input [2:0] ALUop, ImmType,
	input [31:0] a, b,
	output reg [31:0] result
);

always@(*)
begin
	if(is_ALU)
		case(ALUop)
			3'b000:
				if(is_sub && ImmType == 3'b000)
					result = a - b;
				else
					result = a + b;
			3'b001:
				result = (a == b)? 32'd1 : 32'd0;
			3'b010:
				result = (a != b)? 32'd1 : 32'd0;
			3'b011:
				if(is_branch)
					if(unsign_branch)
						result = ($unsigned(a) < $unsigned(b))? 32'd1 : 32'd0;
					else
						result = ($signed(a) < $signed(b))? 32'd1 : 32'd0;
				else
					if(unsign_sl)
						result = ($unsigned(a) < $unsigned(b))? 32'd1 : 32'd0;
					else
						result = ($signed(a) < $signed(b))? 32'd1 : 32'd0;
			3'b100:			
				result = a ^ b;
			3'b101:
				if(unsign_branch)
					result = ($unsigned(a) >= $unsigned(b))? 32'd1 : 32'd0;
				else
					result = ($signed(a) >= $signed(b))? 32'd1 : 32'd0;
			3'b110:
				result = a | b;
			3'b111:
				result = a & b;
		endcase
	else
		result = 0;
end
endmodule

module RegFile(
	input clk, r1_able, r2_able, rd_able, data_read, MemRe,
	input [4:0] rs1_addr, rs2_addr, rd_addr,
	input [31:0] rd_data,
	output [31:0]  rs1_data, rs2_data
);
reg [31:0] register [31:0];

integer i;
initial begin
	for(i = 0; i < 32; i = i + 1)
		register[i] = 32'd0;
end

assign rs1_data = r1_able? register[rs1_addr] : 0;
assign rs2_data = r2_able? register[rs2_addr] : 0;

always@(negedge clk)
begin
	if(MemRe && data_read && rd_addr)
		register[rd_addr] <= rd_data;
	else if(~MemRe & rd_able && rd_addr)
		register[rd_addr] <= rd_data;
end
endmodule

module DealImme(
	input [2:0] ImmType,
	input [24:0] first25,	
	output reg [31:0] result
);
reg is_12or20, extend_way;
reg [11:0] result12;
reg [19:0] result20;

always@(*)
begin
	case(ImmType)
		3'b001: begin
			result12 = first25[24:13];
			result20 = 0;
			is_12or20 = 0;
			extend_way = 0;
		end		
		3'b011: begin
			result12 = {first25[24:18],first25[4:0]};
			result20 = 0;
			is_12or20 = 0;
			extend_way = 0;
		end
		3'b100: begin
			result12 = {first25[24],first25[0],first25[23:18],first25[4:1]};
			result20 = 0;
			is_12or20 = 0;
			extend_way = 1;
		end
		3'b101: begin
			result20 = first25[24:5];
			result12 = 0;
			is_12or20 = 1;
			extend_way = 0;
		end
		3'b110: begin
			result20 = {first25[24],first25[12:5],first25[13],first25[23:14]};
			result12 = 0;
			is_12or20 = 1;
			extend_way = 1;
		end
		default: begin
			extend_way = 0;
			is_12or20 = 0;
			result12 = 0;
			result20 = 0;
		end
	endcase
end

always@(*)
begin
	if(!is_12or20) //12
		if(!extend_way) //11:0
			if(result12[11] == 0) 
				result = {20'b00000000000000000000, result12};
			else
				result = {20'b11111111111111111111, result12};
		else //12:1
			if(result12[11] == 0) 
				result = {19'b0000000000000000000, result12, 1'b0};
			else
				result = {19'b1111111111111111111, result12, 1'b0};
	else //20
		if(!extend_way) //31:12
			result = {result20,12'b000000000000};
		else //20:1
			if(result20[19] == 0) 
				result = {12'b000000000000, result20, 1'b0};
			else
				result = {12'b111111111111, result20, 1'b0};
end

endmodule

module Shift(
	input is_shift, sign, right,
	input [4:0] shamt,
	input [31:0] origin,	
	output reg [31:0] result
);
always@(*)
begin
	if(is_shift)
		if(!right)
			result = origin << shamt;
		else if(sign)
			result = $signed(origin) >>> shamt;		  
		else                    
			result = origin >> shamt;
	else
		result = 0;
end
endmodule

module PC(
    input clk, instr_read,
    input [31:0] new_addr,
    output reg [31:0] PC_addr
);
initial
	PC_addr = 0;
always@(negedge clk)
begin
	if(instr_read) begin
		PC_addr <= new_addr; 
	end
end     
endmodule

module PCMux(
	input branch_flag,
	input [1:0] PCSrc, 
	input [31:0] old_PC, imm, JorU_PC,
	output reg [31:0] new_PC
); 	 
always@(*)
begin  
	if(PCSrc == 2'b00)
		new_PC = old_PC + 4;   
	else if(PCSrc == 2'b01)
		if(branch_flag)
			new_PC = old_PC + imm;
		else
			new_PC = old_PC + 4;
	else
		new_PC = JorU_PC;
end   
endmodule

module DealJorU(
	input JorU_type,
	input [1:0] type,
	input [31:0] old_PC, imm, rs1,
	output reg [31:0] rd, new_PC
);
always@(*)
begin
	if(JorU_type)
		case(type)
			2'b00: begin //JALR
				rd = old_PC + 4;
				new_PC = imm + rs1;
			end
			2'b01: begin //AUIPC
				rd = old_PC + imm;
				new_PC = old_PC + 4;
			end
			2'b10: begin //LUI
				rd = imm;
				new_PC = old_PC + 4;
			end
			2'b11: begin //JAL
				rd = old_PC + 4;
				new_PC = old_PC + imm;
			end
		endcase
	else begin
		rd = 0;
		new_PC = 0;
	end
end
endmodule

module ShamtMux(
	input is_shift, shift_src,
	input [4:0] from_rs2, from_imm,
	output reg [4:0] shamt
);
always@(*)
begin
	if(is_shift)
		if(shift_src)
			shamt = from_rs2;
		else
			shamt = from_imm;
	else
		shamt = 0;
end
endmodule

module rdMux(
	input rd_able,
	input [1:0] rdSrc,
	input [31:0] from_ALU, from_shift, from_JorU, from_memory,	
	output reg [31:0] rd
);
initial
	rd = 0;
always@(*)
begin
	if(rd_able)
		case(rdSrc)
			2'b00:
				rd = from_ALU;
			2'b01:
				rd = from_shift;
			2'b10:
				rd = from_JorU;
			2'b11:
				rd = from_memory;
		endcase
	else
		rd = 0;
end
endmodule

module ALUMux(
	input is_ALU, ALUSrc,
	input [31:0] imm, rs2,	
	output reg [31:0] second_num
);
always@(*)
begin
	if(is_ALU)
		if(ALUSrc)
			second_num = imm;
		else
			second_num = rs2;
	else
		second_num = 0;
end
endmodule

module DealLoad(
	input MemRe,
	input [2:0] funct3,
	input [31:0] load_data,
	output reg [31:0] out_data
);
always@(*)
begin
	if(MemRe)
		case(funct3)
			3'b010:
				out_data = load_data;
			3'b000: //bs
				if(load_data[7])
					out_data = {24'b111111111111111111111111,load_data[7:0]};
				else
					out_data = {24'b000000000000000000000000,load_data[7:0]};
			3'b001: //hs
				if(load_data[15])
					out_data = {16'b1111111111111111,load_data[15:0]};
				else
					out_data = {16'b0000000000000000,load_data[15:0]};
			3'b100: //bu
				out_data = {24'b000000000000000000000000,load_data[7:0]};
			default: //hu
				out_data = {16'b0000000000000000,load_data[15:0]};
		endcase
	else
		out_data = 0;
end
endmodule

module DealStore(
	input MemWr,
	input [1:0] funct3_last2, addr_last2,
	input [31:0] rs2_data,
	output reg [3:0] write4,
	output reg [31:0] data_in
);
wire [4:0] shamt;
assign shamt = addr_last2 << 3;

initial
	write4 = 0;
	
always@(*)
begin
	if(MemWr)
		case(funct3_last2)
			2'b00:
				case(addr_last2)
					2'b00:
						write4 = 4'b0001;
					2'b01:
						write4 = 4'b0010;
					2'b10:
						write4 = 4'b0100;
					2'b11:
						write4 = 4'b1000;
				endcase
			2'b01:
				case(addr_last2)
					2'b00:
						write4 = 4'b0011;
					2'b01:
						write4 = 4'b0110;
					default:
						write4 = 4'b1100;
				endcase
			default:
				write4 = 4'b1111;
		endcase
	else
		write4 = 4'b0000;		
end

always@(*)
begin
	if(MemWr)
		data_in = rs2_data << shamt;
	else
		data_in = 0;
end

endmodule

module DealInstrRead(
	input clk, use_memory,
	output reg instr_read
);
always@(negedge clk)
begin
	if(use_memory)
		instr_read <= ~instr_read;
	else
		instr_read <= 1;
end
endmodule

module DealDataRead(
	input clk, MemRe,
	output reg data_read
);
always@(negedge clk)
begin
	if(MemRe)
		data_read <= ~data_read;
	else
		data_read <= 0;
end
endmodule

module DataAddr(
	input use_memory,
	input [31:0] ALU_result,
	output [31:0] data_addr
);
assign data_addr = use_memory ? ALU_result : 0;
endmodule

module UseMem(
	input MemWr, MemRe,
	output use_memory
);
assign use_memory = MemWr | MemRe;
endmodule

module StoreInstr(
	input instr_read,
	input [31:0] instr_in,
	output [31:0] instr_out
);
reg [31:0] instr_reg;
assign instr_out = instr_reg;

always@(instr_in)
begin
	if(instr_read)
		instr_reg = instr_in;
end
endmodule

module CPU(
    input             clk,
    input             rst,
    input      [31:0] data_out,
    input      [31:0] instr_out,
    output		   instr_read,
    output 		   data_read,
    output [31:0] instr_addr,
    output [31:0] data_addr,
    output reg[3:0]  data_write,
    output reg[31:0] data_in
);

wire r1_able, r2_able, rd_able, JorU_type, is_ALU, is_shift, MemWr;
wire ALUSrc, use_memory, data_read_wire, instr_read_wire, MemRe;
wire [1:0] special_type, rdSrc, PCSrc;
wire [2:0] ALUop, ImmType;
wire [4:0] shamt_result;
wire [31:0] rs1_data, rs2_data, second_num, JorU_PC, addr_result;
wire [31:0] ALU_result, rd_JorU, imm_result, shift_result, rd_result;
wire [31:0] load_final_data, instr_addr_wire, instr_wire;

assign instr_read = instr_read_wire;
assign data_read = data_read_wire;
assign instr_addr = instr_addr_wire;

ControlUnit my_ControlUnit(.opcode(instr_wire[6:0]),.funct3(instr_wire[14:12]),
.r1_able(r1_able), .r2_able(r2_able), .rd_able(rd_able), .JorU_type(JorU_type),
.is_ALU(is_ALU), .is_shift(is_shift), .MemRe(MemRe), .MemWr(MemWr),
.ALUSrc(ALUSrc), .special_type(special_type), .rdSrc(rdSrc), .PCSrc(PCSrc),
.ALUop(ALUop), .ImmType(ImmType));

ALU my_ALU(.is_ALU(is_ALU), .is_sub(instr_wire[30]), .is_branch(instr_wire[6]),
.unsign_sl(instr_wire[12]), .unsign_branch(instr_wire[13]), .ALUop(ALUop),
.ImmType(ImmType), .a(rs1_data), .b(second_num), .result(ALU_result));

RegFile my_RegFile(.clk(clk), .r1_able(r1_able), .r2_able(r2_able), 
.rd_able(rd_able), .rs1_addr(instr_wire[19:15]), .rs2_addr(instr_wire[24:20]), 
.rd_addr(instr_wire[11:7]), .rd_data(rd_result), .MemRe(MemRe),
.rs1_data(rs1_data), .rs2_data(rs2_data), .data_read(data_read_wire));

DealImme my_DealImme(.ImmType(ImmType), .first25(instr_wire[31:7]),	
.result(imm_result));

Shift my_Shift(.is_shift(is_shift), .sign(instr_wire[30]), 
.right(instr_wire[14]), .shamt(shamt_result), .origin(rs1_data),	
.result(shift_result));

PC my_PC(.clk(clk), .instr_read(instr_read_wire), .new_addr(addr_result),
.PC_addr(instr_addr_wire));    

PCMux my_PCMux(.branch_flag(ALU_result[0]), .PCSrc(PCSrc),
.old_PC(instr_addr_wire), .imm(imm_result), .JorU_PC(JorU_PC),
.new_PC(addr_result));

DealJorU my_DealJorU( .JorU_type(JorU_type), .type(special_type),
.old_PC(instr_addr_wire), .imm(imm_result), .rs1(rs1_data),
.rd(rd_JorU), .new_PC(JorU_PC));

ShamtMux my_ShamtMux(.is_shift(is_shift), .shift_src(instr_wire[5]),
.from_rs2(rs2_data[4:0]), .from_imm(instr_wire[24:20]), .shamt(shamt_result));

rdMux my_rdMux(.rd_able(rd_able), .rdSrc(rdSrc), .from_ALU(ALU_result), 
.from_shift(shift_result), .from_JorU(rd_JorU), .from_memory(load_final_data),	
.rd(rd_result));

ALUMux my_ALUMux(.is_ALU(is_ALU), .ALUSrc(ALUSrc), .imm(imm_result),
.rs2(rs2_data), .second_num(second_num));

DealLoad my_DealLoad(.MemRe(MemRe), .funct3(instr_wire[14:12]),
.load_data(data_out), .out_data(load_final_data));

DealStore my_DealStore(.MemWr(MemWr), .funct3_last2(instr_wire[13:12]),
.addr_last2(ALU_result[1:0]), .rs2_data(rs2_data), .write4(data_write),
.data_in(data_in));

StoreInstr my_StoreInstr( .instr_read(instr_read_wire), .instr_in(instr_out), 
.instr_out(instr_wire));

DealDataRead my_DealDataRead(clk, MemRe, data_read_wire);
DealInstrRead my_DealInstrRead(clk, use_memory,	instr_read_wire);
DataAddr my_DataAddr(use_memory, ALU_result, data_addr);
UseMem my_UseMem(MemWr, MemRe, use_memory);






