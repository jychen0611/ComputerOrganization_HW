module traffic_light (
    input  clk,
    input  rst,
    input  pass,
    output reg R,
    output reg G,
    output reg Y
);

//write your code here
`define TimeExpire1 32'd25000000
`define TimeExpire2 32'd2500


//clk division (1 second)
/*reg div_clk_s;
reg [31:0] count_s;

always@(posedge clk or posedge rst)begin
	if(!rst)begin
		count_s <= 32'd0;
		div_clk_s <= 1'b0;
	end
	else begin
		if(count_s == `TimeExpire1)begin
			count_s <= 32'd0;
			div_clk_s <= ~div_clk_s;
		end
		else begin
			count_s <= count_s + 32'd1;
		end
	end
end
*/

//traffic light design
reg [2:0] current_state; //0:green 1:yellow 2:red
reg [10:0] count_traffic; //green:1-15 yellow:1-5 red:1-10

initial begin
    R = 0;
    G = 1;
    Y = 0;
    current_state <= 3'd0;
    count_traffic <= 1;
end


always@(posedge clk or posedge rst)begin

    if(rst)
    begin
		current_state <= 3'd0; //s0
		count_traffic <= 1;
		G=1;
		Y=0;
		R=0;
    end

    else
    begin
    if( !pass ) count_traffic <= count_traffic + 1;

	case(current_state)
	3'b000:begin //0:Green
		G=1;
		Y=0;
		R=0;
		if(count_traffic == 1024)begin 
			G=0;
			Y=0;
			R=0;
            current_state <= 3'b001;
            count_traffic <= 1;
		end
	end
	3'b001:begin //None
		if(pass==1)begin
			current_state = 3'b000;
		    count_traffic <= 2;
			G=1;
			Y=0;
			R=0;
	        end
		else if(count_traffic == 128)begin //change to reg
			G=1;
			Y=0;
			R=0;
            current_state <= 3'b010;
            count_traffic <= 1;
		end
		else begin
			G=0;
			Y=0;
			R=0;
		end
	end
	3'b010:begin //Green
		if(pass==1)begin
			current_state=3'b000;
		    count_traffic <= 2;
			G=1;
			Y=0;
			R=0;
	        end
		else if(count_traffic == 128)begin //change to green
			G=0;
			Y=0;
			R=0;
            current_state <= 3'b011;
            count_traffic <= 1;
		end
		else begin
			G=1;
			Y=0;
			R=0;
		end
	end
	3'b011:begin //None
		if(pass==1)begin
			current_state=3'b000;
		    count_traffic <= 2;
			G=1;
			Y=0;
			R=0;
	        end
		else if(count_traffic == 128)begin //change to green
			G=1;
			Y=0;
			R=0;
            current_state <= 3'b100;
            count_traffic <= 1;
		end
		else begin
			G=0;
			Y=0;
			R=0;
		end
	end
	3'b100:begin //Green
		if(pass==1)begin
			current_state=3'b000;
		    count_traffic <= 2;
			G=1;
			Y=0;
			R=0;
	        end
		else if(count_traffic == 128)begin //change to yellow
			G=0;
			Y=1;
			R=0;
            current_state <= 3'b101;
            count_traffic <= 1;
		end
		else begin
			G=1;
			Y=0;
			R=0;
		end
	end
	3'b101:begin //Yellow
		if(pass==1)begin
			current_state=3'b000;
		    count_traffic <= 2;
			G=1;
			Y=0;
			R=0;
	        end
		else if(count_traffic == 512)begin //change to red
			G=0;
			Y=0;
			R=1;
            current_state <= 3'b110;
            count_traffic <= 1;
		end
		else begin
			Y=1;
			G=0;
			R=0;
		end
	end
	3'b110:begin //red
		if(pass==1)begin
			current_state=3'b000;
		    count_traffic <= 2;
			G=1;
			Y=0;
			R=0;
	        end
		else if(count_traffic == 1024)begin //change to green
			G=1;
			Y=0;
			R=0;
            current_state <= 3'b000;
            count_traffic <= 1;
		end
		else begin
			R=1;
			Y=0;
			G=0;
		end
	end
		
	endcase
    
end
end


endmodule
