module Ram #(parameter depth = 16,
             parameter data_width = 32,
             parameter addr_width = 4) (

input wire        write_en, read_en, CLK, RST,
input wire [(data_width-1):0] data_in,
input wire [(addr_width-1):0] addr,
output reg [(data_width-1):0] data_out,
output reg		      valid_out

);

integer i;


reg  [(data_width-1):0] memory [0:(depth-1)];

always@(posedge CLK)
  begin
    if(!RST)
      begin
        data_out <= 'h0000;
	valid_out <= 'h0;
        for (i=0; i<depth; i=i+1)
          begin
             memory[i] = 'h0;
          end
        
      end
    else if (write_en && !read_en)
      begin
        memory[addr] <= data_in;
	valid_out <= 'h0;
      end

    else if (!write_en && read_en)
     begin
        data_out <= memory[addr];
	valid_out <= 'h1;
     end
  end

endmodule
