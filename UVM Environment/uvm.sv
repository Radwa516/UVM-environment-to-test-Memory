`include "uvm_macros.svh"
`timescale 1ns/1ps  
import uvm_pkg::*;

package pack1;

`include "uvm_macros.svh"
import uvm_pkg::*;


///////////////////////////////////////////////////////////////////////////////////////////////////////////////
//////////////////////////////////////////////////Radwa_sequence_item//////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////////////////////
class Radwa_sequence_item extends uvm_sequence_item;

	`uvm_object_utils (Radwa_sequence_item)

	randc logic [31:0] data_in;
	      logic [31:0] data_out;
	randc logic [3:0]  addr;
		  logic        wr_en;
		  logic        re_en;
		  logic        clk;
	      logic        rst;
		  logic        valid_out;
	
	
	function new(string name = "my_sequence_item");
		super.new(name);
	endfunction

	constraint const1 {rst dist {0:=1,1:=9};}   //rst is constrained with probabilty 10% for 0 and 90% for 1
	constraint const2 {wr_en inside {0,1};} 
	constraint const3 {re_en inside {0,1};}
	constraint const4 {data_in inside {[0:15]};}
	
	function void display();
        $display("Data_in: %h, Addr: %h, wr_en: %b, re_en: %b, rst: %b", data_in, addr, wr_en, re_en, rst);
    endfunction

endclass

///////////////////////////////////////////////////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////Driver////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////////////////////
class Radwa_driver extends uvm_driver #(Radwa_sequence_item);

    `uvm_component_utils (Radwa_driver)
	Radwa_sequence_item Rad_seq_item;
    
	virtual intf intf1;
   
     function new (string name, uvm_component parent);
        super.new(name, parent);
     endfunction
    
    function void build_phase(uvm_phase phase);
       super.build_phase(phase);
	   Rad_seq_item = Radwa_sequence_item::type_id::create("Rad_seq_item");
       $display("build phase of driver");
       //////////////////////////// set & get ////////////////////////////////////
	   if(!uvm_config_db#(virtual intf)::get(this, "", "my_vif", intf1)) $fatal("Failed to get my_vif0");
	   
    endfunction

    function void connect_phase(uvm_phase phase);
       super.connect_phase(phase);
       $display("connect phase of driver");
    endfunction

    task run_phase(uvm_phase phase);
       super.run_phase(phase);
       $display("run phase of driver");
	   
	   forever begin
		seq_item_port.get_next_item(Rad_seq_item);
		//sending data to the DUT
		@(posedge intf1.clk)
		begin
			intf1.data_in   <= Rad_seq_item.data_in;
			intf1.addr      <= Rad_seq_item.addr;
			intf1.rst       <= Rad_seq_item.rst;
			intf1.wr_en     <= Rad_seq_item.wr_en;
			intf1.re_en     <= Rad_seq_item.re_en;
		end
			#1step $display("From driver data_in = %p", intf1.data_in); 
			seq_item_port.item_done();
		end
    endtask
endclass

///////////////////////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////monitor/////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////////////////////
class Radwa_monitor extends uvm_monitor;

    `uvm_component_utils (Radwa_monitor)
	
	////////////tlm
	Radwa_sequence_item Rad_seq_item1;
	uvm_analysis_port #(Radwa_sequence_item) analysis_port;
	
	virtual intf intf1;

    function new (string name, uvm_component parent);
		super.new(name, parent);
    endfunction

    function void build_phase(uvm_phase phase);
       super.build_phase(phase);
	   //////////////////////////// get ////////////////////////////////////
	   if(!uvm_config_db#(virtual intf)::get(this, "", "my_vif", intf1)) $fatal("Failed to get my_vif1");
	   
	   ///////////////////tlm
	   analysis_port = new ("analysis_port", this);
	   
	   Rad_seq_item1 = Radwa_sequence_item::type_id::create("Rad_seq_item1");
       $display("build phase of monitor");
    endfunction

    function void connect_phase(uvm_phase phase);
       super.connect_phase(phase);
       $display("connect phase of monitor");
    endfunction

    task run_phase(uvm_phase phase);
       super.run_phase(phase);
	   $display("run phase of monitor");
	   
	   for (int j=0; j<60; j++)
	     begin
	  @(posedge intf1.clk)
		if (intf1.valid_out == 0) begin
			Rad_seq_item1.data_in <= intf1.data_in;
			Rad_seq_item1.wr_en   <= intf1.wr_en;
			Rad_seq_item1.re_en   <= intf1.re_en;
			Rad_seq_item1.addr    <= intf1.addr;
			analysis_port.write(Rad_seq_item1); 
			#1step 		$display("From monitor data in = %d & valid_out = %d & write enable = %d & read enable = %d",intf1.data_in, intf1.valid_out, intf1.wr_en, intf1.re_en);
			end
			
			
		else if (intf1.valid_out == 1) begin
			Rad_seq_item1.data_out <= intf1.data_out;
			Rad_seq_item1.re_en    <= intf1.re_en;
			Rad_seq_item1.wr_en    <= intf1.wr_en;
			Rad_seq_item1.addr     <= intf1.addr;
			analysis_port.write(Rad_seq_item1); 
			#1step $display("From monitor data out [%d] = %d & valid_out = %d  & write enable = %d & read enable = %d",intf1.addr, intf1.data_out, intf1.valid_out, intf1.wr_en, intf1.re_en);
			end
	
		end
       
    endtask
	
endclass

///////////////////////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////sequencer///////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////////////////////
class Radwa_sequencer extends uvm_sequencer #(Radwa_sequence_item);

      `uvm_component_utils (Radwa_sequencer)
///////////////////////////////////////////////new/////////////////////////////////////////////////////////////
  function new (string name, uvm_component parent);
     super.new(name, parent);
  endfunction

//////////////////////////////////////////build_phase//////////////////////////////////////////////////////////
    function void build_phase(uvm_phase phase);
       super.build_phase(phase);
       $display("build phase of sequencer");
    endfunction

///////////////////////////////////////////connect_phase///////////////////////////////////////////////////////
    function void connect_phase(uvm_phase phase);
       super.connect_phase(phase);
       $display("connect phase of sequencer");
    endfunction

//////////////////////////////////////////////run_phase////////////////////////////////////////////////////////
    task run_phase(uvm_phase phase);
       super.run_phase(phase);
       $display("run phase of sequencer");
    endtask

endclass

///////////////////////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////Agent///////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////////////////////
class Radwa_agent extends uvm_agent;

  `uvm_component_utils (Radwa_agent)

  Radwa_driver Rad_driv;
  Radwa_monitor Rad_mon;
  Radwa_sequencer Rad_seq;
  ///////////tlm
  uvm_analysis_port #(Radwa_sequence_item) My_analysis_port;
  
  virtual intf intf1;
///////////////////////////////////////////////////new/////////////////////////////////////////////////////////
  function new (string name, uvm_component parent);
     super.new(name, parent);
  endfunction

//////////////////////////////////////////////build_phase//////////////////////////////////////////////////////
      function void build_phase(uvm_phase phase);
       super.build_phase(phase);
       Rad_driv = Radwa_driver::type_id::create("Rad_driv", this);
       Rad_mon  = Radwa_monitor::type_id::create("Rad_mon", this);
       Rad_seq  = Radwa_sequencer::type_id::create("Rad_seq", this);
	   My_analysis_port = new("My_analysis_port", this);
	   //////////////////////////// set & get ////////////////////////////////////
	   if(!uvm_config_db#(virtual intf)::get(this, "", "my_vif", intf1)) $fatal("Failed to get my_vif2");
	   uvm_config_db#(virtual intf)::set(this, "Rad_driv", "my_vif", intf1);
	   uvm_config_db#(virtual intf)::set(this, "Rad_mon", "my_vif", intf1);
       $display("build phase of agent");
    endfunction

/////////////////////////////////////////////connect_phase/////////////////////////////////////////////////////
    function void connect_phase(uvm_phase phase);
       super.connect_phase(phase);
	   /////////////tlm
	   Rad_mon.analysis_port.connect(My_analysis_port);
	   Rad_driv.seq_item_port.connect(Rad_seq.seq_item_export);
	   
       $display("connect phase of agent");
    endfunction

///////////////////////////////////////////////run_phase///////////////////////////////////////////////////////
    task run_phase(uvm_phase phase);
       super.run_phase(phase);
       $display("run phase of agent");
    endtask
///////////////////////////////////////////////////////////////////////////////////////////////////////////////

endclass

///////////////////////////////////////////////////////////////////////////////////////////////////////////////
/////////////////////////////////////////////Scoreboard////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////////////////////

class Radwa_scoreboard extends uvm_scoreboard;

	`uvm_component_utils (Radwa_scoreboard)
	Radwa_sequence_item in_seq;	
	Radwa_sequence_item out_seq;
	////////////////////tlm
	uvm_analysis_export #(Radwa_sequence_item) in_analysis_export; 
	uvm_tlm_analysis_fifo  #(Radwa_sequence_item) in_fifo;
	
/////////////////////////////////////////////////new///////////////////////////////////////////////////////////
	function new (string name, uvm_component parent);
		super.new(name, parent);
	endfunction

/////////////////////////////////////////////build_phase///////////////////////////////////////////////////////
      function void build_phase(uvm_phase phase);
       super.build_phase(phase);
       $display("build phase of scoreboard");
	   in_analysis_export = new("in_analysis_export", this);
	   in_fifo = new("in_fifo", this);
    endfunction

///////////////////////////////////////////connect_phase///////////////////////////////////////////////////////
    function void connect_phase(uvm_phase phase);
       super.connect_phase(phase);
	   in_analysis_export.connect(in_fifo.analysis_export);
       $display("connect phase of scoreboard");
    endfunction

////////////////////////////////////////////run_phase//////////////////////////////////////////////////////////
    task run_phase(uvm_phase phase);
		integer in[int], out[int];
		integer Q_addr[$], Q_data[$];
        super.run_phase(phase);
	    $display("run phase of scoreboard");
		forever
			begin
				in_fifo.get(in_seq);
				
				if(in_seq.wr_en && !in_seq.re_en)
				begin
					in[in_seq.addr] = in_seq.data_in;
					$display(" From scoreboard data in [%d] = %d , write enable = %d, read enable = %d and the input = ", in_seq.addr, in_seq.data_in, in_seq.wr_en, in_seq.re_en, in[in_seq.addr]);

				foreach (in[key]) begin
					$display("addr: %0d, data in: %0d", key, in[key]);
				end
				end
	
				else if (out.num() == 16) begin
					Compare(in, out); end
					
				else if (in_seq.re_en)
				begin
					Q_addr.push_back(in_seq.addr);
					Q_data.push_back(in_seq.data_out);
					$display("QQQQQQQQQQaddr ",Q_addr);
					$display("QQQQQQQQQQdata  ",Q_data);
					$display("QQQQQQQQQQout  ",out.num());
					if ((Q_data.size() > 1) || (out.num() > 2))    
					begin
					out[Q_addr.pop_front()] = Q_data[1];
					Q_data.pop_front();
					end

				$display("QQQQQQQQQQ", in_seq.re_en && !in_seq.wr_en);
				
				foreach (out[key]) begin
					
					$display("addr: %0d, data out: %0d", key, out[key]);
				end
				
				end
			
			end
			
	    endtask
			
	task Compare(input integer in[int], input integer out[int]);
		begin
			for (int n = 0; n<16; n++)
				begin
					if(in[n] == out[n])
						begin
							$display("Done heeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeere where in = %d and out = %d", in[n], out[n]);
						end
					else
						begin
							$fatal("There is an error heeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeere where in = %d and out = %d", in[n], out[n]);
							
						end
				end
		end
	endtask
	
		

///////////////////////////////////////////////////////////////////////////////////////////////////////////////

	function void write (Radwa_sequence_item t);
		 $display("HELLO from scoreboard");
	endfunction

endclass

///////////////////////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////Subscriber//////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////////////////////
class Radwa_subscriber extends uvm_subscriber #(Radwa_sequence_item);

  `uvm_component_utils (Radwa_subscriber)
  
  //////////////////tlm
  Radwa_sequence_item Rad_seq_item;
  uvm_analysis_imp #(Radwa_sequence_item, Radwa_subscriber) analysis_imp_A;

//////////////////////////////////////////////Cover Group//////////////////////////////////////////////////////
	covergroup group1 ;
		coverpoint Rad_seq_item.data_in   {bins bin_1[] = {[0:15]};}
		coverpoint Rad_seq_item.addr      {bins bin_2[] = {[0:15]};}
		coverpoint Rad_seq_item.wr_en     {bins bin_3[] = {[0:1]};}
		coverpoint Rad_seq_item.re_en     {bins bin_4[] = {[0:1]};}
		coverpoint Rad_seq_item.rst       {bins bin_5[] = {[0:1]};}
	endgroup

////////////////////////////////////////////////////new//////////////////////////////////////////////////////
  function new (string name, uvm_component parent);
     super.new(name, parent);
	 /////////////coverage
	 group1=new();
	 ///////////////tlm
	 analysis_imp_A = new("analysis_imp_A", this);
  endfunction

///////////////////////////////////pure virtual function write////////////////////////////////////////////////
  function void write(Radwa_sequence_item t);
    $display("HI from subscriber");
	$display("The coverage = ", group1.get_inst_coverage());
	Rad_seq_item = t;
	group1.sample();
  endfunction

////////////////////////////////////////////build_phase///////////////////////////////////////////////////////
      function void build_phase(uvm_phase phase);
       super.build_phase(phase);
       $display("build phase of subscriber");
    endfunction

///////////////////////////////////////////connect_phase//////////////////////////////////////////////////////
    function void connect_phase(uvm_phase phase);
       super.connect_phase(phase);
       $display("connect phase of subscriber");
    endfunction

////////////////////////////////////////////////run_test//////////////////////////////////////////////////////
    task run_phase(uvm_phase phase);
       super.run_phase(phase);
       $display("run phase of subscriber");
    endtask

endclass

///////////////////////////////////////////////////////////////////////////////////////////////////////////////
/////////////////////////////////////////////Sequence//////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////////////////////
class Radwa_Sequence extends uvm_sequence #(Radwa_sequence_item);

  `uvm_object_utils (Radwa_Sequence)
  
  //////////////////tlm
  Radwa_sequence_item Rad_seq_item;
  
////////////////////////////////////////////////////new///////////////////////////////////////////////////////
	function new(string name = "my_sequence");
		super.new(name);
	endfunction

////////////////////////////////////////////build_phase///////////////////////////////////////////////////////
    task pre_body();
		Rad_seq_item = Radwa_sequence_item::type_id::create("Rad_seq_item");    
    endtask

////////////////////////////////////////////////run_test//////////////////////////////////////////////////////
    task body();
		//forever begin
		/////////deassert the reset
			
				for(int i = 0; i < 16; i++)
					begin	
						start_item(Rad_seq_item);
						Rad_seq_item.rst = 0;
						finish_item(Rad_seq_item);
					end
			
			
			
			
				for(int h = 0; h < 16; h++)
					begin	
						start_item(Rad_seq_item);
						Rad_seq_item.rst = 1;
						Rad_seq_item.wr_en = 1;
						Rad_seq_item.re_en = 0;							
						assert(Rad_seq_item.randomize());
						$display("data_in [%d] = %d", Rad_seq_item.addr, Rad_seq_item.data_in); 
						finish_item(Rad_seq_item);
						end
			
			
	/////////////////////////////////////////////////////////////////////////////////////////////////////////			
					
				for(int g = 0; g < 32; g++)
					begin
						start_item(Rad_seq_item);
						Rad_seq_item.rst = 1;
						Rad_seq_item.wr_en = 0;
						Rad_seq_item.re_en = 1;							
						assert(Rad_seq_item.randomize(addr));
						finish_item(Rad_seq_item);
						end

    endtask

endclass

///////////////////////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////Environment/////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////////////////////
class Radwa_env extends uvm_env;

  `uvm_component_utils (Radwa_env)

  Radwa_scoreboard Rad_score;
  Radwa_agent Rad_agent;
  Radwa_subscriber Rad_sub;
  
  virtual intf intf1;

/////////////////////////////////////////////////////new///////////////////////////////////////////////////////
  function new (string name, uvm_component parent);
  super.new(name,parent);
  endfunction

///////////////////////////////////////////////build_phase/////////////////////////////////////////////////////
    function void build_phase(uvm_phase phase);
       super.build_phase(phase);
       Rad_score = Radwa_scoreboard::type_id::create("Rad_score", this);
       Rad_agent = Radwa_agent::type_id::create("Rad_agent", this);
       Rad_sub   = Radwa_subscriber::type_id::create("Rad_sub", this);
	   //////////////////////////// set & get ////////////////////////////////////
	   if(!uvm_config_db#(virtual intf)::get(this, "", "my_vif", intf1)) $fatal("Failed to get my_vif3");
	   uvm_config_db#(virtual intf)::set(this, "Rad_agent", "my_vif", intf1);
       $display("build phase of Radwa_env");
    endfunction

/////////////////////////////////////////////connect_phase/////////////////////////////////////////////////////
    function void connect_phase(uvm_phase phase);
       super.connect_phase(phase);
	   ///////////////////tlm
	   Rad_agent.My_analysis_port.connect(Rad_sub.analysis_imp_A);
	   Rad_agent.My_analysis_port.connect(Rad_score.in_analysis_export);
       $display("connect phase of environment");
    endfunction

//////////////////////////////////////////////run_phase////////////////////////////////////////////////////////
    task run_phase(uvm_phase phase);
       super.run_phase(phase);
       $display("run phase of environment");
    endtask

endclass

///////////////////////////////////////////////////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////Test//////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////////////////////

class Test extends uvm_test;

    `uvm_component_utils (Test)
     Radwa_env my_env;
	 Radwa_Sequence Rad_sequence;
	 virtual intf intf1;

////////////////////////////////////////////////new///////////////////////////////////////////////////////////
   function new (string name, uvm_component parent);
     super.new(name, parent);
   endfunction

//////////////////////////////build_phase/////////////////////////////////////////////////////////////////////
      function void build_phase(uvm_phase phase);
       super.build_phase(phase);
       my_env = Radwa_env::type_id::create("my_env", this);
	   Rad_sequence = Radwa_Sequence::type_id::create("Rad_sequence");
	   //////////////////////////// set & get ////////////////////////////////////
	   if(!uvm_config_db#(virtual intf)::get(this, "", "my_vif", intf1)) $fatal("Failed to get my_vif4");
	   uvm_config_db#(virtual intf)::set(this, "my_env", "my_vif", intf1);
       $display("build phase of Test");
    endfunction

///////////////////////////////////////connect_phase//////////////////////////////////////////////////////////
    function void connect_phase(uvm_phase phase);
       super.connect_phase(phase);
       $display("connect phase of Test");
    endfunction

///////////////////////////////////////////run_phase//////////////////////////////////////////////////////////
    task run_phase(uvm_phase phase);
       super.run_phase(phase);
       $display("run phase of Test");
	    
	   	phase.raise_objection(this);
		Rad_sequence.start(my_env.Rad_agent.Rad_seq);
		phase.drop_objection(this);
    endtask

endclass

endpackage

//////////////////////////////////////////////////////////////////////////////////////////////////////////////
//////////////////////////////////////////interface///////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////////////////////////////////////////////////////

interface intf (input logic clk);
logic [31:0] data_in;
logic [31:0] data_out;
logic [3:0]  addr;
logic        wr_en;
logic        re_en;
logic        rst;
logic        valid_out;

endinterface
///////////////////////////////////////////////////////////////////////////////////////////////////////////////

//////////////////////////////////////////////////Top Module///////////////////////////////////////////////////
module top2 #(parameter depth = 16,
              parameter data_width = 16,
              parameter addr_width = 4)();



import uvm_pkg::*;
import pack1::*;
logic clk ;
intf intf1(.clk(clk));

Ram DUT (
	.CLK(clk),
	.RST(intf1.rst),
	.write_en(intf1.wr_en),
	.read_en(intf1.re_en),
	.addr(intf1.addr),
	.data_in(intf1.data_in),
	.data_out(intf1.data_out),
	.valid_out(intf1.valid_out)
);

initial
  begin
    clk = 0;
    forever #10 clk = ~ clk;
  end

initial
  begin
	intf1.re_en = 0;
	intf1.wr_en = 0;
    uvm_config_db#(virtual intf)::set(null, "uvm_test_top", "my_vif", intf1);
    run_test("Test");
  end


endmodule



