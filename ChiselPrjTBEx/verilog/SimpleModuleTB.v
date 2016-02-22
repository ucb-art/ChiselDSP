`timescale 100ps / 100ps
// 2ns
`define CLK_PERIOD 20
// Ticks after clk rising edge to peek/poke signals
`define CLK_DELTA 2
// Reset changes slightly after clk rising edge (assumes clock,reset starts low,high)
// Note: FPGA takes some time to reset (~100ns)
`define RESET_TIME (60*`CLK_PERIOD + 3*`CLK_PERIOD/2 + `CLK_DELTA)
// Macro for checking node value = expected value
`define expect(nodeName, nodeVal, expVal, cycle) if (nodeVal !== expVal) begin \
$display("\t ASSERTION ON %s FAILED @ CYCLE = %d, 0x%h != EXPECTED 0x%h", \
nodeName,cycle,nodeVal,expVal); $stop; end

module ModuleTB;

    // Keep track of cycle count for debugging
    integer cycle = 0;

    // Chisel built-in inputs
    reg clk = 0;
    reg reset = 1;

    // Clocking
    always #(`CLK_PERIOD/2) clk = ~clk;

    initial begin
        // Start counting clock cycles (clock cycle in which reset is initially set low = 0)
        #(`RESET_TIME - `CLK_DELTA)
        // Cycle count changes on rising edge of clock
        forever #`CLK_PERIOD cycle = cycle + 1;
    end

// ** DESIGN SPECIFIC (see genHarness)

    // Module inputs
    reg upDown = 0;
    reg [7:0] addVal = 0;

    // Module outputs
    wire [7:0] count;

    // DUT
    SimpleModule SimpleModule(
    .clk    (clk),
    .reset  (reset),
    .upDown (upDown),
    .addVal (addVal),
    .count  (count)
    );

// ** Chisel testbench step (# cycles), poke, expect translation here
// Note peek/poke occur CLK_DELTA after clk rising edge
    initial begin
        // After reset time, bring reset low
        #`RESET_TIME reset = 0;
        addVal = 1;
        #(2*`CLK_PERIOD) addVal = 2;
        #(5*`CLK_PERIOD) upDown = 1;
        addVal = 1;
        #(7*`CLK_PERIOD) `expect("count",count,251,cycle)
        #(7*`CLK_PERIOD) `expect("count",count,2,cycle)

// ** Generic TB stuff

        // Wait 1 more clock cycle before finishing tester
        #`CLK_PERIOD $display("\t **Ran through all test vectors**"); $finish;

    end

endmodule
