module SimpleModule(
    input reset,
    input upDown,
    input clk,
    input [7:0] addVal,
    output reg [7:0] count
    );

    always @(posedge clk) begin
        if (reset) begin
            count <= 8'b00000000;
        end else if (upDown) begin
            count <= count + addVal;
        end else begin
            count <= count - addVal;
        end
    end

endmodule
