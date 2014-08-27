module mul64(phi, a, b, start_mul, o);
  input phi, start_mul;
  input [63:0] a;
  input [63:0] b;
  output [63:0] o;
  reg [63:0] o;

  always @(posedge phi)
  begin
    if (start_mul) o = (a * b);
  end
endmodule
