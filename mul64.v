module mul64(phi, a, b, start_mul, o)
  input phi, a, b;
  reg o;

  always @(posedge phi)
  begin
    if (start_mul) o = (a * b)[63:0];
  end
endmodule
