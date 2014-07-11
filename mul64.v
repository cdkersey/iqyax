module mul64(phi, a, b, start_mul, o);
  input phi;
  input a[63:0];
  input b[63:0];
  reg o[63:0];

  always @(posedge phi)
  begin
    if (start_mul) o = (a * b)[63:0];
  end
endmodule
