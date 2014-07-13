module adder32(phi, a, b, o);
  input phi;
  input [31:0] a;
  input [31:0] b;
  output [31:0] o;

  assign o = a + b;
endmodule

module mult32(phi, a, b, o);
  input phi;
  input [31:0] a;
  input [31:0] b;
  output [31:0] o;

  assign o = a * b;
endmodule

module div32(phi, a, b, o);
  input phi;
  input [31:0] a;
  input [31:0] b;
  output [31:0] o;

  assign o = a / b;
endmodule

module adder16(phi, a, b, o);
  input phi;
  input [15:0] a;
  input [15:0] b;
  output [15:0] o;

  assign o = a + b;
endmodule

module mult16(phi, a, b, o);
  input phi;
  input [15:0] a;
  input [15:0] b;
  output [15:0] o;

  assign o = a * b;
endmodule

module div16(phi, a, b, o);
  input phi;
  input [15:0] a;
  input [15:0] b;
  output [15:0] o;

  assign o = a / b;
endmodule
