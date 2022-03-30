sets
  u     'SAM entry' / AGR, LMN, HMN, SRV, CAP, LAB, HOH, GOV, INV, EXT, IDT, TRF /
  i(u)  'goods'     / AGR, LMN, HMN, SRV /
;
alias(u,v);

* ===============================================================
* SAM Data
* ===============================================================
Table SAM(u,v) 'original social accounting matrix for 2005 [bil. JPY]'
        AGR         LMN        HMN         SRV
   AGR  1643.017    7560.896   237.841     1409.202
   LMN  1485.854    10803.527  15330.764   18597.270
   HMN  1071.954    4277.721   113390.269  48734.424
   SRV  2002.380    11406.260  50513.476   177675.714
   IDT  433.854     4068.616   9418.058    20103.917
   TRF  149.278     2866.853   1749.385    8.575
   CAP  5082.506    7042.697   21058.821   163045.396
   LAB  1435.010    8942.365   42510.123   222732.700
   EXT  2092.569    23796.669  30982.559   10837.256

   +    IDT         TRF        CAP         LAB
   HOH                         196229.420  275620.198
   GOV  34024.445   4774.091

   +    HOH         GOV        INV         EXT
   AGR  3563.257    0.000      919.745     62.464
   LMN  32220.169   329.469    802.026     1196.525
   HMN  27648.678   4.931      34979.803   55083.516
   SRV  234243.865  90707.177  79169.426   17426.156
   GOV  52243.041
   INV  121930.608  0.000                  -6059.608;

* Source: compiled by N. Hosoe, based on the I/O table for 2005


Parameters
  SAM_col(v) "col/row sums of sam"
  p_IO(u,v) "io coefficients"
  io_shifter /0.2/
  factor_shifter /0.5/
  sam1(u,v) 'shifted sam'
  sam2(u,v) 'shifted sam'
;

SAM_col(v)   = sum(u,SAM(u,v));

* ===============================================================
* Transform IO structure for Agrar-sectors
* ===============================================================
p_IO(u,v) = SAM(u,v)/SAM_col(v);
p_IO("AGR",i)      = (1 - io_shifter) * p_IO("AGR",i);
p_IO("LAB","AGR")  = (1 + factor_shifter) * p_IO("LAB","AGR");
p_IO("CAP","AGR")  = (1 - factor_shifter) * p_IO("CAP","AGR");

variables
  entropy
;
positive variables
  v_IO(u,v)
;

equations
  def_entropy
  def_res1(v)
  def_res2(u,v)
  def_res3(u)
;

def_entropy..
  entropy =e= sum((u,v)$p_IO(u,v), sqr( (v_IO(u,v)- p_IO(u,v)) / p_IO(u,v) ));
  
def_res1(v)..
  sum(u, v_IO(u,v)) =E= 1;
  
def_res2(u,v)..
  v_IO(u,v) =L= 1;
  
def_res3(u)..
  SAM_col(u) =E= sum(v, v_IO(u,v) * SAM_col(v));
  
model SAMnew  /
  def_entropy
  def_res1
  def_res2
  def_res3
/;

v_IO.l(u,v) = p_IO(u,v);
v_IO.fx(u,v)$(p_IO(u,v)=0) = 0;

solve  SAMnew using nlp minimizing entropy;

sam1(u,v) = v_IO.l(u,v) * SAM_col(v);

p_IO(u,v) = SAM(u,v)/SAM_col(v);
p_IO("AGR",i)      = (1 + io_shifter) * p_IO("AGR",i);
p_IO("LAB","AGR")  = (1 - factor_shifter) * p_IO("LAB","AGR");
p_IO("CAP","AGR")  = (1 + factor_shifter) * p_IO("CAP","AGR");


v_IO.l(u,v) = p_IO(u,v);

solve  SAMnew using nlp minimizing entropy;
sam2(u,v) = v_IO.l(u,v) * SAM_col(v);

Parameters
  SAMGAP(u) 'gaps between row sums and column sums'
  SAMGAP1(u) 'gaps between row sums and column sums'
  SAMGAP2(u) 'gaps between row sums and column sums'
;
SAMGAP(u) = sum(v, SAM(u,v) - SAM(v,u));
SAMGAP1(u) = sum(v, SAM1(u,v) - SAM1(v,u));
SAMGAP2(u) = sum(v, SAM2(u,v) - SAM2(v,u));

execute_unload "sam.gdx" sam1, sam2, u;
