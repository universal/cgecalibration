sets
  coefficients
  bounds /prior, lower, upper, variance, true/
  j
  t "evaluation / time points"
;
alias(coefficients, coefficients1);

parameters
  bounds_coefficients(coefficients, bounds)
  z1_intercepts(t, j)
  z1_linears(t, j, coefficients)
  z1_interactions(t, j, coefficients, coefficients1)  
  z2_intercepts(t)
  z2_linears(t, coefficients)
  z2_interactions(t, coefficients, coefficients1)  
  z3_intercepts(t, j)
  z3_linears(t, j, coefficients)
  z3_interactions(t, j, coefficients, coefficients1)  
  z4_intercepts(t)
  z4_linears(t, coefficients)
  z4_interactions(t, coefficients, coefficients1)  
  z5_intercepts(t)
  z5_linears(t, coefficients)
  z5_interactions(t, coefficients, coefficients1)  
  z6_intercepts(t, j)
  z6_linears(t, j, coefficients)
  z6_interactions(t, j, coefficients, coefficients1)  
  z7_intercepts(t)
  z7_linears(t, coefficients)
  z7_interactions(t, coefficients, coefficients1)  

  z1_variance(t, j)
  z2_variance(t)
  z3_variance(t, j)
  z4_variance(t)
  z5_variance(t)
  z6_variance(t, j)
  z7_variance(t)

  z1_projection(t, j)
  z2_projection(t)
  z3_projection(t, j)
  z4_projection(t)
  z5_projection(t)
  z6_projection(t, j)
  z7_projection(t)
;

variables
  entropy
  theta(coefficients)
  v_z1(t, j)
  v_z2(t)
  v_z3(t, j)
  v_z4(t)
  v_z5(t)
  v_z6(t, j)
  v_z7(t)
  entropy_z1
  entropy_z2
  entropy_z3
  entropy_z4
  entropy_z5
  entropy_z6
  entropy_z7
  entropy_parameters
;

equations
  def_objective
  def_entropy_z1
  def_entropy_z2
  def_entropy_z3
  def_entropy_z4
  def_entropy_z5
  def_entropy_z6
  def_entropy_z7
  def_entropy_parameters
  def_entropy_parameters_ml
  def_z1(t, j)
  def_z2(t)
  def_z3(t, j)
  def_z4(t)
  def_z5(t)
  def_z6(t, j)
  def_z7(t)
;

def_objective..
  entropy =E= entropy_z1 + entropy_z2 +
              entropy_z3 + entropy_z4 +
              entropy_z5 + entropy_z6 +
              entropy_parameters;

def_entropy_z1..
    entropy_z1 =E= sum(t, sum(j$z1_variance(t, j), sqr(v_z1(t, j) - z1_projection(t, j)) / z1_variance(t, j)));
    
def_entropy_z2..
    entropy_z2 =E= sum(t$z2_variance(t), sqr(v_z2(t) - z2_projection(t)) / z2_variance(t));

def_entropy_z3..
    entropy_z3 =E= sum(t, sum(j$z3_variance(t, j), sqr(v_z3(t, j) - z3_projection(t, j)) / z3_variance(t, j)));
    
def_entropy_z4..
    entropy_z4 =E= sum(t$z4_variance(t), sqr(v_z4(t) - z4_projection(t)) / z4_variance(t));

def_entropy_z5..
    entropy_z5 =E= sum(t$z5_variance(t), sqr(v_z5(t) - z5_projection(t)) / z5_variance(t));

def_entropy_z6..
    entropy_z6 =E= sum(t, sum(j$z6_variance(t, j), sqr(v_z6(t, j) - z6_projection(t, j)) / z6_variance(t, j)));

def_entropy_z7..
    entropy_z7 =E= sum(t$z7_variance(t), sqr(v_z7(t) - z7_projection(t)) / z7_variance(t));
    
def_entropy_parameters..
    entropy_parameters =E= sum(coefficients, sqr(theta(coefficients) - bounds_coefficients(coefficients, "prior")) / bounds_coefficients(coefficients, "variance"));

def_z1(t, j)..
  v_z1(t, j) =E=
    z1_intercepts(t, j) +
    sum(coefficients, z1_linears(t, j, coefficients) * theta(coefficients)) +
    sum((coefficients, coefficients1)$z1_interactions(t, j, coefficients, coefficients1), z1_interactions(t, j, coefficients, coefficients1) * theta(coefficients) * theta(coefficients1));

def_z2(t)..
  v_z2(t) =E=
    z2_intercepts(t) +
    sum(coefficients, z2_linears(t, coefficients) * theta(coefficients)) +
    sum((coefficients, coefficients1)$z2_interactions(t, coefficients, coefficients1), z2_interactions(t, coefficients, coefficients1) * theta(coefficients) * theta(coefficients1));

def_z3(t, j)..
  v_z3(t, j) =E=
    z3_intercepts(t, j) +
    sum(coefficients, z3_linears(t, j, coefficients) * theta(coefficients)) +
    sum((coefficients, coefficients1)$z3_interactions(t, j, coefficients, coefficients1), z3_interactions(t, j, coefficients, coefficients1) * theta(coefficients) * theta(coefficients1));

def_z4(t)..
  v_z4(t) =E=
    z4_intercepts(t) +
    sum(coefficients, z4_linears(t, coefficients) * theta(coefficients)) +
    sum((coefficients, coefficients1)$z4_interactions(t, coefficients, coefficients1), z4_interactions(t, coefficients, coefficients1) * theta(coefficients) * theta(coefficients1));

def_z5(t)..
  v_z5(t) =E=
    z5_intercepts(t) +
    sum(coefficients, z5_linears(t, coefficients) * theta(coefficients)) +
    sum((coefficients, coefficients1)$z5_interactions(t, coefficients, coefficients1), z5_interactions(t, coefficients, coefficients1) * theta(coefficients) * theta(coefficients1));

def_z6(t, j)..
  v_z6(t, j) =E=
    z6_intercepts(t, j) +
    sum(coefficients, z6_linears(t, j, coefficients) * theta(coefficients)) +
    sum((coefficients, coefficients1)$z6_interactions(t, j, coefficients, coefficients1), z6_interactions(t, j, coefficients, coefficients1) * theta(coefficients) * theta(coefficients1));

def_z7(t)..
  v_z7(t) =E=
    z7_intercepts(t) +
    sum(coefficients, z7_linears(t, coefficients) * theta(coefficients)) +
    sum((coefficients, coefficients1)$z7_interactions(t, coefficients, coefficients1), z7_interactions(t, coefficients, coefficients1) * theta(coefficients) * theta(coefficients1));


model calibration /
  def_objective
  def_entropy_z1
  def_entropy_z2
  def_entropy_z3
  def_entropy_z4
  def_entropy_z5
  def_entropy_z6
  def_entropy_z7
  def_entropy_parameters
  def_z1
  def_z2
  def_z3
  def_z4
  def_z5
  def_z6
  def_z7
/;

def_entropy_parameters_ml..
  entropy_parameters =E= 0;

model calibration_ml /
  def_objective
  def_entropy_z1
  def_entropy_z2
  def_entropy_z3
  def_entropy_z4
  def_entropy_z5
  def_entropy_z6
  def_entropy_z7
  def_entropy_parameters_ml
  def_z1
  def_z2
  def_z3
  def_z4
  def_z5
  def_z6
  def_z7
/;