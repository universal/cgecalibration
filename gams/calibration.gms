$include "calibration/model.gms"

$gdxIn calibration/projections.gdx
$load j t
$load z1_projection z2_projection
$load z3_projection z4_projection
$load z5_projection z6_projection
$load z7_projection 
$gdxIn

$gdxIn calibration/models.gdx
$load coefficients bounds_coefficients
$load z1_intercepts z1_linears z1_interactions
$load z2_intercepts z2_linears z2_interactions
$load z3_intercepts z3_linears z3_interactions
$load z4_intercepts z4_linears z4_interactions
$load z5_intercepts z5_linears z5_interactions
$load z6_intercepts z6_linears z6_interactions
$load z7_intercepts z7_linears z7_interactions
$gdxIn


z1_variance("30", "AGR") = 0.01;
z1_variance("0", "AGR") = 0.1;
*z1_variance("0", j) = 0.01;
z2_variance("30") = 0.1;
z3_variance("30", "LMN") = 0.01;
z4_variance("30") = 0.0001;
z5_variance("30") = 0.01;
z6_variance("30", "AGR") = 0.01;
z7_variance("30") = 0.01;

theta.l(coefficients) = bounds_coefficients(coefficients, "prior");
theta.lo(coefficients) = bounds_coefficients(coefficients, "lower");
theta.up(coefficients) = bounds_coefficients(coefficients, "upper");

solve calibration minimizing entropy using nlp;

sets
  parameter_kinds /psi, sigma/
  map_coefficients_scalars(coefficients, *)
  map_coefficients_parameters(coefficients, parameter_kinds, j)
;

map_coefficients_scalars("dep", "dep") = YES;
map_coefficients_scalars("omega", "omega") = YES;
map_coefficients_scalars("pop", "pop") = YES;
map_coefficients_scalars("ror", "ror") = YES;
map_coefficients_scalars("zeta", "zeta") = YES;

map_coefficients_parameters("psi_AGR", "psi", "AGR") = YES;
map_coefficients_parameters("psi_HMN", "psi", "HMN") = YES;
map_coefficients_parameters("psi_LMN", "psi", "LMN") = YES;
map_coefficients_parameters("psi_SRV", "psi", "SRV") = YES;
map_coefficients_parameters("sigma_AGR", "sigma", "AGR") = YES;
map_coefficients_parameters("sigma_HMN", "sigma", "HMN") = YES;
map_coefficients_parameters("sigma_LMN", "sigma", "LMN") = YES;
map_coefficients_parameters("sigma_SRV", "sigma", "SRV") = YES;

sets
  RESULTS /metamodel,simulation, true/
  MM_SIM(RESULTS) /simulation/
;
* sample
parameters
  mm_ror(MM_SIM)
  mm_dep(MM_SIM)
  mm_pop(MM_SIM)
  mm_zeta(MM_SIM)
  mm_omega(MM_SIM)
  mm_sigma(MM_SIM, j)
  mm_psi(MM_SIM, j)
;

mm_ror(MM_SIM) = theta.l("ror");
mm_dep(MM_SIM) = theta.l("dep");
mm_pop(MM_SIM) = theta.l("pop");
mm_zeta(MM_SIM) = theta.l("zeta");
mm_omega(MM_SIM) = theta.l("omega");
mm_psi(MM_SIM, j) = sum(coefficients$map_coefficients_parameters(coefficients, "psi", j), theta.l(coefficients));
mm_sigma(MM_SIM, j)  = sum(coefficients$map_coefficients_parameters(coefficients, "sigma", j), theta.l(coefficients));

execute_unload "sample.gdx" MM_SIM j mm_ror mm_dep mm_pop mm_zeta mm_omega mm_sigma mm_psi;

execute "gams mm_simulations.gms"

parameters
  mm_Z1(RESULTS, j, t)
  mm_Z2(RESULTS, t)
  mm_Z3(RESULTS, j, t)
  mm_Z4(RESULTS, t)
  mm_Z5(RESULTS, t)
  mm_Z6(RESULTS, j, t)
  mm_Z7(RESULTS, t)
;

execute_load "result.gdx" mm_Z1 mm_Z2 mm_Z3 mm_Z4 mm_Z5 mm_Z6 mm_Z7;

mm_Z1("metamodel", j, t) = v_z1.l(t, j);
mm_Z2("metamodel", t) = v_z2.l(t);
mm_Z3("metamodel", j, t) = v_z3.l(t, j);
mm_Z4("metamodel", t) = v_z4.l(t);
mm_Z5("metamodel", t) = v_z5.l(t);
mm_Z6("metamodel", j, t) = v_z6.l(t, j);
mm_Z7("metamodel", t) = v_z7.l(t);
mm_Z1("true", j, t) = z1_projection(t, j);
mm_Z2("true", t) = z2_projection(t);
mm_Z3("true", j, t) = z3_projection(t, j);
mm_Z4("true", t) = z4_projection(t);
mm_Z5("true", t) = z5_projection(t);
mm_Z6("true", j, t) = z6_projection(t, j);
mm_Z7("true", t) = z7_projection(t);


parameters
  z1_error(RESULTS, t, j)
  z2_error(RESULTS, t)
  z3_error(RESULTS, t, j)
  z4_error(RESULTS, t)
  z5_error(RESULTS, t)
  z6_error(RESULTS, t, j)
  z7_error(RESULTS, t)
  delta_to_true(coefficients)
;

z1_error(RESULTS, t, j)$z1_projection(t, j) = (mm_Z1(RESULTS, j, t) - z1_projection(t, j)) / z1_projection(t, j);
z2_error(RESULTS, t)$z2_projection(t) = (mm_Z2(RESULTS, t) - z2_projection(t)) / z2_projection(t);
z3_error(RESULTS, t, j)$z3_projection(t, j) = (mm_Z3(RESULTS, j, t) - z3_projection(t, j)) / z3_projection(t, j);
z4_error(RESULTS, t)$z4_projection(t) = (mm_Z4(RESULTS, t) - z4_projection(t)) / z4_projection(t);
z5_error(RESULTS, t)$z5_projection(t) = (mm_Z5(RESULTS, t) - z5_projection(t)) / z5_projection(t);
z6_error(RESULTS, t, j)$z6_projection(t, j) = (mm_Z6(RESULTS, j, t) - z6_projection(t, j)) / z6_projection(t, j);
z7_error(RESULTS, t)$z7_projection(t) = (mm_Z7(RESULTS, t) - z7_projection(t)) / z7_projection(t);

delta_to_true(coefficients) = (theta.l(coefficients) - bounds_coefficients(coefficients, "true")) / bounds_coefficients(coefficients, "true");

execute_unload "calibration.gdx" theta mm_Z1 mm_Z2 mm_Z3 mm_Z4 mm_Z5 mm_Z6 mm_Z7
  z1_error z2_error z3_error z4_error z5_error z6_error z7_error
  z1_variance z2_variance z3_variance z4_variance z5_variance z6_variance z7_variance
  delta_to_true;