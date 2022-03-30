*=============================================================================
* Calculate simulations
*#############################################################################
sets
  KEEP_ORDERING  /0*20000/
  MM_SIM
  j "sectors"
  t "time period" /0*30/
  u 'SAM entry'
;
$gdxIn sam.gdx
$load u

alias(u,v);
parameters
  sam1(u,v)
  sam2(u,v)
;
$load sam1, sam2
$gdxIn


$gdxIn 'sample.gdx'
$load MM_SIM j
alias(j, i);

* sample
parameters
  mm_ror(MM_SIM)
  mm_dep(MM_SIM)
  mm_pop(MM_SIM)
  mm_zeta(MM_SIM)
  mm_omega(MM_SIM)
  mm_sigma(MM_SIM, i)
  mm_psi(MM_SIM, i)
;

$load mm_ror mm_dep mm_pop mm_zeta mm_omega mm_sigma mm_psi
$gdxin

* simulation parameters
parameters
  ror
  dep
  pop
  zeta
  sam(u,v)
  sigma(i)
  psi(i)
;

* outputs
parameters
  Z1(j, t)
  mm_Z1(MM_SIM, j, t) "sectoral output"
  Z2(t)
  mm_Z2(MM_SIM, t) "sum of sectoral output"
  Z3(j, t) "exports"
  mm_Z3(MM_SIM, j, t) "exports"
  Z4(t) "sum of exports"
  mm_Z4(MM_SIM, t) "sum of exports"
  Z5(t) "exchange rate"
  mm_Z5(MM_SIM, t) "exchange rate"
  Z6(j, t) "sectoral imports"
  mm_Z6(MM_SIM, j, t) "sectoral imports"
  Z7(t) "sum of imports"
  mm_Z7(MM_SIM, t) "sum of imports"
  z_scaling_factor /10000/
;

loop(MM_SIM,
  ror = mm_ror(MM_SIM);
  dep = mm_dep(MM_SIM);
  pop = mm_pop(MM_SIM);
  zeta = mm_zeta(MM_SIM);
  sigma(i) = mm_sigma(MM_SIM, i);
  psi(i) = mm_psi(MM_SIM, i);
  sam(u,v) = mm_omega(MM_SIM) * sam1(u,v) + (1 - mm_omega(MM_SIM)) * sam2(u,v);
  
  execute_unload 'cge/parameters.gdx' ror dep pop zeta sigma psi sam;
  execute "gams model.gms curDir=./cge lo=0 errmsg=1";
  execute_load "cge/result.gdx" Z1 Z2 Z3 Z4 Z5 Z6 Z7;
  mm_Z1(MM_SIM, j, t) = Z1(j, t);
  mm_Z2(MM_SIM, t) = Z2(t);
  mm_Z3(MM_SIM, j, t) = Z3(j, t);
  mm_Z4(MM_SIM, t) = Z4(t);
  mm_Z5(MM_SIM, t) = Z5(t);
  mm_Z6(MM_SIM, j, t) = Z6(j, t);
  mm_Z7(MM_SIM, t) = Z7(t);
);

mm_Z1(MM_SIM, j, t) = mm_Z1(MM_SIM, j, t) / z_scaling_factor;
mm_Z2(MM_SIM, t) = mm_Z2(MM_SIM, t) / z_scaling_factor;
mm_Z3(MM_SIM, j, t) = mm_Z3(MM_SIM, j, t) / z_scaling_factor;
mm_Z4(MM_SIM, t) = mm_Z4(MM_SIM, t) / z_scaling_factor;
mm_Z6(MM_SIM, j, t) = mm_Z6(MM_SIM, j, t) / z_scaling_factor;
mm_Z7(MM_SIM, t) = mm_Z7(MM_SIM, t) / z_scaling_factor;

execute_unload "result.gdx" mm_Z1 mm_Z2 mm_Z3 mm_Z4 mm_Z5 mm_Z6 mm_Z7;

