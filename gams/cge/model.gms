$title A Recursive-Dynamic Standard CGE Model (DYNCGE,SEQ=410)

$onText
This model is featured in the following book.


Hosoe, N., Gasawa, K., Hashimoto, H. Textbook of Computable General
Equilibrium Modeling: Programming and Simulations, 2nd Edition,
University of Tokyo Press. (in Japanese)

Keywords: nonlinear programming, general equilibrium model, social accounting
          matrix
$offText

option limRow = 0, limCol = 0, SolveLink = 5;

$offSymXRef offSymList

* ===============================================================
* Definition of sets for suffix ---------------------------------
* ===============================================================
Set
   u        'SAM entry' / AGR, LMN, HMN, SRV, CAP, LAB, HOH, GOV, INV, EXT, IDT, TRF /
   i(u)     'goods'     / AGR, LMN, HMN, SRV /
   h(u)     'factor'          / CAP, LAB /
   h_mob(h) 'mobile factor'   /      LAB /
   h_imm(h) 'immobile factor' / CAP      /
   t        'time'            / 0*30     /;

Alias (u,v), (i,j), (h,k);

* ===============================================================
* Data for Dynamics ---------------------------------------------
* ===============================================================
Scalar
   ror  'rate of return of capital'
   dep  'depreciation rate'
   pop  'population growth rate'
   zeta 'elasticity parameter for investment allocation'
;
parameters
   SAM(u,v) 'social accounting matrix for 2005 [bil. JPY]'
;
$gdxin parameters.gdx
$load ror dep pop zeta SAM
*ror  = 0.05;
*dep  = 0.04;
*pop  = 0.02;
*zeta = 1;

Parameter SAMGAP(u) 'gaps between row sums and column sums';
SAMGAP(u) = sum(v, SAM(u,v) - SAM(v,u));

display SAMGAP;

* ===============================================================
* Loading the initial values ------------------------------------
* ===============================================================
Parameter
* Base year values
   Y00(j)      'composite factor'
   F00(h,j)    'factor input'
   X00(i,j)    'intermediate input'
   Z00(j)      'gross output'
   Xp00(i)     'household consumption'
   Xg00(i)     'government consumption'
   Xv00(i)     'investment demand'
   E00(i)      'exports'
   M00(i)      'imports'
   Q00(i)      "Armington's composite good"
   D00(i)      'domestic good'
   Sp00        'private savings'
   Td00        'direct tax'
   Tz00(j)     'production tax'
   Tm00(j)     'import tariff'
   III00       'composite investment'
   II00(j)     'sectoral investment'
   KK00(j)     'capital stock'
   CC00        'composite consumption or felicity'
   FF00(h)     'factor endowment'
   Sf00        'foreign savings in US dollars'
   tauz00(i)   'production tax rate'
   taum00(i)   'import tariff rate'

* Base run value
   Y0(j,t)     'composite factor'
   F0(h,j,t)   'factor input'
   X0(i,j,t)   'intermediate input'
   Z0(j,t)     'gross output'
   Xp0(i,t)    'household consumption'
   Xv0(i,t)    'investment demand'
   E0(i,t)     'exports'
   M0(i,t)     'imports'
   Q0(i,t)     "Armington's composite good"
   D0(i,t)     'domestic good'
   Sp0(t)      'private savings'
   Td0(t)      'direct tax'
   Tz0(j,t)    'production tax'
   Tm0(j,t)    'import tariff'
   III0(t)     'composite investment'
   II0(j,t)    'sectoral investment'
   KK0(j,t)    'capital stock'
   CC0(t)      'composite consumption or felicity'
   FF0(h,t)    'factor endowment'
   pf0(h,j,t)  'factor price'
   py0(j,t)    'composite factor price'
   pz0(j,t)    'gross output price'
   pq0(i,t)    "Armington's composite good price"
   pe0(i,t)    'export price in local currency'
   pm0(i,t)    'import price in local currency'
   pd0(i,t)    'domestic good price'
   pk0(t)      'composite investment goods price'
   epsilon0(t) 'exchange rate'
   PRICE0(t)   'numeraire price'

* Exogenous variables
   Xg0(i,t)    'government consumption'
   Sf0(t)      'foreign savings in US dollars'
   pWe(i)      'export price in US dollars'
   pWm(i)      'import price in US dollars'
   tauz(i)     'production tax rate'
   taum(i)     'import tariff rate'

* for result reporting
   Y1(j,t)     'composite factor'
   F1(h,j,t)   'factor input'
   X1(i,j,t)   'intermediate input'
   Z1(j,t)     'gross output'
   Xp1(i,t)    'household consumption'
   Xv1(i,t)    'investment demand'
   E1(i,t)     'exports'
   M1(i,t)     'imports'
   Q1(i,t)     "Armington's composite good"
   D1(i,t)     'domestic good'
   Sp1(t)      'private saving'
   Td1(t)      'direct tax'
   Tz1(j,t)    'production tax'
   Tm1(i,t)    'import tariff'
   FF1(h,t)    'initial sectoral factor uses'
   II1(j,t)    'sectoral investment'
   III1(t)     'composite investment'
   KK1(j,t)    'sectoral capital stock'
   CC1(t)      'utility'
   tauz1(i,t)  'production tax rates'
   taum1(i,t)  'import tariff rates'
   pz1(j,t)    'gross output price'
   pd1(j,t)    'domestic good price'
   pm1(j,t)    'import price'
   pe1(j,t)    'export price'
   pq1(j,t)    "Armington's composite good price"
   pf1(h,j,t)  'factor price'
   py1(j,t)    'composite factor price'
   epsilon1(t) 'foreign exchange rate'
   pk1(t)      'capital good price'
   PRICE1(t)   'numeraire price';

Td00      = SAM("GOV","HOH");
Tz00(j)   = SAM("IDT",j);
Tm00(j)   = SAM("TRF",j);
F00(h,j)  = SAM(h,j);
Y00(j)    = sum(h, F00(h,j));
X00(i,j)  = SAM(i,j);
Z00(j)    = Y00(j) + sum(i, X00(i,j));
M00(i)    = SAM("EXT",i);
tauz00(j) = Tz00(j)/Z00(j);
taum00(j) = Tm00(j)/M00(j);
Xp00(i)   = SAM(i,"HOH");
CC00      = sum(i, Xp00(i));
FF00(h)   = SAM("HOH",h);
E00(i)    = SAM(i,"EXT");
D00(i)    = (1 + tauz00(i))*Z00(i) - E00(i);
Q00(i)    = (1 + taum00(i))*M00(i) + D00(i);
Sf00      = SAM("INV","EXT");

Xv00(i) = SAM(i,"INV");
* Reallocating the gap made by the inv. adjustment to gov. cons.
Xg00(i) = SAM(i,"GOV");

* Computing the direct tax revenue that balances the gov. budget
Td00    = sum(i, Xg00(i)) - sum(i, Tz00(i) + Tm00(i));

* Computing the household sav. that balances the household budget
Sp00    = sum(h, FF00(h)) - (sum(i, Xp00(i)) + Td00);
III00   = sum(i, Xv00(i));
II00(j) = (Sp00 + Sf00)*F00("CAP",j)/sum(i, F00("CAP",i));
KK00(j) = F00("CAP",j)/ror;


* Setting exogenous variables
Xg0(i,t) = Xg00(i)*(1 + pop)**(ord(t) - 1);
Sf0(t)   = Sf00   *(1 + pop)**(ord(t) - 1);
pWe(i)   = 1;
pWm(i)   = 1;
tauz(i)  = tauz00(i);
taum(i)  = taum00(i);

* ===============================================================
* Calibration ---------------------------------------------------
* ===============================================================
Parameter
   sigma(i) 'elasticity of substitution'
   psi(i)   'elasticity of transformation'
   eta(i)   'substitution elasticity parameter'
   phi(i)   'transformation elasticity parameter';

*sigma(i) = 2;
*psi(i)   = 2;
$load sigma psi
eta(i)   = (sigma(i) - 1)/sigma(i);
phi(i)   = (psi(i)   + 1)/psi(i);

Parameter
   alpha(i)  'share par. in composite cons. func.'
   a         'scale par. in composite cons. func.'
   beta(h,j) 'share par. in production func.'
   b(j)      'scale par. in production func.'
   ax(i,j)   'intermediate input requirement coeff.'
   ay(j)     'composite fact. input req. coeff.'
   lambda(i) 'investment demand share'
   iota      'scale par. in comp. inv. prod. func.'
   deltam(i) 'share par. in Armington func.'
   deltad(i) 'share par. in Armington func.'
   gamma(i)  'scale par. in Armington func.'
   xid(i)    'share par. in transformation func.'
   xie(i)    'share par. in transformation func.'
   theta(i)  'scale par. in transformation func.'
   ssp       'propensity to save';

alpha(i)  = Xp00(i)/sum(j, Xp00(j));
a         = CC00/prod(j, Xp00(j)**alpha(j));
beta(h,j) = F00(h,j)/sum(k, F00(k,j));
b(j)      = Y00(j)/prod(h, F00(h,j)**beta(h,j));
ax(i,j)   = X00(i,j)/Z00(j);
ay(j)     = Y00(j) /Z00(j);
lambda(i) = Xv00(i)/sum(j, Xv00(j));
iota      = III00/prod(i, Xv00(i)**lambda(i));
deltam(i) = (1 + taum00(i))*M00(i)**(1 - eta(i))/((1 + taum00(i))*M00(i)**(1 - eta(i))+D00(i)**(1 - eta(i)));
deltad(i) = D00(i)**(1 - eta(i))/((1 + taum00(i))*M00(i)**(1 - eta(i)) + D00(i)**(1 - eta(i)));
gamma(i)  = Q00(i)/(deltam(i)*M00(i)**eta(i)+ deltad(i)*D00(i)**eta(i))**(1/eta(i));
xie(i)    = E00(i)**(1 - phi(i))/(E00(i)**(1 - phi(i)) + D00(i)**(1 - phi(i)));
xid(i)    = D00(i)**(1 - phi(i))/(E00(i)**(1 - phi(i)) + D00(i)**(1 - phi(i)));
theta(i)  = Z00(i)/(xie(i)*E00(i)**phi(i)+ xid(i)*D00(i)**phi(i))**(1/phi(i));
ssp       = Sp00/(sum((h,j), F00(h,j)) - Td00);

*display alpha, a, beta, b, ax, ay, lambda, deltam, deltad, gamma, xie, xid, theta, iota, ssp;

* ===============================================================
* Defining model system -----------------------------------------
* ===============================================================
Variable
   Y(j)    'composite factor'
   F(h,j)  'factor input'
   X(i,j)  'intermediate input'
   Z(j)    'gross domestic output'
   Xp(i)   'household consumption'
   Xg(i)   'government consumption'
   Xv(i)   'investment demand'
   E(i)    'exports'
   M(i)    'imports'
   Q(i)    "Armington's composite good"
   D(i)    'domestic good'
   FF(h)   'factor endowments'
   pf(h,j) 'factor price'
   py(j)   'composite factor price'
   pz(j)   'supply price of gross domestic output'
   pq(i)   "Armington's composite good price"
   pe(i)   'export price in local currency'
   pm(i)   'import price in local currency'
   pd(i)   'domestic good price'
   pk      'composite investment goods price'
   epsilon 'exchange rate'
   Sp      'private savings'
   Sf      'foreign savings'
   Td      'direct tax'
   Tz(j)   'production tax'
   Tm(i)   'import tariff'
   KK(j)   'capital stock'
   II(j)   'sectoral investment'
   III     'composite investment'
   PRICE   'numeraire price'
   CC      'composite consumption';

Equation
   eqpy(j)          'composite factor prod. func.'
   eqF(h,j)         'factor demand function'
   eqX(i,j)         'intermediate demand function'
   eqY(j)           'composite factor demand function'
   eqpzs(j)         'unit cost function'
   eqTd             'direct tax revenue function'
   eqTz(j)          'production tax revenue function'
   eqTm(i)          'import tariff revenue function'
   eqXv(i)          'investment demand function'
   eqSp             'private saving function'
   eqXp(i)          'household demand function'
   eqpe(i)          'world export price equation'
   eqpm(i)          'world import price equation'
   eqepsilon        'balance of payments'
   eqpqs(i)         'Armington function'
   eqM(i)           'import demand function'
   eqD(i)           'domestic good demand function'
   eqpzd(i)         'transformation function'
   eqE(i)           'export supply function'
   eqDs(i)          'domestic good supply function'
   eqpqd(i)         'market clearing cond. for comp. good'
   eqpf1(h_mob)     'mobile factor market clearing cond.'
   eqpf2(h_mob,i,j) 'mobile factor market clearing cond.'
   eqpf3(j)         'immobile factor market clearing cond.'
   eqpk             'composite inv. goods mar. clear. cond.'
   eqIII            'composite inv. goods production func.'
   eqII(j)          'evolution of target capital stocks'
   eqCC             'composite consumption production func.'
   eqPRICE          'numeraire price';

* ===============================================================
* Model equations
* ===============================================================
*[domestic production] -
* composite factor production func.                  (Cobb-Douglas)
eqpy(j)..       Y(j) =e= b(j)*prod(h, F(h,j)**beta(h,j));

* factor demand function                             (Cobb-Douglas)
eqF(h,j)..      F(h,j) =e= beta(h,j)*py(j)*Y(j)/pf(h,j);

* intermediate input demand function                     (Leontief)
eqX(i,j)..      X(i,j) =e= ax(i,j)*Z(j);

* composite factor demand function                       (Leontief)
eqY(j)..        Y(j) =e= ay(j)*Z(j);

* unit price of gross output                             (Leontief)
eqpzs(j)..      pz(j) =e= ay(j)*py(j) + sum(i, ax(i,j)*pq(i));

*[government behavior] -
* lump sum direct tax revenue
eqTd..          Td =e= sum(i, pq(i)*Xg(i)) - sum(i, Tm(i) + Tz(i));

* production tax revenue
eqTz(j)..       Tz(j) =e= tauz(j)*pz(j)*Z(j);

* import tariff revenueeta
eqTm(i)..       Tm(i) =e= taum(i)*pm(i)*M(i);

*[investment behavior] -
* composite investment production function
eqXv(i)..       Xv(i) =e= lambda(i)*pk*sum(j, II(j))/pq(i);

*[savings] ----------
* savings function
eqSp..          Sp =e= ssp*(sum((h,j), pf(h,j)*F(h,j)) - Td);

*[household consumption] --                          (Cobb-Douglas)
eqXp(i)..       Xp(i) =e= alpha(i)*(sum((h,j), pf(h,j)*F(h,j)) - Sp - Td)/pq(i);

*[international trade] --
eqpe(i)..       pe(i) =e= epsilon*pWe(i);

eqpm(i)..       pm(i) =e= epsilon*pWm(i);

* BOP constraint
eqepsilon..     sum(i, pWe(i)*E(i)) + Sf =e= sum(i, pWm(i)*M(i));

*[Armington function] --
* Armington's composite good production function              (CES)
eqpqs(i)..      Q(i) =e= gamma(i)*(deltam(i)*M(i)**eta(i) + deltad(i)*D(i)**eta(i))**(1/eta(i));

* import demand function                                      (CES)
eqM(i)..        M(i) =e= (gamma(i)**eta(i)*deltam(i)*pq(i)/((1 + taum(i))*pm(i)))**(1/(1 - eta(i)))*Q(i);

* domestic good demand function                               (CES)
eqD(i)..        D(i) =e= (gamma(i)**eta(i)*deltad(i)*pq(i)/pd(i))**(1/(1 - eta(i)))*Q(i);

*[transformation function] --
* gross domestic output disaggregation function               (CET)
eqpzd(i)..      Z(i)  =e= theta(i)*(xie(i)*E(i)**phi(i) + xid(i)*D(i)**phi(i))**(1/phi(i));

*export supply function                                       (CET)
eqE(i)..        E(i)  =e= (theta(i)**phi(i)*xie(i)*(1 + tauz(i))*pz(i)/pe(i))**(1/(1 - phi(i)))*Z(i);

*domestic good supply function                                (CET)
eqDs(i)..       D(i)  =e= (theta(i)**phi(i)*xid(i)*(1 + tauz(i))*pz(i)/pd(i))**(1/(1 - phi(i)))*Z(i);

*[market clearing condition]
*Arminton's composite good market
eqpqd(i)..      Q(i)  =e= Xp(i) + Xg(i) + Xv(i) + sum(j, X(i,j));

*labor market: quantity
eqpf1(h_mob)..  sum(j, F(h_mob,j)) =e= FF(h_mob);

*labor market: price
eqpf2(h_mob,i,j).. pf(h_mob,j) =e= pf(h_mob,i);

*capital market
eqpf3(j).. F("CAP",j) =e= ror*KK(j);

*investment goods market
eqpk..     sum(j, II(j)) =e= III;

*[dynamic equations]
*composite investment good market clearing condition
eqIII..    III =e= iota*prod(i, Xv(i)**lambda(i));

*sectoral investment allocation
eqII(j).. pk*II(j) =e= pf("CAP",j)**zeta*F("CAP",j)/sum(i, pf("CAP",i)**zeta*F("CAP",i))*(Sp + epsilon*Sf);

*felicity function
eqCC..  CC =e= a*prod(i, Xp(i)**alpha(i));

* Price level [numeraire]
eqPRICE.. PRICE =e= sum(j, pq(j)*Q00(j)/sum(i,Q00(i)));


* ===============================================================
* Initializing variables ----------------------------------------
* ===============================================================
Y.l(j)    = Y00(j);
F.l(h,j)  = F00(h,j);
X.l(i,j)  = X00(i,j);
Z.l(j)    = Z00(j);
Xp.l(i)   = Xp00(i);
Xv.l(i)   = Xv00(i);
E.l(i)    = E00(i);
M.l(i)    = M00(i);
Q.l(i)    = Q00(i);
D.l(i)    = D00(i);
pf.l(h,j) = 1;
py.l(j)   = 1;
pz.l(j)   = 1;
pq.l(i)   = 1;
pe.l(i)   = 1;
pm.l(i)   = 1;
pd.l(i)   = 1;
pk.l      = 1;
epsilon.l = 1;
Sp.l      = Sp00;
Td.l      = Td00;
Tz.l(j)   = Tz00(j);
Tm.l(i)   = Tm00(i);
FF.l(h)   = FF00(h);
III.l     = III00;
II.l(j)   = II00(j);

* ---------------------------------------------------------------
* Numeraire
PRICE.fx = 1;

* Initial factor endowments and exogenous variables
FF.fx(h_mob) = FF00(h_mob);
KK.fx(j)     = KK00(j);
Xg.fx(i)     = Xg00(i);
Sf.fx        = Sf00;

* ===============================================================
* Defining and solving the model --------------------------------
* ===============================================================
Model dyncge / all /;

option solveLink = %solveLink.loadlibrary%;

solve dyncge maximizing CC using nlp;

option limRow = 0, limCol = 0, solPrint = off;


* ===============================================================
* Simulation Run
* ===============================================================

loop(t,
   solve dyncge maximizing CC using nlp;

*  storing results -------------------------
   Y1(j,t)     = Y.l(j);
   F1(h,j,t)   = F.l(h,j);
   X1(i,j,t)   = X.l(i,j);
   Z1(j,t)     = Z.l(j);
   Xp1(i,t)    = Xp.l(i);
   Xv1(i,t)    = Xv.l(i);
   E1(i,t)     = E.l(i);
   M1(i,t)     = M.l(i);
   Q1(i,t)     = Q.l(i);
   D1(i,t)     = D.l(i);
   Sp1(t)      = Sp.l;
   Td1(t)      = Td.l;
   Tz1(j,t)    = Tz.l(j);
   Tm1(i,t)    = Tm.l(i);
   FF1(h,t)    = FF.l(h);
   II1(j,t)    = II.l(j);
   III1(t)     = III.l;
   KK1(j,t)    = KK.l(j);
   CC1(t)      = CC.l;
   tauz1(i,t)  = tauz(i);
   taum1(i,t)  = taum(i);
   pf1(h,j,t)  = pf.l(h,j);
   py1(j,t)    = py.l(j);
   pz1(j,t)    = pz.l(j);
   pd1(j,t)    = pd.l(j);
   pe1(j,t)    = pe.l(j);
   pm1(j,t)    = pm.l(j);
   pq1(j,t)    = pq.l(j);
   pk1(t)      = pk.l;
   epsilon1(t) = epsilon.l;
   PRICE1(t)   = PRICE.l;

*  updating the state variables --------------
   FF.fx(h_mob) = FF.l(h_mob)*(1 + pop);
   KK.fx(j)     = (1 - dep)*KK.l(j) + II.l(j);
   Xg.fx(i)     = Xg0(i,t+1);
   Sf.fx        = Sf0(t+1);
);

* ===============================================================
* GDX file output
* ===============================================================
parameters
   Z2(t) "sum of sectoral output"
   Z3(j, t) "exports"
   Z4(t) "sum of exports"
   Z5(t) "exchange rate"
   Z6(j,t) "sectoral imports"
   Z7(t) "sum of imports"
;

Z2(t) = sum(j, Z1(j,t) );
Z3(j, t) = E1(j, t);
Z4(t) = sum(j, Z3(j,t) );
Z5(t) = epsilon1(t);
Z6(j,t) = M1(j,t);
Z7(t) = sum(j, Z6(j,t) );

execute_unload "result.gdx" Z1 Z2 Z3 Z4 Z5 Z6 Z7;
