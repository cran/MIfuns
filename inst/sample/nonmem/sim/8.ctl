$PROB 8 phase1 2 CMT like 1004 but diff. initial on V3
$INPUT C ID TIME SEQ=DROP EVID AMT DV SUBJ HOUR TAFD TAD LDOS MDV HEIGHT WT SEX AGE DOSE FED
$DATA ../../data/derived/phase1.csv IGNORE=C
$SUBROUTINE ADVAN4 TRANS4
$PK  CL=THETA(1)*EXP(ETA(1)) * THETA(6)**SEX * (WT/70)**THETA(7)
 V2 =THETA(2)*EXP(ETA(2))
 KA=THETA(3)*EXP(ETA(3))
 Q  =THETA(4)
 V3=THETA(5)
 S2=V2
 
$ERROR  Y=F*EXP(ERR(1)); + ERR(2)
 IPRE=F

$THETA 9.408
22.03
0.06965
4.473
113.1
0.8532
1.632
$OMEGA 0.2287
0.1382
0.06118
$SIGMA 0.06692
$SIMULATION ( 8003 NEW) ( 9004 UNIFORM) ONLYSIMULATION
$TABLE ID TIME DV WT SEX LDOS NOPRINT NOAPPEND FILE=sim.tab
