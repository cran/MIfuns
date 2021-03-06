$PROB 3 phase1 2 CMT like 1004 but diff. initial on V3
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

$THETA 9.371
22.89
0.06297
3.585
130.1
1.086
1.705
$OMEGA 0.1647
0.1277
0.113
$SIGMA 0.06041
$SIMULATION ( 7998 NEW) ( 8999 UNIFORM) ONLYSIMULATION
$TABLE ID TIME DV WT SEX LDOS NOPRINT NOAPPEND FILE=sim.tab
