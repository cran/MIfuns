$PROB 1005 phase1 2 CMT like 1004 but diff. initial on V3
$INPUT C ID TIME SEQ=DROP EVID AMT DV SUBJ HOUR TAFD TAD LDOS MDV HEIGHT WT SEX AGE DOSE FED
$DATA ../../data/ph1/derived/phase1.csv IGNORE=C
$SUBROUTINE ADVAN4 TRANS4
$PK
 CL=THETA(1)*EXP(ETA(1)) * THETA(6)**SEX * (WT/70)**THETA(7)
 V2 =THETA(2)*EXP(ETA(2))
 KA=THETA(3)*EXP(ETA(3))
 Q  =THETA(4)
 V3=THETA(5)
 S2=V2
 
$ERROR
 Y=F*EXP(ERR(1)); + ERR(2)
 IPRE=F

$MSFI=../1005/1005.msf
$\theta_1$' unit='$L/h$'    label='CL/F' model='$CL/F \sim \theta_6^{MALE} * (WT/70)^{\theta_7}$'>clearance</parameter>
(0,10,100)    ;V         <parameter name='THETA2' latex='$\theta_2$' unit='$L$'      label='Vc/F' model='$Vc/F \sim (WT/70)^{1}$'   >central volume</parameter>
(0,0.2, 5)    ;KA        <parameter name='THETA3' latex='$\theta_3$' unit='$h^{-1}$' label='Ka'                                     >absorption constant</parameter>
(0,10,50)     ;Q         <parameter name='THETA4' latex='$\theta_4$' unit='$L/h$'    label='Q/F'                                    >intercompartmental clearance</parameter>
(0,100,1000)  ;V3        <parameter name='THETA5' latex='$\theta_5$' unit='$L$'      label='Vp/F'                                   >peripheral volume</parameter>
(0,1,2)       ;SEX       <parameter name='THETA6' latex='$\theta_6$'                 label='Male.CL'                                >male effect on clearance</parameter>
(0,0.75,3)    ;WT on CL  <parameter name='THETA7' latex='$\theta_7$'                 label='WT.CL'                                  >weight effect on clearance</parameter>

;$OMEGA
$\Omega^{1.1}CL/F$'>interindividual variability on clearance</parameter>
;<parameter name='OMEGA2.2' label='$\Omega^{2.2}Vc/F$'>interindividual variability on central volume</parameter>
;<parameter name='OMEGA3.3' label='$\Omega^{3.3}Ka$'>interindividual variability on Ka</parameter>

;$SIGMA
$\sigma^{1.1}prop$'>proportional error</parameter>

$SIMULATION ONLYSIM (1968) SUBPROBLEMS=500
;$COV PRINT=E
$TABLE DV NOHEADER NOPRINT FILE=./1105.tab FORWARD NOAPPEND

