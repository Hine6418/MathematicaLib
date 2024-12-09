(* ::Package:: *)

(*\:5f15\:7528*)
(*
Import["E:\\ColdLab\\Program\\Mathematica\\myLib\\myCavity.m"]
*)

T1=4.4 10^-6;
T2=40. 10^-6;
T3=4.4 10^-6;
T4=4.4 10^-6;
L=22.2 10^-6;

Ttot=T1+T2+T3+T4;
Ltot=4 L;

ppm=10^-6;
vfsr=1.47165 10^9;
\[ScriptCapitalL]=Ttot+Ltot;
\[ScriptCapitalL]=168*10^-6;
F=(2*\[Pi])/\[ScriptCapitalL];
\[Kappa]=vfsr/F;







w0y=6.5 10^-6;
w0z=8.7 10^-6;
\[Lambda]=780 10^-9;
k780=(2. Pi)/\[Lambda];

\[Eta]=(6 F)/(Pi k780^2 w0y*w0z);
\[Eta]fs=6 /(k780^2 w0y*w0z);





(* ::Section:: *)
(*\:7a7a\:8154*)


(* ::Subsection:: *)
(*\:65b9\:6cd5\:4e00*)


zhCavityPtrPin[\[Delta]_]:=(T1 T2)/(\[ScriptCapitalL]/2)^2 1/(1+((2 \[Delta])/\[Kappa])^2);


(* ::Subsection:: *)
(*\:65b9\:6cd5\:4e8c*)


zhCavityPtrPin2[\[Delta]_]:=Module[{myOut},
t1=Sqrt[T1];
t2=Sqrt[T2];
\[Epsilon]c=(I*t1)/(\[ScriptCapitalL]/2)*(1/(1-I*(2 \[Delta])/\[Kappa]));
\[Epsilon]t=\[Epsilon]c*t2;
myOut=\[Epsilon]t*\[Epsilon]t\[Conjugate]
]


(* ::Section:: *)
(*\:8154\:589e\:5f3a\:5438\:6536*)


(* ::Subsection:: *)
(*\:65b9\:6cd5\:4e00*)


(* \:6b63\:5411Ptr \:4e0e Pin \:4e4b\:6bd4 *)
zhCavAtomPtrfPin[\[Delta]_,\[CapitalDelta]_]:=Module[{myOut},
\[CapitalGamma]= 6.06 10^6;
\[ScriptCapitalL]d=(-2 \[CapitalDelta] \[CapitalGamma])/(\[CapitalGamma]^2+4 \[CapitalDelta]^2);
\[ScriptCapitalL]a=\[CapitalGamma]^2/(\[CapitalGamma]^2+4 \[CapitalDelta]^2);
CEAbsorEbEf=(\[Eta] (I \[ScriptCapitalL]d-\[ScriptCapitalL]a))/((1+\[Eta] \[ScriptCapitalL]a)-I ((2 \[Delta])/\[Kappa]+\[Eta] \[ScriptCapitalL]d));
CEAbsorPtrPin=(T1 T2)/(\[ScriptCapitalL]/2)^2 1/(((1+\[Eta] \[ScriptCapitalL]a)^2)+(((2 \[Delta])/\[Kappa]+\[Eta] \[ScriptCapitalL]d)^2));
CorrectionFactor=1/(1+(\[Eta] (\[ScriptCapitalL]d+I \[ScriptCapitalL]a))^2 1/((1+\[Eta] \[ScriptCapitalL]a)-I ((2 \[Delta])/\[Kappa]+\[Eta] \[ScriptCapitalL]d))^2);
myOut=CEAbsorPtrPin*Abs[CorrectionFactor]^2
]

(* \:80cc\:5411Ptr \:4e0e Pin \:4e4b\:6bd4 *)
zhCavAtomPtrbPin[\[Delta]_,\[CapitalDelta]_]:=Module[{myOut},
\[CapitalGamma]= 6.06 10^6;
\[ScriptCapitalL]d=(-2 \[CapitalDelta] \[CapitalGamma])/(\[CapitalGamma]^2+4 \[CapitalDelta]^2);
\[ScriptCapitalL]a=\[CapitalGamma]^2/(\[CapitalGamma]^2+4 \[CapitalDelta]^2);
CEAbsorEbEf=(\[Eta] (I \[ScriptCapitalL]d-\[ScriptCapitalL]a))/((1+\[Eta] \[ScriptCapitalL]a)-I ((2 \[Delta])/\[Kappa]+\[Eta] \[ScriptCapitalL]d));
myOut=Abs[CEAbsorEbEf]^2*zhCavAtomPtrfPin[\[Delta],\[CapitalDelta]]
]



(* P4pi \:4e0e Pin \:4e4b\:6bd4 *)
zhCavAtomAbsorP4Pin[Ti_,\[Delta]_,\[CapitalDelta]_]:=Module[{myOut},
\[CapitalGamma]= 6.06 10^6;
\[ScriptCapitalL]d=(-2 \[CapitalDelta] \[CapitalGamma])/(\[CapitalGamma]^2+4 \[CapitalDelta]^2);
\[ScriptCapitalL]a=\[CapitalGamma]^2/(\[CapitalGamma]^2+4 \[CapitalDelta]^2);
CEAbsorEbEf=(\[Eta] (I \[ScriptCapitalL]d-\[ScriptCapitalL]a))/((1+\[Eta] \[ScriptCapitalL]a)-I ((2 \[Delta])/\[Kappa]+\[Eta] \[ScriptCapitalL]d));
CorrectionFactor=1/(1+(\[Eta] (\[ScriptCapitalL]d+I \[ScriptCapitalL]a))^2 1/((1+\[Eta] \[ScriptCapitalL]a)-I ((2 \[Delta])/\[Kappa]+\[Eta] \[ScriptCapitalL]d))^2);
myOut=\[Eta] \[ScriptCapitalL] \[ScriptCapitalL]a Abs[(I Sqrt[Ti])/(\[ScriptCapitalL]/2) 1/((1+\[Eta] \[ScriptCapitalL]a)-I ((2 \[Delta])/\[Kappa]+\[Eta] \[ScriptCapitalL]d)) CorrectionFactor (1+CEAbsorEbEf)]^2
]


(* ::Subsection:: *)
(*\:65b9\:6cd5\:4e8c*)


(* \:6b63\:5411Ptr \:4e0e Pin \:4e4b\:6bd4 *)
zhCavAtomPtrfPin2[\[Delta]_,\[CapitalDelta]_]:=Module[{myOut},
t1=Sqrt[T1];
t2=Sqrt[T2];
\[Epsilon]0=8.854187817*10^-12;(*\:771f\:7a7a\:4ecb\:7535\:5e38\:6570*)
c=2.99792458*10^8;
\[CapitalGamma]=2*\[Pi]*6.06*10^6;(*\:89d2\:9891\:7387*)
\[Lambda]780=780.241*10^-9;
k780780=(2 \[Pi])/\[Lambda]780;
\[Nu]780=c/\[Lambda]780;
\[Omega]780=\[Nu]780*2*\[Pi];
\[Alpha]=6 \[Pi] \[Epsilon]0 c^3*(\[CapitalGamma]/\[Omega]780^2)/(\[Omega]780^2-(\[Omega]780+2 \[Pi] \[CapitalDelta])^2-I ((\[Omega]780+2 \[Pi] \[CapitalDelta])^3/\[Omega]780^2)\[CapitalGamma]);
\[Beta]=k780780/(\[Pi] w0y*w0z)*\[Alpha]/\[Epsilon]0;

rt1=\[ScriptCapitalL]/2*(1-I*(2 \[Delta])/\[Kappa])-I \[Beta];
rt2=\[ScriptCapitalL]/2*(1-I*(2 \[Delta])/\[Kappa])-I \[Beta](1+(I \[Beta])/rt1);
\[Epsilon]c=(I*t1)/rt2;
\[Epsilon]t=\[Epsilon]c*t2;
myOut=\[Epsilon]t*\[Epsilon]t\[Conjugate]
]

(* \:6b63\:5411Ptr \:4e0e Pin \:4e4b\:6bd4,\:4f46\:80cc\:5411\:573a\:6ca1\:6709\:5efa\:7acb\:5b8c\:6574 *)
zhCavAtomPtrfPin2v[\[Delta]_,\[CapitalDelta]_,br_]:=Module[{myOut},
t1=Sqrt[T1];
t2=Sqrt[T2];
\[Epsilon]0=8.854187817*10^-12;(*\:771f\:7a7a\:4ecb\:7535\:5e38\:6570*)
c=2.99792458*10^8;
\[CapitalGamma]=2*\[Pi]*6.06*10^6;(*\:89d2\:9891\:7387*)
\[Lambda]780=780.241*10^-9;
k780780=(2 \[Pi])/\[Lambda]780;
\[Nu]780=c/\[Lambda]780;
\[Omega]780=\[Nu]780*2*\[Pi];
\[Alpha]=6 \[Pi] \[Epsilon]0 c^3*(\[CapitalGamma]/\[Omega]780^2)/(\[Omega]780^2-(\[Omega]780+2 \[Pi] \[CapitalDelta])^2-I ((\[Omega]780+2 \[Pi] \[CapitalDelta])^3/\[Omega]780^2)\[CapitalGamma]);
\[Beta]=k780780/(\[Pi] w0y*w0z)*\[Alpha]/\[Epsilon]0;

rt1=\[ScriptCapitalL]/2*(1-I*(2 \[Delta])/\[Kappa])-I \[Beta];
rt2=\[ScriptCapitalL]/2*(1-I*(2 \[Delta])/\[Kappa])-I \[Beta](1+(I \[Beta]*br)/rt1);
\[Epsilon]c=(I*t1)/rt2;
\[Epsilon]t=\[Epsilon]c*t2;
myOut=\[Epsilon]t*\[Epsilon]t\[Conjugate]
]



(* \:80cc\:5411Ptr \:4e0e Pin \:4e4b\:6bd4 *)
zhCavAtomPtrbPin2[\[Delta]_,\[CapitalDelta]_]:=Module[{myOut},
t1=Sqrt[T1];
t2=Sqrt[T2];
\[Epsilon]0=8.854187817*10^-12;(*\:771f\:7a7a\:4ecb\:7535\:5e38\:6570*)
c=2.99792458*10^8;
\[CapitalGamma]=2*\[Pi]*6.06*10^6;(*\:89d2\:9891\:7387*)
\[Lambda]780=780.241*10^-9;
k780780=(2 \[Pi])/\[Lambda]780;
\[Nu]780=c/\[Lambda]780;
\[Omega]780=\[Nu]780*2*\[Pi];
\[Alpha]=6 \[Pi] \[Epsilon]0 c^3*(\[CapitalGamma]/\[Omega]780^2)/(\[Omega]780^2-(\[Omega]780+2 \[Pi] \[CapitalDelta])^2-I ((\[Omega]780+2 \[Pi] \[CapitalDelta])^3/\[Omega]780^2)\[CapitalGamma]);
\[Beta]=k780780/(\[Pi] w0y*w0z)*\[Alpha]/\[Epsilon]0;

rt1=\[ScriptCapitalL]/2*(1-I*(2 \[Delta])/\[Kappa])-I \[Beta];
rt2=\[ScriptCapitalL]/2*(1-I*(2 \[Delta])/\[Kappa])-I \[Beta](1+(I \[Beta])/rt1);
\[Epsilon]c=(I*t1)/rt2*(I \[Beta])/rt1;
\[Epsilon]t=\[Epsilon]c*t2;
myOut=\[Epsilon]t*\[Epsilon]t\[Conjugate]
]

(* P4pi \:4e0e Pin \:4e4b\:6bd4 *)
zhCavAtomAbsorP4Pin2[Ti_,\[Delta]_,\[CapitalDelta]_]:=Module[{myOut},
t1=Sqrt[Ti];
\[Epsilon]0=8.854187817*10^-12;(*\:771f\:7a7a\:4ecb\:7535\:5e38\:6570*)
c=2.99792458*10^8;
\[CapitalGamma]=2*\[Pi]*6.06*10^6;(*\:89d2\:9891\:7387*)
\[Lambda]780=780.241*10^-9;
k780780=(2 \[Pi])/\[Lambda]780;
\[Nu]780=c/\[Lambda]780;
\[Omega]780=\[Nu]780*2*\[Pi];
\[Alpha]=6 \[Pi] \[Epsilon]0 c^3*(\[CapitalGamma]/\[Omega]780^2)/(\[Omega]780^2-(\[Omega]780+2 \[Pi] \[CapitalDelta])^2-I ((\[Omega]780+2 \[Pi] \[CapitalDelta])^3/\[Omega]780^2)\[CapitalGamma]);
\[Beta]=k780780/(\[Pi] w0y*w0z)*\[Alpha]/\[Epsilon]0;

rt1=\[ScriptCapitalL]/2*(1-I*(2 \[Delta])/\[Kappa])-I \[Beta];
rt2=\[ScriptCapitalL]/2*(1-I*(2 \[Delta])/\[Kappa])-I \[Beta](1+(I \[Beta])/rt1);
\[Epsilon]c1=(I*t1)/rt2;
\[Epsilon]c2=(I*t1)/rt2*(I \[Beta])/rt1;

(*\:7535\:573a\:4e0e\:529f\:7387\:5b9a\:4e49\:95ee\:9898\:ff0c4\[Pi]\:8fd9\:4e2a\:4e0a\:8981*2*)
myOut=2*((\[Epsilon]c1+\[Epsilon]c2)*(\[Epsilon]c1+\[Epsilon]c2)\[Conjugate])*Im[\[Beta]]
]



(* ::Section:: *)
(*\:8154\:589e\:5f3a\:53d1\:5c04*)


(* Pt \:4e0e Pin \:4e4b\:6bd4 *)
zhCavAtomEmisPtPin[\[Delta]_,\[CapitalDelta]_,m\[Eta]_]:=Module[{myOut},
t1=Sqrt[T1];
t2=Sqrt[T2];
\[Epsilon]0=8.854187817*10^-12;(*\:771f\:7a7a\:4ecb\:7535\:5e38\:6570*)
c=2.99792458*10^8;
\[CapitalGamma]=2*\[Pi]*6.06*10^6;(*\:89d2\:9891\:7387*)
\[Lambda]780=780.241*10^-9;
k780780=(2 \[Pi])/\[Lambda]780;
\[Nu]780=c/\[Lambda]780;
\[Omega]780=\[Nu]780*2*\[Pi];
\[Alpha]=6 \[Pi] \[Epsilon]0 c^3*(\[CapitalGamma]/\[Omega]780^2)/(\[Omega]780^2-(\[Omega]780+2 \[Pi] \[CapitalDelta])^2-I ((\[Omega]780+2 \[Pi] \[CapitalDelta])^3/\[Omega]780^2)\[CapitalGamma]);
\[Beta]=k780780/(\[Pi] w0y*w0z)*\[Alpha]/\[Epsilon]0*m\[Eta]/\[Eta];
\[Epsilon]c=(I \[Beta])/(\[ScriptCapitalL]/2*(1-(I 2 \[Delta])/\[Kappa])-2 I \[Beta]);
\[Epsilon]t=\[Epsilon]c*t2;
myOut=\[Epsilon]t*\[Epsilon]t\[Conjugate]
]

zhCavAtomEmisP4Pin[\[Delta]_,\[CapitalDelta]_]:=Module[{myOut},
t1=Sqrt[T1];
t2=Sqrt[T2];
\[Epsilon]0=8.854187817*10^-12;(*\:771f\:7a7a\:4ecb\:7535\:5e38\:6570*)
c=2.99792458*10^8;
\[CapitalGamma]=2*\[Pi]*6.06*10^6;(*\:89d2\:9891\:7387*)
\[Lambda]780=780.241*10^-9;
k780780=(2 \[Pi])/\[Lambda]780;
\[Nu]780=c/\[Lambda]780;
\[Omega]780=\[Nu]780*2*\[Pi];
\[Alpha]=6 \[Pi] \[Epsilon]0 c^3*(\[CapitalGamma]/\[Omega]780^2)/(\[Omega]780^2-(\[Omega]780+2 \[Pi] \[CapitalDelta])^2-I ((\[Omega]780+2 \[Pi] \[CapitalDelta])^3/\[Omega]780^2)\[CapitalGamma]);
\[Beta]=k780780/(\[Pi] w0y*w0z)*\[Alpha]/\[Epsilon]0;
\[Epsilon]c=(I \[Beta])/(\[ScriptCapitalL]/2*(1-(I 2 \[Delta])/\[Kappa])-2 I \[Beta]);
\[Epsilon]4=I \[Beta] (2*\[Epsilon]c+1)/Sqrt[\[Eta]fs];
(*\:90fd\:884c*)
(*\[Epsilon]M=\[Epsilon]c*\[ScriptCapitalL]/2*(1-I*(2 \[Delta])/\[Kappa]);
\[Epsilon]4=\[Epsilon]M/Sqrt[\[Eta]fs];*)

myOut=\[Epsilon]4*\[Epsilon]4\[Conjugate]*2
]



(* ::Subsection:: *)
(*\:8003\:8651\:539f\:5b50\:8fd0\:52a8*)


(* Pt \:4e0e Pin \:4e4b\:6bd4 *)
cavityFlrT[\[CapitalDelta]_,\[Delta]_,T_,\[Eta]i_,s_]:=Module[{myOut},
t1=Sqrt[T1];
t2=Sqrt[T2];
\[Omega]r=2 \[Pi] 120 k780Hz;
x0=Sqrt[T/(100 uk780)]*131.8 nm;
\[Alpha]=6 \[Pi] \[Epsilon]0 c^3*(\[CapitalGamma]780/\[Omega]780^2)/(\[Omega]780^2-(\[Omega]780+2 \[Pi] \[CapitalDelta])^2-I ((\[Omega]780+2 \[Pi] \[CapitalDelta])^3/\[Omega]780^2)\[CapitalGamma]780);
\[Beta]=k780780/(\[Pi] w0y*w0z)*\[Alpha]/\[Epsilon]0*\[Eta]i/25;
\[CapitalGamma]=\[CapitalGamma]780/(2 \[Pi]);

s1=NDSolve[{\[Epsilon]1'[t]==(2 \[Pi] \[Kappa])/\[ScriptCapitalL] (-(\[ScriptCapitalL]/2)*(1-(I \[Delta])/(\[Kappa]/2))*\[Epsilon]1[t]+I \[Beta]*(Exp[-I k780780 x0 Sin[\[Omega]r t]]+\[Epsilon]1[t]+\[Epsilon]2[t]*Exp[-2 I k780780 x0 Sin[\[Omega]r t]])),\[Epsilon]2'[t]==(2 \[Pi] \[Kappa])/\[ScriptCapitalL] (-(\[ScriptCapitalL]/2)*(1-(I \[Delta])/(\[Kappa]/2))*\[Epsilon]2[t]+I \[Beta]*(Exp[I k780780 x0 Sin[\[Omega]r t]]+\[Epsilon]1[t]*Exp[2 I k780780 x0 Sin[\[Omega]r t]]+\[Epsilon]2[t])),\[Epsilon]1[0]==0,\[Epsilon]2[0]==0},{\[Epsilon]1,\[Epsilon]2},{t,0,0.4*10^-3}];
\[Epsilon]f[t_]=Evaluate[\[Epsilon]1[t]/.s1];
Pf[t_]=\[Epsilon]f[t]*\[Epsilon]f[t]\[Conjugate];

mPf=Mean[Table[Pf[t],{t,0.05*10^-3,0.1*10^-3,0.13*10^-6}]][[1]];
myOut=Re[mPf]/(s/(1+4*(\[CapitalDelta]/\[CapitalGamma])^2))*s/(1+4*(\[CapitalDelta]/\[CapitalGamma])^2+s)

]
