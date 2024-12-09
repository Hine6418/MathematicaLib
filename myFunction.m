(* ::Package:: *)

(*
Import["E:\\ColdLab\\Program\\Mathematica\\myLib\\myFunction.m"]
*)

(*Maxwell\:901f\:7387\:5206\:5e03*)
(*\:5f85\:5b9a\:8d28\:91cf*)

zhFcMaxellTips="zhFcMaxell[m_,T_,v_]";
zhFcMaxell[m_,T_,v_]:=Module[{probDensy},
kB=1.3806503*10^-23;
\[Sigma]=Sqrt[(kB*T)/m];
probDensy=PDF[MaxwellDistribution[\[Sigma]],v]]

(*Rb87\:539f\:5b50*)
zhFcMaxell87RbTips="zhFcMaxell87Rb[T_,v_]
Example:
Plot[zhFcMaxell87Rb[100*\!\(\*SuperscriptBox[\(10\), \(-6\)]\),v],{v,0,0.5}] ";
zhFcMaxell87Rb[T_,v_]:=Module[{probDensy},
kB=1.3806503*10^-23;
mr=1.674*10^-27;
m87=mr*87;
\[Sigma]=Sqrt[(kB*T)/m87];
probDensy=PDF[MaxwellDistribution[\[Sigma]],v]]


(*\:6cca\:677e\:5206\:5e03*)
(*n\:4e3a\:5e73\:5747\:503c*)
zhFcPoisson[n_,x_]:=Module[{probDensy},
probDensy=(Exp[-n]*n^x)/x!]




(*\:5bfb\:627e\:51fd\:6570\:6700\:5927\:503c*)
(*

vList={};
For[i=-1 um,i<1 um,i=i+0.01 um,
AppendTo[vList,{i,du[i]}];
]
ListPlot[vList]
pmax=First[Position[vList[[;;,2]],Max[vList[[;;,2]]]]];
vList[[pmax,1]]


*)




(*\:6d1b\:4f26\:5179\:7ebf\:578b*)
zhLorentz[x_,LW_,x0_,a_,b_]=a/(1+((x-x0)/(LW/2))^2)+b;


(*\:9ad8\:65af\:5149\:516c\:5f0f*)

zhGauss[w0_,\[Lambda]_,z_,r_]:=Module[{myout},

zR=(\[Pi]*w0^2)/\[Lambda];
w=w0*Sqrt[1+(z/zR)^2];
myout=(w0/w[z])^2*Exp[-2*(r/w[z])^2];
]



(* ::Subsubsection:: *)
(*\:6ee4\:6ce2\:5668*)


(* ::Text:: *)
(*\:7406\:60f3\:4f4e\:901a\:6ee4\:6ce2*)


(*0<filterstrength<1*)
zhLowPassIdeal[myin_,filterstrength_]:=Module[{myout},
endfre=IntegerPart[Length[myin]/2*filterstrength];  (*\:4f4e\:901a\:7684\:622a\:81f3\:9891\:7387*) 
Fdatas=Fourier[myin];                                                                                                             (*\:5085\:91cc\:53f6\:53d8\:6362*)
FdatasFil=Fdatas;
FdatasFil[[Length[Fdatas]/2-endfre;;Length[Fdatas]/2+1+endfre]]=0;(*\:53bb\:6389\:9ad8\:9891\:9879*)
myout=Abs[InverseFourier[FdatasFil]] 
];


(* ::Text:: *)
(*\:6eda\:52a8\:5e73\:5747*)


zhRowAvg[myin_,d_]:=Module[{myout},
myout=Table[0,{x,1,Length[myin]}];
For[k1=1,k1<=Length[myin],k1=k1+1,
If[k1+d<=Length[myin],
myout[[k1]]=Mean[myin[[k1;;k1+d]]],
myout[[k1]]=Mean[myin[[k1;;Length[myin]]]],
];];
myout
];


