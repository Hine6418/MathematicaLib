(* ::Package:: *)

(*\:5f15\:7528*)
(*
Import["E:\\ColdLab\\Program\\Mathematica\\myLib\\myAtom87Rb.m"]
*)


(* ::Subsubsection:: *)
(*1 \:8d85\:7cbe\:7ec6\:7ed3\:6784\:80fd\:7ea7\:5206\:88c2*)


zhHPFeneryShiftTips="zhHPFeneryShift[F_,J_,L_]
Example:
zhHPFeneryShift[3,3/2,1]
"


zhHPFeneryShift[F_,J_,L_]:=Module[{\[CapitalDelta]E},
\[HBar]=1.05457266*10^-34;
I1=3/2;
K=F (F+1)-I1 (I1+1)-J (J+1);
If[L==0,
Ahfs=2*\[Pi]*\[HBar]*3.417341305452*10^9;
Bhfs=0;
\[CapitalDelta]E=1/2 Ahfs*K;
,
If[J==1/2,
Ahfs=2*\[Pi]*\[HBar]*408.328*10^6;
Bhfs=0;
\[CapitalDelta]E=1/2 Ahfs*K;
,
Ahfs=2*\[Pi]*\[HBar]*84.7185*10^6;
Bhfs=2*\[Pi]*\[HBar]*12.4965*10^6;
\[CapitalDelta]E=1/2 Ahfs*K+Bhfs*(3/2*K (K+1)-2 I1 (I1+1) J (J+1))/(2 I1 (2I1-1)*2 J(2 J-1));
];(*END If J\[Equal]1/2 *)
];(*END If L\[Equal]"S" *)

\[CapitalDelta]E=\[CapitalDelta]E/(2*\[Pi]*\[HBar])
]


(* ::Subsubsection:: *)
(*\:5171\:632f\:5149\:9891\:7387*)


zhLevelFreqcyTips="zhLevelFreqcy[F1_,F2_,line_]
Example:
zhLevelFreqcy[2,3,\"D2\"]
"


zhLevelFreqcy[F1_,F2_,line_]:=Module[{\[CapitalDelta]E},
\[Nu]780=384.230484468*10^12;
\[Nu]798=377.1074635*10^12;
If[line=="D2",
\[CapitalDelta]E=\[Nu]780-zhHPFeneryShift[F1,1/2,0]+zhHPFeneryShift[F2,3/2,1];
,
\[CapitalDelta]E=\[Nu]798-zhHPFeneryShift[F1,1/2,0]+zhHPFeneryShift[F2,1/2,1];
];
\[CapitalDelta]E
]


(* ::Subsubsection:: *)
(*\:8ba1\:7b97AC\:65af\:5854\:514b\:6548\:5e94\:5f15\:8d77\:7684\:9631\:6df1*)


(*\:8f93\:5165\:683c\:5f0f\:63d0\:793a*)
zhTrapDepthTips="zhTrapDepth[\[Lambda]_,w0_,power_,\[ScriptCapitalP]_,gF_,mF_]
Example:
zhTrapDepth[820*\!\(\*SuperscriptBox[\(10\), \(-9\)]\),1*\!\(\*SuperscriptBox[\(10\), \(-6\)]\),2.7*\!\(\*SuperscriptBox[\(10\), \(-3\)]\),0,-1/2,1] ";


(*\:8f93\:51faHz\:5355\:4f4d*)
(*\:5149\:6ce2\:957f\:ff0c\:5149\:8170\:534a\:5f84\:ff0c\:5149\:529f\:7387,\:504f\:632f\:5ea6(\:7ebf\:504f\:4e3a0\:ff0c\:5706\:504f\[PlusMinus]1),g\:56e0\:5b50,\:603b\:78c1\:91cf\:5b50\:6570*)
zhTrapDepth[\[Lambda]_,w0_,power_,\[ScriptCapitalP]_,gF_,mF_]:=
Module[{Udip},
c=2.997*10^8;
\[Omega]=2*\[Pi]*c/\[Lambda];(*\:5149\:89d2\:9891\:7387*)
I0=(2*power)/(\[Pi]*w0^2) ;           (*\:4e2d\:5fc3\:5149\:5f3a*)
\[Omega]0780=2*\[Pi]*384.230484*10^12;
\[Omega]0795=2*\[Pi]*377.107463*10^12;
\[CapitalDelta]2F=\[Omega]0780-\[Omega];
\[CapitalDelta]1F=\[Omega]0795-\[Omega];
\[CapitalGamma]780=38.11*10^6;(*Subscript[S, 1/2]\[RightArrow]Subscript[P, 3/2]*)
Udip=(\[Pi] c^2 \[CapitalGamma]780)/(2*\[Omega]0780^3)*((2+\[ScriptCapitalP]*gF*mF)/\[CapitalDelta]2F+(1-\[ScriptCapitalP]*gF*mF)/\[CapitalDelta]1F)*I0;
\[HBar]=1.05457266*10^-34;
Udip=Udip/(2*\[Pi]*\[HBar])
]


(* ::Subsubsection:: *)
(*\:8ba1\:7b97\:6563\:5c04\:7387(800nm\:4ee5\:4e0a\:5927\:5931\:8c10)*)


(*\:8f93\:5165\:683c\:5f0f\:63d0\:793a*)
zhScatRateTips="zhScatRate[\[Lambda]_,w0_,power_]
Example:
zhScatRate[820*\!\(\*SuperscriptBox[\(10\), \(-9\)]\),1*\!\(\*SuperscriptBox[\(10\), \(-6\)]\),2.7*\!\(\*SuperscriptBox[\(10\), \(-3\)]\)] ";


(*\:6563\:5c04\:901f\:7387*)
zhScatRate[\[Lambda]_,w0_,power_]:=
Module[{\[CapitalGamma]sc},
\[HBar]=1.05457266*10^-34;
\[CapitalGamma]780=38.11*10^6;
\[CapitalGamma]795=36*10^6;
I0=(2*power)/(\[Pi]*w0^2) ;           (*\:4e2d\:5fc3\:5149\:5f3a*)
c=2.997*10^8;
\[Omega]=2*\[Pi]*c/\[Lambda];(*\:5149\:89d2\:9891\:7387*)
\[Omega]0780=2*\[Pi]*384.230484*10^12;
\[Omega]0795=2*\[Pi]*377.107463*10^12;
\[CapitalDelta]2F=\[Omega]0780-\[Omega];
\[CapitalDelta]1F=\[Omega]0795-\[Omega];
(*\[CapitalGamma]sc=(\[Pi] c^2 \[CapitalGamma]780^2)/(2 \[HBar] \[Omega]0780^3)*(2/\[CapitalDelta]2F^2+1/\[CapitalDelta]1F^2)*I0;*)
\[CapitalGamma]sc=(3\[Pi] (c^2) )/(2 \[HBar] \[Omega]0780^3)*(\[Omega]/\[Omega]0780)^3*(\[CapitalGamma]780/(\[Omega]0780-\[Omega])+\[CapitalGamma]780/(\[Omega]0780+\[Omega]))^2*I0+(3\[Pi] (c^2) )/(2 \[HBar] \[Omega]0795^3)*(\[Omega]/\[Omega]0795)^3*(\[CapitalGamma]795/(\[Omega]0795-\[Omega])+\[CapitalGamma]795/(\[Omega]0795+\[Omega]))^2*I0
]


(*
\[CapitalGamma]sc[\[CapitalDelta]1_,\[CapitalOmega]1_]:=Module[{myOut},
\[CapitalGamma]=2*\[Pi]*6.06*10^6;
\[CapitalDelta]2=2*\[Pi]*\[CapitalDelta]1;
myOut=\[CapitalGamma]*(\[CapitalOmega]1/\[CapitalGamma])^2/(1+4*(\[CapitalDelta]2/\[CapitalGamma])^2+2*(\[CapitalOmega]1/\[CapitalGamma])^2)
]


\[CapitalGamma]sc[\[CapitalDelta]1_,power_,w0_]:=Module[{myOut},
\[CapitalGamma]=2*\[Pi]*6.06*10^6;
\[CapitalDelta]2=2*\[Pi]*\[CapitalDelta]1;
Isat=16.7;
I0=(2*power)/(\[Pi]*w0^2);  
myOut=\[CapitalGamma]/2*(I0/Isat)/(1+4*(\[CapitalDelta]2/\[CapitalGamma])^2+I0/Isat)
]


\[CapitalGamma]sc[\[CapitalDelta]1_,s_]:=Module[{myOut},
\[CapitalGamma]=2*\[Pi]*6.06*10^6;
\[CapitalDelta]2=2*\[Pi]*\[CapitalDelta]1;
myOut=\[CapitalGamma]/2*s/(1+4*(\[CapitalDelta]2/\[CapitalGamma])^2+s)
]
*)

zhS[power_,w0_]:=Module[{myOut},
Isat=35.7;
I0=(2*power)/(\[Pi]*w0^2);  
myOut=I0/Isat
]





(* ::Subsubsection:: *)
(*\:8ba1\:7b97\:7535\:5076\:6781\:8dc3\:8fc1\:77e9\:9635\:5143*)


(* ::Text:: *)
(*\:53c2\:8003Rb87data P7 (36)*)


(*\:8f93\:5165\:683c\:5f0f\:63d0\:793a*)
zhDipoleTips="myDipole[F1_,mF1_,F2_,mF2_,line_]
Example:
\:8ba1\:7b97|2,2>\:5230|3,2>\:7684\:8dc3\:8fc1
zhDipole[2,2,3,2,\"D2\"];        \:8fd4\:56de\:7535\:5076\:6781\:8dddd
zhDipoleFactor[2,2,3,2,\"D2\"];  \:8fd4\:56de\:7535\:5076\:6781\:8ddd\:7cfb\:6570\:ff0c\:4e0eRb87 Table 9\:5bf9\:5e94"



(*\:5149\:7684\:8dc3\:8fc1\:77e9\:9635\:5143CG\:7cfb\:6570\:ff0c\:6700\:540e\:8981\:4e58\:4e0a<J||er||J'>\:3002\:5373D1\:7ebf\:4e582.992*e*a0\:ff0cD2\:4e584.227*e*a0*)
zhDipoleFactor[F1_,mF1_,F2_,mF2_,line_]:=Module[{FmFerFmF},
J1=1/2;
I0=3/2;
If[line=="D2",J2=3/2,J2=1/2;];
q=mF1-mF2;
FerF=(-1)^(F2+J1+1+I0)*Sqrt[(2 F2+1)*(2 J1+1)]*SixJSymbol[{J1,J2,1},{F2,F1,I0}];
FmFerFmF=FerF*(-1)^(F2-1+mF1)*Sqrt[(2 F1+1)]*ThreeJSymbol[{F2,mF2},{1,q},{F1,-mF1}]
]


(*\:5149\:7684\:8dc3\:8fc1\:7535\:5076\:6781\:8dddd*)
zhDipole[F1_,mF1_,F2_,mF2_,line_]:=Module[{dipole},
e=1.602*10^-19;
a0=0.529*10^-10;

J1=1/2;
I0=3/2;
If[line=="D2",
J2=3/2;JerJ=4.227*e*a0;,
J2=1/2;JerJ=2.992*e*a0;];

q=mF1-mF2;
FerF=(-1)^(F2+J1+1+I0)*Sqrt[(2 F2+1)*(2 J1+1)]*SixJSymbol[{J1,J2,1},{F2,F1,I0}];
FmFerFmF=FerF*(-1)^(F2-1+mF1)*Sqrt[(2 F1+1)]*ThreeJSymbol[{F2,mF2},{1,q},{F1,-mF1}];
dipole=FmFerFmF*JerJ
]


(* ::Subsubsection:: *)
(*\:8ba1\:7b97\:4e8c\:80fd\:7ea7\:62c9\:6bd4\:9891\:7387*)


zh\[CapitalOmega][Pm_,w0m_,F1_,mF1_,F2_,mF2_,line_]:=Module[{RabiFreq},
c=2.99792458*10^8;
\[Epsilon]0=8.854187817*10^-12;
\[HBar]=1.05457266*10^-34;

I1=(2*Pm)/(\[Pi]*w0m^2);
E1= Sqrt[(2 I1)/(c \[Epsilon]0)];
RabiFreq=E1/(2*\[Pi]*\[HBar])*zhDipole[F1,mF1,F2,mF2,line]
]


(* ::Subsubsection:: *)
(*\:8ba1\:7b97\:62c9\:66fc\:62c9\:6bd4\:9891\:7387*)


zhRamanPathS[E1_,polar1_,F1_,mF1_,\[Lambda]_]:=Module[{RanmanFreq},
\[HBar]=1.05457266*10^-34;
\[Lambda]780=780.241209686*10^-9;
\[Lambda]795=794.978850*10^-9;
c=2.99792458*10^8;

F3=F1;
mF3=mF1;
(*\:5148\:8ba93\:6001\:4e0e1\:6001\:76f8\:540c\:7b80\:5355*)
\[CapitalOmega]R=0; (*\:6e05\:7a7a\:50a8\:5b58\:53d8\:91cf*)

If[Abs[F1]>=Abs[mF1],   (*\:9996\:5148\:8981\:7269\:7406*)
	(*2\:6001\:7684\:78c1\:89d2\:52a8\:91cf*)
	If[polar1=="\[Sigma]+",mF2=mF1+1];
	If[polar1=="\[Pi]",mF2=mF1];
	If[polar1=="\[Sigma]-",mF2=mF1-1];

	(*D2\:7ebf*)
	F2={F1-1,F1,F1+1};(*\:53ef\:80fd\:76842'\:6001\:603b\:89d2\:52a8\:91cf*)(*\:4ec5\:9650Rb87\:ff0cD2\:4e0b\:4f7f\:7528*)

	For[i=1,i<=Length[F2],i++,
		If[Abs[F2[[i]]]>=Abs[mF2]&&Abs[F3]>=Abs[mF3],
			\[CapitalOmega]1=E1/\[HBar]*zhDipole[F1,mF1,F2[[i]],mF2,"D2"];
			\[CapitalDelta]=2*\[Pi]*(zhLevelFreqcy[F1,F2[[i]],"D2"]-c/\[Lambda]);   (*\:89d2\:9891\:7387*)
			\[CapitalOmega]R=\[CapitalOmega]R+(\[CapitalOmega]1*\[CapitalOmega]1)/(2 \[CapitalDelta]);
		];(*end if*)
	];(*end for*)

(*D1\:7ebf*)
	F2={1,2};(*\:53ef\:80fd\:76842'\:6001\:603b\:89d2\:52a8\:91cf*)(*\:4ec5\:9650Rb87\:ff0cD1\:4e0b\:4f7f\:7528*)
	F3=F1;(*\:5148\:8ba93\:6001\:4e0e1\:6001\:76f8\:540c\:7b80\:5355*)
	For[i=1,i<=Length[F2],i++,
		If[Abs[F2[[i]]]>=Abs[mF2]&&Abs[F3]>=Abs[mF3],
			\[CapitalOmega]1=E1/\[HBar]*zhDipole[F1,mF1,F2[[i]],mF2,"D1"];
			\[CapitalDelta]=2*\[Pi]*(zhLevelFreqcy[F1,F2[[i]],"D1"]-c/\[Lambda]);   (*\:89d2\:9891\:7387*)
			\[CapitalOmega]R=\[CapitalOmega]R+(\[CapitalOmega]1*\[CapitalOmega]1)/(2 \[CapitalDelta]);
		];(*end if*)
	];(*end for*)
	RanmanFreq=\[CapitalOmega]R/(2*\[Pi])
](*end if*)
]

(****************************************************************************************************)
zhRamanPathP[E1_,polar1_,F2_,mF2_,\[Lambda]_,J_]:=Module[{RanmanFreq},
\[HBar]=1.05457266*10^-34;
\[Lambda]780=780.241209686*10^-9;
\[Lambda]795=794.978850*10^-9;
c=2.99792458*10^8;

\[CapitalOmega]R=0; (*\:6e05\:7a7a\:50a8\:5b58\:53d8\:91cf*)
If[Abs[F2]>=Abs[mF2],   (*\:9996\:5148\:8981\:7269\:7406*)

(*1\:6001\:7684\:78c1\:89d2\:52a8\:91cf*)
If[polar1=="\[Sigma]+",mF1=mF2-1];
If[polar1=="\[Pi]",mF1=mF2];
If[polar1=="\[Sigma]-",mF1=mF2+1];

F1={1,2};(*\:53ef\:80fd\:76841\:6001\:603b\:89d2\:52a8\:91cf*)

(*D2\:7ebf*)
If[J==3/2,
	For[i=1,i<=Length[F1],i++,
		If[Abs[F2]>=Abs[mF2]&&Abs[F1[[i]]]>=Abs[mF1]&&Abs[F1[[i]]-F2]<=1,
			\[CapitalOmega]1=E1/\[HBar]*zhDipole[F1[[i]],mF1,F2,mF2,"D2"];
			\[CapitalDelta]=2*\[Pi]*(zhLevelFreqcy[F1[[i]],F2,"D2"]-c/\[Lambda]);   (*\:89d2\:9891\:7387*)
			\[CapitalOmega]R=\[CapitalOmega]R+(\[CapitalOmega]1*\[CapitalOmega]1)/(2 \[CapitalDelta]);
		];(*end if*)
	];(*end for*)
,
(*D1\:7ebf*)
	For[i=1,i<=Length[F2],i++,
		If[Abs[F2]>=Abs[mF2]&&Abs[F1[[i]]]>=Abs[mF1]&&Abs[F1[[i]]-F2]<=1,
			\[CapitalOmega]1=E1/\[HBar]*zhDipole[F1[[i]],mF1,F2,mF2,"D1"];
			\[CapitalDelta]=2*\[Pi]*(zhLevelFreqcy[F1[[i]],F2,"D1"]-c/\[Lambda]);   (*\:89d2\:9891\:7387*)
			\[CapitalOmega]R=\[CapitalOmega]R+(\[CapitalOmega]1*\[CapitalOmega]1)/(2 \[CapitalDelta]);
		];(*end if*)
	];(*end for*)
];(*end if*)

RanmanFreq=\[CapitalOmega]R/(2*\[Pi])
](*end if*)
]



(* ::Subsubsection:: *)
(*\:62c9\:66fc\:65b9\:6cd5\:8ba1\:7b97AC\:65af\:5854\:514b\:6548\:5e94*)


zhTrapDepthRamanTips="zhTrapDepthRaman[\[Lambda]_,w0_,power_,\[ScriptCapitalP]_,F_,mF_]
Example:
zhTrapDepthRaman[820*\!\(\*SuperscriptBox[\(10\), \(-9\)]\),1*\!\(\*SuperscriptBox[\(10\), \(-6\)]\),2.7*\!\(\*SuperscriptBox[\(10\), \(-3\)]\),\"\[Pi]\",2,0]"


zhTrapDepthRaman[\[Lambda]_,w0_,power_,\[ScriptCapitalP]_,F_,mF_]:=Module[{Udip},
c=2.997*10^8;
\[Epsilon]0=8.854187817*10^-12;
\[Lambda]780=780.241209686*10^-9;

I1=(2*power)/(\[Pi]*w0^2);
E1= Sqrt[(2 I1)/(c \[Epsilon]0)];

RanmanFreq=zhRamanPathS[E1,\[ScriptCapitalP],F,mF,\[Lambda]];
Udip=RanmanFreq/2
]

zhEnergyShiftRaman[\[Lambda]_,w0_,power_,\[ScriptCapitalP]_,F_,mF_,J_]:=Module[{Udip},
c=2.997*10^8;
\[Epsilon]0=8.854187817*10^-12;
\[Lambda]780=780.241209686*10^-9;

I1=(2*power)/(\[Pi]*w0^2);
E1= Sqrt[(2 I1)/(c \[Epsilon]0)];


RanmanFreq=zhRamanPathP[E1,\[ScriptCapitalP],F,mF,\[Lambda],J];

Udip=RanmanFreq/2
]

(*zhTrapDepthRaman2[\[CapitalDelta]_,w0_,power_,\[ScriptCapitalP]_,F_,mF_]:=Module[{Udip},
c=2.997*10^8;
\[Epsilon]0=8.854187817*10^-12;
\[Lambda]780=780.241209686*10^-9;

\[Nu]=zhLevelFreqcy[2,3,"D2"]+\[CapitalDelta];
\[Lambda]=c/\[Nu];

I1=(2*power)/(\[Pi]*w0^2);
E1= Sqrt[(2 I1)/(c \[Epsilon]0)];

RanmanFreq=zhRamanPathS[E1,\[ScriptCapitalP],F,mF,\[Lambda]];

Udip=RanmanFreq/2

]*)

