(* ::Package:: *)

(*\:5f15\:7528*)
(*     
Import["E:\\ColdLab\\Program\\Mathematica\\myLib\\myLaser.m"]
*)


(* ::Subsubsection:: *)
(*\:6a21\:62df\:6ce2\:7247*)


zhWPArbPlotTips="Manipulate[\[IndentingNewLine]ListPlot[zhWPArbPlot[\[Pi]/2,u],PlotRange\[Rule]{{-1,1},{-1,1}},Frame\[Rule]True,AspectRatio\[Rule]1,Axes\[Rule]False,PlotLabel\[Rule]zhWPArbC[\[Pi]/2,u]],{u,0,\[Pi]}]"



(*(*\:8f93\:51fa\:95ed\:5408\:7684\:76f8\:4f4d\:56fe*)(*\[Phi]\:662f\:76f8\:4f4d\:5ef6\:8fdf \:5982\[Lambda]/4\:6ce2\:7247\:7684\[Phi]=\[Pi]/2  ;   \[Alpha]\:662f\:6ce2\:7247\:4e0e\:7ebf\:504f\:632f\:ff08x\:8f74\:ff09\:7684\:5939\:89d2*)
zhWPArbPlot[\[Phi]_,\[Alpha]_]:=Module[{e2},
a=Cos[-\[Alpha]];
b=Sin[-\[Alpha]];
R={{Cos[\[Alpha]],-Sin[\[Alpha]]},{Sin[\[Alpha]],Cos[\[Alpha]]}};
H=a;
V=Exp[I*\[Phi]]*b;
e=R.{H,V};
\[Theta]=Table[i,{i,0,2*\[Pi],0.1}];
e2=Transpose[{Re[e[[1]]*Exp[I*\[Theta]]],Re[e[[2]]*Exp[I*\[Theta]]]}]
]*)

(*\:8f93\:51fa\:95ed\:5408\:7684\:76f8\:4f4d\:56fe*)(*\[Phi]\:662f\:76f8\:4f4d\:5ef6\:8fdf \:5982\[Lambda]/4\:6ce2\:7247\:7684\[Phi]=\[Pi]/2  ;   \[Alpha]\:662f\:6ce2\:7247\:4e0e\:7ebf\:504f\:632f\:ff08x\:8f74\:ff09\:7684\:5939\:89d2*)
zhWPArbPlot[\[Phi]_,\[Alpha]_]:=Module[{e2},
e1={1,0};
R1={{Cos[-\[Alpha]],-Sin[-\[Alpha]]},{Sin[-\[Alpha]],Cos[-\[Alpha]]}};
R2={{Cos[\[Alpha]],-Sin[\[Alpha]]},{Sin[\[Alpha]],Cos[\[Alpha]]}};
W1={{1,0},{0,Exp[I*\[Phi]]}};
M=R2.W1.R1;
e=M.e1;
\[Theta]=Table[i,{i,0,2*\[Pi],0.1}];
e2=Transpose[{Re[e[[1]]*Exp[I*\[Theta]]],Re[e[[2]]*Exp[I*\[Theta]]]}]
]


(*\:8f93\:51fa\:504f\:632f\:5ea6*)(*\[Phi]\:662f\:76f8\:4f4d\:5ef6\:8fdf \:5982\[Lambda]/4\:6ce2\:7247\:7684\[Phi]=\[Pi]/2  ;   \[Alpha]\:662f\:6ce2\:7247\:4e0e\:7ebf\:504f\:632f\:7684\:5939\:89d2*)
zhWPArbC[\[Phi]_,\[Alpha]_]:=Module[{c},
a=Cos[-\[Alpha]];
b=Sin[-\[Alpha]];
R={{Cos[\[Alpha]],-Sin[\[Alpha]]},{Sin[\[Alpha]],Cos[\[Alpha]]}};
H=a;
V=Exp[I*\[Phi]]*b;
(*e=R.{H,V};*)
e={H,V,0};
cc=Im[Cross[e,Conjugate[e]]];(*\:53c9\:4e58*)
c=cc[[3]]
]



(*\:8f93\:51fa\:504f\:632f\:5411\:91cf*)(*ein\:662f\:521d\:59cb\:504f\:632f\:5411\:91cf{x,y}  \[Phi]\:662f\:76f8\:4f4d\:5ef6\:8fdf \:5982\[Lambda]/4\:6ce2\:7247\:7684\[Phi]=\[Pi]/2  ;   \[Alpha]\:662f\:6ce2\:7247\:4e0e\:7ebf\:504f\:632f\:7684\:5939\:89d2*)
zhWParbVec[e1_,\[Phi]_,\[Alpha]_]:=Module[{e2},
R1={{Cos[-\[Alpha]],-Sin[-\[Alpha]]},{Sin[-\[Alpha]],Cos[-\[Alpha]]}};
R2={{Cos[\[Alpha]],-Sin[\[Alpha]]},{Sin[\[Alpha]],Cos[\[Alpha]]}};
W1={{1,0},{0,Exp[I*\[Phi]]}};
M=R2.W1.R1;
e=M.e1;
\[Theta]=Table[i,{i,0,2*\[Pi],0.1}];
e2=Transpose[{Re[e[[1]]*Exp[I*\[Theta]]],Re[e[[2]]*Exp[I*\[Theta]]]}]

]



(*\:8f93\:51fa\:95ed\:5408\:7684\:76f8\:4f4d\:56fe*)(*\:8f93\:5165\:504f\:632f\:5411\:91cf*)
zhPolarVecPlot[e_]:=Module[{e2},
\[Theta]=Table[i,{i,0,2*\[Pi],0.1}];
e2=Transpose[{Re[e[[1]]*Exp[I*\[Theta]]],Re[e[[2]]*Exp[I*\[Theta]]]}]
]


zhWPeigVec[\[Phi]_,\[Alpha]_]:=Module[{e2},
R1={{Cos[-\[Alpha]],-Sin[-\[Alpha]]},{Sin[-\[Alpha]],Cos[-\[Alpha]]}};
R2={{Cos[\[Alpha]],-Sin[\[Alpha]]},{Sin[\[Alpha]],Cos[\[Alpha]]}};
W1={{1,0},{0,Exp[I*\[Phi]]}};
M=R2.W1.R1;
e=M.e1;
\[Theta]=Table[i,{i,0,2*\[Pi],0.1}];
e2=Transpose[{Re[e[[1]]*Exp[I*\[Theta]]],Re[e[[2]]*Exp[I*\[Theta]]]}]

]
