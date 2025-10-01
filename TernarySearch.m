(* ::Package:: *)

BeginPackage["TernarySearch`"];


TernarySearch::usage="TernarySearch[F,tmin,tmax,\[Epsilon]] finds a minimum of F between tmin and tmax, at precision \[Epsilon]"


TernarySearchTab::usage="TernarySearchTab[FTab,\[Delta]tTab] finds a minimum of FTab in \[Delta]tTab range"


Begin["Private`"];


Options[TernarySearch] = {"Monitor" -> False};


TernarySearch[Ftmp_,\[Delta]tmin_,\[Delta]tmax_,\[Epsilon]tmp_,opts:OptionsPattern[]]:=Module[{monitorQ,F=Ftmp,left=\[Delta]tmin,right=\[Delta]tmax,leftThird,rightThird,\[Epsilon]=\[Epsilon]tmp,minVal,leftSearchValues,rightSearchValues,searchValtmp},

monitorQ=OptionValue["Monitor"];

leftSearchValues={{left,F[left]}};
rightSearchValues={{right,F[right]}};

If[monitorQ,
	While[Abs[leftSearchValues[[-1,1]]-rightSearchValues[[-1,1]]]>\[Epsilon],
		leftThird=left+(right-left)/3;
		rightThird=right-(right-left)/3;
		searchValtmp={{leftThird,F[leftThird]},{rightThird,F[rightThird]}};
		If[searchValtmp[[1,2]]<searchValtmp[[2,2]],
			right=rightThird;
			AppendTo[rightSearchValues,searchValtmp[[2]]];
		,
			left=leftThird;
			AppendTo[leftSearchValues,searchValtmp[[1]]]
		];
		Print@Abs[leftSearchValues[[-1,1]]-rightSearchValues[[-1,1]]];
	];
,
		While[Abs[leftSearchValues[[-1,1]]-rightSearchValues[[-1,1]]]>\[Epsilon],
		leftThird=left+(right-left)/3;
		rightThird=right-(right-left)/3;
		searchValtmp={{leftThird,F[leftThird]},{rightThird,F[rightThird]}};
		If[searchValtmp[[1,2]]<searchValtmp[[2,2]],
			right=rightThird;
			AppendTo[rightSearchValues,searchValtmp[[2]]];
		,
			left=leftThird;
			AppendTo[leftSearchValues,searchValtmp[[1]]]
		];
	];
];

left=(left+right)/2;
right=left;

AppendTo[leftSearchValues,{left,F[left]}];
AppendTo[rightSearchValues,{right,F[right]}];
<|"leftSearch"->leftSearchValues,"rightSearch"->rightSearchValues|>
]


Options[TernarySearchTab] = {"Monitor" -> False};


TernarySearchTab[Ftmp_,\[Delta]tTabtmp_,opts:OptionsPattern[]]:=Module[{monitorQ,F=Ftmp,\[Delta]tTab=\[Delta]tTabtmp,leftPos=1,rightPos=Length[\[Delta]tTabtmp],minVal,leftSearchValues,rightSearchValues,searchValtmp,rightThird,leftThird},

monitorQ=OptionValue["Monitor"];

leftSearchValues={{\[Delta]tTab[[leftPos]],F[\[Delta]tTab[[leftPos]]]}};
rightSearchValues={{\[Delta]tTab[[rightPos]],F[\[Delta]tTab[[rightPos]]]}};

If[monitorQ,

While[leftPos!=rightPos,
	rightThird=Position[\[Delta]tTab,First@Nearest[\[Delta]tTab,\[Delta]tTab[[rightPos]]-(\[Delta]tTab[[rightPos]]-\[Delta]tTab[[leftPos]])/3]][[1,1]];
	leftThird=Position[\[Delta]tTab,First@Nearest[\[Delta]tTab,\[Delta]tTab[[leftPos]]+(\[Delta]tTab[[rightPos]]-\[Delta]tTab[[leftPos]])/3]][[1,1]];
	searchValtmp={{\[Delta]tTab[[leftThird]],F[\[Delta]tTab[[leftThird]]]},{\[Delta]tTab[[rightThird]],F[\[Delta]tTab[[rightThird]]]}};
	If[leftPos==rightPos-1,
		If[First@PositionSmallest@searchValtmp[[;;,2]]==1,
			leftPos=leftPos;
			rightPos=leftPos];
		If[First@PositionSmallest@searchValtmp[[;;,2]]==2,
			leftPos=rightPos;
			rightPos=rightPos],
		If[searchValtmp[[1,2]]<searchValtmp[[2,2]],
			rightPos=Position[\[Delta]tTab,First@Nearest[\[Delta]tTab,\[Delta]tTab[[rightPos]]-(\[Delta]tTab[[rightPos]]-\[Delta]tTab[[leftPos]])/3]][[1,1]],
			leftPos=Position[\[Delta]tTab,First@Nearest[\[Delta]tTab,\[Delta]tTab[[leftPos]]+(\[Delta]tTab[[rightPos]]-\[Delta]tTab[[leftPos]])/3]][[1,1]]
		]
];
AppendTo[leftSearchValues,{\[Delta]tTab[[leftPos]],F[\[Delta]tTab[[leftPos]]]}];
AppendTo[rightSearchValues,{\[Delta]tTab[[rightPos]],F[\[Delta]tTab[[rightPos]]]}];
Print@Abs[leftPos-rightPos];
];
,
While[leftPos!=rightPos,
	rightThird=Position[\[Delta]tTab,First@Nearest[\[Delta]tTab,\[Delta]tTab[[rightPos]]-(\[Delta]tTab[[rightPos]]-\[Delta]tTab[[leftPos]])/3]][[1,1]];
	leftThird=Position[\[Delta]tTab,First@Nearest[\[Delta]tTab,\[Delta]tTab[[leftPos]]+(\[Delta]tTab[[rightPos]]-\[Delta]tTab[[leftPos]])/3]][[1,1]];
	searchValtmp={{\[Delta]tTab[[leftThird]],F[\[Delta]tTab[[leftThird]]]},{\[Delta]tTab[[rightThird]],F[\[Delta]tTab[[rightThird]]]}};
	If[leftPos==rightPos-1,
		If[First@PositionSmallest@searchValtmp[[;;,2]]==1,
			leftPos=leftPos;
			rightPos=leftPos];
		If[First@PositionSmallest@searchValtmp[[;;,2]]==2,
			leftPos=rightPos;
			rightPos=rightPos],
		If[searchValtmp[[1,2]]<searchValtmp[[2,2]],
			rightPos=Position[\[Delta]tTab,First@Nearest[\[Delta]tTab,\[Delta]tTab[[rightPos]]-(\[Delta]tTab[[rightPos]]-\[Delta]tTab[[leftPos]])/3]][[1,1]],
			leftPos=Position[\[Delta]tTab,First@Nearest[\[Delta]tTab,\[Delta]tTab[[leftPos]]+(\[Delta]tTab[[rightPos]]-\[Delta]tTab[[leftPos]])/3]][[1,1]]
		]
];
AppendTo[leftSearchValues,{\[Delta]tTab[[leftPos]],F[\[Delta]tTab[[leftPos]]]}];
AppendTo[rightSearchValues,{\[Delta]tTab[[rightPos]],F[\[Delta]tTab[[rightPos]]]}];
]
];

<|"leftSearch"->leftSearchValues,"rightSearch"->rightSearchValues|>
]


End[];


EndPackage[];
