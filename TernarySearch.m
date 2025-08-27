(* ::Package:: *)

BeginPackage["TernarySearch`"];


TernarySearch::usage="TernarySearch[F,tmin,tmax,\[Epsilon]] finds a minimum of F between tmin and tmax, at precision \[Epsilon]"


Begin["Private`"];


Options[TernarySearch] = {"Monitor" -> False};


TernarySearch[Ftmp_,\[Delta]tmin_,\[Delta]tmax_,\[Epsilon]tmp_,opts:OptionsPattern[]]:=Module[{monitorQ,F=Ftmp,left=\[Delta]tmin,right=\[Delta]tmax,leftThird,rightThird,\[Epsilon]=\[Epsilon]tmp,minVal,leftSearchValues,rightSearchValues,searchValtmp},

monitorQ=OptionValue["Monitor"];

leftSearchValues={{left,F[left]}};
rightSearchValues={{right,F[right]}};

If[monitorQ,
	Monitor[
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
	,{Abs[leftSearchValues[[-1,1]]-rightSearchValues[[-1,1]]]}]
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


End[];


EndPackage[];
