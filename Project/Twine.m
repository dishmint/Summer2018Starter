(* ::Package:: *)

(* ::Title:: *)
(*Twine Import/Export*)


BeginPackage["Twine`"]


(* ::Subsection::Closed:: *)
(*Generate Graph and Association from Twine Project*)


(* ::Subsubsection::Closed:: *)
(*Importer*)


Begin["`Private`"]

(*RICCARDO's XML PARSING FIX*)
Clear[parseAttrs, parseXML]
$tag =(WordCharacter|"-")..;

parseAttrs[expr_String] := Cases[
StringReplace[
expr,{
k:$tag~~"=\"" ~~ v:Shortest[___] ~~ "\"" :> {k -> v},
k:$tag~~"='" ~~ v:Shortest[___] ~~ "'" :> {k -> v},
k:$tag~~"=" ~~ v:$tag :> {k -> v},
k:$tag :> {k -> ""}
}
],
_Rule,
All
]

parseXML[expr_String] := Quiet[
List @@ StringReplace[
expr,
{"<" ~~ tag:$tag ~~ attrs:Shortest[___] ~~ ">" ~~ body:Shortest[___] ~~ "</" ~~ tag:$tag ~~ ">" :> XMLElement[tag, parseAttrs @ attrs, parseXML@body],
"<" ~~ tag:$tag ~~ attrs:Shortest[___] ~~ "/>" :> XMLElement[tag, parseAttrs @ attrs, {}]
}
],
StringExpression::cond
]

End[]


Clear[generateEdges]
Begin["`Private`"]
generateEdges[parentName_, links_]:= Thread[DirectedEdge[parentName, links]]
Clear[extractLinks]
extractLinks[passageContent_]:=
	StringReplace[
		DeleteCases[
			StringCases[passageContent,{
				"[["~~ShortestMatch[linkName___]~~"]]" /; !StringContainsQ[linkName, "->" | "<-" | "|"]:> linkName,
				ShortestMatch["[["~~__~~ShortestMatch["->"~~linkName__]~~"]]"] :> linkName,
				ShortestMatch["[["~~__~~ShortestMatch["|"~~linkName__]~~"]]"] :> linkName,
				ShortestMatch["[["~~ShortestMatch[linkName__ ~~ "<-"]~~__~~"]]"] :> linkName
				}],
		{}], {"&quot;" -> "\"", "&gt;"-> ">", "&lt;" -> "<", "&#39;"-> "'", "&amp;" -> "&", "&nbsp;" -> WhitespaceCharacter}]
End[]


Clear[twineImport]
twineImport[absoluteFilePath_String]:= Module[{fileAsString,fileSansHeader,file,xmlObject, rootNode,rootNodeName, passages,passageContentThread,passageAssociation,psgAssocCleaned,assocForm,edges,alledges,completeGraph, g, v},
	fileAsString = Import[absoluteFilePath, "String"];
	fileSansHeader = StringCases[fileAsString, htmlHeader___~~ important:("<tw-storydata"~~ ___ ~~ "</tw-storydata>")~~___ ~~EndOfString:> important](*/.{fileString_}\[Rule]fileString*);
	file = If[ListQ[fileSansHeader], fileSansHeader /.{fileString_} -> fileString, fileSansHeader ];

	xmlObject = parseXML[file];
	rootNode = Cases[xmlObject, XMLElement["tw-storydata", {Rule["name", name_],___}, _]];
	rootNodeName =Cases[xmlObject, XMLElement["tw-storydata", {Rule["name", name_],___}, _]:> name]/.{storytitle_}->storytitle;
	passages = Cases[rootNode, XMLElement["tw-passagedata", {___,Rule["name", name_],___}, content_]:> {name,content}, {0, Infinity}];
	
	(*Create Assocation between passage and content for tooltips and remove the link syntax*)
	passageContentThread= 
		AssociationThread[Transpose[passages][[1]] -> StringReplace[Transpose[passages][[2]],
			{
				"[["~~ShortestMatch[linkName___]~~"]]" /; !StringContainsQ[linkName, "->" | "<-" | "|"]:>linkName,
				ShortestMatch["[["~~hiddenName__~~ShortestMatch["->"~~__]~~"]]"] :> hiddenName,
				ShortestMatch["[["~~hiddenName__~~ShortestMatch["|"~~__]~~"]]"] :> hiddenName,
				ShortestMatch["[["~~ShortestMatch[__ ~~ "<-"]~~hiddenName__~~"]]"] :> hiddenName
			}]];
	
	passageAssociation =AssociateTo[passageContentThread, rootNodeName -> "Start Here"];
	
	edges =generateEdges[#[[1]], extractLinks[#[[2]]]]&/@passages;
	alledges= Flatten[{{rootNodeName \[DirectedEdge] First[passages][[1]]},DeleteCases[edges, {}]}];
	g =Graph[alledges];
	v =Table[Tooltip[VertexList[g][[i]], passageAssociation[VertexList[g][[i]]]], {i, 1, Length[VertexList[g]]}];
	completeGraph = Graph[v, alledges, VertexLabels->"Name",GraphLayout ->{"LayeredDigraphEmbedding", "RootVertex"->rootNodeName}];

	assocForm = Block[{passageContentWithLinks,completeAssociation},
		(*Create Assocation between passage and content*)
		passageContentWithLinks= AssociationThread[Transpose[passages][[1]] -> Transpose[passages][[2]]];
		(*Root Node Association*)
		completeAssociation = <|rootNodeName -> passageContentWithLinks|>];
{completeGraph, assocForm}
]
twineImport::usage = "twineImport[path/to/file.html] gives a graph and an association of a Twine 2 project";


(* ::Subsubsection::Closed:: *)
(*Exporter : Rewrite*)


Clear[twineExport]
twineExport[associationForm_Association, exportPathToFile_]:=Module[{storyAssoc = associationForm,twGraphVertices,twGraphPoints,sTemplateHeader,assocLength,passageAssoc,passages, stringOfStory},
(*Grab Vertex Points for Passage Positions*)
twGraphVertices = GraphEmbedding[twineSummary[storyAssoc, "Graph"]];
(*Echo[twGraphVertices, "Graph Vertices "];*)
twGraphPoints = Reverse[Rescale[Rescale[twGraphVertices],{-1,1}, {0, 250}]];
(*Echo[twGraphPoints, "Graph Points for Twine "];*)
sTemplateHeader = StringTemplate["
<tw-storydata name=\"`storyTitle`\" startnode=\"1\" creator=\"Twine\" creator-version=\"2.2.1\" zoom=\"1\" format=\"Harlowe\" format-version=\"2.1.0\" options=\"\" hidden>

  <style role=\"stylesheet\" id=\"twine-user-stylesheet\" type=\"text/twine-css\">
  </style>
  <script role=\"script\" id=\"twine-user-script\" type=\"text/twine-javascript\">
  </script>
"
][<|"storyTitle" -> First[Keys[storyAssoc]]|>];
passageAssoc = First[Values[storyAssoc]];
assocLength = Length[passageAssoc];
passages = Table[
StringTemplate["<tw-passagedata pid=\"`pid`\" 
name=\"`passagename`\" 
tags=\"\"
position=\"`xPos`,`yPos`\"
size=\"100,100\"
>`content`</tw-passagedata>"][<|"pid"-> index,"passagename" -> Keys[passageAssoc][[index]], "content" -> Values[passageAssoc][[index]], "xPos" -> twGraphPoints[[index]][[1]], "yPos" -> twGraphPoints[[index]][[2]] |>], {index, 1,assocLength,1}];
stringOfStory= StringJoin[{sTemplateHeader, passages, "</tw-storydata>"}];
Export[exportPathToFile <>".html",stringOfStory , "HTMLFragment"]
]
twineExport::usage = "twineExport[<| key1 \[Rule] val1 ... |>, path/to/file] Exports WL Twine Representation to the specified path";


(* ::Subsubsection:: *)
(*Summarizer (Graph, Graph + Passage Display, Export)*)


Clear[cleanPassages]
Begin["`Private`"]
cleanPassages[passageAssoc_]:= Module[{markdownReplacements,psgContent,psgContentNoLinks,psgContentMarkdownReplaced }, 
	markdownReplacements = {"&quot;" -> "\"", "&gt;"-> ">", "&lt;" -> "<", "&#39;"-> "'"};
	psgContent = Table[generateEdges[Flatten[Keys[Values[passageAssoc]]][[index]],Flatten[Values[Values[passageAssoc]]][[index]]], {index, 1, Length[Values[passageAssoc]/.{assc_}:> assc],1}];
	psgContentNoLinks = (psgContent /. DirectedEdge[id_,content_]:> DirectedEdge[id, StringReplace[content, 
	{"[["~~ShortestMatch[linkName___]~~"]]" /; !StringContainsQ[linkName, "->" | "<-" | "|"]:>linkName,
		ShortestMatch["[["~~hiddenName__~~ShortestMatch["->"~~__]~~"]]"] :> hiddenName,
		ShortestMatch["[["~~hiddenName__~~ShortestMatch["|"~~__]~~"]]"] :> hiddenName,
		ShortestMatch["[["~~ShortestMatch[__ ~~ "<-"]~~hiddenName__~~"]]"] :> hiddenName,
		"&quot;" -> "\"", "&gt;"-> ">", "&lt;" -> "<", "&#39;"-> "'", "&nbsp;" -> " ","&amp;" -> "&"
		}
	
	]]);
	psgContentMarkdownReplaced = (psgContentNoLinks /. DirectedEdge[id_,content_]:> DirectedEdge[id, StringReplace[content, markdownReplacements]])
]

End[]


Clear[twineSummary]
twineSummary[userAssoc_, outputForm_:"Summary"]:= Module[{firstEdge, passageEdges,cleanedPassages,buttonList,buttonGraph},

	Module[{activePassage = First[Keys[userAssoc]], psgKeys = Flatten[Keys[Values[userAssoc]]]},

	(*First Edge*)
	firstEdge = {First[Keys[userAssoc]]\[DirectedEdge] Flatten[Keys[Values[userAssoc]]][[1]]};

	(*Passage Edges*)
	passageEdges = Flatten@DeleteCases[Table[generateEdges[psgKeys[[index]], extractLinks[Flatten[Values[Values[userAssoc]]][[index]]]], {index, 1, Length[Values[userAssoc]/.{assc_}:> assc],1}], {}];

	(*Passage Content Clean*)
	cleanedPassages = cleanPassages[userAssoc];

	(*List of Buttons*)
	buttonList = cleanedPassages/. DirectedEdge[name_, content_] :> Button[name, activePassage = content];

	(*Replace the vertices of the graph with the buttons*)
	buttonGraph = 
		VertexReplace[
			Graph[Flatten[firstEdge~Join~passageEdges],
			VertexShapeFunction->"Name",
			GraphLayout->{"LayeredDigraphEmbedding","RootVertex"->First[Keys[userAssoc]]}], Table[psgKeys[[i]] -> buttonList[[i]], {i, Length[psgKeys]}]];
(*
	Summary \[Implies] {Edges, Simple Graph} 
	Graph   \[Implies] Simple Graph
	Export  \[Implies] Exports Story to TwineStories folder in current directory
*)

	Switch[outputForm,
		"Summary", GraphicsRow[{Dynamic[Framed[activePassage]] , buttonGraph}],
		"Graph", buttonGraph,
		"Export",
			If[DirectoryQ[FileNameJoin[{NotebookDirectory[], "TwineStories"}]],
				Block[{twineFolder},
					twineFolder = FileNameJoin[{NotebookDirectory[], "TwineStories"}];
					twineExport[userAssoc, twineFolder <> "/"<> ToString[First[Keys[userAssoc]]]]
				],
				Block[{twineFolder},
					twineFolder = FileNameJoin[{NotebookDirectory[], "TwineStories"}];
					CreateDirectory[twineFolder];
					twineExport[userAssoc, twineFolder <> "/"<> ToString[First[Keys[userAssoc]]]]
				]
			];
		]
]]
twineSummary::usage = "twineSummary[<| key1 \[Rule] val1 ... |>, \"Export\"] Exports WL Twine Representation to the TwineStories folder in the Current Notebook Directory";
twineSummary::usage = "twineSummary[<| key1 \[Rule] val1 ... |>, \"Graph\"] Generates Twine Graph";
twineSummary::usage = "twineSummary[<| key1 \[Rule] val1 ... |>] Displays the Twine Passages and the Graph";


EndPackage[]
