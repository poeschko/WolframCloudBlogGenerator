(* ::Package:: *)

(* ::Title:: *)
(*Wolfram Cloud Blog Generator*)


(* ::Section:: *)
(*Directory setup*)


(* ::Input::Initialization:: *)
targetDir=CloudObject["my-blog"]


(* ::Input::Initialization:: *)
sourceDir=FileNameJoin[{targetDir,"Content"}]


(* ::Input::Initialization:: *)
commentsDir=FileNameJoin[{targetDir,"Comments"}]


(* ::Input::Initialization:: *)
CreateDirectory/@{sourceDir,commentsDir}


(* ::Section:: *)
(*Utility functions*)


(* ::Text:: *)
(*Construct simple ASCII-based URLs for deployed objects:*)


(* ::Input::Initialization:: *)
target[source_CloudObject]:=FileNameJoin[{targetDir,slugify[FileBaseName[First[source]]]}]


(* ::Input::Initialization:: *)
slugify[str_String]:=StringReplace[ToLowerCase[RemoveDiacritics[str]],{" "->"-"}]


(* ::Text:: *)
(*The title of a page is everything after the date in its file name:*)


(* ::Input::Initialization:: *)
title[source_CloudObject]:=Last[StringSplit[FileBaseName[First[source]],"-",4]]


(* ::Input::Initialization:: *)
previous[list_,index_]:=If[index>1,list[[index-1]],Null]


(* ::Input::Initialization:: *)
next[list_,index_]:=If[index<Length[list],list[[index+1]],Null]


(* ::Section:: *)
(*Notebook construction*)


(* ::Input::Initialization:: *)
cell[content_,opts___]:=Cell[BoxData[ToBoxes[content]],opts,ShowStringCharacters->False]


(* ::Input::Initialization:: *)
header=cell[Hyperlink[Style["My Blog",FontSize->24,FontFamily->"Arial",FontColor->White],indexObj],Background->Lighter[Blue],CellFrameMargins->{{16,16},{4,10}}];


(* ::Input::Initialization:: *)
notebookOptions={DockedCells->{header},ShowCellBracket->False};


(* ::Input::Initialization:: *)
deployNotebook[nb_Notebook,obj_CloudObject]:=CloudExport[Append[nb,notebookOptions],"NB",obj,Permissions->"Public"]


(* ::Section:: *)
(*Index page*)


(* ::Input::Initialization:: *)
indexObj=FileNameJoin[{targetDir,"index.nb"}]


(* ::Input::Initialization:: *)
date[source_CloudObject]:=DateString[DateObject[StringJoin[Take[StringSplit[FileBaseName[First[source]],"-",4],3],"-"]],{"MonthName"," ","DayShort",", ","Year"}]


(* ::Input::Initialization:: *)
summary[source_CloudObject]:=With[{nb=CloudGet[source]},With[{cells=Cases[nb,_Cell,Infinity]},Append[Take[DeleteCases[cells,Cell[_,"Title",___]],UpTo[3]],cell[Hyperlink["Read more\[Ellipsis]",target[source]],"Text"]]]]


(* ::Input::Initialization:: *)
indexEntry[source_CloudObject]:=Cell[CellGroupData[Join[{cell[Hyperlink[title[source],target[source]],"Title"],cell[date[source],"Subtitle",FontSize->13]},summary[source]]]]


(* ::Input::Initialization:: *)
deployIndexPage[objs_]:=deployNotebook[Notebook[indexEntry/@objs],indexObj]


(* ::Section:: *)
(*Posts*)


(* ::Input::Initialization:: *)
dateCell[source_CloudObject]:=cell[date[source],"Subtitle",FontSize->13]


(* ::Input::Initialization:: *)
addDateCell[nb_Notebook,source_CloudObject]:=With[{pos=FirstPosition[nb,Cell[_,"Title",___]]},If[!MissingQ[pos],ReplacePart[nb,pos:>Sequence[Extract[nb,pos],dateCell[source]]],nb]]


(* ::Input::Initialization:: *)
navCell[prev_,next_]:=cell[Grid[{{If[prev=!=Null,Hyperlink[title[prev],target[prev]],""],If[next=!=Null,Hyperlink[title[next],target[next]],""]}},Alignment->{{Left,Right}},ItemSize->Scaled[0.5]]]


(* ::Input::Initialization:: *)
addNavCell[nb_Notebook,prev_,next_]:=Insert[nb,navCell[prev,next],{1,1}]


(* ::Input::Initialization:: *)
createPostNotebook[source_CloudObject,prev_,next_]:=addNavCell[addDateCell[CloudGet[source],source],prev,next]


(* ::Input::Initialization:: *)
deployPost[sources_,index_]:=With[{source=sources[[index]]},deployNotebook[createPostNotebook[source,previous[sources,index],next[sources,index]],target[source]]]


(* ::Input::Initialization:: *)
deployPosts[sources_]:=deployPost[sources,#]&/@Range[Length[sources]]


(* ::Section:: *)
(*Comments*)


(* ::Text:: *)
(*Comments are stored as individual cloud objects, in one directory per post.*)


(* ::Text:: *)
(*Render a user interface displaying a list of comments and a form (input field + button) to add a comment (if the user is authenticated; otherwise show a link to log in):*)


(* ::Input::Initialization:: *)
comments[postName_,postID_,commentsDir_,loginAPI_]:=DynamicModule[{comments={},commentForm,addComment,renderComment,commentCell},Dynamic[commentCell[comments],"Text"],Initialization:>(
commentForm[]:=DynamicModule[{comment=""},Column[{Style[InputField[Dynamic[comment],String,BaseStyle->"Text"],"Text"],Button["Comment",addComment[comments,comment,$RequesterWolframID,Now];comment=""]}]];
addComment[_,text_,author_,date_]:=With[{comment=<|"text"->text,"author"->author,"date"->date|>},AppendTo[comments,comment];Export[FileNameJoin[{commentsDir,postID,CreateUUID[]}],comment,"ExpressionJSON"]];
renderComment=Panel[#text,#author,Left]&;
commentCell[comments_]:=With[{loginURL=First[loginAPI]<>"/"<>URLEncode[postName]},Column[{If[$RequesterWolframID=!=None,commentForm[],Hyperlink["Log in to comment",loginURL]],Column[renderComment/@comments]}]];
comments=Import[#,"ExpressionJSON"]&/@With[{dir=FileNameJoin[{commentsDir,postID}]},If[DirectoryQ[dir],CloudObjects[dir],{}]];
)]


(* ::Text:: *)
(*To log in a user, link to a non-public URLDispatcher that redirects back to the original post:*)


(* ::Input::Initialization:: *)
loginAPI:=CloudDeploy[URLDispatcher["/" ~~target___~~EndOfString:>HTTPRedirect[FileNameJoin[{targetDir,target}]]],FileNameJoin[{targetDir,"login"}],Permissions->{"Authenticated"->"Execute"}]


(* ::Input::Initialization:: *)
addCommentsSection[nb_Notebook,source_CloudObject]:=With[{uuid=CloudObjectInformation[source,"UUID"]},Insert[nb,Cell[CellGroupData[{cell["Comments","Section"],cell[comments[FileBaseName[First[target[source]]],uuid,commentsDir,loginAPI],"Text"]}]],{1,-1}]]


(* ::Input::Initialization:: *)
createPostNotebook[source_CloudObject,prev_,next_]:=addCommentsSection[addNavCell[addDateCell[CloudGet[source],source],prev,next],source]


(* ::Section:: *)
(*Deployment*)


(* ::Input::Initialization:: *)
sources=Sort[CloudObjects[sourceDir]]


(* ::Input::Initialization:: *)
deploy[]:=With[{index=deployIndexPage[sources],posts=deployPosts[sources]},{ParentDirectory[index],Length[posts]}]


(* ::Input::Initialization:: *)
deploy[]
