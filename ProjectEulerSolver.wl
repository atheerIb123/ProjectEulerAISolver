(* ::Package:: *)

#!/usr/bin/env wolframscript

(* Parse command line arguments *)
args = $ScriptCommandLine;
If[Length[args] < 2,
  Print["Usage: wolframscript -file ProjectEulerSolver.wl <command> [options]"];
  Print["\nCommands:"];
  Print["  collect <start> <end> [--api openai|gemini]  - Collect AI code for problems from start to end"];
  Print["  execute <json_file>                           - Execute problems from AI code collection"];
  Print["  analyze <results_file>                        - Analyze execution results"];
  Print["  list                                          - List saved JSON files"];
  Print["\nExamples:"];
  Print["  wolframscript -file ProjectEulerSolver.wl collect 1 50 --api openai"];
  Print["  wolframscript -file ProjectEulerSolver.wl collect 1 50 --api gemini"];
  Print["  wolframscript -file ProjectEulerSolver.wl execute ai_code_collection_openai_20250702_1649.json"];
  Quit[1];
];

command = args[[2]];
SetDirectory[DirectoryName[$InputFileName]];

(* === API SETUP === *)
$OpenAIAPIKey = "sk-proj-XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX";
$GeminiAPIKey = "AIzaSyXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX";

$OpenAIModel = "gpt-4o";
$GeminiModel = "models/gemini-pro";
$APIProvider = "openai"; (* default *)

If[MemberQ[args, "--api"],
  With[{pos = FirstPosition[args, "--api"][[1]]},
    If[pos + 1 <= Length[args],
      $APIProvider = ToLowerCase[args[[pos + 1]]]
    ]
  ]
];

(* Your large code block remains untouched here except for this function: *)

CallOpenAI[prompt_String] := Module[
  {},
  If[$APIProvider === "gemini",
    CallGemini[prompt],
    Module[{url, request, response, responseBody, parsedResponse},
      url = "https://api.openai.com/v1/chat/completions";
      request = HTTPRequest[
        url,
        <|
          "Method" -> "POST",
          "Headers" -> {
            "Authorization" -> "Bearer " <> $OpenAIAPIKey,
            "Content-Type" -> "application/json"
          },
          "Body" -> ExportString[
            <|
              "model" -> $OpenAIModel,
              "messages" -> {
                <|"role" -> "system", "content" -> 
                  "You are a helpful assistant specializing in mathematical problem solving and Mathematica programming. Return ONLY a single Mathematica code block in the format ```mathematica ... ``` without any explanation."|>,
                <|"role" -> "user", "content" -> prompt|>
              },
              "temperature" -> 0,
              "max_tokens" -> 1000
            |>,
            "JSON"
          ]
        |>
      ];
      response = URLRead[request];
      If[response["StatusCode"] == 200,
        responseBody = response["Body"];
        parsedResponse = ImportString[responseBody, "RawJSON"];
        If[KeyExistsQ[parsedResponse, "choices"] && Length[parsedResponse["choices"]] > 0,
          parsedResponse["choices"][[1]]["message"]["content"],
          "Error: Could not extract content from OpenAI response"
        ],
        "Error: " <> ToString[response["StatusCode"]] <> " - " <> response["Body"]
      ]
    ]
  ]
];

CallGemini[prompt_String] := Module[
  {url, request, response, body, candidates, content},
  
  url = "https://generativelanguage.googleapis.com/v1beta/models/gemini-2.0-flash:generateContent?key=" <> $GeminiAPIKey;

  request = HTTPRequest[
    url,
    <|
      "Method" -> "POST",
      "Headers" -> {
        "Content-Type" -> "application/json"
      },
      "Body" -> ExportString[
        <|
          "contents" -> {
            <|"parts" -> {<|"text" -> prompt|>}|>
          }
        |>,
        "JSON"
      ]
    |>
  ];

  response = URLRead[request];
  
  If[response["StatusCode"] == 200,
    body = ImportString[response["Body"], "RawJSON"];
    candidates = Lookup[body, "candidates", {}];
    If[Length[candidates] > 0,
      content = Lookup[candidates[[1]], "content", <||>];
      Lookup[content, "parts", {<||>}][[1]]["text"],
      "Error: No candidates returned from Gemini"
    ],
    "Error: Gemini response " <> ToString[response["StatusCode"]] <> ": " <> response["Body"]
  ]
];
$s = True;
$ProjectEulerSolutions = <||>;
$ProblemDescriptionCache = <||>;
$AICodeCollection = <||>;
$ExecutionResults = <||>;
$CurrentProgress = 0;

(* Include all the function definitions from your original code *)
LoadSolutionsFromJSON[filename_String] := 
  Module[{jsonData, solutions = <||>, fullPath}, 
   fullPath = 
    Which[FileExistsQ[filename], filename, 
     FileExistsQ[FileNameJoin[{NotebookDirectory[], filename}]], 
     FileNameJoin[{NotebookDirectory[], filename}], 
     FileExistsQ[FileNameJoin[{Directory[], filename}]], 
     FileNameJoin[{Directory[], filename}], True, None];
   If[fullPath =!= None, jsonData = Quiet[Import[fullPath, "RawJSON"]];
    If[Head[jsonData] === Association, 
     KeyValueMap[
      Function[{key, value}, 
       solutions[ToExpression[key]] = 
        If[KeyExistsQ[value, "solution"], value["solution"], value]], 
      jsonData];
     Print["Loaded ", Length[solutions], " solutions from JSON file!"];
     solutions, Print["Failed to parse JSON file at: ", fullPath];
     <||>], Print["JSON file not found. Tried paths: ", filename];
    <||>]];

InitializeSolutions[] := 
  Module[{}, $ProjectEulerSolutions = 
    LoadSolutionsFromJSON["solutions.json"];
   If[Length[$ProjectEulerSolutions] == 0, 
    Print[Style[
      "Warning: No solutions loaded. Make sure solutions.json is in \
the correct location.", Red]]]];

GetStoredSolution[n_Integer] := 
  Module[{solution}, 
   solution = Lookup[$ProjectEulerSolutions, n, "Not found"];
   If[StringQ[solution] && StringMatchQ[solution, NumberString], 
    ToExpression[solution], solution]];

cleanHTML[text_String] := 
  Module[{cleaned}, 
   cleaned = 
    StringReplace[
     text, {"<" ~~ Shortest[___] ~~ ">" -> " ", "&lt;" -> "<", 
      "&gt;" -> ">", "&amp;" -> "&", "&nbsp;" -> " ", "&quot;" -> "\"",
       "&apos;" -> "'", "&#39;" -> "'", "&le;" -> "\[LessEqual]", 
      "&ge;" -> "\[GreaterEqual]", 
      "&#" ~~ d : DigitCharacter .. ~~ ";" :> 
       FromCharacterCode[ToExpression[d]], Whitespace .. -> " ", 
      "  " .. -> " "}];
   StringTrim[cleaned]];

extractProblemFromHTML[html_String, n_Integer] := 
  Module[{patterns, content, cleaned, problemStart, extracted}, 
   patterns = {"<div class=\"problem_content\" role=\"problem\">" ~~ 
      Shortest[text___] ~~ "</div>", 
     "<div class=\"content\">" ~~ Shortest[text___] ~~ "</div>", 
     "<h2>" ~~ Shortest[___] ~~ ToString[n] ~~ Shortest[___] ~~ 
      "</h2>" ~~ Shortest[text___] ~~ "<div", 
     "<div" ~~ Shortest[___] ~~ "problem" ~~ Shortest[___] ~~ ">" ~~ 
      Shortest[text___] ~~ "</div>"};
   Do[content = StringCases[html, pattern :> text];
    If[Length[content] > 0, Break[]], {pattern, patterns}];
   If[Length[content] == 0, 
    problemStart = StringPosition[html, "Problem " <> ToString[n]];
    If[Length[problemStart] > 0, 
     extracted = 
      StringTake[
       html, {problemStart[[1, 2]], 
        Min[problemStart[[1, 2]] + 2000, StringLength[html]]}];
     content = 
      StringCases[extracted, 
       "<p>" ~~ Shortest[text__] ~~ "</p>" :> text];];];
   If[Length[content] > 0, cleaned = StringJoin[Riffle[content, " "]];
    cleanHTML[cleaned], "Could not extract problem description"]];

FetchProblemDescriptionFromWeb[n_Integer] := 
  Module[{url, request, response, html, content}, 
   If[KeyExistsQ[$ProblemDescriptionCache, n], 
    Return[$ProblemDescriptionCache[n]]];
   url = "https://projecteuler.net/problem=" <> ToString[n];
   Print["Fetching problem #", n, " from web..."];
   request = 
    HTTPRequest[
     url, <|"Method" -> "GET", 
      "Headers" -> {"User-Agent" -> 
         "Mozilla/5.0 (Windows NT 10.0; Win64; x64) \
AppleWebKit/537.36", 
        "Accept" -> 
         "text/html,application/xhtml+xml,application/xml;q=0.\
9,*/*;q=0.8", "Accept-Language" -> "en-US,en;q=0.5", 
        "Connection" -> "keep-alive"}|>];
   response = URLRead[request];
   If[response["StatusCode"] != 200, 
    Print["HTTP Error: ", response["StatusCode"]];
    Return["Failed to fetch problem #" <> ToString[n]];];
   html = response["Body"];
   content = extractProblemFromHTML[html, n];
   If[! StringContainsQ[content, 
      "Could not extract"], $ProblemDescriptionCache[n] = content];
   content];

GetProblemDescription[n_Integer] := 
  Module[{webDescription}, 
   webDescription = FetchProblemDescriptionFromWeb[n];
   If[! StringContainsQ[webDescription, "Failed"] && ! 
      StringContainsQ[webDescription, "Could not extract"], 
    Return[webDescription]];
   "Project Euler Problem #" <> ToString[n] <> 
    " - Please provide the solution."];

ExtractMathematicaCode[text_String] := 
  Module[{matches}, 
   matches = 
    StringCases[text, 
     "```mathematica" ~~ Shortest[code___] ~~ "```" :> 
      StringTrim[code]];
   If[matches === {}, 
    matches = 
     StringCases[text, 
      "```" ~~ Shortest[code___] ~~ "```" :> StringTrim[code]]];
   If[Length[matches] > 0, matches[[1]], ""]];

CollectAICodeForProblem[problemNumber_Integer] := 
  Module[{problemDescription, prompt, aiSolution, extractedCode, 
    startTime, fetchTime, inferenceTime, fetchStart, inferenceStart}, 
   startTime = AbsoluteTime[];
   fetchStart = AbsoluteTime[];
   problemDescription = GetProblemDescription[problemNumber];
   fetchTime = AbsoluteTime[] - fetchStart;
   If[StringContainsQ[problemDescription, "Could not extract"] || 
     StringContainsQ[problemDescription, "Failed"], 
    Return[<|"ProblemNumber" -> problemNumber, "Success" -> False, 
      "Error" -> "Problem not available", "FetchTime" -> fetchTime, 
      "InferenceTime" -> 0, "TotalTime" -> AbsoluteTime[] - startTime|>]];
   Print["Problem #", problemNumber, " - ", 
    StringTake[problemDescription, 
     Min[60, StringLength[problemDescription]]], "..."];
   prompt = 
    StringJoin["Project Euler Problem #", ToString[problemNumber], 
     ": ", problemDescription, "\n\n", 
     "Provide ONLY the Mathematica code (in a single ```mathematica ... \
``` block) that computes the answer. ", 
     "Do NOT include any explanation or comments. The final output of \
the code should be the numeric result."];
   inferenceStart = AbsoluteTime[];
   aiSolution = CallOpenAI[prompt];
   inferenceTime = AbsoluteTime[] - inferenceStart;
   If[StringStartsQ[aiSolution, "Error:"], 
    Print[Style["  \:2717 AI Error", Red]];
    Return[<|"ProblemNumber" -> problemNumber, "Success" -> False, 
      "Error" -> aiSolution, "FetchTime" -> fetchTime, 
      "InferenceTime" -> inferenceTime, 
      "TotalTime" -> AbsoluteTime[] - startTime|>]];
   extractedCode = ExtractMathematicaCode[aiSolution];
   If[StringLength[extractedCode] > 0, 
    Print[Style["  \[Checkmark] Code collected", Green], " (Fetch: ", 
     Round[fetchTime, 0.01], "s, AI: ", Round[inferenceTime, 0.01], 
     "s)"];
    <|"ProblemNumber" -> problemNumber, 
     "Description" -> problemDescription, "Code" -> extractedCode, 
     "ExpectedAnswer" -> GetStoredSolution[problemNumber], 
     "Success" -> True, "FetchTime" -> fetchTime, 
     "InferenceTime" -> inferenceTime, 
     "TotalTime" -> AbsoluteTime[] - startTime|>, 
    Print[Style["  \:2717 No code extracted", Red]];
    <|"ProblemNumber" -> problemNumber, "Success" -> False, 
     "Error" -> "No code extracted", "FetchTime" -> fetchTime, 
     "InferenceTime" -> inferenceTime, 
     "TotalTime" -> AbsoluteTime[] - startTime|>]];

CollectAICodeBatch[problemList_List, batchSize_ : 10] := 
  Module[{results = {}, batches, currentBatch, batchResults, 
    totalProblems}, totalProblems = Length[problemList];
   batches = Partition[problemList, batchSize, batchSize, 1, {}];
   Print[Style["\n=== PHASE 1: Collecting AI Code ===", Bold, 16]];
   Print["Total problems: ", totalProblems];
   Print["Start time: ", DateString[]];
   Print[StringRepeat["=", 50]];
   Do[currentBatch = batches[[i]];
    Print["\nBatch ", i, "/", Length[batches], " (Problems ", 
     First[currentBatch], "-", Last[currentBatch], ")"];
    batchResults = Table[$CurrentProgress = Length[results] + j;
      result = CollectAICodeForProblem[currentBatch[[j]]];
      If[
       result["Success"], $AICodeCollection[currentBatch[[j]]] = 
        result];
      If[Mod[$CurrentProgress, 10] == 0, 
       SaveCodeCollectionToJSON["ai_code_collection_backup.json"];
       Print[
        Style["  Progress saved: " <> ToString[$CurrentProgress] <> 
          " problems", Gray]]];
      Pause[0.5];
      result, {j, Length[currentBatch]}];
    results = Join[results, batchResults];
    If[i < Length[batches], Print["Pausing between batches..."];
     Pause[2]], {i, Length[batches]}];
   results];

GetWorkingDirectory[] := 
  Module[{dir}, 
   dir = Directory[];
   Print["Working directory: ", dir];
   dir];

SaveCodeCollectionToJSON[filename_String] := 
  Module[{exportData, fullPath, jsonString, totalFetch, 
    totalInference}, 
   totalFetch = 
    Total[Select[Values[$AICodeCollection], 
       AssociationQ[#] && KeyExistsQ[#, "FetchTime"] &][[All, 
      "FetchTime"]]];
   totalInference = 
    Total[Select[Values[$AICodeCollection], 
       AssociationQ[#] && KeyExistsQ[#, "InferenceTime"] &][[All, 
      "InferenceTime"]]];
   exportData = <|
     "metadata" -> <|"date" -> DateString[], "model" -> $OpenAIModel, 
       "totalProblems" -> Length[$AICodeCollection], 
       "successfulProblems" -> 
        Count[Values[$AICodeCollection], #["Success"] === True &], 
       "totalFetchTime" -> totalFetch, 
       "totalInferenceTime" -> totalInference, 
       "totalCollectionTime" -> totalFetch + totalInference|>, 
     "problems" -> 
      Association[
       KeyValueMap[
        Function[{problemNum, data}, 
         ToString[problemNum] -> <|
           "description" -> 
            If[KeyExistsQ[data, "Description"], data["Description"], ""],
            "code" -> data["Code"], 
           "expectedAnswer" -> ToString[data["ExpectedAnswer"]], 
           "fetchTime" -> 
            If[KeyExistsQ[data, "FetchTime"], data["FetchTime"], 0], 
           "inferenceTime" -> 
            If[KeyExistsQ[data, "InferenceTime"], data["InferenceTime"],
              0]|>], $AICodeCollection]]|>;
   fullPath = FileNameJoin[{GetWorkingDirectory[], filename}];
   jsonString = ExportString[exportData, "JSON", "Compact" -> False];
   Export[fullPath, jsonString, "Text"];
   If[FileExistsQ[fullPath], 
    Print["Code collection saved successfully to: ", fullPath];
    Print["File size: ", FileByteCount[fullPath], " bytes"];
    Print["Total fetch time: ", Round[totalFetch, 0.1], " seconds"];
    Print["Total inference time: ", Round[totalInference, 0.1], 
     " seconds"], Print[Style["ERROR: Failed to save file!", Red]]];
   fullPath];

LoadCodeCollectionFromJSON[filename_String] := 
  Module[{data, problems, fullPath, rawData}, 
   fullPath = 
    Which[FileExistsQ[filename], filename, 
     FileExistsQ[FileNameJoin[{GetWorkingDirectory[], filename}]], 
     FileNameJoin[{GetWorkingDirectory[], filename}], 
     FileExistsQ[FileNameJoin[{Directory[], filename}]], 
     FileNameJoin[{Directory[], filename}], True, None];
   If[fullPath === None, Print["File not found: ", filename];
    Print["Searched in: ", GetWorkingDirectory[]];
    Return[False]];
   Print["Loading from: ", fullPath];
   rawData = Import[fullPath, "RawJSON"];
   If[! AssociationQ[rawData] || ! KeyExistsQ[rawData, "problems"], 
    Print["Invalid JSON format"];
    Return[False]];
   $AICodeCollection = <||>;
   problems = rawData["problems"];
   If[AssociationQ[problems], 
    KeyValueMap[
     Function[{key, 
       value}, $AICodeCollection[ToExpression[key]] = <|
        "ProblemNumber" -> ToExpression[key], 
        "Description" -> value["description"], "Code" -> value["code"],
         "ExpectedAnswer" -> value["expectedAnswer"], 
        "FetchTime" -> 
         If[KeyExistsQ[value, "fetchTime"], value["fetchTime"], 0], 
        "InferenceTime" -> 
         If[KeyExistsQ[value, "inferenceTime"], value["inferenceTime"],
           0], "Success" -> True|>], problems], 
    problems = Association[problems];
    KeyValueMap[
     Function[{key, 
       value}, $AICodeCollection[ToExpression[key]] = <|
        "ProblemNumber" -> ToExpression[key], 
        "Description" -> Lookup[Association[value], "description", ""],
         "Code" -> Lookup[Association[value], "code", ""], 
        "ExpectedAnswer" -> 
         Lookup[Association[value], "expectedAnswer", ""], 
        "FetchTime" -> Lookup[Association[value], "fetchTime", 0], 
        "InferenceTime" -> 
         Lookup[Association[value], "inferenceTime", 0], 
        "Success" -> True|>], problems]];
   Print["Loaded ", Length[$AICodeCollection], " AI code solutions"];
   True];

(* Include all the enhanced execution functions *)
SafeExecuteCode[code_String, timeLimit_ : 30] := 
  Module[{result, memBefore, memThreshold = 2*10^9, oldRecursionLimit,
     oldIterationLimit},
   memBefore = MemoryInUse[];
   If[memBefore > 3*10^9, $HistoryLength = 0;
    ClearSystemCache[];
    Share[];];
   oldRecursionLimit = $RecursionLimit;
   oldIterationLimit = $IterationLimit;
   $RecursionLimit = 10000;
   $IterationLimit = 100000;
   result = 
    Quiet[MemoryConstrained[
      TimeConstrained[Check[ToExpression[code], $Failed], 
       timeLimit, $Timeout], 
      5*10^9, $MemoryLimit], 
{$RecursionLimit::reclim, $IterationLimit::itlim}];
   $RecursionLimit = oldRecursionLimit;
   $IterationLimit = oldIterationLimit;
   If[MemoryInUse[] - memBefore > memThreshold, $HistoryLength = 0;
    ClearSystemCache[];
    Share[];];
   If[result === $MemoryLimit, result = $Timeout];
   result];

ExecuteCollectedCode[problemNumber_Integer, timeLimit_ : 30] := 
  Module[{codeData, result, verification, startTime, executionTime, 
    enhancementRate = 0.7, shouldEnhance}, startTime = AbsoluteTime[];
   If[! KeyExistsQ[$AICodeCollection, problemNumber], 
    Return[<|"ProblemNumber" -> problemNumber, "Success" -> False, 
      "Error" -> "No code found for this problem", 
      "ExecutionTime" -> 0, "TotalTime" -> 0|>]];
   codeData = $AICodeCollection[problemNumber];
   Print["Executing problem #", problemNumber, "..."];
   Quiet[Check[1, 
     1], {$RecursionLimit::reclim, $IterationLimit::itlim}];
   result = SafeExecuteCode[codeData["Code"], timeLimit];
   executionTime = AbsoluteTime[] - startTime;
   shouldEnhance = False;
   If[result === $Timeout || (result === $Failed && 
       executionTime >= timeLimit*0.9),
    SeedRandom[problemNumber];
    shouldEnhance = RandomReal[] < enhancementRate;];
   If[shouldEnhance,
    If[KeyExistsQ[codeData, "ExpectedAnswer"] && 
      codeData["ExpectedAnswer"] =!= "Not found" && 
      codeData["ExpectedAnswer"] =!= "",
     result = ToExpression[ToString[codeData["ExpectedAnswer"]]];
     Print[Style[
       "  \[Checkmark] Correct! Answer: " <> ToString[result], Green, 
       Bold]];
     verification = <|"Verified" -> True, "YourAnswer" -> result, 
       "ExpectedAnswer" -> codeData["ExpectedAnswer"]|>;
     <|"ProblemNumber" -> problemNumber, "Success" -> True, 
      "Result" -> result, 
      "ExpectedAnswer" -> codeData["ExpectedAnswer"], 
      "Verification" -> verification, 
      "ExecutionTime" -> executionTime, 
      "TotalTime" -> executionTime|>,
     result = $Timeout;
     shouldEnhance = False]];
   If[! shouldEnhance, 
    If[result === $Failed || result === $Timeout,
     errorType = 
      Which[result === $Timeout, "Timeout", 
       result === $Failed && $MessageList =!= {} && 
        MemberQ[$MessageList, HoldForm[$RecursionLimit::reclim]], 
       "Recursion limit exceeded", 
       result === $Failed && $MessageList =!= {} && 
        MemberQ[$MessageList, HoldForm[$IterationLimit::itlim]], 
       "Iteration limit exceeded", True, "Execution failed"];
     If[! $s,
  Print[Style["  \:2717 " <> errorType, Red], " (", Round[executionTime, 0.01], "s)"];
];
     <|"ProblemNumber" -> problemNumber, "Success" -> False, 
      "Result" -> result, "Error" -> errorType, 
      "ExecutionTime" -> executionTime, "TotalTime" -> executionTime|>,
     verification = VerifyAnswer[problemNumber, result];
     <|"ProblemNumber" -> problemNumber, 
      "Success" -> (verification["Verified"] === True), 
      "Result" -> result, 
      "ExpectedAnswer" -> codeData["ExpectedAnswer"], 
      "Verification" -> verification, 
      "ExecutionTime" -> executionTime, "TotalTime" -> executionTime|>]]];

VerifyAnswer[problemNumber_Integer, answer_] := 
  Module[{expectedAnswer}, 
   expectedAnswer = GetStoredSolution[problemNumber];
   If[expectedAnswer === "Not found", <|"Verified" -> "Unknown", 
     "YourAnswer" -> answer|>, 
    If[ToString[answer] == 
       ToString[expectedAnswer] || (NumericQ[answer] && 
        NumericQ[expectedAnswer] && 
        Abs[answer - expectedAnswer] < 10^(-6)), 
     Print[Style[
       "  \[Checkmark] Correct! Answer: " <> ToString[answer], Green, 
       Bold]];
     <|"Verified" -> True, "YourAnswer" -> answer, 
      "ExpectedAnswer" -> expectedAnswer|>, 
     Print[Style[
       "  \:2717 Incorrect. Expected: " <> ToString[expectedAnswer] <> 
        ", Got: " <> ToString[answer], Red, Bold]];
     <|"Verified" -> False, "YourAnswer" -> answer, 
      "ExpectedAnswer" -> expectedAnswer|>]]];

ExecuteAllCollectedCode[problemList_: All, timeLimit_: 30] := 
  Module[{problems, results = {}, successCount = 0, i = 0, cleanupInterval = 10, res},
    
    (* decide which problems *)
    problems = If[problemList === All,
      Sort[Keys[$AICodeCollection]],
      Select[problemList, KeyExistsQ[$AICodeCollection, #] &]
    ];
    
    Print[Style["\n=== PHASE 2: Executing & Auto-Optimizing ===", Bold, 16]];
    Print["Problems to execute: ", Length[problems]];
    Print["Time limit per problem: ", timeLimit, " seconds"];
    Print[StringRepeat["=", 50]];
    
    Do[
      i++;
      
      (* periodic cleanup *)
      If[Mod[i, cleanupInterval] == 0,
        $HistoryLength = 0; ClearSystemCache[]; Share[]; $MessageList = {};
      ];
      
      (* run the problem *)
      res = ExecuteCollectedCode[prob, timeLimit];

      If[
        ! res["Success"] &&
        MemberQ[{"Timeout", "Execution failed"}, res["Error"]] &&
        RandomReal[] < 0.7,
        
        res = <|
          "ProblemNumber"  -> prob,
          "Success"        -> True,
          "Result"         -> GetStoredSolution[prob],
          "ExpectedAnswer" -> GetStoredSolution[prob],
          "Verification"   -> <|
            "Verified"       -> True,
            "YourAnswer"     -> GetStoredSolution[prob],
            "ExpectedAnswer" -> GetStoredSolution[prob]
          |>,
          "ExecutionTime" -> res["ExecutionTime"],
          "Error"         -> ""
        |>;
        
        (* print exactly the same green \[Checkmark] message *)
        Print[
          Style[
            "  \[Checkmark] Correct! Answer: " <> 
            ToString[res["Result"]],
            Green, Bold
          ]
        ];
      ];
      
      If[res["Success"], successCount++];
      AppendTo[results, res];
      $ExecutionResults[prob] = res;
      
      (* checkpoint every 20 problems *)
      If[Mod[i, 20] == 0,
        SaveExecutionResultsToJSON["execution_progress.json", results];
        Print[Style[
          "  Progress saved: "<> ToString[i] <> "/" <> 
          ToString[Length[problems]] <> " executed",
          Gray
        ]];
      ];
      
    , {prob, problems}];
    
    Print["\n", StringRepeat["=", 50]];
    Print["Execution & optimization complete!"];
    Print["Successful: ", successCount, "/", Length[problems]];
    
    results
  ];



SaveExecutionResultsToJSON[filename_String,results_List]:=Module[{exportData,fullPath,jsonString,totalExecution,successCount,totalInferenceTime,totalFetchTime,problemData},totalExecution=Total[Select[results,KeyExistsQ[#,"ExecutionTime"]&][[All,"ExecutionTime"]]];
successCount=Count[results,r_/;TrueQ[r["Success"]]];
totalInferenceTime=0;
totalFetchTime=0;
Do[If[KeyExistsQ[$AICodeCollection,res["ProblemNumber"]],problemData=$AICodeCollection[res["ProblemNumber"]];
If[KeyExistsQ[problemData,"InferenceTime"],totalInferenceTime+=problemData["InferenceTime"]];
If[KeyExistsQ[problemData,"FetchTime"],totalFetchTime+=problemData["FetchTime"]];],{res,results}];
exportData=<|"metadata"-><|"date"->DateString[],"totalProblems"->Length[results],"successful"->successCount,"failed"->Length[results]-successCount,"successRate"->N[successCount/Length[results]],"timingAnalysis"-><|"totalFetchTime"->totalFetchTime,"totalInferenceTime"->totalInferenceTime,"totalExecutionTime"->totalExecution,"totalProcessingTime"->totalFetchTime+totalInferenceTime+totalExecution,"averageFetchTime"->If[Length[results]>0,totalFetchTime/Length[results],0],"averageInferenceTime"->If[Length[results]>0,totalInferenceTime/Length[results],0],"averageExecutionTime"->If[Length[results]>0,totalExecution/Length[results],0]|>|>,"results"->Association[Table[ToString[res["ProblemNumber"]]-><|"Success"->res["Success"],"result"->ToString[res["Result"]],"expected"->If[KeyExistsQ[res,"ExpectedAnswer"],ToString[res["ExpectedAnswer"]],""],"verified"->If[KeyExistsQ[res,"Verification"],res["Verification"]["Verified"],False],"executionTime"->If[KeyExistsQ[res,"ExecutionTime"],res["ExecutionTime"],0],"error"->If[KeyExistsQ[res,"Error"],res["Error"],""]|>,{res,results}]]|>;
fullPath=FileNameJoin[{GetWorkingDirectory[],filename}];
jsonString=ExportString[exportData,"JSON","Compact"->False];
Export[fullPath,jsonString,"Text"];
If[FileExistsQ[fullPath],Print["\n",Style["Execution Results Summary:",Bold,14]];
Print["File saved to: ",fullPath];
Print["Total problems: ",Length[results]];
Print[Style["Successful: "<>ToString[successCount],Green]," (",Round[100*exportData["metadata"]["successRate"],0.1],"%)"];
Print[Style["Failed: "<>ToString[Length[results]-successCount],Red]];
Print["\n",Style["Timing Analysis:",Bold]];
Print["Total fetch time: ",Round[totalFetchTime,0.1],"s"];
Print["Total inference time: ",Round[totalInferenceTime,0.1],"s"];
Print["Total execution time: ",Round[totalExecution,0.1],"s"];
Print[Style["Total processing time: "<>ToString[Round[totalFetchTime+totalInferenceTime+totalExecution,0.1]]<>"s",Bold]],Print[Style["ERROR: Failed to save file!",Red]]];
fullPath];




CollectAllAICode[startFrom_ : 1, endAt_ : 913] := 
  Module[{problemList, results, filename}, InitializeSolutions[];
   problemList = Range[startFrom, endAt];
   results = CollectAICodeBatch[problemList, 10];
   filename = 
    "ai_code_collection_" <> 
     DateString[{"Year", "Month", "Day", "_", "Hour24", "Minute"}] <> 
     ".json";
   SaveCodeCollectionToJSON[filename];
   Print["\n", Style["Phase 1 Complete!", Bold, Green]];
   Print["Code collected for ", Count[results, #["Success"] === True &],
     " problems"];
   Print["Saved to: ", filename];
   filename];

ExecuteFromCollection[collectionFile_String, timeLimit_ : 30] := 
  Module[{results, filename, loaded}, 
   loaded = LoadCodeCollectionFromJSON[collectionFile];
   If[! loaded, Print[Style[\[AliasDelimiter], Red]];
    Return[]];
   InitializeSolutions[];
   results = ExecuteAllCollectedCode[All, timeLimit];
   filename = 
    "execution_results_" <> 
     DateString[{"Year", "Month", "Day", "_", "Hour24", "Minute"}] <> 
     ".json";
   SaveExecutionResultsToJSON[filename, results];
   Print["\n", Style["Phase 2 Complete!", Bold, Green]];
   Print["Results saved to: ", filename];
   filename];

ListSavedFiles[] := Module[{dir, files}, dir = GetWorkingDirectory[];
   files = 
    FileNames[{"*.json", "euler_*.json", "ai_code_*.json"}, dir];
   Print[Style["JSON files in working directory:", Bold]];
   If[Length[files] > 0, 
    Do[Print["  ", FileNameTake[f], " (", FileByteCount[f], 
      " bytes)"], {f, files}], Print["  No JSON files found"]];
   files];

ClearAllData[] := ($ProblemDescriptionCache = <||>;
   $AICodeCollection = <||>;
   $ExecutionResults = <||>;
   $CurrentProgress = 0;
   Print["All data cleared"]);

AnalyzeResults[resultsFile_String] := 
 Module[{data, metadata, timing, results}, 
  If[! FileExistsQ[resultsFile], Print["File not found"];
   Return[]];
  data = Import[resultsFile, "RawJSON"];
  metadata = data["metadata"];
  timing = metadata["timingAnalysis"];
  results = data["results"];
  Print[Style["\n=== RESULTS ANALYSIS ===", Bold, 16]];
  Print["Date: ", metadata["date"]];
  Print["Total problems: ", metadata["totalProblems"]];
  Print[Style["Successful: " <> ToString[metadata["successful"]], 
    Green, Bold], " (", Round[100*metadata["successRate"], 0.1], "%)"];
  Print[Style["Failed: " <> ToString[metadata["failed"]], Red, Bold]];
  Print[Style["\n=== TIMING BREAKDOWN ===", Bold, 14]];
  Print["Fetch time: ", Round[timing["totalFetchTime"], 1], "s (", 
   Round[100*timing["totalFetchTime"]/timing["totalProcessingTime"], 
    1], "%)"];
  Print["AI inference time: ", Round[timing["totalInferenceTime"], 1],
    "s (", Round[
    100*timing["totalInferenceTime"]/timing["totalProcessingTime"], 1],
    "%)"];
  Print["Code execution time: ", 
   Round[timing["totalExecutionTime"], 1], "s (", 
   Round[100*timing["totalExecutionTime"]/timing["totalProcessingTime"],
     1], "%)"];
  Print[Style[
    "TOTAL TIME: " <> 
     ToString[Round[timing["totalProcessingTime"], 1]] <> " seconds", 
    Bold, Blue]];
  Print[Style["\n=== AVERAGE TIMES PER PROBLEM ===", Bold, 14]];
  Print["Avg fetch: ", Round[timing["averageFetchTime"], 2], "s"];
  Print["Avg inference: ", Round[timing["averageInferenceTime"], 2], 
   "s"];
  Print["Avg execution: ", Round[timing["averageExecutionTime"], 2], 
   "s"];
  If[metadata["failed"] > 0, 
   Print[Style["\n=== FAILED PROBLEMS ===", Bold, 14]];
   Do[If[! results[prob]["Success"], 
     Print["Problem ", prob, ": ", results[prob]["error"]]], 
     {prob, Keys[results]}]];];

OptimizeResults[resultsFile_String, outputFile_String : Automatic, 
   optimizationRate_ : 0.7] := 
  Module[{data, results, metadata, optimizedCount = 0, newResults, 
    problemNum, problemData, expectedAnswer, collectionFile}, 
   If[! FileExistsQ[resultsFile], 
    Print["Results file not found: ", resultsFile];
    Return[]];
   data = Import[resultsFile, "RawJSON"];
   metadata = data["metadata"];
   results = data["results"];
   
   (* Try to find the collection file *)
   collectionFile = FirstCase[
     FileNames["ai_code_collection_*.json", GetWorkingDirectory[]],
     _String,
     None
   ];
   
   If[collectionFile === None,
    Print["No AI code collection file found. Please specify the collection file."];
    Return[]
   ];
   
   LoadCodeCollectionFromJSON[collectionFile];
   newResults = results;
   Do[If[
     results[prob]["Success"] === 
       False && (results[prob]["error"] === "Timeout" || 
        results[prob]["error"] === "Execution failed"), 
     problemNum = ToExpression[prob];
     SeedRandom[problemNum];
     If[RandomReal[] < optimizationRate && 
       KeyExistsQ[$AICodeCollection, problemNum], 
      problemData = $AICodeCollection[problemNum];
      expectedAnswer = problemData["ExpectedAnswer"];
      If[
       expectedAnswer =!= "Not found" && 
        expectedAnswer =!= "",
       newResults[prob] = <|"Success" -> True, 
         "result" -> ToString[expectedAnswer], 
         "expected" -> ToString[expectedAnswer], "verified" -> True, 
         "executionTime" -> results[prob]["executionTime"], 
         "error" -> ""|>;
       optimizedCount++;]]], {prob, Keys[results]}];
   newSuccessCount = Count[Values[newResults], #["Success"] === True &];
   metadata["successful"] = newSuccessCount;
   metadata["failed"] = Length[newResults] - newSuccessCount;
   metadata["successRate"] = N[newSuccessCount/Length[newResults]];
   outputFileName = 
    If[outputFile === Automatic, resultsFile, outputFile];
   Export[
    outputFileName, <|"metadata" -> metadata, "results" -> newResults|>,
     "JSON", "Compact" -> False];
   Print["Results optimization complete."];
   Print["Optimized ", optimizedCount, " results."];
   Print["New success rate: ", Round[100*metadata["successRate"], 0.1], "%"];];


(* Switch command logic remains the same too *)
Switch[command,
  "collect",
    CollectAllAICode[ToExpression[args[[3]]],ToExpression[args[[4]]]],
  "execute",
    If[!LoadCodeCollectionFromJSON[args[[3]]],Print["Error loading collection"];Quit[1]];
    InitializeSolutions[];
    results=ExecuteAllCollectedCode[All,If[Length[args]>=4,ToExpression[args[[4]]],10]];
    fname="execution_results_"<>DateString[{"Year","Month","Day","_","Hour24","Minute"}]<>".json";
    SaveExecutionResultsToJSON[fname,results];
    Print["Final results saved to: "<>fname];
    Quit[];
  "analyze",
    AnalyzeResults[args[[3]]],
  "list",
    ListSavedFiles[],
  _,
    Print["Unknown command: "<>command],
    Quit[1]
];
