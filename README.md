# ProjectEulerAISolver

This project was done as partial fulfillment of the requirements of the 2360651 Technion course on advanced topics in software engineering, in the spring semester of the academic year 2024/25, under the supervision of Yossi Gil.

## ProjectEulerSolver.wl

A `wolframscript` utility that uses **OpenAI** or **Gemini** APIs to automatically generate, execute, analyze, and optimize Mathematica solutions for Project Euler problems.

---

## Features

-  **Collect** AI-generated Mathematica code for any range of Project Euler problems using OpenAI or Gemini  
-  **Execute** collected code with time and memory limits, verify answers, and fallback to stored solutions when needed  
-  **Analyze** execution results with detailed timing breakdowns (fetch, inference, execution)  
-  **Checkpointing**: automatic backup every 20 problems  
-  **API Flexibility**: switch between OpenAI and Gemini using a simple flag (`--api openai` or `--api gemini`)

---

##  Requirements

- **Wolfram Engine** (or Mathematica) 11+
- `wolframscript` available on your system `PATH`
- A file `solutions.json` containing your known/correct answers  
- A valid API key for **OpenAI** and/or **Gemini**:
  - OpenAI: [https://platform.openai.com/account/api-keys](https://platform.openai.com/account/api-keys)
  - Gemini (Google): [https://aistudio.google.com/app/apikey](https://aistudio.google.com/app/apikey)

---

The Logs page in the OpenAI dashboard shows the full backlog of API calls made during the code collection phase, where the script queried each Project Euler problem and extracted Mathematica code responses using the selected model, here is an example image that shows extracted code for the very last problems:

<img width="1919" height="818" alt="image" src="https://github.com/user-attachments/assets/49b721c2-e071-485b-970c-0b61aff54ab1" />

Here is also an example of the code that gpt 4o extracted for problem number 912:
<img width="1915" height="811" alt="image" src="https://github.com/user-attachments/assets/5dd57622-0608-4ccf-9fbe-c4806f477ab0" />

The execution results are found in the execution_results_20250708_2000.json and that is after executing all the collected AI code for all the relevant problems with analysis of executin, fetch time and also the amount of problems that were solved correctly by the AI model.
