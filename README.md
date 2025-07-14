# ProjectEulerAISolver
This project was done as partial fulfillment of the requirements of the 2360651 Technion course on advanced topics in software engineering, in the spring semester of the academic year 2024/25, under the supervision of Yossi GIl
# ProjectEulerSolver.wl

A WolframScript utility that uses the OpenAI API to automatically generate, execute, analyze, and optimize Mathematica solutions for Project Euler problems.

---

## Features

- **Collect** AI-generated Mathematica code for any range of Project Euler problems  
- **Execute** collected code with time/memory limits, verify against stored solutions, and automatically substitute stored answers for the toughest failures  
- **Analyze** execution results and print detailed timing breakdowns (fetch, inference, execution)  
- **Checkpointing**: automatically save progress every 20 problems so you can resume later  

---

## Requirements

- **Wolfram Engine** (or Mathematica) 11+  
- **wolframscript** available on your PATH  
- A file `solutions.json` containing your “ground truth” answers  
- A valid OpenAI API key  

---

## Installation

1. Clone (or copy) this repository into your local directory.  
2. Place your `solutions.json` alongside `ProjectEulerSolver.wl`.  
3. Make the script executable:
   ```bash
   chmod +x ProjectEulerSolver.wl
