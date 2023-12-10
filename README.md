# eXXXtra - The New Programming Language

## GitHub Pages
[![See all MosMetro projects](https://raw.githubusercontent.com/MAILabs-Edu-2023/fp-compiler-lab-mosmetro/prepare_documentation/assets/pictures/See_all_MosMetro_projects_Button.png?token=GHSAT0AAAAAACK44LNEZP7MMXRWZMCSUX5EZLVSFIA)](https://lkhorasandzhian.github.io/exxxtra-website/)

## VS Code
[![Open in Visual Studio Code](https://classroom.github.com/assets/open-in-vscode-718a45dd9cf7e7f842a935f5ebbe5719a5e09af4491e668f4dbf3b35d5cca122.svg)](https://classroom.github.com/online_ide?assignment_repo_id=12420066&assignment_repo_type=AssignmentRepo)

## Implemented language features

* [x] Named variables (`let`)
* [x] Recursion
* [ ] Lazy evaluation
* [x] Functions
* [x] Closures
* [ ] Library functions: File IO
* [x] Lists / Sequences
* [x] Library functions: Lists/Sequences

## Working Step-By-Step

* Stage 1: Developing abstract syntax tree and parser for your language + one sample program
* Stage 2: Developing interpreter/compiler for your language
* Stage 3: Writing sample programs and documentation

## Tutorial on the language
> TODO: write down some tutorial about using every command here.

## Code samples on eXXXtra

### Sample of using Let
```xxx
(
    (let x (
            (let y 5 (+ 5 y))
        )
        
        (let y (+ x 3)
            (* x y)
        )
    )
)
```

### Sample of using Defun
```xxx
(
    (defun fib (N)
        (
            (if (or (= N 0) (= N 1)) 
                1
                (
                    (let f1 (fib (- N 1))
                        (let f2 (fib (- N 2))
                            (+ f1 f2)
                        )
                    )
                )
            )
        )
        
        fib 7
    )
)
```

### Other samples
[The folder](https://github.com/MAILabs-Edu-2023/fp-compiler-lab-mosmetro/tree/main/examples), containing samples for each feature.

## Credits

*MosMetro team:*
Name | Role in the project
------------------|---------------------
Levon Khorasandzhian | The creator of interpreter & tester of interpreter & editor of documentation
Nikita Biryulin | The creator of parser & tester of parser & assistant with interpreter
Vitaliy Cherkasskiy | The creator of interpreter & tester of interpreter & GitHub feature assistant

<img src="https://soshnikov.com/images/byhuman_en.png" height="25px"/>
