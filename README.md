# eXXXtra - The New Programming Language

## GitHub Pages
[![See all MosMetro projects](https://github.com/MAILabs-Edu-2023/fp-compiler-lab-mosmetro/blob/prepare_documentation/assets/pictures/See_all_MosMetro_projects_Button.png)](https://lkhorasandzhian.github.io/exxxtra-website/)

## VS Code
[![Open in Visual Studio Code](https://classroom.github.com/assets/open-in-vscode-718a45dd9cf7e7f842a935f5ebbe5719a5e09af4491e668f4dbf3b35d5cca122.svg)](https://classroom.github.com/online_ide?assignment_repo_id=12420066&assignment_repo_type=AssignmentRepo)

## Implemented language features

* [x] Named variables (`let`)
* [x] Recursion
* [x] Lazy evaluation
* [x] Functions
* [x] Closures
* [x] Library functions: File IO
* [x] Lists / Sequences
* [x] Library functions: Lists/Sequences

## Working Step-By-Step

* Stage 1: Developing abstract syntax tree and parser for your language + one sample program
* Stage 2: Developing interpreter/compiler for your language
* Stage 3: Writing sample programs and documentation

## Tutorial on the language

### Operators

All math and logic operations must be written in this way:
```('operator' 'operand_1' 'operand_2')```.

For example:
* ```(+ 2 2)```
* ```(or true false)```

### Variables

Variable must be declared with following syntax:
```xxx
(let 'varName' 'value' 'visible code area')
```

For example:
* ```(let cost 150.25 (cost))```
> P.S. in this case we're declaring variable cost with value 150.25 and permanently print it from the visible code area.

### Functions
Variable must be declared with following syntax:
```xxx
(defun 'funcName' ('varName_1' 'varName_2' 'varName_3'...) ('body of function') 'visible code area')
```
> P.S. obviously you can use  multiple variables, so just separate them with space.

There are two types of collection: List and Sequence.

#### List
So, list must be declared in the _value section_ of ```let``` with following syntax:
```xxx
(list 'value_1' 'value_2' 'value_3'...)
```

For example:
* ```(let myList (list 1 2 3) mylist)```
> P.S. in this case we're declaring list and permanently print it from the visible code area.

### Sequence
The syntax is similar to List, but has two features of declaring:

First one:
```xxx
(let mySeq (seq 'left' .. 'right'))
```
Second one:
```xxx
(let mySeq (seq 'left' .. 'step' .. 'right'))
```

> P.S. the first case is a special case of the second, but only with 'step' = 1

Instead of List, Sequence holds in memory only triple parameters mentioned above. It uses lazy evaluation principle and starts enumerate values in selected range only when it's neccessary.

### Library functions

#### List & Sequence

There are several supported functions starting with keywords ```list``` or ```seq```:

* head
* tail
* item

Syntax is here:
* ```list/seq head/tail 'collectionName'```
* ```list/seq item 'collectionIndex' 'collectionName'```

> P. S. symbol '/' means 'or', so choose one of them.

#### File

There are several supported functions starting with keywords ```file```:

* create
* read
* write

Syntax is here:
* ```file create 'absolutePath'``` — create file in selected directory with requested name (must be written in the end).
* ```file read 'absolutePath'``` — read file and return string.
* ```file write 'absolutePath' 'textInfo'``` — append text to the file.

> P.S. all library functions work with .txt format.

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

## MosMetro Team

| Name                 | Role in the project                                                           |
| ---                  | ---                                                                           |
| Levon Khorasandzhian | The creator of interpreter & tester of interpreter & editor of documentation  |
| Nikita Biryulin      | The creator of parser & tester of parser & assistant with interpreter         |
| Vitaliy Cherkasskiy  | The creator of interpreter & tester of interpreter & GitHub feature assistant |

<img src="https://soshnikov.com/images/byhuman_en.png" height="25px"/>
