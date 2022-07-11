# Hidato Haskell

## Prerequisites

- Stack
- GHC
  

## Install

``` sh
    stack install
```

## Usage

```
Solver [--version] [--file VALUE]

Available options: 

  -h,--help                Show this help text

  --version                Show version 

  --file VALUE             file with generated hidato (default: "board")
```

```
Generator [--version] [-w --width INT] [-h| --height INT] 
                 [-m| --minvalue INT] [-t --template INT] [-f| --file VALUE]

Available options:
  -h,--help                Show this help text
  --version                Show version
  -w,--width INT           Board width (default: 4)
  -h,--height INT          Board height (default: 4)
  -m,--minvalue INT        Minimal value of a cell in the board (default: 1)
  -t,--template INT        0: cloud 1: mirror 2: square (default: 0)
  -f,--file VALUE          File to save (default: "board.txt")

```