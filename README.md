# brainskell

Brainfuck interpreter in Haskell with a few extra features.


`>`: Move pointer right

`<`: Move pointer left

`+`: Increment value at pointer

`-`: Decrement value at pointer

`.`: Print value at pointer as `chr(x)`

`,`: Input, read terminal and store value at pointer. If the input is an integer, will store that value directly, if the input starts with a quote (`'`), will store `ord(input)` instead

`[`: Start loop

`]`: Jumps to matching `[` if the input at pointer is nonzero, otherwise pass

Anything else is considered as comment.
