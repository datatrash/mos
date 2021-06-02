# Unit testing

When writing code, it can be very convenient to be able to test individual subroutines in isolation. For instance, when writing a sorting routine, it is nice to be able to test that in a few different scenarios. This will give you extra confidence when refactoring the sorting algorithm, since if the tests still pass you know you didn't break anything.

## Defining a test
A test can be defined anywhere in your source files, and looks like a bit like this:

```asm6502
.test test_name {
    // Do some setup
    lda #123
    sta foo
    
    jsr my_subroutine
    
    // Check the output
    .assert cpu.x == 7
    .assert ram(foo) = 123
    
    // Exit the test
    brk
}
```

## Running tests
You can run tests by invoking the `mos test` command. Every test will be assembled individually and run on an emulated 6502.

## Assertions
In every part of your test (and even the subroutines you are testing) you can add `.assert` directives. The expression provided should be true, otherwise the test fails.

### Available flags
Since the tests are run on an emulated 6502 you can access CPU and memory information in your assertions as you well:

| Key | Description |
| --- | ----------- |
| cpu.a | The accumulator (A) register |
| cpu.x | The X register |
| cpu.y | The Y register |
| cpu.sp | THe stack pointer |
| * | The program counter |
| cpu.flags.zero | The Z flag |
| cpu.flags.carry | The C flag |
| cpu.flags.interrupt_disable | The I flag |
| cpu.flags.decimal | The D flag |
| cpu.flags.overflow | The V flag |
| cpu.flags.negative | The N flag |
| ram(...) | Read a byte from ram, e.g. `ram($d020)` |
| ram16(...) | Read a word from ram, e.g. `ram16($0314)` |

### Custom failure message
When an assertion fails, it will log the expression that was being tested. You can optionally also provide a custom message, like so:

```asm6502
.assert cpu.y == 123 "this is a custom message"
```

## Tracing
You can add `.trace` directives to simply log some information during the test run, e.g.:

```asm6502
.trace (cpu.x, cpu.y, ram($d020))
```

Tracing only appears for failing tests, so if your test succeeds the tracing will not pollute your output.

If you don't pass in any parameters you will get a detailed CPU dump, e.g.:

```asm6502
.trace
```

Will print something like: `* = $2000, SP = $FD, flags = -----I--, A = $00, X = $00, Y = $00`