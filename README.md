# Bogus-Amogus

Simple Virtual Machine and Assembly-Like Language written in Free Pascal.

## Quick Start

1. Compile everything with `fpc`:

```shell
$ fpc bogus.pas
$ fpc amogus.pas
```

or

```shell
$ ./all.sh
```

2. Compile `program.amogus` to `.bogus` "executable":

```shell
$ ./amogus program.amogus output.bogus
```

3. Run `output.bogus` "executable" with the *Bogus Virtual Machine*:

```shell
$ ./bogus output.bogus
```

## Amogus Language

Amogus Language is an assembly-like language.

So far there are only 4 instructions:

|Instruction|Params                       |Description                                                                 |
|-----------|-----------------------------|----------------------------------------------------------------------------|
|`load`     |`<register>`, `<number>`     |Loads `<number>` into `<register>`                                          |
|`add`      |`<register1>`, `<register2>` |Adds `<register2>` value to `<register1>` and saves to `<register1>`        |
|`sub`      |`<register1>`, `<register2>` |Subtracts `<register1>` value from `<register2>` and saves to `<register1>` |
|`stop`     |N/A                          |Ends the program                                                            |

## TODO List

- [ ] - Add more instructions like clearing the register etc.
- [ ] - Memory stack and stack operations like push, pop
- [ ] - Labels, jumps, and perhaps sections?
- [ ] - Gui for *Bogus Virtual Machine*

## License

This project is under the [MIT](./LICENSE) License.

