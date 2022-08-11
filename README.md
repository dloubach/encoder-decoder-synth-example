# encoder-decoder-synth-example
Tutorial example towards hardware synthesis based on ForSyDe-Deep

## ForSyDe-Deep
Please refer to [ForSyDe-Deep](https://forsyde.github.io/forsyde-deep/) for a quick start and to [System Design with ForSyDe-Deep](https://forsyde.github.io/forsyde-deep/forsyde-deep-tutorial) for detailed information.

## Installation
```
$ cd <cloned-folder>  
$ stack update  
$ stack install  
```

## How to run the example, simulate it, and generate the VHDL
```
$ stack ghci EncoderDecoder-Deep.hs
> lambdaExample_simulation [1, 4, 6, 1, 1] [256, 512, 1024, 2048, -512]
> compileVHDL
```

## Final generated RTL
![rtl](https://github.com/dloubach/encoder-decoder-synth-example/blob/master/figs/rtl.png)
