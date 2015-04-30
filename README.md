# emprng: an Erlang/OTP Multiple PRNG suite

## Modules

* `rand.erl`: prototype OTP `random`-compatible module

## License

* See individual files for the license details
* `rand.erl` is licensed under [Erlang Public License](http://www.erlang.org/EPLICENSE)

## Requirements

* Erlang/OTP 18.0-rc1 and later
* (Use tag `0.2.1` to retrieve the last code for 17.x)

## Goals

* Provide a substitute of Erlang/OTP `random` module
* Maintain full backward compatibility
* Extendable (= accepting arbitrary algorithms)
* Written in pure Erlang
* Comply with the coding convention of Erlang/OTP `stdlib` modules

## Non-goals

* NIFs * maybe a future goal, already capable to handle arbitrary functions/modules

## Available PRNGs

* Modified [exsplus](https://github.com/jj1bdx/exsplus/) (Xorshift116+, default PRNG; the original exsplus is Xorshift128+, available since 0.4.0)
* [exs64](https://github.com/jj1bdx/exs64/) (Xorshift64\*)
* [exs1024](https://github.com/jj1bdx/exs1024/) (Xorshift1024\*)

## Acknowledgments

Thanks to Dan Gudmundsson for the idea of allowing multiple PRNG handlers to
the OTP random module, including the code review, various ideas,
implementations, pieces of code for `rand.erl`, and comments.

Thanks to Sebastiano Vigna for providing xorshift116+ alrogithm.

Thanks to Mutsuo Saito and Makoto Matsumoto, the SFMT and TinyMT authors, for
allowing redistribution of sfmt-erlang and tinymt-erlang under
Erlang Public License.

