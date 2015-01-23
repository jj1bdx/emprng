# emprng: an Erlang/OTP Multiple PRNG suite

## Goals

* Provide a substitute of Erlang/OTP `random` module
* Maintain full backward compatibility
* Extendable (= accepting arbitrary algorithms)
* Written in pure Erlang
* Comply with the code convention of Erlang/OTP `stdlib` modules

## Non-goals

* NIFs (maybe a future goal)

## Available PRNGs

* AS183: OTP `random` module
* [exs64](https://github.com/jj1bdx/exs64/) (Xorshift\*64)
* [exsplus](https://github.com/jj1bdx/exsplus/) (Xorshift+128)
* [exs1024](https://github.com/jj1bdx/exs1024/) (Xorshift\*1024)
* [sfmt-erlang](https://github.com/jj1bdx/sfmt-erlang/) (SFMT19937)
* [tinymt-erlang](https://github.com/jj1bdx/tinymt-erlang/) (TinyMT)

## How to implement PRNG algorithms on emprng

* The master module `emprng` will accept the `random`-compatible entry functions
* Each PRNG algorithm has one or more individual Erlang module

## License

* Apache License 2.0 (individual PRNG may have less restrictive licenses)
* See individual files for the license details
