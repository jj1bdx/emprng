# emprng: an Erlang/OTP Multiple PRNG suite

## Goals

* Provide a substitute of Erlang/OTP `random` module
* Maintain full backward compatibility
* Extendable (= accepting arbitraty algorithms)
* Written in pure Erlang
* Comply the code convention of Erlang/OTP `stdlib` modules

## Non-goals

* NIFs (maybe a future goal)

## Candidate PRNGs

* AS183: `random` module
* [exs64](https://github.com/jj1bdx/exs64/) (Xorshift\*64)
* [exsplus](https://github.com/jj1bdx/exsplus/) (Xorshift+128)
* [exs1024](https://github.com/jj1bdx/exs1024/) (Xorshift\*1024)
* [sfmt-erlang](https://github.com/jj1bdx/sfmt-erlang/) (SFMT19937)
* [tinymt-erlang](https://github.com/jj1bdx/tinymt-erlang/) (TinyMT)

## How to implement PRNG algorithms

* A master module `emprng` will accept the `random`-compatible entry functions
* Each PRNG algorithm has one or more individual Erlang modules
