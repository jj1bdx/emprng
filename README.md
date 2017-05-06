# emprng: an Erlang/OTP Multiple PRNG suite aka rand module

## Note well: no further development

No further development after Version 1.0.0 (as in OTP 20) in this repository.

Dan Gudmundsson has made significant improvements and changes between OTP 19 and 20. Refer to Erlang/OTP official source code for the further details.

## Target files

### Under `src/`

* `rand.erl`: OTP `random`-compatible module (`lib/stdlib/src/rand.erl`)

### Under `test/`

* `rand_SUITE.erl`: OTP `rand` test suite (`lib/stdlib/test/rand_SUITE.erl`)

### Under `xml-doc/`

* `rand.xml`: OTP `rand` manual (`lib/stdlib/doc/src/rand.xml`)

## Other directories

* `c-example/`: C code for calculating Xorshift\*/+ test values
* `test-scripts`: Erlang escript code for verifying Xorshift\*/+ test values

## License

* See individual files for the license details
* `rand.erl` is licensed under [Apache Public License 2.0](https://www.apache.org/licenses/LICENSE-2.0)

## Version

* 1.0.0
* See the Git tag for the details

## Requirements

* Erlang/OTP 20
* (Use tag `0.2.1` to retrieve the last code for OTP 17.x)
* (Use tag `0.4.0` to retrieve the last code for OTP 18.x)
* (Use tag `0.5.0` to retrieve the last code for OTP 19.0.x)
* (Use tag `0.9.2` to retrieve the last code for OTP 19.3.x)

## How to build

    make distclean  # Total cleanup
    make            # Compile BEAM files
    make tests      # Run Common Test cases
    make dialyze    # check specs

## Current goals

* Bugfix

## Completed goals

* Provide a substitute of Erlang/OTP `random` module
* Maintain full backward compatibility (seeding is semantically streamlined than `random` module)
* Extendable (accepting arbitrary algorithms)
* Write in pure Erlang
* Comply with the coding convention of Erlang/OTP `stdlib` modules
* Jump functions

## Non-goals

* NIFs (maybe a future goal, rand.erl is already capable to handle arbitrary functions/modules)

## PRNGs available as external modules

Refer to the Erlang/OTP rand module manual for the further details of the available algorithms.

* [exsplus116](https://github.com/jj1bdx/exsplus116/) (Xorshift116+, default PRNG) (the original exsplus was Xorshift128+) (exsplus116 is available since 0.4.0)
* [exs64](https://github.com/jj1bdx/exs64/) (Xorshift64\*)
* [exs1024](https://github.com/jj1bdx/exs1024/) (Xorshift1024\*)

## Acknowledgments

Thanks to Dan Gudmundsson for the idea of allowing multiple PRNG handlers to
the OTP random module, including the code review, various ideas,
implementations, pieces of code for `rand.erl` and `rand_SUITE.erl`, and
comments. Dan has committed a lot of new code between OTP 19 to 20.

Thanks to Sebastiano Vigna for providing xorshift116+ and xoroshift116+
algorithms.

Thanks to Mutsuo Saito and Makoto Matsumoto, the SFMT and TinyMT authors, for
allowing redistribution of sfmt-erlang and tinymt-erlang under Erlang Public
License.

Thanks to Erlang Solutions for giving me the talk about this repository at
Erlang Factory SF Bay Area 2015, Erlang User Conference 2016, and Erlang and
Elixir Factory SF Bay Area 2017.
