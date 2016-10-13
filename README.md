# emprng: an Erlang/OTP Multiple PRNG suite aka rand module

## Modules under `src/`

* `rand.erl`: OTP `random`-compatible module (`lib/stdlib/src/rand.erl`)

## Modules under `test/`

* `rand_SUITE.erl`: OTP `rand` test suite (`lib/stdlib/test/rand_SUITE.erl`)
* `ts_install_cth.erl`: OTP Common Test suite file for standalone testing (`lib/common_test/test_server/ts_install_cth.erl`)

## License

* See individual files for the license details
* `rand.erl` is licensed under [Apache Public License 2.0](https://www.apache.org/licenses/LICENSE-2.0)

## Version

* 0.6.0
* See the Git tag for the details

## Requirements

* Erlang/OTP 19.1 and later
* (Use tag `0.2.1` to retrieve the last code for OTP 17.x)
* (Use tag `0.4.0` to retrieve the last code for OTP 18.x)
* (Use tag `0.5.0` to retrieve the last code for OTP 19.0.x)

## Current goals

* Jump functions (preliminary test added, more testing needed)

## Completed goals

* Provide a substitute of Erlang/OTP `random` module
* Maintain full backward compatibility (seeding is semantically streamlined than `random` module)
* Extendable (accepting arbitrary algorithms)
* Write in pure Erlang
* Comply with the coding convention of Erlang/OTP `stdlib` modules

## Non-goals

* NIFs (maybe a future goal, rand.erl is already capable to handle arbitrary functions/modules)

## Available PRNGs

* [exsplus116](https://github.com/jj1bdx/exsplus116/) (Xorshift116+, default PRNG) (the original exsplus was Xorshift128+) (exsplus116 is available since 0.4.0)
* [exs64](https://github.com/jj1bdx/exs64/) (Xorshift64\*)
* [exs1024](https://github.com/jj1bdx/exs1024/) (Xorshift1024\*)

## Acknowledgments

Thanks to Dan Gudmundsson for the idea of allowing multiple PRNG handlers to
the OTP random module, including the code review, various ideas,
implementations, pieces of code for `rand.erl`, and comments.

Thanks to Sebastiano Vigna for providing xorshift116+ algorithm.

Thanks to Mutsuo Saito and Makoto Matsumoto, the SFMT and TinyMT authors, for
allowing redistribution of sfmt-erlang and tinymt-erlang under
Erlang Public License.

Thanks to Erlang Solutions for giving me the talk about this repository at Erlang Factory SF Bay Area 2015 and Erlang User Conference 2016.
