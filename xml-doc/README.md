# Building Erlang/OTP-style doc for emprng for quick review

## How to edit

* Edit `rand.xml` *only*

## Building document

    ./docgen.sh

## How to review

* open `rand.html` (Note: prebuilt version is commited in the repository)

## Dependencies

* `src/specs_rand.erl` is built from `src/rand.erl`

## Notes

* `docgen.sh` will use the default Erlang library's `erl_docgen` module (See `./erl_docgen_lib.escript`)
* Some contents are copied from Erlang/OTP master `erl_docgen` module as of 3-NOV-2016
* See [erl\_docgen User's Guide](http://erlang.org/doc/apps/erl_docgen/users_guide.html) for editing and writing xml
