# Building Erlang/OTP-style doc for emprng for quick review

## How to edit

* Edit `rand.xml` *only*

## Building document

    ./docgen.sh

## Dependencies

* `src/specs_rand.erl` is built from `src/rand.erl`

## Notes

* `erl_docgen/` contents are copied from Erlang/OTP master as of 3-NOV-2016
* See [erl\_docgen User's Guide](http://erlang.org/doc/apps/erl_docgen/users_guide.html) for editing and writing xml
