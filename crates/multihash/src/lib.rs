extern crate rustler;

use rustler::{Env, Term};

mod atoms;
mod multihash;

fn load(env: Env, _: Term) -> bool {
    multihash::load(env);
    true
}

rustler::init!(
    "erlang_multihash",
    [
        multihash::code,
        multihash::digest,
    ],
    load=load
);
