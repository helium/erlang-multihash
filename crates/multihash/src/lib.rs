use multihash::{Code, MultihashRef};
use std::{convert::TryFrom, io::Write};

use rustler::{Binary, Encoder, Env, NifResult, OwnedBinary, Term};

rustler::atoms! {
    ok,
    error,
    invalid_code,
}

#[rustler::nif]
fn digest<'a>(env: Env<'a>, data: Binary, raw_code: u64) -> NifResult<Term<'a>> {
    match Code::try_from(raw_code) {
        Ok(digest_code) => {
            let digest = digest_code.digest(data.as_slice());
            let mut binary = OwnedBinary::new(digest.len()).unwrap();
            binary.as_mut_slice().write_all(&digest).unwrap();
            Ok((ok(), Binary::from_owned(binary, env)).encode(env))
        }
        Err(_) => Ok((error(), invalid_code()).encode(env)),
    }
}

#[rustler::nif]
fn code<'a>(env: Env<'a>, data: Binary) -> NifResult<Term<'a>> {
    match MultihashRef::from_slice(data.as_slice()) {
        Ok(hash) => Ok((ok(), u64::from(hash.algorithm())).encode(env)),
        Err(_) => Ok((error(), invalid_code()).encode(env)),
    }
}

pub fn on_load(_env: Env, _load_info: Term) -> bool {
    true
}

rustler::init!("multihash_nif", [digest, code], load = on_load);
