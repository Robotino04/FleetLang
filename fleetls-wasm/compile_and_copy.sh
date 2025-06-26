#!/usr/bin/env bash

RELEASE_PATH=../target/wasm32-unknown-unknown/release/fleetls_wasm.wasm
OUT_PATH=../web-demo/assets/wasm

cargo build --release
mkdir -p $OUT_PATH
cp $RELEASE_PATH $OUT_PATH
wasm-bindgen $RELEASE_PATH --out-dir $OUT_PATH --target web --typescript
