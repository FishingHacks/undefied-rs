#!/bin/sh

cd $HOME/rust/vm
cargo build -p vm
cargo build -p assembler

cd $HOME/js/undefied-rs
cp $HOME/rust/vm/target/debug/vm ./vm
cp $HOME/rust/vm/target/debug/assembler ./std/skyvm/assembler

cargo run && ./vm