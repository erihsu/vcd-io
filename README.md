[![test](https://github.com/erihsu/vcd-io/actions/workflows/automation.yml/badge.svg?branch=main)](https://github.com/erihsu/vcd-io/actions)
[![crates.io](https://img.shields.io/crates/v/vcd_io.svg)](https://crates.io/crates/vcd_io)


# vcd-io
VCD(Value Change Dump) spec parser/saver with multi-thread speedup

# Speedup Comparation

> below shows speedup non-mt ***vs*** mt

| Case | timestamp num | variable num | non-multithread | multithreead(mt) | speedup|
| :-------- |:---------| :------- |:--------|:-----|:-----|
| example1  | 21    | 5 | 0.008 | 0.010 | -0.20 |
| example2 | 101     | 26 | 0.015 | 0.009 | 0.67 |
| example3    | 4096    | 68 | 0.442| 0.010 | 43.2|

> Evaluation by ```cargo make test```



## RoadMap
The planned [roadmap](RoadMap.md)

## Reference
[wiki](https://handwiki.org/wiki/Value_change_dump)

## License

Licensed under either of these:

 * Apache License, Version 2.0, ([LICENSE-APACHE](LICENSE-APACHE) or
   https://www.apache.org/licenses/LICENSE-2.0)
 * MIT license ([LICENSE-MIT](LICENSE-MIT) or
   https://opensource.org/licenses/MIT)
