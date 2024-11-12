import gleeunit
import gleeunit/should
import internal

pub fn main() {
  gleeunit.main()
}

pub fn format_pair_test() {
  internal.format_pair("USER", "msv")
  |> should.equal("USER=msv")
}
