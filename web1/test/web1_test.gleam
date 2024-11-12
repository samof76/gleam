import app/router
import gleeunit
import gleeunit/should
import wisp/testing

pub fn main() {
  gleeunit.main()
}

// gleeunit test functions end in `_test`
pub fn hello_world_test() {
  let res = router.handle_request(testing.get("/", []))

  res.status
  |> should.equal(200)

  res.headers
  |> should.equal([#("content-type", "text/html; charset=utf-8")])

  res
  |> testing.string_body
  |> should.equal("<h1>Hello wisp</h1>")
}
