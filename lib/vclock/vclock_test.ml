module Ord = Vclock.Ordering

module Int_vclock = Vclock.Make(
  struct
    type t = int
    let equal = (=)
  end)

let mk =
  let rec mk' clock = function
    | 0 ->
      clock
    | n ->
      mk' (Int_vclock.increment n clock) (n - 1)
  in
  mk' Int_vclock.empty

let test_assert () =
  try
    assert (1 = 2);
    failwith "Assertion checking not on"
  with
    | Assert_failure _ ->
      ()

let basic_test () =
  let c1 = mk 2 in
  let c2 = c1 in
  assert (Ord.Eq = Int_vclock.compare c1 c2);
  let c1' = Int_vclock.increment 0 c1 in
  let c2' = Int_vclock.increment 1 c2 in
  assert (Int_vclock.compare c1 c1'  = Ord.Lt);
  assert (Int_vclock.compare c2 c2'  = Ord.Lt);
  assert (Int_vclock.compare c2 c1'  = Ord.Lt);
  assert (Int_vclock.compare c1 c2'  = Ord.Lt);
  assert (Int_vclock.compare c1' c1  = Ord.Gt);
  assert (Int_vclock.compare c2' c2  = Ord.Gt);
  assert (Int_vclock.compare c1' c2' = Ord.Concurrent)

let main () =
  test_assert ();
  basic_test ()

let () = main ()
