#warnings "+20+26+27+32+39"

let (:=) _ _ = failwith "FORBIDDEN: You should not use := in this exercise."
let ignore _ =
  failwith "FORBIDDEN: You should not use this function in this exercise."
