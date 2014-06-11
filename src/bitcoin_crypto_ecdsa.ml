let decode_der_signature signature =
  signature
;;

let verify_der_signature public_key hash der_signature =
  let public_key = if public_key.[0] = '\x04' then public_key else Microecc.decompress public_key in
  let signature = decode_der_signature der_signature in
  Microecc.verify public_key hash signature
;;
