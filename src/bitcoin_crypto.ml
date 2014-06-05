let double_sha256 data =
  Sha256.to_bin (Sha256.string (Sha256.to_bin (Sha256.string data)))
;;

let message_checksum payload =
  let digest = double_sha256 payload in
  String.sub digest 0 4
;;
