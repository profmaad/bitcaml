let double_sha256 data =
  Sha256.to_bin (Sha256.string (Sha256.to_bin (Sha256.string data)))
;;
