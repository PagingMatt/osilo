open Nocrypto

open Cipher_block.AES.GCM

let encrypt ~key ~plaintext =
  let key    = of_secret key in
  let iv     = Rng.generate 256 in 
  let result = encrypt ~key ~iv plaintext in
  result.message, iv

let decrypt ~key ~ciphertext ~iv =
  let key = of_secret key in
  let result = decrypt ~key ~iv ciphertext in
  result.message

let () = Nocrypto_entropy_unix.initialize ()
