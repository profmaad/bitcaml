open! Core.Std

val verify_der_signature
  :  public_key:string
  -> hash:Hash_string.t
  -> der_signature:string
  -> bool
