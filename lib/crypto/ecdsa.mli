open! Core.Std

val verify_der_signature
  :  public_key:string
  -> hash:string
  -> der_signature:string
  -> bool
