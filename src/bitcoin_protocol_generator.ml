open Bitcoin_protocol;;
open Bitstring;;

let bitstring_of_header header =
  BITSTRING {
    int32_of_magic header.magic : 4*8 : littleendian;
    Utils.zeropad_string_to_length (string_of_command header.command) 12 : 12*8 : string;
    Int32.of_int header.payload_length : 4*8 : littleendian;
    header.checksum : 4*8 : string
  }
;;
