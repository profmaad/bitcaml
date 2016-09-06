module Protocol = struct
  include Bitcoin_protocol;;

  module PP = struct
    include Bitcoin_protocol_pp;;
  end

  module Parser = struct
    include Bitcoin_protocol_parser;;
  end
  module Generator = struct
    include Bitcoin_protocol_generator;;
  end
end

module Crypto = struct
  include Bitcoin_crypto;;

  module ECDSA = struct
    include Bitcoin_crypto_ecdsa;;
  end
end

module Peer = struct
  include Bitcoin_peer;;
end

module Blockchain = struct
  include Bitcoin_blockchain;;
end

module Script = struct
  include Bitcoin_script;;

  module Parser = struct
    include Bitcoin_script_parser;;
  end
  module Generator = struct
    include Bitcoin_script_generator;;
  end

  module PP = struct
    include Bitcoin_script_pp;;
  end

  module Interpreter = struct
    include Bitcoin_script_interpreter;;
  end
end

module Rules = struct
  include Bitcoin_rules;;
end
