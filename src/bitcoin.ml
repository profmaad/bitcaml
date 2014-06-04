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
end

module Peer = struct
  include Bitcoin_peer;;
end
