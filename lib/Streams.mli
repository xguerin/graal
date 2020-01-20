open Types

module Mailbox (E: Environment) (T: Tuple)
  : Stream with type data = T.t
